/* ==================================================================
   Programmer: Yicheng Tu (ytu@cse.usf.edu)
   The basic SDH algorithm implementation for 3D data
   To compile: nvcc SDH.c -o SDH in the C4 lab machines
   ==================================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/time.h>


const long BOX_SIZE = 23000; /* size of the data box on one dimension */

/* descriptors for single atom in the tree */
typedef struct atomdesc {
    double x_pos;
    double y_pos;
    double z_pos;
} atom;

typedef struct hist_entry {
    //float min;
    //float max;
    unsigned long long d_cnt;   /* need a long long type as the count might be huge */
} bucket;

struct debuginfo {
    int idx;
    int ran;
    int i;
    int j;
    float dist;
    int which_bucket;
};


bucket * histogram;  /* list of all buckets in the histogram   */
unsigned long long PDH_acnt; /* total number of data points            */
int num_buckets;  /* total number of buckets in the histogram */
double PDH_res;  /* value of w                             */
atom * atom_list;  /* list of all data points                */

/* These are for an old way of tracking time */
struct timezone Idunno;
struct timeval startTime, endTime;


/*
 distance of two points in the atom_list
*/
double p2p_distance(int ind1, int ind2) {

    double x1 = atom_list[ind1].x_pos;
    double x2 = atom_list[ind2].x_pos;
    double y1 = atom_list[ind1].y_pos;
    double y2 = atom_list[ind2].y_pos;
    double z1 = atom_list[ind1].z_pos;
    double z2 = atom_list[ind2].z_pos;

    return sqrt((x1 - x2)*(x1-x2) + (y1 - y2)*(y1 - y2) + (z1 - z2)*(z1 - z2));
}


/*
 brute-force SDH solution in a single CPU thread
*/
int PDH_baseline() {
    int i, j, h_pos;
    double dist;

    for(i = 0; i < PDH_acnt; i++) {
        for(j = i+1; j < PDH_acnt; j++) {
            dist = p2p_distance(i,j);
            h_pos = (int) (dist / PDH_res);
            if (h_pos >= 0 && h_pos < num_buckets)
                histogram[h_pos].d_cnt++;
            else
                printf("Warning: value %lf falls outside histogram", dist);
        }
    }
    return 0;
}

__global__
void PDH_kernel(long n_threads, bucket *d_buckets, int n_buckets, const atom *d_atoms, double w
#ifdef DEBUG
                , struct debuginfo *d_dinfo
#endif
    ) {
    int idx = blockIdx.x*blockDim.x + threadIdx.x;
    if (idx >= n_threads)
        return;
    
    // Please don't make me explain. It was 2am and I scribbled some math and it works
    int i = (sqrt(8.0*idx + 1.0) - 1.0)/2;
    int j = idx - i*(i + 1)/2;
    i++;

    double deltax = d_atoms[i].x_pos - d_atoms[j].x_pos;
    double deltay = d_atoms[i].y_pos - d_atoms[j].y_pos;
    double deltaz = d_atoms[i].z_pos - d_atoms[j].z_pos;

    double dist = sqrt(deltax*deltax + deltay*deltay + deltaz*deltaz);
    int h_pos = (int) (dist / w);
    if (h_pos >= 0 && h_pos < n_buckets)
        // atomicAdd(&d_buckets[h_pos].d_cnt, 1);
        d_buckets[h_pos*n_threads + idx].d_cnt++; // Coalesce!



#ifdef DEBUG
    d_dinfo[idx].idx = idx;
    d_dinfo[idx].i = i;
    d_dinfo[idx].j = j;
    d_dinfo[idx].ran = 1;
    d_dinfo[idx].dist = dist;
    d_dinfo[idx].which_bucket = (int) (dist / w);
#endif
}

void PDH_gpu() {
    unsigned long num_threads = PDH_acnt*(PDH_acnt - 1)/2;

    // allocate histogram
    bucket *d_buckets;
    cudaMalloc(&d_buckets, sizeof(*histogram) * num_buckets);
    cudaMemset(d_buckets, 0, sizeof(*histogram) * num_buckets);

#ifdef DEBUG
    // allocate debuginfo
    struct debuginfo *d_dinfo;
    cudaMalloc(&d_dinfo, sizeof(*d_dinfo) * num_threads);
    cudaMemset(d_dinfo, 0, sizeof(*d_dinfo) * num_threads);
#endif

    // Copy atoms to device
    atom *d_atoms;
    cudaMalloc(&d_atoms, sizeof(*atom_list) * PDH_acnt);
    cudaMemcpy(d_atoms, atom_list, sizeof(*atom_list) * PDH_acnt, cudaMemcpyHostToDevice);

    PDH_kernel<<<(num_threads + 255)/256, 256>>>(num_threads, d_buckets, num_buckets, d_atoms, PDH_res
    // PDH_kernel<<<1, num_threads>>>(num_threads, d_buckets, num_buckets, d_atoms, PDH_res
#ifdef DEBUG
                                                 , d_dinfo
#endif
        );
    // cudaDeviceSynchronize();

    cudaError_t err = cudaGetLastError();
    if (err != cudaSuccess) {
        printf("CUDA ERROR: %s\n", cudaGetErrorString(err));
        puts("This is probably due to a too-large block count");
    }

    // Copy histogram from device and cleanup
    cudaFree(d_atoms);
    cudaMemcpy(histogram, d_buckets, sizeof(*histogram) * num_buckets, cudaMemcpyDeviceToHost);
    cudaFree(d_buckets);

#ifdef DEBUG
    // Copy debuginfo from device and cleanup
    struct debuginfo *h_dinfo = (struct debuginfo *) malloc(sizeof(*h_dinfo) * num_threads);
    cudaMemcpy(h_dinfo, d_dinfo, sizeof(*h_dinfo) * num_threads, cudaMemcpyDeviceToHost);
    cudaFree(d_dinfo);

    for (unsigned long long i=0; i<num_threads; i++) {
        printf("%llu: idx=%d, ran=%d, i=%d, j=%d, dist=%f, bucket=%d\n",
               i, h_dinfo[i].idx, h_dinfo[i].ran, h_dinfo[i].i, h_dinfo[i].j,
               h_dinfo[i].dist, h_dinfo[i].which_bucket);
    }
#endif
}


/*
 set a checkpoint and show the (natural) running time in seconds
*/
double report_running_time(const char *type) {
    long sec_diff, usec_diff;
    gettimeofday(&endTime, &Idunno);
    sec_diff = endTime.tv_sec - startTime.tv_sec;
    usec_diff= endTime.tv_usec - startTime.tv_usec;
    if (usec_diff < 0) {
        sec_diff--;
        usec_diff += 1000000;
    }
    printf("Running time for %s version: %ld.%06ld\n", type, sec_diff, usec_diff);
    return (double)(sec_diff*1.0 + usec_diff/1000000.0);
}


/*
 print the counts in all buckets of the histogram
*/
void output_histogram() {
    int i;
    unsigned long long total_cnt = 0;
    for(i=0; i<num_buckets; i++) {
        if (i%5 == 0) /* we print 5 buckets in a row */
            printf("\n%02d: ", i);
        printf("%15lld ", histogram[i].d_cnt);
        total_cnt += histogram[i].d_cnt;
        /* we also want to make sure the total distance count is correct */
        if (i == num_buckets - 1)
            printf("\n T:%lld \n", total_cnt);
        else printf("| ");
    }
}


int main(int argc, char **argv)
{
    int i;

    PDH_acnt = atoi(argv[1]);
    PDH_res = atof(argv[2]);
    // printf("args are %d and %f\n", PDH_acnt, PDH_res);

    num_buckets = (int)(BOX_SIZE * 1.732 / PDH_res) + 1;
    histogram = (bucket *)malloc(sizeof(bucket)*num_buckets);

    atom_list = (atom *)malloc(sizeof(atom)*PDH_acnt);


    srand(1);
    /* generate data following a uniform distribution */
    for(i = 0; i < PDH_acnt; i++) {
        atom_list[i].x_pos = ((double)(rand()) / RAND_MAX) * BOX_SIZE;
        atom_list[i].y_pos = ((double)(rand()) / RAND_MAX) * BOX_SIZE;
        atom_list[i].z_pos = ((double)(rand()) / RAND_MAX) * BOX_SIZE;
    }

    // CPU implementation
    puts("Running CPU version...");
    memset(histogram, 0, sizeof(*histogram) * num_buckets);
    gettimeofday(&startTime, &Idunno);
    PDH_baseline();
    report_running_time("CPU");
    output_histogram();

    // GPU implementation
    puts("\nRunning GPU version...");
    memset(histogram, 0, sizeof(*histogram) * num_buckets);
    gettimeofday(&startTime, &Idunno);
    PDH_gpu();
    report_running_time("GPU");
    output_histogram();


    return 0;
}
