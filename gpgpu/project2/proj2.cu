#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <error.h>
#include <unistd.h>

const long BOX_SIZE = 23000; /* size of the data box on one dimension */
#define BLOCK_SIZE 512

#define EXITERROR() error_at_line(errno, errno, __FILE__, __LINE__, "pid %llu", (long long unsigned)getpid())

const char *argv0;

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
unsigned long long num_points; /* total number of data points            */
int num_buckets;  /* total number of buckets in the histogram */
double resolution;  /* value of w                             */
double4 *h_points;


inline __device__ double dist3(double4 a, double4 b) {
    return norm3d(a.x - b.x, a.y - b.y, a.z - b.z);
}


__global__
void PDH_kernel(bucket *g_bins, size_t n_bins, double4 *g_points, size_t n_points, double res) {
    __shared__ double4 s_block[BLOCK_SIZE]; // R for inter-block, L for intra-block

    unsigned int t = threadIdx.x;
    unsigned int b = blockIdx.x;
    unsigned int B = blockDim.x;
    unsigned int M = gridDim.x;

    // Is our anchor point past the end of g_points?
    if (b*B + t >= n_points)
        return;

    // Anchor point in L
    double4 l_point = g_points[b*B + t];

    // Inter-block pairs, with block-level load balancing
    for (unsigned int i=1; i<=M/2; i++) {
        // Load balancing edge case
        if (M%2 == 0 && i >= M/2 && b >= M/2)
            continue;

        // Get R block
        unsigned int r = (b + i) % M;
        __syncthreads();
        if (r*B + t < n_points)
            s_block[t] = g_points[r*B + t];
        __syncthreads();

        // inter-block pairs
        for (unsigned int j=0; j<B; j++) {
            if (r*B + j < n_points) {
                double d = dist3(l_point, s_block[j]);
                unsigned long long bin = (unsigned long long)(d / res);
                if (bin <= n_bins)
                    atomicAdd(&g_bins[bin].d_cnt, 1);
            }
        }
    }

    // Get L block
    __syncthreads();
    if (b*B + t < n_points)
        s_block[t] = g_points[b*B + t];
    __syncthreads();

    // intra-block pairs
    for (unsigned int i=t+1; i<B && b*B+i<n_points; i++) {
        double d = dist3(s_block[t], s_block[i]);
        unsigned long long bin = (unsigned long long)(d / res);
        if (bin <= n_bins)
            atomicAdd(&g_bins[bin].d_cnt, 1);
    }
}

void PDH_gpu() {
    unsigned long num_threads = num_points*(num_points - 1)/2;

    // allocate histogram
    bucket *d_buckets;
    cudaMalloc(&d_buckets, sizeof(*histogram) * num_buckets);
    cudaMemset(d_buckets, 0, sizeof(*histogram) * num_buckets);

    // Copy points to device
    double4 *d_points;
    cudaMalloc(&d_points, sizeof(*h_points) * num_points);
    cudaMemcpy(d_points, h_points, sizeof(*h_points) * num_points, cudaMemcpyHostToDevice);

    PDH_kernel<<<(num_points + BLOCK_SIZE - 1)/BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE>>>(
        d_buckets, num_buckets, d_points, num_points, resolution);

    cudaError_t err = cudaGetLastError();
    if (err != cudaSuccess) {
        printf("CUDA ERROR: %s\n", cudaGetErrorString(err));
        puts("This is probably due to a too-large block count");
    }

    // Copy histogram from device and cleanup
    cudaFree(d_points);
    cudaMemcpy(histogram, d_buckets, sizeof(*histogram) * num_buckets, cudaMemcpyDeviceToHost);
    cudaFree(d_buckets);
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
        if (i == num_buckets - 1) {
            printf("\n          Total: %lld", total_cnt);
            printf("\n Expected total: %lld \n", num_points*(num_points - 1)/2);
        } else {
            printf("| ");
        }
    }
}

void usage(FILE *f, int ret) {
    fprintf(f, "USAGE: %s <SAMPLES> <RESOLUTION>\n", argv0);
    exit(ret);
}

int main(int argc, char **argv) {
    argv0 = argv[0];

    if (argc < 3)
        usage(stderr, 1);

    if (!strcmp(argv[1], "--help") || !strcmp(argv[1], "-h"))
        usage(stdout, 0);

    errno = 0;
    num_points = strtoull(argv[1], NULL, 10);
    if (errno != 0)
        EXITERROR();

    errno = 0;
    resolution = strtof(argv[2], NULL);
    if (errno != 0)
        EXITERROR();


    num_buckets = (int)(BOX_SIZE * 1.732 / resolution) + 1;
    histogram = (bucket *)malloc(sizeof(bucket)*num_buckets);

    h_points = (double4 *)malloc(sizeof(double4)*num_points);


    srand(1);
    /* generate data following a uniform distribution */
    for(size_t i = 0; i < num_points; i++) {
        h_points[i].x = ((double)(rand()) / RAND_MAX) * BOX_SIZE;
        h_points[i].y = ((double)(rand()) / RAND_MAX) * BOX_SIZE;
        h_points[i].z = ((double)(rand()) / RAND_MAX) * BOX_SIZE;
    }

    // GPU implementation
    puts("\nRunning Reg-SHM version...");
    memset(histogram, 0, sizeof(*histogram) * num_buckets);

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);
    cudaEventRecord(start, 0);

    PDH_gpu();

    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    float elapsed;
    cudaEventElapsedTime(&elapsed, start, stop);
    printf("Running time for Reg-SHM version: %.6f sec\n", elapsed/1000.0);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);

    output_histogram();


    return 0;
}
