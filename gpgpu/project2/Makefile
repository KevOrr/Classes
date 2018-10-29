NVCC := nvcc
NVCCOPTS := --compiler-options="-Wall" -Xptxas -O4

proj2: proj2.cu
	$(NVCC) $(NVCCOPTS) $^ -o $@
