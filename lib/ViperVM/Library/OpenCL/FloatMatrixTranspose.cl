__kernel void floatMatrixTranspose(uint W, uint H,
      const float __global *A, uint lda, unsigned int offsetA,
            float __global *B, uint ldb, unsigned int offsetB){

   int i = get_global_id(1);
   int j = get_global_id(0);

   if (i < W && j < H) {
      B[j*ldb + i + offsetB] = A[i*lda + j + offsetA];
   }
}
