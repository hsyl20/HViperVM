__kernel void floatMatrixMul(uint W, uint H, uint K,
      const float __global *A, uint lda, unsigned int offsetA,
      const float __global *B, uint ldb, unsigned int offsetB,
            float __global *C, uint ldc, unsigned int offsetC){

   int i = get_global_id(1);
   int j = get_global_id(0);

   if (i < W && j < H) {
      float sum = 0;
      for(uint k=0;k<K;k++){
         sum += A[i*lda+k+offsetA]*B[k*ldb + j+offsetB];
      }

      C[i*ldc + j + offsetC] = sum;
   }
}
