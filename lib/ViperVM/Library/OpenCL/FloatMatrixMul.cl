__kernel void floatMatrixMul(uint K,
      const float __global *A, uint lda, unsigned int offsetA,
      const float __global *B, uint ldb, unsigned int offsetB,
      __global float *C, int ldc, unsigned int offsetC){

   int i = get_global_id(1);
   int j = get_global_id(0);

   float sum = 0;
   for(uint k=0;k<K;k++){
      sum += A[i*lda+k+offsetA]*B[k*ldb + j+offsetB];
   }

   C[i*ldc + j + offsetC] = sum;
}
