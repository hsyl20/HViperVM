/*
 * Solve X in X*A = B where A is lower-triangular
 * group width must 1
 */
__kernel void floatMatrixTrsm(__global float * A, const uint strideA, const uint offA, 
                              __global float * B, const uint strideB, const uint offB, 
                              __global float * X, const uint strideX, const uint offX, 
                              const uint n, const uint valid) {
   
   int gy = get_global_id(1);

   #define gA(x,y) A[offA+y*strideA+x]
   #define gB(x,y) B[offB+y*strideB+x]
   #define gX(x,y) X[offX+y*strideX+x]
   //TODO: we could have some sharing on B

   if (gy >= valid)
      return;

   uint i = n;

   do {
      i -= 1;
      float xval = gA(i,gy);
      for (uint j=n-1; j>i; j--) {
         xval -= gB(i,j) * gX(j,gy);
      }
      xval /= gB(i,i);
      gX(i,gy) = xval;
   } while (i != 0);
}
