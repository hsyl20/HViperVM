__kernel void floatMatrixAdd(const uint width, const uint height,
   __global float * A, const uint strideA, const uint offA,
   __global float * B, const uint strideB, const uint offB,
   __global float * C, const uint strideC, const uint offC) {

  int gx = get_global_id(0);
  int gy = get_global_id(1);

  if (gx < width && gy < height) {
    C[offC + gy*strideC + gx] = A[offA + gy*strideA + gx] + B[offB + gy*strideB + gx];
  }

}
