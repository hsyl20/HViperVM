__kernel void floatMatrixAdd(const uint width, const uint height,
   __global float* A, __global float* B, __global float* C) {
  int gx = get_global_id(0);
  int gy = get_global_id(1);

  if (gx < width && gy < height) {
    C[gy*width+gx] = A[gy*width+gx] + B[gy*width+gx];
  }

}
