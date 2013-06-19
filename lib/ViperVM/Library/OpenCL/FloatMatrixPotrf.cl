/* Cholesky factorization
 *  - Single work-group
 *  - Row-major order
 */
__kernel void floatMatrixPotrf(const uint n, const uint srcOffset, const uint srcStride, __global float* srcBuf,
      const uint dstOffset, const uint dstStride, __global float* dstBuf) {

#define src(X,Y) srcBuf[srcOffset + X + Y*srcStride]
#define dst(X,Y) dstBuf[dstOffset + X + Y*dstStride]

   int x = get_global_id(0);
   int y = get_global_id(1);
   int active = x < n && y < n;

   __local float tmp[16][16];

   if (y==0 && active) {
      tmp[x][y] = sqrt(src(x,y));
      dst(x,y) = tmp[x][y] * (x == 0);
   }
   barrier(CLK_LOCAL_MEM_FENCE);

   if(x==0 && y>0 && active) {
      tmp[x][y] = src(x,y) / tmp[0][0];
      dst(x,y) = tmp[x][y];
   }
   barrier(CLK_LOCAL_MEM_FENCE);

   if(x>0 && y>0 && x<=y && active) {
      tmp[x][y] = src(x,y) - tmp[0][y]*tmp[0][x];
      dst(x,y) = tmp[x][y];
   }
   barrier(CLK_LOCAL_MEM_FENCE);

   for(int i=1 ; i<n ; i++) {

      if (x>=i && y==i && active) {
         tmp[x][y] = sqrt(tmp[x][y]);
         dst(x,y) = tmp[x][y] * (x == i);
      }
      barrier(CLK_LOCAL_MEM_FENCE);

      if(x==i && y>i && active) {
         tmp[x][y] /= tmp[i][i];
         dst(x,y) = tmp[x][y];
      }
      barrier(CLK_LOCAL_MEM_FENCE);

      if(x>i && y>i && x<=y && active) {
         tmp[x][y] -= tmp[i][y]*tmp[i][x];
         dst(x,y) = tmp[x][y];
      }
      barrier(CLK_LOCAL_MEM_FENCE);
   }
}
