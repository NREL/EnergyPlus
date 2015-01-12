#include "atlas_misc.h"
void ATL_UAXPY(const int N, const SCALAR alpha0, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   register TYPE x0, y0;
   const register TYPE alpha=alpha0;
   const TYPE *stX, *stXN = X+N;
   if (N >= 64)
   {
      stX = X + ((N>>6)<<6) - 32;
      x0 = *X;
      y0 = *Y;
      do
      {
         *Y = y0 + x0 * alpha; x0 = X[32]; y0 = Y[32];
         Y[ 1] += X[ 1] * alpha;
         Y[ 2] += X[ 2] * alpha;
         Y[ 3] += X[ 3] * alpha;
         Y[ 4] += X[ 4] * alpha;
         Y[ 5] += X[ 5] * alpha;
         Y[ 6] += X[ 6] * alpha;
         Y[ 7] += X[ 7] * alpha;
         Y[ 8] += X[ 8] * alpha;
         Y[ 9] += X[ 9] * alpha;
         Y[10] += X[10] * alpha;
         Y[11] += X[11] * alpha;
         Y[12] += X[12] * alpha;
         Y[13] += X[13] * alpha;
         Y[14] += X[14] * alpha;
         Y[15] += X[15] * alpha;
         Y[16] += X[16] * alpha;
         Y[17] += X[17] * alpha;
         Y[18] += X[18] * alpha;
         Y[19] += X[19] * alpha;
         Y[20] += X[20] * alpha;
         Y[21] += X[21] * alpha;
         Y[22] += X[22] * alpha;
         Y[23] += X[23] * alpha;
         Y[24] += X[24] * alpha;
         Y[25] += X[25] * alpha;
         Y[26] += X[26] * alpha;
         Y[27] += X[27] * alpha;
         Y[28] += X[28] * alpha;
         Y[29] += X[29] * alpha;
         Y[30] += X[30] * alpha;
         Y[31] += X[31] * alpha;
         X += 32; Y += 32;
      }
      while(X != stX);
      *Y = y0 + x0 * alpha;
      Y[ 1] += X[ 1] * alpha;
      Y[ 2] += X[ 2] * alpha;
      Y[ 3] += X[ 3] * alpha;
      Y[ 4] += X[ 4] * alpha;
      Y[ 5] += X[ 5] * alpha;
      Y[ 6] += X[ 6] * alpha;
      Y[ 7] += X[ 7] * alpha;
      Y[ 8] += X[ 8] * alpha;
      Y[ 9] += X[ 9] * alpha;
      Y[10] += X[10] * alpha;
      Y[11] += X[11] * alpha;
      Y[12] += X[12] * alpha;
      Y[13] += X[13] * alpha;
      Y[14] += X[14] * alpha;
      Y[15] += X[15] * alpha;
      Y[16] += X[16] * alpha;
      Y[17] += X[17] * alpha;
      Y[18] += X[18] * alpha;
      Y[19] += X[19] * alpha;
      Y[20] += X[20] * alpha;
      Y[21] += X[21] * alpha;
      Y[22] += X[22] * alpha;
      Y[23] += X[23] * alpha;
      Y[24] += X[24] * alpha;
      Y[25] += X[25] * alpha;
      Y[26] += X[26] * alpha;
      Y[27] += X[27] * alpha;
      Y[28] += X[28] * alpha;
      Y[29] += X[29] * alpha;
      Y[30] += X[30] * alpha;
      Y[31] += X[31] * alpha;
      X += 32; Y += 32;
   }
   if (X != stXN)
   {
      do *Y++ += *X++ * alpha; while (X != stXN);
   }
}
