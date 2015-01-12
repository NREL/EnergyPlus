#include "atlas_misc.h"
#include "atlas_prefetch.h"
void ATL_UCOPY(const int N, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int i, n;
   const TYPE *stX, *stX0 = X + N;

   stX = X + ((N>>5)<<5);
   if (X != stX)
   {
      do
      {
         ATL_pfl1R(X+168);
         ATL_pfl1W(Y+168);
         *Y = *X;
         Y[ 1] = X[ 1];
         Y[ 2] = X[ 2];
         Y[ 3] = X[ 3];
         Y[ 4] = X[ 4];
         Y[ 5] = X[ 5];
         Y[ 6] = X[ 6];
         Y[ 7] = X[ 7];
         Y[ 8] = X[ 8];
         Y[ 9] = X[ 9];
         Y[10] = X[10];
         Y[11] = X[11];
         Y[12] = X[12];
         Y[13] = X[13];
         Y[14] = X[14];
         Y[15] = X[15];
         Y[16] = X[16];
         Y[17] = X[17];
         Y[18] = X[18];
         Y[19] = X[19];
         Y[20] = X[20];
         Y[21] = X[21];
         Y[22] = X[22];
         Y[23] = X[23];
         Y[24] = X[24];
         Y[25] = X[25];
         Y[26] = X[26];
         Y[27] = X[27];
         Y[28] = X[28];
         Y[29] = X[29];
         Y[30] = X[30];
         Y[31] = X[31];
         X += 32; Y += 32;
      }
      while (X != stX);
   }
   if (X != stX0) do *Y++ = *X++; while (X != stX0);
}
