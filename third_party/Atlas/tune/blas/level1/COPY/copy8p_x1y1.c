#include "atlas_misc.h"
#include "atlas_prefetch.h"

void ATL_UCOPY(const int N, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int n;
   register TYPE x0, x1, x2, x3, x4, x5, x6, x7;
   const TYPE *stX, *stX0 = X + N;

   if (N >= 16)
   {
      stX = X + ((N>>3)<<3);
      x0 = *X; x1 = X[1]; x2 = X[2]; x3 = X[3];
      x4 = X[4]; x5 = X[5]; x6 = X[6]; x7 = X[7]; X += 8;
      do
      {
         ATL_pfl1R(X+16);
         #ifdef DREAL
            ATL_pfl1R(X+20);
         #endif
         *Y = x0;   x0 = *X;
         Y[1] = x1; x1 = X[1];
         Y[2] = x2; x2 = X[2];
         Y[3] = x3; x3 = X[3];
         ATL_pfl1W(Y+ 8);
         #ifdef DREAL
            ATL_pfl1W(Y+12);
         #endif
         Y[4] = x4; x4 = X[4];
         Y[5] = x5; x5 = X[5];
         Y[6] = x6; x6 = X[6];
         Y[7] = x7; x7 = X[7];
         X += 8;
         Y += 8;
      }
      while (X != stX);
      *Y = x0; Y[1] = x1; Y[2] = x2; Y[3] = x3;
      Y[4] = x4; Y[5] = x5; Y[6] = x6; Y[7] = x7;
      Y += 8;
   }
   if (X != stX0) do *Y++ = *X++; while (X != stX0);
}
