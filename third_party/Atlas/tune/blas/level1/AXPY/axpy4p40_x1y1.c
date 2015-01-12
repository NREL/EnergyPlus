#include "atlas_misc.h"
#include "atlas_prefetch.h"

void ATL_UAXPY(const int N, const SCALAR alpha0, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int n;
   const TYPE *stX, *stX0 = X + N;
   register TYPE alpha=alpha0;
   register TYPE x0, x1, y0, y1, y2, y3;

   n = ATL_AlignOffset(N, Y, ATL_sizeof, ATL_MulBySize(4));
   if (n)
   {
      stX = X + n;
      do *Y++ += alpha * *X++; while(X != stX);
      n = N - n;
   }
   else n = N;

   if (n >= 20)
   {
      stX = X + ((n>>2)<<2) - 10;
      y0 = *Y; y1 = Y[1]; y2 = Y[2]; y3 = Y[3];
      x0 = *X; x1 = X[1];
      y0 += alpha * x0;
      y1 += alpha * x1;
      x0 = X[2]; x1 = X[3];
      y2 += alpha * x0;
      y3 += alpha * x1;
      x0 = X[4]; x1 = X[5]; X += 6;
      do
      {
         ATL_pfl1R(X+40);
         ATL_pfl1W(Y+40);
         *Y = y0; Y[1] = y1; Y[2] = y2; Y[3] = y3;
         y0 = Y[4]; y1 = Y[5]; y2 = Y[6]; y3 = Y[7];
         y0 += alpha * x0; x0 = *X;
         y1 += alpha * x1; x1 = X[1];
         y2 += alpha * x0; x0 = X[2];
         y3 += alpha * x1; x1 = X[3];
         X += 4;
         Y += 4;
      }
      while (X != stX);
      *Y = y0; Y[1] = y1; Y[2] = y2; Y[3] = y3;
      y0 = Y[4]; y1 = Y[5]; y2 = Y[6]; y3 = Y[7];
      y0 += alpha * x0; x0 = *X;
      y1 += alpha * x1; x1 = X[1]; X += 2;
      y2 += alpha * x0;
      y3 += alpha * x1;
      Y += 4;
      *Y = y0; Y[1] = y1; Y[2] = y2; Y[3] = y3;
      Y += 4;
   }
   if (X != stX0) do *Y++ += alpha * *X++; while(X != stX0);
}
