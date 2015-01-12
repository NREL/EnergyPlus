#include "atlas_misc.h"
#include "atlas_prefetch.h"

#define ATL_PFD 24

void ATL_UROT(const int N, TYPE *X, const int incX, TYPE *Y, const int incY,
              const TYPE c, const TYPE s)
/*
 * rot, no unrolling, incX=incY=1, arbitrary S & C
 */
{
   const register TYPE C=c, S=s;
   register TYPE x0, x1, y0, y1;
   TYPE *stX = X + ((N>>2)<<2), *stX0 = X + N;

   if (X != stX)
   {
      do
      {
         x0 = *X;   y0 = *Y;
         x1 = X[1]; y1 = Y[1];
         *X   = C*x0 + S*y0; ATL_pfl1W(X+ATL_PFD); ATL_pfl1W(Y+ATL_PFD);
         *Y   = C*y0 - S*x0; x0 = X[2];
         Y[1] = C*y1 - S*x1; y0 = Y[2];
         X[1] = C*x1 + S*y1; x1 = X[3];
         X[2] = C*x0 + S*y0; y1 = Y[3];
         Y[2] = C*y0 - S*x0;
         X[3] = C*x1 + S*y1;
         Y[3] = C*y1 - S*x1;
         Y += 4;
         X += 4;
      }
      while (X != stX);
   }
   while (X != stX0)
   {
      x0 = *X; y0 = *Y;
      *X++ = C*x0 + S*y0;
      *Y++ = C*y0 - S*x0;
   }
}
