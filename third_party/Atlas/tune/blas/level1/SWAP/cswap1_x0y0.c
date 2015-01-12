#include "atlas_misc.h"
void ATL_USWAP(const int N, TYPE *X, const int incx, TYPE *Y, const int incy)
{
   int i;
   const int incX=incx+incx, incY=incy+incy;
   TYPE rtmp, itmp;
   for (i=N; i; i--, X += incX, Y += incY)
   {
      rtmp = *Y;
      itmp = Y[1];
      *Y = *X;
      Y[1] = X[1];
      *X   = rtmp;
      X[1] = itmp;
   }
}
