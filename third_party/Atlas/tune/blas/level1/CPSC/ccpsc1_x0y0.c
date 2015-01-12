#include "atlas_misc.h"
void ATL_UCPSC(const int N, const SCALAR alpha, const TYPE *X, const int incx,
               TYPE *Y, const int incy)
{
   const int incX=incx+incx, incY=incy+incy;
   const register TYPE ra=(*alpha), ia=alpha[1];
   register TYPE rx, ix;
   int i;
   for (i=N; i; i--, X += incX, Y += incY)
   {
      rx = *X; ix = X[1];
      *Y   = ra*rx - ia*ix;
      Y[1] = ra*ix + ia*rx;
   }
}
