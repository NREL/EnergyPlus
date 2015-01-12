#include "atlas_misc.h"
void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int i;
   const int incx=incX+incX, incy=incY+incY;
   const register TYPE ra=(*alpha), ia=alpha[1];
   register TYPE rx, ix;

   for (i=N; i; i--, X += incx, Y += incy)
   {
      rx = X[0]; ix = X[1];
      *Y   += ra*rx - ia*ix;
      Y[1] += ra*ix + ia*rx;
   }
}
