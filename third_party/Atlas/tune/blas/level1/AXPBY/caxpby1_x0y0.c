#include "atlas_misc.h"
void ATL_UAXPBY(const int N, const SCALAR alpha, const TYPE *X, const int incx,
                const SCALAR beta, TYPE *Y, const int incy)
{
   int i;
   const int incX=incx+incx, incY=incy+incy;
   const register TYPE ra=(*alpha), ia=alpha[1], rb=(*beta), ib=beta[1];
   register TYPE rx, ix, ry, iy;
   for (i=N; i; i--, X += incX, Y += incY)
   {
      rx = *X; ix = X[1]; ry = *Y; iy = Y[1];
      *Y   = ra*rx - ia*ix + rb*ry - ib*iy;
      Y[1] = ra*ix + ia*rx + rb*iy + ib*ry;
   }
}
