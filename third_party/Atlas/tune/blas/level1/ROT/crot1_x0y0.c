#include "atlas_misc.h"
void ATL_UROT(const int N, TYPE *X, const int incx, TYPE *Y, const int incy,
              const TYPE c0, const TYPE s0)
/*
 * rot, no unrolling, arbitrary incX, incY, S & C
 */
{
   int i;
   const int incX = incx+incx, incY = incy+incy;
   const register TYPE c = c0, s = s0;
   register TYPE rx, ix, ry, iy;

   for (i=N; i; i--, Y += incY, X += incX)
   {
      rx = *X;  ix = X[1];
      ry = *Y;  iy = Y[1];
      *X   = c * rx + s * ry;
      X[1] = c * ix + s * iy;
      *Y   = c * ry - s * rx;
      Y[1] = c * iy - s * ix;
   }
}
