#include "atlas_misc.h"
void ATL_UDOT(const int N, const TYPE *X, const int incx,
              const TYPE *Y, const int incy, SCALAR dot)
{
   register TYPE rx, ix, ry, iy, rdot=ATL_rzero, idot=ATL_rzero;
   const int incX=incx+incx, incY=incy+incy;
   int i;
   for (i=N; i; i--, X += incX, Y += incY)
   {
      rx = *X; ix = X[1];
      ry = *Y; iy = Y[1];
      #ifndef Conj_
         rdot += rx*ry - ix*iy;
         idot += rx*iy + ix*ry;
      #else
         rdot += rx*ry + ix*iy;
         idot += rx*iy - ix*ry;
      #endif
   }
   dot[0] = rdot;
   dot[1] = idot;
}
