#include "atlas_misc.h"
void ATL_USWAP(const int N, TYPE *X, const int incX, TYPE *Y, const int incY)
{
   int i;
   const int incx=incX<<2, incy=incY<<2;
   TYPE t0, t1, t2, t3;
   TYPE *x0=X, *x1=X+incX, *x2=x1+incX, *x3=x2+incX;
   TYPE *y0=Y, *y1=Y+incY, *y2=y1+incY, *y3=y2+incY;

   for (i=N>>2; i; i--)
   {
      t0 = *y0;
      t1 = *y1;
      t2 = *y2;
      t3 = *y3;
      *y0 = *x0; y0 += incy;
      *y1 = *x1; y1 += incy;
      *y2 = *x2; y2 += incy;
      *y3 = *x3; y3 += incy;
      *x0 = t0;  x0 += incx;
      *x1 = t1;  x1 += incx;
      *x2 = t2;  x2 += incx;
      *x3 = t3;  x3 += incx;
   }
   for (i=N-((N>>2)<<2); i; i--, x0 += incX, y0 += incY)
   {
      t0 = *y0;
      *y0 = *x0;
      *x0 = t0;
   }
}
