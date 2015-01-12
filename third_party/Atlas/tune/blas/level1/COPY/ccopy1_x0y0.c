#include "atlas_misc.h"
void ATL_UCOPY(const int N, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int i;
   int incy=incY+incY, incx=incX+incX;
   for (i=N; i; i--, X += incx, Y += incy)
   {
      *Y = *X;
      Y[1] = X[1];
   }
}
