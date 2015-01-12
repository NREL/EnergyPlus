#include "atlas_misc.h"
void ATL_UCPSC(const int N, const SCALAR alpha, const TYPE *X, const int incX,
               TYPE *Y, const int incY);

void ATL_CPSC(const int N, const SCALAR alpha, const TYPE *X, const int incX,
              TYPE *Y, const int incY)
{
   int incx=incX, incy=incY;

   if (!SCALAR_IS_ZERO(alpha) && N > 0)
   {
/*
 *    Manipulate incX and inxY such that:
 *    -  Both are positive
 *    -  else if incX or incY has abs()=1, make it positive
 *    -  if both abs(inc) are 1, or if neither, make incY positive
 */
      if (incX > 0 && incY > 0) goto L1;
      else if (incY < 0)
      {
         if (incX < 0) /* make both positive */
         {
            incx = -incX;
            incy = -incY;
            X += ((N-1)SHIFT)*incX;
            Y += ((N-1)SHIFT)*incY;
         }
         else if (incX != 1 || incY == -1)
         {
            incy = -incY;
            Y += ((N-1)SHIFT)*incY;
            incx = -incX;
            X += ((N-1)SHIFT)*incX;
         }
      }
      else if (incX == -1 && incY != 1)
      {
         incx = 1;
         X -= ((N-1)SHIFT);
         incy = -incY;
         Y += ((N-1)SHIFT)*incY;
      }
      else if (!incX || !incY) return;
L1:
      ATL_UCPSC(N, alpha, X, incx, Y, incy);
   }
}
