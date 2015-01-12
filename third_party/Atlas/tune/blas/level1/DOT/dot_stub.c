#include "atlas_misc.h"
#ifdef TREAL
   TYPE ATL_UDOT(const int N, const TYPE *X, const int incX,
                const TYPE *Y, const int incY);
#else
   void ATL_UDOT(const int N, const TYPE *X, const int incX,
                 const TYPE *Y, const int incY, SCALAR dot);
#endif

#ifdef TREAL
   TYPE ATL_DOT(const int N, const TYPE *X, const int incX,
                const TYPE *Y, const int incY)
#else
   void ATL_DOT(const int N, const TYPE *X, const int incX,
                const TYPE *Y, const int incY, SCALAR dot)
#endif
{
   #ifdef TREAL
      TYPE dot=ATL_rzero;
   #endif
   int incx=incX, incy=incY;

   if (N > 0)
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
   #ifdef TREAL
      else if (!incX || !incY) return(0.0);
   #else
      else if (!incX || !incY) {*dot = dot[1] = 0.0; return;}
   #endif
L1:
      #ifdef TREAL
         dot = ATL_UDOT(N, X, incx, Y, incy);
      #else
         ATL_UDOT(N, X, incx, Y, incy, dot);
      #endif
   }
   #ifdef TREAL
      return(dot);
   #endif
}
