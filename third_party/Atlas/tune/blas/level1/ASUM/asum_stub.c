#include "atlas_misc.h"
TYPE ATL_UASUM(const int N, const TYPE *X, const int incX);

TYPE ATL_ASUM(const int N, const TYPE *X, const int incX)
{
   int incx;
   if (N > 0)
   {
      if (incX > 0) incx = incX;
      else if (incX < 0)
      {
         X += ((N-1)SHIFT) * incX;
         incx = -incX;
      }
      else return(ATL_rzero);
      return(ATL_UASUM(N, X, incx));
   }
   return(ATL_rzero);
}
