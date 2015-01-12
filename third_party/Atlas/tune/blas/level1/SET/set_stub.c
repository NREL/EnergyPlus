#include "atlas_misc.h"
void ATL_USET(const int N, const SCALAR alpha, TYPE *X, const int incX);

void ATL_SET(const int N, const SCALAR alpha, TYPE *X, const int incX)
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
      else return;
      ATL_USET(N, alpha, X, incx);
   }
}
