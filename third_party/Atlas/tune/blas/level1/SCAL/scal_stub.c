#include "atlas_misc.h"
void ATL_USCAL(const int N, const SCALAR alpha, TYPE *X, const int incX);

void ATL_SCAL(const int N, const SCALAR alpha, TYPE *X, const int incX)
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
      ATL_USCAL(N, alpha, X, incx);
   }
}
