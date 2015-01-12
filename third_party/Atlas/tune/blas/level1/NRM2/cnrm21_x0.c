#include "atlas_misc.h"
#include <math.h>
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incx)
{
   const int incX=incx+incx;
   register TYPE t0=ATL_rzero;
   int i;

   for (i=N; i; i--, X += incX) t0 += *X * *X + X[1]*X[1];
   t0 = sqrt(t0);
   return(t0);
}
