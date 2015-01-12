#include "atlas_misc.h"
#include "math.h"
#define myabs fabs
TYPE ATL_UASUM(const int N, const TYPE *X, const int incx)
{
   const int incX = incx+incx;
   int i;
   register TYPE t0=ATL_rzero;
   for (i=N; i; i--, X += incX) t0 += myabs(*X) + myabs(X[1]);
   return(t0);
}
