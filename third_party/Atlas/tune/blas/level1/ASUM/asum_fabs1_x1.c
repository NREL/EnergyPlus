#include "atlas_misc.h"
#include "math.h"
#define myabs fabs
TYPE ATL_UASUM(const int N, const TYPE *X, const int incX)
{
   int i;
   register TYPE t0=ATL_rzero;
   for (i=0; i < N; i++) t0 += myabs(X[i]);
   return(t0);
}
