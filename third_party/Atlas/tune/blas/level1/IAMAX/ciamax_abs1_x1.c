#include "atlas_misc.h"
#include <math.h>

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   const int incX2 = incX<<1;
   register TYPE xr, xi, xmax=0.0;
   int i, imax=0;

   for (i=0; i < N; i++)
   {
      xr = *X; xi = X[1]; X += 2;
      xr = fabs(xr) + fabs(xi);
      if (xmax >= xr) continue;
      else { xmax = xr; imax = i; }
   }
   return(imax);
}
