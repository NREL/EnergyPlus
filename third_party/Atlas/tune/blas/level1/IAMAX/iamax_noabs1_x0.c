#include "atlas_misc.h"

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   int i, imax=N;
   register TYPE pmax=0.0, nmax=0.0, x0;

   if (N < 2) return(0);
   for(i=N; i; i--, X += incX)
   {
      x0 = *X;
      if (x0 <= pmax && x0 >= nmax) continue;
      if (x0 > pmax)
      {
         nmax = -x0;
         pmax =  x0;
         imax = i;
      }
      else   /* if (x0 < nmax) */
      {
         nmax =  x0;
         pmax = -x0;
         imax = i;
      }
   }
   return(N-imax);
}
