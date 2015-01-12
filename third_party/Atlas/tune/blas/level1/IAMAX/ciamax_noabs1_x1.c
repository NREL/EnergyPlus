#include "atlas_misc.h"

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   int i, imax=N;
   const int incx = incX<<1;
   register TYPE pmax=0, nmax=0, x0, x1, tmp;
   if (N > 1)
   {
      for(i=N; i; i--, X += 2)
      {
         x0 = *X;
         tmp = X[1];
         x1 = x0 - tmp;
         x0 += tmp;

         if (x0 >= x1)
         {
            if (x0 <= pmax && x1 >= nmax) continue;
            if (x0 > pmax)
            {
               if (x1 >= nmax) { pmax = x0; nmax = -x0; }
               else if (x0-pmax >= nmax-x1) { pmax = x0; nmax = -x0; }
               else { pmax = -x1; nmax = x1; }
            }
            else { pmax = -x1; nmax = x1; }
         }
         else
         {
            if (x1 <= pmax && x0 >= nmax) continue;
            if (x1 > pmax)
            {
               if (x0 >= nmax) { pmax = x1; nmax = -x1; }
               else if (x1-pmax >= nmax-x0) { pmax = x1; nmax = -x1; }
               else { pmax = -x0; nmax = x0; }
            }
            else { pmax = -x0; nmax = x0; }
         }
         imax = i;
      }
   }
   return(N-imax);
}
