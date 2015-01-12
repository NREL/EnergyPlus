#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>
#define myabs fabs
TYPE ATL_UASUM(const int N, const TYPE *X, const int incX)
{
   int n;
   register TYPE t0=ATL_rzero, t1=ATL_rzero, t2=ATL_rzero, t3=ATL_rzero;
   const TYPE *stX, *stX0 = X+N;

   n = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(4));
   if (n)  /* not aligned */
   {
      stX = X + n;
      do t0 += myabs(*X); while(++X != stX);
   }
   n = N - n;

   stX = X + ((n>>2)<<2);
   if (X != stX)
   {
      do
      {
          ATL_pfl1R(X+120);
          t0 += myabs(*X);
          t1 += myabs(X[1]);
          t2 += myabs(X[2]);
          t3 += myabs(X[3]);
          X += 4;
      }
      while (X != stX);
      t0 += t1;
      t2 += t3;
      t0 += t2;
   }
   if (X != stX0)
   {
      do t0 += myabs(*X); while(++X != stX0);
   }
   return(t0);
}
