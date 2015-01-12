#include "atlas_misc.h"
#include "atlas_prefetch.h"

void ATL_USCAL(const int N, const SCALAR alpha0, TYPE *X, const int incX)
{
   int n;
   TYPE *stX, *stX0 = X+N;
   const register TYPE alpha=alpha0;

   n = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(4));
   if (n)  /* not aligned */
   {
      stX = X + n;
      do *X++ *= alpha; while (X != stX);
   }
   n = N - n;

   stX = X + ((n>>2)<<2);
   if (X != stX)
   {
      do
      {
         ATL_pfl1W(X+48);
         *X *= alpha;
         X[1] *= alpha;
         X[2] *= alpha;
         X[3] *= alpha;
         X += 4;
      }
      while(X != stX);
   }
   if (X != stX0) do *X++ *= alpha; while (X != stX0);
}
