#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>

#define fabs fabs
int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax=0, x0, x1;
   const TYPE *stX=X+N, *x, *xp=X, *xn;
   int nr;

   if (N > 0)
   {
      nr = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(4));
      if (nr) /* not aligned */
      {
         xmax = fabs(*X);
         x = X + 1;
         if (nr > 1)
         {
            stX = X + nr;
            do
            {
               x0 = *x;
               x0 = fabs(x0);
               if (x0 > xmax) { xmax = x0; xp = x; }
            }
            while(++x != stX);
         }
         nr = N - nr;
      }
      else { x = X; nr = N; }

      if (nr > 4)
      {
         stX = x + ((nr>>2)<<2);
         do
         {
            ATL_pfl1R((x+24));
            x0 = *x; x1 = x[1];
            x0 = fabs(x0); x1 = fabs(x1);
            if (xmax >= x0 && xmax >= x1) goto L1;
            else if (x0 >= x1) { xmax = x0; xp = x; }
            else { xmax = x1; xp = x + 1; }

L1:         x0 = x[2]; x1 = x[3];
            x0 = fabs(x0); x1 = fabs(x1);
            if (xmax >= x0 && xmax >= x1) goto L2;
            else if (x0 >= x1) { xmax = x0; xp = x + 2; }
            else { xmax = x1; xp = x + 3; }
L2:         x += 4;
         }
         while (x != stX);
         nr -= (nr>>2)<<2;
      }
      if (nr)
      {
         stX = x + nr;
         do
         {
            x0 = *x;
            x0 = fabs(x0);
            if (xmax >= x0) continue;
            else { xmax = x0; xp = x; }
         }
         while(++x != stX);
      }
   }
   return((int)(xp-X));
}
