#include "atlas_misc.h"
#include <math.h>

#undef Mabs
/* #define Mabs(x) ( ((x) >= 0.0) ? (x) : -(x) ) */
#define Mabs fabs
int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax, x0, x1;
   const TYPE *stX=X+N, *x, *xp=X;
   if (N > 0)
   {
      xmax = *X;
      xmax = Mabs(xmax);
      if ((N>>1)<<1 == N)
      {
         x0 = X[1];
         x0 = Mabs(x0);
         if (x0 > xmax) { xmax = x0; xp++; };
         x = X + 2;
      }
      else x = X + 1;
      if (N > 2)
      {
         do
         {
            x0 = *x; x1 = x[1]; x += 2;
            x0 = Mabs(x0); x1 = Mabs(x1);
            if (xmax >= x0 && xmax >= x1) continue;
            else if (x0 >= x1) { xmax = x0; xp = x - 2; }
            else { xmax = x1; xp = x - 1; }
         }
         while (x != stX);
      }
   }
   return((int)(xp-X));
}
