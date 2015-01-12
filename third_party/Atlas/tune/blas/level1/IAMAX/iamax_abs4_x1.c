#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax, x0, x1, x2, x3;
   const TYPE *stX=X+N, *x, *xp=X;
   int nr;
   if (N > 0)
   {
      xmax = *X;
      xmax = fabs(xmax);
      nr = N-1;
      nr = nr - ((nr>>2)<<2);
      if (nr)
      {
         x0 = X[1];
         x0 = fabs(x0);
         if (x0 > xmax) { xmax = x0; xp++; }
         if (nr != 1)
         {
            x0 = X[2];
            x0 = fabs(x0);
            if (x0 > xmax) { xmax = x0; xp = X + 2; }
            x = X + 3;
            if (nr == 3)
            {
               x0 = X[3];
               x0 = fabs(x0);
               if (x0 > xmax) { xmax = x0; xp = X + 3; }
               x = X + 4;
            }
         }
         else x = X + 2;
      }
      else x = X + 1;
      if (N > 4)
      {
         do
         {
         ATL_pfl1R(x+12);
            x0 = *x; x1 = x[1]; x2 = x[2]; x3 = x[3]; x += 4;
            x0 = fabs(x0); x1 = fabs(x1); x2 = fabs(x2); x3 = fabs(x3);
            if (xmax >= x0 && xmax >= x1 && xmax >= x2 && xmax >= x3) continue;
            else if (x0 >= x1 && x0 >= x2 && x0 >= x3)
            {
               xmax = x0;
               xp = x - 4;
            }
            else if (x1 >= x2 && x1 >= x3)
            {
               xmax = x1;
               xp = x - 3;
            }
            else if (x2 >= x3)
            {
               xmax = x2;
               xp = x - 2;
            }
            else
            {
               xmax = x3;
               xp = x - 1;
            }
         }
         while (x != stX);
      }
   }
   return((int)(xp-X));
}
