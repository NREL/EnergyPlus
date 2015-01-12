#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax, x0, x1, x2;
   const TYPE *stX=X+N, *x, *xp=X;
   int nr;
   #ifdef ATL_AltiVec
      int cwrd;
   #endif
   if (N > 0)
   {
      #ifdef ATL_AltiVec
         cwrd = ATL_MulBySize(N)>>4;
         if (cwrd >= 64) cwrd = ATL_GetCtrl(512, (cwrd+31)>>5, 0);
         else cwrd = ATL_GetCtrl(64, (cwrd+3)>>2, 4);
         ATL_pfavR(X, cwrd, 0);
      #endif
      xmax = *X;
      xmax = fabs(xmax);
      nr = N-1;
      nr = nr - (nr/3)*3;
      if (nr)
      {
         x0 = X[1];
         x0 = fabs(x0);
         if (x0 > xmax) { xmax = x0; xp++; }
         if (nr == 2)
         {
            x0 = X[2];
            x0 = fabs(x0);
            if (x0 > xmax) { xmax = x0; xp = X + 2; }
            x = X + 3;
         }
         else x = X + 2;
      }
      else x = X + 1;
      if (N > 3)
      {
         do
         {
            x0 = *x; x1 = x[1]; x2 = x[2]; x += 3;
            x0 = fabs(x0); x1 = fabs(x1); x2 = fabs(x2);
            if (xmax >= x0 && xmax >= x1 && xmax >= x2) continue;
            else if (x0 >= x1 && x0 >= x2) { xmax = x0; xp = x - 3; }
            else if (x1 >= x2) { xmax = x1; xp = x - 2; }
            else { xmax = x2; xp = x - 1; }
         }
         while (x != stX);
      }
   }
   return((int)(xp-X));
}
