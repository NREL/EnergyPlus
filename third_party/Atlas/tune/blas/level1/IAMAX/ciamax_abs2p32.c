#include "atlas_misc.h"
#ifdef ATL_AltiVec
   #define ATL_NoFakePF
#endif
#include "atlas_prefetch.h"
#include <math.h>
int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   const int incX2 = incX+incX;
   register TYPE xr, xi, yr, yi, xmax;
   int imax=0;
   const TYPE *stX = X + N+N, *xp=X, *x;
   #ifdef ATL_AltiVec
      int cwrd = ATL_MulBySize(N)>>4;
   #endif

   if (N > 0)
   {
      #ifdef ATL_AltiVec
         if (cwrd >= 64) cwrd = ATL_GetCtrl(512, (cwrd+31)>>5, 0);
         else cwrd = ATL_GetCtrl(64, (cwrd+3)>>2, 4);
         ATL_pfavR(X, cwrd, 0);
      #endif
      xr = *X;
      xi = X[1];
      xmax = fabs(xr) + fabs(xi);
      if ((N>>1)<<1 == N)
      {
         xr = X[2]; xi = X[3];
         xr = fabs(xr) + fabs(xi);
         if (xr > xmax) { xmax = xr; xp = X + 2; }
         x = X + 4;
      }
      else x = X + 2;
   }
   if (N > 2)
   {
      do
      {
         ATL_pfl1R(x + 32);
         xr = *x; xi = x[1]; yr = x[2]; yi = x[3];  x += 4;
         xr = fabs(xr) + fabs(xi); yr = fabs(yr) + fabs(yi);
         if (xmax >= xr && xmax >= yr) continue;
         else if (xr >= yr) { xmax = xr; xp = x - 4; }
         else { xmax = yr; xp = x - 2; }
      }
      while(x != stX);
   }
   imax = (int) (xp - X);
   return(imax>>1);
}
