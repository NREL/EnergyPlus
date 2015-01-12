#include "atlas_misc.h"
#include "atlas_prefetch.h"

TYPE ATL_UDOT(const int N, const TYPE *X, const int incX,
             const TYPE *Y, const int incY)
{
   register TYPE dot0=ATL_rzero, dot1=ATL_rzero, dot2=ATL_rzero, dot3=ATL_rzero;
   const TYPE *stX, *stX0=X+N;
   int i;
   int cwrd = ATL_MulBySize(N)>>4;

   stX = X + ((N>>2)<<2);
   if (X != stX)
   {
      #ifdef ATL_AltiVec
         if (cwrd >= 64)
         {
            cwrd = (cwrd+31)>>5;
            cwrd = ATL_GetCtrl(512, cwrd <= 255 ? cwrd : 0, 0);
         }
         else cwrd = ATL_GetCtrl(64, (cwrd+3)>>2, 4);
         ATL_pfavR(X, cwrd, 0);
         ATL_pfavR(Y, cwrd, 1);
      #endif
      do
      {
         ATL_pfl1R(X+80);
         dot0 += *X * *Y;
         dot1 += X[1] * Y[1];
         dot2 += X[2] * Y[2];
         dot3 += X[3] * Y[3];
         X += 4;
         Y += 4;
      }
      while (X != stX);
      dot0 += dot1;
      dot2 += dot3;
      dot0 += dot2;
   }
   while (X != stX0) dot0 += *X++ * *Y++;
   return(dot0);
}
