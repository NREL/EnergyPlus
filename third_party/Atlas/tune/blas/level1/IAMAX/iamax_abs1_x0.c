#include "atlas_misc.h"
#include <math.h>

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax, x0;
   int i, iret=0;
   if (N > 0)
   {
      xmax = *X;
      xmax = fabs(xmax); X += incX;
      for (i=1; i < N; i++, X += incX)
      {
         x0 = *X;
         x0 = fabs(x0);
         if (x0 <= xmax) continue;
         else
         {
            xmax = x0;
            iret = i;
         }
      }
   }
   return(iret);
}
