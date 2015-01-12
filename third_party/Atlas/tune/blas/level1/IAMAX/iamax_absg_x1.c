#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax, x0;
   const TYPE *x=X+1, *stX = X + N, *mptr=X+1;
   int i, iret=0;
   if (N > 0)
   {
      xmax = *X;
      xmax = fabs(xmax);
      if (x != stX)
      {
XLOOP:
            ATL_pfl1R(x+36);
            x0 = *x++;
            x0 = fabs(x0);
            if (x0 > xmax) goto NEWMAX;
         if (x != stX) goto XLOOP;
      }
   }
   return((int)(mptr-X)-1);
NEWMAX:
   xmax = x0;
   mptr = x;
   if (x != stX) goto XLOOP;
   return((int)(mptr-X)-1);
}
