#include "atlas_misc.h"
#include "atlas_level1.h"

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   const int n = (N >> 2)<<2;
   const int nr = N - n;
   const TYPE *x=X, *stX = X + n, *xmax;
   register TYPE pmax=0.0, nmax=0.0, x0, x1, x2, x3;

   if (N < 2) return(0);
   if (n)
   {
      x0 = *x;
      x1 = x[1];
      x2 = x[2];
      x3 = x[3];
      x += 4;
      xmax = x;
      if (n != 4)
      {
         do
         {
            if (x0 <= pmax && x0 >= nmax) goto L10;
            if (x0 > pmax)
            {
               nmax = -x0;
               pmax =  x0;
               xmax = x;
            }
            else /* if (x0 < nmax) */
            {
               pmax = -x0;
               nmax =  x0;
               xmax = x;
            }
L10 :        x0 = *x;
            if (x1 <= pmax && x1 >= nmax) goto L20;
            if (x1 > pmax)
            {
               nmax = -x1;
               pmax =  x1;
               xmax = x+1;
            }
            else /* if (x1 < nmax) */
            {
               pmax = -x1;
               nmax =  x1;
               xmax = x+1;
            }
L20 :        x1 = x[1];
            if (x2 <= pmax && x2 >= nmax) goto L30;
            if (x2 > pmax)
            {
               nmax = -x2;
               pmax =  x2;
               xmax = x+2;
            }
            else /* if (x2 < nmax) */
            {
               pmax = -x2;
               nmax =  x2;
               xmax = x+2;
            }
L30 :        x2 = x[2];
            if (x3 <= pmax && x3 >= nmax) goto L40;
            if (x3 > pmax)
            {
               nmax = -x3;
               pmax =  x3;
               xmax = x+3;
            }
            else /* if (x3 < nmax) */
            {
               pmax = -x3;
               nmax =  x3;
               xmax = x+3;
            }
L40 :        x3 = x[3];
            x += 4;
         }
         while (x != stX);
      }
      if (x0 <= pmax && x0 >= nmax) goto L15;
      if (x0 > pmax)
      {
         nmax = -x0;
         pmax =  x0;
         xmax = x;
      }
      else /* if (x0 < nmax) */
      {
         pmax = -x0;
         nmax =  x0;
         xmax = x;
      }
L15 :
      if (x1 <= pmax && x1 >= nmax) goto L25;
      if (x1 > pmax)
      {
         nmax = -x1;
         pmax =  x1;
         xmax = x+1;
      }
      else /* if (x1 < nmax) */
      {
         pmax = -x1;
         nmax =  x1;
         xmax = x+1;
      }
L25 :
      if (x2 <= pmax && x2 >= nmax) goto L35;
      if (x2 > pmax)
      {
         nmax = -x2;
         pmax =  x2;
         xmax = x+2;
      }
      else /* if (x2 < nmax) */
      {
         pmax = -x2;
         nmax =  x2;
         xmax = x+2;
      }
L35 :
      if (x3 <= pmax && x3 >= nmax) goto L45;
      if (x3 > pmax)
      {
         nmax = -x3;
         pmax =  x3;
         xmax = x+3;
      }
      else /* if (x3 < nmax) */
      {
         pmax = -x3;
         nmax =  x3;
         xmax = x+3;
      }
L45 :
      xmax -= 4;
   }
   else xmax = X+1;
   if (nr)
   {
      stX = x + nr;
      do
      {
         x0 = *x++;
         if (x0 <= pmax && x0 >= nmax) continue;
         if (x0 > pmax)
         {
            nmax = -x0;
            pmax =  x0;
            xmax = x-1;
         }
         else /* if (x0 < nmax) */
         {
            pmax = -x0;
            nmax =  x0;
            xmax = x-1;
         }
      }
      while (x != stX);
   }
   if (pmax == 0.0 || nmax == 0.0) return(0);
   return(xmax-X);
}
