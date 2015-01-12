#include "atlas_misc.h"
#include <math.h>
static void SSQ(const int N, const TYPE *X, const int incX,
                TYPE *scal0, TYPE *ssq0)
{
   TYPE t0, ax, ssq=(*ssq0), scal=(*scal0);
   const TYPE *stX = X + N*incX;

   if (scal == ATL_rzero) /* need to start ops */
   {
      while (X != stX && *X == ATL_rzero) X += incX;
      if (X != stX)
      {
         scal = fabs(*X);
         ssq = ATL_rone;
         X += incX;
      }
      else return;
   }

   if (X != stX)
   {
      do
      {
         ax = fabs(*X);
         X += incX;
         if (scal >= ax)
         {
            t0 = ax / scal;
            ssq += t0*t0;
         }
         else
         {
            t0 = scal / ax;
            t0 *= t0;
            ssq = ATL_rone + ssq * t0;
            scal = ax;
         }
      }
      while (X != stX);
   }
   *ssq0 = ssq;
   *scal0 = scal;
}
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
{
   TYPE ssq=ATL_rone, scal=ATL_rzero;
   if (N > 1) SSQ(N, X, incX, &scal, &ssq);
   else if (N == 1) return(fabs(*X));
   return(scal * sqrt(ssq));
}
