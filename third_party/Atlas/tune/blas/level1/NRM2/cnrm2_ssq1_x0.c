#include "atlas_misc.h"
#include <math.h>
static void SSQ(const int N, const TYPE *X, const int incX0,
                TYPE *scal0, TYPE *ssq0)
{
   const int incX = incX0+incX0;
   TYPE t0, ar, ai, ssq=(*ssq0), scal=(*scal0);
   const TYPE *stX = X + N*incX;

   if (scal == ATL_rzero) /* need to start ops */
   {
      while (X != stX && *X == ATL_rzero && X[1] == ATL_rzero) X += incX;
      if (X != stX)
      {
         ar = fabs(*X);
         ai = fabs(X[1]);
         if (ar != ATL_rzero && ai != ATL_rzero)
         {
            if (ar < ai) { t0 = ai; ai = ar; ar = t0; }
            t0 = ai / ar;
            scal = ar;
            ssq = ATL_rone + t0*t0;
         }
         else if (ai == ATL_rzero)
         {
            scal = ar;
            ssq = ATL_rone;
         }
         else
         {
            scal = ai;
            ssq = ATL_rone;
         }
         X += incX;
      }
      else return;
   }

   if (X != stX)
   {
      do
      {
         ar = fabs(*X); ai = fabs(X[1]);
         X += incX;
         if (scal >= ar && scal >= ai)
         {
            ar /= scal;
            ssq += ar*ar;
            ai /= scal;
            ssq += ai*ai;
         }
         else if (ar >= ai)
         {
            t0 = scal / ar;
            t0 *= t0;
            ssq = ATL_rone + ssq * t0;
            scal = ar;
            ai /= ar;
            ssq += ai * ai;
         }
         else
         {
            t0 = scal / ai;
            t0 *= t0;
            ssq = ATL_rone + ssq * t0;
            scal = ai;
            ar /= ai;
            ssq += ar * ar;
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
   SSQ(N, X, incX, &scal, &ssq);
   return(scal * sqrt(ssq));
}
