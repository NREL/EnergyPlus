#include "atlas_misc.h"
#include <math.h>
static void SSQ(const int N, const TYPE *X, const int incX,
                TYPE *scal0, TYPE *ssq0)
{
   TYPE t0, ax, ssq=(*ssq0), scal=(*scal0);
   int i;

   if (scal == ATL_rzero) /* need to start ops */
   {
      for (i=0; i < N && X[i] == ATL_rzero; i++);
      if (i < N)
      {
         scal = fabs(X[i]);
         ssq = ATL_rone;
         i++;
      }
      else return;
   }
   else i = 0;
   for (; i < N; i++)
   {
      ax = fabs(X[i]);
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
