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
#include <float.h>
#if FLT_RADIX != 2
   #define SSQr SSQ
#else  /* SSQr depends on using power of 2 storage */

   #ifdef SREAL
      #define ATL_MIN_EXP FLT_MIN_EXP
      #define ATL_MAX_EXP FLT_MAX_EXP
   #else
      #define ATL_MIN_EXP DBL_MIN_EXP
      #define ATL_MAX_EXP DBL_MAX_EXP
   #endif
static TYPE RecipScal(TYPE scal)
/*
 * We guarantee this function never called with scal of 0, so returning
 * zero indicates it is not safe to recipricate the number scal.  Otherwise,
 * the function returns 1 / scal
 */
{
/*
 * Use smallest max exponent so that it can be reciprocated in both directions
 */
   static const int maxexp = Mmin(ATL_MAX_EXP, -ATL_MIN_EXP);
   TYPE mant, rscal=ATL_rzero;
   int iexp, j;

   mant = frexp(scal, &iexp);
   mant = 0.5 / mant;
   mant = frexp(mant, &j);
   iexp = 1 + j - iexp;
   if (Mabs(iexp) < maxexp) rscal = ldexp(mant, iexp);
   return(rscal);
}

static void SSQr(const int N, const TYPE *X, const int incX,
                 TYPE *scal0, TYPE *ssq0)
{
   TYPE t0, ax, ssq=(*ssq0), scal=(*scal0), rscal;
   int i, iexp;

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

   rscal = RecipScal(scal);
   if (rscal == ATL_rzero) /* not safe to reciprocate, call non-rec SSQ */
   {
      *scal0 = scal;
      *ssq0 = ssq;
      SSQ(N-i, X+i, 1, scal0, ssq0);
      return;
   }

   for (; i < N; i++)
   {
      ax = fabs(X[i]);
      if (scal >= ax)
      {
         t0 = ax * rscal;
         ssq += t0*t0;
      }
      else  /* getting new scal */
      {
         rscal = RecipScal(ax);
         if (rscal != ATL_rzero)
         {
            t0 = scal * rscal;
            t0 *= t0;
            ssq = ATL_rone + ssq * t0;
            scal = ax;
         }
         else /* need to use non-rec SSQ */
         {
            *scal0 = scal;
            *ssq0 = ssq;
            SSQ(N-i, X+i, incX, scal0, ssq0);
            return;
         }
      }
   }
   *ssq0 = ssq;
   *scal0 = scal;
}
#endif
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
{
   TYPE ssq=ATL_rone, scal=ATL_rzero;
   if (N > 1) SSQr(N, X, incX, &scal, &ssq);
   else if (N == 1) return(fabs(*X));
   return(scal * sqrt(ssq));
}
