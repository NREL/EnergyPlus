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

#include "atlas_prefetch.h"
#ifndef PFDIST
   #ifdef SREAL
      #define PFDIST 112
   #else
      #define PFDIST 56
   #endif
#endif
static void SSQr(const int N, const TYPE *X, const int incX,
                 TYPE *scal0, TYPE *ssq0)
{
   const TYPE *stX, *stX0=X+N;
   TYPE x0, x1, x2, x3, t0, rscal, scal=(*scal0), ssq=(*ssq0);
   int nr;

   if (scal == ATL_rzero) /* need to start ops */
   {
      while(X != stX0 && *X == ATL_rzero) X++;
      if (X != stX0)
      {
         scal = fabs(*X);
         ssq = ATL_rone;
         X++;
      }
      else return;
   }

   rscal = RecipScal(scal);
   if (rscal == ATL_rzero) /* not safe to reciprocate, call non-rec SSQ */
   {
      *scal0 = scal; *ssq0 = ssq;
      SSQ((int)(stX0-X), X, 1, scal0, ssq0);
      return;
   }

   nr = (int) (stX0 - X);
   nr = (nr>>2)<<2;
   stX = X + nr;
   if (nr)
   {
      do
      {
         x0 = fabs(*X); x1 = fabs(X[1]); x2 = fabs(X[2]); x3 = fabs(X[3]);
         if (x0 <= scal && x1 <= scal && x2 <= scal && x3 <= scal)
         {
            x0 *= rscal; x1 *= rscal; x2 *= rscal; x3 *= rscal;
            ATL_pfl1R(X+PFDIST);
            ssq += x0*x0 + x1*x1 + x2*x2 + x3*x3;
            X += 4;
            continue;
         }
         else if (x0 >= x1 && x0 >= x2 && x0 >= x3)
         {
            rscal = RecipScal(x0);
            ATL_pfl1R(X+PFDIST);
            if (rscal != ATL_rzero)
            {
               x1 *= rscal; x2 *= rscal; x3 *= rscal;
               t0 = scal * rscal;
               t0 *= t0;
               ssq = ATL_rone + ssq * t0 + x1*x1 + x2*x2 + x3*x3;
               scal = x0;
            }
            else
            {
               *scal0 = scal; *ssq0 = ssq;
               SSQ((int)(stX0-X), X, 1, scal0, ssq0);
               return;
            }
         }
         else if (x1 >= x2 && x1 >= x3)
         {
            x0 *= rscal;
            ssq += x0 * x0;
            ATL_pfl1R(X+PFDIST);
            rscal = RecipScal(x1);
            if (rscal != ATL_rzero)
            {
               x2 *= rscal; x3 *= rscal;
               t0 = scal * rscal;
               t0 *= t0;
               ssq = ATL_rone + ssq * t0 + x2*x2 + x3*x3;
               scal = x1;
            }
            else
            {
               *scal0 = scal; *ssq0 = ssq;
               SSQ((int)(stX0-X-1), X+1, 1, scal0, ssq0);
               return;
            }
         }
         else if (x2 > x3)
         {
            x0 *= rscal; x1 *= rscal;
            ssq += x0*x0 + x1*x1;
            ATL_pfl1R(X+PFDIST);
            rscal = RecipScal(x2);
            if (rscal != ATL_rzero)
            {
               x3 *= rscal;
               t0 = scal * rscal;
               t0 *= t0;
               ssq = ATL_rone + ssq * t0 + x3*x3;
               scal = x2;
            }
            else
            {
               *scal0 = scal; *ssq0 = ssq;
               SSQ((int)(stX0-X-2), X+2, 1, scal0, ssq0);
               return;
            }
         }
         else
         {
            x0 *= rscal; x1 *= rscal; x2 *= rscal;
            ssq += x0*x0 + x1*x1 + x2*x2;
            ATL_pfl1R(X+PFDIST);
            rscal = RecipScal(x3);
            if (rscal != ATL_rzero)
            {
               t0 = scal * rscal;
               t0 *= t0;
               ssq = ATL_rone + ssq * t0;
               scal = x3;
            }
            else
            {
               *scal0 = scal; *ssq0 = ssq;
               SSQ((int)(stX0-X-3), X+3, 1, scal0, ssq0);
               return;
            }
         }
         X += 4;
      }
      while (X != stX);
   }
   if (X != stX0) SSQ((int)(stX0-X), X, 1, &scal, &ssq);
   *scal0 = scal;
   *ssq0 = ssq;
}
#endif
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
{
   TYPE ssq=ATL_rone, scal=ATL_rzero;
   if (N > 1) SSQr(N, X, incX, &scal, &ssq);
   else if (N == 1) return(fabs(*X));
   return(scal * sqrt(ssq));
}
