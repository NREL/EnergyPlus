#include "atlas_misc.h"
#include "math.h"
static void SSQ(ATL_CINT N, const TYPE *X, ATL_CINT incX,
                TYPE *scal0, TYPE *ssq0)
{
   register TYPE scal = *scal0, ssq = *ssq0, ax;
   register int i, in, n4;
   register __m256d vax;
   const register __m256d vone = {ATL_rone, ATL_rone, ATL_rone, ATL_rone};

/*
 * Align vector to 32-byte boundary
 */
   for (in=0; in < N && (((size_t)(X+in))&0x1F); in++);
   for (i=0; i < in; i++)
   {
      register TYPE t0;
      ax = fabs(X[i]);
      if (scal >= ax)
      {
         t0 = ax / scal;
         ssq += t0 * t0;
      }
      else
      {
         t0 = scal / ax;
         t0 *= t0;
         ssq = ATL_rone + ssq * t0;
         scal = ax;
      }
   }
   n4 = ((N-in)>>3)<<3;
   if (n4)
   {
      register __m256d vscal = {scal, scal, scal, scal};
      register __m256d vssq  = {ssq, ATL_rzero, ATL_rzero, ATL_rzero};
      register __m256d vabs  = {-0.0,-0.0,-0.0,-0.0};
      X += in;
      for (i=0; i < N4; i += 4)
      {
         register __m256d v0, vcmp;
         vax = _mm256_load_pd(X+i);
         vax = _mm256_andnot_pd(vabs, vax);   /* vax = fabs(x) */

         vcmp = _mm256_cmp_pd(vax, vscal,14); /* vcmp = (vax > vscal */
         RESCALE = _mm_movemask_ps(vcmp);
         if (RESCALE) goto LRESCALE;
         v0 = _mm256_div_pd(vax, scal);
         v0 = _mm256_mul_pd(v0, v0);
         vssq = _mm256_add_pd(vssq, v0);
LEOL:;
      }
      X -= in;
   }
   for (i=in+n8; i < N; i++)
   {
      register TYPE t0;
      ax = fabs(X[i]);
      if (scal >= ax)
      {
         t0 = ax / scal;
         ssq += t0 * t0;
      }
      else
      {
         t0 = scal / ax;
         t0 *= t0;
         ssq = ATL_rone + ssq * t0;
         scal = ax;
      }
   }
   *scal0 = scal;
   *ssq0 = ssq;
   return;
LRESCAL:
   {
      TYPE v[4];
      register __m256d v0;
      register TYPE x0, x1, x2, x3, t0;
      TYPE oscal;
      _mm256_store_pd(v, vax);
      x0 = *v; x1 = v[1]; x2 = v[2]; x3 = v[3];
      x0 = Mmax(x0,x1);
      x2 = Mmax(x2,x3);
      x0 = Mmax(x0,x2);
      *v = x0;
      vscal = _mm256_broadcast_sd(v);
      _mm_store_sd(&oscal, (__m128d)vscal);
      t0 = oscal / x0;
      t0 *= t0;
      *v = t0;
      v0 = _mm256_broadcast_sd(v);
      vssq = _mm256_mul_pd(ssq, v0);
      vssq = _mm256_add_pd(ssq, vone);
      goto LEOL;
   }
}

#define GetMax8(ptr_, xmax_) \
{ \
   const TYPE *x = (ptr_); \
   register TYPE r0=fabs(*x),r1=fabs(x[1]),r2=fabs(x[2]),r3=fabs(x[3]), \
                 r4=fabs(x[4]),r5=fabs(x[5]),r6=fabs(x[6]),r7=fabs(x[7]); \
   r0 = Mmax(r0,r1); \
   r2 = Mmax(r2,r3); \
   r4 = Mmax(r4,r5); \
   r6 = Mmax(r6,r7); \
   r0 = Mmax(r0,r2); \
   r4 = Mmax(r4,r6); \
   xmax_ = Mmax(r0,r4); \
}

TYPE ATL_UNRM2(ATL_CINT N, const TYPE *X, ATL_CINT incX)
{
   TYPE ssq=ATL_rzero, scal=ATL_rzero;
/*
 * Find a non-zero initial scale from beginning of vector
 */
   if (N <= 24)
   {
      register int i;
      if (N == 1)
         return(fabs(*X));
      else if (N < 1)
         return(ATL_rzero);
      for (i=0; i < N; i++)
      {
         const register TYPE ax = fabs(X[i]);
         scal = (scal >= ax) ? scal : ax;
      }
      if (scal == ATL_rzero)
         return(ATL_rzero);
      for (i=0; i < N; i++)
      {
         const register TYPE ax = fabs(X[i]);
         register TYPE t0 = ax / scal;
         ssq += t0 * t0;
      }
      return(scal * sqrt(ssq));
   }
   else
   {
      TYPE s2;
      register int i=0;
/*
 *    Find a non-zero scale factor from start of vector
 */
      do
      {
         GetMax8(X+i, scal);
         i += 8;
      }
      while (scal == ATL_rzero && N-i >= 8);
/*
 *    Even with only 0-7 elts left, everything was zero
 */
      if (scal == ATL_rzero)
      {
         const int i0 = i;
         for (; i < N; i++)
         {
            s2 = fabs(X[i]);
            scal = Mmax(scal, s2);
         }
         if (scal == ATL_rzero)
            return(ATL_rzero);
         for (i=i0; i < N; i++)
         {
            const register TYPE ax = fabs(X[i]);
            register TYPE t0 = ax / scal;
            ssq += t0 * t0;
         }
         return(scal * sqrt(ssq));
      }
/*
 *    Scope end of vector to improve scaling bet for structured data
 */
      GetMax8(X+N-8, s2);
      scal = Mmax(scal, s2);
      SSQ(N, X, incX, &scal, &ssq);
      return(scal * sqrt(ssq));
   }
}
