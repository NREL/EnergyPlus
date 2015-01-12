#include "atlas_misc.h"
#include "atlas_level1.h"
#ifdef Conj_
void Mjoin(PATL,ger2ck_Nlt8)
#else
void Mjoin(PATL,ger2k_Nlt8)
#endif
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *X0,
    ATL_CINT incX0, const TYPE *Y0, ATL_CINT incY0,
    const SCALAR alpha1, const TYPE *X1, ATL_CINT incX1, const TYPE *Y1,
    ATL_CINT incY1, TYPE *A, ATL_CINT lda)
/*
 * A += alpha0*X0*Y0 + alpha1*X1*Y1;
 * This routine typically called when N is very small, and so we can't afford
 * to copy the vectors even if M is large; in this case we simply address
 * the columns of A one-by-one with simple loops.
 */
{
#ifdef TCPLX
   const TYPE *xp0, *xp1;
   const TYPE al0r = *alpha0, al0i = alpha0[1];
   const TYPE al1r = *alpha1, al1i = alpha1[1];
   ATL_CINT incAn=(lda-M)<<1, incY02=incY0+incY0, incY12=incY1+incY1;
   ATL_CINT incX02=incX0+incX0, incX12=incX1+incX1;
   ATL_INT i, j;
   register TYPE y0r, y0i, y1r, y1i, x0r, x0i, x1r, x1i;

   for (j=0; j < N; j++, A += incAn, Y0 += incY02, Y1 += incY12)
   {
/*
 *    Load values from Y
 */
      #ifdef Conj_
         y0r = *Y0; y0i = -Y0[1];
         y1r = *Y1; y1i = -Y1[1];
      #else
         y0r = *Y0; y0i = Y0[1];
         y1r = *Y1; y1i = Y1[1];
      #endif
/*
 *    Apply alpha to Y registers
 */
      x0r = al0r * y0r - al0i * y0i;
      y0i = al0r * y0i + al0i * y0r;
      y0r = x0r;
      x0r = al1r * y1r - al1i * y1i;
      y1i = al1r * y1i + al1i * y1r;
      y1r = x0r;
      xp0 = X0; xp1 = X1;
/*
 *    Now do A += x0*y0 + x1*y1
 */
      for (i=0; i < M; i++, A += 2, xp0 += incX02, xp1 += incX12)
      {
         x0r = *xp0; x0i = xp0[1];
         x1r = *xp1; x1i = xp1[1];
         *A   += x0r * y0r - x0i * y0i + x1r * y1r - x1i * y1i;
         A[1] += x0r * y0i + x0i * y0r + x1r * y1i + x1i * y1r;
      }
   }
#else
   ATL_INT i, j;
   const TYPE *xp0, *xp1;
   register TYPE y0, y1;
   for (j=0; j < N; j++, A += lda, Y0 += incY0, Y1 += incY1)
   {
      y0 = alpha0 * *Y0;
      y1 = alpha1 * *Y1;
      xp0 = X0;
      xp1 = X1;
      for (i=0; i < M; i++, xp0 += incX0, xp1 += incX1)
         A[i] += *xp0 * y0 + *xp1 * y1;
   }
#endif
}
