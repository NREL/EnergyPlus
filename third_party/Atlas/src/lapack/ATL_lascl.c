#include "atlas_misc.h"
#include "atlas_lapack.h"
#include "atlas_lamch.h"

int Mjoin(PATL,lascl)
(
   const enum ATL_LAMATTYPE mtyp, /* matrix type */
   ATL_CINT KL,                   /* lower bandwidth for banded mat types */
   ATL_CINT KU,                   /* upper bandwidth for banded mat types */
   const TYPE den0,               /* denominator of scaling fraction */
   const TYPE num0,               /* numerator of scaling fraction */
   ATL_CINT M,                    /* matrix rows for non-band matrices */
   ATL_CINT N,                    /* matrix cols for non-band matrices */
   TYPE *A,                       /* matrix to safely scale by fraction */
   ATL_CINT lda
)
/*
 * Safely scales the matrix A by the fraction (num0/den0)
 * This routine tested to produce same ansers as _lascl for overflow, underflow,
 * and normal fractions for all matrix types except banded (not implemented)
 */
{
   TYPE bignum, num, den, anum, aden, aden0, anum0, mul, mul2;
   #ifdef TCPLX
      TYPE cmul[2] = {ATL_rzero, ATL_rzero};
      TYPE cmul2[2] = {ATL_rzero, ATL_rzero};
      ATL_CINT lda2 = lda+lda;
   #else
      #define cmul mul
      #define cmul2 mul2
      #define lda2 lda
   #endif
   ATL_INT i, j, k1, k2, MN;

   if (den0 == 0.0 || den0 != den0)  /* illegal to have 0 or NaN denom */
      return(-4);
   if (num0 != num0)                 /* NaN not allowed for numerator */
      return(-5);
   switch (mtyp)
   {
   case LAMATG:
   case LAMATL:
   case LAMATU:
   case LAMATH:
      if (!N || !M)
         return(0);
      if (M < 0)
         return(-6);
      if (N < 0)
         return(-7);
      if (lda < M)
         return(-9);
      break;
   case LAMATB:  /* ATLAS does not provide any banded lapack routs */
   case LAMATQ:  /* so I've got no way to debug these cases */
   case LAMATZ:  /* so just don't try */
      ATL_assert(0);
#if 0
   case LAMATB:
      if (!N || !KL)
         return(0);
      if (N < 0)
         return(-7);
      if (KL < 0)
         return(-2);
      break;
   case LAMATQ:
      if (!N || !KU)
         return(0);
      if (N < 0)
         return(-7);
      if (KU < 0)
         return(-3);
      break;
   case LAMATZ:
      if (M < 0)
         return(-6);
      if (N < 0)
         return(-7);
      if (KL < 0)
         return(-2);
      if (KU < 0)
         return(-3);
      break;
#endif
   default:
      return(-1);
   }

   bignum = ATL_rone / ATL_laSAFMIN;
   den = den0 * ATL_laSAFMIN;
/*
 * If multiplying by smallest invertable # doesn't change denom, it is
 * is infinity or NaN, and error check ruled out NaN
 */
   if (den == den0)
      mul = num0 / den0;
/*
 * If not denom not infinity, keep checking for safe scaling
 */
   else
   {
      num = num0 / bignum;  /* should be tiny number */
      anum = Mabs(num);
      aden = Mabs(den);
      anum0 = Mabs(num0);
      aden0 = Mabs(den0);
/*
 *    If multiplying by smallest invertable # doesn't change numerator,
 *    it is NaN or infinity, and NaN ruled out by error check, just
 *    scale everything by infinity (LAPACK does this, so so do I)
 */
      if (num == num0)
          mul = num0;
/*
 *    If the denom scaled as small as is safe is still larger than than
 *    a non-zero numerator, we cannot safely compute num / den (underflow),
 *    and so must apply them in special way: we first scale A by smallest
 *    invertable number, and then scale A by num0 / (den0*ATL_laSAFMIN).
 *
 */
      else if (aden > anum0 && num0 != ATL_rzero)
      {
         mul = ATL_laSAFMIN;
         mul2 = num0 / den;
         goto DOUBLE_SCALE;
      }
/*
 *    If numerator scaled as small as is safe is still larger than denominator
 *    then we cannot safely compute num/den due to overflow, so must apply
 *    separately
 */
      else if (anum > aden0)
      {
         mul = bignum;
         mul2 = num * bignum;
         goto DOUBLE_SCALE;
      }
      else
         mul = num0 / den0;
   }
/*
 * If we reach here, just need to scale matrix by mul and we are done!
 */
   #ifdef TCPLX
      cmul[0] = mul;
   #endif
   switch (mtyp)
   {
   case LAMATG:
      Mjoin(PATLU,gescal)(M SHIFT, N, mul, A, lda SHIFT);
      break;
   case LAMATL:
      Mjoin(PATL,trscal)(AtlasLower, M, N, cmul, A, lda);
      break;
   case LAMATU:
      Mjoin(PATL,trscal)(AtlasUpper, M, N, cmul, A, lda);
      break;
   case LAMATH:  /* Upper hessenburg matrix */
/*
 *    I'm lazy, so just scale hessenburg as triangle, then do vector scale
 *    on subdiagonal (take extra TLB misses for subdiagonal scale)
 */
      Mjoin(PATL,trscal)(AtlasUpper, M, N, cmul, A, lda);
      MN = (M > N) ? N : M-1;
      Mjoin(PATL,scal)(MN, cmul, A+(1 SHIFT), lda+1);
      break;
#if 0
   case LAMATB:  /* symmetric band mat wt only lower KL entries stored */
      k1 = KL+1;
      for (j=0; j < N; j++, A += lda)
      {
         k2 = N-j+1;
         k2 = (k2 < k1) ? k2 : k1;
         Mjoin(PATL,scal)(k2, cmul, A, 1);
      }
      break;
   case LAMATQ:  /* symmetric band mat wt only upper KU entries stored */
      k2 = KU + 1;  /* number of diagonals */
      for (A+=lda, j=1; j < N; j++, A += lda)
      {
         k1 = k2 - j;
         Mjoin(PATL,scal)(k2-k1, cmul, A+(k1 SHIFT), 1);
      }
      break;
   case LAMATZ:  /* band mat wt KL lower bandwidth and KU upper */
      k1 = kl + ku + 1;  /* number of diagonals */
      for (j=0; j < N; j++, A += lda)  /* loop over matrix order */
      {
      }
#else
   case LAMATB:  /* symmetric band mat wt only lower KL entries stored */
   case LAMATQ:  /* symmetric band mat wt only upper KU entries stored */
   case LAMATZ:  /* band mat wt KL lower bandwidth and KU upper */
      exit(-1);
#endif
   }
   return(0);
/*
 * This code scales matrix by mul first, and then by mul2, and is used
 * when num0/den0 cannot be formed without over-/under-flow.
 * I call _SCAL so that complex/real are the same.  Two calls to SCAL
 * transforms register reuse into cache reuse (assuming one col can fit
 * in the cache), but I don't want to have to write all the cases in full
 * loops for both precisions, especially since these cases are extremely
 * rare.  Even when it happens, this should be much faster than what
 * lapack does, which is stream entire matrix through twice (only getting
 * cache reuse when entire MxN matrix fits in cache)
 */
DOUBLE_SCALE:
   #ifdef TCPLX
      cmul[0] = mul;
      cmul2[0] = mul2;
   #endif
   switch (mtyp)
   {
   case LAMATG:
      for (j=0; j < N; j++, A += lda2)
      {
         Mjoin(PATL,scal)(M, cmul, A, 1);
         Mjoin(PATL,scal)(M, cmul2, A, 1);
      }
      break;
   case LAMATL:
      MN = Mmin(M,N);
      for (j=0; j < MN; j++, A += lda2)
      {
         Mjoin(PATL,scal)(M-j, cmul, A+(j SHIFT), 1);
         Mjoin(PATL,scal)(M-j, cmul2, A+(j SHIFT), 1);
      }
      break;
   case LAMATU:
   case LAMATH:
      MN = Mmin(M,N);
      for (j=0; j < MN; j++, A += lda2)
      {
         Mjoin(PATL,scal)(j+1, cmul, A, 1);
         Mjoin(PATL,scal)(j+1, cmul2, A, 1);
      }
      for (j=MN; j < N; j++, A += lda2)
      {
         Mjoin(PATL,scal)(M, cmul, A, 1);
         Mjoin(PATL,scal)(M, cmul2, A, 1);
      }
      if (mtyp == LAMATH)  /* need to handle extra subdiagonal */
      {
         MN = (M > N) ? N : M-1;
         A -= (N * lda - 1)SHIFT;
         Mjoin(PATL,scal)(MN, cmul, A, lda+1);
         Mjoin(PATL,scal)(MN, cmul2, A, lda+1);
      }
      break;
   }
   return(0);
}
#ifndef TCPLX
   #undef cmul
   #undef cmul2
   #undef lda2
#endif
