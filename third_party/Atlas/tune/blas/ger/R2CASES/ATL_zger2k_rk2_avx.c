#include "atlas_misc.h"
#include "atlas_prefetch.h"
#ifndef ATL_AVX
   #error "This routine requires AVX!"
#endif
#include <immintrin.h>
#include <xmmintrin.h>
#include <pmmintrin.h>

#if defined(__GNUC__) || \
    (defined(__STDC_VERSION__) && (__STDC_VERSION__/100 >= 1999))
   #define ATL_SINLINE static inline
#else
   #define ATL_SINLINE static
#endif
/*
 * Subtract off x0 & x1 contribution to all remaining equations using a
 * rank-2 update with mu=2, nu=3, ku=2.  This version is for 16 AVX regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable vectorization & software pipelining of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk2(ATL_CINT M, const TYPE *pA0, const TYPE *pA1,
                         const TYPE *pB0, const TYPE *pB1,
                         TYPE *C, ATL_CINT ldc0)
{
   ATL_CINT ldc=ldc0+ldc0;
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1);
   ATL_CINT M1 = (M&1), MM =  (M & 2) ? M-2-M1 : M-4-M1;
   int i;
   register __m256d rB00, iB00, rB01, iB01, rB02, iB02;
   register __m256d C00, C01, C02;
   register __m256d C20, C21, C22;
   register __m256d A, a;

   rB00 = _mm256_set_pd(*pB1, *pB0, *pB1, *pB0);
                                                /* rB10 rB00 rB10 rB00 */
   iB00 = _mm256_set_pd(pB1[1], pB0[1], pB1[1], pB0[1]);
                                                /* iB10 iB00 iB10 iB00 */
   rB01 = _mm256_set_pd(pB1[2], pB0[2], pB1[2], pB0[2]);
                                                /* rB11 rB01 rB11 rB01 */
   iB01 = _mm256_set_pd(pB1[3], pB0[3], pB1[3], pB0[3]);
                                                /* iB11 iB01 iB11 iB01 */
   rB02 = _mm256_set_pd(pB1[4], pB0[4], pB1[4], pB0[4]);
                                                /* rB12 rB02 rB12 rB02 */
   iB02 = _mm256_set_pd(pB1[5], pB0[5], pB1[5], pB0[5]);
                                                /* iB12 iB02 iB12 iB02 */
   C00  = _mm256_load_pd(pC0);                  /* iC10 rC10 iC00 rC00 */
   C01  = _mm256_load_pd(pC1);                  /* iC11 rC11 iC01 rC01 */
   C02  = _mm256_load_pd(pC2);                  /* iC12 rC12 iC02 rC02 */

   A    = _mm256_load_pd(pA0);                  /* iA10 rA10 iA00 rA00 */
   a    = _mm256_shuffle_pd(A, A, 0x5);         /* rA10 iA10 rA00 iA00 */

   for (i=0; i < MM; i += 4, pA0 += 8, pA1 += 8, pC0 += 8, pC1 += 8, pC2 += 8)
   {                                     /* rB00 = rB10 rB00 rB10 rB00 */
                                         /* iB00 = iB10 iB00 iB10 iB00 */
      register __m256d m, b;
/*
 *    Do M=K=0 calcs
 */
      b = _mm256_unpacklo_pd(rB00, rB00);       /* rB00 rB00 rB00 rB00 */
      m    = _mm256_mul_pd(A, b);
      C00 = _mm256_add_pd(m, C00);  C20 = _mm256_load_pd(pC0+4);
      b = _mm256_unpacklo_pd(rB01, rB01);       /* rB01 rB01 rB01 rB01 */
      m    = _mm256_mul_pd(A, b);
      C01 = _mm256_add_pd(m, C01);  C21 = _mm256_load_pd(pC1+4);
      b = _mm256_unpacklo_pd(rB02, rB02);       /* rB02 rB02 rB02 rB02 */
      m    = _mm256_mul_pd(A, b);
      C02 = _mm256_add_pd(m, C02);  A = _mm256_load_pd(pA1);

      b = _mm256_unpacklo_pd(iB00, iB00);       /* iB00 iB00 iB00 iB00 */
      m    = _mm256_mul_pd(a, b);
      C00 = _mm256_addsub_pd(C00, m);  C22 = _mm256_load_pd(pC2+4);
      b = _mm256_unpacklo_pd(iB01, iB01);
      m    = _mm256_mul_pd(a, b);
      C01 = _mm256_addsub_pd(C01, m);
      b = _mm256_unpacklo_pd(iB02, iB02);
      m    = _mm256_mul_pd(a, b);
      C02 = _mm256_addsub_pd(C02, m); a    = _mm256_shuffle_pd(A, A, 0x5);
/*
 *    Do M=0, K=1 calcs
 */
      b = _mm256_unpackhi_pd(rB00, rB00);       /* rB10 rB10 rB10 rB10 */
      m    = _mm256_mul_pd(A, b);
      C00 = _mm256_add_pd(m, C00);
      b = _mm256_unpackhi_pd(rB01, rB01);       /* rB11 rB11 rB11 rB11 */
      m    = _mm256_mul_pd(A, b);
      C01 = _mm256_add_pd(m, C01);
      b = _mm256_unpackhi_pd(rB02, rB02);       /* rB12 rB12 rB12 rB12 */
      m    = _mm256_mul_pd(A, b);
      C02 = _mm256_add_pd(m, C02);  A = _mm256_load_pd(pA0+4);

      b = _mm256_unpackhi_pd(iB00, iB00);       /* iB10 iB10 iB10 iB10 */
      m    = _mm256_mul_pd(a, b);
      C00 = _mm256_addsub_pd(C00, m); _mm256_store_pd(pC0, C00);
      b = _mm256_unpackhi_pd(iB01, iB01);
      m    = _mm256_mul_pd(a, b);
      C01 = _mm256_addsub_pd(C01, m); _mm256_store_pd(pC1, C01);
      b = _mm256_unpackhi_pd(iB02, iB02);
      m    = _mm256_mul_pd(a, b);
      C02 = _mm256_addsub_pd(C02, m); _mm256_store_pd(pC2, C02);
/*
 *    Do M=2, K=0 calcs
 */
      b = _mm256_unpacklo_pd(rB00, rB00);       /* rB00 rB00 rB00 rB00 */
      m    = _mm256_mul_pd(A, b);
      C20 = _mm256_add_pd(m, C20);  a    = _mm256_shuffle_pd(A, A, 0x5);
      b = _mm256_unpacklo_pd(rB01, rB01);
      m    = _mm256_mul_pd(A, b);
      C21 = _mm256_add_pd(m, C21);  C00 = _mm256_load_pd(pC0+8);
      b = _mm256_unpacklo_pd(rB02, rB02);
      m    = _mm256_mul_pd(A, b);
      C22 = _mm256_add_pd(m, C22);  A = _mm256_load_pd(pA1+4);

      b = _mm256_unpacklo_pd(iB00, iB00);       /* iB00 iB00 iB00 iB00 */
      m    = _mm256_mul_pd(a, b);
      C20 = _mm256_addsub_pd(C20, m);  C01 = _mm256_load_pd(pC1+8);
      b = _mm256_unpacklo_pd(iB01, iB01);
      m    = _mm256_mul_pd(a, b);
      C21 = _mm256_addsub_pd(C21, m);  C02 = _mm256_load_pd(pC2+8);
      b = _mm256_unpacklo_pd(iB02, iB02);
      m    = _mm256_mul_pd(a, b);
      C22 = _mm256_addsub_pd(C22, m);   a    = _mm256_shuffle_pd(A, A, 0x5);
/*
 *    M=2, K=1 calcs
 */
      b = _mm256_unpackhi_pd(rB00, rB00);       /* rB10 rB10 rB10 rB10 */
      m    = _mm256_mul_pd(A, b);
      C20 = _mm256_add_pd(m, C20);
      b = _mm256_unpackhi_pd(rB01, rB01);
      m    = _mm256_mul_pd(A, b);
      C21 = _mm256_add_pd(m, C21);
      b = _mm256_unpackhi_pd(rB02, rB02);
      m    = _mm256_mul_pd(A, b);
      C22 = _mm256_add_pd(m, C22);   A = _mm256_load_pd(pA0+8);

      b = _mm256_unpackhi_pd(iB00, iB00);       /* iB10 iB10 iB10 iB10 */
      m    = _mm256_mul_pd(a, b);
      C20 = _mm256_addsub_pd(C20, m); _mm256_store_pd(pC0+4, C20);
      b = _mm256_unpackhi_pd(iB01, iB01);
      m    = _mm256_mul_pd(a, b);
      C21 = _mm256_addsub_pd(C21, m); _mm256_store_pd(pC1+4, C21);
      b = _mm256_unpackhi_pd(iB02, iB02);
      m    = _mm256_mul_pd(a, b);
      C22 = _mm256_addsub_pd(C22, m); _mm256_store_pd(pC2+4, C22);
      a    = _mm256_shuffle_pd(A, A, 0x5);
   }
/*
 * Drain pipes
 */
   {
      register __m256d m, b;
/*
 *    Do M=K=0 calcs
 */
      b = _mm256_unpacklo_pd(rB00, rB00);       /* rB00 rB00 rB00 rB00 */
      m    = _mm256_mul_pd(A, b);
      C00 = _mm256_add_pd(m, C00);
      b = _mm256_unpacklo_pd(rB01, rB01);       /* rB01 rB01 rB01 rB01 */
      m    = _mm256_mul_pd(A, b);
      C01 = _mm256_add_pd(m, C01);
      b = _mm256_unpacklo_pd(rB02, rB02);       /* rB02 rB02 rB02 rB02 */
      m    = _mm256_mul_pd(A, b);
      C02 = _mm256_add_pd(m, C02);  A = _mm256_load_pd(pA1);

      b = _mm256_unpacklo_pd(iB00, iB00);       /* iB00 iB00 iB00 iB00 */
      m    = _mm256_mul_pd(a, b);
      C00 = _mm256_addsub_pd(C00, m);
      b = _mm256_unpacklo_pd(iB01, iB01);
      m    = _mm256_mul_pd(a, b);
      C01 = _mm256_addsub_pd(C01, m);
      b = _mm256_unpacklo_pd(iB02, iB02);
      m    = _mm256_mul_pd(a, b);
      C02 = _mm256_addsub_pd(C02, m); a    = _mm256_shuffle_pd(A, A, 0x5);
/*
 *    Do M=0, K=1 calcs
 */
      b = _mm256_unpackhi_pd(rB00, rB00);       /* rB10 rB10 rB10 rB10 */
      m    = _mm256_mul_pd(A, b);
      C00 = _mm256_add_pd(m, C00);
      b = _mm256_unpackhi_pd(rB01, rB01);       /* rB11 rB11 rB11 rB11 */
      m    = _mm256_mul_pd(A, b);
      C01 = _mm256_add_pd(m, C01);
      b = _mm256_unpackhi_pd(rB02, rB02);       /* rB12 rB12 rB12 rB12 */
      m    = _mm256_mul_pd(A, b);
      C02 = _mm256_add_pd(m, C02);

      b = _mm256_unpackhi_pd(iB00, iB00);       /* iB10 iB10 iB10 iB10 */
      m    = _mm256_mul_pd(a, b);
      C00 = _mm256_addsub_pd(C00, m); _mm256_store_pd(pC0, C00);
      b = _mm256_unpackhi_pd(iB01, iB01);
      m    = _mm256_mul_pd(a, b);
      C01 = _mm256_addsub_pd(C01, m); _mm256_store_pd(pC1, C01);
      b = _mm256_unpackhi_pd(iB02, iB02);
      m    = _mm256_mul_pd(a, b);
      C02 = _mm256_addsub_pd(C02, m); _mm256_store_pd(pC2, C02);
      if (!(M&2))
      {
         A = _mm256_load_pd(pA0+4);
         C20 = _mm256_load_pd(pC0+4);
         C21 = _mm256_load_pd(pC1+4);
         C22 = _mm256_load_pd(pC2+4);
         a    = _mm256_shuffle_pd(A, A, 0x5);
/*
 *       Do M=2, K=0 calcs
 */
         b = _mm256_unpacklo_pd(rB00, rB00);       /* rB00 rB00 rB00 rB00 */
         m    = _mm256_mul_pd(A, b);
         C20 = _mm256_add_pd(m, C20);  a    = _mm256_shuffle_pd(A, A, 0x5);
         b = _mm256_unpacklo_pd(rB01, rB01);
         m    = _mm256_mul_pd(A, b);
         C21 = _mm256_add_pd(m, C21);
         b = _mm256_unpacklo_pd(rB02, rB02);
         m    = _mm256_mul_pd(A, b);
         C22 = _mm256_add_pd(m, C22);  A = _mm256_load_pd(pA1+4);

         b = _mm256_unpacklo_pd(iB00, iB00);       /* iB00 iB00 iB00 iB00 */
         m    = _mm256_mul_pd(a, b);
         C20 = _mm256_addsub_pd(C20, m);
         b = _mm256_unpacklo_pd(iB01, iB01);
         m    = _mm256_mul_pd(a, b);
         C21 = _mm256_addsub_pd(C21, m);
         b = _mm256_unpacklo_pd(iB02, iB02);
         m    = _mm256_mul_pd(a, b);
         C22 = _mm256_addsub_pd(C22, m);   a    = _mm256_shuffle_pd(A, A, 0x5);
/*
 *       M=2, K=1 calcs
 */
         b = _mm256_unpackhi_pd(rB00, rB00);       /* rB10 rB10 rB10 rB10 */
         m    = _mm256_mul_pd(A, b);
         C20 = _mm256_add_pd(m, C20);
         b = _mm256_unpackhi_pd(rB01, rB01);
         m    = _mm256_mul_pd(A, b);
         C21 = _mm256_add_pd(m, C21);
         b = _mm256_unpackhi_pd(rB02, rB02);
         m    = _mm256_mul_pd(A, b);
         C22 = _mm256_add_pd(m, C22);

         b = _mm256_unpackhi_pd(iB00, iB00);       /* iB10 iB10 iB10 iB10 */
         m    = _mm256_mul_pd(a, b);
         C20 = _mm256_addsub_pd(C20, m); _mm256_store_pd(pC0+4, C20);
         b = _mm256_unpackhi_pd(iB01, iB01);
         m    = _mm256_mul_pd(a, b);
         C21 = _mm256_addsub_pd(C21, m); _mm256_store_pd(pC1+4, C21);
         b = _mm256_unpackhi_pd(iB02, iB02);
         m    = _mm256_mul_pd(a, b);
         C22 = _mm256_addsub_pd(C22, m); _mm256_store_pd(pC2+4, C22);
      }
   }
/*
 * Do a MU=1, NU=3, KU=2 GEMM cleanup
 */
   if (M1)
   {
      const register TYPE rb00=(*pB0), ib00 = pB0[1], rb10=(*pB1), ib10=pB1[1],
                          rb01=pB0[2], ib01=pB0[3], rb11=pB1[2], ib11=pB1[3],
                          rb02=pB0[4], ib02=pB0[5], rb12=pB1[4], ib12=pB1[5];
      register TYPE ra00, ia00, ra01, ia01;
      i = (M&2) ? 4 : 8;
      pA0 += i; pA1 += i;
      pC0 += i; pC1 += i; pC2 += i;
      ra00 = *pA0;
      ia00 = pA0[1];
      ra01 = *pA1;
      ia01 = pA1[1];
      *pC0   += ra00*rb00-ia00*ib00 + ra01*rb10-ia01*ib10;
      pC0[1] += ra00*ib00+ia00*rb00 + ra01*ib10+ia01*rb10;
      *pC1   += ra00*rb01-ia00*ib01 + ra01*rb11-ia01*ib11;
      pC1[1] += ra00*ib01+ia00*rb01 + ra01*ib11+ia01*rb11;
      *pC2   += ra00*rb02-ia00*ib02 + ra01*rb12-ia01*ib12;
      pC2[1] += ra00*ib02+ia00*rb02 + ra01*ib12+ia01*rb12;

   }
   if (M1)  /* do I have a single cleanup step left? */
   {
      register __m256d b, B;
      __m128d c0, c1, t0;
                                           /* rB0x={rB1x rB0x rB1x rB0x} */
                                           /* iB0x={iB1x iB0x iB1x iB0x} */
      i = (M&2) ? 4 : 8;
      pA0 += i; pA1 += i;
      pC0 += i; pC1 += i; pC2 += i;



      a =_mm256_movedup_pd(_mm256_load_pd(pA0));/* A = {a0i, a0i, a0r, a0r} */
      b = _mm256_unpacklo_pd(rB00, iB00);       /* b = {b0i, b0r, b0i ,b0r} */
      t0 = _mm_load_pd(pC0);                    /* t0= {c0i, c0r} */
      b = _mm256_shuffle_pd(b, b, 6);           /* b = {b0r, b0i, b0i ,b0r} */
      b = _mm256_mul_pd(a, b);                  /* b0= {c0iB,-c0rB,c0iA,c0rA} */
      A =_mm256_movedup_pd(_mm256_load_pd(pA1));/* A = {a1i, a1i, a1r, a1r} */
      B = _mm256_unpackhi_pd(rB00, iB00);       /* B = {b1i, b1r, b1i ,b1r} */
      B = _mm256_shuffle_pd(B, B, 6);           /* B = {b1r, b1i, b1i ,b1r} */
      B = _mm256_mul_pd(A, B);                  /* B = {c1iB,-c1rB,c1iA,c1rA} */
      b = _mm256_add_pd(B, b);
      c0 = _mm256_extractf128_pd(b, 0);         /* c0= {c0iA, c0rA} */
      c0 = _mm_add_pd(c0, t0);
      c1 = _mm256_extractf128_pd(b, 1);         /* c0= {c0iB,-c0rA} */
      c0 = _mm_addsub_pd(c0, c1);               /* c0= {c0i, c0r} */
      _mm_store_pd(pC0, c0);
   }
}
void ATL_UGER2K
   (ATL_CINT M, ATL_CINT N, const TYPE *X0, const TYPE *Y0,
    const TYPE *X1, const TYPE *Y1, TYPE *A, ATL_CINT lda)
{
   const TYPE *x0, *x1;
   register ATL_INT i, j;
   ATL_CINT incA = lda+lda + (lda<<2);

   ATL_assert((N/3)*3 == N);
   for (j=0; j < N; j += 3, A += incA, Y0 += 6, Y1 += 6)
      ATL_rk2(M, X0, X1, Y0, Y1, A, lda);
}
