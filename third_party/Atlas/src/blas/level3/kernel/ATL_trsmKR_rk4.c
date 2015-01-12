#include "atlas_misc.h"
#include "atlas_prefetch.h"
#define RTYPE register TYPE

#if defined(__GNUC__) || \
    (defined(__STDC_VERSION__) && (__STDC_VERSION__/100 >= 1999))
   #define ATL_SINLINE static inline
#else
   #define ATL_SINLINE static
#endif
#if defined(ATL_AVX) && defined(DREAL)
   #define NRHS 3
   #define ATL_BINWRK 1
   #include <immintrin.h>
/*
 * Subtract off x0...x3 contribution to all remaining equations using a
 * rank-4 update with mu=4, nu=3, ku=4.  This version is for 16 AVX regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable software pipelinine of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk4(ATL_CINT M, const TYPE *A, ATL_CINT lda,
                           TYPE *pB0, ATL_CINT ldb, TYPE *C, ATL_CINT ldc)
{
   const TYPE *pA0 = A, *pA1 = A+lda,
              *pA2 = A+((lda)<<1), *pA3=pA1+((lda)<<1);
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1);
   ATL_CINT MM =  (M & 4) ? M-4 : M-8;
   int i;
   register __m256d rB00, rB01, rB02;
   register __m256d rB20, rB21, rB22;
   register __m256d rC00, rC01, rC02;
   register __m256d rC40, rC41, rC42;
   register __m256d rA0, rA1;

   if (M < 4)
      return;
   rB00 = _mm256_broadcast_pd((void*)pB0);              /* B10 B00 B10 B00 */
   rB20 = _mm256_broadcast_pd((void*)(pB0+2));
   rB01 = _mm256_broadcast_pd((void*)(pB0+ldb));
   rB21 = _mm256_broadcast_pd((void*)(pB0+ldb+2));
   rB02 = _mm256_broadcast_pd((void*)(pB0+ldb+ldb));
   rB22 = _mm256_broadcast_pd((void*)(pB0+ldb+ldb+2));

   rC00 = _mm256_load_pd(pC0);                          /* C30 C20 C10 C00 */
   rC01 = _mm256_load_pd(pC1);
   rC02 = _mm256_load_pd(pC2);
   rA0  = _mm256_load_pd(pA0);                          /* A30 A20 A10, A00 */
   for (i=0; i < MM; i += 8, pA0 += 8, pA1 += 8, pA2 += 8, pA3 += 8,
        pC0 += 8, pC1 += 8, pC2 += 8)
   {
      register __m256d rB;

      rB = _mm256_unpacklo_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA0);
      rC00 = _mm256_sub_pd(rC00, rB); rA1 = _mm256_load_pd(pA1);
      rB = _mm256_unpacklo_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA0);
      rC01 = _mm256_sub_pd(rC01, rB); rC40 =_mm256_load_pd(pC0+4);
      rB = _mm256_unpacklo_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA0);
      rC02 = _mm256_sub_pd(rC02, rB); rA0 = _mm256_load_pd(pA2);

      rB = _mm256_unpackhi_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA1);
      rC00 = _mm256_sub_pd(rC00, rB); rC41 =_mm256_load_pd(pC1+4);
      rB = _mm256_unpackhi_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA1);
      rC01 = _mm256_sub_pd(rC01, rB); rC42 =_mm256_load_pd(pC2+4);
      rB = _mm256_unpackhi_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA1);
      rC02 = _mm256_sub_pd(rC02, rB); rA1 = _mm256_load_pd(pA3);

      rB = _mm256_unpacklo_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA0);
      rC00 = _mm256_sub_pd(rC00, rB);
      rB = _mm256_unpacklo_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA0);
      rC01 = _mm256_sub_pd(rC01, rB);
      rB = _mm256_unpacklo_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA0);
      rC02 = _mm256_sub_pd(rC02, rB); rA0 = _mm256_load_pd(pA0+4);

      rB = _mm256_unpackhi_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA1);
      rC00 = _mm256_sub_pd(rC00, rB); _mm256_store_pd(pC0, rC00);
      rB = _mm256_unpackhi_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA1);
      rC01 = _mm256_sub_pd(rC01, rB); _mm256_store_pd(pC1, rC01);
      rB = _mm256_unpackhi_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA1);
      rC02 = _mm256_sub_pd(rC02, rB); rA1 = _mm256_load_pd(pA1+4);
/*
 *    2nd row of C regs
 */
      rB = _mm256_unpacklo_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA0);
      rC40 = _mm256_sub_pd(rC40, rB); _mm256_store_pd(pC2, rC02);
      rB = _mm256_unpacklo_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA0);
      rC41 = _mm256_sub_pd(rC41, rB); rC00 = _mm256_load_pd(pC0+8);
      rB = _mm256_unpacklo_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA0);
      rC42 = _mm256_sub_pd(rC42, rB); rA0 = _mm256_load_pd(pA2+4);

      rB = _mm256_unpackhi_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA1);
      rC40 = _mm256_sub_pd(rC40, rB); rC01 = _mm256_load_pd(pC1+8);
      rB = _mm256_unpackhi_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA1);
      rC41 = _mm256_sub_pd(rC41, rB); rC02 = _mm256_load_pd(pC2+8);
      rB = _mm256_unpackhi_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA1);
      rC42 = _mm256_sub_pd(rC42, rB); rA1 = _mm256_load_pd(pA3+4);

      rB = _mm256_unpacklo_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA0);
      rC40 = _mm256_sub_pd(rC40, rB);
      rB = _mm256_unpacklo_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA0);
      rC41 = _mm256_sub_pd(rC41, rB);
      rB = _mm256_unpacklo_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA0);
      rC42 = _mm256_sub_pd(rC42, rB); rA0 = _mm256_load_pd(pA0+8);

      rB = _mm256_unpackhi_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA1);
      rC40 = _mm256_sub_pd(rC40, rB); _mm256_store_pd(pC0+4, rC40);
      rB = _mm256_unpackhi_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA1);
      rC41 = _mm256_sub_pd(rC41, rB); _mm256_store_pd(pC1+4, rC41);
      rB = _mm256_unpackhi_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA1);
      rC42 = _mm256_sub_pd(rC42, rB); _mm256_store_pd(pC2+4, rC42);
   }
/*
 * Drain C load/use pipeline
 */
   if (M-MM == 4)   /* drain pipe over 1 iteration */
   {
      register __m256d rB;

      rB = _mm256_unpacklo_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA0);
      rC00 = _mm256_sub_pd(rC00, rB); rA1 = _mm256_load_pd(pA1);
      rB = _mm256_unpacklo_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA0);
      rC01 = _mm256_sub_pd(rC01, rB);
      rB = _mm256_unpacklo_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA0);
      rC02 = _mm256_sub_pd(rC02, rB); rA0 = _mm256_load_pd(pA2);

      rB = _mm256_unpackhi_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA1);
      rC00 = _mm256_sub_pd(rC00, rB);
      rB = _mm256_unpackhi_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA1);
      rC01 = _mm256_sub_pd(rC01, rB);
      rB = _mm256_unpackhi_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA1);
      rC02 = _mm256_sub_pd(rC02, rB); rA1 = _mm256_load_pd(pA3);

      rB = _mm256_unpacklo_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA0);
      rC00 = _mm256_sub_pd(rC00, rB);
      rB = _mm256_unpacklo_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA0);
      rC01 = _mm256_sub_pd(rC01, rB);
      rB = _mm256_unpacklo_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA0);
      rC02 = _mm256_sub_pd(rC02, rB);

      rB = _mm256_unpackhi_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA1);
      rC00 = _mm256_sub_pd(rC00, rB); _mm256_store_pd(pC0, rC00);
      rB = _mm256_unpackhi_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA1);
      rC01 = _mm256_sub_pd(rC01, rB); _mm256_store_pd(pC1, rC01);
      rB = _mm256_unpackhi_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA1);
      rC02 = _mm256_sub_pd(rC02, rB); _mm256_store_pd(pC2, rC02);
   }
   else /* M-MM = 8, drain pipe over 2 iterations */
   {
      register __m256d rB;

      rB = _mm256_unpacklo_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA0);
      rC00 = _mm256_sub_pd(rC00, rB); rA1 = _mm256_load_pd(pA1);
      rB = _mm256_unpacklo_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA0);
      rC01 = _mm256_sub_pd(rC01, rB); rC40 =_mm256_load_pd(pC0+4);
      rB = _mm256_unpacklo_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA0);
      rC02 = _mm256_sub_pd(rC02, rB); rA0 = _mm256_load_pd(pA2);

      rB = _mm256_unpackhi_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA1);
      rC00 = _mm256_sub_pd(rC00, rB); rC41 =_mm256_load_pd(pC1+4);
      rB = _mm256_unpackhi_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA1);
      rC01 = _mm256_sub_pd(rC01, rB); rC42 =_mm256_load_pd(pC2+4);
      rB = _mm256_unpackhi_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA1);
      rC02 = _mm256_sub_pd(rC02, rB); rA1 = _mm256_load_pd(pA3);

      rB = _mm256_unpacklo_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA0);
      rC00 = _mm256_sub_pd(rC00, rB);
      rB = _mm256_unpacklo_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA0);
      rC01 = _mm256_sub_pd(rC01, rB);
      rB = _mm256_unpacklo_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA0);
      rC02 = _mm256_sub_pd(rC02, rB); rA0 = _mm256_load_pd(pA0+4);

      rB = _mm256_unpackhi_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA1);
      rC00 = _mm256_sub_pd(rC00, rB); _mm256_store_pd(pC0, rC00);
      rB = _mm256_unpackhi_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA1);
      rC01 = _mm256_sub_pd(rC01, rB); _mm256_store_pd(pC1, rC01);
      rB = _mm256_unpackhi_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA1);
      rC02 = _mm256_sub_pd(rC02, rB); rA1 = _mm256_load_pd(pA1+4);
/*
 *    2nd row of C regs
 */
      rB = _mm256_unpacklo_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA0);
      rC40 = _mm256_sub_pd(rC40, rB); _mm256_store_pd(pC2, rC02);
      rB = _mm256_unpacklo_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA0);
      rC41 = _mm256_sub_pd(rC41, rB);
      rB = _mm256_unpacklo_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA0);
      rC42 = _mm256_sub_pd(rC42, rB); rA0 = _mm256_load_pd(pA2+4);

      rB = _mm256_unpackhi_pd(rB00, rB00);
      rB = _mm256_mul_pd(rB, rA1);
      rC40 = _mm256_sub_pd(rC40, rB);
      rB = _mm256_unpackhi_pd(rB01, rB01);
      rB = _mm256_mul_pd(rB, rA1);
      rC41 = _mm256_sub_pd(rC41, rB);
      rB = _mm256_unpackhi_pd(rB02, rB02);
      rB = _mm256_mul_pd(rB, rA1);
      rC42 = _mm256_sub_pd(rC42, rB); rA1 = _mm256_load_pd(pA3+4);

      rB = _mm256_unpacklo_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA0);
      rC40 = _mm256_sub_pd(rC40, rB);
      rB = _mm256_unpacklo_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA0);
      rC41 = _mm256_sub_pd(rC41, rB);
      rB = _mm256_unpacklo_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA0);
      rC42 = _mm256_sub_pd(rC42, rB);

      rB = _mm256_unpackhi_pd(rB20, rB20);
      rB = _mm256_mul_pd(rB, rA1);
      rC40 = _mm256_sub_pd(rC40, rB); _mm256_store_pd(pC0+4, rC40);
      rB = _mm256_unpackhi_pd(rB21, rB21);
      rB = _mm256_mul_pd(rB, rA1);
      rC41 = _mm256_sub_pd(rC41, rB); _mm256_store_pd(pC1+4, rC41);
      rB = _mm256_unpackhi_pd(rB22, rB22);
      rB = _mm256_mul_pd(rB, rA1);
      rC42 = _mm256_sub_pd(rC42, rB); _mm256_store_pd(pC2+4, rC42);
   }
}
#elif defined(ATL_SSE2) && defined(DREAL)
   #define NRHS 3
   #define ATL_BINWRK 1
   #include <xmmintrin.h>
/*
 * Subtract off x0...x3 contribution to all remaining equations using a
 * rank-4 update with mu=4, nu=3, ku=4.  This version is for 16 SSE2 regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable software pipelinine of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk4(ATL_CINT M, const TYPE *A, ATL_CINT lda,
                           TYPE *pB0, ATL_CINT ldb, TYPE *C, ATL_CINT ldc)
{
   const TYPE *pA0 = A, *pA1 = A+lda,
              *pA2 = A+((lda)<<1), *pA3=pA1+((lda)<<1);
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1);
   const int MM = M-4;
   int i;
   register __m128d rB00, rB01, rB02;
   register __m128d rB20, rB21, rB22;
   register __m128d rC00, rC01, rC02;
   register __m128d rC20, rC21, rC22;
   register __m128d rA0, rA1;

   if (M < 4)
      return;
   rB00 = _mm_load_pd(pB0);
   rB20 = _mm_load_pd(pB0+2);
   rB01 = _mm_load_pd(pB0+ldb);
   rB21 = _mm_load_pd(pB0+ldb+2);
   rB02 = _mm_load_pd(pB0+2*ldb);
   rB22 = _mm_load_pd(pB0+2*ldb+2);

   rC00 = _mm_load_pd(pC0);
   rC01 = _mm_load_pd(pC1);
   rC02 = _mm_load_pd(pC2);
   rA0  = _mm_load_pd(pA0);  /* A1, A0 */
   for (i=0; i < MM; i += 4, pA0 += 4, pA1 += 4, pA2 += 4, pA3 += 4,
        pC0 += 4, pC1 += 4, pC2 += 4)
   {
      register __m128d rB;

      rB = _mm_unpacklo_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA0);
      rC00 = _mm_sub_pd(rC00, rB); rA1 = _mm_load_pd(pA1);
      rB = _mm_unpacklo_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA0);
      rC01 = _mm_sub_pd(rC01, rB); rC20 =_mm_load_pd(pC0+2);
      rB = _mm_unpacklo_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA0);
      rC02 = _mm_sub_pd(rC02, rB); rA0 = _mm_load_pd(pA2);

      rB = _mm_unpackhi_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA1);
      rC00 = _mm_sub_pd(rC00, rB); rC21 =_mm_load_pd(pC1+2);
      rB = _mm_unpackhi_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA1);
      rC01 = _mm_sub_pd(rC01, rB); rC22 =_mm_load_pd(pC2+2);
      rB = _mm_unpackhi_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA1);
      rC02 = _mm_sub_pd(rC02, rB); rA1 = _mm_load_pd(pA3);

      rB = _mm_unpacklo_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA0);
      rC00 = _mm_sub_pd(rC00, rB);
      rB = _mm_unpacklo_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA0);
      rC01 = _mm_sub_pd(rC01, rB);
      rB = _mm_unpacklo_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA0);
      rC02 = _mm_sub_pd(rC02, rB); rA0 = _mm_load_pd(pA0+2);

      rB = _mm_unpackhi_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA1);
      rC00 = _mm_sub_pd(rC00, rB); _mm_store_pd(pC0, rC00);
      rB = _mm_unpackhi_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA1);
      rC01 = _mm_sub_pd(rC01, rB); _mm_store_pd(pC1, rC01);
      rB = _mm_unpackhi_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA1);
      rC02 = _mm_sub_pd(rC02, rB); rA1 = _mm_load_pd(pA1+2);
/*
 *    2nd row of C regs
 */
      rB = _mm_unpacklo_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA0);
      rC20 = _mm_sub_pd(rC20, rB); _mm_store_pd(pC2, rC02);
      rB = _mm_unpacklo_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA0);
      rC21 = _mm_sub_pd(rC21, rB); rC00 = _mm_load_pd(pC0+4);
      rB = _mm_unpacklo_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA0);
      rC22 = _mm_sub_pd(rC22, rB); rA0 = _mm_load_pd(pA2+2);

      rB = _mm_unpackhi_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA1);
      rC20 = _mm_sub_pd(rC20, rB); rC01 = _mm_load_pd(pC1+4);
      rB = _mm_unpackhi_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA1);
      rC21 = _mm_sub_pd(rC21, rB); rC02 = _mm_load_pd(pC2+4);
      rB = _mm_unpackhi_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA1);
      rC22 = _mm_sub_pd(rC22, rB); rA1 = _mm_load_pd(pA3+2);

      rB = _mm_unpacklo_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA0);
      rC20 = _mm_sub_pd(rC20, rB);
      rB = _mm_unpacklo_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA0);
      rC21 = _mm_sub_pd(rC21, rB);
      rB = _mm_unpacklo_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA0);
      rC22 = _mm_sub_pd(rC22, rB); rA0 = _mm_load_pd(pA0+4);

      rB = _mm_unpackhi_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA1);
      rC20 = _mm_sub_pd(rC20, rB); _mm_store_pd(pC0+2, rC20);
      rB = _mm_unpackhi_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA1);
      rC21 = _mm_sub_pd(rC21, rB); _mm_store_pd(pC1+2, rC21);
      rB = _mm_unpackhi_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA1);
      rC22 = _mm_sub_pd(rC22, rB); _mm_store_pd(pC2+2, rC22);
   }
/*
 * Drain C load/use pipeline
 */
   {
      register __m128d rB;

      rB = _mm_unpacklo_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA0);
      rC00 = _mm_sub_pd(rC00, rB); rA1 = _mm_load_pd(pA1);
      rB = _mm_unpacklo_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA0);
      rC01 = _mm_sub_pd(rC01, rB); rC20 =_mm_load_pd(pC0+2);
      rB = _mm_unpacklo_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA0);
      rC02 = _mm_sub_pd(rC02, rB); rA0 = _mm_load_pd(pA2);

      rB = _mm_unpackhi_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA1);
      rC00 = _mm_sub_pd(rC00, rB); rC21 =_mm_load_pd(pC1+2);
      rB = _mm_unpackhi_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA1);
      rC01 = _mm_sub_pd(rC01, rB); rC22 =_mm_load_pd(pC2+2);
      rB = _mm_unpackhi_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA1);
      rC02 = _mm_sub_pd(rC02, rB); rA1 = _mm_load_pd(pA3);

      rB = _mm_unpacklo_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA0);
      rC00 = _mm_sub_pd(rC00, rB);
      rB = _mm_unpacklo_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA0);
      rC01 = _mm_sub_pd(rC01, rB);
      rB = _mm_unpacklo_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA0);
      rC02 = _mm_sub_pd(rC02, rB); rA0 = _mm_load_pd(pA0+2);

      rB = _mm_unpackhi_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA1);
      rC00 = _mm_sub_pd(rC00, rB); _mm_store_pd(pC0, rC00);
      rB = _mm_unpackhi_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA1);
      rC01 = _mm_sub_pd(rC01, rB); _mm_store_pd(pC1, rC01);
      rB = _mm_unpackhi_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA1);
      rC02 = _mm_sub_pd(rC02, rB); rA1 = _mm_load_pd(pA1+2);
/*
 *    2nd row of C regs
 */
      rB = _mm_unpacklo_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA0);
      rC20 = _mm_sub_pd(rC20, rB); _mm_store_pd(pC2, rC02);
      rB = _mm_unpacklo_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA0);
      rC21 = _mm_sub_pd(rC21, rB);
      rB = _mm_unpacklo_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA0);
      rC22 = _mm_sub_pd(rC22, rB); rA0 = _mm_load_pd(pA2+2);

      rB = _mm_unpackhi_pd(rB00, rB00);
      rB = _mm_mul_pd(rB, rA1);
      rC20 = _mm_sub_pd(rC20, rB);
      rB = _mm_unpackhi_pd(rB01, rB01);
      rB = _mm_mul_pd(rB, rA1);
      rC21 = _mm_sub_pd(rC21, rB);
      rB = _mm_unpackhi_pd(rB02, rB02);
      rB = _mm_mul_pd(rB, rA1);
      rC22 = _mm_sub_pd(rC22, rB); rA1 = _mm_load_pd(pA3+2);

      rB = _mm_unpacklo_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA0);
      rC20 = _mm_sub_pd(rC20, rB);
      rB = _mm_unpacklo_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA0);
      rC21 = _mm_sub_pd(rC21, rB);
      rB = _mm_unpacklo_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA0);
      rC22 = _mm_sub_pd(rC22, rB);

      rB = _mm_unpackhi_pd(rB20, rB20);
      rB = _mm_mul_pd(rB, rA1);
      rC20 = _mm_sub_pd(rC20, rB); _mm_store_pd(pC0+2, rC20);
      rB = _mm_unpackhi_pd(rB21, rB21);
      rB = _mm_mul_pd(rB, rA1);
      rC21 = _mm_sub_pd(rC21, rB); _mm_store_pd(pC1+2, rC21);
      rB = _mm_unpackhi_pd(rB22, rB22);
      rB = _mm_mul_pd(rB, rA1);
      rC22 = _mm_sub_pd(rC22, rB); _mm_store_pd(pC2+2, rC22);
   }
}
#elif defined(ATL_SSE2) && defined(SREAL)
   #define NRHS 4
   #define ATL_BINWRK 1
   #include <xmmintrin.h>
/*
 * Subtract off x0...x3 contribution to all remaining equations using a
 * rank-4 update with mu=8, nu=4, ku=4.  This version is for 16 SSE regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable vectorizations & software pipelinine of load/use.
 * Code operates on any multiple of 4 despite using MU=8.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk4(ATL_CINT M, const TYPE *A, ATL_CINT lda,
                           TYPE *pB0, ATL_CINT ldb, TYPE *C, ATL_CINT ldc)
{
   const TYPE *pA0 = A, *pA1 = A+lda,
              *pA2 = A+((lda)<<1), *pA3=pA1+((lda)<<1);
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1), *pC3 = pC2+ldc;
   ATL_CINT MM =  (M & 4) ? M-4 : M-8;
   int i;
   register __m128 rB00, rB01, rB02, rB03;
   register __m128 rC00, rC01, rC02, rC03;
   register __m128 rC40, rC41, rC42, rC43;
   register __m128 rA0, rA1;

   if (M < 4)
      return;
   rB00 = _mm_load_ps(pB0);
   rB01 = _mm_load_ps(pB0+ldb);
   rB02 = _mm_load_ps(pB0+(ldb<<1));
   rB03 = _mm_load_ps(pB0+(ldb<<1)+ldb);

   rC00 = _mm_load_ps(pC0);
   rC01 = _mm_load_ps(pC1);
   rC02 = _mm_load_ps(pC2);
   rC03 = _mm_load_ps(pC3);

   rA0 = _mm_load_ps(pA0);

   for (i=0; i < MM; i += 8, pA0 += 8, pA1 += 8, pA2 += 8, pA3 += 8,
        pC0 += 8, pC1 += 8, pC2 += 8, pC3 += 8)
   {
      register __m128 rB;
/*
 *    K=0 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC00 = _mm_sub_ps(rC00, rB);  rA1 = _mm_load_ps(pA1);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC01 = _mm_sub_ps(rC01, rB);  rC40 = _mm_load_ps(pC0+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC02 = _mm_sub_ps(rC02, rB);  rC41 = _mm_load_ps(pC1+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC03 = _mm_sub_ps(rC03, rB);  rC42 = _mm_load_ps(pC2+4);
/*
 *    K=1 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC00 = _mm_sub_ps(rC00, rB);  rA0 = _mm_load_ps(pA2);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC01 = _mm_sub_ps(rC01, rB);  rC43 = _mm_load_ps(pC3+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC03 = _mm_sub_ps(rC03, rB);  rA1 = _mm_load_ps(pA3);
/*
 *    K=2 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC00 = _mm_sub_ps(rC00, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC01 = _mm_sub_ps(rC01, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC03 = _mm_sub_ps(rC03, rB);  rA0 = _mm_load_ps(pA0+4);
/*
 *    K=3 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC00 = _mm_sub_ps(rC00, rB);  _mm_store_ps(pC0, rC00);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC01 = _mm_sub_ps(rC01, rB);  _mm_store_ps(pC1, rC01);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC02 = _mm_sub_ps(rC02, rB);  _mm_store_ps(pC2, rC02);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC03 = _mm_sub_ps(rC03, rB); _mm_store_ps(pC3, rC03);

/*
 *    K=0 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC40 = _mm_sub_ps(rC40, rB);  rA1 = _mm_load_ps(pA1+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC41 = _mm_sub_ps(rC41, rB);  rC00 = _mm_load_ps(pC0+8);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC42 = _mm_sub_ps(rC42, rB);  rC01 = _mm_load_ps(pC1+8);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC43 = _mm_sub_ps(rC43, rB);  rC02 = _mm_load_ps(pC2+8);
/*
 *    K=1 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC40 = _mm_sub_ps(rC40, rB);  rA0 = _mm_load_ps(pA2+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC41 = _mm_sub_ps(rC41, rB);  rC03 = _mm_load_ps(pC3+8);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC42 = _mm_sub_ps(rC42, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC43 = _mm_sub_ps(rC43, rB);  rA1 = _mm_load_ps(pA3+4);
/*
 *    K=2 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC40 = _mm_sub_ps(rC40, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC41 = _mm_sub_ps(rC41, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC42 = _mm_sub_ps(rC42, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC43 = _mm_sub_ps(rC43, rB);  rA0 = _mm_load_ps(pA0+8);
/*
 *    K=3 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC40 = _mm_sub_ps(rC40, rB);  _mm_store_ps(pC0+4, rC40);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC41 = _mm_sub_ps(rC41, rB);  _mm_store_ps(pC1+4, rC41);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC42 = _mm_sub_ps(rC42, rB);  _mm_store_ps(pC2+4, rC42);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC43 = _mm_sub_ps(rC43, rB); _mm_store_ps(pC3+4, rC43);
   }
/*
 * If orig M was multiple of 4 rather than 8, drain pipe over last 4 rows
 */
   if (M&4)
   {
      register __m128 rB;
/*
 *    K=0 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC00 = _mm_sub_ps(rC00, rB);  rA1 = _mm_load_ps(pA1);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC01 = _mm_sub_ps(rC01, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC03 = _mm_sub_ps(rC03, rB);  rA0 = _mm_load_ps(pA2);
/*
 *    K=1 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC00 = _mm_sub_ps(rC00, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC01 = _mm_sub_ps(rC01, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC03 = _mm_sub_ps(rC03, rB);  rA1 = _mm_load_ps(pA3);
/*
 *    K=2 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC00 = _mm_sub_ps(rC00, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC01 = _mm_sub_ps(rC01, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC03 = _mm_sub_ps(rC03, rB);
/*
 *    K=3 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC00 = _mm_sub_ps(rC00, rB);  _mm_store_ps(pC0, rC00);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC01 = _mm_sub_ps(rC01, rB);  _mm_store_ps(pC1, rC01);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC02 = _mm_sub_ps(rC02, rB);  _mm_store_ps(pC2, rC02);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC03 = _mm_sub_ps(rC03, rB); _mm_store_ps(pC3, rC03);
   }
   else /* drain pipe with MU=8 */
   {
      register __m128 rB;
/*
 *    K=0 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC00 = _mm_sub_ps(rC00, rB);  rA1 = _mm_load_ps(pA1);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC01 = _mm_sub_ps(rC01, rB);  rC40 = _mm_load_ps(pC0+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC02 = _mm_sub_ps(rC02, rB);  rC41 = _mm_load_ps(pC1+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC03 = _mm_sub_ps(rC03, rB);  rC42 = _mm_load_ps(pC2+4);
/*
 *    K=1 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC00 = _mm_sub_ps(rC00, rB);  rA0 = _mm_load_ps(pA2);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC01 = _mm_sub_ps(rC01, rB);  rC43 = _mm_load_ps(pC3+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC03 = _mm_sub_ps(rC03, rB);  rA1 = _mm_load_ps(pA3);
/*
 *    K=2 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC00 = _mm_sub_ps(rC00, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC01 = _mm_sub_ps(rC01, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC02 = _mm_sub_ps(rC02, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC03 = _mm_sub_ps(rC03, rB);  rA0 = _mm_load_ps(pA0+4);
/*
 *    K=3 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC00 = _mm_sub_ps(rC00, rB);  _mm_store_ps(pC0, rC00);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC01 = _mm_sub_ps(rC01, rB);  _mm_store_ps(pC1, rC01);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC02 = _mm_sub_ps(rC02, rB);  _mm_store_ps(pC2, rC02);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC03 = _mm_sub_ps(rC03, rB); _mm_store_ps(pC3, rC03);

/*
 *    K=0 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC40 = _mm_sub_ps(rC40, rB);  rA1 = _mm_load_ps(pA1+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC41 = _mm_sub_ps(rC41, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC42 = _mm_sub_ps(rC42, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x00);
      rB = _mm_mul_ps(rB, rA0);
      rC43 = _mm_sub_ps(rC43, rB);
/*
 *    K=1 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC40 = _mm_sub_ps(rC40, rB);  rA0 = _mm_load_ps(pA2+4);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC41 = _mm_sub_ps(rC41, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC42 = _mm_sub_ps(rC42, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0x55);
      rB = _mm_mul_ps(rB, rA1);
      rC43 = _mm_sub_ps(rC43, rB);  rA1 = _mm_load_ps(pA3+4);
/*
 *    K=2 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC40 = _mm_sub_ps(rC40, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC41 = _mm_sub_ps(rC41, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC42 = _mm_sub_ps(rC42, rB);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xAA);
      rB = _mm_mul_ps(rB, rA0);
      rC43 = _mm_sub_ps(rC43, rB);
/*
 *    K=3 block
 */
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB00, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC40 = _mm_sub_ps(rC40, rB);  _mm_store_ps(pC0+4, rC40);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB01, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC41 = _mm_sub_ps(rC41, rB);  _mm_store_ps(pC1+4, rC41);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB02, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC42 = _mm_sub_ps(rC42, rB);  _mm_store_ps(pC2+4, rC42);
      rB = (__m128) _mm_shuffle_epi32((__m128i) rB03, 0xFF);
      rB = _mm_mul_ps(rB, rA1);
      rC43 = _mm_sub_ps(rC43, rB); _mm_store_ps(pC3+4, rC43);
   }
}
#else
   #define NRHS 4
   #define ATL_BINWRK 0
/*
 * Subtract off x0...x3 contribution to all remaining equations using a
 * rank-4 update with mu=2, nu=4, ku=4.  This version is for 32 scalar
 * registers, and assumes the scalar registers rB00..rB33 are live on entry.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable software pipelinine of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
#define ATL_rk4(M_, A_, lda_, C_, ldc_) if (M_ > 1) \
{ \
   const TYPE *pA0 = A_, *pA1 = A_+lda_, \
              *pA2 = A_+((lda_)<<1), *pA3=pA1+((lda_)<<1); \
   TYPE *pC0 = C_, *pC1 = C_+ldc_, \
               *pC2 = C_+((ldc_)<<1), *pC3=pC1+((ldc_)<<1); \
   register TYPE rC00= *pC0, rC01= *pC1, rC02 = *pC2, rC03 = *pC3; \
   register TYPE rc00, rc01, rc02, rc03; \
   register TYPE rA0 = *pA0, rA1; \
   ATL_CINT MM = M_ - 2; \
   ATL_INT i; \
 \
   for (i=0; i < MM; i += 2, pA0 += 2, pA1 += 2, pA2 += 2, pA3 += 2, \
        pC0 += 2, pC1 += 2, pC2 += 2, pC3 += 2) \
   { \
      rC00 -= rA0 * rB00; rA1 = *pA1; \
      rC01 -= rA0 * rB01; rc00 = pC0[1]; \
      rC02 -= rA0 * rB02; rc01 = pC1[1]; \
      rC03 -= rA0 * rB03; rc02 = pC2[1]; \
 \
      rC00 -= rA1 * rB10; rA0 = *pA2; \
      rC01 -= rA1 * rB11; rc03 = pC3[1]; \
      rC02 -= rA1 * rB12;  \
      rC03 -= rA1 * rB13;  \
       \
      rC00 -= rA0 * rB20; rA1 = *pA3; \
      rC01 -= rA0 * rB21; \
      rC02 -= rA0 * rB22;  \
      rC03 -= rA0 * rB23; rA0 = pA0[1]; \
       \
      rC00 -= rA1 * rB30; *pC0 = rC00; \
      rC01 -= rA1 * rB31; *pC1 = rC01; \
      rC02 -= rA1 * rB32; *pC2 = rC02; \
      rC03 -= rA1 * rB33; *pC3 = rC03; \
       \
      rc00 -= rA0 * rB00; rA1 = pA1[1]; \
      rc01 -= rA0 * rB01; rC00 = pC0[2]; \
      rc02 -= rA0 * rB02; rC01 = pC1[2]; \
      rc03 -= rA0 * rB03; rC02 = pC2[2]; \
       \
      rc00 -= rA1 * rB10; rA0 = pA2[1]; \
      rc01 -= rA1 * rB11; rC03 = pC3[2]; \
      rc02 -= rA1 * rB12; \
      rc03 -= rA1 * rB13;  \
       \
      rc00 -= rA0 * rB20; rA1 = pA3[1]; \
      rc01 -= rA0 * rB21; \
      rc02 -= rA0 * rB22;  \
      rc03 -= rA0 * rB23; rA0 = pA0[2]; \
       \
      rc00 -= rA1 * rB30; pC0[1] = rc00; \
      rc01 -= rA1 * rB31; pC1[1] = rc01; \
      rc02 -= rA1 * rB32; pC2[1] = rc02; \
      rc03 -= rA1 * rB33; pC3[1] = rc03; \
   } \
/* \
 *  Drain the C fetch/store pipe \
 */ \
   rC00 -= rA0 * rB00; rA1 = *pA1; \
   rC01 -= rA0 * rB01; rc00 = pC0[1]; \
   rC02 -= rA0 * rB02; rc01 = pC1[1]; \
   rC03 -= rA0 * rB03; rc02 = pC2[1]; \
 \
   rC00 -= rA1 * rB10; rA0 = *pA2; \
   rC01 -= rA1 * rB11; rc03 = pC3[1]; \
   rC02 -= rA1 * rB12;  \
   rC03 -= rA1 * rB13;  \
    \
   rC00 -= rA0 * rB20; rA1 = *pA3; \
   rC01 -= rA0 * rB21; \
   rC02 -= rA0 * rB22;  \
   rC03 -= rA0 * rB23; rA0 = pA0[1]; \
    \
   rC00 -= rA1 * rB30; *pC0 = rC00; \
   rC01 -= rA1 * rB31; *pC1 = rC01; \
   rC02 -= rA1 * rB32; *pC2 = rC02; \
   rC03 -= rA1 * rB33; *pC3 = rC03; \
    \
   rc00 -= rA0 * rB00; rA1 = pA1[1]; \
   rc01 -= rA0 * rB01; \
   rc02 -= rA0 * rB02; \
   rc03 -= rA0 * rB03; \
    \
   rc00 -= rA1 * rB10; rA0 = pA2[1]; \
   rc01 -= rA1 * rB11; \
   rc02 -= rA1 * rB12; \
   rc03 -= rA1 * rB13; \
    \
   rc00 -= rA0 * rB20; rA1 = pA3[1]; \
   rc01 -= rA0 * rB21; \
   rc02 -= rA0 * rB22; \
   rc03 -= rA0 * rB23; \
    \
   rc00 -= rA1 * rB30; pC0[1] = rc00; \
   rc01 -= rA1 * rB31; pC1[1] = rc01; \
   rc02 -= rA1 * rB32; pC2[1] = rc02; \
   rc03 -= rA1 * rB33; pC3[1] = rc03; \
}
#endif

#if NRHS == 3
/*
 * Solve 4x4 L with 3 RHS symbolically
 * Answer is output into rBxx regs, which are live on input and output
 */
#define  ATL_trsmL4(L_, ldl_, r_, ldr_) \
{ \
   const RTYPE L00=(*(L_)), L10=L_[1], L20=L_[2], L30=L_[3]; \
   const RTYPE L11=L_[ldl_+1], L21=L_[ldl_+2], L31=a[ldl_+3]; \
   const RTYPE L22=L_[2*(ldl_)+2], L32=L_[2*(ldl_)+3]; \
   const RTYPE L33=L_[3*(ldl_)+3]; \
/* \
 * x0 = b0 / L00 \
 */ \
   rB00 *= L00; \
   rB01 *= L00; \
   rB02 *= L00; \
/* \
 * x1 = (b1 - L10 * x0) / L11 \
 */ \
   rB10 = (rB10 - L10*rB00) * L11;  \
   rB11 = (rB11 - L10*rB01) * L11; \
   rB12 = (rB12 - L10*rB02) * L11; \
   ATL_pfl1W(r_ + ((ldr_)<<2)); \
/* \
 * x2 = (b2 - L20*x0 - L21*x1) / L22 \
 */ \
   rB20 = (rB20 - L20*rB00 - L21*rB10) * L22; \
   rB21 = (rB21 - L20*rB01 - L21*rB11) * L22; \
   rB22 = (rB22 - L20*rB02 - L21*rB12) * L22; \
   ATL_pfl1W(r_ + ldr_+((ldr_)<<2)); \
/* \
 * x3 = (b3 - L30*x0 - L31*x1 - L32*x2) / L33 \
 */ \
   rB30 = (rB30 - L30*rB00 - L31*rB10 - L32*rB20) * L33; \
   rB31 = (rB31 - L30*rB01 - L31*rB11 - L32*rB21) * L33; \
   rB32 = (rB32 - L30*rB02 - L31*rB12 - L32*rB22) * L33; \
   ATL_pfl1W(r_ + ((ldr_)<<1)+((ldr_)<<2)); \
}  /* complete 4x4 NRHS=3 solve block */

#define  ATL_trsmU4(U_, ldu_, r_, ldr_) \
{ \
   const RTYPE U00=(*(U_)); \
   const RTYPE U01=(U_)[ldu_], U11=(U_)[ldu_+1]; \
   const RTYPE U02=(U_)[2*(ldu_)], U12= *(U_+2*(ldu_)+1),  \
               U22 = *(U_+2*(ldu_)+2); \
   const RTYPE U03 = *(U_+3*(ldu_)), U13 = *(U_+3*(ldu_)+1), \
               U23 = *(U_+3*(ldu_)+2), U33 = *(U_+3*(ldu_)+3); \
\
/* \
 * x3 = b3 / U33 \
 */ \
   rB30 *= U33; \
   rB31 *= U33; \
   rB32 *= U33; \
   ATL_pfl1W(r_ + ((ldr_)<<2)); \
/* \
 * x2 = (b2 - U23 * x3) / U22 \
 */ \
   rB20 = (rB20 - U23*rB30) * U22;  \
   rB21 = (rB21 - U23*rB31) * U22; \
   rB22 = (rB22 - U23*rB32) * U22; \
   ATL_pfl1W(r_ + ldr_+((ldr_)<<2)); \
/* \
 * x1 = (b1 - U12*x2 - U13*x3) / U11 \
 */ \
   rB10 = (rB10 - U12*rB20 - U13*rB30) * U11; \
   rB11 = (rB11 - U12*rB21 - U13*rB31) * U11; \
   rB12 = (rB12 - U12*rB22 - U13*rB32) * U11; \
   ATL_pfl1W(r_ + ((ldr_)<<1)+((ldr_)<<2)); \
/* \
 * x0 = (b0 - U01*x1 - U02*x2 - U03*x3) / U00 \
 */ \
   rB00 = (rB00 - U01*rB10 - U02*rB20 - U03*rB30) * U00; \
   rB01 = (rB01 - U01*rB11 - U02*rB21 - U03*rB31) * U00; \
   ATL_pfl1W(r_ + ldr_+((ldr_)<<1)+((ldr_)<<2)); \
   rB02 = (rB02 - U01*rB12 - U02*rB22 - U03*rB32) * U00; \
}  /* complete M=4, N=3 solve block */
#elif NRHS == 4
/*
 * Solve 4x4 L with 4 RHS symbolically
 * Answer is output into rBxx regs, which are live on input and output
 */
#define  ATL_trsmL4(L_, ldl_, r_, ldr_) \
{ \
   const RTYPE L00=(*(L_)), L10=L_[1], L20=L_[2], L30=L_[3]; \
   const RTYPE L11=L_[ldl_+1], L21=L_[ldl_+2], L31=a[ldl_+3]; \
   const RTYPE L22=L_[2*(ldl_)+2], L32=L_[2*(ldl_)+3]; \
   const RTYPE L33=L_[3*(ldl_)+3]; \
/* \
 * x0 = b0 / L00 \
 */ \
   rB00 *= L00; \
   rB01 *= L00; \
   rB02 *= L00; \
   rB03 *= L00; \
   ATL_pfl1W(r_ + ((ldr_)<<2)); \
/* \
 * x1 = (b1 - L10 * x0) / L11 \
 */ \
   rB10 = (rB10 - L10*rB00) * L11;  \
   rB11 = (rB11 - L10*rB01) * L11; \
   rB12 = (rB12 - L10*rB02) * L11; \
   rB13 = (rB13 - L10*rB03) * L11; \
   ATL_pfl1W(r_ + ldr_ +((ldr_)<<2)); \
/* \
 * x2 = (b2 - L20*x0 - L21*x1) / L22 \
 */ \
   rB20 = (rB20 - L20*rB00 - L21*rB10) * L22; \
   rB21 = (rB21 - L20*rB01 - L21*rB11) * L22; \
   rB22 = (rB22 - L20*rB02 - L21*rB12) * L22; \
   rB23 = (rB23 - L20*rB03 - L21*rB13) * L22; \
   ATL_pfl1W(r_ + ((ldr_)<<1)+((ldr_)<<2)); \
/* \
 * x3 = (b3 - L30*x0 - L31*x1 - L32*x2) / L33 \
 */ \
   rB30 = (rB30 - L30*rB00 - L31*rB10 - L32*rB20) * L33; \
   rB31 = (rB31 - L30*rB01 - L31*rB11 - L32*rB21) * L33; \
   ATL_pfl1W(r_ + ldr_+((ldr_)<<1)+((ldr_)<<2)); \
   rB32 = (rB32 - L30*rB02 - L31*rB12 - L32*rB22) * L33; \
   rB33 = (rB33 - L30*rB03 - L31*rB13 - L32*rB23) * L33; \
}  /* complete 4x4 solve block */

#define  ATL_trsmU4(U_, ldu_, r_, ldr_) \
{ \
   const RTYPE U00=(*(U_)); \
   const RTYPE U01=(U_)[ldu_], U11=(U_)[ldu_+1]; \
   const RTYPE U02=(U_)[2*(ldu_)], U12= *(U_+2*(ldu_)+1),  \
               U22 = *(U_+2*(ldu_)+2); \
   const RTYPE U03 = *(U_+3*(ldu_)), U13 = *(U_+3*(ldu_)+1), \
               U23 = *(U_+3*(ldu_)+2), U33 = *(U_+3*(ldu_)+3); \
\
/* \
 * x3 = b3 / U33 \
 */ \
   rB30 *= U33; \
   rB31 *= U33; \
   rB32 *= U33; \
   rB33 *= U33; \
   ATL_pfl1W(r_ + ((ldr_)<<2)); \
/* \
 * x2 = (b2 - U23 * x3) / U22 \
 */ \
   rB20 = (rB20 - U23*rB30) * U22;  \
   rB21 = (rB21 - U23*rB31) * U22; \
   rB22 = (rB22 - U23*rB32) * U22; \
   rB23 = (rB23 - U23*rB33) * U22; \
   ATL_pfl1W(r_ + ldr_+((ldr_)<<2)); \
/* \
 * x1 = (b1 - U12*x2 - U13*x3) / U11 \
 */ \
   rB10 = (rB10 - U12*rB20 - U13*rB30) * U11; \
   rB11 = (rB11 - U12*rB21 - U13*rB31) * U11; \
   rB12 = (rB12 - U12*rB22 - U13*rB32) * U11; \
   rB13 = (rB13 - U12*rB23 - U13*rB33) * U11; \
   ATL_pfl1W(r_ + ((ldr_)<<1)+((ldr_)<<2)); \
/* \
 * x0 = (b0 - U01*x1 - U02*x2 - U03*x3) / U00 \
 */ \
   rB00 = (rB00 - U01*rB10 - U02*rB20 - U03*rB30) * U00; \
   rB01 = (rB01 - U01*rB11 - U02*rB21 - U03*rB31) * U00; \
   ATL_pfl1W(r_ + ldr_+((ldr_)<<1)+((ldr_)<<2)); \
   rB02 = (rB02 - U01*rB12 - U02*rB22 - U03*rB32) * U00; \
   rB03 = (rB03 - U01*rB13 - U02*rB23 - U03*rB33) * U00; \
}  /* complete 4x4 solve block */
#endif

static void ATL_trsmRLN
(
   ATL_CINT  M,   /* size of orig triangular matrix A */
   ATL_CINT  N,   /* number of RHS in B */
   const TYPE *A, /* M4xM4 lower matrix A, diag has inverse of original diag */
   TYPE *W        /* M4xN, on input padded B, on output, padded X */
)
{
   int j;
   ATL_CINT M4 = ((M+3)>>2)<<2;

   #define lda M4
/*
 * Loop over RHS, NRHS RHS at a time
 */
   for (j=0; j < N; j += NRHS, W += NRHS*M4)
   {
      const int nb = Mmin(NRHS, N-j);
      int k, i;
      TYPE *w = W;
      const TYPE *a;
/*
 *    Completely solve these RHSs by looping over entire triangular matrix
 */
      w = W;
      a = A;
      for (k=0; k < M; k += 4, w += 4, a += (lda+1)<<2)
      {
         ATL_CINT mr = Mmin(4,M-k);
         RTYPE rB00 = *w, rB10=w[1], rB20=w[2], rB30=w[3];
         RTYPE rB01=w[M4], rB11=w[M4+1], rB21=w[M4+2], rB31=w[M4+3];
         #if NRHS > 2
            RTYPE rB02=w[2*M4],rB12=w[2*M4+1],rB22=w[2*M4+2],rB32=w[2*M4+3];
         #endif
         #if NRHS > 3
            RTYPE rB03=w[3*M4],rB13=w[3*M4+1],rB23=w[3*M4+2],rB33=w[3*M4+3];
         #endif
/*
 *       Solve M=4 NRHS=4 TRSM symbolically
 */
         ATL_trsmL4(a, lda, w, M4);
/*
 *       Write solved 4xNRHS block out to workspace (final answer)
 */
         *w = rB00;
         w[1] = rB10;
         w[2] = rB20;
         w[3] = rB30;
         w[M4] = rB01;
         w[M4+1] = rB11;
         w[M4+2] = rB21;
         w[M4+3] = rB31;
         #if NRHS > 2
            w[M4+M4] = rB02;
            w[M4+M4+1] = rB12;
            w[M4+M4+2] = rB22;
            w[M4+M4+3] = rB32;
         #endif
         #if NRHS > 3
            w[3*M4] = rB03;
            w[3*M4+1] = rB13;
            w[3*M4+2] = rB23;
            w[3*M4+3] = rB33;
         #endif
/*
 *       Subtract off x0-x4 contribution from rest of B using rank-4 update
 */
         #if ATL_BINWRK
            ATL_rk4(M4-k-4, a+4, lda, w, M4, w+4, M4);
         #else
            ATL_rk4(M4-k-4, a+4, lda, w+4, M4);
         #endif
      }     /* end k-loop that loops through L */
   }        /* end j-loop over RHS */
   #undef lda
}           /* end routine */

static void ATL_trsmRUN
(
   ATL_CINT  M,         /* size of orig triangular matrix A */
   ATL_CINT  N,         /* number of RHS in W */
   const TYPE *A,       /* M4xM4 Upper matrix A, diag is inverted */
   TYPE *W              /* M4xN workspace with good alignment */
)
{
   int j;
   ATL_CINT M4 = ((M+3)>>2)<<2, mr = M4-M;
/*
 * Loop over RHS, NRHS RHS at a time
 */
   for (j=0; j < N; j += NRHS, W += NRHS*M4)
   {
      const int nb = Mmin(NRHS, N-j);
      int k, i;
      TYPE *w = W;
      const TYPE *Ac = A + (M4-4)*M4, *a = Ac + M4-4;
/*
 *    Completely solve these RHSs by looping over entire triangular matrix
 */
      w = W + M4-4;
      for (k=0; k < M; k += 4, w -= 4, a -= (M4+1)<<2, Ac -= M4<<2)
      {
         ATL_CINT mr = Mmin(4,M-k);
         RTYPE rB00 = *w, rB10=w[1], rB20=w[2], rB30=w[3];
         RTYPE rB01=w[M4], rB11=w[M4+1], rB21=w[M4+2], rB31=w[M4+3];
         #if NRHS > 2
            RTYPE rB02=w[2*M4],rB12=w[2*M4+1],rB22=w[2*M4+2],rB32=w[2*M4+3];
         #endif
         #if NRHS > 3
            RTYPE rB03=w[3*M4],rB13=w[3*M4+1],rB23=w[3*M4+2],rB33=w[3*M4+3];
         #endif
/*
 *       Solve M=4 NRHS=4 TRSM symbolically
 */
         ATL_trsmU4(a, M4, w, M4);
/*
 *       Store solved 4xNRHS elts of X to workspace
 */
         *w = rB00;
         w[1] = rB10;
         w[2] = rB20;
         w[3] = rB30;
         w[M4] = rB01;
         w[M4+1] = rB11;
         w[M4+2] = rB21;
         w[M4+3] = rB31;
         #if NRHS > 2
            w[2*M4] = rB02;
            w[2*M4+1] = rB12;
            w[2*M4+2] = rB22;
            w[2*M4+3] = rB32;
         #endif
         #if NRHS > 3
            w[3*M4] = rB03;
            w[3*M4+1] = rB13;
            w[3*M4+2] = rB23;
            w[3*M4+3] = rB33;
         #endif
/*
 *       Subtract off x0-x4 contribution from rest of B using rank-4 update
 */
         #if ATL_BINWRK
            ATL_rk4(M4-k-4, Ac, M4, w, M4, W, M4);
         #else
            ATL_rk4(M4-k-4, Ac, M4, W, M4);
         #endif
      }     /* end k-loop that loops through L */
   }        /* end j-loop over RHS */
}           /* end routine */

ATL_SINLINE void trL2U
   (ATL_CINT N, const TYPE *L, ATL_CINT ldl, TYPE *U, ATL_CINT ldu)
/*
 * reflects lower part of L into upper part of U
 */
{
   const TYPE *Lc=L;
   ATL_INT i, j;

   for (j=0; j < N; j++, Lc += ldl)
   {
      TYPE *Ur = U + j;
      for (i=j; i < N; i++)
         U[j+i*ldu] = Lc[i];
   }
}

ATL_SINLINE void trU2L
   (ATL_CINT N, const TYPE *U, ATL_CINT ldu, TYPE *L, ATL_CINT ldl)
/*
 * reflects upper part of U into lower part of L
 */
{
   ATL_INT i, j;
   const TYPE *Uc = U;

   for (j=0; j < N; j++, Uc += ldu)
   {
      TYPE *Lr = L + j;
      for (i=0; i <= j; i++, Lr += ldl)
         *Lr = Uc[i];
   }
}

/*
 * Copy original U to aligned workspace, invert diagonal elts, pad wt I
 * Padding is at top of upper triangular matrix
 */
static void cpypadU
(
   enum ATLAS_DIAG Diag,
   ATL_CINT N,                  /* size of triangular matrix A */
   const TYPE *A,               /* lower triangular matrix */
   ATL_CINT lda,                /* leading dim of A */
   TYPE *a,                     /* cpy of A, padded to N4 with I */
   ATL_CINT N4                  /* leading dim of A */
)
{
   int i, j;
   const int mr = N4-N;

   for (j=0; j < mr; j++, a += N4)
   {
      for (i=0; i < N4; i++)
         a[i] = ATL_rzero;
      a[j] = ATL_rone;
   }
   for (; j < N4; j++, a += N4, A += lda)
   {
      for (i=0; i < mr; i++)
         a[i] = ATL_rzero;
      for (; i < j; i++)
         a[i] = A[i-mr];
      a[j] = (Diag == AtlasNonUnit) ? 1.0 / A[j-mr] : ATL_rone;
   }
}

/*
 * Copy original L to aligned workspace, invert diagonal elts, pad wt I
 */
static void cpypadL
(
   enum ATLAS_DIAG Diag,
   ATL_CINT N,                  /* size of triangular matrix A */
   const TYPE *A,               /* lower triangular matrix */
   ATL_CINT lda,                /* leading dim of A */
   TYPE *a,                     /* cpy of A, padded to N4 with I */
   ATL_CINT N4                  /* leading dim of A */

)
{
   int i, j;

   for (j=0; j < N; j++, a += N4, A += lda)
   {
      a[j] = (Diag == AtlasNonUnit) ? 1.0 / A[j] : ATL_rone;
      for (i=j+1; i < N; i++)
         a[i] = A[i];
      for (; i < N4; i++)
         a[i] = ATL_rzero;
   }
   for (; j < N4; j++, a += N4)
   {
      for (i=0; i < N4; i++)
         a[i] = ATL_rzero;
      a[j] = ATL_rone;
   }
}
/*
 * This routine computes X * op(A) = B by first computing:
 *    op(A)^T * X^T = B^T
 * and then transposing the answer back out.
 */
int Mjoin(PATL,trsmKR_rk4)
(
   enum ATLAS_SIDE Side,
   enum ATLAS_UPLO Uplo,
   enum ATLAS_TRANS TA,
   enum ATLAS_DIAG Diag,
   ATL_CINT  M,         /* number of RHS in B */
   ATL_CINT  N,         /* size of triangular matrix A */
   const SCALAR alpha,  /* scale factor for B */
   const TYPE *A,       /* MxM triangular matrix A */
   ATL_CINT  lda,
   TYPE *B,             /* on input, B, on output X, of A x = b */
   ATL_CINT  ldb        /* leading dim of B */
)
{
   void *vp;
   TYPE *a, *w, *b;
   enum ATLAS_TRANS TRANSA;
   ATL_CINT N4 = ((N+3)>>2)<<2, nr = N4-N;
   ATL_CINT Mc = Mmin(((M+NRHS-1)/NRHS)*NRHS, 36);
   int UPPER = (Uplo == AtlasUpper);
   const int TRANS = (TA == AtlasTrans), UNIT=(Diag == AtlasUnit);
   ATL_INT i, j;

   vp = malloc((Mc*N4+N4*N4)*sizeof(TYPE) + 2*ATL_Cachelen);
   if (!vp)
      return(1);
   a = ATL_AlignPtr(vp);
   w = a+N4*N4;
   w = ATL_AlignPtr(w);
/*
 * If matrix is already transposed, keep Uplo same and just copy to space
 * and invert the diagonal, and swap the transpose setting
 */
   if (TRANS)
   {
     if (UPPER)
        cpypadU(Diag, N, A, lda, a, N4);
     else
        cpypadL(Diag, N, A, lda, a, N4);
   }
/*
 * If the matrix is presently in NoTranspose format, transpose it, which
 * will swap Uplo setting
 */
   else
   {
      if (UPPER)
      {
         TYPE *c=a;
         trU2L(N, A, lda, a, N4);  /* transpose A into lower-triangular a */
/*
 *       invert diagonal and pad N4-N gap at bottom of Lower matrix
 */
         for (j=0; j < N; j++, c += N4)
         {
            c[j] = (UNIT) ? ATL_rone : ATL_rone / c[j];
            for (i=N; i < N4; i++)
               c[i] = ATL_rzero;
         }
/*
 *       Pad last N4-N columns with identity matrix
 */
         for (; j < N4; j++, c += N4)
         {
            for (i=0; i < N4; i++)
               c[i] = ATL_rzero;
            c[j] = ATL_rone;
         }
      }
      else  /* matrix is currently lower */
      {
         TYPE *c=a+nr*(N4+1);
/*
 *       Transpose Lower matrix to Upper; padding goes at top of Upper matrix
 */
         trL2U(N, A, lda, c, N4);
/*
 *       Invert diagonal elements
 */
         if (UNIT)
            for (j=nr; j < N4; j++, c += N4+1)
               *c = ATL_rone;
         else
            for (j=nr; j < N4; j++, c += N4+1)
               *c = ATL_rone / *c;

/*
 *       If N != N4, then we must pad the matrix
 */
         if (nr)
         {
            c = a;
/*
 *          Pad first nr cols with identity matrix
 */
            for (j=0; j < nr; j++, c += N4)
            {
               for (i=0; i < N4; i++)
                  c[i] = ATL_rzero;
               c[j] = ATL_rone;
            }
/*
 *          Pad first nr rows of remaining columns with zeros
 */
            for (; j < N4; j++, c += N4)
            {
               for (i=0; i < nr; i++)
                  c[i] = ATL_rzero;
            }
         }
      }
      UPPER = !UPPER;  /* transposition swaps UPLO setting */
   }
/*
 * a now contains correctly padded A^T, so now loop over Mc rows of RHS;
 * Mc is set to a value near 32 so we can get some TLB reuse when doing
 * the transpose of the RHS vectors from row-access to column-access
 */
   b = (UPPER) ? w + nr : w;
   for (i=0; i < M; i += Mc, B += Mc)
   {
      const int mr = Mmin(M-i, Mc);
      Mjoin(PATL,gemoveT)(N, mr, alpha, B, ldb, b, N4);
      if (UPPER)
      {
         if (nr)
            Mjoin(PATL,gezero)(nr, mr, w, N4);
         ATL_trsmRUN(N, mr, a, w);
      }
      else
      {
         if (nr)
            Mjoin(PATL,gezero)(nr, mr, w+N, N4);
         ATL_trsmRLN(N, mr, a, w);
      }
      Mjoin(PATL,gemoveT)(mr, N, ATL_rone, b, N4, B, ldb);
   }
   free(vp);
   return(0);
}
