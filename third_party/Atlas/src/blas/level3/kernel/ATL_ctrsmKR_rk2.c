#include "atlas_misc.h"
#include "atlas_prefetch.h"
#define RTYPE register TYPE

#if defined(__GNUC__) || \
    (defined(__STDC_VERSION__) && (__STDC_VERSION__/100 >= 1999))
   #define ATL_SINLINE static inline
#else
   #define ATL_SINLINE static
#endif
#if defined(ATL_AVX) && defined(DCPLX)
   #define NRHS 3
   #define TRU  2     /* this kernel requires A padded to vector length */
   #include <immintrin.h>
/*
 * Subtract off x0 & x1 contribution to all remaining equations using a
 * rank-2 update with mu=2, nu=3, ku=2.  This version is for 16 AVX regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable vectorization & software pipelining of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk2(ATL_CINT M, const TYPE *pA0, ATL_CINT lda0,
                           TYPE *pB0, ATL_CINT ldb0, TYPE *C, ATL_CINT ldc0)
{
   ATL_CINT lda=lda0+lda0, ldb=ldb0+ldb0, ldc=ldc0+ldc0;
   const TYPE *pA1 = pA0+lda;
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1);
   TYPE *pB2 = pB0 + (ldb<<1);
   ATL_CINT MM =  (M & 2) ? M-2 : M-4;
   int i;
   register __m256d rB00, iB00, rB01, iB01, rB02, iB02;
   register __m256d C00, C01, C02;
   register __m256d C20, C21, C22;
   register __m256d A, a;

   a = _mm256_set1_pd(ATL_rnone);
   rB00 = _mm256_set_pd(pB0[2], *pB0, pB0[2], *pB0);
                                                /* rB10 rB00 rB10 rB00 */
   rB00 = _mm256_mul_pd(a, rB00);
                                                /* negate B for alpha=-1 */
   iB00 = _mm256_set_pd(pB0[3], pB0[1], pB0[3], pB0[1]);
                                                /* iB10 iB00 iB10 iB00 */
   iB00 = _mm256_mul_pd(a, iB00);
                                                /* negate B for alpha=-1 */

   rB01 = _mm256_set_pd(pB0[ldb+2], pB0[ldb], pB0[ldb+2], pB0[ldb]);
                                                /* rB10 rB00 rB10 rB00 */
   rB01 = _mm256_mul_pd(a, rB01);
                                                /* negate B for alpha=-1 */
   iB01 = _mm256_set_pd(pB0[ldb+3], pB0[ldb+1], pB0[ldb+3], pB0[ldb+1]);
                                                /* iB10 iB00 iB10 iB00 */
   iB01 = _mm256_mul_pd(a, iB01);               /* negate B for alpha=-1 */

   rB02 = _mm256_set_pd(pB2[2], *pB2, pB2[2], *pB2);
                                                /* rB12 rB02 rB12 rB02 */
   rB02 = _mm256_mul_pd(a, rB02);
                                                /* negate B for alpha=-1 */
   iB02 = _mm256_set_pd(pB2[3], pB2[1], pB2[3], pB2[1]);
                                                /* iB10 iB00 iB10 iB00 */
   iB02 = _mm256_mul_pd(a, iB02);
                                                /* negate B for alpha=-1 */

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
}
#elif defined(ATL_SSE3) && defined(DCPLX)
   #define NRHS 3
   #define TRU 2
   #include <xmmintrin.h>
   #include <pmmintrin.h>
/*
 * Subtract off x0 & x1 contribution to all remaining equations using a
 * rank-2 update with mu=2, nu=4, ku=2.  This version is for 16 SSE regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable vectorization & software pipelining of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk2(ATL_CINT M, const TYPE *pA0, ATL_CINT lda0,
                           TYPE *pB0, ATL_CINT ldb0, TYPE *C, ATL_CINT ldc0)
{
   ATL_CINT lda=lda0+lda0, ldb=ldb0+ldb0, ldc=ldc0+ldc0;
   const TYPE *pA1 = pA0+lda;
   const TYPE none = ATL_rnone;
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1);
   ATL_INT i;
   ATL_CINT MM = (M&1) ? M-1 : M-2;
   register __m128d B00, B10, B01, B11, B02, B12;
   register __m128d C00, C01, C02, C10, C11, C12;
   register __m128d A, a;

   A = _mm_loaddup_pd(&none);
   B00 = _mm_load_pd(pB0);
   B00 = _mm_mul_pd(B00, A);
   B10 = _mm_load_pd(pB0+2);
   B10 = _mm_mul_pd(B10, A);
   B01 = _mm_load_pd(pB0+ldb);
   B01 = _mm_mul_pd(B01, A);
   B11 = _mm_load_pd(pB0+ldb+2);
   B11 = _mm_mul_pd(B11, A);
   B02 = _mm_load_pd(pB0+ldb+ldb);
   B02 = _mm_mul_pd(B02, A);
   B12 = _mm_load_pd(pB0+ldb+ldb+2);    	/* iB12, rB12 */
   B12 = _mm_mul_pd(B12, A);


   C00 = _mm_load_pd(pC0);
   C01 = _mm_load_pd(pC1);
   C02 = _mm_load_pd(pC2);
   A = _mm_load_pd(pA0);                		/* iA00, rA00 */
   for (i=0; i < MM; i += 2, pA0 += 4, pA1 += 4, pC0 += 4, pC1 += 4, pC2 += 4)
   {
      register __m128d b;
/*
 *    K=0, M=[0,1], apply real components of B0x
 */
      b = _mm_movedup_pd(B00);			/* rB00,      rB00 */
      b = _mm_mul_pd(b, A);                     /* iA00*rB00, rA00*rB00 */
      C00 = _mm_add_pd(C00, b);
         a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA00, iA00 */
      b = _mm_movedup_pd(B01);
      b = _mm_mul_pd(b, A);
      C01 = _mm_add_pd(C01, b);
         C10 = _mm_load_pd(pC0+2);
      b = _mm_movedup_pd(B02);
      b = _mm_mul_pd(b, A);
      C02 = _mm_add_pd(C02, b);
         A = _mm_load_pd(pA1);                		/* iA01, rA01 */
/*
 *    K=0, M=0, apply imaginary components of B0x
 */
      b = (__m128d)_mm_shuffle_epi32((__m128i)B00, 0xEE); /* iB00, iB00 */
      b = _mm_mul_pd(b, a);                     /* rA00*iB00, iA00*iB00 */
      C00 = _mm_addsub_pd(C00, b);
         C11 = _mm_load_pd(pC1+2);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B01, 0xEE);
      b = _mm_mul_pd(b, a);
      C01 = _mm_addsub_pd(C01, b);
         C12 = _mm_load_pd(pC2+2);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B02, 0xEE);
      b = _mm_mul_pd(b, a);
      C02 = _mm_addsub_pd(C02, b);
/*
 *    K=1, M=0, apply real components of B1x
 */
      b = _mm_movedup_pd(B10);			/* rB10,      rB10 */
      b = _mm_mul_pd(b, A);                     /* iA01*rB10, rA01*rB10 */
      C00 = _mm_add_pd(C00, b);
      a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA01, iA01 */
      b = _mm_movedup_pd(B11);
      b = _mm_mul_pd(b, A);
      C01 = _mm_add_pd(C01, b);
      b = _mm_movedup_pd(B12);
      b = _mm_mul_pd(b, A);
      C02 = _mm_add_pd(C02, b);
         A = _mm_load_pd(pA0+2);                /* iA10, rA10 */
/*
 *    K=1, M=0, apply imaginary components of B1x
 */
      b = (__m128d)_mm_shuffle_epi32((__m128i)B10, 0xEE); /* iB10, iB10 */
      b = _mm_mul_pd(b, a);                     /* rA01*iB10, iA01*iB10 */
      C00 = _mm_addsub_pd(C00, b);
         _mm_store_pd(pC0, C00);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B11, 0xEE);
      b = _mm_mul_pd(b, a);
      C01 = _mm_addsub_pd(C01, b);
         _mm_store_pd(pC1, C01);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B12, 0xEE);
      b = _mm_mul_pd(b, a);
      C02 = _mm_addsub_pd(C02, b);
         _mm_store_pd(pC2, C02);
/*
 *    K=0, M=1, apply real components of B0x
 */
      b = _mm_movedup_pd(B00);			/* rB00,      rB00 */
      b = _mm_mul_pd(b, A);                     /* iA10*rB00, rA10*rB00 */
      C10 = _mm_add_pd(C10, b);
         a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA10, iA10 */
      b = _mm_movedup_pd(B01);
      b = _mm_mul_pd(b, A);
      C11 = _mm_add_pd(C11, b);
         C00 = _mm_load_pd(pC0+4);
      b = _mm_movedup_pd(B02);
      b = _mm_mul_pd(b, A);
      C12 = _mm_add_pd(C12, b);
         A = _mm_load_pd(pA1+2);               		/* iA11, rA11 */
/*
 *    K=0, M=1, apply imaginary components of B0x
 */
      b = (__m128d)_mm_shuffle_epi32((__m128i)B00, 0xEE); /* iB00, iB00 */
      b = _mm_mul_pd(b, a);                     /* rA10*iB00, iA10*iB00 */
      C10 = _mm_addsub_pd(C10, b);
         C01 = _mm_load_pd(pC1+4);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B01, 0xEE);
      b = _mm_mul_pd(b, a);
      C11 = _mm_addsub_pd(C11, b);
         C02 = _mm_load_pd(pC2+4);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B02, 0xEE);
      b = _mm_mul_pd(b, a);
      C12 = _mm_addsub_pd(C12, b);
/*
 *    K=1, M=1, apply real components of B1x
 */
      b = _mm_movedup_pd(B10);			/* rB10,      rB10 */
      b = _mm_mul_pd(b, A);                     /* iA11*rB10, rA11*rB10 */
      C10 = _mm_add_pd(C10, b);
      a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA11, iA11 */
      b = _mm_movedup_pd(B11);
      b = _mm_mul_pd(b, A);
      C11 = _mm_add_pd(C11, b);
      b = _mm_movedup_pd(B12);
      b = _mm_mul_pd(b, A);
      C12 = _mm_add_pd(C12, b);
         A = _mm_load_pd(pA0+4);               		/* iA20, rA20 */
/*
 *    K=1, M=1, apply imaginary components of B1x
 */
      b = (__m128d)_mm_shuffle_epi32((__m128i)B10, 0xEE); /* iB10, iB10 */
      b = _mm_mul_pd(b, a);                     /* rA11*iB10, iA11*iB10 */
      C10 = _mm_addsub_pd(C10, b);
         _mm_store_pd(pC0+2, C10);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B11, 0xEE);
      b = _mm_mul_pd(b, a);
      C11 = _mm_addsub_pd(C11, b);
         _mm_store_pd(pC1+2, C11);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B12, 0xEE);
      b = _mm_mul_pd(b, a);
      C12 = _mm_addsub_pd(C12, b);
         _mm_store_pd(pC2+2, C12);
   }
/*
 * Drain pipes
 */
   {
      register __m128d b;
/*
 *    K=0, M=[0,1], apply real components of B0x
 */
      b = _mm_movedup_pd(B00);			/* rB00,      rB00 */
      b = _mm_mul_pd(b, A);                     /* iA00*rB00, rA00*rB00 */
      C00 = _mm_add_pd(C00, b);
         a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA00, iA00 */
      b = _mm_movedup_pd(B01);
      b = _mm_mul_pd(b, A);
      C01 = _mm_add_pd(C01, b);
      b = _mm_movedup_pd(B02);
      b = _mm_mul_pd(b, A);
      C02 = _mm_add_pd(C02, b);
         A = _mm_load_pd(pA1);                		/* iA01, rA01 */
/*
 *    K=0, M=0, apply imaginary components of B0x
 */
      b = (__m128d)_mm_shuffle_epi32((__m128i)B00, 0xEE); /* iB00, iB00 */
      b = _mm_mul_pd(b, a);                     /* rA00*iB00, iA00*iB00 */
      C00 = _mm_addsub_pd(C00, b);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B01, 0xEE);
      b = _mm_mul_pd(b, a);
      C01 = _mm_addsub_pd(C01, b);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B02, 0xEE);
      b = _mm_mul_pd(b, a);
      C02 = _mm_addsub_pd(C02, b);
/*
 *    K=1, M=0, apply real components of B1x
 */
      b = _mm_movedup_pd(B10);			/* rB10,      rB10 */
      b = _mm_mul_pd(b, A);                     /* iA01*rB10, rA01*rB10 */
      C00 = _mm_add_pd(C00, b);
      a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA01, iA01 */
      b = _mm_movedup_pd(B11);
      b = _mm_mul_pd(b, A);
      C01 = _mm_add_pd(C01, b);
      b = _mm_movedup_pd(B12);
      b = _mm_mul_pd(b, A);
      C02 = _mm_add_pd(C02, b);
/*
 *    K=1, M=0, apply imaginary components of B1x
 */
      b = (__m128d)_mm_shuffle_epi32((__m128i)B10, 0xEE); /* iB10, iB10 */
      b = _mm_mul_pd(b, a);                     /* rA01*iB10, iA01*iB10 */
      C00 = _mm_addsub_pd(C00, b);
         _mm_store_pd(pC0, C00);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B11, 0xEE);
      b = _mm_mul_pd(b, a);
      C01 = _mm_addsub_pd(C01, b);
         _mm_store_pd(pC1, C01);
      b = (__m128d)_mm_shuffle_epi32((__m128i)B12, 0xEE);
      b = _mm_mul_pd(b, a);
      C02 = _mm_addsub_pd(C02, b);
         _mm_store_pd(pC2, C02);
      if (!(M&1))
      {
         C10 = _mm_load_pd(pC0+2);
         C11 = _mm_load_pd(pC1+2);
         C12 = _mm_load_pd(pC2+2);
         A = _mm_load_pd(pA0+2);                /* iA10, rA10 */
/*
 *       K=0, M=1, apply real components of B0x
 */
         b = _mm_movedup_pd(B00);			/* rB00,      rB00 */
         b = _mm_mul_pd(b, A);                     /* iA10*rB00, rA10*rB00 */
         C10 = _mm_add_pd(C10, b);
            a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA10, iA10 */
         b = _mm_movedup_pd(B01);
         b = _mm_mul_pd(b, A);
         C11 = _mm_add_pd(C11, b);
         b = _mm_movedup_pd(B02);
         b = _mm_mul_pd(b, A);
         C12 = _mm_add_pd(C12, b);
            A = _mm_load_pd(pA1+2);               		/* iA11, rA11 */
/*
 *       K=0, M=1, apply imaginary components of B0x
 */
         b = (__m128d)_mm_shuffle_epi32((__m128i)B00, 0xEE); /* iB00, iB00 */
         b = _mm_mul_pd(b, a);                     /* rA10*iB00, iA10*iB00 */
         C10 = _mm_addsub_pd(C10, b);
         b = (__m128d)_mm_shuffle_epi32((__m128i)B01, 0xEE);
         b = _mm_mul_pd(b, a);
         C11 = _mm_addsub_pd(C11, b);
         b = (__m128d)_mm_shuffle_epi32((__m128i)B02, 0xEE);
         b = _mm_mul_pd(b, a);
         C12 = _mm_addsub_pd(C12, b);
/*
 *       K=1, M=1, apply real components of B1x
 */
         b = _mm_movedup_pd(B10);			/* rB10,      rB10 */
         b = _mm_mul_pd(b, A);                     /* iA11*rB10, rA11*rB10 */
         C10 = _mm_add_pd(C10, b);
         a = (__m128d)_mm_shuffle_epi32((__m128i)A, 0x4E);  	/* rA11, iA11 */
         b = _mm_movedup_pd(B11);
         b = _mm_mul_pd(b, A);
         C11 = _mm_add_pd(C11, b);
         b = _mm_movedup_pd(B12);
         b = _mm_mul_pd(b, A);
         C12 = _mm_add_pd(C12, b);
/*
 *       K=1, M=1, apply imaginary components of B1x
 */
         b = (__m128d)_mm_shuffle_epi32((__m128i)B10, 0xEE); /* iB10, iB10 */
         b = _mm_mul_pd(b, a);                     /* rA11*iB10, iA11*iB10 */
         C10 = _mm_addsub_pd(C10, b);
            _mm_store_pd(pC0+2, C10);
         b = (__m128d)_mm_shuffle_epi32((__m128i)B11, 0xEE);
         b = _mm_mul_pd(b, a);
         C11 = _mm_addsub_pd(C11, b);
            _mm_store_pd(pC1+2, C11);
         b = (__m128d)_mm_shuffle_epi32((__m128i)B12, 0xEE);
         b = _mm_mul_pd(b, a);
         C12 = _mm_addsub_pd(C12, b);
            _mm_store_pd(pC2+2, C12);
      }
   }
}
#elif defined(ATL_SSE3) && defined(SCPLX)
   #define NRHS 4
   #define TRU  4
   #include <xmmintrin.h>
   #include <pmmintrin.h>
/*
 * Subtract off x0 & x1 contribution to all remaining equations using a
 * rank-2 update with mu=2, nu=4, ku=2.  This version is for 16 SSE regs.
 * nu is the # of RHS, ku is the number of equations solved, and mu is
 * unrolled only to enable vectorization & software pipelining of load/use.
 * Loop order is MKN, so that B is kept completely in registers, and
 * C and A are streamed in (and out, for C) from cache during the operation.
 */
ATL_SINLINE void ATL_rk2(ATL_CINT M, const TYPE *pA0, ATL_CINT lda0,
                           TYPE *pB0, ATL_CINT ldb0, TYPE *C, ATL_CINT ldc0)
{
   ATL_CINT lda=lda0+lda0, ldb=ldb0+ldb0, ldc=ldc0+ldc0;
   const TYPE *pA1 = pA0+lda;
   TYPE *pC0 = C, *pC1 = C+ldc, *pC2 = C+((ldc)<<1), *pC3 = pC2+ldc;
   ATL_INT i;
   ATL_CINT MM = (M&2) ? M-2 : M-4;
   register __m128 B00, B01, B02, B03;
   register __m128 C00, C01, C02, C03;
   register __m128 C20, C21, C22, C23;
   register __m128 A, a;

   A = _mm_set1_ps(ATL_rnone);
   B00 = _mm_load_ps(pB0);              /* iB10 rB10 iB00 rB00 */
   B00 = _mm_mul_ps(B00, A);            /* negate */
   B01 = _mm_load_ps(pB0+ldb);
   B01 = _mm_mul_ps(B01, A);
   B02 = _mm_load_ps(pB0+ldb+ldb);
   B02 = _mm_mul_ps(B02, A);
   B03 = _mm_load_ps(pB0+ldb+ldb+ldb);
   B03 = _mm_mul_ps(B03, A);

   C00 = _mm_load_ps(pC0);
   C01 = _mm_load_ps(pC1);
   C02 = _mm_load_ps(pC2);
   C03 = _mm_load_ps(pC3);
   A = _mm_load_ps(pA0);                /* iA10 rA10 iA00 rA00 */
   a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
       /* rA10 iA10 rA00 iA00 */
   for (i=0; i < MM; i += 4, pA0 += 8, pA1 += 8,
        pC0 += 8, pC1 += 8, pC2 += 8, pC3 += 8)
   {
      register __m128 b;
/*
 *    M=K=0 block, multiply by real B vals
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x00);
                                        /* rB00 rB00 rB00 rB00 */
      b = _mm_mul_ps(b, A);
      C00 = _mm_add_ps(C00, b);  C20 = _mm_load_ps(pC0+4);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x00);
      b = _mm_mul_ps(b, A);
      C01 = _mm_add_ps(C01, b);  C21 = _mm_load_ps(pC1+4);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x00);
      b = _mm_mul_ps(b, A);
      C02 = _mm_add_ps(C02, b);  C22 = _mm_load_ps(pC2+4);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x00);
      b = _mm_mul_ps(b, A);
      C03 = _mm_add_ps(C03, b); A = _mm_load_ps(pA1);
/*
 *    M=K=0 block, multiply by imaginary B vals
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x55);
                                        /* iB00 iB00, iB00, iB00 */
      b = _mm_mul_ps(b, a);
      C00 = _mm_addsub_ps(C00, b);  C23 = _mm_load_ps(pC3+4);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x55);
      b = _mm_mul_ps(b, a);
      C01 = _mm_addsub_ps(C01, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x55);
      b = _mm_mul_ps(b, a);
      C02 = _mm_addsub_ps(C02, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x55);
      b = _mm_mul_ps(b, a);
      C03 = _mm_addsub_ps(C03, b);
      a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
/*
 *    M=0,K=1, multiply by real B values
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xAA);
                                                /* rB10 rB10, rB10, rBi0 */
      b = _mm_mul_ps(b, A);
      C00 = _mm_add_ps(C00, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xAA);
      b = _mm_mul_ps(b, A);
      C01 = _mm_add_ps(C01, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xAA);
      b = _mm_mul_ps(b, A);
      C02 = _mm_add_ps(C02, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xAA);
      b = _mm_mul_ps(b, A);
      C03 = _mm_add_ps(C03, b); A = _mm_load_ps(pA0+4);
/*
 *    M=0,K=1, multiply by imaginary B values
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xFF);
                                                /* iB10 iB10, iB10, iB10 */
      b = _mm_mul_ps(b, a);
      C00 = _mm_addsub_ps(C00, b); _mm_store_ps(pC0, C00);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xFF);
      b = _mm_mul_ps(b, a);
      C01 = _mm_addsub_ps(C01, b); _mm_store_ps(pC1, C01);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xFF);
      b = _mm_mul_ps(b, a);
      C02 = _mm_addsub_ps(C02, b); _mm_store_ps(pC2, C02);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xFF);
      b = _mm_mul_ps(b, a);
      C03 = _mm_addsub_ps(C03, b);
      a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
/*
 *    M=2,K=0 block, multiply by real B vals
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x00);
                                                /* rB00 rB00 rB00 rB00 */
      b = _mm_mul_ps(b, A);
      C20 = _mm_add_ps(C20, b); _mm_store_ps(pC3, C03);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x00);
      b = _mm_mul_ps(b, A);
      C21 = _mm_add_ps(C21, b);  C00 = _mm_load_ps(pC0+8);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x00);
      b = _mm_mul_ps(b, A);
      C22 = _mm_add_ps(C22, b);  C01 = _mm_load_ps(pC1+8);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x00);
      b = _mm_mul_ps(b, A);
      C23 = _mm_add_ps(C23, b); A = _mm_load_ps(pA1+4);
/*
 *    M=2,K=0 block, multiply by imaginary B vals
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x55);
                                                /* iB00 iB00, iB00, iB00 */
      b = _mm_mul_ps(b, a);
      C20 = _mm_addsub_ps(C20, b);  C02 = _mm_load_ps(pC2+8);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x55);
      b = _mm_mul_ps(b, a);
      C21 = _mm_addsub_ps(C21, b);  C03 = _mm_load_ps(pC3+8);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x55);
      b = _mm_mul_ps(b, a);
      C22 = _mm_addsub_ps(C22, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x55);
      b = _mm_mul_ps(b, a);
      C23 = _mm_addsub_ps(C23, b);
      a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
/*
 *    M=2, K=1, multiply by real B values
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xAA);
                                                /* rB10 rB10, rB10, rBi0 */
      b = _mm_mul_ps(b, A);
      C20 = _mm_add_ps(C20, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xAA);
      b = _mm_mul_ps(b, A);
      C21 = _mm_add_ps(C21, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xAA);
      b = _mm_mul_ps(b, A);
      C22 = _mm_add_ps(C22, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xAA);
      b = _mm_mul_ps(b, A);
      C23 = _mm_add_ps(C23, b); A = _mm_load_ps(pA0+8);
/*
 *    M=2, K=1, multiply by imaginary B values
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xFF);
                                                /* iB10 iB10, iB10, iB10 */
      b = _mm_mul_ps(b, a);
      C20 = _mm_addsub_ps(C20, b);  _mm_store_ps(pC0+4, C20);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xFF);
      b = _mm_mul_ps(b, a);
      C21 = _mm_addsub_ps(C21, b);  _mm_store_ps(pC1+4, C21);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xFF);
      b = _mm_mul_ps(b, a);
      C22 = _mm_addsub_ps(C22, b);  _mm_store_ps(pC2+4, C22);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xFF);
      b = _mm_mul_ps(b, a);
      C23 = _mm_addsub_ps(C23, b);  _mm_store_ps(pC3+4, C23);
      a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
   }
/*
 * Drain pipe
 */
   {
      register __m128 b;
/*
 *    M=K=0 block, multiply by real B vals
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x00);
          /* rB00 rB00 rB00 rB00 */
      b = _mm_mul_ps(b, A);
          /* iA10*rB00 rA10*rB00 iA00*rB00 rA00*rB00 */
      C00 = _mm_add_ps(C00, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x00);
      b = _mm_mul_ps(b, A);
      C01 = _mm_add_ps(C01, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x00);
      b = _mm_mul_ps(b, A);
      C02 = _mm_add_ps(C02, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x00);
      b = _mm_mul_ps(b, A);
      C03 = _mm_add_ps(C03, b); A = _mm_load_ps(pA1);
/*
 *    M=K=0 block, multiply by imaginary B vals
 */
      /* a = rA10 iA10 rA00 iA00 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x55);
                                        /* iB00 iB00, iB00, iB00 */
      b = _mm_mul_ps(b, a); /* rA10*iB00 iA10*iB00 rA00*iB00 iA00*iB00 */
      C00 = _mm_addsub_ps(C00, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x55);
      b = _mm_mul_ps(b, a);
      C01 = _mm_addsub_ps(C01, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x55);
      b = _mm_mul_ps(b, a);
      C02 = _mm_addsub_ps(C02, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x55);
      b = _mm_mul_ps(b, a);
      C03 = _mm_addsub_ps(C03, b);
      a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
/*
 *    M=0,K=1, multiply by real B values
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xAA);
                                                /* rB10 rB10, rB10, rB10 */
      b = _mm_mul_ps(b, A);
      C00 = _mm_add_ps(C00, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xAA);
      b = _mm_mul_ps(b, A);
      C01 = _mm_add_ps(C01, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xAA);
      b = _mm_mul_ps(b, A);
      C02 = _mm_add_ps(C02, b);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xAA);
      b = _mm_mul_ps(b, A);
      C03 = _mm_add_ps(C03, b);
/*
 *    M=0,K=1, multiply by imaginary B values
 */
      b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xFF);
                                                /* iB10 iB10, iB10, iB10 */
      b = _mm_mul_ps(b, a);
      C00 = _mm_addsub_ps(C00, b); _mm_store_ps(pC0, C00);
      b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xFF);
      b = _mm_mul_ps(b, a);
      C01 = _mm_addsub_ps(C01, b); _mm_store_ps(pC1, C01);
      b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xFF);
      b = _mm_mul_ps(b, a);
      C02 = _mm_addsub_ps(C02, b); _mm_store_ps(pC2, C02);
      b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xFF);
      b = _mm_mul_ps(b, a);
      C03 = _mm_addsub_ps(C03, b);  _mm_store_ps(pC3, C03);
/*
 *    M=2,K=0 block, multiply by real B vals
 */
      if (!(M&2))
      {
         A = _mm_load_ps(pA0+4);
         a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
         C20 = _mm_load_ps(pC0+4);
         C21 = _mm_load_ps(pC1+4);
         b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x00);
         C22 = _mm_load_ps(pC2+4);
         C23 = _mm_load_ps(pC3+4);
                                                   /* rB00 rB00 rB00 rB00 */
         b = _mm_mul_ps(b, A);
         C20 = _mm_add_ps(C20, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x00);
         b = _mm_mul_ps(b, A);
         C21 = _mm_add_ps(C21, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x00);
         b = _mm_mul_ps(b, A);
         C22 = _mm_add_ps(C22, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x00);
         b = _mm_mul_ps(b, A);
         C23 = _mm_add_ps(C23, b); A = _mm_load_ps(pA1+4);
/*
 *       M=2,K=0 block, multiply by imaginary B vals
 */
         b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0x55);
                                                   /* iB00 iB00, iB00, iB00 */
         b = _mm_mul_ps(b, a);
         C20 = _mm_addsub_ps(C20, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0x55);
         b = _mm_mul_ps(b, a);
         C21 = _mm_addsub_ps(C21, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0x55);
         b = _mm_mul_ps(b, a);
         C22 = _mm_addsub_ps(C22, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0x55);
         b = _mm_mul_ps(b, a);
         C23 = _mm_addsub_ps(C23, b);
         a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
/*
 *       M=2, K=1, multiply by real B values
 */
         b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xAA);
                                                   /* rB10 rB10, rB10, rBi0 */
         b = _mm_mul_ps(b, A);
         C20 = _mm_add_ps(C20, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xAA);
         b = _mm_mul_ps(b, A);
         C21 = _mm_add_ps(C21, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xAA);
         b = _mm_mul_ps(b, A);
         C22 = _mm_add_ps(C22, b);
         b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xAA);
         b = _mm_mul_ps(b, A);
         C23 = _mm_add_ps(C23, b);
/*
 *       M=2, K=1, multiply by imaginary B values
 */
         b = (__m128)_mm_shuffle_epi32((__m128i)B00, 0xFF);
                                                   /* iB10 iB10, iB10, iB10 */
         b = _mm_mul_ps(b, a);
         C20 = _mm_addsub_ps(C20, b);  _mm_store_ps(pC0+4, C20);
         b = (__m128)_mm_shuffle_epi32((__m128i)B01, 0xFF);
         b = _mm_mul_ps(b, a);
         C21 = _mm_addsub_ps(C21, b);  _mm_store_ps(pC1+4, C21);
         b = (__m128)_mm_shuffle_epi32((__m128i)B02, 0xFF);
         b = _mm_mul_ps(b, a);
         C22 = _mm_addsub_ps(C22, b);  _mm_store_ps(pC2+4, C22);
         b = (__m128)_mm_shuffle_epi32((__m128i)B03, 0xFF);
         b = _mm_mul_ps(b, a);
         C23 = _mm_addsub_ps(C23, b);  _mm_store_ps(pC3+4, C23);
         a = (__m128)_mm_shuffle_epi32((__m128i)A, 0xB1);
      }
   }
}
#else   /* 32 register version goes here */
   #define NRHS 3
   #define TRU  2   /* this kernel requires no padding, but solve needs 2 */
/*
 * This routine optimized for OOE machines; for statically scheduled
 * machines, the asg of rCxx followed immediately by the dependent store will
 * be bad news.  Would need to pipeline another loop iteration to avoid.
 */
ATL_SINLINE void ATL_rk2(ATL_CINT M, const TYPE *pA0, ATL_CINT lda0,
                         TYPE *B, ATL_CINT ldb0, TYPE *pC0, ATL_CINT ldc0)
{
   ATL_CINT lda=lda0+lda0, ldb=ldb0+ldb0, ldc=ldc0+ldc0;
   ATL_CINT MM =  (M & 1) ? M-1 : M-2;
   ATL_INT i;
   TYPE *pC1=pC0+ldc, *pC2=pC0+(ldc<<1), *pC3=pC1+(ldc<<1);
   const TYPE *pA1=pA0+lda;
   const RTYPE rB00=(*B), iB00=B[1], rB01=B[ldb], iB01=B[ldb+1],
               rB02=B[ldb+ldb], iB02=B[ldb+ldb+1];
   const RTYPE rB10=B[2], iB10=B[3], rB11=B[ldb+2], iB11=B[ldb+3],
               rB12=B[ldb+ldb+2], iB12=B[ldb+ldb+3];
   RTYPE rC00, rC01, rC02, rC10, rC11, rC12;
   RTYPE iC00, iC01, iC02, iC10, iC11, iC12;
   RTYPE rA00, rA10, rA01, rA11, iA00, iA10, iA01, iA11;

/*
 * Fetch C and A for M=0
 */
   rC00 = *pC0; iC00 = pC0[1];
   rC01 = *pC1; iC01 = pC1[1];
   rC02 = *pC2; iC02 = pC2[1];
   rA00 = *pA0; iA00 = pA0[1];
   rA01 = *pA1; iA01 = pA1[1];
   for (i=0; i < MM; i += 2, pA0 += 4, pA1 += 4, pC0 += 4, pC1 += 4, pC2 += 4)
   {
/*
 *    M=0, K=0, fetch M=1 C's and then K=1's A's
 */
      rC00 -= rA00 * rB00; rC10 = pC0[2];
      iC00 -= rA00 * iB00; iC10 = pC0[3];
      rC01 -= rA00 * rB01; rC11 = pC1[2];
      iC01 -= rA00 * iB01; iC11 = pC1[3];
      rC02 -= rA00 * rB02; rC12 = pC2[2];
      iC02 -= rA00 * iB02; iC12 = pC2[3];

      rC00 += iA00 * iB00; rA10 = pA0[2];
      iC00 -= iA00 * rB00; iA10 = pA0[3];
      rC01 += iA00 * iB01; rA11 = pA1[2];
      iC01 -= iA00 * rB01; iA11 = pA1[3];
      rC02 += iA00 * iB02;
      iC02 -= iA00 * rB02;
/*
 *    K == 1, and then finished, so store C out
 */
      rC00 -= rA01 * rB10;
      iC00 -= rA01 * iB10;
      rC01 -= rA01 * rB11;
      iC01 -= rA01 * iB11;
      rC02 -= rA01 * rB12;
      iC02 -= rA01 * iB12;

      rC00 += iA01 * iB10; *pC0 = rC00;
      iC00 -= iA01 * rB10; pC0[1] = iC00;
      rC01 += iA01 * iB11; *pC1 = rC01;
      iC01 -= iA01 * rB11; pC1[1] = iC01;
      rC02 += iA01 * iB12; *pC2 = rC02;
      iC02 -= iA01 * rB12; pC2[1] = iC02;
/*
 *    M=1, K=0, fetch M=2's C's and A's
 */
      rC10 -= rA10 * rB00; rC00 = pC0[4];
      iC10 -= rA10 * iB00; iC00 = pC0[5];
      rC11 -= rA10 * rB01; rC01 = pC1[4];
      iC11 -= rA10 * iB01; iC01 = pC1[5];
      rC12 -= rA10 * rB02; rC02 = pC2[4];
      iC12 -= rA10 * iB02; iC02 = pC2[5];

      rC10 += iA10 * iB00; rA00 = pA0[4];
      iC10 -= iA10 * rB00; iA00 = pA0[5];
      rC11 += iA10 * iB01; rA01 = pA1[4];
      iC11 -= iA10 * rB01; iA01 = pA1[5];
      rC12 += iA10 * iB02;
      iC12 -= iA10 * rB02;
/*
 *    M=1, K=1
 */
      rC10 -= rA11 * rB10;
      iC10 -= rA11 * iB10;
      rC11 -= rA11 * rB11;
      iC11 -= rA11 * iB11;
      rC12 -= rA11 * rB12;
      iC12 -= rA11 * iB12;

      rC10 += iA11 * iB10; pC0[2] = rC10;
      iC10 -= iA11 * rB10; pC0[3] = iC10;
      rC11 += iA11 * iB11; pC1[2] = rC11;
      iC11 -= iA11 * rB11; pC1[3] = iC11;
      rC12 += iA11 * iB12; pC2[2] = rC12;
      iC12 -= iA11 * rB12; pC2[3] = iC12;
   }
/*
 * Drain pipeline
 */
/*
 *    M=0, K=0
 */
   rC00 -= rA00 * rB00;
   iC00 -= rA00 * iB00;
   rC01 -= rA00 * rB01;
   iC01 -= rA00 * iB01;
   rC02 -= rA00 * rB02;
   iC02 -= rA00 * iB02;

   rC00 += iA00 * iB00;
   iC00 -= iA00 * rB00;
   rC01 += iA00 * iB01;
   iC01 -= iA00 * rB01;
   rC02 += iA00 * iB02;
   iC02 -= iA00 * rB02;
/*
 * K == 1, and then finished, so store C out
 */
   rC00 -= rA01 * rB10;
   iC00 -= rA01 * iB10;
   rC01 -= rA01 * rB11;
   iC01 -= rA01 * iB11;
   rC02 -= rA01 * rB12;
   iC02 -= rA01 * iB12;

   rC00 += iA01 * iB10; *pC0 = rC00;
   iC00 -= iA01 * rB10; pC0[1] = iC00;
   rC01 += iA01 * iB11; *pC1 = rC01;
   iC01 -= iA01 * rB11; pC1[1] = iC01;
   rC02 += iA01 * iB12; *pC2 = rC02;
   iC02 -= iA01 * rB12; pC2[1] = iC02;
/*
 * M=1, K=0, fetch M=2's C's and A's
 */
   if (!(M&1))
   {
      rC10 = pC0[2];
      iC10 = pC0[3];
      rC11 = pC1[2];
      iC11 = pC1[3];
      rC12 = pC2[2];
      iC12 = pC2[3];
      rA10 = pA0[2];
      iA10 = pA0[3];
      rA11 = pA1[2];
      iA11 = pA1[3];
      rC10 -= rA10 * rB00;
      iC10 -= rA10 * iB00;
      rC11 -= rA10 * rB01;
      iC11 -= rA10 * iB01;
      rC12 -= rA10 * rB02;
      iC12 -= rA10 * iB02;

      rC10 += iA10 * iB00;
      iC10 -= iA10 * rB00;
      rC11 += iA10 * iB01;
      iC11 -= iA10 * rB01;
      rC12 += iA10 * iB02;
      iC12 -= iA10 * rB02;
/*
 *    M=1, K=1
 */
      rC10 -= rA11 * rB10;
      iC10 -= rA11 * iB10;
      rC11 -= rA11 * rB11;
      iC11 -= rA11 * iB11;
      rC12 -= rA11 * rB12;
      iC12 -= rA11 * iB12;

      rC10 += iA11 * iB10; pC0[2] = rC10;
      iC10 -= iA11 * rB10; pC0[3] = iC10;
      rC11 += iA11 * iB11; pC1[2] = rC11;
      iC11 -= iA11 * rB11; pC1[3] = iC11;
      rC12 += iA11 * iB12; pC2[2] = rC12;
      iC12 -= iA11 * rB12; pC2[3] = iC12;
   }
}
#endif

#if NRHS == 3
/*
 * Solve 2x2 L with 3 RHS symbolically for complex arithmetic;
 * Diagonal elements have already been inverted
 */
ATL_SINLINE void ATL_trsmU2(const TYPE *U, ATL_CINT ldu, TYPE *B, ATL_CINT ldb)
{
   ATL_CINT ldU=ldu+ldu, ldB = ldb+ldb, ldB2=(ldb<<2);
   const RTYPE rU00=(*U), iU00=U[1], rU01=U[ldU], iU01=U[ldU+1];
   const RTYPE rU11=U[ldU+2], iU11 = U[ldU+3];
   RTYPE rB00=(*B), iB00=B[1], rB10=B[2], iB10=B[3];
   RTYPE rB01=B[ldB], iB01=B[ldB+1], rB11=B[ldB+2], iB11=B[ldB+3];
   RTYPE rB02=B[ldB2], iB02=B[ldB2+1], rB12=B[ldB2+2], iB12=B[ldB2+3];
   RTYPE rX;
/*
 * x1 = b1 / U11;  U11 is recipricol, so solve x1 = b1 * U11;
 */
   B[2] = rX = rB10 * rU11 - iB10 * iU11;
   B[3] = iB10 = rB10 * iU11 + iB10 * rU11; rB10 = rX;
   B[ldB+2] = rX = rB11 * rU11 - iB11 * iU11;
   B[ldB+3] = iB11 = rB11 * iU11 + iB11 * rU11; rB11 = rX;
   B[ldB2+2] = rX = rB12 * rU11 - iB12 * iU11;
   B[ldB2+3] = iB12 = rB12 * iU11 + iB12 * rU11; rB12 = rX;
/*
 * x0 = (b0 - U01*x1) / U00, do x0 = (b0 - U01*x1) first
 */
   rB00 = rB00 - rU01*rB10 + iU01*iB10;
   iB00 = iB00 - rU01*iB10 - iU01*rB10;
   rB01 = rB01 - rU01*rB11 + iU01*iB11;
   iB01 = iB01 - rU01*iB11 - iU01*rB11;
   rB02 = rB02 - rU01*rB12 + iU01*iB12;
   iB02 = iB02 - rU01*iB12 - iU01*rB12;
/*
 * Finish x0 = (b0 - U01*x1) / U00 by multiplying by U00 (1/U00)
 */
   *B = rB00 * rU00 - iB00 * iU00;
   B[1] = rB00 * iU00 + iB00 * rU00;
   B[ldB]   = rB01 * rU00 - iB01 * iU00;
   B[ldB+1] = rB01 * iU00 + iB01 * rU00;
   B[ldB2]   = rB02 * rU00 - iB02 * iU00;
   B[ldB2+1] = rB02 * iU00 + iB02 * rU00;
}

/*
 * Solve 2x2 L with 3 RHS symbolically for complex arithmetic;
 * Diagonal elements have already been inverted
 */
ATL_SINLINE void ATL_trsmL2(const TYPE *L, ATL_CINT ldl, TYPE *B, ATL_CINT ldb0)
{
   const RTYPE rL00=(*L), iL00=L[1], rL10=L[2], iL10=L[3],
               rL11=L[ldl+ldl+2], iL11=L[ldl+ldl+3];
   ATL_CINT ldb=ldb0+ldb0, ldb2=(ldb0<<2);
   RTYPE rB00=(*B), iB00=B[1], rB10=B[2], iB10=B[3];
   RTYPE rB01=B[ldb], iB01=B[ldb+1], rB11=B[ldb+2], iB11=B[ldb+3];
   RTYPE rB02=B[ldb2], iB02=B[ldb2+1], rB12=B[ldb2+2], iB12=B[ldb2+3];
   RTYPE rX;
/*
 * x0 = b0 / L00
 */
   *B        = rX = rB00*rL00 - iB00*iL00;
   B[1]      = iB00 = rB00*iL00 + iB00*rL00; rB00 = rX;
   B[ldb]    = rX = rB01*rL00 - iB01*iL00;
   B[ldb+1]  = iB01 = rB01*iL00 + iB01*rL00; rB01 = rX;
   B[ldb2]   = rX = rB02*rL00 - iB02*iL00;
   B[ldb2+1] = iB02 = rB02*iL00 + iB02*rL00; rB02 = rX;
/*
 * x1 = (b1 - L10 * x0)  [divide by diagonal in next step]
 */
   rB10 = (rB10 - rL10*rB00 + iL10*iB00);
   iB10 = (iB10 - rL10*iB00 - iL10*rB00);
   rB11 = (rB11 - rL10*rB01 + iL10*iB01);
   iB11 = (iB11 - rL10*iB01 - iL10*rB01);
   rB12 = (rB12 - rL10*rB02 + iL10*iB02);
   iB12 = (iB12 - rL10*iB02 - iL10*rB02);
/*
 * Mult by recipricol of L11 to finish x1 = (b1 - L10 * x0)/L11.
 */
   B[2]      = rB10*rL11 - iB10*iL11;
   B[3]      = rB10*iL11 + iB10*rL11;
   B[ldb+2]  = rB11*rL11 - iB11*iL11;
   B[ldb+3]  = rB11*iL11 + iB11*rL11;
   B[ldb2+2] = rB12*rL11 - iB12*iL11;
   B[ldb2+3] = iB12 = rB12*iL11 + iB12*rL11;
}
#elif NRHS == 4
/*
 * Solve 2x2 L with 4 RHS symbolically for complex arithmetic;
 * Diagonal elements have already been inverted
 */
ATL_SINLINE void ATL_trsmU2(const TYPE *U, ATL_CINT ldu, TYPE *B, ATL_CINT ldb)
{
   ATL_CINT ldU=ldu+ldu, ldB = ldb+ldb, ldB2=(ldb<<2), ldB3=ldB+ldB2;
   const RTYPE rU00=(*U), iU00=U[1], rU01=U[ldU], iU01=U[ldU+1];
   const RTYPE rU11=U[ldU+2], iU11 = U[ldU+3];
   RTYPE rB00=(*B), iB00=B[1], rB10=B[2], iB10=B[3];
   RTYPE rB01=B[ldB], iB01=B[ldB+1], rB11=B[ldB+2], iB11=B[ldB+3];
   RTYPE rB02=B[ldB2], iB02=B[ldB2+1], rB12=B[ldB2+2], iB12=B[ldB2+3];
   RTYPE rB03=B[ldB3], iB03=B[ldB3+1], rB13=B[ldB3+2], iB13=B[ldB3+3];
   RTYPE rX;
/*
 * x1 = b1 / U11;  U11 is recipricol, so solve x1 = b1 * U11;
 */
   B[2] = rX = rB10 * rU11 - iB10 * iU11;
   B[3] = iB10 = rB10 * iU11 + iB10 * rU11; rB10 = rX;
   B[ldB+2] = rX = rB11 * rU11 - iB11 * iU11;
   B[ldB+3] = iB11 = rB11 * iU11 + iB11 * rU11; rB11 = rX;
   B[ldB2+2] = rX = rB12 * rU11 - iB12 * iU11;
   B[ldB2+3] = iB12 = rB12 * iU11 + iB12 * rU11; rB12 = rX;
   B[ldB3+2] = rX = rB13 * rU11 - iB13 * iU11;
   B[ldB3+3] = iB13 = rB13 * iU11 + iB13 * rU11; rB13 = rX;
/*
 * x0 = (b0 - U01*x1) / U00, do x0 = (b0 - U01*x1) first
 */
   rB00 = rB00 - rU01*rB10 + iU01*iB10;
   iB00 = iB00 - rU01*iB10 - iU01*rB10;
   rB01 = rB01 - rU01*rB11 + iU01*iB11;
   iB01 = iB01 - rU01*iB11 - iU01*rB11;
   rB02 = rB02 - rU01*rB12 + iU01*iB12;
   iB02 = iB02 - rU01*iB12 - iU01*rB12;
   rB03 = rB03 - rU01*rB13 + iU01*iB13;
   iB03 = iB03 - rU01*iB13 - iU01*rB13;
/*
 * Finish x0 = (b0 - U01*x1) / U00 by multiplying by U00 (1/U00)
 */
   *B = rB00 * rU00 - iB00 * iU00;
   B[1] = rB00 * iU00 + iB00 * rU00;
   B[ldB]   = rB01 * rU00 - iB01 * iU00;
   B[ldB+1] = rB01 * iU00 + iB01 * rU00;
   B[ldB2]   = rB02 * rU00 - iB02 * iU00;
   B[ldB2+1] = rB02 * iU00 + iB02 * rU00;
   B[ldB3]   = rB03 * rU00 - iB03 * iU00;
   B[ldB3+1] = rB03 * iU00 + iB03 * rU00;
}

/*
 * Solve 2x2 L with 4 RHS symbolically for complex arithmetic;
 * Diagonal elements have already been inverted
 */
ATL_SINLINE void ATL_trsmL2(const TYPE *L, ATL_CINT ldl, TYPE *B, ATL_CINT ldb0)
{
   const RTYPE rL00=(*L), iL00=L[1], rL10=L[2], iL10=L[3],
               rL11=L[ldl+ldl+2], iL11=L[ldl+ldl+3];
   ATL_CINT ldb=ldb0+ldb0, ldb2=(ldb0<<2), ldb3=ldb+ldb2;
   RTYPE rB00=(*B), iB00=B[1], rB10=B[2], iB10=B[3];
   RTYPE rB01=B[ldb], iB01=B[ldb+1], rB11=B[ldb+2], iB11=B[ldb+3];
   RTYPE rB02=B[ldb2], iB02=B[ldb2+1], rB12=B[ldb2+2], iB12=B[ldb2+3];
   RTYPE rB03=B[ldb3], iB03=B[ldb3+1], rB13=B[ldb3+2], iB13=B[ldb3+3];
   RTYPE rX;
/*
 * x0 = b0 / L00
 */
   *B        = rX = rB00*rL00 - iB00*iL00;
   B[1]      = iB00 = rB00*iL00 + iB00*rL00; rB00 = rX;
   B[ldb]    = rX = rB01*rL00 - iB01*iL00;
   B[ldb+1]  = iB01 = rB01*iL00 + iB01*rL00; rB01 = rX;
   B[ldb2]   = rX = rB02*rL00 - iB02*iL00;
   B[ldb2+1] = iB02 = rB02*iL00 + iB02*rL00; rB02 = rX;
   B[ldb3]   = rX = rB03*rL00 - iB03*iL00;
   B[ldb3+1] = iB03 = rB03*iL00 + iB03*rL00; rB03 = rX;
/*
 * x1 = (b1 - L10 * x0)  [divide by diagonal in next step]
 */
   rB10 = (rB10 - rL10*rB00 + iL10*iB00);
   iB10 = (iB10 - rL10*iB00 - iL10*rB00);
   rB11 = (rB11 - rL10*rB01 + iL10*iB01);
   iB11 = (iB11 - rL10*iB01 - iL10*rB01);
   rB12 = (rB12 - rL10*rB02 + iL10*iB02);
   iB12 = (iB12 - rL10*iB02 - iL10*rB02);
   rB13 = (rB13 - rL10*rB03 + iL10*iB03);
   iB13 = (iB13 - rL10*iB03 - iL10*rB03);
/*
 * Mult by recipricol of L11 to finish x1 = (b1 - L10 * x0)/L11.
 */
   B[2]      = rB10*rL11 - iB10*iL11;
   B[3]      = rB10*iL11 + iB10*rL11;
   B[ldb+2]  = rB11*rL11 - iB11*iL11;
   B[ldb+3]  = rB11*iL11 + iB11*rL11;
   B[ldb2+2] = rB12*rL11 - iB12*iL11;
   B[ldb2+3] = iB12 = rB12*iL11 + iB12*rL11;
   B[ldb3+2] = rB13*rL11 - iB13*iL11;
   B[ldb3+3] = iB13 = rB13*iL11 + iB13*rL11;
}
#endif

static void ATL_trsmRLN
(
   ATL_CINT  M,   /* size of orig triangular matrix A */
   ATL_CINT  N,   /* number of RHS in B */
   const TYPE *A, /* McxMc lower matrix A, diag has inverse of original diag */
   TYPE *W        /* McxN, on input padded B, on output, padded X */
)
{
   int j;
   ATL_CINT Mc = ((M+TRU-1)/TRU)*TRU, Mc2=Mc+Mc;

/*
 * Loop over RHS, NRHS RHS at a time
 */
   for (j=0; j < N; j += NRHS, W += NRHS*Mc2)
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
      for (k=0; k < M; k += 2, w += 4, a += (Mc+1)<<2)
      {
/*
 *       Solve M=2 NRHS=NRHS TRSM symbolically
 */
         ATL_trsmL2(a, Mc, w, Mc);
/*
 *       Subtract off x0,x1 contribution from rest of B using rank-2 update
 */
	 if (M-k-2 > 0)
            ATL_rk2(Mc-k-2, a+4, Mc, w, Mc, w+4, Mc);
      }     /* end k-loop that loops through L */
   }        /* end j-loop over RHS */
}           /* end routine */

static void ATL_trsmRUN
(
   ATL_CINT  M,         /* size of orig triangular matrix A */
   ATL_CINT  N,         /* number of RHS in W */
   const TYPE *A,       /* McxMc Upper matrix A, diag is inverted */
   TYPE *W              /* McxN workspace with good alignment */
)
{
   int j;
   ATL_CINT Mc = ((M+TRU-1)/TRU)*TRU, mr = Mc-M, Mc2=Mc+Mc;
/*
 * Loop over RHS, NRHS RHS at a time
 */
   for (j=0; j < N; j += NRHS, W += NRHS*Mc2)
   {
      const int nb = Mmin(NRHS, N-j);
      int k, i;
      TYPE *w = W;
      const TYPE *Ac = A + (Mc-2)*Mc2, *a = Ac + Mc2-4;
/*
 *    Completely solve these RHSs by looping over entire triangular matrix
 */
      w = W + Mc2-4;
      for (k=0; k < M; k += 2, w -= 4, a -= (Mc+1)<<2, Ac -= Mc<<2)
      {
         ATL_CINT mr = Mc-k-2;
/*
 *       Solve M=4 NRHS=4 TRSM symbolically
 */
         ATL_trsmU2(a, Mc, w, Mc);
/*
 *       Subtract off x0,x1 contribution from rest of B using rank-2 update
 */

          if (mr)
            ATL_rk2(mr, Ac, Mc, w, Mc, W, Mc);
      }     /* end k-loop that loops through L */
   }        /* end j-loop over RHS */
}           /* end routine */
ATL_SINLINE void trU2Lc
   (enum ATLAS_DIAG Diag, ATL_CINT N, const TYPE *U, ATL_CINT ldu,
    TYPE *L, ATL_CINT ldl)
/*
 * reflects upper part of U into lower part of L with conjugation,
 * Lower, right part of L is padded to ldl with I to reach ldl size
 */
{
   ATL_CINT N2=N+N, ldU=ldu+ldu, ldL=ldl+ldl;
   ATL_INT i, j;
   const TYPE *Uc = U;

   for (j=0; j < N2; j += 2, Uc += ldU)
   {
      TYPE *Lr = L + j;
      for (i=0; i < j; i += 2, Lr += ldL)
      {
         *Lr = Uc[i];
         Lr[1] = -Uc[i+1];
      }
      if (Diag == AtlasUnit)
      {
         *Lr = ATL_rone;
         Lr[1] = ATL_rzero;
      }
      else
      {
         *Lr = Uc[j];
         Lr[1] = -Uc[j+1];
         Mjoin(PATL,cplxinvert)(1, Lr, 1, Lr, 1);
      }
   }
/*
 * Pad left and lower portion of L if ldl > N
 */
  if (ldl > N)
  {
/*
 *    Pad the last ldl-N rows of L with zeros in the first N columns of L
 */
      for (j=0; j < N2; j += 2, L += ldL)
      {
         for (i=N2; i < ldL; i++)
            L[i] = ATL_rzero;
      }

/*
 *    Pad the last ldl-N columns of L with the identity matrix
 */
      for (; j < ldL; j += 2, L += ldL)
      {
         L[j] = ATL_rone;
         L[j+1] = ATL_rzero;
         for (i=j+2; i < ldL; i++)
            L[i] = ATL_rzero;
      }
   }
}
ATL_SINLINE void trU2L
   (enum ATLAS_DIAG Diag, ATL_CINT N, const TYPE *U, ATL_CINT ldu,
    TYPE *L, ATL_CINT ldl)
/*
 * reflects upper part of U into lower part of L,
 * Lower, right part of L is padded to ldl with I to reach ldl size
 */
{
   ATL_CINT N2=N+N, ldU=ldu+ldu, ldL=ldl+ldl;
   ATL_INT i, j;
   const TYPE *Uc = U;

   for (j=0; j < N2; j += 2, Uc += ldU)
   {
      TYPE *Lr = L + j;
      for (i=0; i < j; i += 2, Lr += ldL)
      {
         *Lr = Uc[i];
         Lr[1] = Uc[i+1];
      }
      if (Diag == AtlasUnit)
      {
         *Lr = ATL_rone;
         Lr[1] = ATL_rzero;
      }
      else
         Mjoin(PATL,cplxinvert)(1, (TYPE*)(Uc+j), 1, Lr, 1);
   }
/*
 * Pad left and lower portion of L if ldl > N
 */
  if (ldl > N)
  {
/*
 *    Pad the last ldl-N rows of L with zeros in the first N columns of L
 */
      for (j=0; j < N2; j += 2, L += ldL)
      {
         for (i=N2; i < ldL; i++)
            L[i] = ATL_rzero;
      }

/*
 *    Pad the last ldl-N columns of L with the identity matrix
 */
      for (; j < ldL; j += 2, L += ldL)
      {
         L[j] = ATL_rone;
         L[j+1] = ATL_rzero;
         for (i=j+2; i < ldL; i++)
            L[i] = ATL_rzero;
      }
   }
}

ATL_SINLINE void trL2U
   (enum ATLAS_DIAG Diag, ATL_CINT N, const TYPE *L, ATL_CINT ldl,
    TYPE *U, ATL_CINT ldu)
/*
 * reflects lower part of L into upper part of U,
 * The upper, left part of U is padded with I to reach ldu size
 */
{
   ATL_CINT N2=N+N, ldu2=ldu+ldu, ldl2=ldl+ldl, gap = ldu2-N2;
   ATL_INT i, j;
   TYPE *u;

   if (gap)
   {
/*
 *    Pad ldu-N first columns with I
 */
      for (j=0; j < gap; j += 2, U += ldu2)
      {
         for (i=0; i < ldu2; i++)
            U[i] = ATL_rzero;
         U[j] = ATL_rone;
      }
/*
 *    Pad first ldu-N rows with zeros
 */
      u = U;
      for (j=0; j < N2; j += 2, u += ldu2)
      {
         for (i=0; i < gap; i++)
            u[i] = ATL_rzero;
      }
      U += gap;
   }
/*
 * Now transpose L into last N rows/cols of U.  Access rows of U since it
 * is known to be contiguous, so non-cont L is accessed by columns.
 */
   for (j=0; j < N2; j += 2, L += ldl2+2, U += ldu2+2)
   {
      if (Diag == AtlasUnit)
      {
         *U = ATL_rone;
         U[1] = ATL_rzero;
      }
      else
         Mjoin(PATL,cplxinvert)(1, (TYPE*)L, 1, U, 1);
      for (u=U+ldu2,i=j+2; i < N2; i += 2, u += ldu2)
      {
         *u = L[i-j];
         u[1] = L[i-j+1];
      }
   }
}
ATL_SINLINE void trL2Uc
   (enum ATLAS_DIAG Diag, ATL_CINT N, const TYPE *L, ATL_CINT ldl,
    TYPE *U, ATL_CINT ldu)
/*
 * reflects & conjugates lower part of L into upper part of U,
 * The upper, left part of U is padded with I to reach ldu size
 */
{
   ATL_CINT N2=N+N, ldu2=ldu+ldu, ldl2=ldl+ldl, gap = ldu2-N2;
   ATL_INT i, j;
   TYPE *u;

   if (gap)
   {
/*
 *    Pad ldu-N first columns with I
 */
      for (j=0; j < gap; j += 2, U += ldu2)
      {
         for (i=0; i < ldu2; i++)
            U[i] = ATL_rzero;
         U[j] = ATL_rone;
      }
/*
 *    Pad first ldu-N rows with zeros
 */
      u = U;
      for (j=0; j < N2; j += 2, u += ldu2)
      {
         for (i=0; i < gap; i++)
            u[i] = ATL_rzero;
      }
      U += gap;
   }
/*
 * Now transpose L into last N rows/cols of U.  Access rows of U since it
 * is known to be contiguous, so non-cont L is accessed by columns.
 */
   for (j=0; j < N2; j += 2, L += ldl2+2, U += ldu2+2)
   {
      if (Diag == AtlasUnit)
      {
         *U = ATL_rone;
         U[1] = ATL_rzero;
      }
      else
      {
         *U = *L;
         U[1] = -L[1];
         Mjoin(PATL,cplxinvert)(1, U, 1, U, 1);
      }
      for (u=U+ldu2,i=j+2; i < N2; i += 2, u += ldu2)
      {
         *u = L[i-j];
         u[1] = -L[i-j+1];
      }
   }
}
/*
 * Conjugate original U to aligned workspace, invert diagonal elts, pad wt I
 * U is padded with I at top left of matrix to fit Nc
 */
static void cpypadUc
(
   enum ATLAS_DIAG Diag,
   ATL_CINT N,                  /* size of triangular matrix A */
   const TYPE *A,               /* upper triangular matrix */
   ATL_CINT lda,                /* leading dim of A */
   TYPE *a,                     /* cpy of A, padded to Nc with I */
   ATL_CINT Nc                  /* leading dim of A, how much to pad */
)
{
   int i, j;
   ATL_CINT lda2 = lda+lda, N2=N+N, Nc2=Nc+Nc;

/*
 * Pad Nc-N upper, left of U with I
 */
   if (Nc != N)
   {
      const int nn = Nc2-N2;
      TYPE *aa;
      for (j=0; j < nn; j += 2, a += Nc2)
      {
         for (i=0; i < Nc2; i++)
            a[i] = ATL_rzero;
         a[j] = ATL_rone;
      }
/*
 *    Now zero first few rows of each column above that actual U
 */
      aa = a;
      for (j=0; j < N; j++, aa += Nc2)
      {
         for (i=0; i < nn; i++)
            aa[i] = ATL_rzero;
      }
      a += nn;  /* a now pts to place to copy actual U */
   }
/*
 * Copy unpadded portion to U
 */
   for (j=0; j < N2; j += 2, a += Nc2, A += lda2)
   {
      for (i=0; i < j; i += 2)
      {
         a[i] = A[i];
         a[i+1] = -A[i+1];
      }
      if (Diag == AtlasNonUnit)
      {
         a[j] = A[j];
         a[j+1] = -A[j+1];
         Mjoin(PATL,cplxinvert)(1, a+j, 1, a+j, 1);
      }
      else
      {
         a[j] = ATL_rone;
         a[j+1] = ATL_rzero;
      }
   }
}
/*
 * Conjugate original L to aligned workspace, invert diagonal elts, pad wt I
 */
static void cpypadLc
(
   enum ATLAS_DIAG Diag,
   ATL_CINT N,                  /* size of triangular matrix A */
   const TYPE *A,               /* lower triangular matrix */
   ATL_CINT lda,                /* leading dim of A */
   TYPE *a,                     /* cpy of A, padded to N4 with I */
   ATL_CINT Nc                  /* leading dim of A, how much to pad */
)
{
   int i, j;
   ATL_CINT lda2 = lda+lda, N2=N+N, Nc2=Nc+Nc;

   for (j=0; j < N; j++, a += Nc2, A += lda2)
   {
      ATL_CINT j2 = j+j;

      if (Diag == AtlasNonUnit)
      {
         a[j2] = A[j2];
	 a[j2+1] = -A[j2+1];
         Mjoin(PATL,cplxinvert)(1, a+j2, 1, a+j2, 1);
      }
      else
      {
         a[j2] = ATL_rone;
         a[j2+1] = ATL_rzero;
      }
      for (i=j2+2; i < N2; i += 2)
      {
         a[i] = A[i];
         a[i+1] = -A[i+1];
      }
      for (; i < Nc2; i++)
         a[i] = ATL_rzero;
   }
   for (; j < Nc; j++, a += Nc2)
   {
      for (i=0; i < Nc2; i++)
         a[i] = ATL_rzero;
      a[j+j] = ATL_rone;
   }
}
/*
 * Copy original U to aligned workspace, invert diagonal elts, pad wt I
 * U is padded with I at top left of matrix to fit Nc
 */
static void cpypadU
(
   enum ATLAS_DIAG Diag,
   ATL_CINT N,                  /* size of triangular matrix A */
   const TYPE *A,               /* upper triangular matrix */
   ATL_CINT lda,                /* leading dim of A */
   TYPE *a,                     /* cpy of A, padded to Nc with I */
   ATL_CINT Nc                  /* leading dim of A, how much to pad */
)
{
   int i, j;
   ATL_CINT lda2 = lda+lda, N2=N+N, Nc2=Nc+Nc;

/*
 * Pad Nc-N upper, left of U with I
 */
   if (Nc != N)
   {
      const int nn = Nc2-N2;
      TYPE *aa;
      for (j=0; j < nn; j += 2, a += Nc2)
      {
         for (i=0; i < Nc2; i++)
            a[i] = ATL_rzero;
         a[j] = ATL_rone;
      }
/*
 *    Now zero first few rows of each column above that actual U
 */
      aa = a;
      for (j=0; j < N; j++, aa += Nc2)
      {
         for (i=0; i < nn; i++)
            aa[i] = ATL_rzero;
      }
      a += nn;  /* a now pts to place to copy actual U */
   }
/*
 * Copy unpadded portion to U
 */
   for (j=0; j < N2; j += 2, a += Nc2, A += lda2)
   {
      for (i=0; i < j; i++)
         a[i] = A[i];
      if (Diag == AtlasNonUnit)
         Mjoin(PATL,cplxinvert)(1, (TYPE*)(A+j), 1, a+j, 1);
      else
      {
         a[j] = ATL_rone;
         a[j+1] = ATL_rzero;
      }
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
   ATL_CINT Nc                  /* leading dim of A, how much to pad */
)
{
   int i, j;
   ATL_CINT lda2 = lda+lda, N2=N+N, Nc2=Nc+Nc;

   for (j=0; j < N; j++, a += Nc2, A += lda2)
   {
      ATL_CINT j2 = j+j;

      if (Diag == AtlasNonUnit)
         Mjoin(PATL,cplxinvert)(1, (TYPE*)(A+j2), 1, a+j2, 1);
      else
      {
         a[j2] = ATL_rone;
         a[j2+1] = ATL_rzero;
      }
      for (i=j2+2; i < N2; i++)
         a[i] = A[i];
      for (; i < Nc2; i++)
         a[i] = ATL_rzero;
   }
   for (; j < Nc; j++, a += Nc2)
   {
      for (i=0; i < Nc2; i++)
         a[i] = ATL_rzero;
      a[j+j] = ATL_rone;
   }
}
/*
 * This routine computes X * op(A) = B by first computing:
 *    op(A)^T * X^T = B^T
 * and then transposing the answer back out.
 */
int Mjoin(PATL,trsmKR_rk2)
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
   const TYPE one[2] = {ATL_rone, ATL_rzero};
   enum ATLAS_TRANS TRANSA;
   ATL_CINT Nc = ((N+TRU-1)/TRU)*TRU, nr = Nc-N, Nc2=Nc+Nc, nr2 = nr+nr;
   ATL_CINT Mc = Mmin(((M+NRHS-1)/NRHS)*NRHS, 36), Mc2 = Mc+Mc;
   int UPPER = (Uplo == AtlasUpper);
   const int UNIT=(Diag == AtlasUnit);
   ATL_INT i, j;

   vp = malloc(ATL_MulBySize(Nc*(Mc+Nc)) + 2*ATL_Cachelen);
   if (!vp)
      return(1);
   a = ATL_AlignPtr(vp);
   w = a+Nc*Nc2;
   w = ATL_AlignPtr(w);
/*
 * If matrix is already transposed, keep Uplo same and just copy to space
 * and invert the diagonal, and swap the transpose setting
 */
   if (TA == AtlasTrans)
   {
     if (UPPER)
        cpypadU(Diag, N, A, lda, a, Nc);
     else
        cpypadL(Diag, N, A, lda, a, Nc);
      TA = AtlasNoTrans;
   }
/*
 * If matrix is conjugate transposed, keep Uplo same and just conjugate copy
 * to space and invert the diagonal, and swap the transpose setting
 */
   else if (TA == AtlasConjTrans)
   {
     if (UPPER)
        cpypadUc(Diag, N, A, lda, a, Nc);
     else
        cpypadLc(Diag, N, A, lda, a, Nc);
      TA = AtlasNoTrans;
   }
/*
 * If the matrix is presently in NoTranspose format, transpose it, which
 * will swap Uplo setting
 */
   else
   {
      if (UPPER)
         trU2L(Diag, N, A, lda, a, Nc);  /* transpose A into lower-triang a */
/*
 *       Transpose Lower matrix to Upper; padding goes at top of Upper matrix
 */
      else  /* matrix is currently lower */
         trL2U(Diag, N, A, lda, a, Nc);
      UPPER = !UPPER;  /* transposition swaps UPLO setting */
   }
/*
 * a now contains correctly padded A^T, so now loop over Mc rows of RHS;
 * Mc is set to a value near 32 so we can get some TLB reuse when doing
 * the transpose of the RHS vectors from row-access to column-access
 */
   b = (UPPER) ? w + nr2 : w;
   for (i=0; i < M; i += Mc, B += Mc2)
   {
      const int mr = Mmin(M-i, Mc);
      Mjoin(PATL,gemoveT)(N, mr, alpha, B, ldb, b, Nc);
      if (UPPER)
      {
         if (nr)
            Mjoin(PATL,gezero)(nr, mr, w, Nc);
         ATL_trsmRUN(N, mr, a, w);
      }
      else
      {
         if (nr)
            Mjoin(PATL,gezero)(nr, mr, w+N+N, Nc);
         ATL_trsmRLN(N, mr, a, w);
      }
      Mjoin(PATL,gemoveT)(mr, N, one, b, Nc, B, ldb);
   }
   free(vp);
   return(0);
}
