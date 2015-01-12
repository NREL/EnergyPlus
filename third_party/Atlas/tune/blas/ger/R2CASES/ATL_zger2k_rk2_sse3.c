/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2012, 2011 R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
#ifndef ATL_SSE3
   #error "This routine requires SSE3!"
#endif
#include "atlas_misc.h"
#include "atlas_prefetch.h"
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
 * rank-2 update with mu=2, nu=3, ku=2.  This version is for 16 SSE regs.
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
   ATL_INT i;
   ATL_CINT MM = (M&1) ? M-1 : M-2;
   register __m128d B00, B10, B01, B11, B02, B12;
   register __m128d C00, C01, C02, C10, C11, C12;
   register __m128d A, a;

   B00 = _mm_load_pd(pB0);
   B10 = _mm_load_pd(pB1);
   B01 = _mm_load_pd(pB0+2);
   B11 = _mm_load_pd(pB1+2);
   B02 = _mm_load_pd(pB0+4);
   B12 = _mm_load_pd(pB1+4);    	/* iB12, rB12 */


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
