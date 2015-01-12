/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010, 2009 R. Clint Whaley
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
#include <xmmintrin.h>
#include "atlas_misc.h"

#define ATL_USEPF
#ifndef PFDIST
   #define PFDIST 32
#endif
static void gerk_sse3u
   (ATL_CINT M0, ATL_CINT N, const TYPE *X, const TYPE *Y0,
    TYPE *A0, ATL_CINT lda0)
/*
 * N must be a multiple of 4
 */
{
   ATL_CINT lda=lda0+lda0, lda4 = lda<<2, M = M0+M0;
   TYPE *A1=A0+lda, *A2=A1+lda, *A3=A2+lda;
   register __m128d y0a, y0b, y1a, y1b, y2a, y2b, y3a, y3b, x0a, x0b, xn;
   register __m128d a00, a01, a02, a03;
   const __m128d vnone = {-1.0, -1.0};
   ATL_INT i, j;

   for (j=0; j < N; j += 4, A0 += lda4, A1 += lda4, A2 += lda4, A3 += lda4,
                            Y0 += 8)
   {
      a00 = _mm_loadu_pd(A0);
      a01 = _mm_loadu_pd(A1);
      a02 = _mm_loadu_pd(A2);
      a03 = _mm_loadu_pd(A3);
      x0a = _mm_loadu_pd(X);             /* x0a = {Xi, Xr} */
      x0b = (__m128d)_mm_shuffle_epi32((__m128i)x0a, 0x4E);   /* x0b = {Yr, Xi} */

      y0a = _mm_load1_pd(Y0);           /* y0a = {Yr, Yr} */
      y0b = _mm_load1_pd(Y0+1);         /* y0b = {Yi, Yi} */
      y0b = _mm_mul_sd(y0b, vnone);     /* y0b = {Yi,-Yi} */
      y1a = _mm_load1_pd(Y0+2);         /* y1a = {Yr, Yr} */
      y1b = _mm_load1_pd(Y0+3);         /* y1b = {Yi, Yi} */
      y1b = _mm_mul_sd(y1b, vnone);     /* y1b = {Yi,-Yi} */
      y2a = _mm_load1_pd(Y0+4);         /* y2a = {Yr, Yr} */
      y2b = _mm_load1_pd(Y0+5);         /* y2b = {Yi, Yi} */
      y2b = _mm_mul_sd(y2b, vnone);     /* y2b = {Yi,-Yi} */
      y3a = _mm_load1_pd(Y0+6);         /* y3a = {Yr, Yr} */
      y3b = _mm_load1_pd(Y0+7);         /* y3b = {Yi, Yi} */
      y3b = _mm_mul_sd(y3b, vnone);     /* y3b = {Yi,-Yi} */
      for (i=2; i < M; i += 2)
      {
/*
 *       Do Ar += xr*yr;  Ai += xi*yr; for all 4 columns of A
 */

         #ifdef ATL_USEPF
            _mm_prefetch(A0+PFDIST, _MM_HINT_T0);
         #endif
         xn = x0a;                    /* xn  = {Xi, Xr} */
         x0a = _mm_mul_pd(x0a, y0a);  /* x0a = {Xi*Yr, Xr*Yr} */
         a00 = _mm_add_pd(a00, x0a);  /* a00 = {Ai+Xi*Yr, Ar+Xr*Yr} */
         x0a = xn;
         #ifdef ATL_USEPF
            _mm_prefetch(A1+PFDIST, _MM_HINT_T0);
         #endif
         xn  = _mm_mul_pd(xn, y1a);   /* xn  = {Xi*Yr, Xr*Yr} */
         a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr, Ar+Xr*Yr} */
         #ifdef ATL_USEPF
            _mm_prefetch(A2+PFDIST, _MM_HINT_T0);
         #endif
         xn = x0a;
         x0a = _mm_mul_pd(x0a, y2a);  /* x0a = {Xi*Yr, Xr*Yr} */
         a02 = _mm_add_pd(a02, x0a);  /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
         x0a = _mm_loadu_pd(X+i);      /* x0a = {Xi, Xr} */
         #ifdef ATL_USEPF
            _mm_prefetch(A3+PFDIST, _MM_HINT_T0);
         #endif
         xn  = _mm_mul_pd(xn, y3a);   /* xn  = {Xi*Yr, Xr*Yr} */
         a03 = _mm_add_pd(a03, xn);   /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
/*
 *       Do Ar += xi*(-yi); Ai += xr*yi; for all 4 columns of A
 */
                                      /* y0b = {Yi,-Yi} */
         xn = x0b;                    /* xn  = {Xr, Xi} */
         x0b = _mm_mul_pd(x0b, y0b);  /* x0b = {Xr*Yi, -Xi*Yi} */
         a00 = _mm_add_pd(a00, x0b);  /* a00 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_storeu_pd(A0+i-2, a00);   /* *A0 = a00 */
         a00 = _mm_loadu_pd(A0+i);
         x0b = xn;                    /* x0b = {Xr, Xi} */
         xn  = _mm_mul_pd(xn,  y1b);  /* xn  = {Xr*Yi, -Xi*Yi} */
         a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_storeu_pd(A1+i-2, a01);   /* *A1 = a01 */
         a01 = _mm_loadu_pd(A1+i);
         xn = x0b;
         x0b = _mm_mul_pd(x0b, y2b);  /* x0b = {Xr*Yi, -Xi*Yi} */
         a02 = _mm_add_pd(a02, x0b);  /* a02 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_storeu_pd(A2+i-2, a02);   /* *A2 = a02 */
         a02 = _mm_loadu_pd(A2+i);
         x0b = (__m128d)_mm_shuffle_epi32((__m128i)x0a, 0x4E);   /* x0b = {Yr, Xi} */
         xn  = _mm_mul_pd(xn,  y3b);  /* xn  = {Xr*Yi, -Xi*Yi} */
         a03 = _mm_add_pd(a03, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_storeu_pd(A3+i-2, a03);   /* *A3 = a03 */
         a03 = _mm_loadu_pd(A3+i);
      }
/*
 *    ===================
 *    Drain load/use pipe
 *    ===================
 */
/*
 *    Do Ar += xr*yr;  Ai += xi*yr; for all 4 columns of A
 */
      xn = x0a;                    /* xn  = {Xi, Xr} */
      x0a = _mm_mul_pd(x0a, y0a);  /* x0a = {Xi*Yr, Xr*Yr} */
      a00 = _mm_add_pd(a00, x0a);  /* a00 = {Ai+Xi*Yr, Ar+Xr*Yr} */
      x0a = xn;
      xn  = _mm_mul_pd(xn, y1a);   /* xn  = {Xi*Yr, Xr*Yr} */
      a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr, Ar+Xr*Yr} */
      xn = x0a;
      x0a = _mm_mul_pd(x0a, y2a);  /* x0a = {Xi*Yr, Xr*Yr} */
      a02 = _mm_add_pd(a02, x0a);  /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
      xn  = _mm_mul_pd(xn, y3a);   /* xn  = {Xi*Yr, Xr*Yr} */
      a03 = _mm_add_pd(a03, xn);   /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
/*
 *    Do Ar += xi*(-yi); Ai += xr*yi; for all 4 columns of A
 */
                                   /* y0b = {Yi,-Yi} */
      xn = x0b;                    /* xn  = {Xr, Xi} */
      x0b = _mm_mul_pd(x0b, y0b);  /* x0b = {Xr*Yi, -Xi*Yi} */
      a00 = _mm_add_pd(a00, x0b);  /* a00 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_storeu_pd(A0+i-2, a00);   /* *A0 = a00 */
      x0b = xn;                    /* x0b = {Xr, Xi} */
      xn  = _mm_mul_pd(xn,  y1b);  /* xn  = {Xr*Yi, -Xi*Yi} */
      a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_storeu_pd(A1+i-2, a01);   /* *A1 = a01 */
      xn = x0b;
      x0b = _mm_mul_pd(x0b, y2b);  /* x0b = {Xr*Yi, -Xi*Yi} */
      a02 = _mm_add_pd(a02, x0b);  /* a02 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_storeu_pd(A2+i-2, a02);   /* *A2 = a02 */
      x0b = (__m128d)_mm_shuffle_epi32((__m128i)x0a, 0x4E);   /* x0b = {Yr, Xi} */
      xn  = _mm_mul_pd(xn,  y3b);  /* xn  = {Xr*Yi, -Xi*Yi} */
      a03 = _mm_add_pd(a03, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_storeu_pd(A3+i-2, a03);   /* *A3 = a03 */
   }
}
static void gerk_sse3
   (ATL_CINT M0, ATL_CINT N, const TYPE *X, const TYPE *Y0,
    TYPE *A0, ATL_CINT lda0)
/*
 * N must be a multiple of 4
 */
{
   ATL_CINT lda=lda0+lda0, lda4 = lda<<2, M = M0+M0;
   TYPE *A1=A0+lda, *A2=A1+lda, *A3=A2+lda;
   register __m128d y0a, y0b, y1a, y1b, y2a, y2b, y3a, y3b, x0a, x0b, xn;
   register __m128d a00, a01, a02, a03;
   const __m128d vnone = {-1.0, -1.0};
   ATL_INT i, j;

   for (j=0; j < N; j += 4, A0 += lda4, A1 += lda4, A2 += lda4, A3 += lda4,
                            Y0 += 8)
   {
      a00 = _mm_load_pd(A0);
      a01 = _mm_load_pd(A1);
      a02 = _mm_load_pd(A2);
      a03 = _mm_load_pd(A3);
      x0a = _mm_load_pd(X);             /* x0a = {Xi, Xr} */
      x0b = (__m128d)_mm_shuffle_epi32((__m128i)x0a, 0x4E);   /* x0b = {Yr, Xi} */

      y0a = _mm_load1_pd(Y0);           /* y0a = {Yr, Yr} */
      y0b = _mm_load1_pd(Y0+1);         /* y0b = {Yi, Yi} */
      y0b = _mm_mul_sd(y0b, vnone);     /* y0b = {Yi,-Yi} */
      y1a = _mm_load1_pd(Y0+2);         /* y1a = {Yr, Yr} */
      y1b = _mm_load1_pd(Y0+3);         /* y1b = {Yi, Yi} */
      y1b = _mm_mul_sd(y1b, vnone);     /* y1b = {Yi,-Yi} */
      y2a = _mm_load1_pd(Y0+4);         /* y2a = {Yr, Yr} */
      y2b = _mm_load1_pd(Y0+5);         /* y2b = {Yi, Yi} */
      y2b = _mm_mul_sd(y2b, vnone);     /* y2b = {Yi,-Yi} */
      y3a = _mm_load1_pd(Y0+6);         /* y3a = {Yr, Yr} */
      y3b = _mm_load1_pd(Y0+7);         /* y3b = {Yi, Yi} */
      y3b = _mm_mul_sd(y3b, vnone);     /* y3b = {Yi,-Yi} */
      for (i=2; i < M; i += 2)
      {
/*
 *       Do Ar += xr*yr;  Ai += xi*yr; for all 4 columns of A
 */

         #ifdef ATL_USEPF
            _mm_prefetch(A0+PFDIST, _MM_HINT_T0);
         #endif
         xn = x0a;                    /* xn  = {Xi, Xr} */
         x0a = _mm_mul_pd(x0a, y0a);  /* x0a = {Xi*Yr, Xr*Yr} */
         a00 = _mm_add_pd(a00, x0a);  /* a00 = {Ai+Xi*Yr, Ar+Xr*Yr} */
         x0a = xn;
         #ifdef ATL_USEPF
            _mm_prefetch(A1+PFDIST, _MM_HINT_T0);
         #endif
         xn  = _mm_mul_pd(xn, y1a);   /* xn  = {Xi*Yr, Xr*Yr} */
         a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr, Ar+Xr*Yr} */
         #ifdef ATL_USEPF
            _mm_prefetch(A2+PFDIST, _MM_HINT_T0);
         #endif
         xn = x0a;
         x0a = _mm_mul_pd(x0a, y2a);  /* x0a = {Xi*Yr, Xr*Yr} */
         a02 = _mm_add_pd(a02, x0a);  /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
         x0a = _mm_load_pd(X+i);      /* x0a = {Xi, Xr} */
         #ifdef ATL_USEPF
            _mm_prefetch(A3+PFDIST, _MM_HINT_T0);
         #endif
         xn  = _mm_mul_pd(xn, y3a);   /* xn  = {Xi*Yr, Xr*Yr} */
         a03 = _mm_add_pd(a03, xn);   /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
/*
 *       Do Ar += xi*(-yi); Ai += xr*yi; for all 4 columns of A
 */
                                      /* y0b = {Yi,-Yi} */
         xn = x0b;                    /* xn  = {Xr, Xi} */
         x0b = _mm_mul_pd(x0b, y0b);  /* x0b = {Xr*Yi, -Xi*Yi} */
         a00 = _mm_add_pd(a00, x0b);  /* a00 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_store_pd(A0+i-2, a00);   /* *A0 = a00 */
         a00 = _mm_load_pd(A0+i);
         x0b = xn;                    /* x0b = {Xr, Xi} */
         xn  = _mm_mul_pd(xn,  y1b);  /* xn  = {Xr*Yi, -Xi*Yi} */
         a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_store_pd(A1+i-2, a01);   /* *A1 = a01 */
         a01 = _mm_load_pd(A1+i);
         xn = x0b;
         x0b = _mm_mul_pd(x0b, y2b);  /* x0b = {Xr*Yi, -Xi*Yi} */
         a02 = _mm_add_pd(a02, x0b);  /* a02 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_store_pd(A2+i-2, a02);   /* *A2 = a02 */
         a02 = _mm_load_pd(A2+i);
         x0b = (__m128d)_mm_shuffle_epi32((__m128i)x0a, 0x4E);   /* x0b = {Yr, Xi} */
         xn  = _mm_mul_pd(xn,  y3b);  /* xn  = {Xr*Yi, -Xi*Yi} */
         a03 = _mm_add_pd(a03, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
         _mm_store_pd(A3+i-2, a03);   /* *A3 = a03 */
         a03 = _mm_load_pd(A3+i);
      }
/*
 *    ===================
 *    Drain load/use pipe
 *    ===================
 */
/*
 *    Do Ar += xr*yr;  Ai += xi*yr; for all 4 columns of A
 */
      xn = x0a;                    /* xn  = {Xi, Xr} */
      x0a = _mm_mul_pd(x0a, y0a);  /* x0a = {Xi*Yr, Xr*Yr} */
      a00 = _mm_add_pd(a00, x0a);  /* a00 = {Ai+Xi*Yr, Ar+Xr*Yr} */
      x0a = xn;
      xn  = _mm_mul_pd(xn, y1a);   /* xn  = {Xi*Yr, Xr*Yr} */
      a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr, Ar+Xr*Yr} */
      xn = x0a;
      x0a = _mm_mul_pd(x0a, y2a);  /* x0a = {Xi*Yr, Xr*Yr} */
      a02 = _mm_add_pd(a02, x0a);  /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
      xn  = _mm_mul_pd(xn, y3a);   /* xn  = {Xi*Yr, Xr*Yr} */
      a03 = _mm_add_pd(a03, xn);   /* a02 = {Ai+Xi*Yr, Ar+Xr*Yr} */
/*
 *    Do Ar += xi*(-yi); Ai += xr*yi; for all 4 columns of A
 */
                                   /* y0b = {Yi,-Yi} */
      xn = x0b;                    /* xn  = {Xr, Xi} */
      x0b = _mm_mul_pd(x0b, y0b);  /* x0b = {Xr*Yi, -Xi*Yi} */
      a00 = _mm_add_pd(a00, x0b);  /* a00 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_store_pd(A0+i-2, a00);   /* *A0 = a00 */
      x0b = xn;                    /* x0b = {Xr, Xi} */
      xn  = _mm_mul_pd(xn,  y1b);  /* xn  = {Xr*Yi, -Xi*Yi} */
      a01 = _mm_add_pd(a01, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_store_pd(A1+i-2, a01);   /* *A1 = a01 */
      xn = x0b;
      x0b = _mm_mul_pd(x0b, y2b);  /* x0b = {Xr*Yi, -Xi*Yi} */
      a02 = _mm_add_pd(a02, x0b);  /* a02 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_store_pd(A2+i-2, a02);   /* *A2 = a02 */
      x0b = (__m128d)_mm_shuffle_epi32((__m128i)x0a, 0x4E);   /* x0b = {Yr, Xi} */
      xn  = _mm_mul_pd(xn,  y3b);  /* xn  = {Xr*Yi, -Xi*Yi} */
      a03 = _mm_add_pd(a03, xn);   /* a01 = {Ai+Xi*Yr+Xr*Yi, Ar+Xr*Yr-Xi*Yi} */
      _mm_store_pd(A3+i-2, a03);   /* *A3 = a03 */
   }
}
void ATL_UGERK
   (ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y, TYPE *A, ATL_CINT lda)
{
   size_t ia = (size_t) A, ix = (size_t) X;
   ATL_CINT N4 = (N>>2)<<2;
   const TYPE one[2] = {ATL_rone, ATL_rzero};

   if (N4)
   {
      if ((ia>>4)<<4 == ia && (ix>>4)<<4 == ix)
         gerk_sse3(M, N4, X, Y, A, lda);
      else
         gerk_sse3u(M, N4, X, Y, A, lda);
   }
   if (N4 != N)
      Mjoin(PATL,gerk_axpy)(M, N-N4, one, X, 1, Y+(N4 SHIFT), 1,
                            A+N4*(lda SHIFT), lda);

}
