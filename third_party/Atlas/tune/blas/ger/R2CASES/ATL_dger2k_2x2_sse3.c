/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2012, 2010 R. Clint Whaley
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
#ifdef ATL_USEREAL
   #ifdef ATL_ALIGNED
      #define vloada(addr_) ( (__m128d)_mm_load_ps((float*)(addr_)) )
      #define vstorea(addr_, r_) _mm_store_ps((float*)(addr_), (__m128)(r_))
   #else
      #define vloada(addr_) ( (__m128d)_mm_loadu_ps((void*)(addr_)) )
      #define vstorea(addr_, r_) _mm_storeu_ps((float*)(addr_), (__m128)(r_))
   #endif
   #define vload(addr_) ( (__m128d)_mm_load_ps((float*)(addr_)) )
   #define sload(addr_) ( (__m128d)_mm_load_ss((float*)(addr_)) )
   #define vmovh2l(vsrc_) ( (__m128d)_mm_movehl_ps((__m128)(vsrc_), (__m128)(vsrc_)) )
#else
   #ifdef ATL_ALIGNED
      #define vloada(addr_) _mm_load_pd((addr_))
      #define vstorea(addr_, r_) _mm_store_pd((addr_), (r_))
   #else
      #define vloada(addr_) _mm_loadu_pd(addr_)
      #define vstorea(addr_, r_) _mm_storeu_pd((addr_), (r_))
   #endif
   #define vload(addr_) _mm_load_pd((addr_))
   #define sload(addr_) _mm_load_sd((addr_))
   #define vmovh2l(vsrc_) _mm_unpackhi_pd((vsrc_), (vsrc_) )
#endif
#define sstore(addr_, r_) _mm_store_sd((addr_), (r_))
#ifdef GCC_FINALLY_HAS_SSE
   #define vdup0(src_) _mm_movedup_pd(src_);
#else
   #define vdup0(src_) ( (__m128d) _mm_shuffle_epi32((__m128i)(src_), 0x44) )
#endif


void ATL_UGER2K
   (ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y, const TYPE *W,
    const TYPE *Z, TYPE *A, ATL_CINT lda)
/*
 * A += xy + wz
 */
{
   register __m128d a00, a01, p0, p1;
   register __m128d x0_x1, w0_w1, y0_y0, z0_z0, y1_y1, z1_z1;
   ATL_CINT M2 = (M>>1)<<1, N2 = (N>>1)<<1, incA = lda+lda;
   TYPE *A0 = A, *A1 = A + lda;
   ATL_INT i, j;

   for (j=0; j < N2; j += 2, Y += 2, Z += 2, A0 += incA, A1 += incA)
   {
      y1_y1 = vload(Y);                    /* y1_y1 = {y1, y0} */
      y0_y0 = vdup0(y1_y1);                /* y0_y0 = {y0, y0} */
      y1_y1 = vmovh2l(y1_y1);              /* y1_y1 = {y1, y1} */

      z1_z1 = vload(Z);                    /* z1_z1 = {z1, z0} */
      z0_z0 = vdup0(z1_z1);                /* z0_z0 = {z0, z0} */
      z1_z1 = vmovh2l(z1_z1);              /* z1_z1 = {z1, z1} */
      for (i=0; i < M2; i += 2)
      {
         x0_x1 = vload(X+i);             /* x0_x1 = {x1, x0} */
         p0 = _mm_mul_pd(x0_x1, y0_y0);  /* p0    = {x1*y0, x0*y0} */
         a00 = vloada(A0+i);             /* a00 = {a10, a00} */
         p1 = _mm_mul_pd(x0_x1, y1_y1);  /* p1    = {x1*y1, x0*y1} */
         a00 = _mm_add_pd(a00, p0);      /* a00   = {a10+x1*y0,a00+x0*y0} */
         a01 = vloada(A1+i);             /* a01   = {a11, a01} */
         a01 = _mm_add_pd(a01, p1);      /* a01   = {a11+x1*y1,a10+x0*y1} */
         w0_w1 = vload(W+i);             /* w0_w1 = {w1, w0} */
         p0 = _mm_mul_pd(w0_w1, z0_z0);  /* p0    = {w1*z0, w0*z0} */
         a00 = _mm_add_pd(a00, p0); /* a00={a10+x1*y0+w1*z0,a00+x0*y0+w0*z0} */
         vstorea(A0+i, a00);
         p1 = _mm_mul_pd(w0_w1, z1_z1);  /* p1    = {w1*z1, w0*z1} */
         a01 = _mm_add_pd(a01, p1); /* a01={a11+x1*y1+w1*z1,a10+x0*y1+w0*z1} */
         vstorea(A1+i, a01);
      }
      if (M2 != M)
      {
         x0_x1 = sload(X+i);             /* x0_x1 = {XX, x0} */
         p0 = _mm_mul_sd(x0_x1, y0_y0);  /* p0    = {XXXXX, x0*y0} */
         a00 = sload(A0+i);              /* a00 = {XXX, a00} */
         p1 = _mm_mul_sd(x0_x1, y1_y1);  /* p1    = {XXXXX, x0*y1} */
         a00 = _mm_add_sd(a00, p0);      /* a00   = {XXXXXXXXX,a00+x0*y0} */
         a01 = sload(A1+i);              /* a01   = {XXX, a01} */
         a01 = _mm_add_pd(a01, p1);      /* a01   = {a11+x1*y1,a10+x0*y1} */
         w0_w1 = sload(W+i);             /* w0_w1 = {XX, w0} */
         p0 = _mm_mul_sd(w0_w1, z0_z0);  /* p0    = {XXXXX, w0*z0} */
         a00 = _mm_add_sd(a00, p0); /* a00={XXXXXXXXXXXXXXX,a00+x0*y0+w0*z0} */
         sstore(A0+i, a00);
         p1 = _mm_mul_sd(w0_w1, z1_z1);  /* p1    = {XXXXX, w0*z1} */
         a01 = _mm_add_sd(a01, p1); /* a01={XXXXXXXXXXXXXXX,a10+x0*y1+w0*z1} */
         sstore(A1+i, a01);
      }
   }
}
