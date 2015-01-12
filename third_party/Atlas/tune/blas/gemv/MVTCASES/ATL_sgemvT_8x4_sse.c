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
#include <stdio.h>
#include "atlas_misc.h"
#define _my_hadd_ps(dst, src) \
   __asm__ __volatile__ ("haddps %2, %0" : "=x"(dst) : "0" (dst), "x"(src))


void ATL_UGEMV(ATL_CINT M, ATL_CINT N, const TYPE *A, ATL_CINT lda1,
               const TYPE *X, TYPE *Y)
{/* BEGIN GEMVN: nMU=1, MU=8, NU=4 */
   ATL_INT i, j;
   ATL_CINT MAp = (M > 11) ?
       ( (((((size_t)A)+15)>>4)<<4) - ((size_t)A) )/sizeof(TYPE) : M;
   ATL_CINT MA=M-MAp;
   ATL_CINT M8=((MA/8)*8)+MAp, N4=((N/4)*4), lda2=lda1+lda1, lda3=lda2+lda1, lda4=lda3+lda1;
   __m128 y0, y1, y2, y3, x0, x4;
   __m128 a0_0, a4_0, a0_1, a4_1, a0_2, a4_2, a0_3, a4_3;

   for (j=0; j < N4; j += 4, A += lda4, Y += 4)
   {/* BEGIN N-LOOP UR=4 */
      if (MAp)
      {
         i=0;
         x0 = _mm_load_ss(X+i);
         y0 =_mm_load_ss(A+i);
         y0 = _mm_mul_ss(y0, x0);
         y1 =_mm_load_ss(A+i+lda1);
         y1 = _mm_mul_ss(y1, x0);
         y2 =_mm_load_ss(A+i+lda2);
         y2 = _mm_mul_ss(y2, x0);
         y3 =_mm_load_ss(A+i+lda3);
         y3 = _mm_mul_ss(y3, x0);
         for (i=1; i < MAp; i++)
         {/* peel to force X/A alignment */
            x0 = _mm_load_ss(X+i);
            a0_0 =_mm_load_ss(A+i);
            a0_0 = _mm_mul_ss(a0_0, x0);
            y0 = _mm_add_ss(y0, a0_0);
            a0_1 =_mm_load_ss(A+i+lda1);
            a0_1 = _mm_mul_ss(a0_1, x0);
            y1 = _mm_add_ss(y1, a0_1);
            a0_2 =_mm_load_ss(A+i+lda2);
            a0_2 = _mm_mul_ss(a0_2, x0);
            y2 = _mm_add_ss(y2, a0_2);
            a0_3 =_mm_load_ss(A+i+lda3);
            a0_3 = _mm_mul_ss(a0_3, x0);
            y3 = _mm_add_ss(y3, a0_3);
         } /* end force-align peel */
      }
      else
      {
         y0 = _mm_xor_ps(y0, y0);
         y1 = _mm_xor_ps(y1, y1);
         y2 = _mm_xor_ps(y2, y2);
         y3 = _mm_xor_ps(y3, y3);
      }
      for (i=MAp; i < M8; i += 8)
      {/* ----- BEGIN M-LOOP BODY ----- */
         x0 = _mm_load_ps(X+i);
         a0_0 =_mm_load_ps(A+i);
         a0_0 = _mm_mul_ps(a0_0, x0);
         y0 = _mm_add_ps(y0, a0_0);
         a0_1 =_mm_load_ps(A+lda1+i);
         a0_1 = _mm_mul_ps(a0_1, x0);
         y1 = _mm_add_ps(y1, a0_1);
         a0_2 =_mm_load_ps(A+lda2+i);
         a0_2 = _mm_mul_ps(a0_2, x0);
         y2 = _mm_add_ps(y2, a0_2);
         a0_3 =_mm_load_ps(A+lda3+i);
         a0_3 = _mm_mul_ps(a0_3, x0);
         y3 = _mm_add_ps(y3, a0_3);

         x4 = _mm_load_ps(X+i+4);
         a4_0 =_mm_load_ps(A+i+4);
         a4_0 = _mm_mul_ps(a4_0, x4);
         y0 = _mm_add_ps(y0, a4_0);
         a4_1 =_mm_load_ps(A+lda1+i+4);
         a4_1 = _mm_mul_ps(a4_1, x4);
         y1 = _mm_add_ps(y1, a4_1);
         a4_2 =_mm_load_ps(A+lda2+i+4);
         a4_2 = _mm_mul_ps(a4_2, x4);
         y2 = _mm_add_ps(y2, a4_2);
         a4_3 =_mm_load_ps(A+lda3+i+4);
         a4_3 = _mm_mul_ps(a4_3, x4);
         y3 = _mm_add_ps(y3, a4_3);
      }/* ----- END M-LOOP BODY ----- */
      for (i=M8; i < M; i++)
      {/* ----- BEGIN SCALAR M CLEANUP ----- */
         x0 = _mm_load_ss(X+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
         a0_1 =_mm_load_ss(A+i+lda1);
         a0_1 = _mm_mul_ss(a0_1, x0);
         y1 = _mm_add_ss(y1, a0_1);
         a0_2 =_mm_load_ss(A+i+lda2);
         a0_2 = _mm_mul_ss(a0_2, x0);
         y2 = _mm_add_ss(y2, a0_2);
         a0_3 =_mm_load_ss(A+i+lda3);
         a0_3 = _mm_mul_ss(a0_3, x0);
         y3 = _mm_add_ss(y3, a0_3);
      }/* ----- END SCALAR M CLEANUP ----- */
                            /* y3 = {y3d, y3c, y3b, y3a} */
                            /* y2 = {y2d, y2c, y2b, y2a} */
                            /* y1 = {y1d, y1c, y1b, y1a} */
                            /* y0 = {y0d, y0c, y0b, y0a} */
      _my_hadd_ps(y0, y1);  /* y0 = {y1d+y1c, y1b+y1a, y0d+y0c, y0b+y0a} */
      _my_hadd_ps(y2, y3);  /* y2 = {y3d+y3c, y3b+y3a, y2d+y2c, y2b+y2a} */
      _my_hadd_ps(y0, y2);  /* y0 = {y3abcd, y2abcd, y1abcd, y0abcd} */
      #ifndef BETA0
         a0_0 = _mm_load_ps(Y);
         y0 = _mm_add_ps(y0, a0_0);
      #endif
      _mm_store_ps(Y, y0);
   }/* END N-LOOP UR=4 */

   for (j=N4; j < N; j++, A += lda1, Y++)
   {/* BEGIN N-LOOP UR=1 */
      y0 = _mm_xor_ps(y0, y0);
      y1 = _mm_xor_ps(y1, y1);
      for (i=0; i < MAp; i++)
      {/* peel to force X/A alignment */
         x0 = _mm_load_ss(X+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
      } /* end force-align peel */

      for (i=MAp; i < M8; i += 8)
      {/* ----- BEGIN M-LOOP BODY ----- */
         x0 = _mm_load_ps(X+i);
         a0_0 =_mm_load_ps(A+i);
         a0_0 = _mm_mul_ps(a0_0, x0);
         y0 = _mm_add_ps(y0, a0_0);
         x4 = _mm_load_ps(X+i+4);
         a4_0 =_mm_load_ps(A+i+4);
         a4_0 = _mm_mul_ps(a4_0, x4);
         y1 = _mm_add_ps(y1, a4_0);
      }/* ----- END M-LOOP BODY ----- */
      for (i=M8; i < M; i++)
      {/* ----- BEGIN SCALAR M CLEANUP ----- */
         x0 = _mm_load_ss(X+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
      }/* ----- END SCALAR M CLEANUP ----- */
                            /* y1 = {y0h, y0g, y0f, y0e} */
                            /* y0 = {y0d, y0c, y0b, y0a} */
      y0 = _mm_add_ps(y0, y1);
      _my_hadd_ps(y0, y0);
      _my_hadd_ps(y0, y0);
      #ifndef BETA0
         a0_0 = _mm_load_ss(Y);
         y0 = _mm_add_ss(y0, a0_0);
      #endif
      _mm_store_ss(Y, y0);
   }/* END N-LOOP UR=1 */
}/* END GER: nMU=1, MU=8, NU=4 */
#ifdef MA
   #undef MA
#endif
#ifdef MAp
   #undef MAp
#endif
