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
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif
#include <xmmintrin.h>
#include <stdio.h>
#include "atlas_misc.h"

void ATL_UGEMV(ATL_CINT M, ATL_CINT N, const TYPE *A, ATL_CINT lda1,
               const TYPE *X, TYPE *Y)
{/* BEGIN GEMVN: nMU=1, MU=8, NU=4 */
   ATL_INT i, j;
   ATL_CINT MAp = ( (((((size_t)A)+15)>>4)<<4) - ((size_t)A) )/sizeof(TYPE);
   ATL_CINT MA=M-MAp;
   ATL_CINT M8=((MA/8)*8)+MAp, N4=((N/4)*4), lda2=lda1+lda1, lda3=lda2+lda1, lda4=lda3+lda1;
   __m128 y0, y1, y2, y3, y4, y5, y6, y7, x0, x1, x2, x3, a0_0, m0_0, a1_0, m1_0, a2_0, m2_0, a3_0, m3_0, a4_0, m4_0, a5_0, m5_0, a6_0, m6_0, a7_0, m7_0, a0_1, m0_1, a1_1, m1_1, a2_1, m2_1, a3_1, m3_1, a4_1, m4_1, a5_1, m5_1, a6_1, m6_1, a7_1, m7_1, a0_2, m0_2, a1_2, m1_2, a2_2, m2_2, a3_2, m3_2, a4_2, m4_2, a5_2, m5_2, a6_2, m6_2, a7_2, m7_2, a0_3, m0_3, a1_3, m1_3, a2_3, m2_3, a3_3, m3_3, a4_3, m4_3, a5_3, m5_3, a6_3, m6_3, a7_3, m7_3;

   #ifdef BETA0
      for (i=0; i < M; i++)
         Y[i] = ATL_rzero;
   #endif
   for (j=0; j < N4; j += 4, A += lda4, X += 4)
   {/* BEGIN N-LOOP UR=4 */
      x0 = _mm_load1_ps(X);
      x1 = _mm_load1_ps(X+1);
      x2 = _mm_load1_ps(X+2);
      x3 = _mm_load1_ps(X+3);
      for (i=0; i < MAp; i++)
      {/* peel to force X/A alignment */
         y0 = _mm_load_ss(Y+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
         a0_1 =_mm_load_ss(A+i+lda1);
         a0_1 = _mm_mul_ss(a0_1, x1);
         y0 = _mm_add_ss(y0, a0_1);
         a0_2 =_mm_load_ss(A+i+lda2);
         a0_2 = _mm_mul_ss(a0_2, x2);
         y0 = _mm_add_ss(y0, a0_2);
         a0_3 =_mm_load_ss(A+i+lda3);
         a0_3 = _mm_mul_ss(a0_3, x3);
         y0 = _mm_add_ss(y0, a0_3);
         _mm_store_ss(Y+i, y0);
      } /* end force-align peel */

      for (i=MAp; i < M8; i += 8)
      {/* ----- BEGIN M-LOOP BODY ----- */
         /* --- BEGIN MUxNU UNROLL 0 --- */
         y0 = _mm_load_ps(Y+i+0);
         a0_0 = _mm_load_ps(A+i+0);
         a0_0 = _mm_mul_ps(a0_0, x0);
         y0 = _mm_add_ps(y0, a0_0);
         y4 = _mm_load_ps(Y+i+4);
         a4_0 = _mm_load_ps(A+i+4);
         a4_0 = _mm_mul_ps(a4_0, x0);
         y4 = _mm_add_ps(y4, a4_0);

         a0_1 = _mm_load_ps(A+i+lda1);
         a0_1 = _mm_mul_ps(a0_1, x1);
         y0 = _mm_add_ps(y0, a0_1);
         a4_1 = _mm_load_ps(A+i+4+lda1);
         a4_1 = _mm_mul_ps(a4_1, x1);
         y4 = _mm_add_ps(y4, a4_1);

         a0_2 = _mm_load_ps(A+i+lda2);
         a0_2 = _mm_mul_ps(a0_2, x2);
         y0 = _mm_add_ps(y0, a0_2);
         a4_2 = _mm_load_ps(A+i+4+lda2);
         a4_2 = _mm_mul_ps(a4_2, x2);
         y4 = _mm_add_ps(y4, a4_2);

         a0_3 = _mm_load_ps(A+i+lda3);
         a0_3 = _mm_mul_ps(a0_3, x3);
         y0 = _mm_add_ps(y0, a0_3);
         a4_3 = _mm_load_ps(A+i+4+lda3);
         a4_3 = _mm_mul_ps(a4_3, x3);
         y4 = _mm_add_ps(y4, a4_3);

         _mm_store_ps(Y+i, y0);
         _mm_store_ps(Y+i+4, y4);
         /* --- END MUxNU UNROLL 0 --- */
      }/* ----- END M-LOOP BODY ----- */
      if (M != M8)
      {/* ----- BEGIN VECTOR UNROLL M CLEANUP ----- */
         for (i=M8; i < M; i++)
         {/* ----- BEGIN SCALAR M CLEANUP ----- */
         y0 = _mm_load_ss(Y+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
         a0_1 =_mm_load_ss(A+i+lda1);
         a0_1 = _mm_mul_ss(a0_1, x1);
         y0 = _mm_add_ss(y0, a0_1);
         a0_2 =_mm_load_ss(A+i+lda2);
         a0_2 = _mm_mul_ss(a0_2, x2);
         y0 = _mm_add_ss(y0, a0_2);
         a0_3 =_mm_load_ss(A+i+lda3);
         a0_3 = _mm_mul_ss(a0_3, x3);
         y0 = _mm_add_ss(y0, a0_3);
         _mm_store_ss(Y+i, y0);
         }/* ----- END SCALAR M CLEANUP ----- */
      }/* ----- END VECTOR UNROLL M CLEANUP ----- */
   }/* END N-LOOP UR=4 */

   for (j=N4; j < N; j += 1, A += lda1, X++)
   {/* BEGIN N-LOOP UR=1 */
      x0 = _mm_load1_ps(X);
      for (i=0; i < MAp; i++)
      {/* peel to force X/A alignment */
         y0 = _mm_load_ss(Y+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
         _mm_store_ss(Y+i, y0);
      } /* end force-align peel */

      for (i=MAp; i < M8; i += 8)
      {/* ----- BEGIN M-LOOP BODY ----- */
         /* --- BEGIN MUxNU UNROLL 0 --- */
         y0 = _mm_load_ps(Y+i+0);
         a0_0 = _mm_load_ps(A+i+0);
         a0_0 = _mm_mul_ps(a0_0, x0);
         y0 = _mm_add_ps(y0, a0_0);
         y4 = _mm_load_ps(Y+i+4);
         a4_0 = _mm_load_ps(A+i+4);
         a4_0 = _mm_mul_ps(a4_0, x0);
         y4 = _mm_add_ps(y4, a4_0);
         _mm_store_ps(Y+i, y0);
         _mm_store_ps(Y+i+4, y4);
         /* --- END MUxNU UNROLL 0 --- */
      }/* ----- END M-LOOP BODY ----- */
      if (M != M8)
      {/* ----- BEGIN VECTOR UNROLL M CLEANUP ----- */
         for (i=M8; i < M; i++)
         {/* ----- BEGIN SCALAR M CLEANUP ----- */
         y0 = _mm_load_ss(Y+i);
         a0_0 =_mm_load_ss(A+i);
         a0_0 = _mm_mul_ss(a0_0, x0);
         y0 = _mm_add_ss(y0, a0_0);
         _mm_store_ss(Y+i, y0);
         }/* ----- END SCALAR M CLEANUP ----- */
      }/* ----- END VECTOR UNROLL M CLEANUP ----- */
   }/* END N-LOOP UR=1 */
}/* END GER: nMU=1, MU=8, NU=4 */
#ifdef MA
   #undef MA
#endif
#ifdef MAp
   #undef MAp
#endif
