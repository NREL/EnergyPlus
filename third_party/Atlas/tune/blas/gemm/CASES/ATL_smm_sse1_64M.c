/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 2000 Peter Soendergaard
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
#define SSE
#include "SSE3Dnow.h"
#include "atlas_misc.h"

void ATL_USERMM
(const int M, const int N, const int K, const float alpha, const float *A, const int lda, const float *B, const int ldb, const float beta, float *C, const int ldc)

{
   /*--- program info ---*/
   /*  $Revision: 1.3 $  */
   /*  loadfirst = 'b'  */
   /*  nu = 2  */
   /*  k_loop = None  */
   /*  problem = 'gemm_m'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 1  */
   /*  ku = 4  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  used_outside_len = 64  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_lastuse = 'a'  */
   /*  outside_len = 64  */
   /*  lastuse = 1  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 1  */
   /*  used_directload_a = 0  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   float *pC1 = C+(ldc SHIFT);
   const float *stM = A + M*KB;
   const float *stN = B + NB*KB;
   const int incAm = 1*KB-KB+64;
   const int incBm = -KB+64;
   const int incCm = (1 SHIFT);
   const int incAn = -M*KB;
   const int incBn = 2*KB;
   const int incCn = ((ldc*2-M) SHIFT);

   /*--- initial arhitecture specific statements ---*/

   /*--- main program statements ---*/
   vec_mov_mr_1(&beta,reg0);
   vec_mov_rm(reg0,betavec);
   do /* N-loop */
   {
      do /* M-loop */
      {
#ifdef BETA0
         vec_mov_mr(zerovec,reg7);
         vec_mov_rr(reg7,reg0);
         vec_mov_rr(reg7,reg1);
#elif defined(BETA1)
         vec_mov_mr_1(pC0,reg0);
         vec_mov_mr_1(pC1,reg1);
#else
         vec_mov_mr(betavec,reg7);
         vec_mov_mr_1(pC0,reg0);
         vec_mul_rr(reg7,reg0);
         vec_mov_mr_1(pC1,reg1);
         vec_mul_rr(reg7,reg1);
#endif
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pB0,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+4,reg4);
         vec_mov_mr_a(pB0+4,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+4+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+8,reg4);
         vec_mov_mr_a(pB0+8,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+8+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+12,reg4);
         vec_mov_mr_a(pB0+12,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+12+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+16,reg4);
         vec_mov_mr_a(pB0+16,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+16+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+20,reg4);
         vec_mov_mr_a(pB0+20,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+20+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+24,reg4);
         vec_mov_mr_a(pB0+24,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+24+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+28,reg4);
         vec_mov_mr_a(pB0+28,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+28+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+32,reg4);
         vec_mov_mr_a(pB0+32,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+32+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+36,reg4);
         vec_mov_mr_a(pB0+36,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+36+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+40,reg4);
         vec_mov_mr_a(pB0+40,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+40+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+44,reg4);
         vec_mov_mr_a(pB0+44,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+44+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+48,reg4);
         vec_mov_mr_a(pB0+48,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+48+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+52,reg4);
         vec_mov_mr_a(pB0+52,reg3);
         vec_mul_rr(reg4,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pB0+52+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+56,reg4);
         vec_mov_mr_a(pB0+56,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+56+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+60,reg4);
         vec_mov_mr_a(pB0+60,reg2);
         vec_mul_rr(reg4,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pB0+60+KB,reg4);
         vec_add_rr(reg4,reg1);
         vec_sum(reg0);
         vec_sum(reg1);
         vec_mov_rm_1(reg0,pC0);
         vec_mov_rm_1(reg1,pC1);
         pA0 += incAm;
         pB0 += incBm;
         pC0 += incCm;
         pC1 += incCm;
      }
      while(pA0 != stM);

      pA0 += incAn;
      pB0 += incBn;
      pC0 += incCn;
      pC1 += incCn;
   }
   while(pB0 != stN);

   }
