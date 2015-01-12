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
   /*  loadfirst = 'a'  */
   /*  nu = 1  */
   /*  k_loop = None  */
   /*  problem = 'gemm_n'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  ku = 4  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  used_outside_len = 56  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_lastuse = 'b'  */
   /*  outside_len = 56  */
   /*  lastuse = 1  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  used_directload_a = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + N*KB;
   const int incAm = 2*KB-KB+56;
   const int incBm = -KB+56;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

   /*--- initial arhitecture specific statements ---*/

   /*--- main program statements ---*/
   vec_mov_mr_1(&beta,reg0);
   vec_mov_rm(reg0,betavec);
   while(pB0 != stN)
   {
      do /* M-loop */
      {
#ifdef BETA0
         vec_mov_mr(zerovec,reg7);
         vec_mov_rr(reg7,reg0);
         vec_mov_rr(reg7,reg1);
#elif defined(BETA1)
         vec_mov_mr_1(pC0,reg0);
         vec_mov_mr_1(pC0+(1 SHIFT),reg1);
#else
         vec_mov_mr(betavec,reg7);
         vec_mov_mr_1(pC0,reg0);
         vec_mul_rr(reg7,reg0);
         vec_mov_mr_1(pC0+(1 SHIFT),reg1);
         vec_mul_rr(reg7,reg1);
#endif
         vec_mov_mr_a(pB0,reg5);
         vec_mov_mr_a(pA0,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+KB,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pB0+4,reg6);
         vec_mov_mr_a(pA0+4,reg3);
         vec_mul_rr(reg6,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pA0+4+KB,reg6);
         vec_add_rr(reg6,reg1);
         vec_mov_mr_a(pB0+8,reg7);
         vec_mov_mr_a(pA0+8,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_mr_a(pA0+8+KB,reg7);
         vec_add_rr(reg7,reg1);
         vec_mov_mr_a(pB0+12,reg5);
         vec_mov_mr_a(pA0+12,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+12+KB,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pB0+16,reg6);
         vec_mov_mr_a(pA0+16,reg3);
         vec_mul_rr(reg6,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pA0+16+KB,reg6);
         vec_add_rr(reg6,reg1);
         vec_mov_mr_a(pB0+20,reg7);
         vec_mov_mr_a(pA0+20,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_mr_a(pA0+20+KB,reg7);
         vec_add_rr(reg7,reg1);
         vec_mov_mr_a(pB0+24,reg5);
         vec_mov_mr_a(pA0+24,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+24+KB,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pB0+28,reg6);
         vec_mov_mr_a(pA0+28,reg3);
         vec_mul_rr(reg6,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pA0+28+KB,reg6);
         vec_add_rr(reg6,reg1);
         vec_mov_mr_a(pB0+32,reg7);
         vec_mov_mr_a(pA0+32,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_mr_a(pA0+32+KB,reg7);
         vec_add_rr(reg7,reg1);
         vec_mov_mr_a(pB0+36,reg5);
         vec_mov_mr_a(pA0+36,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+36+KB,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pB0+40,reg6);
         vec_mov_mr_a(pA0+40,reg3);
         vec_mul_rr(reg6,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pA0+40+KB,reg6);
         vec_add_rr(reg6,reg1);
         vec_mov_mr_a(pB0+44,reg7);
         vec_mov_mr_a(pA0+44,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_mr_a(pA0+44+KB,reg7);
         vec_add_rr(reg7,reg1);
         vec_mov_mr_a(pB0+48,reg5);
         vec_mov_mr_a(pA0+48,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+48+KB,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pB0+52,reg5);
         vec_mov_mr_a(pA0+52,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+52+KB,reg5);
         vec_add_rr(reg5,reg1);
         vec_sum(reg0);
         vec_sum(reg1);
         vec_mov_rm_1(reg0,pC0);
         vec_mov_rm_1(reg1,pC0+(1 SHIFT));
         pA0 += incAm;
         pB0 += incBm;
         pC0 += incCm;
      }
      while(pA0 != stM);

      pA0 += incAn;
      pB0 += incBn;
      pC0 += incCn;
   }

   }
