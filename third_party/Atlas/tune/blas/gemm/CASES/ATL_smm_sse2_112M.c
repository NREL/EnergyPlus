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
   /*  nu = 1  */
   /*  k_loop = None  */
   /*  problem = 'gemm_m'  */
   /*  nregs = 8  */
   /*  split = 1  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  ku = 16  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  used_outside_len = 112  */
   /*  k_cleanup = {}  */
   /*  outputdir = 'Linux_P4/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  atlasname = 'Linux_P4'  */
   /*  method = 'acc'  */
   /*  used_lastuse = None  */
   /*  used_directload_a = 1  */
   /*  outside_len = 112  */
   /*  veclen = 4  */
   /*  sched = ['spread', 'fuse']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 0  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + (M-M%4)*KB;
   const float *stN = B + NB*KB;
   const int incAm = 4*KB-KB+112;
   const int incBm = -KB+112;
   const int incCm = (4 SHIFT);
   const int incAn = -(M-M%4)*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-(M-M%4)) SHIFT);
   const int incAm_m = KB-KB+112;
   const int incAn_m = -(M%4)*KB;
   const int incCn_m = (ldc*1-(M%4))SHIFT;
   const float *stM_m = A + M*KB;

   /*--- initial arhitecture specific statements ---*/

   /*--- main program statements ---*/
   vec_mov_mr_1(&beta,reg0);
   vec_mov_rm(reg0,betavec);
   if (M>=4)
   {
      do /* N-loop */
      {
         do /* M-loop */
         {
#ifdef BETA0
            vec_mov_mr(zerovec,reg7);
            vec_mov_rr(reg7,reg0);
            vec_mov_rr(reg7,reg1);
            vec_mov_rr(reg7,reg2);
            vec_mov_rr(reg7,reg3);
#elif defined(BETA1)
            vec_mov_mr_1(pC0,reg0);
            vec_mov_mr_1(pC0+(1 SHIFT),reg1);
            vec_mov_mr_1(pC0+(2 SHIFT),reg2);
            vec_mov_mr_1(pC0+(3 SHIFT),reg3);
#else
            vec_mov_mr(betavec,reg7);
            vec_mov_mr_1(pC0,reg0);
            vec_mul_rr(reg7,reg0);
            vec_mov_mr_1(pC0+(1 SHIFT),reg1);
            vec_mul_rr(reg7,reg1);
            vec_mov_mr_1(pC0+(2 SHIFT),reg2);
            vec_mul_rr(reg7,reg2);
            vec_mov_mr_1(pC0+(3 SHIFT),reg3);
            vec_mul_rr(reg7,reg3);
#endif
            vec_mov_mr_a(pB0,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+4,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+4,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+4+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+4+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+4+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+8,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+8,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+8+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+8+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+8+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+12,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+12,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+12+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+12+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+12+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+16,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+16,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+16+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+16+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+16+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+20,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+20,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+20+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+20+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+20+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+24,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+24,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+24+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+24+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+24+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+28,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+28,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+28+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+28+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+28+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+32,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+32,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+32+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+32+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+32+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+36,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+36,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+36+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+36+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+36+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+40,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+40,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+40+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+40+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+40+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+44,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+44,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+44+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+44+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+44+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+48,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+48,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+48+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+48+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+48+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+52,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+52,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+52+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+52+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+52+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+56,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+56,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+56+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+56+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+56+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+60,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+60,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+60+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+60+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+60+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+64,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+64,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+64+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+64+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+64+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+68,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+68,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+68+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+68+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+68+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+72,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+72,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+72+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+72+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+72+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+76,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+76,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+76+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+76+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+76+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+80,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+80,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+80+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+80+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+80+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+84,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+84,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+84+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+84+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+84+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+88,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+88,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+88+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+88+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+88+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+92,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+92,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+92+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+92+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+92+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+96,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+96,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+96+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+96+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+96+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+100,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+100,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+100+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+100+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+100+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+104,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+104,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+104+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+104+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+104+3*KB,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+108,reg7);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+108,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_rr(reg7,reg5);
            vec_mul_mr_a(pA0+108+KB,reg5);
            vec_add_rr(reg5,reg1);
            vec_mov_rr(reg7,reg6);
            vec_mul_mr_a(pA0+108+2*KB,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_rr(reg7,reg4);
            vec_mul_mr_a(pA0+108+3*KB,reg4);
            vec_add_rr(reg4,reg3);
#ifndef TCPLX
            vec_sum_full(reg0,reg1,reg2,reg3,reg5,reg6,reg7);
            vec_mov_rm(reg5,pC0);
#else
            vec_sum(reg0);
            vec_sum(reg1);
            vec_sum(reg2);
            vec_sum(reg3);
            vec_mov_rm_1(reg0,pC0);
            vec_mov_rm_1(reg1,pC0+(1 SHIFT));
            vec_mov_rm_1(reg2,pC0+(2 SHIFT));
            vec_mov_rm_1(reg3,pC0+(3 SHIFT));
#endif
            pA0 += incAm;
            pB0 += incBm;
            pC0 += incCm;
         }
         while(pA0 != stM);

         pA0 += incAn;
         pB0 += incBn;
         pC0 += incCn;
      }
      while(pB0 != stN);

   }
   if (M%4>0)
   {
      pC0 = C+((M-M%4)SHIFT);
      pA0 = A+(M-M%4)*KB;
      pB0 = B;
      do /* N-loop */
      {
         do /* M-loop */
         {
#ifdef BETA0
            vec_mov_mr(zerovec,reg7);
            vec_mov_rr(reg7,reg0);
#elif defined(BETA1)
            vec_mov_mr_1(pC0,reg0);
#else
            vec_mov_mr(betavec,reg7);
            vec_mov_mr_1(pC0,reg0);
            vec_mul_rr(reg7,reg0);
#endif
            vec_mov_mr_a(pB0,reg1);
            vec_mul_mr_a(pA0,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+4,reg2);
            vec_mul_mr_a(pA0+4,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+8,reg3);
            vec_mul_mr_a(pA0+8,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+12,reg1);
            vec_mul_mr_a(pA0+12,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+16,reg2);
            vec_mul_mr_a(pA0+16,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+20,reg3);
            vec_mul_mr_a(pA0+20,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+24,reg1);
            vec_mul_mr_a(pA0+24,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+28,reg2);
            vec_mul_mr_a(pA0+28,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+32,reg3);
            vec_mul_mr_a(pA0+32,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+36,reg1);
            vec_mul_mr_a(pA0+36,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+40,reg2);
            vec_mul_mr_a(pA0+40,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+44,reg3);
            vec_mul_mr_a(pA0+44,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+48,reg1);
            vec_mul_mr_a(pA0+48,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+52,reg2);
            vec_mul_mr_a(pA0+52,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+56,reg3);
            vec_mul_mr_a(pA0+56,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+60,reg1);
            vec_mul_mr_a(pA0+60,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+64,reg2);
            vec_mul_mr_a(pA0+64,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+68,reg3);
            vec_mul_mr_a(pA0+68,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+72,reg1);
            vec_mul_mr_a(pA0+72,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+76,reg2);
            vec_mul_mr_a(pA0+76,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+80,reg3);
            vec_mul_mr_a(pA0+80,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+84,reg1);
            vec_mul_mr_a(pA0+84,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+88,reg2);
            vec_mul_mr_a(pA0+88,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+92,reg3);
            vec_mul_mr_a(pA0+92,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+96,reg1);
            vec_mul_mr_a(pA0+96,reg1);
            vec_add_rr(reg1,reg0);
            vec_mov_mr_a(pB0+100,reg2);
            vec_mul_mr_a(pA0+100,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr_a(pB0+104,reg3);
            vec_mul_mr_a(pA0+104,reg3);
            vec_add_rr(reg3,reg0);
            vec_mov_mr_a(pB0+108,reg1);
            vec_mul_mr_a(pA0+108,reg1);
            vec_add_rr(reg1,reg0);
            vec_sum(reg0);
            vec_mov_rm_1(reg0,pC0);
            pA0 += incAm_m;
            pB0 += incBm;
            pC0 += (1 SHIFT);
         }
         while(pA0 != stM_m);

         pA0 += incAn_m;
         pB0 += incBn;
         pC0 += incCn_m;
      }
      while(pB0 != stN);

   }
   }
