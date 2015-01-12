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

#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif

#define SSE2
#include "SSE3Dnow.h"
#include "atlas_misc.h"

void ATL_USERMM
(const int M, const int N, const int K, const double alpha, const double *A, const int lda, const double *B, const int ldb, const double beta, double *C, const int ldc)

{
   /*--- program info ---*/
   /*  $Revision: 1.3 $  */
   /*  loadfirst = 'a'  */
   /*  nu = 1  */
   /*  k_loop = 'for'  */
   /*  problem = 'gemm_n'  */
   /*  nregs = 8  */
   /*  split = 1  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  ku = 8  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'double'  */
   /*  m_cleanup = {}  */
   /*  used_outside_len = 8  */
   /*  k_cleanup = {}  */
   /*  outputdir = 'Linux_P4/'  */
   /*  arch = 'sse2'  */
   /*  pipelength = 3  */
   /*  atlasname = 'Linux_P4'  */
   /*  method = 'acc'  */
   /*  used_lastuse = None  */
   /*  used_directload_a = 1  */
   /*  outside_len = 'ku'  */
   /*  veclen = 2  */
   /*  sched = ['spread', 'fuse']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 0  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0};
   const double *pA0 = A;
   const double *pB0 = B;
   double *pC0 = C;
   const double *stM = A + MB*KB;
   const double *stN = B + N*KB;
   const int incAm = 4*KB-KB+8;
   const int incBm = -KB+8;
   const int incCm = (4 SHIFT);
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
         vec_mov_mr_a(pA0,reg4);
         vec_mul_rr(reg7,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mov_mr_a(pA0+2*KB,reg6);
         for (k=0; k<KB-8; k+=8)
         {
            vec_add_rr(reg4,reg0);
            vec_mul_rr(reg7,reg5);
            vec_add_rr(reg5,reg1);
            vec_mul_rr(reg7,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_mr_a(pA0+3*KB,reg4);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+2,reg7);
            vec_mov_mr_a(pA0+2,reg5);
            vec_mul_rr(reg7,reg5);
            vec_mov_mr_a(pA0+2+KB,reg6);
            vec_mov_mr_a(pA0+2+2*KB,reg4);
            vec_add_rr(reg5,reg0);
            vec_mul_rr(reg7,reg6);
            vec_add_rr(reg6,reg1);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg2);
            vec_mov_mr_a(pA0+2+3*KB,reg5);
            vec_mul_rr(reg7,reg5);
            vec_add_rr(reg5,reg3);
            vec_mov_mr_a(pB0+4,reg7);
            vec_mov_mr_a(pA0+4,reg6);
            vec_mul_rr(reg7,reg6);
            vec_mov_mr_a(pA0+4+KB,reg4);
            vec_mov_mr_a(pA0+4+2*KB,reg5);
            vec_add_rr(reg6,reg0);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg1);
            vec_mul_rr(reg7,reg5);
            vec_add_rr(reg5,reg2);
            vec_mov_mr_a(pA0+4+3*KB,reg6);
            vec_mul_rr(reg7,reg6);
            vec_add_rr(reg6,reg3);
            vec_mov_mr_a(pB0+6,reg7);
            vec_mov_mr_a(pA0+6,reg4);
            vec_mul_rr(reg7,reg4);
            vec_mov_mr_a(pA0+6+KB,reg5);
            vec_mov_mr_a(pA0+6+2*KB,reg6);
            vec_add_rr(reg4,reg0);
            vec_mul_rr(reg7,reg5);
            vec_add_rr(reg5,reg1);
            vec_mul_rr(reg7,reg6);
            vec_add_rr(reg6,reg2);
            vec_mov_mr_a(pA0+6+3*KB,reg4);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg3);
            vec_mov_mr_a(pB0+8,reg7);
            vec_mov_mr_a(pA0+8,reg4);
            vec_mul_rr(reg7,reg4);
            vec_mov_mr_a(pA0+8+KB,reg5);
            vec_mov_mr_a(pA0+8+2*KB,reg6);

            pA0 += 8;
            pB0 += 8;
         }
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg1);
         vec_mul_rr(reg7,reg6);
         vec_add_rr(reg6,reg2);
         vec_mov_mr_a(pA0+3*KB,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg3);
         vec_mov_mr_a(pB0+2,reg7);
         vec_mov_mr_a(pA0+2,reg5);
         vec_mul_rr(reg7,reg5);
         vec_mov_mr_a(pA0+2+KB,reg6);
         vec_mov_mr_a(pA0+2+2*KB,reg4);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg6);
         vec_add_rr(reg6,reg1);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg2);
         vec_mov_mr_a(pA0+2+3*KB,reg5);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg3);
         vec_mov_mr_a(pB0+4,reg7);
         vec_mov_mr_a(pA0+4,reg6);
         vec_mul_rr(reg7,reg6);
         vec_mov_mr_a(pA0+4+KB,reg4);
         vec_mov_mr_a(pA0+4+2*KB,reg5);
         vec_add_rr(reg6,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg2);
         vec_mov_mr_a(pA0+4+3*KB,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg3);
         vec_mov_mr_a(pB0+6,reg7);
         vec_mov_mr_a(pA0+6,reg4);
         vec_mul_rr(reg7,reg4);
         vec_mov_mr_a(pA0+6+KB,reg5);
         vec_mov_mr_a(pA0+6+2*KB,reg6);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg1);
         vec_mul_rr(reg7,reg6);
         vec_add_rr(reg6,reg2);
         vec_mov_mr_a(pA0+6+3*KB,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg3);
         vec_sum(reg0);
         vec_sum(reg1);
         vec_sum(reg2);
         vec_sum(reg3);
         vec_mov_rm_1(reg0,pC0);
         vec_mov_rm_1(reg1,pC0+(1 SHIFT));
         vec_mov_rm_1(reg2,pC0+(2 SHIFT));
         vec_mov_rm_1(reg3,pC0+(3 SHIFT));
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
