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

#define THREEDNOW
#include "SSE3Dnow.h"
#include "atlas_misc.h"

void ATL_USERMM
(const int M, const int N, const int K, const float alpha, const float *A, const int lda, const float *B, const int ldb, const float beta, float *C, const int ldc)

{
   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   float *pC1 = C+(ldc SHIFT);
   const float *stM = A + MB*KB;
   const float *stN = B + (N-N%2)*KB;
   const int incAm = 2*KB-KB+4;
   const int incBm = -KB+4;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 2*KB;
   const int incCn = ((ldc*2-MB) SHIFT);
   const int incBn_n = KB;
   const float *stN_n = B + N*KB;

   /*--- initial arhitecture specific statements ---*/
   vec_enter();

   /*--- main program statements ---*/
   vec_mov_mr_1(&beta,reg0);
   vec_mov_rm(reg0,betavec);
   while(pB0 != stN)
   {
      do /* M-loop */
      {
#ifdef BETA0
         vec_zero(reg0);
         vec_zero(reg1);
         vec_zero(reg2);
         vec_zero(reg3);
#elif defined(BETA1)
         vec_mov_mr_1(pC0,reg0);
         vec_mov_mr_1(pC0+(1 SHIFT),reg1);
         vec_mov_mr_1(pC1,reg2);
         vec_mov_mr_1(pC1+(1 SHIFT),reg3);
#else
         vec_mov_mr(betavec,reg7);
         vec_mov_mr_1(pC0,reg0);
         vec_mul_rr(reg7,reg0);
         vec_mov_mr_1(pC0+(1 SHIFT),reg1);
         vec_mul_rr(reg7,reg1);
         vec_mov_mr_1(pC1,reg2);
         vec_mul_rr(reg7,reg2);
         vec_mov_mr_1(pC1+(1 SHIFT),reg3);
         vec_mul_rr(reg7,reg3);
#endif
         vec_mov_mr(pA0,reg4);
         vec_mul_mr(pB0,reg4);
         vec_mov_mr(pA0+KB,reg5);
         vec_mul_mr(pB0,reg5);
         vec_mov_mr(pA0,reg6);
         vec_mov_mr(pA0+KB,reg7);
         align();
         for (k=0; k<KB-4; k+=16)
         {
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+2,reg4);
            vec_mul_mr(pB0+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+2+KB,reg5);
            vec_mul_mr(pB0+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+2,reg6);
            vec_mul_mr(pB0+2,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+2+KB,reg7);
            vec_mul_mr(pB0+2,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+4,reg4);
            vec_mul_mr(pB0+2+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+4+KB,reg5);
            vec_mul_mr(pB0+2+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+4,reg6);
            vec_mul_mr(pB0+4,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+4+KB,reg7);
            vec_mul_mr(pB0+4,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+6,reg4);
            vec_mul_mr(pB0+4+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+6+KB,reg5);
            vec_mul_mr(pB0+4+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+6,reg6);
            vec_mul_mr(pB0+6,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+6+KB,reg7);
            vec_mul_mr(pB0+6,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+8,reg4);
            vec_mul_mr(pB0+6+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+8+KB,reg5);
            vec_mul_mr(pB0+6+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+8,reg6);
            vec_mul_mr(pB0+8,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+8+KB,reg7);
            vec_mul_mr(pB0+8,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+10,reg4);
            vec_mul_mr(pB0+8+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+10+KB,reg5);
            vec_mul_mr(pB0+8+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+10,reg6);
            vec_mul_mr(pB0+10,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+10+KB,reg7);
            vec_mul_mr(pB0+10,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+12,reg4);
            vec_mul_mr(pB0+10+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+12+KB,reg5);
            vec_mul_mr(pB0+10+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+12,reg6);
            vec_mul_mr(pB0+12,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+12+KB,reg7);
            vec_mul_mr(pB0+12,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+14,reg4);
            vec_mul_mr(pB0+12+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+14+KB,reg5);
            vec_mul_mr(pB0+12+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+14,reg6);
            vec_mul_mr(pB0+14,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+14+KB,reg7);
            vec_mul_mr(pB0+14,reg5);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+16,reg4);
            vec_mul_mr(pB0+14+KB,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+16+KB,reg5);
            vec_mul_mr(pB0+14+KB,reg7);
            vec_add_rr(reg6,reg2);
            vec_mov_mr(pA0+16,reg6);
            vec_mul_mr(pB0+16,reg4);
            vec_add_rr(reg7,reg3);
            vec_mov_mr(pA0+16+KB,reg7);
            vec_mul_mr(pB0+16,reg5);

            pA0 += 16;
            pB0 += 16;
         }
         vec_add_rr(reg4,reg0);
         vec_mov_mr(pA0+2,reg4);
         vec_mul_mr(pB0+KB,reg6);
         vec_add_rr(reg5,reg1);
         vec_mov_mr(pA0+2+KB,reg5);
         vec_mul_mr(pB0+KB,reg7);
         vec_add_rr(reg6,reg2);
         vec_mov_mr(pA0+2,reg6);
         vec_mul_mr(pB0+2,reg4);
         vec_add_rr(reg7,reg3);
         vec_mov_mr(pA0+2+KB,reg7);
         vec_mul_mr(pB0+2,reg5);
         vec_add_rr(reg4,reg0);
         vec_add_rr(reg5,reg1);
         vec_mul_mr(pB0+2+KB,reg6);
         vec_add_rr(reg6,reg2);
         vec_mul_mr(pB0+2+KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_sum(reg0);
         vec_sum(reg1);
         vec_sum(reg2);
         vec_sum(reg3);
         vec_mov_rm_1(reg0,pC0);
         vec_mov_rm_1(reg1,pC0+(1 SHIFT));
         vec_mov_rm_1(reg2,pC1);
         vec_mov_rm_1(reg3,pC1+(1 SHIFT));
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

   pA0 = A;
   while(pB0 != stN_n)
   {
      do /* M-loop */
      {
#ifdef BETA0
         vec_zero(reg0);
         vec_zero(reg1);
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
         vec_mov_mr(pB0,reg6);
         vec_mov_mr(pA0,reg2);
         vec_mul_rr(reg6,reg2);
         vec_mov_mr(pA0+KB,reg3);
         align();
         for (k=0; k<KB-4; k+=16)
         {
            vec_add_rr(reg2,reg0);
            vec_mov_mr(pA0+2,reg4);
            vec_mul_rr(reg6,reg3);
            vec_mov_mr(pB0+2,reg7);
            vec_add_rr(reg3,reg1);
            vec_mov_mr(pA0+2+KB,reg5);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+4,reg2);
            vec_mul_rr(reg7,reg5);
            vec_mov_mr(pB0+4,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+4+KB,reg3);
            vec_mul_rr(reg6,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr(pA0+6,reg4);
            vec_mul_rr(reg6,reg3);
            vec_mov_mr(pB0+6,reg7);
            vec_add_rr(reg3,reg1);
            vec_mov_mr(pA0+6+KB,reg5);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+8,reg2);
            vec_mul_rr(reg7,reg5);
            vec_mov_mr(pB0+8,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+8+KB,reg3);
            vec_mul_rr(reg6,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr(pA0+10,reg4);
            vec_mul_rr(reg6,reg3);
            vec_mov_mr(pB0+10,reg7);
            vec_add_rr(reg3,reg1);
            vec_mov_mr(pA0+10+KB,reg5);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+12,reg2);
            vec_mul_rr(reg7,reg5);
            vec_mov_mr(pB0+12,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+12+KB,reg3);
            vec_mul_rr(reg6,reg2);
            vec_add_rr(reg2,reg0);
            vec_mov_mr(pA0+14,reg4);
            vec_mul_rr(reg6,reg3);
            vec_mov_mr(pB0+14,reg7);
            vec_add_rr(reg3,reg1);
            vec_mov_mr(pA0+14+KB,reg5);
            vec_mul_rr(reg7,reg4);
            vec_add_rr(reg4,reg0);
            vec_mov_mr(pA0+16,reg2);
            vec_mul_rr(reg7,reg5);
            vec_mov_mr(pB0+16,reg6);
            vec_add_rr(reg5,reg1);
            vec_mov_mr(pA0+16+KB,reg3);
            vec_mul_rr(reg6,reg2);

            pA0 += 16;
            pB0 += 16;
         }
         vec_add_rr(reg2,reg0);
         vec_mov_mr(pA0+2,reg2);
         vec_mul_rr(reg6,reg3);
         vec_mov_mr(pB0+2,reg6);
         vec_add_rr(reg3,reg1);
         vec_mov_mr(pA0+2+KB,reg3);
         vec_mul_rr(reg6,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_rr(reg6,reg3);
         vec_add_rr(reg3,reg1);
         vec_sum(reg0);
         vec_sum(reg1);
         vec_mov_rm_1(reg0,pC0);
         vec_mov_rm_1(reg1,pC0+(1 SHIFT));
         pA0 += incAm;
         pB0 += incBm;
         pC0 += incCm;
         pC1 += incCm;
      }
      while(pA0 != stM);

      pA0 += incAn;
      pB0 += incBn_n;
      pC0 += incCn;
      pC1 += incCn;
   }

   vec_exit();
}
