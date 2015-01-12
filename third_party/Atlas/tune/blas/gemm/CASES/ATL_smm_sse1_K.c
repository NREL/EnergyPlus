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
/*
 * This file combines all of Peter Soendergaard's K-cleanup routines, and
 * then uses Camm Maguire's general routine for other cases of K-cleanup.
 */

/*****************************************************************************/
/*                             ATL_mm_sse1_4K.c                              */
/*****************************************************************************/
#if (KB == 4)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 4  */
   /*  used_directload_a = 1  */
   /*  outside_len = 4  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 4*KB-KB+4;
   const int incBm = -KB+4;
   const int incCm = (4 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0,reg6);
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+3*KB,reg6);
         vec_add_rr(reg6,reg3);
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

/*****************************************************************************/
/*                             ATL_mm_sse1_8K.c                              */
/*****************************************************************************/
#elif (KB == 8)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 8  */
   /*  used_directload_a = 1  */
   /*  outside_len = 8  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
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
         vec_mov_mr_a(pB0,reg6);
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+4,reg6);
         vec_mov_mr_a(pA0+4,reg4);
         vec_mov_mr_a(pA0+4+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+4+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+4+3*KB,reg6);
         vec_add_rr(reg6,reg3);
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

/*****************************************************************************/
/*                             ATL_mm_sse1_12K.c                             */
/*****************************************************************************/
#elif (KB == 12)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 12  */
   /*  used_directload_a = 1  */
   /*  outside_len = 12  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 4*KB-KB+12;
   const int incBm = -KB+12;
   const int incCm = (4 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0,reg6);
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+4,reg7);
         vec_mov_mr_a(pA0+4,reg5);
         vec_mov_mr_a(pA0+4+KB,reg4);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+4+2*KB,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+4+3*KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_mov_mr_a(pB0+8,reg6);
         vec_mov_mr_a(pA0+8,reg4);
         vec_mov_mr_a(pA0+8+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+8+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+8+3*KB,reg6);
         vec_add_rr(reg6,reg3);
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

/*****************************************************************************/
/*                             ATL_mm_sse1_16K.c                             */
/*****************************************************************************/
#elif (KB == 16)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 16  */
   /*  used_directload_a = 1  */
   /*  outside_len = 16  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 4*KB-KB+16;
   const int incBm = -KB+16;
   const int incCm = (4 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0,reg6);
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+4,reg7);
         vec_mov_mr_a(pA0+4,reg5);
         vec_mov_mr_a(pA0+4+KB,reg4);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+4+2*KB,reg5);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg2);
         vec_mul_mr_a(pA0+4+3*KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_mov_mr_a(pB0+8,reg6);
         vec_mov_mr_a(pA0+8,reg4);
         vec_mov_mr_a(pA0+8+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+8+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+8+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+12,reg6);
         vec_mov_mr_a(pA0+12,reg4);
         vec_mov_mr_a(pA0+12+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+12+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+12+3*KB,reg6);
         vec_add_rr(reg6,reg3);
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

/*****************************************************************************/
/*                             ATL_mm_sse1_20K.c                             */
/*****************************************************************************/
#elif (KB == 20)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 20  */
   /*  used_directload_a = 1  */
   /*  outside_len = 20  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 4*KB-KB+20;
   const int incBm = -KB+20;
   const int incCm = (4 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0,reg6);
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+4,reg7);
         vec_mov_mr_a(pA0+4,reg5);
         vec_mov_mr_a(pA0+4+KB,reg4);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+4+2*KB,reg5);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg2);
         vec_mul_mr_a(pA0+4+3*KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_mov_mr_a(pB0+8,reg6);
         vec_mov_mr_a(pA0+8,reg4);
         vec_mov_mr_a(pA0+8+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+8+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+8+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+12,reg7);
         vec_mov_mr_a(pA0+12,reg5);
         vec_mov_mr_a(pA0+12+KB,reg4);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+12+2*KB,reg4);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+12+3*KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_mov_mr_a(pB0+16,reg6);
         vec_mov_mr_a(pA0+16,reg4);
         vec_mov_mr_a(pA0+16+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+16+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+16+3*KB,reg6);
         vec_add_rr(reg6,reg3);
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

/*****************************************************************************/
/*                             ATL_mm_sse1_24K.c                             */
/*****************************************************************************/
#elif (KB == 24)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 4  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 2  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 24  */
   /*  used_directload_a = 1  */
   /*  outside_len = 24  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 4*KB-KB+24;
   const int incBm = -KB+24;
   const int incCm = (4 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0,reg6);
         vec_mov_mr_a(pA0,reg4);
         vec_mov_mr_a(pA0+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+4,reg7);
         vec_mov_mr_a(pA0+4,reg5);
         vec_mov_mr_a(pA0+4+KB,reg4);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+4+2*KB,reg5);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg2);
         vec_mul_mr_a(pA0+4+3*KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_mov_mr_a(pB0+8,reg6);
         vec_mov_mr_a(pA0+8,reg4);
         vec_mov_mr_a(pA0+8+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+8+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+8+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+12,reg7);
         vec_mov_mr_a(pA0+12,reg5);
         vec_mov_mr_a(pA0+12+KB,reg4);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg0);
         vec_mul_rr(reg7,reg4);
         vec_add_rr(reg4,reg1);
         vec_mov_mr_a(pA0+12+2*KB,reg5);
         vec_mul_rr(reg7,reg5);
         vec_add_rr(reg5,reg2);
         vec_mul_mr_a(pA0+12+3*KB,reg7);
         vec_add_rr(reg7,reg3);
         vec_mov_mr_a(pB0+16,reg6);
         vec_mov_mr_a(pA0+16,reg4);
         vec_mov_mr_a(pA0+16+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+16+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+16+3*KB,reg6);
         vec_add_rr(reg6,reg3);
         vec_mov_mr_a(pB0+20,reg6);
         vec_mov_mr_a(pA0+20,reg4);
         vec_mov_mr_a(pA0+20+KB,reg5);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg0);
         vec_mul_rr(reg6,reg5);
         vec_add_rr(reg5,reg1);
         vec_mov_mr_a(pA0+20+2*KB,reg4);
         vec_mul_rr(reg6,reg4);
         vec_add_rr(reg4,reg2);
         vec_mul_mr_a(pA0+20+3*KB,reg6);
         vec_add_rr(reg6,reg3);
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

/*****************************************************************************/
/*                             ATL_mm_sse1_28K.c                             */
/*****************************************************************************/
#elif (KB == 28)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 28  */
   /*  used_directload_a = 1  */
   /*  outside_len = 28  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+28;
   const int incBm = -KB+28;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_32K.c                             */
/*****************************************************************************/
#elif (KB == 32)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 32  */
   /*  used_directload_a = 1  */
   /*  outside_len = 32  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+32;
   const int incBm = -KB+32;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0+28,reg5);
         vec_mov_mr_a(pA0+28,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+28+KB,reg5);
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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_36K.c                             */
/*****************************************************************************/
#elif (KB == 36)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 36  */
   /*  used_directload_a = 1  */
   /*  outside_len = 36  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+36;
   const int incBm = -KB+36;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0+32,reg5);
         vec_mov_mr_a(pA0+32,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+32+KB,reg5);
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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_40K.c                             */
/*****************************************************************************/
#elif (KB == 40)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 40  */
   /*  used_directload_a = 1  */
   /*  outside_len = 40  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+40;
   const int incBm = -KB+40;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_44K.c                             */
/*****************************************************************************/
#elif (KB == 44)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 44  */
   /*  used_directload_a = 1  */
   /*  outside_len = 44  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+44;
   const int incBm = -KB+44;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0+40,reg5);
         vec_mov_mr_a(pA0+40,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+40+KB,reg5);
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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_48K.c                             */
/*****************************************************************************/
#elif (KB == 48)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 48  */
   /*  used_directload_a = 1  */
   /*  outside_len = 48  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+48;
   const int incBm = -KB+48;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0+44,reg5);
         vec_mov_mr_a(pA0+44,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+44+KB,reg5);
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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_52K.c                             */
/*****************************************************************************/
#elif (KB == 52)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 52  */
   /*  used_directload_a = 1  */
   /*  outside_len = 52  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+52;
   const int incBm = -KB+52;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_56K.c                             */
/*****************************************************************************/
#elif (KB == 56)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 56  */
   /*  used_directload_a = 1  */
   /*  outside_len = 56  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_mm_sse1_60K.c                             */
/*****************************************************************************/
#elif (KB == 60)
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
   /*  problem = 'gemm'  */
   /*  nregs = 8  */
   /*  split = 0  */
   /*  n_cleanup = {}  */
   /*  mu = 2  */
   /*  k_loop = None  */
   /*  rev = '$Revision: 1.3 $'  */
   /*  applyfilter = 0  */
   /*  align_jumps = 0  */
   /*  prec = 'single'  */
   /*  m_cleanup = {}  */
   /*  k_cleanup = {'mustfit': 56, 'method': 'acc1', 'compile': 1}  */
   /*  outputdir = 'Linux_PIII/'  */
   /*  arch = 'sse'  */
   /*  pipelength = 3  */
   /*  used_lastuse = 'b'  */
   /*  ku = 4  */
   /*  atlasname = 'Linux_PIII'  */
   /*  method = 'acc'  */
   /*  used_outside_len = 60  */
   /*  used_directload_a = 1  */
   /*  outside_len = 60  */
   /*  veclen = 4  */
   /*  sched = ['fuse', 'spread']  */
   /*  used_directload_b = 0  */
   /*  lastuse = 1  */

   /*--- achitecture specific declarations ---*/

   /*--- program specific declarations ---*/
   int i, j, k;
   vector betavec;
   vector zerovec = {0.0,0.0,0.0,0.0};
   const float *pA0 = A;
   const float *pB0 = B;
   float *pC0 = C;
   const float *stM = A + MB*KB;
   const float *stN = B + NB*KB;
   const int incAm = 2*KB-KB+60;
   const int incBm = -KB+60;
   const int incCm = (2 SHIFT);
   const int incAn = -MB*KB;
   const int incBn = 1*KB;
   const int incCn = ((ldc*1-MB) SHIFT);

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
         vec_mov_mr_a(pB0+52,reg6);
         vec_mov_mr_a(pA0+52,reg3);
         vec_mul_rr(reg6,reg3);
         vec_add_rr(reg3,reg0);
         vec_mul_mr_a(pA0+52+KB,reg6);
         vec_add_rr(reg6,reg1);
         vec_mov_mr_a(pB0+56,reg5);
         vec_mov_mr_a(pA0+56,reg2);
         vec_mul_rr(reg5,reg2);
         vec_add_rr(reg2,reg0);
         vec_mul_mr_a(pA0+56+KB,reg5);
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
   while(pB0 != stN);

   }

/*****************************************************************************/
/*                             ATL_gemm_SSE.c                                */
/*****************************************************************************/
#elif ( (KB/4)*4 == KB )

#include "camm_util.h"
#include "camm_strat.h"

#define VERS 1

#if defined(DREAL) || defined(DCPLX)
#define NR KB8
#else
#define NR KB4
#endif

#define pf(a_,b_)  /*  f(nta,a_,b_) */



#if defined(DREAL) || defined(DCPLX)
#define Z1(a_,b_) pc(a_,b_) ps(1,b_,b_) pasr(b_,a_)
#else
#define Z1(a_,b_) phl(a_,b_) pa(b_,a_) pc(a_,b_) ps(1,b_,b_) pasr(b_,a_)
#endif
#if defined(DREAL) || defined (SREAL)
#ifdef DREAL
#define Z1x4    f(t0,0,cx) pc(4,0) pul(5,4) pc(6,1) puh(5,0) pul(7,6)  \
                puh(7,1) pa(0,4) pa(1,6) pu(4,0,cx) pu(6,SS(CS,CS),cx)
#define Z1x2    f(t0,0,cx) pc(4,0) pul(5,4) puh(5,0)  \
                pa(0,4) pu(4,0,cx)
#else
#define Z1x4    f(t0,0,cx) pc(4,0) pul(5,4) pc(6,1) puh(5,0) pul(7,6)  \
                pa(0,4) puh(7,1) pc(4,2) pa(1,6) ps(68,6,4) ps(238,6,2) pa(4,2) pu(2,0,cx)
#define Z1x2    f(t0,0,cx) pc(4,0) pul(5,4) puh(5,0)  \
                pa(0,4) phl(4,2) pa(2,4) pud(4,0,cx)
#endif
#else
#define Z1x4    Z1(4,0) pus(4,0,cx) Z1(5,1) pus(5,CS,cx) \
                Z1(6,0) pus(6,SS(CS,CS),cx) Z1(7,1) pus(7,SS(SS(CS,CS),CS),cx)
#define Z1x2    Z1(4,0) pus(4,0,cx) Z1(5,1) pus(5,CS,cx)
#endif
#define Z1x1    Z1(4,0) pus(4,0,cx)

#ifdef BETA0
#define W1x4    px(4) px(5) px(6) px(7)
#define W1x2    px(4) px(5)
#define W1x1    px(4)
#endif
#ifdef BETA1
#define W1x4    pls(0,cx,4) pls(CS,cx,5) pls(SS(CS,CS),cx,6) \
                pls(SS(SS(CS,CS),CS),cx,7)
#define W1x2    pls(0,cx,4) pls(CS,cx,5)
#define W1x1    pls(0,cx,4)
#endif
#ifdef BETAX
#define W1x4    pls(0,cx,4) pls(CS,cx,5) pls(SS(CS,CS),cx,6) \
                pls(SS(SS(CS,CS),CS),cx,7) \
                pmsr(3,4) pmsr(3,5) pmsr(3,6) pmsr(3,7)
#define W1x2    pls(0,cx,4) pls(CS,cx,5) pmsr(3,4) pmsr(3,5)
#define W1x1    pls(0,cx,4) pmsr(3,4)
#endif

#if defined(DREAL) || defined(SREAL)
#ifdef DREAL
#define CS 8
#else
#define CS 4
#endif
#define LDCM 1
#else
#ifdef DCPLX
#define CS 16
#else
#define CS 8
#endif
#define LDCM 2
#endif



#if defined(SREAL) || defined(SCPLX)
#define MTYPE float
#else
#define MTYPE double
#endif



void
ATL_USERMM (int m, int n, int k, MTYPE alpha, const MTYPE *a,
	    int lda,const MTYPE *b, int ldb, MTYPE beta, MTYPE *c,
	    int ldc) {

  const MTYPE *bbp=&beta;

  ASM (

#if KB % 4
#error KB must be divisible by four -- m n cleanup needs alignment
#endif

#ifdef BETAX
       pls(0,di,3)
#endif

       "pushl %%ebx\n\t"
       "movl  %%esi,%%ebx\n\t"

#if MB == 0 || NB == 0
       a(4,sp)

#if MB == 0
       "movl %4,%%esi\n\t"
#endif
#if NB == 0
       "movl %5,%%edi\n\t"
#endif
       a(-4,sp)
#endif

       "pushl %%ebp\n\t"
#if NB == 0
       "movl %%edi,%%ebp\n\t"
#else
       mm(MM(NR,NB),bp)
       ra(bx,bp)
#endif

#if MB == 0
       a(8,sp)
       "movl %6,%%edi\n\t"
       a(-8,sp)
#else
       mm(MM(NR,E4(MB)),di)
       ra(ax,di)
#endif

       lab(loopb)

#if NB == 0
       cmp(bx,bp)
       je(end)
#endif

       "pushl %%edi\n\t"
       "pushl %%eax\n\t"

       lab(loopa)

#if MB == 0
       cmp(ax,di)
       je(2)
#endif

#if MB == 0 ||  MB >= 4
#undef N
#define N Mjoin(1x4_,VERS)
#include "camm_pipe2.h"

       W1x4
       KB_block
       Z1x4

       a(SS(SS(NR,NR),SS(NR,NR)),ax)
       a(SS(SS(CS,CS),SS(CS,CS)),cx)

#endif

#if MB == 0
       jmp(loopa)
#else
       cmp(ax,di)
       jne(loopa)
#endif

#if MB == 0
       lab(2)
       a(SS(NR,NR),di)
       cmp(di,si)
       jl(1)
#endif

#if MB == 0 || ( MB / 2 ) % 2
#undef N
#define N Mjoin(1x2_,VERS)
#include "camm_pipe2.h"

       W1x2
       KB_block
       Z1x2

       a(SS(NR,NR),ax)
       a(SS(CS,CS),cx)

#endif

#if MB == 0
       lab(1)
       cmp(ax,si)
       je(stop)
#endif

#if MB == 0 || MB % 2

#undef N
#define N Mjoin(1x1_,VERS)
#include "camm_pipe2.h"

       W1x1
       KB_block
       Z1x1

/*         a(NR,ax) */
       a(CS,cx)

#endif

#if MB == 0
       lab(stop)
#endif

       "popl %%eax\n\t"
       "popl %%edi\n\t"
       ra(dx,cx)
       a(NR,bx)

#if NB == 0
       jmp(loopb)
       lab(end)
#else
       cmp(bx,bp)
       jne(loopb)
#endif

       "popl %%ebp\n\t"
       "popl %%ebx\n\t"


       ::"a" (a),"S" (b),"c" (c),"d" ((ldc-m)*LDCM*sizeof(*c)),
       "m" (a+m*KB),"m" (b+n*KB),"m" (a+((m>>2)<<2)*KB)
#ifdef BETAX
       ,"D" (bbp):"memory");
#else
       :"di","memory");
#endif

}
#else
   #error Unsupported KB!!
#endif
