/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2007 R. Clint Whaley
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
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include <stdlib.h>

int Mjoin(PATL,mmJITcp)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                        const int M0, const int N, const int K,
                        const SCALAR alpha, const TYPE *A, const int lda,
                        const TYPE *B, const int ldb, const SCALAR beta,
                        TYPE *C, const int ldc)
/*
 * Copy matmul algorithm, copies A and B on-the-fly
 * If M < 0, allocates only (MB+NB)*KB workspace
 */
{
   void *v=NULL;
   const TYPE *a=A;
   TYPE *pA, *pB, *pB0;
   MAT2BLK2 A2blk, B2blk;
   NBMM0 NBmm0, NBmm1, pNBmm0;
   const int M = (M0 >= 0) ? M0 : -M0;
   int nkblks, nmblks, nnblks, mr, nr, kr, KR, bigK, h, i, j, ZEROC;
   size_t incAk, incBk, incAm, incBn, incAW, incAWp, incBW, incBWp, incW;

/*
 * If both M and N <= NB, and one of them is not full, call BPP, which
 * can sometimes avoid doing cleanup forall cases
 */
   if (M <= MB && N <= NB && (M != MB || N != NB))
      return(Mjoin(PATL,mmBPP)(TA, TB, M, N, K, alpha, A, lda, B, ldb,
                               beta, C, ldc));
/*
 * If these workspace increments are 0, we do JIT NBxNB copies instead of
 * copying entire array/panel.  Don't copy mat if you can't reuse it.
 */
   if (M0 > 0)
   {
      incAW = (N > NB) ? KB*MB : 0;
      incBW = (M > NB) ? KB*NB : 0;
   }
   else /* allocate in minimal space */
      incAW = incBW = 0;
   nmblks = M/MB;
   nnblks = N/NB;
   nkblks = K/KB;
   mr = M - nmblks*MB;
   nr = N - nnblks*NB;
   kr = K - nkblks*KB;
/*
 * K-loop is special, in that we don't call user cleanup, must explicitly zero,
 * and K-cleanup is typically slower even for generated kernels.  Therefore,
 * allow extra leaway for doing extra flops.  Note error is unaffected by
 * any of these extra flops: K-loop has elts zeroed, and multiplying zeros
 * and adding in zeros doesn't add to error
 */
   KR = (kr && kr+4 >= KB) ? KB : kr;
   bigK = nkblks*KB+KR;
   if (incAW)
   {
      i = MB*bigK;
      incAWp = KB*mr;
   }
   else
   {
      i = MB*KB;
      incAWp = 0;
   }
   if (incBW)
   {
      incBWp = KB*nr;
      incW = bigK*NB;
      i += N*bigK;
   }
   else
   {
      incBWp = incW = 0;
      i += NB*KB;
   }
   i *= sizeof(TYPE);
   if (i <= ATL_MaxMalloc || !(incAW | incBW))
      v = malloc(ATL_Cachelen+i);
   if (!v) return(-1);
   pA = ATL_AlignPtr(v);
   pB0 = pA + (incAW ? bigK*MB : KB*MB);
   if (TA == AtlasNoTrans)
   {
      A2blk = Mjoin(PATL,gemoveT);
      incAk = lda*KB;
      incAm = MB;
   }
   else
   {
      A2blk = Mjoin(PATL,gemove);
      incAk = KB;
      incAm = MB*lda;
   }
   if (TB == AtlasNoTrans)
   {
      B2blk = Mjoin(PATL,gemove);
      incBk = KB;
      incBn = NB*ldb;
   }
   else
   {
      B2blk = Mjoin(PATL,gemoveT);
      incBk = ldb*KB;
      incBn = NB;
   }
/*
 * See what kernel we're calling
 */
   if ( SCALAR_IS_ONE(beta) )
   {
      NBmm0 = NBmm_b1;
      pNBmm0 = Mjoin(PATL,pNBmm_b1);
   }
   else if ( SCALAR_IS_ZERO(beta) )
   {
      NBmm0 = NBmm_b0;
      pNBmm0 = Mjoin(PATL,pNBmm_b0);
   }
   else
   {
      NBmm0 = NBmm_bX;
      pNBmm0 = Mjoin(PATL,pNBmm_bX);
   }
   KR = (KR == KB) ? KB : 0;
   ZEROC = !KR && SCALAR_IS_ZERO(beta);

   for (i=0; i < nmblks; i++)
   {
      a = A+i*incAm;
      pB = pB0;       /* foreach row-panel of A, start at B's copy space */
      for (j=nnblks; j; j--)
      {
         Mjoin(PATL,mmK)(MB, MB, NB, NB, nkblks, kr, KR, ATL_rone, alpha, beta,
                         a, lda, incAk, pA, incAW, B, ldb, incBk, pB, incBW,
                         C, ldc, A2blk, B2blk, NBmm0, NBmm_b1);
         B += incBn;             /* copy next col panel of B */
         pB += incW;             /* to next col panel of pB  */
         a = (incAW ? NULL : a); /* reuse row-panel of A if copied */
         C += ldc*NB;
      }
      if (nr)
      {
         if (ZEROC)
            Mjoin(PATL,gezero)(MB, nr, C, ldc);
         Mjoin(PATL,mmK)(MB, MB, nr, nr, nkblks, kr, KR, ATL_rone, alpha, beta,
                         a, lda, incAk, pA, incAW, B, ldb, incBk, pB, incBWp,
                         C, ldc, A2blk, B2blk, pNBmm0, Mjoin(PATL,pNBmm_b1));
      }
      C += MB - nnblks*ldc*NB;
      if (incBW)
      {
         B = NULL;              /* finished copying B */
         incBn = 0;
      }
      else
         B -= nnblks*incBn;
   }
   if (mr)
   {
      a = A + nmblks*incAm;
      pB = pB0;
      if ( SCALAR_IS_ONE(beta) ) NBmm0 = Mjoin(PATL,pMBmm_b1);
      else if ( SCALAR_IS_ZERO(beta) ) NBmm0 = Mjoin(PATL,pMBmm_b0);
      else NBmm0 = Mjoin(PATL,pMBmm_bX);
      for (j=nnblks; j; j--)
      {
         Mjoin(PATL,mmK)(mr, mr, NB, NB, nkblks, kr, KR, ATL_rone, alpha, beta,
                         a, lda, incAk, pA, incAWp, B, ldb, incBk, pB, incBW,
                         C, ldc, A2blk, B2blk, NBmm0, Mjoin(PATL,pMBmm_b1));
         B += incBn;              /* copy next col panel of B */
         pB += incW;              /* to next col panel of pB  */
         a = (incAW ? NULL : a);  /* reuse row-panel of A if copied */
         C += ldc*NB;
      }
      if (nr)
      {
         if ( SCALAR_IS_ZERO(beta) )
            Mjoin(PATL,gezero)(mr, nr, C, ldc);
         Mjoin(PATL,mmK)(mr, mr, nr, nr, nkblks, kr, (incAW | incBW) ? KR:0,
                         ATL_rone, alpha, beta, a, lda, incAk, pA, incAWp,
                         B, ldb, incBk, pB, incBWp, C, ldc, A2blk, B2blk,
                         Mjoin(PATL,pKBmm), Mjoin(PATL,pKBmm));
      }
   }
   free(v);
   return(0);
}
