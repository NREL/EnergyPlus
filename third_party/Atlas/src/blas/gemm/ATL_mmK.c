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

void Mjoin(PATL,mmK)(int M,  /* true # of rows in row-panel, M <= MB */
                     int m,  /* # of rows to operate on, m >= M */
                     int N,  /* true # of cols in col-panel, N < = NB */
                     int n,  /* # of cols to operate on, n >= N */
                     int nblk, /* # of blocks in K dimension */
                     int kr,   /* kr = K - nKb*KB; */
                     int KR,   /* 0 : do not do full KB-call to avoid cleanup */
                     const SCALAR alphaA,  /* alpha to apply during A copy */
                     const SCALAR alphaB,  /* alpha to apply during B copy */
                     const SCALAR beta,    /* beta to apply to C */
                     const TYPE *A, /* array to copy from, NULL if already cp */
                     const int lda,  /* leading dimension of A */
                     const int incA, /* inc to next blk in A */
                     TYPE *pA,       /* wrkspace to copy A to */
                     const int incAW, /* 0 : keep using same KBxMB space */
                     const TYPE *B, /* array to copy from, NULL if already cp */
                     const int ldb,  /* leading dimension of B */
                     const int incB, /* inc to next blk in B */
                     TYPE *pB,       /* wrkspace to copy B to */
                     const int incBW, /* 0 : keep using same KBxNB space */
                     TYPE *C,         /* output matrix */
                     const int ldc,
                     MAT2BLK2 A2blk, /* rout to copy A */
                     MAT2BLK2 B2blk, /* rout to copy B */
                     NBMM0 NBmm0,    /* rout to do first mul (applies beta) */
                     NBMM0 NBmm1)    /* rout to do later muls (beta=1) */
/*
 * Performs a K-inner-loop matmul, while copying A & B if necessary.
 * If M > m, we are doing extra flops so we don't call cleanup (same for N)
 */
{
   int k;
   if (nblk)
   {
      if (B) { B2blk(KB, N, alphaB, B, ldb, pB, KB); B += incB; }
      if (A) { A2blk(KB, M, alphaA, A, lda, pA, KB); A += incA; }
      NBmm0(m, n, KB, ATL_rone, pA, KB, pB, KB, beta, C, ldc);
      pA += incAW; pB += incBW;
      for (k = nblk-1; k; k--)
      {
         if (B) { B2blk(KB, N, alphaB, B, ldb, pB, KB); B += incB; }
         if (A) { A2blk(KB, M, alphaA, A, lda, pA, KB); A += incA; }
         NBmm1(m, n, KB, ATL_rone, pA, KB, pB, KB, ATL_rone, C, ldc);
         pA += incAW; pB += incBW;
      }
   }
   if (kr)  /* need to cleanup K loop */
   {
      if (KR)
      {
         if (B)
         {
            B2blk(kr, N, alphaB, B, ldb, pB, KB);
            Mjoin(PATL,gezero)(KB-kr, n, pB+kr, KB);
         }
         if (A)
         {
            A2blk(kr, M, alphaA, A, lda, pA, KB);
            Mjoin(PATL,gezero)(KB-kr, m, pA+kr, KB);
         }
         if (nblk)
            NBmm1(m, n, KB, ATL_rone, pA, KB, pB, KB, ATL_rone, C, ldc);
         else
            NBmm0(m, n, KB, ATL_rone, pA, KB, pB, KB, beta, C, ldc);
      }
      else
      {
         if (B) B2blk(kr, N, alphaB, B, ldb, pB, kr);
         if (A) A2blk(kr, M, alphaA, A, lda, pA, kr);
         Mjoin(PATL,pKBmm)(M, N, kr, ATL_rone, pA, kr, pB, kr,
                           nblk ? ATL_rone : beta, C, ldc);
      }
   }
}

