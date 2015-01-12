/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2003 R. Clint Whaley
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
#include "atlas_pkblas.h"
void Mjoin(Mjoin(PATL,pcol2blk),NM)
   (const int M, const int N, const TYPE alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V)
/*
 * Given a packed matrix A, copies N columns starting at A into
 * block-major column panel
 *    ldainc =  0 : General
 *    ldainc =  1 : Upper
 *    ldainc = -1 : Lower
 * NOTE: specialize to alpha cases after it works!
 */
{
   const int kb = Mmin(M,NB);
   const int nrb = M / kb, mr = M - nrb*kb;
   int i, ib, j, J;
   const int NN = N*kb;
   TYPE *v = V + nrb*NN;

   if (ldainc)
   {
      if (ldainc == -1) lda--;
      ATL_assert(N <= NB);
      for (j=0; j != N; j++)
      {
         for (ib=nrb; ib; ib--)
         {
            for (i=0; i < kb; i++) V[i] = ATL_MulByALPHA(A[i]);
            V += NN;
            A += kb;
         }
         if (mr)
         {
            for (i=0; i < mr; i++) v[i] = ATL_MulByALPHA(A[i]);
            v += mr;
         }
         V += kb - nrb*NN;
         A += lda - nrb*kb;
         lda += ldainc;
      }
   }
   else Mjoin(Mjoin(PATL,col2blk),NM)(M, N, A, lda, V, alpha);
}

#ifdef ALPHA1
void Mjoin(PATL,pcol2blkF)
   (const int M, const int N, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V)
/*
 * Copies entire MxN matrix to block major format
 */
{
   int j, jb;
   const int incV = ATL_MulByNB(M);
   const enum PACK_UPLO UA = (ldainc == 1) ? PackUpper :
      ( (lda == -1) ? PackLower : PackGen );
   void (*col2blk)(const int M, const int N, const SCALAR alpha, const TYPE *A,
                   int lda, const int ldainc, TYPE *V);

   if (ldainc)
   {
      if (alpha == ATL_rone) col2blk = Mjoin(PATL,pcol2blk_a1);
      else col2blk = Mjoin(PATL,pcol2blk_aX);

      for (j=0; j < N; j += NB)
      {
         jb = N-j;
         jb = Mmin(jb, NB);
         col2blk(M, jb, alpha, A+MindexP(UA,0,j,lda), Mpld(UA,j,lda), ldainc,V);
         V += incV;
      }
   }
   else if (alpha == ATL_rone)
      Mjoin(PATL,col2blk2_a1)(M, N, A, lda, V, alpha);
   else
      Mjoin(PATL,col2blk2_aX)(M, N, A, lda, V, alpha);
}
#endif
