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
void Mjoin(Mjoin(PATL,prow2blkT),NM)
   (const int M, const int N, const TYPE alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V)
/*
 * Given a packed Upper matrix A, copies & transposes M rows starting at A into
 * block-major row panel
 *    ldainc =  0 : General rectangular
 *    ldainc =  1 : Upper
 *    ldainc = -1 : Lower
 * NOTE: specialize to alpha cases after it works!
 */
{
   const int kb = Mmin(NB,N);
   const int ncb = N / kb, nr = N - ncb*kb;
   const int incV = kb*M - kb;
   int jb, i, j;
   TYPE *v;

   if (ldainc)
   {
      if (ldainc == -1) lda--;
      for (jb=ncb; jb; jb--)
      {
         for (j=kb; j; j--)
         {
            v = V++;
            for (i=0; i != M; i++, v += kb) *v = ATL_MulByALPHA(A[i]);
            A += lda;
            lda += ldainc;
         }
         V += incV;
      }
      for (j=nr; j; j--)
      {
         v = V++;
         for (i=0; i != M; i++, v += nr) *v = ATL_MulByALPHA(A[i]);
         A += lda;
         lda += ldainc;
      }
   }
   else Mjoin(Mjoin(PATL,row2blkT),NM)(N, M, A, lda, V, alpha);
}

#ifdef ALPHA1
/*
 * These two routines copy an mbxnb section of a matrix A to a block-major
 * nbxmb matrix V (A is transposed in the copy)
 */
static void ATL_prow2blk_KB_a1(const int mb, const int nb, const SCALAR alpha,
                               const TYPE *A, int lda, const int ldainc,TYPE *V)
{
   TYPE *v;
   int i, j;

   if (ldainc == -1) lda--;
   for (j=nb; j; j--)
   {
      v = V++;
      for (i=0; i != mb; i++, v += nb) *v = A[i];
      A += lda;
      lda += ldainc;
   }
}
static void ATL_prow2blk_KB_aX(const int mb, const int nb, const SCALAR alpha,
                               const TYPE *A, int lda, const int ldainc, TYPE *V)
{
   TYPE *v;
   int i, j;

   if (ldainc == -1) lda--;
   for (j=nb; j; j--)
   {
      v = V++;
      for (i=0; i != mb; i++, v += nb) *v = alpha * A[i];
      A += lda;
      lda += ldainc;
   }
}

void Mjoin(PATL,prow2blkTF)(const int M, const int N, const SCALAR alpha,
                            const TYPE *A, int lda, const int ldainc, TYPE *V)
{
   const int mb = Mmin(NB,M), nMb = ATL_DivByNB(M);
   const int m = ATL_MulByNB(nMb), n = ATL_MulByNB(ATL_DivByNB(N));
   const int nr = N - n, mr = M - m;
   const int incVm = ATL_MulByNB(N), incVV = ATL_MulByNB(mr);
   int i, j, ib, jb;
   const enum PACK_UPLO UA = (ldainc == 1) ? PackUpper :
      ( (ldainc == -1) ? PackLower : PackGen );
   TYPE *v, *vv = V+nMb*incVm;
   void (*row2blk)(const int M, const int N, const TYPE alpha, const TYPE *A,
                   int lda, const int ldainc, TYPE *V);

   if (ldainc)
   {
      if (alpha == ATL_rone) row2blk = ATL_prow2blk_KB_a1;
      else row2blk = ATL_prow2blk_KB_aX;

      for (j=0; j < n; j += NB)
      {
         for (v=V, i=0; i < m; i += NB, v += incVm)
            row2blk(NB, NB, alpha, A+MindexP(UA,i,j,lda), Mpld(UA,j,lda),
                    ldainc, v);
         if (mr)
         {
            row2blk(mr, NB, alpha, A+MindexP(UA,m,j,lda), Mpld(UA,j,lda),
                    ldainc, vv);
            vv += incVV;
         }
         V += NBNB;
      }
      if (nr)
      {
         for (v=V, i=0; i < m; i += NB, v += incVm)
            row2blk(NB, nr, alpha, A+MindexP(UA,i,n,lda), Mpld(UA,n,lda),
                    ldainc, v);
         if (mr)
            row2blk(mr, nr, alpha, A+MindexP(UA,m,n,lda), Mpld(UA,n,lda),
                    ldainc, vv);
      }
   }
   else if (SCALAR_IS_ONE(alpha))
      Mjoin(PATL,row2blkT2_a1)(M, N, A, lda, V, alpha);
   else
      Mjoin(PATL,row2blkT2_aX)(M, N, A, lda, V, alpha);
}
#endif

