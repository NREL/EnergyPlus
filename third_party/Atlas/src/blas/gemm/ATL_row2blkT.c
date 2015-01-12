/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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
#include "atlas_prefetch.h"

#define ATL_row2blkT_NB Mjoin(Mjoin(PATL,row2blkT_NB),NM)
#define ATL_row2blkT_KB Mjoin(Mjoin(PATL,row2blkT_KB),NM)
#define ATL_row2blkT Mjoin(Mjoin(PATL,row2blkT),NM)
#define ATL_row2blkT2 Mjoin(Mjoin(PATL,row2blkT2),NM)

#if defined (DREAL) && defined(ATL_GAS_x8664) && 0
void ATL_row2blkT_NB(const int M, const int N, const TYPE *A, const int lda,
                     TYPE *V, const TYPE alpha);
#else
static void ATL_row2blkT_NB
   (const int M, const int N, const TYPE *A, const int lda, TYPE *V,
    const TYPE alpha0)
/*
 * copy where M & N are NB, compiler should be able to completely unroll
 */
{
   const int lda2 = lda<<1;
   int i, j;
   TYPE *v=V;
   const TYPE *pA0 = A, *pA1 = A + lda;
   const register TYPE alpha=alpha0;
   #ifdef ATL_AltiVec
      static int cwrd=0;
      if (cwrd) goto L1;
      i = 1; /* one block unless NB is too big */
      j = ATL_MulBySize(NB)>>4;
      while (j > 32) { j >>= 1; i <<= 1; }
      if (j == 32) j = 0;
      cwrd = ATL_GetCtrl(j<<4, i, j);
L1:
      ATL_pfavR(pA0, cwrd, 2);
      ATL_pfavR(pA1, cwrd, 3);
   #endif

#if (NB/2)*2 != NB  /* ATLAS should ensure NB is divisable by two */
   assert((NB/2)*2 == NB);
#endif
   for (j=NB; j; j -= 2)
   {
      #ifdef ATL_AltiVec
         ATL_pfavR(pA0+lda2, cwrd, 0);
         ATL_pfavR(pA1+lda2, cwrd, 1);
      #endif
      for (i=0; i != NB; i++, v += NB)
      {
         *v = ATL_MulByALPHA(pA0[i]);
         v[1] = ATL_MulByALPHA(pA1[i]);
      }
      V += 2;
      v = V;
      pA0 += lda2;
      pA1 += lda2;
   }
}
#endif

static void ATL_row2blkT_KB
   (const int M, const int N, const TYPE *A, const int lda, TYPE *V,
    const TYPE alpha0)
{
   const int n = N >> 1, lda2 = lda<<1;
   int i, j;
   TYPE *v=V;
   const TYPE *pA0 = A, *pA1 = A + lda;
   const register TYPE alpha=alpha0;

   for (j=n; j; j--)
   {
      for (i=0; i != M; i++, v += N)
      {
         *v = ATL_MulByALPHA(pA0[i]);
         v[1] = ATL_MulByALPHA(pA1[i]);
      }
      V += 2;
      v = V;
      pA0 += lda2;
      pA1 += lda2;
   }
   if ((n<<1) != N)
      for (i=0; i != M; i++, v += N) *v = ATL_MulByALPHA(pA0[i]);
}

void ATL_row2blkT(const int N, const int nb, const TYPE *A, const int lda,
                  TYPE *V, const SCALAR alpha)
/*
 * A is a nbxN matrix, v is a N*nb length vector.
 * v receives trans(A) in block major order.
 */
{
   const int Nb = ATL_DivByNB(N), incA = ATL_MulByNB(lda);
   const int incV = ATL_MulByNB(nb);
   int k;

   if (nb == NB)
      for (k=0; k != Nb; k++, A += incA, V += incV)
         ATL_row2blkT_NB(Nb, NB, A, lda, V, alpha);

   else
      for (k=0; k != Nb; k++, A += incA, V += incV)
         ATL_row2blkT_KB(nb, NB, A, lda, V, alpha);
   if (k = N - ATL_MulByNB(Nb))
      ATL_row2blkT_KB(nb, k, A, lda, V, alpha);
}
void ATL_row2blkT2(const int M, const int N, const TYPE *A, const int lda,
                   TYPE *V, const SCALAR alpha)
{
   const int Mb = ATL_DivByNB(M), Nb = ATL_DivByNB(N);
   const int mr = M - ATL_MulByNB(Mb), nr = N - ATL_MulByNB(Nb);
   const int incV = ATL_MulByNB(N), incA = ATL_MulByNB(lda) - M + mr;
   const int incVV = ATL_MulByNB(mr);
   int i, j;
   TYPE *v=V, *vv = V+Mb*incV;

   for (j=Nb; j; j--)
   {
      for (i=Mb; i; i--, A += NB, v += incV)
         ATL_row2blkT_NB(NB, NB, A, lda, v, alpha);
      if (mr)
      {
         ATL_row2blkT_KB(mr, NB, A, lda, vv, alpha);
         vv += incVV;
      }
      A += incA;
      V += NBNB;
      v = V;
   }
   if (nr)
   {
      for (i=Mb; i; i--, A += NB, v += incV)
         ATL_row2blkT_KB(NB, nr, A, lda, v, alpha);
      if (mr) ATL_row2blkT_KB(mr, nr, A, lda, vv, alpha);
   }
}
