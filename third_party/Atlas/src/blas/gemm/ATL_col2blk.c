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

void Mjoin(Mjoin(PATL,col2blk),NM)
   (const int M, const int N, const TYPE *A, const int lda, TYPE *V,
    const SCALAR alpha0)
{
   const int Mb = ATL_DivByNB(M), m = ATL_MulByNB(Mb), mr = M-m;
   const int Nb = ATL_DivByNB(N), n = ATL_MulByNB(Nb), nr = N-n;
   const int nb2 = NB >> 1;
   const int incA = (lda<<1) - m;
   const int incV  = ( Mb ? (NB<<1) - Mb*NBNB : 0 );
   const int incVv = ( Mb ? (Mb-1)*NBNB + ATL_MulByNB(mr) : ATL_MulByNB(mr) );
   const int incV0 = ATL_MulByNB(nr);
   const int incV1 = ( Mb ? (NB<<1) - incV0*Mb : (mr<<1) );
   int ib, jb, i, j;
   const TYPE *pA0 = A, *pA1 = A + lda;
   TYPE *v0 = V, *vv=V + ATL_MulByNB(m);
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
   #endif


   #if ((NB/2)*2 != NB)  /* ATLAS should ensure NB divisible by 2 */
      ATL_assert((NB/2)*2 == NB);
   #endif
#if defined(DREAL) && defined(ATL_GAS_x8664) && 0
   for (jb=Nb; jb; jb--, v0 += M*NB, pA0 += NB*lda)
      Mjoin(ATL_dcol2blk_NB,NM)(M, NB, pA0, lda, v0, alpha);
   pA1 = pA0 + lda;
#else
   for (jb=Nb; jb; jb--, v0 += incVv)
   {
      vv = v0 + ATL_MulByNB(m);
      for (j=nb2; j; j--, v0 += incV, pA0 += incA, pA1 += incA)
      {
         for (ib=Mb; ib; ib--, pA0 += NB, pA1 += NB, v0 += NBNB)
         {
            #ifdef ATL_AltiVec
               ATL_pfavR(pA0+NB, cwrd, 0);
               ATL_pfavR(pA1+NB, cwrd, 1);
            #endif
            for (i=0; i != NB; i++)  /* easy loop to unroll */
            {
               v0[i] = ATL_MulByALPHA(pA0[i]);
               v0[NB+i] = ATL_MulByALPHA(pA1[i]);
            }
         }
         if (mr)
         {
            for (i=0; i != mr; i++)
            {
               vv[i] = ATL_MulByALPHA(pA0[i]);
               vv[mr+i] = ATL_MulByALPHA(pA1[i]);
            }
            vv += mr << 1;
         }
      }
   }
#endif
   if (nr)  /* partial column panel remainder */
   {
      v0 = V + Nb * NB * M;
      vv = v0 + nr*m;
      for (j=jb=(nr>>1); j; j--, pA0 += incA, pA1 += incA, v0 += incV1)
      {
         for (ib=Mb; ib; ib--, pA0 += NB, pA1 += NB, v0 += incV0)
         {
            for (i=0; i != NB; i++)  /* easy loop to unroll */
            {
               v0[i] = ATL_MulByALPHA(pA0[i]);
               v0[i+NB] = ATL_MulByALPHA(pA1[i]);
            }
         }
         if (mr)
         {
            for (i=0; i != mr; i++)
            {
               vv[i] = ATL_MulByALPHA(pA0[i]);
               vv[i+mr] = ATL_MulByALPHA(pA1[i]);
            }
            vv += mr << 1;
         }
      }
      if ((jb<<1) != nr)
      {
         for (ib=Mb; ib; ib--, pA0 += NB, v0 += incV0)
         {
            for (i=0; i != NB; i++) v0[i] = ATL_MulByALPHA(pA0[i]);
         }
         if (mr) for (i=0; i != mr; i++) vv[i] = ATL_MulByALPHA(pA0[i]);
      }
   }
}

void Mjoin(Mjoin(PATL,col2blk2),NM)
   (const int M, const int N, const TYPE *A, const int lda,
    TYPE *V, const SCALAR alpha)
{
   Mjoin(Mjoin(PATL,col2blk),NM)(M, N, A, lda, V, alpha);
}
