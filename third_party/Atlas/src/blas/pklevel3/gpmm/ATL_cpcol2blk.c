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
#ifdef Conj_
   #if defined(ALPHA1)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = *(A_); \
         *(ip_) = -((A_)[1]); \
      }
   #elif defined(ALPHAXI0)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = ralpha * *(A_); \
         *(ip_) = calpha * (A_)[1]; \
      }
   #elif defined(ALPHAX)
      #define scalcp(A_, rp_, ip_) \
      { \
         ra = *(A_); ia = (A_)[1]; \
         *(rp_) = ralpha * ra + ialpha * ia; \
         *(ip_) = ialpha * ra - ralpha * ia; \
      }
   #endif
#else
   #if defined(ALPHA1)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = *(A_); \
         *(ip_) = (A_)[1]; \
      }
   #elif defined(ALPHAXI0)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = ralpha * *(A_); \
         *(ip_) = ralpha * (A_)[1]; \
      }
   #elif defined(ALPHAX)
      #define scalcp(A_, rp_, ip_) \
      { \
         ra = *(A_); ia = (A_)[1]; \
         *(rp_) = ralpha * ra - ialpha * ia; \
         *(ip_) = ialpha * ra + ralpha * ia; \
      }
   #endif
#endif
#ifdef Conj_
   #define dcol2blkF_a1   Mjoin(PATL,col2blkConj2_a1)
   #define dcol2blkF_aX   Mjoin(PATL,col2blkConj2_aX)
   #define dcol2blkF_aXi0 Mjoin(PATL,col2blkConj2_aXi0)
   #define pcol2blk Mjoin(Mjoin(PATL,pcol2blkConj),NM)
   #define pcol2blkF Mjoin(PATL,pcol2blkConjF)
#else
   #define dcol2blkF_a1   Mjoin(PATL,col2blk2_a1)
   #define dcol2blkF_aX   Mjoin(PATL,col2blk2_aX)
   #define dcol2blkF_aXi0 Mjoin(PATL,col2blk2_aXi0)
   #define pcol2blk Mjoin(Mjoin(PATL,pcol2blk),NM)
   #define pcol2blkF Mjoin(PATL,pcol2blkF)
#endif

#ifdef ALPHA1
void Mjoin(pcol2blkF,_blk)
   (const int blk, const int M, const int N, const SCALAR alpha, const TYPE *A,
    int lda, const int ldainc, TYPE *V)
/*
 * Copies entire MxN matrix to block major format
 */
{
   int j, jb;
   const int incV = blk*(M+M);
   const enum PACK_UPLO UA = (ldainc == 1) ? PackUpper :
      ( (lda == -1) ? PackLower : PackGen );
   void (*col2blk)(const int blk, const int M, const int N, const SCALAR alpha,
                   const TYPE *A, int lda, const int ldainc, TYPE *V);

#ifdef Conj_
   if (alpha[1] == ATL_rzero)
   {
      if (*alpha == ATL_rone) col2blk = Mjoin(Mjoin(PATL,pcol2blkConj_a1),_blk);
      else col2blk = Mjoin(Mjoin(PATL,pcol2blkConj_aXi0),_blk);
   }
   else col2blk = Mjoin(Mjoin(PATL,pcol2blkConj_aX),_blk);
#else
   if (alpha[1] == ATL_rzero)
   {
      if (*alpha == ATL_rone) col2blk = Mjoin(Mjoin(PATL,pcol2blk_a1),_blk);
      else col2blk = Mjoin(Mjoin(PATL,pcol2blk_aXi0),_blk);
   }
   else col2blk = Mjoin(Mjoin(PATL,pcol2blk_aX),_blk);
#endif

   for (j=0; j < N; j += blk)
   {
      jb = N-j;
      jb = Mmin(jb, blk);
      col2blk(blk, M, jb, alpha, A+MindexP(UA,0,j,lda), Mpld(UA,j,lda),
              ldainc, V);
      V += incV;
   }
}

void pcol2blkF
   (const int M, const int N, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V)
{
   if (ldainc) Mjoin(pcol2blkF,_blk)(NB, M, N, alpha, A, lda, ldainc, V);
   else if (alpha[1] == ATL_rzero)
   {
      if (*alpha == ATL_rone) dcol2blkF_a1(M, N, A, lda, V, alpha);
      else dcol2blkF_aXi0(M, N, A, lda, V, alpha);
   }
   else dcol2blkF_aX(M, N, A, lda, V, alpha);
}
#endif

void Mjoin(pcol2blk,_blk)(const int blk, const int M, const int N,
                          const SCALAR alpha, const TYPE *A, int lda,
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
   const int kb = Mmin(M,blk);
   const int nrb = M / kb, mr = M - nrb*kb;
   const int nv = kb*N, nvv = mr*N;
   const int NN = nv+nv - kb;
   const int ldainc2 = ldainc+ldainc, M2 = M+M;
   int i, ib, j, J;
   TYPE *v = V + nrb*(NN+kb);
   #ifdef ALPHAXI0
      #ifdef Conj_
         const register TYPE ralpha = *alpha, calpha = -ralpha;
      #else
         const register TYPE ralpha = *alpha;
      #endif
   #elif defined(ALPHAX)
      const register TYPE ralpha=(*alpha), ialpha = alpha[1];
      register TYPE ra, ia;
   #endif

   if (ldainc == -1) lda--;
   lda += lda;
   ATL_assert(N <= blk);
   for (j=0; j != N; j++)
   {
      for (ib=nrb; ib; ib--)
      {
         for (i=0; i < kb; i++, A += 2, V++) scalcp(A, V+nv, V);
         V += NN;
      }
      if (mr)
      {
         for (i=0; i < mr; i++, A += 2, v++) scalcp(A, v+nvv, v);
      }
      V += kb - nrb*(NN+kb);
      A += lda - M2;
      lda += ldainc2;
   }
}

void pcol2blk(const int M, const int N, const SCALAR alpha, const TYPE *A,
              int lda, const int ldainc, TYPE *V)
{
   if (ldainc) Mjoin(pcol2blk,_blk)(NB, M, N, alpha, A, lda, ldainc, V);
#ifdef Conj_
   else Mjoin(Mjoin(PATL,col2blkConj),NM)(M, N, A, lda, V, alpha);
#else
   else Mjoin(Mjoin(PATL,col2blk),NM)(M, N, A, lda, V, alpha);
#endif
}
