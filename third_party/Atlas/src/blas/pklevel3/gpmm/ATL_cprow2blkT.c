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
   #define prow2blkT Mjoin(Mjoin(PATL,prow2blkH),NM)
   #define prow2blkTF Mjoin(PATL,prow2blkHF)
   #define prow2blk_KB Mjoin(Mjoin(PATL,prow2blkH_KB),NM)
#else
   #define prow2blkT Mjoin(Mjoin(PATL,prow2blkT),NM)
   #define prow2blkTF Mjoin(PATL,prow2blkTF)
   #define prow2blk_KB Mjoin(Mjoin(PATL,prow2blkT_KB),NM)
#endif
void prow2blk_KB(const int mb, const int nb, const SCALAR alpha, const TYPE *A,
                 int lda, const int ldainc, TYPE *V)
/*
 * This routine used by full copy to copy one mbxnb block of a matrix A to
 * block-major nbxmb storage (A is transposed during the copy)
 */
{
   TYPE *v;
   const int mn = mb * nb, ldainc2 = ldainc+ldainc;
   int i, j;
   #ifdef ALPHAXI0
      #ifdef Conj_
         const register TYPE ralpha = *alpha, calpha = -ralpha;
      #else
         const register TYPE ralpha = *alpha;
      #endif
   #elif defined(ALPHAX)
      register const TYPE ralpha=(*alpha), ialpha = alpha[1];
      register TYPE ra, ia;
   #endif

   if (ldainc == -1) lda--;
   lda -= mb;
   lda += lda;
   for (j=nb; j; j--)
   {
      v = V++;
      for (i=0; i != mb; i++, v += nb, A += 2) scalcp(A, v+mn, v);
      A += lda;
      lda += ldainc2;
   }
}

void Mjoin(prow2blkT,_blk)(const int blk, const int M, const int N,
                           const SCALAR alpha, const TYPE *A, int lda,
                           const int ldainc, TYPE *V)
/*
 * Given a packed Upper matrix A, copies & transposes M rows starting at A into
 * block-major row panel
 *    ldainc =  0 : General rectangular
 *    ldainc =  1 : Upper
 *    ldainc = -1 : Lower
 */
{
   const int kb = Mmin(blk,N);
   const int ncb = N / kb, nr = N - ncb*kb;
   const int incV = kb*M - kb;
   const int VN = kb*M, vn = nr*M;
   int jb, i, j;
   TYPE *v;
   #ifdef ALPHAXI0
      #ifdef Conj_
         const register TYPE ralpha = *alpha, calpha = -ralpha;
      #else
         const register TYPE ralpha = *alpha;
      #endif
   #elif defined(ALPHAX)
      register const TYPE ralpha=(*alpha), ialpha = alpha[1];
      register TYPE ra, ia;
   #endif

   if (ldainc == -1) lda--;
   lda -= M;
   lda += lda;
   for (jb=ncb; jb; jb--)
   {
      for (j=kb; j; j--)
      {
         v = V++;
         for (i=0; i != M; i++, v += kb, A += 2) scalcp(A, v+VN, v);
         A += lda;
         lda += ldainc;
      }
      V += incV;
   }
   for (j=nr; j; j--)
   {
      v = V++;
      for (i=0; i != M; i++, v += nr, A += 2) scalcp(A, v+vn, v);
      A += lda;
      lda += ldainc;
   }
}

void prow2blkT(const int M, const int N, const SCALAR alpha, const TYPE *A,
               int lda, const int ldainc, TYPE *V)
{
   if (ldainc) Mjoin(prow2blkT,_blk)(NB, M, N, alpha, A, lda, ldainc, V);
#ifdef Conj_
   else Mjoin(Mjoin(PATL,row2blkC),NM)(N, M, A, lda, V, alpha);
#else
   else Mjoin(Mjoin(PATL,row2blkT),NM)(N, M, A, lda, V, alpha);
#endif
}
#ifdef ALPHA1
#ifdef Conj_
   #define dr2bT2_a1   Mjoin(PATL,row2blkC2_a1)
   #define dr2bT2_aXi0 Mjoin(PATL,row2blkC2_aXi0)
   #define dr2bT2_aX   Mjoin(PATL,row2blkC2_aX)
   #define ATL_row2blk_KB_a1   Mjoin(Mjoin(PATL,prow2blkH_KB),_a1)
   #define ATL_row2blk_KB_aXi0 Mjoin(Mjoin(PATL,prow2blkH_KB),_aXi0)
   #define ATL_row2blk_KB_aX   Mjoin(Mjoin(PATL,prow2blkH_KB),_aX)
#else
   #define dr2bT2_a1   Mjoin(PATL,row2blkT2_a1)
   #define dr2bT2_aXi0 Mjoin(PATL,row2blkT2_aXi0)
   #define dr2bT2_aX   Mjoin(PATL,row2blkT2_aX)
   #define ATL_row2blk_KB_a1   Mjoin(Mjoin(PATL,prow2blkT_KB),_a1)
   #define ATL_row2blk_KB_aXi0 Mjoin(Mjoin(PATL,prow2blkT_KB),_aXi0)
   #define ATL_row2blk_KB_aX   Mjoin(Mjoin(PATL,prow2blkT_KB),_aX)
#endif
void Mjoin(prow2blkTF,_blk)
   (const int blk, const int M, const int N, const SCALAR alpha,
    const TYPE *A, int lda, const int ldainc, TYPE *V)
{
   const int mb = Mmin(blk,M), nMb = M/blk;
   const int m = blk*nMb, n = blk*(N/blk);
   const int nr = N - n, mr = M - m;
   const int incVm = blk*(N+N), incVV = blk*(mr+mr), nbnb=(blk*blk)<<1;
   int i, j, ib, jb;
   const enum PACK_UPLO UA = (ldainc == 1) ? PackUpper :
      ( (ldainc == -1) ? PackLower : PackGen );
   TYPE *v, *vv = V+nMb*incVm;
   void (*row2blk)(const int M, const int N, const SCALAR alpha, const TYPE *A,
                   int lda, const int ldainc, TYPE *V);

   if (alpha[1] == ATL_rzero)
   {
      if (*alpha == ATL_rone) row2blk = ATL_row2blk_KB_a1;
      else row2blk = ATL_row2blk_KB_aXi0;
   }
   else row2blk = ATL_row2blk_KB_aX;

   for (j=0; j < n; j += blk)
   {
      for (v=V, i=0; i < m; i += blk, v += incVm)
         row2blk(blk, blk, alpha, A+MindexP(UA,i,j,lda), Mpld(UA,j,lda),
                 ldainc, v);
      if (mr)
      {
         row2blk(mr, blk, alpha, A+MindexP(UA,m,j,lda), Mpld(UA,j,lda),
                 ldainc, vv);
         vv += incVV;
      }
      V += nbnb;
   }
   if (nr)
   {
      for (v=V, i=0; i < m; i += blk, v += incVm)
         row2blk(blk, nr, alpha, A+MindexP(UA,i,n,lda), Mpld(UA,n,lda),
                 ldainc, v);
      if (mr)
         row2blk(mr, nr, alpha, A+MindexP(UA,m,n,lda), Mpld(UA,n,lda),
                 ldainc, vv);
   }
}

void prow2blkTF(const int M, const int N, const SCALAR alpha, const TYPE *A,
                int lda, const int ldainc, TYPE *V)
{
   if (ldainc) Mjoin(prow2blkTF,_blk)(NB, M, N, alpha, A, lda, ldainc, V);
   else if (alpha[1] == ATL_rzero)
   {
      if (*alpha == ATL_rone) dr2bT2_a1(M, N, A, lda, V, alpha);
      else dr2bT2_aXi0(M, N, A, lda, V, alpha);
   }
   else dr2bT2_aX(M, N, A, lda, V, alpha);
}
#endif
