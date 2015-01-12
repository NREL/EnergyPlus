/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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

#ifdef Conj_
   #if defined(ALPHA1)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = *(A_); \
         *(ip_) = -((A_)[1]); \
      }
   #elif defined(ALPHAN1)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = -(*(A_)); \
         *(ip_) = (A_)[1]; \
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
   #elif defined(ALPHAN1)
      #define scalcp(A_, rp_, ip_) \
      { \
         *(rp_) = -(*(A_)); \
         *(ip_) = -((A_)[1]); \
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

static void row2blkT_NB(const int M, const int N, const TYPE *A, const int lda,
                        TYPE *vr, TYPE *vi, const SCALAR alpha)
{
   const int incA = lda<<2, incv = 2 - NBNB;
   const TYPE *pA0 = A, *pA1 = A + (lda<<1);
   int i, j;
   #ifdef ALPHAXI0
      #ifdef Conj_
         const register TYPE ralpha = *alpha, calpha = -ralpha;
      #else
         const register TYPE ralpha = *alpha;
      #endif
   #elif defined(ALPHAX)
      const register TYPE ralpha = *alpha, ialpha = alpha[1];
      register TYPE ra, ia;
   #endif

   #if ((NB/2)*2 != NB)  /* ATLAS should ensure NB divisible by 2 */
      ATL_assert((NB/2)*2 == NB);
   #endif
   for (j=(NB>>1); j; j --, pA0 += incA, pA1 += incA, vr += incv, vi += incv)
   {
      for (i=0; i != NB2; i += 2, vr += NB, vi += NB)
      {
         scalcp(pA0+i, vr, vi);
         scalcp(pA1+i, vr+1, vi+1);
      }
   }
}

static void row2blkT_KB(const int M, const int N, const TYPE *A, const int lda,
                        TYPE *vr, TYPE *vi, const SCALAR alpha)
{
   const int M2 = M<<1, lda2 = lda<<1, incv = 1 - M*N;
   int i, j;
   #ifdef ALPHAXI0
      #ifdef Conj_
         const register TYPE ralpha = *alpha, calpha = -ralpha;
      #else
         const register TYPE ralpha = *alpha;
      #endif
   #elif defined(ALPHAX)
      const register TYPE ralpha = *alpha, ialpha = alpha[1];
      register TYPE ra, ia;
   #endif

   for (j=N; j; j--, A += lda2, vr += incv, vi += incv)
   {
      for (i=0; i != M2; i += 2, vr += N, vi += N) scalcp(A+i, vr, vi);
   }
}

#ifdef Conj_
   #define row2blkT Mjoin(Mjoin(PATL,row2blkC),NM)
   #define row2blkT2 Mjoin(Mjoin(PATL,row2blkC2),NM)
#else
   #define row2blkT Mjoin(Mjoin(PATL,row2blkT),NM)
   #define row2blkT2 Mjoin(Mjoin(PATL,row2blkT2),NM)
#endif

void row2blkT(const int N, const int nb, const TYPE *A, const int lda,
              TYPE *v, const SCALAR alpha)
{
   const int nNb = ATL_DivByNB(N), incA = ATL_MulByNB(lda)<<1;
   const int incv = ATL_MulByNB(nb), incV = incv<<1;
   int k;

   if (nb == NB)
      for (k=nNb; k; k--, A += incA, v += NBNB2)
         row2blkT_NB(NB, NB, A, lda, v+NBNB, v, alpha);
   else
      for (k=nNb; k; k--, A += incA, v += incV)
         row2blkT_KB(nb, NB, A, lda, v+incv, v, alpha);
   if ( k = N-ATL_MulByNB(nNb) )
      row2blkT_KB(nb, k, A, lda, v+k*nb, v, alpha);
}

void row2blkT2(const int M, const int N, const TYPE *A, const int lda,
               TYPE *V, const SCALAR alpha)
{
   const int nNb = ATL_DivByNB(N), nMb = ATL_DivByNB(M);
   const int mr = M - ATL_MulByNB(nMb), nr = N - ATL_MulByNB(nNb);
   const int incA = (ATL_MulByNB(lda) - M + mr)<<1;
   const int incV = ATL_MulByNB(N)<<1, incvv = ATL_MulByNB(mr), incVV=incvv<<1;
   TYPE *v = V, *vv = V + nMb * incV;
   int i, j;

   for (j=nNb; j; j--)
   {
      for (i=nMb; i; i--, A += NB2, v += incV)
         row2blkT_NB(NB, NB, A, lda, v+NBNB, v, alpha);
      if (mr)
      {
         row2blkT_KB(mr, NB, A, lda, vv+incvv, vv, alpha);
         vv += incVV;
      }
      A += incA;
      V += NBNB2;
      v = V;
   }
   if (nr)
   {
      j = ATL_MulByNB(nr);
      for (i=nMb; i; i--, A += NB2, v += incV)
         row2blkT_KB(NB, nr, A, lda, v+j, v, alpha);
      if (mr) row2blkT_KB(mr, nr, A, lda, vv+mr*nr, vv, alpha);
   }
}
