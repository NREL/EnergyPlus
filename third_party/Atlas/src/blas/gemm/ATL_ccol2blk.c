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


#ifdef Conj_
   #define col2blk Mjoin(Mjoin(PATL,col2blkConj),NM)
   #define col2blk2 Mjoin(Mjoin(PATL,col2blkConj2),NM)
#else
   #define col2blk Mjoin(Mjoin(PATL,col2blk),NM)
   #define col2blk2 Mjoin(Mjoin(PATL,col2blk2),NM)
#endif

void col2blk(const int M, const int N, const TYPE *A, const int lda, TYPE *V,
             const SCALAR alpha)
{
   const int nMb = ATL_DivByNB(M), ib = M - ATL_MulByNB(nMb);
   const int incA = (lda - M)<<1, incv = ATL_MulByNB(N);
   const int incV = (incv<<1) - NB;
   int i, ii, j;
   TYPE *rp = V+ATL_MulByNB(N), *ip = V, *prp, *pip;
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

   pip = V + (M-ib)*(N<<1);
   prp = pip + ib*N;

   for (j=N; j; j--, V += NB, A += incA)
   {
      ip = V;
      rp = V + incv;
      for (ii=nMb; ii; ii--, rp += incV, ip += incV)
      {
         for (i=NB; i; i--, A += 2, rp++, ip++) scalcp(A, rp, ip);
      }
      for (i=ib; i; i--, A += 2, prp++, pip++) scalcp(A, prp, pip);
   }
}

void col2blk2(const int M, const int N, const TYPE *A, const int lda, TYPE *V,
              const TYPE *alpha)
{
   int j;
   const int nNb = ATL_DivByNB(N), jb = N - ATL_MulByNB(nNb);
   size_t incA = (lda*NB)<<1, incV = (M*NB)<<1;

   for (j=nNb; j; j--)
   {
      col2blk(M, NB, A, lda, V, alpha);
      A += incA;
      V += incV;
   }
   if (jb) col2blk(M, jb, A, lda, V, alpha);
}
