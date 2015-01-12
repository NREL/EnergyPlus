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
#include <stddef.h>

#ifdef NoTransA_
   #define ETA AtlasNoTrans
   #define TA_ N
   #define ta_ n
   #define nA_ K
   #define incA (NB SHIFT)
   #define A2BLK Mjoin(PATL,row2blkT)
   #define A_M_ M
   #define A_N_ K
#elif defined(ConjTransA_)
   #define ETA AtlasConjTrans
   #define TA_ C
   #define ta_ c
   #define nA_ M
   #define incA ATL_MulByNB(lda<<1)
   #define A2BLK Mjoin(PATL,col2blkConj)
   #define A_M_ K
   #define A_N_ M
#else
   #define ETA AtlasTrans
   #define TA_ T
   #define ta_ t
   #define nA_ M
   #define incA ATL_MulByNB(lda SHIFT)
   #define A2BLK Mjoin(PATL,col2blk)
   #define A_M_ K
   #define A_N_ M
#endif

#ifdef NoTransB_
   #define ETB AtlasNoTrans
   #define TB_ N
   #define tb_ n
   #define incB ATL_MulByNB(ldb SHIFT)
   #define nB_ N
   #define B2BLK Mjoin(PATL,col2blk)
   #define B_M_ K
   #define B_N_ N
#elif defined(ConjTransB_)
   #define ETB AtlasConjTrans
   #define TB_ C
   #define tb_ c
   #define nB_ K
   #define B2BLK Mjoin(PATL,row2blkC)
   #define B_M_ N
   #define B_N_ K
   #define incB NB2
#else
   #define ETB AtlasTrans
   #define TB_ T
   #define tb_ t
   #define nB_ K
   #define B2BLK Mjoin(PATL,row2blkT)
   #define B_M_ N
   #define B_N_ K
   #define incB (NB SHIFT)
#endif
#define tatb Mjoin(ta_,tb_)
#define TATB Mjoin(TA_,TB_)

void Mjoin(Mjoin(PATL,aliased_gemm),TATB)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc)
/*
 * Does right thing when output matrix is aliased with one or more of the input
 * matrices
 */
{
   const ptrdiff_t  bA=(ptrdiff_t)A, bB=(ptrdiff_t)B, bC=(ptrdiff_t)C;
   const ptrdiff_t eA = bA + ATL_MulBySize(nA_*lda);
   const ptrdiff_t eB = bB + ATL_MulBySize(nB_*ldb);
   const ptrdiff_t eC = bC + ATL_MulBySize(N*ldc);
   const int AC_aliased = ( (bA <= bC && eA >= bC) || (bC <= bA && eC >= bA) );
   const int BC_aliased = ( (bB <= bC && eB >= bC) || (bC <= bB && eC >= bB) );
   const int nMb=ATL_DivByNB(M), nNb=ATL_DivByNB(N), nKb=ATL_DivByNB(K);
   const int ib = M - ATL_MulByNB(nMb);
   const int jb = N - ATL_MulByNB(nNb);
   const int kb = K - ATL_MulByNB(nKb);
   void *vA=NULL, *vB=NULL;
   TYPE *pA, *pB;
   NBMM0 NBmm0;
   MAT2BLK mat2blk;
   #ifdef TCPLX
      MATSCAL gescal;
   #endif

#ifdef USERGEMM
   int ldpb=ldb, ldpa=lda;
   pA = (TYPE *) A; pB = (TYPE *) B;

   if (BC_aliased)
   {
      vB = malloc(ATL_Cachelen + ATL_MulBySize(N*K));
      ATL_assert(vB);
      pB = ATL_AlignPtr(vB);
      #ifdef NoTransB_
         Mjoin(PATL,gecopy)(K, N, B, ldb, pB, K);
         ldpb = K;
      #else
         Mjoin(PATL,gecopy)(N, K, B, ldb, pB, N);
         ldpb = N;
      #endif
    }
    if (AC_aliased)
    {
       vA = malloc(ATL_Cachelen + ATL_MulBySize(M*K));
       ATL_assert(vA);
       pA = ATL_AlignPtr(vA);
       #ifdef NoTransA_
          Mjoin(PATL,gecopy)(M, K, A, lda, pA, M);
          ldpa = M;
       #else
          Mjoin(PATL,gecopy)(K, M, A, lda, pA, K);
          ldpa = K;
       #endif
    }
    if (Mjoin(PATU,usergemm)(ETA, ETB, M, N, K, alpha, pA, ldpa, pB, ldpb,
                             beta, C, ldc))
       ATL_assert(Mjoin(PATL,NCmmJIK)(ETA, ETB, M, N, K, alpha, pA, ldpa,
                                      pB, ldpb, beta, C, ldc)==0);
#else

   #ifdef TCPLX
      if (beta[1] == ATL_rzero)
      {
         gescal = NULL;
         if (*beta == ATL_rone) NBmm0 = Mjoin(PATL,CNBmm_b1);
         else if (*beta == ATL_rzero) NBmm0 = Mjoin(PATL,CNBmm_b0);
         else NBmm0 = Mjoin(PATL,CNBmm_bX);
      }
      else
      {
         NBmm0 = Mjoin(PATL,CNBmm_b1);
         gescal = Mjoin(PATL,gescal_bX);
      }
   #else
      if ( SCALAR_IS_ONE(beta) ) NBmm0 = NBmm_b1;
      else if ( SCALAR_IS_ZERO(beta) ) NBmm0 = NBmm_b0;
      else NBmm0 = NBmm_bX;
   #endif

   if (N >= M)  /* B on outside, A in inner loop */
   {
/*
 *    If B & C are aliased, we will need to copy entire matrix up front
 *    unless B is NoTranspose, and B & C start at the same place
 */
      if (BC_aliased)
      {
         #ifdef NoTransB_
            if (B != C || ldb != ldc)
            {
         #endif
         vB = malloc(ATL_Cachelen + ATL_MulBySize(N*K));
         ATL_assert(vB);
         pB = ATL_AlignPtr(vB);
         Mjoin(B2BLK,2_a1)(B_M_, B_N_, B, ldb, pB, alpha);
         mat2blk = NULL;
         B = NULL;
         #ifdef NoTransB_
            }
         #endif
      }
/*
 *    Otherwise, we can copy B column panel at a time
 */
      if (vB == NULL)
      {
/*
 *       See if we can avoid the copy of B
 */
         #if defined(NoTransB_) && defined(TREAL)
            if (!BC_aliased && ldb == NB && K == NB)
            {
               pB = (TYPE *) B;
               mat2blk = NULL;
               B = NULL;
            }
            else
            {
         #endif

         vB = malloc(ATL_Cachelen + ATL_MulBySize(NB*K));
         ATL_assert(vB);
         pB = ATL_AlignPtr(vB);
         mat2blk = Mjoin(B2BLK,_a1);

         #if defined(NoTransB_) && defined(TREAL)
            }
         #endif
      }
/*
 *    See if it is possible to avoid copying A
 */
      #if !defined(NoTransA_) && defined(TREAL)
         if ( !AC_aliased && lda == NB && K == NB && SCALAR_IS_ONE(alpha) )
            pA = (TYPE *) A;
         else
         {
      #endif

         vA = malloc(ATL_Cachelen + ATL_MulBySize(M*K));
         ATL_assert(vA);
         pA = ATL_AlignPtr(vA);
         if ( SCALAR_IS_ONE(alpha) )
            Mjoin(A2BLK,2_a1)(A_M_, A_N_, A, lda, pA, alpha);
         else Mjoin(A2BLK,2_aX)(A_M_, A_N_, A, lda, pA, alpha);

      #if !defined(NoTransA_) && defined(TREAL)
         }
      #endif

      #ifdef TREAL
         Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, pA,
                              B, ldb, pB, incB, mat2blk, beta,
                              C, ldc, C, NULL, NBmm0);
      #else
         Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, pA,
                            B, ldb, pB, incB, mat2blk, beta,
                            C, ldc, gescal, NBmm0);
      #endif
   }
   else  /* put A as outer matrix, B as inner */
   {

/*
 *    If A & C are aliased, we will need to copy entire matrix up front
 *    unless A is NoTranspose, and A & C start at the same place, with the same
 *    stride between columns
 */
      if (AC_aliased)
      {
         #ifdef NoTransA_
            if (A != C || lda != ldc)
            {
         #endif
         vA = malloc(ATL_Cachelen + ATL_MulBySize(M*K));
         ATL_assert(vA);
         pA = ATL_AlignPtr(vA);
         Mjoin(A2BLK,2_a1)(A_M_, A_N_, A, lda, pA, alpha);
         mat2blk = NULL;
         A = NULL;
         #ifdef NoTransA_
            }
         #endif
      }
/*
 *    Otherwise, we can copy A row panel at a time
 */
      if (vA == NULL)
      {
/*
 *       See if we can avoid the copy of A
 */
         #if !defined(NoTransA_) && defined(TREAL)
            if (!AC_aliased && lda == NB && K == NB)
            {
               pA = (TYPE *) A;
               mat2blk = NULL;
               A = NULL;
            }
            else
            {
         #endif

         vA = malloc(ATL_Cachelen + ATL_MulBySize(NB*K));
         ATL_assert(vA);
         pA = ATL_AlignPtr(vA);
         mat2blk = Mjoin(A2BLK,_a1);

         #if !defined(NoTransA_) && defined(TREAL)
            }
         #endif
      }
/*
 *    See if it is possible to avoid copying B
 */
      #if defined(NoTransB_) && defined(TREAL)
         if (!BC_aliased && ldb == NB && K == NB && alpha == 1.0)
            pB = (TYPE *) B;
         else
         {
      #endif

         vB = malloc(ATL_Cachelen + ATL_MulBySize(N*K));
         ATL_assert(vB);
         pB = ATL_AlignPtr(vB);
         if ( SCALAR_IS_ONE(alpha) )
              Mjoin(B2BLK,2_a1)(B_M_, B_N_, B, ldb, pB, alpha);
         else Mjoin(B2BLK,2_aX)(B_M_, B_N_, B, ldb, pB, alpha);

      #if defined(NoTransB_) && defined(TREAL)
         }
      #endif
      #ifdef TREAL
         Mjoin(PATL,mmIJK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, A, lda, pA,
                              incA, mat2blk, pB, beta, C, ldc, C, NULL, NBmm0);
      #else
         Mjoin(PATL,mmIJK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, A, lda, pA,
                            incA, mat2blk, pB, beta, C, ldc, gescal, NBmm0);
      #endif
   }
#endif
   if (vA) free(vA);
   if (vB) free(vB);
}

#undef tatb
#undef TATB

#undef ETA
#undef TA_
#undef ta_
#undef incA
#undef nA_
#undef A2BLK
#undef A_M_
#undef A_N_
#undef incA

#undef ETB
#undef TB_
#undef tb_
#undef incB
#undef nB_
#undef B2BLK
#undef B_M_
#undef B_N_
