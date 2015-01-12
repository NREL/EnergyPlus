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
#ifdef ATL_TGEMM
   #include "atlas_tcacheedge.h"
#else
   #include "atlas_cacheedge.h"
#endif
#ifdef DCPLX
   #include "atlas_zdNKB.h"
   #ifdef ATL_DZNKB
      #define NKB_maxK (ATL_DZNKB * KB)
   #else
      #define NKB_maxK 0
   #endif
#elif defined(SCPLX)
   #include "atlas_csNKB.h"
   #ifdef ATL_CSNKB
      #define NKB_maxK (ATL_CSNKB * KB)
   #else
      #define NKB_maxK 0
   #endif
#endif

#ifndef C2R_K
   #define C2R_K (~(1<<31))
#endif

#ifdef NoTransA_
   #define ETA AtlasNoTrans
   #define TA_ N
   #define ta_ n
   #define incA (Kp*lda SHIFT)
#elif defined(TransA_)
   #define ETA AtlasTrans
   #define TA_ T
   #define ta_ t
   #define incA (Kp SHIFT)
#elif defined(ConjTransA_)
   #define ETA AtlasConjTrans
   #define TA_ C
   #define ta_ c
   #define incA (Kp SHIFT)
#endif

#ifdef NoTransB_
   #define ETB AtlasNoTrans
   #define TB_ N
   #define tb_ n
   #define incB (Kp SHIFT)
#elif defined (TransB_)
   #define ETB AtlasTrans
   #define TB_ T
   #define tb_ t
   #define incB (Kp*ldb SHIFT)
#elif defined (ConjTransB_)
   #define ETB AtlasConjTrans
   #define TB_ C
   #define tb_ c
   #define incB (Kp*ldb SHIFT)
#endif
#define tatb Mjoin(ta_,tb_)
#define TATB Mjoin(TA_,TB_)

#if defined(BIG_MM) || defined(SMALLK_MM) || defined(CRBIG_MM)
   #define ATL_OOM ATL_bigmmOutOfMem =
#else
   #define ATL_OOM
#endif

#ifndef Cgemm__
   #define UNDEF_ATL_VOID
   #define ATL_VOID void
   #ifdef ALIASED_GEMM
      #define Cgemm__ Mjoin(Mjoin(PATL,aliased_gemm),TATB)
   #elif defined(ATL_TGEMM)
      #define Cgemm__ Mjoin(Mjoin(PATL,tgemm),TATB)
   #else
      #define Cgemm__ Mjoin(Mjoin(PATL,gemm),TATB)
   #endif
#endif

#ifdef FindingJITCPCE
   #define FindingCE
#endif

ATL_VOID Cgemm__(const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc)
{
   int DOCOPY;
   #if defined(CacheEdge) || defined(FindingCE)
      #ifdef FindingCE
         extern int FoundCE, CompCE;
         const int CE_maxK = ( (ATL_DivBySize(FoundCE)-(MB*NB)) /
                               ((MB+NB)*KB) ) * KB;
      #else
         static const int CE_maxK = ( (ATL_DivBySize(CacheEdge)-(MB*NB)) /
                                      (KB*(MB+NB)) )*KB;
      #endif
      int Kp, k=K;
      #ifdef TCPLX
         const TYPE ONE[2] = {1.0, 0.0};
         const TYPE *bet=beta;
      #else
         const TYPE ONE=1.0;
         TYPE bet=beta;
      #endif
   #endif
   MMINTR mm1, mm2, mmNC;

/*
 *  If computing Kp only, return it w/o calling anything
 */
   #ifdef FindingCE
      if (CompCE)
      {
         CompCE = CE_maxK;
         return;
      }
   #endif
   if (!M  ||  !N || !K) return;  /* quick return */
   #ifdef USERGEMM
      mm1 = mm2 = Mjoin(PATU,usergemm);
      if (N >= M)
      {
         mm2 = Mjoin(PATL,mmJIK);
         mmNC = Mjoin(PATL,NCmmJIK);
      }
      else
      {
         mm2 = Mjoin(PATL,mmJIK);
         mmNC = Mjoin(PATL,NCmmIJK);
      }
   #elif defined(FindingJITCPCE) || defined(CRBIG_MM)
      mm2 = mm1 = Mjoin(PATL,mmJITcp);
      mmNC = (N >= M) ? Mjoin(PATL,NCmmJIK) : Mjoin(PATL,NCmmIJK);
   #else
/*
 *    Chose outside loop order based on workspace needs unless we are
 *    doing a rank-K update, where we want JIK for superior C access
 */
      if (N >= M || (K <= KB+KB && M > MB && N > NB))
      {
         mm1 = Mjoin(PATL,mmJIK);
         mm2 = Mjoin(PATL,mmIJK);
         mmNC = Mjoin(PATL,NCmmJIK);
      }
      else
      {
         mm1 = Mjoin(PATL,mmIJK);
         mm2 = Mjoin(PATL,mmJIK);
         mmNC = Mjoin(PATL,NCmmIJK);
      }
      #ifdef TREAL
/*
 *       Use mmJITcp which does JIT A&B copy to avoid streaming data twice
 *       from mem if the reuse of the copied matrices is low; this algorithm
 *       is slower on many systems due to prefetch & noisy cache use, so
 *       don't use it unless K completely dominates
 */
              if ((M<=MB || N <= NB) && ((K>>4) > ATL_3NB))
      #else
/*
 *        For complex, JIT code calls real GEMM, which is faster on some
 *        platforms for all shapes, once K is long enough
 */
         if (K >= C2R_K || ((M < MB || N < NB) && (K>>4) >= ATL_3NB))
      #endif
         {
            mm2 = mm1;
            mm1 = Mjoin(PATL,mmJITcp);
         }
   #endif
   #ifdef SMALLK_MM
      if (ATL_OOM Mjoin(PATL,mmJKI)(ETA, ETB, M, N, K, alpha, A, lda, B, ldb,
                                    beta, C, ldc))
         ATL_assert(mmNC(ETA, ETB, M, N, K, alpha, A, lda, B, ldb,
                         beta, C, ldc) == 0);
      return;
   #endif

/*
 * See what shape matrix has, in order to determine crossover point
 */
   #if defined(SMALL_MM)
      DOCOPY = 0;
   #elif defined(BIG_MM) || defined(FindingCE) || defined(ALIASED_GEMM) || \
         defined(CRBIG_MM)
      DOCOPY = 1;
   #else
      #if defined(TREAL) && defined(ATL_RK_MAXK)
         if (K <= ATL_RK_MAXK) DOCOPY = M*N >= Mjoin(TATB,_MNK_K)/K;
      #else
         if (K <= ATL_3NB) DOCOPY = M*N >= Mjoin(TATB,_MNK_K)/K;
      #endif
      else if (N <= ATL_3NB)
      {
         if (M <= ATL_3NB) DOCOPY = M*N >= Mjoin(TATB,_MNK_MN)/K;
         else DOCOPY = M*N >= Mjoin(TATB,_MNK_N)/K;
      }
      else if (M <= ATL_3NB) DOCOPY = M*N >= Mjoin(TATB,_MNK_M)/K;
      else DOCOPY = M*N >= Mjoin(TATB,_MNK_GE)/K;
   #endif
   if (!DOCOPY)
   {
/*
 *    If we've got a low-rank GEMM with a long M loop that cannot use copy code,
 *    can use axpy-based algorithm.  Should time for crossover, but I'm lazy.
 */
      if (K <= 4 && M > 40)
      {
         if (!Mjoin(PATL,mmJKI)(ETA, ETB, M, N, K, alpha, A, lda, B, ldb,
                                beta, C, ldc))
             return;
      }
      mm1 = mm2 = mmNC;
   }
/*
 * If CacheEdge is set, try to partition K so we have cache reuse
 * on panels of outer matrix
 */
   #if defined(CacheEdge) || defined(FindingCE)
      #ifdef TREAL
         Kp = Mmin(CE_maxK, K);
      #else
         if (mm1 == Mjoin(PATL,mmJITcp))
            Kp = Mmin(NKB_maxK, K);
         else
            Kp = Mmin(CE_maxK, K);
      #endif
      if (Kp < KB) Kp = K;
/*
 *    If we aren't cutting K, make sure we don't need to cut in order to be
 *    able to allocate the required panels of A & B
 *    K so that we have something that will fit
 */
      #ifdef TREAL
      if (K == Kp)
      #else
      if (mm1 != Mjoin(PATL,mmJITcp) && K == Kp)
      #endif
      {
         Kp = (ATL_DivBySize(ATL_MaxMalloc) - MB*NB) / (MB+NB);
         if (Kp > K || Kp < KB) Kp = K;
      }
      do
      {
            if ( mm1(ETA, ETB, M, N, Kp, alpha, A, lda, B, ldb, bet, C, ldc) )
               if ( ATL_OOM mm2(ETA, ETB, M, N, Kp, alpha, A, lda, B, ldb,
                                bet, C, ldc) )
                  if ( ATL_OOM Mjoin(PATL,mmJITcp)(ETA, ETB, -M, N, Kp, alpha,
                          A, lda, B, ldb, bet, C, ldc) )
                  ATL_assert(mmNC(ETA, ETB, M, N, Kp, alpha, A, lda, B, ldb,
                                  bet, C, ldc) == 0);
         bet = ONE;
         A += incA;
         B += incB;
         k -= Kp;
         if (Kp > k) Kp = k;
      }
      while(k);
   #else
      if ( mm1(ETA, ETB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc) )
         if ( mm2(ETA, ETB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc) )
            if ( Mjoin(PATL,mmJITcp)(ETA, ETB, -M, N, K, alpha, A, lda, B, ldb,
                                     beta, C, ldc) )
               ATL_assert(mmNC(ETA, ETB, M, N, K, alpha, A, lda, B, ldb,
                               beta, C, ldc) == 0);
   #endif
}

#undef TATB
#undef tatb
#undef ETB
#undef TB_
#undef tb_
#undef incB
#undef ETA
#undef TA_
#undef ta_
#undef incA
#ifdef UNDEF_ATL_VOID
   #undef UNDEF_ATL_VOID
   #undef ATL_VOID
#endif
