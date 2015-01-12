#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
#include "atlas_lvl3.h"

void Mjoin(PATL,tvheApAc)(const enum ATLAS_UPLO Uplo, ATL_CINT N, const void *A,
                          ATL_CINT lda, const void *beta, void *C, ATL_CINT ldc)
{
   Mjoin(PATL,heApAc)(Uplo, N, A, lda, SVVAL((const TYPE*)beta), C, ldc);
}

void Mjoin(PATL,ther2k)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const TYPE beta0, TYPE *C, ATL_CINT ldc)
{
   ATL_SYR2K_t sy;
   #ifdef TREAL
      const TYPE ONE = ATL_rone, ZERO = ATL_rzero;
   #else
      const TYPE ONE[2]={ATL_rone, ATL_rzero}, ZERO[2]={ATL_rzero,ATL_rzero};
      const TYPE alpha2[2]={*alpha,(alpha[1]!=ATL_rzero)?-alpha[1]:ATL_rzero};
      const TYPE beta[2] = {beta0, ATL_rzero};
   #endif

   if (N < 1)
      return;
   if (SCALAR_IS_ZERO(alpha) || K < 1)
   {
      if (beta0 != ATL_rone)
         Mjoin(PATL,hescal)(Uplo, N, N, beta0, C, ldc);
      return;
   }
/*
 * This call to serial is usually a performance optimization, but was actually
 * put in to avoid a failure in the LAPACK eigenvalue tests that is caused
 * by subtractive cancellation.  There is no bug, but our parallel algorithm
 * does the operations in a different order than small-case serial, and in
 * one case used by the lapack testers, this causes subtractive cancellation
 * to occur (the serial code can use different orders for differing problem
 * sizes).  Other problems will experience cancelation under the serial order,
 * so AFAIK, neither is more correct numerically.
 */
   if (N < 3*NB && K < 3*NB)
   {
      Mjoin(PATL,her2k)(Uplo, Trans, N, K, alpha, A, lda, B, ldb,
                        beta0, C, ldc);
      return;
   }
   sy.alpha = SADD alpha;
   sy.alpha2 = alpha2;
   sy.beta  = beta;
   sy.one = SADD ONE;
   sy.zero = SADD ZERO;
   sy.tvgemm = Mjoin(PATL,tvgemm);
   sy.tvApAt = Mjoin(PATL,tvheApAc);
   sy.K = K;
   sy.lda = lda;
   sy.ldb = ldb;
   sy.ldc = ldc;
   sy.eltsh = Mjoin(PATL,shift);
   sy.Uplo = Uplo;
   sy.trans = Trans;
   if (Trans == AtlasNoTrans)
   {
      sy.TA = AtlasNoTrans;
      sy.TB = AtlasConjTrans;
      sy.TA2 = AtlasConjTrans;
      sy.TB2 = AtlasNoTrans;
   }
   else
   {
      sy.TA = AtlasConjTrans;
      sy.TB = AtlasNoTrans;
      sy.TA2 = AtlasNoTrans;
      sy.TB2 = AtlasConjTrans;
   }
   sy.nb = Mjoin(PATL,GetNB)();
   ATL_tvsyr2k_rec(&sy, N/sy.nb, N%sy.nb, A, B, C);
}

