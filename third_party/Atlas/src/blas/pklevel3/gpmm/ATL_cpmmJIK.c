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
#define ATL_NOL3
#include "atlas_pkblas.h"
/*
 * Get NB defined to call the real matmul
 */
#ifdef SCPLX
   #undef SCPLX
   #define SREAL
   #include "atlas_lvl3.h"
   #undef SREAL
   #define SCPLX
#elif defined DCPLX
   #undef DCPLX
   #define DREAL
   #include "atlas_lvl3.h"
   #undef DREAL
   #define DCPLX
#endif

/*
 * Can't use standard include file, since we are doing inter-type calls,
 * so dump explicit headers here
 */
void NBmm_b0(const int M, const int N, const int K, const TYPE alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pNBmm_b0)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pMBmm_b0)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm_b0)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void NBmm_b1(const int M, const int N, const int K, const TYPE alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pNBmm_b1)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pMBmm_b1)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm_b1)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void NBmm_bX(const int M, const int N, const int K, const TYPE alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pNBmm_bX)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pMBmm_bX)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm_bX)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);

static void ATL_gNBmm_b0
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc)
/*
 * ALPHA is known to be 1 (handled by copy)
 * BETA is known to be 0; we handle actual BETA in putblk phase
 */
{
   if (M == MB && N == NB && K == KB)
   {
      NBmm_b0(M, N, K, ATL_rone, A, lda, B, ldb, ATL_rzero, C+NBNB, ldc);
      NBmm_b0(M, N, K, ATL_rone, A, lda, B+NBNB, ldb, ATL_rzero, C, ldc);
      NBmm_bX(M, N, K, ATL_rone, A+NBNB, lda, B+NBNB, ldb, ATL_rnone,
              C+NBNB, ldc);
      NBmm_b1(M, N, K, ATL_rone, A+NBNB, lda, B, ldb, ATL_rone, C, ldc);
   }
   else if (M != MB)
   {
      if (N == NB && K == KB)
      {
         Mjoin(PATLU,pMBmm_b0)(M, N, K, ATL_rone, A, lda, B, ldb,
                               ATL_rzero, C+M*N, ldc);
         Mjoin(PATLU,pMBmm_b0)(M, N, K, ATL_rone, A, lda, B+NBNB, ldb,
                               ATL_rzero, C, ldc);
         Mjoin(PATLU,pMBmm_bX)(M, N, K, ATL_rone, A+M*K, lda, B+NBNB, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pMBmm_b1)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                               ATL_rone, C, ldc);
      }
      else
      {
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B, ldb,
                            ATL_rzero, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                            ATL_rzero, C, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                            ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                            ATL_rone, C, ldc);
      }
   }
   else if (N != NB)  /* ib is full */
   {
      if (K == KB)
      {
         Mjoin(PATLU,pNBmm_b0)(M, N, K, ATL_rone, A, lda, B, ldb,
                               ATL_rzero, C+M*N, ldc);
         Mjoin(PATLU,pNBmm_b0)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                               ATL_rzero, C, ldc);
         Mjoin(PATLU,pNBmm_bX)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pNBmm_b1)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                               ATL_rone, C, ldc);
      }
      else
      {
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B, ldb,
                            ATL_rzero, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                            ATL_rzero, C, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                            ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                            ATL_rone, C, ldc);
      }
   }
   else  /* ib and jb are full */
   {
         Mjoin(PATLU,pKBmm_b0)(M, N, K, ATL_rone, A, lda, B, ldb,
                               ATL_rzero, C+M*N, ldc);
         Mjoin(PATLU,pKBmm_b0)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                               ATL_rzero, C, ldc);
         Mjoin(PATLU,pKBmm_bX)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm_b1)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                               ATL_rone, C, ldc);
   }
}

static void ATL_gNBmm_b1
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc)
/*
 * ALPHA is known to be 1 (handled by copy)
 * BETA is known to be 1; we handle actual BETA in putblk phase
 */
{
   if (M == MB && N == NB && K == KB)
   {
      NBmm_bX(M, N, K, ATL_rone, A, lda, B, ldb, ATL_rnone, C+NBNB, ldc);
      NBmm_b1(M, N, K, ATL_rone, A, lda, B+NBNB, ldb, ATL_rone, C, ldc);
      NBmm_bX(M, N, K, ATL_rone, A+NBNB, lda, B+NBNB, ldb, ATL_rnone,
              C+NBNB, ldc);
      NBmm_b1(M, N, K, ATL_rone, A+NBNB, lda, B, ldb, ATL_rone, C, ldc);
   }
   else if (M != MB)
   {
      if (N == NB && K == KB)
      {
         Mjoin(PATLU,pMBmm_bX)(M, N, K, ATL_rone, A, lda, B, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pMBmm_b1)(M, N, K, ATL_rone, A, lda, B+NBNB, ldb,
                               ATL_rone, C, ldc);
         Mjoin(PATLU,pMBmm_bX)(M, N, K, ATL_rone, A+M*K, lda, B+NBNB, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pMBmm_b1)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                               ATL_rone, C, ldc);
      }
      else
      {
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B, ldb,
                            ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                            ATL_rone, C, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                            ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                            ATL_rone, C, ldc);
      }
   }
   else if (N != NB)  /* ib is full */
   {
      if (K == KB)
      {
         Mjoin(PATLU,pNBmm_bX)(M, N, K, ATL_rone, A, lda, B, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pNBmm_b1)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                               ATL_rone, C, ldc);
         Mjoin(PATLU,pNBmm_bX)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pNBmm_b1)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                               ATL_rone, C, ldc);
      }
      else
      {
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B, ldb,
                            ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                            ATL_rone, C, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                            ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                            ATL_rone, C, ldc);
      }
   }
   else  /* ib and jb are full */
   {
         Mjoin(PATLU,pKBmm_bX)(M, N, K, ATL_rone, A, lda, B, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm_b1)(M, N, K, ATL_rone, A, lda, B+N*K, ldb,
                               ATL_rone, C, ldc);
         Mjoin(PATLU,pKBmm_bX)(M, N, K, ATL_rone, A+M*K, lda, B+N*K, ldb,
                               ATL_rnone, C+M*N, ldc);
         Mjoin(PATLU,pKBmm_b1)(M, N, K, ATL_rone, A+M*K, lda, B, ldb,
                               ATL_rone, C, ldc);
   }
}

int Mjoin(PATL,pmmJIK)
   (const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
    const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, const enum PACK_UPLO UC, TYPE *C, const int ldc)
{
   ATL_xerbla(0, __FILE__, "This routine not yet implemented\n");
   return(-1);
}

int Mjoin(PATL,pmmJIKF)
   (const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
    const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, const enum PACK_UPLO UC, TYPE *C, const int ldc)
{
   ATL_xerbla(0, __FILE__, "This routine not yet implemented\n");
   return(-1);
}
