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
#ifndef ATLAS_PKBLAS_H
#define ATLAS_PKBLAS_H

#include "atlas_misc.h"
#ifndef ATL_NOL3
#include "atlas_lvl3.h"
#endif

#define CBLAS_ENUM_ONLY
#include "cblas.h"
#undef CBLAS_ENUM_ONLY

enum PACK_UPLO {PackUpper=121, PackLower=122, PackGen=123};

#define PACK_ORDER CBLAS_ORDER
   #define PackRowMajor CblasRowMajor
   #define PackColMajor CblasColMajor
#define PACK_TRANS CBLAS_TRANSPOSE
   #define PackNoTrans CblasNoTrans
   #define PackTrans CblasTrans
   #define PackConjTrans CblasConjTrans
   #define PackConj AtlasConj
#define PACK_DIAG CBLAS_DIAG
   #define PackNonUnit CblasNonUnit
   #define PackUnit CblasUnit
#define PACK_SIDE CBLAS_SIDE
   #define PackLeft  CblasLeft
   #define PackRight CblasRight

#ifndef ATL_pkMaxMalloc
   #define ATL_pkMaxMalloc ATL_MaxMalloc
#endif

#ifdef TCPLX
   #define MindexPL(I_,J_,lda_) \
      ( (((J_)*(((size_t)(lda_))+(lda_)-(J_)-1))) + (I_)+(I_) )
   #define MindexPU(I_,J_,lda_) \
      ( (((((size_t)(lda_))+(lda_)+(J_)-1)*(J_))) + (I_)+(I_) )
#else
   #define MindexPL(I_,J_,lda_) \
      ( (((J_)*(((size_t)(lda_))+(lda_)-(J_)-1))>>1) + (I_) )
   #define MindexPU(I_,J_,lda_) \
      ( (((((size_t)(lda_))+(lda_)+(J_)-1)*(J_))>>1) + (I_) )
#endif
#define MindexP(uplo_,I_,J_,lda_) \
   ( (uplo_) == PackUpper ? MindexPU(I_,J_,lda_) : \
     ( (uplo_) == PackLower ? MindexPL(I_,J_,lda_) : \
                              (((J_)*((size_t)(lda_))+(I_))SHIFT) ) )
#define Mpld(uplo_,J_,lda_) (uplo_) == PackUpper ? (lda_)+(J_) : \
              ( (uplo_) == PackLower ? (lda_)-(J_) : (lda_) )


void ATL_sgpmm(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum PACK_UPLO UB, const enum PACK_TRANS TB,
               const enum PACK_UPLO UC,
               const int M, const int N, const int K, const float alpha,
               const float *A, const int IA, const int JA, const int lda,
               const float *B, const int IB, const int JB, const int ldb,
               const float beta, float *C, const int IC, const int JC,
               const int ldc);
void ATL_sprankK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                      const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K, int R,
                      const SCALAR alpha, const TYPE *A, int lda,
                      const TYPE *B, int ldb, const SCALAR beta,
                      const enum PACK_UPLO UC, TYPE *C, int ldc);
int ATL_spmmJIKF(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                 const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                 const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, const enum PACK_UPLO UC,
                 TYPE *C, const int ldc);
int ATL_spmmJIK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                const int M, const int N, const int K, const float alpha,
                const float *A, const int lda, const float *B, const int ldb,
                const float beta, const enum PACK_UPLO UC,
                float *C, const int ldc);
void ATL_spcol2blkF(const int M, const int N, const float alpha,
                    const float *A, int lda, const int ldainc, float *V);
void ATL_sprow2blkTF(const int M, const int N, const float alpha,
                     const float *A, int lda, const int ldainc, float *V);
void ATL_spcol2blk_a1(const int M, const int N, const float alpha,
                      const float *A, int lda, const int ldainc, float *V);
void ATL_spcol2blk_aX(const int M, const int N, const float alpha,
                      const float *A, int lda, const int ldainc, float *V);
void ATL_sprow2blkT_a1(const int M, const int N, const float alpha,
                       const float *A, int lda, const int ldainc, float *V);
void ATL_sprow2blkT_aX(const int M, const int N, const float alpha,
                       const float *A, int lda, const int ldainc, float *V);
void ATL_spputblk(const int M, const int N, const TYPE *V, TYPE *C,
                  int ldc, int ldcinc, const SCALAR beta);
void ATL_spputblk_diag
   (const int M, const int N, const float *V, const enum ATLAS_UPLO UC,
    float *C, int ldc, int ldcinc, const float alpha, const float beta);
void ATL_spputblk_aX
   (const int M, const int N, const float *V, float *C, int ldc, int ldcinc,
    const float alpha, const float beta);
void ATL_ssprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const float alpha,
               const float *A, const int IA, const int JA, const int lda,
               const float beta,
               float *C, const int IC, const int JC, const int ldc);
void ATL_shprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const float alpha,
               const float *A, const int IA, const int JA, const int lda,
               const float beta,
               float *C, const int IC, const int JC, const int ldc);
void ATL_shprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const float alpha,
                  const float *A, int lda, const float beta,
                  float *C, const int ldc);
int ATL_sphk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const float alpha, const float *A, const int lda,
                 const float beta, const int CP, float *C, const int ldc);
void ATL_ssprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const float alpha,
                  const float *A, int lda, const float beta,
                  float *C, const int ldc);
int ATL_sprk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const float alpha, const float *A, const int lda,
                 const float beta, const int CP, float *C, const int ldc);

void ATL_dgpmm(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum PACK_UPLO UB, const enum PACK_TRANS TB,
               const enum PACK_UPLO UC,
               const int M, const int N, const int K, const double alpha,
               const double *A, const int IA, const int JA, const int lda,
               const double *B, const int IB, const int JB, const int ldb,
               const double beta, double *C, const int IC, const int JC,
               const int ldc);
void ATL_dprankK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                      const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K, int R,
                      const SCALAR alpha, const TYPE *A, int lda,
                      const TYPE *B, int ldb, const SCALAR beta,
                      const enum PACK_UPLO UC, TYPE *C, int ldc);
int ATL_dpmmJIKF(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                 const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                 const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, const enum PACK_UPLO UC,
                 TYPE *C, const int ldc);
int ATL_dpmmJIK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                const int M, const int N, const int K, const double alpha,
                const double *A, const int lda, const double *B, const int ldb,
                const double beta, const enum PACK_UPLO UC,
                double *C, const int ldc);
void ATL_dpcol2blkF(const int M, const int N, const double alpha,
                    const double *A, int lda, const int ldainc, double *V);
void ATL_dprow2blkTF(const int M, const int N, const double alpha,
                     const double *A, int lda, const int ldainc, double *V);
void ATL_dpcol2blk_a1(const int M, const int N, const double alpha,
                      const double *A, int lda, const int ldainc, double *V);
void ATL_dpcol2blk_aX(const int M, const int N, const double alpha,
                      const double *A, int lda, const int ldainc, double *V);
void ATL_dprow2blkT_a1(const int M, const int N, const double alpha,
                       const double *A, int lda, const int ldainc, double *V);
void ATL_dprow2blkT_aX(const int M, const int N, const double alpha,
                       const double *A, int lda, const int ldainc, double *V);
void ATL_dpputblk(const int M, const int N, const TYPE *V, TYPE *C,
                  int ldc, int ldcinc, const SCALAR beta);
void ATL_dpputblk_diag
   (const int M, const int N, const double *V, const enum ATLAS_UPLO UC,
    double *C, int ldc, int ldcinc, const double alpha, const double beta);
void ATL_dpputblk_aX
   (const int M, const int N, const double *V, double *C, int ldc, int ldcinc,
    const double alpha, const double beta);
void ATL_dsprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const double alpha,
               const double *A, const int IA, const int JA, const int lda,
               const double beta,
               double *C, const int IC, const int JC, const int ldc);
void ATL_dhprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const double alpha,
               const double *A, const int IA, const int JA, const int lda,
               const double beta,
               double *C, const int IC, const int JC, const int ldc);
void ATL_dhprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const double alpha,
                  const double *A, int lda, const double beta,
                  double *C, const int ldc);
int ATL_dphk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const double alpha, const double *A, const int lda,
                 const double beta, const int CP, double *C, const int ldc);
void ATL_dsprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const double alpha,
                  const double *A, int lda, const double beta,
                  double *C, const int ldc);
int ATL_dprk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const double alpha, const double *A, const int lda,
                 const double beta, const int CP, double *C, const int ldc);

void ATL_cgpmm(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum PACK_UPLO UB, const enum PACK_TRANS TB,
               const enum PACK_UPLO UC,
               const int M, const int N, const int K, const float* alpha,
               const float *A, const int IA, const int JA, const int lda,
               const float *B, const int IB, const int JB, const int ldb,
               const float* beta, float *C, const int IC, const int JC,
               const int ldc);
void ATL_cprankK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                      const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K, int R,
                      const SCALAR alpha, const TYPE *A, int lda,
                      const TYPE *B, int ldb, const SCALAR beta,
                      const enum PACK_UPLO UC, TYPE *C, int ldc);
int ATL_cpmmJIKF(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                 const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                 const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, const enum PACK_UPLO UC,
                 TYPE *C, const int ldc);
int ATL_cpmmJIK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                const int M, const int N, const int K, const float* alpha,
                const float *A, const int lda, const float *B, const int ldb,
                const float* beta, const enum PACK_UPLO UC,
                float *C, const int ldc);
void ATL_cpcol2blkF(const int M, const int N, const float* alpha,
                    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkTF(const int M, const int N, const float* alpha,
                     const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blk_a1(const int M, const int N, const float* alpha,
                      const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blk_aX(const int M, const int N, const float* alpha,
                      const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkT_a1(const int M, const int N, const float* alpha,
                       const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkT_aX(const int M, const int N, const float* alpha,
                       const float *A, int lda, const int ldainc, float *V);
void ATL_cpputblk(const int M, const int N, const TYPE *V, TYPE *C,
                  int ldc, int ldcinc, const SCALAR beta);
void ATL_cpputblk_diag
   (const int M, const int N, const float *V, const enum ATLAS_UPLO UC,
    float *C, int ldc, int ldcinc, const float* alpha, const float* beta);
void ATL_cpputblk_aX
   (const int M, const int N, const float *V, float *C, int ldc, int ldcinc,
    const float* alpha, const float* beta);
void ATL_csprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const float* alpha,
               const float *A, const int IA, const int JA, const int lda,
               const float* beta,
               float *C, const int IC, const int JC, const int ldc);
void ATL_chprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const float alpha,
               const float *A, const int IA, const int JA, const int lda,
               const float beta,
               float *C, const int IC, const int JC, const int ldc);
void ATL_chprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const float* alpha,
                  const float *A, int lda, const float* beta,
                  float *C, const int ldc);
int ATL_cphk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const float* alpha, const float *A, const int lda,
                 const float* beta, const int CP, float *C, const int ldc);
void ATL_csprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const float* alpha,
                  const float *A, int lda, const float* beta,
                  float *C, const int ldc);
int ATL_cprk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const float* alpha, const float *A, const int lda,
                 const float* beta, const int CP, float *C, const int ldc);

void ATL_zgpmm(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum PACK_UPLO UB, const enum PACK_TRANS TB,
               const enum PACK_UPLO UC,
               const int M, const int N, const int K, const double* alpha,
               const double *A, const int IA, const int JA, const int lda,
               const double *B, const int IB, const int JB, const int ldb,
               const double* beta, double *C, const int IC, const int JC,
               const int ldc);
void ATL_zprankK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                      const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K, int R,
                      const SCALAR alpha, const TYPE *A, int lda,
                      const TYPE *B, int ldb, const SCALAR beta,
                      const enum PACK_UPLO UC, TYPE *C, int ldc);
int ATL_zpmmJIKF(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                 const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                 const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, const enum PACK_UPLO UC,
                 TYPE *C, const int ldc);
int ATL_zpmmJIK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                const int M, const int N, const int K, const double* alpha,
                const double *A, const int lda, const double *B, const int ldb,
                const double* beta, const enum PACK_UPLO UC,
                double *C, const int ldc);
void ATL_zpcol2blkF(const int M, const int N, const double* alpha,
                    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkTF(const int M, const int N, const double* alpha,
                     const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blk_a1(const int M, const int N, const double* alpha,
                      const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blk_aX(const int M, const int N, const double* alpha,
                      const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkT_a1(const int M, const int N, const double* alpha,
                       const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkT_aX(const int M, const int N, const double* alpha,
                       const double *A, int lda, const int ldainc, double *V);
void ATL_zpputblk(const int M, const int N, const TYPE *V, TYPE *C,
                  int ldc, int ldcinc, const SCALAR beta);
void ATL_zpputblk_diag
   (const int M, const int N, const double *V, const enum ATLAS_UPLO UC,
    double *C, int ldc, int ldcinc, const double* alpha, const double* beta);
void ATL_zpputblk_aX
   (const int M, const int N, const double *V, double *C, int ldc, int ldcinc,
    const double* alpha, const double* beta);
void ATL_zsprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const double* alpha,
               const double *A, const int IA, const int JA, const int lda,
               const double* beta,
               double *C, const int IC, const int JC, const int ldc);
void ATL_zhprk(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
               const enum ATLAS_UPLO UC, const int CP,
               const int N, const int K, const double alpha,
               const double *A, const int IA, const int JA, const int lda,
               const double beta,
               double *C, const int IC, const int JC, const int ldc);
void ATL_zhprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const double* alpha,
                  const double *A, int lda, const double* beta,
                  double *C, const int ldc);
int ATL_zphk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const double* alpha, const double *A, const int lda,
                 const double* beta, const int CP, double *C, const int ldc);
void ATL_zsprk_rK(const enum PACK_UPLO UA, const enum PACK_TRANS TA,
                  const enum ATLAS_UPLO UC, const int CP,
                  const int N, const int K, int R, const double* alpha,
                  const double *A, int lda, const double* beta,
                  double *C, const int ldc);
int ATL_zprk_kmm(const enum ATLAS_UPLO UC, const enum PACK_UPLO UA,
                 const enum ATLAS_TRANS TA, const int N, const int K,
                 const double* alpha, const double *A, const int lda,
                 const double* beta, const int CP, double *C, const int ldc);

void ATL_cpcol2blk_aX_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkT_aX_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blk_a1_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkT_a1_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkTF_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkF_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConjF
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConj_a1
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConj_aX
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blk_aXi0
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConj_aXi0
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc,float*V);
void ATL_cprow2blkHF
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkH_a1
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkH_aX
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkH_aXi0
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkT_aXi0
   (const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConjF_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConj_a1_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConj_aX_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blk_aXi0_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cpcol2blkConj_aXi0_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc,float*V);
void ATL_cprow2blkHF_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkH_a1_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkH_aX_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkH_aXi0_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);
void ATL_cprow2blkT_aXi0_blk
   (const int blk, const int M, const int N, const float* alpha,
    const float *A, int lda, const int ldainc, float *V);

void ATL_cprow2blkT_KB_aXi0
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_cprow2blkT_KB_aX
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_cprow2blkT_KB_a1
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_cprow2blkH_KB_aXi0
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_cprow2blkH_KB_aX
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_cprow2blkH_KB_a1
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_zpcol2blk_aX_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkT_aX_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blk_a1_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkT_a1_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkTF_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkF_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConjF
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConj_a1
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConj_aX
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blk_aXi0
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConj_aXi0
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc,double*V);
void ATL_zprow2blkHF
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkH_a1
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkH_aX
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkH_aXi0
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkT_aXi0
   (const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConjF_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConj_a1_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConj_aX_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blk_aXi0_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zpcol2blkConj_aXi0_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc,double*V);
void ATL_zprow2blkHF_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkH_a1_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkH_aX_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkH_aXi0_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);
void ATL_zprow2blkT_aXi0_blk
   (const int blk, const int M, const int N, const double* alpha,
    const double *A, int lda, const int ldainc, double *V);

void ATL_zprow2blkT_KB_aXi0
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_zprow2blkT_KB_aX
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_zprow2blkT_KB_a1
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_zprow2blkH_KB_aXi0
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_zprow2blkH_KB_aX
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);
void ATL_zprow2blkH_KB_a1
   (const int mb, const int nb, const SCALAR alpha, const TYPE *A, int lda,
    const int ldainc, TYPE *V);

#endif
