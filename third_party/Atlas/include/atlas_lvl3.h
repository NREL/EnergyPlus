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

#ifndef ATLAS_LVL3_H
#define ATLAS_LVL3_H

#include "atlas_misc.h"
#include "atlas_f77.h"
#include "atlas_level3.h"
#if defined(SREAL)
   #include "smm.h"
   #include "sXover.h"
#elif defined(DREAL)
   #include "dmm.h"
   #include "dXover.h"
#elif defined(QREAL)
   #include "qmm.h"
   #include "qXover.h"
#elif defined(SCPLX)
   #ifdef ATL_NCMM
      #include "atlas_cNCmm.h"
   #else
      #include "cmm.h"
   #endif
   #include "cXover.h"
#elif defined(DCPLX)
   #ifdef ATL_NCMM
      #include "atlas_zNCmm.h"
   #else
      #include "zmm.h"
   #endif
   #include "zmm.h"
   #include "zXover.h"
#endif
#ifndef ATL_3NB
   #define ATL_3NB 3*NB

   #define NN_MNK_M  NBNB*NB
   #define NN_MNK_N  NBNB*NB
   #define NN_MNK_K  NBNB*NB
   #define NN_MNK_MN NBNB*NB
   #define NN_MNK_GE NBNB*NB

   #define NT_MNK_M  NBNB*NB
   #define NT_MNK_N  NBNB*NB
   #define NT_MNK_K  NBNB*NB
   #define NT_MNK_MN NBNB*NB
   #define NT_MNK_GE NBNB*NB

   #define TN_MNK_M  NBNB*NB
   #define TN_MNK_N  NBNB*NB
   #define TN_MNK_K  NBNB*NB
   #define TN_MNK_MN NBNB*NB
   #define TN_MNK_GE NBNB*NB

   #define TT_MNK_M  NBNB*NB
   #define TT_MNK_N  NBNB*NB
   #define TT_MNK_K  NBNB*NB
   #define TT_MNK_MN NBNB*NB
   #define TT_MNK_GE NBNB*NB
#endif

#ifndef CN_MNK_M
   #define CN_MNK_M  TN_MNK_M
   #define CN_MNK_N  TN_MNK_N
   #define CN_MNK_K  TN_MNK_K
   #define CN_MNK_MN TN_MNK_MN
   #define CN_MNK_GE TN_MNK_GE
#endif
#ifndef NC_MNK_M
   #define NC_MNK_M  NT_MNK_M
   #define NC_MNK_N  NT_MNK_N
   #define NC_MNK_K  NT_MNK_K
   #define NC_MNK_MN NT_MNK_MN
   #define NC_MNK_GE NT_MNK_GE
#endif
#ifndef CT_MNK_M
   #define CT_MNK_M  TT_MNK_M
   #define CT_MNK_N  TT_MNK_N
   #define CT_MNK_K  TT_MNK_K
   #define CT_MNK_MN TT_MNK_MN
   #define CT_MNK_GE TT_MNK_GE
#endif
#ifndef TC_MNK_M
   #define TC_MNK_M  TT_MNK_M
   #define TC_MNK_N  TT_MNK_N
   #define TC_MNK_K  TT_MNK_K
   #define TC_MNK_MN TT_MNK_MN
   #define TC_MNK_GE TT_MNK_GE
#endif
#ifndef CC_MNK_M
   #define CC_MNK_M  TT_MNK_M
   #define CC_MNK_N  TT_MNK_N
   #define CC_MNK_K  TT_MNK_K
   #define CC_MNK_MN TT_MNK_MN
   #define CC_MNK_GE TT_MNK_GE
#endif

#define CPAT Mjoin(C_ATL_, PRE);

#ifndef ATL_MaxMalloc
   #ifdef ATL_MaxMalloc_MB
      #define ATL_MaxMalloc (((size_t)(ATL_MaxMalloc_MB))<<20)
   #else
      #define ATL_MaxMalloc 67108864
   #endif
#endif

typedef void (*MAT2BLK)(int, int, const TYPE*, int, TYPE*, const SCALAR);
typedef void (*MAT2BLK2)(const int, const int, const SCALAR, const TYPE*,
                         const int, TYPE*, const int);
typedef void (*MATSCAL)(const int, const int, const SCALAR, TYPE*, const int);
typedef void (*PUTBLK)(int, int, TYPE*, TYPE*, int, const SCALAR);
typedef void (*NBCLEANUP)(const TYPE*, const TYPE*, TYPE*, const int);
typedef int (*MMINTR)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                      const int, const int, const int, const SCALAR,
                      const TYPE *, const int, const TYPE *, const int,
                      const SCALAR, TYPE *, const int);
typedef void (*NBMM0)(const int, const int, const int, const TYPE,
                      const TYPE*, const int, const TYPE*, const int,
                      const TYPE, TYPE*, const int);

void ATL_xerbla(int p, char *rout, char *form, ...);
int Mjoin(PATL,GetNB)(void);
int Mjoin(PATL,GetNCNB)(void);

void Mjoin(PATL, gescal_bX)(const int, const int, const SCALAR, TYPE*,
           const int);
void Mjoin(PATL, gescal_bn1)(const int, const int, const SCALAR, TYPE*,
           const int);
void Mjoin(PATL, gescal_b0)(const int, const int, const SCALAR, TYPE*,
           const int);

void Mjoin(PATL,pKBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pNBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pMBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pKBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pNBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pMBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pKBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pNBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pMBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,pKBmm)(const int M, const int N, const int K,
                       const TYPE alpha, const TYPE *A, const int lda,
                       const TYPE *B, const int ldb, const TYPE beta,
                       TYPE *C, const int ldc);

void Mjoin(PATL,MBJBmm)(const int N, const int K, const TYPE *A, const TYPE *B,
                        const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATL,IBJBmm)(int IB, int JB, int K, const TYPE *A, const TYPE *B,
                        const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATL,IBNBmm)(const int M, const int K, const TYPE *A, const TYPE *B,
                        const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATL,putblk_bX)(int M, int N, TYPE *V, TYPE *C, int ldc, const SCALAR beta);
void Mjoin(PATL,putblk_bn1)(int M, int N, TYPE *V, TYPE *C, int ldc, const SCALAR beta);
void Mjoin(PATL,putblk_b1)(int M, int N, TYPE *V, TYPE *C, int ldc, const SCALAR beta);
void Mjoin(PATL,putblk_b0)(int M, int N, TYPE *V, TYPE *C, int ldc, const SCALAR beta);

#ifdef TCPLX

void Mjoin(PATL,CNBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,CNBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,CNBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATL,putblk_bXi0)(int M, int N, TYPE *V, TYPE *C, int ldc, const SCALAR beta);
void Mjoin(PATL, gescal_bXi0)(const int, const int, const SCALAR, TYPE*,
           const int);

void Mjoin(PATL,row2blkT_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkT2_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk2_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);

void Mjoin(PATL,row2blkC_aX)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkC2_aX)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blkConj_aX)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blkConj2_aX)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkC_a1)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkC2_a1)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blkConj_a1)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blkConj2_a1)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkC_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkC2_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blkConj_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blkConj2_aXi0)
   (const int, const int, const TYPE*, const int, TYPE*, const SCALAR);

void Mjoin(PATL,mmJIK2)
   (int K, int nMb, int nNb, int nKb, int ib, int jb, int kb,
    const SCALAR alpha, const TYPE *pA0, const TYPE *B, int ldb, TYPE *pB0,
    int incB, MAT2BLK B2blk, const SCALAR beta, TYPE *C, int ldc,
    MATSCAL gescal, NBMM0 NBmm0);

void Mjoin(PATL,mmIJK2)
   (int K, int nMb, int nNb, int nKb, int ib, int jb, int kb,
    const SCALAR alpha, const TYPE *A, const int lda, TYPE *pA0, const int incA,
    MAT2BLK A2blk, TYPE *pB0, const SCALAR beta, TYPE *C, int ldc,
    MATSCAL gescal, NBMM0 NBmm0);
void ATL_gereal2cplx(const int M, const int N, TYPE *alpha, TYPE *R, int ldr,
                     TYPE *I, int ldi, TYPE *beta, TYPE *C, int ldc);

#else  /* real */


void NBmm_b1(const int M, const int N, const int K, const TYPE alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const TYPE beta, TYPE *C, const int ldc);
void NBmm_b0(const int M, const int N, const int K, const TYPE alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const TYPE beta, TYPE *C, const int ldc);
void NBmm_bX(const int M, const int N, const int K, const TYPE alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const TYPE beta, TYPE *C, const int ldc);
void Mjoin(PATL,mmJIK2)(int K, int nMb, int nNb, int nKb, int ib, int jb,
                          int kb, const SCALAR alpha, const TYPE *pA0,
                          const TYPE *B, int ldb, TYPE *pB0, int incB,
                          MAT2BLK B2blk, const SCALAR beta, TYPE *C, int ldc,
                          TYPE *pC, PUTBLK putblk, NBMM0 NBmm0);

void Mjoin(PATL,mmIJK2)(int K, int nMb, int nNb, int nKb, int ib, int jb,
                          int kb, const SCALAR alpha, const TYPE *A, int lda,
                          TYPE *pA0, int incA, MAT2BLK A2blk, const TYPE *pB0,
                          const SCALAR beta, TYPE *C, int ldc, TYPE *pC,
                          PUTBLK putblk, NBMM0 NBmm0);


void Mjoin(PATL,aliased_gemm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,f77gemm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,small_mm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,big_mm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
#endif

#ifdef USERGEMM
int  Mjoin(PATU,usergemm)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                          const int, const int, const int, const SCALAR,
                          const TYPE*, const int, const TYPE*,
                          const int, const SCALAR, TYPE*, const int);
#endif
int  Mjoin(PATL,NCmmJIK)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                         const int, const int, const int, const SCALAR,
                         const TYPE*, const int, const TYPE*,
                         const int, const SCALAR, TYPE*, const int);
int  Mjoin(PATL,NCmmIJK)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                         const int, const int, const int, const SCALAR,
                         const TYPE*, const int, const TYPE*,
                         const int, const SCALAR, TYPE*, const int);
int  Mjoin(PATL,NCmmJIK_c)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                           const int, const int, const int, const SCALAR,
                           const TYPE*, const int, const TYPE*,
                           const int, const SCALAR, TYPE*, const int);
int  Mjoin(PATL,NCmmIJK_c)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                           const int, const int, const int, const SCALAR,
                           const TYPE*, const int, const TYPE*,
                           const int, const SCALAR, TYPE*, const int);

void Mjoin(PATL,row2blkT2_aX)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkT_aX)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk2_aX)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk_aX)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkT2_an1)(int, int, const TYPE*, int, TYPE*,
                               const SCALAR);
void Mjoin(PATL,row2blkT_an1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk2_an1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk_an1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkT2_a1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,row2blkT_a1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk2_a1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk_a1)(int, int, const TYPE*, int, TYPE*, const SCALAR);

#ifdef TREAL
int Mjoin(PATL,vrankK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                       ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, const TYPE *B,
                       ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
#endif
int Mjoin(PATL,mmJITcp)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                        const int M, const int N, const int K,
                        const SCALAR alpha, const TYPE *A, const int lda,
                        const TYPE *B, const int ldb, const SCALAR beta,
                        TYPE *C, const int ldc);
int Mjoin(PATL,mmJIK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda,
                      const TYPE *B, const int ldb, const SCALAR beta,
                      TYPE *C, const int ldc);
int Mjoin(PATL,mmIJK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda,
                      const TYPE *B, const int ldb, const SCALAR beta,
                      TYPE *C, const int ldc);
int Mjoin(PATL,mmJKI)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda,
                      const TYPE *B, const int ldb, const SCALAR beta,
                      TYPE *C, const int ldc);

void Mjoin(PATL,mmK)
   (int M, int m, int N, int n, int nblk, int kr, int KR, const SCALAR alphaA,
    const SCALAR alphaB, const SCALAR beta, const TYPE *A, const int lda,
    const int incA, TYPE *pA, const int incAW, const TYPE *B, const int ldb,
    const int incB, TYPE *pB, const int incBW, TYPE *C, const int ldc,
    MAT2BLK2 A2blk, MAT2BLK2 B2blk, NBMM0 NBmm0, NBMM0 NBmm1);

int Mjoin(PATL,mmBPP)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda,
                      const TYPE *B, const int ldb, const SCALAR beta,
                      TYPE *C, const int ldc);


void Mjoin(PATL,gemmTT)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmTT)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmTN)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmTN)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmNT)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmNT)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmNN)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmNN)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
#ifdef TCPLX
void Mjoin(PATL,gemmCC)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmCC)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmCT)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmCT)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmCN)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmCN)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmTC)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmTC)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,gemmNC)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(PATL,aliased_gemmNC)
   (const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
#endif


void NCmmNNIJK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNTIJK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTNIJK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTTIJK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNNIJK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNTIJK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTNIJK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTTIJK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNNJIK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNTJIK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTNJIK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTTJIK_c
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNNJIK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmNTJIK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTNJIK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void NCmmTTJIK
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

#endif
