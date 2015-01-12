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

#include "atlas_level2.h"
#include "atlas_kernel2.h"
#ifndef ATLAS_LVL2_H
#define ATLAS_LVL2_H

int ATL_L2AIsOverlapped(int rank, int sz, size_t M, size_t N,
                        size_t A, size_t lda);

/*
 * Real kernels
 */
void ATL_sger_OOC
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_sger_L1
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_sger_L2
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_sger
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_sger2_OOC
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_sger2_L1
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_sger2_L2
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_sger2
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void Mjoin(PATL,gemv)
   (const enum ATLAS_TRANS TA, ATL_CINT M, ATL_CINT N, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *X, ATL_CINT incX,
    const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);

void ATL_smvnk_smallN
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float beta,
    float *Y, ATL_CINT incY);
void ATL_smvnk_Mlt16
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float beta,
    float *Y, ATL_CINT incY);
void ATL_smvtk_smallN
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float beta,
    float *Y, ATL_CINT incY);
void ATL_smvtk_Mlt16
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float beta,
    float *Y, ATL_CINT incY);
void ATL_sger2k_Mlt16
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_sger2k_Nlt8
   (ATL_CINT M, ATL_CINT N, const float alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_sgerk_Mlt16
   (const int M, const int N, const float alpha, const float *X,
    const int incX, const float *Y, const int incY, float *A, const int lda);
void ATL_sgerk_axpy
   (const int M, const int N, const float alpha, const float *X,
    const int incX, const float *Y, const int incY, float *A, const int lda);
void ATL_dger_OOC
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_dger_L1
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_dger_L2
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_dger
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_dger2_OOC
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_dger2_L1
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_dger2_L2
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_dger2
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void Mjoin(PATL,gemv)
   (const enum ATLAS_TRANS TA, ATL_CINT M, ATL_CINT N, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *X, ATL_CINT incX,
    const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);

void ATL_dmvnk_smallN
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double beta,
    double *Y, ATL_CINT incY);
void ATL_dmvnk_Mlt16
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double beta,
    double *Y, ATL_CINT incY);
void ATL_dmvtk_smallN
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double beta,
    double *Y, ATL_CINT incY);
void ATL_dmvtk_Mlt16
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double beta,
    double *Y, ATL_CINT incY);
void ATL_dger2k_Mlt16
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_dger2k_Nlt8
   (ATL_CINT M, ATL_CINT N, const double alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_dgerk_Mlt16
   (const int M, const int N, const double alpha, const double *X,
    const int incX, const double *Y, const int incY, double *A, const int lda);
void ATL_dgerk_axpy
   (const int M, const int N, const double alpha, const double *X,
    const int incX, const double *Y, const int incY, double *A, const int lda);

/*
 * Complex kernels
 */
void Mjoin(PATL,gemvCT_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCT_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCT)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCN_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCN_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void ATL_cgeru_OOC
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgeru_L2
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgeru_L1
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgeru
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgerc_OOC
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgerc_L2
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgerc_L1
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cgerc
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, float *A, ATL_CINT lda);
void ATL_cger2u_OOC
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2u_L2
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2u_L1
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2u
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2c_OOC
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2c_L2
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2c_L1
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2c
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cmvnk_smallN
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float *beta,
    float *Y, ATL_CINT incY);
void ATL_cmvnk_Mlt16
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float *beta,
    float *Y, ATL_CINT incY);
void ATL_cmvtk_smallN
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float *beta,
    float *Y, ATL_CINT incY);
void ATL_cmvtk_Mlt16
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    const float *X, ATL_CINT incX, const float *beta,
    float *Y, ATL_CINT incY);
void ATL_cger2ck_Mlt16
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2ck_Nlt8
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2k_Mlt16
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cger2k_Nlt8
   (ATL_CINT M, ATL_CINT N, const float *alpha, const float *X,
    ATL_CINT incX, const float *Y, ATL_CINT incY, const float *beta,
    const float *W, ATL_CINT incW, const float *Z, ATL_CINT incZ,
    float *A, ATL_CINT lda);
void ATL_cgerck_Mlt16
   (const int M, const int N, const float *alpha, const float *X,
    const int incX, const float *Y, const int incY, float *A, const int lda);
void ATL_cgerck_axpy
   (const int M, const int N, const float *alpha, const float *X,
    const int incX, const float *Y, const int incY, float *A, const int lda);
void ATL_cgerk_Mlt16
   (const int M, const int N, const float *alpha, const float *X,
    const int incX, const float *Y, const int incY, float *A, const int lda);
void ATL_cgerk_axpy
   (const int M, const int N, const float *alpha, const float *X,
    const int incX, const float *Y, const int incY, float *A, const int lda);
void Mjoin(PATL,gemvCT_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCT_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCT)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvT)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCN_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCN_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvCN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L2)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN_L1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void Mjoin(PATL,gemvN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
void ATL_zgeru_OOC
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgeru_L2
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgeru_L1
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgeru
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgerc_OOC
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgerc_L2
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgerc_L1
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zgerc
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, double *A, ATL_CINT lda);
void ATL_zger2u_OOC
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2u_L2
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2u_L1
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2u
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2c_OOC
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2c_L2
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2c_L1
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2c
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zmvnk_smallN
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double *beta,
    double *Y, ATL_CINT incY);
void ATL_zmvnk_Mlt16
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double *beta,
    double *Y, ATL_CINT incY);
void ATL_zmvtk_smallN
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double *beta,
    double *Y, ATL_CINT incY);
void ATL_zmvtk_Mlt16
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    const double *X, ATL_CINT incX, const double *beta,
    double *Y, ATL_CINT incY);
void ATL_zger2ck_Mlt16
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2ck_Nlt8
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2k_Mlt16
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zger2k_Nlt8
   (ATL_CINT M, ATL_CINT N, const double *alpha, const double *X,
    ATL_CINT incX, const double *Y, ATL_CINT incY, const double *beta,
    const double *W, ATL_CINT incW, const double *Z, ATL_CINT incZ,
    double *A, ATL_CINT lda);
void ATL_zgerck_Mlt16
   (const int M, const int N, const double *alpha, const double *X,
    const int incX, const double *Y, const int incY, double *A, const int lda);
void ATL_zgerck_axpy
   (const int M, const int N, const double *alpha, const double *X,
    const int incX, const double *Y, const int incY, double *A, const int lda);
void ATL_zgerk_Mlt16
   (const int M, const int N, const double *alpha, const double *X,
    const int incX, const double *Y, const int incY, double *A, const int lda);
void ATL_zgerk_axpy
   (const int M, const int N, const double *alpha, const double *X,
    const int incX, const double *Y, const int incY, double *A, const int lda);

#endif
