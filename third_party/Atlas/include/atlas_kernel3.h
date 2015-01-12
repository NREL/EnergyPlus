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
#ifndef ATLAS_KERNEL3_H
#define ATLAS_KERNEL3_H

/*
 * Real level 3 kernels
 */
void ATL_ssymmRU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_ssymmLU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_ssymmRL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_ssymmLL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_strsmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strsmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_strmmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ssyrkLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_ssyrkUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_ssyrkLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_ssyrkUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
int ATL_ssyr2kLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_ssyr2kUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_ssyr2kLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_ssyr2kUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
void ATL_dsymmRU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_dsymmLU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_dsymmRL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_dsymmLL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_dtrsmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrsmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dtrmmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_dsyrkLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_dsyrkUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_dsyrkLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_dsyrkUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
int ATL_dsyr2kLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_dsyr2kUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_dsyr2kLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_dsyr2kUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);

/*
 * Complex level 3 kernels
 */
void ATL_chemmRU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_chemmLU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_chemmRL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_chemmLL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_csymmRU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_csymmLU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_csymmRL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_csymmLL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_ctrsmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmLUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmLUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrsmRUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ctrmmRUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_cherkLC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_cherkUC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_cherkLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_cherkUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_csyrkLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_csyrkUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_csyrkLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_csyrkUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
int ATL_cher2kLC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_cher2kUC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_cher2kLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_cher2kUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_csyr2kLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_csyr2kUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_csyr2kLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_csyr2kUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
void ATL_zhemmRU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zhemmLU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zhemmRL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zhemmLL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zsymmRU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zsymmLU
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zsymmRL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_zsymmLL
   (const int M, const int N, const void *alpha, const void *A, const int lda,
    const void *B, const int ldb, const void *beta, void *C, const int ldc);
void ATL_ztrsmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmLUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmLUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRLTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRLTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRLNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRLNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRLCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRLCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRUTN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRUTU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRUNN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRUNU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRUCN
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrsmRUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_ztrmmRUCU
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc);
void ATL_zherkLC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zherkUC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zherkLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zherkUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zsyrkLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zsyrkUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zsyrkLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
void ATL_zsyrkUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc);
int ATL_zher2kLC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zher2kUC
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zher2kLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zher2kUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zsyr2kLT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zsyr2kUT
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zsyr2kLN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);
int ATL_zsyr2kUN
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *B, const int ldb, const void *vbeta, void *C, const int ldc);

/*
 * Real level 3 kernel auxiliaries
 */
void ATL_ssycopyU_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_ssycopyL_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2L_N_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2L_U_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2U_N_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2U_U_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2L_N_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2L_U_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2U_N_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2U_U_a0
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_ssycopyU_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_ssycopyL_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2L_N_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2L_U_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2U_N_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2U_U_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2L_N_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2L_U_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2U_N_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2U_U_a1
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_ssycopyU_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_ssycopyL_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2L_N_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2L_U_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2U_N_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyU2U_U_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2L_N_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2L_U_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2U_N_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strcopyL2U_U_aX
   (const int N, const float alpha, const float *A, const int lda, float *C);
void ATL_strinvertUU(const int N, float *A, const int lda);
void ATL_strinvertLU(const int N, float *A, const int lda);
void ATL_strinvertUN(const int N, float *A, const int lda);
void ATL_strinvertLN(const int N, float *A, const int lda);
void ATL_ssyr2k_putU_bX
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_ssyr2k_putL_bX
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strputU_bX
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strputL_bX
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_ssyr2k_putU_b1
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_ssyr2k_putL_b1
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strputU_b1
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strputL_b1
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_ssyr2k_putU_b0
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_ssyr2k_putL_b0
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strputU_b0
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strputL_b0
   (const int N, const float *v, const float beta, float *A, const int lda);
void ATL_strsmKLLTN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLLTU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLLNN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLLNU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLUTN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLUTU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLUNN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKLUNU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRLTN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRLTU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRLNN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRLNU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRUTN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRUTU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRUNN
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_strsmKRUNU
   (const int M, const int N, const float alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_dsycopyU_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dsycopyL_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2L_N_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2L_U_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2U_N_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2U_U_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2L_N_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2L_U_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2U_N_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2U_U_a0
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dsycopyU_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dsycopyL_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2L_N_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2L_U_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2U_N_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2U_U_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2L_N_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2L_U_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2U_N_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2U_U_a1
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dsycopyU_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dsycopyL_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2L_N_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2L_U_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2U_N_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyU2U_U_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2L_N_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2L_U_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2U_N_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrcopyL2U_U_aX
   (const int N, const double alpha, const double *A, const int lda, double *C);
void ATL_dtrinvertUU(const int N, double *A, const int lda);
void ATL_dtrinvertLU(const int N, double *A, const int lda);
void ATL_dtrinvertUN(const int N, double *A, const int lda);
void ATL_dtrinvertLN(const int N, double *A, const int lda);
void ATL_dsyr2k_putU_bX
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dsyr2k_putL_bX
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrputU_bX
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrputL_bX
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dsyr2k_putU_b1
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dsyr2k_putL_b1
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrputU_b1
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrputL_b1
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dsyr2k_putU_b0
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dsyr2k_putL_b0
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrputU_b0
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrputL_b0
   (const int N, const double *v, const double beta, double *A, const int lda);
void ATL_dtrsmKLLTN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLLTU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLLNN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLLNU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLUTN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLUTU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLUNN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKLUNU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRLTN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRLTU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRLNN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRLNU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRUTN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRUTU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRUNN
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_dtrsmKRUNU
   (const int M, const int N, const double alpha, const double *A,
    const int lda, double *C, const int ldc);

/*
 * Complex level 3 kernel auxiliaries
 */
void ATL_cCtrsmKL
   (enum ATLAS_UPLO Uplo, enum ATLAS_TRANS Trans, enum ATLAS_DIAG Diag,
    const int M, const int N, const float *alpha, const float *A,
    const int lda, float *B, const int ldb);
void ATL_checopy
   (const int N, const float *A, const int lda, float *C);
void ATL_csycopy
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2L_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2Lc_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2L_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2Lc_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2U_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2Uc_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2U_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyU2Uc_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2L_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2Lc_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2L_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2Lc_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2U_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2Uc_N
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2U_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrcopyL2Uc_U
   (const int N, const float *A, const int lda, float *C);
void ATL_ctrmv_scalLNU_an1
   (const int N, const float *alpha, const float *A, const int lda, float *X);
void ATL_ctrmv_scalLNN_aX
   (const int N, const float *alpha, const float *A, const int lda, float *X);
void ATL_ctrmv_scalUNU_an1
   (const int N, const float *alpha, const float *A, const int lda, float *X);
void ATL_ctrmv_scalUNN_aX
   (const int N, const float *alpha, const float *A, const int lda, float *X);
void ATL_ctrinvertUU(const int N, float *A, const int lda);
void ATL_ctrinvertLU(const int N, float *A, const int lda);
void ATL_ctrinvertUN(const int N, float *A, const int lda);
void ATL_ctrinvertLN(const int N, float *A, const int lda);
void ATL_ctrputU_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputL_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putU_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putL_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputU_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputL_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putU_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putL_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputU_bX
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputL_bX
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putU_bX
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putL_bX
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputU_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputL_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putU_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putL_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputU_bn1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrputL_bn1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putU_bn1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_csyr2k_putL_bn1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cher2k_putU_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cher2k_putL_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cheputU_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cheputL_b0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cher2k_putU_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cher2k_putL_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cheputU_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cheputL_b1
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cher2k_putU_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cher2k_putL_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cheputU_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_cheputL_bXi0
   (const int N, const float *v, const float *beta, float *A, const int lda);
void ATL_ctrsm0LLTN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LLTU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LLNN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LLNU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LLCN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LLCU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LUTN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LUTU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LUNN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LUNU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LUCN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0LUCU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RLTN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RLTU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RLNN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RLNU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RLCN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RLCU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RUTN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RUTU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RUNN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RUNU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RUCN
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_ctrsm0RUCU
   (const int M, const int N, const float *alpha, const float *A,
    const int lda, float *C, const int ldc);
void ATL_zCtrsmKL
   (enum ATLAS_UPLO Uplo, enum ATLAS_TRANS Trans, enum ATLAS_DIAG Diag,
    const int M, const int N, const double *alpha, const double *A,
    const int lda, double *B, const int ldb);
void ATL_zhecopy
   (const int N, const double *A, const int lda, double *C);
void ATL_zsycopy
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2L_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2Lc_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2L_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2Lc_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2U_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2Uc_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2U_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyU2Uc_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2L_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2Lc_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2L_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2Lc_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2U_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2Uc_N
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2U_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrcopyL2Uc_U
   (const int N, const double *A, const int lda, double *C);
void ATL_ztrmv_scalLNU_an1
   (const int N, const double *alpha, const double *A, const int lda, double *X);
void ATL_ztrmv_scalLNN_aX
   (const int N, const double *alpha, const double *A, const int lda, double *X);
void ATL_ztrmv_scalUNU_an1
   (const int N, const double *alpha, const double *A, const int lda, double *X);
void ATL_ztrmv_scalUNN_aX
   (const int N, const double *alpha, const double *A, const int lda, double *X);
void ATL_ztrinvertUU(const int N, double *A, const int lda);
void ATL_ztrinvertLU(const int N, double *A, const int lda);
void ATL_ztrinvertUN(const int N, double *A, const int lda);
void ATL_ztrinvertLN(const int N, double *A, const int lda);
void ATL_ztrputU_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputL_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putU_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putL_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputU_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputL_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putU_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putL_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputU_bX
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputL_bX
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putU_bX
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putL_bX
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputU_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputL_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putU_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putL_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputU_bn1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrputL_bn1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putU_bn1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zsyr2k_putL_bn1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zher2k_putU_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zher2k_putL_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zheputU_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zheputL_b0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zher2k_putU_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zher2k_putL_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zheputU_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zheputL_b1
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zher2k_putU_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zher2k_putL_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zheputU_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_zheputL_bXi0
   (const int N, const double *v, const double *beta, double *A, const int lda);
void ATL_ztrsm0LLTN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LLTU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LLNN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LLNU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LLCN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LLCU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LUTN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LUTU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LUNN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LUNU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LUCN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0LUCU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RLTN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RLTU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RLNN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RLNU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RLCN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RLCU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RUTN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RUTU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RUNN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RUNU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RUCN
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);
void ATL_ztrsm0RUCU
   (const int M, const int N, const double *alpha, const double *A,
    const int lda, double *C, const int ldc);

#endif
