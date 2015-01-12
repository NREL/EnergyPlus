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
#include "atlas_kern3.h"

#ifdef Transpose_
   #define ATL_TRANS AtlasTrans
#elif defined(ConjTrans_)
   #define ATL_TRANS AtlasConjTrans
#else
   #define ATL_TRANS AtlasNoTrans
#endif
#ifdef UnitDiag_
   #define ATL_UNIT AtlasUnit
#else
   #define ATL_UNIT AtlasNonUnit
#endif
#ifdef Upper_
   #define ATL_UPLO AtlasUpper
   #ifdef Transpose_
      #ifdef UnitDiag_
         #define ATLP  UTU
         #define ATLPt LNU
         #define ATL_trcopy Mjoin(PATL,trcopyU2L_U)
         #define ATL_invert Mjoin(PATL,trinvertLU)
      #else
         #define ATLP  UTN
         #define ATLPt LNN
         #define ATL_trcopy Mjoin(PATL,trcopyU2L_N)
         #define ATL_invert Mjoin(PATL,trinvertLN)
      #endif
   #elif ConjTrans_
      #ifdef UnitDiag_
         #define ATLP  UCU
         #define ATLPt LNU
         #define ATL_trcopy Mjoin(PATL,trcopyU2Lc_U)
         #define ATL_invert Mjoin(PATL,trinvertLU)
      #else
         #define ATLP  UCN
         #define ATLPt LNN
         #define ATL_trcopy Mjoin(PATL,trcopyU2Lc_N)
         #define ATL_invert Mjoin(PATL,trinvertLN)
      #endif
   #else
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyU2U_U)
         #define ATL_invert Mjoin(PATL,trinvertUU)
         #define ATLP UNU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyU2U_N)
         #define ATL_invert Mjoin(PATL,trinvertUN)
         #define ATLP UNN
      #endif
   #endif
#else
   #define ATL_UPLO AtlasLower
   #ifdef Transpose_
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyL2U_U)
         #define ATL_invert Mjoin(PATL,trinvertUU)
         #define ATLP  LTU
         #define ATLPt UNU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyL2U_N)
         #define ATL_invert Mjoin(PATL,trinvertUN)
         #define ATLP  LTN
         #define ATLPt UNN
      #endif
   #elif defined(ConjTrans_)
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyL2Uc_U)
         #define ATL_invert Mjoin(PATL,trinvertUU)
         #define ATLP  LCU
         #define ATLPt UNU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyL2Uc_N)
         #define ATL_invert Mjoin(PATL,trinvertUN)
         #define ATLP  LCN
         #define ATLPt UNN
      #endif
   #else
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyL2L_U)
         #define ATL_invert Mjoin(PATL,trinvertLU)
         #define ATLP LNU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyL2L_N)
         #define ATL_invert Mjoin(PATL,trinvertLN)
         #define ATLP LNN
      #endif
   #endif
#endif

#ifdef TREAL
void Mjoin(ATL_trcopy,_a1)
   (const int N, const SCALAR alpha0, const TYPE *A, const int lda, TYPE *C);
#else
void ATL_trcopy (const int N, const TYPE *A, const int lda, TYPE *C);
#endif

void Mjoin(Mjoin(PATL,trsmR),ATLP)
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc)
{
   const TYPE *alpha=valpha;
#ifdef TREAL
   int ierr=1;
   int Mjoin(PATL,trsmKR_rk4)(enum ATLAS_SIDE Side, enum ATLAS_UPLO Uplo,
      enum ATLAS_TRANS TA, enum ATLAS_DIAG Diag, ATL_CINT M, ATL_CINT N,
      const SCALAR alpha, const TYPE *A, ATL_CINT lda, TYPE *B, ATL_CINT ldb);
   if (N >= 8 && M >= 8)
      ierr = Mjoin(PATL,trsmKR_rk4)(AtlasRight, ATL_UPLO, ATL_TRANS, ATL_UNIT,
                                    M, N, *alpha, A, lda, C, ldc);
   if (ierr)
      Mjoin(PATL,reftrsm)(AtlasRight, ATL_UPLO, ATL_TRANS, ATL_UNIT,
                          M, N, *alpha, A, lda, C, ldc);
#else
   int ierr=1;
   int Mjoin(PATL,trsmKR_rk2)(enum ATLAS_SIDE Side, enum ATLAS_UPLO Uplo,
      enum ATLAS_TRANS TA, enum ATLAS_DIAG Diag, ATL_CINT M, ATL_CINT N,
      const SCALAR alpha, const TYPE *A, ATL_CINT lda, TYPE *B, ATL_CINT ldb);
   if (N >= 8 && M >= 8)
      ierr = Mjoin(PATL,trsmKR_rk2)(AtlasRight, ATL_UPLO, ATL_TRANS, ATL_UNIT,
                                    M, N, alpha, A, lda, C, ldc);
   if (ierr)
      Mjoin(PATL,reftrsm)(AtlasRight, ATL_UPLO, ATL_TRANS, ATL_UNIT,
                          M, N, alpha, A, lda, C, ldc);
#endif
}
