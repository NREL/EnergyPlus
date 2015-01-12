/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
 * Contributor(s) : R. Clint Whaley
 * Originally developed at the University of Tennessee,
 * Innovative Computing Laboratory, Knoxville TN, 37996-1301, USA.
 *
 * ---------------------------------------------------------------------
 *
 * -- Copyright notice and Licensing terms:
 *
 *  Redistribution  and  use in  source and binary forms, with or without
 *  modification, are  permitted provided  that the following  conditions
 *  are met:
 *
 * 1. Redistributions  of  source  code  must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce  the above copyright
 *    notice,  this list of conditions, and the  following disclaimer in
 *    the documentation and/or other materials provided with the distri-
 *    bution.
 * 3. The name of the University,  the ATLAS group,  or the names of its
 *    contributors  may not be used to endorse or promote products deri-
 *    ved from this software without specific written permission.
 *
 * -- Disclaimer:
 *
 * THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
 * RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
 * CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ---------------------------------------------------------------------
 */
#ifndef ATLAS_KERNEL2_H
#define ATLAS_KERNEL2_H
/*
 * =====================================================================
 * Macro function definitions
 * =====================================================================
 */
#define    ATL_GetPartSBMV    ATL_GetPartSYMV
#define    ATL_GetPartSPMV    ATL_GetPartSYMV
#define    ATL_GetPartP1      ATL_GetPartR1

#define MLpprev( n_, a_, lda_ ) \
 { a_ -= ( (((n_) * (lda_)) + (((n_)*((n_)+1)) >> 1)) SHIFT ); lda_ += (n_); }
#define MUpprev( n_, a_, lda_ ) \
 { a_ -= ( (((n_) * (lda_)) - (((n_)*((n_)-1)) >> 1)) SHIFT ); lda_ -= (n_); }
#define MLpnext( n_, a_, lda_ ) \
 { a_ += ( (((n_) * (lda_)) - (((n_)*((n_)-1)) >> 1)) SHIFT ); lda_ -= (n_); }
#define MUpnext( n_, a_, lda_ ) \
 { a_ += ( (((n_) * (lda_)) + (((n_)*((n_)+1)) >> 1)) SHIFT ); lda_ += (n_); }

#define MLrprev( n_, a_, lda_ ) \
 { a_ -= ( ((n_) * ((lda_)+1)) SHIFT ); }
#define MUrprev( n_, a_, lda_ ) \
 { a_ -= ( ((n_) * ((lda_)+1)) SHIFT ); }
#define MLrnext( n_, a_, lda_ ) \
 { a_ += ( ((n_) * ((lda_)+1)) SHIFT ); }
#define MUrnext( n_, a_, lda_ ) \
 { a_ += ( ((n_) * ((lda_)+1)) SHIFT ); }
/*
 * =====================================================================
 * Recursive Level 2 BLAS function prototypes
 * =====================================================================
 */
void       ATL_strsvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_strsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvLTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvLNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvLTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvLNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvUTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvUNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvUTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvUNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbsvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_strmvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_strmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stpmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvLTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvLNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvLTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvLNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvUTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvUNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvUTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvUNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_stbmvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ssyr2U
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_ssyr2L
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_sspr2U
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_sspr2L
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_ssyrU
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_ssyrL
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_ssprU
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_ssprL
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_ssymvU
(
  const int,
  const float *,          const int,
  const float *,
  const float,
  float *
);

void       ATL_ssymvL
(
  const int,
  const float *,          const int,
  const float *,
  const float,
  float *
);

void       ATL_sspmvU
(
  const int,
  const float *,          const int,
  const float *,
  const float,
  float *
);

void       ATL_sspmvL
(
  const int,
  const float *,          const int,
  const float *,
  const float,
  float *
);

void       ATL_ssbmvU
(
  const int,              const int,
  const float *,          const int,
  const float *,
  const float,
  float *
);

void       ATL_ssbmvL
(
  const int,              const int,
  const float *,          const int,
  const float *,
  const float,
  float *
);

void       ATL_sgpmv
(
  const enum ATLAS_UPLO,  const enum ATLAS_TRANS,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgprU
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_sgprL
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_sgpr
(
  const enum ATLAS_UPLO,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_sgpr1U_a1_x1_yX
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_sgpr1L_a1_x1_yX
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_sgpmvUT_a1_x1_bX_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvUN_a1_x1_bX_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvUT_a1_x1_b1_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvUN_a1_x1_b1_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvUT_a1_x1_b0_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvUN_a1_x1_b0_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvLT_a1_x1_bX_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvLN_a1_x1_bX_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvLT_a1_x1_b1_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvLN_a1_x1_b1_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvLT_a1_x1_b0_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgpmvLN_a1_x1_b0_y1
(
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgbmvT_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgbmvN_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgbmvT_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgbmvN_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgbmvT_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_sgbmvN_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const float,
  const float *,          const int,
  const float *,          const int,
  const float,
  float *,                const int
);

void       ATL_dtrsvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvLTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvLNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvLTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvLNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvUTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvUNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvUTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvUNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbsvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtrmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtpmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvLTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvLNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvLTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvLNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvUTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvUNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvUTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvUNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dtbmvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_dsyr2U
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_dsyr2L
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_dspr2U
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_dspr2L
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_dsyrU
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_dsyrL
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_dsprU
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_dsprL
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_dsymvU
(
  const int,
  const double *,         const int,
  const double *,
  const double,
  double *
);

void       ATL_dsymvL
(
  const int,
  const double *,         const int,
  const double *,
  const double,
  double *
);

void       ATL_dspmvU
(
  const int,
  const double *,         const int,
  const double *,
  const double,
  double *
);

void       ATL_dspmvL
(
  const int,
  const double *,         const int,
  const double *,
  const double,
  double *
);

void       ATL_dsbmvU
(
  const int,              const int,
  const double *,         const int,
  const double *,
  const double,
  double *
);

void       ATL_dsbmvL
(
  const int,              const int,
  const double *,         const int,
  const double *,
  const double,
  double *
);

void       ATL_dgpmv
(
  const enum ATLAS_UPLO,  const enum ATLAS_TRANS,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgprU
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_dgprL
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_dgpr
(
  const enum ATLAS_UPLO,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_dgpr1U_a1_x1_yX
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_dgpr1L_a1_x1_yX
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_dgpmvUT_a1_x1_bX_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvUN_a1_x1_bX_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvUT_a1_x1_b1_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvUN_a1_x1_b1_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvUT_a1_x1_b0_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvUN_a1_x1_b0_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvLT_a1_x1_bX_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvLN_a1_x1_bX_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvLT_a1_x1_b1_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvLN_a1_x1_b1_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvLT_a1_x1_b0_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgpmvLN_a1_x1_b0_y1
(
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgbmvT_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgbmvN_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgbmvT_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgbmvN_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgbmvT_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_dgbmvN_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const double,
  const double *,         const int,
  const double *,         const int,
  const double,
  double *,               const int
);

void       ATL_ctrsvLHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLHU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLCU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLHN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLCN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUHU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUCU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUHN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUCN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbsvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctrmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUHU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUCU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUTU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUNU
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUHN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUCN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUTN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUNN
(
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUH
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUC
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctpmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLHU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLCU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLHN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLCN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUHU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUCU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUTU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUNU
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUHN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUCN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUTN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUNN
(
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_ctbmvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const float *,          const int,
  float *
);

void       ATL_cher2U
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_cher2L
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_chpr2U
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_chpr2L
(
  const int,
  const float *,
  const float *,
  float *,                const int
);

void       ATL_cherU
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_cherL
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_chprU
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_chprL
(
  const int,
  const float *,
  const float *,          const int,
  float *,                const int
);

void       ATL_chemvU
(
  const int,
  const float *,          const int,
  const float *,
  const float *,
  float *
);

void       ATL_chemvL
(
  const int,
  const float *,          const int,
  const float *,
  const float *,
  float *
);

void       ATL_chpmvU
(
  const int,
  const float *,          const int,
  const float *,
  const float *,
  float *
);

void       ATL_chpmvL
(
  const int,
  const float *,          const int,
  const float *,
  const float *,
  float *
);

void       ATL_chbmvU
(
  const int,              const int,
  const float *,          const int,
  const float *,
  const float *,
  float *
);

void       ATL_chbmvL
(
  const int,              const int,
  const float *,          const int,
  const float *,
  const float *,
  float *
);

void       ATL_cgpmv
(
  const enum ATLAS_UPLO,  const enum ATLAS_TRANS,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpruU
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpruL
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpru
(
  const enum ATLAS_UPLO,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgprcU
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgprcL
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgprc
(
  const enum ATLAS_UPLO,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpr1uU_a1_x1_yX
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpr1uL_a1_x1_yX
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpr1cU_a1_x1_yX
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpr1cL_a1_x1_yX
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  float *,                const int
);

void       ATL_cgpmvUNc_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUC_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUT_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUN_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUNc_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUC_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUT_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUN_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUNc_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUC_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUT_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUN_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUNc_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUC_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUT_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvUN_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLNc_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLC_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLT_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLN_a1_x1_bX_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLNc_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLC_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLT_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLN_a1_x1_b1_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLNc_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLC_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLT_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLN_a1_x1_bXi0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLNc_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLC_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLT_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgpmvLN_a1_x1_b0_y1
(
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvNc_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvC_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvT_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvN_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvNc_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvC_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvT_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvN_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvNc_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvC_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvT_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvN_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvNc_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvC_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvT_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_cgbmvN_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const float *,
  const float *,          const int,
  const float *,          const int,
  const float *,
  float *,                const int
);

void       ATL_ztrsvLHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpsvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLHU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLCU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLHN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLCN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUHU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUCU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUHN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUCN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbsvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztrmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUHU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUCU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUTU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUNU
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUHN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUCN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUTN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUNN
(
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvLN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUH
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUC
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUT
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztpmvUN
(
  const enum ATLAS_DIAG,
  const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLHU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLCU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLHN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLCN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUHU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUCU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUTU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUNU
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUHN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUCN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUTN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUNN
(
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvLN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUH
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUC
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUT
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_ztbmvUN
(
  const enum ATLAS_DIAG,
  const int,              const int,
  const double *,         const int,
  double *
);

void       ATL_zher2U
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_zher2L
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_zhpr2U
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_zhpr2L
(
  const int,
  const double *,
  const double *,
  double *,               const int
);

void       ATL_zherU
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_zherL
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_zhprU
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_zhprL
(
  const int,
  const double *,
  const double *,         const int,
  double *,               const int
);

void       ATL_zhemvU
(
  const int,
  const double *,         const int,
  const double *,
  const double *,
  double *
);

void       ATL_zhemvL
(
  const int,
  const double *,         const int,
  const double *,
  const double *,
  double *
);

void       ATL_zhpmvU
(
  const int,
  const double *,         const int,
  const double *,
  const double *,
  double *
);

void       ATL_zhpmvL
(
  const int,
  const double *,         const int,
  const double *,
  const double *,
  double *
);

void       ATL_zhbmvU
(
  const int,              const int,
  const double *,         const int,
  const double *,
  const double *,
  double *
);

void       ATL_zhbmvL
(
  const int,              const int,
  const double *,         const int,
  const double *,
  const double *,
  double *
);

void       ATL_zgpmv
(
  const enum ATLAS_UPLO,  const enum ATLAS_TRANS,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpruU
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpruL
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpru
(
  const enum ATLAS_UPLO,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgprcU
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgprcL
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgprc
(
  const enum ATLAS_UPLO,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpr1uU_a1_x1_yX
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpr1uL_a1_x1_yX
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpr1cU_a1_x1_yX
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpr1cL_a1_x1_yX
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  double *,               const int
);

void       ATL_zgpmvUNc_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUC_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUT_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUN_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUNc_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUC_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUT_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUN_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUNc_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUC_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUT_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUN_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUNc_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUC_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUT_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvUN_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLNc_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLC_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLT_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLN_a1_x1_bX_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLNc_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLC_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLT_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLN_a1_x1_b1_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLNc_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLC_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLT_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLN_a1_x1_bXi0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLNc_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLC_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLT_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgpmvLN_a1_x1_b0_y1
(
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvNc_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvC_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvT_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvN_a1_x1_bX_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvNc_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvC_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvT_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvN_a1_x1_b1_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvNc_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvC_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvT_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvN_a1_x1_bXi0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvNc_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvC_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvT_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);

void       ATL_zgbmvN_a1_x1_b0_y1
(
  const int,              const int,
  const int,              const int,
  const double *,
  const double *,         const int,
  const double *,         const int,
  const double *,
  double *,               const int
);


#endif
/*
 * End of atlas_kernel2.h
 */
