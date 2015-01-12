
/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
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
#ifndef ATLAS_PTLEVEL3_H
#define ATLAS_PTLEVEL3_H
/*
 * =====================================================================
 * Include files
 * =====================================================================
 */
#include "atlas_enum.h"
#include "atlas_pthreads.h"
/*
 * =====================================================================
 * Prototypes  for single precision real  Level 3  multi-threaded  ATLAS
 * BLAS routines.
 * =====================================================================
 */
void              ATL_sptgeadd
(  const int,       const int,       const float,     const float *,
   const int,       const float,     float *,         const int );
void              ATL_sptgezero
(  const int,       const int,       float *,         const int );
void              ATL_sptgescal
(  const int,       const int,       const float,     float *,
   const int );
void              ATL_spttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const float,     float *,         const int );

void              ATL_sptgemm
(  const enum ATLAS_TRANS,           const enum ATLAS_TRANS,
   const int,       const int,       const int,       const float,
   const float *,   const int,       const float *,   const int,
   const float,     float *,         const int );
void              ATL_sptsymm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const float,     const float *,
   const int,       const float *,   const int,       const float,
   float *,         const int );
void              ATL_sptsyrk
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const float,     const float *,
   const int,       const float,     float *,         const int );
void              ATL_sptsyr2k
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const float,     const float *,
   const int,       const float *,   const int,       const float,
   float *,         const int );
void              ATL_spttrmm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const float,     const float *,
   const int,       float *,         const int );
void              ATL_spttrsm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const float,     const float *,
   const int,       float *,         const int );
/*
 * =====================================================================
 * Prototypes  for double precision real  Level 3  multi-threaded  ATLAS
 * BLAS routines.
 * =====================================================================
 */
void              ATL_dptgeadd
(  const int,       const int,       const double,    const double *,
   const int,       const double,    double *,        const int );
void              ATL_dptgezero
(  const int,       const int,       double *,        const int );
void              ATL_dptgescal
(  const int,       const int,       const double,    double *,
   const int );
void              ATL_dpttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const double,    double *,        const int );

void              ATL_dptgemm
(  const enum ATLAS_TRANS,           const enum ATLAS_TRANS,
   const int,       const int,       const int,       const double,
   const double *,  const int,       const double *,  const int,
   const double,    double *,        const int );
void              ATL_dptsymm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const double,    const double *,
   const int,       const double *,  const int,       const double,
   double *,        const int );
void              ATL_dptsyrk
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const double,    const double *,
   const int,       const double,    double *,        const int );
void              ATL_dptsyr2k
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const double,    const double *,
   const int,       const double *,  const int,       const double,
   double *,        const int );
void              ATL_dpttrmm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const double,    const double *,
   const int,       double *,        const int );
void              ATL_dpttrsm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const double,    const double *,
   const int,       double *,        const int );
/*
 * =====================================================================
 * Prototypes  for single precision complex Level 3 multi-threaded ATLAS
 * BLAS routines.
 * =====================================================================
 */
void              ATL_cptgeadd
(  const int,       const int,       const float *,   const float *,
   const int,       const float *,   float *,         const int );
void              ATL_cptgezero
(  const int,       const int,       float *,         const int );
void              ATL_cptgescal
(  const int,       const int,       const float *,   float *,
   const int );
void              ATL_cpttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const float *,   float *,         const int );
void              ATL_cpthescal
(  const enum ATLAS_UPLO,            const int,       const int,
   const float,     float *,         const int );

void              ATL_cptgemm
(  const enum ATLAS_TRANS,           const enum ATLAS_TRANS,
   const int,       const int,       const int,       const float *,
   const float *,   const int,       const float *,   const int,
   const float *,   float *,         const int );
void              ATL_cptsymm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const float *,   const float *,
   const int,       const float *,   const int,       const float *,
   float *,         const int );
void              ATL_cptsyrk
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const float *,   const float *,
   const int,       const float *,   float *,         const int );
void              ATL_cptsyr2k
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const float *,   const float *,
   const int,       const float *,   const int,       const float *,
   float *,         const int );
void              ATL_cpttrmm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const float *,   const float *,
   const int,       float *,         const int );
void              ATL_cpttrsm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const float *,   const float *,
   const int,       float *,         const int );
/*
 * =====================================================================
 * Prototypes  for double precision complex Level 3 multi-threaded ATLAS
 * BLAS routines.
 * =====================================================================
 */
void              ATL_zptgeadd
(  const int,       const int,       const double *,  const double *,
   const int,       const double *,  double *,        const int );
void              ATL_zptgezero
(  const int,       const int,       double *,        const int );
void              ATL_zptgescal
(  const int,       const int,       const double *,  double *,
   const int );
void              ATL_zpttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const double *,  double *,        const int );
void              ATL_zpthescal
(  const enum ATLAS_UPLO,            const int,       const int,
   const double,    double *,        const int );

void              ATL_zptgemm
(  const enum ATLAS_TRANS,           const enum ATLAS_TRANS,
   const int,       const int,       const int,       const double *,
   const double *,  const int,       const double *,  const int,
   const double *,  double *,        const int );
void              ATL_zptsymm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const double *,  const double *,
   const int,       const double *,  const int,       const double *,
   double *,        const int );
void              ATL_zptsyrk
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const double *,  const double *,
   const int,       const double *,  double *,        const int );
void              ATL_zptsyr2k
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const double *,  const double *,
   const int,       const double *,  const int,       const double *,
   double *,        const int );
void              ATL_zpttrmm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const double *,  const double *,
   const int,       double *,        const int );
void              ATL_zpttrsm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const double *,  const double *,
   const int,       double *,        const int );

void              ATL_cpthemm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const float *,   const float *,
   const int,       const float *,   const int,       const float *,
   float *,         const int );
void              ATL_cptherk
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const float,     const float *,
   const int,       const float,     float *,         const int );
void              ATL_cpther2k
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const float *,   const float *,
   const int,       const float *,   const int,       const float,
   float *,         const int );

void              ATL_zpthemm
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const double *,  const double *,
   const int,       const double *,  const int,       const double *,
   double *,        const int );
void              ATL_zptherk
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const double,    const double *,
   const int,       const double,    double *,        const int );
void              ATL_zpther2k
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const double *,  const double *,
   const int,       const double *,  const int,       const double,
   double *,        const int );

#endif
/*
 * End of atlas_ptlevel3.h
 */
