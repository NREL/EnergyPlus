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
#ifndef ATLAS_RBLAS3_H
#define ATLAS_RBLAS3_H
/*
 * =====================================================================
 * Include files
 * =====================================================================
 */
#include "atlas_misc.h"
/*
 * =====================================================================
 * #define macros definitions
 * =====================================================================
 */
#define    Mrc3( a_, i_, j_, lda_, siz_ ) \
           ( (void*) ( (char*)(a_) + ( ( (i_)+(j_)*((size_t)lda_) )*(siz_) ) ) )
/*
 * =====================================================================
 * #typedef definitions
 * =====================================================================
 */
typedef void           (*KR3_FUN_GEMM_T)
( const int,       const int,       const int,       const void *,
  const void *,    const int,       const void *,    const int,
  const void *,    void *,          const int );
typedef void           (*KR3_FUN_HEMM_T)
( const int,       const int,       const void *,    const void *,
  const int,       const void *,    const int,       const void *,
  void *,          const int );
typedef int            (*KR3_FUN_HER2K_T)
( const int,       const int,       const void *,    const void *,
  const int,       const void *,    const int,       const void *,
  void *,          const int );
typedef void           (*KR3_FUN_HERK_T)
( const int,       const int,       const void *,    const void *,
  const int,       const void *,    void *,          const int );
typedef void           (*KR3_FUN_SYMM_T)
( const int,       const int,       const void *,    const void *,
  const int,       const void *,    const int,       const void *,
  void *,          const int );
typedef int            (*KR3_FUN_SYR2K_T)
( const int,       const int,       const void *,    const void *,
  const int,       const void *,    const int,       const void *,
  void *,          const int );
typedef void           (*KR3_FUN_SYRK_T)
( const int,       const int,       const void *,    const void *,
  const int,       const void *,    void *,          const int );
typedef void           (*KR3_FUN_TRMM_T)
( const int,       const int,       const void *,    const void *,
  const int,       void *,          const int );
typedef void           (*KR3_FUN_TRSM_T)
( const int,       const int,       const void *,    const void *,
  const int,       void *,          const int );

typedef struct
{
   size_t             size;
   void               * one;
   KR3_FUN_GEMM_T     TgemmNN;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_SYMM_T     Tsymm;
} RC3_SYMM_T;

typedef struct
{
   size_t             size;
   void               * one;
   KR3_FUN_GEMM_T     TgemmNN;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_HEMM_T     Themm;
} RC3_HEMM_T;

typedef struct
{
   size_t             size;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_SYRK_T     Tsyrk;
} RC3_SYRK_T;

typedef struct
{
   size_t             size;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_HERK_T     Therk;
} RC3_HERK_T;

typedef struct
{
   size_t             size;
   void               * one;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_SYR2K_T    Tsyr2k;
} RC3_SYR2K_T;

typedef struct
{
   size_t             size;
   void               * one;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_HER2K_T    Ther2k;
} RC3_HER2K_T;

typedef struct
{
   size_t             size;
   void               * one;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_TRMM_T     Ttrmm;
} RC3_TRMM_T;

typedef struct
{
   size_t             size;
   void               * one, * negone;
   KR3_FUN_GEMM_T     Tgemm;
   KR3_FUN_TRSM_T     Ttrsm;
} RC3_TRSM_T;

typedef void           (*RC3_FUN_HEMM_T)
(  RC3_HEMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
typedef void           (*RC3_FUN_HER2K_T)
(  RC3_HER2K_T *,   const int,       const int,       const void *,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int,
   const int );
typedef void           (*RC3_FUN_HERK_T)
(  RC3_HERK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
typedef void           (*RC3_FUN_SYMM_T)
(  RC3_SYMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
typedef void           (*RC3_FUN_SYR2K_T)
(  RC3_SYR2K_T *,   const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
typedef void           (*RC3_FUN_SYRK_T)
(  RC3_SYRK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
typedef void           (*RC3_FUN_TRMM_T)
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
typedef void           (*RC3_FUN_TRSM_T)
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
/*
 * =====================================================================
 * Level 3 recursive BLAS internal function prototypes
 * =====================================================================
 */
void           ATL_sgemmTN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_sgemmNT_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_sgemmNN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_dgemmTN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_dgemmNT_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_dgemmNN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_cgemmCN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_cgemmNC_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_cgemmTN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_cgemmNT_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_cgemmNN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_zgemmCN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_zgemmNC_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_zgemmTN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_zgemmNT_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
void           ATL_zgemmNN_RB
(  const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
/*
 * =====================================================================
 * Recursive BLAS function prototypes
 * =====================================================================
 */
void           ATL_rsymmRU
(  RC3_SYMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rhemmRU
(  RC3_HEMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rsymmRL
(  RC3_SYMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rhemmRL
(  RC3_HEMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rsymmLU
(  RC3_SYMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rhemmLU
(  RC3_HEMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rsymmLL
(  RC3_SYMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rhemmLL
(  RC3_HEMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );

void           ATL_rsyrkUT
(  RC3_SYRK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rsyr2kUT
(  RC3_SYR2K_T *,   const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rsyrkUN
(  RC3_SYRK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rsyr2kUN
(  RC3_SYR2K_T *,   const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rsyrkLT
(  RC3_SYRK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rsyr2kLT
(  RC3_SYR2K_T *,   const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );
void           ATL_rsyrkLN
(  RC3_SYRK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rsyr2kLN
(  RC3_SYR2K_T *,   const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int,       const int );

void           ATL_rherkUC
(  RC3_HERK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rher2kUC
(  RC3_HER2K_T *,   const int,       const int,       const void *,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int,
   const int );
void           ATL_rherkUN
(  RC3_HERK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rher2kUN
(  RC3_HER2K_T *,   const int,       const int,       const void *,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int,
   const int );
void           ATL_rherkLC
(  RC3_HERK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rher2kLC
(  RC3_HER2K_T *,   const int,       const int,       const void *,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int,
   const int );
void           ATL_rherkLN
(  RC3_HERK_T *,    const int,       const int,       const void *,
   const void *,    const int,       const void *,    void *,
   const int,       const int );
void           ATL_rher2kLN
(  RC3_HER2K_T *,   const int,       const int,       const void *,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int,
   const int );

void           ATL_rtrmmRUC
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmRUC
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmRLC
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmRLC
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmRUT
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmRUT
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmRLT
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmRLT
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmRUN
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmRUN
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmRLN
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmRLN
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmLUC
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmLUC
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmLLC
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmLLC
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmLUT
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmLUT
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmLLT
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmLLT
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmLUN
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmLUN
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrmmLLN
(  RC3_TRMM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );
void           ATL_rtrsmLLN
(  RC3_TRSM_T *,    const int,       const int,       const void *,
   const void *,    const int,       void *,          const int,
   const int );

#endif
/*
 * End of atlas_rblas3.h
 */
