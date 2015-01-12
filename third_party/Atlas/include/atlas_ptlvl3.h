
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
#ifndef ATLAS_PTLVL3_H
#define ATLAS_PTLVL3_H
/*
 * =====================================================================
 * Include files
 * =====================================================================
 */
#include "atlas_ptmisc.h"
#include "atlas_level3.h"
#include "atlas_rblas3.h"
/*
 * =====================================================================
 * macro constants
 * =====================================================================
 */
#ifdef TREAL
#define   ATL_XOVER_L3_DEFAULT      8     /* number of NB x NB blocks */
#else
#define   ATL_XOVER_L3_DEFAULT      4
#endif
/*
 * =====================================================================
 * macro functions
 * =====================================================================
 */
#define    Mpt3(  a_, i_, siz_ ) ( ( (char*)(a_) + ( (i_) * (siz_) ) ) )
#define    Mvpt3( a_, i_, siz_ ) ( (void *)(Mpt3( (a_), (i_), (siz_) )))
/*
 * =====================================================================
 * typedef definitions
 * =====================================================================
 */
typedef           PT_TREE_T              (*PT_GEMM_FUN_T)
(
   const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_TRANS,               const enum ATLAS_TRANS,
   const int,         const int,         const int,         const void *,
   const void *,      const int,         const void *,      const int,
   const void *,      void *,            const int
);

typedef           PT_TREE_T              (*PT_TRMM_FUN_T)
(
   const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_SIDE,                const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,               const enum ATLAS_DIAG,
   const int,         const int,         const void *,      const void *,
   const int,         void *,            const int
);

typedef           int                    (*PT_SYR2K_FUN_T)
(
   const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const enum ATLAS_TRANS,
   const enum ATLAS_TRANS,               const int,       const int,
   const void *,    const void *,        const int,       const void *,
   const int,       const void *,        void *,          const int
);


typedef struct
{
   size_t                     size;
   void                       * negone, * one, * zero;
   PT_FUN_T                   geadd0, gemm0,  symm0, hemm0, syrk0, syr2k0,
                              herk0,  her2k0, trmm0, trsm0;
   PT_GEMM_FUN_T              ptgemm;
   PT_TRMM_FUN_T              pttrmm;
   PT_SYR2K_FUN_T             ptsyr2k0, pther2k0;
} PT_LVL3_TYPE_T;

typedef struct
{
   const void                 * a, * al, * b, * be;
   void                       * c;
   enum ATLAS_TRANS           ta, tb;
   int                        k, la, lb, lc, m, n;
} PT_GEMM_ARGS_T;

typedef struct
{
   const void                 * a, * al, * b, * be;
   void                       * c;
   enum ATLAS_SIDE            si;
   enum ATLAS_UPLO            up;
   int                        la, lb, lc, m, n;
} PT_SYMM_ARGS_T;

typedef struct
{
   const void                 * a, * al, * be;
   void                       * c;
   enum ATLAS_UPLO            up;
   enum ATLAS_TRANS           tr;
   int                        l, la, lc, m, n, k;
} PT_SYRK_ARGS_T;

typedef struct
{
   const void                 * a, * al, * ac, * b, * be;
   void                       * c;
   enum ATLAS_UPLO            up;
   enum ATLAS_TRANS           tr;
   int                        l, la, lb, lc, m, n, k;
} PT_SYR2K_ARGS_T;

typedef struct
{
   const void                 * a, * al;
   void                       * b;
   enum ATLAS_SIDE            si;
   enum ATLAS_UPLO            up;
   enum ATLAS_TRANS           tr;
   enum ATLAS_DIAG            di;
   int                        la, lb, m, n;
} PT_TRMM_ARGS_T;

typedef struct
{
   const void                 * a, * al;
   void                       * b;
   enum ATLAS_SIDE            si;
   enum ATLAS_UPLO            up;
   enum ATLAS_TRANS           tr;
   enum ATLAS_DIAG            di;
   int                        la, lb, m, n;
} PT_TRSM_ARGS_T;

/*
 * =====================================================================
 * Function prototypes
 * =====================================================================
 */
PT_TREE_T         ATL_Sgemm
(  const PT_LVL3_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_TRANS,               const enum ATLAS_TRANS,
   const int,         const int,         const int,         const void *,
   const void *,      const int,         const void *,      const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_Ssymm
(  const PT_LVL3_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_TRANS,               const enum ATLAS_SIDE,
   const enum ATLAS_UPLO,                const int,         const int,
   const void *,      const void *,      const int,         const void *,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_Ssyrk
(  const PT_LVL3_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_UPLO,                const enum ATLAS_TRANS,
   const enum ATLAS_TRANS,               const int,         const int,
   const int,         const int,         const void *,      const void *,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_Ssyr2k
(  const PT_LVL3_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_UPLO,                const enum ATLAS_TRANS,
   const enum ATLAS_TRANS,               const int,         const int,
   const int,         const int,         const void *,      const void *,
   const void *,      const int,         const void *,      const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_Strmm
(  const PT_LVL3_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_SIDE,                const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,               const enum ATLAS_DIAG,
   const int,         const int,         const void *,      const void *,
   const int,         void *,            const int );
PT_TREE_T         ATL_Strsm
(  const PT_LVL3_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_SIDE,                const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,               const enum ATLAS_DIAG,
   const int,         const int,         const void *,      const void *,
   const int,         void *,            const int );

#if defined( TREAL ) || defined( TCPLX )

int               Mjoin( PATL, GetNB     )          ( void );

void              Mjoin( PATL, ptl3settype )        ( PT_LVL3_TYPE_T * );

void              Mjoin( PATL, gemmNN )
(  const int,       const int,       const int,       const SCALAR,
   const TYPE *,    const int,       const TYPE *,    const int,
   const SCALAR,    TYPE *,          const int );
void              Mjoin( PATL, gemmNT )
(  const int,       const int,       const int,       const SCALAR,
   const TYPE *,    const int,       const TYPE *,    const int,
   const SCALAR,    TYPE *,          const int );
void              Mjoin( PATL, gemmTN )
(  const int,       const int,       const int,       const SCALAR,
   const TYPE *,    const int,       const TYPE *,    const int,
   const SCALAR,    TYPE *,          const int );

#if defined( TCPLX )
void              Mjoin( PATL, gemmNC )
(  const int,       const int,       const int,       const SCALAR,
   const TYPE *,    const int,       const TYPE *,    const int,
   const SCALAR,    TYPE *,          const int );
void              Mjoin( PATL, gemmCN )
(  const int,       const int,       const int,       const SCALAR,
   const TYPE *,    const int,       const TYPE *,    const int,
   const SCALAR,    TYPE *,          const int );
#endif

PT_FUN_ARG_T      Mjoin( PATL, ptgemm0   )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, ptsymm0   )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, ptsyr2k0  )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, ptsyrk0   )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, pttrmm0   )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, pttrsm0   )          ( PT_FUN_ARG_T );

#if defined( TCPLX )
PT_FUN_ARG_T      Mjoin( PATL, pthemm0   )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, pther2k0  )          ( PT_FUN_ARG_T );
PT_FUN_ARG_T      Mjoin( PATL, ptherk0   )          ( PT_FUN_ARG_T );
#endif
/*
 * =====================================================================
 * Prototypes for the Level 3 multi-threaded ATLAS BLAS routines
 * =====================================================================
 */
PT_TREE_T         Mjoin( PATL, ptgemm_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_TRANS,           const enum ATLAS_TRANS,
   const int,       const int,       const int,       const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
PT_TREE_T         Mjoin( PATL, ptsymm_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const void *,    const void *,
   const int,       const void *,    const int,       const void *,
   void *,          const int );
PT_TREE_T         Mjoin( PATL, ptsyr2k_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const void *,    const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
int               Mjoin( PATL, ptsyr2k0_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const enum ATLAS_TRANS,           const int,       const int,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int );
PT_TREE_T         Mjoin( PATL, ptsyrk_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const void *,    const void *,
   const int,       const void *,    void *,          const int );
PT_TREE_T         Mjoin( PATL, pttrmm_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const void *,    const void *,
   const int,       void *,          const int );
PT_TREE_T         Mjoin( PATL, pttrsm_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const void *,    const void *,
   const int,       void *,          const int );

void              Mjoin( PATL, ptgemm )
(  const enum ATLAS_TRANS,           const enum ATLAS_TRANS,
   const int,       const int,       const int,       const SCALAR,
   const TYPE *,    const int,       const TYPE *,    const int,
   const SCALAR,    TYPE *,          const int );
void              Mjoin( PATL, ptsymm )
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       const TYPE *,    const int,       const SCALAR,
   TYPE *,          const int );
void              Mjoin( PATL, ptsyr2k )
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       const TYPE *,    const int,       const SCALAR,
   TYPE *,          const int );
void              Mjoin( PATL, ptsyrk )
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       const SCALAR,    TYPE *,          const int );
void              Mjoin( PATL, pttrmm )
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       TYPE *,          const int );
void              Mjoin( PATL, pttrsm )
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,           const enum ATLAS_DIAG,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       TYPE *,          const int );

#if defined( TCPLX )
PT_TREE_T         Mjoin( PATL, pthemm_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const void *,    const void *,
   const int,       const void *,    const int,       const void *,
   void *,          const int );
PT_TREE_T         Mjoin( PATL, pther2k_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const void *,    const void *,
   const void *,    const int,       const void *,    const int,
   const void *,    void *,          const int );
int               Mjoin( PATL, pther2k0_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const enum ATLAS_TRANS,           const int,       const int,
   const void *,    const void *,    const int,       const void *,
   const int,       const void *,    void *,          const int );
PT_TREE_T         Mjoin( PATL, ptherk_nt )
(  const unsigned int,               pthread_attr_t *,
   const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const void *,    const void *,
   const int,       const void *,    void *,          const int );

void              Mjoin( PATL, pthemm )
(  const enum ATLAS_SIDE,            const enum ATLAS_UPLO,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       const TYPE *,    const int,       const SCALAR,
   TYPE *,          const int );
void              Mjoin( PATL, pther2k )
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const SCALAR,    const TYPE *,
   const int,       const TYPE *,    const int,       const TYPE,
   TYPE *,          const int );
void              Mjoin( PATL, ptherk )
(  const enum ATLAS_UPLO,            const enum ATLAS_TRANS,
   const int,       const int,       const TYPE,      const TYPE *,
   const int,       const TYPE,      TYPE *,          const int );
#endif

#endif

#endif
/*
 * End of atlas_ptlvl3.h
 */
