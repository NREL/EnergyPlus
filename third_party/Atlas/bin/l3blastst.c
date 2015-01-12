/*
 *             Automatically Tuned Linear Algebra Software v@(ver)
 *                      (C) Copyright 1998 Jeff Horner
 *
 * Code contributers : Jeff Horner, Antoine Petitet, R. Clint Whaley
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
/*
 * =====================================================================
 * Include files
 * =====================================================================
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#ifdef GCCWIN
   ___main(){} __main(){} MAIN__(){} _MAIN_(){}
   #ifndef isdigit
      #define isdigit(ch_) ( ((ch_)=='0')||((ch_)=='1')||((ch_)=='2')|| \
                             ((ch_)=='3')||((ch_)=='4')||((ch_)=='5')|| \
                             ((ch_)=='6')||((ch_)=='7')||((ch_)=='8')|| \
                             ((ch_)=='9') )
   #endif
#else
   #include <ctype.h>
#endif

#include "atlas_misc.h"
#include "atlas_tst.h"
/*
 * =====================================================================
 * #define macro constants
 * =====================================================================
 */
#define    MEGA                     1000000.0
#if defined( SREAL ) || defined( SCPLX )
   #define    THRESH                        50.0f
#else
   #define    THRESH                        50.0
#endif
static int NSAMP=1;

/* #define    ATLAS_DEBUG */
/*
 * =====================================================================
 * # macro functions
 * =====================================================================
 *
 * The following and mutually exclusive  macros  allow to select various
 * BLAS implementations to test the ATLAS implementation against:
 *
 *    USE_F77_BLAS     : Fortran 77 BLAS interface,
 *    USE_L3_REFERENCE : C ATLAS reference implementation,
 *
 * If none of these macros is defined at compile time, the  ATLAS imple-
 * mentation is to be tested against itself,  after all this is the only
 * version we are sure to have available.
 *
 * By default the mono-threaded  ATLAS  routines are tested. To test the
 * multi-threaded ATLAS routines, define the following macro:
 *    USE_L3_PTHREADS  : multi-threaded ATLAS implementation.
 */
#define USE_F77_BLAS

#ifdef ATL_USEPTHREADS
#define USE_L3_PTHREADS
#endif

#ifndef USE_L3_PTHREADS
#endif
/*
 * =====================================================================
 */
#if defined(USE_F77_BLAS) || defined(TEST_F77)
   #define  TP3 Mjoin(PATL,f77)
#elif defined( USE_L3_REFERENCE )
   #include "atlas_reflevel3.h"
   #define  TP3      Mjoin( PATL,   ref )
#else /* defined( USE_L3_ATLAS ) */  /* use ATLAS itself !! (default) */
   #include "atlas_level3.h"
   #define  TP3      PATL
#endif

#define trusted_gemm(TA, TB, M, N, K, al, A, lA, B, lB, be, C, lC) \
   Mjoin(TP3, gemm)(TA, TB, M, N, K, al, A, lA, B, lB, be, C, lC)
#define trusted_trsm( SI, UP, TA, DI, M, N, al, A, lA, B, lB) \
   Mjoin( TP3, trsm  )(  SI, UP, TA, DI, M, N,    al, A, lA, B, lB)
#define trusted_trmm(SI, UP, TA, DI, M, N, al, A, lA, B, lB) \
   Mjoin( TP3, trmm  )(SI, UP, TA, DI, M, N,  al, A, lA, B, lB)
#define trusted_syr2k(UP, TA, N, K, al, A, lA, B, lB, be, C, lC) \
   Mjoin( TP3, syr2k )(UP, TA, N, K, al, A, lA, B, lB, be, C, lC)
#define trusted_symm(SI, UP, M, N, al, A, lA, B, lB, be, C, lC) \
   Mjoin(TP3,symm)(SI, UP, M, N, al, A, lA, B, lB, be, C, lC)
#define trusted_syrk(UP, TA, N, K, al, A, lA, be, C, lC) \
   Mjoin(TP3, syrk)(UP, TA, N, K, al, A, lA, be, C, lC)
#ifdef TCPLX
   #define trusted_herk(UP, TA, N, K, al, A, lA, be, C, lC) \
      Mjoin(TP3, herk)(UP, TA, N, K, al, A, lA, be, C, lC)
   #define trusted_her2k(UP, TA, N, K, al, A, lA, B, lB, be, C, lC) \
      Mjoin(TP3,her2k)(UP, TA, N, K, al, A, lA, B, lB, be, C, lC)
   #define trusted_hemm( SI, UP, M, N, al, A, lA, B, lB, be, C, lC) \
      Mjoin(TP3,hemm)(SI, UP, M, N, al, A, lA, B, lB, be, C, lC)
#endif

/*
 * ATLAS version of the BLAS to test.
 */
/* #define PSPRK */
/* #define HPRK */
#if defined(PSPRK) || defined(HPRK)
#include "atlas_pkblas.h"
#endif
#if defined(TEST_F77)
   #define  AP3 Mjoin(PATL,f77)
   #define  AP4 Mjoin(PATL,f77)
#elif defined(USE_L3_PTHREADS)
   #include "atlas_pthreads.h"
   #include "atlas_ptlvl3.h"
   #include "atlas_tlvl3.h"
   #define  AP3  Mjoin(PATL, t)
   #define  AP4      Mjoin( PATL,   t  )
#else
   #include "atlas_level3.h"
   #define  AP3      PATL
   #define  AP4      PATL
#endif

#ifdef THREADS_AvC
   #define test_gemm(TA, TB, M, N, K, al, A, lA, B, lB, be, C, lC) \
      Mjoin(PATL,tgemm)(TA, TB, M, N, K, al, A, lA, B, lB, be, C, lC)
   #define test_trsm(SI, UP, TA, DI, M, N, al, A, lA, B, lB) \
      Mjoin(PATL,ttrsm)(SI, UP, TA, DI, M, N, al, A, lA, B, lB)
   #define test_trmm(SI, UP, TA, DI, M, N, al, A, lA, B, lB) \
      Mjoin(PATL,ttrmm)(SI, UP, TA, DI, M, N, al, A, lA, B, lB)
   #define test_syr2k(UP, TA, N, K, al, A, lA, B, lB, be, C, lC) \
      Mjoin(PATL,tsyr2k)(UP, TA, N, K, al, A, lA, B, lB, be, C, lC)
   #define test_symm(SI, UP, M, N, al, A, lA, B, lB, be, C, lC) \
      Mjoin(PATL,tsymm)(SI, UP, M, N, al, A, lA, B, lB, be, C, lC)
   #ifndef PSPRK
      #define test_syrk(UP, TA, N, K, al, A, lA, be, C, lC) \
         Mjoin(PATL,tsyrk)(UP, TA, N, K, al, A, lA, be, C, lC)
   #else
      #define test_syrk(UP, TA, N, K, al, A, lA, be, C, lC) \
         Mjoin(PATL,sprk)(PackGen, TA, UP, 0, N, K, al, A, 0, 0, lA, \
                          be, C, 0, 0, lC)
   #endif
   #ifdef TCPLX
      #define test_her2k(UP, TA, N, K, al, A, lA, B, lB, be, C, lC) \
         Mjoin(PATL, ther2k)(UP, TA, N, K, al, A, lA, B, lB, be, C, lC)
      #define test_hemm(SI, UP, M, N, al, A, lA, B, lB, be, C, lC) \
         Mjoin(PATL,themm)(SI, UP, M, N, al, A, lA, B, lB, be, C, lC)
      #ifndef HPRK
         #define test_herk(UP, TA, N, K, al, A, lA, be, C, lC) \
            Mjoin(PATL,therk)(UP, TA, N, K, al, A, lA, be, C, lC)
      #else
         #define test_herk(UP, TA, N, K, al, A, lA, be, C, lC) \
            Mjoin(PATL,hprk)(PackGen, TA, UP, 0, N, K, al, A, 0, 0, lA, \
                             be, C, 0, 0, lC)
      #endif
   #endif
#else
   #define test_gemm(TA, TB, M, N, K, al, A, lA, B, lB, be, C, lC) \
      Mjoin( AP4, gemm  )(TA, TB, M, N, K, al, A, lA, B, lB, be, C, lC)
   #define test_trsm(SI, UP, TA, DI, M, N, al, A, lA, B, lB) \
      Mjoin(AP4, trsm)(SI, UP, TA, DI, M, N, al, A, lA, B, lB)
   #define test_trmm(SI, UP, TA, DI, M, N, al, A, lA, B, lB) \
      Mjoin( AP4, trmm  )(SI, UP, TA, DI, M, N, al, A, lA, B, lB)
   #define test_syr2k(UP, TA, N, K, al, A, lA, B, lB, be, C, lC) \
      Mjoin( AP4, syr2k )(UP, TA, N, K, al, A, lA, B, lB, be, C, lC)
   #define test_symm(SI, UP, M, N, al, A, lA, B, lB, be, C, lC) \
      Mjoin(AP4,symm)(SI, UP, M, N, al, A, lA, B, lB, be, C, lC)
   #ifndef PSPRK
      #define test_syrk(UP, TA, N, K, al, A, lA, be, C, lC) \
         Mjoin(AP4,syrk)(UP, TA, N, K, al, A, lA, be, C, lC)
   #else
      #define test_syrk(UP, TA, N, K, al, A, lA, be, C, lC) \
         Mjoin(PATL,sprk)(PackGen, TA, UP, 0, N, K, al, A, 0, 0, lA, \
                          be, C, 0, 0, lC)
   #endif
   #ifdef TCPLX
      #define test_her2k(UP, TA, N, K, al, A, lA, B, lB, be, C, lC) \
         Mjoin( AP4, her2k )(UP, TA, N, K, al, A, lA, B, lB, be, C, lC)
      #define test_hemm(SI, UP, M, N, al, A, lA, B, lB, be, C, lC) \
         Mjoin(AP4,hemm)(SI, UP, M, N, al, A, lA, B, lB, be, C, lC)
      #ifndef HPRK
         #define test_herk(UP, TA, N, K, al, A, lA, be, C, lC) \
            Mjoin(AP4,herk)(UP, TA, N, K, al, A, lA, be, C, lC)
      #else
         #define test_herk(UP, TA, N, K, al, A, lA, be, C, lC) \
            Mjoin(PATL,hprk)(PackGen, TA, UP, 0, N, K, al, A, 0, 0, lA, \
                             be, C, 0, 0, lC)
      #endif
   #endif
#endif

/*
 * =====================================================================
 * macro functions
 * =====================================================================
 */
#ifdef TCPLX
#define Mabs1(X) (Mabs(*X) + Mabs(*(X+1)))
#else
#define Mabs1(X) (Mabs(X))
#endif

#ifdef  ATL_NTHREADS
#define LCSIZE          ATL_NTHREADS * L2SIZE
#else
#define LCSIZE          L2SIZE
#endif
/*
 * =====================================================================
 * typedef definitions
 * =====================================================================
 */
#ifdef TREAL
enum LVL3_ROUT { GEMM=0, SYMM, SYRK, SYR2K, TRMM, TRSM, ALLROUTS };
#else
enum LVL3_ROUT
{ GEMM=0, HEMM, HERK, HER2K, SYMM, SYRK, SYR2K, TRMM, TRSM, ALLROUTS };
#endif
/*
 * =====================================================================
 * Prototypes for the testing routines
 * =====================================================================
 */
double     opbl3
(  const enum LVL3_ROUT,           const int,      const int,
   const int );
void       trddom
(  const enum ATLAS_UPLO,          const int,      TYPE *,
   const int );

TYPE       gemmtst
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const enum ATLAS_TRANS,
   const enum ATLAS_TRANS,         const int,      const int,
   const int,      const SCALAR,   const int,      const int,
   const SCALAR,   const int,      const TYPE,     double *,
   double *,       double *,       double * );
TYPE       symmtst
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const enum ATLAS_SIDE,
   const enum ATLAS_UPLO,          const int,      const int,
   const SCALAR,   const int,      const int,      const SCALAR,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       syr2ktst
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,         const int,      const int,
   const SCALAR,   const int,      const int,      const SCALAR,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       syrktst
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,         const int,      const int,
   const SCALAR,   const int,      const SCALAR,   const int,
   const TYPE,     double *,       double *,       double *,
   double * );
TYPE       trmmtst
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const enum ATLAS_SIDE,
   const enum ATLAS_UPLO,          const enum ATLAS_TRANS,
   const enum ATLAS_DIAG,          const int,      const int,
   const SCALAR,   const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );
TYPE       trsmtst
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const enum ATLAS_SIDE,
   const enum ATLAS_UPLO,          const enum ATLAS_TRANS,
   const enum ATLAS_DIAG,          const int,      const int,
   const SCALAR,   const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );

int        gemmcase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const enum ATLAS_TRANS,         const enum ATLAS_TRANS,
   const int,      const int,      const int,      const SCALAR,
   const int,      const int,      const SCALAR,   const int,
   const TYPE,     double *,       double *,       double *,
   double * );
int        symmcase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const enum ATLAS_SIDE,          const enum ATLAS_UPLO,
   const int,      const int,      const SCALAR,   const int,
   const int,      const SCALAR,   const int,      const TYPE,
   double *,       double *,       double *,       double * );
int        syr2kcase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const enum ATLAS_TRANS,
   const int,      const int,      const SCALAR,   const int,
   const int,      const SCALAR,   const int,      const TYPE,
   double *,       double *,       double *,       double * );
int        syrkcase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const enum ATLAS_TRANS,
   const int,      const int,      const SCALAR,   const int,
   const SCALAR,   const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        trxmcase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const enum ATLAS_SIDE,          const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,         const enum ATLAS_DIAG,
   const int,      const int,      const SCALAR,   const int,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );

void       RungemmCase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_TRANS *,
   const int,      const enum ATLAS_TRANS *,       int,
   int,            int,            int,            int,
   int,            int,            int,            int,
   const int,      const TYPE *,   const int,      const TYPE *,
   const TYPE,     int *,          int * );
void      RunsymmCase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_SIDE *,
   const int,      const enum ATLAS_UPLO *,        int,
   int,            int,            int,            int,
   int,            const int,      const TYPE *,   const int,
   const TYPE *,   const TYPE,     int *,          int * );
void      Runsyr2kCase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   const int,      const enum ATLAS_TRANS *,       int,
   int,            int,            int,            int,
   int,            const int,      const TYPE *,   const int,
   const TYPE *,   const TYPE,     int *,          int * );
void      RunsyrkCase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   const int,      const enum ATLAS_TRANS *,       int,
   int,            int,            int,            int,
   int,            const int,      const TYPE *,   const int,
   const TYPE *,   const TYPE,     int *,          int * );
void      RuntrxmCase
(  const int CACHESIZE,
   const enum LVL3_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_SIDE *,
   const int,      const enum ATLAS_UPLO *,        const int,
   const enum ATLAS_TRANS *,       const int,      const enum ATLAS_DIAG *,
   int,            int,            int,            int,
   int,            int,            const int,      const TYPE *,
   const TYPE,     int *,          int * );

void       RunCases
(  const int,      const int,      const int,      const int,
   const int,
   const enum ATLAS_SIDE *,        const int,      const enum ATLAS_UPLO *,
   const int,      const enum ATLAS_TRANS *,       const int,
   const enum ATLAS_TRANS *,       const int,      const enum ATLAS_DIAG *,
   const int,      const int,      const int,      const int,
   const int,      const int,      const int,      const int,
   const int,      const int,      const TYPE *,   const int,
   const TYPE *,   const int,      const enum LVL3_ROUT * );

void       PrintUsage
(  char * nam );

void       GetFlags
(  int,            char **,        int *,          enum LVL3_ROUT **,
   int *,          int *,          int *,          int *,
   int *,          enum ATLAS_SIDE **,             int *,
   enum ATLAS_UPLO **,             int *,          enum ATLAS_TRANS **,
   int *,          enum ATLAS_TRANS **,            int *,
   enum ATLAS_DIAG **,             int *,          int *,
   int *,          int *,          int *,          int *,
   int *,          int *,          int *,          int *,
   TYPE **,        int *,          TYPE ** );

int        main
(  int,            char ** );
/*
 * =====================================================================
 */
double opbl3
(
   const enum LVL3_ROUT       ROUT,
   const int                  M,
   const int                  N,
   const int                  K
)
{
   double                     adds = 0.0, em, en, ek, muls = 0.0;
/*
 * On entry,  M,  N,  and K contain parameter values used by the Level 3
 * BLAS.  The output matrix is always M x N or N x N if symmetric, but K
 * has different uses in different contexts. For example, in the matrix-
 * matrix multiply routine,  we  have C = A * B where  C is M x N,  A is
 * M x K, and B is K x N. In xSYMM, xHEMM, xTRMM, and xTRSM, K indicates
 * whether the matrix A is applied on the left or right.  If K <= 0, the
 * matrix is aqpplied on the left, and if K > 0, on  the  right.
 */
   if( M <= 0 ) return( 0.0 );

   em = (double)(M); en = (double)(N); ek = (double)(K);

   if(      ROUT == GEMM ) { muls = em * ek * en; adds = em * ek * en; }
#ifdef TREAL
   else if( ROUT == SYMM )
#else
   else if( ( ROUT == SYMM ) || ( ROUT == HEMM ) )
#endif
   {                 /* If K <= 0, assume A multiplies B on the left. */
      if( K <= 0 ) { muls = em * em * en; adds = em * em * en; }
      else         { muls = em * en * en; adds = em * en * en; }
   }
   else if( ROUT == TRMM )
   {                 /* If K <= 0, assume A multiplies B on the left. */
      if( K <= 0 )
      {
         muls = en * em * ( em + 1.0 ) / 2.0;
         adds = en * em * ( em - 1.0 ) / 2.0;
      }
      else
      {
         muls = em * en * ( en + 1.0 ) / 2.0;
         adds = em * en * ( en - 1.0 ) / 2.0;
      }
   }
#ifdef TREAL
   else if( ROUT == SYRK )
#else
   else if( ( ROUT == SYRK ) || ( ROUT == HERK ) )
#endif
   {
      muls = ek * em * ( em + 1.0 ) / 2.0;
      adds = ek * em * ( em + 1.0 ) / 2.0;
   }
#ifdef TREAL
   else if( ROUT == SYR2K )
#else
   else if( ( ROUT == SYR2K ) || ( ROUT == HER2K ) )
#endif
   { muls = ek * em * em; adds = ek * em * em + em; }
   else if( ROUT == TRSM )
   {                 /* If K <= 0, assume A multiplies B on the left. */
      if( K <= 0 )
      {
         muls = en * em * ( em + 1.0 ) / 2.0;
         adds = en * em * ( em - 1.0 ) / 2.0;
      }
      else
      {
         muls = em * en * ( en + 1.0 ) / 2.0;
         adds = em * en * ( en - 1.0 ) / 2.0;
      }
   }
#ifdef TREAL
   return(       muls +       adds );
#else
   return( 6.0 * muls + 2.0 * adds );
#endif
}

void trddom
(
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   TYPE                       * A,
   const int                  LDA
)
{
/*
 * Scale strictly lower (resp. upper) part of triangular matrix by 1 / N
 * to make it diagonally dominant.
 */
   int                        i, iaij, j, jaj, lda2 = ( LDA SHIFT ),
                              ldap12 = (( LDA + 1 ) SHIFT);
   TYPE                       alpha;

   if( N <= 0 ) return;

   alpha = ATL_rone / (TYPE)(N);

   if( UPLO == AtlasUpper )
   {
      for( j = 0, jaj = 0; j < N; j++, jaj += lda2 )
      {
         for( i = 0, iaij = jaj; i < j; i++, iaij += (1 SHIFT) )
         {
            A[iaij  ] *= alpha;
#ifdef TCPLX
            A[iaij+1] *= alpha;
#endif
         }
         if( A[iaij  ] >= ATL_rzero ) A[iaij  ] += ATL_rone;
         else                         A[iaij  ] -= ATL_rone;
#ifdef TCPLX
         if( A[iaij+1] >= ATL_rzero ) A[iaij+1] += ATL_rone;
         else                         A[iaij+1] -= ATL_rone;
#endif
      }
   }
   else
   {
      for( j = N-1, jaj = (N-1)*ldap12; j >= 0; j--, jaj -= ldap12 )
      {
         if( A[jaj  ] >= ATL_rzero ) A[jaj  ] += ATL_rone;
         else                        A[jaj  ] -= ATL_rone;
#ifdef TCPLX
         if( A[jaj+1] >= ATL_rzero ) A[jaj+1] += ATL_rone;
         else                        A[jaj+1] -= ATL_rone;
#endif
         for( i = j+1, iaij = jaj+(1 SHIFT); i < N; i++, iaij += (1 SHIFT) )
         {
            A[iaij  ] *= alpha;
#ifdef TCPLX
            A[iaij+1] *= alpha;
#endif
         }
      }
   }
}
/*
 * =====================================================================
 * tst functions
 * =====================================================================
 */
TYPE gemmtst
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_TRANS     TRANSA,
   const enum ATLAS_TRANS     TRANSB,
   const int                  M,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normB, normC, normD, resid;
   TYPE                       * A  = NULL, * B = NULL, * C = NULL, * C0,
                              * a, * b, * c;
   int                        mA, mB, nA, nB, Aseed, Bseed, Cseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( ( M == 0 ) || ( N == 0 ) ) { return( ATL_rzero ); }

   if( TRANSA == AtlasNoTrans ) { mA = M; nA = K; }
   else                         { mA = K; nA = M; }
   if( TRANSB == AtlasNoTrans ) { mB = K; nB = N; }
   else                         { mB = N; nB = K; }

   ops = opbl3( ROUT, M, N, K );
/*
 * Allocate L2 cache space, A, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( LDA ) * nA     );
   B  = (TYPE *)malloc( ATL_MulBySize( LDB ) * nB     );
   C  = (TYPE *)malloc( ATL_MulBySize( LDC ) * N  * 2 );

   if( ( A == NULL ) || ( B == NULL ) || ( C == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( B ) free( B );
      if( C ) free( C );
      return( ATL_rnone );
   }

   C0 = C + LDC * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = mA * nA + 513 *  7 + 90;
   Bseed = mB * nB + 127 * 50 + 77;
   Cseed = M  * N  + 101 *  2 + 53;

   Mjoin( PATL, gegen )( mA, nA, A,  LDA, Aseed );
   Mjoin( PATL, gegen )( mB, nB, B,  LDB, Bseed );
   Mjoin( PATL, gegen )( M,  N,  C,  LDC, Cseed );
   Mjoin( PATL, gegen )( M,  N,  C0, LDC, Cseed );
/*
 * Compute the norm of C for later use in testing
 */
   if( TEST )
   {
      normC = Mjoin( PATL, genrm1 )( M, N, C, LDC );
      if( Mabs1( BETA ) > ATL_rone ) normC *= Mabs1( BETA  );
      if( normC == ATL_rzero ) normC = ATL_rone;
   }
   else { normC = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   a = A; b = B; c = C0;

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_gemm( TRANSA, TRANSB, M, N, K, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   a = A; b = B; c = C;

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   test_gemm(    TRANSA, TRANSB, M, N, K, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( B ); free( C ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normA = Mjoin( PATL, genrm1 )( mA, nA, A, LDA );
   if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
   if( normA == ATL_rzero ) normA = ATL_rone;
   free( A  );

   normB = Mjoin( PATL, genrm1 )( mB, nB, B, LDB );
   if( normB == ATL_rzero ) normB = ATL_rone;
   free( B  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( M, N, C, LDC, C0, LDC );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normB, ATL_rone ) *
                     Mmax( normC, ATL_rone ) * EPSILON *
                     Mmax( Mmax( M, N ), K ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normA=%f, normB=%f, normC=%f, eps=%e\n",
      resid, normD, normA, normB, normC, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "C_trusted", M, N, C0, LDC );
      Mjoin( PATL, geprint )( "C_test",    M, N, C,  LDC );
#endif
   }

   free( C  );

   return( resid );
}

TYPE symmtst
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normB, normC, normD, resid;
   TYPE                       * A  = NULL, * B = NULL, * C = NULL, * C0,
                              * a, * b, * c;
   int                        nA, Aseed, Bseed, Cseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( SIDE == AtlasLeft ) { ops = opbl3( ROUT, M, N, -1 ); nA = M; }
   else                    { ops = opbl3( ROUT, M, N,  1 ); nA = N; }
/*
 * Allocate L2 cache space, A, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( LDA ) * nA     );
   B  = (TYPE *)malloc( ATL_MulBySize( LDB ) * N      );
   C  = (TYPE *)malloc( ATL_MulBySize( LDC ) * N  * 2 );

   if( ( A == NULL ) || ( B == NULL ) || ( C == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( B ) free( B );
      if( C ) free( C );
      return( ATL_rnone );
   }

   C0 = C + LDC * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = nA * nA + 513 *  7 + 90;
   Bseed = M  * N  + 127 * 50 + 77;
   Cseed = M  * N  + 101 *  2 + 53;

   Mjoin( PATL, gegen )( nA, nA, A,  LDA, Aseed );
   Mjoin( PATL, gegen )( M,  N,  B,  LDB, Bseed );
   Mjoin( PATL, gegen )( M,  N,  C,  LDC, Cseed );
   Mjoin( PATL, gegen )( M,  N,  C0, LDC, Cseed );
/*
 * Compute the norm of C for later use in testing
 */
   if( TEST )
   {
      normC = Mjoin( PATL, genrm1 )( M, N, C, LDC );
      if( Mabs1( BETA ) > ATL_rone ) normC *= Mabs1( BETA  );
      if( normC == ATL_rzero ) normC = ATL_rone;
   }
   else { normC = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   a = A; b = B; c = C0;

#ifdef TREAL
   l2ret = ATL_flushcache( -1 );
   t0     = time00();
   trusted_symm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
#else
   if( ROUT == SYMM )
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      trusted_symm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
      ttrust = time00() - t0;
      if( ttrust > 0.0 )
      { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
   }
   else /* if( ROUT == HEMM ) */
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      trusted_hemm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
      ttrust = time00() - t0;
      if( ttrust > 0.0 )
      { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
   }
#endif
/*
 * Start cold cache timing operations for the tested routine
 */
   a = A; b = B; c = C;

#ifdef TREAL
   l2ret = ATL_flushcache( -1 );
   t0     = time00();
   test_symm(    SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
#else
   if( ROUT == SYMM )
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      test_symm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
      ttest  = time00() - t0;
      if( ttest  > 0.0 )
      { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
   }
   else /* if( ROUT == HEMM ) */
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      test_hemm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
      ttest  = time00() - t0;
      if( ttest  > 0.0 )
      { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
   }
#endif
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( B ); free( C ); return( ATL_rzero ); }
/*
 * else perform error check
 */
#ifdef TREAL
   normA = Mjoin( PATL, synrm )( UPLO, nA, A, LDA );
#else
   if( ROUT == SYMM ) normA = Mjoin( PATL, synrm )( UPLO, nA, A, LDA );
   else               normA = Mjoin( PATL, henrm )( UPLO, nA, A, LDA );
#endif
   if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
   if( normA == ATL_rzero ) normA = ATL_rone;
   free( A  );

   normB = Mjoin( PATL, genrm1 )( M, N, B, LDB );
   if( normB == ATL_rzero ) normB = ATL_rone;
   free( B  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( M, N, C, LDC, C0, LDC );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normB, ATL_rone ) *
                     Mmax( normC, ATL_rone ) * EPSILON * Mmax( M, N ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normA=%f, normB=%f, normC=%f, eps=%e\n",
      resid, normD, normA, normB, normC, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "C_trusted", M, N, C0, LDC );
      Mjoin( PATL, geprint )( "C_test",    M, N, C,  LDC );
#endif
   }

   free( C  );

   return( resid );
}

TYPE syr2ktst
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normB, normC, normD, resid;
   TYPE                       * A = NULL, * B = NULL, * C = NULL, * C0,
                              * a, * b, * c;
   int                        mAB, nAB, Aseed, Bseed, Cseed;
   enum ATLAS_TRANS           ta;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( TRANS == AtlasNoTrans )
   { ta = TRANS; mAB = N; nAB = K; }
   else
   { ta = ( ROUT == SYR2K ? AtlasTrans : AtlasConjTrans ); mAB = K; nAB = N; }

   ops = opbl3( ROUT, N, 0, K );
/*
 * Allocate L2 cache space, A, C and C0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   A = (TYPE *)malloc( ATL_MulBySize( LDA ) * nAB    );
   B = (TYPE *)malloc( ATL_MulBySize( LDB ) * nAB    );
   C = (TYPE *)malloc( ATL_MulBySize( LDC ) * N  * 2 );

   if( ( A == NULL ) || ( B == NULL ) || ( C == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( B ) free( B );
      if( C ) free( C );
      return( ATL_rnone );
   }

   C0 = C + LDC * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = mAB * nAB + 513 *  7 + 90;
   Bseed = mAB * nAB + 127 * 50 + 77;
   Cseed = N   * N   + 101 *  2 + 53;

   Mjoin( PATL, gegen )( mAB, nAB, A,  LDA, Aseed );
   Mjoin( PATL, gegen )( mAB, nAB, B,  LDB, Bseed );
   Mjoin( PATL, gegen )( N,   N,   C,  LDC, Cseed );
   Mjoin( PATL, gegen )( N,   N,   C0, LDC, Cseed );
/*
 * Compute the norm of C for later use in testing
 */
   if( TEST )
   {
#ifdef TREAL
      normC = Mjoin( PATL, synrm )( UPLO, N, C, LDC );
      if( Mabs1( BETA ) > ATL_rone ) normC *= Mabs1( BETA  );
      if( normC == ATL_rzero ) normC = ATL_rone;
#else
      if( ROUT == SYR2K )
      {
         normC = Mjoin( PATL, synrm )( UPLO, N, C, LDC );
         if( Mabs1( BETA ) > ATL_rone ) normC *= Mabs1( BETA  );
         if( normC == ATL_rzero ) normC = ATL_rone;
      }
      else
      {
         normC = Mjoin( PATL, henrm )( UPLO, N, C, LDC );
         if( Mabs( BETA[0] ) > ATL_rone ) normC *= Mabs( BETA[0] );
         if( normC == ATL_rzero ) normC = ATL_rone;
      }
#endif
   }
   else { normC = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   a = A; b = B; c = C0;
#ifdef TREAL
   l2ret = ATL_flushcache( -1 );
   t0     = time00();
   trusted_syr2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
   ttrust = time00() - t0;
#else
   if( ROUT == SYR2K )
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      trusted_syr2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == HER2K ) */
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      trusted_her2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (TYPE)(BETA[0]),
                     c, LDC );
      ttrust = time00() - t0;
   }
#endif
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   a = A; b = B; c = C;
#ifdef TREAL
   l2ret = ATL_flushcache( -1 );
   t0     = time00();
   test_syr2k(    UPLO, TRANS, N, K, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
   ttest  = time00() - t0;
#else
   if( ROUT == SYR2K )
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      test_syr2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, BETA, c, LDC );
      ttest  = time00() - t0;
   }
   else /* if( ROUT == HERK ) */
   {
      l2ret = ATL_flushcache( -1 );
      t0     = time00();
      test_her2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (TYPE)(BETA[0]),
                  c, LDC );
      ttest  = time00() - t0;
   }
#endif
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( C ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normA = Mjoin( PATL, genrm1 )( mAB, nAB, A, LDA );
   if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
   if( normA == ATL_rzero ) normA = ATL_rone;
   free( A  );

   normB = Mjoin( PATL, genrm1 )( mAB, nAB, B, LDB );
   if( normB == ATL_rzero ) normB = ATL_rone;
   free( B  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( N, N, C, LDC, C0, LDC );
   resid = normD / ( Mmax( normC, ATL_rone ) * Mmax( normA, ATL_rone ) *
                     Mmax( normB, ATL_rone ) * EPSILON * Mmax( N, K ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:    resid=%f, normD=%f, normA=%f, normC=%f, eps=%e\n",
      resid, normD, normA, normC, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "C_trusted", N, N, C0, LDC );
      Mjoin( PATL, geprint )( "C_test",    N, N, C,  LDC );
#endif
   }

   free( C  );

   return( resid );
}

TYPE syrktst
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normC, normD, resid;
   TYPE                       * A = NULL, * C = NULL, * C0, * a, * c;
   int                        mA, nA, Aseed, Cseed;
   enum ATLAS_TRANS           ta;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( TRANS == AtlasNoTrans )
   { ta = TRANS; mA = N; nA = K; }
   else
   { ta = ( ROUT == SYRK ? AtlasTrans : AtlasConjTrans ); mA = K; nA = N; }

   ops = opbl3( ROUT, N, 0, K );
/*
 * Allocate L2 cache space, A, C and C0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   A = (TYPE *)malloc( ATL_MulBySize( LDA ) * nA     );
   C = (TYPE *)malloc( ATL_MulBySize( LDC ) * N  * 2 );

   if( ( A == NULL ) || ( C == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( C ) free( C );
      return( ATL_rnone );
   }

   C0 = C + LDC * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = mA * nA + 513 *  7 + 90;
   Cseed = N  * N  + 101 *  2 + 53;

   Mjoin( PATL, gegen )( mA, nA, A,  LDA, Aseed );
   Mjoin( PATL, gegen )( N,  N,  C,  LDC, Cseed );
   Mjoin( PATL, gegen )( N,  N,  C0, LDC, Cseed );
/*
 * Compute the norm of C for later use in testing
 */
   if( TEST )
   {
#ifdef TREAL
      normC = Mjoin( PATL, synrm )( UPLO, N, C, LDC );
      if( Mabs1( BETA ) > ATL_rone ) normC *= Mabs1( BETA  );
      if( normC == ATL_rzero ) normC = ATL_rone;
#else
      if( ROUT == SYRK )
      {
         normC = Mjoin( PATL, synrm )( UPLO, N, C, LDC );
         if( Mabs1( BETA ) > ATL_rone ) normC *= Mabs1( BETA  );
         if( normC == ATL_rzero ) normC = ATL_rone;
      }
      else
      {
         normC = Mjoin( PATL, henrm )( UPLO, N, C, LDC );
         if( Mabs( BETA[0] ) > ATL_rone ) normC *= Mabs( BETA[0] );
         if( normC == ATL_rzero ) normC = ATL_rone;
      }
#endif
   }
   else { normC = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   a = A; c = C0;
#ifdef TREAL
   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_syrk( UPLO, ta, N, K, ALPHA, a, LDA, BETA, c, LDC );
   ttrust = time00() - t0;
#else
   if( ROUT == SYRK )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_syrk( UPLO, ta, N, K, ALPHA, a, LDA, BETA, c, LDC );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == HERK ) */
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_herk( UPLO, ta, N, K, (TYPE)(ALPHA[0]), a, LDA, (TYPE)(BETA[0]),
                    c, LDC );
      ttrust = time00() - t0;
   }
#endif
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   a = A; c = C;
#ifdef TREAL
   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   test_syrk(    UPLO, TRANS, N, K, ALPHA, a, LDA, BETA, c, LDC );
   ttest  = time00() - t0;
#else
   if( ROUT == SYRK )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      test_syrk( UPLO, ta, N, K, ALPHA, a, LDA, BETA, c, LDC );
      ttest  = time00() - t0;
   }
   else /* if( ROUT == HERK ) */
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      test_herk( UPLO, ta, N, K, (TYPE)(ALPHA[0]), a, LDA, (TYPE)(BETA[0]),
                 c, LDC );
      ttest  = time00() - t0;
   }
#endif
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( C ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normA = Mjoin( PATL, genrm1 )( mA, nA, A, LDA );
#ifdef TREAL
   if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
#else
   if( ROUT == SYRK )
   { if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA ); }
   else
   { if( Mabs( ALPHA[0] ) > ATL_rone ) normA *= Mabs( ALPHA[0] ); }
#endif
   if( normA == ATL_rzero ) normA = ATL_rone;
   free( A  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( N, N, C, LDC, C0, LDC );
   resid = normD / ( Mmax( normC, ATL_rone ) * Mmax( normA, ATL_rone ) *
                     EPSILON * Mmax( N, K ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:    resid=%f, normD=%f, normA=%f, normC=%f, eps=%e\n",
      resid, normD, normA, normC, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "C_trusted", N, N, C0, LDC );
      Mjoin( PATL, geprint )( "C_test",    N, N, C,  LDC );
#endif
   }

   free( C  );

   return( resid );
}

TYPE trmmtst
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normB, normD, resid;
   TYPE                       * A = NULL, * B = NULL, * B0, * a, * b;
   int                        nA, Aseed, Bseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( ( M == 0 ) || ( N == 0 ) ) { return( ATL_rzero ); }

   if( SIDE == AtlasLeft ) { nA = M; ops = opbl3( ROUT, M, N, -1 ); }
   else                    { nA = N; ops = opbl3( ROUT, M, N,  1 ); }
/*
 * Allocate L2 cache space, A, X and X0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   A = (TYPE *)malloc( ATL_MulBySize( LDA ) * nA    );
   B = (TYPE *)malloc( ATL_MulBySize( LDB ) * N * 2 );

   if( ( A == NULL ) || ( B == NULL ) )
   {
      l2ret  = ATL_flushcache( 0 );
      if( A ) free( A );
      if( B ) free( B );
      return( ATL_rnone );
   }

   B0 = B + LDB * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = nA * nA + 513 *  7 + 90;
   Bseed = M  * N  + 127 * 50 + 77;

   Mjoin( PATL, gegen )( nA, nA, A,  LDA, Aseed );
   Mjoin( PATL, gegen )( M,  N,  B,  LDB, Bseed );
   Mjoin( PATL, gegen )( M,  N,  B0, LDB, Bseed );
/*
 * Compute the norm of B for later use in testing
 */
   if( TEST )
   {
      normB = Mjoin( PATL, genrm1 )( M, N, B, LDB );
      if( Mabs1( ALPHA ) > ATL_rone ) normB *= Mabs1( ALPHA );
      if( normB == ATL_rzero ) normB = ATL_rone;
   }
   else { normB = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   a = A; b = B0;

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_trmm( SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   a = A; b = B;

   l2ret = ATL_flushcache( -1 );
   t0    = time00();
   test_trmm(    SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( B ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normA = Mjoin( PATL, trnrm1 )( UPLO, DIAG, nA, A, LDA );
   if( normA == ATL_rzero ) normA = ATL_rone;
   free( A  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( M, N, B, LDB, B0, LDB );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normB, ATL_rone ) *
                     EPSILON * Mmax( M, N ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normA=%f, normB=%f, eps=%e\n",
      resid, normD, normA, normB, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "B_trusted", M, N, B0, LDB );
      Mjoin( PATL, geprint )( "B_test",    M, N, B,  LDB );
#endif
   }

   free( B );

   return( resid );
}

TYPE trsmtst
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normB, normD, resid;
   TYPE                       * A = NULL, * B = NULL, * B0, * a, * b;
   int                        nA, Aseed, Bseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( ( M == 0 ) || ( N == 0 ) ) { return( ATL_rzero ); }

   if( SIDE == AtlasLeft ) { nA = M; ops = opbl3( ROUT, M, N, -1 ); }
   else                    { nA = N; ops = opbl3( ROUT, M, N,  1 ); }
/*
 * Allocate L2 cache space, A, X and X0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( LDA ) * nA    );
   B  = (TYPE *)malloc( ATL_MulBySize( LDB ) * N * 2 );

   if( ( A == NULL ) || ( B == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( B ) free( B );
      return( ATL_rnone );
   }

   B0 = B + LDB * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = nA * nA + 513 *  7 + 90;
   Bseed = M  * N  + 127 * 50 + 77;

   Mjoin( PATL, gegen )( nA, nA, A,  LDA, Aseed ); trddom( UPLO, nA, A, LDA );
   Mjoin( PATL, gegen )( M,  N,  B,  LDB, Bseed );
   Mjoin( PATL, gegen )( M,  N,  B0, LDB, Bseed );
/*
 * Compute the norm of B for later use in testing
 */
   if( TEST )
   {
      normB = Mjoin( PATL, genrm1 )( M, N, B, LDB );
      if( Mabs1( ALPHA ) > ATL_rone ) normB *= Mabs1( ALPHA );
      if( normB == ATL_rzero ) normB = ATL_rone;
   }
   else { normB = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   a = A; b = B0;

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_trsm( SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   a = A; b = B;

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   test_trsm(    SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( B ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normA = Mjoin( PATL, trnrm1 )( UPLO, DIAG, nA, A, LDA );
   if( normA == ATL_rzero ) normA = ATL_rone;
   free( A  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( M, N, B, LDB, B0, LDB );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normB, ATL_rone ) *
                     EPSILON * Mmax( M, N ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normA=%f, normB=%f, eps=%e\n",
      resid, normD, normA, normB, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "B_trusted", M, N, B0, LDB );
      Mjoin( PATL, geprint )( "B_test",    M, N, B,  LDB );
#endif
   }

   free( B );

   return( resid );
}
/*
 * =====================================================================
 * case functions
 * =====================================================================
 */
int gemmcase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_TRANS     TRANSA,
   const enum ATLAS_TRANS     TRANSB,
   const int                  M,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
#ifdef TREAL
   TYPE                       bet,  beta,    nbeta;
#else
   TYPE                       *bet, beta[2], nbeta[2];
#endif
   TYPE                       * a, * stA, *b, * stB, * c, * stC, * A,
                              * A0 = NULL, * B, * B0 = NULL, * C, * C0 = NULL;
   unsigned long              ir, reps;
   int                        inca, incb, incc, lA, lB, lC, mA, nA, mB, nB,
                              passed, Aseed, Bseed, Cseed;

   if( ( MEGA * MFLOP <= ( flops = opbl3( ROUT, M, N, K ) ) ) || ( TEST ) )
   {
      resid = gemmtst( CACHESIZE, ROUT, TEST, TRANSA, TRANSB, M, N, K, ALPHA,
		       LDA, LDB, BETA, LDC, EPSILON, TTRUST0, TTEST0,
		       MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( TRANSA == AtlasNoTrans ) { mA = M; nA = K; } else { mA = K; nA = M; }
   if( TRANSB == AtlasNoTrans ) { mB = K; nB = N; } else { mB = N; nB = K; }

   inca = LDA  * ( nA SHIFT );
   incb = LDB  * ( nB SHIFT );
   incc = LDC  * ( N  SHIFT );

   lA = inca  * ( ( ATL_DivBySize( LCSIZE ) + mA*nA - 1 ) / ( mA * nA ) );
   lB = incb  * ( ( ATL_DivBySize( LCSIZE ) + mB*nB - 1 ) / ( mB * nB ) );
   lC = incc  * ( ( ATL_DivBySize( LCSIZE ) + M * N - 1 ) / ( M  * N  ) );

   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   B0 = (TYPE *)malloc( ATL_MulBySize( lB ) );
   C0 = (TYPE *)malloc( ATL_MulBySize( lC ) );

   if( ( A0 == NULL ) || ( B0 == NULL ) || ( C0 == NULL ) )
   {
      if( A0 ) free( A0 );
      if( B0 ) free( B0 );
      if( C0 ) free( C0 );
      return( -1 );
   }

   A = A0; stA = A0 + ( lA SHIFT );
   B = B0; stB = B0 + ( lB SHIFT );
   C = C0; stC = C0 + ( lC SHIFT );

#ifdef TREAL
   beta   =  BETA;
   nbeta  = -BETA;
#else
   *beta  =    *BETA; beta [1] =  BETA[1];
   *nbeta = -(*BETA); nbeta[1] = -BETA[1];
#endif

   Aseed = mA * nA + 513 *  7 + 90;
   Bseed = mB * nB + 127 * 50 + 77;
   Cseed = M  * N  + 101 *  2 + 53;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   bet = beta; a = A; b = B; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_gemm( TRANSA, TRANSB, M, N, K, ALPHA, a, LDA, b, LDB,
                    (SCALAR)(bet), c, LDC );
      a += inca; if( a == stA ) { a = A; }
      b += incb; if( b == stB ) { b = B; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   bet = beta; a = A; b = B; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_gemm(    TRANSA, TRANSB, M, N, K, ALPHA, a, LDA, b, LDB,
                    (SCALAR)(bet), c, LDC );
      a += inca; if( a == stA ) { a = A; }
      b += incb; if( b == stB ) { b = B; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( C0 );
   free( B0 );
   free( A0 );

   return( passed );
}

int symmcase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
#ifdef TREAL
   TYPE                       bet,  beta,    nbeta;
#else
   TYPE                       *bet, beta[2], nbeta[2];
#endif
   TYPE                       * a, * stA, *b, * stB, * c, * stC, * A,
                              * A0 = NULL, * B, * B0 = NULL, * C, * C0 = NULL;
   unsigned long              ir, reps;
   int                        inca, incb, incc, lA, lB, lC, nA, passed, Aseed,
                              Bseed, Cseed;

   flops = opbl3( ROUT, M, N, ( SIDE == AtlasLeft ? -1 : 1 ) );

   if( ( MEGA * MFLOP <= flops ) || ( TEST ) )
   {
      resid = symmtst( CACHESIZE, ROUT, TEST, SIDE, UPLO, M, N, ALPHA,
		       LDA, LDB, BETA, LDC, EPSILON, TTRUST0, TTEST0,
		       MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( SIDE == AtlasLeft ) { nA = M; } else { nA = N; }

   inca = LDA  * ( nA SHIFT );
   incb = LDB  * ( N  SHIFT );
   incc = LDC  * ( N  SHIFT );

   lA = inca  * ( ( ATL_DivBySize( LCSIZE ) + nA*nA - 1 ) / ( nA * nA ) );
   lB = incb  * ( ( ATL_DivBySize( LCSIZE ) + M * N - 1 ) / ( M  * N  ) );
   lC = incc  * ( ( ATL_DivBySize( LCSIZE ) + M * N - 1 ) / ( M  * N  ) );

   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   B0 = (TYPE *)malloc( ATL_MulBySize( lB ) );
   C0 = (TYPE *)malloc( ATL_MulBySize( lC ) );

   if( ( A0 == NULL ) || ( B0 == NULL ) || ( C0 == NULL ) )
   {
      if( A0 ) free( A0 );
      if( B0 ) free( B0 );
      if( C0 ) free( C0 );
      return( -1 );
   }

   A = A0; stA = A0 + ( lA SHIFT );
   B = B0; stB = B0 + ( lB SHIFT );
   C = C0; stC = C0 + ( lC SHIFT );

#ifdef TREAL
   beta   =  BETA;
   nbeta  = -BETA;
#else
   *beta  =    *BETA; beta [1] =  BETA[1];
   *nbeta = -(*BETA); nbeta[1] = -BETA[1];
#endif

   Aseed = nA * nA + 513 *  7 + 90;
   Bseed = M  * N  + 127 * 50 + 77;
   Cseed = M  * N  + 101 *  2 + 53;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   bet = beta; a = A; b = B; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

#ifdef TREAL
   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_symm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                    c, LDC );
      a += inca; if( a == stA ) { a = A; }
      b += incb; if( b == stB ) { b = B; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttrust = time00() - t0;
#else
   if( ROUT == SYMM )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_symm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                       c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttrust = time00() - t0;
   }
   else /* if( ROUT == HEMM ) */
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_hemm( SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                       c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttrust = time00() - t0;
   }
#endif
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   bet = beta; a = A; b = B; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

#ifdef TREAL
   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_symm(    SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                    c, LDC );
      a += inca; if( a == stA ) { a = A; }
      b += incb; if( b == stB ) { b = B; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttest = time00() - t0;
#else
   if( ROUT == SYMM )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_symm(    SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                       c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttest = time00() - t0;
   }
   else /* if( ROUT == HEMM ) */
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_hemm(    SIDE, UPLO, M, N, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                       c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttest = time00() - t0;
   }
#endif
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( C0 );
   free( B0 );
   free( A0 );

   return( passed );
}

int syr2kcase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
#ifdef TREAL
   TYPE                       bet,  beta,    nbeta;
#else
   TYPE                       *bet, beta[2], nbeta[2];
#endif
   TYPE                       * a, * stA, *b, * stB, * c, * stC, * A,
                              * A0 = NULL, * B, * B0 = NULL, * C, * C0 = NULL;
   unsigned long              ir, reps;
   int                        inca, incb, incc, lA, lB, lC, mAB, nAB, passed,
                              Aseed, Bseed, Cseed;
   enum ATLAS_TRANS           ta;

   if( ( MEGA * MFLOP <= ( flops = opbl3( ROUT, N, 0, K ) ) ) || ( TEST ) )
   {
      resid = syr2ktst( CACHESIZE, ROUT, TEST, UPLO, TRANS, N, K, ALPHA,
			LDA, LDB, BETA, LDC, EPSILON, TTRUST0, TTEST0,
			MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( TRANS == AtlasNoTrans )
   { ta = TRANS; mAB = N; nAB = K; }
   else
   { ta = ( ROUT == SYR2K ? AtlasTrans : AtlasConjTrans ); mAB = K; nAB = N; }

   inca = LDA  * ( nAB SHIFT );
   incb = LDB  * ( nAB SHIFT );
   incc = LDC  * ( N   SHIFT );

   lA = inca  * ( ( ATL_DivBySize( LCSIZE ) + mAB*nAB - 1 ) / ( mAB * nAB ) );
   lB = incb  * ( ( ATL_DivBySize( LCSIZE ) + mAB*nAB - 1 ) / ( mAB * nAB ) );
   lC = incc  * ( ( ATL_DivBySize( LCSIZE ) + N * N   - 1 ) / ( N   * N   ) );

   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   B0 = (TYPE *)malloc( ATL_MulBySize( lB ) );
   C0 = (TYPE *)malloc( ATL_MulBySize( lC ) );

   if( ( A0 == NULL ) || ( B0 == NULL ) || ( C0 == NULL ) )
   {
      if( A0 ) free( A0 );
      if( B0 ) free( B0 );
      if( C0 ) free( C0 );
      return( -1 );
   }

   A = A0; stA = A0 + ( lA SHIFT );
   B = B0; stB = B0 + ( lB SHIFT );
   C = C0; stC = C0 + ( lC SHIFT );

#ifdef TREAL
   beta   =  BETA;
   nbeta  = -BETA;
#else
   *beta  =    *BETA; beta [1] =  BETA[1];
   *nbeta = -(*BETA); nbeta[1] = -BETA[1];
#endif

   Aseed = mAB * nAB + 513 *  7 + 90;
   Bseed = mAB * nAB + 127 * 50 + 77;
   Cseed = N   * N   + 101 *  2 + 53;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   bet = beta; a = A; b = B; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

#ifdef TREAL
   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_syr2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                     c, LDC );
      a += inca; if( a == stA ) { a = A; }
      b += incb; if( b == stB ) { b = B; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttrust = time00() - t0;
#else
   if( ROUT == SYR2K )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_syr2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                        c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttrust = time00() - t0;
   }
   else /* if( ROUT == HER2K ) */
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_her2k( UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (TYPE)(bet[0]),
                        c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttrust = time00() - t0;
   }
#endif
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   bet = beta; a = A; b = B; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

#ifdef TREAL
   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_syr2k(    UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                     c, LDC );
      a += inca; if( a == stA ) { a = A; }
      b += incb; if( b == stB ) { b = B; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttest = time00() - t0;
#else
   if( ROUT == SYR2K )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_syr2k(    UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (SCALAR)(bet),
                        c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttest = time00() - t0;
   }
   else /* if( ROUT == HER2K ) */
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_her2k(    UPLO, ta, N, K, ALPHA, a, LDA, b, LDB, (TYPE)(bet[0]),
                        c, LDC );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttest = time00() - t0;
   }
#endif
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( C0 );
   free( B0 );
   free( A0 );

   return( passed );
}

int syrkcase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const SCALAR               BETA,
   const int                  LDC,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
#ifdef TREAL
   TYPE                       bet,  beta,    nbeta;
#else
   TYPE                       *bet, beta[2], nbeta[2];
#endif
   TYPE                       * a, * stA, * c, * stC, * A, * A0 = NULL,
                              * C, * C0 = NULL;
   unsigned long              ir, reps;
   int                        inca, incc, lA, lC, mA, nA, passed, Aseed, Cseed;
   enum ATLAS_TRANS           ta;

   if( ( MEGA * MFLOP <= ( flops = opbl3( ROUT, N, 0, K ) ) ) || ( TEST ) )
   {
      resid = syrktst( CACHESIZE, ROUT, TEST, UPLO, TRANS, N, K, ALPHA, LDA,
		       BETA, LDC, EPSILON, TTRUST0, TTEST0, MFTRUST0,
		       MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( TRANS == AtlasNoTrans )
   { ta = TRANS; mA = N; nA = K; }
   else
   { ta = ( ROUT == SYRK ? AtlasTrans : AtlasConjTrans ); mA = K; nA = N; }

   inca = LDA  * ( nA SHIFT );
   incc = LDC  * ( N  SHIFT );

   lA = inca  * ( ( ATL_DivBySize( LCSIZE ) + mA*nA - 1 ) / ( mA * nA ) );
   lC = incc  * ( ( ATL_DivBySize( LCSIZE ) + N * N - 1 ) / ( N   * N ) );

   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   C0 = (TYPE *)malloc( ATL_MulBySize( lC ) );

   if( ( A0 == NULL ) || ( C0 == NULL ) )
   { if( A0 ) free( A0 ); if( C0 ) free( C0 ); return( -1 ); }

   A = A0; stA = A0 + ( lA SHIFT );
   C = C0; stC = C0 + ( lC SHIFT );

#ifdef TREAL
   beta   =  BETA;
   nbeta  = -BETA;
#else
   *beta  =    *BETA; beta [1] =  BETA[1];
   *nbeta = -(*BETA); nbeta[1] = -BETA[1];
#endif

   Aseed = mA * nA + 513 *  7 + 90;
   Cseed = N  * N  + 101 *  2 + 53;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   bet = beta; a = A; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

#ifdef TREAL
   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_syrk( UPLO, ta, N, K, ALPHA, a, LDA, (SCALAR)(bet), c, LDC );
      a += inca; if( a == stA ) { a = A; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttrust = time00() - t0;
#else
   if( ROUT == SYRK )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_syrk( UPLO, ta, N, K, ALPHA, a, LDA, (SCALAR)(bet), c, LDC );
         a += inca; if( a == stA ) { a = A; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttrust = time00() - t0;
   }
   else /* if( ROUT == HERK ) */
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_herk( UPLO, ta, N, K, (TYPE)(ALPHA[0]), a, LDA,
                       (TYPE)(bet[0]), c, LDC );
         a += inca; if( a == stA ) { a = A; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttrust = time00() - t0;
   }
#endif
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   bet = beta; a = A; c = C;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lC, 1, C0, lC, Cseed );

#ifdef TREAL
   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_syrk(    UPLO, ta, N, K, ALPHA, a, LDA, (SCALAR)(bet), c, LDC );
      a += inca; if( a == stA ) { a = A; }
      c += incc;
      if( c == stC ) { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
   }
   ttest = time00() - t0;
#else
   if( ROUT == SYRK )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_syrk(    UPLO, ta, N, K, ALPHA, a, LDA, (SCALAR)(bet), c, LDC );
         a += inca; if( a == stA ) { a = A; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttest = time00() - t0;
   }
   else /* if( ROUT == HERK ) */
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_herk(    UPLO, ta, N, K, (TYPE)(ALPHA[0]), a, LDA,
                       (TYPE)(bet[0]), c, LDC );
         a += inca; if( a == stA ) { a = A; }
         c += incc;
         if( c == stC )
         { c = C; if( bet == beta ) bet = nbeta; else bet = beta; }
      }
      ttest = time00() - t0;
   }
#endif
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( C0 );
   free( A0 );

   return( passed );
}

int trxmcase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  LDB,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
   TYPE                       * a, * stA, * b, * stB, * A, * A0 = NULL,
                              * B, * B0 = NULL;
   unsigned long              ir, reps;
   int                        inca, incb, lA, lB, nA, passed, Aseed, Bseed;

   flops = opbl3( ROUT, M, N, ( SIDE == AtlasLeft ? -1 : 1 ) );

   if( ( MEGA * MFLOP <= flops ) || ( TEST ) )
   {
      if( ROUT == TRMM )
      {
         resid = trmmtst( CACHESIZE, ROUT, TEST, SIDE, UPLO, TRANS, DIAG,
			  M, N, ALPHA, LDA, LDB, EPSILON, TTRUST0, TTEST0,
			  MFTRUST0, MFTEST0 );
      }
      else
      {
         resid = trsmtst( CACHESIZE, ROUT, TEST, SIDE, UPLO, TRANS, DIAG,
			  M, N, ALPHA, LDA, LDB, EPSILON, TTRUST0, TTEST0,
			  MFTRUST0, MFTEST0 );
      }
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( SIDE == AtlasLeft ) { nA = M; } else { nA = N; }

   inca = LDA * ( nA SHIFT );
   incb = LDB * ( N  SHIFT );

   lA = inca * ( ( ATL_DivBySize( LCSIZE ) + nA*nA - 1 ) / ( nA * nA ) );
   lB = incb * ( ( ATL_DivBySize( LCSIZE ) + M * N - 1 ) / ( M   * N ) );

   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   B0 = (TYPE *)malloc( ATL_MulBySize( lB ) );

   if( ( A0 == NULL ) || ( B0 == NULL ) )
   { if( A0 ) free( A0 ); if( B0 ) free( B0 ); return( -1 ); }

   A = A0; stA = A0 + ( lA SHIFT );
   B = B0; stB = B0 + ( lB SHIFT );

   Aseed = nA * nA + 513 *  7 + 90;
   Bseed = M  * N  + 101 *  2 + 53;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   a = A; b = B;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );

   if( ROUT == TRMM )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_trmm( SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
      }
      ttrust = time00() - t0;
   }
   else /* if( ROUT == TRSM ) */
   {
      do { trddom( UPLO, nA, a, LDA ); a += inca; } while( a != stA ); a = A;

      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_trsm( SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   a = A; b = B;

   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   Mjoin( PATL, gegen )( lB, 1, B0, lB, Bseed );

   if( ROUT == TRMM )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_trmm(    SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
      }
      ttest = time00() - t0;
   }
   else /* if( ROUT == TRSM ) */
   {
      do { trddom( UPLO, nA, a, LDA ); a += inca; } while( a != stA ); a = A;

      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_trsm(    SIDE, UPLO, TRANS, DIAG, M, N, ALPHA, a, LDA, b, LDB );
         a += inca; if( a == stA ) { a = A; }
         b += incb; if( b == stB ) { b = B; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( B0 );
   free( A0 );

   return( passed );
}
/*
 * =====================================================================
 * Run functions
 * =====================================================================
 */
void RungemmCase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NTRANA,
   const enum ATLAS_TRANS     * TRANSA,
   const int                  NTRANB,
   const enum ATLAS_TRANS     * TRANSB,
   int                        M0,
   int                        MN,
   int                        MINC,
   int                        N0,
   int                        NN,
   int                        NINC,
   int                        K0,
   int                        KN,
   int                        KINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, k, kk, lda, ldb, ldc, m, mm, n,
                              nn, ta, tb, ksame=0, msame=0;
   char                       ctrana, ctranb;

   if( M0 == -1 ) { msame = 1; M0 = MN = MINC = NN; }
   if( K0 == -1 ) { ksame = 1; K0 = KN = KINC = NN; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "--------------------------------- ", "GEMM",
                   " ----------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# A B    M    N    K ALPHA  LDA  LDB  BETA",
                   "  LDC  TIME MFLOP SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = ==== ==== ==== ===== ==== ==== =====",
                   " ==== ===== ===== ==== =====\n" );
form = "%4d %c %c %4d %4d %4d %5.1f %4d %4d %5.1f %4d %5.2f %5.1f %4.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "----------------------------------- ", "GEMM",
                   " ---------------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# A B    M    N    K rALP iALP  LDA  LDB",
                   " rBET iBET  LDC TIME MFLOP SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = ==== ==== ==== ==== ==== ==== ====",
                   " ==== ==== ==== ==== ===== ==== =====\n" );
   form =
"%4d %c %c %4d %4d %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %4.1f %5.1f %4.2f %5s\n";
#endif

   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) { m = nn; } else { m = mm; }
         if( LDA_IS_M ) ldc = Mmax( 1, m ); else ldc = MN;

         for( kk = K0; kk <= KN; kk += KINC )
         {
            if( ksame ) { k = nn; } else { k = kk; }

            for( ta = 0; ta < NTRANA; ta++ )
            {
               if(      TRANSA[ta] == AtlasNoTrans   ) ctrana = 'N';
               else if( TRANSA[ta] == AtlasTrans     ) ctrana = 'T';
               else                                    ctrana = 'C';

               if(      TRANSA[ta] == AtlasNoTrans   )
               { if( LDA_IS_M ) lda = Mmax( 1, m ); else lda = MN; }
               else
               { if( LDA_IS_M ) lda = Mmax( 1, k ); else lda = KN; }

               for( tb = 0; tb < NTRANB; tb++ )
               {
                  if(      TRANSB[tb] == AtlasNoTrans   ) ctranb = 'N';
                  else if( TRANSB[tb] == AtlasTrans     ) ctranb = 'T';
                  else                                    ctranb = 'C';

                  if(      TRANSB[tb] == AtlasNoTrans   )
                  { if( LDA_IS_M ) ldb = Mmax( 1, k ); else ldb = KN; }
                  else
                  { if( LDA_IS_M ) ldb = Mmax( 1, n ); else ldb = NN; }

                  for( al = 0; al < NALPHA; al++ )
                  {
                     for( be = 0; be < NBETA; be++ )
                     {
                        int isam;
                        for (isam=0; isam < NSAMP; isam++)
                        {
#ifdef TREAL
                        ires = gemmcase( CACHESIZE, ROUT, TEST, MFLOP,
					 TRANSA[ta],
                                         TRANSB[tb], m, n, k, ALPHAS[al], lda,
                                         ldb, BETAS[be], ldc, EPSILON, &ttrust,
                                         &ttest, &mftrust, &mftest );
#else
                        ires = gemmcase( CACHESIZE, ROUT, TEST, MFLOP,
					 TRANSA[ta],
                                         TRANSB[tb], m, n, k, ALPHAS+2*al, lda,
                                         ldb, BETAS+2*be, ldc, EPSILON, &ttrust,
                                         &ttest, &mftrust, &mftest );
#endif
                        if(     !( TEST ) ) pass = "SKIP ";
                        else if( ires < 0 ) pass = "NoMEM";
                        else if( ires     ) pass = "PASS ";
                        else                pass = "FAIL ";

                        if( ires > 0 ) (*NPASSED)++;

                        if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                           t0 = mftest / mftrust;
                        else t0 = 0.0;
#ifdef TREAL
                        (void) fprintf( stdout, form, *NTESTS, ctrana, ctranb,
                                        m, n, k, ALPHAS[al], lda, ldb,
                                        BETAS[be], ldc, ttrust, mftrust, 1.0,
                                        "-----" );
                        (void) fprintf( stdout, form, *NTESTS, ctrana, ctranb,
                                        m, n, k, ALPHAS[al], lda, ldb,
                                        BETAS[be], ldc, ttest,  mftest,  t0,
                                        pass );
#else
                        (void) fprintf( stdout, form, *NTESTS, ctrana, ctranb,
                                        m, n, k,  ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, ldb, BETAS [2*be], BETAS [2*be+1],
                                        ldc, ttrust, mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, ctrana, ctranb,
                                        m, n, k,  ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, ldb, BETAS [2*be], BETAS [2*be+1],
                                        ldc, ttest,  mftest,  t0,  pass );
#endif
                        (*NTESTS)++;
                        }
                     }
                  }
               }
            }
         }
      }
   }
}

void RunsymmCase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NSIDE,
   const enum ATLAS_SIDE      * SIDES,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   int                        M0,
   int                        MN,
   int                        MINC,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, lda, ldb, ldc, m, msame=0, mm, n,
                              nn, si, up;
   char                       cside, cuplo;

   if( M0 == -1 ) { msame = 1; M0 = MN = MINC = NN; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "-------------------------------- ", "SYMM",
                   " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# S U    M    N ALPHA  LDA  LDB  BETA  LDC",
                   "   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = ==== ==== ===== ==== ==== ===== ====",
                   " ====== ===== ===== =====\n" );
   form = "%4d %c %c %4d %4d %5.1f %4d %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#else
   if( ROUT == SYMM )
   {
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------------ ", "SYMM",
                      " ------------------------------------" );
      (void) fprintf( stdout, "%s%s",
                      "TST# S U    M    N rALP iALP  LDA  LDB rBET iBET",
                      "  LDC   TIME MFLOP  SpUp  TEST\n" );
      (void) fprintf( stdout, "%s%s",
                      "==== = = ==== ==== ==== ==== ==== ==== ==== ====",
                      " ==== ====== ===== ===== =====\n" );
      form =
"%4d %c %c %4d %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %6.2f %5.1f %5.2f %5s\n";
   }
   else
   {
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------------ ", "HEMM",
                      " ------------------------------------" );
      (void) fprintf( stdout, "%s%s",
                      "TST# S U    M    N rALP iALP  LDA  LDB iBET rBET",
                      "  LDC   TIME MFLOP  SpUp  TEST\n" );
      (void) fprintf( stdout, "%s%s",
                      "==== = = ==== ==== ==== ==== ==== ==== ==== ====",
                      " ==== ====== ===== ===== =====\n" );
      form =
"%4d %c %c %4d %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %6.2f %5.1f %5.2f %5s\n";
   }
#endif
   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) { m = nn; } else { m = mm; }
         if( LDA_IS_M ) { ldb = ldc = Mmax( 1, m ); } else { ldb = ldc = MN; }

         for( si = 0; si < NSIDE; si++ )
         {
            if( SIDES[si] == AtlasLeft )
            { cside = 'L'; if( LDA_IS_M ) lda = Mmax( 1, m ); else lda = MN; }
            else
            { cside = 'R'; if( LDA_IS_M ) lda = Mmax( 1, n ); else lda = NN; }

            for( up = 0; up < NUPLO; up++ )
            {
               if( UPLOS[up] == AtlasLower ) cuplo = 'L';
               else                          cuplo = 'U';

               for( al = 0; al < NALPHA; al++ )
               {
                  for( be = 0; be < NBETA; be++ )
                  {
                     int isam;
                     for (isam=0; isam < NSAMP; isam++)
                     {
#ifdef TREAL
                     ires = symmcase( CACHESIZE, ROUT, TEST, MFLOP,
				      SIDES[si], UPLOS[up],
                                      m, n, ALPHAS[al], lda, ldb, BETAS[be],
                                      ldc, EPSILON, &ttrust, &ttest, &mftrust,
                                      &mftest );
#else
                     ires = symmcase( CACHESIZE, ROUT, TEST, MFLOP,
				      SIDES[si], UPLOS[up],
                                      m, n, ALPHAS+2*al, lda, ldb, BETAS+2*be,
                                      ldc, EPSILON, &ttrust, &ttest, &mftrust,
                                      &mftest );
#endif
                     if(     !( TEST ) ) pass = "SKIP ";
                     else if( ires < 0 ) pass = "NoMEM";
                     else if( ires     ) pass = "PASS ";
                     else                pass = "FAIL ";

                     if( ires > 0 ) (*NPASSED)++;

                     if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                        t0 = mftest / mftrust;
                     else t0 = 0.0;
#ifdef TREAL
                     (void) fprintf( stdout, form, *NTESTS, cside, cuplo, m, n,
                                     ALPHAS[al], lda, ldb, BETAS[be], ldc,
                                     ttrust, mftrust, 1.0, "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cside, cuplo, m, n,
                                     ALPHAS[al], lda, ldb, BETAS[be], ldc,
                                     ttest,  mftest,  t0,  pass    );
#else
                     (void) fprintf( stdout, form, *NTESTS, cside, cuplo, m, n,
                                     ALPHAS[2*al], ALPHAS[2*al+1], lda, ldb,
                                     BETAS[2*be], BETAS[2*be+1], ldc, ttrust,
                                     mftrust, 1.0, "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cside, cuplo, m, n,
                                     ALPHAS[2*al], ALPHAS[2*al+1], lda, ldb,
                                     BETAS[2*be], BETAS[2*be+1], ldc, ttest,
                                     mftest,  t0,  pass    );
#endif
                     (*NTESTS)++;
                     }
                  }
               }
            }
         }
      }
   }
}

void Runsyr2kCase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   int                        N0,
   int                        NN,
   int                        NINC,
   int                        K0,
   int                        KN,
   int                        KINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, k, kk, ksame=0, lda, ldb, ldc, n,
                              nn, up, ta;
   char                       ctran, cuplo;

   if( K0 == -1 ) { ksame = 1; K0 = KN = KINC = NN; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "-------------------------------- ", "SYR2K",
                   " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# U T    N    K ALPHA  LDA  LDB  BETA  LDC   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = ==== ==== ===== ==== ==== ===== ==== ======",
                   " ====== ===== =====\n" );
   form = "%4d %c %c %4d %4d %5.1f %4d %4d %5.1f %4d %6.2f %6.1f %5.2f %5s\n";
#else
   if( ROUT == SYR2K )
   {
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------------ ", "SYR2K",
                      " ------------------------------------" );
      (void) fprintf( stdout, "%s%s",
                      "TST# U T    N    K rALP iALP  LDA  LDB rBET iBET",
                      "  LDC   TIME  MFLOP  SpUp  TEST\n" );
      (void) fprintf( stdout, "%s%s",
                      "==== = = ==== ==== ==== ==== ==== ==== ==== ====",
                      " ==== ====== ====== ===== =====\n" );
      form =
"%4d %c %c %4d %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %6.2f %6.1f %5.2f %5s\n";
   }
   else
   {
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "----------------------------------- ", "HER2K",
                      " -----------------------------------" );
      (void) fprintf( stdout, "%s%s",
                      "TST# U T    N    K rALPH iALPH  LDA  LDB  BETA",
                      "  LDC   TIME  MFLOP  SpUp  TEST\n" );
      (void) fprintf( stdout, "%s%s",
                      "==== = = ==== ==== ===== ===== ==== ==== =====",
                      " ==== ====== ====== ===== =====\n" );
      form =
      "%4d %c %c %4d %4d %5.1f %5.1f %4d %4d %5.1f %4d %6.2f %6.1f %5.2f %5s\n";
   }
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn; if( LDA_IS_M ) ldc = Mmax( 1, n ); else ldc = NN;

      for( kk = K0; kk <= KN; kk += KINC )
      {
         if( ksame ) { k = nn; } else { k = kk; }

         for( up = 0; up < NUPLO; up++ )
         {
            if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
            else                          cuplo = 'L';

            for( ta = 0; ta < NTRAN; ta++ )
            {
#ifdef TREAL
               if( TRANS[ta] == AtlasNoTrans )
               {
                  ctran = 'N';
                  if( LDA_IS_M ) lda = ldb = n; else lda = ldb = NN;
               }
               else
               {
                  ctran = 'T';
                  if( LDA_IS_M ) lda = ldb = k; else lda = ldb = KN;
               }
#else
               if( ROUT == SYR2K )
               {
                  if( TRANS[ta] == AtlasNoTrans )
                  {
                     ctran = 'N';
                     if( LDA_IS_M ) lda = ldb = n; else lda = ldb = NN;
                  }
                  else
                  {
                     ctran = 'T';
                     if( LDA_IS_M ) lda = ldb = k; else lda = ldb = KN;
                  }
               }
               else
               {
                  if( TRANS[ta] == AtlasNoTrans )
                  {
                     ctran = 'N';
                     if( LDA_IS_M ) lda = ldb = n; else lda = ldb = NN;
                  }
                  else
                  {
                     ctran = 'C';
                     if( LDA_IS_M ) lda = ldb = k; else lda = ldb = KN;
                  }
               }
#endif
               for( al = 0; al < NALPHA; al++ )
               {
                  for( be = 0; be < NBETA; be++ )
                  {
                     int isam;
                     for (isam=0; isam < NSAMP; isam++)
                     {
#ifdef TREAL
                     ires = syr2kcase( CACHESIZE, ROUT, TEST, MFLOP,
				       UPLOS[up], TRANS[ta],
                                       n, k, ALPHAS[al], lda, ldb, BETAS[be],
                                       ldc, EPSILON, &ttrust, &ttest, &mftrust,
                                       &mftest );
#else
                     ires = syr2kcase( CACHESIZE, ROUT, TEST, MFLOP,
				       UPLOS[up], TRANS[ta],
                                       n, k, ALPHAS+2*al, lda, ldb, BETAS+2*be,
                                       ldc, EPSILON, &ttrust, &ttest, &mftrust,
                                       &mftest );
#endif
                     if(     !( TEST ) ) pass = "SKIP ";
                     else if( ires < 0 ) pass = "NoMEM";
                     else if( ires     ) pass = "PASS ";
                     else                pass = "FAIL ";

                     if( ires > 0 ) (*NPASSED)++;

                     if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                        t0 = mftest / mftrust;
                     else t0 = 0.0;
#ifdef TREAL
                     (void) fprintf( stdout, form, *NTESTS, cuplo, ctran, n, k,
                                     ALPHAS[al], lda, ldb, BETAS[be], ldc,
                                     ttrust, mftrust, 1.0, "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cuplo, ctran, n, k,
                                     ALPHAS[al], lda, ldb, BETAS[be], ldc,
                                     ttest, mftest, t0, pass );
#else
                     if( ROUT == SYR2K )
                     {
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, ldb, BETAS [2*be], BETAS [2*be+1],
                                        ldc, ttrust, mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, ldb, BETAS [2*be], BETAS [2*be+1],
                                        ldc, ttest, mftest, t0, pass );
                     }
                     else
                     {
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, ldb, BETAS [2*be], ldc, ttrust,
                                        mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, ldb, BETAS [2*be], ldc, ttest,
                                        mftest, t0, pass );
                     }
#endif
                     (*NTESTS)++;
                     }
                  }
               }
            }
         }
      }
   }
}

void RunsyrkCase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   int                        N0,
   int                        NN,
   int                        NINC,
   int                        K0,
   int                        KN,
   int                        KINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, k, kk, ksame=0, lda, ldc, n, nn,
                              up, ta;
   char                       ctran, cuplo;

   if( K0 == -1 ) { ksame = 1; K0 = KN = KINC = NN; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------ ", "SYRK",
                   " ------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# U T    N    K ALPHA  LDA  BETA  LDC   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = ==== ==== ===== ==== ===== ==== ======",
                   " ====== ===== =====\n" );
   form = "%4d %c %c %4d %4d %5.1f %4d %5.1f %4d %6.2f %6.1f %5.2f %5s\n";
#else
   if( ROUT == SYRK )
   {
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------------ ", "SYRK",
                      " ------------------------------------" );
      (void) fprintf( stdout, "%s%s",
                      "TST# U T    N    K rALPH iALPH  LDA rBETA iBETA",
                      "  LDC   TIME  MFLOP  SpUp  TEST\n" );
      (void) fprintf( stdout, "%s%s",
                      "==== = = ==== ==== ===== ===== ==== ===== =====",
                      " ==== ====== ====== ===== =====\n" );
      form =
    "%4d %c %c %4d %4d %5.1f %5.1f %4d %5.1f %5.1f %4d %6.2f %6.1f %5.2f %5s\n";
   }
   else
   {
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------ ", "HERK",
                      " ------------------------------" );
      (void) fprintf( stdout, "%s%s",
                      "TST# U T    N    K ALPHA  LDA  BETA",
                      "  LDC   TIME  MFLOP  SpUp  TEST\n" );
      (void) fprintf( stdout, "%s%s",
                      "==== = = ==== ==== ===== ==== =====",
                      " ==== ====== ====== ===== =====\n" );
      form = "%4d %c %c %4d %4d %5.1f %4d %5.1f %4d %6.2f %6.1f %5.2f %5s\n";
   }
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn; if( LDA_IS_M ) ldc = Mmax( 1, n ); else ldc = NN;

      for( kk = K0; kk <= KN; kk += KINC )
      {
         if( ksame ) { k = nn; } else { k = kk; }

         for( up = 0; up < NUPLO; up++ )
         {
            if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
            else                          cuplo = 'L';

            for( ta = 0; ta < NTRAN; ta++ )
            {
#ifdef TREAL
               if( TRANS[ta] == AtlasNoTrans )
               { ctran = 'N'; if( LDA_IS_M ) lda = n; else lda = NN; }
               else
               { ctran = 'T'; if( LDA_IS_M ) lda = k; else lda = KN; }
#else
               if( ROUT == SYRK )
               {
                  if( TRANS[ta] == AtlasNoTrans )
                  { ctran = 'N'; if( LDA_IS_M ) lda = n; else lda = NN; }
                  else
                  { ctran = 'T'; if( LDA_IS_M ) lda = k; else lda = KN; }
               }
               else
               {
                  if( TRANS[ta] == AtlasNoTrans )
                  { ctran = 'N'; if( LDA_IS_M ) lda = n; else lda = NN; }
                  else
                  { ctran = 'C'; if( LDA_IS_M ) lda = k; else lda = KN; }
               }
#endif
               for( al = 0; al < NALPHA; al++ )
               {
                  for( be = 0; be < NBETA; be++ )
                  {
                     int isam;
                     for (isam=0; isam < NSAMP; isam++)
                     {
#ifdef TREAL
                     ires = syrkcase( CACHESIZE, ROUT, TEST, MFLOP,
				      UPLOS[up], TRANS[ta],
                                      n, k, ALPHAS[al], lda, BETAS[be], ldc,
                                      EPSILON, &ttrust, &ttest, &mftrust,
                                      &mftest );
#else
                     ires = syrkcase( CACHESIZE, ROUT, TEST, MFLOP,
				      UPLOS[up], TRANS[ta],
                                      n, k, ALPHAS+2*al, lda, BETAS+2*be, ldc,
                                      EPSILON, &ttrust, &ttest, &mftrust,
                                      &mftest );
#endif
                     if(     !( TEST ) ) pass = "SKIP ";
                     else if( ires < 0 ) pass = "NoMEM";
                     else if( ires     ) pass = "PASS ";
                     else                pass = "FAIL ";

                     if( ires > 0 ) (*NPASSED)++;

                     if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                        t0 = mftest / mftrust;
                     else t0 = 0.0;
#ifdef TREAL
                     (void) fprintf( stdout, form, *NTESTS, cuplo, ctran, n, k,
                                     ALPHAS[al], lda, BETAS[be], ldc, ttrust,
                                     mftrust, 1.0, "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cuplo, ctran, n, k,
                                     ALPHAS[al], lda, BETAS[be], ldc, ttest,
                                     mftest, t0, pass );
#else
                     if( ROUT == SYRK )
                     {
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, BETAS [2*be], BETAS [2*be+1],
                                        ldc, ttrust, mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, BETAS [2*be], BETAS [2*be+1],
                                        ldc, ttest, mftest, t0, pass );
                     }
                     else
                     {
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], lda, BETAS[2*be],
                                        ldc, ttrust, mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        n, k, ALPHAS[2*al], lda, BETAS[2*be],
                                        ldc, ttest, mftest, t0, pass );
                     }
#endif
                     (*NTESTS)++;
                     }
                  }
               }
            }
         }
      }
   }
}

void RuntrxmCase
(
   const int                  CACHESIZE,
   const enum LVL3_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NSIDE,
   const enum ATLAS_SIDE      * SIDES,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   const int                  NDIAG,
   const enum ATLAS_DIAG      * DIAGS,
   int                        M0,
   int                        MN,
   int                        MINC,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, si, up, ta, di, ires, lda, ldb, m, mm,
                              msame=0, n, nn;
   char                       cside, ctran, cdiag, cuplo;

   if( M0 == -1 ) { msame = 1; M0 = MN = MINC = NN; }

#ifdef TREAL
   if( ROUT == TRMM )
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "---------------------------", " TRMM ",
                      "----------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "---------------------------", " TRSM ",
                      "----------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# S U T D    M    N ALPHA  LDA  LDB  TIME",
                   " MFLOP SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = = = ==== ==== ===== ==== ==== =====",
                   " ===== ==== =====\n" );
   form = "%4d %c %c %c %c %4d %4d %5.1f %4d %4d %5.2f %5.1f %4.2f %5s\n";
#else
   if( ROUT == TRMM )
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------", " TRMM ",
                      "-------------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "------------------------------", " TRSM ",
                      "-------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# S U T D    M    N rALPH iALPH  LDA  LDB  TIME",
                   " MFLOP SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = = = = ==== ==== ===== ===== ==== ==== =====",
                   " ===== ==== =====\n" );
   form = "%4d %c %c %c %c %4d %4d %5.1f %5.1f %4d %4d %5.2f %5.1f %4.2f %5s\n";
#endif
   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) { m = nn; } else { m = mm; }
         if( LDA_IS_M ) { ldb = Mmax( 1, m ); } else { ldb = MN; }

         for( si = 0; si < NSIDE; si++ )
         {
            if( SIDES[si] == AtlasLeft )
            { cside = 'L'; if( LDA_IS_M ) lda = Mmax( 1, m ); else lda = MN; }
            else
            { cside = 'R'; if( LDA_IS_M ) lda = Mmax( 1, n ); else lda = NN; }

            for( up = 0; up < NUPLO; up++ )
            {
               if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
               else                          cuplo = 'L';

               for( ta = 0; ta < NTRAN; ta++ )
               {
                  if(      TRANS[ta] == AtlasNoTrans   ) ctran = 'N';
                  else if( TRANS[ta] == AtlasTrans     ) ctran = 'T';
                  else                                   ctran = 'C';

                  for( di = 0; di < NDIAG; di++ )
                  {
                     if( DIAGS[di] == AtlasUnit ) cdiag = 'U';
                     else                         cdiag = 'N';

                     for( al = 0; al < NALPHA; al++ )
                     {
                        int isam;
                        for (isam=0; isam < NSAMP; isam++)
                        {
#ifdef TREAL
                        ires = trxmcase( CACHESIZE, ROUT, TEST, MFLOP,
					 SIDES[si],
                                         UPLOS[up], TRANS[ta], DIAGS[di],
                                         m, n, ALPHAS[al], lda, ldb, EPSILON,
                                         &ttrust, &ttest, &mftrust, &mftest );
#else
                        ires = trxmcase( CACHESIZE, ROUT, TEST, MFLOP,
					 SIDES[si],
                                         UPLOS[up], TRANS[ta], DIAGS[di],
                                         m, n, ALPHAS+2*al, lda, ldb, EPSILON,
                                         &ttrust, &ttest, &mftrust, &mftest );
#endif
                        if(     !( TEST ) ) pass = "SKIP ";
                        else if( ires < 0 ) pass = "NoMEM";
                        else if( ires     ) pass = "PASS ";
                        else                pass = "FAIL ";

                        if( ires > 0 ) (*NPASSED)++;

                        if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                           t0 = mftest / mftrust;
                        else t0 = 0.0;
#ifdef TREAL
                        (void) fprintf( stdout, form, *NTESTS, cside, cuplo,
                                        ctran, cdiag, m, n, ALPHAS[al], lda,
                                        ldb, ttrust, mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cside, cuplo,
                                        ctran, cdiag, m, n, ALPHAS[al], lda,
                                        ldb, ttest, mftest,  t0,  pass   );
#else
                        (void) fprintf( stdout, form, *NTESTS, cside, cuplo,
                                        ctran, cdiag, m, n, ALPHAS[2*al],
                                        ALPHAS[2*al+1], lda, ldb, ttrust,
                                        mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cside, cuplo,
                                        ctran, cdiag, m, n, ALPHAS[2*al],
                                        ALPHAS[2*al+1], lda, ldb, ttest,
                                        mftest,  t0,  pass    );
#endif
                        (*NTESTS)++;
                        }
                     }
                  }
               }
            }
         }
      }
   }
}

void RunCases
(
   const int                  TEST,
   const int                  CACHESIZE,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NSIDE,
   const enum ATLAS_SIDE      * SIDES,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRANA,
   const enum ATLAS_TRANS     * TRANSA,
   const int                  NTRANB,
   const enum ATLAS_TRANS     * TRANSB,
   const int                  NDIAG,
   const enum ATLAS_DIAG      * DIAGS,
   const int                  M0,
   const int                  MN,
   const int                  MINC,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  K0,
   const int                  KN,
   const int                  KINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const int                  NROUT,
   const enum LVL3_ROUT       * ROUTS
)
{
   TYPE                       eps;
   int                        ro, ntests=0, np=0;

   eps = Mjoin( PATL, epsilon )();

   for( ro = 0; ro < NROUT; ro++ )
   {
      if(      ROUTS[ro] == GEMM  )
      {
         RungemmCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NTRANA,
		      TRANSA, NTRANB,
                      TRANSB, M0, MN, MINC, N0, NN, NINC, K0, KN, KINC, NALPHA,
                      ALPHAS, NBETA, BETAS, eps, &np, &ntests );
      }
#ifdef TREAL
      else if(   ROUTS[ro] == SYMM )
#else
      else if( ( ROUTS[ro] == SYMM ) || ( ROUTS[ro] == HEMM ) )
#endif
      {
         RunsymmCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NSIDE,
		      SIDES, NUPLO,
                      UPLOS, M0, MN, MINC, N0, NN, NINC, NALPHA, ALPHAS, NBETA,
                      BETAS, eps, &np, &ntests );
      }
#ifdef TREAL
      else if(   ROUTS[ro] == SYRK )
#else
      else if( ( ROUTS[ro] == SYRK ) || ( ROUTS[ro] == HERK ) )
#endif
      {
         RunsyrkCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		      UPLOS, NTRANA,
                      TRANSA, N0, NN, NINC, K0, KN, KINC, NALPHA, ALPHAS, NBETA,
                      BETAS, eps, &np, &ntests );
      }
#ifdef TREAL
      else if(   ROUTS[ro] == SYR2K )
#else
      else if( ( ROUTS[ro] == SYR2K ) || ( ROUTS[ro] == HER2K ) )
#endif
      {
         Runsyr2kCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		       UPLOS, NTRANA,
                       TRANSA, N0, NN, NINC, K0, KN, KINC, NALPHA, ALPHAS,
                       NBETA, BETAS, eps, &np, &ntests );
      }
      else if( ( ROUTS[ro] == TRMM ) || ( ROUTS[ro] == TRSM ) )
      {
         RuntrxmCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NSIDE,
		      SIDES, NUPLO,
                      UPLOS, NTRANA, TRANSA, NDIAG, DIAGS, M0, MN, MINC, N0,
                      NN, NINC, NALPHA, ALPHAS, eps, &np, &ntests );
      }
   }

   if( TEST )
      (void) fprintf( stdout, "\n%d tests run, %d passed\n\n", ntests, np );
   else
      (void) fprintf( stdout, "\n%d tests run, all skipped\n\n", ntests );
}
/*
 * =====================================================================
 * Main functions
 * =====================================================================
 */
void PrintUsage( char * nam )
{
   (void) fprintf( stderr, "ATLAS usage:\n" );
   (void) fprintf( stderr, "    %s [-options ...]\n\n", nam );
   (void) fprintf( stderr, "where options include:\n" );

   (void) fprintf( stderr, "   -h                                  " );
   (void) fprintf( stderr, ". print this message                    \n" );

   (void) fprintf( stderr, "   -R <rout>                           " );
   (void) fprintf( stderr, ". select  one  or all routines to test. \n" );
   (void) fprintf( stderr, "                                       " );
#ifdef TREAL
   (void) fprintf( stderr, "  rout must be in {all, gemm,symm,syrk, \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  syr2k,trmm,trsm}.                     \n" );
#else
   (void) fprintf( stderr, "  rout must be in {all, gemm,hemm,herk, \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  her2k,symm,syr2k,syrk,trmm,trsm}.     \n" );
#endif
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -R gemm.  Ex: -R trmm        \n" );

   (void) fprintf( stderr, "   -R <nrout> <rout1> ... <routN>      " );
   (void) fprintf( stderr, ". same as above for more than one rout- \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  tine. Ex: -R 3 genn trmm symm         \n" );

   (void) fprintf( stderr, "   -S <nside>  L/R                     " );
   (void) fprintf( stderr, ". select values for the SIDE parameter. \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -S 1 L. Ex: -S 2 L R         \n" );

   (void) fprintf( stderr, "   -U <nuplo>  L/U                     " );
   (void) fprintf( stderr, ". select values for the UPLO parameter. \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -U 1 L. Ex: -U 2 L U         \n" );

   (void) fprintf( stderr, "   -A <ntrans> N/T/C                   " );
   (void) fprintf( stderr, ". select values of the TRANSA parameter.\n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -A 1 n. Ex: -A 2 N T         \n" );

   (void) fprintf( stderr, "   -B <ntrans> N/T/C                   " );
   (void) fprintf( stderr, ". select values of the TRANSB parameter.\n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -B 1 N. Ex: -B 2 N T         \n" );

   (void) fprintf( stderr, "   -D <ndiags> N/U                     " );
   (void) fprintf( stderr, ". select values for the DIAG parameter. \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -D 1 N. Ex: -Diag 2 N U      \n" );

   (void) fprintf( stderr, "   -m <m>                              " );
   (void) fprintf( stderr, ". select one value for the parameter M. \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Ex: -m 100                            \n" );

   (void) fprintf( stderr, "   -n <n>                              " );
   (void) fprintf( stderr, ". same as above for the parameter N.    \n" );

   (void) fprintf( stderr, "   -k <k>                              " );
   (void) fprintf( stderr, ". same as above for the parameter K.    \n" );

   (void) fprintf( stderr, "   -M <m1>  <mN>  <minc>               " );
   (void) fprintf( stderr, ". select the values of M, from m1 to mN \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  by increment of minc. m1 > 0.         \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Ex: -M 100 1000 100                   \n" );

   (void) fprintf( stderr, "   -N <n1>  <nN>  <ninc>               " );
   (void) fprintf( stderr, ". same as above for the values of N.    \n" );

   (void) fprintf( stderr, "   -K <k1>  <kN>  <kinc>               " );
   (void) fprintf( stderr, ". same as above for the values of K.    \n" );

#ifdef TREAL
   (void) fprintf( stderr, "   -a <nalphas> <a1> ... <aN>          " );
   (void) fprintf( stderr, ". select the values of ALPHA.  Default: \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  -a 1 1.0. Ex: -a 3 -1.0 0.0 1.0       \n" );
   (void) fprintf( stderr, "   -b <nbetas>  <beta1>  ... <betaN>   " );
   (void) fprintf( stderr, ". same as above for the parameter BETA. \n" );
#else
   (void) fprintf( stderr, "   -a <nalphas> <a1r> <a1i> ... <aNi>  " );
   (void) fprintf( stderr, ". select the values of ALPHA, where a1r \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  and  a1i  are the  real and imaginary \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  parts of a1. Default: -a 1 1.0 0.0    \n" );
   (void) fprintf( stderr, "   -b <nbetas>  <b1r> <b1i> ... <bNi>  " );
   (void) fprintf( stderr, ". same as above for the parameter BETA. \n" );
#endif

   (void) fprintf( stderr, "   -d                                  " );
   (void) fprintf( stderr, ". use smallest possible leading  dimen- \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  sion for the array A.                 \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: max( mN, nN, kN ).           \n" );

   (void) fprintf( stderr, "   -T <0/1>                            " );
   (void) fprintf( stderr, ". disable/enable computational check.   \n" );
   (void) fprintf( stderr, "                                       " );
   (void) fprintf( stderr, "  Default: -T 1                         \n" );

   (void) fprintf( stderr, "   -F <mflops>                         " );
   (void) fprintf( stderr, ". perform at least mflops per measure.  \n" );

   (void) fprintf( stderr, "   -C <CacheSize>                       " );
   (void) fprintf( stderr, ". select how much memory in kilobytes\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  to flush to clear the caches. Put 1\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  for no cache flushing. Default is the\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  size of the biggest cache.\n" );

   (void) fprintf( stderr, "\n" );
   exit( -1 );
}

void GetFlags
(
   int                        NARGS,
   char                       * ARGS[],
   int                        * NROUT,
   enum LVL3_ROUT             ** ROUTS,
   int                        * TEST,
   int                        * LDA_IS_M,
   int                        * CACHESIZE,
   int                        * MFLOP,
   int                        * NSIDE,
   enum ATLAS_SIDE            ** SIDES,
   int                        * NUPLO,
   enum ATLAS_UPLO            ** UPLOS,
   int                        * NTRANA,
   enum ATLAS_TRANS           ** TRANSA,
   int                        * NTRANB,
   enum ATLAS_TRANS           ** TRANSB,
   int                        * NDIAG,
   enum ATLAS_DIAG            ** DIAGS,
   int                        * M0,
   int                        * MN,
   int                        * MINC,
   int                        * N0,
   int                        * NN,
   int                        * NINC,
   int                        * K0,
   int                        * KN,
   int                        * KINC,
   int                        * NALPHA,
   TYPE                       ** ALPHAS,
   int                        * NBETA,
   TYPE                       ** BETAS
)
{
   char                       ch;
   int                        i = 1, j;
/*
 * Set up defaults
 */
   *NROUT    = -1;                         /* No routine to be tested */
   *TEST     = 1;                               /* Enable the testing */
   *LDA_IS_M = 0;    /* Leading dimension chosen as max testing value */
   *MFLOP    = 0;                /* smallest number of flops possible */
                                            /* Default bandwidth is 1 */
#ifdef L2SIZE
   *CACHESIZE = L2SIZE;             /* Size of largest cache to flush */
#else
   *CACHESIZE = 4*1024*1024;
#endif

   *NSIDE  = *NUPLO = *NTRANA = *NTRANB = *NDIAG = -1;
   *M0     = *N0    = *K0     = -1;
   *NALPHA = *NBETA = -1;

   fprintf(stdout, "\n\n");
   for (i=0; i < NARGS; i++) fprintf(stdout, "%s ", ARGS[i]);
   fprintf(stdout, "\n\n");
   for( i = 1; i < NARGS; )
   {
      if( ARGS[i][0] != '-' ) PrintUsage( ARGS[0] );

      switch( ARGS[i++][1] )
      {
         case 'h':
            PrintUsage( ARGS[0] );
            break;
         case 'T':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *TEST = atoi( ARGS[i++] );
            break;
         case 'd':
            *LDA_IS_M = 1;
            break;
         case '#':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            NSAMP = atoi( ARGS[i++] );
            if( NSAMP < 0      ) PrintUsage( ARGS[0] );
            break;
         case 'F':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *MFLOP = atoi( ARGS[i++] );
            if( *MFLOP < 0      ) PrintUsage( ARGS[0] );
            break;
         case 'C':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
	    *CACHESIZE = 1024*atoi(ARGS[i++]);
            break;
         case 'S':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NSIDE = atoi( ARGS[i++] );
            if( *NSIDE <= 0     ) PrintUsage( ARGS[0] );
            *SIDES = (enum ATLAS_SIDE *)malloc( *NSIDE *
                                                sizeof( enum ATLAS_SIDE ) );
            ATL_assert( *SIDES );
            for( j = 0; j != *NSIDE; j++)
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               ch = *ARGS[i++];
               if(      ch == 'l' || ch == 'L' ) (*SIDES)[j] = AtlasLeft;
               else if( ch == 'r' || ch == 'R' ) (*SIDES)[j] = AtlasRight;
               else PrintUsage( ARGS[0] );
            }
            break;
         case 'U':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NUPLO = atoi( ARGS[i++] );
            if( *NUPLO <= 0     ) PrintUsage( ARGS[0] );
            *UPLOS = (enum ATLAS_UPLO *)malloc( *NUPLO *
                                                sizeof( enum ATLAS_UPLO ) );
            ATL_assert( *UPLOS );
            for( j = 0; j != *NUPLO; j++)
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               ch = *ARGS[i++];
               if(      ch == 'u' || ch == 'U' ) (*UPLOS)[j] = AtlasUpper;
               else if( ch == 'l' || ch == 'L' ) (*UPLOS)[j] = AtlasLower;
               else PrintUsage( ARGS[0] );
            }
            break;
         case 'A':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NTRANA = atoi(ARGS[i++]);
            if( *NTRANA <= 0    ) PrintUsage( ARGS[0] );
            *TRANSA = (enum ATLAS_TRANS *)malloc( *NTRANA *
                                                  sizeof( enum ATLAS_TRANS ) );
            ATL_assert( *TRANSA );
            for( j = 0; j != *NTRANA; j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               ch = *ARGS[i++];
               if(      ch == 'n' || ch == 'N' ) (*TRANSA)[j] = AtlasNoTrans;
               else if( ch == 't' || ch == 'T' ) (*TRANSA)[j] = AtlasTrans;
               else if( ch == 'c' || ch == 'C' ) (*TRANSA)[j] = AtlasConjTrans;
               else PrintUsage( ARGS[0] );
            }
            break;
         case 'B':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NTRANB = atoi(ARGS[i++]);
            if( *NTRANB <= 0    ) PrintUsage( ARGS[0] );
            *TRANSB = (enum ATLAS_TRANS *)malloc( *NTRANB *
                                                  sizeof( enum ATLAS_TRANS ) );
            ATL_assert( *TRANSB );
            for( j = 0; j != *NTRANB; j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               ch = *ARGS[i++];
               if(      ch == 'n' || ch == 'N' ) (*TRANSB)[j] = AtlasNoTrans;
               else if( ch == 't' || ch == 'T' ) (*TRANSB)[j] = AtlasTrans;
               else if( ch == 'c' || ch == 'C' ) (*TRANSB)[j] = AtlasConjTrans;
               else PrintUsage( ARGS[0] );
            }
            break;
         case 'D':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NDIAG = atoi( ARGS[i++] );
            if( *NDIAG <= 0     ) PrintUsage( ARGS[0] );
            *DIAGS = (enum ATLAS_DIAG *)malloc( *NDIAG *
                                                sizeof( enum ATLAS_DIAG ) );
            ATL_assert( *DIAGS );
            for( j = 0; j != *NDIAG; j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               ch = *ARGS[i++];
               if(      ch == 'u' || ch == 'U' ) (*DIAGS)[j] = AtlasUnit;
               else if( ch == 'n' || ch == 'N' ) (*DIAGS)[j] = AtlasNonUnit;
               else PrintUsage( ARGS[0] );
            }
            break;

         case 'M':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *M0 = atoi( ARGS[i++] );
            if( *M0 < 0         ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *MN = atoi( ARGS[i++] );
            if( *MN < 0         ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *MINC = atoi( ARGS[i++] );
            if( *MINC <= 0      ) PrintUsage( ARGS[0] );
            break;
         case 'm':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *M0 = *MN = atoi( ARGS[i++] ); *MINC = 1;
            if( *M0 <= 0        ) PrintUsage( ARGS[0] );
            break;
         case 'N':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *N0 = atoi( ARGS[i++] );
            if( *N0 < 0         ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NN = atoi( ARGS[i++] );
            if( *NN < 0         ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NINC = atoi( ARGS[i++] );
            if( *NINC <= 0      ) PrintUsage( ARGS[0] );
            break;
         case 'n':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *N0 = *NN = atoi( ARGS[i++] ); *NINC = 1;
            if( *N0 < 0         ) PrintUsage( ARGS[0] );
            break;
         case 'K':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *K0 = atoi( ARGS[i++] );
            if( *K0 < 0         ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KN = atoi( ARGS[i++] );
            if( *KN < 0         ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KINC = atoi( ARGS[i++] );
            if( *KINC <= 0      ) PrintUsage( ARGS[0] );
            break;
         case 'k':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *K0 = *KN = atoi( ARGS[i++] ); *KINC = 1;
            if( *K0 < 0         ) PrintUsage( ARGS[0] );
            break;

         case 'a':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NALPHA = atoi( ARGS[i++] );
            if( *NALPHA <= 0    ) PrintUsage( ARGS[0] );
            *ALPHAS = (TYPE *)malloc( ATL_MulBySize( *NALPHA ) );
            ATL_assert( *ALPHAS );
            for( j = 0; j < (*NALPHA SHIFT); j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               (*ALPHAS)[j] = (TYPE)atof( ARGS[i++] );
            }
            break;
         case 'b':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NBETA  = atoi( ARGS[i++] );
            if( *NBETA <= 0     ) PrintUsage( ARGS[0] );
            *BETAS  = (TYPE *)malloc( ATL_MulBySize( *NBETA ) );
            ATL_assert( *BETAS );
            for( j = 0; j < (*NBETA SHIFT); j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               (*BETAS)[j] = (TYPE)atof( ARGS[i++] );
            }
            break;

         case 'R':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );

            if( ( strcmp( ARGS[i], "ALL"  ) == 0 ) ||
                ( strcmp( ARGS[i], "all"  ) == 0 ) )
            {
#ifdef TREAL
               *NROUT = 6;
#else
               *NROUT = 9;
#endif
               *ROUTS = (enum LVL3_ROUT *)malloc( (*NROUT) *
                                                  sizeof( enum LVL3_ROUT ) );
               ATL_assert( *ROUTS );

               (*ROUTS)[ 0] = GEMM; (*ROUTS)[ 1] = SYMM; (*ROUTS)[ 2] = SYR2K;
               (*ROUTS)[ 3] = SYRK; (*ROUTS)[ 4] = TRMM; (*ROUTS)[ 5] = TRSM;
#ifdef TCPLX
               (*ROUTS)[ 6] = HEMM; (*ROUTS)[ 7] = HERK; (*ROUTS)[ 8] = HER2K;
#endif
               i++;
            }
            else
            {
               if( isdigit( *ARGS[i] ) ) { *NROUT = atoi( ARGS[i++] ); }
               else                      { *NROUT = 1;                 }
               *ROUTS = (enum LVL3_ROUT *)malloc( (*NROUT) *
                                                  sizeof( enum LVL3_ROUT ) );
               ATL_assert( *ROUTS );

               for( j = 0; j < *NROUT; j++ )
               {
                  if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );

                  if(      ( strcmp( ARGS[i], "GEMM"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "gemm"  ) == 0 ) )
                     (*ROUTS)[j] = GEMM;
                  else if( ( strcmp( ARGS[i], "SYMM"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "symm"  ) == 0 ) )
                     (*ROUTS)[j] = SYMM;
                  else if( ( strcmp( ARGS[i], "SYR2K" ) == 0 ) ||
                           ( strcmp( ARGS[i], "syr2k" ) == 0 ) )
                     (*ROUTS)[j] = SYR2K;
                  else if( ( strcmp( ARGS[i], "SYRK"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "syrk"  ) == 0 ) )
                     (*ROUTS)[j] = SYRK;
                  else if( ( strcmp( ARGS[i], "TRMM"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "trmm"  ) == 0 ) )
                     (*ROUTS)[j] = TRMM;
                  else if( ( strcmp( ARGS[i], "TRSM"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "trsm"  ) == 0 ) )
                     (*ROUTS)[j] = TRSM;
#ifdef TCPLX
                  else if( ( strcmp( ARGS[i], "HEMM"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "hemm"  ) == 0 ) )
                     (*ROUTS)[j] = HEMM;
                  else if( ( strcmp( ARGS[i], "HER2K" ) == 0 ) ||
                           ( strcmp( ARGS[i], "her2k" ) == 0 ) )
                     (*ROUTS)[j] = HER2K;
                  else if( ( strcmp( ARGS[i], "HERK"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "herk"  ) == 0 ) )
                     (*ROUTS)[j] = HERK;
#endif
                  else PrintUsage( ARGS[0] );
                  i++;
               }
            }
            break;
         default:
            PrintUsage( ARGS[0] );
            break;
      }
   }
/*
 * Finish setting up defaults if the user has not selected
 */
   if( *NROUT == -1 )
   {
      *NROUT = 1;
      *ROUTS = (enum LVL3_ROUT *)malloc( sizeof( enum LVL3_ROUT ) );
      ATL_assert( *ROUTS );
      (*ROUTS)[0] = GEMM;
   }

   if( *NSIDE == -1 )
   {
      *NSIDE = 1;
      *SIDES = (enum ATLAS_SIDE *)malloc( sizeof( enum ATLAS_SIDE ) );
      ATL_assert( *SIDES );
      (*SIDES)[0] = AtlasLeft;
   }
   if( *NUPLO == -1 )
   {
      *NUPLO = 1;
      *UPLOS = (enum ATLAS_UPLO *)malloc( sizeof( enum ATLAS_UPLO ) );
      ATL_assert( *UPLOS );
      (*UPLOS)[0] = AtlasLower;
   }
   if( *NTRANA == -1 )
   {
      *NTRANA = 1;
      *TRANSA = (enum ATLAS_TRANS *)malloc( sizeof( enum ATLAS_TRANS ) );
      ATL_assert( *TRANSA );
      (*TRANSA)[0] = AtlasNoTrans;
   }
   if( *NTRANB == -1 )
   {
      *NTRANB = 1;
      *TRANSB = (enum ATLAS_TRANS *)malloc( sizeof( enum ATLAS_TRANS ) );
      ATL_assert( *TRANSB );
      (*TRANSB)[0] = AtlasNoTrans;
   }
   if( *NDIAG == -1 )
   {
      *NDIAG = 1;
      *DIAGS = (enum ATLAS_DIAG *)malloc( sizeof( enum ATLAS_DIAG ) );
      ATL_assert( *DIAGS );
      (*DIAGS)[0] = AtlasNonUnit;
   }

   if( *N0 == -1 ) { *N0 = 100; *NN = 1000; *NINC = 100; }

   if( *NALPHA == -1 )
   {
      *NALPHA = 1;
      *ALPHAS = (TYPE *)malloc( ATL_MulBySize( 1 ) );
      ATL_assert( *ALPHAS );
#ifdef TREAL
      (*ALPHAS)[0] = ATL_rone;
#else
      (*ALPHAS)[0] = ATL_rone;
      (*ALPHAS)[1] = ATL_rzero;
#endif
   }
   if( *NBETA == -1 )
   {
      *NBETA = 1;
      *BETAS = (TYPE *)malloc( ATL_MulBySize( 1 ) );
      ATL_assert( *BETAS );
#ifdef TREAL
      (*BETAS)[0] = ATL_rone;
#else
      (*BETAS)[0] = ATL_rone;
      (*BETAS)[1] = ATL_rzero;
#endif
   }
}

int main( int NARGS, char **ARGS )
{
   int                        kinc, kstart, kstop, ldaism, mflopmin, minc,
                              mstart, mstop, ninc, nstart, nstop, nalpha,
                              nbeta, ndiag, nrout, nside, ntrana, ntranb,
                              nuplo, test, cachesize;

   TYPE                       * alphas = NULL, * betas = NULL;
   enum LVL3_ROUT             * routs  = NULL;
   enum ATLAS_SIDE            * sides  = NULL;
   enum ATLAS_UPLO            * uplos  = NULL;
   enum ATLAS_TRANS           * transa = NULL, * transb = NULL;
   enum ATLAS_DIAG            * diags  = NULL;

   GetFlags( NARGS, ARGS, &nrout, &routs, &test, &ldaism, &cachesize,
	     &mflopmin, &nside, &sides, &nuplo, &uplos, &ntrana,
	     &transa, &ntranb, &transb, &ndiag, &diags, &mstart, &mstop,
	     &minc, &nstart, &nstop, &ninc, &kstart, &kstop, &kinc,
	     &nalpha, &alphas, &nbeta, &betas );
   RunCases( test, cachesize, mflopmin, ldaism, nside, sides, nuplo, uplos,
	     ntrana, transa, ntranb, transb, ndiag, diags, mstart,
	     mstop, minc, nstart, nstop, ninc, kstart, kstop, kinc,
	     nalpha, alphas, nbeta, betas, nrout, routs );

   if( sides  ) free( sides  );
   if( uplos  ) free( uplos  );
   if( transa ) free( transa );
   if( transb ) free( transb );
   if( diags  ) free( diags  );
   if( alphas ) free( alphas );
   if( betas  ) free( betas  );
   if( routs  ) free( routs  );

   return( 0 );
}
