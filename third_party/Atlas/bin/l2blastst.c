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
 *    USE_L2_REFERENCE : C ATLAS reference implementation,
 *
 * If none of these macros is defined at compile time, the  ATLAS imple-
 * mentation is to be tested against itself,  after all this is the only
 * version we are sure to have available.
 *
 * By default the mono-threaded  ATLAS  routines are tested. To test the
 * multi-threaded ATLAS routines, define the following macro:
 *    USE_L2_PTHREADS  : multi-threaded ATLAS implementation.
 */
#define USE_F77_BLAS

#ifdef ATL_USEPTHREADS
   #define USE_L2_PTHREADS
   #include "atlas_tlvl2.h"
#endif
/*
 * =====================================================================
 */
#if   defined( USE_F77_BLAS ) /* Trusted BLAS version to test against */
#define  TP2      Mjoin( PATL,   f77 )
#elif defined( USE_L2_REFERENCE )
#include "atlas_reflevel2.h"
#define  TP2      Mjoin( PATL,   ref )
#else /* defined( USE_L2_ATLAS ) */  /* use ATLAS itself !! (default) */
#include "atlas_level2.h"
#define  TP2      PATL
#endif

#define trusted_gbmv(   TA,      M, N, KL, KU, al, A, lA, X, iX, be, Y, iY) \
Mjoin( TP2, gbmv )(     TA,      M, N, KL, KU, al, A, lA, X, iX, be, Y, iY)
#define trusted_gemv(   TA,      M, N,         al, A, lA, X, iX, be, Y, iY) \
Mjoin( TP2, gemv )(     TA,      M, N,         al, A, lA, X, iX, be, Y, iY)

#ifdef TREAL
#define trusted_sbmv(UP,         N,    K,      al, A, lA, X, iX, be, Y, iY) \
Mjoin( TP2, sbmv )(  UP,         N,    K,      al, A, lA, X, iX, be, Y, iY)
#define trusted_spmv(UP,         N,            al, A,     X, iX, be, Y, iY) \
Mjoin( TP2, spmv )(  UP,         N,            al, A,     X, iX, be, Y, iY)
#define trusted_symv(UP,         N,            al, A, lA, X, iX, be, Y, iY) \
Mjoin( TP2, symv )(  UP,         N,            al, A, lA, X, iX, be, Y, iY)
#else
#define trusted_sbmv(UP,         N,    K,      al, A, lA, X, iX, be, Y, iY) \
Mjoin( TP2, hbmv )(  UP,         N,    K,      al, A, lA, X, iX, be, Y, iY)
#define trusted_spmv(UP,         N,            al, A,     X, iX, be, Y, iY) \
Mjoin( TP2, hpmv )(  UP,         N,            al, A,     X, iX, be, Y, iY)
#define trusted_symv(UP,         N,            al, A, lA, X, iX, be, Y, iY) \
Mjoin( TP2, hemv )(  UP,         N,            al, A, lA, X, iX, be, Y, iY)
#endif

#define trusted_tbmv(UP, TA, DI, N,    K,          A, lA, X, iX) \
Mjoin( TP2, tbmv )(  UP, TA, DI, N,    K,          A, lA, X, iX)
#define trusted_tpmv(UP, TA, DI, N,                A,     X, iX) \
Mjoin( TP2, tpmv )(  UP, TA, DI, N,                A,     X, iX)
#define trusted_trmv(UP, TA, DI, N,                A, lA, X, iX) \
Mjoin( TP2, trmv )(  UP, TA, DI, N,                A, lA, X, iX)

#define trusted_tbsv(UP, TA, DI, N,    K,          A, lA, X, iX) \
Mjoin( TP2, tbsv )(  UP, TA, DI, N,    K,          A, lA, X, iX)
#define trusted_tpsv(UP, TA, DI, N,                A,     X, iX) \
Mjoin( TP2, tpsv )(  UP, TA, DI, N,                A,     X, iX)
#define trusted_trsv(UP, TA, DI, N,                A, lA, X, iX) \
Mjoin( TP2, trsv )(  UP, TA, DI, N,                A, lA, X, iX)

#ifdef TREAL
   #define trusted_geru(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(TP2,ger)(M, N, al, X, iX, Y, iY, A, lA)
   #define trusted_gerc(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(TP2,ger)(M, N, al, X, iX, Y, iY, A, lA)
   #define trusted_ger2u(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      ger2_ger(0, M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
   #define trusted_ger2c(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      ger2_ger(0, M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
#else
   #define trusted_geru(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(TP2,geru)(M, N, al, X, iX, Y, iY, A, lA)
   #define trusted_gerc(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(TP2,gerc)(M, N, al, X, iX, Y, iY, A, lA)
   #define trusted_ger2c(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      ger2_ger(1, M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
   #define trusted_ger2u(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      ger2_ger(0, M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
#endif

#ifdef TREAL
#define  trusted_spr(UP,         N,            al, X, iX,            A    ) \
Mjoin( TP2, spr )(   UP,         N,            al, X, iX,            A    )
#define  trusted_syr(UP,         N,            al, X, iX,            A, lA) \
Mjoin( TP2, syr )(   UP,         N,            al, X, iX,            A, lA)
#else
#define  trusted_spr(UP,         N,            al, X, iX,            A    ) \
Mjoin( TP2, hpr )(   UP,         N,            al, X, iX,            A    )
#define  trusted_syr(UP,         N,            al, X, iX,            A, lA) \
Mjoin( TP2, her )(   UP,         N,            al, X, iX,            A, lA)
#endif

#ifdef TREAL
#define trusted_spr2(UP,         N,            al, X, iX, Y, iY,     A    ) \
Mjoin( TP2, spr2 )(  UP,         N,            al, X, iX, Y, iY,     A    )
#define trusted_syr2(UP,         N,            al, X, iX, Y, iY,     A, lA) \
Mjoin( TP2, syr2 )(  UP,         N,            al, X, iX, Y, iY,     A, lA)
#else
#define trusted_spr2(UP,         N,            al, X, iX, Y, iY,     A    ) \
Mjoin( TP2, hpr2 )(  UP,         N,            al, X, iX, Y, iY,     A    )
#define trusted_syr2(UP,         N,            al, X, iX, Y, iY,     A, lA) \
Mjoin( TP2, her2 )(  UP,         N,            al, X, iX, Y, iY,     A, lA)
#endif
/*
 * ATLAS version of the BLAS to test.
 */
#if defined( USE_L2_PTHREADS ) && !defined(ATL_MIKE)
   #include "atlas_pthreads.h"
   #include "atlas_tlvl2.h"
   #define  AP2 PATL
   #define  AP3 Mjoin(PATL, t)
#else
   #ifdef ATL_MIKE
      #include "atlas_pthreads.h"
   #endif
   #include "atlas_level2.h"
   #define  AP2 PATL
   #define  AP3 PATL
#endif

#define test_gbmv(      TA,      M, N, KL, KU, al, A, lA, X, iX, be, Y, iY) \
Mjoin( AP2, gbmv )(     TA,      M, N, KL, KU, al, A, lA, X, iX, be, Y, iY)
#define test_gemv(      TA,      M, N,         al, A, lA, X, iX, be, Y, iY) \
Mjoin( AP3, gemv )(     TA,      M, N,         al, A, lA, X, iX, be, Y, iY)

#ifdef TREAL
#define test_sbmv(   UP,         N,    K,      al, A, lA, X, iX, be, Y, iY) \
Mjoin( AP2, sbmv )(  UP,         N,    K,      al, A, lA, X, iX, be, Y, iY)
#define test_spmv(   UP,         N,            al, A,     X, iX, be, Y, iY) \
Mjoin( AP2, spmv )(  UP,         N,            al, A,     X, iX, be, Y, iY)
#define test_symv(   UP,         N,            al, A, lA, X, iX, be, Y, iY) \
Mjoin( AP2, symv )(  UP,         N,            al, A, lA, X, iX, be, Y, iY)
#else
#define test_sbmv(   UP,         N,    K,      al, A, lA, X, iX, be, Y, iY) \
Mjoin( AP2, hbmv )(  UP,         N,    K,      al, A, lA, X, iX, be, Y, iY)
#define test_spmv(   UP,         N,            al, A,     X, iX, be, Y, iY) \
Mjoin( AP2, hpmv )(  UP,         N,            al, A,     X, iX, be, Y, iY)
#define test_symv(   UP,         N,            al, A, lA, X, iX, be, Y, iY) \
Mjoin( AP2, hemv )(  UP,         N,            al, A, lA, X, iX, be, Y, iY)
#endif

#define test_tbmv(   UP, TA, DI, N,    K,          A, lA, X, iX) \
Mjoin( AP2, tbmv )(  UP, TA, DI, N,    K,          A, lA, X, iX)
#define test_tpmv(   UP, TA, DI, N,                A,     X, iX) \
Mjoin( AP2, tpmv )(  UP, TA, DI, N,                A,     X, iX)
#define test_trmv(   UP, TA, DI, N,                A, lA, X, iX) \
Mjoin( AP2, trmv )(  UP, TA, DI, N,                A, lA, X, iX)

#define test_tbsv(   UP, TA, DI, N,    K,          A, lA, X, iX) \
Mjoin( AP2, tbsv )(  UP, TA, DI, N,    K,          A, lA, X, iX)
#define test_tpsv(   UP, TA, DI, N,                A,     X, iX) \
Mjoin( AP2, tpsv )(  UP, TA, DI, N,                A,     X, iX)
#define test_trsv(   UP, TA, DI, N,                A, lA, X, iX) \
Mjoin( AP2, trsv )(  UP, TA, DI, N,                A, lA, X, iX)

#ifdef TREAL
   #define test_geru(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(AP3,ger)(M, N, al, X, iX, Y, iY, A, lA)
   #define test_gerc(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(AP3,ger)(M, N, al, X, iX, Y, iY, A, lA)
   #define test_ger2u(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      Mjoin(AP2,ger2)(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
   #define test_ger2c(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      Mjoin(AP2,ger2)(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
#else
   #define test_geru(M, N, al, X, iX, Y, iY, A, lA) \
      Mjoin(AP3,geru)(M, N, al, X, iX, Y, iY, A, lA)
   #define test_gerc(M, N,  al, X, iX, Y, iY, A, lA) \
      Mjoin(AP3,gerc)(M, N, al, X, iX, Y, iY, A, lA)
   #define test_ger2c(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      Mjoin(AP2,ger2c)(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
   #define test_ger2u(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA) \
      Mjoin(AP2,ger2u)(M, N, al, X, iX, Y, iY, beta, W, iW, Z, iZ, A, lA)
#endif

#ifdef TREAL
#define  test_spr(   UP,         N,            al, X, iX,            A    ) \
Mjoin( AP2, spr )(   UP,         N,            al, X, iX,            A    )
#define  test_syr(   UP,         N,            al, X, iX,            A, lA) \
Mjoin( AP2, syr )(   UP,         N,            al, X, iX,            A, lA)
#else
#define  test_spr(   UP,         N,            al, X, iX,            A    ) \
Mjoin( AP2, hpr )(   UP,         N,            al, X, iX,            A    )
#define  test_syr(   UP,         N,            al, X, iX,            A, lA) \
Mjoin( AP2, her )(   UP,         N,            al, X, iX,            A, lA)
#endif

#ifdef TREAL
#define test_spr2(   UP,         N,            al, X, iX, Y, iY,     A    ) \
Mjoin( AP2, spr2 )(  UP,         N,            al, X, iX, Y, iY,     A    )
#define test_syr2(   UP,         N,            al, X, iX, Y, iY,     A, lA) \
Mjoin( AP2, syr2 )(  UP,         N,            al, X, iX, Y, iY,     A, lA)
#else
#define test_spr2(   UP,         N,            al, X, iX, Y, iY,     A    ) \
Mjoin( AP2, hpr2 )(  UP,         N,            al, X, iX, Y, iY,     A    )
#define test_syr2(   UP,         N,            al, X, iX, Y, iY,     A, lA) \
Mjoin( AP2, her2 )(  UP,         N,            al, X, iX, Y, iY,     A, lA)
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
enum LVL2_ROUT /* 17 + 1 = 18 */
{
   GEMV=0, GBMV, SBMV, SPMV, SYMV, TBMV, TPMV, TRMV, TBSV, TPSV, TRSV,
   GERU,   GERC, GER2U, GER2C, SYR,  SPR,  SYR2, SPR2,
   ALLROUTS
};
/*
 * =====================================================================
 * Prototypes for the testing routines
 * =====================================================================
 */
double     opbl2
(  const enum LVL2_ROUT,           const int,      const int,
   const int,      const int );
void       tbddom
(  const enum ATLAS_UPLO,          const int,      const int,
   TYPE *,         const int );
void       tpddom
(  const enum ATLAS_UPLO,          const int,      TYPE * );
void       trddom
(  const enum ATLAS_UPLO,          const int,      TYPE *,
   const int );

TYPE       gmvtst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const enum ATLAS_TRANS,
   const int,      const int,      const int,      const int,
   const SCALAR,   const int,      const int,      const SCALAR,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       smvtst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const enum ATLAS_UPLO,
   const int,      const int,      const SCALAR,   const int,
   const int,      const SCALAR,   const int,      const TYPE,
   double *,       double *,       double *,       double * );
TYPE       tmvtst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,         const enum ATLAS_DIAG,
   const int,      const int,      const int,      const int,
   const TYPE,     double *,       double *,       double *,
   double * );
TYPE       tsvtst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const enum ATLAS_UPLO,
   const enum ATLAS_TRANS,         const enum ATLAS_DIAG,
   const int,      const int,      const int,      const int,
   const TYPE,     double *,       double *,       double *,
   double * );
TYPE       gr1tst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const SCALAR,   const int,      const int,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       sr1tst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const enum ATLAS_UPLO,
   const int,      const TYPE,     const int,      const int,
   const TYPE,     double *,       double *,       double *,
   double * );
TYPE       sr2tst
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const enum ATLAS_UPLO,
   const int,      const SCALAR,   const int,      const int,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );

int        gmvcase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const enum ATLAS_TRANS,         const int,      const int,
   const int,      const int,      const SCALAR,   const int,
   const int,      const SCALAR,   const int,      const TYPE,
   double *,       double *,       double *,       double * );
int        smvcase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const int,      const int,
   const SCALAR,   const int,      const int,      const SCALAR,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
int        tmvcase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const enum ATLAS_TRANS,
   const enum ATLAS_DIAG,          const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        tsvcase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const enum ATLAS_TRANS,
   const enum ATLAS_DIAG,          const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        gr1case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const SCALAR,   const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        sr1case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const int,      const TYPE,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        sr2case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const enum ATLAS_UPLO,          const int,      const SCALAR,
   const int,      const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );

void       RungbmCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_TRANS *,
   int,            int,            int,            int,
   int,            int,            int,            int,
   int,            int,            int,            int,
   const int,      const TYPE *,   const int,      const TYPE *,
   const int,      const int *,    const int,      const int *,
   const TYPE,     int *,          int * );
void       RungemCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_TRANS *,
   int,            int,            int,            int,
   int,            int,            const int,      const TYPE *,
   const int,      const TYPE *,   const int,      const int *,
   const int,      const int *,    const TYPE,     int *,
   int * );
void       RunsbCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   int,            int,            int,            int,
   int,            int,            int,            int,
   int,            const int,      const TYPE *,   const int,
   const TYPE *,   const int,      const int *,    const int,
   const int *,    const TYPE,     int *,          int * );
void       RunspCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const enum ATLAS_UPLO *,        const int,
   const int,      const int,      const int,      const TYPE *,
   const int,      const TYPE *,   const int,      const int *,
   const int,      const int *,    const TYPE,     int *,
   int * );
void       RunsyCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   int,            int,            int,            const int,
   const TYPE *,   const int,      const TYPE *,   const int,
   const int *,    const int,      const int *,    const TYPE,
   int *,          int * );
void       RuntbCase
(   const int CACHESIZE,
    const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   const int,      const enum ATLAS_TRANS *,       const int,
   const enum ATLAS_DIAG *,        int,            int,
   int,            int,            int,            int,
   int,            int,            int,            const int,
   const int *,    const TYPE,     int *,          int * );
void       RuntpCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const enum ATLAS_UPLO *,        const int,
   const enum ATLAS_TRANS *,       const int,      const enum ATLAS_DIAG *,
   const int,      const int,      const int,      const int,
   const int *,    const TYPE,     int *,          int * );
void       RuntrCase
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   const int,      const enum ATLAS_TRANS *,       const int,
   const enum ATLAS_DIAG *,        int,            int,
   int,            const int,      const int *,    const TYPE,
   int *,          int * );
void       Rungr1Case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      int,            int,            int,
   int,            int,            int,            const int,
   const TYPE *,   const int,      const int *,    const int,
   const int *,    const TYPE,     int *,          int * );
void       Runsp1Case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const enum ATLAS_UPLO *,        const int,
   const int,      const int,      const int,      const TYPE *,
   const int,      const int *,    const TYPE,     int *,
   int * );
void       Runsr1Case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   int,            int,            int,            const int,
   const TYPE *,   const int,      const int *,    const TYPE,
   int *,          int * );
void       Runsp2Case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const enum ATLAS_UPLO *,        const int,
   const int,      const int,      const int,      const TYPE *,
   const int,      const int *,    const int,      const int *,
   const TYPE,     int *,          int * );
void       Runsr2Case
(  const int CACHESIZE,
   const enum LVL2_ROUT,           const int,      const int,
   const int,      const int,      const enum ATLAS_UPLO *,
   int,            int,            int,            const int,
   const TYPE *,   const int,      const int *,    const int,
   const int *,    const TYPE,     int *,          int * );
void       RunCases
(  const int,      const int,      const int,      const int,
   const int,
   const enum ATLAS_UPLO *,        const int,      const enum ATLAS_TRANS *,
   const int,      const enum ATLAS_DIAG *,        const int,
   const int,      const int,      const int,      const int,
   const int,      const int,      const int,      const int,
   const int,      const int,      const int,      const int,
   const TYPE *,   const int,      const TYPE *,   const int,
   const int *,    const int,      const int *,    const int,
   const enum LVL2_ROUT * );

void       PrintUsage
(  char * );

void       GetFlags
(  int,            char **,        int *,          enum LVL2_ROUT **,
   int *,          int *,          int *,          int *,
   int *,          enum ATLAS_UPLO **,             int *,
   enum ATLAS_TRANS **,            int *,          enum ATLAS_DIAG **,
   int *,          int *,          int *,          int *,
   int *,          int *,          int *,          int *,
   int *,          int *,          int *,          int *,
   int *,          TYPE **,        int *,          TYPE **,
   int *,          int **,         int *,          int ** );

int        main
(  int,            char ** );
/*
 * =====================================================================
 */
void ger2_ger
   (ATL_INT CONJ, ATL_CINT M, ATL_CINT N, const SCALAR alpha,
    const TYPE *X, ATL_CINT incX, const TYPE *Y, ATL_CINT incY,
    const SCALAR beta, const TYPE *W, ATL_CINT incW, const TYPE *Z,
    ATL_CINT incZ, TYPE *A, ATL_CINT lda)
/*
 * Performs A <- alpha*X*Y + beta*W*Z using two calls to GER
 */
{
   #ifdef TREAL
      test_geru(M, N, alpha, X, incX, Y, incY, A, lda);
      test_geru(M, N,  beta, W, incW, Z, incZ, A, lda);
   #else
      if (CONJ)
      {
         test_gerc(M, N, alpha, X, incX, Y, incY, A, lda);
         test_gerc(M, N,  beta, W, incW, Z, incZ, A, lda);
      }
      else
      {
         test_geru(M, N, alpha, X, incX, Y, incY, A, lda);
         test_geru(M, N,  beta, W, incW, Z, incZ, A, lda);
      }
   #endif
}

double opbl2
(
   const enum LVL2_ROUT       ROUT,
   const int                  M,
   const int                  N,
   const int                  KKL,
   const int                  KKU
)
{
   int                        j;
   double                     i0, i1, adds = 0.0, em, en, ek, kl, ku,
                              muls = 0.0;

   if( ( M <= 0 ) || ( N <= 0 ) ) return( 0.0 );

   kl = (double)( Mmax( 0, Mmin( M - 1, KKL ) ) );
   ku = (double)( Mmax( 0, Mmin( N - 1, KKU ) ) );
   em = (double)(M);
   en = (double)(N);
   ek = kl;

   if(      ROUT == GEMV )
   {
      muls = em * ( en + 1.0 );
      adds = em * en;

   }
   else if( ROUT == GBMV )
   {
      for( j = 0; j < N; j++ )
      {
         i1 = (double)(j) + kl; i0 = em - 1.0; i1 = Mmin( i1, i0 );
         i0 = (double)(j) - ku; i0 = Mmax( i0, 0.0 );
         i1 -= i0 - 1;
         muls += Mmax( i1, 0.0 );
      }
      adds = muls;
/*
 *    muls = em * ( en + 1.0 ) - ( em - 1.0 - kl ) * ( em - kl ) / 2.0 -
 *                               ( en - 1.0 - ku ) * ( en - ku ) / 2.0;
 *    adds = em * ( en + 1.0 ) - ( em - 1.0 - kl ) * ( em - kl ) / 2.0 -
 *                               ( en - 1.0 - ku ) * ( en - ku ) / 2.0;
 */
   }
   else if( ( ROUT == SPMV ) || ( ROUT == SYMV ) )
   {                       /* ( ( ROUT == HPMV ) || ( ROUT == HEMV ) ) */
      muls = em * ( em + 1.0 );
      adds = em * em;
   }
   else if( ROUT == SBMV )
   {                                               /* ( ROUT == HBMV ) */
      muls = em * ( em + 1.0 ) - ( em - 1.0 - ek ) * ( em - ek );
      adds = em * em           - ( em - 1.0 - ek ) * ( em - ek );
   }
   else if( ( ROUT == TPMV ) || ( ROUT == TRMV ) )
   {
      muls = em * ( em + 1.0 ) / 2.0;
      adds = ( em - 1.0 ) * em / 2.0;
   }
   else if( ROUT == TBMV )
   {
      muls = em * ( em + 1.0 ) / 2.0 - ( em - ek - 1.0 ) * ( em - ek ) / 2.0;
      adds = ( em - 1.0 ) * em / 2.0 - ( em - ek - 1.0 ) * ( em - ek ) / 2.0;
   }
   else if( ( ROUT == TPSV ) || ( ROUT == TRSV ) )
   {
      muls = em * ( em + 1.0 ) / 2.0;
      adds = ( em - 1.0 ) * em / 2.0;
   }
   else if( ROUT == TBSV )
   {
      muls = em * ( em + 1.0 ) / 2.0 - ( em - ek - 1.0 ) * ( em - ek ) / 2.0;
      adds = ( em - 1.0 ) * em / 2.0 - ( em - ek - 1.0 ) * ( em - ek ) / 2.0;
   }
   else if( ( ROUT == GERC ) || ( ROUT == GERU ) ||
            ( ROUT == GER2C ) || ( ROUT == GER2U ) ) /* ( ROUT == GER  ) */
   {
      muls = em * en + Mmin( em, en );
      adds = em * en;
      if (ROUT == GER2C || ROUT == GER2U)
      {
         muls *= 2;
         adds *= 2;
      }
   }
   else if( ( ROUT == SPR ) || ( ROUT == SYR ) )
   {                         /* ( ( ROUT == HPR ) || ( ROUT == HER ) ) */
      muls = em * ( em + 1.0 ) / 2.0 + em;
      adds = em * ( em + 1.0 ) / 2.0;
   }
   else if( ( ROUT == SPR2 ) || ( ROUT == SYR2 ) )
   {                       /* ( ( ROUT == HPR2 ) || ( ROUT == HER2 ) ) */
      muls = em * ( em + 1.0 ) + 2.0 * em;
      adds = em * ( em + 1.0 );
   }
#ifdef TREAL
   return(       muls +       adds );
#else
   return( 6.0 * muls + 2.0 * adds );
#endif
}

void tbddom
(
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const int                  K,
   TYPE                       * A,
   const int                  LDA
)
{
/*
 * Scale strictly lower (resp. upper) part of triangular band matrix by 1 / N
 * to make it diagonally dominant.
 */
   int                        i, i0, i1, iaij, j, jaj, l, lda2 = ( LDA SHIFT );
   TYPE                       alpha;

   if( N <= 0 ) return;

   alpha = ATL_rone / (TYPE)(N);

   if( UPLO == AtlasUpper )
   {
      for( j = 0, jaj  = 0; j < N; j++, jaj += lda2 )
      {
         l       = K - j;
         i0      = ( j - K > 0 ? j - K : 0 );

         for( i = i0, iaij = ((l+i0) SHIFT)+jaj; i < j; i++, iaij += (1 SHIFT) )
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
      for( j = N-1, jaj = (N-1)*lda2; j >= 0; j--, jaj -= lda2 )
      {
         if( A[jaj  ] >= ATL_rzero ) A[jaj  ] += ATL_rone;
         else                        A[jaj  ] -= ATL_rone;
#ifdef TCPLX
         if( A[jaj+1] >= ATL_rzero ) A[jaj+1] += ATL_rone;
         else                        A[jaj+1] -= ATL_rone;
#endif
         i1   = ( N - 1 > j + K ? j + K : N - 1 );
         for( i  = j+1, iaij = (1 SHIFT)+jaj; i <= i1; i++, iaij += (1 SHIFT) )
         {
            A[iaij  ] *= alpha;
#ifdef TCPLX
            A[iaij+1] *= alpha;
#endif
         }
      }
   }
}

void tpddom
(
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   TYPE                       * A
)
{
/*
 * Scale strictly lower (resp. upper) part of triangular packed matrix by 1 / N
 * to make it diagonally dominant.
 */
   int                        i, iaij, j;
   TYPE                       alpha;

   if( N <= 0 ) return;

   alpha = ATL_rone / (TYPE)(N);

   if( UPLO == AtlasUpper )
   {
      for( j = 0, iaij= 0; j < N; j++ )
      {
         for( i = 0; i < j; i++, iaij += (1 SHIFT) )
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
         iaij += (1 SHIFT);
      }
   }
   else
   {
      for( j = N-1, iaij = ((((N-1)*(N+2)) >> 1) SHIFT); j >= 0; j-- )
      {
         if( A[iaij  ] >= ATL_rzero ) A[iaij  ] += ATL_rone;
         else                         A[iaij  ] -= ATL_rone;
#ifdef TCPLX
         if( A[iaij+1] >= ATL_rzero ) A[iaij+1] += ATL_rone;
         else                         A[iaij+1] -= ATL_rone;
#endif
         iaij += (1 SHIFT);
         for( i = j+1; i < N; i++, iaij += (1 SHIFT) )
         {
            A[iaij  ] *= alpha;
#ifdef TCPLX
            A[iaij+1] *= alpha;
#endif
         }
         iaij -= ( ( N - j ) << (1 SHIFT) ) + (1 SHIFT);
      }
   }
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
TYPE gmvtst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_TRANS     TRANS,
   const int                  M,
   const int                  N,
   const int                  KL,
   const int                  KU,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  INCX,
   const SCALAR               BETA,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normY, normX, resid;
   TYPE                       * A  = NULL, * X = NULL, * Y = NULL, * Y0,
                              * YD = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        nY, nX, Aseed, Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( ( M == 0 ) || ( N == 0 ) ) { return( ATL_rzero ); }

   if( TRANS == AtlasNoTrans ) { nY = M; nX = N; }
   else                        { nY = N; nX = M; }

   ops = opbl2( ROUT, M, N, KL, KU );
/*
 * Allocate L2 cache space, A, X, Y and Y0
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( LDA ) * N        );
   X  = (TYPE *)malloc( ATL_MulBySize( nX  ) * aincX     );
   Y  = (TYPE *)malloc( ATL_MulBySize( nY  ) * aincY * 2 );

   if( ( A == NULL ) || ( X == NULL ) || ( Y == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( 0 );
      if( A  ) free( A  );
      if( X  ) free( X  );
      if( Y  ) free( Y  );
      return( ATL_rnone );
   }

   Y0 = Y + nY * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Aseed = M * N + LDA;
   Xseed = nX  * aincX * 27 + 213;
   Yseed = nY  * aincY;

   if( ROUT == GBMV ) Mjoin( PATL, gegen )( KL+KU+1, N, A, LDA, Aseed );
   else               Mjoin( PATL, gegen )( M,      N, A, LDA, Aseed );
   Mjoin( PATL, gegen )( 1,  nX, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  nY, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1,  nY, Y0, aincY, Yseed );
/*
 * Compute the norm of Y for later use in testing
 */
   if( TEST )
   {
      normY = Mjoin( PATL, infnrm )( nY, Y, aincY );
      if( Mabs1( BETA ) > ATL_rone ) normY *= Mabs1( BETA  );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
   else { normY = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - nX ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - nY ) * INCY ) SHIFT );

   if( ROUT == GBMV )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_gbmv( TRANS, M, N, KL, KU, ALPHA, A, LDA, x, INCX, BETA, y,
                    INCY );
      ttrust = time00() - t0;
   }
   else
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_gemv( TRANS, M, N, ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - nX ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - nY ) * INCY ) SHIFT );

   if( ROUT == GBMV )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_gbmv( TRANS, M, N, KL, KU, ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttest = time00() - t0;
   }
   else
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_gemv( TRANS, M, N, ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   if( ROUT == GBMV )
      normA = Mjoin( PATL, gbnrm1 )( M, N, KL, KU, A, LDA );
   else
      normA = Mjoin( PATL, genrm1 )( M, N,         A, LDA );
   if( normA == ATL_rzero ) normA = ATL_rone;

   free( A  );

   normX = Mjoin( PATL, infnrm )( nX, X, aincX );
   if( Mabs1( ALPHA ) > ATL_rone ) normX *= Mabs1( ALPHA );
   if( normX == ATL_rzero ) normX = ATL_rone;

   free( X );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   YD = (TYPE *)malloc( ATL_MulBySize( nY ) );
   if( YD == NULL ) { free( Y ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( nY, Y, aincY, Y0, aincY, YD, 1 );

   normD = Mjoin( PATL, infnrm )( nY, YD, 1 );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     Mmax( normY, ATL_rone ) * EPSILON * Mmax( M, N ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normA=%f, normX=%f, normY=%f, eps=%e\n",
      resid, normD, normA, normX, normY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "Y_trusted", 1, nY, Y0, aincY );
      Mjoin( PATL, geprint )( "Y_test",    1, nY, Y,  aincY );
#endif
   }

   free( Y  );
   free( YD );

   return( resid );
}

TYPE smvtst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  INCX,
   const SCALAR               BETA,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normY, normX, resid;
   TYPE                       * A  = NULL, * X = NULL, * Y = NULL, * Y0,
                              * YD = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        lA, Aseed, Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( ROUT == SPMV ) lA = ( N * ( N + 1 ) ) >> 1;   /* ( ROUT == HPMV ) */
   else               lA = LDA * N;

   ops = opbl2( ROUT, N, N, K, K );
/*
 * Allocate L2 cache space, A, X, Y and Y0
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( lA )              );
   X  = (TYPE *)malloc( ATL_MulBySize( N  ) * aincX      );
   Y  = (TYPE *)malloc( ATL_MulBySize( N  ) * aincY * 2  );

   if( ( A == NULL ) || ( X == NULL ) || ( Y == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   Y0 = Y + N * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Aseed = N * N + LDA;
   Xseed = N * aincX * 27 + 213;
   Yseed = N * aincY;

   if(      ROUT == SBMV )       /* ( ROUT == HBMV ) */
      Mjoin( PATL, gegen )( K+1, N, A, LDA, Aseed );
   else if( ROUT == SPMV )       /* ( ROUT == HPMV ) */
      Mjoin( PATL, gegen )( lA,  1, A, lA,  Aseed );
   else /* if( ROUT == SYMV ) */ /* ( ROUT == HEMV ) */
      Mjoin( PATL, gegen )( N,   N, A, LDA, Aseed );

   Mjoin( PATL, gegen )( 1, N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1, N, Y0, aincY, Yseed );
/*
 * Compute the norm of Y for later use in testing
 */
   if( TEST )
   {
      normY = Mjoin( PATL, infnrm )( N, Y, aincY );
      if( Mabs1( BETA ) > ATL_rone ) normY *= Mabs1( BETA  );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
   else { normY = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - N ) * INCY ) SHIFT );

   if(      ROUT == SBMV ) /* ( ROUT == HBMV ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_sbmv( UPLO, N, K, ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttrust = time00() - t0;
   }
   else if( ROUT == SPMV ) /* ( ROUT == HPMV ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_spmv( UPLO, N,    ALPHA, A,      x, INCX, BETA, y, INCY );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == SYMV ) */ /* ( ROUT == HEMV ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_symv( UPLO, N,    ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   if(      ROUT == SBMV ) /* ( ROUT == HBMV ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_sbmv( UPLO, N, K, ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttest = time00() - t0;
   }
   else if( ROUT == SPMV ) /* ( ROUT == HPMV ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_spmv( UPLO, N,    ALPHA, A,      x, INCX, BETA, y, INCY );
      ttest = time00() - t0;
   }
   else /* if( ROUT == SYMV ) */ /* ( ROUT == HEMV ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_symv( UPLO, N,    ALPHA, A, LDA, x, INCX, BETA, y, INCY );
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check
 */
#ifdef TREAL
   if(      ROUT == SBMV ) normA = Mjoin( PATL, sbnrm )( UPLO, N, K, A, LDA );
   else if( ROUT == SPMV ) normA = Mjoin( PATL, spnrm )( UPLO, N,    A      );
   else                    normA = Mjoin( PATL, synrm )( UPLO, N,    A, LDA );
#else
   if(      ROUT == SBMV ) normA = Mjoin( PATL, hbnrm )( UPLO, N, K, A, LDA );
   else if( ROUT == SPMV ) normA = Mjoin( PATL, hpnrm )( UPLO, N,    A      );
   else                    normA = Mjoin( PATL, henrm )( UPLO, N,    A, LDA );
#endif
   if( normA == ATL_rzero ) normA = ATL_rone;

   free( A  );

   normX = Mjoin( PATL, infnrm )( N, X, aincX );
   if( Mabs1( ALPHA ) >  ATL_rone ) normX *= Mabs1( ALPHA );
   if( normX == ATL_rzero ) normX = ATL_rone;

   free( X  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   YD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( YD == NULL ) { free( Y ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, Y, aincY, Y0, aincY, YD, 1 );

   normD = Mjoin( PATL, infnrm )( N, YD, 1 );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     Mmax( normY, ATL_rone ) * EPSILON * (TYPE)(N) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normA=%f, normX=%f, normY=%f, eps=%e\n",
      resid, normD, normA, normX, normY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "Y_trusted", 1, N, Y0, aincY );
      Mjoin( PATL, geprint )( "Y_test",    1, N, Y,  aincY );
#endif
   }

   free( Y  );
   free( YD );

   return( resid );
}

TYPE tmvtst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  N,
   const int                  K,
   const int                  LDA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normX, resid;
   TYPE                       * A = NULL, * X = NULL, * X0, * XD = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        lA, Aseed, Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( ROUT == TPMV ) lA = ( N * ( N + 1 ) ) >> 1;
   else               lA = LDA * N;

   ops = opbl2( ROUT, N, N, K, K );
/*
 * Allocate L2 cache space, A, X and X0
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( lA )             );
   X  = (TYPE *)malloc( ATL_MulBySize( N  ) * aincX * 2 );

   if( ( A == NULL ) || ( X == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      return( ATL_rnone );
   }
   X0 = X + N * ( aincX SHIFT );
/*
 * Generate random operands
 */
   Aseed = N * N + LDA;
   Xseed = N * aincX * 27 + 213;

   if(      ROUT == TBMV ) Mjoin( PATL, gegen )( K+1, N, A,  LDA, Aseed );
   else if( ROUT == TPMV ) Mjoin( PATL, gegen )( lA,  1, A,  lA,  Aseed );
   else                    Mjoin( PATL, gegen )( N,   N, A,  LDA, Aseed );
   Mjoin( PATL, gegen )( 1, N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, X0, aincX, Xseed );
/*
 * Compute the norm of X for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( normX == ATL_rzero ) normX = ATL_rone;
   }
   else { normX = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );

   if(      ROUT == TBMV )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_tbmv( UPLO, TRANS, DIAG, N, K, A, LDA, x, INCX );
      ttrust = time00() - t0;
   }
   else if( ROUT == TPMV )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_tpmv( UPLO, TRANS, DIAG, N,    A,      x, INCX );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == TRMV ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_trmv( UPLO, TRANS, DIAG, N,    A, LDA, x, INCX );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );

   if(      ROUT == TBMV )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_tbmv( UPLO, TRANS, DIAG, N, K, A, LDA, x, INCX );
      ttest = time00() - t0;
   }
   else if( ROUT == TPMV )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_tpmv( UPLO, TRANS, DIAG, N,    A,      x, INCX );
      ttest = time00() - t0;
   }
   else /* if( ROUT == TRMV ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_trmv( UPLO, TRANS, DIAG, N,    A, LDA, x, INCX );
      ttest = time00() - t0;
   }
   if( ttest > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   if(      ROUT == TBMV )
      normA = Mjoin( PATL, tbnrm1 )( UPLO, DIAG, N, K, A, LDA );
   else if( ROUT == TPMV )
      normA = Mjoin( PATL, tpnrm1 )( UPLO, DIAG, N,    A      );
   else /* if( ROUT == TRMV ) */
      normA = Mjoin( PATL, trnrm1 )( UPLO, DIAG, N,    A, LDA );
   if( normA == ATL_rzero ) normA = ATL_rone;

   free( A  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   XD = (TYPE * )malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( X ); return( -1 ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );
   normD = Mjoin( PATL, infnrm )( N, XD, 1 );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     EPSILON * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf(stderr,
      "ERROR:   resid=%f, normD=%f, normA=%f, normX=%f, eps=%e\n",
      resid, normD, normA, normX, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
#endif
   }

   free( X  );
   free( XD );

   return( resid );
}

TYPE tsvtst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  N,
   const int                  K,
   const int                  LDA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normX, resid;
   TYPE                       * A = NULL, * X = NULL, * X0, * XD = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        lA, Aseed, Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( ROUT == TPSV ) lA = ( N * ( N + 1 ) ) >> 1;
   else               lA = LDA * N;

   ops = opbl2( ROUT, N, N, K, K );
/*
 * Allocate L2 cache space, A, X and X0
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE   *)malloc( ATL_MulBySize( lA )             );
   X  = (TYPE   *)malloc( ATL_MulBySize( N  ) * aincX * 2 );

   if( ( A == NULL ) || ( X == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      return( ATL_rnone );
   }
   X0 = X + N * ( aincX SHIFT );
/*
 * Generate random operands
 */
   Aseed = N * N + LDA;
   Xseed = N * aincX * 27 + 213;

   if(      ROUT == TBSV )
   {
      Mjoin( PATL, gegen )( K+1, N, A, LDA, Aseed );
      tbddom( UPLO, N, K, A, LDA );
   }
   else if( ROUT == TPSV )
   {
      Mjoin( PATL, gegen )( lA,  1, A, lA,  Aseed );
      tpddom( UPLO, N,    A      );
   }
   else
   {
      Mjoin( PATL, gegen )( N,   N, A, LDA, Aseed );
      trddom( UPLO, N,    A, LDA );
   }
   Mjoin( PATL, gegen )( 1, N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, X0, aincX, Xseed );
/*
 * Compute the norm of X for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( normX == ATL_rzero ) normX = ATL_rone;
   }
   else { normX = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );

   if(      ROUT == TBSV )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_tbsv( UPLO, TRANS, DIAG, N, K, A, LDA, x, INCX );
      ttrust = time00() - t0;
   }
   else if( ROUT == TPSV )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_tpsv( UPLO, TRANS, DIAG, N,    A,      x, INCX );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == TRSV ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_trsv( UPLO, TRANS, DIAG, N,    A, LDA, x, INCX );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );

   if(      ROUT == TBSV )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_tbsv( UPLO, TRANS, DIAG, N, K, A, LDA, x, INCX );
      ttest = time00() - t0;
   }
   else if( ROUT == TPSV )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_tpsv( UPLO, TRANS, DIAG, N,    A,      x, INCX );
      ttest = time00() - t0;
   }
   else /* if( ROUT == TRSV ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_trsv( UPLO, TRANS, DIAG, N,    A, LDA, x, INCX );
      ttest = time00() - t0;
   }
   if( ttest > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   if(      ROUT == TBSV )
      normA = Mjoin( PATL, tbnrm1 )( UPLO, DIAG, N, K, A, LDA );
   else if( ROUT == TPSV )
      normA = Mjoin( PATL, tpnrm1 )( UPLO, DIAG, N,    A      );
   else /* if( ROUT == TRSV ) */
      normA = Mjoin( PATL, trnrm1 )( UPLO, DIAG, N,    A, LDA );
   if( normA == ATL_rzero ) normA = ATL_rone;

   free( A  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   XD = (TYPE * )malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( X ); return( -1 ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );
   normD = Mjoin( PATL, infnrm )( N, XD, 1 );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     EPSILON * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf(stderr,
      "ERROR:   resid=%f, normD=%f, normA=%f, normX=%f, eps=%e\n",
      resid, normD, normA, normX, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
#endif
   }

   free( X  );
   free( XD );

   return( resid );
}

TYPE gr2tst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const SCALAR               BETA,
   const int                  INCW,
   const int                  INCZ,
   const int                  LDA,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normY, normX, resid;
   TYPE                       * A = NULL, * A0, * X = NULL, * Y = NULL, * x,
                              * y;
   TYPE *W, *Z, *w, *z;
   const int aincW = Mabs(INCW), aincZ = Mabs(INCZ);
   int Wseed, Zseed;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Aseed, Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( ( M == 0 ) || ( N == 0 ) ) { return( ATL_rzero ); }

   ops = opbl2( ROUT, M, N, 0, 0 );
/*
 * Allocate L2 cache space, A, A0, X and Y
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( LDA ) * N * 2 );
   X  = (TYPE *)malloc( ATL_MulBySize( M   ) * aincX );
   Y  = (TYPE *)malloc( ATL_MulBySize( N   ) * aincY );
   W  = (TYPE *)malloc( ATL_MulBySize( M   ) * aincW );
   Z  = (TYPE *)malloc( ATL_MulBySize( N   ) * aincZ );

   if(!A || !X || !Y || !W || !Z)
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      if( Y ) free( Y );
      if( W ) free( W );
      if( Z ) free( Z );
      return( ATL_rnone );
   }

   A0 = A + LDA * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = M * N + LDA;
   Xseed = M * aincX * 27 + 213;
   Yseed = N * aincY;

   Mjoin( PATL, gegen )( M, N, A,  LDA,   Aseed );
   Mjoin( PATL, gegen )( M, N, A0, LDA,   Aseed );
   Mjoin( PATL, gegen )( 1, M, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, Y,  aincY, Yseed );
   Wseed = 0x0F83888| Xseed;
   Zseed = 0xF2994732 | Yseed;
   Mjoin( PATL, gegen )( 1, M, W,  aincW, Wseed );
   Mjoin( PATL, gegen )( 1, N, Z,  aincZ, Zseed );
/*
 * Compute the norm of A for later use in testing
 */
   if( TEST )
   {
      normA = Mjoin( PATL, genrm1 )( M, N, A, LDA );
      if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
      if( Mabs1( BETA  ) > ATL_rone ) normA *= Mabs1( BETA  );
      if( normA == ATL_rzero ) normA = ATL_rone;
   }
   else { normA = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - M ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );
   w = W; if( INCW < 0 ) w = W + ( ( ( 1 - M ) * INCW ) SHIFT );
   z = Z; if( INCZ < 0 ) z = Z + ( ( ( 1 - N ) * INCZ ) SHIFT );

   if( ROUT == GER2C ) /* ( ROUT == GER ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_ger2c( M, N, ALPHA, x, INCX, y, INCY, BETA, w, INCW,
                     z, INCZ, A0, LDA );
      ttrust = time00() - t0;
   }
   else /* if ( ROUT == GERU ) */ /* ( ROUT == GER  ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_ger2u( M, N, ALPHA, x, INCX, y, INCY, BETA, w, INCW,
                     z, INCZ, A0, LDA );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - M ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );
   w = W; if( INCW < 0 ) w = W + ( ( ( 1 - M ) * INCW ) SHIFT );
   z = Z; if( INCZ < 0 ) z = Z + ( ( ( 1 - N ) * INCZ ) SHIFT );

   if( ROUT == GER2C ) /* ( ROUT == GER ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_ger2c( M, N, ALPHA, x, INCX, y, INCY, BETA, w, INCW,
                     z, INCZ, A, LDA );
      ttest = time00() - t0;
   }
   else /* if ( ROUT == GERU ) */ /* ( ROUT == GER  ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_ger2u( M, N, ALPHA, x, INCX, y, INCY, BETA, w, INCW,
                  z, INCZ, A, LDA );
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normX = Mjoin( PATL, infnrm )( M, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;
   free( X  );

   normY = Mjoin( PATL, infnrm )( N, Y, aincY );
   if( normY == ATL_rzero ) normY = ATL_rone;
   free( Y  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( M, N, A, LDA, A0, LDA );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     Mmax( normY, ATL_rone ) * EPSILON * Mmax( M, N ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:   resid=%f, normD=%f, normA=%f, normX=%f, normY=%f, eps=%e\n",
      resid, normD, normA, normX, normY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "A_trusted", M, N, A0, LDA );
      Mjoin( PATL, geprint )( "A_test",    M, N, A,  LDA );
#endif
   }

   free( A  );

   return( resid );
}
TYPE gr1tst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const int                  LDA,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normY, normX, resid;
   TYPE                       * A = NULL, * A0, * X = NULL, * Y = NULL, * x,
                              * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Aseed, Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( ( M == 0 ) || ( N == 0 ) ) { return( ATL_rzero ); }

   ops = opbl2( ROUT, M, N, 0, 0 );
/*
 * Allocate L2 cache space, A, A0, X and Y
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( LDA ) * N * 2 );
   X  = (TYPE *)malloc( ATL_MulBySize( M   ) * aincX );
   Y  = (TYPE *)malloc( ATL_MulBySize( N   ) * aincY );

   if( ( A == NULL ) || ( X == NULL ) || ( Y == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   A0 = A + LDA * ( N SHIFT );
/*
 * Generate random operands
 */
   Aseed = M * N + LDA;
   Xseed = M * aincX * 27 + 213;
   Yseed = N * aincY;

   Mjoin( PATL, gegen )( M, N, A,  LDA,   Aseed );
   Mjoin( PATL, gegen )( M, N, A0, LDA,   Aseed );
   Mjoin( PATL, gegen )( 1, M, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, Y,  aincY, Yseed );
/*
 * Compute the norm of A for later use in testing
 */
   if( TEST )
   {
      normA = Mjoin( PATL, genrm1 )( M, N, A, LDA );
      if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
      if( normA == ATL_rzero ) normA = ATL_rone;
   }
   else { normA = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - M ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   if( ROUT == GERC ) /* ( ROUT == GER ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_gerc( M, N, ALPHA, x, INCX, y, INCY, A0, LDA );
      ttrust = time00() - t0;
   }
   else /* if ( ROUT == GERU ) */ /* ( ROUT == GER  ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_geru( M, N, ALPHA, x, INCX, y, INCY, A0, LDA );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - M ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   if( ROUT == GERC ) /* ( ROUT == GER ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_gerc( M, N, ALPHA, x, INCX, y, INCY, A, LDA );
      ttest = time00() - t0;
   }
   else /* if ( ROUT == GERU ) */ /* ( ROUT == GER  ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_geru( M, N, ALPHA, x, INCX, y, INCY, A, LDA );
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normX = Mjoin( PATL, infnrm )( M, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;
   free( X  );

   normY = Mjoin( PATL, infnrm )( N, Y, aincY );
   if( normY == ATL_rzero ) normY = ATL_rone;
   free( Y  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   normD = Mjoin( PATL, gediffnrm1 )( M, N, A, LDA, A0, LDA );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     Mmax( normY, ATL_rone ) * EPSILON * Mmax( M, N ) );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:   resid=%f, normD=%f, normA=%f, normX=%f, normY=%f, eps=%e\n",
      resid, normD, normA, normX, normY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "A_trusted", M, N, A0, LDA );
      Mjoin( PATL, geprint )( "A_test",    M, N, A,  LDA );
#endif
   }

   free( A  );

   return( resid );
}

TYPE sr1tst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const TYPE                 ALPHA,
   const int                  INCX,
   const int                  LDA,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normX, resid;
   TYPE                       * A = NULL, * A0, * X = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        lA, Aseed, Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( ROUT == SPR ) lA = ( N * ( N + 1 ) ) >> 1; /* ( ROUT == HER ) */
   else              lA = LDA * N;

   ops = opbl2( ROUT, N, N, 0, 0 );
/*
 * Allocate L2 cache space, A, A0 and X
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A  = (TYPE *)malloc( ATL_MulBySize( lA ) * 2     );
   X  = (TYPE *)malloc( ATL_MulBySize( N  ) * aincX );

   if( ( A == NULL ) || ( X == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      return( ATL_rnone );
   }

   A0 = A + ( lA SHIFT );
/*
 * Generate random operands
 */
   Aseed = N * N + LDA;
   Xseed = N * aincX * 27 + 213;

   if(      ROUT == SPR ) /* ( ROUT == HPR ) */
   {
      Mjoin( PATL, gegen )( lA, 1, A,  lA,  Aseed );
      Mjoin( PATL, gegen )( lA, 1, A0, lA,  Aseed );
   }
   else /* if( ROUT == SYR ) */ /* ( ROUT == HER ) */
   {
      Mjoin( PATL, gegen )( N,  N, A,  LDA, Aseed );
      Mjoin( PATL, gegen )( N,  N, A0, LDA, Aseed );
   }
   Mjoin( PATL, gegen )( 1, N, X,  aincX, Xseed );
/*
 * Compute the norm of A for later use in testing
 */
   if( TEST )
   {
#ifdef TREAL
      if(      ROUT == SPR )
         normA = Mjoin( PATL, spnrm )( UPLO, N, A      );
      else /* if( ROUT == SYR ) */
         normA = Mjoin( PATL, synrm )( UPLO, N, A, LDA );
#else
      if(      ROUT == SPR )
         normA = Mjoin( PATL, hpnrm )( UPLO, N, A      );
      else /* if( ROUT == SYR ) */
         normA = Mjoin( PATL, henrm )( UPLO, N, A, LDA );
#endif
      if( Mabs( ALPHA ) > ATL_rone ) normA *= Mabs( ALPHA );
      if( normA == ATL_rzero ) normA = ATL_rone;
   }
   else { normA = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   if( ROUT == SPR ) /* ( ROUT == HPR ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_spr( UPLO, N, ALPHA,  x, INCX, A0      );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == SYR ) */ /* ( ROUT == HER ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_syr( UPLO, N, ALPHA,  x, INCX, A0, LDA );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   if( ROUT == SPR ) /* ( ROUT == HPR ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_spr( UPLO, N, ALPHA,  x, INCX, A      );
      ttest = time00() - t0;
   }
   else /* if( ROUT == SYR ) */ /* ( ROUT == HER ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_syr( UPLO, N, ALPHA,  x, INCX, A, LDA );
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normX = Mjoin( PATL, infnrm )( N, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;
   free( X  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   if(      ROUT == SPR ) /* ( ROUT == HPR ) */
      normD = Mjoin( PATL, gediffnrm1 )( lA, 1, A, lA,  A0, lA  );
   else /* if( ROUT == SYR ) */ /* ( ROUT == HER ) */
      normD = Mjoin( PATL, gediffnrm1 )( N,  N, A, LDA, A0, LDA );

   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     EPSILON * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:    resid=%f, normD=%f, normA=%f, normX=%f, eps=%e\n",
      resid, normD, normA, normX, EPSILON );
#ifdef ATLAS_DEBUG
      if(      ROUT == SPR )
      {
         Mjoin( PATL, geprint )( "A_trusted", lA, 1, A0, lA  );
         Mjoin( PATL, geprint )( "A_test",    lA, 1, A,  lA  );
      }
      else /* if( ROUT == SYR ) */
      {
         Mjoin( PATL, geprint )( "A_trusted", N,  N, A0, LDA );
         Mjoin( PATL, geprint )( "A_test",    N,  N, A,  LDA );
      }
#endif
   }

   free( A  );

   return( resid );
}

TYPE sr2tst
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const int                  LDA,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normA, normD, normY, normX, resid;
   TYPE                       * A = NULL, * A0, * X = NULL, * Y = NULL,
                              * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        lA, Aseed, Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( ROUT == SPR2 ) lA = ( N * ( N + 1 ) ) >> 1;
   else               lA = LDA * N;

   ops = opbl2( ROUT, N, N, 0, 0 );
/*
 * Allocate L2 cache space, A, A0, X and Y
 */
   if (CACHESIZE > 0)
      l2ret = ATL_flushcache( CACHESIZE );
   A = (TYPE *)malloc( ATL_MulBySize( lA ) * 2     );
   X = (TYPE *)malloc( ATL_MulBySize( N  ) * aincX );
   Y = (TYPE *)malloc( ATL_MulBySize( N  ) * aincY );

   if( ( A == NULL ) || ( X == NULL ) || ( Y == NULL ) )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( 0 );
      if( A ) free( A );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   A0 = A + ( lA SHIFT );
/*
 * Generate random operands
 */
   Aseed = N * N + LDA;
   Xseed = N * aincX * 27 + 213;
   Yseed = N * aincY;

   if(      ROUT == SPR2 )
   {
      Mjoin( PATL, gegen )( lA, 1, A,  lA,  Aseed );
      Mjoin( PATL, gegen )( lA, 1, A0, lA,  Aseed );
   }
   else /* if( ROUT == SYR2 ) */
   {
      Mjoin( PATL, gegen )( N,  N, A,  LDA, Aseed );
      Mjoin( PATL, gegen )( N,  N, A0, LDA, Aseed );
   }
   Mjoin( PATL, gegen )( 1, N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, Y,  aincY, Yseed );
/*
 * Compute the norm of A for later use in testing
 */
   if( TEST )
   {
#ifdef TREAL
      if(      ROUT == SPR2 )
         normA = Mjoin( PATL, spnrm )( UPLO, N, A      );
      else /* if( ROUT == SYR2 ) */
         normA = Mjoin( PATL, synrm )( UPLO, N, A, LDA );
#else
      if(      ROUT == SPR2 )
         normA = Mjoin( PATL, hpnrm )( UPLO, N, A      );
      else /* if( ROUT == HER2 ) */
         normA = Mjoin( PATL, henrm )( UPLO, N, A, LDA );
#endif
      if( Mabs1( ALPHA ) > ATL_rone ) normA *= Mabs1( ALPHA );
      if( normA == ATL_rzero ) normA = ATL_rone;
   }
   else { normA = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   if( ROUT == SPR2 )
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_spr2( UPLO, N, ALPHA, x, INCX, y, INCY, A0      );
      ttrust = time00() - t0;
   }
   else /* if( ROUT == SYR2 ) */
   {
      if (CACHESIZE > 0)
         l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_syr2( UPLO, N, ALPHA, x, INCX, y, INCY, A0, LDA );
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   if( ROUT == SPR2 )
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_spr2( UPLO, N, ALPHA, x, INCX, y, INCY, A      );
      ttest = time00() - t0;
   }
   else /* if( ROUT == SYR2 ) */
   {
      if (CACHESIZE > 0)
         l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_syr2( UPLO, N, ALPHA, x, INCX, y, INCY, A, LDA );
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   if (CACHESIZE > 0)
      l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( A ); free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normX = Mjoin( PATL, infnrm )( N, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;
   free( X  );

   normY = Mjoin( PATL, infnrm )( N, Y, aincY );
   if( normY == ATL_rzero ) normY = ATL_rone;
   free( Y  );
/*
 * Ensure the difference of the output operands is relatively tiny enough
 */
   if(      ROUT == SPR2 )
      normD = Mjoin( PATL, gediffnrm1 )( lA, 1, A, lA,  A0, lA  );
   else /* if( ROUT == SYR2 ) */
      normD = Mjoin( PATL, gediffnrm1 )( N,  N, A, LDA, A0, LDA );
   resid = normD / ( Mmax( normA, ATL_rone ) * Mmax( normX, ATL_rone ) *
                     Mmax( normY, ATL_rone ) * EPSILON * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:    resid=%f, normD=%f, normA=%f, normX=%f, normY=%f, eps=%e\n",
      resid, normD, normA, normX, normY, EPSILON );
#ifdef ATLAS_DEBUG
      if(      ROUT == SPR2 )
      {
         Mjoin( PATL, geprint )( "A_trusted", lA, 1, A0, lA  );
         Mjoin( PATL, geprint )( "A_test",    lA, 1, A,  lA  );
      }
      else /* if( ROUT == SYR2 ) */
      {
         Mjoin( PATL, geprint )( "A_trusted", N,  N, A0, LDA );
         Mjoin( PATL, geprint )( "A_test",    N,  N, A,  LDA );
      }
#endif
   }

   free( A  );

   return( resid );
}
/*
 * =====================================================================
 * case functions
 * =====================================================================
 */
int gmvcase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_TRANS     TRANS,
   const int                  M,
   const int                  N,
   const int                  KL,
   const int                  KU,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  INCX,
   const SCALAR               BETA,
   const int                  INCY,
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
   TYPE                       * a, * stA, * stX, * stY, * x, * y, * A,
                              * A0 = NULL, * X, * X0 = NULL, * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, inca, incx, incy, lX, lY, lA,
                              mA, nA, nY, nX, passed, Aseed, Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, M, N, KL, KU ) ) ) || ( TEST ) )
   {
      resid = gmvtst( CACHESIZE, ROUT, TEST, TRANS, M, N, KL, KU, ALPHA,
		      LDA, INCX, BETA, INCY, EPSILON, TTRUST0, TTEST0,
MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( ROUT == GBMV )
   {
      mA = KL + 1 + KU;
      nA = N;
      if( TRANS == AtlasNoTrans ) { nY = M; nX = N; }
      else                        { nY = N; nX = M; }
   }
   else
   {
      mA = M;
      nA = N;
      if( TRANS == AtlasNoTrans ) { nY = M; nX = N; }
      else                        { nY = N; nX = M; }
   }

   incy = INCY * ( nY  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( nX  SHIFT ), aincX = Mabs( INCX );
   inca = LDA  * ( nA SHIFT );

   lY = M * aincY * ( ( ATL_DivBySize( LCSIZE ) + nY      - 1 ) / nY          );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + nX      - 1 ) / nX          );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*mA*nA - 1 ) / ( mA * nA ) );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

#ifdef TREAL
   beta   =  BETA;
   nbeta  = -BETA;
#else
   *beta  =    *BETA; beta [1] =  BETA[1];
   *nbeta = -(*BETA); nbeta[1] = -BETA[1];
#endif

   Yseed = nY * aincY;
   Xseed = nX * aincX + 127 * 50 + 77;
   Aseed = N * M     + 513 *  7 + 90;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   bet = beta; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if( ROUT == GBMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_gbmv( TRANS, M, N, KL, KU, ALPHA, a, LDA, x, INCX,
                       (SCALAR)(bet), y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_gemv( TRANS, M, N,         ALPHA, a, LDA, x, INCX,
                       (SCALAR)(bet), y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   bet = beta; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if( ROUT == GBMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_gbmv( TRANS, M, N, KL, KU, ALPHA, a, LDA, x, INCX,
                    (SCALAR)(bet), y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_gemv( TRANS, M, N,         ALPHA, a, LDA, x, INCX,
                    (SCALAR)(bet), y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );
   free( A0 );

   return( passed );
}

int smvcase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const int                  LDA,
   const int                  INCX,
   const SCALAR               BETA,
   const int                  INCY,
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
   TYPE                       bet,   beta,    nbeta;
#else
   TYPE                       * bet, beta[2], nbeta[2];
#endif
   TYPE                       * a, * stA, * stX, * stY, * x, * y, * A,
                              * A0 = NULL, * X, * X0 = NULL, * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, inca, incx, incy, lX, lY, lA,
                              mA, nA, passed, Aseed, Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, N, N, K, K ) ) ) || ( TEST ) )
   {
      resid = smvtst( CACHESIZE, ROUT, TEST, UPLO, N, K, ALPHA, LDA, INCX,
		      BETA, INCY, EPSILON, TTRUST0, TTEST0, MFTRUST0,
		      MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if(      ROUT == SBMV ) { mA = K + 1;                  nA = N; }
   else if( ROUT == SPMV ) { mA = ( N * ( N + 1 ) ) >> 1; nA = 1; }
   else                    { mA = N;                      nA = N; }

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ); aincX = Mabs( INCX );
   if( ROUT == SPMV ) inca = ( mA SHIFT );
   else               inca = LDA  * ( nA SHIFT );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*mA*nA - 1 ) / ( mA * nA ) );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

#ifdef TREAL
   beta   =  BETA;
   nbeta  = -BETA;
#else
   *beta  =    *BETA; beta [1] =  BETA[1];
   *nbeta = -(*BETA); nbeta[1] = -BETA[1];
#endif

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;
   Aseed = N * N     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   bet = beta; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == SBMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_sbmv( UPLO, N, K, ALPHA, a, LDA, x, INCX, (SCALAR)(bet),
                       y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else if( ROUT == SPMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_spmv( UPLO, N, ALPHA, a, x, INCX, (SCALAR)(bet), y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_symv( UPLO, N,    ALPHA, a, LDA, x, INCX, (SCALAR)(bet),
                       y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   bet = beta; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == SBMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_sbmv( UPLO, N, K, ALPHA, a, LDA, x, INCX, (SCALAR)(bet),
                    y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else if( ROUT == SPMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_spmv( UPLO, N,    ALPHA, a, x, INCX, (SCALAR)(bet),
                    y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_symv( UPLO, N,    ALPHA, a, LDA, x, INCX, (SCALAR)(bet),
                    y, INCY );
         y += incy;
         if( y == stY )
         { y = Y; if( bet == beta ) bet = nbeta; else bet = beta; }
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );
   free( A0 );

   return( passed );
}

int tmvcase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  N,
   const int                  K,
   const int                  LDA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
   TYPE                       * a, * stA, * stX, * x, * A, * A0 = NULL, * X,
                              * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, inca, incx, lX, lA, mA, nA, passed,
                              Aseed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, N, N, K, K ) ) ) || ( TEST ) )
   {
      resid = tmvtst( CACHESIZE, ROUT, TEST, UPLO, TRANS, DIAG, N, K, LDA,
		      INCX, EPSILON, TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if(      ROUT == TBMV ) { mA = K + 1;                  nA = N; }
   else if( ROUT == TPMV ) { mA = ( N * ( N + 1 ) ) >> 1; nA = 1; }
   else                    { mA = N;                      nA = N; }

   incx = INCX * ( N  SHIFT ); aincX = Mabs( INCX );
   if( ROUT == TPMV ) inca = ( mA SHIFT );
   else               inca = LDA  * ( nA SHIFT );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*mA*nA - 1 ) / ( mA * nA ) );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );

   if( ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

   Xseed = N * aincX + 127 * 50 + 77;
   Aseed = N * N     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   x = X; a = A;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == TBMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_tbmv( UPLO, TRANS, DIAG, N, K, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else if( ROUT == TPMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_tpmv( UPLO, TRANS, DIAG, N, a, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_trmv( UPLO, TRANS, DIAG, N, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   x = X; a = A;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == TBMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_tbmv( UPLO, TRANS, DIAG, N, K, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else if( ROUT == TPMV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_tpmv( UPLO, TRANS, DIAG, N, a, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_trmv( UPLO, TRANS, DIAG, N, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );
   free( A0 );

   return( passed );
}

int tsvcase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  N,
   const int                  K,
   const int                  LDA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
   TYPE                       * a, * stA, * stX, * x, * A, * A0 = NULL, * X,
                              * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, inca, incx, lX, lA, mA, nA, passed,
                              Aseed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, N, N, K, K ) ) ) || ( TEST ) )
   {
      resid = tsvtst( CACHESIZE, ROUT, TEST, UPLO, TRANS, DIAG, N, K, LDA,
		      INCX, EPSILON, TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if(      ROUT == TBSV ) { mA = K + 1;                  nA = N; }
   else if( ROUT == TPSV ) { mA = ( N * ( N + 1 ) ) >> 1; nA = 1; }
   else                    { mA = N;                      nA = N; }

   incx = INCX * ( N  SHIFT ); aincX = Mabs( INCX );
   if( ROUT == TPSV ) inca = ( mA SHIFT );
   else               inca = LDA  * ( nA SHIFT );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*mA*nA - 1 ) / ( mA * nA ) );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );

   if( ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

   Xseed = N * aincX + 127 * 50 + 77;
   Aseed = N * N     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   a = A;

   if(      ROUT == TBSV )
   do { tbddom( UPLO, N, K, a, LDA ); a += inca; } while( a != stA );
   else if( ROUT == TPSV )
   do { tpddom( UPLO, N,    a      ); a += inca; } while( a != stA );
   else
   do { trddom( UPLO, N,    a, LDA ); a += inca; } while( a != stA );

   x = X; a = A;

   if(      ROUT == TBSV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_tbsv( UPLO, TRANS, DIAG, N, K, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else if( ROUT == TPSV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_tpsv( UPLO, TRANS, DIAG, N, a, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_trsv( UPLO, TRANS, DIAG, N, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   a = A;

   if(      ROUT == TBSV )
   do { tbddom( UPLO, N, K, a, LDA ); a += inca; } while( a != stA );
   else if( ROUT == TPSV )
   do { tpddom( UPLO, N,    a      ); a += inca; } while( a != stA );
   else
   do { trddom( UPLO, N,    a, LDA ); a += inca; } while( a != stA );

   x = X; a = A;

   if(      ROUT == TBSV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_tbsv( UPLO, TRANS, DIAG, N, K, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else if( ROUT == TPSV )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_tpsv( UPLO, TRANS, DIAG, N, a, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_trsv( UPLO, TRANS, DIAG, N, a, LDA, x, INCX );
         x += incx; if( x == stX ) { x = X; }
         a += inca; if( a == stA ) { a = A; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );
   free( A0 );

   return( passed );
}

int gr2case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const SCALAR               BETA,
   const int                  INCW,
   const int                  INCZ,
   const int                  LDA,
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
   TYPE                       alph,  alpha,    nalpha;
#else
   TYPE                       *alph, alpha[2], nalpha[2];
#endif
   TYPE                       * a, * stA, * stX, * stY, * x, * y, * A,
                              * A0 = NULL, * X, * X0 = NULL, * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, inca, incx, incy, lX, lY, lA,
                              passed, Aseed, Xseed, Yseed;
   int incw, aincW, incz, aincZ, lW, lZ, Wseed, Zseed;
   TYPE *W0, *Z0, *W, *Z, *w, *z, *stW, *stZ;
   #ifdef TREAL
      TYPE  bet,  beta,    nbeta;
   #else
      TYPE *bet, beta[2], nbeta[2];
   #endif

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, M, N, 0, 0 ) ) ) || ( TEST ) )
   {
      resid = gr2tst( CACHESIZE, ROUT, TEST, M, N, ALPHA, INCX, INCY, BETA,
                      INCW, INCZ, LDA, EPSILON, TTRUST0, TTEST0, MFTRUST0,
                      MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( M  SHIFT ), aincX = Mabs( INCX );
   inca = LDA  * ( N SHIFT );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N     - 1 ) / N         );
   lX = M * aincX * ( ( ATL_DivBySize( LCSIZE ) + M     - 1 ) / M         );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*M*N - 1 ) / ( M * N ) );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   incw = INCW * ( M  SHIFT ), aincW = Mabs( INCW );
   incz = INCZ * ( N  SHIFT ); aincZ = Mabs( INCZ );
   lW = M * aincW * ( ( ATL_DivBySize( LCSIZE ) + M     - 1 ) / M         );
   lZ = N * aincZ * ( ( ATL_DivBySize( LCSIZE ) + N     - 1 ) / N         );
   W0 = (TYPE *)malloc( ATL_MulBySize( lW ) );
   Z0 = (TYPE *)malloc( ATL_MulBySize( lZ ) );

   if( !Y0 || !X0 || !A0 || !W0 || !Z0)
   {
      if( W0 ) free( W0 );
      if( Z0 ) free( Z0 );
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   if( INCW < 1 ) { W = W0 + ( lW SHIFT ); stW = W0; }
   else           { W = W0; stW = W0 + ( lX SHIFT ); }
   if( INCZ < 1 ) { Z = Z0 + ( lZ SHIFT ); stZ = Z0; }
   else           { Z = Z0; stZ = Z0 + ( lY SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

#ifdef TREAL
   alpha  =  ALPHA;
   nalpha = -ALPHA;
#else
   *alpha  =  ALPHA[0]; alpha [1] =  ALPHA[1];
   *nalpha = -ALPHA[0]; nalpha[1] = -ALPHA[1];
#endif
#ifdef TREAL
   beta   =  BETA ;
   nbeta  = -BETA ;
#else
   *beta   =  BETA [0]; beta  [1] =  BETA [1];
   *nbeta  = -BETA [0]; nbeta [1] = -BETA [1];
#endif

   Yseed = N * aincY;
   Xseed = M * aincX + 127 * 50 + 77;
   Aseed = N * M     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   alph = alpha; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   bet = beta; z = Z; w = W;
   Wseed = lY | 0xAC39E; Zseed = 0xFF00003 | (Yseed+78);
   Mjoin( PATL, gegen )( lW, 1, W0, lW, Wseed );
   Mjoin( PATL, gegen )( lZ, 1, Z0, lZ, Zseed );

   if( ROUT == GER2U )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_ger2u( M, N, (SCALAR)(alph), x, INCX, y, INCY,
                        (SCALAR)(bet), w, INCW, z, INCZ, a, LDA );
         w += incw; if( w == stW ) { w = W; }
         z += incz; if( z == stZ ) { z = Z; }
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         {
            a = A;
            if( alph == alpha ) alph = nalpha;
            else alph = alpha;
            if( bet  == beta  ) bet  = nbeta ;
            else bet  = beta ;
         }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_ger2u( M, N, (SCALAR)(alph), x, INCX, y, INCY,
                        (SCALAR)(bet), w, INCW, z, INCZ, a, LDA );
         w += incw; if( w == stW ) { w = W; }
         z += incz; if( z == stZ ) { z = Z; }
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         {
            a = A;
            if( alph == alpha ) alph = nalpha;
            else alph = alpha;
            if( bet  == beta  ) bet  = nbeta ;
            else bet  = beta ;
         }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   alph = alpha; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );
   bet = beta; z = Z; w = W;
   Mjoin( PATL, gegen )( lW, 1, W0, lW, Wseed );
   Mjoin( PATL, gegen )( lZ, 1, Z0, lZ, Zseed );

   if( ROUT == GERU )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_ger2u( M, N, (SCALAR)(alph), x, INCX, y, INCY,
                     (SCALAR)(bet), w, INCW, z, INCZ, a, LDA );
         w += incw; if( w == stW ) { w = W; }
         z += incz; if( z == stZ ) { z = Z; }
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         {
            a = A;
            alph = (alph == alpha) ? nalpha : alpha;
            bet = (bet == beta) ? nbeta : beta;
         }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_ger2c( M, N, (SCALAR)(alph), x, INCX, y, INCY,
                     (SCALAR)(bet), w, INCW, z, INCZ, a, LDA );
         w += incw; if( w == stW ) { w = W; }
         z += incz; if( z == stZ ) { z = Z; }
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         {
            a = A;
            alph = (alph == alpha) ? nalpha : alpha;
            bet = (bet == beta) ? nbeta : beta;
         }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Z0 );
   free( W0 );
   free( Y0 );
   free( X0 );
   free( A0 );

   return( passed );
}
int gr1case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const int                  LDA,
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
   TYPE                       alph,  alpha,    nalpha;
#else
   TYPE                       *alph, alpha[2], nalpha[2];
#endif
   TYPE                       * a, * stA, * stX, * stY, * x, * y, * A,
                              * A0 = NULL, * X, * X0 = NULL, * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, inca, incx, incy, lX, lY, lA,
                              passed, Aseed, Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, M, N, 0, 0 ) ) ) || ( TEST ) )
   {
      resid = gr1tst( CACHESIZE, ROUT, TEST, M, N, ALPHA, INCX, INCY, LDA,
		      EPSILON, TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( M  SHIFT ), aincX = Mabs( INCX );
   inca = LDA  * ( N SHIFT );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N     - 1 ) / N         );
   lX = M * aincX * ( ( ATL_DivBySize( LCSIZE ) + M     - 1 ) / M         );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*M*N - 1 ) / ( M * N ) );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );
   if( ( Y0 == NULL ) || ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

#ifdef TREAL
   alpha  =  ALPHA;
   nalpha = -ALPHA;
#else
   *alpha  =  ALPHA[0]; alpha [1] =  ALPHA[1];
   *nalpha = -ALPHA[0]; nalpha[1] = -ALPHA[1];
#endif

   Yseed = N * aincY;
   Xseed = M * aincX + 127 * 50 + 77;
   Aseed = N * M     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   alph = alpha; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if( ROUT == GERU )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_geru( M, N, (SCALAR)(alph), x, INCX, y, INCY, a, LDA );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_gerc( M, N, (SCALAR)(alph), x, INCX, y, INCY, a, LDA );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   alph = alpha; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if( ROUT == GERU )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_geru( M, N, (SCALAR)(alph), x, INCX, y, INCY, a, LDA );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_gerc( M, N, (SCALAR)(alph), x, INCX, y, INCY, a, LDA );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );
   free( A0 );

   return( passed );
}

int sr1case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const TYPE                 ALPHA,
   const int                  INCX,
   const int                  LDA,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       alph,  alpha, nalpha, resid = ATL_rzero;
   TYPE                       * a, * stA, * stX, * x, * A, * A0 = NULL, * X,
                              * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, inca, incx, lX, lA, mA, nA, passed,
                              Aseed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, N, N, 0, 0 ) ) ) || ( TEST ) )
   {
      resid = sr1tst( CACHESIZE, ROUT, TEST, UPLO, N, ALPHA, INCX, LDA,
		     EPSILON, TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( ROUT == SPR ) { mA = ( N * ( N + 1 ) ) >> 1; nA = 1; }
   else              { mA = N;                      nA = N; }

   incx = INCX * ( N  SHIFT ); aincX = Mabs( INCX );
   if( ROUT == SPR ) inca = ( mA SHIFT );
   else              inca = LDA  * ( nA SHIFT );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*mA*nA - 1 ) / ( mA * nA ) );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );

   if( ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

   alpha  =  ALPHA;
   nalpha = -ALPHA;

   Xseed = N * aincX + 127 * 50 + 77;
   Aseed = N * N     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   alph = alpha; x = X; a = A;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == SPR )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
#ifdef TREAL
         trusted_spr( UPLO, N, (SCALAR)(alph), x, INCX, a      );
#else
         trusted_spr( UPLO, N, (TYPE  )(alph), x, INCX, a      );
#endif
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
#ifdef TREAL
         trusted_syr( UPLO, N, (SCALAR)(alph), x, INCX, a, LDA );
#else
         trusted_syr( UPLO, N, (TYPE  )(alph), x, INCX, a, LDA );
#endif
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   alph = alpha; x = X; a = A;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == SPR )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
#ifdef TREAL
         test_spr( UPLO, N, (SCALAR)(alph), x, INCX, a      );
#else
         test_spr( UPLO, N, (TYPE  )(alph), x, INCX, a      );
#endif
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
#ifdef TREAL
         test_syr( UPLO, N, (SCALAR)(alph), x, INCX, a, LDA );
#else
         test_syr( UPLO, N, (TYPE  )(alph), x, INCX, a, LDA );
#endif
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );
   free( A0 );

   return( passed );
}

int sr2case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const int                  LDA,
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
   TYPE                       alph,  alpha,    nalpha;
#else
   TYPE                       *alph, alpha[2], nalpha[2];
#endif
   TYPE                       * a, * stA, * stX, * stY, * x, * y, * A,
                              * A0 = NULL, * X, * X0 = NULL, * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, inca, incx, incy, lX, lY, lA,
                              mA, nA, passed, Aseed, Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl2( ROUT, N, N, 0, 0 ) ) ) || ( TEST ) )
   {
      resid = sr2tst( CACHESIZE, ROUT, TEST, UPLO, N, ALPHA, INCX, INCY, LDA,
                      EPSILON, TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   if( ROUT == SPR2 ) { mA = ( N * ( N + 1 ) ) >> 1; nA = 1; }
   else               { mA = N;                      nA = N; }

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ); aincX = Mabs( INCX );
   if( ROUT == SPR2 ) inca = ( mA SHIFT );
   else               inca = LDA  * ( nA SHIFT );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N       - 1 ) / N           );
   lA =     inca  * ( ( ATL_DivBySize( LCSIZE ) + 2*mA*nA - 1 ) / ( mA * nA ) );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );
   A0 = (TYPE *)malloc( ATL_MulBySize( lA ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) || ( A0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      if( A0 ) free( A0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }
   A = A0; stA = A0 + ( lA SHIFT );

#ifdef TREAL
   alpha   =  ALPHA;
   nalpha  = -ALPHA;
#else
   *alpha  =  (*ALPHA); alpha [1] =  ALPHA[1];
   *nalpha = -(*ALPHA); nalpha[1] = -ALPHA[1];
#endif

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;
   Aseed = N * N     + 513 *  7 + 90;

   reps = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   alph = alpha; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == SPR2 )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_spr2( UPLO, N, (SCALAR)(alph), x, INCX, y, INCY, a      );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttrust = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_syr2( UPLO, N, (SCALAR)(alph), x, INCX, y, INCY, a, LDA );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttrust = time00() - t0;
   }
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   alph = alpha; y = Y; x = X; a = A;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );
   Mjoin( PATL, gegen )( lA, 1, A0, lA, Aseed );

   if(      ROUT == SPR2 )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_spr2( UPLO, N, (SCALAR)(alph), x, INCX, y, INCY, a      );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttest = time00() - t0;
   }
   else
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_syr2( UPLO, N, (SCALAR)(alph), x, INCX, y, INCY, a, LDA );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
         a += inca;
         if( a == stA )
         { a = A; if( alph == alpha ) alph = nalpha; else alph = alpha; }
      }
      ttest = time00() - t0;
   }
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );
   free( A0 );

   return( passed );
}

void RungbmCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   int                        M0,
   int                        MN,
   int                        MINC,
   int                        N0,
   int                        NN,
   int                        NINC,
   int                        KL0,
   int                        KLN,
   int                        KLINC,
   int                        KU0,
   int                        KUN,
   int                        KUINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, ix, iy, kkl, kku, lda, m, mm, n,
                              nn, ta, msame=0;
   char                       ctran;

   if( M0 == -1 ) { M0 = MN = MINC = NN; msame = 1; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "---------------------------------- ", "GBMV",
                   " -----------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# T    M    N   KL   KU ALPHA  LDA INCX  BETA",
                   " INCY  TIME MFLOP SpUp TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = ==== ==== ==== ==== ===== ==== ==== =====",
                   " ==== ===== ===== ==== ====\n" );
form = "%4d %c %4d %4d %4d %4d %5.1f %4d %4d %5.1f %4d %5.2f %5.1f %4.2f %4s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "----------------------------------- ", "GBMV",
                   " ---------------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# T    M    N   KL   KU    ALPHA  LDA INCX",
                   "     BETA INCY TIME MFLOP SpUp TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== = ==== ==== ==== ==== ======== ==== ====",
                   " ======== ==== ==== ===== ==== ====\n" );
form =
"%4d %c %4d %4d %4d %4d %4.1f%4.1f %4d %4d %4.1f%4.1f %4d %4.1f %5.1f %4.2f %4s\n";
#endif

   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) m = n; else m = mm;

         for( kkl = KL0; kkl <= KLN; kkl += KLINC )
         {
            for( kku = KU0; kku <= KUN; kku += KUINC )
            {
               if( LDA_IS_M ) lda = kkl+1+kku;
               else           lda = KLN+1+KUN;

               for( ta = 0; ta < NTRAN; ta++ )
               {
                  if(      TRANS[ta] == AtlasNoTrans   ) ctran = 'N';
                  else if( TRANS[ta] == AtlasTrans     ) ctran = 'T';
                  else                                   ctran = 'C';

                  for( iy = 0; iy < NINCY; iy++ )
                  {
                     for( ix = 0; ix < NINCX; ix++ )
                     {
                        for( al = 0; al < NALPHA; al++ )
                        {
                           for( be = 0; be < NBETA; be++ )
                           {
#ifdef TREAL
                              ires = gmvcase( CACHESIZE, ROUT, TEST, MFLOP,
					      TRANS[ta], m, n, kkl, kku,
					      ALPHAS[al], lda,
                                              INCXS[ix], BETAS[be], INCYS[iy],
                                              EPSILON, &ttrust, &ttest,
                                              &mftrust, &mftest );
#else
                              ires = gmvcase( CACHESIZE, ROUT, TEST, MFLOP,
					      TRANS[ta], m, n, kkl, kku,
					      ALPHAS+2*al, lda,
                                              INCXS[ix], BETAS+2*be, INCYS[iy],
                                              EPSILON, &ttrust, &ttest,
                                              &mftrust, &mftest );
#endif
                              if(     !( TEST ) ) pass = "SKIP";
                              else if( ires < 0 ) pass = "NoMEM";
                              else if( ires     ) pass = "PASS";
                              else                pass = "FAIL";

                              if( ires > 0 ) (*NPASSED)++;

                              if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                                 t0 = mftest / mftrust;
                              else t0 = 0.0;
#ifdef TREAL
                              (void) fprintf( stdout, form, *NTESTS, ctran, m,
                                              n, kkl, kku, ALPHAS[al], lda,
                                              INCXS[ix], BETAS[be], INCYS[iy],
                                              ttrust, mftrust, 1.0, "----" );
                              (void) fprintf( stdout, form, *NTESTS, ctran, m,
                                              n, kkl, kku, ALPHAS[al], lda,
                                              INCXS[ix], BETAS[be], INCYS[iy],
                                              ttest,  mftest,  t0,  pass );
#else
                              (void) fprintf( stdout, form, *NTESTS, ctran,
                                              m, n, kkl, kku, ALPHAS[2*al],
                                              ALPHAS[2*al+1], lda, INCXS[ix],
                                              BETAS[2*be], BETAS[2*be+1],
                                              INCYS[iy], ttrust, mftrust, 1.0,
                                              "----" );
                              (void) fprintf( stdout, form, *NTESTS, ctran,
                                              m, n, kkl, kku, ALPHAS[2*al],
                                              ALPHAS[2*al+1], lda, INCXS[ix],
                                              BETAS[2*be], BETAS[2*be+1],
                                              INCYS[iy], ttest,  mftest,  t0,
                                              pass );
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
}

void RungemCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
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
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, ix, iy, lda, m, mm, msame=0, n,
                              nn, ta;
   char                       ctran;

   if( M0 == -1 ) { M0 = MN = MINC = NN; msame = 1; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------- ", "GEMV",
                   " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# TR    M    N ALPHA  LDA INCX  BETA INCY",
                   "   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ==== ===== ==== ==== ===== ====",
                   " ====== ===== ===== =====\n" );
form = "%4d  %c %4d %4d %5.1f %4d %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "----------------------------------- ", "GEMV",
                   " ------------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# TR    M    N     ALPHA  LDA INCX      BETA",
                   " INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ==== ==== ==== ==== ==== ==== ====",
                   " ==== ====== ===== ===== =====\n" );
form =
"%4d  %c %4d %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %6.2f %5.1f %5.2f %5s\n";
#endif

   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) m = n; else m = mm;

         for( ta = 0; ta < NTRAN; ta++ )
         {
            if(      TRANS[ta] == AtlasNoTrans ) ctran = 'N';
            else if( TRANS[ta] == AtlasTrans   ) ctran = 'T';
            else                                 ctran = 'C';

            if( LDA_IS_M ) lda = m;
            else lda = MN;

            for( iy = 0; iy < NINCY; iy++ )
            {
               for( ix = 0; ix < NINCX; ix++ )
               {
                  for( al = 0; al < NALPHA; al++ )
                  {
                     for( be = 0; be < NBETA; be++ )
                     {
#ifdef TREAL
                        ires = gmvcase( CACHESIZE, ROUT, TEST, MFLOP,
					TRANS[ta], m, n, 0, 0, ALPHAS[al],
					lda, INCXS[ix], BETAS[be], INCYS[iy],
                                        EPSILON, &ttrust, &ttest, &mftrust,
                                        &mftest );
#else
                        ires = gmvcase( CACHESIZE, ROUT, TEST, MFLOP,
					TRANS[ta], m, n, 0, 0, ALPHAS+2*al,
					lda, INCXS[ix], BETAS+2*be, INCYS[iy],
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
                        (void) fprintf( stdout, form, *NTESTS, ctran, m,
                                        n, ALPHAS[al], lda, INCXS[ix],
                                        BETAS[be], INCYS[iy], ttrust,
                                        mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, ctran, m,
                                        n, ALPHAS[al], lda, INCXS[ix],
                                        BETAS[be], INCYS[iy], ttest,
                                        mftest,  t0,  pass );
#else
                        (void) fprintf( stdout, form, *NTESTS, ctran, m,
                                        n, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, INCXS[ix], BETAS[2*be],
                                        BETAS[2*be+1], INCYS[iy], ttrust,
                                        mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, ctran, m,
                                        n, ALPHAS[2*al], ALPHAS[2*al+1],
                                        lda, INCXS[ix], BETAS[2*be],
                                        BETAS[2*be+1], INCYS[iy], ttest,
                                        mftest,  t0,  pass );
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

void RunsbCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   int                        N0,
   int                        NN,
   int                        NINC,
   int                        KL0,
   int                        KLN,
   int                        KLINC,
   int                        KU0,
   int                        KUN,
   int                        KUINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, ix, iy, kkl, kku, lda, nn, up;
   char                       cuplo;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------- ", "SBMV",
                   " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UP    N    K ALPHA  LDA INCX  BETA INCY",
                   "   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ==== ===== ==== ==== ===== ====",
                   " ====== ===== ===== =====\n" );
   form = "%4d  %c %4d %4d %5.1f %4d %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------------ ", "HBMV",
                   " -----------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UP    N    K     ALPHA  LDA INCX      BETA",
                   " INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ==== ==== ==== ==== ==== ==== ====",
                   " ==== ====== ===== ===== =====\n" );
form =
"%4d  %c %4d %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %6.2f %5.1f %5.2f %5s\n";
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasLower )
         {
            cuplo = 'L';

            for( kkl = KL0; kkl <= KLN; kkl += KLINC )
            {
               if( LDA_IS_M ) lda = kkl+1;
               else           lda = KLN+1;

               for( ix = 0; ix < NINCX; ix++ )
               {
                  for( iy = 0; iy < NINCY; iy++ )
                  {
                     for( al = 0; al < NALPHA; al++ )
                     {
                        for( be = 0; be < NBETA; be++ )
                        {
#ifdef TREAL
                           ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up], nn,
                                           kkl, ALPHAS[al], lda, INCXS[ix],
                                           BETAS[be], INCYS[iy], EPSILON,
                                           &ttrust, &ttest, &mftrust, &mftest );
#else
                           ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up], nn,
                                           kkl, ALPHAS+2*al, lda, INCXS[ix],
                                           BETAS+2*be, INCYS[iy], EPSILON,
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
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kkl, ALPHAS[al], lda, INCXS[ix],
                                           BETAS[be], INCYS[iy], ttrust,
                                           mftrust, 1.0, "-----" );
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kkl, ALPHAS[al], lda, INCXS[ix],
                                           BETAS[be], INCYS[iy], ttest,
                                           mftest,  t0,  pass );
#else
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kkl, ALPHAS[2*al], ALPHAS[2*al+1],
                                           lda, INCXS[ix], BETAS[2*be],
                                           BETAS[2*be+1], INCYS[iy], ttrust,
                                           mftrust, 1.0, "-----" );
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kkl, ALPHAS[2*al], ALPHAS[2*al+1],
                                           lda, INCXS[ix], BETAS[2*be],
                                           BETAS[2*be+1], INCYS[iy], ttest,
                                           mftest,  t0,  pass );
#endif
                           (*NTESTS)++;
                        }
                     }
                  }
               }
            }
         }
         else
         {
            cuplo = 'U';

            for( kku = KU0; kku <= KUN; kku += KUINC )
            {
               if( LDA_IS_M ) lda = 1+kku;
               else           lda = 1+KUN;

               for( ix = 0; ix < NINCX; ix++ )
               {
                  for( iy = 0; iy < NINCY; iy++ )
                  {
                     for( al = 0; al < NALPHA; al++ )
                     {
                        for( be = 0; be < NBETA; be++ )
                        {
#ifdef TREAL
                           ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up], nn,
                                           kku, ALPHAS[al], lda, INCXS[ix],
                                           BETAS[be], INCYS[iy], EPSILON,
                                           &ttrust, &ttest, &mftrust, &mftest );
#else
                           ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up], nn,
                                           kku, ALPHAS+2*al, lda, INCXS[ix],
                                           BETAS+2*be, INCYS[iy], EPSILON,
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
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kku, ALPHAS[al], lda, INCXS[ix],
                                           BETAS[be], INCYS[iy], ttrust,
                                           mftrust, 1.0, "-----" );
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kku, ALPHAS[al], lda, INCXS[ix],
                                           BETAS[be], INCYS[iy], ttest,
                                           mftest,  t0,  pass );
#else
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kku, ALPHAS[2*al], ALPHAS[2*al+1],
                                           lda, INCXS[ix], BETAS[2*be],
                                           BETAS[2*be+1], INCYS[iy], ttrust,
                                           mftrust, 1.0, "-----" );
                           (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                           kku, ALPHAS[2*al], ALPHAS[2*al+1],
                                           lda, INCXS[ix], BETAS[2*be],
                                           BETAS[2*be+1], INCYS[iy], ttest,
                                           mftest,  t0,  pass );
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

void RunspCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, ix, iy, nn, up;
   char                       cuplo;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "-------------------------- ", "SPMV",
                   " ---------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UP    N ALPHA INCX  BETA INCY",
                   "   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ===== ==== ===== ====",
                   " ====== ===== ===== =====\n" );
   form = "%4d  %c %4d %5.1f %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------ ", "HPMV",
                   " -------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UP    N     ALPHA INCX      BETA",
                   " INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ==== ==== ==== ==== ====",
                   " ==== ====== ===== ===== =====\n" );
   form = "%4d  %c %4d %4.1f %4.1f %4d %4.1f %4.1f %4d %6.2f %5.1f %5.2f %5s\n";
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasLower ) cuplo = 'L';
         else                          cuplo = 'U';

         for( ix = 0; ix < NINCX; ix++ )
         {
            for( iy = 0; iy < NINCY; iy++ )
            {
               for( al = 0; al < NALPHA; al++ )
               {
                  for( be = 0; be < NBETA; be++ )
                  {
#ifdef TREAL
                     ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
				     UPLOS[up], nn, 0,
                                     ALPHAS[al], 1, INCXS[ix], BETAS[be],
                                     INCYS[iy], EPSILON, &ttrust, &ttest,
                                     &mftrust, &mftest );
#else
                     ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
				     UPLOS[up], nn, 0,
                                     ALPHAS+2*al, 1, INCXS[ix], BETAS+2*be,
                                     INCYS[iy], EPSILON, &ttrust, &ttest,
                                     &mftrust, &mftest );
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
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[al], INCXS[ix], BETAS[be],
                                     INCYS[iy], ttrust, mftrust, 1.0,
                                     "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[al], INCXS[ix], BETAS[be],
                                     INCYS[iy], ttest, mftest, t0, pass );
#else
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[2*al], ALPHAS[2*al+1],
                                     INCXS[ix], BETAS[2*be], BETAS[2*be+1],
                                     INCYS[iy], ttrust, mftrust, 1.0,
                                     "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[2*al], ALPHAS[2*al+1],
                                     INCXS[ix], BETAS[2*be], BETAS[2*be+1],
                                     INCYS[iy], ttest, mftest, t0, pass );
#endif
                     (*NTESTS)++;
                  }
               }
            }
         }
      }
   }
}

void RunsyCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, be, ires, ix, iy, lda, nn, up;
   char                       cuplo;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "----------------------------- ", "SYMV",
                   " -----------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UP    N ALPHA  LDA INCX  BETA INCY",
                   "   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ===== ==== ==== ===== ====",
                   " ====== ===== ===== =====\n" );
   form = "%4d  %c %4d %5.1f %4d %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "--------------------------------- ", "HEMV",
                   " ---------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UP    N     ALPHA  LDA INCX      BETA",
                   " INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== == ==== ==== ==== ==== ==== ==== ====",
                   " ==== ====== ===== ===== =====\n" );
form =
"%4d  %c %4d %4.1f %4.1f %4d %4d %4.1f %4.1f %4d %6.2f %5.1f %5.2f %5s\n";
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      if( LDA_IS_M ) lda = Mmax( 1, nn ); else lda = NN;

      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasLower ) cuplo = 'L';
         else                          cuplo = 'U';

         for( ix = 0; ix < NINCX; ix++ )
         {
            for( iy = 0; iy < NINCY; iy++ )
            {
               for( al = 0; al < NALPHA; al++ )
               {
                  for( be = 0; be < NBETA; be++ )
                  {
#ifdef TREAL
                     ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
				     UPLOS[up], nn,
                                     0, ALPHAS[al], lda, INCXS[ix],
                                     BETAS[be], INCYS[iy], EPSILON, &ttrust,
                                     &ttest, &mftrust, &mftest );
#else
                     ires = smvcase( CACHESIZE, ROUT, TEST, MFLOP,
				     UPLOS[up], nn,
                                     0, ALPHAS+2*al, lda, INCXS[ix],
                                     BETAS+2*be, INCYS[iy], EPSILON, &ttrust,
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
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[al], lda, INCXS[ix],
                                     BETAS[be], INCYS[iy], ttrust,
                                     mftrust, 1.0, "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[al], lda, INCXS[ix],
                                     BETAS[be], INCYS[iy], ttest,
                                     mftest,  t0,  pass );
#else
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[2*al], ALPHAS[2*al+1],
                                     lda, INCXS[ix], BETAS[2*be],
                                     BETAS[2*be+1], INCYS[iy], ttrust,
                                     mftrust, 1.0, "-----" );
                     (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                     ALPHAS[2*al], ALPHAS[2*al+1],
                                     lda, INCXS[ix], BETAS[2*be],
                                     BETAS[2*be+1], INCYS[iy], ttest,
                                     mftest,  t0,  pass );
#endif
                     (*NTESTS)++;
                  }
               }
            }
         }
      }
   }
}

void RuntbCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   const int                  NDIAG,
   const enum ATLAS_DIAG      * DIAGS,
   int                        N0,
   int                        NN,
   int                        NINC,
   int                        KL0,
   int                        KLN,
   int                        KLINC,
   int                        KU0,
   int                        KUN,
   int                        KUINC,
   const int                  NINCX,
   const int                  * INCXS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        di, ires, ix, kkl, kku, lda, nn, ta, up;
   char                       ctran, cdiag, cuplo;

   if( ROUT == TBMV )
      (void) fprintf( stdout, "\n%s%s%s\n", "-----------------------------",
                      " TBMV ", "------------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n", "-----------------------------",
                      " TBSV ", "------------------------------" );

   (void) fprintf( stdout, "%s%s",
                   "TST# UPLO TRAN DIAG    N    K  LDA INCX   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== ==== ==== ==== ==== ==== ==== ==== ======",
                   " ====== ===== =====\n" );
   form = "%4d    %c    %c    %c %4d %4d %4d %4d %6.2f %6.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasLower )
         {
            cuplo = 'L';

            for( kkl = KL0; kkl <= KLN; kkl += KLINC )
            {
               if( LDA_IS_M ) lda = kkl+1;
               else           lda = KLN+1;

               for( ta = 0; ta < NTRAN; ta++ )
               {
                  if(      TRANS[ta] == AtlasNoTrans ) ctran = 'N';
                  else if( TRANS[ta] == AtlasTrans   ) ctran = 'T';
                  else                                 ctran = 'C';

                  for( di = 0; di < NDIAG; di++ )
                  {
                     if( DIAGS[di] == AtlasUnit ) cdiag = 'U';
                     else                         cdiag = 'N';

                     for( ix = 0; ix < NINCX; ix++ )
                     {
                        if( ROUT == TBMV )
                           ires = tmvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up],
                                           TRANS[ta], DIAGS[di], nn, kkl,
                                           lda, INCXS[ix], EPSILON, &ttrust,
                                           &ttest, &mftrust, &mftest );
                        else
                           ires = tsvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up],
                                           TRANS[ta], DIAGS[di], nn, kkl,
                                           lda, INCXS[ix], EPSILON, &ttrust,
                                           &ttest, &mftrust, &mftest );

                        if(     !( TEST ) ) pass = "SKIP ";
                        else if( ires < 0 ) pass = "NoMEM";
                        else if( ires     ) pass = "PASS ";
                        else                pass = "FAIL ";

                        if( ires > 0 ) (*NPASSED)++;

                        if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                           t0 = mftest / mftrust;
                        else t0 = 0.0;

                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        cdiag, nn, kkl, lda, INCXS[ix], ttrust,
                                        mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        cdiag, nn, kkl, lda, INCXS[ix], ttest,
                                        mftest,  t0, pass );
                        (*NTESTS)++;
                     }
                  }
               }
            }
         }
         else
         {
            cuplo = 'U';

            for( kku = KU0; kku <= KUN; kku += KUINC )
            {
               if( LDA_IS_M ) lda = 1+kku;
               else           lda = 1+KUN;

               for( ta = 0; ta < NTRAN; ta++ )
               {
                  if(      TRANS[ta] == AtlasNoTrans ) ctran = 'N';
                  else if( TRANS[ta] == AtlasTrans   ) ctran = 'T';
                  else                                 ctran = 'C';

                  for( di = 0; di < NDIAG; di++ )
                  {
                     if( DIAGS[di] == AtlasUnit ) cdiag = 'U';
                     else                         cdiag = 'N';

                     for( ix = 0; ix < NINCX; ix++ )
                     {
                        if( ROUT == TBMV )
                           ires = tmvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up],
                                           TRANS[ta], DIAGS[di], nn, kku,
                                           lda, INCXS[ix], EPSILON, &ttrust,
                                           &ttest, &mftrust, &mftest );
                        else
                           ires = tsvcase( CACHESIZE, ROUT, TEST, MFLOP,
					   UPLOS[up],
                                           TRANS[ta], DIAGS[di], nn, kku,
                                           lda, INCXS[ix], EPSILON, &ttrust,
                                           &ttest, &mftrust, &mftest );

                        if(     !( TEST ) ) pass = "SKIP ";
                        else if( ires < 0 ) pass = "NoMEM";
                        else if( ires     ) pass = "PASS ";
                        else                pass = "FAIL ";

                        if( ires > 0 ) (*NPASSED)++;

                        if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                           t0 = mftest / mftrust;
                        else t0 = 0.0;

                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        cdiag, nn, kku, lda, INCXS[ix], ttrust,
                                        mftrust, 1.0, "-----" );
                        (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                        cdiag, nn, kku, lda, INCXS[ix], ttest,
                                        mftest,  t0, pass );
                        (*NTESTS)++;
                     }
                  }
               }
            }
         }
      }
   }
}

void RuntpCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   const int                  NDIAG,
   const enum ATLAS_DIAG      * DIAGS,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  NINCX,
   const int                  * INCXS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        nn, up, ta, di, ix, ires;
   char                       ctran, cdiag, cuplo;

   if( ROUT == TPMV )
      (void) fprintf( stdout, "\n%s%s%s\n", "------------------------",
                      " TPMV ", "-------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n", "------------------------",
                      " TPSV ", "-------------------------" );
   (void) fprintf( stdout, "%s%s", "TST# UPLO TRAN DIAG    N INCX   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s", "==== ==== ==== ==== ==== ==== ======",
                   " ====== ===== =====\n" );
   form = "%4d    %c    %c    %c %4d %4d %6.2f %6.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
         else                          cuplo = 'L';

         for( ta = 0; ta < NTRAN; ta++ )
         {
            if(      TRANS[ta] == AtlasNoTrans   ) ctran = 'N';
            else if( TRANS[ta] == AtlasTrans     ) ctran = 'T';
            else if( TRANS[ta] == AtlasConjTrans ) ctran = 'C';
            else                                   ctran = 'N';

            for( di = 0; di < NDIAG; di++ )
            {
               if( DIAGS[di] == AtlasUnit ) cdiag = 'U';
               else                         cdiag = 'N';

               for( ix = 0; ix < NINCX; ix++ )
               {
                  if( ROUT == TPMV )
                     ires = tmvcase( CACHESIZE, ROUT, TEST, MFLOP,
				     UPLOS[up], TRANS[ta],
                                     DIAGS[di], nn, 0, 1, INCXS[ix], EPSILON,
                                     &ttrust, &ttest, &mftrust, &mftest );
                  else
                     ires = tsvcase( CACHESIZE, ROUT, TEST, MFLOP,
				     UPLOS[up], TRANS[ta],
                                     DIAGS[di], nn, 0, 1, INCXS[ix], EPSILON,
                                     &ttrust, &ttest, &mftrust, &mftest );

                  if(     !( TEST ) ) pass = "SKIP ";
                  else if( ires < 0 ) pass = "NoMEM";
                  else if( ires     ) pass = "PASS ";
                  else                pass = "FAIL ";

                  if( ires > 0 ) (*NPASSED)++;

                  if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                     t0 = mftest / mftrust;
                  else t0 = 0.0;

                  (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                  cdiag, nn, INCXS[ix], ttrust, mftrust,
                                  1.0, "-----" );
                  (void) fprintf( stdout, form, *NTESTS, cuplo, ctran,
                                  cdiag, nn, INCXS[ix], ttest,  mftest,
                                  t0, pass );
                  (*NTESTS)++;
               }
            }
         }
      }
   }
}

void RuntrCase
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   const int                  NDIAG,
   const enum ATLAS_DIAG      * DIAGS,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NINCX,
   const int                  * INCXS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        nn, up, ta, di, ix, ires, lda;
   char                       ctran, cdiag, cuplo;

   if( ROUT == TRMV )
      (void) fprintf( stdout, "\n%s%s%s\n", "--------------------------",
                      " TRMV ", "----------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n", "--------------------------",
                      " TRSV ", "----------------------------" );

   (void) fprintf( stdout, "%s%s", "TST# UPLO TRAN DIAG    N  LDA INCX   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s", "==== ==== ==== ==== ==== ==== ==== ======",
                   " ====== ===== =====\n" );
   form = "%4d    %c    %c    %c %4d %4d %4d %6.2f %6.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      if( LDA_IS_M ) lda = Mmax( 1, nn ); else lda = NN;

      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
         else                          cuplo = 'L';

         for( ta = 0; ta < NTRAN; ta++ )
         {
            if(      TRANS[ta] == AtlasNoTrans   ) ctran = 'N';
            else if( TRANS[ta] == AtlasTrans     ) ctran = 'T';
            else if( TRANS[ta] == AtlasConjTrans ) ctran = 'C';
            else                                   ctran = 'N';

            for( di = 0; di < NDIAG; di++ )
            {
               if( DIAGS[di] == AtlasUnit ) cdiag = 'U';
               else                         cdiag = 'N';

               for( ix = 0; ix < NINCX; ix++ )
               {
                  if( ROUT == TRMV )
                     ires = tmvcase( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
				     TRANS[ta], DIAGS[di], nn, 0, lda,
				     INCXS[ix], EPSILON,
                                     &ttrust, &ttest, &mftrust, &mftest );
                  else
                     ires = tsvcase( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
				     TRANS[ta], DIAGS[di], nn, 0, lda,
				     INCXS[ix], EPSILON, &ttrust, &ttest,
				     &mftrust, &mftest );

                  if(     !( TEST ) ) pass = "SKIP ";
                  else if( ires < 0 ) pass = "NoMEM";
                  else if( ires     ) pass = "PASS ";
                  else                pass = "FAIL ";

                  if( ires > 0 ) (*NPASSED)++;

                  if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                     t0 = mftest / mftrust;
                  else t0 = 0.0;

                  (void) fprintf( stdout, form, *NTESTS, cuplo, ctran, cdiag,
                                  nn, lda, INCXS[ix], ttrust, mftrust, 1.0,
                                  "-----" );
                  (void) fprintf( stdout, form, *NTESTS, cuplo, ctran, cdiag,
                                  nn, lda, INCXS[ix], ttest,  mftest,  t0,
                                  pass );
                  (*NTESTS)++;
               }
            }
         }
      }
   }
}

void Rungr1Case
(  const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   int                        M0,
   int                        MN,
   int                        MINC,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, ires, ix, iy, lda, m, mm, msame=0, n, nn;

   if( M0 == -1 ) { M0 = MN = MINC = NN; msame = 1; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------ ", "GER1",
                   " -----------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST#     M     N ALPHA INCX INCY   LDA   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== ===== ===== ===== ==== ==== ===== ======",
                   " ====== ===== =====\n" );
   form = "%4d %5d %5d %5.1f %4d %4d %5d %6.2f %6.1f %5.2f %5s\n";
#else
   if( ROUT == GERC )
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "-------------------------------- ", "GER1C",
                      " --------------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "-------------------------------- ", "GER1U",
                      " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST#     M     N       ALPHA INCX INCY   LDA",
                   "   TIME  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== ===== ===== ===== ===== ==== ==== =====",
                   " ====== ====== ===== =====\n" );
   form = "%4d %5d %5d %5.1f %5.1f %4d %4d %5d %6.2f %6.1f %5.2f %5s\n";
#endif
   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) m = n; else m = mm;
         if( LDA_IS_M ) lda = Mmax( 1, m ); else lda = MN;

         for( iy = 0; iy < NINCY; iy++ )
         {
            for( ix = 0; ix < NINCX; ix++ )
            {
               for( al = 0; al < NALPHA; al++ )
               {
#ifdef TREAL
                  ires = gr1case( CACHESIZE, ROUT, TEST, MFLOP, m, n,
				  ALPHAS[al],
                                  INCXS[ix], INCYS[iy], lda, EPSILON, &ttrust,
                                  &ttest, &mftrust, &mftest );
#else
                  ires = gr1case( CACHESIZE, ROUT, TEST, MFLOP, m, n,
				  ALPHAS+2*al,
                                  INCXS[ix], INCYS[iy], lda, EPSILON, &ttrust,
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
                  (void) fprintf( stdout, form, *NTESTS, m, n, ALPHAS[al],
                                  INCXS[ix], INCYS[iy], lda, ttrust,
                                  mftrust, 1.0, "-----" );
                  (void) fprintf( stdout, form, *NTESTS, m, n, ALPHAS[al],
                                  INCXS[ix], INCYS[iy], lda, ttest,
                                  mftest, t0, pass );
#else
                  (void) fprintf( stdout, form, *NTESTS, m, n, ALPHAS[2*al],
                                  ALPHAS[2*al+1], INCXS[ix], INCYS[iy],
                                  lda, ttrust, mftrust, 1.0, "-----" );
                  (void) fprintf( stdout, form, *NTESTS, m, n, ALPHAS[2*al],
                                  ALPHAS[2*al+1], INCXS[ix], INCYS[iy],
                                  lda, ttest, mftest, t0, pass );
#endif
                  (*NTESTS)++;
               }
            }
         }
      }
   }
}

void Rungr2Case
(  const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   int                        M0,
   int                        MN,
   int                        MINC,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 *BETAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, ires, ix, iy, lda, m, mm, msame=0, n, nn;
   int iw, iz, ib;

   if( M0 == -1 ) { M0 = MN = MINC = NN; msame = 1; }

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------ ", "GER2",
                   " -----------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST#     M     N ALPHA INCX INCY  BETA INCW INCX ",
                   "LDA   TIME  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== ===== ===== ===== ==== ==== ===== ==== ==== ",
                   "==== ===== ====== ===== =====\n" );
   form = "%4d %5d %5d %5.1f %4d %4d %5.1f %4d %4d %4d %6.2f %6.1f %5.2f %5s\n";
#else
   if( ROUT == GER2C )
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "-------------------------------- ", "GER2C",
                      " --------------------------------" );
   else
      (void) fprintf( stdout, "\n%s%s%s\n",
                      "-------------------------------- ", "GER2U",
                      " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                "TST#     M     N       ALPHA INCX INCY BETAr BETAi INCW INCY ",
                   " LDA   TIME  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                "==== ===== ===== ===== ===== ==== ==== ===== ===== ==== ==== ",
                   "=====, ====== ====== ===== =====\n" );
   form = "%4d %5d %5d %5.1f %5.1f %4d %4d %5.1f %5.1f %4d %4d %5d %6.2f %6.1f %5.2f %5s\n";
#endif
   for( mm = M0; mm <= MN; mm += MINC )
   {
      for( nn = N0; nn <= NN; nn += NINC )
      {
         n = nn; if( msame ) m = n; else m = mm;
         if( LDA_IS_M ) lda = Mmax( 1, m ); else lda = MN;

         for( iy = 0; iy < NINCY; iy++ )
         {
            for( ix = 0; ix < NINCX; ix++ )
            {
               for( al = 0; al < NALPHA; al++ )
               {
                  for( ib = 0; ib < NBETA; ib++ )
                  {
                     for(iw=0; iw < NINCX; iw++ )
                     {
                        for(iz=0; iz < NINCY; iz++ )
                        {

                           ires = gr2case( CACHESIZE, ROUT, TEST, MFLOP,
                           #ifdef TREAL
                                 m, n, ALPHAS[al], INCXS[ix], INCYS[iy],
                                 BETAS[ib], INCXS[iw], INCYS[iz], lda,
                           #else
                                 m, n, ALPHAS+2*al, INCXS[ix], INCYS[iy],
                                 BETAS+2*ib, INCXS[iw], INCYS[iz], lda,
                           #endif
                                 EPSILON, &ttrust, &ttest, &mftrust, &mftest );
                           if(     !( TEST ) ) pass = "SKIP ";
                           else if( ires < 0 ) pass = "NoMEM";
                           else if( ires     ) pass = "PASS ";
                           else                pass = "FAIL ";

                           if( ires > 0 ) (*NPASSED)++;

                           if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                              t0 = mftest / mftrust;
                           else t0 = 0.0;
                           #ifdef TREAL
                              fprintf(stdout, form, *NTESTS, m, n, ALPHAS[al],
                                      INCXS[ix], INCYS[iy], BETAS[ib],
                                      INCXS[iw], INCYS[iz], lda, ttrust,
                                      mftrust, 1.0, "-----");
                              fprintf(stdout, form, *NTESTS, m, n, ALPHAS[al],
                                      INCXS[ix], INCYS[iy], BETAS[ib],
                                      INCXS[iw], INCYS[iz], lda, ttest,
                                      mftest, t0, pass);
                           #else
                              fprintf(stdout, form, *NTESTS, m, n, ALPHAS[2*al],
                                      ALPHAS[2*al+1], INCXS[ix], INCYS[iy],
                                      BETAS[2*ib], BETAS[2*ib+1], INCXS[iw],
                                      INCYS[iz],
                                      lda, ttrust, mftrust, 1.0, "-----");
                              fprintf(stdout, form, *NTESTS, m, n, ALPHAS[2*al],
                                      ALPHAS[2*al+1], INCXS[ix], INCYS[iy],
                                      BETAS[2*ib], BETAS[2*ib+1], INCXS[iw],
                                      INCYS[iz],
                                      lda, ttest, mftest, t0, pass);
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

void Runsp1Case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NINCX,
   const int                  * INCXS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, ires, ix, nn, up;
   char                       cuplo;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n", "----------------------- ", "SPR",
                   " ------------------------" );
#else
   (void) fprintf( stdout, "\n%s%s%s\n", "----------------------- ", "HPR",
                   " ------------------------" );
#endif
   (void) fprintf( stdout, "%s%s", "TST# UPLO     N ALPHA INCX   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s", "==== ==== ===== ===== ==== ======",
                   " ====== ===== =====\n" );
   form = "%4d    %c %5d %5.1f %4d %6.2f %6.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
         else                          cuplo = 'L';

         for( ix = 0; ix < NINCX; ix++ )
         {
            for( al = 0; al < NALPHA; al++ )
            {
#ifdef TREAL
               ires = sr1case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
			       nn, ALPHAS[al],
                               INCXS[ix], 1, EPSILON, &ttrust, &ttest,
                               &mftrust, &mftest );
#else
               ires = sr1case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
			       nn, ALPHAS[2*al],
                               INCXS[ix], 1, EPSILON, &ttrust, &ttest,
                               &mftrust, &mftest );
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
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[al],
                               INCXS[ix], ttrust, mftrust, 1.0, "-----" );
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[al],
                               INCXS[ix], ttest,  mftest,  t0,  pass );
#else
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[2*al],
                               INCXS[ix], ttrust, mftrust, 1.0, "-----" );
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[2*al],
                               INCXS[ix], ttest,  mftest,  t0,  pass );
#endif
               (*NTESTS)++;
            }
         }
      }
   }
}

void Runsr1Case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NINCX,
   const int                  * INCXS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, ires, ix, lda, nn, up;
   char                       cuplo;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n", "--------------------------- ", "SYR",
                   " --------------------------" );
#else
   (void) fprintf( stdout, "\n%s%s%s\n", "--------------------------- ", "HER",
                   " --------------------------" );
#endif
   (void) fprintf( stdout, "%s%s", "TST# UPLO     N ALPHA INCX   LDA   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s", "==== ==== ===== ===== ==== ===== ======",
                   " ====== ===== =====\n" );
   form = "%4d    %c %5d %5.1f %4d %5d %6.2f %6.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      if( LDA_IS_M ) lda = Mmax( 1, nn ); else lda = NN;

      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
         else                          cuplo = 'L';

         for( ix = 0; ix < NINCX; ix++ )
         {
            for( al = 0; al < NALPHA; al++ )
            {
#ifdef TREAL
               ires = sr1case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
			       nn, ALPHAS[al],
                               INCXS[ix], lda, EPSILON, &ttrust, &ttest,
                               &mftrust, &mftest );
#else
               ires = sr1case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
			       nn, ALPHAS[2*al],
                               INCXS[ix], lda, EPSILON, &ttrust, &ttest,
                               &mftrust, &mftest );
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
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[al],
                               INCXS[ix], lda, ttrust, mftrust, 1.0,
                               "-----" );
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[al],
                               INCXS[ix], lda, ttest,  mftest,  t0,
                               pass );
#else
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[2*al],
                               INCXS[ix], lda, ttrust, mftrust, 1.0,
                               "-----" );
               (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[2*al],
                               INCXS[ix], lda, ttest,  mftest,  t0,
                               pass );
#endif
               (*NTESTS)++;
            }
         }
      }
   }
}

void Runsp2Case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, ires, ix, iy, nn, up;
   char                       cuplo;

#ifdef TREAL
         (void) fprintf( stdout, "\n%s%s%s\n",
                         "-------------------------- ", "SPR2",
                         " -------------------------" );
         (void) fprintf( stdout, "%s%s",
                         "TST# UPLO     N ALPHA INCX INCY   TIME",
                         "  MFLOP  SpUp  TEST\n" );
         (void) fprintf( stdout, "%s%s",
                         "==== ==== ===== ===== ==== ==== ======",
                         " ====== ===== =====\n" );
         form = "%4d    %c %5d %5.1f %4d %4d %6.2f %6.1f %5.2f %5s\n";
#else
         (void) fprintf( stdout, "\n%s%s%s\n",
                         "----------------------------- ", "HPR2",
                         " ----------------------------" );
         (void) fprintf( stdout, "%s%s",
                         "TST# UPLO     N       ALPHA INCX INCY",
                         "   TIME  MFLOP  SpUp  TEST\n");
         (void) fprintf( stdout, "%s%s",
                         "==== ==== ===== ===== ===== ==== ====",
                         " ====== ====== ===== =====\n");
         form = "%4d    %c %5d %5.1f %5.1f %4d %4d %6.2f %6.1f %5.2f %5s\n";
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
         else                          cuplo = 'L';

         for( iy = 0; iy < NINCY; iy++ )
         {
            for( ix = 0; ix < NINCX; ix++ )
            {
               for( al = 0; al < NALPHA; al++ )
               {
#ifdef TREAL
                  ires = sr2case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
				  nn, ALPHAS[al], INCXS[ix], INCYS[iy], 1,
                                  EPSILON, &ttrust, &ttest, &mftrust,
                                  &mftest );
#else
                  ires = sr2case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
				  nn, ALPHAS+2*al, INCXS[ix], INCYS[iy], 1,
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
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[al],
                                  INCXS[ix], INCYS[iy], ttrust, mftrust, 1.0,
                                  "-----" );
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn, ALPHAS[al],
                                  INCXS[ix], INCYS[iy], ttest, mftest, t0,
                                  pass );
#else
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                  ALPHAS[2*al], ALPHAS[2*al+1], INCXS[ix],
                                  INCYS[iy], ttrust, mftrust, 1.0, "-----" );
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                  ALPHAS[2*al], ALPHAS[2*al+1], INCXS[ix],
                                  INCYS[iy], ttest, mftest, t0, pass );
#endif
                  (*NTESTS)++;
               }
            }
         }
      }
   }
}

void Runsr2Case
(
   const int                  CACHESIZE,
   const enum LVL2_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  LDA_IS_M,
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   int                        N0,
   int                        NN,
   int                        NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        al, ires, ix, iy, lda, nn, up;
   char                       cuplo;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "---------------------------- ", "SYR2",
                   " -----------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UPLO     N ALPHA INCX INCY   LDA   TIME",
                   "  MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s%s",
                   "==== ==== ===== ===== ==== ==== ===== ======",
                   " ====== ===== =====\n" );
   form = "%4d    %c %5d %5.1f %4d %4d %5d %6.2f %6.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s%s%s\n",
                   "------------------------------- ", "HER2",
                   " --------------------------------" );
   (void) fprintf( stdout, "%s%s",
                   "TST# UPLO     N       ALPHA INCX INCY   LDA",
                   "   TIME  MFLOP  SpUp  TEST\n");
   (void) fprintf( stdout, "%s%s",
                   "==== ==== ===== ===== ===== ==== ==== =====",
                   " ====== ====== ===== =====\n");
   form = "%4d    %c %5d %5.1f %5.1f %4d %4d %5d %6.2f %6.1f %5.2f %5s\n";
#endif
   for( nn = N0; nn <= NN; nn += NINC )
   {
      if( LDA_IS_M ) lda = Mmax( 1, nn ); else lda = NN;

      for( up = 0; up < NUPLO; up++ )
      {
         if( UPLOS[up] == AtlasUpper ) cuplo = 'U';
         else                          cuplo = 'L';

         for( iy = 0; iy < NINCY; iy++ )
         {
            for( ix = 0; ix < NINCX; ix++ )
            {
               for( al = 0; al < NALPHA; al++ )
               {
#ifdef TREAL
                  ires = sr2case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up],
				  nn, ALPHAS[al], INCXS[ix], INCYS[iy], lda,
                                  EPSILON, &ttrust, &ttest, &mftrust, &mftest );
#else
                  ires = sr2case( CACHESIZE, ROUT, TEST, MFLOP, UPLOS[up], nn,
                                  ALPHAS+2*al, INCXS[ix], INCYS[iy], lda,
                                  EPSILON, &ttrust, &ttest, &mftrust, &mftest );
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
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                  ALPHAS[al], INCXS[ix], INCYS[iy],
                                  lda, ttrust, mftrust, 1.0, "-----" );
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                  ALPHAS[al], INCXS[ix], INCYS[iy],
                                  lda, ttest, mftest, t0, pass );
#else
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                  ALPHAS[2*al], ALPHAS[2*al+1], INCXS[ix],
                                  INCYS[iy], lda, ttrust, mftrust, 1.0,
                                  "-----" );
                  (void) fprintf( stdout, form, *NTESTS, cuplo, nn,
                                  ALPHAS[2*al], ALPHAS[2*al+1], INCXS[ix],
                                  INCYS[iy], lda, ttest, mftest, t0, pass );
#endif
                  (*NTESTS)++;
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
   const int                  NUPLO,
   const enum ATLAS_UPLO      * UPLOS,
   const int                  NTRAN,
   const enum ATLAS_TRANS     * TRANS,
   const int                  NDIAG,
   const enum ATLAS_DIAG      * DIAGS,
   const int                  M0,
   const int                  MN,
   const int                  MINC,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  KL0,
   const int                  KLN,
   const int                  KLINC,
   const int                  KU0,
   const int                  KUN,
   const int                  KUINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NBETA,
   const TYPE                 * BETAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const int                  NROUT,
   const enum LVL2_ROUT       * ROUTS
)
{
   TYPE                       eps;
   int                        ro, ntests=0, np=0;

   eps = Mjoin( PATL, epsilon )();

   for( ro = 0; ro < NROUT; ro++ )
   {
      if( ROUTS[ro] == GBMV  )
      {
         RungbmCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NTRAN,
		     TRANS, M0, MN,
                     MINC, N0, NN, NINC, KL0, KLN, KLINC, KU0, KUN, KUINC,
                     NALPHA, ALPHAS, NBETA, BETAS, NINCX, INCXS, NINCY, INCYS,
                     eps, &np, &ntests );
      }
      else if( ROUTS[ro] == GEMV  )
      {
         RungemCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NTRAN,
		     TRANS, M0, MN,
                     MINC, N0, NN, NINC, NALPHA, ALPHAS, NBETA, BETAS, NINCX,
                     INCXS, NINCY, INCYS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == SBMV )
      {
         RunsbCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		    UPLOS, N0, NN,
                    NINC, KL0, KLN, KLINC, KU0, KUN, KUINC, NALPHA, ALPHAS,
                    NBETA, BETAS, NINCX, INCXS, NINCY, INCYS, eps, &np,
                    &ntests );
      }
      else if( ROUTS[ro] == SPMV )
      {
         RunspCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, NUPLO, UPLOS, N0,
		    NN, NINC, NALPHA,
                    ALPHAS, NBETA, BETAS, NINCX, INCXS, NINCY, INCYS, eps, &np,
                    &ntests );
      }
      else if( ROUTS[ro] == SYMV )
      {
         RunsyCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		    UPLOS, N0, NN,
                    NINC, NALPHA, ALPHAS, NBETA, BETAS, NINCX, INCXS, NINCY,
                    INCYS, eps, &np, &ntests );
      }
      else if( ( ROUTS[ro] == TBMV ) || ( ROUTS[ro] == TBSV ) )
      {
         RuntbCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		    UPLOS, NTRAN,
                    TRANS, NDIAG, DIAGS, N0, NN, NINC, KL0, KLN, KLINC,
                    KU0, KUN, KUINC, NINCX, INCXS, eps, &np, &ntests );
      }
      else if( ( ROUTS[ro] == TPMV ) || ( ROUTS[ro] == TPSV ) )
      {
         RuntpCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, NUPLO, UPLOS,
		    NTRAN, TRANS, NDIAG,
                    DIAGS, N0, NN, NINC, NINCX, INCXS, eps, &np, &ntests );
      }
      else if( ( ROUTS[ro] == TRMV ) || ( ROUTS[ro] == TRSV ) )
      {
         RuntrCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		    UPLOS, NTRAN,
                    TRANS, NDIAG, DIAGS, N0, NN, NINC, NINCX, INCXS, eps,
                    &np, &ntests );
      }
      else if( ( ROUTS[ro] == GERC ) || ( ROUTS[ro] == GERU ) )
      {
         Rungr1Case( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, M0, MN,
		     MINC, N0, NN,
                     NINC, NALPHA, ALPHAS, NINCX, INCXS, NINCY, INCYS, eps,
                     &np, &ntests );
      }
      else if( ( ROUTS[ro] == GER2C ) || ( ROUTS[ro] == GER2U ) )
      {
         Rungr2Case( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, M0, MN,
		     MINC, N0, NN, NINC, NALPHA, ALPHAS, NBETA, BETAS,
                     NINCX, INCXS, NINCY, INCYS, eps,
                     &np, &ntests );
      }
      else if( ROUTS[ro] == SPR )
      {
         Runsp1Case( CACHESIZE, ROUTS[ro], TEST, MFLOP, NUPLO, UPLOS,
		     N0, NN, NINC,
                     NALPHA, ALPHAS, NINCX, INCXS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == SYR )
      {
         Runsr1Case( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		     UPLOS, N0, NN,
                     NINC, NALPHA, ALPHAS, NINCX, INCXS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == SPR2 )
      {
         Runsp2Case( CACHESIZE, ROUTS[ro], TEST, MFLOP, NUPLO, UPLOS,
		     N0, NN, NINC,
                     NALPHA, ALPHAS, NINCX, INCXS, NINCY, INCYS, eps, &np,
                     &ntests );
      }
      else if( ROUTS[ro] == SYR2 )
      {
         Runsr2Case( CACHESIZE, ROUTS[ro], TEST, MFLOP, LDA_IS_M, NUPLO,
		     UPLOS, N0, NN,
                     NINC, NALPHA, ALPHAS, NINCX, INCXS, NINCY, INCYS, eps,
                     &np, &ntests );
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

   (void) fprintf( stderr, "   -h                                   " );
   (void) fprintf( stderr, ". print this message                   \n" );

   (void) fprintf( stderr, "   -R <rout>                            " );
   (void) fprintf( stderr, ". select  one  or all routines to test.\n" );
   (void) fprintf( stderr, "                                        " );
#ifdef TREAL
   (void) fprintf( stderr, "  rout must be in {gbmv,gemv,sbmv,spmv,\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  symv,tbmv,tpmv,trmv,tbsv,tpsv,trsv,  \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  ger,ger2, spr,syr,spr2,syr2, all}.         \n" );
#else
   (void) fprintf( stderr, "  rout must be in {gbmv,gemv,hbmv,hpmv,\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  hemv,tbmv,tpmv,trmv,tbsv,tpsv,trsv,  \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  gerc,geru,ger2c,ger2u,hpr,her,hpr2,her2, all}.   \n" );
#endif
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default: -R gemv.  Ex: -R tpmv       \n" );

   (void) fprintf( stderr, "   -R <nrout> <rout1> ... <routN>       " );
   (void) fprintf( stderr, ". same as above for more than one rout-\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  tine. Ex: -R 3 gemv tpmv tbsv        \n" );

   (void) fprintf( stderr, "   -U <nuplo>  L/U                      " );
   (void) fprintf( stderr, ". select values for the UPLO parameter.\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default: -U 1 L. Ex: -U 2 L U        \n" );

   (void) fprintf( stderr, "   -A <ntrans> n/t/c                    " );
   (void) fprintf( stderr, ". select values of the TRANS parameter.\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default: -A 1 n. Ex: -A 2 n t        \n" );

   (void) fprintf( stderr, "   -D <ndiags> N/U                      " );
   (void) fprintf( stderr, ". select values for the DIAG parameter.\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default: -D 1 N. Ex: -Diag 2 N U     \n" );

   (void) fprintf( stderr, "   -m <m>                               " );
   (void) fprintf( stderr, ". select one value for the parameter M.\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Ex: -m 100                           \n" );

   (void) fprintf( stderr, "   -n <n>                               " );
   (void) fprintf( stderr, ". same as above for the parameter N.   \n" );

   (void) fprintf( stderr, "   -p <kl>                              " );
   (void) fprintf( stderr, ". same as above for the parameter KL.  \n" );

   (void) fprintf( stderr, "   -q <ku>                              " );
   (void) fprintf( stderr, ". same as above for the parameter KU.  \n" );

   (void) fprintf( stderr, "   -M <m1>  <mN>  <minc>                " );
   (void) fprintf( stderr, ". select the values of M, from m1 to mN\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  by increment of minc. m1 > 0.        \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Ex: -M 100 1000 100                  \n" );

   (void) fprintf( stderr, "   -N <n1>  <nN>  <ninc>                " );
   (void) fprintf( stderr, ". same as above for the values of N.   \n" );

   (void) fprintf( stderr, "   -P <kl1> <klN> <klinc>               " );
   (void) fprintf( stderr, ". same as above for the values of KL.  \n" );

   (void) fprintf( stderr, "   -Q <ku1> <kuN> <kuinc>               " );
   (void) fprintf( stderr, ". same as above for the values of KU.  \n" );

#ifdef TREAL
   (void) fprintf( stderr, "   -a <nalphas> <a1> ... <aN>           " );
   (void) fprintf( stderr, ". select the values of ALPHA.  Default:\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  -a 1 1.0. Ex: -a 3 -1.0 0.0 1.0      \n" );
   (void) fprintf( stderr, "   -b <nbetas>  <beta1>  ... <betaN>    " );
   (void) fprintf( stderr, ". same as above for the parameter BETA.\n" );
#else
   (void) fprintf( stderr, "   -a <nalphas> <a1r> <a1i> ... <aNi>  " );
   (void) fprintf( stderr, ". select the values of ALPHA, where a1r\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  and  a1i  are the  real and imaginary\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  parts of a1. Default: -a 1 1.0 0.0   \n" );
   (void) fprintf( stderr, "   -b <nbetas>  <b1r> <b1i> ... <bNi>   " );
   (void) fprintf( stderr, ". same as above for the parameter BETA.\n" );
#endif

   (void) fprintf( stderr, "   -X <nincXs>  <incX0>  ... <incX1>    " );
   (void) fprintf( stderr, ". select  the  values  of the increment\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  INCX. Default: -X 1 1; Ex: -X 2 1 -1 \n" );

   (void) fprintf( stderr, "   -Y <nincYs>  <incY0>  ... <incYN>    " );
   (void) fprintf( stderr, ". same as above for the the values of  \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  INCY.                                \n" );

   (void) fprintf( stderr, "   -d                                   " );
   (void) fprintf( stderr, ". use smallest possible leading  dimen-\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  sion for the array A.                \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default: max( mN, nN ).              \n" );

   (void) fprintf( stderr, "   -T <0/1>                             " );
   (void) fprintf( stderr, ". disable/enable computational check.  \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default: -T 1                        \n" );

   (void) fprintf( stderr, "   -F <mflops>                          " );
   (void) fprintf( stderr, ". perform at least mflops per measure. \n" );

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
   enum LVL2_ROUT             ** ROUTS,
   int                        * TEST,
   int                        * LDA_IS_M,
   int                        * CACHESIZE,
   int                        * MFLOP,
   int                        * NUPLO,
   enum ATLAS_UPLO            ** UPLOS,
   int                        * NTRAN,
   enum ATLAS_TRANS           ** TRANS,
   int                        * NDIAG,
   enum ATLAS_DIAG            ** DIAGS,
   int                        * M0,
   int                        * MN,
   int                        * MINC,
   int                        * N0,
   int                        * NN,
   int                        * NINC,
   int                        * KL0,
   int                        * KLN,
   int                        * KLINC,
   int                        * KU0,
   int                        * KUN,
   int                        * KUINC,
   int                        * NALPHA,
   TYPE                       ** ALPHAS,
   int                        * NBETA,
   TYPE                       ** BETAS,
   int                        * NINCX,
   int                        ** INCXS,
   int                        * NINCY,
   int                        ** INCYS
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
   *CACHESIZE = L2SIZE;               /* Size of largest cache to flush */
#else
   *CACHESIZE = 4*1024*1024;
#endif

   *KL0      = *KU0   = *KLN   = *KUN   = *KLINC = *KUINC = 1;

   *NUPLO    = *NTRAN = *NDIAG = -1;
   *M0       = *N0    = -1;
   *NALPHA   = *NBETA = -1;
   *NINCX    = *NINCY = -1;

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
         case 'F':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *MFLOP = atoi( ARGS[i++] );
            if( *MFLOP < 0      ) PrintUsage( ARGS[0] );
            break;
         case 'C':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
	    *CACHESIZE = 1024*atoi(ARGS[i++]);
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
         case 'A':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NTRAN = atoi(ARGS[i++]);
            if( *NTRAN <= 0     ) PrintUsage( ARGS[0] );
            *TRANS = (enum ATLAS_TRANS *)malloc( *NTRAN *
                                                 sizeof( enum ATLAS_TRANS ) );
            ATL_assert( *TRANS );
            for( j = 0; j != *NTRAN; j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               ch = *ARGS[i++];
               if(      ch == 'n' || ch == 'N' ) (*TRANS)[j] = AtlasNoTrans;
               else if( ch == 't' || ch == 'T' ) (*TRANS)[j] = AtlasTrans;
               else if( ch == 'c' || ch == 'C' ) (*TRANS)[j] = AtlasConjTrans;
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
         case 'P':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KL0 = atoi( ARGS[i++] );
            if( *KL0 < 0        ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KLN = atoi( ARGS[i++] );
            if( *KLN < 0        ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KLINC = atoi( ARGS[i++] );
            if( *KLINC <= 0     ) PrintUsage( ARGS[0] );
            break;
         case 'p':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KL0 = *KLN = atoi( ARGS[i++] ); *KLINC = 1;
            if( *KL0 <  0       ) PrintUsage( ARGS[0] );
            break;
         case 'Q':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KU0 = atoi( ARGS[i++] );
            if( *KU0 < 0        ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KUN = atoi( ARGS[i++] );
            if( *KUN < 0        ) PrintUsage( ARGS[0] );
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KUINC = atoi( ARGS[i++] );
            if( *KUINC <= 0     ) PrintUsage( ARGS[0] );
            break;
         case 'q':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *KU0 = *KUN = atoi( ARGS[i++] ); *KUINC = 1;
            if( *KU0 <  0       ) PrintUsage( ARGS[0] );
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

         case 'X':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NINCX = atoi( ARGS[i++] );
            if( *NINCX <= 0     ) PrintUsage( ARGS[0] );
            *INCXS = (int *)malloc( *NINCX * sizeof( int ) );
            ATL_assert( *INCXS );
            for( j = 0; j < *NINCX; j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               (*INCXS)[j] = atoi( ARGS[i++] );
            }
            break;
         case 'Y':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *NINCY = atoi( ARGS[i++] );
            if( *NINCY <= 0     ) PrintUsage( ARGS[0] );
            *INCYS = (int *)malloc( *NINCY * sizeof( int ) );
            ATL_assert( *INCYS );
            for( j = 0; j < *NINCY; j++ )
            {
               if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
               (*INCYS)[j] = atoi( ARGS[i++] );
            }
            break;

         case 'R':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );

            if( ( strcmp( ARGS[i], "ALL"  ) == 0 ) ||
                ( strcmp( ARGS[i], "all"  ) == 0 ) )
            {
#ifdef TREAL
               *NROUT = 16;
#else
               *NROUT = 17;
#endif
               *ROUTS = (enum LVL2_ROUT *)malloc( (*NROUT) *
                                                  sizeof( enum LVL2_ROUT ) );
               ATL_assert( *ROUTS );

               (*ROUTS)[ 0] = GBMV; (*ROUTS)[ 1] = GEMV; (*ROUTS)[ 2] = SBMV;
               (*ROUTS)[ 3] = SPMV; (*ROUTS)[ 4] = SYMV; (*ROUTS)[ 5] = TBMV;
               (*ROUTS)[ 6] = TPMV; (*ROUTS)[ 7] = TRMV; (*ROUTS)[ 8] = TBSV;
               (*ROUTS)[ 9] = TPSV; (*ROUTS)[10] = TRSV;
#ifdef TREAL
               (*ROUTS)[11] = GERC; (*ROUTS)[12] = SPR;  (*ROUTS)[13] = SYR;
               (*ROUTS)[14] = SPR2; (*ROUTS)[15] = SYR2;
#else
               (*ROUTS)[11] = GERC; (*ROUTS)[12] = GERU; (*ROUTS)[13] = SPR;
               (*ROUTS)[14] = SYR;  (*ROUTS)[15] = SPR2; (*ROUTS)[16] = SYR2;
#endif
               i++;
            }
            else
            {
               if( isdigit( *ARGS[i] ) ) { *NROUT = atoi( ARGS[i++] ); }
               else                      { *NROUT = 1;                 }
               *ROUTS = (enum LVL2_ROUT *)malloc( (*NROUT) *
                                                  sizeof( enum LVL2_ROUT ) );
               ATL_assert( *ROUTS );

               for( j = 0; j < *NROUT; j++ )
               {
                  if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );

                  if(      ( strcmp( ARGS[i], "GBMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "gbmv" ) == 0 ) )
                     (*ROUTS)[j] = GBMV;
                  else if( ( strcmp( ARGS[i], "GEMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "gemv" ) == 0 ) )
                     (*ROUTS)[j] = GEMV;
#ifdef TREAL
                  else if( ( strcmp( ARGS[i], "SBMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "sbmv" ) == 0 ) )
                     (*ROUTS)[j] = SBMV;
                  else if( ( strcmp( ARGS[i], "SPMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "spmv" ) == 0 ) )
                     (*ROUTS)[j] = SPMV;
                  else if( ( strcmp( ARGS[i], "SYMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "symv" ) == 0 ) )
                     (*ROUTS)[j] = SYMV;
#else
                  else if( ( strcmp( ARGS[i], "HBMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "hbmv" ) == 0 ) )
                     (*ROUTS)[j] = SBMV;
                  else if( ( strcmp( ARGS[i], "HPMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "hpmv" ) == 0 ) )
                     (*ROUTS)[j] = SPMV;
                  else if( ( strcmp( ARGS[i], "HEMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "hemv" ) == 0 ) )
                     (*ROUTS)[j] = SYMV;
#endif
                  else if( ( strcmp( ARGS[i], "TBMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "tbmv" ) == 0 ) )
                     (*ROUTS)[j] = TBMV;
                  else if( ( strcmp( ARGS[i], "TPMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "tpmv" ) == 0 ) )
                     (*ROUTS)[j] = TPMV;
                  else if( ( strcmp( ARGS[i], "TRMV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "trmv" ) == 0 ) )
                     (*ROUTS)[j] = TRMV;

                  else if( ( strcmp( ARGS[i], "TBSV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "tbsv" ) == 0 ) )
                     (*ROUTS)[j] = TBSV;
                  else if( ( strcmp( ARGS[i], "TPSV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "tpsv" ) == 0 ) )
                     (*ROUTS)[j] = TPSV;
                  else if( ( strcmp( ARGS[i], "TRSV" ) == 0 ) ||
                           ( strcmp( ARGS[i], "trsv" ) == 0 ) )
                     (*ROUTS)[j] = TRSV;
#ifdef TREAL
                  else if( ( strcmp( ARGS[i], "GER"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "ger"  ) == 0 ) )
                     (*ROUTS)[j] = GERU;
                  else if( ( strcmp( ARGS[i], "GER2"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "ger2"  ) == 0 ) )
                     (*ROUTS)[j] = GER2U;
                  else if( ( strcmp( ARGS[i], "SPR"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "spr"  ) == 0 ) )
                     (*ROUTS)[j] = SPR;
                  else if( ( strcmp( ARGS[i], "SYR"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "syr"  ) == 0 ) )
                     (*ROUTS)[j] = SYR;
                  else if( ( strcmp( ARGS[i], "SPR2" ) == 0 ) ||
                           ( strcmp( ARGS[i], "spr2" ) == 0 ) )
                     (*ROUTS)[j] = SPR2;
                  else if( ( strcmp( ARGS[i], "SYR2" ) == 0 ) ||
                           ( strcmp( ARGS[i], "syr2" ) == 0 ) )
                     (*ROUTS)[j] = SYR2;
#else
                  else if( ( strcmp( ARGS[i], "GERU" ) == 0 ) ||
                           ( strcmp( ARGS[i], "geru" ) == 0 ) )
                     (*ROUTS)[j] = GERU;
                  else if( ( strcmp( ARGS[i], "GERC" ) == 0 ) ||
                           ( strcmp( ARGS[i], "gerc" ) == 0 ) )
                     (*ROUTS)[j] = GERC;
                  else if( ( strcmp( ARGS[i], "GER2U" ) == 0 ) ||
                           ( strcmp( ARGS[i], "ger2u" ) == 0 ) )
                     (*ROUTS)[j] = GER2U;
                  else if( ( strcmp( ARGS[i], "GER2C" ) == 0 ) ||
                           ( strcmp( ARGS[i], "ger2c" ) == 0 ) )
                     (*ROUTS)[j] = GER2C;
                  else if( ( strcmp( ARGS[i], "HPR"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "hpr"  ) == 0 ) )
                     (*ROUTS)[j] = SPR;
                  else if( ( strcmp( ARGS[i], "HER"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "her"  ) == 0 ) )
                     (*ROUTS)[j] = SYR;
                  else if( ( strcmp( ARGS[i], "HPR2" ) == 0 ) ||
                           ( strcmp( ARGS[i], "hpr2" ) == 0 ) )
                     (*ROUTS)[j] = SPR2;
                  else if( ( strcmp( ARGS[i], "HER2" ) == 0 ) ||
                           ( strcmp( ARGS[i], "her2" ) == 0 ) )
                     (*ROUTS)[j] = SYR2;
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
      *ROUTS = (enum LVL2_ROUT *)malloc( sizeof( enum LVL2_ROUT ) );
      ATL_assert( *ROUTS );
      (*ROUTS)[0] = GEMV;
   }

   if( *NUPLO == -1 )
   {
      *NUPLO = 1;
      *UPLOS = (enum ATLAS_UPLO *)malloc( sizeof( enum ATLAS_UPLO ) );
      ATL_assert( *UPLOS );
      (*UPLOS)[0] = AtlasLower;
   }
   if( *NTRAN == -1 )
   {
      *NTRAN = 1;
      *TRANS = (enum ATLAS_TRANS *)malloc( sizeof( enum ATLAS_TRANS ) );
      ATL_assert( *TRANS );
      (*TRANS)[0] = AtlasNoTrans;
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

   if( *NINCX == -1 )
   {
      *NINCX = 1;
      *INCXS = (int *)malloc(sizeof(int));
      ATL_assert( *INCXS );
      (*INCXS)[0] = 1;
   }
   if( *NINCY == -1 )
   {
      *NINCY = 1;
      *INCYS = (int *)malloc( sizeof( int ) );
      ATL_assert( *INCYS );
      (*INCYS)[0] = 1;
   }
}

int main( int NARGS, char **ARGS )
{
   int                        klinc, klstart, klstop, kuinc, kustart,
                              kustop, ldaism, mflopmin, minc, mstart,
                              mstop, ninc, nstart, nstop, nalpha, nbeta,
                              cachesize,
                              ndiag, nincx, nincy, nrout, ntran, nuplo, test;
   int                        * incxs  = NULL, * incys = NULL;
   TYPE                       * alphas = NULL, * betas = NULL;
   enum LVL2_ROUT             * routs  = NULL;
   enum ATLAS_UPLO            * uplos  = NULL;
   enum ATLAS_TRANS           * trans  = NULL;
   enum ATLAS_DIAG            * diags  = NULL;

   GetFlags( NARGS, ARGS, &nrout, &routs, &test, &ldaism, &cachesize,
	     &mflopmin, &nuplo, &uplos, &ntran, &trans, &ndiag, &diags,
	     &mstart, &mstop, &minc, &nstart, &nstop, &ninc, &klstart,
	     &klstop, &klinc, &kustart, &kustop, &kuinc, &nalpha,
	     &alphas, &nbeta, &betas, &nincx, &incxs, &nincy, &incys );

   RunCases( test, cachesize, mflopmin, ldaism, nuplo, uplos, ntran, trans,
	     ndiag, diags, mstart, mstop, minc, nstart, nstop, ninc, klstart,
             klstop, klinc, kustart, kustop, kuinc, nalpha, alphas, nbeta,
             betas, nincx, incxs, nincy, incys, nrout, routs );

   if( uplos  ) free( uplos  );
   if( trans  ) free( trans  );
   if( diags  ) free( diags  );
   if( alphas ) free( alphas );
   if( incxs  ) free( incxs  );
   if( incys  ) free( incys  );
   if( betas  ) free( betas  );
   if( routs  ) free( routs  );

   return( 0 );
}
