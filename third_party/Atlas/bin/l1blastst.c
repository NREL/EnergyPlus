/*
 * =====================================================================
 * Include files
 * =====================================================================
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
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

#define    ATL_isf77amax           ATL_sf77amax
#define    ATL_idf77amax           ATL_df77amax
#define    ATL_icf77amax           ATL_cf77amax
#define    ATL_izf77amax           ATL_zf77amax

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
 *    USE_L1_REFERENCE : C ATLAS reference implementation,
 *
 * If none of these macros is defined at compile time, the  ATLAS imple-
 * mentation is to be tested against itself,  after all this is the only
 * version we are sure to have available.
 *
 * By default the mono-threaded  ATLAS  routines are tested. To test the
 * multi-threaded ATLAS routines, define the following macro:
 *    USE_L1_PTHREADS  : multi-threaded ATLAS implementation.
 */

#define USE_F77_BLAS

#if defined(ATL_USEPTHREADS) && !defined(ATL_MIKE)
   #define USE_L1_PTHREADS
#endif
/*
 * =====================================================================
 */
#if   defined( USE_F77_BLAS ) /* Trusted BLAS version to test against */
#define  TP1      Mjoin( PATL,    f77 )
#ifdef TREAL
#define  TSC      TP1
#define  TCS      TP1
#else
#define  TSC      Mjoin( Mjoin( PATLU, PRE ), f77 )
#define  TCS      Mjoin( Mjoin( PATL,  UPR ), f77 )
#endif
#ifdef SREAL
#define  TDS      Mjoin( ATL_,  dsf77 )
#define  TSD      Mjoin( ATL_, sdsf77 )
#endif
#elif defined( USE_L1_REFERENCE )
#include "atlas_reflevel1.h"
#define  TP1      Mjoin( PATL,    ref )
#ifdef TREAL
#define  TSC      TP1
#define  TCS      TP1
#else
#define  TSC      Mjoin( Mjoin( PATLU, PRE ), ref )
#define  TCS      Mjoin( Mjoin( PATL,  UPR ), ref )
#endif
#ifdef SREAL
#define  TDS      Mjoin( ATL_,  dsref )
#define  TSD      Mjoin( ATL_, sdsref )
#endif
#else /* defined( USE_L1_ATLAS ) */  /* use ATLAS itself !! (default) */
#include "atlas_level1.h"
#define  TP1      PATL
#ifdef TREAL
#define  TSC      TP1
#define  TCS      TP1
#else
#define  TSC      Mjoin( PATLU, PRE )
#define  TCS      Mjoin( PATL,  UPR )
#endif
#ifdef SREAL
#define  TDS      Mjoin( ATL_,  ds )
#define  TSD      Mjoin( ATL_, sds )
#endif
#endif

#define  trusted_rotg(                         A,  B,  C,  S         ) \
Mjoin( TP1, rotg )(                            A,  B,  C,  S         )

#ifdef TREAL
#define  trusted_rotmg(                        D1, D2, X1, Y1, PARAM ) \
Mjoin( TP1, rotmg )(                           D1, D2, X1, Y1, PARAM )
#endif

#define trusted_nrm2(    N,     X, iX                                ) \
Mjoin( TSC, nrm2 )(      N,     X, iX                                )
#define trusted_asum(    N,     X, iX                                ) \
Mjoin( TSC, asum )(      N,     X, iX                                )
#if   defined( USE_F77_BLAS )
#define trusted_amax(    N,     X, iX                                ) \
Mjoin( TP1, amax )(      N,     X, iX                                )
#elif defined( USE_L1_REFERENCE )
#define trusted_amax(    N,     X, iX                                ) \
Mjoin( Mjoin( ATL_i, PRE ), refamax ) \
                  (      N,     X, iX                                )
#else
#define trusted_amax(    N,     X, iX                                ) \
Mjoin( Mjoin( ATL_i, PRE ), amax) \
                  (      N,     X, iX                                )
#endif
#define trusted_scal(    N, al, X, iX                                ) \
Mjoin( TP1, scal )(      N, al, X, iX                                )

#ifdef TCPLX
#if   defined( USE_F77_BLAS ) || defined( USE_L1_REFERENCE )
#define trusted_rscal(   N, al, X, iX                                ) \
Mjoin( TCS, scal ) (     N, al, X, iX                                )
#else
#define trusted_rscal(   N, al, X, iX                                ) \
Mjoin( TP1, scal ) (     N, al, X, iX                                )
#endif
#endif

#define trusted_axpy(    N, al, X, iX, Y, iY                         ) \
Mjoin( TP1, axpy )(      N, al, X, iX, Y, iY                         )
#define trusted_copy(    N,     X, iX, Y, iY                         ) \
Mjoin( TP1, copy )(      N,     X, iX, Y, iY                         )
#define trusted_swap(    N,     X, iX, Y, iY                         ) \
Mjoin( TP1, swap )(      N,     X, iX, Y, iY                         )
#define trusted_rot(     N,     X, iX, Y, iY,          C,  S         ) \
Mjoin( TCS, rot )(       N,     X, iX, Y, iY,          C,  S         )

#ifdef TREAL
#define trusted_rotm(    N,     X, iX, Y, iY,                  PARAM ) \
Mjoin( TP1, rotm )(      N,     X, iX, Y, iY,                  PARAM )
#define trusted_dot(     N,     X, iX, Y, iY                         ) \
Mjoin( TP1, dot )(       N,     X, iX, Y, iY                         )
#else
#define trusted_dotc(    N,     X, iX, Y, iY,                  DOT   ) \
Mjoin( TP1, dotc_sub )(  N,     X, iX, Y, iY,                  DOT   )
#define trusted_dotu(    N,     X, iX, Y, iY,                  DOT   ) \
Mjoin( TP1, dotu_sub )(  N,     X, iX, Y, iY,                  DOT   )
#endif

#ifdef SREAL
#define trusted_dsdot(   N,     X, iX, Y, iY                         ) \
Mjoin( TDS, dot )(       N,     X, iX, Y, iY                         )
#define trusted_sdsdot(  N, B,  X, iX, Y, iY                         ) \
Mjoin( TSD, dot )(       N, B,  X, iX, Y, iY                         )
#endif
/*
 * ATLAS version of the BLAS to test.
 */
#if defined( USE_L1_PTHREADS )
   #include "atlas_pthreads.h"
   #include "atlas_ptlvl1.h"
   #define  AP1      Mjoin( PATL,   pt  )
   #ifdef TREAL
      #define  ASC      AP1
      #define  ACS      AP1
   #else
      #define  ASC      Mjoin( Mjoin( PATLU, PRE ),  t )
      #define  ACS      Mjoin( Mjoin( PATL,  UPR ),  t )
   #endif
   #ifdef SREAL
      #define  ADS      Mjoin( ATL_,   dst )
      #define  ASD      Mjoin( ATL_,  sdst )
   #endif
#else
   #ifdef ATL_MIKE
      #include "atlas_pthreads.h"
   #endif
   #include "atlas_level1.h"
   #define  AP1      PATL
   #ifdef TREAL
      #define  ASC      AP1
      #define  ACS      AP1
   #else
      #define  ASC      Mjoin( PATLU, PRE )
      #define  ACS      Mjoin( PATL,  UPR )
   #endif
   #ifdef SREAL
      #define  ADS      Mjoin( ATL_,  ds )
      #define  ASD      Mjoin( ATL_, sds )
   #endif
#endif

#define  test_rotg(                            A,  B,  C,  S         ) \
Mjoin( AP1, rotg )(                            A,  B,  C,  S         )

#ifdef TREAL
#define  test_rotmg(                           D1, D2, X1, Y1, PARAM ) \
Mjoin( AP1, rotmg )(                           D1, D2, X1, Y1, PARAM )
#endif

#define test_nrm2(       N,     X, iX                                ) \
Mjoin( ASC, nrm2 )(      N,     X, iX                                )
#define test_asum(       N,     X, iX                                ) \
Mjoin( ASC, asum )(      N,     X, iX                                )
#if defined( USE_L1_PTHREADS )
#define test_amax(       N,     X, iX                                ) \
Mjoin( Mjoin( ATL_i, PRE ), tamax ) \
                  (      N,     X, iX                                )
#else
#define test_amax(       N,     X, iX                                ) \
Mjoin( Mjoin( ATL_i, PRE ), amax ) \
                  (      N,     X, iX                                )
#endif
#define test_scal(       N, al, X, iX                                ) \
Mjoin( AP1, scal )(      N, al, X, iX                                )

#ifdef TCPLX
#if   defined( USE_L1_PTHREADS )
#define test_rscal(      N, al, X, iX                                ) \
Mjoin( ACS, scal ) (     N, al, X, iX                                )
#else
#define test_rscal(      N, al, X, iX                                ) \
Mjoin( AP1, scal ) (     N, al, X, iX                                )
#endif
#endif

#define test_axpy(       N, al, X, iX, Y, iY                         ) \
Mjoin( AP1, axpy )(      N, al, X, iX, Y, iY                         )
#define test_copy(       N,     X, iX, Y, iY                         ) \
Mjoin( AP1, copy )(      N,     X, iX, Y, iY                         )
#define test_swap(       N,     X, iX, Y, iY                         ) \
Mjoin( AP1, swap )(      N,     X, iX, Y, iY                         )
#define test_rot(        N,     X, iX, Y, iY,          C,  S         ) \
Mjoin( ACS, rot )(       N,     X, iX, Y, iY,          C,  S         )

#ifdef TREAL
#define test_rotm(       N,     X, iX, Y, iY,                  PARAM ) \
Mjoin( AP1, rotm )(      N,     X, iX, Y, iY,                  PARAM )
   #ifdef ATL_MIKE
      TYPE Mjoin(PRE,dot_mike)(const int N, TYPE *X, const int incX,
                               TYPE *Y, const int incY);
      #define test_dot(        N,     X, iX, Y, iY                         ) \
         Mjoin(PRE,dot_mike)(N, X, iX, Y, iY)
   #else
      #define test_dot(        N,     X, iX, Y, iY                         ) \
         Mjoin( AP1, dot )(       N,     X, iX, Y, iY                         )
   #endif
#else
#define test_dotc(       N,     X, iX, Y, iY,                  DOT   ) \
Mjoin( AP1, dotc_sub )(  N,     X, iX, Y, iY,                  DOT   )
#define test_dotu(       N,     X, iX, Y, iY,                  DOT   ) \
Mjoin( AP1, dotu_sub )(  N,     X, iX, Y, iY,                  DOT   )
#endif

#ifdef SREAL
#define test_dsdot(      N,     X, iX, Y, iY                         ) \
Mjoin( ADS, dot )(       N,     X, iX, Y, iY                         )
#define test_sdsdot(     N, B,  X, iX, Y, iY                         ) \
Mjoin( ASD, dot )(       N, B,  X, iX, Y, iY                         )
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
enum LVL1_ROUT /* 16 + 1 = 17 */
{
   AXPY=0, COPY, SWAP, ROT, ROTM, DOTU, DOTC, DSDOT, SDSDOT, ROTG,
   ROTMG, NRM2, ASUM, AMAX, SCAL, RSCAL, ALLROUTS
};
/*
 * =====================================================================
 * Prototypes for the testing routines
 * =====================================================================
 */
double     opbl1
(  const enum LVL1_ROUT,           const int );
TYPE rotgtst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const TYPE,
   double *,       double *,       double *,       double * );
#ifdef TREAL
TYPE rotmgtst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const TYPE,
   double *,       double *,       double *,       double * );
#endif
TYPE       sumtst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       nrmtst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       maxtst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const TYPE,     double *,       double *,
   double *,       double * );
TYPE       scltst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const SCALAR,   const int,      const TYPE,     double *,
   double *,       double *,       double * );
#ifdef TCPLX
TYPE       rsctst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const SCALAR,   const int,      const TYPE,     double *,
   double *,       double *,       double * );
#endif
TYPE       xpytst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const SCALAR,   const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );
TYPE       cpytst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
TYPE       swptst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
TYPE       dottst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
TYPE       rottst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     const TYPE,
   const TYPE,     double *,       double *,       double *,
   double * );
#ifdef TREAL
TYPE       rotmtst
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE *,   const TYPE,
   double *,       double *,       double *,       double * );
#endif

int        rotgcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const TYPE,     double *,       double *,       double *,
   double * );
#ifdef TREAL
int        rotmgcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const TYPE,     double *,       double *,       double *,
   double * );
#endif
int        sumcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        nrmcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        maxcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const TYPE,     double *,
   double *,       double *,       double * );
int        sclcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const SCALAR,   const int,      const TYPE,
   double *,       double *,       double *,       double * );
#ifdef TCPLX
int        rsccase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const SCALAR,   const int,      const TYPE,
   double *,       double *,       double *,       double * );
#endif
int        xpycase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const SCALAR,   const int,      const int,
   const TYPE,     double *,       double *,       double *,
   double * );
int        cpycase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );
int        swpcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );
int        dotcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const TYPE,
   double *,       double *,       double *,       double * );
int        rotcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const TYPE,
   const TYPE,     const TYPE,     double *,       double *,
   double *,       double * );
#ifdef TREAL
int        rotmcase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const TYPE *,
   const TYPE,     double *,       double *,       double *,
   double * );
#endif

void       RunrotgCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,     const int,
   const TYPE,     int *,          int * );
#ifdef TREAL
void       RunrotmgCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,     const int,
   const TYPE,     int *,          int * );
#endif
void       RunsumCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const TYPE,     int *,          int * );
void       RunnrmCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const TYPE,     int *,          int * );
void       RunmaxCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const TYPE,     int *,          int * );
void       RunsclCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const TYPE *,   const int,      const int *,    const TYPE,
   int *,          int * );
#ifdef TCPLX
void       RunrscCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const TYPE *,   const int,      const int *,    const TYPE,
   int *,          int * );
#endif
void       RunxpyCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const TYPE *,   const int,      const int *,    const int,
   const int *,    const TYPE,     int *,          int * );
void       RuncpyCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const int,      const int *,    const TYPE,
   int *,          int * );
void       RunswpCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const int,      const int *,    const TYPE,
   int *,          int * );
void       RundotCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const int,      const int *,    const TYPE,
   int *,          int * );
void       RunrotCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const int,      const int *,    const int,
   const TYPE *,   const TYPE,     int *,          int * );
#ifdef TREAL
void       RunrotmCase
(  const int CACHESIZE,
   const enum LVL1_ROUT,           const int,      const int,
   const int,      const int,      const int,      const int,
   const int *,    const int,      const int *,    const TYPE,
   int *,          int * );
#endif

void       RunCases
(  const int,      const int,      const int,      const int,
   const int,      const int,      const int,      const TYPE *,
   const int,      const int *,    const int,      const int *,
   const int,      const enum LVL1_ROUT * );

void       PrintUsage
(  char * );

void       GetFlags
(  int,            char **,        int *,          enum LVL1_ROUT **,
   int *,          int *,          int *,          int *,
   int *,          int *,          int *,          TYPE **,
   int *,          int **,         int *,          int ** );

int        main
(  int,             char ** );
/*
 * =====================================================================
 */
double opbl1
(
   const enum LVL1_ROUT       ROUT,
   const int                  N
)
{
   double                     adds = 0.0, en, muls = 0.0;

   if( N <= 0 ) return( 0.0 );

   en = (double)(N);

                        /* Approximate count for rotation computation */
   if(      ROUT == ROTG   ) { muls = 8.0;           adds = 6.0;        }
        /* Approximate count for modified-Givens rotation computation */
   else if( ROUT == ROTMG  ) { muls = 9.0;           adds = 3.0;        }
                            /* Square root counted as 9 muls + 8 adds */
   else if( ROUT == NRM2   ) { muls = en+9.0;        adds = en-1.0+8.0; }
                                  /* Absolute value counted as 1 adds */
   else if( ROUT == ASUM   ) { muls = 0.0;           adds = 2.0 * en;   }
                           /* Absolute value + test counted as 2 adds */
   else if( ROUT == AMAX   ) { muls = 0.0;           adds = 2.0 * en;   }
   else if( ROUT == SCAL   ) { muls = en;            adds = 0.0;        }
                         /* A little bit of cheating for real scaling */
   else if( ROUT == RSCAL  ) { muls = 0.0;           adds = en;         }
   else if( ROUT == AXPY   ) { muls = en;            adds = en;         }
                        /* Count copy just like adds for practicality */
   else if( ROUT == COPY   ) { muls = 0.0;           adds = en;         }
                        /* Count swap just like adds for practicality */
   else if( ROUT == SWAP   ) { muls = 0.0;           adds = 2.0 * en;   }
#ifdef TREAL
   else if( ROUT == ROT    ) { muls = 4.0*en;        adds = 2.0 * en;   }
#else
   else if( ROUT == ROT    ) { muls = (4.0*en)/3.0;  adds = 2.0 * en;   }
#endif
   else if( ROUT == ROTM   ) { muls = 4.0*en;        adds = 2.0 * en;   }
   else if( ROUT == DOTU   ) { muls = en;            adds = en - 1.0;   }
   else if( ROUT == DOTC   ) { muls = en;            adds = en - 1.0;   }
   else if( ROUT == DSDOT  ) { muls = en;            adds = en - 1.0;   }
   else if( ROUT == SDSDOT ) { muls = en;            adds = en;         }

#ifdef TREAL
   return(       muls +       adds );
#else
   return( 6.0 * muls + 2.0 * adds );
#endif
}
/*
 * =====================================================================
 * tst functions
 * =====================================================================
 */
TYPE rotgtst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       residA, residB, residC, residS;
#ifdef TREAL
   TYPE                       te_A, tr_A, te_B, tr_B, te_C, tr_C, te_S, tr_S;
#else
   TYPE                       te_A[2], tr_A[2], te_B[2], tr_B[2], te_C, tr_C,
                              te_S[2], tr_S[2], diff[2];
#endif
   int                        Aseed, Bseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0; ops = opbl1( ROUT, 0 );
/*
 * Generate random operands
 */
   Aseed = 129;
   Bseed = 213;
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );

#ifdef TREAL
   Mjoin( PATL, gegen )( 1, 1, &te_A, 1, Aseed );
   Mjoin( PATL, gegen )( 1, 1, &tr_A, 1, Aseed );
   Mjoin( PATL, gegen )( 1, 1, &te_B, 1, Bseed );
   Mjoin( PATL, gegen )( 1, 1, &tr_B, 1, Bseed );
#else
   Mjoin( PATL, gegen )( 1, 1, te_A, 1, Aseed );
   Mjoin( PATL, gegen )( 1, 1, tr_A, 1, Aseed );
   Mjoin( PATL, gegen )( 1, 1, te_B, 1, Bseed );
   Mjoin( PATL, gegen )( 1, 1, tr_B, 1, Bseed );
#endif
/*
 * Start timing operations for the trusted routine with cold caches.
 */
   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
#ifdef TREAL
   trusted_rotg( &tr_A, &tr_B, &tr_C, &tr_S );
#else
   trusted_rotg(  tr_A,  tr_B, &tr_C,  tr_S );
#endif
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start timing operations for the tested routine with cold caches.
 */
   l2ret  = ATL_flushcache( -1 );
   t0    = time00();
#ifdef TREAL
   test_rotg( &te_A, &te_B, &te_C, &te_S );
#else
   test_rotg(  te_A,  te_B, &te_C,  te_S );
#endif
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { return( ATL_rzero );
     }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
#ifdef TREAL
   residA = Mabs1( tr_A - te_A ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
   residB = Mabs1( tr_B - te_B ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
   residC = Mabs1( tr_C - te_C ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
   residS = Mabs1( tr_S - te_S ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
#else
   diff[0] = tr_A[0] - te_A[0]; diff[1] = tr_A[1] - te_A[1];
   residA = Mabs1( diff ) / ( Mmax( 1.0, ops ) * EPSILON );
   diff[0] = tr_B[0] - te_B[0]; diff[1] = tr_B[1] - te_B[1];
   residB = Mabs1( diff ) / ( Mmax( 1.0, ops ) * EPSILON );
   residC = Mabs(  tr_C - te_C ) / ( Mmax( 1.0, ops ) * EPSILON );
   diff[0] = tr_S[0] - te_S[0]; diff[1] = tr_S[1] - te_S[1];
   residS = Mabs1( diff ) / ( Mmax( 1.0, ops ) * EPSILON );
#endif

   if( ( residA > THRESH ) || ( residA != residA ) )
   {
#ifdef TREAL
      (void) fprintf( stderr, "ERROR:  residA=%f, tr_A=%f, te_A=%f, eps=%e\n",
                      residA, tr_A, te_A, EPSILON );
#else
      (void) fprintf( stderr,
 "ERROR:  residA=%f, tr_A[0]=%f, tr_A[1]=%f, te_A[0]=%f, te_A[1]=%f, eps=%e\n",
 residA, tr_A[0], tr_A[1], te_A[0], te_A[1], EPSILON );
#endif
   }

   if( ( residB > THRESH ) || ( residB != residB ) )
   {
#ifdef TREAL
      (void) fprintf( stderr, "ERROR:  residB=%f, tr_B=%f, te_B=%f, eps=%e\n",
                      residB, tr_B, te_B, EPSILON );
#else
      (void) fprintf( stderr,
 "ERROR:  residB=%f, tr_B[0]=%f, tr_B[1]=%f, te_B[0]=%f, te_B[1]=%f, eps=%e\n",
 residB, tr_B[0], tr_B[1], te_B[0], te_B[1], EPSILON );
#endif
   }

   if( ( residC > THRESH ) || ( residC != residC ) )
   {
      (void) fprintf( stderr, "ERROR:  residC=%f, tr_C=%f, te_C=%f, eps=%e\n",
                      residC, tr_C, te_C, EPSILON );
   }

   if( ( residS > THRESH ) || ( residS != residS ) )
   {
#ifdef TREAL
      (void) fprintf( stderr, "ERROR:  residS=%f, tr_S=%f, te_S=%f, eps=%e\n",
                      residS, tr_S, te_S, EPSILON );
#else
      (void) fprintf( stderr,
 "ERROR:  residS=%f, tr_S[0]=%f, tr_S[1]=%f, te_S[0]=%f, te_S[1]=%f, eps=%e\n",
 residS, tr_S[0], tr_S[1], te_S[0], te_S[1], EPSILON );
#endif
   }


   return( Mmax( Mmax( Mmax( residA, residB ), residC ), residS ) );
}

#ifdef TREAL
TYPE rotmgtst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       residD1, residD2, residX1, residP;
   TYPE                       te_D1, tr_D1, te_D2, tr_D2, te_X1, tr_X1,
                              te_Y1, tr_Y1, te_PARAM[5], tr_PARAM[5];
   int                        D1seed, D2seed, X1seed, Y1seed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0; ops = opbl1( ROUT, 0 );
/*
 * Generate random operands
 */
   D1seed = 129; D2seed = 213; X1seed = 319; Y1seed = 222;

   Mjoin( PATL, gegen )( 1, 1, &te_D1, 1, D1seed ); te_D1 = Mabs( te_D1 );
   Mjoin( PATL, gegen )( 1, 1, &tr_D1, 1, D1seed ); tr_D1 = Mabs( tr_D1 );
   Mjoin( PATL, gegen )( 1, 1, &te_D2, 1, D2seed );
   Mjoin( PATL, gegen )( 1, 1, &tr_D2, 1, D2seed );
   Mjoin( PATL, gegen )( 1, 1, &te_X1, 1, X1seed );
   Mjoin( PATL, gegen )( 1, 1, &tr_X1, 1, X1seed );
   Mjoin( PATL, gegen )( 1, 1, &te_Y1, 1, Y1seed );
   Mjoin( PATL, gegen )( 1, 1, &tr_Y1, 1, Y1seed );

   te_PARAM[0] = ATL_rzero; tr_PARAM[0] = ATL_rzero;
   te_PARAM[1] = ATL_rzero; tr_PARAM[1] = ATL_rzero;
   te_PARAM[2] = ATL_rzero; tr_PARAM[2] = ATL_rzero;
   te_PARAM[3] = ATL_rzero; tr_PARAM[3] = ATL_rzero;
   te_PARAM[4] = ATL_rzero; tr_PARAM[4] = ATL_rzero;

   /* Allocate L2 cache space */
   l2ret = ATL_flushcache( CACHESIZE );
/*
 * Start cold cache timing operations for the trusted routine
 */
   l2ret = ATL_flushcache( -1 ); /* flush */
   t0     = time00();
   trusted_rotmg( &tr_D1, &tr_D2, &tr_X1, tr_Y1, tr_PARAM );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   l2ret = ATL_flushcache( -1 ); /* flush */
   t0    = time00();
   test_rotmg( &te_D1, &te_D2, &te_X1, te_Y1, te_PARAM );
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret = ATL_flushcache( 0 ); /* dealloc */
   if( !( TEST ) ) { return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   residD1 = Mabs( tr_D1 - te_D1 ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
   residD2 = Mabs( tr_D2 - te_D2 ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
   residX1 = Mabs( tr_X1 - te_X1 ) / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );
   residP  = Mabs( tr_PARAM[0] - te_PARAM[0] ) +
             Mabs( tr_PARAM[1] - te_PARAM[1] ) +
             Mabs( tr_PARAM[2] - te_PARAM[2] ) +
             Mabs( tr_PARAM[3] - te_PARAM[3] ) +
             Mabs( tr_PARAM[4] - te_PARAM[4] );
   residP  = residP / ( 2.0 * Mmax( 1.0, ops ) * EPSILON );

   if( ( residD1 > THRESH ) || ( residD1 != residD1 ) )
   {
      (void) fprintf( stderr,
                      "ERROR:  residD1=%f, tr_D1=%f, te_D1=%f, eps=%e\n",
                      residD1, tr_D1, te_D1, EPSILON );
   }

   if( ( residD2 > THRESH ) || ( residD2 != residD2 ) )
   {
      (void) fprintf( stderr,
                      "ERROR:  residD2=%f, tr_D2=%f, te_D2=%f, eps=%e\n",
                      residD2, tr_D2, te_D2, EPSILON );
   }

   if( ( residX1 > THRESH ) || ( residX1 != residX1 ) )
   {
      (void) fprintf( stderr,
                      "ERROR:  residX1=%f, tr_X1=%f, te_X1=%f, eps=%e\n",
                      residX1, tr_X1, te_X1, EPSILON );
   }

   if( ( residP > THRESH ) || ( residP != residP ) )
   {
      (void) fprintf( stderr,
      "ERROR:  residP=%f, tr_P=%f,%f,%f,%f,%f, te_P=%f,%f,%f,%f,%f, eps=%e\n",
      residP, tr_PARAM[0], tr_PARAM[1], tr_PARAM[2], tr_PARAM[3], tr_PARAM[4],
      te_PARAM[0], te_PARAM[1], te_PARAM[2], te_PARAM[3], te_PARAM[4],
      EPSILON );
   }

   return( Mmax( Mmax( Mmax( residD1, residD2 ), residX1 ), residP ) );
}
#endif

TYPE sumtst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normX, te_sum, tr_sum, resid;
   TYPE                       * X = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X     = (TYPE *)malloc( ATL_MulBySize( N ) * aincX );

   if( X == NULL )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      return( ATL_rnone );
   }
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   tr_sum = trusted_asum( N, x, INCX );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   te_sum = test_asum( N, x, INCX );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normX = Mjoin( PATL, infnrm )( N, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;

   free( X );

   resid = tr_sum - te_sum;
   resid = Mabs( resid ) / ( EPSILON * Mmax( normX, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, tr_sum=%f, te_sum=%f, normX=%f, eps=%e\n",
      resid, tr_sum, te_sum, normX, EPSILON );
   }

   return( resid );
}

TYPE nrmtst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normX, te_nrm, tr_nrm, resid;
   TYPE                       * X = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X     = (TYPE *)malloc( ATL_MulBySize( N ) * aincX );

   if( X == NULL )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      return( ATL_rnone );
   }
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   tr_nrm = trusted_nrm2( N, x, INCX );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   te_nrm = test_nrm2( N, x, INCX );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   normX = Mjoin( PATL, infnrm )( N, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;

   free( X );

   resid = tr_nrm - te_nrm;
   resid = Mabs( resid ) / ( EPSILON * Mmax( normX, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, tr_nrm=%f, te_nrm=%f, normX=%f, eps=%e\n",
      resid, tr_nrm, te_nrm, normX, EPSILON );
   }

   return( resid );
}

TYPE maxtst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   TYPE                       * X = NULL, * x;
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       resid;
   const int                  aincX = Mabs( INCX );
   int                        te_max, tr_max, Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X     = (TYPE *)malloc( ATL_MulBySize( N ) * aincX );

   if( X == NULL )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      return( ATL_rnone );
   }
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   tr_max = trusted_amax( N, x, INCX );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   te_max = test_amax( N, x, INCX );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); return( ATL_rzero ); }
/*
 * else perform error check
 */
   free( X );

   resid = ( (TYPE)(tr_max - te_max) ) / ( EPSILON );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, tr_max=%d, te_max=%d, eps=%e\n",
      resid, tr_max, te_max, EPSILON );
   }

   return( resid );
}

TYPE scltst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normD, normX, resid;
   TYPE                       * X = NULL, * X0, * XD = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( SCALAR_IS_ONE( ALPHA ) ) ops = 0.0; else ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X = (TYPE *)malloc( ATL_MulBySize( N ) * aincX * 2 );

   if( X == NULL )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      return( ATL_rnone );
   }

   X0 = X + N * ( aincX SHIFT );
/*
 * Generate random operands
 */
   Xseed = N * aincX * 27 + 213;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, X0, aincX, Xseed );
/*
 * Compute the norm of X for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( Mabs1( ALPHA ) > ATL_rone ) normX *= Mabs1( ALPHA );
      if( normX == ATL_rzero ) normX = ATL_rone;
   }
   else { normX = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_scal( N, ALPHA, x, INCX );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   test_scal( N, ALPHA, x, INCX );
   ttest  = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   XD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( X ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );

   normD = Mjoin( PATL, infnrm )( N, XD, 1 );
   resid = normD / ( EPSILON * Mmax( normX, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normX=%f, eps=%e\n",
      resid, normD, normX, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
#endif
   }

   free( X  );
   free( XD );

   return( resid );
}

#ifdef TCPLX

TYPE rsctst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normD, normX, resid, Calph[2];
   TYPE                       * X = NULL, * X0, * XD = NULL, * x;
   const int                  aincX = Mabs( INCX );
   int                        Xseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( *ALPHA == ATL_rone ) ops = 0.0; else ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE *)malloc( ATL_MulBySize( N ) * aincX * 2 );

   if( X == NULL )
   {
      l2ret  = ATL_flushcache( 0 );
      if( X  ) free( X  );
      return( ATL_rnone );
   }

   X0 = X + N * ( aincX SHIFT );
/*
 * Generate random operands
 */
   Xseed = N * aincX * 27 + 213;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, X0, aincX, Xseed );

   Calph[0] = *ALPHA; Calph[1] = ATL_rzero;
/*
 * Compute the norm of X for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( Mabs( *ALPHA ) > ATL_rone ) normX *= Mabs( *ALPHA );
      if( normX == ATL_rzero ) normX = ATL_rone;
   }
   else { normX = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret = ATL_flushcache( -1 );
#if   defined( USE_F77_BLAS ) || defined( USE_L1_REFERENCE )
   t0 = time00(); trusted_rscal( N, Calph[0], x, INCX ); ttrust = time00() - t0;
#else
   t0 = time00(); trusted_rscal( N, Calph,    x, INCX ); ttrust = time00() - t0;
#endif
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );

   l2ret = ATL_flushcache( -1 );
#if   defined( USE_L1_PTHREADS )
   t0 = time00(); test_rscal( N, Calph[0], x, INCX ); ttest = time00() - t0;
#else
   t0 = time00(); test_rscal( N, Calph,    x, INCX ); ttest = time00() - t0;
#endif
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   XD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( X ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );

   normD = Mjoin( PATL, infnrm )( N, XD, 1 );
   resid = normD / ( EPSILON * Mmax( normX, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normX=%f, eps=%e\n",
      resid, normD, normX, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
#endif
   }

   free( X  );
   free( XD );

   return( resid );
}

#endif


TYPE xpytst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normD, normX, normY, resid;
   TYPE                       * X = NULL, * Y = NULL, * Y0, * YD = NULL,
                              * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE *)malloc( ATL_MulBySize( N ) * aincX     );
   Y  = (TYPE *)malloc( ATL_MulBySize( N ) * aincY * 2 );

   if( ( X == NULL ) || ( Y == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   Y0 = Y + N * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;
   Yseed = N  * aincY;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1,  N, Y0, aincY, Yseed );
/*
 * Compute the norm of Y for later use in testing
 */
   if( TEST )
   {
      normY = Mjoin( PATL, infnrm )( N, Y, aincY );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
   else { normY = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_axpy( N, ALPHA, x, INCX, y, INCY );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret = ATL_flushcache( -1 );
   t0    = time00();
   test_axpy( N, ALPHA, x, INCX, y, INCY );
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   normX = Mjoin( PATL, infnrm )( N, X, aincX );
   if( Mabs1( ALPHA ) > ATL_rone ) normX *= Mabs1( ALPHA );
   if( normX == ATL_rzero ) normX = ATL_rone;

   free( X );

   YD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( YD == NULL ) { free( Y ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, Y, aincY, Y0, aincY, YD, 1 );

   normD = Mjoin( PATL, infnrm )( N, YD, 1 );
   resid = normD / ( EPSILON * Mmax( normX, ATL_rone ) *
                     Mmax( normY, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, normX=%f, normY=%f, eps=%e\n",
      resid, normD, normX, normY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "Y_trusted", 1, N, Y0, aincY );
      Mjoin( PATL, geprint )( "Y_test",    1, N, Y,  aincY );
#endif
   }

   free( Y  );
   free( YD );

   return( resid );
}

TYPE cpytst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normD, normY, resid;
   TYPE                       * X = NULL, * Y = NULL, * Y0,
                              * YD = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE   *)malloc( ATL_MulBySize( N   ) * aincX     );
   Y  = (TYPE   *)malloc( ATL_MulBySize( N   ) * aincY * 2 );

   if( ( X == NULL ) || ( Y == NULL ) )
   {
      l2ret  = ATL_flushcache( 0 );
      if( X  ) free( X  );
      if( Y  ) free( Y  );
      return( ATL_rnone );
   }

   Y0 = Y + N * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;
   Yseed = N  * aincY;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1,  N, Y0, aincY, Yseed );
/*
 * Compute the norm of Y for later use in testing
 */
   if( TEST )
   {
      normY = Mjoin( PATL, infnrm )( N, Y, aincY );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X;  if( INCX < 0 ) x = X  + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_copy( N, x, INCX, y, INCY );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret = ATL_flushcache( -1 );
   t0    = time00();
   test_copy( N, x, INCX, y, INCY );
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   free( X );

   YD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( YD == NULL ) { free( Y ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, Y, aincY, Y0, aincY, YD, 1 );

   normD = Mjoin( PATL, infnrm )( N, YD, 1 );
   resid = normD / ( EPSILON * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normD=%f, eps=%e\n", resid, normD, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "Y_trusted", 1, N, Y0, aincY );
      Mjoin( PATL, geprint )( "Y_test",    1, N, Y,  aincY );
#endif
   }

   free( Y  );
   free( YD );

   return( resid );
}

TYPE swptst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normDX, normDY, normX, normY, resid;
   TYPE                       * X = NULL, * X0, * XD = NULL, * Y = NULL,
                              * Y0, * YD = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE   *)malloc( ATL_MulBySize( N   ) * aincX * 2 );
   Y  = (TYPE   *)malloc( ATL_MulBySize( N   ) * aincY * 2 );

   if( ( X == NULL ) || ( Y == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   X0 = X + N * ( aincX SHIFT );
   Y0 = Y + N * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;
   Yseed = N  * aincY;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, X0, aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1,  N, Y0, aincY, Yseed );
/*
 * Compute the norm of Y for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( normX == ATL_rzero ) normX = ATL_rone;
      normY = Mjoin( PATL, infnrm )( N, Y, aincY );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_swap( N, x, INCX, y, INCY );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret = ATL_flushcache( -1 );
   t0    = time00();
   test_swap( N, x, INCX, y, INCY );
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   XD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( Y ); free( X ); return( ATL_rnone ); }
   YD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( YD == NULL ) { free( XD ); free( Y ); free( X ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );
   Mjoin( PATL, vdiff )( N, Y, aincY, Y0, aincY, YD, 1 );

   normDX = Mjoin( PATL, infnrm )( N, XD, 1 );
   normDY = Mjoin( PATL, infnrm )( N, YD, 1 );
   resid  = Mmax( normDX, normDY ) / ( EPSILON * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
                      "ERROR:  resid=%f, normDX=%f, normDY=%f, eps=%e\n",
                      resid, normDX, normDY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
      Mjoin( PATL, geprint )( "Y_trusted", 1, N, Y0, aincY );
      Mjoin( PATL, geprint )( "Y_test",    1, N, Y,  aincY );
#endif
   }

   free( X  );
   free( Y  );
   free( XD );
   free( YD );

   return( resid );
}


TYPE dottst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest=0.0, ttrust=0.0;
   TYPE                       normX, normY, resid = ATL_rzero;
#ifdef TREAL
   TYPE                       te_dot=ATL_rzero, tr_dot= ATL_rzero;
#ifdef SREAL
   double                     te_ddot=0.0, tr_ddot=0.0;
   float                      b = ATL_rone;
#endif
#else
   TYPE                       te_dotx[2], tr_dotx[2];
#endif
   TYPE                       * X = NULL, * Y = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE   *)malloc( ATL_MulBySize( N ) * aincX );
   Y  = (TYPE   *)malloc( ATL_MulBySize( N ) * aincY );

   if( ( X == NULL ) || ( Y == NULL ) )
   {
      l2ret = ATL_flushcache( 0 );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;
   Yseed = N  * aincY;

   Mjoin( PATL, gegen )( 1, N, X, aincX, Xseed );
   Mjoin( PATL, gegen )( 1, N, Y, aincY, Yseed );
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

#ifdef TREAL
   if( ( ROUT == DOTU ) || ( ROUT == DOTC ) )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      tr_dot = trusted_dot( N, x, INCX, y, INCY );
      ttrust = time00() - t0;
   }
#ifdef SREAL
   else if( ROUT == DSDOT )
   {
      l2ret   = ATL_flushcache( -1 );
      t0      = time00();
      tr_ddot = trusted_dsdot( N, x, INCX, y, INCY );
      ttrust  = time00() - t0;
   }
   else if( ROUT == SDSDOT )
   {
      Mjoin( PATL, gegen )( 1, 1, &b, 1, 297 );
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      tr_dot = trusted_sdsdot( N, b, x, INCX, y, INCY );
      ttrust = time00() - t0;
   }
#endif
#else
   if( ROUT == DOTU )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_dotu( N, x, INCX, y, INCY, tr_dotx );
      ttrust = time00() - t0;
   }
   else if( ROUT == DOTC )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      trusted_dotc( N, x, INCX, y, INCY, tr_dotx );
      ttrust = time00() - t0;
   }
#endif
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

#ifdef TREAL
   if( ( ROUT == DOTU ) || ( ROUT == DOTC ) )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      te_dot = test_dot( N, x, INCX, y, INCY );
      ttest  = time00() - t0;
   }
#ifdef SREAL
   else if( ROUT == DSDOT )
   {
      l2ret   = ATL_flushcache( -1 );
      t0      = time00();
      te_ddot = test_dsdot( N, x, INCX, y, INCY );
      ttest   = time00() - t0;
   }
   else if( ROUT == SDSDOT )
   {
      l2ret  = ATL_flushcache( -1 );
      t0     = time00();
      te_dot = test_sdsdot( N, b, x, INCX, y, INCY );
      ttest  = time00() - t0;
   }
#endif
#else
   if( ROUT == DOTU )
   {
      l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_dotu( N, x, INCX, y, INCY, te_dotx );
      ttest = time00() - t0;
   }
   else if( ROUT == DOTC )
   {
      l2ret = ATL_flushcache( -1 );
      t0    = time00();
      test_dotc( N, x, INCX, y, INCY, te_dotx );
      ttest = time00() - t0;
   }
#endif
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   normX =  Mjoin( PATL, infnrm )( N, X, aincX );
   if( normX == ATL_rzero ) normX = ATL_rone;
   free( X );

   normY =  Mjoin( PATL, infnrm )( N, Y, aincY );
   if( normY == ATL_rzero ) normY = ATL_rone;
   free( Y );

#if   defined( SREAL )
   if( ( ROUT == DOTU ) || ( ROUT == DOTC ) || ( ROUT == SDSDOT ) )
   {
      resid = tr_dot - te_dot;
      resid = Mabs( resid ) / ( EPSILON * Mmax( normX, ATL_rone ) *
                                Mmax( normY, ATL_rone ) * N );

      if( ( resid > THRESH ) || ( resid != resid ) )
      {
         (void) fprintf( stderr,
         "ERROR:  resid=%f, tr_dot=%f, te_dot=%f, normX=%f, normY=%f, eps=%e\n",
         resid, tr_dot, te_dot, normX, normY, EPSILON );
      }
   }
   else if( ROUT == DSDOT  )
   {
      resid = (TYPE)( tr_ddot - te_ddot );
      resid = Mabs( resid ) / ( EPSILON * Mmax( normX, ATL_rone ) *
                                Mmax( normY, ATL_rone ) * N );

      if( ( resid > THRESH ) || ( resid != resid ) )
      {
         (void) fprintf( stderr,
      "ERROR:  resid=%f, tr_ddot=%f, te_ddot=%f, normX=%f, normY=%f, eps=%e\n",
      resid, tr_ddot, te_ddot, normX, normY, EPSILON );
      }
   }
#elif defined( DREAL )
   resid = tr_dot - te_dot;
   resid = Mabs( resid ) / ( EPSILON * Mmax( normX, ATL_rone ) *
                             Mmax( normY, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, tr_dot=%f, te_dot=%f, normX=%f, normY=%f, eps=%e\n",
      resid, tr_dot, te_dot, normX, normY, EPSILON );
   }
#else
   resid = Mabs( te_dotx[0] - tr_dotx[0] ) + Mabs( te_dotx[1] - tr_dotx[1] );
   resid = Mabs( resid ) / ( EPSILON * Mmax( normX, ATL_rone ) *
                             Mmax( normY, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
   "ERROR:  resid=%f, tr_dot=%f,%f, te_dot=%f,%f, normX=%f, normY=%f, eps=%e\n",
   resid, tr_dotx[0], tr_dotx[1], te_dotx[0], te_dotx[1], normX, normY,
   EPSILON );
   }
#endif

   return( resid );
}

TYPE rottst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 CO,
   const TYPE                 SI,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normDX, normDY, normX, normY, resid;
   TYPE                       * X = NULL, * X0, * XD = NULL, * Y = NULL,
                              * Y0, * YD = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   if( CO == ATL_rone ) ops = 0.0; else ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE *)malloc( ATL_MulBySize( N ) * aincX * 2 );
   Y  = (TYPE *)malloc( ATL_MulBySize( N ) * aincY * 2 );

   if( ( X == NULL ) || ( Y == NULL ) )
   {
      l2ret  = ATL_flushcache( 0 );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   X0 = X + N * ( aincX SHIFT );
   Y0 = Y + N * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;
   Yseed = N  * aincY;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, X0, aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1,  N, Y0, aincY, Yseed );
/*
 * Compute the norm of X and Y for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( normX == ATL_rzero ) normX = ATL_rone;
      normY = Mjoin( PATL, infnrm )( N, Y, aincY );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
   else { normX = ATL_rone; normY = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_rot( N, x, INCX, y, INCY, CO, SI );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret = ATL_flushcache( -1 );
   t0    = time00();
   test_rot( N, x, INCX, y, INCY, CO, SI );
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   XD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( X ); free( Y ); return( ATL_rnone ); }

   YD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( YD == NULL ) { free( XD ); free( X ); free( Y ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );
   Mjoin( PATL, vdiff )( N, Y, aincY, Y0, aincY, YD, 1 );

   normDX = Mjoin( PATL, infnrm )( N, XD, 1 );
   normDY = Mjoin( PATL, infnrm )( N, YD, 1 );

   resid = Mmax( normDX, normDY ) / ( EPSILON * Mmax( normX, ATL_rone ) *
                                      Mmax( normY, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normDX=%f, normDY=%f, eps=%e\n",
      resid, normDX, normDY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "Y_trusted", 1, N, Y0, aincY );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
      Mjoin( PATL, geprint )( "Y_test",    1, N, Y,  aincY );
#endif
   }

   free( YD );
   free( XD );
   free( Y  );
   free( X );

   return( resid );
}

#ifdef TREAL
TYPE rotmtst
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 * PARAM,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     l2ret, ops, t0, ttest, ttrust;
   TYPE                       normDX, normDY, normX, normY, resid;
   TYPE                       * X = NULL, * X0, * XD = NULL, * Y = NULL,
                              * Y0, * YD = NULL, * x, * y;
   const int                  aincX = Mabs( INCX ), aincY = Mabs( INCY );
   int                        Xseed, Yseed;

   *TTRUST0 = *TTEST0 = *MFTEST0 = *MFTRUST0 = 0.0;
   if( N == 0 ) { return( ATL_rzero ); }

   ops = opbl1( ROUT, N );
/*
 * Allocate L2 cache space, X, Y and Y0
 */
   l2ret = ATL_flushcache( CACHESIZE );
   X  = (TYPE *)malloc( ATL_MulBySize( N ) * aincX * 2 );
   Y  = (TYPE *)malloc( ATL_MulBySize( N ) * aincY * 2 );

   if( ( X == NULL ) || ( Y == NULL ) )
   {
      l2ret  = ATL_flushcache( 0 );
      if( X ) free( X );
      if( Y ) free( Y );
      return( ATL_rnone );
   }

   X0 = X + N * ( aincX SHIFT );
   Y0 = Y + N * ( aincY SHIFT );
/*
 * Generate random operands
 */
   Xseed = N  * aincX * 27 + 213;
   Yseed = N  * aincY;

   Mjoin( PATL, gegen )( 1,  N, X,  aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, X0, aincX, Xseed );
   Mjoin( PATL, gegen )( 1,  N, Y,  aincY, Yseed );
   Mjoin( PATL, gegen )( 1,  N, Y0, aincY, Yseed );
/*
 * Compute the norm of X and Y for later use in testing
 */
   if( TEST )
   {
      normX = Mjoin( PATL, infnrm )( N, X, aincX );
      if( normX == ATL_rzero ) normX = ATL_rone;
      normY = Mjoin( PATL, infnrm )( N, Y, aincY );
      if( normY == ATL_rzero ) normY = ATL_rone;
   }
   else { normX = ATL_rone; normY = ATL_rone; }
/*
 * Start cold cache timing operations for the trusted routine
 */
   x = X0; if( INCX < 0 ) x = X0 + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y0; if( INCY < 0 ) y = Y0 + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret  = ATL_flushcache( -1 );
   t0     = time00();
   trusted_rotm( N, x, INCX, y, INCY, PARAM );
   ttrust = time00() - t0;
   if( ttrust > 0.0 )
   { *TTRUST0 = ttrust; *MFTRUST0 = ops / ( ttrust * MEGA ); }
/*
 * Start cold cache timing operations for the tested routine
 */
   x = X; if( INCX < 0 ) x = X + ( ( ( 1 - N ) * INCX ) SHIFT );
   y = Y; if( INCY < 0 ) y = Y + ( ( ( 1 - N ) * INCY ) SHIFT );

   l2ret = ATL_flushcache( -1 );
   t0    = time00();
   test_rotm( N, x, INCX, y, INCY, PARAM );
   ttest = time00() - t0;
   if( ttest  > 0.0 )
   { *TTEST0  = ttest;  *MFTEST0  = ops / ( ttest  * MEGA ); }
/*
 * if timing only, I am done ... so leave.
 */
   l2ret  = ATL_flushcache( 0 );

   if( !( TEST ) ) { free( X ); free( Y ); return( ATL_rzero ); }
/*
 * else perform error check - Ensure the difference of the output operands
 * is relatively tiny enough
 */
   XD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( XD == NULL ) { free( X ); free( Y ); return( ATL_rnone ); }

   YD = (TYPE *)malloc( ATL_MulBySize( N ) );
   if( YD == NULL ) { free( XD ); free( X ); free( Y ); return( ATL_rnone ); }

   Mjoin( PATL, vdiff )( N, X, aincX, X0, aincX, XD, 1 );
   Mjoin( PATL, vdiff )( N, Y, aincY, Y0, aincY, YD, 1 );

   normDX = Mjoin( PATL, infnrm )( N, XD, 1 );
   normDY = Mjoin( PATL, infnrm )( N, YD, 1 );

   resid = Mmax( normDX, normDY ) / ( EPSILON * Mmax( normX, ATL_rone ) *
                                      Mmax( normY, ATL_rone ) * N );

   if( ( resid > THRESH ) || ( resid != resid ) )
   {
      (void) fprintf( stderr,
      "ERROR:  resid=%f, normDX=%f, normDY=%f, eps=%e\n",
      resid, normDX, normDY, EPSILON );
#ifdef ATLAS_DEBUG
      Mjoin( PATL, geprint )( "X_trusted", 1, N, X0, aincX );
      Mjoin( PATL, geprint )( "Y_trusted", 1, N, Y0, aincY );
      Mjoin( PATL, geprint )( "X_test",    1, N, X,  aincX );
      Mjoin( PATL, geprint )( "Y_test",    1, N, Y,  aincY );
#endif
   }

   free( YD );
   free( XD );
   free( Y  );
   free( X );

   return( resid );
}
#endif

/*
 * =====================================================================
 * case functions
 * =====================================================================
 */
int rotgcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   TYPE                       resid = ATL_rzero;
   int                        passed;

   resid = rotgtst( CACHESIZE, ROUT, TEST, EPSILON, TTRUST0, TTEST0,
		    MFTRUST0, MFTEST0 );
   if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );

   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   return( passed );
}

#ifdef TREAL
int rotmgcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   TYPE                       resid = ATL_rzero;
   int                        passed;

   resid = rotmgtst( CACHESIZE, ROUT, TEST, EPSILON, TTRUST0, TTEST0,
		     MFTRUST0, MFTEST0 );
   if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );

   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   return( passed );
}
#endif

int sumcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       * stX, * x, * X, * X0 = NULL;
   TYPE                       resid = ATL_rzero, te_sum, tr_sum;
   unsigned long              ir, reps;
   int                        aincX, incx, lX, passed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = sumtst( CACHESIZE, ROUT, TEST, N, INCX, EPSILON, TTRUST0,
		      TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( X0 == NULL ) { if( X0 ) free( X0 ); return( -1 ); }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      tr_sum = trusted_asum( N, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      te_sum = test_asum( N, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );

   return( passed );
}

int nrmcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero, te_nrm, tr_nrm;
   TYPE                       * stX, * x, * X, * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, incx, lX, passed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = nrmtst( CACHESIZE, ROUT, TEST, N, INCX, EPSILON, TTRUST0,
		      TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( X0 == NULL ) { if( X0 ) free( X0 ); return( -1 ); }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      tr_nrm = trusted_nrm2( N, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      te_nrm = test_nrm2( N, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );

   return( passed );
}

int maxcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
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
   TYPE                       * stX, * x, * X, * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, incx, lX, passed, te_max, tr_max, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = maxtst( CACHESIZE, ROUT, TEST, N, INCX, EPSILON, TTRUST0,
		      TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( X0 == NULL ) { if( X0 ) free( X0 ); return( -1 ); }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      tr_max = trusted_amax( N, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      te_max = test_amax( N, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );

   return( passed );
}

int sclcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const SCALAR               ALPHA,
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
   TYPE                       * stX, * x, * X, * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, incx, lX, passed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = scltst( CACHESIZE, ROUT, TEST, N, ALPHA, INCX, EPSILON,
		      TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( ( MEGA * MFLOP <= flops ) || ( SCALAR_IS_ONE( ALPHA ) ) )
      return( passed );

   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( X0 == NULL ) { if( X0 ) free( X0 ); return( -1 ); }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_scal( N, ALPHA, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_scal( N, ALPHA, x, INCX );
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );

   return( passed );
}

#ifdef TCPLX
int rsccase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero, Calph[2];
   TYPE                       * stX, * x, * X, * X0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, incx, lX, passed, Xseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = rsctst( CACHESIZE, ROUT, TEST, N, ALPHA, INCX, EPSILON,
		      TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( ( MEGA * MFLOP <= flops ) || ( *ALPHA == ATL_rone ) ) return( passed );

   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( X0 == NULL ) { if( X0 ) free( X0 ); return( -1 ); }

   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;

   Calph[0] = *ALPHA; Calph[1] = ATL_rzero;
/*
 * Generate the random data and time the trusted routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
#if   defined( USE_F77_BLAS ) || defined( USE_L1_REFERENCE )
      trusted_rscal( N, Calph[0], x, INCX );
#else
      trusted_rscal( N, Calph,    x, INCX );
#endif
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   x = X;

   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
#if   defined( USE_L1_PTHREADS )
      test_rscal( N, Calph[0], x, INCX );
#else
      test_rscal( N, Calph,    x, INCX );
#endif
      x += incx; if( x == stX ) { x = X; }
      if( Calph[0] != ATL_rzero ) Calph[0] = ATL_rone / Calph[0];
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( X0 );

   return( passed );
}
#endif

int xpycase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const SCALAR               ALPHA,
   const int                  INCX,
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
   TYPE                       * stX, * stY, * x, * y, * X, * X0 = NULL,
                              * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, incx, incy, lX, lY, passed,
                              Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = xpytst( CACHESIZE, ROUT, TEST, N, ALPHA, INCX, INCY, EPSILON,
		      TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_axpy( N, ALPHA, x, INCX, y, INCY );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_axpy( N, ALPHA, x, INCX, y, INCY );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );

   return( passed );
}

int cpycase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
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
   TYPE                       * stX, * stY, * x, * y, * X, * X0 = NULL,
                              * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, incx, incy, lX, lY, passed,
                              Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = cpytst( CACHESIZE, ROUT, TEST, N, INCX, INCY, EPSILON, TTRUST0,
		      TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_copy( N, x, INCX, y, INCY );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_copy( N, x, INCX, y, INCY );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );

   return( passed );
}

int swpcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
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
   TYPE                       * stX, * stY, * x, * y, * X, * X0 = NULL,
                              * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, incx, incy, lX, lY, passed,
                              Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = swptst( CACHESIZE, ROUT, TEST, N, INCX, INCY, EPSILON, TTRUST0,
		      TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_swap( N, x, INCX, y, INCY );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_swap( N, x, INCX, y, INCY );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );

   return( passed );
}

int dotcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust=0.0, ttest=0.0, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
#ifdef TREAL
   TYPE                       dot;
#ifdef SREAL
   double                     ddot;
   float                      b = 0.5;
#endif
#else
   TYPE                       dotx[2];
#endif
   TYPE                       * stX, * stY, * x, * y, * X, * X0 = NULL,
                              * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, incx, incy, lX, lY, passed,
                              Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = dottst( CACHESIZE, ROUT, TEST, N, INCX, INCY, EPSILON, TTRUST0,
		      TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( MEGA * MFLOP <= flops ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

#ifdef TREAL
   if( ( ROUT == DOTU ) || ( ROUT == DOTC ) )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         dot = trusted_dot( N, x, INCX, y, INCY );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttrust = time00() - t0;
   }
#ifdef SREAL
   else if( ROUT == DSDOT )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         ddot = trusted_dsdot( N, x, INCX, y, INCY );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttrust = time00() - t0;
   }
   else if( ROUT == SDSDOT )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         ddot = trusted_sdsdot( N, b, x, INCX, y, INCY );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttrust = time00() - t0;
   }
#endif
#else
   if( ROUT == DOTU )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_dotu( N, x, INCX, y, INCY, dotx );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttrust = time00() - t0;
   }
   else if( ROUT == DOTC )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         trusted_dotc( N, x, INCX, y, INCY, dotx );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
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
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

#ifdef TREAL
   if( ( ROUT == DOTU ) || ( ROUT == DOTC ) )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         dot = test_dot( N, x, INCX, y, INCY );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttest = time00() - t0;
   }
#ifdef SREAL
   else if( ROUT == DSDOT )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         ddot = test_dsdot( N, x, INCX, y, INCY );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttest = time00() - t0;
   }
   else if( ROUT == SDSDOT )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         ddot = test_sdsdot( N, b, x, INCX, y, INCY );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttest = time00() - t0;
   }
#endif
#else
   if( ROUT == DOTU )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_dotu( N, x, INCX, y, INCY, dotx );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
      }
      ttest = time00() - t0;
   }
   else if( ROUT == DOTC )
   {
      t0 = time00();
      for( ir = reps; ir; ir-- )
      {
         test_dotc( N, x, INCX, y, INCY, dotx );
         y += incy; if( y == stY ) { y = Y; }
         x += incx; if( x == stX ) { x = X; }
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
   free( Y0 );
   free( X0 );

   return( passed );
}

int rotcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 CO,
   const TYPE                 SI,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
   TYPE                       * stX, * stY, * x, * y, * X, * X0 = NULL,
                              * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, incx, incy, lX, lY, passed,
                              Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = rottst( CACHESIZE, ROUT, TEST, N, INCX, INCY, CO, SI, EPSILON,
		      TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

   if( ( MEGA * MFLOP <= flops ) || ( CO == ATL_rone ) ) return( passed );

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_rot( N, x, INCX, y, INCY, CO, SI );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_rot( N, x, INCX, y, INCY, CO, SI );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );

   return( passed );
}

#ifdef TREAL
int rotmcase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N,
   const int                  INCX,
   const int                  INCY,
   const TYPE                 * PARAM,
   const TYPE                 EPSILON,
   double                     * TTRUST0,
   double                     * TTEST0,
   double                     * MFTRUST0,
   double                     * MFTEST0
)
{
   double                     flops, ttrust, ttest, mftrust, mftest, t0;
   TYPE                       resid = ATL_rzero;
   TYPE                       * stX, * stY, * x, * y, * X, * X0 = NULL,
                              * Y, * Y0 = NULL;
   unsigned long              ir, reps;
   int                        aincX, aincY, incx, incy, lX, lY, passed,
                              Xseed, Yseed;

   if( ( MEGA * MFLOP <= ( flops = opbl1( ROUT, N ) ) ) || ( TEST ) )
   {
      resid = rotmtst( CACHESIZE, ROUT, TEST, N, INCX, INCY, PARAM, EPSILON,
		       TTRUST0, TTEST0, MFTRUST0, MFTEST0 );
      if( resid > THRESH ) (void) fprintf( stderr, "   resid=%f\n", resid );
   }
   if( resid < ATL_rzero ) passed = -1;
   else                    passed = ( resid < THRESH );

#ifdef SREAL
   if( ( MEGA * MFLOP <= flops ) || ( PARAM[0] == -2.0f ) ) return( passed );
#else
   if( ( MEGA * MFLOP <= flops ) || ( PARAM[0] == -2.0  ) ) return( passed );
#endif

   incy = INCY * ( N  SHIFT ); aincY = Mabs( INCY );
   incx = INCX * ( N  SHIFT ), aincX = Mabs( INCX );

   lY = N * aincY * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );
   lX = N * aincX * ( ( ATL_DivBySize( LCSIZE ) + N - 1 ) / N );

   Y0 = (TYPE *)malloc( ATL_MulBySize( lY ) );
   X0 = (TYPE *)malloc( ATL_MulBySize( lX ) );

   if( ( Y0 == NULL ) || ( X0 == NULL ) )
   {
      if( Y0 ) free( Y0 );
      if( X0 ) free( X0 );
      return( -1 );
   }

   if( INCY < 1 ) { Y = Y0 + ( lY SHIFT ); stY = Y0; }
   else           { Y = Y0; stY = Y0 + ( lY SHIFT ); }
   if( INCX < 1 ) { X = X0 + ( lX SHIFT ); stX = X0; }
   else           { X = X0; stX = X0 + ( lX SHIFT ); }

   Yseed = N * aincY;
   Xseed = N * aincX + 127 * 50 + 77;

   reps  = ( MEGA * MFLOP ) / flops;
/*
 * Generate the random data and time the trusted routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      trusted_rotm( N, x, INCX, y, INCY, PARAM );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttrust = time00() - t0;
   if( ttrust > 0.0 ) mftrust = ( reps * flops ) / ( MEGA * ttrust );
   else               mftrust = 0.0;
   ttrust /= reps; *TTRUST0 = ttrust; *MFTRUST0 = mftrust;
/*
 * Generate the random data and time the tested routine
 */
   y = Y; x = X;

   Mjoin( PATL, gegen )( lY, 1, Y0, lY, Yseed );
   Mjoin( PATL, gegen )( lX, 1, X0, lX, Xseed );

   t0 = time00();
   for( ir = reps; ir; ir-- )
   {
      test_rotm( N, x, INCX, y, INCY, PARAM );
      y += incy; if( y == stY ) { y = Y; }
      x += incx; if( x == stX ) { x = X; }
   }
   ttest = time00() - t0;
   if( ttest  > 0.0 ) mftest  = ( reps * flops ) / ( MEGA * ttest  );
   else               mftest  = 0.0;
   ttest  /= reps; *TTEST0  = ttest;  *MFTEST0  = mftest;
/*
 * release the memory and exit
 */
   free( Y0 );
   free( X0 );

   return( passed );
}
#endif

/*
 * =====================================================================
 * Run functions
 * =====================================================================
 */
void RunrotgCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        ires;

   (void) fprintf( stdout, "\n%s\n",
                   "------------ ROTG -----------" );
   (void) fprintf( stdout, "%s",
                   "TST#   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ====== ===== ===== =====\n" );
   form = "%4d %6.2f %5.1f %5.2f %5s\n";

   ires = rotgcase( CACHESIZE, ROUT, TEST, MFLOP, EPSILON, &ttrust, &ttest,
		    &mftrust, &mftest );

   if(     !( TEST ) ) pass = "SKIP ";
   else if( ires < 0 ) pass = "NoMEM";
   else if( ires     ) pass = "PASS ";
   else                pass = "FAIL ";

   if( ires > 0 ) (*NPASSED)++;

   if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
   else                                        t0 = 0.0;

   (void) fprintf( stdout, form, *NTESTS, ttrust, mftrust, 1.0, "-----" );
   (void) fprintf( stdout, form, *NTESTS, ttest,  mftest,  t0,  pass    );
   (*NTESTS)++;
}

#ifdef TREAL
void RunrotmgCase
(
   const int CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   char                       * pass, * form;
   int                        ires;

   (void) fprintf( stdout, "\n%s\n",
                   "----------- ROTMG -----------" );
   (void) fprintf( stdout, "%s",
                   "TST#   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ====== ===== ===== =====\n" );
   form = "%4d %6.2f %5.1f %5.2f %5s\n";

   ires = rotmgcase( CACHESIZE, ROUT, TEST, MFLOP, EPSILON, &ttrust,
		     &ttest, &mftrust, &mftest );

   if(     !( TEST ) ) pass = "SKIP ";
   else if( ires < 0 ) pass = "NoMEM";
   else if( ires     ) pass = "PASS ";
   else                pass = "FAIL ";

   if( ires > 0 ) (*NPASSED)++;

   if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
   else                                        t0 = 0.0;

   (void) fprintf( stdout, form, *NTESTS, ttrust, mftrust, 1.0, "-----" );
   (void) fprintf( stdout, form, *NTESTS, ttest,  mftest,  t0,  pass    );
   (*NTESTS)++;
}
#endif

void RunsumCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
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
   int                        ires, ix, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "---------------- ASUM -----------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( ix = 0; ix < NINCX; ix++ )
      {
         ires = sumcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			 EPSILON, &ttrust, &ttest, &mftrust, &mftest );

         if(     !( TEST ) ) pass = "SKIP ";
         else if( ires < 0 ) pass = "NoMEM";
         else if( ires     ) pass = "PASS ";
         else                pass = "FAIL ";

         if( ires > 0 ) (*NPASSED)++;

         if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
         else                                        t0 = 0.0;

         (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], ttrust, mftrust,
                         1.0, "-----" );
         (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], ttest,  mftest,
                         t0,  pass    );
         (*NTESTS)++;
      }
   }
}

void RunnrmCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
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
   int                        ires, ix, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "---------------- NRM2 -----------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( ix = 0; ix < NINCX; ix++ )
      {
         ires = nrmcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			 EPSILON, &ttrust, &ttest, &mftrust, &mftest );

         if(     !( TEST ) ) pass = "SKIP ";
         else if( ires < 0 ) pass = "NoMEM";
         else if( ires     ) pass = "PASS ";
         else                pass = "FAIL ";

         if( ires > 0 ) (*NPASSED)++;

         if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
         else                                        t0 = 0.0;

         (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], ttrust, mftrust,
                         1.0, "-----" );
         (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], ttest,  mftest,
                         t0,  pass    );
         (*NTESTS)++;
      }
   }
}

void RunmaxCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
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
   int                        ires, ix, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "---------------- AMAX -----------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( ix = 0; ix < NINCX; ix++ )
      {
         ires = maxcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			 EPSILON, &ttrust,  &ttest, &mftrust, &mftest );

         if(     !( TEST ) ) pass = "SKIP ";
         else if( ires < 0 ) pass = "NoMEM";
         else if( ires     ) pass = "PASS ";
         else                pass = "FAIL ";

         if( ires > 0 ) (*NPASSED)++;

         if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
         else                                        t0 = 0.0;

         (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], ttrust, mftrust,
                         1.0, "-----" );
         (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], ttest,  mftest,
                         t0,  pass    );
         (*NTESTS)++;
      }
   }
}

void RunsclCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
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
   int                        al, ires, ix, n, nn;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s\n",
                   "------------------ SCAL ---------------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N ALPHA INCX   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ===== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s\n",
             "--------------------- SCAL ------------------------" );
   (void) fprintf( stdout, "%s",
             "TST#    N       ALPHA INCX   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
             "==== ==== ===== ===== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %5.1f %5.1f %4d %6.2f %5.1f %5.2f %5s\n";
#endif

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( ix = 0; ix < NINCX; ix++ )
      {
         for( al = 0; al < NALPHA; al++ )
         {
#ifdef TREAL
            ires = sclcase( CACHESIZE, ROUT, TEST, MFLOP, n, ALPHAS[al],
			    INCXS[ix], EPSILON, &ttrust, &ttest, &mftrust,
			    &mftest );
#else
            ires = sclcase( CACHESIZE, ROUT, TEST, MFLOP, n, ALPHAS+2*al,
			    INCXS[ix], EPSILON, &ttrust, &ttest, &mftrust,
			    &mftest );
#endif
            if(     !( TEST ) ) pass = "SKIP ";
            else if( ires < 0 ) pass = "NoMEM";
            else if( ires     ) pass = "PASS ";
            else                pass = "FAIL ";

            if( ires > 0 ) (*NPASSED)++;

            if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
            else                                        t0 = 0.0;
#ifdef TREAL
            (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[al], INCXS[ix],
                            ttrust, mftrust, 1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[al], INCXS[ix],
                            ttest,  mftest,  t0,  pass    );
#else
            (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[2*al],
                            ALPHAS[2*al+1], INCXS[ix], ttrust, mftrust,
                            1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[2*al],
                            ALPHAS[2*al+1], INCXS[ix], ttest,  mftest,
                            t0,  pass    );
#endif
            (*NTESTS)++;
         }
      }
   }
}

#ifdef TCPLX

void RunrscCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
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
   int                        al, ires, ix, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "------------------ RSCAL --------------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N ALPHA INCX   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ===== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %5.1f %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( ix = 0; ix < NINCX; ix++ )
      {
         for( al = 0; al < NALPHA; al++ )
         {
            ires = rsccase( CACHESIZE, ROUT, TEST, MFLOP, n, ALPHAS+2*al,
			    INCXS[ix], EPSILON, &ttrust, &ttest, &mftrust, &mftest );

            if(     !( TEST ) ) pass = "SKIP ";
            else if( ires < 0 ) pass = "NoMEM";
            else if( ires     ) pass = "PASS ";
            else                pass = "FAIL ";

            if( ires > 0 ) (*NPASSED)++;

            if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
            else                                        t0 = 0.0;

            (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[2*al], INCXS[ix],
                            ttrust, mftrust, 1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[2*al], INCXS[ix],
                            ttest,  mftest,  t0,  pass    );
            (*NTESTS)++;
         }
      }
   }
}
#endif

void RunxpyCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
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
   int                        al, ires, ix, iy, n, nn;

#ifdef TREAL
   (void) fprintf( stdout, "\n%s\n",
                   "--------------------- AXPY -----------------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N ALPHA INCX INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ===== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %5.1f %4d %4d %6.2f %5.1f %5.2f %5s\n";
#else
   (void) fprintf( stdout, "\n%s\n",
             "------------------------ AXPY --------------------------" );
   (void) fprintf( stdout, "%s",
             "TST#    N       ALPHA INCX INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
             "==== ==== ===== ===== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %5.1f %5.1f %4d %4d %6.2f %5.1f %5.2f %5s\n";
#endif

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( iy = 0; iy < NINCY; iy++ )
      {
         for( ix = 0; ix < NINCX; ix++ )
         {
            for( al = 0; al < NALPHA; al++ )
            {
#ifdef TREAL
               ires = xpycase( CACHESIZE, ROUT, TEST, MFLOP, n, ALPHAS[al],
			       INCXS[ix], INCYS[iy], EPSILON, &ttrust, &ttest,
			       &mftrust, &mftest );
#else
               ires = xpycase( CACHESIZE, ROUT, TEST, MFLOP, n, ALPHAS+2*al,
			       INCXS[ix], INCYS[iy], EPSILON, &ttrust, &ttest,
			       &mftrust, &mftest );
#endif
               if(     !( TEST ) ) pass = "SKIP ";
               else if( ires < 0 ) pass = "NoMEM";
               else if( ires     ) pass = "PASS ";
               else                pass = "FAIL ";

               if( ires > 0 ) (*NPASSED)++;

               if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                  t0 = mftest / mftrust;
               else
                  t0 = 0.0;
#ifdef TREAL
               (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[al], INCXS[ix],
                               INCYS[iy], ttrust, mftrust, 1.0, "-----" );
               (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[al], INCXS[ix],
                               INCYS[iy], ttest,  mftest,  t0,  pass    );
#else
               (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[2*al],
                               ALPHAS[2*al+1], INCXS[ix], INCYS[iy], ttrust,
                               mftrust, 1.0, "-----" );
               (void) fprintf( stdout, form, *NTESTS, n, ALPHAS[2*al],
                               ALPHAS[2*al+1], INCXS[ix], INCYS[iy], ttest,
                               mftest,  t0,  pass    );
#endif
               (*NTESTS)++;
            }
         }
      }
   }
}

void RuncpyCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
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
   int                        ires, ix, iy, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "------------------ COPY --------------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( iy = 0; iy < NINCY; iy++ )
      {
         for( ix = 0; ix < NINCX; ix++ )
         {
            ires = cpycase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			    INCYS[iy], EPSILON, &ttrust, &ttest, &mftrust, &mftest );
            if(     !( TEST ) ) pass = "SKIP ";
            else if( ires < 0 ) pass = "NoMEM";
            else if( ires     ) pass = "PASS ";
            else                pass = "FAIL ";

            if( ires > 0 ) (*NPASSED)++;

            if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
            else                                        t0 = 0.0;

            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            ttrust, mftrust, 1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            ttest,  mftest,  t0,  pass    );
            (*NTESTS)++;
         }
      }
   }
}

void RunswpCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
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
   int                        ires, ix, iy, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "------------------ SWAP --------------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( iy = 0; iy < NINCY; iy++ )
      {
         for( ix = 0; ix < NINCX; ix++ )
         {
            ires = swpcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			    INCYS[iy], EPSILON, &ttrust, &ttest, &mftrust, &mftest );
            if(     !( TEST ) ) pass = "SKIP ";
            else if( ires < 0 ) pass = "NoMEM";
            else if( ires     ) pass = "PASS ";
            else                pass = "FAIL ";

            if( ires > 0 ) (*NPASSED)++;

            if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
            else                                        t0 = 0.0;

            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            ttrust, mftrust, 1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            ttest,  mftest,  t0,  pass    );
            (*NTESTS)++;
         }
      }
   }
}

void RundotCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
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
   int                        ires, ix, iy, n, nn;

#ifdef TREAL
   if( ( ROUT == DOTU ) || ( ROUT == DOTC ) )
   {
      (void) fprintf( stdout, "\n%s\n",
                      "------------------ DOT ---------------------" );
   }
#ifdef SREAL
   else if( ROUT == DSDOT )
   {
      (void) fprintf( stdout, "\n%s\n",
                      "----------------- DSDOT --------------------" );
   }
   else if( ROUT == SDSDOT )
   {
      (void) fprintf( stdout, "\n%s\n",
                      "---------------- SDSDOT --------------------" );
   }
#endif
#else
   if( ROUT == DOTU )
   {
      (void) fprintf( stdout, "\n%s\n",
                      "------------------ DOTU --------------------" );
   }
   else if( ROUT == DOTC )
   {
      (void) fprintf( stdout, "\n%s\n",
                      "------------------ DOTC --------------------" );
   }
#endif
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX INCY   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %4d %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( iy = 0; iy < NINCY; iy++ )
      {
         for( ix = 0; ix < NINCX; ix++ )
         {
            ires = dotcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			    INCYS[iy], EPSILON, &ttrust, &ttest, &mftrust, &mftest );
            if(     !( TEST ) ) pass = "SKIP ";
            else if( ires < 0 ) pass = "NoMEM";
            else if( ires     ) pass = "PASS ";
            else                pass = "FAIL ";

            if( ires > 0 ) (*NPASSED)++;

            if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) ) t0 = mftest / mftrust;
            else                                        t0 = 0.0;

            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            ttrust, mftrust, 1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            ttest,  mftest,  t0,  pass    );
            (*NTESTS)++;
         }
      }
   }
}

void RunrotCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const TYPE                 EPSILON,
   int                        * NPASSED,
   int                        * NTESTS
)
{
   double                     t0, mftrust, mftest, ttrust, ttest;
   TYPE                       co, si;
   char                       * pass, * form;
   int                        al, ires, ix, iy, n, nn;

   (void) fprintf( stdout, "\n%s\n",
                   "----------------------- ROT --------------------------" );
   (void) fprintf( stdout, "%s",
                   "TST#    N INCX INCY    C    S   TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
                   "==== ==== ==== ==== ==== ==== ====== ===== ===== =====\n" );
   form = "%4d %4d %4d %4d %4.1f %4.1f %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( iy = 0; iy < NINCY; iy++ )
      {
         for( ix = 0; ix < NINCX; ix++ )
         {
            for( al = 0; al < NALPHA; al++ )
            {
#ifdef TREAL
               if(      ALPHAS[al] == ATL_rzero       )
               { co = ATL_rzero; }
               else if( Mabs( ALPHAS[al] ) > ATL_rone )
               { co = ATL_rone / ALPHAS[al]; }
               else
               { co = ALPHAS[al]; }
               si = (TYPE)(sqrt( (double)(ATL_rone - co * co) ));
               if( Mabs( ALPHAS[al] ) > ATL_rone ) si = -si;
#else
               if(      ALPHAS[2*al] == ATL_rzero       )
               { co = ATL_rzero; }
               else if( Mabs( ALPHAS[2*al] ) > ATL_rone )
               { co = ATL_rone / ALPHAS[2*al]; }
               else
               { co = ALPHAS[2*al]; }
               si = (TYPE)(sqrt( (double)(ATL_rone - co * co) ));
               if( Mabs( ALPHAS[2*al] ) > ATL_rone ) si = -si;
#endif
               ires = rotcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			       INCYS[iy], co, si, EPSILON, &ttrust, &ttest,
			       &mftrust, &mftest );
               if(     !( TEST ) ) pass = "SKIP ";
               else if( ires < 0 ) pass = "NoMEM";
               else if( ires     ) pass = "PASS ";
               else                pass = "FAIL ";

               if( ires > 0 ) (*NPASSED)++;

               if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
                  t0 = mftest / mftrust;
               else
                  t0 = 0.0;

               (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                               co, si, ttrust, mftrust, 1.0, "-----" );
               (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                               co, si, ttest,  mftest,  t0,  pass    );
               (*NTESTS)++;
            }
         }
      }
   }
}

#ifdef TREAL
void RunrotmCase
(
   const int                  CACHESIZE,
   const enum LVL1_ROUT       ROUT,
   const int                  TEST,
   const int                  MFLOP,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
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
   TYPE                       param[5];
   char                       * pass, * form;
   int                        ires, ix, iy, n, nn;

   (void) fprintf( stdout, "\n%s\n",
   "----------------------------- ROTM ---------------------------------" );
   (void) fprintf( stdout, "%s",
   "TST#    N INCX INCY P[0] P[1] P[2] P[3] P[4]  TIME MFLOP  SpUp  TEST\n" );
   (void) fprintf( stdout, "%s",
   "==== ==== ==== ==== ==== ==== ==== ==== ==== ====== ===== ===== =====\n" );
   form =
   "%4d %4d %4d %4d %4.1f %4.1f %4.1f %4.1f %4.1f %6.2f %5.1f %5.2f %5s\n";

   for( nn = N0; nn <= NN; nn += NINC )
   {
      n = nn;

      for( iy = 0; iy < NINCY; iy++ )
      {
         for( ix = 0; ix < NINCX; ix++ )
         {
#ifdef SREAL
            param[0] = ATL_rone; param[1] =  0.567f;
            param[2] =  0.123f;  param[3] = -0.222f; param[4] = -0.988f;
#else
            param[0] = ATL_rone; param[1] =  0.567;
            param[2] =  0.123;   param[3] = -0.222;  param[4] = -0.988;
#endif
            ires = rotmcase( CACHESIZE, ROUT, TEST, MFLOP, n, INCXS[ix],
			     INCYS[iy], param, EPSILON, &ttrust, &ttest,
			     &mftrust, &mftest );
            if(     !( TEST ) ) pass = "SKIP ";
            else if( ires < 0 ) pass = "NoMEM";
            else if( ires     ) pass = "PASS ";
            else                pass = "FAIL ";

            if( ires > 0 ) (*NPASSED)++;

            if( ( mftrust > 0.0 ) && ( mftest > 0.0 ) )
               t0 = mftest / mftrust;
            else
               t0 = 0.0;

            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            param[0], param[1], param[2], param[3], param[4],
                            ttrust, mftrust, 1.0, "-----" );
            (void) fprintf( stdout, form, *NTESTS, n, INCXS[ix], INCYS[iy],
                            param[0], param[1], param[2], param[3], param[4],
                            ttest,  mftest,  t0,  pass    );
            (*NTESTS)++;
         }
      }
   }
}
#endif

void RunCases
(
   const int                  TEST,
   const int                  CACHESIZE,
   const int                  MFLOP,
   const int                  N0,
   const int                  NN,
   const int                  NINC,
   const int                  NALPHA,
   const TYPE                 * ALPHAS,
   const int                  NINCX,
   const int                  * INCXS,
   const int                  NINCY,
   const int                  * INCYS,
   const int                  NROUT,
   const enum LVL1_ROUT       * ROUTS
)
{
   TYPE                       eps;
   int                        ro, ntests=0, np=0;

   eps = Mjoin( PATL, epsilon )();

   for( ro = 0; ro < NROUT; ro++ )
   {
      if( ROUTS[ro] == ROTG )
      {
         RunrotgCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, eps, &np, &ntests );
      }
#ifdef TREAL
      else if( ROUTS[ro] == ROTMG )
      {
         RunrotmgCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, eps, &np, &ntests );
      }
#endif
      else if( ROUTS[ro] == ASUM  )
      {
         RunsumCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == NRM2  )
      {
         RunnrmCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == AMAX  )
      {
         RunmaxCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == SCAL )
      {
         RunsclCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NALPHA,
		     ALPHAS, NINCX, INCXS, eps, &np, &ntests );
      }
#ifdef TCPLX
      else if( ROUTS[ro] == RSCAL )
      {
         RunrscCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NALPHA,
		     ALPHAS, NINCX, INCXS, eps, &np, &ntests );
      }
#endif
      else if(  ROUTS[ro] == AXPY )
      {
         RunxpyCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NALPHA,
		     ALPHAS, NINCX, INCXS, NINCY, INCYS, eps, &np, &ntests );
      }
      else if(  ROUTS[ro] == COPY )
      {
         RuncpyCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, NINCY, INCYS, eps, &np, &ntests );
      }
      else if(  ROUTS[ro] == SWAP )
      {
         RunswpCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, NINCY, INCYS, eps, &np, &ntests );
      }
      else if( ROUTS[ro] == ROT  )
      {
         RunrotCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, NINCY, INCYS, NALPHA, ALPHAS, eps, &np, &ntests );
      }
#ifdef TREAL
      else if( ROUTS[ro] == ROTM )
      {
         RunrotmCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		      INCXS, NINCY, INCYS, eps, &np, &ntests );
      }
#endif
      else if( ( ROUTS[ro] == DOTC  ) || ( ROUTS[ro] == DOTU   ) ||
               ( ROUTS[ro] == DSDOT ) || ( ROUTS[ro] == SDSDOT ) )
      {
         RundotCase( CACHESIZE, ROUTS[ro], TEST, MFLOP, N0, NN, NINC, NINCX,
		     INCXS, NINCY, INCYS, eps, &np, &ntests );
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
#if   defined( TREAL )
   (void) fprintf( stderr, "  rout must be in {rotg,rot,nrm2,amax, \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  asum,scal,axpy,copy,swap,dot,rotmg,  \n" );
   (void) fprintf( stderr, "                                        " );
#if   defined( SREAL )
   (void) fprintf( stderr, "  rotm,dot,dsdot,sdsdot}.              \n" );
#elif defined( DREAL )
   (void) fprintf( stderr, "  rotm,dot}.                           \n" );
#endif
#else
   (void) fprintf( stderr, "  rout must be in {rotg,rot,nrm2,amax, \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  asum,scal,rscal,axpy,copy,swap,dotc, \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  dotu}.                               \n" );
#endif
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Default is -R axpy.  Ex: -R swap     \n" );

   (void) fprintf( stderr, "   -R <nrout> <rout1> ... <routN>       " );
   (void) fprintf( stderr, ". same as above for more than one rou- \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  tine. Ex: -R 3 nrm2 copy rotg        \n" );

   (void) fprintf( stderr, "   -n <n>                               " );
   (void) fprintf( stderr, ". select one value for the parameter N.\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Ex: -n 100                           \n" );

   (void) fprintf( stderr, "   -N <n1> <nN> <ninc>                  " );
   (void) fprintf( stderr, ". select the values of N, from n1 to nN\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  by increment of ninc. n1 > 0.        \n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  Ex: -N 100 1000 100                  \n" );

#ifdef TREAL
   (void) fprintf( stderr, "   -a <nalphas> <a1> ... <aN>           " );
   (void) fprintf( stderr, ". select the values of  alpha.  Default\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  is -a 1 1.0. Ex: -a 3 -1.0 0.0 1.0   \n" );
#else
   (void) fprintf( stderr, "   -a <nalphas> <a1r> <a1i> ...  <aNi>  " );
   (void) fprintf( stderr, ". select the values of alpha, where a1r\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  and  a1i  are the  real and imaginary\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  parts of a1. Default is -a 1 1.0 0.0 \n" );
#endif

   (void) fprintf( stderr, "   -X <nincXs>  <incX0>  ... <incX1>    " );
   (void) fprintf( stderr, ". select  the values  of the  increment\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  INCX. Default is 1; Ex: -X 2 1 -1    \n" );

   (void) fprintf( stderr, "   -Y <nincYs>  <incY0>  ... <incYN>    " );
   (void) fprintf( stderr, ". same  as  above for the the values of\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  INCY.                                \n" );

   (void) fprintf( stderr, "   -T <0/1>                             " );
   (void) fprintf( stderr, ". disable computational check.  Default\n" );
   (void) fprintf( stderr, "                                        " );
   (void) fprintf( stderr, "  is -T 1                              \n" );

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
   enum LVL1_ROUT             ** ROUTS,
   int                        * TEST,
   int                        * CACHESIZE,
   int                        * MFLOP,
   int                        * N0,
   int                        * NN,
   int                        * NINC,
   int                        * NALPHA,
   TYPE                       ** ALPHAS,
   int                        * NINCX,
   int                        ** INCXS,
   int                        * NINCY,
   int                        ** INCYS
)
{
   int                        i = 1, j;
/*
 * Set up defaults
 */
   *NROUT      = -1;                         /* No routine to be tested */
   *TEST       = 1;                               /* Enable the testing */
   *MFLOP      = 0;                /* smallest number of flops possible */
#ifdef L2SIZE
   *CACHESIZE = L2SIZE;               /* Size of largest cache to flush */
#else
   *CACHESIZE = 4*1024*1024;
#endif
   *N0         = -1;
   *NALPHA     = -1;
   *NINCX      = *NINCY = -1;
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
         case 'F':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
            *MFLOP = atoi( ARGS[i++] );
            if( *MFLOP < 0      ) PrintUsage( ARGS[0] );
            break;
         case 'C':
            if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );
	    *CACHESIZE = 1024*atoi(ARGS[i++]);
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
#if   defined( SREAL )
               *NROUT = 14;
#elif defined( DREAL )
               *NROUT = 12;
#elif defined( TCPLX )
               *NROUT = 12;
#endif
               *ROUTS = (enum LVL1_ROUT *)malloc( (*NROUT) *
                                                  sizeof( enum LVL1_ROUT ) );
               ATL_assert( *ROUTS );

               (*ROUTS)[ 0] = AXPY;  (*ROUTS)[ 1] = COPY;  (*ROUTS)[ 2] = SWAP;
               (*ROUTS)[ 3] = ROTG;  (*ROUTS)[ 4] = ROT;   (*ROUTS)[ 5] = SCAL;
               (*ROUTS)[ 6] = NRM2;  (*ROUTS)[ 7] = ASUM;  (*ROUTS)[ 8] = AMAX;
#if   defined( SREAL )
               (*ROUTS)[ 9] = ROTMG; (*ROUTS)[10] = ROTM;  (*ROUTS)[11] = DOTC;
               (*ROUTS)[12] = DSDOT; (*ROUTS)[13] = SDSDOT;
#elif defined( DREAL )
               (*ROUTS)[ 9] = ROTMG; (*ROUTS)[10] = ROTM;  (*ROUTS)[11] = DOTC;
#elif defined( TCPLX )
               (*ROUTS)[ 9] = DOTC;  (*ROUTS)[10] = DOTU;  (*ROUTS)[11] = RSCAL;
#endif
               i++;
            }
            else
            {
               if( isdigit( *ARGS[i] ) ) { *NROUT = atoi( ARGS[i++] ); }
               else                      { *NROUT = 1;                 }
               *ROUTS = (enum LVL1_ROUT *)malloc( (*NROUT) *
                                                  sizeof( enum LVL1_ROUT ) );
               ATL_assert( *ROUTS );

               for( j = 0; j < *NROUT; j++ )
               {
                  if( ARGS[i] == NULL ) PrintUsage( ARGS[0] );

                  if(      ( strcmp( ARGS[i], "AXPY"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "axpy"   ) == 0 ) )
                     (*ROUTS)[j] = AXPY;
                  else if( ( strcmp( ARGS[i], "COPY"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "copy"   ) == 0 ) )
                     (*ROUTS)[j] = COPY;
                  else if( ( strcmp( ARGS[i], "SWAP"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "swap"   ) == 0 ) )
                     (*ROUTS)[j] = SWAP;
                  else if( ( strcmp( ARGS[i], "ROTG"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "rotg"   ) == 0 ) )
                     (*ROUTS)[j] = ROTG;
                  else if( ( strcmp( ARGS[i], "ROT"    ) == 0 ) ||
                           ( strcmp( ARGS[i], "rot"    ) == 0 ) )
                     (*ROUTS)[j] = ROT;
                  else if( ( strcmp( ARGS[i], "SCAL"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "scal"   ) == 0 ) )
                     (*ROUTS)[j] = SCAL;
                  else if( ( strcmp( ARGS[i], "NRM2"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "nrm2"   ) == 0 ) )
                     (*ROUTS)[j] = NRM2;
                  else if( ( strcmp( ARGS[i], "ASUM"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "asum"   ) == 0 ) )
                     (*ROUTS)[j] = ASUM;
                  else if( ( strcmp( ARGS[i], "AMAX"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "amax"   ) == 0 ) )
                     (*ROUTS)[j] = AMAX;
#if   defined( SREAL )
                  else if( ( strcmp( ARGS[i], "ROTMG"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "rotmg"  ) == 0 ) )
                     (*ROUTS)[j] = ROTMG;
                  else if( ( strcmp( ARGS[i], "ROTM"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "rotm"   ) == 0 ) )
                     (*ROUTS)[j] = ROTM;
                  else if( ( strcmp( ARGS[i], "DOT"    ) == 0 ) ||
                           ( strcmp( ARGS[i], "dot"    ) == 0 ) )
                     (*ROUTS)[j] = DOTC;
                  else if( ( strcmp( ARGS[i], "DSDOT"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "dsdot"  ) == 0 ) )
                     (*ROUTS)[j] = DSDOT;
                  else if( ( strcmp( ARGS[i], "SDSDOT" ) == 0 ) ||
                           ( strcmp( ARGS[i], "sdsdot" ) == 0 ) )
                     (*ROUTS)[j] = SDSDOT;
#elif defined( DREAL )
                  else if( ( strcmp( ARGS[i], "ROTMG"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "rotmg"  ) == 0 ) )
                     (*ROUTS)[j] = ROTMG;
                  else if( ( strcmp( ARGS[i], "ROTM"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "rotm"   ) == 0 ) )
                     (*ROUTS)[j] = ROTM;
                  else if( ( strcmp( ARGS[i], "DOT"    ) == 0 ) ||
                           ( strcmp( ARGS[i], "dot"    ) == 0 ) )
                     (*ROUTS)[j] = DOTC;
#elif defined( TCPLX )
                  else if( ( strcmp( ARGS[i], "DOTC"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "dotc"   ) == 0 ) )
                     (*ROUTS)[j] = DOTC;
                  else if( ( strcmp( ARGS[i], "DOTU"   ) == 0 ) ||
                           ( strcmp( ARGS[i], "dotu"   ) == 0 ) )
                     (*ROUTS)[j] = DOTU;
                  else if( ( strcmp( ARGS[i], "RSCAL"  ) == 0 ) ||
                           ( strcmp( ARGS[i], "rscal"  ) == 0 ) )
                     (*ROUTS)[j] = RSCAL;
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
      *ROUTS = (enum LVL1_ROUT *)malloc( sizeof( enum LVL1_ROUT ) );
      ATL_assert( *ROUTS );
      (*ROUTS)[0] = AXPY;
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
   int                        mflopmin, ninc, nstart, nstop, nalphas,
                              cachesize, nincx, nincy, nrout, test;
   int                        * incxs  = NULL, * incys = NULL;
   TYPE                       * alphas = NULL;
   enum LVL1_ROUT             * routs  = NULL;

   GetFlags( NARGS, ARGS, &nrout, &routs, &test, &cachesize, &mflopmin,
             &nstart, &nstop, &ninc, &nalphas, &alphas, &nincx, &incxs,
             &nincy, &incys );

   RunCases( test, cachesize, mflopmin, nstart, nstop, ninc, nalphas,
             alphas, nincx, incxs, nincy, incys, nrout, routs );

   if( alphas ) free( alphas );
   if( incxs  ) free( incxs  );
   if( incys  ) free( incys  );
   if( routs  ) free( routs  );

   return( 0 );
}
