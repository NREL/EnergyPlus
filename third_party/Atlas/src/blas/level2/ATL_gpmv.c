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
/*
 * Include files
 */
#include "atlas_misc.h"
#include "atlas_level1.h"
#include "atlas_kernel2.h"
#include "atlas_lvl2.h"
#include "atlas_mv.h"
#include "atlas_r1.h"

#include "atlas_reflvl2.h"          /* temporary for building purposes */
#include "atlas_reflevel2.h"        /* used for gbmv, gpmv and gpr.    */

void Mjoin( PATL, gpmv )
(
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
/*
 * .. Local Variables ..
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) ||
       ( ( SCALAR_IS_ZERO( ALPHA ) ) && ( SCALAR_IS_ONE( BETA ) ) ) ) return;

   if( SCALAR_IS_ZERO( ALPHA ) )
   {
      if( !( SCALAR_IS_ONE( BETA ) ) ) Mjoin( PATL, scal )( M, BETA, Y, INCY );
      return;
   }

   Mjoin( PATL, refgpmv )( UPLO, TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA,
                           Y, INCY );
/*
 * End of Mjoin( PATL, gpmv )
 */
}

void Mjoin( PATL, gpmvLN_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasNoTrans, M, N, one, A, LDA,
                        X, 1, zero, Y, 1 );
}

void Mjoin( PATL, gpmvLT_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasTrans, M, N, one, A, LDA, X, 1,
                        zero, Y, 1 );
}

#ifdef TCPLX
void Mjoin( PATL, gpmvLC_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        zero, Y, 1 );
}

void Mjoin( PATL, gpmvLNc_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasConj, M, N, one, A, LDA, X, 1,
                        zero, Y, 1 );
}

#endif

#ifdef TCPLX
void Mjoin( PATL, gpmvLN_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasLower, AtlasNoTrans, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

void Mjoin( PATL, gpmvLT_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasLower, AtlasTrans, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

void Mjoin( PATL, gpmvLC_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one [2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasLower, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

void Mjoin( PATL, gpmvLNc_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one [2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasLower, AtlasConj, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

#endif

void Mjoin( PATL, gpmvLN_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasNoTrans, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

void Mjoin( PATL, gpmvLT_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasTrans, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

#ifdef TCPLX
void Mjoin( PATL, gpmvLC_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

void Mjoin( PATL, gpmvLNc_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasConj, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

#endif

void Mjoin( PATL, gpmvLN_a1_x1_bX_y1  )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasNoTrans, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

void Mjoin( PATL, gpmvLT_a1_x1_bX_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasLower, AtlasTrans, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

#ifdef TCPLX

void Mjoin( PATL, gpmvLC_a1_x1_bX_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };

   Mjoin( PATL, gpmv )( AtlasLower, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

void Mjoin( PATL, gpmvLNc_a1_x1_bX_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };

   Mjoin( PATL, gpmv )( AtlasLower, AtlasConj, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

#endif

void Mjoin( PATL, gpmvUN_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasNoTrans, M, N, one, A, LDA,
                        X, 1, zero, Y, 1 );
}

void Mjoin( PATL, gpmvUT_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasTrans, M, N, one, A, LDA, X, 1,
                        zero, Y, 1 );
}

#ifdef TCPLX
void Mjoin( PATL, gpmvUC_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        zero, Y, 1 );
}

void Mjoin( PATL, gpmvUNc_a1_x1_b0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#define    zero               ATL_rzero
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConj, M, N, one, A, LDA, X, 1,
                        zero, Y, 1 );
}

#endif

#ifdef TCPLX
void Mjoin( PATL, gpmvUN_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasNoTrans, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

void Mjoin( PATL, gpmvUT_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasTrans, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

void Mjoin( PATL, gpmvUC_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one [2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

void Mjoin( PATL, gpmvUNc_a1_x1_bXi0_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one [2] = { ATL_rone, ATL_rzero };
   TYPE                       beta[2];

   beta[0] = *BETA; beta[1] = ATL_rzero;
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConj, M, N, one, A, LDA, X, 1,
                        beta, Y, 1 );
}

#endif

void Mjoin( PATL, gpmvUN_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasNoTrans, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

void Mjoin( PATL, gpmvUT_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasTrans, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

#ifdef TCPLX
void Mjoin( PATL, gpmvUC_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

void Mjoin( PATL, gpmvUNc_a1_x1_b1_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConj, M, N, one, A, LDA, X, 1,
                        one, Y, 1 );
}

#endif

void Mjoin( PATL, gpmvUN_a1_x1_bX_y1  )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasNoTrans, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

void Mjoin( PATL, gpmvUT_a1_x1_bX_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
#ifdef TREAL
#define    one                ATL_rone
#else
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };
#endif
   Mjoin( PATL, gpmv )( AtlasUpper, AtlasTrans, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

#ifdef TCPLX

void Mjoin( PATL, gpmvUC_a1_x1_bX_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };

   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConjTrans, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

void Mjoin( PATL, gpmvUNc_a1_x1_bX_y1 )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * X,
   const int                  INCX,
   const SCALAR               BETA,
   TYPE                       * Y,
   const int                  INCY
)
{
   const TYPE                 one[2] = { ATL_rone, ATL_rzero };

   Mjoin( PATL, gpmv )( AtlasUpper, AtlasConj, M, N, one, A, LDA, X, 1,
                        BETA, Y, 1 );
}

#endif
