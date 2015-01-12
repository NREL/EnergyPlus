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

void Mjoin( PATL, hbmv )
(
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const int                  K,
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
 * Purpose
 * =======
 *
 * Mjoin( PATL, hbmv ) performs the matrix-vector operation
 *
 *    y := alpha * A * x + beta * y,
 *
 * where alpha and beta are scalars, x and y are n-element vectors and A
 * is an n by n Hermitian band matrix, with k super-diagonals.
 *
 * This is a blocked version of the algorithm.  For a more detailed des-
 * cription of  the arguments of this function, see the reference imple-
 * mentation in the ATLAS/src/blas/reference directory.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   void                       (*gbmv0)( const int, const int, const int,
                              const int, const SCALAR, const TYPE *, const int,
                              const TYPE *, const int, const SCALAR, TYPE *,
                              const int );
   void                       (*gbmv1)( const int, const int, const int,
                              const int, const SCALAR, const TYPE *, const int,
                              const TYPE *, const int, const SCALAR, TYPE *,
                              const int );
   void                       (*gbmvN)( const int, const int, const int,
                              const int, const SCALAR, const TYPE *, const int,
                              const TYPE *, const int, const SCALAR, TYPE *,
                              const int );
#ifdef TREAL
   TYPE                       alphaY, beta0;
#define lda2                  LDA
#define one                   ATL_rone
#define zero                  ATL_rzero
   void                       * vx = NULL, * vy = NULL;
   TYPE                       * x, * y, * y00;
#else
   const TYPE                 one [2] = { ATL_rone,  ATL_rzero },
                              zero[2] = { ATL_rzero, ATL_rzero };
   const TYPE                 * alphaY, * beta0;
   void                       * vx = NULL, * vy = NULL;
   TYPE                       * x, * y, * y00;
   const int                  lda2 = ( LDA SHIFT );
#endif
   int                        ian, ia, j, ja, jan, k, kb, kl, ku, ma, mb,
                              mb1, n, na, nb;
/* ..
 * .. Executable Statements ..
 *
 */
   if( N == 0 ) return;

   if( SCALAR_IS_ZERO( ALPHA ) )
   {
      if( !( SCALAR_IS_ONE( BETA ) ) ) Mjoin( PATL, scal )( N, BETA, Y, INCY );
      return;
   }
   Mjoin(PATL,refhbmv)(UPLO, N, K, ALPHA, A, LDA, X, INCX, BETA, Y, INCY);

/*
 * End of Mjoin( PATL, hbmv )
 */
}
