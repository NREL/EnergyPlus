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

void Mjoin( PATL, tpsv )
(
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  N,
   const TYPE                 * A,
   TYPE                       * X,
   const int                  INCX
)
{
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, tpsv ) solves one of the systems of equations
 *
 *    A * x = b,   or   conjg( A  ) * x = b,   or
 *
 *    A'* x = b,   or   conjg( A' ) * x = b,
 *
 * where b and x are n-element vectors and  A is an n by n unit, or non-
 * unit, upper or lower triangular matrix, supplied in packed form.
 *
 * No test for  singularity  or  near-singularity  is included  in  this
 * routine. Such tests must be performed before calling this routine.
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
   void                       * vx = NULL;
   TYPE                       * x;
/* ..
 * .. Executable Statements ..
 *
 */
   if( N == 0 ) return;
   Mjoin(PATL,reftpsv)(UPLO, TRANS, DIAG, N, A, X, INCX);
   return;

   if( INCX == 1 ) { x = X; }
   else
   {
      vx = (TYPE *)malloc( ATL_Cachelen + ATL_MulBySize( N ) );
      ATL_assert( vx ); x = ATL_AlignPtr( vx );
      Mjoin( PATL, copy )( N, X, INCX, x, 1 );
   }

#ifdef TREAL
   if( ( TRANS == AtlasNoTrans ) || ( TRANS == AtlasConj ) )
#else
   if( TRANS == AtlasNoTrans )
#endif
   {
      if( UPLO == AtlasUpper ) Mjoin( PATL, tpsvUN )( DIAG, N,    A,   1, x );
      else                     Mjoin( PATL, tpsvLN )( DIAG, N,    A,   N, x );
   }
#ifdef TCPLX
   else if( TRANS == AtlasConj )
   {
      if( UPLO == AtlasUpper ) Mjoin( PATL, tpsvUC )( DIAG, N,    A,   1, x );
      else                     Mjoin( PATL, tpsvLC )( DIAG, N,    A,   N, x );
   }
#endif
#ifdef TREAL
   else
#else
   else if( TRANS == AtlasTrans )
#endif
   {
      if( UPLO == AtlasUpper ) Mjoin( PATL, tpsvUT )( DIAG, N,    A,   1, x );
      else                     Mjoin( PATL, tpsvLT )( DIAG, N,    A,   N, x );
   }
#ifdef TCPLX
   else
   {
      if( UPLO == AtlasUpper ) Mjoin( PATL, tpsvUH )( DIAG, N,    A,   1, x );
      else                     Mjoin( PATL, tpsvLH )( DIAG, N,    A,   N, x );
   }
#endif
   if( vx ) { Mjoin( PATL, copy )( N, x, 1, X, INCX ); free( vx ); }
/*
 * End of Mjoin( PATL, tpsv )
 */
}
