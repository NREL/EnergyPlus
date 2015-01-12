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

void Mjoin( PATL, gpr )
(
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * X,
   const int                  INCX,
   const TYPE                 * Y,
   const int                  INCY,
   TYPE                       * A,
   const int                  LDA
)
{
/*
 * .. Local Variables ..
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) || ( SCALAR_IS_ZERO( ALPHA ) ) ) return;

   Mjoin( PATL, refgpr )( UPLO, M, N, ALPHA, X, INCX, Y, INCY, A, LDA );
/*
 * End of Mjoin( PATL, gpr )
 */
}

void Mjoin( PATL, gpr1L_a1_x1_yX )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * X,
   const int                  INCX,
   const TYPE                 * Y,
   const int                  INCY,
   TYPE                       * A,
   const int                  LDA
)
{ Mjoin( PATL, gpr )( AtlasLower, M, N, ATL_rone, X, 1, Y, INCY, A, LDA ); }

void Mjoin( PATL, gpr1U_a1_x1_yX )
(
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * X,
   const int                  INCX,
   const TYPE                 * Y,
   const int                  INCY,
   TYPE                       * A,
   const int                  LDA
)
{ Mjoin( PATL, gpr )( AtlasUpper, M, N, ATL_rone, X, 1, Y, INCY, A, LDA ); }
