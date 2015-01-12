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
#include "atlas_rblas3.h"
#include "atlas_kernel3.h"
#include "atlas_lvl3.h"

#ifndef HEMM_NB
#define HEMM_NB      NB
#endif

void Mjoin( PATL, hemm )
(
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * B,
   const int                  LDB,
   const SCALAR               BETA,
   TYPE                       * C,
   const int                  LDC
)
{
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, hemm )  performs one of the matrix-matrix operations
 *
 *    C := alpha * A * B + beta * C,
 *
 * or
 *
 *    C := alpha * B * A + beta * C,
 *
 * where alpha and beta are scalars,  A  is a Hermitian matrix and B and
 * C are m by n matrices.
 *
 * This is a  recursive  version of the  algorithm.  For a more detailed
 * description of  the arguments of this function, see the reference im-
 * plementation in the  ATLAS/src/blas/reference directory.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
#ifdef TREAL
   TYPE                       alpha0 = (TYPE)(ALPHA), beta0 = (TYPE)(BETA);
   const TYPE                 one = ATL_rone;
#else
   TYPE                       one[2] = { ATL_rone, ATL_rzero };
#endif
   TYPE                       * alpha, * beta;
   RC3_FUN_HEMM_T             ATL_rhemm;
   RC3_HEMM_T                 type;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) ||
       ( ( SCALAR_IS_ZERO( ALPHA ) ) && ( SCALAR_IS_ONE( BETA ) ) ) ) return;

   if( SCALAR_IS_ZERO( ALPHA ) )
   { Mjoin( PATL, gescal )( M, N, BETA, C, LDC ); return; }
#ifdef TREAL
   type.size    = sizeof( TYPE );           type.one = (void *)(&one);
   type.TgemmNN = Mjoin( PATL, gemmNN_RB );
   alpha        = &alpha0;                  beta     = &beta0;
#else
   type.size    = sizeof( TYPE[2] );        type.one = (void *)(one);
   type.TgemmNN = Mjoin( PATL, gemmNN_RB );
   alpha = (TYPE *)(ALPHA);                 beta     = (TYPE *)(BETA);
#endif

   if( SIDE == AtlasLeft )
   {
      type.Tgemm = Mjoin( PATL, gemmCN_RB );
      if( UPLO == AtlasUpper )
      { type.Themm = Mjoin( PATL, hemmLU ); ATL_rhemm = ATL_rhemmLU; }
      else
      { type.Themm = Mjoin( PATL, hemmLL ); ATL_rhemm = ATL_rhemmLL; }
   }
   else
   {
      type.Tgemm = Mjoin( PATL, gemmNC_RB );
      if( UPLO == AtlasUpper )
      { type.Themm = Mjoin( PATL, hemmRU ); ATL_rhemm = ATL_rhemmRU; }
      else
      { type.Themm = Mjoin( PATL, hemmRL ); ATL_rhemm = ATL_rhemmRL; }
   }

   ATL_rhemm( &type, M, N, ((void *)alpha), ((void *)A), LDA, ((void *)B),
              LDB, ((void *)beta), ((void *)C), LDC, HEMM_NB );
/*
 * End of Mjoin( PATL, hemm )
 */
}
