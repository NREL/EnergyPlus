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
#define USE_SPRK
/*
 * Include files
 */
#include "atlas_rblas3.h"
#include "atlas_kernel3.h"
#include "atlas_lvl3.h"
#ifdef USE_SPRK
#include "atlas_pkblas.h"
#endif

#ifndef HERK_NB
#define HERK_NB      NB
#endif

void Mjoin( PATL, herk )
(
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  N,
   const int                  K,
   const TYPE                 ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 BETA,
   TYPE                       * C,
   const int                  LDC
)
{
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, herk )  performs one of the @(syhe_comm) rank k operations
 *
 *    C := alpha * A * conjg( A' ) + beta * C,
 *
 * or
 *
 *    C := alpha * conjg( A' ) * A + beta * C,
 *
 * where alpha and beta are  real  scalars, C is an n by n @(syhe_comm) ma-
 * trix and  A is an n by k matrix in the first case and a k by n matrix
 * in the second case.
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
   TYPE                       Calph[2], Cbeta[2];
   RC3_FUN_HERK_T            ATL_rherk;
   RC3_HERK_T                type;
/* ..
 * .. Executable Statements ..
 *
 */
   #if defined (USE_SPRK)
      Mjoin(PATL,hprk)(PackGen, TRANS, UPLO, 0, N, K, ALPHA, A, 0, 0, LDA,
                       BETA, C, 0, 0, LDC);
      return;
   #endif
   if( ( N == 0 ) ||
       ( ( ( ALPHA == ATL_rzero ) || ( K == 0 ) ) && ( BETA == ATL_rone ) ) )
      return;

   if( ( ALPHA == ATL_rzero ) || ( K == 0 ) )
   { Mjoin( PATL, hescal )( UPLO, N, N, BETA, C, LDC ); return; }

   type.size = sizeof( TYPE[2] );
   *Calph    = ALPHA; Calph[1] = ATL_rzero;
   *Cbeta    = BETA;  Cbeta[1] = ATL_rzero;

   if( TRANS == AtlasNoTrans )
   {
      type.Tgemm = Mjoin( PATL, gemmNC_RB );
      if( UPLO == AtlasUpper )
      { type.Therk = Mjoin( PATL, herkUN ); ATL_rherk = ATL_rherkUN; }
      else
      { type.Therk = Mjoin( PATL, herkLN ); ATL_rherk = ATL_rherkLN; }
   }
   else
   {
      type.Tgemm = Mjoin( PATL, gemmCN_RB );
      if( UPLO == AtlasUpper )
      { type.Therk = Mjoin( PATL, herkUC ); ATL_rherk = ATL_rherkUC; }
      else
      { type.Therk = Mjoin( PATL, herkLC ); ATL_rherk = ATL_rherkLC; }
   }

   ATL_rherk( &type, N, K, (void *)(Calph), (void *)(A), LDA, (void *)(Cbeta),
              (void *)(C), LDC, HERK_NB );
/*
 * End of Mjoin( PATL, herk )
 */
}
