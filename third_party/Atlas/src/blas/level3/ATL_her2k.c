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

#ifndef HER2K_NB
#define HER2K_NB      NB
#endif

void Mjoin( PATL, her2k )
(
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const int                  N,
   const int                  K,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   const TYPE                 * B,
   const int                  LDB,
   const TYPE                 BETA,
   TYPE                       * C,
   const int                  LDC
)
{
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, her2k )  performs one of the @(syhe_comm) rank 2k operations
 *
 *    C := alpha * A * conjg( B )' + conjg( alpha ) * B * conjg( A )' +
 *         beta * C,
 *
 * or
 *
 *    C := alpha * conjg( A' ) * B + conjg( alpha ) * conjg( B' ) * A +
 *         beta * C,
 *
 * where  alpha  and  beta are scalars with  beta  real,  C is an n by n
 * @(syhe_comm) matrix and  A  and  B are n by k matrices in the first case
 * and k by n matrices in the second case.
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
   TYPE                       one[2] = { ATL_rone, ATL_rzero };
   TYPE                       * alpha;
   TYPE                       Calph[2], Cbeta[2];
   RC3_FUN_HER2K_T            ATL_rher2k;
   RC3_HER2K_T                type;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( N == 0 ) ||
       ( ( SCALAR_IS_ZERO( ALPHA ) || ( K == 0 ) ) && ( BETA == ATL_rone ) ) )
      return;

   if( ( SCALAR_IS_ZERO( ALPHA ) ) || ( K == 0 ) )
   { Mjoin( PATL, hescal )( UPLO, N, N, BETA, C, LDC ); return; }

   type.size = sizeof( TYPE[2] ); type.one = (void *)one;
   alpha     = (TYPE *)(ALPHA);
   *Calph    = *ALPHA;            Calph[1] = -ALPHA[1];
   *Cbeta    = BETA;              Cbeta[1] = ATL_rzero;

   if( TRANS == AtlasNoTrans )
   {
      type.Tgemm = Mjoin( PATL, gemmNC_RB );
      if( UPLO == AtlasUpper )
      { type.Ther2k = Mjoin( PATL, her2kUN ); ATL_rher2k = ATL_rher2kUN; }
      else
      { type.Ther2k = Mjoin( PATL, her2kLN ); ATL_rher2k = ATL_rher2kLN; }
   }
   else
   {
      type.Tgemm = Mjoin( PATL, gemmCN_RB );
      if( UPLO == AtlasUpper )
      { type.Ther2k = Mjoin( PATL, her2kUC ); ATL_rher2k = ATL_rher2kUC; }
      else
      { type.Ther2k = Mjoin( PATL, her2kLC ); ATL_rher2k = ATL_rher2kLC; }
   }

   ATL_rher2k( &type, N, K, (void *)(alpha), (void *)(Calph), (void *)(A),
               LDA, (void *)(B), LDB, (void *)(Cbeta), (void *)(C), LDC,
               HER2K_NB );
/*
 * End of Mjoin( PATL, her2k )
 */
}
