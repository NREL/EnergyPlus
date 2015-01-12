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

#ifndef SYR2K_NB
#define SYR2K_NB      NB
#endif

void Mjoin( PATL, syr2k )
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
   const SCALAR               BETA,
   TYPE                       * C,
   const int                  LDC
)
{
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, syr2k )  performs one of the @(syhe_comm) rank 2k operations
 *
 *    C := alpha * A * B' + alpha * B * A' + beta * C,
 *
 * or
 *
 *    C := alpha * A' * B + alpha * B' * A + beta * C,
 *
 * where alpha and beta are scalars, C is an n by n @(syhe_comm) matrix and
 * A and B are n by k matrices in the first case and k by n  matrices in
 * the second case.
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
   TYPE                       one = ATL_rone;
   TYPE                       * alpha, * beta;
#else
   TYPE                       one[2] = { ATL_rone, ATL_rzero };
   TYPE                       * alpha, * beta;
#endif
   RC3_FUN_SYR2K_T            ATL_rsyr2k;
   RC3_SYR2K_T                type;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( N == 0 ) ||
       ( ( SCALAR_IS_ZERO( ALPHA ) || ( K == 0 ) ) && SCALAR_IS_ONE( BETA ) ) )
      return;

   if( ( SCALAR_IS_ZERO( ALPHA ) ) || ( K == 0 ) )
   { Mjoin( PATL, trscal )( UPLO, N, N, BETA, C, LDC ); return; }
#ifdef TREAL
   type.size = sizeof( TYPE );    type.one = (void *)(&one);
   alpha     = &alpha0;           beta     = &beta0;
#else
   type.size = sizeof( TYPE[2] ); type.one = (void *)one;
   alpha     = (TYPE *)(ALPHA);   beta     = (TYPE *)(BETA);
#endif

   if( TRANS == AtlasNoTrans )
   {
      type.Tgemm = Mjoin( PATL, gemmNT_RB );
      if( UPLO == AtlasUpper )
      { type.Tsyr2k = Mjoin( PATL, syr2kUN ); ATL_rsyr2k = ATL_rsyr2kUN; }
      else
      { type.Tsyr2k = Mjoin( PATL, syr2kLN ); ATL_rsyr2k = ATL_rsyr2kLN; }
   }
   else
   {
      type.Tgemm = Mjoin( PATL, gemmTN_RB );
      if( UPLO == AtlasUpper )
      { type.Tsyr2k = Mjoin( PATL, syr2kUT ); ATL_rsyr2k = ATL_rsyr2kUT; }
      else
      { type.Tsyr2k = Mjoin( PATL, syr2kLT ); ATL_rsyr2k = ATL_rsyr2kLT; }
   }

   ATL_rsyr2k( &type, N, K, (void *)(alpha), (void *)(A), LDA, (void *)(B),
               LDB, (void *)(beta), (void *)(C), LDC, SYR2K_NB );
/*
 * End of Mjoin( PATL, syr2k )
 */
}
