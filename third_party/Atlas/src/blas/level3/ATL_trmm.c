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

#ifndef TRMM_NB
#define TRMM_NB      NB
#endif

void Mjoin( PATL, trmm )
(
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  M,
   const int                  N,
   const SCALAR               ALPHA,
   const TYPE                 * A,
   const int                  LDA,
   TYPE                       * B,
   const int                  LDB
)
{
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, trmm )  performs one of the matrix-matrix operations
 *
 *    B := alpha * op( A ) * B,   or    B := alpha * B * op( A ),
 *
 * where alpha is a scalar, B is an m by n matrix, A is a unit,  or non-
 * unit, upper or lower triangular matrix and op( X ) is one of
 *
 *    op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ).
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
   TYPE                       alpha0 = (TYPE)(ALPHA);
   const TYPE                 one = ATL_rone;
#else
   TYPE                       one[2] = { ATL_rone, ATL_rzero };
#endif
   TYPE                       * alpha;
   RC3_FUN_TRMM_T             ATL_rtrmm;
   RC3_TRMM_T                 type;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) ) return;

   if( SCALAR_IS_ZERO( ALPHA ) )
   { Mjoin( PATL, gescal )( M, N, ALPHA, B, LDB ); return; }

#ifdef TREAL
   type.size   = sizeof( TYPE );    type.one = (void *)(&one);
   alpha       = &alpha0;
#else
   type.size   = sizeof( TYPE[2] ); type.one = (void *)one;
   alpha       = (TYPE *)(ALPHA);
#endif

   if( SIDE == AtlasLeft )
   {
      if( TRANS == AtlasNoTrans )
      {
         type.Tgemm = Mjoin( PATL, gemmNN_RB );
         if( UPLO == AtlasUpper )
         {
            ATL_rtrmm = ATL_rtrmmLUN;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmLUNN );
            else                       type.Ttrmm = Mjoin( PATL, trmmLUNU );
         }
         else
         {
            ATL_rtrmm = ATL_rtrmmLLN;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmLLNN );
            else                       type.Ttrmm = Mjoin( PATL, trmmLLNU );
         }
      }
#ifdef TREAL
      else
#else
      else if( TRANS == AtlasTrans )
#endif
      {
         type.Tgemm = Mjoin( PATL, gemmTN_RB );
         if( UPLO == AtlasUpper)
         {
            ATL_rtrmm = ATL_rtrmmLUT;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmLUTN );
            else                       type.Ttrmm = Mjoin( PATL, trmmLUTU );
         }
         else
         {
            ATL_rtrmm = ATL_rtrmmLLT;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmLLTN );
            else                       type.Ttrmm = Mjoin( PATL, trmmLLTU );
         }
      }
#ifdef TCPLX
      else
      {
         type.Tgemm = Mjoin( PATL, gemmCN_RB );
         if( UPLO == AtlasUpper )
         {
            ATL_rtrmm = ATL_rtrmmLUC;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmLUCN );
            else                       type.Ttrmm = Mjoin( PATL, trmmLUCU );
         }
         else
         {
            ATL_rtrmm = ATL_rtrmmLLC;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmLLCN );
            else                       type.Ttrmm = Mjoin( PATL, trmmLLCU );
         }
      }
#endif
   }
   else
   {
      if( TRANS == AtlasNoTrans )
      {
         type.Tgemm = Mjoin( PATL, gemmNN_RB );
         if( UPLO == AtlasUpper )
         {
            ATL_rtrmm = ATL_rtrmmRUN;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmRUNN );
            else                       type.Ttrmm = Mjoin( PATL, trmmRUNU );
         }
         else
         {
            ATL_rtrmm = ATL_rtrmmRLN;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmRLNN );
            else                       type.Ttrmm = Mjoin( PATL, trmmRLNU );
         }
      }
#ifdef TREAL
      else
#else
      else if( TRANS == AtlasTrans )
#endif
      {
         type.Tgemm = Mjoin( PATL, gemmNT_RB );
         if( UPLO == AtlasUpper )
         {
            ATL_rtrmm = ATL_rtrmmRUT;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmRUTN );
            else                       type.Ttrmm = Mjoin( PATL, trmmRUTU );
         }
         else
         {
            ATL_rtrmm = ATL_rtrmmRLT;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmRLTN );
            else                       type.Ttrmm = Mjoin( PATL, trmmRLTU );
         }
      }
#ifdef TCPLX
      else
      {
         type.Tgemm = Mjoin( PATL, gemmNC_RB );
         if( UPLO == AtlasUpper )
         {
            ATL_rtrmm = ATL_rtrmmRUC;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmRUCN );
            else                       type.Ttrmm = Mjoin( PATL, trmmRUCU );
         }
         else
         {
            ATL_rtrmm = ATL_rtrmmRLC;
            if( DIAG == AtlasNonUnit ) type.Ttrmm = Mjoin( PATL, trmmRLCN );
            else                       type.Ttrmm = Mjoin( PATL, trmmRLCU );
         }
      }
#endif
   }

   ATL_rtrmm( &type, M, N, (void *)(alpha), (void *)(A), LDA, (void *)(B),
              LDB, TRMM_NB );
/*
 * End of Mjoin( PATL, trmm )
 */
}
