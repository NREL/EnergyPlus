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

void ATL_rhemmLL
(
   RC3_HEMM_T                 * RTYP,
   const int                  M,
   const int                  N,
   const void                 * ALPHA,
   const void                 * A,
   const int                  LDA,
   const void                 * B,
   const int                  LDB,
   const void                 * BETA,
   void                       * C,
   const int                  LDC,
   const int                  RB
)
{
/*
 * Purpose
 * =======
 *
 * ATL_rhemmLL performs the following matrix-matrix operation
 *
 *    C := alpha * A * B + beta * C,
 *
 * where alpha and beta are scalars, A is a (lower) Hermitian matrix and
 * B and C are m by n matrices.
 *
 * This is a type-less recursive version of the algorithm.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   size_t                     size;
   int                        m1, m2;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( m1 = M - RB ) <= 0 )
   { RTYP->Themm( M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC ); return; }

   m2 = M - ( m1 = RB + ( m1 / ( RB << 1 ) ) * RB ); size = RTYP->size;

   ATL_rhemmLL( RTYP, m1, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC, RB );

   RTYP->TgemmNN( m2, N, m1, ALPHA, Mrc3( A, m1, 0, LDA, size ), LDA, B,
                  LDB, BETA, Mrc3( C, m1, 0, LDC, size ), LDC );

   RTYP->Tgemm( m1, N, m2, ALPHA, Mrc3( A, m1, 0, LDA, size ), LDA,
                Mrc3( B, m1, 0, LDB, size ), LDB, RTYP->one, C, LDC );

   ATL_rhemmLL( RTYP, m2, N, ALPHA, Mrc3( A, m1, m1, LDA, size ), LDA,
                Mrc3( B, m1, 0, LDB, size ), LDB, RTYP->one, Mrc3( C,
                m1, 0, LDC, size ), LDC, RB );
/*
 * End of ATL_rhemmLL
 */
}
