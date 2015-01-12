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

void ATL_rtrsmLUT
(
   RC3_TRSM_T                 * RTYP,
   const int                  M,
   const int                  N,
   const void                 * ALPHA,
   const void                 * A,
   const int                  LDA,
   void                       * B,
   const int                  LDB,
   const int                  RB
)
{
/*
 * Purpose
 * =======
 *
 * ATL_rtrsmLUT solves the matrix equation
 *
 *    A' * X = alpha * B,
 *
 * where alpha is a scalar, X and B are m by n matrices, A is a unit, or
 * non-unit, upper triangular matrix. The matrix X is overwritten on B.
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
   { RTYP->Ttrsm( M, N, ALPHA, A, LDA, B, LDB ); return; }

   m2 = M - ( m1 = RB + ( m1 / ( RB << 1 ) ) * RB ); size = RTYP->size;

   ATL_rtrsmLUT( RTYP, m1, N, ALPHA, A, LDA, B, LDB, RB );

   RTYP->Tgemm( m2, N, m1, RTYP->negone, Mrc3( A, 0, m1, LDA, size ),
                LDA, B, LDB, ALPHA, Mrc3( B, m1, 0, LDB, size ), LDB );

   ATL_rtrsmLUT( RTYP, m2, N, RTYP->one, Mrc3( A, m1, m1, LDA, size ),
                 LDA, Mrc3( B, m1, 0, LDB, size ), LDB, RB );
/*
 * End of ATL_rtrsmLUT
 */
}
