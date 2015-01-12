/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
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
#include "atlas_refmisc.h"
#include "atlas_reflvl2.h"
#include "atlas_reflevel2.h"

void ATL_zreftbsvLHN
(
   const int                  N,
   const int                  K,
   const double               * A,
   const int                  LDA,
   double                     * X,
   const int                  INCX
)
{
/*
 * Purpose
 * =======
 *
 * ATL_zreftbsvLHN( ... )
 *
 * <=>
 *
 * ATL_zreftbsv( AtlasLower, AtlasConjTrans, AtlasNonUnit, ... )
 *
 * See ATL_zreftbsv for details.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   register double            t0_i, t0_r;
   int                        i, i1, iaij, incx2 = 2 * INCX, ix, j, jaj,
                              jx, kx = 0, lda2 = ( LDA << 1 );
/* ..
 * .. Executable Statements ..
 *
 */
   for( j = N-1,     jaj  = (N-1)*lda2, jx  = kx+(N-1)*incx2;
        j >= 0; j--, jaj -= lda2,       jx -= incx2 )
   {
      Mset( X[jx], X[jx+1], t0_r, t0_i );
      i1   = ( N - 1 > j + K ? j + K : N - 1 );
      for( i = j+1,      iaij  = 2+jaj, ix  = jx + incx2;
           i <= i1; i++, iaij += 2,     ix += incx2 )
      { Mmls( A[iaij], -A[iaij+1], X[ix], X[ix+1], t0_r, t0_i ); }
      Mddiv( A[jaj], -A[jaj+1], t0_r, t0_i );
      Mset( t0_r, t0_i, X[jx], X[jx+1] );
   }
/*
 * End of ATL_zreftbsvLHN
 */
}
