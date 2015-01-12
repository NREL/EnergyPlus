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

void ATL_zrefhpmvL
(
   const int                  N,
   const double               * ALPHA,
   const double               * A,
   const int                  LDA,
   const double               * X,
   const int                  INCX,
   const double               * BETA,
   double                     * Y,
   const int                  INCY
)
{
/*
 * Purpose
 * =======
 *
 * ATL_zrefhpmvL( ... )
 *
 * <=>
 *
 * ATL_zrefhpmv( AtlasLower, ... )
 *
 * See ATL_zrefhpmv for details.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   register double            t0_i, t0_r, t1_i, t1_r;
   int                        i, iaij, ix, iy, j, jaj = 0, jx, jy,
                              lda2 = ( LDA << 1 ), incx2 = 2 * INCX,
                              incy2 = 2 * INCY;
/* ..
 * .. Executable Statements ..
 *
 */
   Mzvscal( N, BETA, Y, INCY );

   for( j = 0, jx = 0, jy = 0; j < N; j++, jx += incx2, jy += incy2 )
   {
      Mmul( ALPHA[0], ALPHA[1], X[jx], X[jx+1], t0_r, t0_i );
      Mset( ATL_dZERO, ATL_dZERO, t1_r, t1_i );
      Mset( Y[jy] + A[jaj]*t0_r, Y[jy+1] + A[jaj]*t0_i, Y[jy], Y[jy+1] );
      for( i = j+1,    iaij  = jaj+2, ix  = jx+incx2, iy  = jy+incy2;
           i < N; i++, iaij += 2,     ix += incx2,    iy += incy2 )
      {
         Mmla( A[iaij],  A[iaij+1], t0_r, t0_i, Y[iy], Y[iy+1] );
         Mmla( A[iaij], -A[iaij+1], X[ix], X[ix+1], t1_r, t1_i );
      }
      Mmla( ALPHA[0], ALPHA[1], t1_r, t1_i, Y[jy], Y[jy+1] );
      jaj += lda2; lda2 -= 2;
   }
/*
 * End of ATL_zrefhpmvL
 */
}
