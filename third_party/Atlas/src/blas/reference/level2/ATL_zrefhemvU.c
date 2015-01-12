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

void ATL_zrefhemvU
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
 * ATL_zrefhemvU( ... )
 *
 * <=>
 *
 * ATL_zrefhemv( AtlasUpper, ... )
 *
 * See ATL_zrefhemv for details.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
/*
 * .. Local Variables ..
 */
   register double            t0_i, t0_r, t1_i, t1_r;
   int                        i, iaij, ix, iy, j, jaj, jx, jy;
   int                        incx2 = 2 * INCX, incy2 = 2 * INCY,
                              lda2 = ( LDA << 1 );
/* ..
 * .. Executable Statements ..
 *
 */
   Mzvscal( N, BETA, Y, INCY );

   for( j = 0,      jaj  = 0,    jx  = 0,     jy  = 0;
        j < N; j++, jaj += lda2, jx += incx2, jy += incy2 )
   {
      Mmul( ALPHA[0], ALPHA[1], X[jx], X[jx+1], t0_r, t0_i );
      Mset( ATL_dZERO, ATL_dZERO, t1_r, t1_i );

      for( i = 0,      iaij  = jaj, ix  = 0,     iy  = 0;
           i < j; i++, iaij += 2,   ix += incx2, iy += incy2 )
      {
         Mmla( A[iaij], A[iaij+1], t0_r, t0_i, Y[iy], Y[iy+1] );
         Mmla( A[iaij], -A[iaij+1], X[ix], X[ix+1], t1_r, t1_i );
      }
      Mset( Y[jy] + A[iaij]*t0_r, Y[jy+1] + A[iaij]*t0_i, Y[jy], Y[jy+1] );
      Mmla( ALPHA[0], ALPHA[1], t1_r, t1_i, Y[jy], Y[jy+1] );
   }
/*
 * End of ATL_zrefhemvU
 */
}
