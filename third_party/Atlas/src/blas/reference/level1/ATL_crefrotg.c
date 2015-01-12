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
#include "atlas_reflevel1.h"

void ATL_crefrotg
(
   float                      * A,
   const float                * B,
   float                      * C,
   float                      * S
)
{
/*
 * Purpose
 * =======
 *
 * ATL_crefrotg  constructs a Givens plane rotation. Given the scalars a
 * and b, this routine computes the following quantities:
 *
 * if |a| = 0, then c = 0, s = 1, and r = b;
 * and otherwise:
 *    norm = sqrt( |a|^2 + |b|^2 );
 *    c    = |a| / norm;
 *    s    = a * conjg( b ) / ( |a| * norm );
 *    r    = a / ( |a| * norm );
 *
 * See ``Basic Linear Algebra Subprograms for Fortran Usage'' by C. Law-
 * son, R. Hanson, D. Kincaid and F. Krogh, ACM Transactions on Mathema-
 * tical Software, 1979, 5(3) pp 308-323, for further information.
 *
 * Arguments
 * =========
 *
 * A       (input/output)                float *
 *         On entry, A specifies the scalar a. On exit, A is overwritten
 *         by the scalar r defined above.
 *
 * B       (input)                       const float *
 *         On entry, B  specifies the scalar b. Unchanged on exit.
 *
 * C       (output)                      float *
 *         On exit, C  specifies the real scalar c defined above.
 *
 * S       (output)                      float *
 *         On exit, S  specifies the scalar s defined above.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   register float             absa, absb, ia, ialpha, ib, norm,
                              ra, ralpha, rb, scale, tmp, w, z;
/* ..
 * .. Executable Statements ..
 *
 */
   ra = Msabs( *A   ); ia = Msabs( A[1] );
   w  = Mmax( ra, ia ); z  = Mmin( ra, ia );
   if( z == ATL_sZERO ) { absa = w; }
   else { tmp  = z / w; absa = w * sqrt( ATL_sONE + ( tmp * tmp ) ); }

   if( absa == ATL_sZERO )
   {
      *C     = ATL_sZERO;
      Mset( ATL_sONE, ATL_sZERO, *S, S[1] );
      Mset( *B, B[1], *A, A[1] );
   }
   else
   {
      rb     = Msabs( *B   ); ib     = Msabs( B[1] );
      w      = Mmax( rb, ib ); z      = Mmin( rb, ib );
      if( z == ATL_sZERO ) { absb = w; }
      else { tmp  = z / w; absb = w * sqrt( ATL_sONE + ( tmp * tmp ) ); }

      scale  = absa + absb;
      Mset( *A / scale, A[1] / scale, ra, ia );
      Mset( *B / scale, B[1] / scale, rb, ib );
      norm   = scale * sqrt( ( ra * ra ) + ( ia * ia ) +
                             ( rb * rb ) + ( ib * ib ) );
      Mset( *A / absa, A[1] / absa, ralpha, ialpha );
      *C     = absa / norm;
      Mmul( ralpha, ialpha, *B, -B[1], *S, S[1] );
      Mset( *S  / norm, S[1] / norm, *S, S[1] );
      Mset( ralpha * norm, ialpha * norm, *A, A[1] );
   }
/*
 * End of ATL_crefrotg
 */
}
