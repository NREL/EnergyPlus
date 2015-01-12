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
#include "atlas_f77wrap.h"

void Mjoin( PATLF77WRAP, tpsv )
(
   F77_INTEGER                * IUPLO,
   F77_INTEGER                * ITRANS,
   F77_INTEGER                * IDIAG,
   F77_INTEGER                * N,
   TYPE                       * A,
   TYPE                       * X,
   F77_INTEGER                * INCX
)
{
/*
 * Purpose
 * =======
 *
 * ATL_F77wrap_tpsv solves one of the systems of equations
 *
 *    A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
 *
 * where b and x are n-element vectors and  A is an n by n unit, or non-
 * unit, upper or lower triangular matrix, supplied in packed form.
 *
 * No test for  singularity  or  near-singularity  is included  in  this
 * routine. Such tests must be performed before calling this routine.
 *
 * Notes
 * =====
 *
 * This routine is an internal wrapper function written in  C  called by
 * the corresponding Fortran 77 user callable subroutine.  It calls  the
 * appropriate ATLAS routine performing the actual computation.
 *
 * This wrapper layer resolves the following portability issues:
 *
 *    - the routines' name sheme translation imposed by the  Fortran / C
 *      compilers of your target computer,
 *    - the translation of Fortran characters into the ATLAS  correspon-
 *      ding C enumerated type (in cooperation with the Fortan user cal-
 *      lable subroutine),
 *    - the translation of Fortran integers into the proper C correspon-
 *      ding native type;
 *
 * and the following ease-of-programming issue:
 *
 *    - a pointer to the the first entry of vector operands (when appli-
 *      cable) is passed to the  ATLAS computational routine even if the
 *      corresponding input increment value is negative. This allows for
 *      a more natural expression in  C  of the computation performed by
 *      these ATLAS functions.
 *
 * ---------------------------------------------------------------------
 */
/* ..
 * .. Executable Statements ..
 *
 */
   Mjoin( PATL, tpsv )( *IUPLO, *ITRANS, *IDIAG, *N, A, W1N( N, X, INCX ),
                        *INCX );
/*
 * End of Mjoin( PATLF77WRAP, tpsv )
 */
}
