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

void ATL_csrefrot
(
   const int                  N,
   float                      * X,
   const int                  INCX,
   float                      * Y,
   const int                  INCY,
   const float                C,
   const float                S
)
{
/*
 * Purpose
 * =======
 *
 * ATL_csrefrot applies a  plane  rotation to the two n-vectors x and y.
 * This routine computes:
 *
 *    [ x_i ]   [ c  s ] [ x_i ]
 *    [ y_i ] = [ -s c ] [ y_i ]    for all i = 1 .. n.
 *
 * If n <= 0 or if c = 1 and s = 0, this subroutine returns immediately.
 *
 * Arguments
 * =========
 *
 * N       (input)                       const int
 *         On entry, N specifies the length of the vector x. N  must  be
 *         at least zero. Unchanged on exit.
 *
 * X       (input/output)                float *
 *         On entry,  X  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCX ) ) * sizeof( float [2] ),
 *         that contains the vector x.  On exit,  the entries of the in-
 *         cremented array  X are rotated with the entries of the incre-
 *         mented array  Y.
 *
 * INCX    (input)                       const int
 *         On entry, INCX specifies the increment for the elements of X.
 *         INCX must not be zero. Unchanged on exit.
 *
 * Y       (input/output)                float *
 *         On entry,  Y  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCY ) ) * sizeof( float [2] ),
 *         that contains the vector y.  On exit,  the entries of the in-
 *         cremented array  Y are rotated with the entries of the incre-
 *         mented array  X.
 *
 * INCY    (input)                       const int
 *         On entry, INCY specifies the increment for the elements of Y.
 *         INCY must not be zero. Unchanged on exit.
 *
 * C       (input)                       const float
 *         On entry, C  specifies the real scalar c definiting the plane
 *         rotation. Unchanged on exit.
 *
 * S       (input)                       const float *
 *         On entry, S specifies the scalar s definiting the plane rota-
 *         tion. Unchanged on exit.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   register const float       co = C, si = S;
   register float             x0_r, x0_i, x1_r, x1_i, y0_r, y0_i, y1_r, y1_i;
   float                      * StX;
   register int               i;
   int                        incx2 = 2 * INCX,  incy2 = 2 * INCY,  nu;
   const int                  incX2 = 2 * incx2, incY2 = 2 * incy2;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( N > 0 ) && !Msone( co, si ) )
   {
      if( ( nu = ( N >> 1 ) << 1 ) != 0 )
      {
         StX = (float *)X + nu * incx2;

         do
         {
            x0_r = (*X);       y0_r = (*Y);
            x0_i = X[1];       y0_i = Y[1];
            x1_r = X[incx2  ]; y1_r = Y[incy2  ];
            x1_i = X[incx2+1]; y1_i = Y[incy2+1];

            *X         = co * x0_r + si * y0_r;
            *Y         = co * y0_r - si * x0_r;
            X[1]       = co * x0_i + si * y0_i;
            Y[1]       = co * y0_i - si * x0_i;
            X[incx2  ] = co * x1_r + si * y1_r;
            Y[incy2  ] = co * y1_r - si * x1_r;
            X[incx2+1] = co * x1_i + si * y1_i;
            Y[incy2+1] = co * y1_i - si * x1_i;

            X  += incX2;
            Y  += incY2;

         } while( X != StX );
      }

      for( i = N - nu; i != 0; i-- )
      {
         x0_r = (*X); y0_r = (*Y);
         x0_i = X[1]; y0_i = Y[1];

         *X   = co * x0_r + si * y0_r;
         *Y   = co * y0_r - si * x0_r;
         X[1] = co * x0_i + si * y0_i;
         Y[1] = co * y0_i - si * x0_i;

         X  += incx2;
         Y  += incy2;
      }
   }
/*
 * End of ATL_csrefrot
 */
}
