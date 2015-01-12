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

void ATL_csrefscal
(
   const int                  N,
   const float                ALPHA,
   float                      * X,
   const int                  INCX
)
{
/*
 * Purpose
 * =======
 *
 * ATL_csrefscal performs the following operation:
 *
 *    x := alpha * x,
 *
 * where alpha is a real scalar and x is an n-vector.
 *
 * Arguments
 * =========
 *
 * N       (input)                       const int
 *         On entry, N specifies the length of the vector x. N  must  be
 *         at least zero. Unchanged on exit.
 *
 * ALPHA   (input)                       const float
 *         On entry, ALPHA specifies the real scalar alpha.  When  ALPHA
 *         is supplied as zero,  then the entries of the incremented ar-
 *         ray X need not be set on input. Unchanged on exit.
 *
 * X       (input/output)                float *
 *         On entry,  X  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCX ) ) * sizeof( float [2] ),
 *         that contains the vector x.  On exit,  the entries of the in-
 *         cremented array X are mutiplied by alpha.
 *
 * INCX    (input)                       const int
 *         On entry, INCX specifies the increment for the elements of X.
 *         INCX must not be zero. Unchanged on exit.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   register const float       alpha = ALPHA;
   register float             x0_r, x0_i, x1_r, x1_i, x2_r, x2_i, x3_r, x3_i;
   float                      * StX;
   register int               i;
   int                        incx2 = 2 * INCX,  nu;
   const int                  incX2 = 2 * incx2, incX3 = 3 * incx2,
                              incX4 = 4 * incx2;
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( N > 0 ) && ( alpha != ATL_sONE ) )
   {
      if( alpha == ATL_sZERO )
      {
         if( ( nu = ( N >> 2 ) << 2 ) != 0 )
         {
            StX = (float *)X + nu * incx2;

            do
            {
               (*X)       = ATL_sZERO; X[incX2  ] = ATL_sZERO;
               X[1]       = ATL_sZERO; X[incX2+1] = ATL_sZERO;
               X[incx2  ] = ATL_sZERO; X[incX3  ] = ATL_sZERO;
               X[incx2+1] = ATL_sZERO; X[incX3+1] = ATL_sZERO;

               X  += incX4;

            } while( X != StX );
         }

         for( i = N - nu; i != 0; i-- )
         {
            *X   = ATL_sZERO;
            X[1] = ATL_sZERO;
            X   += incx2;
         }
      }
      else
      {
         if( ( nu = ( N >> 2 ) << 2 ) != 0 )
         {
            StX = (float *)X + nu * incx2;

            do
            {
               x0_r  = (*X);       x2_r  = X[incX2  ];
               x0_i  = X[1];       x2_i  = X[incX2+1];
               x1_r  = X[incx2  ]; x3_r  = X[incX3  ];
               x1_i  = X[incx2+1]; x3_i  = X[incX3+1];

               x0_r *= alpha;      x2_r *= alpha;
               x0_i *= alpha;      x2_i *= alpha;
               x1_r *= alpha;      x3_r *= alpha;
               x1_i *= alpha;      x3_i *= alpha;

               (*X)       = x0_r;  X[incX2  ] = x2_r;
               X[1]       = x0_i;  X[incX2+1] = x2_i;
               X[incx2  ] = x1_r;  X[incX3  ] = x3_r;
               X[incx2+1] = x1_i;  X[incX3+1] = x3_i;

               X  += incX4;

            } while( X != StX );
         }

         for( i = N - nu; i != 0; i-- )
         {
            x0_r  = (*X);  x0_i  = X[1];
            x0_r *= alpha; x0_i *= alpha;
            *X    = x0_r;  X[1]  = x0_i;
            X    += incx2;
         }
      }
   }
/*
 * End of ATL_csrefscal
 */
}
