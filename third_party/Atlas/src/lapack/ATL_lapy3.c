/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2012, 2009 Siju Samuel
 *
 * Code contributers : Siju Samuel, Anthony M. Castaldo, R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*-----------------------------------------------------------------------------
 *  This is the C translation of the standard LAPACK Fortran routine:
 *      DOUBLE PRECISION FUNCTION DLAPY3( X, Y, Z )
 *      NOTE : ATL_lapy3.c  will get compiled to
 *             single precision complex (ATL_clapy3.o)  and
 *              double precision complex (ATL_zlapt3.o)
 *
 *   Purpose
 *   =======
 *   ATL_lapy3 returns sqrt(x**2+y**2+z**2), taking care not to cause
 *         unnecessary overflow.
 *
 *   Arguments
 *   =========
 *
 *   X       (input) single/double precision
 *   Y       (input) single/double precision
 *   Z       (input) single/double precision
 *                 X, Y and Z specify the values x, y and z.
-----------------------------------------------------------------------------*/
#include "cblas.h"
#include "atlas_lapack.h"
#include "math.h"

TYPE  ATL_lapy3(const TYPE X, const TYPE Y, const TYPE Z)
{
   TYPE  ONE=1.0, ZERO=0.0, W, Wtemp,  XABS, YABS, ZABS, TEMP;

   XABS = Mabs(X);
   YABS = Mabs(Y);
   ZABS = Mabs(Z);

/* W : get the maximum absolute value from x,y,z                              */
   Wtemp = (XABS<YABS)?YABS:XABS;
   W = (Wtemp<ZABS)?ZABS:Wtemp;

   if (W == ZERO)
   {
/*    W can be zero for max(0,nan,0). Adding all three entries                */
/*    together will make sure  NaN will not disappear.                        */

      return( XABS + YABS + ZABS);
   }
   else
   {
      TEMP =( XABS / W )*( XABS / W ) +
            ( YABS / W )*( YABS / W ) +
            ( ZABS / W )*( ZABS / W ) ;

      #if defined(SREAL) || defined(SCPLX)
         return (W * sqrtf(TEMP));           /* Use single precision sqrt.    */
      #else
         return (W * sqrt(TEMP));            /* Use double precision sqrt.    */
      #endif
   }
}                                            /* END ATL_?lapy3                */
