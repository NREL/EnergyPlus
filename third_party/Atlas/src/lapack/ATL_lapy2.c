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

/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
 *
 *      It is important to use the appropriate sqrt function for the
 *      precision given in order to test against the LAPACK fortran
 *      reference library; in particular due to high serial data
 *      dependence the SGEHRD routine can drift far enough on larger
 *      problems to appear to be in error; about 0.0004 in a single
 *      result element. (Modification by Tony Castaldo, 12/29/2011).
 *
 *  Purpose
 *  =======
 *
 *  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
 *  overflow.
 *
 *  Arguments
 *  =========
 *
 *  X       (input) single/double precision
 *  Y       (input) single/double precision
 *          X and Y specify the values x and y.
 *
 */
#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_lapack.h"
#include "math.h"

TYPE  ATL_lapy2(TYPE X, TYPE Y)
{
   TYPE  ONE=1.0, ZERO=0.0, W, Z, XABS, YABS, TEMP;

/* Find absolute values                                                       */
   XABS = Mabs(X);
   YABS = Mabs(Y);
   W = (XABS<YABS)?YABS:XABS;
   Z = (XABS<YABS)?XABS:YABS;

   if (Z == ZERO) return(W);
/* NOTE: If Z != 0, then W != 0                                               */
   TEMP = Z/W;

   TEMP = ONE + TEMP*TEMP;
   #if defined(SREAL) || defined(SCPLX)
   return(W * sqrtf(TEMP));                     /* Use single precision sqrt. */
   #else
   return(W * sqrt(TEMP));                      /* Use double precision sqrt. */
   #endif
}                                               /* END ATL_?lapy2             */


