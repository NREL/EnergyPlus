/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 Siju Samuel
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
 *      SUBROUTINE DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
 *
 * ATL_larf.c
 * void ATL_larf(const enum  CBLAS_SIDE SIDE, const int M, const int N,TYPE *V,
 *              int INCV, SCALAR TAU, TYPE *C, int LDC, TYPE *WORK)
 *
 *     NOTE :   ATL_larf.c will get compiled to four precisions
 *                   single precision real,      double precision real
 *                   single precision complex,   double precision complex
 *
 *  Purpose
 *  =======
 *
 *  ATL_larf  applies a real/complex elementary reflector H to a real/complex
 *  m by n matrix  C, from either the left or the right. H is represented in
 *  the form
 *
 *        H = I - tau * v * v'                      ( For Real precision)
 *        H = I - tau * v * conjugate(v)'           ( For Real precision)
 *
 *  where tau is a real/complex  scalar and v is a real/complex  vector.
 *
 *  If tau = 0, then H is taken to be the unit matrix.
 *
 *  Arguments
 *  =========
 *
 *  SIDE    (input) CHARACTER*1
 *          = 'L': form  H * C
 *          = 'R': form  C * H
 *
 *  M       (input) INTEGER
 *          The number of rows of the matrix C.
 *
 *  N       (input) INTEGER
 *          The number of columns of the matrix C.
 *
 *  V       (input) array, dimension
 *                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
 *                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
 *          The vector v in the representation of H. V is not used if
 *          TAU = 0.
 *
 *  INCV    (input) INTEGER
 *          The increment between elements of v. INCV <> 0.
 *
 *  TAU     (input)
 *          The value tau in the representation of H.
 *          For complex  precison, it is a pointer  to  array
 *
 *  C       (input/output) array, dimension (LDC,N)
 *          On entry, the m by n matrix C.
 *          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
 *          or C * H if SIDE = 'R'.
 *
 *  LDC     (input) INTEGER
 *          The leading dimension of the array C. LDC >= max(1,M).
 *
 *  WORK    (workspace) array, dimension
 *                         (N) if SIDE = 'L'
 *                      or (M) if SIDE = 'R'
 *
 */
#include "atlas_misc.h"
#include <math.h>
#include "cblas.h"
#include "atlas_lapack.h"

#ifdef TREAL
    #define MY_TRANS CblasTrans
#else
    #define MY_TRANS CblasConjTrans
#endif

void ATL_larf(const enum CBLAS_SIDE SIDE, ATL_CINT M, ATL_CINT N,
              const TYPE *V, ATL_CINT INCV, const SCALAR TAU,
              TYPE *C, int LDC, TYPE *WORK)
{
   #ifdef TREAL
      const TYPE ONE     = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      TYPE NEGTAUVAL ;
   #else
      const TYPE ONE[2]     = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      TYPE NEGTAUVAL[2] ;
   #endif

   if (SIDE == CblasLeft)
   {
/*
 *        Form  H * C
 */
      if (! SCALAR_IS_ZERO(TAU) )
      {
         #ifdef TREAL
            NEGTAUVAL = 0.-TAU;
         #else
            NEGTAUVAL[0]=0.-TAU[0];
            NEGTAUVAL[1]=0.-TAU[1];
         #endif
/*
 *           w := C' * v
 */
         cblas_gemv(CblasColMajor, MY_TRANS, M, N, ONE, C, LDC,
                    V, INCV, ZEROVAL, WORK, 1);
/*
 *           C := C - v * w'
 */
         #ifdef TREAL
            cblas_ger(CblasColMajor, M, N, NEGTAUVAL, V, INCV, WORK, 1,
                      C, LDC);
         #else
            cblas_gerc(CblasColMajor, M, N, NEGTAUVAL, V, INCV, WORK, 1,
                       C, LDC);
         #endif

      }
   }
   else                                     /* SIDE != CblasLeft              */
   {
/*
 *        Form  C * H
 */
      if (! SCALAR_IS_ZERO(TAU) )
      {
         #ifdef TREAL
            NEGTAUVAL = 0.-TAU;
         #else
            NEGTAUVAL[0]=0.-TAU[0];
            NEGTAUVAL[1]=0.-TAU[1];
         #endif
/*
 *           w := C * v
 */
         cblas_gemv(CblasColMajor, CblasNoTrans, M, N, ONE,
                    C, LDC, V, INCV, ZEROVAL, WORK, 1);
/*
 *           C := C - w * v'
 */
         #ifdef TREAL
            cblas_ger(CblasColMajor, M, N, NEGTAUVAL, WORK, 1,
                      V, INCV, C, LDC);
         #else
            cblas_gerc(CblasColMajor, M, N, NEGTAUVAL, WORK, 1,
                       V, INCV, C, LDC);
         #endif
      }
   }
      return;
}                                           /* END ATL_larf                   */
