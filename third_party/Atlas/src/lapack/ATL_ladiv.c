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
 *      void      FUNCTION ZLADIV( X, Y, Z)
 *
 * ATL_ladiv.c :
 *             void ATL_cmladiv( TYPE *X, TYPE *Y, TYPE  *Z)
 *     NOTE :  a) ATL_ladiv.c will get compiled to two  precisions
 *                single precision complex,   double precision complex
 *
 *             b) This should be called only for real numbers
 *
 * Purpose
 * =======
 *
 * Z := X / Y, where X and Y are complex.  The computation of X / Y
 * will not overflow on an intermediary step unless the results
 * overflows.
 *
 *        This performs complex division in  real arithmetic
 *
 *                               a + i*b
 *                    p + i*q = ---------
 *                               c + i*d
 *
 *        The algorithm is due to Robert L. Smith and can be found
 *         in D. Knuth, The art of Computer Programming, Vol.2, p.195
 *
 * Arguments
 * =========
 *
 *         X       (input)
 *         Y       (input)
 *                 The complex scalars X and Y ( pointer to X and Y).
 *         Z       (input/output) is the output  ( pointer  to Z)
 *
 */
#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_lapack.h"


void ATL_ladiv(const TYPE *X, const TYPE *Y, TYPE  *Z)
{
   TYPE   E, F;

/* If           X[0], X[1], Y[0], Y[1], &Z[0], &Z[1]  is mapped to            */
/* real numbers  A,    B,    C,    D,    *P,   *Q                             */
/* the computation is as below                                                */
/*                                                                            */
/*   if ( fabs(D) < fabs( C) ) {                                              */
/*         E = D / C ;                                                        */
/*         F = C + D*E ;                                                      */
/*         *P = ( A+B*E ) / F ;                                               */
/*         *Q = ( B-A*E ) / F ;                                               */
/*                                                                            */
/*   } else{                                                                  */
/*         E = C / D ;                                                        */
/*         F = D + C*E ;                                                      */
/*         *P = ( B+A*E ) / F ;                                               */
/*         *Q = ( -A+B*E ) / F ;                                              */
/*   }                                                                        */

   if ( Mabs(Y[1])  < Mabs(Y[0]) )
   {
      E = Y[1]/Y[0];
      F = Y[0] + Y[1]*E;
      *(Z)   = ( X[0] + X[1]*E ) / F ;
      *(Z+1) = ( X[1] - X[0]*E ) / F ;
   }
   else
   {
      E = Y[0]/Y[1];
      F = Y[1] + Y[0]*E;
      *(Z)   = (X[1]+ X[0]*E ) / F ;
      *(Z+1) = (-X[0] + X[1]*E ) / F ;
   }
}                                           /* END AL_ladiv                   */
