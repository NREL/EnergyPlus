/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 1999 Antoine P. Petitet
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
#include "atlas_misc.h"

void Mjoin( PATL, hescal )
(
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const TYPE                 ALPHA,
   TYPE                       * A,
   const int                  LDA
)
{
/*
 * Purpose
 * =======
 *
 * ATL_hescal  scales a (trapezoidal)  Hermitian  m-by-n matrix A by the
 * real scalar alpha.  The imaginary parts of the diagonal elements of A
 * need not be set on input,  they are assumed to be zero,  and  on exit
 * they are set to zero.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   int                        i, incA, j, mn;
   TYPE                       * a_j;
/* ..
 * .. Executable Statements ..
 *
 */
   if( UPLO == AtlasLower )
   {
      incA = ( ( LDA - M + 1 ) SHIFT ); mn = Mmin( M, N );

      if(      ALPHA == ATL_rzero )
      {
         for( j = 0; j < mn; j++ )
         {
            for( i = j; i < M; i++, A += 2 ) { *A = A[1] = ATL_rzero; }
            A += incA;
            incA += 2;
         }
      }
      else if( ALPHA != ATL_rone )
      {
         for( j = 0; j < mn; j++ )
         {
            *A *= ALPHA; A[1] = ATL_rzero; A += 2;

            for( i = j+1; i < M; i++, A += 2 ) { *A *= ALPHA; A[1] *= ALPHA; }
            A += incA;
            incA += 2;
         }
      }
   }
   else
   {
      incA = ( LDA SHIFT );

      if(      ALPHA == ATL_rzero )
      {
         for( j = 0, mn = M - N; j < N; j++, mn++, A += incA )
         {
            a_j = A;
            for( i = 0; i <= mn; i++, a_j += 2 )
            {
               *a_j = a_j[1] = ATL_rzero;
            }
         }
      }
      else if( ALPHA != ATL_rone )
      {
         for( j = 0, mn = M - N; j < N; j++, mn++, A += incA )
         {
            a_j = A;
            for( i = 0; i < mn; i++, a_j += 2 )
            {
               *a_j   *= ALPHA;
               a_j[1] *= ALPHA;
            }
            *a_j *= ALPHA; a_j[1] = ATL_rzero;
         }
      }
   }
}
