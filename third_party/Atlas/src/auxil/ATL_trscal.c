/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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

void Mjoin( PATL, trscal )
(
   const enum ATLAS_UPLO UPLO,
   ATL_CINT M,
   ATL_CINT N,
   const SCALAR ALPHA,
   TYPE *A,
   ATL_CINT lda
)
{
/*
 * Scales a trapezoidal MxN matrix A by the scalar alpha
 */
#ifndef TCPLX
   register ATL_INT i, j;
   ATL_CINT mn = Mmin(M,N);
   const register TYPE alpha = ALPHA;

   if (alpha == ATL_rone || mn < 1)
      return;
   if( UPLO == AtlasLower )
   {
      if (alpha == ATL_rzero)
      {
         for (j=0; j < mn; j++, A += lda)
            for (i=j; i < M; i++)
               A[i] = alpha;
      }
      else
      {
         for (j=0; j < mn; j++, A += lda)
            for (i=j; i < M; i++)
               A[i] *= alpha;
      }
   }
   else  /* Upper matrix */
   {
      if (alpha == ATL_rzero)
      {
         for (j=0; j < mn; j++, A += lda)
            for (i=0; i <= j; i++)
               A[i] = alpha;
      }
      else
      {
         for (j=0; j < mn; j++, A += lda)
            for (i=0; i <= j; i++)
               A[i] *= alpha;
      }
      if (N > mn)  /* scale rectangular portion */
         Mjoin(PATL,gescal)(M, N-mn, alpha, A, lda);
   }
}
#else
   ATL_CINT M2 = M+M, incA = lda+lda, mn = Mmin(M,N);
   register ATL_INT i, j;
#ifdef TCPLX
   register TYPE ra, ia;
   register const TYPE rb = ALPHA[0], ib = ALPHA[1];
#endif
   if( UPLO == AtlasLower )
   {
      if (ib == ATL_rzero)              /* real scalar */
      {
         if (rb == ATL_rzero)           /* scale by zero */
         {
            for (j=0; j < mn; j++, A += incA)
            {
               for (i=j+j; i < M2; i++)
                  A[i] = rb;
            }
         }
         else if (rb == ATL_rone)       /* no scaling to be done */
            return;
         else                           /* scale by real scalar */
         {
            for (j=0; j < mn; j++, A += incA)
            {
               for (i=j+j; i < M2; i++)
                  A[i] *= rb;
            }
         }
      }
      else /* must apply complex scalar */
      {
         for (j=0; j < mn; j++, A += incA)
         {
            for (i=j+j; i < M2; i += 2)
            {
               ra = A[i];
               ia = A[i+1];
               A[i] = ra*rb - ia*ib;
               A[i+1] = ra*ib + ia*rb;
            }
         }
      }
   }
   else  /* Upper matrix */
   {
      if (ib == ATL_rzero)              /* real scalar */
      {
         if (rb == ATL_rzero)           /* scale by zero */
         {
            for (j=1; j <= mn; j++, A += incA)
            {
               for (i=0; i < j+j; i++)
                  A[i] = ATL_rzero;
            }
         }
         else if (rb == ATL_rone)       /* no scaling to be done */
            return;
         else                           /* scale by real scalar */
         {
            for (j=1; j <= mn; j++, A += incA)
            {
               for (i=0; i < j+j; i++)
                  A[i] *= rb;
            }
         }
      }
      else                              /* scale by complex scalar */
      {
         for (j=1; j <= mn; j++, A += incA)
         {
            for (i=0; i < j+j; i += 2)
            {
               ra = A[i];
               ia = A[i+1];
               A[i] = ra*rb - ia*ib;
               A[i+1] = ra*ib + ia*rb;
            }
         }
      }
/*
 *    Finish off any remaining rectangular portion
 */
      if (N > mn)
         Mjoin(PATL,gescal)(M, N-mn, ALPHA, A, lda);
   }  /* end if over matrix type */
}
#endif
