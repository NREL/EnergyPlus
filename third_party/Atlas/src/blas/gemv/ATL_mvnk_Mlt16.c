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
#include "atlas_lvl2.h"
#include "atlas_lvl3.h"
#include "atlas_reflevel2.h"


#ifdef TREAL
static void ATL_mvn_Meq1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0;

   y0 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
   }
}
static void ATL_mvn_Meq2
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
   }
}
static void ATL_mvn_Meq3
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
   }
}
static void ATL_mvn_Meq4
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
   }
}
static void ATL_mvn_Meq5
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
   }
}
static void ATL_mvn_Meq6
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
   }
}
static void ATL_mvn_Meq7
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
   }
}
static void ATL_mvn_Meq8
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
   }
}
static void ATL_mvn_Meq9
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
   }
}
static void ATL_mvn_Meq10
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   y9 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      y9 += A[9] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
      Y[9*incY] = y9*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
      Y[9*incY] += y9*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
      Y[9*incY] = y9*alpha + beta*Y[9*incY];
   }
}
static void ATL_mvn_Meq11
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   y9 = ATL_rzero;
   y10 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      y9 += A[9] * x0;
      y10 += A[10] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
      Y[9*incY] = y9*alpha;
      Y[10*incY] = y10*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
      Y[9*incY] += y9*alpha;
      Y[10*incY] += y10*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
      Y[9*incY] = y9*alpha + beta*Y[9*incY];
      Y[10*incY] = y10*alpha + beta*Y[10*incY];
   }
}
static void ATL_mvn_Meq12
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   y9 = ATL_rzero;
   y10 = ATL_rzero;
   y11 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      y9 += A[9] * x0;
      y10 += A[10] * x0;
      y11 += A[11] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
      Y[9*incY] = y9*alpha;
      Y[10*incY] = y10*alpha;
      Y[11*incY] = y11*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
      Y[9*incY] += y9*alpha;
      Y[10*incY] += y10*alpha;
      Y[11*incY] += y11*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
      Y[9*incY] = y9*alpha + beta*Y[9*incY];
      Y[10*incY] = y10*alpha + beta*Y[10*incY];
      Y[11*incY] = y11*alpha + beta*Y[11*incY];
   }
}
static void ATL_mvn_Meq13
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   y9 = ATL_rzero;
   y10 = ATL_rzero;
   y11 = ATL_rzero;
   y12 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      y9 += A[9] * x0;
      y10 += A[10] * x0;
      y11 += A[11] * x0;
      y12 += A[12] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
      Y[9*incY] = y9*alpha;
      Y[10*incY] = y10*alpha;
      Y[11*incY] = y11*alpha;
      Y[12*incY] = y12*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
      Y[9*incY] += y9*alpha;
      Y[10*incY] += y10*alpha;
      Y[11*incY] += y11*alpha;
      Y[12*incY] += y12*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
      Y[9*incY] = y9*alpha + beta*Y[9*incY];
      Y[10*incY] = y10*alpha + beta*Y[10*incY];
      Y[11*incY] = y11*alpha + beta*Y[11*incY];
      Y[12*incY] = y12*alpha + beta*Y[12*incY];
   }
}
static void ATL_mvn_Meq14
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12;
   register TYPE y13;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   y9 = ATL_rzero;
   y10 = ATL_rzero;
   y11 = ATL_rzero;
   y12 = ATL_rzero;
   y13 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      y9 += A[9] * x0;
      y10 += A[10] * x0;
      y11 += A[11] * x0;
      y12 += A[12] * x0;
      y13 += A[13] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
      Y[9*incY] = y9*alpha;
      Y[10*incY] = y10*alpha;
      Y[11*incY] = y11*alpha;
      Y[12*incY] = y12*alpha;
      Y[13*incY] = y13*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
      Y[9*incY] += y9*alpha;
      Y[10*incY] += y10*alpha;
      Y[11*incY] += y11*alpha;
      Y[12*incY] += y12*alpha;
      Y[13*incY] += y13*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
      Y[9*incY] = y9*alpha + beta*Y[9*incY];
      Y[10*incY] = y10*alpha + beta*Y[10*incY];
      Y[11*incY] = y11*alpha + beta*Y[11*incY];
      Y[12*incY] = y12*alpha + beta*Y[12*incY];
      Y[13*incY] = y13*alpha + beta*Y[13*incY];
   }
}
static void ATL_mvn_Meq15
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE x0, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12;
   register TYPE y13, y14;

   y0 = ATL_rzero;
   y1 = ATL_rzero;
   y2 = ATL_rzero;
   y3 = ATL_rzero;
   y4 = ATL_rzero;
   y5 = ATL_rzero;
   y6 = ATL_rzero;
   y7 = ATL_rzero;
   y8 = ATL_rzero;
   y9 = ATL_rzero;
   y10 = ATL_rzero;
   y11 = ATL_rzero;
   y12 = ATL_rzero;
   y13 = ATL_rzero;
   y14 = ATL_rzero;
   for (j=0; j < N; j++)
   {
      x0 = *X;
      y0 += A[0] * x0;
      y1 += A[1] * x0;
      y2 += A[2] * x0;
      y3 += A[3] * x0;
      y4 += A[4] * x0;
      y5 += A[5] * x0;
      y6 += A[6] * x0;
      y7 += A[7] * x0;
      y8 += A[8] * x0;
      y9 += A[9] * x0;
      y10 += A[10] * x0;
      y11 += A[11] * x0;
      y12 += A[12] * x0;
      y13 += A[13] * x0;
      y14 += A[14] * x0;
      A += lda;
      X += incX;
   }
   if (beta == ATL_rzero)
   {
      Y[0*incY] = y0*alpha;
      Y[1*incY] = y1*alpha;
      Y[2*incY] = y2*alpha;
      Y[3*incY] = y3*alpha;
      Y[4*incY] = y4*alpha;
      Y[5*incY] = y5*alpha;
      Y[6*incY] = y6*alpha;
      Y[7*incY] = y7*alpha;
      Y[8*incY] = y8*alpha;
      Y[9*incY] = y9*alpha;
      Y[10*incY] = y10*alpha;
      Y[11*incY] = y11*alpha;
      Y[12*incY] = y12*alpha;
      Y[13*incY] = y13*alpha;
      Y[14*incY] = y14*alpha;
   }
   else if (beta == ATL_rone)
   {
      Y[0*incY] += y0*alpha;
      Y[1*incY] += y1*alpha;
      Y[2*incY] += y2*alpha;
      Y[3*incY] += y3*alpha;
      Y[4*incY] += y4*alpha;
      Y[5*incY] += y5*alpha;
      Y[6*incY] += y6*alpha;
      Y[7*incY] += y7*alpha;
      Y[8*incY] += y8*alpha;
      Y[9*incY] += y9*alpha;
      Y[10*incY] += y10*alpha;
      Y[11*incY] += y11*alpha;
      Y[12*incY] += y12*alpha;
      Y[13*incY] += y13*alpha;
      Y[14*incY] += y14*alpha;
   }
   else
   {
      Y[0*incY] = y0*alpha + beta*Y[0*incY];
      Y[1*incY] = y1*alpha + beta*Y[1*incY];
      Y[2*incY] = y2*alpha + beta*Y[2*incY];
      Y[3*incY] = y3*alpha + beta*Y[3*incY];
      Y[4*incY] = y4*alpha + beta*Y[4*incY];
      Y[5*incY] = y5*alpha + beta*Y[5*incY];
      Y[6*incY] = y6*alpha + beta*Y[6*incY];
      Y[7*incY] = y7*alpha + beta*Y[7*incY];
      Y[8*incY] = y8*alpha + beta*Y[8*incY];
      Y[9*incY] = y9*alpha + beta*Y[9*incY];
      Y[10*incY] = y10*alpha + beta*Y[10*incY];
      Y[11*incY] = y11*alpha + beta*Y[11*incY];
      Y[12*incY] = y12*alpha + beta*Y[12*incY];
      Y[13*incY] = y13*alpha + beta*Y[13*incY];
      Y[14*incY] = y14*alpha + beta*Y[14*incY];
   }
}
typedef void (*ATL_MVFUNC)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
#endif

void Mjoin(PATL,mvnk_Mlt16)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
/*
 * y = alpha*A*x + beta*y
 */
{
#ifdef TREAL
   const static ATL_MVFUNC mvfunc[15] = {ATL_mvn_Meq1,
                                         ATL_mvn_Meq2,
                                         ATL_mvn_Meq3,
                                         ATL_mvn_Meq4,
                                         ATL_mvn_Meq5,
                                         ATL_mvn_Meq6,
                                         ATL_mvn_Meq7,
                                         ATL_mvn_Meq8,
                                         ATL_mvn_Meq9,
                                         ATL_mvn_Meq10,
                                         ATL_mvn_Meq11,
                                         ATL_mvn_Meq12,
                                         ATL_mvn_Meq13,
                                         ATL_mvn_Meq14,
                                         ATL_mvn_Meq15
                                         };

   if ( M < 1 || N < 1 || (SCALAR_IS_ZERO(alpha) && SCALAR_IS_ONE(beta)) )
      return;
/*
 * Base max unrolling we use on how many regs we think we have
 */
   #ifdef ATL_GAS_x8664
   if (M > 14)
   #elif defined(ATL_GAS_x8632)
   if (M > 6)
   #else
   if (M > 15)
   #endif
   {
      Mjoin(PATL,mvnk_smallN)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      return;
   }
   mvfunc[M-1](M, N, alpha, A, lda, X, incX, beta, Y, incY);
#else
   #ifndef TUNING
   if (M <= 8)
      Mjoin(PATL,refgemv)(AtlasNoTrans, M, N, alpha, A, lda, X, incX,
                          beta, Y, incY);
   else
   #endif
      Mjoin(PATL,mvnk_smallN)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
#endif
}
