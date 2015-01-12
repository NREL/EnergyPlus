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
static void ATL_mvt_Meq1_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0;

   x0 = alpha * X[0*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq1_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0;

   x0 = alpha * X[0*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq1_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0;

   x0 = alpha * X[0*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq2_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq2_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq2_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq3_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq3_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq3_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq4_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq4_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq4_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq5_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq5_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq5_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq6_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq6_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq6_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq7_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq7_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq7_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq8_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq8_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq8_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq9_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq9_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq9_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq10_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq10_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq10_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq11_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq11_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq11_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq12_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq12_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq12_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq13_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq13_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq13_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq14_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   x13 = alpha * X[13*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           + A[13] * x13
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq14_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   x13 = alpha * X[13*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           + A[13] * x13
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq14_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   x13 = alpha * X[13*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           + A[13] * x13
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq15_bX
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13, x14;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   x13 = alpha * X[13*incX];
   x14 = alpha * X[14*incX];
   for (j=0; j < N; j++)
   {
      *Y = beta * *Y + *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           + A[13] * x13
           + A[14] * x14
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq15_b1
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13, x14;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   x13 = alpha * X[13*incX];
   x14 = alpha * X[14*incX];
   for (j=0; j < N; j++)
   {
      *Y += *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           + A[13] * x13
           + A[14] * x14
           ;
      A += lda;
      Y += incY;
   }
}
static void ATL_mvt_Meq15_b0
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13, x14;

   x0 = alpha * X[0*incX];
   x1 = alpha * X[1*incX];
   x2 = alpha * X[2*incX];
   x3 = alpha * X[3*incX];
   x4 = alpha * X[4*incX];
   x5 = alpha * X[5*incX];
   x6 = alpha * X[6*incX];
   x7 = alpha * X[7*incX];
   x8 = alpha * X[8*incX];
   x9 = alpha * X[9*incX];
   x10 = alpha * X[10*incX];
   x11 = alpha * X[11*incX];
   x12 = alpha * X[12*incX];
   x13 = alpha * X[13*incX];
   x14 = alpha * X[14*incX];
   for (j=0; j < N; j++)
   {
      *Y = *A * x0
           + A[1] * x1
           + A[2] * x2
           + A[3] * x3
           + A[4] * x4
           + A[5] * x5
           + A[6] * x6
           + A[7] * x7
           + A[8] * x8
           + A[9] * x9
           + A[10] * x10
           + A[11] * x11
           + A[12] * x12
           + A[13] * x13
           + A[14] * x14
           ;
      A += lda;
      Y += incY;
   }
}

typedef void (*ATL_MVFUNC)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY);
#endif

void Mjoin(PATL,mvtk_Mlt16)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
/*
 * y = alpha*A*x + beta*y
 * A is MxN, so X is of length N and Y is of length M.
 *
 * For now, just call axpy-based implementation.  Need to fix using unrollings
 * ASAP.
 * A is MxN, so X is of length M and Y is of length N (A stores transposed mat)
 *
 * ATLAS's normal MVT kernels are optimized for long-M, and loop over M
 * in the innermost loop.  To avoid this killing us on short, wide matrices,
 * have special case code for M < 16 (which allows all kernels to assume
 * M >= 16).
 *
 * NOTE: for right now, just call dot-prod implementation in order to get
 *       things working.  This is terrible for perf, so need to fix using
 *       unrolling as in GER ASAP.
 */
{
#ifdef TREAL
   const static ATL_MVFUNC mvfunc_bX[15] = {ATL_mvt_Meq1_bX,
                                                ATL_mvt_Meq2_bX,
                                                ATL_mvt_Meq3_bX,
                                                ATL_mvt_Meq4_bX,
                                                ATL_mvt_Meq5_bX,
                                                ATL_mvt_Meq6_bX,
                                                ATL_mvt_Meq7_bX,
                                                ATL_mvt_Meq8_bX,
                                                ATL_mvt_Meq9_bX,
                                                ATL_mvt_Meq10_bX,
                                                ATL_mvt_Meq11_bX,
                                                ATL_mvt_Meq12_bX,
                                                ATL_mvt_Meq13_bX,
                                                ATL_mvt_Meq14_bX,
                                                ATL_mvt_Meq15_bX
                                               };
   const static ATL_MVFUNC mvfunc_b1[15] = {ATL_mvt_Meq1_b1,
                                                ATL_mvt_Meq2_b1,
                                                ATL_mvt_Meq3_b1,
                                                ATL_mvt_Meq4_b1,
                                                ATL_mvt_Meq5_b1,
                                                ATL_mvt_Meq6_b1,
                                                ATL_mvt_Meq7_b1,
                                                ATL_mvt_Meq8_b1,
                                                ATL_mvt_Meq9_b1,
                                                ATL_mvt_Meq10_b1,
                                                ATL_mvt_Meq11_b1,
                                                ATL_mvt_Meq12_b1,
                                                ATL_mvt_Meq13_b1,
                                                ATL_mvt_Meq14_b1,
                                                ATL_mvt_Meq15_b1
                                               };
   const static ATL_MVFUNC mvfunc_b0[15] = {ATL_mvt_Meq1_b0,
                                                ATL_mvt_Meq2_b0,
                                                ATL_mvt_Meq3_b0,
                                                ATL_mvt_Meq4_b0,
                                                ATL_mvt_Meq5_b0,
                                                ATL_mvt_Meq6_b0,
                                                ATL_mvt_Meq7_b0,
                                                ATL_mvt_Meq8_b0,
                                                ATL_mvt_Meq9_b0,
                                                ATL_mvt_Meq10_b0,
                                                ATL_mvt_Meq11_b0,
                                                ATL_mvt_Meq12_b0,
                                                ATL_mvt_Meq13_b0,
                                                ATL_mvt_Meq14_b0,
                                                ATL_mvt_Meq15_b0
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
      Mjoin(PATL,mvtk_smallN)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      return;
   }
   if (beta == ATL_rone)
      mvfunc_b1[M-1](M, N, alpha, A, lda, X, incX, beta, Y, incY);
   else if (beta == ATL_rzero)
      mvfunc_b0[M-1](M, N, alpha, A, lda, X, incX, beta, Y, incY);
   else
      mvfunc_bX[M-1](M, N, alpha, A, lda, X, incX, beta, Y, incY);
#else
   #ifndef TUNING
   if (M <= 8)
      Mjoin(PATL,refgemv)(AtlasTrans, N, M, alpha, A, lda, X, incX,
                          beta, Y, incY);
   else
   #endif
      Mjoin(PATL,mvtk_smallN)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
#endif
}
