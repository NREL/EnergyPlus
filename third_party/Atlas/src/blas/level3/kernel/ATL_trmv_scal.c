/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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
#include "atlas_kern3.h"

#ifdef TCPLX

#ifdef Upper_

#ifdef UnitDiag_
   #define ATL_trmv Mjoin(PATL,Mjoin(trmv_scalUNU,NM))
#else
   #define ATL_trmv Mjoin(PATL,Mjoin(trmv_scalUNN,NM))
#endif

void ATL_trmv(const int N, const SCALAR alpha, const TYPE *A, const int lda,
              TYPE *X)
{
   int i, j;
   const int lda2=(lda<<1), n=(N>>1)<<1;
   const TYPE zero=0.0;
   TYPE *x;
   const TYPE *Ad=A, *a;
   register TYPE xr, xi, yr0, yi0, yr1, yi1, Ar0, Ai0, Ar1, Ai1;

   for (i=0; i != n; i += 2)
   {
/*      yr0 = *x; yi0 = x[1];  yr1=x[2];  xi1 = x[3]; */
      yr0 = yi0 = yr1 = yi1 = zero;
   /*
    * Handle diagonal part
    */
      x = X;
      xr = *x;
      xi = x[1];
      x += 2;
      #ifdef Unitdiag_
         yr0 += xr;
         yi0 += xi;
      #else
         Ar0 = *Ad;
         Ai0 = Ad[1];
         yr0 += Ar0 * xr;
         yi0 += Ai0 * xr;
         yr0 -= Ai0 * xi;
         yi0 += Ar0 * xi;
      #endif
      Ad += lda2;

      xr = *x;
      xi = x[1];
      x += 2;
      Ar0 = *Ad;
      Ai0 = Ad[1];
      yr0 += Ar0 * xr;
      yi0 += Ai0 * xr;
      #ifdef UnitDiag_
         yr1 += xr;
      #else
         Ar1 = Ad[2];
         Ai1 = Ad[3];
         yr1 += Ar1 * xr;
         yi1 += Ai1 * xr;
      #endif
      a = Ad + lda2;
      Ad = a + 4;

      yr0 -= Ai0 * xi;
      yi0 += Ar0 * xi;
      #ifdef UnitDiag_
         yi1 += xi;
      #else
         yr1 -= Ai1 * xi;
         yi1 += Ar1 * xi;
      #endif

   /*
    * Handle rectangular part
    */
      for (j=i+2; j < N; j++)
      {
         xr = *x;
         xi = x[1];
         Ar0 = *a;
         Ai0 = a[1];
         Ar1 = a[2];
         Ai1 = a[3];
         yr0 += Ar0 * xr;
         yi0 += Ai0 * xr;
         x += 2;
         yr1 += Ar1 * xr;
         yi1 += Ai1 * xr;
         yr0 -= Ai0 * xi;
         yi0 += Ar0 * xi;
         a += lda2;
         yr1 -= Ai1 * xi;
         yi1 += Ar1 * xi;
      }
      #ifdef ALPHAN1
         *X = -yr0;
         X[1] = -yi0;
         X[2] = -yr1;
         X[3] = -yi1;
      #else
         xr = *alpha;
         xi = alpha[1];
         Ar0 = xr * yr0;
         Ai0 = xi * yr0;
         Ar1 = xr * yr1;
         Ai1 = xi * yr1;
         Ar0 -= xi * yi0;
         Ai0 += xr * yi0;
         Ar1 -= xi * yi1;
         Ai1 += xr * yi1;
         *X = Ar0;
         X[1] = Ai0;
         X[2] = Ar1;
         X[3] = Ai1;
      #endif
      X += 4;
   }
   if (N-n)
   {
      if (!n) a = A;
      #ifdef UnitDiag_
         #ifdef ALPHAN1
            *X = -(*X);
            X[1] = -X[1];
         #else
            xr = *X;
            xi = X[1];
            Ar0 = *alpha;
            Ai0 = alpha[1];
            yr0 = xr * Ar0;
            yi0 = xi * Ar0;
            yr0 -= xi * Ai0;
            yi0 += xr * Ai0;
            *X = yr0;
            X[1] = yi0;
         #endif
      #else
         xr = *X;
         xi = X[1];
         Ar0 = *Ad;
         Ai0 = Ad[1];
         yr0 = xr * Ar0;
         yi0 = xi * Ar0;
         yr0 -= xi * Ai0;
         yi0 += xr * Ai0;
         #ifdef ALPHAN1
            *X = -yr0;
            X[1] = -yi0;
         #else
            xr = *alpha;
            xi = alpha[1];
            Ar0 = xr * yr0;
            Ai0 = xi * yr0;
            Ar0 -= xi * yi0;
            Ai0 += xr * yi0;
            *X = Ar0;
            X[1] = Ai0;
         #endif
      #endif
   }
}

#else  /* Lower triangular matrix */

#ifdef UnitDiag_
   #define ATL_trmv Mjoin(PATL,Mjoin(trmv_scalLNU,NM))
#else
   #define ATL_trmv Mjoin(PATL,Mjoin(trmv_scalLNN,NM))
#endif

void ATL_trmv(const int N, const SCALAR alpha, const TYPE *A, const int lda,
              TYPE *X)
{
   int i, j;
   const int n = (N>>1)<<1, lda2=lda<<1;
   #ifdef UnitDiag_
      #define ii i
   #else
      int ii;
   #endif
   const TYPE *a;
   TYPE *y, *x;
   register TYPE yr0, yi0, yr1, yi1, xr, xi;
   register TYPE ar0, ai0, ar1, ai1;

   if (n)
   {
      i = (N - 2)<<1;
      A += i;
      y = X + i;
      for (i=N-2; i >= 0; i -= 2)
      {
         #ifndef UnitDiag_
            ii = i + 1;
         #endif
         a = A;
         x = X;
         yr0 = yi0 = yr1 = yi1 = 0.0;
         for (j=0; j != ii; j++)
         {
            ar0 = *a;
            xr = *x;
            xi = x[1];
            yr0 += ar0 * xr;
            ar1 = a[2];
            yi0 += ar0 * xi;
            ai0 = a[1];
            yr1 += ar1 * xr;
            ai1 = a[3];
            yi1 += ar1 * xi;
            a += lda2;
            yr0 -= ai0 * xi;
            x += 2;
            yi0 += ai0 * xr;
            yr1 -= ai1 * xi;
            yi1 += ai1 * xr;
         }
         xr = *x;
         xi = x[1];
         #ifdef UnitDiag_
            yr0 += xr;
            ar1 = a[2];
            yi0 += xi;
            ai1 = a[3];
            yr1 += ar1 * xr;
            a += lda2;
            yi1 += ar1 * xi;
            yr1 -= ai1 * xi;
            xi = x[3];
            yi1 += ai1 * xr;
            xr = x[2];
            yr1 += xr;
            yi1 += xi;
         #else
            ar1 = a[2];
            yr1 += ar1 * xr;
            ai1 = a[3];
            yi1 += ar1 * xi;
            yr1 -= ai1 * xi;
            yi1 += ai1 * xr;
         #endif
         #ifdef ALPHAN1
            *y   = -yr0;
            y[1] = -yi0;
            y[2] = -yr1;
            y[3] = -yi1;
         #else
            xr = *alpha;
            ar0 = xr * yr0;
            xi = alpha[1];
            ai0 = xr * yi0;
            ar0 -= xi * yi0;
            ai0 += xi * yr0;
            ar1 = xr * yr1;
            ai1 = xr * yi1;
            ar1 -= xi * yi1;
            ai1 += xi * yr1;
            *y = ar0;
            y[1] = ai0;
            y[2] = ar1;
            y[3] = ai1;
         #endif
         A -= 4;
         y -= 4;
      }
   }
   if (N-n)
   {
      xr = *X;
      xi = X[1];
      #ifdef UnitDiag_
         #ifdef ALPHAN1
            *X = -xr;
            X[1] = -xi;
         #endif
      #else
         if (n) A += 2;
         ar0 = *A;
         yr0 = ar0 * xr;
         ai0 = A[1];
         yi0 = ar0 * xi;
         yr0 -= ai0 * xi;
         yi0 += ai0 * xr;
         #ifndef ALPHAN1
            xr = *alpha;
            xi = alpha[1];
            ar0 = xr * yr0;
            ai0 = xr * yi0;
            ar0 -= xi * yi0;
            ai0 += xi * yr0;
            *X = ar0;
            X[1] = ai0;
         #endif
      #endif
   }
}

#endif

#endif
