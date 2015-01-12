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

#ifdef TREAL

#ifdef Upper_

#define ATL_trmv_scal Mjoin(ATL_dtrmv_scalUN,UnitNM)
static void ATL_trmv_scal(const int N, const SCALAR alpha0, const TYPE *A,
                          const int lda, TYPE *X)
/*
 * X <- alpha * A * X
 */
{
   int i, j;
   const int n = (N>>2)<<2;
   const int incAd = (lda+1)<<2;
   register TYPE y0, y1, y2, y3, x0;
   const register TYPE alpha=alpha0;
   const TYPE *Ad=A, *Ac;
   TYPE *x, *stX = X + N;

   for (i=0; i != n; i += 4)
   {
      Ac = Ad;
      Ad += incAd;

      x0 = *X;
      #ifdef UnitDiag_
         y0 = x0;
      #else
         y0 = *Ac * x0;
      #endif
      Ac += lda;

      x0 = X[1];
      #ifdef UnitDiag_
         y1 = x0;
      #else
         y1 = Ac[1] * x0;
      #endif
      y0 += *Ac * x0;
      Ac += lda;

      x0 = X[2];
      #ifdef UnitDiag_
         y2 = x0;
      #else
         y2 = Ac[2] * x0;
      #endif
      y1 += Ac[1] * x0;
      y0 += *Ac * x0;
      Ac += lda;

      x0 = X[3];
      #ifdef UnitDiag_
         y3 = x0;
      #else
         y3 = Ac[3] * x0;
      #endif
      y2 += Ac[2] * x0;
      y1 += Ac[1] * x0;
      y0 += *Ac * x0;
      Ac += lda;

      x = X + 4;
      if (x != stX)
      {
         do
         {
            x0 = *x++;
            y0 += *Ac * x0;
            y1 += Ac[1] * x0;
            y2 += Ac[2] * x0;
            y3 += Ac[3] * x0;
            Ac += lda;
         }
         while (x != stX);
      }
      y0 *= alpha;
      y1 *= alpha;
      y2 *= alpha;
      y3 *= alpha;
      *X = y0;
      X[1] = y1;
      X[2] = y2;
      X[3] = y3;
      X += 4;
   }
   i = N - n;
   switch(N-n)  /* ixi trmv */
   {
   case 1:
      #ifdef UnitDiag_
         *X *= alpha;
      #else
         *X *= alpha * *Ad;
      #endif
      break;
   case 2:
      #ifdef UnitDiag_
         *X = alpha * (*X + Ad[lda] * X[1]);
         X[1] *= alpha;
      #else
         *X = alpha * (*Ad * *X + Ad[lda] * X[1]);
         X[1] = alpha * (Ad[lda+1] * X[1]);
      #endif
      break;
   case 3:
         i = lda << 1;
      #ifdef UnitDiag_
         *X = alpha * (*X + Ad[lda] * X[1] + Ad[i] * X[2]);
         X[1] = alpha * (X[1] + Ad[i+1] * X[2]);
         X[2] *= alpha;
      #else
         *X = alpha * (*Ad * *X + Ad[lda] * X[1] + Ad[i] * X[2]);
         X[1] = alpha * (Ad[lda+1] * X[1] + Ad[i+1] * X[2]);
         X[2] *= alpha * Ad[i+2];
      #endif
      break;
   default:;
   }
}

void Mjoin(Mjoin(PATL,trinvertU),UnitNM)(const int N, TYPE *A, const int lda)
{
   int j;
   register int i;
   const int ione=1;
   #ifdef UnitDiag_
      #define DIAG "U"
      #define Ajj -1.0
   #else
      #define DIAG "N"
      TYPE Ajj;
      const int ldap1 = lda + 1;
      TYPE one=1.0;
   #endif
   TYPE *Ac=A;

   if (N > 0)
   {
      for (j=0; j != N; j++)
      {
         #ifndef UnitDiag_
            Ajj = one / Ac[j];
            Ac[j] = Ajj;
            Ajj = -Ajj;
         #endif
         #ifdef OldDtrmv
            dtrmv_("U", "N", DIAG, &j, A, &lda, Ac, &ione);
            for (i=0; i != j; i++) Ac[i] *= Ajj;
         #else
            ATL_trmv_scal(j, Ajj, A, lda, Ac);
         #endif
         Ac += lda;
      }
   }
}

#else

#define ATL_trmv_scal Mjoin(ATL_dtrmv_scalLN,UnitNM)
static void ATL_trmv_scal(const int N, const SCALAR alpha0, const TYPE *A,
                          const int lda, TYPE *X)
{
   int i;
   const int n = (N>>2)<<2;
   TYPE *x = X, *stX = X+N-4;
   const TYPE *Ac, *a=A;
   const register TYPE alpha=alpha0;
   register TYPE y0, y1, y2, y3, x0;
   if (n)
   {
      A += N-4;
      for (i=0; i != n; i += 4)
      {
         Ac = A;
         y0 = y1 = y2 = y3 = 0.0;
         if (X != stX)
         {
            do
            {
               x0 = *X;
               y0 += *Ac * x0;
               y1 += Ac[1] * x0;
               y2 += Ac[2] * x0;
               y3 += Ac[3] * x0;
               Ac += lda;
            }
            while(++X != stX);
         }
         x0 = *X;
         #ifdef UnitDiag_
            y0 += x0;
         #else
            y0 += *Ac * x0;
         #endif
         y1 += Ac[1] * x0;
         y2 += Ac[2] * x0;
         y3 += Ac[3] * x0;
         x0 = X[1];
         Ac += lda;

         y0 *= alpha;
         #ifdef UnitDiag_
            y1 += x0;
         #else
            y1 += Ac[1] * x0;
         #endif
         y2 += Ac[2] * x0;
         y3 += Ac[3] * x0;
         x0 = X[2];
         Ac += lda;

         y1 *= alpha;
         #ifdef UnitDiag_
            y2 += x0;
         #else
            y2 += Ac[2] * x0;
         #endif
         y3 += Ac[3] * x0;
         Ac += lda;

         y2 *= alpha;
         #ifdef UnitDiag_
            y3 += X[3];
         #else
            y3 += Ac[3] * X[3];
         #endif
         y3 *= alpha;

         *stX = y0;
         stX[1] = y1;
         stX[2] = y2;
         stX[3] = y3;
         stX -= 4;
         X = x;
         A -= 4;
      }
      A += N-n;
   }
   A = a;
   switch(N-n)
   {
   case 1:
      #ifdef UnitDiag_
         *x *= alpha;
      #else
         *x *= alpha * *A;
      #endif
      break;
   case 2:
      #ifdef UnitDiag_
	 x[1] = alpha * (*x * A[1] + x[1]);
         *x *= alpha;
      #else
	 x[1] = alpha * (*x * A[1] + x[1] * A[lda+1]);
         *x *= alpha * *A;
      #endif
      break;
   case 3:
      Ac = A + lda;
      #ifdef UnitDiag_
         x[2] = alpha * (*x * A[2] + x[1] * Ac[2] + x[2]);
	 x[1] = alpha * (*x * A[1] + x[1]);
         *x *= alpha;
      #else
         x[2] = alpha * (*x * A[2] + x[1] * Ac[2] + x[2] * Ac[lda+2]);
	 x[1] = alpha * (*x * A[1] + x[1] * Ac[1]);
         *x *= alpha * *A;
      #endif
      break;
   }
}
#ifndef NoUse
void Mjoin(Mjoin(PATL,trinvertL),UnitNM)(const int N, TYPE *A, const int lda)
{
   int j;
   register int i;
   const int ione=1;
   const int ldap1=lda+1;
   #ifdef UnitDiag_
      #define DIAG "U"
      #define Ajj -1.0
   #else
      #define DIAG "N"
      register TYPE Ajj;
      TYPE one=1.0;
   #endif
   TYPE *Ad = A + (N-1)*ldap1;

   for (j=0; j != N; j++)
   {
      #ifndef UnitDiag_
         Ajj = one / *Ad;
         *Ad = Ajj;
         Ajj = -Ajj;
      #endif
      #ifndef OldDtrmv
         ATL_trmv_scal(j, Ajj, Ad+ldap1, lda, Ad+1);
      #else
         dtrmv_("L", "N", DIAG, &j, Ad+ldap1, &lda, Ad+1, &ione);
         #ifdef UnitDiag_
            for (i=1; i <= j; i++) Ad[i] = -Ad[i];
         #else
            for (i=1; i <= j; i++) Ad[i] *= Ajj;
         #endif
      #endif
      Ad -= ldap1;
   }
}
#endif

#endif

#else /* complex code */

#ifdef Upper_

void Mjoin(Mjoin(PATL,trinvertU),UnitNM)(const int N, TYPE *A, const int lda)
{
   int j;
   register int i;
   const int ione=1, lda2=lda<<1;
   #ifdef UnitDiag_
      #define DIAG "U"
      const TYPE Ajj[2] = {-1.0, 0.0};
   #else
      #define DIAG "N"
      const int ldap1 = lda2+2;
      TYPE Ajj[2], *Ad=A;
   #endif
   TYPE *Ac=A;
   TYPE rtmp, itmp, t0;

   if (N > 0)
   {
      #ifndef UnitDiag_
         Mjoin(PATL,cplxinvert)(N, A, lda+1, A, lda+1);/* invert diag entries */
      #endif
      for (j=0; j != N; j++)
      {
         #ifndef UnitDiag_
            *Ajj = -(*Ad);
            Ajj[1] = -Ad[1];
         #endif
         #if 0
            Mjoin(PRE,trmv_)("U", "N", DIAG, &j, A, &lda, Ac, &ione);
            Mjoin(PRE,scal_)(&j, Ajj, Ac, &ione);
         #else
            #ifdef UnitDiag_
               Mjoin(PATL,trmv_scalUNU_an1)(j, Ajj, A, lda, Ac);
            #else
               Mjoin(PATL,trmv_scalUNN_aX)(j, Ajj, A, lda, Ac);
            #endif
         #endif
         #ifndef UnitDiag_
            Ad += ldap1;
         #endif
         Ac += lda2;
      }
   }
}

#else

void Mjoin(Mjoin(PATL,trinvertL),UnitNM)(const int N, TYPE *A, const int lda)
{
   int j;
   register int i;
   const int ione=1;
   const int ldap1=(lda<<1)+2;
   #ifdef UnitDiag_
      #define DIAG "U"
      const TYPE Ajj[2] = {-1.0, 0.0};
   #else
      #define DIAG "N"
      TYPE Ajj[2];
   #endif
   TYPE *Ad = A + (N-1)*ldap1;

   #ifndef UnitDiag_
      Mjoin(PATL,cplxinvert)(N, A, lda+1, A, lda+1);  /* invert diag entries */
   #endif
   for (j=0; j != N; j++)
   {
      #ifndef UnitDiag_
         *Ajj = -(*Ad);
         Ajj[1] = -Ad[1];
      #endif
      #if 1
         #ifdef UnitDiag_
            Mjoin(PATL,trmv_scalLNU_an1)(j, Ajj, Ad+ldap1, lda, Ad+2);
         #else
            Mjoin(PATL,trmv_scalLNN_aX)(j, Ajj, Ad+ldap1, lda, Ad+2);
         #endif
      #else
         Mjoin(PRE,trmv_)("L", "N", DIAG, &j, Ad+ldap1, &lda, Ad+2, &ione);
         Mjoin(PRE,scal_)(&j, Ajj, Ad+2, &ione);
      #endif
      Ad -= ldap1;
   }
}

#endif

#endif
