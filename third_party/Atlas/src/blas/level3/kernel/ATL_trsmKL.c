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


void Mjoin(Mjoin(Mjoin(Mjoin(Mjoin(PATL,trsmK),SideNM),UploNM),N),UnitNM)
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *B, const int ldb)
/*
 * This source file is multiply compiled to create the following routines:
 * ATL_trsmKLUNU : trsm Side='Left', Uplo='Upper', Trans='N', Unit='Unit'
 * ATL_trsmKLUNN : trsm Side='Left', Uplo='Upper', Trans='N', Unit='NonUnit'
 * ATL_trsmKLLNU : trsm Side='Left', Uplo='Lower', Trans='N', Unit='Unit'
 * ATL_trsmKLLNN : trsm Side='Left', Uplo='Lower', Trans='N', Unit='NonUnit'
 *
 * The transpose cases are handled by a higher level routine that copies A
 */
{
   int inc, m, n = (N>>3)<<3;
   register int k, i, j;
   TYPE *X0=B, *X1=B+ldb, *X2=X1+ldb, *X3=X2+ldb, *X4=X3+ldb;
   TYPE *X5=X4+ldb, *X6=X5+ldb, *X7=X6+ldb;
   register TYPE x0, x1, x2, x3, x4, x5, x6, x7, a0;
   const TYPE *a;
   #ifdef Upper_
      const int ldap1 = lda + 1;
      const TYPE *Ad;
   #endif
   #ifndef UnitDiag_
      const TYPE one=1.0;
      void *vp;
      TYPE *diag;
   #endif
/*
 * If non-unit, precompute inverse of diagonal entries
 */
   #ifndef UnitDiag_
      vp = malloc(ATL_Cachelen+ATL_MulBySize(M));
      ATL_assert(vp);
      diag = ATL_AlignPtr(vp);
      a = A;
      inc = lda + 1;
      for (i=0; i != M; i++)
      {
         diag[i] = one / *a;
         a += inc;
      }
   #endif

   inc = ldb << 3;
   for (j=0; j != n; j += 8)
   {
   #ifdef Upper_
      Ad = A + M*lda+M-1;
      for (i=M-1; i >= 0; i--)
   #else
      for (i=0; i != M; i++)
   #endif
      {
         x0 = X0[i];
         x0 *=  alpha;
         x1 = X1[i];
         x1 *=  alpha;
         x2 = X2[i];
         x2 *=  alpha;
         #ifdef Upper_
            a = Ad;
            Ad -= ldap1;
         #else
            a = A + i;
         #endif
         x3 = X3[i];
         x3 *=  alpha;
         x4 = X4[i];
         x4 *=  alpha;
         x5 = X5[i];
         x5 *=  alpha;
         x6 = X6[i];
         x6 *=  alpha;
         x7 = X7[i];
         x7 *=  alpha;
      #ifdef Upper_
         for (k=i+1; k != M; k++)
      #else
         for (k=0; k != i; k++)
      #endif
         {
            a0 = *a;
            x0 -= a0 * X0[k];
            x1 -= a0 * X1[k];
            a += lda;
            x2 -= a0 * X2[k];
            x3 -= a0 * X3[k];
            x4 -= a0 * X4[k];
            x5 -= a0 * X5[k];
            x6 -= a0 * X6[k];
            x7 -= a0 * X7[k];
         }
         #ifndef UnitDiag_
            a0 = diag[i];
            x0 *= a0;
            x1 *= a0;
            x2 *= a0;
            x3 *= a0;
            x4 *= a0;
            x5 *= a0;
            x6 *= a0;
            x7 *= a0;
         #endif
         X0[i] = x0;
         X1[i] = x1;
         X2[i] = x2;
         X4[i] = x4;
         X3[i] = x3;
         X5[i] = x5;
         X6[i] = x6;
         X7[i] = x7;
      }
      X0 += inc;
      X1 += inc;
      X2 += inc;
      X3 += inc;
      X4 += inc;
      X5 += inc;
      X6 += inc;
      X7 += inc;
   }
   if ( n=N-n )
   {
      inc = lda << 3;
      B = X0;

      for (j=0; j != n; j++)  /* N-loop cleanup */
      {
      #ifdef Upper_
         Ad = A + M*lda+M-1;
         for (i=M-1; i >= 0; i--)
      #else
         for (i=0; i != M; i++)
      #endif
         {
         #ifdef Upper_
            X0 = (TYPE*) Ad;
            Ad -= ldap1;
         #else
            X0 = (TYPE*) A+i;
         #endif
            X1 = X0 + lda;
            X2 = X1+lda;
            X3 = X2+lda;
            X4 = X3+lda;
            X5 = X4+lda;
            X6 = X5+lda;
            X7 = X6+lda;
            x0 = B[i];
            x0 *=  alpha;
            x1 = x2 = x3 = x4 = x5 = x6 = x7 = 0.0;
         #ifdef Upper_
            k = i + 1;
            m = M - k;
            m = (m >> 3)<<3;
            for (m += k; k != m; k += 8)
         #else
            m = (i >> 3)<<3;
            for (k=0; k != m; k += 8)
         #endif
            {
               x0 -= *X0 * B[k];
               X0 += inc;
               x1 -= *X1 * B[k+1];
               X1 += inc;
               x2 -= *X2 * B[k+2];
               X2 += inc;
               x3 -= *X3 * B[k+3];
               X3 += inc;
               x4 -= *X4 * B[k+4];
               X4 += inc;
               x5 -= *X5 * B[k+5];
               X5 += inc;
               x6 -= *X6 * B[k+6];
               X6 += inc;
               x7 -= *X7 * B[k+7];
               X7 += inc;
            }
         #if Upper_
            switch(M-m)
         #else
            switch(i-m)
         #endif
            {
            case 1:
               x0 -= *X0 * B[m];
               break;
            case 2:
               x0 -= *X0 * B[m];
               x1 -= *X1 * B[m+1];
               break;
            case 3:
               x0 -= *X0 * B[m];
               x1 -= *X1 * B[m+1];
               x2 -= *X2 * B[m+2];
               break;
            case 4:
               x0 -= *X0 * B[m];
               x1 -= *X1 * B[m+1];
               x2 -= *X2 * B[m+2];
               x3 -= *X3 * B[m+3];
               break;
            case 5:
               x0 -= *X0 * B[m];
               x1 -= *X1 * B[m+1];
               x2 -= *X2 * B[m+2];
               x3 -= *X3 * B[m+3];
               x4 -= *X4 * B[m+4];
               break;
            case 6:
               x0 -= *X0 * B[m];
               x1 -= *X1 * B[m+1];
               x2 -= *X2 * B[m+2];
               x3 -= *X3 * B[m+3];
               x4 -= *X4 * B[m+4];
               x5 -= *X5 * B[m+5];
               break;
            case 7:
               x0 -= *X0 * B[m];
               x1 -= *X1 * B[m+1];
               x2 -= *X2 * B[m+2];
               x3 -= *X3 * B[m+3];
               x4 -= *X4 * B[m+4];
               x5 -= *X5 * B[m+5];
               x6 -= *X6 * B[m+6];
               break;
            default:;
            }
            x0 += x1;
            x2 += x3;
            x4 += x5;
            x6 += x7;

            x0 += x2;
            x4 += x6;
            x0 += x4;
            #ifndef UnitDiag_
               x0 *= diag[i];
            #endif
            B[i] = x0;
         }
         B += ldb;
      }
   }
   #ifndef UnitDiag_
      free(vp);
   #endif
}
