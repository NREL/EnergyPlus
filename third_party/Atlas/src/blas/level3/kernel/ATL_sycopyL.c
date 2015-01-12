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

Mjoin(void Mjoin(PATL,sycopyL),NM)
   (const int N, const SCALAR alpha0, const TYPE *A, const int lda, TYPE *C)
/*
 * Copies a symmetric matrix stored in Lower format to a dense matrix
 */
{
   int i, j;
   const register TYPE alpha=alpha0;
   const TYPE *Ar, *Ac = A;
   TYPE *rC, *cC=C;

   if (N > 1)
   {
      for (j=0; j != N; j++)
      {
         Ar = A + j;
         for (i=0; i <= j; i++, Ar += lda) C[i] = ATL_MulByALPHA(*Ar);
         for (i=j+1; i < N; i++) C[i] = ATL_MulByALPHA(Ac[i]);
         C += N;
         Ac += lda;
      }
   }
   else if (N == 1) *C = ATL_MulByALPHA(*A);
}

#else

#ifdef Herm_
   void Mjoin(PATL,hecopyL)
#else
   void Mjoin(PATL,sycopyL)
#endif
   (const int N, const TYPE *A, const int lda, TYPE *C)
{
   int j2, i;
   const int N2 = N<<1, lda2=lda<<1;
   #define ldc2 N2
   const TYPE *a;
   #ifdef Herm_
      const TYPE zero=0.0;
   #endif
   TYPE *c;

   for (j2=0; j2 != N2; j2 += 2)
   {
      a = A + j2;
      for (i=0; i != j2; i += 2)
      {
         C[i] = *a;
         #ifdef Herm_
            C[i+1] = -a[1];
         #else
            C[i+1] = a[1];
         #endif
         a += lda2;
      }
      C[j2] = *a;
      #ifdef Herm_
         C[j2+1] = zero;
      #else
         C[j2+1] = a[1];
      #endif
      if (j2 != N2)
      {
         for (i=j2+2; i != N2; i += 2)
         {
            a += 2;
            C[i] = *a;
            C[i+1] = a[1];
         }
      }
      C += ldc2;
   }
}

#endif
