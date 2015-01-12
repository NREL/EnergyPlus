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

void Mjoin(Mjoin(PATL,trputU),BNM)
   (const int N, const TYPE *v, const SCALAR beta0, TYPE *A, const int lda)
/*
 * Writes the upper part of an NxN workspace v to the upper part of the array A
 */
{
   register int i, j;
   const register SCALAR beta=beta0;

   for (j=0; j != N; j++)
   {
      for (i=0; i <= j; i++)
         #if defined(BETA1)
            A[i] += v[i];
         #elif defined(BETA0)
            A[i] = v[i];
         #else
            A[i] = beta*A[i] + v[i];
         #endif
      v += N;
      A += lda;
   }
}

#else

#ifdef Herm_
   void Mjoin(Mjoin(PATL,heputU),BNM)
#else
   void Mjoin(Mjoin(PATL,trputU),BNM)
#endif
   (const int N, const TYPE *v, const SCALAR beta0, TYPE *A, const int lda)
/*
 * Writes the upper part of an NxN workspace v to the upper part of the array A
 */
{
   int i, j2;
   const int N2=N<<1, lda2=lda<<1;
   #ifdef Herm_
      const TYPE zero=0.0;
   #endif
   #ifdef BETAXI0
      const register TYPE rbeta=*beta0;
   #elif defined(BETAX)
      register TYPE ra, ia;
      const register TYPE rbeta=*beta0, ibeta=beta0[1];
   #endif

   for (j2=0; j2 != N2; j2 += 2)
   {
      #ifdef BETA1
         for (i=0; i != j2; i++) A[i] += v[i];
         A[j2] += v[j2];
         #ifdef Herm_
            A[j2+1] = zero;
         #else
            A[j2+1] += v[j2+1];
         #endif
      #elif defined(BETA0)
         for (i=0; i != j2; i++) A[i] = v[i];
         A[j2] = v[j2];
         #ifdef Herm_
            A[j2+1] = zero;
         #else
            A[j2+1] = v[j2+1];
         #endif
      #elif defined(BETAN1) || defined(BETAXI0)
         for (i=0; i != j2; i++) A[i] = ATL_MulByBETA(A[i]) + v[i];
         A[j2] = ATL_MulByBETA(A[j2]) + v[j2];
         #ifdef Herm_
            A[j2+1] = zero;
         #else
            A[j2+1] = ATL_MulByBETA(A[j2+1]) + v[j2+1];
         #endif
      #else
/*
 *       Last iteration unrolled to get around compiler error on IA64
 */
         for (i=0; i != j2; i += 2)
         {
            ra = A[i];
            ia = A[i+1];
            A[i] = ra*rbeta - ia*ibeta + v[i];
            A[i+1] = ra*ibeta + ia*rbeta + v[i+1];
         }
         #ifdef Herm_
            #error "Hermitian matrix must have real BETA"
         #else
            ra = A[j2];
            ia = A[j2+1];
            A[j2] = ra*rbeta - ia*ibeta + v[j2];
            A[j2+1] = ra*ibeta + ia*rbeta + v[j2+1];
         #endif
      #endif
      v += N2;
      A += lda2;
   }
}

#endif
