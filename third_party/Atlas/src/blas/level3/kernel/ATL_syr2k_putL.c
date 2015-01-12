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

#ifdef Herm_
   #define MPi -
#else
   #define MPi +
#endif
#ifdef TREAL

void Mjoin(Mjoin(PATL,syr2k_putL),BNM)
   (const int N, const TYPE *D, const SCALAR beta0, TYPE *A, const int lda)

/*
 * Takes D with property (D + D') = (D + D')', and writes it to
 * lower part of symmetric matrix A
 */
{
   register int i, j;
   const TYPE *Dc=D, *Dr;
   TYPE *Ac=A;
   const register SCALAR beta=beta0;

   for (j=0; j != N; j++)
   {
      for (Dr=Dc+j, i=j; i != N; i++, Dr += N)
         #if defined(BETA1)
            Ac[i] += Dc[i] + *Dr;
         #elif defined (BETA0)
            Ac[i] =  Dc[i] + *Dr;
         #else
            Ac[i] = Ac[i]*beta + Dc[i] + *Dr;
         #endif
      Ac += lda;
      Dc += N;
   }
}

#else

/*
 * Workaround for icc errors on IA64Itan2
 */
#ifdef ATL_IntelIccBugs
#pragma optimize("", off)
#endif
#ifdef Herm_
   void Mjoin(Mjoin(PATL,her2k_putL),BNM)
#else
   void Mjoin(Mjoin(PATL,syr2k_putL),BNM)
#endif
   (const int N, const TYPE *D, const SCALAR beta0, TYPE *A, const int lda)

/*
 * Takes D with property (D + D') = (D + D')', and writes it to
 * lower part of symmetric matrix A
 */
{
   register int i, j2;
   const int N2=N<<1, lda2=lda<<1;
   #define ldD2 N2
   #ifdef Herm_
      const TYPE zero=0.0;
   #endif
   const TYPE *Dc=D, *Dr;
   #ifdef BETAXI0
      const register TYPE rbeta=*beta0;
   #elif defined(BETAX)
      register TYPE ra, ia;
      const register TYPE rbeta=*beta0, ibeta=beta0[1];
   #endif

   for (j2=0; j2 != N2; j2 += 2)
   {
      Dr = Dc + j2 + ldD2;
      #ifdef BETA1
         A[j2] += Dc[j2] + Dc[j2];
         #ifdef Herm_
            A[j2+1] = zero;
         #else
            A[j2+1] += Dc[j2+1] + Dc[j2+1];
         #endif
         if (N2 != j2)
         {
            for (i=j2+2; i != N2; i += 2, Dr += ldD2)
            {
               A[i] += Dc[i] + *Dr;
               A[i+1] += Dc[i+1] MPi Dr[1];
            }
         }
      #elif defined(BETA0)
         A[j2] = Dc[j2] + Dc[j2];
         #ifdef Herm_
            A[j2+1] = zero;
         #else
            A[j2+1] = Dc[j2+1] + Dc[j2+1];
         #endif
         if (N2 != j2)
         {
            for (i=j2+2; i != N2; i += 2, Dr += ldD2)
            {
               A[i] = Dc[i] + *Dr;
               A[i+1] = Dc[i+1] MPi Dr[1];
            }
         }
      #elif defined(BETAN1) || defined(BETAXI0)
         A[j2] = ATL_MulByBETA(A[j2]) + Dc[j2] + Dc[j2];
         #ifdef Herm_
            A[j2+1] = zero;
         #else
            A[j2+1] = ATL_MulByBETA(A[j2+1]) + Dc[j2+1] + Dc[j2+1];
         #endif
         if (N2 != j2)
         {
            for (i=j2+2; i != N2; i += 2, Dr += ldD2)
            {
               A[i] = ATL_MulByBETA(A[i]) + Dc[i] + *Dr;
               A[i+1] = ATL_MulByBETA(A[i+1]) + Dc[i+1] MPi Dr[1];
            }
         }
      #else
         ra = A[j2];
         ia = A[j2+1];
         A[j2] = ra*rbeta - ia*ibeta + Dc[j2] + Dc[j2];
         A[j2+1] = rbeta*ia + ibeta*ra + Dc[j2+1] + Dc[j2+1];
         if (j2 != N2)
         {
            for (i=j2+2; i != N2; i += 2, Dr += ldD2)
            {
               ra = A[i];
               ia = A[i+1];
               A[i] = ra*rbeta - ia*ibeta + Dc[i] + *Dr;
               A[i+1] = rbeta*ia + ibeta*ra + Dc[i+1] + Dr[1];
            }
         }
      #endif
      A += lda2;
      Dc += ldD2;
   }
}

#endif
#undef MPi
