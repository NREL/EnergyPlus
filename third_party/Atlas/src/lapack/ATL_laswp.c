/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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
#include "atlas_lapack.h"

void ATL_laswp(const int N, TYPE *A, const int lda0, const int K1,
               const int K2, const int *piv, const int inci)
{
   #ifdef TCPLX
      const int lda = lda0<<1;
   #else
      #define lda lda0
   #endif
   const int n = K2 - K1;
   int nb = N >> 5;
   const int mr = N - (nb<<5);
   const int incA = lda << 5;
   const int *ipiv;
   int i, ip, i1, i2, KeepOn;
   register int h;
   TYPE *a0, *a1;
   #ifdef TCPLX
      register TYPE r0, r1;
   #else
      register TYPE r;
   #endif

   if (K2 < K1) return;
   if (inci < 0)
   {
      piv -= (K2-1) * inci;
      i1 = K2-1;
      i2 = K1;
   }
   else
   {
      piv += K1*inci;
      i1 = K1;
      i2 = K2-1;
   }

   if (nb)
   {
      do
      {
         ipiv = piv;
         i = i1;
         do
         {
            ip = *ipiv; ipiv += inci;
            if (ip != i)
            {
               a0 = A + (i SHIFT);
               a1 = A + (ip SHIFT);
               for (h=32; h; h--)
               {
                  #ifdef TCPLX
                     r0 = *a0;
                     r1 = a0[1];
                     *a0 = *a1;
                     a0[1] = a1[1];
                     *a1 = r0;
                     a1[1] = r1;
                  #else
                     r = *a0;
                     *a0 = *a1;
                     *a1 = r;
                  #endif
                  a0 += lda;
                  a1 += lda;
               }
            }
            if (inci > 0) KeepOn = (++i <= i2);
            else KeepOn = (--i >= i2);
         }
         while(KeepOn);
         A += incA;
      }
      while(--nb);
   }
   if (mr)
   {
      ipiv = piv;
      i = i1;
      do
      {
         ip = *ipiv; ipiv += inci;
         if (ip != i)
         {
            a0 = A + (i SHIFT);
            a1 = A + (ip SHIFT);
            for (h=mr; h; h--)
            {
               #ifdef TCPLX
                  r0 = *a0;
                  r1 = a0[1];
                  *a0 = *a1;
                  a0[1] = a1[1];
                  *a1 = r0;
                  a1[1] = r1;
               #else
                  r = *a0;
                  *a0 = *a1;
                  *a1 = r;
               #endif
               a0 += lda;
               a1 += lda;
            }
         }
         if (inci > 0) KeepOn = (++i <= i2);
         else KeepOn = (--i >= i2);
      }
      while(KeepOn);
   }
}
