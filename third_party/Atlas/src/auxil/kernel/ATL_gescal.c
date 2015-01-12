/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009, 1999 R. Clint Whaley
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


void Mjoin(Mjoin(PATL,gescal),BNM)
   (ATL_CINT M, ATL_CINT N, const SCALAR beta0, TYPE *C, ATL_CINT ldc)
/*
 * C <- beta * C
 */
{
#ifdef BETA0
   Mjoin(PATL,gezero)(M, N, C, ldc);
#elif defined (TREAL)

   ATL_CINT incC = (ldc << 1), incC2 = incC - M, incC3 = ldc - M;
   TYPE *C0 = C, *C1 = C0 + ldc;
   TYPE *stM = C0 + M, *stN = C0 + ldc*N;
   register TYPE beta=beta0;

   if ( (((M >> 2) << 2) == M) && (((N >> 1) << 1) == N) )
   {
      do
      {
         do
         {
            #if defined(BETAN1)
               *C0 = -(*C0);
               *C1 = -(*C1);
               C0[1] = -C0[1];
               C1[1] = -C1[1];
               C0[2] = -C0[2];
               C1[2] = -C1[2];
               C0[3] = -C0[3];
               C1[3] = -C1[3];
            #else
               *C0 *= beta;
               *C1 *= beta;
               C0[1] *= beta;
               C1[1] *= beta;
               C0[2] *= beta;
               C1[2] *= beta;
               C0[3] *= beta;
               C1[3] *= beta;
            #endif
            C0 += 4;
            C1 += 4;
         }
         while(C0 != stM);
         stM += incC;
         C0 += incC2;
         C1 += incC2;
      }
      while(C0 != stN);
   }
   else
   {
      do
      {
         #if defined(BETAN1)
            do *C0 = -(*C0); while(++C0 != stM);
         #else
            do *C0 *= beta; while(++C0 != stM);
         #endif
         stM += ldc;
         C0 += incC3;
      }
      while (C0 != stN);
   }

#elif defined(TCPLX)

#if defined(BETAXI0)
   Mjoin(ATL_,Mjoin(UPR,gescal_bX))(M<<1, N, *beta0, C, ldc<<1);
#else
   ATL_CINT incC=(ldc<<2)-(M<<1), n=N>>1;
   const register TYPE rbeta = *beta0, ibeta = beta0[1];
   TYPE *C0=C, *C1 = C + (ldc<<1);
   register TYPE r0, r1, i0, i1, ar0, ai0, ar1, ai1;
   register ATL_INT j, i;

   for (j=n; j; j--, C0 += incC, C1 += incC)
   {
      for (i=M; i; i--, C0 += 2, C1 += 2)
      {
         r0 = *C0;
         r1 = *C1;
         i0 = C0[1];
         i1 = C1[1];

         ar0 = r0 * rbeta;
         ar1 = r1 * rbeta;
         ai0 = i0 * rbeta;
         ai1 = i1 * rbeta;

         i0 *= ibeta;
         r0 *= ibeta;
         i1 *= ibeta;
         r1 *= ibeta;

         ar0 -= i0;
         ai0 += r0;
         ar1 -= i1;
         ai1 += r1;
         *C0 = ar0;
         C0[1] = ai0;
         *C1 = ar1;
         C1[1] = ai1;
      }
   }
   if (N-(n<<1))
   {
      for (i=M; i; i--, C0 += 2)
      {
         r0 = *C0;
         i0 = C0[1];
         ar0 = r0 * rbeta;
         ai0 = i0 * rbeta;
         i0 *= ibeta;
         r0 *= ibeta;
         *C0 = ar0 - i0;
         C0[1] = ai0 + r0;
      }
   }
#endif
#endif
}
