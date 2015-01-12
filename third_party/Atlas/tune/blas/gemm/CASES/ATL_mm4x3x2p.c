/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2001 R. Clint Whaley
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

#if !defined(NB) || !defined(MB) || !defined(KB)
   #error "MB, NB, and KB must be compile-time constants!"
#endif
#if NB != MB || NB != KB
   #error "MB = NB = KB required"
#elif (MB/4)*4 != MB
   #error "MB must be multiple of 4"
#elif (NB/3)*3 != NB
   #error "NB must be multiple of 3"
#elif (KB/2)*2 != KB
   #error "NB must be multiple of 2"
#endif

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, muladd=0, lat=4 lda=ldb=MB=KB=NB, ldc=0,
 * mu=4, nu=3, ku=2, and register prefetch
 *
 */
{
   const TYPE *stM = A + NBNB;
   const TYPE *stN = B + NBNB;
   #define incAm NB3
   #define incBn NB3
   const int incAn = -NBNB, incBm = -NB;
   const int incCn = (3*ldc - NB)SHIFT;
   TYPE *pC0=C, *pC1=pC0+(ldc SHIFT), *pC2=pC1+(ldc SHIFT);
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   register int k;
   register TYPE rA0, rA1, rA2, rA3, ra0, ra1, ra2, ra3;
   register TYPE rB0, rB1, rB2, rb0, rb1, rb2;
   register TYPE m0, m1, m2, m3;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC0_1, rC1_1, rC2_1, rC3_1,
                   rC0_2, rC1_2, rC2_2, rC3_2;

   do /* N-loop */
   {
      do /* M-loop */
      {
         #ifdef BETA0
            rC0_0 = rC1_0 = rC2_0 = rC3_0 =
            rC0_1 = rC1_1 = rC2_1 = rC3_1 =
            rC0_2 = rC1_2 = rC2_2 = rC3_2 = ATL_rzero;
         #else
            #ifdef TREAL
               rC0_0 = *pC0; rC1_0 = pC0[1]; rC2_0 = pC0[2]; rC3_0 = pC0[3];
               rC0_1 = *pC1; rC1_1 = pC1[1]; rC2_1 = pC1[2]; rC3_1 = pC1[3];
               rC0_2 = *pC2; rC1_2 = pC2[1]; rC2_2 = pC2[2]; rC3_2 = pC2[3];
            #else
               rC0_0 = *pC0; rC1_0 = pC0[2]; rC2_0 = pC0[4]; rC3_0 = pC0[6];
               rC0_1 = *pC1; rC1_1 = pC1[2]; rC2_1 = pC1[4]; rC3_1 = pC1[6];
               rC0_2 = *pC2; rC1_2 = pC2[2]; rC2_2 = pC2[4]; rC3_2 = pC2[6];
            #endif
            #ifdef BETAX
               ra3 = beta;
               rC0_0 *= ra3; rC1_0 *= ra3; rC2_0 *= ra3; rC3_0 *= ra3;
               rC0_1 *= ra3; rC1_1 *= ra3; rC2_1 *= ra3; rC3_1 *= ra3;
               rC0_2 *= ra3; rC1_2 *= ra3; rC2_2 *= ra3; rC3_2 *= ra3;
            #endif
         #endif
/*
 *       Start pipeline
 */
         rA0 = *pA0; rB0 = *pB0;
         rA1 = pA0[NB]; rA2 = pA0[NB2]; rA3 = pA0[NB3];
         rB1 = pB0[NB]; rB2 = pB0[NB2];

         m0 = rA0 * rB0; ra0 = pA0[1];
         m1 = rA1 * rB0;
         m2 = rA2 * rB0; rb0 = pB0[1];
         m3 = rA3 * rB0;

         for (k=NB-2; k; k -= 2) /* easy loop to unroll */
         {
            rC0_0 += m0; m0 = rA0 * rB1; ra1 = pA0[NB+1];
            rC1_0 += m1; m1 = rA1 * rB1;
            rC2_0 += m2; m2 = rA2 * rB1; ra2 = pA0[NB2+1];
            rC3_0 += m3; m3 = rA3 * rB1;
            rC0_1 += m0; m0 = rA0 * rB2; ra3 = pA0[NB3+1];
            rC1_1 += m1; m1 = rA1 * rB2;
            rC2_1 += m2; m2 = rA2 * rB2; rb1 = pB0[NB+1];
            rC3_1 += m3; m3 = rA3 * rB2;

            rC0_2 += m0; m0 = ra0 * rb0; rb2 = pB0[NB2+1];
            rC1_2 += m1; m1 = ra1 * rb0;
            rC2_2 += m2; m2 = ra2 * rb0; rA0 = pA0[2];
            rC3_2 += m3; m3 = ra3 * rb0; rB0 = pB0[2];
            rC0_0 += m0; m0 = ra0 * rb1;
            rC1_0 += m1; m1 = ra1 * rb1; rA1 = pA0[NB+2];
            rC2_0 += m2; m2 = ra2 * rb1;
            rC3_0 += m3; m3 = ra3 * rb1; rA2 = pA0[NB2+2];
            rC0_1 += m0; m0 = ra0 * rb2;
            rC1_1 += m1; m1 = ra1 * rb2; rA3 = pA0[NB3+2];
            rC2_1 += m2; m2 = ra2 * rb2; pA0 += 2;
            rC3_1 += m3; m3 = ra3 * rb2; rB1 = pB0[NB+2];

            rC0_2 += m0; m0 = rA0 * rB0; rB2 = pB0[NB2+2]; pB0 += 2;
            rC1_2 += m1; m1 = rA1 * rB0;
            rC2_2 += m2; m2 = rA2 * rB0; ra0 = pA0[1];
            rC3_2 += m3; m3 = rA3 * rB0; rb0 = pB0[1];
         }
         rC0_0 += m0; m0 = rA0 * rB1; ra1 = pA0[NB+1];
         rC1_0 += m1; m1 = rA1 * rB1;
         rC2_0 += m2; m2 = rA2 * rB1; ra2 = pA0[NB2+1];
         rC3_0 += m3; m3 = rA3 * rB1;
         rC0_1 += m0; m0 = rA0 * rB2; ra3 = pA0[NB3+1];
         rC1_1 += m1; m1 = rA1 * rB2;
         rC2_1 += m2; m2 = rA2 * rB2; rb1 = pB0[NB+1];
         rC3_1 += m3; m3 = rA3 * rB2;

         rC0_2 += m0; m0 = ra0 * rb0; rb2 = pB0[NB2+1];
         rC1_2 += m1; m1 = ra1 * rb0;
         rC2_2 += m2; m2 = ra2 * rb0;
         rC3_2 += m3; m3 = ra3 * rb0;
         rC0_0 += m0; m0 = ra0 * rb1;
         rC1_0 += m1; m1 = ra1 * rb1;
         rC2_0 += m2; m2 = ra2 * rb1;
         rC3_0 += m3; m3 = ra3 * rb1;
         rC0_1 += m0; m0 = ra0 * rb2;
         rC1_1 += m1; m1 = ra1 * rb2;
         rC2_1 += m2; m2 = ra2 * rb2;
         rC3_1 += m3; m3 = ra3 * rb2;

         rC0_2 += m0;
         rC1_2 += m1;
         rC2_2 += m2;
         rC3_2 += m3;

         *pC0 = rC0_0;   pA0 += incAm + 2;
         #ifdef TREAL
            pC0[1] = rC1_0; pC0[2] = rC2_0; pC0[3] = rC3_0;
         #else
            pC0[2] = rC1_0; pC0[4] = rC2_0; pC0[6] = rC3_0;
         #endif
         pB0 += incBm + 2;
         *pC1 = rC0_1;
         #ifdef TREAL
            pC1[1] = rC1_1;
            pC1[2] = rC2_1; pC0 += 4;
            pC1[3] = rC3_1;
         #else
            pC1[2] = rC1_1;
            pC1[4] = rC2_1; pC0 += 8;
            pC1[6] = rC3_1;
         #endif
         *pC2 = rC0_2;
         #ifdef TREAL
            pC2[1] = rC1_2; pC1 += 4;
            pC2[2] = rC2_2;
            pC2[3] = rC3_2; pC2 += 4;
         #else
            pC2[2] = rC1_2; pC1 += 8;
            pC2[4] = rC2_2;
            pC2[6] = rC3_2; pC2 += 8;
         #endif
      }
      while(pA0 != stM);
      pC0 += incCn;
      pC1 += incCn;
      pC2 += incCn;
      pA0 += incAn;
      pB0 += incBn;
   }
   while(pB0 != stN);
}
#undef incAm
#undef incBn
