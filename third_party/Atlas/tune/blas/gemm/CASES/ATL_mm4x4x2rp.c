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

#if (MB/4)*4 != MB
   #error "MB must be multiple of 4!!"
#endif
#if (NB/4)*4 != NB
   #error "NB must be multiple of 4!!"
#endif
#ifndef KB
   #error "KB must be a compile time constant!!"
#elif  (KB/2)*2 != KB || KB == 0
   #error "KB must be multiple of 2!!"
#endif

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=40, NB=40, KB=40,
 * lda=40, ldb=40, ldc=0, mu=4, nu=4, ku=1
 */
{
   const TYPE *stM = A + KB*M;
   const TYPE *stN = B + KB*N;
   #define incBn KB4
   const int incAm = KB3 + 2, incAn = -KB*M;
   const int incBm = 2-KB;
   const int incCn = ((ldc<<2) - M)SHIFT;
   TYPE *pC0=C, *pC1=pC0+(ldc SHIFT), *pC2=pC1+(ldc SHIFT),
        *pC3=pC2+(ldc SHIFT);
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   #ifdef BETAX
      TYPE *bp = (TYPE *) &beta;
   #endif
   const int startK = (KB>>1)-1;
   register int k;
   register TYPE rA0, rA1, rA2, rA3, ra0, ra1, ra2, ra3;
   register TYPE rB0, rB1, rB2, rB3;
   register TYPE m0, m1, m2, m3;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC0_1, rC1_1, rC2_1, rC3_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC0_3, rC1_3, rC2_3, rC3_3;
   do /* N-loop */
   {
      do /* M-loop */
      {
         #ifdef BETA0
            rC0_0 = rC1_0 = rC2_0 = rC3_0 =
            rC0_1 = rC1_1 = rC2_1 = rC3_1 =
            rC0_2 = rC1_2 = rC2_2 = rC3_2 =
            rC0_3 = rC1_3 = rC2_3 = rC3_3 = ATL_rzero;
         #else
            rC0_0 = *pC0; rC0_1 = *pC1; rC0_2 = *pC2; rC0_3 = *pC3;
            #ifdef TREAL
               rC1_0 = pC0[1]; rC1_1 = pC1[1]; rC1_2 = pC2[1]; rC1_3 = pC3[1];
               rC2_0 = pC0[2]; rC2_1 = pC1[2]; rC2_2 = pC2[2]; rC2_3 = pC3[2];
               rC3_0 = pC0[3]; rC3_1 = pC1[3]; rC3_2 = pC2[3]; rC3_3 = pC3[3];
            #else
               rC1_0 = pC0[2]; rC1_1 = pC1[2]; rC1_2 = pC2[2]; rC1_3 = pC3[2];
               rC2_0 = pC0[4]; rC2_1 = pC1[4]; rC2_2 = pC2[4]; rC2_3 = pC3[4];
               rC3_0 = pC0[6]; rC3_1 = pC1[6]; rC3_2 = pC2[6]; rC3_3 = pC3[6];
            #endif
            #ifdef BETAX
/*
 *             Use *bp instead of beta to prevent compiler from putting beta in
 *             a register, thus starving the inner loop, which needs all 32
 */
               ra3 = *bp;
               rC0_0 *= ra3; rC1_0 *= ra3; rC2_0 *= ra3; rC3_0 *= ra3;
               rC0_1 *= ra3; rC1_1 *= ra3; rC2_1 *= ra3; rC3_1 *= ra3;
               rC0_2 *= ra3; rC1_2 *= ra3; rC2_2 *= ra3; rC3_2 *= ra3;
               rC0_3 *= ra3; rC1_3 *= ra3; rC2_3 *= ra3; rC3_3 *= ra3;
            #endif
         #endif
/*
 *       Start pipeline
 */
         rA0 = *pA0; rB0 = *pB0;
         rA1 = pA0[KB]; rA2 = pA0[KB2]; rA3 = pA0[KB3];
         rB1 = pB0[KB]; rB2 = pB0[KB2]; rB3 = pB0[KB3];
         ra0 = pA0[1]; ra1 = pA0[KB+1]; ra2 = pA0[KB2+1]; ra3 = pA0[KB3+1];
         m0 = rA0 * rB0;
         m1 = rA1 * rB0;
         m2 = rA2 * rB0;
         m3 = rA3 * rB0; rB0 = pB0[1];

         for (k=startK; k; k--) /* easy loop to unroll */
         {
            rC0_0 += m0; m0 = rA0 * rB1;
            rC1_0 += m1; m1 = rA1 * rB1;
            rC2_0 += m2; m2 = rA2 * rB1;
            rC3_0 += m3; m3 = rA3 * rB1; rB1 = pB0[KB+1];
            rC0_1 += m0; m0 = rA0 * rB2;
            rC1_1 += m1; m1 = rA1 * rB2;
            rC2_1 += m2; m2 = rA2 * rB2;
            rC3_1 += m3; m3 = rA3 * rB2; rB2 = pB0[KB2+1];
            rC0_2 += m0; m0 = rA0 * rB3; rA0 = pA0[    2];
            rC1_2 += m1; m1 = rA1 * rB3; rA1 = pA0[KB +2];
            rC2_2 += m2; m2 = rA2 * rB3; rA2 = pA0[KB2+2];
            rC3_2 += m3; m3 = rA3 * rB3; rB3 = pB0[KB3+1];
            rC0_3 += m0; m0 = ra0 * rB0; rA3 = pA0[KB3+2];
            rC1_3 += m1; m1 = ra1 * rB0;
            rC2_3 += m2; m2 = ra2 * rB0;
            rC3_3 += m3; m3 = ra3 * rB0; rB0 = pB0[2];

            rC0_0 += m0; m0 = ra0 * rB1;
            rC1_0 += m1; m1 = ra1 * rB1;
            rC2_0 += m2; m2 = ra2 * rB1;
            rC3_0 += m3; m3 = ra3 * rB1; rB1 = pB0[KB+2];
            rC0_1 += m0; m0 = ra0 * rB2;
            rC1_1 += m1; m1 = ra1 * rB2;
            rC2_1 += m2; m2 = ra2 * rB2;
            rC3_1 += m3; m3 = ra3 * rB2; rB2 = pB0[KB2+2];
            rC0_2 += m0; m0 = ra0 * rB3; ra0 = pA0[    3];
            rC1_2 += m1; m1 = ra1 * rB3; ra1 = pA0[KB +3];
            rC2_2 += m2; m2 = ra2 * rB3; ra2 = pA0[KB2+3];
            rC3_2 += m3; m3 = ra3 * rB3; rB3 = pB0[KB3+2];
            rC0_3 += m0; m0 = rA0 * rB0; ra3 = pA0[KB3+3]; pA0 += 2;
            rC1_3 += m1; m1 = rA1 * rB0;
            rC2_3 += m2; m2 = rA2 * rB0;
            rC3_3 += m3; m3 = rA3 * rB0; rB0 = pB0[3]; pB0 += 2;
         }
/*
 *       Drain pipe on last iteration of K-loop
 */
         rC0_0 += m0; m0 = rA0 * rB1;
         rC1_0 += m1; m1 = rA1 * rB1;
         rC2_0 += m2; m2 = rA2 * rB1;
         rC3_0 += m3; m3 = rA3 * rB1; rB1 = pB0[KB+1];
         rC0_1 += m0; m0 = rA0 * rB2;
         rC1_1 += m1; m1 = rA1 * rB2;
         rC2_1 += m2; m2 = rA2 * rB2;
         rC3_1 += m3; m3 = rA3 * rB2; rB2 = pB0[KB2+1];
         rC0_2 += m0; m0 = rA0 * rB3;
         rC1_2 += m1; m1 = rA1 * rB3;
         rC2_2 += m2; m2 = rA2 * rB3;
         rC3_2 += m3; m3 = rA3 * rB3; rB3 = pB0[KB3+1];
         rC0_3 += m0; m0 = ra0 * rB0;
         rC1_3 += m1; m1 = ra1 * rB0;
         rC2_3 += m2; m2 = ra2 * rB0;
         rC3_3 += m3; m3 = ra3 * rB0;

         rC0_0 += m0; m0 = ra0 * rB1;
         rC1_0 += m1; m1 = ra1 * rB1;
         rC2_0 += m2; m2 = ra2 * rB1;
         rC3_0 += m3; m3 = ra3 * rB1;
         rC0_1 += m0; m0 = ra0 * rB2;
         rC1_1 += m1; m1 = ra1 * rB2;
         rC2_1 += m2; m2 = ra2 * rB2;
         rC3_1 += m3; m3 = ra3 * rB2;
         rC0_2 += m0; m0 = ra0 * rB3;
         rC1_2 += m1; m1 = ra1 * rB3;
         rC2_2 += m2; m2 = ra2 * rB3;
         rC3_2 += m3; m3 = ra3 * rB3;
         rC0_3 += m0; pA0 += incAm;
         rC1_3 += m1;
         rC2_3 += m2; pB0 += incBm;
         rC3_3 += m3;

         #ifdef TREAL
         *pC0 = rC0_0; pC0[1] = rC1_0; pC0[2] = rC2_0; pC0[3] = rC3_0; pC0 += 4;
         *pC1 = rC0_1; pC1[1] = rC1_1; pC1[2] = rC2_1; pC1[3] = rC3_1; pC1 += 4;
         *pC2 = rC0_2; pC2[1] = rC1_2; pC2[2] = rC2_2; pC2[3] = rC3_2; pC2 += 4;
         *pC3 = rC0_3; pC3[1] = rC1_3; pC3[2] = rC2_3; pC3[3] = rC3_3; pC3 += 4;
         #else
         *pC0 = rC0_0; pC0[2] = rC1_0; pC0[4] = rC2_0; pC0[6] = rC3_0; pC0 += 8;
         *pC1 = rC0_1; pC1[2] = rC1_1; pC1[4] = rC2_1; pC1[6] = rC3_1; pC1 += 8;
         *pC2 = rC0_2; pC2[2] = rC1_2; pC2[4] = rC2_2; pC2[6] = rC3_2; pC2 += 8;
         *pC3 = rC0_3; pC3[2] = rC1_3; pC3[4] = rC2_3; pC3[6] = rC3_3; pC3 += 8;
         #endif
      }
      while(pA0 != stM);
      pC0 += incCn; pC1 += incCn; pC2 += incCn; pC3 += incCn;
      pA0 += incAn; pB0 += incBn;
   }
   while(pB0 != stN);
}
#undef incBn
