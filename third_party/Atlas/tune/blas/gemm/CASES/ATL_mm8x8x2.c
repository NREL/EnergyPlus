/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2000 R. Clint Whaley
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
/* #undef ATL_ARCH_IA64Itan */
#include "atlas_prefetch.h"

void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                const TYPE *A, const int lda, const TYPE *B, const int ldb,
                const TYPE beta, TYPE *C, const int ldc)
{
   const int incAm = (lda<<3)-K, incAn = -M*lda;
   const int incBm = -K, incBn = (ldb<<3);
   const int Kstart = (K-2)>>1;
   #ifdef TREAL
      #define incCm 8
      #define ldc2 ldc
      const int incCn = (ldc<<3) - M;
   #else
      #define incCm 16
      const int ldc2 = ldc+ldc;
      const int incCn = (ldc<<4) - M - M;
   #endif

   TYPE *pC0=C, *pC1=C+ldc2, *pC2=pC1+ldc2, *pC3=pC2+ldc2, *pC4=pC3+ldc2,
        *pC5=pC4+ldc2, *pC6=pC5+ldc2, *pC7=pC6+ldc2;
   const TYPE *pA0=A, *pA1=pA0+lda, *pA2=pA1+lda, *pA3=pA2+lda, *pA4=pA3+lda,
              *pA5=pA4+lda, *pA6=pA5+lda, *pA7=pA6+lda;
   const TYPE *pB0=B, *pB1=pB0+ldb, *pB2=pB1+ldb, *pB3=pB2+ldb, *pB4=pB3+ldb,
              *pB5=pB4+ldb, *pB6=pB5+ldb, *pB7=pB6+ldb;
   const TYPE *stM = A + lda*M;
   const TYPE *stN = B + ldb*N;
   const TYPE *pfA = stM;
   #ifdef BETAX
      TYPE *bp = (TYPE *) &beta;
   #endif
   register int k;
   register TYPE rA0, rA1, rA2, rA3, rA4, rA5, rA6, rA7;
   register TYPE ra0, ra1, ra2, ra3, ra4, ra5, ra6, ra7;
   register TYPE rB0, rB1, rB2, rB3, rB4, rB5, rB6, rB7;
   register TYPE rb0, rb1, rb2, rb3, rb4, rb5, rb6, rb7;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC4_0, rC5_0, rC6_0, rC7_0,
                 rC0_1, rC1_1, rC2_1, rC3_1, rC4_1, rC5_1, rC6_1, rC7_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC4_2, rC5_2, rC6_2, rC7_2,
                 rC0_3, rC1_3, rC2_3, rC3_3, rC4_3, rC5_3, rC6_3, rC7_3,
                 rC0_4, rC1_4, rC2_4, rC3_4, rC4_4, rC5_4, rC6_4, rC7_4,
                 rC0_5, rC1_5, rC2_5, rC3_5, rC4_5, rC5_5, rC6_5, rC7_5,
                 rC0_6, rC1_6, rC2_6, rC3_6, rC4_6, rC5_6, rC6_6, rC7_6,
                 rC0_7, rC1_7, rC2_7, rC3_7, rC4_7, rC5_7, rC6_7, rC7_7;

   do
   {
      do
      {
         #ifdef BETA0
            rC0_0 = rC1_0 = rC2_0 = rC3_0 = rC4_0 = rC5_0 = rC6_0 = rC7_0 =
            rC0_1 = rC1_1 = rC2_1 = rC3_1 = rC4_1 = rC5_1 = rC6_1 = rC7_1 =
            rC0_2 = rC1_2 = rC2_2 = rC3_2 = rC4_2 = rC5_2 = rC6_2 = rC7_2 =
            rC0_3 = rC1_3 = rC2_3 = rC3_3 = rC4_3 = rC5_3 = rC6_3 = rC7_3 =
            rC0_4 = rC1_4 = rC2_4 = rC3_4 = rC4_4 = rC5_4 = rC6_4 = rC7_4 =
            rC0_5 = rC1_5 = rC2_5 = rC3_5 = rC4_5 = rC5_5 = rC6_5 = rC7_5 =
            rC0_6 = rC1_6 = rC2_6 = rC3_6 = rC4_6 = rC5_6 = rC6_6 = rC7_6 =
            rC0_7 = rC1_7 = rC2_7 = rC3_7 = rC4_7 = rC5_7 = rC6_7 = rC7_7 =
                    ATL_rzero;
         #else
            rC0_0 = *pC0; rC1_0 = pC0[1 SHIFT];
            rC2_0 = pC0[2 SHIFT]; rC3_0 = pC0[3 SHIFT];
            rC4_0 = pC0[4 SHIFT]; rC5_0 = pC0[5 SHIFT];
            rC6_0 = pC0[6 SHIFT]; rC7_0 = pC0[7 SHIFT];
            rC0_1 = *pC1; rC1_1 = pC1[1 SHIFT];
            rC2_1 = pC1[2 SHIFT]; rC3_1 = pC1[3 SHIFT];
            rC4_1 = pC1[4 SHIFT]; rC5_1 = pC1[5 SHIFT];
            rC6_1 = pC1[6 SHIFT]; rC7_1 = pC1[7 SHIFT];
            rC0_2 = *pC2; rC1_2 = pC2[1 SHIFT];
            rC2_2 = pC2[2 SHIFT]; rC3_2 = pC2[3 SHIFT];
            rC4_2 = pC2[4 SHIFT]; rC5_2 = pC2[5 SHIFT];
            rC6_2 = pC2[6 SHIFT]; rC7_2 = pC2[7 SHIFT];
            rC0_3 = *pC3; rC1_3 = pC3[1 SHIFT];
            rC2_3 = pC3[2 SHIFT]; rC3_3 = pC3[3 SHIFT];
            rC4_3 = pC3[4 SHIFT]; rC5_3 = pC3[5 SHIFT];
            rC6_3 = pC3[6 SHIFT]; rC7_3 = pC3[7 SHIFT];
            rC0_4 = *pC4; rC1_4 = pC4[1 SHIFT];
            rC2_4 = pC4[2 SHIFT]; rC3_4 = pC4[3 SHIFT];
            rC4_4 = pC4[4 SHIFT]; rC5_4 = pC4[5 SHIFT];
            rC6_4 = pC4[6 SHIFT]; rC7_4 = pC4[7 SHIFT];
            rC0_5 = *pC5; rC1_5 = pC5[1 SHIFT];
            rC2_5 = pC5[2 SHIFT]; rC3_5 = pC5[3 SHIFT];
            rC4_5 = pC5[4 SHIFT]; rC5_5 = pC5[5 SHIFT];
            rC6_5 = pC5[6 SHIFT]; rC7_5 = pC5[7 SHIFT];
            rC0_6 = *pC6; rC1_6 = pC6[1 SHIFT];
            rC2_6 = pC6[2 SHIFT]; rC3_6 = pC6[3 SHIFT];
            rC4_6 = pC6[4 SHIFT]; rC5_6 = pC6[5 SHIFT];
            rC6_6 = pC6[6 SHIFT]; rC7_6 = pC6[7 SHIFT];
            rC0_7 = *pC7; rC1_7 = pC7[1 SHIFT];
            rC2_7 = pC7[2 SHIFT]; rC3_7 = pC7[3 SHIFT];
            rC4_7 = pC7[4 SHIFT]; rC5_7 = pC7[5 SHIFT];
            rC6_7 = pC7[6 SHIFT]; rC7_7 = pC7[7 SHIFT];
            #ifdef BETAX
               rb7 = *bp;
               rC0_0 *= rb7; rC1_0 *= rb7; rC2_0 *= rb7; rC3_0 *= rb7;
               rC4_0 *= rb7; rC5_0 *= rb7; rC6_0 *= rb7; rC7_0 *= rb7;
               rC0_1 *= rb7; rC1_1 *= rb7; rC2_1 *= rb7; rC3_1 *= rb7;
               rC4_1 *= rb7; rC5_1 *= rb7; rC6_1 *= rb7; rC7_1 *= rb7;
               rC0_2 *= rb7; rC1_2 *= rb7; rC2_2 *= rb7; rC3_2 *= rb7;
               rC4_2 *= rb7; rC5_2 *= rb7; rC6_2 *= rb7; rC7_2 *= rb7;
               rC0_3 *= rb7; rC1_3 *= rb7; rC2_3 *= rb7; rC3_3 *= rb7;
               rC4_3 *= rb7; rC5_3 *= rb7; rC6_3 *= rb7; rC7_3 *= rb7;
               rC0_4 *= rb7; rC1_4 *= rb7; rC2_4 *= rb7; rC3_4 *= rb7;
               rC4_4 *= rb7; rC5_4 *= rb7; rC6_4 *= rb7; rC7_4 *= rb7;
               rC0_5 *= rb7; rC1_5 *= rb7; rC2_5 *= rb7; rC3_5 *= rb7;
               rC4_5 *= rb7; rC5_5 *= rb7; rC6_5 *= rb7; rC7_5 *= rb7;
               rC0_6 *= rb7; rC1_6 *= rb7; rC2_6 *= rb7; rC3_6 *= rb7;
               rC4_6 *= rb7; rC5_6 *= rb7; rC6_6 *= rb7; rC7_6 *= rb7;
               rC0_7 *= rb7; rC1_7 *= rb7; rC2_7 *= rb7; rC3_7 *= rb7;
               rC4_7 *= rb7; rC5_7 *= rb7; rC6_7 *= rb7; rC7_7 *= rb7;
            #endif
         #endif
         rA0 = *pA0++; rA1 = *pA1++; rA2 = *pA2++; rA3 = *pA3++;
         rA4 = *pA4++; rA5 = *pA5++; rA6 = *pA6++; rA7 = *pA7++;
         rB0 = *pB0++; rB1 = *pB1++; rB2 = *pB2++; rB3 = *pB3++;
         rB4 = *pB4++; rB5 = *pB5++; rB6 = *pB6++; rB7 = *pB7++;
         for (k=Kstart; k; k--) /* easy loop to unroll */
         {
            rC0_0 += rA0 * rB0;
                                   ra0 = *pA0++;
            rC1_0 += rA1 * rB0;
                                   ra1 = *pA1++;
            rC2_0 += rA2 * rB0;
                                   ra2 = *pA2++;
            rC3_0 += rA3 * rB0;
                                   ra3 = *pA3++;
            rC4_0 += rA4 * rB0;
                                   ra4 = *pA4++;
            rC5_0 += rA5 * rB0;
                                   ra5 = *pA5++;
            rC6_0 += rA6 * rB0;
                                   ra6 = *pA6++;
            rC7_0 += rA7 * rB0;
                                   ra7 = *pA7++;
            rC0_1 += rA0 * rB1;
                                   rb0 = *pB0++;
            rC1_1 += rA1 * rB1;
                                   rb1 = *pB1++;
            rC2_1 += rA2 * rB1;
                                   rb2 = *pB2++;
            rC3_1 += rA3 * rB1;
                                   rb3 = *pB3++;
            rC4_1 += rA4 * rB1;
                                   rb4 = *pB4++;
            rC5_1 += rA5 * rB1;
                                   rb5 = *pB5++;
            rC6_1 += rA6 * rB1;
                                   rb6 = *pB6++;
            rC7_1 += rA7 * rB1;
                                   rb7 = *pB7++;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
                                   rB0 = *pB0++;
            rC4_2 += rA4 * rB2;
                                   rB1 = *pB1++;
            rC5_2 += rA5 * rB2;
            rC6_2 += rA6 * rB2;
            rC7_2 += rA7 * rB2;
                                   rB2 = *pB2++;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3;
            rC5_3 += rA5 * rB3;
            rC6_3 += rA6 * rB3;
            rC7_3 += rA7 * rB3;
                                   rB3 = *pB3++;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4;
            rC5_4 += rA5 * rB4;
            rC6_4 += rA6 * rB4;
            rC7_4 += rA7 * rB4;
                                   rB4 = *pB4++;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5;
            rC5_5 += rA5 * rB5;
            rC6_5 += rA6 * rB5;
            rC7_5 += rA7 * rB5;
                                   rB5 = *pB5++;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6;
            rC5_6 += rA5 * rB6;
            rC6_6 += rA6 * rB6;
            rC7_6 += rA7 * rB6;
                                   rB6 = *pB6++;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7;
            rC5_7 += rA5 * rB7;
            rC6_7 += rA6 * rB7;
            rC7_7 += rA7 * rB7;
                                   rB7 = *pB7++;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0;
                                   rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
                                   rA1 = *pA1++;
            rC3_0 += ra3 * rb0;
                                   rA2 = *pA2++;
            rC4_0 += ra4 * rb0;
                                   rA3 = *pA3++;
            rC5_0 += ra5 * rb0;
                                   rA4 = *pA4++;
            rC6_0 += ra6 * rb0;
                                   rA5 = *pA5++;
            rC7_0 += ra7 * rb0;
                                   rA6 = *pA6++;
            rC0_1 += ra0 * rb1;
                                   rA7 = *pA7++;
            rC1_1 += ra1 * rb1;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1;
            rC5_1 += ra5 * rb1;
            rC6_1 += ra6 * rb1;
            rC7_1 += ra7 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2;
            rC5_2 += ra5 * rb2;
            rC6_2 += ra6 * rb2;
            rC7_2 += ra7 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3;
            rC5_3 += ra5 * rb3;
            rC6_3 += ra6 * rb3;
            rC7_3 += ra7 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4;
            rC5_4 += ra5 * rb4;
            rC6_4 += ra6 * rb4;
            rC7_4 += ra7 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5;
            rC5_5 += ra5 * rb5;
            rC6_5 += ra6 * rb5;
            rC7_5 += ra7 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC6_6 += ra6 * rb6;
            rC7_6 += ra7 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7;
            rC6_7 += ra6 * rb7;
            rC7_7 += ra7 * rb7;

         }
                                ATL_pfl1R(pfA);
         rC0_0 += rA0 * rB0;
                                ATL_pfl1R(pfA+16);
         rC1_0 += rA1 * rB0;
                                ATL_pfl1R(pfA+32);
         rC2_0 += rA2 * rB0;
                                ATL_pfl1R(pfA+48);
         rC3_0 += rA3 * rB0;
                                pfA += 64;
         rC4_0 += rA4 * rB0;
         rC5_0 += rA5 * rB0;
         rC6_0 += rA6 * rB0;
         rC7_0 += rA7 * rB0;
         rC0_1 += rA0 * rB1;
                                   ra0 = *pA0++;
         rC1_1 += rA1 * rB1;
                                   ra1 = *pA1++;
         rC2_1 += rA2 * rB1;
                                   ra2 = *pA2++;
         rC3_1 += rA3 * rB1;
                                   ra3 = *pA3++;
         rC4_1 += rA4 * rB1;
                                   ra4 = *pA4++;
         rC5_1 += rA5 * rB1;
                                   ra5 = *pA5++;
         rC6_1 += rA6 * rB1;
                                   ra6 = *pA6++;
         rC7_1 += rA7 * rB1;
                                   ra7 = *pA7++;
         rC0_2 += rA0 * rB2;
                                   rb0 = *pB0++;
         rC1_2 += rA1 * rB2;
                                   rb1 = *pB1++;
         rC2_2 += rA2 * rB2;
                                   rb2 = *pB2++;
         rC3_2 += rA3 * rB2;
                                   rb3 = *pB3++;
         rC4_2 += rA4 * rB2;
                                   rb4 = *pB4++;
         rC5_2 += rA5 * rB2;
                                   rb5 = *pB5++;
         rC6_2 += rA6 * rB2;
                                   rb6 = *pB6++;
         rC7_2 += rA7 * rB2;
                                   rb7 = *pB7++;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3;
                         pA0 += incAm;
         rC2_3 += rA2 * rB3;
                         pA1 += incAm;
         rC3_3 += rA3 * rB3;
                         pA2 += incAm;
         rC4_3 += rA4 * rB3;
                         pA3 += incAm;
         rC5_3 += rA5 * rB3;
                         pA4 += incAm;
         rC6_3 += rA6 * rB3;
                         pA5 += incAm;
         rC7_3 += rA7 * rB3;
                         pA6 += incAm;
         rC0_4 += rA0 * rB4;
                         pA7 += incAm;
         rC1_4 += rA1 * rB4;
                         pB0 += incBm;
         rC2_4 += rA2 * rB4;
                         pB1 += incBm;
         rC3_4 += rA3 * rB4;
                         pB2 += incBm;
         rC4_4 += rA4 * rB4;
                         pB3 += incBm;
         rC5_4 += rA5 * rB4;
                         pB4 += incBm;
         rC6_4 += rA6 * rB4;
                         pB5 += incBm;
         rC7_4 += rA7 * rB4;
                         pB6 += incBm;
         rC0_5 += rA0 * rB5;
                         pB7 += incBm;
         rC1_5 += rA1 * rB5;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5;
         rC5_5 += rA5 * rB5;
         rC6_5 += rA6 * rB5;
         rC7_5 += rA7 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6;
         rC5_6 += rA5 * rB6;
         rC6_6 += rA6 * rB6;
         rC7_6 += rA7 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7;
         rC5_7 += rA5 * rB7;
         rC6_7 += rA6 * rB7;
         rC7_7 += rA7 * rB7;

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0;
         rC5_0 += ra5 * rb0;
         rC6_0 += ra6 * rb0;
         rC7_0 += ra7 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1;
         rC5_1 += ra5 * rb1;
         rC6_1 += ra6 * rb1;
         rC7_1 += ra7 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2;
         rC5_2 += ra5 * rb2;
         rC6_2 += ra6 * rb2;
         rC7_2 += ra7 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3;
         rC5_3 += ra5 * rb3;
         rC6_3 += ra6 * rb3;
         rC7_3 += ra7 * rb3;
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4;
         rC5_4 += ra5 * rb4;
         rC6_4 += ra6 * rb4;
         rC7_4 += ra7 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5;
         rC5_5 += ra5 * rb5;
         rC6_5 += ra6 * rb5;
         rC7_5 += ra7 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC6_6 += ra6 * rb6;
         rC7_6 += ra7 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7;
         rC6_7 += ra6 * rb7;
         rC7_7 += ra7 * rb7;

         *pC0   = rC0_0; pC0[1 SHIFT] = rC1_0;
         pC0[2 SHIFT] = rC2_0; pC0[3 SHIFT] = rC3_0;
         pC0[4 SHIFT] = rC4_0; pC0[5 SHIFT] = rC5_0;
         pC0[6 SHIFT] = rC6_0; pC0[7 SHIFT] = rC7_0;
         *pC1   = rC0_1; pC1[1 SHIFT] = rC1_1;
         pC1[2 SHIFT] = rC2_1; pC1[3 SHIFT] = rC3_1;
         pC1[4 SHIFT] = rC4_1; pC1[5 SHIFT] = rC5_1;
         pC1[6 SHIFT] = rC6_1; pC1[7 SHIFT] = rC7_1;
         *pC2   = rC0_2; pC2[1 SHIFT] = rC1_2;
         pC2[2 SHIFT] = rC2_2; pC2[3 SHIFT] = rC3_2;
         pC2[4 SHIFT] = rC4_2; pC2[5 SHIFT] = rC5_2;
         pC2[6 SHIFT] = rC6_2; pC2[7 SHIFT] = rC7_2;
         *pC3   = rC0_3; pC3[1 SHIFT] = rC1_3;
         pC3[2 SHIFT] = rC2_3; pC3[3 SHIFT] = rC3_3;
         pC3[4 SHIFT] = rC4_3; pC3[5 SHIFT] = rC5_3;
         pC3[6 SHIFT] = rC6_3; pC3[7 SHIFT] = rC7_3;
         *pC4   = rC0_4; pC4[1 SHIFT] = rC1_4;
         pC4[2 SHIFT] = rC2_4; pC4[3 SHIFT] = rC3_4;
         pC4[4 SHIFT] = rC4_4; pC4[5 SHIFT] = rC5_4;
         pC4[6 SHIFT] = rC6_4; pC4[7 SHIFT] = rC7_4;
         *pC5   = rC0_5; pC5[1 SHIFT] = rC1_5;
         pC5[2 SHIFT] = rC2_5; pC5[3 SHIFT] = rC3_5;
         pC5[4 SHIFT] = rC4_5; pC5[5 SHIFT] = rC5_5;
         pC5[6 SHIFT] = rC6_5; pC5[7 SHIFT] = rC7_5;
         *pC6   = rC0_6; pC6[1 SHIFT] = rC1_6;
         pC6[2 SHIFT] = rC2_6; pC6[3 SHIFT] = rC3_6;
         pC6[4 SHIFT] = rC4_6; pC6[5 SHIFT] = rC5_6;
         pC6[6 SHIFT] = rC6_6; pC6[7 SHIFT] = rC7_6;
         *pC7   = rC0_7; pC7[1 SHIFT] = rC1_7;
         pC7[2 SHIFT] = rC2_7; pC7[3 SHIFT] = rC3_7;
         pC7[4 SHIFT] = rC4_7; pC7[5 SHIFT] = rC5_7;
         pC7[6 SHIFT] = rC6_7; pC7[7 SHIFT] = rC7_7;
         pC0 += incCm; pC1 += incCm; pC2 += incCm; pC3 += incCm;
         pC4 += incCm; pC5 += incCm; pC6 += incCm; pC7 += incCm;
      }
      while(pA0 != stM);
      pC0 += incCn; pC1 += incCn; pC2 += incCn; pC3 += incCn;
      pC4 += incCn; pC5 += incCn; pC6 += incCn; pC7 += incCn;
      pA0 += incAn; pA1 += incAn; pA2 += incAn; pA3 += incAn;
      pA4 += incAn; pA5 += incAn; pA6 += incAn; pA7 += incAn;
      pB0 += incBn; pB1 += incBn; pB2 += incBn; pB3 += incBn;
      pB4 += incBn; pB5 += incBn; pB6 += incBn; pB7 += incBn;
   }
   while(pB0 != stN);
}
#ifndef TREAL
   #undef ldc2
#endif
