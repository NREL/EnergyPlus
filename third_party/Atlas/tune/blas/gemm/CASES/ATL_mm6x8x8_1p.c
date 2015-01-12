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
/*
 * prefetch actually slows down complex, so don't do it
 */
#if defined(TCPLX) && defined(ATL_ARCH_IA64Itan)
   #undef ATL_ARCH_IA64Itan
#endif
#include "atlas_prefetch.h"

#if !defined(MB) || MB == 0
   #define ATL_CleanM
#elif ( (MB/6)*6 != MB )
   #error MB must be multiple of 6!!
#endif
#if !defined(NB) || NB == 0
   #define ATL_CleanN
#elif ( (NB/8)*8 != NB )
   #error NB must be multiple of 8!!
#endif
#if !defined(KB) || KB == 0
   #define ATL_CleanK
#elif ( (KB/8)*8 != KB )
   #error KB must be multiple of 8!!
#endif

#ifdef ATL_CleanM

#if defined(CleanN) || defined(CleanK)
   #error Can clean only one dimension at a time!!
#endif
static void CleanM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=0, NB=0, KB=0,
 * lda=0, ldb=0, ldc=0, mu=6, nu=8, ku=2
 */
{
   const int Nb = (N>>3)<<3;
   const int Kb = (K>>3)<<3;
   #define PFD KB6
   const int Kstart = (K>>3) - 1;
   #define PFB 16
   const TYPE *stN = B + (ldb*Nb);
   const int incAn = -K, incBn = (ldb<<3) - K, incCn = (ldc<<3)SHIFT;
   #ifdef TREAL
      #define ldc2 ldc
   #else
      const int ldc2=ldc<<1;
   #endif
   TYPE *pC0=C, *pC1=pC0+(ldc2), *pC2=pC1+(ldc2), *pC3=pC2+(ldc2),
        *pC4=pC3+(ldc2), *pC5=pC4+(ldc2), *pC6=pC5+(ldc2), *pC7=pC6+(ldc2);
   const TYPE *pA0=A, *pA1=pA0+(lda), *pA2=pA1+(lda), *pA3=pA2+(lda),
              *pA4=pA3+(lda), *pA5=A;
   const TYPE *pB0=B, *pB1=pB0+(ldb), *pB2=pB1+(ldb), *pB3=pB2+(ldb),
              *pB4=pB3+(ldb), *pB5=pB4+(ldb), *pB6=pB5+(ldb), *pB7=pB6+ldb;
   register int k;
   #ifdef BETAX
      TYPE *bp = (TYPE *) &beta;
   #endif
   register TYPE rA0, rA1, rA2, rA3, rA4, rA5;
   register TYPE ra0, ra1, ra2, ra3, ra4, ra5;
   register TYPE rB0, rB1, rB2, rB3, rB4, rB5, rB6, rB7;
   register TYPE rb0, rb1, rb2, rb3, rb4, rb5, rb6, rb7;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC4_0, rC5_0,
                 rC0_1, rC1_1, rC2_1, rC3_1, rC4_1, rC5_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC4_2, rC5_2,
                 rC0_3, rC1_3, rC2_3, rC3_3, rC4_3, rC5_3,
                 rC0_4, rC1_4, rC2_4, rC3_4, rC4_4, rC5_4,
                 rC0_5, rC1_5, rC2_5, rC3_5, rC4_5, rC5_5,
                 rC0_6, rC1_6, rC2_6, rC3_6, rC4_6, rC5_6,
                 rC0_7, rC1_7, rC2_7, rC3_7, rC4_7, rC5_7;

   switch(M)
   {
   case 1:
      pA1 = A;
   case 2:
      pA2 = A;
   case 3:
      pA3 = A;
   case 4:
      pA4 = A;
   default:;
   }
   do /* N-loop */
   {
      rC0_0 = rC1_0 = rC2_0 = rC3_0 = rC4_0 = rC5_0 =
      rC0_1 = rC1_1 = rC2_1 = rC3_1 = rC4_1 = rC5_1 =
      rC0_2 = rC1_2 = rC2_2 = rC3_2 = rC4_2 = rC5_2 =
      rC0_3 = rC1_3 = rC2_3 = rC3_3 = rC4_3 = rC5_3 =
      rC0_4 = rC1_4 = rC2_4 = rC3_4 = rC4_4 = rC5_4 =
      rC0_5 = rC1_5 = rC2_5 = rC3_5 = rC4_5 = rC5_5 =
      rC0_6 = rC1_6 = rC2_6 = rC3_6 = rC4_6 = rC5_6 =
      rC0_7 = rC1_7 = rC2_7 = rC3_7 = rC4_7 = rC5_7 = ATL_rzero;
      #ifndef BETA0
         switch(M)
         {
         case 5:
            #ifdef TREAL
               rC4_0 = pC0[4]; rC4_1 = pC1[4]; rC4_2 = pC2[4]; rC4_3 = pC3[4];
               rC4_4 = pC4[4]; rC4_5 = pC5[4]; rC4_6 = pC6[4]; rC4_7 = pC7[4];
            #else
               rC4_0 = pC0[8]; rC4_1 = pC1[8]; rC4_2 = pC2[8]; rC4_3 = pC3[8];
               rC4_4 = pC4[8]; rC4_5 = pC5[8]; rC4_6 = pC6[8]; rC4_7 = pC7[8];
            #endif
         case 4:
            #ifdef TREAL
               rC3_0 = pC0[3]; rC3_1 = pC1[3]; rC3_2 = pC2[3]; rC3_3 = pC3[3];
               rC3_4 = pC4[3]; rC3_5 = pC5[3]; rC3_6 = pC6[3]; rC3_7 = pC7[3];
            #else
               rC3_0 = pC0[6]; rC3_1 = pC1[6]; rC3_2 = pC2[6]; rC3_3 = pC3[6];
               rC3_4 = pC4[6]; rC3_5 = pC5[6]; rC3_6 = pC6[6]; rC3_7 = pC7[6];
            #endif
         case 3:
            #ifdef TREAL
               rC2_0 = pC0[2]; rC2_1 = pC1[2]; rC2_2 = pC2[2]; rC2_3 = pC3[2];
               rC2_4 = pC4[2]; rC2_5 = pC5[2]; rC2_6 = pC6[2]; rC2_7 = pC7[2];
            #else
               rC2_0 = pC0[4]; rC2_1 = pC1[4]; rC2_2 = pC2[4]; rC2_3 = pC3[4];
               rC2_4 = pC4[4]; rC2_5 = pC5[4]; rC2_6 = pC6[4]; rC2_7 = pC7[4];
            #endif
         case 2:
            #ifdef TREAL
               rC1_0 = pC0[1]; rC1_1 = pC1[1]; rC1_2 = pC2[1]; rC1_3 = pC3[1];
               rC1_4 = pC4[1]; rC1_5 = pC5[1]; rC1_6 = pC6[1]; rC1_7 = pC7[1];
            #else
               rC1_0 = pC0[2]; rC1_1 = pC1[2]; rC1_2 = pC2[2]; rC1_3 = pC3[2];
               rC1_4 = pC4[2]; rC1_5 = pC5[2]; rC1_6 = pC6[2]; rC1_7 = pC7[2];
            #endif
         default:
            rC0_0 = *pC0; rC0_1 = *pC1; rC0_2 = *pC2; rC0_3 = *pC3;
            rC0_4 = *pC4; rC0_5 = *pC5; rC0_6 = *pC6; rC0_7 = *pC7;
         }
         #ifdef BETAX
            rb7 = *bp;
            rC0_0 *= rb7; rC1_0 *= rb7; rC2_0 *= rb7;
            rC3_0 *= rb7; rC4_0 *= rb7; rC5_0 *= rb7;
            rC0_1 *= rb7; rC1_1 *= rb7; rC2_1 *= rb7;
            rC3_1 *= rb7; rC4_1 *= rb7; rC5_1 *= rb7;
            rC0_2 *= rb7; rC1_2 *= rb7; rC2_2 *= rb7;
            rC3_2 *= rb7; rC4_2 *= rb7; rC5_2 *= rb7;
            rC0_3 *= rb7; rC1_3 *= rb7; rC2_3 *= rb7;
            rC3_3 *= rb7; rC4_3 *= rb7; rC5_3 *= rb7;
            rC0_4 *= rb7; rC1_4 *= rb7; rC2_4 *= rb7;
            rC3_4 *= rb7; rC4_4 *= rb7; rC5_4 *= rb7;
            rC0_5 *= rb7; rC1_5 *= rb7; rC2_5 *= rb7;
            rC3_5 *= rb7; rC4_5 *= rb7; rC5_5 *= rb7;
            rC0_6 *= rb7; rC1_6 *= rb7; rC2_6 *= rb7;
            rC3_6 *= rb7; rC4_6 *= rb7; rC5_6 *= rb7;
            rC0_7 *= rb7; rC1_7 *= rb7; rC2_7 *= rb7;
            rC3_7 *= rb7; rC4_7 *= rb7; rC5_7 *= rb7;
         #endif
      #endif
      rA0 = *pA0++; rA1 = *pA1++; rA2 = *pA2++; rA3 = *pA3++;
      rA4 = *pA4++; rA5 = *pA5++;
      rB0 = *pB0++; rB1 = *pB1++; rB2 = *pB2++; rB3 = *pB3++;
      rB4 = *pB4++; rB5 = *pB5++; rB6 = *pB6++; rB7 = *pB7++;
      for (k=Kstart; k; k--) /* easy loop to unroll */
      {
         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0; ATL_pfl1R(pA0+PFD-1);
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3;
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5; ATL_pfl1R(pA1+PFD-2);
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7;

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3;
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7;

         rC0_0 += rA0 * rB0; ATL_pfl1R(pA2+PFD-3);
         rC1_0 += rA1 * rB0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3;
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5; ATL_pfl1R(pA3+PFD-3);
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7;

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2; ATL_pfl1R(pA4+PFD-4);
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3;
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7; ATL_pfl1R(pA5+PFD-4);

         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3; ATL_pfl1R(pB0+PFB-4);
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7; ATL_pfl1R(pB1+PFB-4);

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3; ATL_pfl1R(pB2+PFB-5);
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7; ATL_pfl1R(pB3+PFB-5);

         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3; ATL_pfl1R(pB4+PFB-6);
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7; ATL_pfl1R(pB5+PFB-7);

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3; ATL_pfl1R(pB6+PFB-7);
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7; ATL_pfl1R(pB7+PFB-8);
      }
      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7; rB0 = *pB0++;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0; rA0 = *pA0++;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0; rA1 = *pA1++;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1; rA2 = *pA2++;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1; rA3 = *pA3++;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2; rA4 = *pA4++;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2; rA5 = *pA5++;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3; rB1 = *pB1++;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3; rB2 = *pB2++;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4; rB3 = *pB3++;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4; rB4 = *pB4++;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5; rB5 = *pB5++;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; rB6 = *pB6++;
      rC5_5 += ra5 * rb5;
      rC0_6 += ra0 * rb6;
      rC1_6 += ra1 * rb6;
      rC2_6 += ra2 * rb6; rB7 = *pB7++;
      rC3_6 += ra3 * rb6;
      rC4_6 += ra4 * rb6;
      rC5_6 += ra5 * rb6;
      rC0_7 += ra0 * rb7;
      rC1_7 += ra1 * rb7;
      rC2_7 += ra2 * rb7;
      rC3_7 += ra3 * rb7;
      rC4_7 += ra4 * rb7;
      rC5_7 += ra5 * rb7;

      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7; rB0 = *pB0++;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0; rA0 = *pA0++;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0; rA1 = *pA1++;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1; rA2 = *pA2++;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1; rA3 = *pA3++;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2; rA4 = *pA4++;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2; rA5 = *pA5++;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3; rB1 = *pB1++;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3; rB2 = *pB2++;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4; rB3 = *pB3++;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4; rB4 = *pB4++;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5; rB5 = *pB5++;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; rB6 = *pB6++;
      rC5_5 += ra5 * rb5;
      rC0_6 += ra0 * rb6;
      rC1_6 += ra1 * rb6;
      rC2_6 += ra2 * rb6; rB7 = *pB7++;
      rC3_6 += ra3 * rb6;
      rC4_6 += ra4 * rb6;
      rC5_6 += ra5 * rb6;
      rC0_7 += ra0 * rb7;
      rC1_7 += ra1 * rb7;
      rC2_7 += ra2 * rb7;
      rC3_7 += ra3 * rb7;
      rC4_7 += ra4 * rb7;
      rC5_7 += ra5 * rb7;
      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7; rB0 = *pB0++;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0; rA0 = *pA0++;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0; rA1 = *pA1++;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1; rA2 = *pA2++;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1; rA3 = *pA3++;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2; rA4 = *pA4++;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2; rA5 = *pA5++;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3; rB1 = *pB1++;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3; rB2 = *pB2++;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4; rB3 = *pB3++;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4; rB4 = *pB4++;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5; rB5 = *pB5++;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; rB6 = *pB6++;
      rC5_5 += ra5 * rb5;
      rC0_6 += ra0 * rb6;
      rC1_6 += ra1 * rb6;
      rC2_6 += ra2 * rb6; rB7 = *pB7++;
      rC3_6 += ra3 * rb6;
      rC4_6 += ra4 * rb6;
      rC5_6 += ra5 * rb6;
      rC0_7 += ra0 * rb7;
      rC1_7 += ra1 * rb7;
      rC2_7 += ra2 * rb7;
      rC3_7 += ra3 * rb7;
      rC4_7 += ra4 * rb7;
      rC5_7 += ra5 * rb7;

      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; pA0 += incAn;
      rC5_5 += ra5 * rb5; pA1 += incAn;
      rC0_6 += ra0 * rb6; pA2 += incAn;
      rC1_6 += ra1 * rb6; pA3 += incAn;
      rC2_6 += ra2 * rb6; pA4 += incAn;
      rC3_6 += ra3 * rb6; pA5 += incAn;
      rC4_6 += ra4 * rb6; pB0 += incBn;
      rC5_6 += ra5 * rb6; pB1 += incBn;
      rC0_7 += ra0 * rb7; pB2 += incBn;
      rC1_7 += ra1 * rb7; pB3 += incBn;
      rC2_7 += ra2 * rb7; pB4 += incBn;
      rC3_7 += ra3 * rb7; pB5 += incBn;
      rC4_7 += ra4 * rb7; pB6 += incBn;
      rC5_7 += ra5 * rb7; pB7 += incBn;

      switch(M)
      {
      case 5:
         #ifdef TREAL
            pC0[4] = rC4_0; pC1[4] = rC4_1; pC2[4] = rC4_2; pC3[4] = rC4_3;
            pC4[4] = rC4_4; pC5[4] = rC4_5; pC6[4] = rC4_6; pC7[4] = rC4_7;
         #else
            pC0[8] = rC4_0; pC1[8] = rC4_1; pC2[8] = rC4_2; pC3[8] = rC4_3;
            pC4[8] = rC4_4; pC5[8] = rC4_5; pC6[8] = rC4_6; pC7[8] = rC4_7;
         #endif
      case 4:
         #ifdef TREAL
            pC0[3] = rC3_0; pC1[3] = rC3_1; pC2[3] = rC3_2; pC3[3] = rC3_3;
            pC4[3] = rC3_4; pC5[3] = rC3_5; pC6[3] = rC3_6; pC7[3] = rC3_7;
         #else
            pC0[6] = rC3_0; pC1[6] = rC3_1; pC2[6] = rC3_2; pC3[6] = rC3_3;
            pC4[6] = rC3_4; pC5[6] = rC3_5; pC6[6] = rC3_6; pC7[6] = rC3_7;
         #endif
      case 3:
         #ifdef TREAL
            pC0[2] = rC2_0; pC1[2] = rC2_1; pC2[2] = rC2_2; pC3[2] = rC2_3;
            pC4[2] = rC2_4; pC5[2] = rC2_5; pC6[2] = rC2_6; pC7[2] = rC2_7;
         #else
            pC0[4] = rC2_0; pC1[4] = rC2_1; pC2[4] = rC2_2; pC3[4] = rC2_3;
            pC4[4] = rC2_4; pC5[4] = rC2_5; pC6[4] = rC2_6; pC7[4] = rC2_7;
         #endif
      case 2:
         #ifdef TREAL
            pC0[1] = rC1_0; pC1[1] = rC1_1; pC2[1] = rC1_2; pC3[1] = rC1_3;
            pC4[1] = rC1_4; pC5[1] = rC1_5; pC6[1] = rC1_6; pC7[1] = rC1_7;
         #else
            pC0[2] = rC1_0; pC1[2] = rC1_1; pC2[2] = rC1_2; pC3[2] = rC1_3;
            pC4[2] = rC1_4; pC5[2] = rC1_5; pC6[2] = rC1_6; pC7[2] = rC1_7;
         #endif
      default:
         *pC0 = rC0_0; *pC1 = rC0_1; *pC2 = rC0_2; *pC3 = rC0_3;
         *pC4 = rC0_4; *pC5 = rC0_5; *pC6 = rC0_6; *pC7 = rC0_7;
      }
      pC0 += incCn; pC1 += incCn; pC2 += incCn; pC3 += incCn;
      pC4 += incCn; pC5 += incCn; pC6 += incCn; pC7 += incCn;
   }
   while(pB0 != stN);
}
#ifdef ldc2
   #undef ldc2
#endif
#ifdef PFD
   #undef PFD
#endif
#ifdef PFB
   #undef PFB
#endif

#endif /* end CleanM */

#ifdef ATL_CleanN

#if defined(CleanM) || defined(CleanK)
   #error Can clean only one dimension at a time!!
#endif
static void CleanN
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=0, NB=0, KB=0,
 * lda=0, ldb=0, ldc=0, mu=6, nu=8, ku=2
 */
{
   const int Mb = (M/6)*6;
   const int Nb = (N>>3)<<3;
   #define PFD KB6
   const int Kstart = (K>>3) - 1;
   #define PFB 16
   const TYPE *stM = A + (lda*Mb);
   const TYPE *stN = B + (ldb*Nb);
   const int incAm = ((((lda) << 2)+((lda) << 1)) - K), incAn = -(Mb*lda);
   const int incBm = -K, incBn = (((ldb) << 3));
   #ifdef TREAL
      #define incCm 6
      #define ldc2 ldc
      const int incCn = (((ldc) << 3)) - (Mb);
   #else
      #define incCm 12
      const int incCn = ((((ldc) << 3)) - (Mb))<<1, ldc2=ldc<<1;
   #endif
   TYPE *pC0=C, *pC1=pC0+(ldc2), *pC2=pC1+(ldc2), *pC3=pC2+(ldc2),
        *pC4=pC3+(ldc2), *pC5=pC4+(ldc2), *pC6=pC5+(ldc2), *pC7=pC6+(ldc2);
   const TYPE *pA0=A, *pA1=pA0+(lda), *pA2=pA1+(lda), *pA3=pA2+(lda),
              *pA4=pA3+(lda), *pA5=pA4+(lda);
   const TYPE *pB0=B, *pB1=pB0+(ldb), *pB2=pB1+(ldb), *pB3=pB2+(ldb),
              *pB4=pB3+(ldb), *pB5=pB4+(ldb), *pB6=pB5+(ldb), *pB7=B;
   register int k;
   #ifdef BETAX
      TYPE *bp = (TYPE *) &beta;
   #endif
   register TYPE rA0, rA1, rA2, rA3, rA4, rA5;
   register TYPE ra0, ra1, ra2, ra3, ra4, ra5;
   register TYPE rB0, rB1, rB2, rB3, rB4, rB5, rB6, rB7;
   register TYPE rb0, rb1, rb2, rb3, rb4, rb5, rb6, rb7;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC4_0, rC5_0,
                 rC0_1, rC1_1, rC2_1, rC3_1, rC4_1, rC5_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC4_2, rC5_2,
                 rC0_3, rC1_3, rC2_3, rC3_3, rC4_3, rC5_3,
                 rC0_4, rC1_4, rC2_4, rC3_4, rC4_4, rC5_4,
                 rC0_5, rC1_5, rC2_5, rC3_5, rC4_5, rC5_5,
                 rC0_6, rC1_6, rC2_6, rC3_6, rC4_6, rC5_6,
                 rC0_7, rC1_7, rC2_7, rC3_7, rC4_7, rC5_7;

   switch(N)
   {
   case 1:
      pB1 = B;
   case 2:
      pB2 = B;
   case 3:
      pB3 = B;
   case 4:
      pB4 = B;
   case 5:
      pB5 = B;
   case 6:
      pB6 = B;
   default:;
   }
   do /* M-loop */
   {
      rC0_0 = rC1_0 = rC2_0 = rC3_0 = rC4_0 = rC5_0 =
      rC0_1 = rC1_1 = rC2_1 = rC3_1 = rC4_1 = rC5_1 =
      rC0_2 = rC1_2 = rC2_2 = rC3_2 = rC4_2 = rC5_2 =
      rC0_3 = rC1_3 = rC2_3 = rC3_3 = rC4_3 = rC5_3 =
      rC0_4 = rC1_4 = rC2_4 = rC3_4 = rC4_4 = rC5_4 =
      rC0_5 = rC1_5 = rC2_5 = rC3_5 = rC4_5 = rC5_5 =
      rC0_6 = rC1_6 = rC2_6 = rC3_6 = rC4_6 = rC5_6 =
      rC0_7 = rC1_7 = rC2_7 = rC3_7 = rC4_7 = rC5_7 = ATL_rzero;
      #ifndef BETA0
         switch(N)
         {
         case 7:
            #ifdef TREAL
               rC0_6 = *pC6;   rC1_6 = pC6[1]; rC2_6 = pC6[ 2];
               rC3_6 = pC6[3]; rC4_6 = pC6[4]; rC5_6 = pC6[ 5];
            #else
               rC0_6 = *pC6;   rC1_6 = pC6[2]; rC2_6 = pC6[ 4];
               rC3_6 = pC6[6]; rC4_6 = pC6[8]; rC5_6 = pC6[10];
            #endif
         case 6:
            #ifdef TREAL
               rC0_5 = *pC5;   rC1_5 = pC5[1]; rC2_5 = pC5[ 2];
               rC3_5 = pC5[3]; rC4_5 = pC5[4]; rC5_5 = pC5[ 5];
            #else
               rC0_5 = *pC5;   rC1_5 = pC5[2]; rC2_5 = pC5[ 4];
               rC3_5 = pC5[6]; rC4_5 = pC5[8]; rC5_5 = pC5[10];
            #endif
         case 5:
            #ifdef TREAL
               rC0_4 = *pC4;   rC1_4 = pC4[1]; rC2_4 = pC4[ 2];
               rC3_4 = pC4[3]; rC4_4 = pC4[4]; rC5_4 = pC4[ 5];
            #else
               rC0_4 = *pC4;   rC1_4 = pC4[2]; rC2_4 = pC4[ 4];
               rC3_4 = pC4[6]; rC4_4 = pC4[8]; rC5_4 = pC4[10];
            #endif
         case 4:
            #ifdef TREAL
               rC0_3 = *pC3;   rC1_3 = pC3[1]; rC2_3 = pC3[ 2];
               rC3_3 = pC3[3]; rC4_3 = pC3[4]; rC5_3 = pC3[ 5];
            #else
               rC0_3 = *pC3;   rC1_3 = pC3[2]; rC2_3 = pC3[ 4];
               rC3_3 = pC3[6]; rC4_3 = pC3[8]; rC5_3 = pC3[10];
            #endif
         case 3:
            #ifdef TREAL
               rC0_2 = *pC2;   rC1_2 = pC2[1]; rC2_2 = pC2[ 2];
               rC3_2 = pC2[3]; rC4_2 = pC2[4]; rC5_2 = pC2[ 5];
            #else
               rC0_2 = *pC2;   rC1_2 = pC2[2]; rC2_2 = pC2[ 4];
               rC3_2 = pC2[6]; rC4_2 = pC2[8]; rC5_2 = pC2[10];
            #endif
         case 2:
            #ifdef TREAL
               rC0_1 = *pC1;   rC1_1 = pC1[1]; rC2_1 = pC1[ 2];
               rC3_1 = pC1[3]; rC4_1 = pC1[4]; rC5_1 = pC1[ 5];
            #else
               rC0_1 = *pC1;   rC1_1 = pC1[2]; rC2_1 = pC1[ 4];
               rC3_1 = pC1[6]; rC4_1 = pC1[8]; rC5_1 = pC1[10];
            #endif
         default:
            #ifdef TREAL
               rC0_0 = *pC0;   rC1_0 = pC0[1]; rC2_0 = pC0[ 2];
               rC3_0 = pC0[3]; rC4_0 = pC0[4]; rC5_0 = pC0[ 5];
            #else
               rC0_0 = *pC0;   rC1_0 = pC0[2]; rC2_0 = pC0[ 4];
               rC3_0 = pC0[6]; rC4_0 = pC0[8]; rC5_0 = pC0[10];
            #endif
         }
         #ifdef BETAX
            rb7 = *bp;
            rC0_0 *= rb7; rC1_0 *= rb7; rC2_0 *= rb7;
            rC3_0 *= rb7; rC4_0 *= rb7; rC5_0 *= rb7;
            rC0_1 *= rb7; rC1_1 *= rb7; rC2_1 *= rb7;
            rC3_1 *= rb7; rC4_1 *= rb7; rC5_1 *= rb7;
            rC0_2 *= rb7; rC1_2 *= rb7; rC2_2 *= rb7;
            rC3_2 *= rb7; rC4_2 *= rb7; rC5_2 *= rb7;
            rC0_3 *= rb7; rC1_3 *= rb7; rC2_3 *= rb7;
            rC3_3 *= rb7; rC4_3 *= rb7; rC5_3 *= rb7;
            rC0_4 *= rb7; rC1_4 *= rb7; rC2_4 *= rb7;
            rC3_4 *= rb7; rC4_4 *= rb7; rC5_4 *= rb7;
            rC0_5 *= rb7; rC1_5 *= rb7; rC2_5 *= rb7;
            rC3_5 *= rb7; rC4_5 *= rb7; rC5_5 *= rb7;
            rC0_6 *= rb7; rC1_6 *= rb7; rC2_6 *= rb7;
            rC3_6 *= rb7; rC4_6 *= rb7; rC5_6 *= rb7;
            rC0_7 *= rb7; rC1_7 *= rb7; rC2_7 *= rb7;
            rC3_7 *= rb7; rC4_7 *= rb7; rC5_7 *= rb7;
         #endif
      #endif
      rA0 = *pA0++; rA1 = *pA1++; rA2 = *pA2++; rA3 = *pA3++;
      rA4 = *pA4++; rA5 = *pA5++;
      rB0 = *pB0++; rB1 = *pB1++; rB2 = *pB2++; rB3 = *pB3++;
      rB4 = *pB4++; rB5 = *pB5++; rB6 = *pB6++; rB7 = *pB7++;
      for (k=Kstart; k; k--) /* easy loop to unroll */
      {
         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0; ATL_pfl1R(pA0+PFD-1);
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3;
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5; ATL_pfl1R(pA1+PFD-2);
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7;

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3;
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7;

         rC0_0 += rA0 * rB0; ATL_pfl1R(pA2+PFD-3);
         rC1_0 += rA1 * rB0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3;
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5; ATL_pfl1R(pA3+PFD-3);
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7;

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2; ATL_pfl1R(pA4+PFD-4);
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3;
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7; ATL_pfl1R(pA5+PFD-4);

         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3; ATL_pfl1R(pB0+PFB-4);
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7; ATL_pfl1R(pB1+PFB-4);

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3; ATL_pfl1R(pB2+PFB-5);
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7; ATL_pfl1R(pB3+PFB-5);

         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0;
         rC4_0 += rA4 * rB0; rb0 = *pB0++;
         rC5_0 += rA5 * rB0;
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra0 = *pA0++;
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1;
         rC4_1 += rA4 * rB1; ra1 = *pA1++;
         rC5_1 += rA5 * rB1;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2; ra2 = *pA2++;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC4_2 += rA4 * rB2; ra3 = *pA3++;
         rC5_2 += rA5 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3; ra4 = *pA4++;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;
         rC4_3 += rA4 * rB3; ra5 = *pA5++;
         rC5_3 += rA5 * rB3; ATL_pfl1R(pB4+PFB-6);
         rC0_4 += rA0 * rB4;
         rC1_4 += rA1 * rB4; rb1 = *pB1++;
         rC2_4 += rA2 * rB4;
         rC3_4 += rA3 * rB4;
         rC4_4 += rA4 * rB4; rb2 = *pB2++;
         rC5_4 += rA5 * rB4;
         rC0_5 += rA0 * rB5;
         rC1_5 += rA1 * rB5; rb3 = *pB3++;
         rC2_5 += rA2 * rB5;
         rC3_5 += rA3 * rB5;
         rC4_5 += rA4 * rB5; rb4 = *pB4++;
         rC5_5 += rA5 * rB5;
         rC0_6 += rA0 * rB6;
         rC1_6 += rA1 * rB6; rb5 = *pB5++;
         rC2_6 += rA2 * rB6;
         rC3_6 += rA3 * rB6;
         rC4_6 += rA4 * rB6; rb6 = *pB6++;
         rC5_6 += rA5 * rB6;
         rC0_7 += rA0 * rB7;
         rC1_7 += rA1 * rB7; rb7 = *pB7++;
         rC2_7 += rA2 * rB7;
         rC3_7 += rA3 * rB7;
         rC4_7 += rA4 * rB7; rB0 = *pB0++;
         rC5_7 += rA5 * rB7; ATL_pfl1R(pB5+PFB-7);

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0; rA0 = *pA0++;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC4_0 += ra4 * rb0; rA1 = *pA1++;
         rC5_0 += ra5 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1; rA2 = *pA2++;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC4_1 += ra4 * rb1; rA3 = *pA3++;
         rC5_1 += ra5 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2; rA4 = *pA4++;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC4_2 += ra4 * rb2; rA5 = *pA5++;
         rC5_2 += ra5 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3; rB1 = *pB1++;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         rC4_3 += ra4 * rb3; rB2 = *pB2++;
         rC5_3 += ra5 * rb3; ATL_pfl1R(pB6+PFB-7);
         rC0_4 += ra0 * rb4;
         rC1_4 += ra1 * rb4; rB3 = *pB3++;
         rC2_4 += ra2 * rb4;
         rC3_4 += ra3 * rb4;
         rC4_4 += ra4 * rb4; rB4 = *pB4++;
         rC5_4 += ra5 * rb4;
         rC0_5 += ra0 * rb5;
         rC1_5 += ra1 * rb5; rB5 = *pB5++;
         rC2_5 += ra2 * rb5;
         rC3_5 += ra3 * rb5;
         rC4_5 += ra4 * rb5; rB6 = *pB6++;
         rC5_5 += ra5 * rb5;
         rC0_6 += ra0 * rb6;
         rC1_6 += ra1 * rb6;
         rC2_6 += ra2 * rb6; rB7 = *pB7++;
         rC3_6 += ra3 * rb6;
         rC4_6 += ra4 * rb6;
         rC5_6 += ra5 * rb6;
         rC0_7 += ra0 * rb7;
         rC1_7 += ra1 * rb7;
         rC2_7 += ra2 * rb7;
         rC3_7 += ra3 * rb7;
         rC4_7 += ra4 * rb7;
         rC5_7 += ra5 * rb7; ATL_pfl1R(pB7+PFB-8);
      }
      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7; rB0 = *pB0++;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0; rA0 = *pA0++;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0; rA1 = *pA1++;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1; rA2 = *pA2++;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1; rA3 = *pA3++;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2; rA4 = *pA4++;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2; rA5 = *pA5++;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3; rB1 = *pB1++;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3; rB2 = *pB2++;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4; rB3 = *pB3++;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4; rB4 = *pB4++;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5; rB5 = *pB5++;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; rB6 = *pB6++;
      rC5_5 += ra5 * rb5;
      rC0_6 += ra0 * rb6;
      rC1_6 += ra1 * rb6;
      rC2_6 += ra2 * rb6; rB7 = *pB7++;
      rC3_6 += ra3 * rb6;
      rC4_6 += ra4 * rb6;
      rC5_6 += ra5 * rb6;
      rC0_7 += ra0 * rb7;
      rC1_7 += ra1 * rb7;
      rC2_7 += ra2 * rb7;
      rC3_7 += ra3 * rb7;
      rC4_7 += ra4 * rb7;
      rC5_7 += ra5 * rb7;

      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7; rB0 = *pB0++;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0; rA0 = *pA0++;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0; rA1 = *pA1++;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1; rA2 = *pA2++;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1; rA3 = *pA3++;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2; rA4 = *pA4++;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2; rA5 = *pA5++;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3; rB1 = *pB1++;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3; rB2 = *pB2++;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4; rB3 = *pB3++;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4; rB4 = *pB4++;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5; rB5 = *pB5++;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; rB6 = *pB6++;
      rC5_5 += ra5 * rb5;
      rC0_6 += ra0 * rb6;
      rC1_6 += ra1 * rb6;
      rC2_6 += ra2 * rb6; rB7 = *pB7++;
      rC3_6 += ra3 * rb6;
      rC4_6 += ra4 * rb6;
      rC5_6 += ra5 * rb6;
      rC0_7 += ra0 * rb7;
      rC1_7 += ra1 * rb7;
      rC2_7 += ra2 * rb7;
      rC3_7 += ra3 * rb7;
      rC4_7 += ra4 * rb7;
      rC5_7 += ra5 * rb7;
      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7; rB0 = *pB0++;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0; rA0 = *pA0++;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0; rA1 = *pA1++;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1; rA2 = *pA2++;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1; rA3 = *pA3++;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2; rA4 = *pA4++;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2; rA5 = *pA5++;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3; rB1 = *pB1++;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3; rB2 = *pB2++;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4; rB3 = *pB3++;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4; rB4 = *pB4++;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5; rB5 = *pB5++;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; rB6 = *pB6++;
      rC5_5 += ra5 * rb5;
      rC0_6 += ra0 * rb6;
      rC1_6 += ra1 * rb6;
      rC2_6 += ra2 * rb6; rB7 = *pB7++;
      rC3_6 += ra3 * rb6;
      rC4_6 += ra4 * rb6;
      rC5_6 += ra5 * rb6;
      rC0_7 += ra0 * rb7;
      rC1_7 += ra1 * rb7;
      rC2_7 += ra2 * rb7;
      rC3_7 += ra3 * rb7;
      rC4_7 += ra4 * rb7;
      rC5_7 += ra5 * rb7;

      rC0_0 += rA0 * rB0;
      rC1_0 += rA1 * rB0;
      rC2_0 += rA2 * rB0;
      rC3_0 += rA3 * rB0;
      rC4_0 += rA4 * rB0; rb0 = *pB0++;
      rC5_0 += rA5 * rB0;
      rC0_1 += rA0 * rB1;
      rC1_1 += rA1 * rB1; ra0 = *pA0++;
      rC2_1 += rA2 * rB1;
      rC3_1 += rA3 * rB1;
      rC4_1 += rA4 * rB1; ra1 = *pA1++;
      rC5_1 += rA5 * rB1;
      rC0_2 += rA0 * rB2;
      rC1_2 += rA1 * rB2; ra2 = *pA2++;
      rC2_2 += rA2 * rB2;
      rC3_2 += rA3 * rB2;
      rC4_2 += rA4 * rB2; ra3 = *pA3++;
      rC5_2 += rA5 * rB2;
      rC0_3 += rA0 * rB3;
      rC1_3 += rA1 * rB3; ra4 = *pA4++;
      rC2_3 += rA2 * rB3;
      rC3_3 += rA3 * rB3;
      rC4_3 += rA4 * rB3; ra5 = *pA5++;
      rC5_3 += rA5 * rB3;
      rC0_4 += rA0 * rB4;
      rC1_4 += rA1 * rB4; rb1 = *pB1++;
      rC2_4 += rA2 * rB4;
      rC3_4 += rA3 * rB4;
      rC4_4 += rA4 * rB4; rb2 = *pB2++;
      rC5_4 += rA5 * rB4;
      rC0_5 += rA0 * rB5;
      rC1_5 += rA1 * rB5; rb3 = *pB3++;
      rC2_5 += rA2 * rB5;
      rC3_5 += rA3 * rB5;
      rC4_5 += rA4 * rB5; rb4 = *pB4++;
      rC5_5 += rA5 * rB5;
      rC0_6 += rA0 * rB6;
      rC1_6 += rA1 * rB6; rb5 = *pB5++;
      rC2_6 += rA2 * rB6;
      rC3_6 += rA3 * rB6;
      rC4_6 += rA4 * rB6; rb6 = *pB6++;
      rC5_6 += rA5 * rB6;
      rC0_7 += rA0 * rB7;
      rC1_7 += rA1 * rB7; rb7 = *pB7++;
      rC2_7 += rA2 * rB7;
      rC3_7 += rA3 * rB7;
      rC4_7 += rA4 * rB7;
      rC5_7 += rA5 * rB7;

      rC0_0 += ra0 * rb0;
      rC1_0 += ra1 * rb0;
      rC2_0 += ra2 * rb0;
      rC3_0 += ra3 * rb0;
      rC4_0 += ra4 * rb0;
      rC5_0 += ra5 * rb0;
      rC0_1 += ra0 * rb1;
      rC1_1 += ra1 * rb1;
      rC2_1 += ra2 * rb1;
      rC3_1 += ra3 * rb1;
      rC4_1 += ra4 * rb1;
      rC5_1 += ra5 * rb1;
      rC0_2 += ra0 * rb2;
      rC1_2 += ra1 * rb2;
      rC2_2 += ra2 * rb2;
      rC3_2 += ra3 * rb2;
      rC4_2 += ra4 * rb2;
      rC5_2 += ra5 * rb2;
      rC0_3 += ra0 * rb3;
      rC1_3 += ra1 * rb3;
      rC2_3 += ra2 * rb3;
      rC3_3 += ra3 * rb3;
      rC4_3 += ra4 * rb3;
      rC5_3 += ra5 * rb3;
      rC0_4 += ra0 * rb4;
      rC1_4 += ra1 * rb4;
      rC2_4 += ra2 * rb4;
      rC3_4 += ra3 * rb4;
      rC4_4 += ra4 * rb4;
      rC5_4 += ra5 * rb4;
      rC0_5 += ra0 * rb5;
      rC1_5 += ra1 * rb5;
      rC2_5 += ra2 * rb5;
      rC3_5 += ra3 * rb5;
      rC4_5 += ra4 * rb5; pA0 += incAm;
      rC5_5 += ra5 * rb5; pA1 += incAm;
      rC0_6 += ra0 * rb6; pA2 += incAm;
      rC1_6 += ra1 * rb6; pA3 += incAm;
      rC2_6 += ra2 * rb6; pA4 += incAm;
      rC3_6 += ra3 * rb6; pA5 += incAm;
      rC4_6 += ra4 * rb6; pB0 += incBm;
      rC5_6 += ra5 * rb6; pB1 += incBm;
      rC0_7 += ra0 * rb7; pB2 += incBm;
      rC1_7 += ra1 * rb7; pB3 += incBm;
      rC2_7 += ra2 * rb7; pB4 += incBm;
      rC3_7 += ra3 * rb7; pB5 += incBm;
      rC4_7 += ra4 * rb7; pB6 += incBm;
      rC5_7 += ra5 * rb7; pB7 += incBm;

      switch(N)
      {
      case 7:
         #ifdef TREAL
            *pC6 = rC0_6;   pC6[1] = rC1_6; pC6[ 2] = rC2_6;
            pC6[3] = rC3_6; pC6[4] = rC4_6; pC6[ 5] = rC5_6;
         #else
            *pC6 = rC0_6;   pC6[2] = rC1_6; pC6[ 4] = rC2_6;
            pC6[6] = rC3_6; pC6[8] = rC4_6; pC6[10] = rC5_6;
         #endif
      case 6:
         #ifdef TREAL
            *pC5 = rC0_5;   pC5[1] = rC1_5; pC5[ 2] = rC2_5;
            pC5[3] = rC3_5; pC5[4] = rC4_5; pC5[ 5] = rC5_5;
         #else
            *pC5 = rC0_5;   pC5[2] = rC1_5; pC5[ 4] = rC2_5;
            pC5[6] = rC3_5; pC5[8] = rC4_5; pC5[10] = rC5_5;
         #endif
      case 5:
         #ifdef TREAL
            *pC4 = rC0_4;   pC4[1] = rC1_4; pC4[ 2] = rC2_4;
            pC4[3] = rC3_4; pC4[4] = rC4_4; pC4[ 5] = rC5_4;
         #else
            *pC4 = rC0_4;   pC4[2] = rC1_4; pC4[ 4] = rC2_4;
            pC4[6] = rC3_4; pC4[8] = rC4_4; pC4[10] = rC5_4;
         #endif
      case 4:
         #ifdef TREAL
            *pC3 = rC0_3;   pC3[1] = rC1_3; pC3[ 2] = rC2_3;
            pC3[3] = rC3_3; pC3[4] = rC4_3; pC3[ 5] = rC5_3;
         #else
            *pC3 = rC0_3;   pC3[2] = rC1_3; pC3[ 4] = rC2_3;
            pC3[6] = rC3_3; pC3[8] = rC4_3; pC3[10] = rC5_3;
         #endif
      case 3:
         #ifdef TREAL
            *pC2 = rC0_2;   pC2[1] = rC1_2; pC2[ 2] = rC2_2;
            pC2[3] = rC3_2; pC2[4] = rC4_2; pC2[ 5] = rC5_2;
         #else
            *pC2 = rC0_2;   pC2[2] = rC1_2; pC2[ 4] = rC2_2;
            pC2[6] = rC3_2; pC2[8] = rC4_2; pC2[10] = rC5_2;
         #endif
      case 2:
         #ifdef TREAL
            *pC1 = rC0_1;   pC1[1] = rC1_1; pC1[ 2] = rC2_1;
            pC1[3] = rC3_1; pC1[4] = rC4_1; pC1[ 5] = rC5_1;
         #else
            *pC1 = rC0_1;   pC1[2] = rC1_1; pC1[ 4] = rC2_1;
            pC1[6] = rC3_1; pC1[8] = rC4_1; pC1[10] = rC5_1;
         #endif
      default:
         #ifdef TREAL
            *pC0 = rC0_0;   pC0[1] = rC1_0; pC0[ 2] = rC2_0;
            pC0[3] = rC3_0; pC0[4] = rC4_0; pC0[ 5] = rC5_0;
         #else
            *pC0 = rC0_0;   pC0[2] = rC1_0; pC0[ 4] = rC2_0;
            pC0[6] = rC3_0; pC0[8] = rC4_0; pC0[10] = rC5_0;
         #endif
      }
      pC0 += incCm; pC1 += incCm; pC2 += incCm; pC3 += incCm;
      pC4 += incCm; pC5 += incCm; pC6 += incCm; pC7 += incCm;
   }
   while(pA0 != stM);
}
#ifdef incCm
   #undef incCm
#endif
#ifdef ldc2
   #undef ldc2
#endif
#ifdef PFD
   #undef PFD
#endif
#ifdef PFB
   #undef PFB
#endif

#endif /* end CleanN */

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=0, NB=0, KB=0,
 * lda=0, ldb=0, ldc=0, mu=6, nu=8, ku=2
 */
{
   const int Mb = (M/6)*6;
   const int Nb = (N>>3)<<3;
   const int Kb = (K>>3)<<3;
   #ifdef ATL_CleanK
      const int kr = K - Kb;
      const int PFD=lda*6;
   #else
      #define PFD KB6
   #endif
   const int Kstart = (K>>3) - 1;
   #define PFB 16
   const TYPE *stM = A + (lda*Mb);
   const TYPE *stN = B + (ldb*Nb);
   const int incAm = ((((lda) << 2)+((lda) << 1)) - K), incAn = -(Mb*lda);
   const int incBm = -K, incBn = (((ldb) << 3));
   #ifdef TREAL
      #define incCm 6
      #define ldc2 ldc
      const int incCn = (((ldc) << 3)) - (Mb);
   #else
      #define incCm 12
      const int incCn = ((((ldc) << 3)) - (Mb))<<1, ldc2=ldc<<1;
   #endif
   TYPE *pC0=C, *pC1=pC0+(ldc2), *pC2=pC1+(ldc2), *pC3=pC2+(ldc2),
        *pC4=pC3+(ldc2), *pC5=pC4+(ldc2), *pC6=pC5+(ldc2), *pC7=pC6+(ldc2);
   const TYPE *pA0=A, *pA1=pA0+(lda), *pA2=pA1+(lda), *pA3=pA2+(lda),
              *pA4=pA3+(lda), *pA5=pA4+(lda);
   const TYPE *pB0=B, *pB1=pB0+(ldb), *pB2=pB1+(ldb), *pB3=pB2+(ldb),
              *pB4=pB3+(ldb), *pB5=pB4+(ldb), *pB6=pB5+(ldb), *pB7=pB6+(ldb);
   register int k;
   #ifdef BETAX
      TYPE *bp = (TYPE *) &beta;
   #endif
   register TYPE rA0, rA1, rA2, rA3, rA4, rA5;
   register TYPE ra0, ra1, ra2, ra3, ra4, ra5;
   register TYPE rB0, rB1, rB2, rB3, rB4, rB5, rB6, rB7;
   register TYPE rb0, rb1, rb2, rb3, rb4, rb5, rb6, rb7;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC4_0, rC5_0,
                 rC0_1, rC1_1, rC2_1, rC3_1, rC4_1, rC5_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC4_2, rC5_2,
                 rC0_3, rC1_3, rC2_3, rC3_3, rC4_3, rC5_3,
                 rC0_4, rC1_4, rC2_4, rC3_4, rC4_4, rC5_4,
                 rC0_5, rC1_5, rC2_5, rC3_5, rC4_5, rC5_5,
                 rC0_6, rC1_6, rC2_6, rC3_6, rC4_6, rC5_6,
                 rC0_7, rC1_7, rC2_7, rC3_7, rC4_7, rC5_7;
   #ifdef ATL_CleanN
   if (Nb)
   {
   #elif defined(ATL_CleanM)
   if (Mb)
   {
   #endif
   do /* N-loop */
   {
      do /* M-loop */
      {
         #ifdef BETA0
            rC0_0 = rC1_0 = rC2_0 = rC3_0 = rC4_0 = rC5_0 =
            rC0_1 = rC1_1 = rC2_1 = rC3_1 = rC4_1 = rC5_1 =
            rC0_2 = rC1_2 = rC2_2 = rC3_2 = rC4_2 = rC5_2 =
            rC0_3 = rC1_3 = rC2_3 = rC3_3 = rC4_3 = rC5_3 =
            rC0_4 = rC1_4 = rC2_4 = rC3_4 = rC4_4 = rC5_4 =
            rC0_5 = rC1_5 = rC2_5 = rC3_5 = rC4_5 = rC5_5 =
            rC0_6 = rC1_6 = rC2_6 = rC3_6 = rC4_6 = rC5_6 =
            rC0_7 = rC1_7 = rC2_7 = rC3_7 = rC4_7 = rC5_7 = ATL_rzero;
         #else
            #ifdef TREAL
               rC0_0 = *pC0;   rC1_0 = pC0[1]; rC2_0 = pC0[ 2];
               rC3_0 = pC0[3]; rC4_0 = pC0[4]; rC5_0 = pC0[ 5];
               rC0_1 = *pC1;   rC1_1 = pC1[1]; rC2_1 = pC1[ 2];
               rC3_1 = pC1[3]; rC4_1 = pC1[4]; rC5_1 = pC1[ 5];
               rC0_2 = *pC2;   rC1_2 = pC2[1]; rC2_2 = pC2[ 2];
               rC3_2 = pC2[3]; rC4_2 = pC2[4]; rC5_2 = pC2[ 5];
               rC0_3 = *pC3;   rC1_3 = pC3[1]; rC2_3 = pC3[ 2];
               rC3_3 = pC3[3]; rC4_3 = pC3[4]; rC5_3 = pC3[ 5];
               rC0_4 = *pC4;   rC1_4 = pC4[1]; rC2_4 = pC4[ 2];
               rC3_4 = pC4[3]; rC4_4 = pC4[4]; rC5_4 = pC4[ 5];
               rC0_5 = *pC5;   rC1_5 = pC5[1]; rC2_5 = pC5[ 2];
               rC3_5 = pC5[3]; rC4_5 = pC5[4]; rC5_5 = pC5[ 5];
               rC0_6 = *pC6;   rC1_6 = pC6[1]; rC2_6 = pC6[ 2];
               rC3_6 = pC6[3]; rC4_6 = pC6[4]; rC5_6 = pC6[ 5];
               rC0_7 = *pC7;   rC1_7 = pC7[1]; rC2_7 = pC7[ 2];
               rC3_7 = pC7[3]; rC4_7 = pC7[4]; rC5_7 = pC7[ 5];
            #else
               rC0_0 = *pC0;   rC1_0 = pC0[2]; rC2_0 = pC0[ 4];
               rC3_0 = pC0[6]; rC4_0 = pC0[8]; rC5_0 = pC0[10];
               rC0_1 = *pC1;   rC1_1 = pC1[2]; rC2_1 = pC1[ 4];
               rC3_1 = pC1[6]; rC4_1 = pC1[8]; rC5_1 = pC1[10];
               rC0_2 = *pC2;   rC1_2 = pC2[2]; rC2_2 = pC2[ 4];
               rC3_2 = pC2[6]; rC4_2 = pC2[8]; rC5_2 = pC2[10];
               rC0_3 = *pC3;   rC1_3 = pC3[2]; rC2_3 = pC3[ 4];
               rC3_3 = pC3[6]; rC4_3 = pC3[8]; rC5_3 = pC3[10];
               rC0_4 = *pC4;   rC1_4 = pC4[2]; rC2_4 = pC4[ 4];
               rC3_4 = pC4[6]; rC4_4 = pC4[8]; rC5_4 = pC4[10];
               rC0_5 = *pC5;   rC1_5 = pC5[2]; rC2_5 = pC5[ 4];
               rC3_5 = pC5[6]; rC4_5 = pC5[8]; rC5_5 = pC5[10];
               rC0_6 = *pC6;   rC1_6 = pC6[2]; rC2_6 = pC6[ 4];
               rC3_6 = pC6[6]; rC4_6 = pC6[8]; rC5_6 = pC6[10];
               rC0_7 = *pC7;   rC1_7 = pC7[2]; rC2_7 = pC7[ 4];
               rC3_7 = pC7[6]; rC4_7 = pC7[8]; rC5_7 = pC7[10];
            #endif
            #ifdef BETAX
               rb7 = *bp;
               rC0_0 *= rb7; rC1_0 *= rb7; rC2_0 *= rb7;
               rC3_0 *= rb7; rC4_0 *= rb7; rC5_0 *= rb7;
               rC0_1 *= rb7; rC1_1 *= rb7; rC2_1 *= rb7;
               rC3_1 *= rb7; rC4_1 *= rb7; rC5_1 *= rb7;
               rC0_2 *= rb7; rC1_2 *= rb7; rC2_2 *= rb7;
               rC3_2 *= rb7; rC4_2 *= rb7; rC5_2 *= rb7;
               rC0_3 *= rb7; rC1_3 *= rb7; rC2_3 *= rb7;
               rC3_3 *= rb7; rC4_3 *= rb7; rC5_3 *= rb7;
               rC0_4 *= rb7; rC1_4 *= rb7; rC2_4 *= rb7;
               rC3_4 *= rb7; rC4_4 *= rb7; rC5_4 *= rb7;
               rC0_5 *= rb7; rC1_5 *= rb7; rC2_5 *= rb7;
               rC3_5 *= rb7; rC4_5 *= rb7; rC5_5 *= rb7;
               rC0_6 *= rb7; rC1_6 *= rb7; rC2_6 *= rb7;
               rC3_6 *= rb7; rC4_6 *= rb7; rC5_6 *= rb7;
               rC0_7 *= rb7; rC1_7 *= rb7; rC2_7 *= rb7;
               rC3_7 *= rb7; rC4_7 *= rb7; rC5_7 *= rb7;
            #endif
         #endif
         #ifdef ATL_CleanK
         if (Kb)
         {
         #endif
         rA0 = *pA0++; rA1 = *pA1++; rA2 = *pA2++; rA3 = *pA3++;
         rA4 = *pA4++; rA5 = *pA5++;
         rB0 = *pB0++; rB1 = *pB1++; rB2 = *pB2++; rB3 = *pB3++;
         rB4 = *pB4++; rB5 = *pB5++; rB6 = *pB6++; rB7 = *pB7++;
         for (k=Kstart; k; k--) /* easy loop to unroll */
         {
            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0; ATL_pfl1R(pA0+PFD-1);
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5; ATL_pfl1R(pA1+PFD-2);
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7;

            rC0_0 += rA0 * rB0; ATL_pfl1R(pA2+PFD-3);
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5; ATL_pfl1R(pA3+PFD-3);
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2; ATL_pfl1R(pA4+PFD-4);
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7; ATL_pfl1R(pA5+PFD-4);

            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3; ATL_pfl1R(pB0+PFB-4);
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7; ATL_pfl1R(pB1+PFB-4);

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3; ATL_pfl1R(pB2+PFB-5);
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7; ATL_pfl1R(pB3+PFB-5);

            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3; ATL_pfl1R(pB4+PFB-6);
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7; ATL_pfl1R(pB5+PFB-7);

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3; ATL_pfl1R(pB6+PFB-7);
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7; ATL_pfl1R(pB7+PFB-8);
         }
            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7;

            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7;
            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7; rB0 = *pB0++;
            rC5_7 += rA5 * rB7;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0; rA0 = *pA0++;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0; rA1 = *pA1++;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA2 = *pA2++;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1; rA3 = *pA3++;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA4 = *pA4++;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2; rA5 = *pA5++;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rB1 = *pB1++;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3; rB2 = *pB2++;
            rC5_3 += ra5 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4; rB3 = *pB3++;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4; rB4 = *pB4++;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5; rB5 = *pB5++;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
            rC4_5 += ra4 * rb5; rB6 = *pB6++;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6; rB7 = *pB7++;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7;

            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC4_0 += rA4 * rB0; rb0 = *pB0++;
            rC5_0 += rA5 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra0 = *pA0++;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC4_1 += rA4 * rB1; ra1 = *pA1++;
            rC5_1 += rA5 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = *pA2++;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC4_2 += rA4 * rB2; ra3 = *pA3++;
            rC5_2 += rA5 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra4 = *pA4++;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
            rC4_3 += rA4 * rB3; ra5 = *pA5++;
            rC5_3 += rA5 * rB3;
            rC0_4 += rA0 * rB4;
            rC1_4 += rA1 * rB4; rb1 = *pB1++;
            rC2_4 += rA2 * rB4;
            rC3_4 += rA3 * rB4;
            rC4_4 += rA4 * rB4; rb2 = *pB2++;
            rC5_4 += rA5 * rB4;
            rC0_5 += rA0 * rB5;
            rC1_5 += rA1 * rB5; rb3 = *pB3++;
            rC2_5 += rA2 * rB5;
            rC3_5 += rA3 * rB5;
            rC4_5 += rA4 * rB5; rb4 = *pB4++;
            rC5_5 += rA5 * rB5;
            rC0_6 += rA0 * rB6;
            rC1_6 += rA1 * rB6; rb5 = *pB5++;
            rC2_6 += rA2 * rB6;
            rC3_6 += rA3 * rB6;
            rC4_6 += rA4 * rB6; rb6 = *pB6++;
            rC5_6 += rA5 * rB6;
            rC0_7 += rA0 * rB7;
            rC1_7 += rA1 * rB7; rb7 = *pB7++;
            rC2_7 += rA2 * rB7;
            rC3_7 += rA3 * rB7;
            rC4_7 += rA4 * rB7;
            rC5_7 += rA5 * rB7;

            rC0_0 += ra0 * rb0;
            rC1_0 += ra1 * rb0;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0;
            rC4_0 += ra4 * rb0;
            rC5_0 += ra5 * rb0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1;
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1;
            rC4_1 += ra4 * rb1;
            rC5_1 += ra5 * rb1;
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2;
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2;
            rC4_2 += ra4 * rb2;
            rC5_2 += ra5 * rb2;
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3;
            rC2_3 += ra2 * rb3;
            rC3_3 += ra3 * rb3;
            rC4_3 += ra4 * rb3;
            rC5_3 += ra5 * rb3;
            rC0_4 += ra0 * rb4;
            rC1_4 += ra1 * rb4;
            rC2_4 += ra2 * rb4;
            rC3_4 += ra3 * rb4;
            rC4_4 += ra4 * rb4;
            rC5_4 += ra5 * rb4;
            rC0_5 += ra0 * rb5;
            rC1_5 += ra1 * rb5;
            rC2_5 += ra2 * rb5;
            rC3_5 += ra3 * rb5;
         #ifndef ATL_CleanK
            rC4_5 += ra4 * rb5; pA0 += incAm;
            rC5_5 += ra5 * rb5; pA1 += incAm;
            rC0_6 += ra0 * rb6; pA2 += incAm;
            rC1_6 += ra1 * rb6; pA3 += incAm;
            rC2_6 += ra2 * rb6; pA4 += incAm;
            rC3_6 += ra3 * rb6; pA5 += incAm;
            rC4_6 += ra4 * rb6; pB0 += incBm;
            rC5_6 += ra5 * rb6; pB1 += incBm;
            rC0_7 += ra0 * rb7; pB2 += incBm;
            rC1_7 += ra1 * rb7; pB3 += incBm;
            rC2_7 += ra2 * rb7; pB4 += incBm;
            rC3_7 += ra3 * rb7; pB5 += incBm;
            rC4_7 += ra4 * rb7; pB6 += incBm;
            rC5_7 += ra5 * rb7; pB7 += incBm;
         #else
            rC4_5 += ra4 * rb5;
            rC5_5 += ra5 * rb5;
            rC0_6 += ra0 * rb6;
            rC1_6 += ra1 * rb6;
            rC2_6 += ra2 * rb6;
            rC3_6 += ra3 * rb6;
            rC4_6 += ra4 * rb6;
            rC5_6 += ra5 * rb6;
            rC0_7 += ra0 * rb7;
            rC1_7 += ra1 * rb7;
            rC2_7 += ra2 * rb7;
            rC3_7 += ra3 * rb7;
            rC4_7 += ra4 * rb7;
            rC5_7 += ra5 * rb7;
         }
            for (k=kr; k; k--)
            {
               rA0 = *pA0++; rB0 = *pB0++;
               rA1 = *pA1++; rA2 = *pA2++; rA3 = *pA3++;
               rA4 = *pA4++; rA5 = *pA5++;
               rB1 = *pB1++; rB2 = *pB2++; rB3 = *pB3++;
               rB4 = *pB4++; rB5 = *pB5++; rB6 = *pB6++; rB7 = *pB7++;
               rC0_0 += rA0 * rB0;
               rC1_0 += rA1 * rB0;
               rC2_0 += rA2 * rB0;
               rC3_0 += rA3 * rB0;
               rC4_0 += rA4 * rB0;
               rC5_0 += rA5 * rB0;
               rC0_1 += rA0 * rB1;
               rC1_1 += rA1 * rB1;
               rC2_1 += rA2 * rB1;
               rC3_1 += rA3 * rB1;
               rC4_1 += rA4 * rB1;
               rC5_1 += rA5 * rB1;
               rC0_2 += rA0 * rB2;
               rC1_2 += rA1 * rB2;
               rC2_2 += rA2 * rB2;
               rC3_2 += rA3 * rB2;
               rC4_2 += rA4 * rB2;
               rC5_2 += rA5 * rB2;
               rC0_3 += rA0 * rB3;
               rC1_3 += rA1 * rB3;
               rC2_3 += rA2 * rB3;
               rC3_3 += rA3 * rB3;
               rC4_3 += rA4 * rB3;
               rC5_3 += rA5 * rB3;
               rC0_4 += rA0 * rB4;
               rC1_4 += rA1 * rB4;
               rC2_4 += rA2 * rB4;
               rC3_4 += rA3 * rB4;
               rC4_4 += rA4 * rB4;
               rC5_4 += rA5 * rB4;
               rC0_5 += rA0 * rB5;
               rC1_5 += rA1 * rB5;
               rC2_5 += rA2 * rB5;
               rC3_5 += rA3 * rB5;
               rC4_5 += rA4 * rB5;
               rC5_5 += rA5 * rB5;
               rC0_6 += rA0 * rB6;
               rC1_6 += rA1 * rB6;
               rC2_6 += rA2 * rB6;
               rC3_6 += rA3 * rB6;
               rC4_6 += rA4 * rB6;
               rC5_6 += rA5 * rB6;
               rC0_7 += rA0 * rB7;
               rC1_7 += rA1 * rB7;
               rC2_7 += rA2 * rB7;
               rC3_7 += rA3 * rB7;
               rC4_7 += rA4 * rB7;
               rC5_7 += rA5 * rB7;
            }
            pA0 += incAm; pA1 += incAm; pA2 += incAm; pA3 += incAm;
            pA4 += incAm; pA5 += incAm;
            pB0 += incBm; pB1 += incBm; pB2 += incBm; pB3 += incBm;
            pB4 += incBm; pB5 += incBm; pB6 += incBm; pB7 += incBm;
         #endif

         #ifdef TREAL
            *pC0 =   rC0_0; pC0[1] = rC1_0; pC0[ 2] = rC2_0;
            pC0[3] = rC3_0; pC0[4] = rC4_0; pC0[ 5] = rC5_0;
            *pC1 =   rC0_1; pC1[1] = rC1_1; pC1[ 2] = rC2_1;
            pC1[3] = rC3_1; pC1[4] = rC4_1; pC1[ 5] = rC5_1;
            *pC2 =   rC0_2; pC2[1] = rC1_2; pC2[ 2] = rC2_2;
            pC2[3] = rC3_2; pC2[4] = rC4_2; pC2[ 5] = rC5_2;
            *pC3 =   rC0_3; pC3[1] = rC1_3; pC3[ 2] = rC2_3;
            pC3[3] = rC3_3; pC3[4] = rC4_3; pC3[ 5] = rC5_3;
            *pC4 =   rC0_4; pC4[1] = rC1_4; pC4[ 2] = rC2_4;
            pC4[3] = rC3_4; pC4[4] = rC4_4; pC4[ 5] = rC5_4;
            *pC5 =   rC0_5; pC5[1] = rC1_5; pC5[ 2] = rC2_5;
            pC5[3] = rC3_5; pC5[4] = rC4_5; pC5[ 5] = rC5_5;
            *pC6 =   rC0_6; pC6[1] = rC1_6; pC6[ 2] = rC2_6;
            pC6[3] = rC3_6; pC6[4] = rC4_6; pC6[ 5] = rC5_6;
            *pC7 =   rC0_7; pC7[1] = rC1_7; pC7[ 2] = rC2_7;
            pC7[3] = rC3_7; pC7[4] = rC4_7; pC7[ 5] = rC5_7;
         #else
            *pC0 =   rC0_0; pC0[2] = rC1_0; pC0[ 4] = rC2_0;
            pC0[6] = rC3_0; pC0[8] = rC4_0; pC0[10] = rC5_0;
            *pC1 =   rC0_1; pC1[2] = rC1_1; pC1[ 4] = rC2_1;
            pC1[6] = rC3_1; pC1[8] = rC4_1; pC1[10] = rC5_1;
            *pC2 =   rC0_2; pC2[2] = rC1_2; pC2[ 4] = rC2_2;
            pC2[6] = rC3_2; pC2[8] = rC4_2; pC2[10] = rC5_2;
            *pC3 =   rC0_3; pC3[2] = rC1_3; pC3[ 4] = rC2_3;
            pC3[6] = rC3_3; pC3[8] = rC4_3; pC3[10] = rC5_3;
            *pC4 =   rC0_4; pC4[2] = rC1_4; pC4[ 4] = rC2_4;
            pC4[6] = rC3_4; pC4[8] = rC4_4; pC4[10] = rC5_4;
            *pC5 =   rC0_5; pC5[2] = rC1_5; pC5[ 4] = rC2_5;
            pC5[6] = rC3_5; pC5[8] = rC4_5; pC5[10] = rC5_5;
            *pC6 =   rC0_6; pC6[2] = rC1_6; pC6[ 4] = rC2_6;
            pC6[6] = rC3_6; pC6[8] = rC4_6; pC6[10] = rC5_6;
            *pC7 =   rC0_7; pC7[2] = rC1_7; pC7[ 4] = rC2_7;
            pC7[6] = rC3_7; pC7[8] = rC4_7; pC7[10] = rC5_7;
         #endif
         pC0 += incCm; pC1 += incCm; pC2 += incCm; pC3 += incCm;
         pC4 += incCm; pC5 += incCm; pC6 += incCm; pC7 += incCm;
      }
      while(pA0 != stM);
      pC0 += incCn; pC1 += incCn; pC2 += incCn; pC3 += incCn;
      pC4 += incCn; pC5 += incCn; pC6 += incCn; pC7 += incCn;
      pA0 += incAn; pA1 += incAn; pA2 += incAn; pA3 += incAn;
      pA4 += incAn; pA5 += incAn;
      pB0 += incBn; pB1 += incBn; pB2 += incBn; pB3 += incBn;
      pB4 += incBn; pB5 += incBn; pB6 += incBn; pB7 += incBn;
   }
   while(pB0 != stN);
   #ifdef ATL_CleanN
   }
      if (k=N-Nb) CleanN(M, k, K, alpha, A, lda, pB0, ldb, beta, pC0, ldc);
   #elif defined(ATL_CleanM)
   }
      if (k=M-Mb) CleanM(k, N, K, alpha, A+Mb*lda, lda, B, ldb, beta,
                         C+(Mb SHIFT), ldc);
   #endif
}
#ifdef incCm
   #undef incCm
#endif
#ifdef incCn
   #undef incCn
#endif
#ifdef ldc2
   #undef ldc2
#endif
#ifdef PFD
   #undef PFD
#endif
#ifdef PFB
   #undef PFB
#endif
