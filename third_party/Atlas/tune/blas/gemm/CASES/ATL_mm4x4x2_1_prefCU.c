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

#if KB == 1

static void ATL_myger(const int M, const int N, const TYPE *X, const TYPE *Y,
                      const TYPE beta, TYPE *C, const int ldc)
{
   const TYPE *stY = Y + N;
   #ifdef TCPLX
      const int ldc2 = ldc<<1;
      #define incC 2
   #else
      #define ldc2 ldc
      #define incC 1
   #endif
   do
   {
      #ifdef BETAX
         Mjoin(PATLU,axpby)(M, *Y++, X, 1, beta, C, incC);
      #elif defined(BETA0)
         Mjoin(PATLU,cpsc)(M, *Y++, X, 1, C, incC);
      #else
         Mjoin(PATLU,axpy)(M, *Y++, X, 1, C, incC);
      #endif
      C += ldc2;
   }
   while (Y != stY);
}
#undef incC
#ifdef ldc2
   #undef ldc2
#endif

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
{
   ATL_myger(M, N, A, B, beta, C, ldc);
}

#else

#if ( KB != ((KB/2)*2) )
   #define ODDKB
#endif
void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with muladd=1, TA=T, TB=N, mu=4, nu=4, ku=2, prefetching A and B
 */
{
   const TYPE *stM = A + KB*M;
   const TYPE *stN = B + KB*N;
   const int incAn = -KB*M;
   const int incBm = -KB;
   #define incAm KB3
   #define incBn KB4
   #ifdef TREAL
      #define incCm 4
      const int incCn = (((ldc) << 2)) - M;
   #else
      #define incCm 8
      const int incCn = (((ldc) << 3)) - (M+M);
   #endif
   TYPE *pC0=C, *pC1=pC0+(ldc SHIFT), *pC2=pC1+(ldc SHIFT),*pC3=pC2+(ldc SHIFT);
   TYPE *bp = (TYPE *) &beta;
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   register int k;
   register TYPE rA0, rA1, rA2, rA3, ra0, ra1, ra2, ra3;
   register TYPE rB0, rB1, rB2, rB3, rb0, rb1, rb2, rb3;
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
            #ifdef TREAL
               rC0_0 = *pC0; rC1_0 = pC0[1]; rC2_0 = pC0[2]; rC3_0 = pC0[3];
               rC0_1 = *pC1; rC1_1 = pC1[1]; rC2_1 = pC1[2]; rC3_1 = pC1[3];
               rC0_2 = *pC2; rC1_2 = pC2[1]; rC2_2 = pC2[2]; rC3_2 = pC2[3];
               rC0_3 = *pC3; rC1_3 = pC3[1]; rC2_3 = pC3[2]; rC3_3 = pC3[3];
            #else
               rC0_0 = *pC0; rC1_0 = pC0[2]; rC2_0 = pC0[4]; rC3_0 = pC0[6];
               rC0_1 = *pC1; rC1_1 = pC1[2]; rC2_1 = pC1[4]; rC3_1 = pC1[6];
               rC0_2 = *pC2; rC1_2 = pC2[2]; rC2_2 = pC2[4]; rC3_2 = pC2[6];
               rC0_3 = *pC3; rC1_3 = pC3[2]; rC2_3 = pC3[4]; rC3_3 = pC3[6];
            #endif
            #ifdef BETAX
               rA0 = *bp;
               rC0_0 *= rA0; rC1_0 *= rA0; rC2_0 *= rA0; rC3_0 *= rA0;
               rC0_1 *= rA0; rC1_1 *= rA0; rC2_1 *= rA0; rC3_1 *= rA0;
               rC0_2 *= rA0; rC1_2 *= rA0; rC2_2 *= rA0; rC3_2 *= rA0;
               rC0_3 *= rA0; rC1_3 *= rA0; rC2_3 *= rA0; rC3_3 *= rA0;
            #endif
         #endif
         rA0 = *pA0; rA1 = pA0[KB]; rA2 = pA0[KB2]; rA3 = pA0[KB3]; pA0++;
         rB0 = *pB0; rB1 = pB0[KB]; rB2 = pB0[KB2]; rB3 = pB0[KB3]; pB0++;
         rb0 = *pB0; rb1 = pB0[KB]; rb2 = pB0[KB2]; rb3 = pB0[KB3]; pB0++;
         for (k=(KB>>1)-1; k; k --) /* easy loop to unroll */
         {
            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0; ra0 = *pA0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = *pB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1; ra1 = pA0[KB];
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2; ra2 = pA0[KB2];
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2];
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3; ra3 = pA0[KB3];
            rC2_3 += rA2 * rB3; pA0++;
            rC3_3 += rA3 * rB3; rB3 = pB0[KB3];

            rC0_0 += ra0 * rb0; pB0++;
            rC1_0 += ra1 * rb0; rA0 = *pA0;
            rC2_0 += ra2 * rb0;
            rC3_0 += ra3 * rb0; rb0 = *pB0;
            rC0_1 += ra0 * rb1;
            rC1_1 += ra1 * rb1; rA1 = pA0[KB];
            rC2_1 += ra2 * rb1;
            rC3_1 += ra3 * rb1; rb1 = pB0[KB];
            rC0_2 += ra0 * rb2;
            rC1_2 += ra1 * rb2; rA2 = pA0[KB2];
            rC2_2 += ra2 * rb2;
            rC3_2 += ra3 * rb2; rb2 = pB0[KB2];
            rC0_3 += ra0 * rb3;
            rC1_3 += ra1 * rb3; rA3 = pA0[KB3];
            rC2_3 += ra2 * rb3; pA0++;
            rC3_3 += ra3 * rb3; rb3 = pB0[KB3]; pB0++;
         }
         rC0_0 += rA0 * rB0;
         rC1_0 += rA1 * rB0; ra0 = *pA0;
         rC2_0 += rA2 * rB0;
         rC3_0 += rA3 * rB0; ra1 = pA0[KB];
         rC0_1 += rA0 * rB1;
         rC1_1 += rA1 * rB1; ra2 = pA0[KB2];
         rC2_1 += rA2 * rB1;
         rC3_1 += rA3 * rB1; ra3 = pA0[KB3]; pA0++;
         rC0_2 += rA0 * rB2;
         rC1_2 += rA1 * rB2;
         rC2_2 += rA2 * rB2;
         rC3_2 += rA3 * rB2;
         rC0_3 += rA0 * rB3;
         rC1_3 += rA1 * rB3;
         rC2_3 += rA2 * rB3;
         rC3_3 += rA3 * rB3;

         rC0_0 += ra0 * rb0;
         rC1_0 += ra1 * rb0;
         rC2_0 += ra2 * rb0;
         rC3_0 += ra3 * rb0;
         rC0_1 += ra0 * rb1;
         rC1_1 += ra1 * rb1;
         rC2_1 += ra2 * rb1;
         rC3_1 += ra3 * rb1;
         rC0_2 += ra0 * rb2;
         rC1_2 += ra1 * rb2;
         rC2_2 += ra2 * rb2;
         rC3_2 += ra3 * rb2;
         rC0_3 += ra0 * rb3;
         rC1_3 += ra1 * rb3;
         rC2_3 += ra2 * rb3;
         rC3_3 += ra3 * rb3;
         #ifdef ODDKB
            rA0 = *pA0; rA1 = pA0[KB]; rA2 = pA0[KB2]; rA3 = pA0[KB3]; pA0++;
            rB0 = *pB0; rB1 = pB0[KB]; rB2 = pB0[KB2]; rB3 = pB0[KB3]; pB0++;
            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0;
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1;
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2;
            rC0_3 += rA0 * rB3;
            rC1_3 += rA1 * rB3;
            rC2_3 += rA2 * rB3;
            rC3_3 += rA3 * rB3;
         #endif
         #ifdef TREAL
            *pC0 = rC0_0; pC0[1] = rC1_0; pC0[2] = rC2_0; pC0[3] = rC3_0;
            *pC1 = rC0_1; pC1[1] = rC1_1; pC1[2] = rC2_1; pC1[3] = rC3_1;
            *pC2 = rC0_2; pC2[1] = rC1_2; pC2[2] = rC2_2; pC2[3] = rC3_2;
            *pC3 = rC0_3; pC3[1] = rC1_3; pC3[2] = rC2_3; pC3[3] = rC3_3;
         #else
            *pC0 = rC0_0; pC0[2] = rC1_0; pC0[4] = rC2_0; pC0[6] = rC3_0;
            *pC1 = rC0_1; pC1[2] = rC1_1; pC1[4] = rC2_1; pC1[6] = rC3_1;
            *pC2 = rC0_2; pC2[2] = rC1_2; pC2[4] = rC2_2; pC2[6] = rC3_2;
            *pC3 = rC0_3; pC3[2] = rC1_3; pC3[4] = rC2_3; pC3[6] = rC3_3;
         #endif
         pC0 += incCm;
         pC1 += incCm;
         pC2 += incCm;
         pC3 += incCm;
         pA0 += incAm;
         pB0 += incBm;
      }
      while(pA0 != stM);
      pC0 += incCn;
      pC1 += incCn;
      pC2 += incCn;
      pC3 += incCn;
      pA0 += incAn;
      pB0 += incBn;
   }
   while(pB0 != stN);
}
#ifdef incAm
   #undef incAm
#endif
#ifdef incBn
   #undef incBn
#endif
#ifdef incCm
   #undef incCm
#endif

#ifdef ODDKB
   #undef ODDKB
#endif
#endif
