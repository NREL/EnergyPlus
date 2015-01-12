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
#define ATL_NoFakePF
#include "atlas_prefetch.h"

#ifdef MB
   #if (MB/4)*4 != MB
      #error This kernel requires MB be a multiple of 4!!
   #endif
#endif
#ifdef NB
   #if (NB/4)*4 != NB
      #error This kernel requires NB be a multiple of 4!!
   #endif
#endif
#ifndef KB
   #error This kernel requires constant KB
#elif KB != 56 || KB == 0
   #error KB must be 56!!
#endif

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with muladd=1, TA=T, TB=N, mu=4, nu=4, ku=2, prefetching A and B
 */
{
   const TYPE *stM = A + KB*M;
   const TYPE *stN = B + KB*N;
   #ifdef ATL_AltiVec
      int blkstride, cwrdKB, cwrdC=ATL_MulBySize(8);
   #endif
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
   register TYPE rA0, rA1, rA2, rA3, rB0, rB1, rB2, rB3;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC0_1, rC1_1, rC2_1, rC3_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC0_3, rC1_3, rC2_3, rC3_3;

   #ifdef ATL_AltiVec
/*
 *    k is blkcount, cwrdKB is block size
 */
      k = 1; /* blkcount set to 1 unless KB too large */
      cwrdKB = (ATL_MulBySize(KB)+15) >> 4;  /* # of 16-byte words in KB */
      while (cwrdKB > 32)
      {
         cwrdKB >>= 1;
         k <<= 1;
      }
      if (cwrdKB == 32) cwrdKB = 0;
      blkstride = (KB * sizeof(TYPE)) / k;
      ATL_pfavR(A, ATL_GetCtrl(blkstride, k*KB, cwrdKB), 3);
      cwrdKB = ATL_GetCtrl(blkstride, k, cwrdKB);
      if (cwrdC >= 16) cwrdC >>= 4;
      else cwrdC = 1;
      cwrdC = ATL_GetCtrl(0, 1, cwrdC);
   #endif
   do /* N-loop */
   {
      ATL_pfavR(pB0, cwrdKB, 0);
      ATL_pfavR(pB0+KB , cwrdKB, 1);
      ATL_pfavR(pB0+KB2, cwrdKB, 2);
      ATL_pfavR(pB0+KB3, cwrdKB, 3);
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
         rA0 = *pA0; rA1 = pA0[KB]; rA2 = pA0[KB2]; rA3 = pA0[KB3];
         rB0 = *pB0; rB1 = pB0[KB]; rB2 = pB0[KB2]; rB3 = pB0[KB3];
         {
            rC0_0 += rA0 * rB0;
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[1];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+1];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+1];
            rC0_3 += rA0 * rB3; rA0 = pA0[1];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+1];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+1];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+1];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+1];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[2];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+2];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+2];
            rC0_3 += rA0 * rB3; rA0 = pA0[2];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+2];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+2];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+2];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+2];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[3];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+3];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+3];
            rC0_3 += rA0 * rB3; rA0 = pA0[3];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+3];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+3];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+3];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+3];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[4];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+4];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+4];
            rC0_3 += rA0 * rB3; rA0 = pA0[4];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+4];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+4];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+4];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+4];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[5];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+5];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+5];
            rC0_3 += rA0 * rB3; rA0 = pA0[5];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+5];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+5];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+5];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+5];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[6];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+6];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+6];
            rC0_3 += rA0 * rB3; rA0 = pA0[6];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+6];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+6];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+6];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+6];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[7];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+7];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+7];
            rC0_3 += rA0 * rB3; rA0 = pA0[7];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+7];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+7];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+7];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+7];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[8];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+8];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+8];
            rC0_3 += rA0 * rB3; rA0 = pA0[8];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+8];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+8];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+8];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+8];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[9];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+9];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+9];
            rC0_3 += rA0 * rB3; rA0 = pA0[9];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+9];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+9];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+9];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+9];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[10];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+10];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+10];
            rC0_3 += rA0 * rB3; rA0 = pA0[10];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+10];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+10];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+10];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+10];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[11];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+11];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+11];
            rC0_3 += rA0 * rB3; rA0 = pA0[11];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+11];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+11];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+11];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+11];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[12];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+12];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+12];
            rC0_3 += rA0 * rB3; rA0 = pA0[12];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+12];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+12];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+12];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+12];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[13];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+13];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+13];
            rC0_3 += rA0 * rB3; rA0 = pA0[13];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+13];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+13];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+13];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+13];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[14];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+14];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+14];
            rC0_3 += rA0 * rB3; rA0 = pA0[14];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+14];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+14];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+14];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+14];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[15];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+15];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+15];
            rC0_3 += rA0 * rB3; rA0 = pA0[15];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+15];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+15];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+15];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+15];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[16];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+16];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+16];
            rC0_3 += rA0 * rB3; rA0 = pA0[16];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+16];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+16];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+16];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+16];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[17];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+17];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+17];
            rC0_3 += rA0 * rB3; rA0 = pA0[17];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+17];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+17];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+17];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+17];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[18];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+18];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+18];
            rC0_3 += rA0 * rB3; rA0 = pA0[18];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+18];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+18];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+18];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+18];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[19];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+19];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+19];
            rC0_3 += rA0 * rB3; rA0 = pA0[19];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+19];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+19];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+19];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+19];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[20];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+20];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+20];
            rC0_3 += rA0 * rB3; rA0 = pA0[20];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+20];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+20];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+20];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+20];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[21];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+21];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+21];
            rC0_3 += rA0 * rB3; rA0 = pA0[21];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+21];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+21];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+21];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+21];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[22];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+22];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+22];
            rC0_3 += rA0 * rB3; rA0 = pA0[22];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+22];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+22];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+22];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+22];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[23];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+23];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+23];
            rC0_3 += rA0 * rB3; rA0 = pA0[23];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+23];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+23];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+23];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+23];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[24];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+24];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+24];
            rC0_3 += rA0 * rB3; rA0 = pA0[24];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+24];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+24];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+24];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+24];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[25];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+25];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+25];
            rC0_3 += rA0 * rB3; rA0 = pA0[25];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+25];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+25];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+25];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+25];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[26];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+26];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+26];
            rC0_3 += rA0 * rB3; rA0 = pA0[26];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+26];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+26];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+26];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+26];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[27];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+27];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+27];
            rC0_3 += rA0 * rB3; rA0 = pA0[27];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+27];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+27];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+27];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+27];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[28];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+28];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+28];
            rC0_3 += rA0 * rB3; rA0 = pA0[28];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+28];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+28];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+28];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+28];
            rC1_0 += rA1 * rB0; pA0 += 28;
            rC2_0 += rA2 * rB0; pB0 += 28;
            rC3_0 += rA3 * rB0; rB0 = pB0[1];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+1];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+1];
            rC0_3 += rA0 * rB3; rA0 = pA0[1];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+1];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+1];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+1];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+1];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[2];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+2];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+2];
            rC0_3 += rA0 * rB3; rA0 = pA0[2];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+2];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+2];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+2];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+2];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[3];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+3];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+3];
            rC0_3 += rA0 * rB3; rA0 = pA0[3];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+3];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+3];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+3];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+3];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[4];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+4];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+4];
            rC0_3 += rA0 * rB3; rA0 = pA0[4];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+4];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+4];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+4];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+4];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[5];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+5];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+5];
            rC0_3 += rA0 * rB3; rA0 = pA0[5];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+5];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+5];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+5];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+5];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[6];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+6];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+6];
            rC0_3 += rA0 * rB3; rA0 = pA0[6];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+6];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+6];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+6];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+6];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[7];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+7];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+7];
            rC0_3 += rA0 * rB3; rA0 = pA0[7];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+7];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+7];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+7];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+7];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[8];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+8];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+8];
            rC0_3 += rA0 * rB3; rA0 = pA0[8];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+8];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+8];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+8];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+8];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[9];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+9];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+9];
            rC0_3 += rA0 * rB3; rA0 = pA0[9];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+9];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+9];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+9];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+9];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[10];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+10];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+10];
            rC0_3 += rA0 * rB3; rA0 = pA0[10];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+10];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+10];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+10];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+10];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[11];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+11];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+11];
            rC0_3 += rA0 * rB3; rA0 = pA0[11];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+11];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+11];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+11];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+11];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[12];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+12];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+12];
            rC0_3 += rA0 * rB3; rA0 = pA0[12];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+12];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+12];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+12];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+12];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[13];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+13];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+13];
            rC0_3 += rA0 * rB3; rA0 = pA0[13];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+13];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+13];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+13];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+13];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[14];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+14];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+14];
            rC0_3 += rA0 * rB3; rA0 = pA0[14];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+14];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+14];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+14];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+14];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[15];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+15];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+15];
            rC0_3 += rA0 * rB3; rA0 = pA0[15];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+15];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+15];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+15];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+15];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[16];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+16];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+16];
            rC0_3 += rA0 * rB3; rA0 = pA0[16];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+16];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+16];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+16];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+16];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[17];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+17];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+17];
            rC0_3 += rA0 * rB3; rA0 = pA0[17];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+17];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+17];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+17];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+17];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[18];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+18];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+18];
            rC0_3 += rA0 * rB3; rA0 = pA0[18];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+18];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+18];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+18];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+18];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[19];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+19];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+19];
            rC0_3 += rA0 * rB3; rA0 = pA0[19];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+19];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+19];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+19];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+19];
            rC1_0 += rA1 * rB0; ATL_pfavW(pC0, cwrdC, 0);
            rC2_0 += rA2 * rB0; ATL_pfavW(pC1, cwrdC, 1);
            rC3_0 += rA3 * rB0; rB0 = pB0[20];
            rC0_1 += rA0 * rB1; ATL_pfavW(pC2, cwrdC, 2);
            rC1_1 += rA1 * rB1; ATL_pfavW(pC3, cwrdC, 3);
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+20];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+20];
            rC0_3 += rA0 * rB3; rA0 = pA0[20];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+20];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+20];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+20];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+20];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[21];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+21];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+21];
            rC0_3 += rA0 * rB3; rA0 = pA0[21];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+21];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+21];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+21];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+21];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[22];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+22];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+22];
            rC0_3 += rA0 * rB3; rA0 = pA0[22];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+22];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+22];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+22];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+22];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[23];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1; ATL_pfavR(pA0+KB3+28, cwrdKB, 0);
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+23];
            rC0_2 += rA0 * rB2; ATL_pfavR(pA0+KB4+28, cwrdKB, 1);
            rC1_2 += rA1 * rB2; ATL_pfavR(pA0+KB5+28, cwrdKB, 2);
            rC2_2 += rA2 * rB2; ATL_pfavR(pA0+KB6+28, cwrdKB, 3);
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+23];
            rC0_3 += rA0 * rB3; rA0 = pA0[23];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+23];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+23];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+23];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+23];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[24];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+24];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+24];
            rC0_3 += rA0 * rB3; rA0 = pA0[24];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+24];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+24];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+24];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+24];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[25];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+25];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+25];
            rC0_3 += rA0 * rB3; rA0 = pA0[25];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+25];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+25];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+25];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+25];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[26];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+26];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+26];
            rC0_3 += rA0 * rB3; rA0 = pA0[26];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+26];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+26];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+26];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+26];
            rC1_0 += rA1 * rB0;
            rC2_0 += rA2 * rB0;
            rC3_0 += rA3 * rB0; rB0 = pB0[27];
            rC0_1 += rA0 * rB1;
            rC1_1 += rA1 * rB1;
            rC2_1 += rA2 * rB1;
            rC3_1 += rA3 * rB1; rB1 = pB0[KB+27];
            rC0_2 += rA0 * rB2;
            rC1_2 += rA1 * rB2;
            rC2_2 += rA2 * rB2;
            rC3_2 += rA3 * rB2; rB2 = pB0[KB2+27];
            rC0_3 += rA0 * rB3; rA0 = pA0[27];
            rC1_3 += rA1 * rB3; rA1 = pA0[KB+27];
            rC2_3 += rA2 * rB3; rA2 = pA0[KB2+27];
            rC3_3 += rA3 * rB3; rA3 = pA0[KB3+27];

            rC0_0 += rA0 * rB0; rB3 = pB0[KB3+27];
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

            pA0 += 28;
            pB0 += 28;
         }
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
