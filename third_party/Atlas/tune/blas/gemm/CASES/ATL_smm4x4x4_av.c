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

#ifdef ATL_ARCH_PPCG5
   #undef  ATL_pfavR
   #undef  ATL_pfavW
   #define ATL_pfavR(a_, ctl_, st_)
   #define ATL_pfavW(a_, ctl_, st_)
   #define LINEFETCH
#endif

#ifdef KB
   #if (KB/4)*4 != KB || KB == 0
      #error KB must be multiple of 4!
   #endif
#else
   #error KB must be compile time constant!
#endif
#ifdef MB
   #if (MB/4)*4 != MB
      #error MB must by multiple of 4!
   #endif
#endif
#ifdef NB
   #if (NB/4)*4 != NB
      #error NB must by multiple of 4!
   #endif
#endif

#define VecReorder(v0, v1, v2, v3) \
{ \
   vA0 = vec_mergeh(v0, v2); \
   vA2 = vec_mergel(v0, v2); \
   vA1 = vec_mergeh(v1, v3); \
   vA3 = vec_mergel(v1, v3); \
   v0  = vec_mergeh(vA0, vA1); \
   v2  = vec_mergel(vA0, vA1); \
   v1  = vec_mergeh(vA2, vA3); \
   v3  = vec_mergel(vA2, vA3); \
}
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
   void *vC;
   TYPE *tC;
   TYPE *pC0=C, *pC1=pC0+(ldc SHIFT), *pC2=pC1+(ldc SHIFT),*pC3=pC2+(ldc SHIFT);
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   #ifdef LINEFETCH
      const TYPE *pfA = pA0 - incAn;
      const TYPE *pfB;
   #endif
   register int k;
   register TYPE rA0;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC0_1, rC1_1, rC2_1, rC3_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC0_3, rC1_3, rC2_3, rC3_3;
   vector float vA0, vA1, vA2, vA3, vB0, vB1, vB2, vB3;
   vector float  vC0_0, vC1_0, vC2_0, vC3_0, vC0_1, vC1_1, vC2_1, vC3_1,
                 vC0_2, vC1_2, vC2_2, vC3_2, vC0_3, vC1_3, vC2_3, vC3_3;
   const vector float nzero =  VECTOR_INIT(-0.0f, -0.0f, -0.0f, -0.0f);
   #ifndef ATL_NoIEEE
      #ifdef ATL_AVgcc
         const vector int izero   =  VECTOR_INITI(0,0,0,0);
         vec_mtvscr(izero);

      #else
         vec_mtvscr((vector unsigned long)(0));
      #endif
   #endif

   vC = malloc(ATL_Cachelen + sizeof(float)*16);
   ATL_assert( vC && ( ((M>>2)<<2) == M ) && ( ((N>>2)<<2) == N ) );
   tC = ATL_AlignPtr(vC);

   #ifndef LINEFETCH
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
      #ifdef LINEFETCH
         pfB = pB0 + KB3;
      #endif
      do /* M-loop */
      {
         vC0_0 = nzero;
         #ifdef LINEFETCH
            ATL_pfl1R(pfB);
            pfB += 16;
         #endif
         vC1_0 = vC2_0 = vC3_0 = vC0_0;
         vC0_1 = vC1_1 = vC2_1 = vC3_1 = vC0_0;
         vC0_2 = vC1_2 = vC2_2 = vC3_2 = vC0_0;
         vC0_3 = vC1_3 = vC2_3 = vC3_3 = vC0_0;
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
         #endif
         vA0 = vec_ld(0, pA0);
         vA1 = vec_ld(0, pA0+KB);
         vA2 = vec_ld(0, pA0+KB2);
         vA3 = vec_ld(0, pA0+KB3); pA0 += 4;

         vB0 = vec_ld(0, pB0);
         vB1 = vec_ld(0, pB0+KB);
         vB2 = vec_ld(0, pB0+KB2);
         vB3 = vec_ld(0, pB0+KB3); pB0 += 4;

         for (k=(KB>>2)-1; k; k--)
         {
            vC0_0 = vec_madd(vA0, vB0, vC0_0);
            vC1_0 = vec_madd(vA1, vB0, vC1_0);
            vC2_0 = vec_madd(vA2, vB0, vC2_0);
            vC3_0 = vec_madd(vA3, vB0, vC3_0); vB0 = vec_ld(0, pB0);

            vC0_1 = vec_madd(vA0, vB1, vC0_1);
            vC1_1 = vec_madd(vA1, vB1, vC1_1);
            vC2_1 = vec_madd(vA2, vB1, vC2_1);
            vC3_1 = vec_madd(vA3, vB1, vC3_1); vB1 = vec_ld(0, pB0+KB);

            vC0_2 = vec_madd(vA0, vB2, vC0_2);
            vC1_2 = vec_madd(vA1, vB2, vC1_2);
            vC2_2 = vec_madd(vA2, vB2, vC2_2);
            vC3_2 = vec_madd(vA3, vB2, vC3_2); vB2 = vec_ld(0, pB0+KB2);

            vC0_3 = vec_madd(vA0, vB3, vC0_3); vA0 = vec_ld(0, pA0);
            vC1_3 = vec_madd(vA1, vB3, vC1_3); vA1 = vec_ld(0, pA0+KB);
            vC2_3 = vec_madd(vA2, vB3, vC2_3); vA2 = vec_ld(0, pA0+KB2);
            vC3_3 = vec_madd(vA3, vB3, vC3_3); vB3 = vec_ld(0, pB0+KB3);
                    vA3 = vec_ld(0, pA0+KB3);
            pA0 += 4;
            pB0 += 4;
         }
         vC0_0 = vec_madd(vA0, vB0, vC0_0); ATL_pfavW(pC0, cwrdC, 0);
         vC1_0 = vec_madd(vA1, vB0, vC1_0); ATL_pfavW(pC1, cwrdC, 1);
         vC2_0 = vec_madd(vA2, vB0, vC2_0); ATL_pfavW(pC2, cwrdC, 2);
         vC3_0 = vec_madd(vA3, vB0, vC3_0); ATL_pfavW(pC3, cwrdC, 3);

         vC0_1 = vec_madd(vA0, vB1, vC0_1);
         vC1_1 = vec_madd(vA1, vB1, vC1_1);
         vC2_1 = vec_madd(vA2, vB1, vC2_1);
         vC3_1 = vec_madd(vA3, vB1, vC3_1);

         vC0_2 = vec_madd(vA0, vB2, vC0_2);
         vC1_2 = vec_madd(vA1, vB2, vC1_2);
         vC2_2 = vec_madd(vA2, vB2, vC2_2);
         vC3_2 = vec_madd(vA3, vB2, vC3_2);

         vC0_3 = vec_madd(vA0, vB3, vC0_3);
         vC1_3 = vec_madd(vA1, vB3, vC1_3);
         vC2_3 = vec_madd(vA2, vB3, vC2_3);
         vC3_3 = vec_madd(vA3, vB3, vC3_3);

         VecReorder(vC0_0, vC1_0, vC2_0, vC3_0);
         VecReorder(vC0_1, vC1_1, vC2_1, vC3_1);
         vC0_0 = vec_add(vC0_0, vC1_0);
         vC0_1 = vec_add(vC0_1, vC1_1);
         vC2_0 = vec_add(vC2_0, vC3_0); ATL_pfavR(pA0+KB3, cwrdKB, 0);
         vC2_1 = vec_add(vC2_1, vC3_1); ATL_pfavR(pA0+KB4, cwrdKB, 1);
         vC0_0 = vec_add(vC0_0, vC2_0); ATL_pfavR(pA0+KB5, cwrdKB, 2);
         vC0_1 = vec_add(vC0_1, vC2_1); ATL_pfavR(pA0+KB6, cwrdKB, 3);

         VecReorder(vC0_2, vC1_2, vC2_2, vC3_2);
         VecReorder(vC0_3, vC1_3, vC2_3, vC3_3);
         vC0_2 = vec_add(vC0_2, vC1_2);
         vC0_3 = vec_add(vC0_3, vC1_3);
         #ifdef LINEFETCH
            ATL_pfl1R(pfA);
            pfA += 16;
         #endif
         vC2_2 = vec_add(vC2_2, vC3_2);
         vC2_3 = vec_add(vC2_3, vC3_3);
         vC0_2 = vec_add(vC0_2, vC2_2);
         vC0_3 = vec_add(vC0_3, vC2_3);
         vec_st(vC0_0, 0, tC);
         vec_st(vC0_1, 0, tC+4);
         vec_st(vC0_2, 0, tC+8);
         vec_st(vC0_3, 0, tC+12);
         #ifdef BETAX
            rA0 = beta;
            rC0_0 = rC0_0*rA0 + *tC;
            rC1_0 = rC1_0*rA0 + tC[1];
            rC2_0 = rC2_0*rA0 + tC[2];
            rC3_0 = rC3_0*rA0 + tC[3];

            rC0_1 = rC0_1*rA0 + tC[4];
            rC1_1 = rC1_1*rA0 + tC[5];
            rC2_1 = rC2_1*rA0 + tC[6];
            rC3_1 = rC3_1*rA0 + tC[7];

            rC0_2 = rC0_2*rA0 + tC[8];
            rC1_2 = rC1_2*rA0 + tC[9];
            rC2_2 = rC2_2*rA0 + tC[10];
            rC3_2 = rC3_2*rA0 + tC[11];

            rC0_3 = rC0_3*rA0 + tC[12];
            rC1_3 = rC1_3*rA0 + tC[13];
            rC2_3 = rC2_3*rA0 + tC[14];
            rC3_3 = rC3_3*rA0 + tC[15];
         #else
            rC0_0 += *tC; rC1_0 += tC[1]; rC2_0 += tC[2]; rC3_0 += tC[3];
            rC0_1 += tC[4]; rC1_1 += tC[5]; rC2_1 += tC[6]; rC3_1 += tC[7];
            rC0_2 += tC[8]; rC1_2 += tC[9]; rC2_2 += tC[10]; rC3_2 += tC[11];
            rC0_3 += tC[12]; rC1_3 += tC[13]; rC2_3 += tC[14]; rC3_3 += tC[15];
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
   free(vC);
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
