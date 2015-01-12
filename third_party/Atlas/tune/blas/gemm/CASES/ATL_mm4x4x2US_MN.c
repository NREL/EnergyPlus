/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2000 R. Clint Whaley
 *
 * Code contributers : R. Clint Whaley, Viet Nguyen and Peter Strazdins
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

/*
 * NOTE: This is a direct adaption of Viet Nguyen's and Peter Strazdin's
 *       ATL_mm4x4x2US.c code for fixed NB.  I'm not sure who should hold
 *       the copyright in such a case; essentially, I did the typing, but it
 *       completely uses their design for the inner kernel.  This file handles
 *       M or N loop cleanup of arbitrary dimension.
 */
#include <atlas_misc.h>

#if !defined(KB) || KB == 0 || (KB/2)*2 != KB
   #error KB must be nonzero multiple of 2
#endif
#if !defined(NB) || NB == 0
   #define CLEANING_N
#elif ( (NB/4)*4 != NB )
   #error NB must be multiple of 4
#endif

#if !defined(MB) || MB == 0
   #define CLEANING_M
#elif ( (MB/4)*4 != MB )
   #error MB must be multiple of 4
#endif

#if defined(CLEANING_M) && defined(CLEANING_N)
   #error One of MB and NB must be defined
#endif

#if defined(CLEANING_M) || defined(CLEANING_N)
static void ATL_mm1x1x1
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=0, NB=0, KB=0,
 * lda=0, ldb=0, ldc=0, mu=1, nu=1, ku=1
 */
{
   #define Mb M
   #define Nb N
   #define Kb K
   const TYPE *stM = A + (lda*Mb);
   const TYPE *stN = B + (ldb*Nb);
   const int incAm = ((lda) - Kb), incAn = -(Mb*lda);
   const int incBm = -(Kb), incBn = (ldb);
   #ifdef TREAL
      #define incCm 1
      const int incCn = (ldc) - (Mb);
   #else
      #define incCm 2
      const int incCn = (ldc - Mb)<<1;
   #endif
   TYPE *pC0=C;
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   register int k;
   register TYPE rA0;
   register TYPE rB0;
   register TYPE rC0_0;
   do /* N-loop */
   {
      do /* M-loop */
      {
         #ifdef BETA0
            rC0_0 = ATL_rzero;
         #elif defined(BETA1)
            rC0_0 = *pC0;
         #else
            rC0_0 = *pC0 * beta;
         #endif
         for (k=K; k; k--) /* easy loop to unroll */
         {
            rA0 = *pA0++;
            rB0 = *pB0++;
            rC0_0 += rA0 * rB0;
         }
         *pC0 = rC0_0;
         pC0 += incCm;
         pA0 += incAm;
         pB0 += incBm;
      }
      while(pA0 != stM);
      pC0 += incCn;
      pA0 += incAn;
      pB0 += incBn;
   }
   while(pB0 != stN);
}
#ifdef incCm
   #undef incCm
#endif
#ifdef Mb
   #undef Mb
#endif
#ifdef Nb
   #undef Nb
#endif
#ifdef Kb
   #undef Kb
#endif

#endif

#ifdef CLEANING_M
static void ATL_mm1x4x1
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=0, NB=0, KB=0,
 * lda=0, ldb=0, ldc=0, mu=1, nu=4, ku=1
 */
{
   #define Mb M
   const int Nb = (N>>2)<<2;
   #define Kb K
   const int Kloop = K - 2;
   const TYPE *ca=A, *cb=B;
   TYPE *cc=C;
   const TYPE *stM = A + (lda*Mb);
   const TYPE *stN = B + (ldb*Nb);
   #define incAk 1
   const int incAm = ((lda) - Kb), incAn = -(Mb*lda);
   #define incBk 1
   const int incBm = -(Kb), incBn = (((ldb) << 2));
   #define incAk0 incAk
   #define incBk0 incBk
   #ifdef TREAL
      #define incCm 1
      #define ldc2 ldc
      const int incCn = (((ldc) << 2)) - (Mb);
   #else
      #define incCm 2
      const int incCn = ((((ldc) << 2)) - (Mb))<<1, ldc2=ldc<<1;
   #endif
   TYPE *pC0=C, *pC1=pC0+(ldc2), *pC2=pC1+(ldc2), *pC3=pC2+(ldc2);
   const TYPE *pA0=A;
   const TYPE *pB0=B, *pB1=pB0+(ldb), *pB2=pB1+(ldb), *pB3=pB2+(ldb);
   register int k;
   register TYPE rA0;
   register TYPE rB0, rB1, rB2, rB3;
   register TYPE m0, m1, m2, m3;
   register TYPE rC0_0, rC0_1, rC0_2, rC0_3;
   if (K < 3)
   {
      ATL_mm1x1x1(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      return;
   }
   if (pB0 != stN)
   {
      do /* N-loop */
      {
         do /* M-loop */
         {
            #ifdef BETA0
               rC0_0 = rC0_1 = rC0_2 = rC0_3 = ATL_rzero;
            #else
               rC0_0 = *pC0; rC0_1 = *pC1; rC0_2 = *pC2; rC0_3 = *pC3;
               #ifdef BETAX
                  rB3 = beta;
                  rC0_0 *= rB3; rC0_1 *= rB3; rC0_2 *= rB3; rC0_3 *= rB3;
               #endif
            #endif
/*
 *          Start pipeline
 */
            rA0 = *pA0;
            rB0 = *pB0;
            rB1 = *pB1;
            rB2 = *pB2;
            rB3 = *pB3;
            m0 = rA0 * rB0;
            m1 = rA0 * rB1;
            m2 = rA0 * rB2;
            m3 = rA0 * rB3;
            rA0 = pA0[1];
            rB0 = pB0[1];
            rB1 = pB1[1];
            rB2 = pB2[1];
            rB3 = pB3[1];

            pA0 += (incAk0);
            pB0 += (incBk0);
            pB1 += (incBk0);
            pB2 += (incBk0);
            pB3 += (incBk0);
            for (k=Kloop; k; k--) /* easy loop to unroll */
            {
               rC0_0 += m0;
               m0 = rA0 * rB0;
               rC0_1 += m1;
               m1 = rA0 * rB1;
               rC0_2 += m2;
               m2 = rA0 * rB2;
               rC0_3 += m3;
               m3 = rA0 * rB3;
               rA0 = pA0[1];
               rB0 = pB0[1];
               rB1 = pB1[1];
               rB2 = pB2[1];
               rB3 = pB3[1];
               pA0 += incAk;
               pB0 += incBk;
               pB1 += incBk;
               pB2 += incBk;
               pB3 += incBk;
            }
/*
 *          Drain pipe on last iteration of K-loop
 */
            rC0_0 += m0;
            m0 = rA0 * rB0;
            rC0_1 += m1;
            m1 = rA0 * rB1;
            rC0_2 += m2;
            m2 = rA0 * rB2;
            rC0_3 += m3;
            m3 = rA0 * rB3;
            rC0_0 += m0;
            rC0_1 += m1;
            rC0_2 += m2;
            rC0_3 += m3;
            pA0 += incAk0;
            pB0 += incBk0;
            pB1 += incBk0;
            pB2 += incBk0;
            pB3 += incBk0;
            *pC0 = rC0_0;
            *pC1 = rC0_1;
            *pC2 = rC0_2;
            *pC3 = rC0_3;
            pC0 += incCm;
            pC1 += incCm;
            pC2 += incCm;
            pC3 += incCm;
            pA0 += incAm;
            pB0 += incBm;
            pB1 += incBm;
            pB2 += incBm;
            pB3 += incBm;
         }
         while(pA0 != stM);
         pC0 += incCn;
         pC1 += incCn;
         pC2 += incCn;
         pC3 += incCn;
         pA0 += incAn;
         pB0 += incBn;
         pB1 += incBn;
         pB2 += incBn;
         pB3 += incBn;
      }
      while(pB0 != stN);
   }
   if (k=N-Nb)
      ATL_mm1x1x1(M, k, K, alpha, ca, lda, cb + (Nb*ldb), ldb, beta,
                  cc + (Nb*ldc2), ldc);
}
#ifdef ldc2
   #undef ldc2
#endif
#ifdef incAm
   #undef incAm
#endif
#ifdef incAn
   #undef incAn
#endif
#ifdef incAk
   #undef incAk
#endif
#ifdef incBm
   #undef incBm
#endif
#ifdef incBn
   #undef incBn
#endif
#ifdef incBk
   #undef incBk
#endif
#ifdef incCm
   #undef incCm
#endif
#ifdef incCn
   #undef incCn
#endif
#ifdef incCk
   #undef incCk
#endif
#ifdef Mb
   #undef Mb
#endif
#ifdef Nb
   #undef Nb
#endif
#ifdef Kb
   #undef Kb
#endif

#endif

#ifdef CLEANING_N
static void ATL_mm4x1x1
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, MB=0, NB=0, KB=0,
 * lda=0, ldb=0, ldc=0, mu=4, nu=1, ku=1
 */
{
   const int Mb = (M>>2)<<2;
   #define Nb N
   #define Kb K
   const int Kloop = K - 2;
   const TYPE *ca=A, *cb=B;
   TYPE *cc=C;
   const TYPE *stM = A + (lda*Mb);
   const TYPE *stN = B + (ldb*Nb);
   #define incAk 1
   const int incAm = ((((lda) << 2)) - Kb), incAn = -(Mb*lda);
   #define incBk 1
   const int incBm = -(Kb), incBn = (ldb);
   #define incAk0 incAk
   #define incBk0 incBk
   #ifdef TREAL
      #define incCm 4
      const int incCn = (ldc) - (Mb);
   #else
      #define incCm 8
      const int incCn = (ldc - Mb)<<1;
   #endif
   TYPE *pC0=C;
   const TYPE *pA0=A, *pA1=pA0+(lda), *pA2=pA1+(lda), *pA3=pA2+(lda);
   const TYPE *pB0=B;
   register int k;
   register TYPE rA0, rA1, rA2, rA3;
   register TYPE rB0;
   register TYPE m0, m1, m2, m3;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0;
   if (K < 3)
   {
      ATL_mm1x1x1(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      return;
   }
   if (pA0 != stM)
   {
      do /* N-loop */
      {
         do /* M-loop */
         {
            #ifdef BETA0
               rC0_0 = rC1_0 = rC2_0 = rC3_0 = ATL_rzero;
            #else
               #ifdef TREAL
                  rC0_0 = *pC0; rC1_0 = pC0[1]; rC2_0 = pC0[2]; rC3_0 = pC0[3];
               #else
                  rC0_0 = *pC0; rC1_0 = pC0[2]; rC2_0 = pC0[4]; rC3_0 = pC0[6];
               #endif
               #ifdef BETAX
                  rA3 = beta;
                  rC0_0 *= rA3; rC1_0 *= rA3; rC2_0 *= rA3; rC3_0 *= rA3;
               #endif
            #endif
/*
 *          Start pipeline
 */
            rA0 = *pA0;
            rB0 = *pB0;
            rA1 = *pA1;
            rA2 = *pA2;
            rA3 = *pA3;
            m0 = rA0 * rB0;
            m1 = rA1 * rB0;
            m2 = rA2 * rB0;
            m3 = rA3 * rB0;
            rA0 = pA0[1];
            rB0 = pB0[1];
            rA1 = pA1[1];
            rA2 = pA2[1];
            rA3 = pA3[1];

            pA0 += (incAk0);
            pA1 += (incAk0);
            pA2 += (incAk0);
            pA3 += (incAk0);
            pB0 += (incBk0);
            for (k=Kloop; k; k--) /* easy loop to unroll */
            {
               rC0_0 += m0;
               m0 = rA0 * rB0;
               rC1_0 += m1;
               m1 = rA1 * rB0;
               rC2_0 += m2;
               m2 = rA2 * rB0;
               rC3_0 += m3;
               m3 = rA3 * rB0;
               rA0 = pA0[1];
               rB0 = pB0[1];
               rA1 = pA1[1];
               rA2 = pA2[1];
               rA3 = pA3[1];
               pA0 += incAk;
               pA1 += incAk;
               pA2 += incAk;
               pA3 += incAk;
               pB0 += incBk;
            }
/*
 *          Drain pipe on last iteration of K-loop
 */
            rC0_0 += m0;
            m0 = rA0 * rB0;
            rC1_0 += m1;
            m1 = rA1 * rB0;
            rC2_0 += m2;
            m2 = rA2 * rB0;
            rC3_0 += m3;
            m3 = rA3 * rB0;
            rC0_0 += m0;
            rC1_0 += m1;
            rC2_0 += m2;
            rC3_0 += m3;
            pA0 += incAk0;
            pA1 += incAk0;
            pA2 += incAk0;
            pA3 += incAk0;
            pB0 += incBk0;
            #ifdef TREAL
               *pC0 = rC0_0; pC0[1] = rC1_0; pC0[2] = rC2_0; pC0[3] = rC3_0;
            #else
               *pC0 = rC0_0; pC0[2] = rC1_0; pC0[4] = rC2_0; pC0[6] = rC3_0;
            #endif
            pC0 += incCm;
            pA0 += incAm;
            pA1 += incAm;
            pA2 += incAm;
            pA3 += incAm;
            pB0 += incBm;
         }
         while(pA0 != stM);
         pC0 += incCn;
         pA0 += incAn;
         pA1 += incAn;
         pA2 += incAn;
         pA3 += incAn;
         pB0 += incBn;
      }
      while(pB0 != stN);
   }
   if (k=M-Mb)
      ATL_mm1x1x1(k, N, K, alpha, ca + (Mb*lda), lda, cb, ldb, beta,
                  cc + (Mb SHIFT), ldc);
}
#ifdef incAk
   #undef incAk
#endif
#ifdef incBk
   #undef incBk
#endif
#ifdef incCm
   #undef incCm
#endif
#ifdef incCn
   #undef incCn
#endif
#ifdef incCk
   #undef incCk
#endif
#ifdef Mb
   #undef Mb
#endif
#ifdef Nb
   #undef Nb
#endif
#ifdef Kb
   #undef Kb
#endif

#endif

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, lda = ldb = MB = KB = KB, ldc=0, mu=4, nu=4, ku=2
 * and register prefetch along A
 */
{
   const int Mb = (M>>2)<<2;
   const int Nb = (N>>2)<<2;
   const TYPE *ca=A, *cb=B;
   TYPE *cc=C;
   const TYPE *stM = A + lda*Mb;
   const TYPE *stN = B + ldb*Nb;
   const int incAm = (lda<<2) - KB, incAn = -(Mb*lda);
   const int incBm = 1-KB, incBn = (ldb<<2);
   #ifdef TREAL
      #define incCm 4
      const int incCn = ((((ldc) << 2)) - Mb);
      #define ldc2 ldc
   #else
      #define incCm 8
      const int incCn = ((((ldc) << 2)) - Mb)<<1, ldc2=ldc<<1;
   #endif
   TYPE *pC0=C, *pC1=pC0+(ldc2), *pC2=pC1+(ldc2),*pC3=pC2+(ldc2);
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   TYPE *bp = (TYPE *) &beta;
   const int Kstart = (KB>>1)-1;
   register int k;
   register TYPE m0, m1, m2, m3;
   register TYPE rA0, rA1, rA2, rA3;
   register TYPE ra0, ra1, ra2, ra3;
   register TYPE rB0, rB1, rB2, rB3;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC0_1, rC1_1, rC2_1, rC3_1,
                 rC0_2, rC1_2, rC2_2, rC3_2, rC0_3, rC1_3, rC2_3, rC3_3;

   if (pA0 != stM && pB0 != stN)
   {
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
               rC0_0 = *pC0; rC0_1 = *pC1; rC0_2 = *pC2; rC0_3 = *pC3;
               rC1_0 = pC0[1]; rC1_1 = pC1[1]; rC1_2 = pC2[1]; rC1_3 = pC3[1];
               rC2_0 = pC0[2]; rC2_1 = pC1[2]; rC2_2 = pC2[2]; rC2_3 = pC3[2];
               rC3_0 = pC0[3]; rC3_1 = pC1[3]; rC3_2 = pC2[3]; rC3_3 = pC3[3];
	    #else
               rC0_0 = *pC0; rC0_1 = *pC1; rC0_2 = *pC2; rC0_3 = *pC3;
               rC1_0 = pC0[2]; rC1_1 = pC1[2]; rC1_2 = pC2[2]; rC1_3 = pC3[2];
               rC2_0 = pC0[4]; rC2_1 = pC1[4]; rC2_2 = pC2[4]; rC2_3 = pC3[4];
               rC3_0 = pC0[6]; rC3_1 = pC1[6]; rC3_2 = pC2[6]; rC3_3 = pC3[6];
	    #endif
            #ifdef BETAX
               /*
                * use *bp instead of beta to avoid having compiler put beta in
                * a register, and thus register starving the loop
                */
               ra0 = *bp;
               rC0_0 *= ra0; rC0_1 *= ra0; rC0_2 *= ra0; rC0_3 *= ra0;
               rC1_0 *= ra0; rC1_1 *= ra0; rC1_2 *= ra0; rC1_3 *= ra0;
               rC2_0 *= ra0; rC2_1 *= ra0; rC2_2 *= ra0; rC2_3 *= ra0;
               rC3_0 *= ra0; rC3_1 *= ra0; rC3_2 *= ra0; rC3_3 *= ra0;
            #endif
         #endif

	 rA0 = *pA0; rA1 = pA0[KB];
	 rB0 = *pB0; rB1 = pB0[KB];
         rA2 = pA0[KB2]; rA3 = pA0[KB3]; pA0++;
         rB2 = pB0[KB2]; rB3 = pB0[KB3]; pB0++;

	 ra0 = *pA0;       m0 = rA0 * rB0;
	 ra1 = pA0[KB];      m1 = rA1 * rB0;
	 ra2 = pA0[KB2];      m2 = rA2 * rB0;
	 ra3 = pA0[KB3];     pA0++;
         for (k=Kstart; k; k--) /* easy loop to unroll */
         {
	    m3 = rA3 * rB0;
	    rB0 = *pB0;
            rC0_0 += m0;
	    m0 = rA0 * rB1;
            rC1_0 += m1;
	    m1 = rA1 * rB1;
            rC2_0 += m2;
	    m2 = rA2 * rB1;
            rC3_0 += m3;
	    m3 = rA3 * rB1;
	    rB1 = pB0[KB];
            rC0_1 += m0;
	    m0 = rA0 * rB2;
            rC1_1 += m1;
	    m1 = rA1 * rB2;
            rC2_1 += m2;
	    m2 = rA2 * rB2;
            rC3_1 += m3;
	    m3 = rA3 * rB2;
	    rB2 = pB0[KB2];
            rC0_2 += m0;
	    m0 = rA0 * rB3;
	    rA0 = *pA0;
            rC1_2 += m1;
	    m1 = rA1 * rB3;
            rC2_2 += m2;
	    m2 = rA2 * rB3;
	    rA1 = pA0[KB];
            rC3_2 += m3;
	    m3 = rA3 * rB3;
	    rB3 = pB0[KB3]; pB0++;
            rC0_3 += m0;
	    m0 = ra0 * rB0;
            rC1_3 += m1;
	    m1 = ra1 * rB0;
	    rA2 = pA0[KB2];
            rC2_3 += m2;
	    m2 = ra2 * rB0;
            rC3_3 += m3;
	    m3 = ra3 * rB0;
	    rA3 = pA0[KB3]; pA0++;

            rC0_0 += m0;
	    m0 = ra0 * rB1;
            rC1_0 += m1;
	    m1 = ra1 * rB1;
	    rB0 = *pB0;
            rC2_0 += m2;
	    m2 = ra2 * rB1;
            rC3_0 += m3;
	    m3 = ra3 * rB1;
	    rB1 = pB0[KB];
            rC0_1 += m0;
	    m0 = ra0 * rB2;
            rC1_1 += m1;
	    m1 = ra1 * rB2;
            rC2_1 += m2;
	    m2 = ra2 * rB2;
            rC3_1 += m3;
	    m3 = ra3 * rB2;
	    rB2 = pB0[KB2];
            rC0_2 += m0;
	    m0 = ra0 * rB3;
	    ra0 = *pA0;
            rC1_2 += m1;
	    m1 = ra1 * rB3;
	    ra1 = pA0[KB];
            rC2_2 += m2;
	    m2 = ra2 * rB3;
	    ra2 = pA0[KB2];
            rC3_2 += m3;
	    m3 = ra3 * rB3;
	    rB3 = pB0[KB3];
            rC0_3 += m0;
	    m0 = rA0 * rB0;
	    ra3 = pA0[KB3];
	    pB0++;
            rC1_3 += m1;
	    m1 = rA1 * rB0;
	    pA0++;
            rC2_3 += m2;
	    m2 = rA2 * rB0;
            rC3_3 += m3;
         }

	 m3 = rA3 * rB0;
	 rB0 = *pB0;
	 rC0_0 += m0;
	 m0 = rA0 * rB1;
	 rC1_0 += m1;
	 m1 = rA1 * rB1;
	 rC2_0 += m2;
	 m2 = rA2 * rB1;
	 rC3_0 += m3;
	 m3 = rA3 * rB1;
	 rB1 = pB0[KB];
	 rC0_1 += m0;
	 m0 = rA0 * rB2;
	 rC1_1 += m1;
	 m1 = rA1 * rB2; pA0 += incAm;
	 rC2_1 += m2;
	 m2 = rA2 * rB2;
	 rC3_1 += m3;
	 m3 = rA3 * rB2;
	 rB2 = pB0[KB2];
	 rC0_2 += m0;
	 m0 = rA0 * rB3;
	 rC1_2 += m1;
	 m1 = rA1 * rB3;
	 rC2_2 += m2;
	 m2 = rA2 * rB3;
	 rC3_2 += m3;
	 m3 = rA3 * rB3;
	 rB3 = pB0[KB3];
	 rC0_3 += m0;
	 m0 = ra0 * rB0;
	 rC1_3 += m1; pB0 += incBm;
	 m1 = ra1 * rB0;
	 rC2_3 += m2;
	 m2 = ra2 * rB0;
	 rC3_3 += m3;
	 m3 = ra3 * rB0;
	 rC0_0 += m0;
	 m0 = ra0 * rB1;
	 rC1_0 += m1;
	 m1 = ra1 * rB1;
	 rC2_0 += m2;
	 m2 = ra2 * rB1;
         *pC0 = rC0_0;
	 rC3_0 += m3;
	 m3 = ra3 * rB1;
         pC0[1 SHIFT] = rC1_0;
	 rC0_1 += m0;
	 m0 = ra0 * rB2;
         pC0[2 SHIFT] = rC2_0;
	 rC1_1 += m1;
	 m1 = ra1 * rB2;
         pC0[3 SHIFT] = rC3_0; pC0 += incCm;
	 rC2_1 += m2;
	 m2 = ra2 * rB2;
         *pC1 = rC0_1;
	 rC3_1 += m3;
	 m3 = ra3 * rB2;
         pC1[1 SHIFT] = rC1_1;
	 rC0_2 += m0;
	 m0 = ra0 * rB3;
         pC1[2 SHIFT] = rC2_1;
	 rC1_2 += m1;
	 m1 = ra1 * rB3;
         pC1[3 SHIFT] = rC3_1; pC1 += incCm;
	 rC2_2 += m2;
	 m2 = ra2 * rB3;
         *pC2 = rC0_2;
	 rC3_2 += m3;
	 m3 = ra3 * rB3;
         pC2[1 SHIFT] = rC1_2;
	 rC0_3 += m0;
         pC2[2 SHIFT] = rC2_2;
	 rC1_3 += m1;
         pC2[3 SHIFT] = rC3_2; pC2 += incCm;
	 rC2_3 += m2;
         *pC3 = rC0_3;
	 rC3_3 += m3;
         pC3[1 SHIFT] = rC1_3;
         pC3[2 SHIFT] = rC2_3;
         pC3[3 SHIFT] = rC3_3;
         pC3 += incCm;
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
   #ifdef CLEANING_N
      if (N == Nb) return;
      ATL_mm4x1x1(M, N-Nb, K, alpha, ca, lda, cb + (Nb*ldb), ldb,
                  beta, cc + Nb*ldc2, ldc);
   #elif defined(CLEANING_M)
      if (M == Mb) return;
      ATL_mm1x4x1(M-Mb, N, K, alpha, ca + (Mb*lda), lda, cb, ldb,
                  beta, cc + (Mb SHIFT), ldc);
   #endif
}

#undef incCm
#ifdef ldc2
   #undef ldc2
#endif
