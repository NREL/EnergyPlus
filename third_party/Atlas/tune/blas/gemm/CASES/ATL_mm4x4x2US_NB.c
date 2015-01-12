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
 *       completely uses their design for the inner kernel.
 */
#include <atlas_misc.h>

#if (NB != MB || NB != KB || NB != (NB/4)*4)
   #error NB must equal MB and KB, and be a multiple of 4
#endif
void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
/*
 * matmul with TA=T, TB=N, lda = ldb = MB = KB = NB, ldc=0, mu=4, nu=4, ku=2
 * and register prefetch along A
 */
{
   const TYPE *stM = A + NBNB;
   const TYPE *stN = B + NBNB;
   const int incAm = NB3, incAn = -NBNB;
   const int incBm = 1-NB;
   #define incBn NB4
   #ifdef TREAL
      #define incCm 4
   #else
      #define incCm 8
   #endif
   const int incCn = ((((ldc) << 2)) - NB)SHIFT;
   TYPE *pC0=C, *pC1=pC0+(ldc SHIFT), *pC2=pC1+(ldc SHIFT),*pC3=pC2+(ldc SHIFT);
   const TYPE *pA0=A;
   const TYPE *pB0=B;
   TYPE *bp = (TYPE *) &beta;
   const int Kstart = (NB>>1)-1;
   register int k;
   register TYPE m0, m1, m2, m3;
   register TYPE rA0, rA1, rA2, rA3;
   register TYPE ra0, ra1, ra2, ra3;
   register TYPE rB0, rB1, rB2, rB3;
   register TYPE rC0_0, rC1_0, rC2_0, rC3_0, rC0_1, rC1_1, rC2_1, rC3_1, rC0_2, rC1_2, rC2_2, rC3_2, rC0_3, rC1_3, rC2_3, rC3_3;

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
               rC0_0 *= ra0;
               rC0_1 *= ra0;
               rC0_2 *= ra0;
               rC0_3 *= ra0;
               rC1_0 *= ra0;
               rC1_1 *= ra0;
               rC1_2 *= ra0;
               rC1_3 *= ra0;
               rC2_0 *= ra0;
               rC2_1 *= ra0;
               rC2_2 *= ra0;
               rC2_3 *= ra0;
               rC3_0 *= ra0;
               rC3_1 *= ra0;
               rC3_2 *= ra0;
               rC3_3 *= ra0;
            #endif
         #endif

	 rA0 = *pA0; rA1 = pA0[NB];
	 rB0 = *pB0; rB1 = pB0[NB];
         rA2 = pA0[NB2]; rA3 = pA0[NB3]; pA0++;
         rB2 = pB0[NB2]; rB3 = pB0[NB3]; pB0++;

	 ra0 = *pA0;       m0 = rA0 * rB0;
	 ra1 = pA0[NB];      m1 = rA1 * rB0;
	 ra2 = pA0[NB2];      m2 = rA2 * rB0;
	 ra3 = pA0[NB3];     pA0++;
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
	    rB1 = pB0[NB];
            rC0_1 += m0;
	    m0 = rA0 * rB2;
            rC1_1 += m1;
	    m1 = rA1 * rB2;
            rC2_1 += m2;
	    m2 = rA2 * rB2;
            rC3_1 += m3;
	    m3 = rA3 * rB2;
	    rB2 = pB0[NB2];
            rC0_2 += m0;
	    m0 = rA0 * rB3;
	    rA0 = *pA0;
            rC1_2 += m1;
	    m1 = rA1 * rB3;
            rC2_2 += m2;
	    m2 = rA2 * rB3;
	    rA1 = pA0[NB];
            rC3_2 += m3;
	    m3 = rA3 * rB3;
	    rB3 = pB0[NB3]; pB0++;
            rC0_3 += m0;
	    m0 = ra0 * rB0;
            rC1_3 += m1;
	    m1 = ra1 * rB0;
	    rA2 = pA0[NB2];
            rC2_3 += m2;
	    m2 = ra2 * rB0;
            rC3_3 += m3;
	    m3 = ra3 * rB0;
	    rA3 = pA0[NB3]; pA0++;

            rC0_0 += m0;
	    m0 = ra0 * rB1;
            rC1_0 += m1;
	    m1 = ra1 * rB1;
	    rB0 = *pB0;
            rC2_0 += m2;
	    m2 = ra2 * rB1;
            rC3_0 += m3;
	    m3 = ra3 * rB1;
	    rB1 = pB0[NB];
            rC0_1 += m0;
	    m0 = ra0 * rB2;
            rC1_1 += m1;
	    m1 = ra1 * rB2;
            rC2_1 += m2;
	    m2 = ra2 * rB2;
            rC3_1 += m3;
	    m3 = ra3 * rB2;
	    rB2 = pB0[NB2];
            rC0_2 += m0;
	    m0 = ra0 * rB3;
	    ra0 = *pA0;
            rC1_2 += m1;
	    m1 = ra1 * rB3;
	    ra1 = pA0[NB];
            rC2_2 += m2;
	    m2 = ra2 * rB3;
	    ra2 = pA0[NB2];
            rC3_2 += m3;
	    m3 = ra3 * rB3;
	    rB3 = pB0[NB3];
            rC0_3 += m0;
	    m0 = rA0 * rB0;
	    ra3 = pA0[NB3];
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
	 rB1 = pB0[NB];
	 rC0_1 += m0;
	 m0 = rA0 * rB2;
	 rC1_1 += m1;
	 m1 = rA1 * rB2; pA0 += incAm;
	 rC2_1 += m2;
	 m2 = rA2 * rB2;
	 rC3_1 += m3;
	 m3 = rA3 * rB2;
	 rB2 = pB0[NB2];
	 rC0_2 += m0;
	 m0 = rA0 * rB3;
	 rC1_2 += m1;
	 m1 = rA1 * rB3;
	 rC2_2 += m2;
	 m2 = rA2 * rB3;
	 rC3_2 += m3;
	 m3 = rA3 * rB3;
	 rB3 = pB0[NB3];
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

#undef incBn
#undef incCm
