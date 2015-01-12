/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *           (C) Copyright 1999 The Australian National University
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

/************************************************************************/
/*	Level 3 BLAS - UltraSPARC-tuned Matrix-Matrix multiply       	*/
/************************************************************************/

/* this file contains a LI ATLAS dgemm() implementation
 * with code optimized for the UltraSPARC I & II architectures.
 * It should be compiled with gcc 2.8.1 or later:
 *	gcc -mcpu=ultrasparc -O -fomit-frame-pointer -mtune=ultrasparc ...
 * to preserve the pipelining of operations.
 *
 * authors:
 *	Viet Nguyen (Feb 1999):	dgemm() and matrix multiply routines
 *	Peter Strazdins (1998, Aug 2000): support routines and packaging
 * from the Department of Computer Science, Australian National University
 *
 * Copyright (C) 1998-2000 The Australian National University
 *
 * Modified by Clint Whaley on Oct 2000 to add precision independence,
 * and use of ATLAS infrastructure.
 */

#include "atlas_misc.h"

#if !defined(KB) || (KB == 1) || (KB == 0)

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

#endif
static inline void __InnerLoop_(int K, const TYPE *A, int CIncA,
				const TYPE *B, int RIncB, const TYPE beta, TYPE *C, int LDC);

/***************** main driver routines *******************************/

void ATL_USERMM(int m, int n, int k,
		 const TYPE alpha, const TYPE *A, const int LdA,
		 const TYPE *B, const int LdB, const TYPE beta,
		 TYPE *C, const int LdC
		)
{
  int i, j, msize, ki;
   const int incA=LdA<<2, incB=LdB<<2, incC=(LdC<<2)SHIFT;

   #if !defined(KB) || (KB == 1) || (KB == 0)
      if (k == 1)
      {
         ATL_myger(m, n, A, B, beta, C, LdC);
         return;
      }
   #endif
  while (m>0) {				/* while : partition level */
    const TYPE *a, *b; TYPE *c;
    c = C; b = B;
    msize = m;
    for (i=-n; i<0; i+=4) {
      TYPE *cx;
      cx = c; a = A;
      for (j=-msize; j<0; j+=4) {
	__InnerLoop_(k, a, LdA, b, LdB, beta, cx, (LdC SHIFT));
	a += incA; cx += 4 SHIFT;
      } /* for j */
      b += incB; c += incC;
    } /* for i */
    A += msize*LdA; C += msize SHIFT;
    m -= msize;
  } /* while m>0 */
}

static inline void __InnerLoop_(int K, const TYPE *A, int CIncA,
				const TYPE *B, int RIncB, TYPE beta,
                                TYPE *C, int LDC)
/*
 * Post: C^T = B^T * A^T
 * Pre:  C^T is row major, LDC is RIncC
 *       RIncA (CIncA) is row(column) inc of A^T.
 *	 Similar for RIncB, CIncB.
 *       K > 1
 */
{
  register TYPE c00, c01, c02, c03, c10, c11, c12, c13;
  register TYPE c20, c21, c22, c23, c30, c31, c32, c33;
  register TYPE a0, a1, a2, a3, a0a, a1a, a2a, a3a;
  register TYPE b0, b1, b2, b3;
  register TYPE t0, t1, t2, t3;
  const register TYPE *A0, *A2, *B0, *B2;

#ifdef BETA0
c00 = c01 = c02 = c03 = c10 = c11 = c12 = c13 = c20 = c21 = c22 = c23 = c30 = c31 = c32 = c33 = 0.0;
C += 3*LDC;
A0 = A; A2 = A0 + (CIncA<<1);
B0 = B; B2 = B0 + (RIncB<<1);
#else
  c00 = *C; c01 = C[1 SHIFT];		A0 = A;
  c02 = C[2 SHIFT]; c03 = C[3 SHIFT];		C += LDC;
  c10 = *C; c11 = C[1 SHIFT];		A2 = A0 + (CIncA<<1);
  c12 = C[2 SHIFT]; c13 = C[3 SHIFT];		C += LDC;
  c20 = *C; c21 = C[1 SHIFT];		B0 = B;
  c22 = C[2 SHIFT]; c23 = C[3 SHIFT];		C += LDC;
  c30 = *C; c31 = C[1 SHIFT];		B2 = B0 + (RIncB<<1);
  c32 = C[2 SHIFT]; c33 = C[3 SHIFT];
#ifdef BETAX
   a0 = beta;
   c00 *= a0;
   c10 *= a0;
   c20 *= a0;
   c30 *= a0;
   c01 *= a0;
   c11 *= a0;
   c21 *= a0;
   c31 *= a0;
   c02 *= a0;
   c12 *= a0;
   c22 *= a0;
   c32 *= a0;
   c03 *= a0;
   c13 *= a0;
   c23 *= a0;
   c33 *= a0;
#endif
#endif

  a0 = *A0; a1 = *(A0+CIncA);		A0++;
  b0 = *B0; b1 = *(B0+RIncB);		B0++;
  a2 = *A2; a3 = *(A2+CIncA);		A2++;
  b2 = *B2; b3 = *(B2+RIncB);    	B2++;

  a0a = *A0;						t0 = b0*a0;
  a1a = *(A0+CIncA);			A0++;	t1 = b0*a1;
  a2a = *A2;						t2 = b0*a2;
  a3a = *(A2+CIncA);			A2++;

  for (K=-K+3; K<0; K+=2) {

    t3 = b0*a3;
			b0 = *B0;
							c00 += t0;
    t0 = b1*a0;
							c01 += t1;
    t1 = b1*a1;
							c02 += t2;
    t2 = b1*a2;
							c03 += t3;
    t3 = b1*a3;
			b1 = *(B0+RIncB);
							c10 += t0;
    t0 = b2*a0;
					B0++;
							c11 += t1;
    t1 = b2*a1;
							c12 += t2;
    t2 = b2*a2;
							c13 += t3;
    t3 = b2*a3;
			b2 = *B2;
							c20 += t0;
    t0 = b3*a0;
			a0 = *A0;
							c21 += t1;
    t1 = b3*a1;

							c22 += t2;
    t2 = b3*a2;
			a1 = *(A0+CIncA);
							c23 += t3;
    t3 = b3*a3;
			b3 = *(B2+RIncB);
							c30 += t0;
    t0 = b0*a0a;
					A0++;
							c31 += t1;
    t1 = b0*a1a;
			a2 = *A2;
							c32 += t2;
    t2 = b0*a2a;
					B2++;
							c33 += t3;
    t3 = b0*a3a;
			a3 = *(A2+CIncA);
							c00 += t0;
    t0 = b1*a0a;
					A2++;
							c01 += t1;
    t1 = b1*a1a;
			b0 = *B0;
							c02 += t2;
    t2 = b1*a2a;
							c03 += t3;
    t3 = b1*a3a;
			b1 = *(B0+RIncB);
							c10 += t0;
    t0 = b2*a0a;
					B0++;
							c11 += t1;
    t1 = b2*a1a;
							c12 += t2;
    t2 = b2*a2a;
							c13 += t3;
    t3 = b2*a3a;
			b2 = *B2;
							c20 += t0;
    t0 = b3*a0a;
			a0a = *A0;
							c21 += t1;
    t1 = b3*a1a;
			a1a = *(A0+CIncA);
							c22 += t2;
    t2 = b3*a2a;
			a2a = *A2;
					A0++;
							c23 += t3;
    t3 = b3*a3a;
			b3 = *(B2+RIncB);
							c30 += t0;
    t0 = b0*a0;
			a3a = *(A2+CIncA);
					B2++;
							c31 += t1;
    t1 = b0*a1;

					A2++;
							c32 += t2;
    t2 = b0*a2;

							c33 += t3;

  } /* for */

    t3 = b0*a3;
			b0 = *B0;
							c00 += t0;
    t0 = b1*a0;
							c01 += t1;
    t1 = b1*a1;
							c02 += t2;
    t2 = b1*a2;
							c03 += t3;
    t3 = b1*a3;
			b1 = *(B0+RIncB);
							c10 += t0;
    t0 = b2*a0;
					B0++;
							c11 += t1;
    t1 = b2*a1;
							c12 += t2;
    t2 = b2*a2;
							c13 += t3;

  if (K) {

    t3 = b2*a3;
			b2 = *B2;
							c20 += t0;
    t0 = b3*a0;
							c21 += t1;
    t1 = b3*a1;

							c22 += t2;
    t2 = b3*a2;
							c23 += t3;
    t3 = b3*a3;
			b3 = *(B2+RIncB);
							c30 += t0;
    t0 = b0*a0a;
							c31 += t1;
    t1 = b0*a1a;
					B2++;
							c32 += t2;
    t2 = b0*a2a;
					C -= LDC;
							c33 += t3;
    t3 = b0*a3a;
					C -= LDC;
							c00 += t0;
    t0 = b1*a0a;
					C -= LDC;
							c01 += t1;
    t1 = b1*a1a;
							c02 += t2;
    t2 = b1*a2a;
			*C = c00;
							c03 += t3;
    t3 = b1*a3a;
			C[1 SHIFT] = c01;
							c10 += t0;
    t0 = b2*a0a;
			C[2 SHIFT] = c02;
							c11 += t1;
    t1 = b2*a1a;
			C[3 SHIFT] = c03;
					C += LDC;
							c12 += t2;
    t2 = b2*a2a;
			*C = c10;
							c13 += t3;
    t3 = b2*a3a;
			C[1 SHIFT] = c11;
							c20 += t0;
    t0 = b3*a0a;
			C[2 SHIFT] = c12;
							c21 += t1;
    t1 = b3*a1a;
			C[3 SHIFT] = c13;
					C += LDC;
							c22 += t2;
    t2 = b3*a2a;
			*C = c20;
							c23 += t3;
    t3 = b3*a3a;
			C[1 SHIFT] = c21;
							c30 += t0;
			C[2 SHIFT] = c22;
							c31 += t1;
			C[3 SHIFT] = c23;
					C += LDC;
							c32 += t2;
			*C = c30;
							c33 += t3;
			C[1 SHIFT] = c31;
			C[2 SHIFT] = c32;
			C[3 SHIFT] = c33;
  } else {

    t3 = b2*a3;
			b2 = *B2;
							c20 += t0;
    t0 = b3*a0;
			a0 = *A0;
							c21 += t1;
    t1 = b3*a1;

							c22 += t2;
    t2 = b3*a2;
			a1 = *(A0+CIncA);
							c23 += t3;
    t3 = b3*a3;
			b3 = *(B2+RIncB);
							c30 += t0;
    t0 = b0*a0a;
					A0++;
							c31 += t1;
    t1 = b0*a1a;
			a2 = *A2;
							c32 += t2;
    t2 = b0*a2a;
					B2++;
							c33 += t3;
    t3 = b0*a3a;
			a3 = *(A2+CIncA);
							c00 += t0;
    t0 = b1*a0a;
					A2++;
							c01 += t1;
    t1 = b1*a1a;
			b0 = *B0;
							c02 += t2;
    t2 = b1*a2a;
							c03 += t3;
    t3 = b1*a3a;
			b1 = *(B0+RIncB);
							c10 += t0;
    t0 = b2*a0a;
					B0++;
							c11 += t1;
    t1 = b2*a1a;
							c12 += t2;
    t2 = b2*a2a;
							c13 += t3;
    t3 = b2*a3a;
			b2 = *B2;
							c20 += t0;
    t0 = b3*a0a;
							c21 += t1;
    t1 = b3*a1a;
							c22 += t2;
    t2 = b3*a2a;
							c23 += t3;
    t3 = b3*a3a;
			b3 = *(B2+RIncB);
							c30 += t0;
    t0 = b0*a0;
					B2++;
							c31 += t1;
    t1 = b0*a1;

							c32 += t2;
    t2 = b0*a2;

							c33 += t3;
    t3 = b0*a3;
					C -= LDC;
							c00 += t0;
    t0 = b1*a0;
					C -= LDC;
							c01 += t1;
    t1 = b1*a1;
					C -= LDC;
							c02 += t2;
    t2 = b1*a2;
			*C = c00;
							c03 += t3;
    t3 = b1*a3;
			C[1 SHIFT] = c01;
							c10 += t0;
    t0 = b2*a0;
			C[2 SHIFT] = c02;
							c11 += t1;
    t1 = b2*a1;
			C[3 SHIFT] = c03;
					C += LDC;
							c12 += t2;
    t2 = b2*a2;
			*C = c10;
							c13 += t3;
    t3 = b2*a3;
			C[1 SHIFT] = c11;
							c20 += t0;
    t0 = b3*a0;
			C[2 SHIFT] = c12;
							c21 += t1;
    t1 = b3*a1;
			C[3 SHIFT] = c13;
					C += LDC;
							c22 += t2;
    t2 = b3*a2;
			*C = c20;
							c23 += t3;
    t3 = b3*a3;
			C[1 SHIFT] = c21;
							c30 += t0;
			C[2 SHIFT] = c22;
							c31 += t1;
			C[3 SHIFT] = c23;
					C += LDC;
							c32 += t2;
			*C = c30;
							c33 += t3;
			C[1 SHIFT] = c31;
			C[2 SHIFT] = c32;
			C[3 SHIFT] = c33;

  } /* if (k) */
} /* __InnerLoop_() */
