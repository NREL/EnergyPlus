/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009, 1999 R. Clint Whaley
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

#ifdef ALPHA0

void Mjoin(Mjoin(Mjoin(PATL,geadd),NM),BNM)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- alpha*A + beta*C
 */
{
   Mjoin(PATL,Mjoin(gescal,BNM))(M, N, beta, C, ldc);
}

#elif defined(BETA0)

void Mjoin(Mjoin(Mjoin(PATL,geadd),NM),BNM)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- alpha*A + beta*C
 */
{
   Mjoin(PATL,Mjoin(gemove,NM))(M, N, alpha, A, lda, C, ldc);
}

#else

#ifdef TREAL
void Mjoin(Mjoin(Mjoin(PATL,geadd),NM),BNM)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- alpha*A + beta*C
 */
{
   ATL_CINT n = N >> 1, incA = lda << 1, incC = ldc << 1;
   const TYPE *a0 = A, *a1 = A + lda;
   register ATL_INT i, j;
   TYPE *c0 = C, *c1 = C + ldc;

   for (j=n; j; j--, c0 += incC, c1 += incC, a0 += incA, a1 += incA)
   {
      for (i=0; i != M; i++)
      {
         #ifdef BETA0
            #if defined(ALPHA1)
               c0[i] = a0[i];
               c1[i] = a1[i];
            #else
               c0[i] = alpha*a0[i];
               c1[i] = alpha*a1[i];
            #endif
         #elif defined(BETA1)
            #if defined(ALPHA1)
               c0[i] += a0[i];
               c1[i] += a1[i];
            #else
               c0[i] += alpha*a0[i];
               c1[i] += alpha*a1[i];
            #endif
         #else
            #if defined(ALPHA1)
               c0[i] = beta*c0[i] + a0[i];
               c1[i] = beta*c1[i] + a1[i];
            #else
               c0[i] = beta*c0[i] + alpha*a0[i];
               c1[i] = beta*c1[i] + alpha*a1[i];
            #endif
         #endif
      }
   }
   if (N-(n<<1))
   {
      for (i=0; i != M; i++)
      {
         #ifdef BETA0
            #if defined(ALPHA1)
               c0[i] = a0[i];
            #else
               c0[i] = alpha*a0[i];
            #endif
         #elif defined(BETA1)
            #if defined(ALPHA1)
               c0[i] += a0[i];
            #else
               c0[i] += alpha*a0[i];
            #endif
         #else
            #if defined(ALPHA1)
               c0[i] = beta*c0[i] + a0[i];
            #else
               c0[i] = beta*c0[i] + alpha*a0[i];
            #endif
         #endif
      }
   }
}
#elif (defined(ALPHA0) && defined(BETA0))
void Mjoin(PATL,geadd_a0_b0)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_a0_b0)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHA0) && defined(BETA1))
void Mjoin(PATL,geadd_a0_b1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_a0_b1)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHA0) && defined(BETAXI0))
void Mjoin(PATL,geadd_a0_bXi0)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_a0_bX)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHA1) && defined(BETA0))
void Mjoin(PATL,geadd_a1_b0)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_a1_b0)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHA1) && defined(BETA1))
void Mjoin(PATL,geadd_a1_b1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_a1_b1)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHA1) && defined(BETAXI0))
void Mjoin(PATL,geadd_a1_bXi0)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_a1_bX)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHAXI0) && defined(BETA0))
void Mjoin(PATL,geadd_aXi0_b0)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_aX_b0)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHAXI0) && defined(BETA1))
void Mjoin(PATL,geadd_aXi0_b1)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_aX_b1)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#elif (defined(ALPHAXI0) && defined(BETAXI0))
void Mjoin(PATL,geadd_aXi0_bXi0)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   Mjoin(Mjoin(ATL_,UPR),geadd_aX_bX)(M<<1, N, *alpha, A, lda<<1, *beta, C, ldc<<1);
}
#else
void Mjoin(Mjoin(Mjoin(PATL,geadd),NM),BNM)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- alpha*A + beta*C
 */
{
   ATL_CINT incA = (lda-M)<<1, incC = (ldc-M)<<1;
   register ATL_INT j, i;
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   const register TYPE rbeta = *beta, ibeta = beta[1];
   register TYPE cr, ci, ar, ai, t0;

   for (j=N; j; j--, A += incA, C += incC)
   {
      for (i=M; i; i--, A += 2, C += 2)
      {
         t0 = cr = *C;
         ci = C[1];
         #ifdef BETAXI0
            cr *= rbeta;
            ci *= rbeta;
         #else
            cr = cr * rbeta - ci * ibeta;
            ci = t0 * ibeta + ci * rbeta;
         #endif

         t0 = ar = *A;
         ai = A[1];
         #ifdef ALPHAXI0
            ar *= ralpha;
            ai *= ralpha;
         #else
            ar = ar * ralpha - ai * ialpha;
            ai = t0 * ialpha + ai * ralpha;
         #endif

         cr += ar;
         ci += ai;
         *C = cr;
         C[1] = ci;
      }
   }
}
#endif

#endif
