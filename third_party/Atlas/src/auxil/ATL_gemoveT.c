/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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
 * C <- alpha * transpose(A), C is NxM, A is MxN
 * NOTE: C is written row-wise, on assumption you are copying to a smaller
 *       matrix.  Also, the multiple writes to C will tend to drive it
 *       into non-LRU caches (using all the ways).  Should be unrolled
 *       for optimization, but perfecting the cache helps to make up some.
 */
#ifdef TCPLX
static void ATL_gemoveT_aX(ATL_CINT N, ATL_CINT M, const SCALAR alpha,
                           const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc)
{
   size_t incA = lda+lda;
   ATL_INT i;

   for (i=0; i < N; i++, A += incA, C += 2)
   #ifdef Conj_
      Mjoin(PATL,moveConj)(M, alpha, A, 1, C, ldc);
   #else
      Mjoin(PATL,cpsc)(M, alpha, A, 1, C, ldc);
   #endif
}
#else
static void ATL_gemoveT_a1(ATL_CINT N, ATL_CINT M, const SCALAR alpha,
                           const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc)
{
   ATL_INT i, j;
   ATL_CINT incA = lda - M;
   size_t incC = 1 - ldc*M;

   for (j=N; j; j--, A += incA, C += incC)
      for (i=M; i; i--, C += ldc)
         *C = *A++;
}
static void ATL_gemoveT_an1(ATL_CINT N, ATL_CINT M, const SCALAR alpha,
                            const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc)
{
   ATL_INT i, j;
   ATL_CINT incA = lda - M;
   size_t incC = 1 - ldc*M;

   for (j=N; j; j--, A += incA, C += incC)
      for (i=M; i; i--, C += ldc)
         *C = -(*A++);
}
static void ATL_gemoveT_aX(ATL_CINT N, ATL_CINT M, const SCALAR alpha,
                           const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc)
{
   ATL_INT i, j;
   ATL_CINT incA = lda - M, incC = 1 - ldc*M;

   for (j=N; j; j--, A += incA, C += incC)
      for (i=M; i; i--, C += ldc)
         *C = alpha*(*A++);
}
static void ATL_gemoveT_a0(ATL_CINT N, ATL_CINT M, const SCALAR alpha,
                           const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc)
{
   Mjoin(PATL,gezero)(M, N, C, ldc);
}
#endif

#define NB 32
#define MulByNB(n_) ((n_)<<5)
#define DivByNB(n_) ((n_)>>5)

#ifdef Conj_
void Mjoin(PATL,gemoveC)
#else
void Mjoin(PATL,gemoveT)
#endif
   (ATL_CINT N, ATL_CINT M, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    TYPE *C, ATL_CINT ldc)
/*
 * C <- alpha * transpose(A), C is NxM, A is MxN
 */
{
   ATL_INT i, j, Mb, Nb, mr, nr, nb, mb;
#ifdef TREAL
   void (*gemoveT)(ATL_CINT N, ATL_CINT M, const SCALAR alpha,
                   const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc);

   if (alpha == ATL_rzero)
      gemoveT = ATL_gemoveT_a0;
   else if (alpha == ATL_rone)
      gemoveT = ATL_gemoveT_a1;
   else if (alpha == ATL_rnone)
      gemoveT = ATL_gemoveT_an1;
    else
      gemoveT = ATL_gemoveT_aX;
#else
   #define gemoveT ATL_gemoveT_aX
#endif
/*
 * Just call unblocked code for small problems
 */
   if (M < NB || N < NB)
   {
      gemoveT(N, M, alpha, A, lda, C, ldc);
      return;
   }
/*
 * Otherwise, block the copy for TLB reuse
 */
   Mb = MulByNB(DivByNB(M));
   Mb = (Mb == M) ? M - NB : Mb;
   Nb = MulByNB(DivByNB(N));
   Nb = (Nb == N) ? N - NB : Nb;
   mr = M - Mb;
   nr = N - Nb;
/*
 * Run loops backwards, with C columnwise, so that we retain last col panal
 * of C in cache if possible
 */
   nb = mr;
   for (j=Mb; j >= 0; j -= NB)
   {
      mb = nr;
      for (i=Nb; i >= 0; i -= NB)
      {
         gemoveT(mb, nb, alpha, A+((j+i*lda)SHIFT), lda,
                 C+((i+j*ldc)SHIFT), ldc);
         mb = NB;
      }
      nb = NB;
   }
}
