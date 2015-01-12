/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2008 R. Clint Whaley
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
#define NB 40  /* all three matrices fit in 128 entry TLB */
#ifdef Conj_
   #define ApBt_NB Mjoin(PATL,geApBc_NB)
   #define ApAt_NB Mjoin(PATL,heApAc_NB)
void Mjoin(PATL,heApAc)
#else
   #define ApBt_NB Mjoin(PATL,geApBt_NB)
   #define ApAt_NB Mjoin(PATL,syApAt_NB)
void Mjoin(PATL,syApAt)
#endif
   (const enum ATLAS_UPLO Uplo, ATL_CINT N, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- A + A', C is Upper or Lower symmetric
 * Does by blocks in order to prevent massive TLB problems.
 */
{
   ATL_INT i, j, m, n, istart, iend;

   for (j=0; j < N; j += NB)
   {
      n = N - j;
      n = Mmin(NB, n);
      if (Uplo == AtlasLower) { istart = j; iend = N; }
      else { istart = 0; iend = j+NB; }

      for (i=istart; i < iend; i += NB)
      {
         m = N - i;
         m = Mmin(NB, m);
         if (i != j)
            ApBt_NB(m, n, A+((i+j*lda)SHIFT), lda, A+((j+i*lda)SHIFT),
                    lda, beta, C+((i+j*ldc)SHIFT), ldc);
         else
            ApAt_NB(Uplo, n, A+((i+j*lda)SHIFT), lda, beta,
                    C+((i+j*ldc)SHIFT), ldc);
      }
   }
}
