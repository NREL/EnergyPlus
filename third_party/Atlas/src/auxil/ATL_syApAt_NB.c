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
#ifdef Conj_
   #define axpbyT Mjoin(PATL,axpbyConj)
void Mjoin(PATL,heApAc_NB)
#else
   #define axpbyT Mjoin(PATL,axpby)
void Mjoin(PATL,syApAt_NB)
#endif
   (const enum ATLAS_UPLO uplo, ATL_CINT N, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- beta*C + A + A', C is Upper or Lower symmetric
 */
{
   const TYPE *Ac, *Ar;
   #ifdef Conj_
      TYPE *C0 = C;
   #endif
   #ifdef TREAL
      #define ldc2 ldc
      #define lda2 lda
      #define ONE ATL_rone
      const int ldap1 = lda+1, ldcp1 = ldc+1;
   #else
      const int lda2 = lda+lda, ldc2 = ldc+ldc, ldap1 = lda2+2, ldcp1 = ldc2+2;
      TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #endif
   int j;

   if (uplo == AtlasUpper)
   {
      Ac = Ar = A;
      for (j=0; j < N; j++)
      {
         Mjoin(PATL,axpby)(j+1, ONE, Ac, 1, beta, C, 1);
         axpbyT(j+1, ONE, Ar, lda, ONE, C, 1);
         C += ldc2;
         Ar += 1 SHIFT;
         Ac += lda2;
      }
   }
   else
   {
      for (j=0; j < N; j++)
      {
         Mjoin(PATL,axpby)(N-j, ONE, A, 1, beta, C, 1);
         axpbyT(N-j, ONE, A, lda, ONE, C, 1);
         C += ldcp1;
         A += ldap1;
      }
   }
#ifdef Conj_
   Mjoin(PATLU,zero)(N, C0+1, ldcp1);  /* zero imag part of diagonal */
#endif
}
#ifdef TREAL
   #undef lda2
   #undef ldb2
   #undef ONE
#endif

