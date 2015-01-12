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

void Mjoin(PATL,tradd)(const enum ATLAS_UPLO Uplo, ATL_CINT N, const TYPE *A,
                       ATL_CINT lda, const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * C <- beta*C + A, A & C are triangular matrices of order N
 */
{
   int j;

   #ifdef TCPLX
      ATL_CINT lda2 = lda+lda, ldc2 = ldc+ldc;
      ATL_CINT ldap1 = lda2 + 2, ldcp1 = ldc2+2;
      TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #else
      TYPE ONE = ATL_rone;
      ATL_CINT ldap1 = lda+1, ldcp1 = ldc+1;
      #define lda2 lda
      #define ldc2 ldc
   #endif
   if (Uplo == AtlasLower)
   {
      for (j=0; j < N; j++, A += ldap1, C += ldcp1)
         Mjoin(PATL,axpby)(N-j, ONE, A, 1, beta, C, 1);
   }
   else /* Upper */
   {
      for (j=0; j < N; j++, A += lda2, C += ldc2)
         Mjoin(PATL,axpby)(j+1, ONE, A, 1, beta, C, 1);
   }
}
