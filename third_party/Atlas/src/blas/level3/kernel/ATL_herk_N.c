/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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
#include "atlas_kern3.h"

#define her_put Mjoin(Mjoin(PATL,heput),UploNM)
void Mjoin(Mjoin(Mjoin(PATL,herk),UploNM),N)
   (const int N, const int K, const void *valpha, const void *A, const int lda,
    const void *vbeta, void *C, const int ldc)
{
   void *vc;
   TYPE *c;
   TYPE alpha[2];
   const TYPE beta = *( (const TYPE *)vbeta  );
   const TYPE zero[2] = {0.0, 0.0};

   alpha[0] = *( (const TYPE *)valpha );
   if (K > HERK_Xover)
   {
      alpha[1] = 0.0;
      vc = malloc(ATL_Cachelen+ATL_MulBySize(N)*N);
      ATL_assert(vc);
      c = ATL_AlignPtr(vc);
      CgemmNC(N, N, K, alpha, A, lda, A, lda, zero, c, N);
      if ( beta == 1.0 ) Mjoin(her_put,_b1)(N, c, vbeta, C, ldc);
      else if ( beta == 0.0 ) Mjoin(her_put,_b0)(N, c, vbeta, C, ldc);
      else Mjoin(her_put,_bXi0)(N, c, vbeta, C, ldc);
      free(vc);
   }
   else Mjoin(PATL,refherk)(Uplo_, AtlasNoTrans, N, K, *alpha, A, lda,
                            beta, C, ldc);
}
