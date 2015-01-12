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

void Mjoin(PATL,Mjoin(gemove,NM))
   (ATL_CINT M0, ATL_CINT N, const SCALAR alpha0,
    const TYPE *A, ATL_CINT lda, TYPE *C, ATL_CINT ldc)
/*
 * C <- alpha * A
 */
{
#ifdef ALPHA0
   Mjoin(PATL,gezero)(M0, N, C, ldc);
#elif defined(ALPHA1)
   Mjoin(PATL,gecopy)(M0, N, A, lda, C, ldc);
#elif defined(TREAL) || defined(ALPHAXI0)
   ATL_INT i, j;
   ATL_CINT n = N>>1;
   #ifdef TREAL
      #define M   M0
      const register TYPE alpha = alpha0;
      ATL_CINT incA = lda<<1, incC = ldc<<1;
   #else
      const register TYPE ralpha = *alpha0;
      ATL_CINT M = M0<<1, incA = lda<<2, incC = ldc<<2;
   #endif
   const TYPE *A0 = A, *A1 = A+(lda SHIFT);
   TYPE *C0 = C, *C1 = C+(ldc SHIFT);

   for (j=n; j; j--, A0 += incA, A1 += incA, C0 += incC, C1 += incC)
   {
      for (i=0; i != M; i++)
      {
         C0[i] = ATL_MulByALPHA(A0[i]);
         C1[i] = ATL_MulByALPHA(A1[i]);
      }
   }
   if (N != (n<<1)) for (i=0; i != M; i++) C0[i] = ATL_MulByALPHA(A0[i]);

#elif defined(TCPLX)
   ATL_CINT incA = (lda-M0)<<1, incC = (ldc-M0)<<1;
   ATL_INT i, j;
   const register TYPE ralpha = *alpha0, ialpha = alpha0[1];
   register TYPE rtmp, itmp;

   for (j=N; j; j--, A += incA, C += incC)
   {
      for(i=M0; i; i--, A += 2, C += 2)
      {
         rtmp = *A;  itmp = A[1];
         *C = rtmp * ralpha - itmp * ialpha;
         C[1] = rtmp * ialpha + itmp * ralpha;
      }
   }
#endif
}
