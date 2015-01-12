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

#ifdef Upper_
   #ifdef Transpose_
      #ifdef UnitDiag_
         #define ATLP UTU
         #define ATL_trcopy Mjoin(PATL,trcopyU2U_U)
      #else
         #define ATLP UTN
         #define ATL_trcopy Mjoin(PATL,trcopyU2U_N)
      #endif
   #elif defined(ConjTrans_)
      #ifdef UnitDiag_
         #define ATLP UCU
         #define ATL_trcopy Mjoin(PATL,trcopyU2Uc_U)
      #else
         #define ATLP UCN
         #define ATL_trcopy Mjoin(PATL,trcopyU2Uc_N)
      #endif
   #else
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyU2L_U)
         #define ATLP UNU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyU2L_N)
         #define ATLP UNN
      #endif
   #endif
#else
   #ifdef Transpose_
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyL2L_U)
         #define ATLP LTU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyL2L_N)
         #define ATLP LTN
      #endif
   #elif defined(ConjTrans_)
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyL2Lc_U)
         #define ATLP LCU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyL2Lc_N)
         #define ATLP LCN
      #endif
   #else
      #ifdef UnitDiag_
         #define ATL_trcopy Mjoin(PATL,trcopyL2U_U)
         #define ATLP LNU
      #else
         #define ATL_trcopy Mjoin(PATL,trcopyL2U_N)
         #define ATLP LNN
      #endif
   #endif
#endif

void Mjoin(Mjoin(PATL,trmmL),ATLP)
   (const int M, const int N, const void *valpha, const void *A, const int lda,
    void *C, const int ldc)
{
   #ifdef TREAL
      const SCALAR alpha=*( (const SCALAR *)valpha );
      const SCALAR one=1.0, zero=0.0;
   #else
      const TYPE zero[2]={0.0,0.0};
      #define alpha valpha
   #endif
   void *va;
   TYPE *a;

   if (N > TRMM_Xover)
   {
      va = malloc(ATL_Cachelen + ATL_MulBySize(M)*M);
      ATL_assert(va);
      a = ATL_AlignPtr(va);
      #ifdef TREAL
         if ( SCALAR_IS_ONE(alpha) ) Mjoin(ATL_trcopy,_a1)(M, alpha, A, lda, a);
         else Mjoin(ATL_trcopy,_aX)(M, alpha, A, lda, a);
         CAgemmTN(M, N, M, one, a, M, C, ldc, zero, C, ldc);
      #else
         ATL_trcopy(M, A, lda, a);
         CAgemmTN(M, N, M, valpha, a, M, C, ldc, zero, C, ldc);
      #endif
      free(va);
   }
   else Mjoin(PATL,reftrmm)(AtlasLeft, Uplo_, Trans_, Unit_, M, N, alpha,
                            A, lda, C, ldc);
}
