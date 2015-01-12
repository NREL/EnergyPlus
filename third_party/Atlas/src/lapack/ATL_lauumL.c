/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2001 R. Clint Whaley
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
#include "atlas_lapack.h"
#ifdef TREAL
   #define my_syrk cblas_syrk
   #define my_trans CblasTrans
#else
   #define my_syrk cblas_herk
   #define my_trans CblasConjTrans
#endif

#ifdef RowMajor_
   #define MyOrder CblasRowMajor
   #define ATL_lauumU Mjoin(PATL,lauumRU)
   #define ATL_lauumL Mjoin(PATL,lauumRL)
#else
   #define MyOrder CblasColMajor
   #define ATL_lauumU Mjoin(PATL,lauumCU)
   #define ATL_lauumL Mjoin(PATL,lauumCL)
#endif

void ATL_lauumL(const int N, TYPE *A, const int lda)
{
   int Nleft, Nright;
   #ifdef TREAL
      const TYPE one=ATL_rone;
   #else
      const TYPE one[2]={ATL_rone, ATL_rzero};
   #endif
   TYPE *G, *U0=A, *U1;

   if (N > 1)
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      #ifdef RowMajor_
         G = A + Nleft*(lda SHIFT);
         U1 = G + (Nleft SHIFT);
      #else
         G = A + (Nleft SHIFT);
         U1 = G + Nleft*(lda SHIFT);
      #endif
      ATL_lauumL(Nleft, U0, lda);
      my_syrk(MyOrder, CblasLower, my_trans, Nleft, Nright, ATL_rone,
              G, lda, ATL_rone, U0, lda);
      cblas_trmm(MyOrder, CblasLeft, CblasLower, my_trans, CblasNonUnit,
                 Nright, Nleft, one, U1, lda, G, lda);
      ATL_lauumL(Nright, U1, lda);
   }
   else *A = *A * *A;
}
