/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2003 R. Clint Whaley
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
/*
 * HERK actually uses real gemm, so use real blocking factors
 */
#ifdef SCPLX
   #define CMM_H
   #include "smm.h"
#elif defined(DCPLX)
   #define ZMM_H
   #include "zmm.h"
#endif
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include "atlas_level3.h"
#include "atlas_level1.h"
#include "atlas_lapack.h"
#include <math.h>

#define ATL_potrfRU Mjoin(PATL,potrfRU)
int ATL_potrfRU(const int N, TYPE *A, const int lda)
{
   TYPE *An, *Ac;
   int Nleft, Nright, ierr;
   static const TYPE ONE[2] = {ATL_rone, ATL_rzero};
   const size_t lda2=lda+lda;

   if (N > 1)
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB<<1) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      ierr = ATL_potrfRU(Nleft, A, lda);
      if (!ierr)
      {
         Ac = A + Nleft + Nleft;
         An = Ac + Nleft*lda2 ;
         cblas_trsm(CblasRowMajor, CblasLeft, CblasUpper, AtlasConjTrans,
                    CblasNonUnit, Nleft, Nright, ONE, A, lda, Ac, lda);
         cblas_herk(CblasRowMajor, CblasUpper, AtlasConjTrans, Nright, Nleft,
                    ATL_rnone, Ac, lda, ATL_rone, An, lda);
         ierr = ATL_potrfRU(Nright, An, lda);
         if (ierr) return(ierr+Nleft);
      }
      else return(ierr);
   }
   else if (N == 1)
   {
      if (*A > ATL_rzero)
      {
         *A = sqrt(*A);
         A[1] = ATL_rzero;
      }
      else return(1);
   }
   return(0);
}
