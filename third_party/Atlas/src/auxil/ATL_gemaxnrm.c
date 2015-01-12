/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
#include "atlas_level1.h"

#if defined(DCPLX)
   #define iamax ATL_izamax
#elif defined(SCPLX)
   #define iamax ATL_icamax
#elif defined(SREAL)
   #define iamax ATL_isamax
#else
   #define iamax ATL_idamax
#endif
TYPE Mjoin(PATL,gemaxnrm)
   (ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT lda)
/*
 * Returns maximum absolute value in A;
 * for complex value CV returns maximum of abs(real(CV)) + abs(cplx(CV))
 */
{
   TYPE maxval=0.0, mv;
   ATL_INT j, i;
   #ifdef TCPLX
      ATL_CINT lda2 = lda+lda;
   #else
      #define lda2 lda
   #endif

   for (j=0; j < N; j++, A += lda2)
   {
      i = iamax(M, A, 1);
      #ifdef TCPLX
         i += i;
         mv = Mabs(A[i]) + Mabs(A[i+1]);
      #else
         mv = Mabs(A[i]);
      #endif
      maxval = (maxval >= mv) ? maxval : mv;
   }
   return(maxval);
}
#ifndef TCPLX
   #undef lda2
#endif
