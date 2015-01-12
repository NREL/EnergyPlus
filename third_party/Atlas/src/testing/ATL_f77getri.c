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

#include "atlas_misc.h"
#include "atlas_tst.h"
#include "atlas_f77.h"

#if defined(NoChange)
   #define F77GETRI Mjoin(PRE,getri)
#elif defined (UpCase)
   #define F77GETRI Mjoin(PREU,GETRI)
#elif defined (Add_) || defined(Add__)
   #define F77GETRI Mjoin(PRE,getri_)
#endif
#define f77getri Mjoin(PATL,f77getri)

int f77getri(const enum ATLAS_ORDER Order, const int N, TYPE *A, const int lda,
             int *ipiv, TYPE *work, int *lwork)
{
   int i;
   const int MN=N;
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77N=N, F77lda=lda, F77lwork=(*lwork);
      F77_INTEGER info, *F77ipiv;
   #else
      int info;
      #define F77M M
      #define F77N N
      #define F77lda lda
      #define F77ipiv ipiv
      #define F77lwork lwork
      #define F77lwork (*lwork)
   #endif
   ATL_assert(Order == AtlasColMajor);
      #define F77ipiv ipiv
   #ifdef ATL_FunkyInts
      F77ipiv = malloc(MN * sizeof(F77_INTEGER));
      ATL_assert(F77ipiv);
   #endif

   #ifdef ATL_FunkyInts
      *lwork = F77lwork;
      for (i=0; i < MN; i++) ipiv[i] = F77ipiv[i] + 1;
      free(F77ipiv);
   #else
      for (i=0; i < MN; i++) ipiv[i]++;
   #endif
   F77GETRI(&F77N, A, &F77lda, F77ipiv, work, &F77lwork, &info);
   #ifdef ATL_FunkyInts
      for (i=0; i < MN; i++) ipiv[i] = F77ipiv[i] - 1;
      free(F77ipiv);
   #else
      for (i=0; i < MN; i++) ipiv[i]--;
   #endif
   return(info);
}
