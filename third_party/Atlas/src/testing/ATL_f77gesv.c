/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2007 R. Clint Whaley
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
   #define F77GESV Mjoin(PRE,gesv)
#elif defined (UpCase)
   #define F77GESV Mjoin(PREU,GESV)
#elif defined (Add_) || defined(Add__)
   #define F77GESV Mjoin(PRE,gesv_)
#endif
#define f77gesv Mjoin(PATL,f77gesv)

int f77gesv(const int N, const int NRHS, TYPE *A, const int lda,
            int *ipiv, TYPE *B, const int ldb)
{
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77N=N, F77lda=lda, F77ldb=ldb, F77NRHS=NRHS;
      F77_INTEGER info;
      F77_INTEGER *F77ipiv;
   #else
      int info;
      #define F77N N
      #define F77NRHS NRHS
      #define F77lda lda
      #define F77ldb ldb
      #define F77ipiv ipiv
   #endif
   int i;
   #ifdef ATL_FunkyInts
      F77ipiv = malloc(N*sizeof(F77_INTEGER));
      ATL_assert(F77ipiv);
   #endif
   F77GESV(&F77N, &F77NRHS, A, &F77lda, F77ipiv, B, &F77ldb, &info);
   #ifdef ATL_FunkyInts
      for (i=0; i < N; i++) ipiv[i] = F77ipiv[i] - 1;
      free(F77ipiv);
   #else
      for (i=0; i < N; i++) ipiv[i]--;
   #endif
   return(info);
}
