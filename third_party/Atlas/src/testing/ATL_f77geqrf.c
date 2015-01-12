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
#include "atlas_tst.h"
#include "atlas_f77.h"

#if defined(NoChange)
   #define F77GEQRF Mjoin(PRE,geqrf)
#elif defined (UpCase)
   #define F77GEQRF Mjoin(PREU,GEQRF)
#elif defined (Add_) || defined(Add__)
   #define F77GEQRF Mjoin(PRE,geqrf_)
#endif
#define f77geqrf Mjoin(PATL,f77geqrf)

int f77geqrf(const enum ATLAS_ORDER Order, const int M, const int N,
             TYPE *A, const int lda, TYPE *tau, TYPE *work, int lwork)
{
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77M=M, F77N=N, F77lda=lda, F77lwork=lwork, F77info;
   #else
      int F77info;
      #define F77M M
      #define F77N N
      #define F77lda lda
      #define F77lwork lwork
   #endif
   ATL_assert(Order == AtlasColMajor);

   F77GEQRF(&F77M, &F77N, A, &F77lda, tau, work, &F77lwork, &F77info);

   return(F77info);
}
