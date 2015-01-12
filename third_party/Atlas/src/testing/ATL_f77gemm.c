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
#include "atlas_tst.h"
#include "atlas_f77blas.h"

#define F77GEMM F77gemm
#define f77gemm Mjoin(PATL,f77gemm)

void f77gemm(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
             const int M, const int N, const int K, const SCALAR alpha,
             const TYPE *A, const int lda, const TYPE *B, const int ldb,
             const SCALAR beta, TYPE *C, const int ldc)
{
   #if defined(StringSunStyle)
      #if defined(ATL_FunkyInts)
         F77_INTEGER ONE=1;
      #else
         int ONE=1;
      #endif
   #elif defined(StringStructVal) || defined(StringStructPtr) || defined(StringCrayStyle)
      F77_CHAR fta;
      F77_CHAR ftb;
   #endif
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77M=M, F77N=N, F77K=K, F77lda=lda, F77ldb=ldb,
                        F77ldc=ldc;
   #else
      #define F77M M
      #define F77N N
      #define F77K K
      #define F77lda lda
      #define F77ldb ldb
      #define F77ldc ldc
   #endif
   char cta, ctb;
   if (TA == AtlasNoTrans) cta = 'N';
   else if (TA == AtlasTrans) cta = 'T';
   else if (TA == AtlasConjTrans) cta = 'C';
   if (TB == AtlasNoTrans) ctb = 'N';
   else if (TB == AtlasTrans) ctb = 'T';
   else if (TB == AtlasConjTrans) ctb = 'C';

   #if defined(StringSunStyle)
      F77GEMM(&cta, &ctb, &F77M, &F77N, &F77K, SADD alpha, A, &F77lda,
              B, &F77ldb, SADD beta, C, &F77ldc, ONE, ONE);
   #elif defined(StringCrayStyle)
      fta = ATL_C2F_TransChar(cta);
      ftb = ATL_C2F_TransChar(ctb);
      F77GEMM(fta, ftb, &F77M, &F77N, &F77K, SADD alpha, A, &F77lda, B, &F77ldb,
              SADD beta, C, &F77ldc);
   #elif defined(StringStructVal)
      fta.len = ftb.len = 1;
      fta.cp = &cta; ftb.cp = &ctb;
      F77GEMM(fta, ftb, &F77M, &F77N, &F77K, SADD alpha, A, &F77lda, B, &F77ldb,
              SADD beta, C, &F77ldc);
   #elif defined(StringStructPtr)
      fta.len = ftb.len = 1;
      fta.cp = &cta; ftb.cp = &ctb;
      F77GEMM(&fta, &ftb, &F77M, &F77N, &F77K, SADD alpha, A, &F77lda,
              B, &F77ldb, SADD beta, C, &F77ldc);
   #else
      fprintf(stderr, "\n\nF77/C interface not defined!!\n\n");
      exit(-1);
   #endif
}
