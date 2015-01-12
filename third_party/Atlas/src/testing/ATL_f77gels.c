/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2006 R. Clint Whaley
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
   #define F77GELS Mjoin(PRE,gels)
#elif defined (UpCase)
   #define F77GELS Mjoin(PREU,GELS)
#elif defined (Add_) || defined(Add__)
   #define F77GELS Mjoin(PRE,gels_)
#endif
#define f77gels Mjoin(PATL,f77gels)

int f77gels(const enum ATLAS_TRANS TA, const int M, const int N, const int NRHS,
            TYPE *A, const int lda, TYPE *B, const int ldb)
{
   #if defined(StringSunStyle)
      #if defined(ATL_FunkyInts)
         F77_INTEGER ONE=1;
      #else
         int ONE=1;
      #endif
   #elif defined(StringStructVal) || defined(StringStructPtr) || defined(StringCrayStyle)
      F77_CHAR ftrans;
   #endif
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77N=N, F77lda=lda, F77ldb=ldb, F77M=M, F77NRHS=NRHS;
      F77_INTEGER lwork, info;
   #else
      int info, lwork;
      #define F77M M
      #define F77N N
      #define F77NRHS NRHS
      #define F77lda lda
      #define F77ldb ldb
   #endif
   char ctrans;
   TYPE *work, wrk[2];

   if (TA == AtlasNoTrans) ctrans = 'N';
   else if (TA == AtlasTrans) ctrans = 'T';
   else ctrans = 'C';
   #if defined(StringSunStyle)
      #define args &ctrans, &F77M, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, \
                   work, &lwork, &info, ONE
   #elif defined(StringCrayStyle)
      ftrans = ATL_C2F_TransChar(cuplo);
      #define args ftrans, &F77M, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, \
                   work, &lwork, &info
   #elif defined(StringStructVal)
      ftrans.len = 1;
      ftrans.cp = &ctrans;
      #define args ftrans, &F77M, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, \
                   work, &lwork, &info
   #elif defined(StringStructPtr)
      ftrans.len = 1;
      ftrans.cp = &ctrans;
      #define args &ftrans, &F77M, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, \
                   work, &lwork, &info
   #else
      #define args NULL
      fprintf(stderr, "\n\nF77/C interface not defined!!\n\n");
      exit(-1);
   #endif
/*
 * Query routine for optimal workspace, allocate it, and call routine with it
 */
   work = wrk;
   lwork = -1;
   F77GELS(args);
   lwork = wrk[0];
   work = malloc(ATL_MulBySize(lwork));
   ATL_assert(work);
   info = 0;
   F77GELS(args);
   free(work);
   return(info);
}
