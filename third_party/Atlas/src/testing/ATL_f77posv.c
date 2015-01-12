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
   #define F77POSV Mjoin(PRE,posv)
#elif defined (UpCase)
   #define F77POSV Mjoin(PREU,POSV)
#elif defined (Add_) || defined(Add__)
   #define F77POSV Mjoin(PRE,posv_)
#endif
#define f77posv Mjoin(PATL,f77posv)

int f77posv(const enum ATLAS_UPLO UPLO, const int N, const int NRHS,
            TYPE *A, const int lda, TYPE *B, const int ldb)
{
   #if defined(StringSunStyle)
      #if defined(ATL_FunkyInts)
         F77_INTEGER ONE=1;
      #else
         int ONE=1;
      #endif
   #elif defined(StringStructVal) || defined(StringStructPtr) || defined(StringCrayStyle)
      F77_CHAR fuplo;
   #endif
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77N=N, F77lda=lda, F77ldb=ldb, F77NRHS=NRHS;
      F77_INTEGER info;
   #else
      int info;
      #define F77N N
      #define F77NRHS NRHS
      #define F77lda lda
      #define F77ldb ldb
   #endif
   char cuplo;
   cuplo = (UPLO == AtlasUpper) ? 'U' : 'L';
   #if defined(StringSunStyle)
      #define args &cuplo, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, &info, ONE
   #elif defined(StringCrayStyle)
      ftrans = ATL_C2F_TransChar(cuplo);
      #define args cuplo, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, &info
   #elif defined(StringStructVal)
      fuplo.len = 1;
      fuplo.cp = &cuplo;
      #define args fuplo, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, &info
   #elif defined(StringStructPtr)
      fuplo.len = 1;
      fuplo.cp = &cuplo;
      #define args &fuplo, &F77N, &F77NRHS, A, &F77lda, B, &F77ldb, &info
   #else
      #define args NULL
      fprintf(stderr, "\n\nF77/C interface not defined!!\n\n");
      exit(-1);
   #endif
   F77POSV(args);
   return(info);
}
