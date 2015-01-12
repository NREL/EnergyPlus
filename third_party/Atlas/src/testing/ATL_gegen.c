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
#define FILLCONST -2560000000.0

void Mjoin(PATL,gefillgap)
   (const int M, const int N, TYPE *A, const int lda0)
{
   const int lda=lda0 SHIFT, n = lda0 - M;
   int j;

   if (n)
   {
      A += M SHIFT;
      for (j=0; j < N; j++, A += lda)
         Mjoin(PATLU,set)(n SHIFT, FILLCONST, A, 1);
   }
}

int Mjoin(PATL,gechkgap)
   (const int M0, const int N, TYPE *A, const int lda0)
{
   const int M = M0 SHIFT, lda=lda0 SHIFT, n = lda0 - M0;
   int i, j, OVERWRITES=0;
   if (n)
   {
      for (j=0; j < N; j++)
      {
         for (i=M; i < lda; i++)
         {
            if (A[j*lda+i] != FILLCONST)
            {
               fprintf(stderr, "   Overwrite in lda gap, A(%d,%d) = %f!!\n",
                       i, j, A[j*lda+i]);
               OVERWRITES++;
            }
         }
      }
   }
   return(OVERWRITES);
}

void Mjoin(PATL,gegen)
   (const int M0, const int N, TYPE *A, const int lda0, const int seed)
{
   const int M = M0 SHIFT, lda = lda0 SHIFT;
   int i, j;

   dumb_seed(seed);
   Mjoin(PATL,gefillgap)(M0, N, A, lda0);
   for (j=N; j; j--)
   {
      for (i=0; i != M; i++) A[i] = dumb_rand();
      A += lda;
   }
}
