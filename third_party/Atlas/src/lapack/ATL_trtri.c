/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 2001 Peter Soendergaard
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
#include "atlas_lvl3.h"

int ATL_trtri(const enum ATLAS_ORDER Order, const enum ATLAS_UPLO Uplo,
	      const enum ATLAS_DIAG Diag, const int N, TYPE *A, const int lda)
{
   const int ldap1 = (lda+1)SHIFT;
   int i;

   if (N > 0)
   {
/*
 *    Check for singularity if nonunit
 */
      if (Diag == AtlasNonUnit)
      {
         for (i=0; i != N; i++, A += ldap1)
         {
            #ifdef TREAL
               if (*A == ATL_rzero) return(i+1);
            #else
               if (*A == ATL_rzero && A[1] == ATL_rzero) return(i+1);
            #endif
         }
         A -= N*ldap1;
      }
      if (Uplo == AtlasUpper)
      {
         if (Order == AtlasColMajor) return(ATL_trtriCU(Diag, N, A, lda));
         else return(ATL_trtriRU(Diag, N, A, lda));
      }
      else
      {
         if (Order == AtlasColMajor) return(ATL_trtriCL(Diag, N, A, lda));
         else return(ATL_trtriRL(Diag, N, A, lda));
      }
   }
   return(0);
}
