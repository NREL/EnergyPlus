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
#include "atlas_pkblas.h"

void Mjoin(PATL,pputblk)(const int M, const int N, const TYPE *V, TYPE *C,
                         int ldc, int ldcinc, const SCALAR beta)
/*
 * Given a MxN block-major block V, copy to a packed C
 *    ldcinc =  0 : General rectangular
 *    ldcinc =  1 : Upper
 *    ldcinc = -1 : Lower
 */
{
   int i, j;
   const int mn = M*N;
   const register TYPE rbeta = *beta, ibeta = beta[1];
   register TYPE rc, ic, t0;

   if (ldcinc == -1) ldc--;
   ldc -= M;
   ldc += ldc;
   if (ibeta == ATL_rzero)
   {
      if (rbeta == ATL_rzero)
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--, C += 2)
            {
               *C = V[mn];
               C[1] = *V++;
            }
            C += ldc;
            ldc += ldcinc;
         }
      }
      else if (rbeta == ATL_rone)
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--, C += 2)
            {
               *C += V[mn];
               C[1] += *V++;
            }
            C += ldc;
            ldc += ldcinc;
         }
      }
      else /* beta real X */
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--, C += 2)
            {
               *C = *C * rbeta + V[mn];
               C[1] = C[1] * rbeta + *V++;
            }
            C += ldc;
            ldc += ldcinc;
         }
      }
   }
   else /* beta = complex X */
   {
      for (j=N; j; j--)
      {
         for (i=M; i; i--, C += 2)
         {
            t0 = rc = *C; ic = C[1];
            rc = rc * rbeta - ic * ibeta;
            ic = t0 * ibeta + ic * rbeta;
            rc += V[mn];
            ic += *V++;
            *C = rc;
            C[1] = ic;
         }
         C += ldc;
         ldc += ldcinc;
      }
   }
}
