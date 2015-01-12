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

void Mjoin(PATL,gereal2cplx)
   (const int M, const int N, const TYPE *alpha, const TYPE *R, const int ldr,
    const TYPE *I, const int ldi, const TYPE *beta, TYPE *C, const int ldc)
/*
 * C = beta*C + (R,I)*alpha
 * where R is a MxN matrix representing the real components to be added into
 * the output complex matrix, and the I matrix contains the imaginary components
 */
{
   const TYPE ralp=(*alpha), ialp=alpha[1], rbet=(*beta), ibet=beta[1];
   register TYPE ra, ia, rc, ic, tmp;
   const int ldc2 = (ldc-M)<<1;
   int i, j;

/*
 * Cannot read C if BETA is 0
 */
   if (rbet == ATL_rzero && ibet == ATL_rzero)
   {
      if (ialp == ATL_rzero)  /* alpha is a real number */
      {
         if (ralp == ATL_rone) /* alpha = 1.0 */
         {
            for (j=0; j < N; j++, R += ldr, I += ldi, C += ldc2)
            {
               for (i=0; i < M; i++, C += 2)
               {
                  *C = R[i];
                  C[1] = I[i];
               }
            }
         }
         else
         {
            for (j=0; j < N; j++, R += ldr, I += ldi, C += ldc2)
            {
               for (i=0; i < M; i++, C += 2)
               {
                  *C = ralp * R[i];
                  C[1] = ralp * I[i];
               }
            }
         }
      }
      else                   /* alpha is a complex number */
      {
         for (j=0; j < N; j++, R += ldr, I += ldi, C += ldc2)
         {
            for (i=0; i < M; i++, C += 2)
            {
               ra = R[i]; ia = I[i];
               C[0] = ralp * ra - ialp * ia;
               C[1] = ralp * ia + ialp * ra;
            }
         }
      }
   }
/*
 * If alpha and beta are both real numbers
 */
   else if (ialp == ATL_rzero && ibet == ATL_rzero)
   {
      if (ralp == ATL_rone && rbet == ATL_rone)
      {
         for (j=0; j < N; j++, R += ldr, I += ldi, C += ldc2)
         {
            for (i=0; i < M; i++, C += 2)
            {
               *C += R[i];
               C[1] += I[i];
            }
         }
      }
      else  /* general case real alpha, beta case */
      {
         for (j=0; j < N; j++, R += ldr, I += ldi, C += ldc2)
         {
            for (i=0; i < M; i++, C += 2)
            {
               *C = ralp*R[i] + rbet * *C;
               C[1] = ralp*I[i] + rbet*C[1];
            }
         }
      }
   }
/*
 * General case where both alpha & beta are complex, specialize later
 */
   else
   {
      for (j=0; j < N; j++, R += ldr, I += ldi, C += ldc2)
      {
         for (i=0; i < M; i++, C += 2)
         {
            rc = *C; ic = C[1];
            ra = R[i]; ia = I[i];
            tmp = rc*rbet - ic*ibet;
            ic = rc*ibet + ic*rbet;
            tmp += ra*ralp - ia*ialp;
            ic += ra*ialp + ia*ralp;
            C[0] = tmp; C[1] = ic;
         }
      }
   }
}
