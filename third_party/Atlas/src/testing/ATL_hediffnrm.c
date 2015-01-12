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
TYPE Mjoin(PATL,hediffnrm)
   (const enum ATLAS_ORDER Order, const enum ATLAS_UPLO Uplo, const int N,
    const TYPE *A0, const int ld0, const TYPE *A1, const int ld1)
/*
 * Returns ||A0 - A1||, assuming both matrices are hermitian.
 */
{
   int i, j, k;
   const int ld02 = (ld0 SHIFT), ld12 = (ld1 SHIFT);
   enum ATLAS_UPLO uplo=Uplo;
   TYPE t0, t1, max=ATL_rzero;
   const TYPE *a0, *a1;
   if (Order == AtlasRowMajor)
   {
      if (Uplo == AtlasUpper) uplo = AtlasLower;
      else uplo = AtlasUpper;
   }
   if (uplo == AtlasUpper)
   {
      for (k=0; k != N; k++)
      {
         t0 = ATL_rzero;
         a0 = A0 + k*ld02;
         a1 = A1 + k*ld12;
         for (i=0; i < (k SHIFT); i++)
         {
            t1 = *a0++ - *a1++;
            t0 += Mabs(t1);
         }
         t1 = *a0 - *a1;
         t0 += Mabs(t1);
         for (i++,a0 += ld02,a1 += ld12; i < N; i++, a0 += ld02, a1 += ld12)
         {
            t1 = *a0 - *a1;
            t0 += Mabs(t1);
            #ifdef TCPLX
               t1 = a0[1] - a1[1];
               t0 += Mabs(t1);
            #endif
         }
         if (t0 > max) max = t0;
      }
   }
   else  /* matrix stored in lower triangle */
   {
      for (k=0; k != N; k++)
      {
         t0 = ATL_rzero;
         for (i=0; i != k; i++, a0 += ld02, a1 += ld12)
         {
            t1 = *a0 - *a1;
            t0 += Mabs(t1);
            #ifdef TCPLX
               t1 = a0[1] - a1[1];
               t0 += Mabs(t1);
            #endif
         }
         t1 = *a0++ - *a1++;
         t0 += Mabs(t1);
         #ifdef TCPLX
            a0++; a1++;
         #endif
         for (i++; i < N; i++)
         {
            t1 = *a0++ - *a1++;
            t0 += Mabs(t1);
            #ifdef TCPLX
               t1 = *a0++ - *a1++;
               t0 += Mabs(t1);
            #endif
         }
         if (t0 > max) max = t0;
      }
   }
   return(max);
}
