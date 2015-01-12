/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2011, 2010 Vesperix Corporation
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
#ifndef ATL_GAS_ARM
   #error "This routine requires GAS/ARM assembly"
#endif
#ifndef ATL_NEON
   #error "This routine requires an ARM NEON SIMD unit!"
#endif
#ifndef ATL_NONIEEE
   #error "This NEON routine requires turning off IEEE compliance!"
#endif


/*

  ATLAS 3.9.58

  Copyright (c) 2010-11, Vesperix Corporation. All rights reserved.

  Redistribution and use in source and binary forms, with or without modification, are
  permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the names of Vesperix Corporation, nor the names of its contributors
      may be used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
  SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 */

#include "atlas_misc.h"

#if (__GNUC__ * 100 + __GNUC_MINOR__) < 405
   #error Versions of GCC earlier than 4.5 have serious bugs in ARM assembly support -- skipping NEON GEMM kernel!
#endif

void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda,
                const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc)
{
   int i, j, k;

   register double D24 asm ("d24");
   register double D25 asm ("d25");
   register double D26 asm ("d26");
   register double D27 asm ("d27");
   register double D28 asm ("d28");
   register double D29 asm ("d29");
   register double D30 asm ("d30");
   register double D31 asm ("d31");

   const int inca = 4 * lda, jnca = 16 - 12 * lda,incb = 4 * ldb, jncb = 16 - 12 * ldb, eight = 8;
#ifdef SREAL
   const int incc = 4 * ldc;
#elif defined(SCPLX)
   const int incc = 8 * ldc - 24;
#else
   #error This ARM NEON gemm routine provides only single and single complex support!
#endif
   float *pA, *pB, *pC, *pCtmp;
   for (j=0; j < N; j += 4) {
      for (i=0; i < M; i += 4) {
         pA = (float *)A + i * lda;
         pB = (float *)B + j * ldb;
#ifdef SREAL
         pC = (float *)C + i + j * ldc;
#elif defined(SCPLX)
         pC = (float *)C + 2 * (i + j * ldc);
#endif
         pCtmp = pC;
         __asm__ __volatile__(
#ifdef BETA0
           /*  load zeros into q12-q15 (C)*/
           "vmov.f32                          q12,          #0.0               \n\t"
           "vmov.f32                          q13,          #0.0               \n\t"
           "vmov.f32                          q14,          #0.0               \n\t"
           "vmov.f32                          q15,          #0.0               \n\t"
#else
#ifdef SREAL
           /* C is real; load the block into q12-q15 */
           "vld1.32                     {d24-d25},    [%[pCtmp]],       %[incc]\n\t"
           "vld1.32                     {d26-d27},    [%[pCtmp]],       %[incc]\n\t"
           "vld1.32                     {d28-d29},    [%[pCtmp]],       %[incc]\n\t"
           "vld1.32                     {d30-d31},    [%[pCtmp]]               \n\t"
#elif defined(SCPLX)
           /* C is complex; load the block into q12-q15, striding by 2 floats (8 bytes) */
           "vld1.32                      {d24[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d24[1]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d25[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d25[1]},    [%[pCtmp]],       %[incc]\n\t"
           "vld1.32                      {d26[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d26[1]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d27[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d27[1]},    [%[pCtmp]],       %[incc]\n\t"
           "vld1.32                      {d28[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d28[1]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d29[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d29[1]},    [%[pCtmp]],       %[incc]\n\t"
           "vld1.32                      {d30[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d30[1]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d31[0]},    [%[pCtmp]],      %[eight]\n\t"
           "vld1.32                      {d31[1]},    [%[pCtmp]]               \n\t"
#endif
#if !defined(BETA1)
           /* multiply the block of C by beta */
           "vmov.32                         d0[0],       %[beta]               \n\t"
           "vmul.f32                          q12,           q12,         d0[0]\n\t"
           "vmul.f32                          q13,           q13,         d0[0]\n\t"
           "vmul.f32                          q14,           q14,         d0[0]\n\t"
           "vmul.f32                          q15,           q15,         d0[0]\n\t"
#endif
#endif
           : [pCtmp] "+r" (pCtmp), "=w" (D24), "=w" (D25), "=w" (D26), "=w" (D27), "=w" (D28), "=w" (D29), "=w" (D30), "=w" (D31)
           : [incc] "r" (incc), [beta] "r" (beta), [eight] "r" (eight)
           : "s0"
         );

         for(k=0;k < K; k += 4) {
         __asm__ __volatile__(
           /* load the next block of A^T into q8-q11 */
           "vld4.32 {d16[0],d18[0],d20[0],d22[0]},       [%[pA]],       %[inca]\n\t"
           "vld4.32 {d16[1],d18[1],d20[1],d22[1]},       [%[pA]],       %[inca]\n\t"
           "vld4.32 {d17[0],d19[0],d21[0],d23[0]},       [%[pA]],       %[inca]\n\t"
           "vld4.32 {d17[1],d19[1],d21[1],d23[1]},       [%[pA]],       %[jnca]\n\t"
           /* load the next block of B into q4-q7 */
           "vld1.32                       {d8-d9},       [%[pB]],       %[incb]\n\t"
           "vld1.32                     {d10-d11},       [%[pB]],       %[incb]\n\t"
           "vld1.32                     {d12-d13},       [%[pB]],       %[incb]\n\t"
           "vld1.32                     {d14-d15},       [%[pB]],       %[jncb]\n\t"
           /*  multiply q4-q7 (B) by q8-q11 (A) and add the result to q12-q15 (beta * C)*/
           "vmla.f32                          q12,            q8,         d8[0]\n\t"
           "vmla.f32                          q13,            q8,        d10[0]\n\t"
           "vmla.f32                          q14,            q8,        d12[0]\n\t"
           "vmla.f32                          q15,            q8,        d14[0]\n\t"
           "vmla.f32                          q12,            q9,         d8[1]\n\t"
           "vmla.f32                          q13,            q9,        d10[1]\n\t"
           "vmla.f32                          q14,            q9,        d12[1]\n\t"
           "vmla.f32                          q15,            q9,        d14[1]\n\t"
           "vmla.f32                          q12,           q10,         d9[0]\n\t"
           "vmla.f32                          q13,           q10,        d11[0]\n\t"
           "vmla.f32                          q14,           q10,        d13[0]\n\t"
           "vmla.f32                          q15,           q10,        d15[0]\n\t"
           "vmla.f32                          q12,           q11,         d9[1]\n\t"
           "vmla.f32                          q13,           q11,        d11[1]\n\t"
           "vmla.f32                          q14,           q11,        d13[1]\n\t"
           "vmla.f32                          q15,           q11,        d15[1]\n\t"
           : [pA] "+r" (pA), [pB] "+r" (pB), "+w" (D24), "+w" (D25), "+w" (D26), "+w" (D27), "+w" (D28), "+w" (D29), "+w" (D30), "+w" (D31)
           : [inca] "r" (inca), [jnca] "r" (jnca), [incb] "r" (incb), [jncb] "r" (jncb)
           : "d8", "d9","d10", "d11", "d12", "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23"
         );
         }
         __asm__ __volatile__(
#ifdef SREAL
         /* C is real, write the result out from q12-q15 */
           "vst1.32                     {d24-d25},       [%[pC]],       %[incc]\n\t"
           "vst1.32                     {d26-d27},       [%[pC]],       %[incc]\n\t"
           "vst1.32                     {d28-d29},       [%[pC]],       %[incc]\n\t"
           "vst1.32                     {d30-d31},       [%[pC]]               \n\t"
#elif defined(SCPLX)
         /* C is complex, write the result out from q12-q15, again striding by 2 floats */
           "vst1.32                      {d24[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d24[1]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d25[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d25[1]},       [%[pC]],       %[incc]\n\t"
           "vst1.32                      {d26[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d26[1]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d27[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d27[1]},       [%[pC]],       %[incc]\n\t"
           "vst1.32                      {d28[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d28[1]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d29[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d29[1]},       [%[pC]],       %[incc]\n\t"
           "vst1.32                      {d30[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d30[1]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d31[0]},       [%[pC]],      %[eight]\n\t"
           "vst1.32                      {d31[1]},       [%[pC]]               \n\t"
#endif
           : [pC] "+r" (pC)
           : [incc] "r" (incc), [eight] "r" (eight), "w" (D24), "w" (D25), "w" (D26), "w" (D27), "w" (D28), "w" (D29), "w" (D30), "w" (D31)
           : "memory"
         );
      }
   }
}
