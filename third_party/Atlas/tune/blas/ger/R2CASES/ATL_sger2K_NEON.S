/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2011 Md. Rakib Hasan
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


#ifndef ATL_NEON
   #error "This routine requires NEON!"
#endif

/*
 * This code works with any alignment and any lda.
 */

/*
 * Info about system:
 *       1. add/sub are 1 cycle faster without constant shift.
 *       2. Instruction decoder can decode 2 instruction at a time.
 *       3. 2 arithmetic pipes.
 *       4. 32KB L1 cache (separate for code and data), 512KB L2 (!).
 *       5. Supports upto 4 data cache line fill request. (load/prefetch)
 *
 * Comments:
 *       1. 32byte cache line size.
 *       2. Specifying the alignment during load/store improves
 *          performance slightly (about 20-30 Mflops).
 *       3. Tried alignment of 32, 64, 128 and 256. Alignment of 32
 *          is slow but 64, 128 or 256 gives the same performance.
 *          For this reason 64 bit alignment is handled.
 *       4. Since cache line size was 32 bytes, used 8 as unrolling
 *          factor for M. Tried few prefetch distance, multiple of 32.
 *          64 gave the best performance.
 *       5. For FMLA, whether d or q registers are used, speed is same.
 *       6. For FMLA dependency, ld/use needs to be at least 8 independent
 *          instruction apart. Performance is same if more inst used.
 *       7. After instruction scheduling, tried Software pipelining on the
 *          common (optimized) case, no noticable difference on performance,
 *          thats why software pipelining is not done on other loops.
 *       8. Tried strided access of columns for lda not multiple of 4.
 *          It was slower by 25%.
 */

#define M   r0
#define Mi  r14
#define N   r1
#define X   r2
#define Y   r3
#define W   r4
#define Z   r5
#define A0  r6
#define lda r7

#define A1  r8
#define A2  r9
#define A3  r10

#define Nr  r11
#define Mr  r12

#define SP  r13

#define rY0h d0
#define rY0 d0[0]
#define rY1 d0[1]
#define rY2 d1[0]
#define rY3 d1[1]
#define rY0d d0,d1
#define rY0q q0

#define rZ0h d2
#define rZ0 d2[0]
#define rZ1 d2[1]
#define rZ2 d3[0]
#define rZ3 d3[1]
#define rZ0d d2,d3
#define rZ0q q1

#define T3q q2
#define T4q q3

#define rX0h  d8
#define rX0h_ d9
#define rX0d  d8,d9
#define rX0q  q4
#define rX1h  d10
#define rX1h_ d11
#define rX1d  d10,d11
#define rX1q  q5
#define rW0h  d12
#define rW0h_ d13
#define rW0d  d12,d13
#define rW0q  q6
#define rW1h  d14
#define rW1h_ d15
#define rW1d  d14,d15
#define rW1q  q7

#define rA0h  d16
#define rA0h_ d16
#define rA0d  d16,d17
#define rA0q  q8
#define rA1h  d18
#define rA1h_ d19
#define rA1d  d18,d19
#define rA1q  q9
#define rA2h  d20
#define rA2h_  d21
#define rA2d  d20,d21
#define rA2q  q10
#define rA3h  d22
#define rA3h_ d23
#define rA3d  d22,d23
#define rA3q  q11
#define rA4h  d24
#define rA4h_ d25
#define rA4d  d24,d25
#define rA4q  q12
#define rA5h  d26
#define rA5h_ d27
#define rA5d  d26,d27
#define rA5q  q13
#define rA6h  d28
#define rA6h_ d29
#define rA6d  d28,d29
#define rA6q  q14
#define rA7h  d30
#define rA7h_ d31
#define rA7d d30,d31
#define rA7q q15

#define AALIGN
@#define AALIGN ,:128
#define XALIGN
@#define XALIGN ,:64
#define YALIGN
@#define YALIGN ,:64

#define PFDIST 64

/*
 *                         r0          r1             r2             r3
 * void ATL_UGER2K(ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y,
 *		   const TYPE *W, const TYPE *Z, TYPE *A, ATL_CINT lda)
 *                            0(%r13(sp))    4        8              12
 */
.code 32
.fpu neon
.text
.align 2
.globl ATL_asmdecor(ATL_UGER2K)
.type ATL_asmdecor(ATL_UGER2K), %function
ATL_asmdecor(ATL_UGER2K):
      pld [X]
      pld [Y]
   mov r12, SP                   /* save stack pointer to load params */
   stmDB SP!, {r4-r11, r14}      /* store callee-saved registers and lr */
   vstmDB SP!, {d8-d15}          /* save callee-saved d registers */
   ldm   r12, {r4-r7}            /* load params */
      pld [W]
      pld [Z]
      pld [A0]
   fmrx Mi, FPSCR
   mvn Mr, #0x0F
   push {A0, Mi}
   and Mr, Mr, Mi
   bic Mr, Mr, #(1<<24)
   fmxr FPSCR, Mr
   /* Initial Setup */
   /*mov lda4, lda, LSL #2*/
   add A1, A0, lda, LSL #2
   add A2, A1, lda, LSL #2
   add A3, A2, lda, LSL #2
      pld [A1]
      pld [A2]
      pld [A3]
   and Nr, N, #3
   sub N, Nr
   ands Mr, A0, #0x0F               /* we need only 8 byte aligned */
   movNE Mr, Mr, LSR #2            /* */
   rsbNE Mr, Mr, #4              @ Mr = 4 - Mr
   cmp Mr, M
   movGE Mr, M                   @ if Mr > M, then Mr = M
   tst Mr, Mr
   subNE M, Mr
   bNE M_LT8                     /* Process one row for alignment */
A_ALIGNED:
   tst N, N
   and Mr, M, #7
   sub M, Mr
   bEQ N_LT4
   tst M, M
   bEQ M_LT8

   /* startup NEON */
LOOP_N:
   movs Mi, M
   VLD1.32 {rY0d}, [Y YALIGN]!
   VLD1.32 {rZ0d}, [Z YALIGN]!

   bEQ LOOP_M_DONE
      /**/
      VLD1.32 {rX0d, rX1d}, [X XALIGN]!
      VLD1.32 {rW0d, rW1d}, [W XALIGN]!
      VLD1.32 {rA0d, rA1d}, [A0 AALIGN]
      VLD1.32 {rA2d, rA3d}, [A1 AALIGN]
      VLD1.32 {rA4d, rA5d}, [A2 AALIGN]
      VLD1.32 {rA6d, rA7d}, [A3 AALIGN]/**/
      subs Mi, #8
   bEQ LOOP_M_END
   LOOP_M:

         pld [X, #PFDIST]
         pld [W, #PFDIST]
         pld [A0, #PFDIST]
         pld [A1, #PFDIST]

      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA1q, rX1q, rY0
      VMLA.F32 rA2q, rX0q, rY1
      VMLA.F32 rA3q, rX1q, rY1
      VMLA.F32 rA4q, rX0q, rY2
      VMLA.F32 rA5q, rX1q, rY2
      VMLA.F32 rA6q, rX0q, rY3
      VMLA.F32 rA7q, rX1q, rY3
      VLD1.32 {rX0d, rX1d}, [X XALIGN]!

         pld [A2, #PFDIST]
         pld [A3, #PFDIST]

      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA1q, rW1q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1
      VMLA.F32 rA3q, rW1q, rZ1
      VMLA.F32 rA4q, rW0q, rZ2
      VMLA.F32 rA5q, rW1q, rZ2
      VMLA.F32 rA6q, rW0q, rZ3
      VMLA.F32 rA7q, rW1q, rZ3
      VLD1.32 {rW0d, rW1d}, [W XALIGN]!

      /**/
      subs Mi, #8
      VST1.32 {rA0d, rA1d}, [A0 AALIGN]!
      VLD1.32 {rA0d, rA1d}, [A0 AALIGN]
      VST1.32 {rA2d, rA3d}, [A1 AALIGN]!
      VLD1.32 {rA2d, rA3d}, [A1 AALIGN]
      VST1.32 {rA4d, rA5d}, [A2 AALIGN]!
      VLD1.32 {rA4d, rA5d}, [A2 AALIGN]
      VST1.32 {rA6d, rA7d}, [A3 AALIGN]!
      VLD1.32 {rA6d, rA7d}, [A3 AALIGN]/**/

   bNE LOOP_M

   LOOP_M_END:
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA1q, rX1q, rY0
      VMLA.F32 rA2q, rX0q, rY1
      VMLA.F32 rA3q, rX1q, rY1
      VMLA.F32 rA4q, rX0q, rY2
      VMLA.F32 rA5q, rX1q, rY2
      VMLA.F32 rA6q, rX0q, rY3
      VMLA.F32 rA7q, rX1q, rY3

      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA1q, rW1q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1
      VMLA.F32 rA3q, rW1q, rZ1
      VMLA.F32 rA4q, rW0q, rZ2
      VMLA.F32 rA5q, rW1q, rZ2
      VMLA.F32 rA6q, rW0q, rZ3
      VMLA.F32 rA7q, rW1q, rZ3

      /**/
      VST1.32 {rA0d, rA1d}, [A0 AALIGN]!
      VST1.32 {rA2d, rA3d}, [A1 AALIGN]!
      VST1.32 {rA4d, rA5d}, [A2 AALIGN]!
      VST1.32 {rA6d, rA7d}, [A3 AALIGN]!/**/


LOOP_M_DONE:
   tst Mr, Mr
   bNE CLUP_M

CLUP_M_DONE:
      pld [Y, #PFDIST]
      pld [Z, #PFDIST]
   add A3, M, Mr
   sub A0, A3, LSL #2
   sub X, A3, LSL #2
   add A0, A0, lda, LSL #4
   sub W, A3, LSL #2
   subs N, #4
   add A1, A0, lda, LSL #2
   add A2, A0, lda, LSL #3
   add A3, A1, lda, LSL #3
   bNE LOOP_N

   tst Nr, Nr
   bNE N_LT4
DONE:
   pop {A0, Mi}
   fmxr FPSCR, Mi
   vldmIA SP!, {d8-d15}
   ldmIA SP!, {r4-r11, r14}           /* restore registers */
   bx lr

CLUP_M:
   tst Mr, #4
   bEQ CHECK_M2N4
   VLD1.32 {rX0d}, [X]!
   VLD1.32 {rW0d}, [W]!
   VLD1.32 {rA0d}, [A0]
   VLD1.32 {rA2d}, [A1]
   VLD1.32 {rA4d}, [A2]
   VLD1.32 {rA6d}, [A3]
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA2q, rX0q, rY1
      VMLA.F32 rA4q, rX0q, rY2
      VMLA.F32 rA6q, rX0q, rY3
      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1
      VMLA.F32 rA4q, rW0q, rZ2
      VMLA.F32 rA6q, rW0q, rZ3
      VST1.32 {rA0d}, [A0]!
      VST1.32 {rA2d}, [A1]!
      VST1.32 {rA4d}, [A2]!
      VST1.32 {rA6d}, [A3]!
CHECK_M2N4:
   tst Mr, #2
   bEQ CHECK_M1N4
   VLD1.32 {rX0h}, [X]!
   VLD1.32 {rW0h}, [W]!
   VLD1.32 {rA0h}, [A0]
   VLD1.32 {rA2h}, [A1]
   VLD1.32 {rA4h}, [A2]
   VLD1.32 {rA6h}, [A3]
      VMLA.F32 rA0h, rX0h, rY0
      VMLA.F32 rA2h, rX0h, rY1
      VMLA.F32 rA4h, rX0h, rY2
      VMLA.F32 rA6h, rX0h, rY3
      VMLA.F32 rA0h, rW0h, rZ0
      VMLA.F32 rA2h, rW0h, rZ1
      VMLA.F32 rA4h, rW0h, rZ2
      VMLA.F32 rA6h, rW0h, rZ3
      VST1.32 {rA0h}, [A0]!
      VST1.32 {rA2h}, [A1]!
      VST1.32 {rA4h}, [A2]!
      VST1.32 {rA6h}, [A3]!
CHECK_M1N4:
   tst Mr, #1
   bEQ CLUP_M_DONE
   VLD1.32 {rX0h[0]}, [X]!
   VLD1.32 {rW0h[0]}, [W]!
   VLD1.32 {rA0h[0]}, [A0]
   VLD1.32 {rA2h[0]}, [A1]
   VLD1.32 {rA4h[0]}, [A2]
   VLD1.32 {rA6h[0]}, [A3]
      VMLA.F32 rA0h, rX0h, rY0
      VMLA.F32 rA2h, rX0h, rY1
      VMLA.F32 rA4h, rX0h, rY2
      VMLA.F32 rA6h, rX0h, rY3
      VMLA.F32 rA0h, rW0h, rZ0
      VMLA.F32 rA2h, rW0h, rZ1
      VMLA.F32 rA4h, rW0h, rZ2
      VMLA.F32 rA6h, rW0h, rZ3
      VST1.32 {rA0h[0]}, [A0]!
      VST1.32 {rA2h[0]}, [A1]!
      VST1.32 {rA4h[0]}, [A2]!
      VST1.32 {rA6h[0]}, [A3]!
   b CLUP_M_DONE

N_LT4:
   tst Nr, #2
   bEQ CHECK_N1

   movs Mi, M
   VLD1.32 {rY0h}, [Y]!
   VLD1.32 {rZ0h}, [Z]!

   bEQ LOOP_M_N2_DONE
   LOOP_M_N2:
      /**/
      VLD1.32 {rX0d, rX1d}, [X]!
      VLD1.32 {rW0d, rW1d}, [W]!
      VLD1.32 {rA0d, rA1d}, [A0]
      VLD1.32 {rA2d, rA3d}, [A1]

         pld [X, #PFDIST]
         pld [W, #PFDIST]
         pld [A0, #PFDIST]
         pld [A1, #PFDIST]

      /**/
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA1q, rX1q, rY0
      VMLA.F32 rA2q, rX0q, rY1
      VMLA.F32 rA3q, rX1q, rY1

      /**/
      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA1q, rW1q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1
      VMLA.F32 rA3q, rW1q, rZ1

      /**/
      subs Mi, #8
      VST1.32 {rA0d, rA1d}, [A0]!
      VST1.32 {rA2d, rA3d}, [A1]!

   bNE LOOP_M_N2

LOOP_M_N2_DONE:
   tst Mr, Mr
   bEQ CHECK_N1_restoreAs

   tst Mr, #4
   bEQ CHECK_M2N2
   VLD1.32 {rX0d}, [X]!
   VLD1.32 {rW0d}, [W]!
   VLD1.32 {rA0d}, [A0]
   VLD1.32 {rA2d}, [A1]
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA2q, rX0q, rY1
      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1
      VST1.32 {rA0d}, [A0]!
      VST1.32 {rA2d}, [A1]!
CHECK_M2N2:
   tst Mr, #2
   bEQ CHECK_M1N2
   VLD1.32 {rX0h}, [X]!
   VLD1.32 {rW0h}, [W]!
   VLD1.32 {rA0h}, [A0]
   VLD1.32 {rA2h}, [A1]
      VMLA.F32 rA0h, rX0h, rY0
      VMLA.F32 rA2h, rX0h, rY1
      VMLA.F32 rA0h, rW0h, rZ0
      VMLA.F32 rA2h, rW0h, rZ1
      VST1.32 {rA0h}, [A0]!
      VST1.32 {rA2h}, [A1]!
CHECK_M1N2:
   tst Mr, #1
   bEQ CHECK_N1_restoreAs
   VLD1.32 {rX0h[0]}, [X]!
   VLD1.32 {rW0h[0]}, [W]!
   VLD1.32 {rA0h[0]}, [A0]
   VLD1.32 {rA2h[0]}, [A1]
      VMLA.F32 rA0h, rX0h, rY0
      VMLA.F32 rA2h, rX0h, rY1
      VMLA.F32 rA0h, rW0h, rZ0
      VMLA.F32 rA2h, rW0h, rZ1
      VST1.32 {rA0h[0]}, [A0]!
      VST1.32 {rA2h[0]}, [A1]!

CHECK_N1_restoreAs:
      pld [Y, #PFDIST]
      pld [Z, #PFDIST]
   add A3, M, Mr
   sub A0, A3, LSL #2
   sub X, A3, LSL #2
   add A0, A0, lda, LSL #3
   sub W, A3, LSL #2

CHECK_N1:
   tst Nr, #1
   bEQ DONE

   movs Mi, M
   VLD1.32 {rY0h[0]}, [Y]!
   VLD1.32 {rZ0h[0]}, [Z]!

   bEQ LOOP_M_N1_DONE
   LOOP_M_N1:
      /**/
      VLD1.32 {rX0d, rX1d}, [X]!
      VLD1.32 {rW0d, rW1d}, [W]!
      VLD1.32 {rA0d, rA1d}, [A0]

         pld [X, #PFDIST]
         pld [W, #PFDIST]
         pld [A0, #PFDIST]

      /**/
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA1q, rX1q, rY0

      /**/
      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA1q, rW1q, rZ0

      /**/
      VST1.32 {rA0d, rA1d}, [A0]!

      subs Mi, #8
   bNE LOOP_M_N1

LOOP_M_N1_DONE:
   tst Mr, Mr
   bEQ DONE

   tst Mr, #4
   bEQ CHECK_M2N1
   VLD1.32 {rX0d}, [X]!
   VLD1.32 {rW0d}, [W]!
   VLD1.32 {rA0d}, [A0]
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA0q, rW0q, rZ0
      VST1.32 {rA0d}, [A0]!
CHECK_M2N1:
   tst Mr, #2
   bEQ CHECK_M1N1
   VLD1.32 {rX0h}, [X]!
   VLD1.32 {rW0h}, [W]!
   VLD1.32 {rA0h}, [A0]
      VMLA.F32 rA0h, rX0h, rY0
      VMLA.F32 rA0h, rW0h, rZ0
      VST1.32 {rA0h}, [A0]!
CHECK_M1N1:
   tst Mr, #1
   bEQ DONE
   VLD1.32 {rX0h[0]}, [X]!
   VLD1.32 {rW0h[0]}, [W]!
   VLD1.32 {rA0h[0]}, [A0]
      VMLA.F32 rA0h, rX0h, rY0
      VMLA.F32 rA0h, rW0h, rZ0
      VST1.32 {rA0h[0]}, [A0]!
   b DONE

M_LT8:   /* Specially handle if M < 8 So that no need for inner loop */
   tst Mr, #4
   bEQ CHECK_M2_MLT8
   VLD1.32 {rX0d}, [X]!
   VLD1.32 {rW0d}, [W]!

CHECK_M2_MLT8:
   tst Mr, #2
   bEQ CHECK_M1_MLT8
   VLD1.32 {rX1h}, [X]!
   VLD1.32 {rW1h}, [W]!

CHECK_M1_MLT8:
   tst Mr, #1
   bEQ N_LOOP_MLT8
   VLD1.32 {rX1h_[0]}, [X]!
   VLD1.32 {rW1h_[0]}, [W]!

N_LOOP_MLT8:
   movs Mi, N
   bEQ LOOP_N_M4_MLT8_DONE
   LOOP_N_M4_MLT8:
      tst Mr, #4
      VLD1.32 {rY0d}, [Y]!
      VLD1.32 {rZ0d}, [Z]!

      bEQ N_M2_MLT8
      VLD1.32 {rA0d}, [A0]
      VLD1.32 {rA2d}, [A1]
      VLD1.32 {rA4d}, [A2]
      VLD1.32 {rA6d}, [A3]
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA2q, rX0q, rY1

         pld [Y, #PFDIST]
         pld [Z, #PFDIST]
         pld [A0, lda]
         pld [A1, lda]

      VMLA.F32 rA4q, rX0q, rY2
      VMLA.F32 rA6q, rX0q, rY3
      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1

         pld [A2, lda]
         pld [A3, lda]

      VMLA.F32 rA4q, rW0q, rZ2
      VMLA.F32 rA6q, rW0q, rZ3
      VST1.32 {rA0d}, [A0]!
      VST1.32 {rA2d}, [A1]!
      VST1.32 {rA4d}, [A2]!
      VST1.32 {rA6d}, [A3]!

   N_M2_MLT8:
      tst Mr, #2
      bEQ N_M1_MLT8
      VLD1.32 {rA1h}, [A0]
      VLD1.32 {rA3h}, [A1]
      VLD1.32 {rA5h}, [A2]
      VLD1.32 {rA7h}, [A3]
      VMLA.F32 rA1h, rX1h, rY0
      VMLA.F32 rA3h, rX1h, rY1

         pld [Y, #PFDIST]
         pld [Z, #PFDIST]
         pld [A0, lda]
         pld [A1, lda]

      VMLA.F32 rA5h, rX1h, rY2
      VMLA.F32 rA7h, rX1h, rY3
      VMLA.F32 rA1h, rW1h, rZ0
      VMLA.F32 rA3h, rW1h, rZ1

         pld [A2, lda]
         pld [A3, lda]

      VMLA.F32 rA5h, rW1h, rZ2
      VMLA.F32 rA7h, rW1h, rZ3
      VST1.32 {rA1h}, [A0]!
      VST1.32 {rA3h}, [A1]!
      VST1.32 {rA5h}, [A2]!
      VST1.32 {rA7h}, [A3]!

   N_M1_MLT8:
      tst Mr, #1
      bEQ N_CONT_MLT8
      VLD1.32 {rA1h_[0]}, [A0]
      VLD1.32 {rA3h_[0]}, [A1]
      VLD1.32 {rA5h_[0]}, [A2]
      VLD1.32 {rA7h_[0]}, [A3]
      VMLA.F32 rA1h_, rX1h_, rY0
      VMLA.F32 rA3h_, rX1h_, rY1

         pld [Y, #PFDIST]
         pld [Z, #PFDIST]
         pld [A0, lda]
         pld [A1, lda]

      VMLA.F32 rA5h_, rX1h_, rY2
      VMLA.F32 rA7h_, rX1h_, rY3
      VMLA.F32 rA1h_, rW1h_, rZ0
      VMLA.F32 rA3h_, rW1h_, rZ1

         pld [A2, lda]
         pld [A3, lda]

      VMLA.F32 rA5h_, rW1h_, rZ2
      VMLA.F32 rA7h_, rW1h_, rZ3
      VST1.32 {rA1h_[0]}, [A0]!
      VST1.32 {rA3h_[0]}, [A1]!
      VST1.32 {rA5h_[0]}, [A2]!
      VST1.32 {rA7h_[0]}, [A3]!
   N_CONT_MLT8:
   /**** Update pointers pending [done]...*/
      sub A0, Mr, LSL #2
      subs Mi, #4
      add A0, A0, lda, LSL #4
      add A1, A0, lda, LSL #2
      add A2, A0, lda, LSL #3
      add A3, A1, lda, LSL #3
   bNE LOOP_N_M4_MLT8
LOOP_N_M4_MLT8_DONE:
   tst Nr, Nr
   bEQ DONE_MLT8

   tst Nr, #2
   bEQ CHECK_MLT8N1

      tst Mr, #4
      VLD1.32 {rY0h}, [Y]!
      VLD1.32 {rZ0h}, [Z]!

      bEQ N2_M2_MLT8
      VLD1.32 {rA0d}, [A0]
      VLD1.32 {rA2d}, [A1]
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA2q, rX0q, rY1
      VMLA.F32 rA0q, rW0q, rZ0
      VMLA.F32 rA2q, rW0q, rZ1
      VST1.32 {rA0d}, [A0]!
      VST1.32 {rA2d}, [A1]!

   N2_M2_MLT8:
      tst Mr, #2
      bEQ N2_M1_MLT8
      VLD1.32 {rA1h}, [A0]
      VLD1.32 {rA3h}, [A1]
      VMLA.F32 rA1h, rX1h, rY0
      VMLA.F32 rA3h, rX1h, rY1
      VMLA.F32 rA1h, rW1h, rZ0
      VMLA.F32 rA3h, rW1h, rZ1
      VST1.32 {rA1h}, [A0]!
      VST1.32 {rA3h}, [A1]!

   N2_M1_MLT8:
      tst Mr, #1
      bEQ CHECK_MLT8N1_addr
      VLD1.32 {rA1h_[0]}, [A0]
      VLD1.32 {rA3h_[0]}, [A1]
      VMLA.F32 rA1h_, rX1h_, rY0
      VMLA.F32 rA3h_, rX1h_, rY1
      VMLA.F32 rA1h_, rW1h_, rZ0
      VMLA.F32 rA3h_, rW1h_, rZ1
      VST1.32 {rA1h_[0]}, [A0]!
      VST1.32 {rA3h_[0]}, [A1]!

CHECK_MLT8N1_addr:
   sub A1, Mr, LSL #2
   add A0, A1, lda, LSL #2

CHECK_MLT8N1:
   tst Nr, #1
   bEQ DONE_MLT8

      tst Mr, #4
      VLD1.32 {rY0h[0]}, [Y]!
      VLD1.32 {rZ0h[0]}, [Z]!

      bEQ N1_M2_MLT8
      VLD1.32 {rA0d}, [A0]
      VMLA.F32 rA0q, rX0q, rY0
      VMLA.F32 rA0q, rW0q, rZ0
      VST1.32 {rA0d}, [A0]!

   N1_M2_MLT8:
      tst Mr, #2
      bEQ N1_M1_MLT8
      VLD1.32 {rA1h}, [A0]
      VMLA.F32 rA1h, rX1h, rY0
      VMLA.F32 rA1h, rW1h, rZ0
      VST1.32 {rA1h}, [A0]!

   N1_M1_MLT8:
      tst Mr, #1
      bEQ DONE_MLT8
      VLD1.32 {rA1h_[0]}, [A0]
      VMLA.F32 rA1h_, rX1h_, rY0
      VMLA.F32 rA1h_, rW1h_, rZ0
      VST1.32 {rA1h_[0]}, [A0]!
DONE_MLT8:
   tst M, M
   bEQ DONE
   /* setup addresses of As and Y, Z */
   ldr A0, [SP]         @ called to align, so address saved
   add A0, Mr, LSL #2   @ adjust A0, since we already processed one row
   add A3, N, Nr
   add A1, A0, lda, LSL #2
   sub Y, A3, LSL #2
   add A2, A0, lda, LSL #3
   sub Z, A3, LSL #2
   add A3, A1, lda, LSL #3
   b A_ALIGNED

.end
