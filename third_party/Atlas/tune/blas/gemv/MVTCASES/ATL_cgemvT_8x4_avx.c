/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2012, 2011 R. Clint Whaley
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
#ifndef ATL_AVX
   #error "This routine requires AVX!"
#endif

#include "atlas_asm.h"
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
#define M       %rdi
#define M_w      %di
#define N       %rsi
#define N_w     %si
#define N_l     %esi
#define pA      %rdx
#define lda     %rcx
#define pX      %r8
#define pY      %r9
#define pA1     %rbx
#define pA2     %r11
#define Mr      %rax
#define pA3     %r10
#define FLAGS   %r12 /* bitfield: 0:set if M%8 >= 4;  1:set if Mr is non-zero */
#define Nr      %r14
//#define incII   %r15
#define M0      %r13
/*
 * SSE register assignment
 */
#define rA0     %ymm0
#define rX0     %ymm1
#define rx0     %ymm2
#define rt0     %ymm3
#define rY0r    %ymm4
#define rY0i    %ymm5
#define rY1r    %ymm6
#define rY1i    %ymm7
#define rY2r    %ymm8
#define rY2i    %ymm9
#define rY3r    %ymm10
#define rY3i    %ymm11
// #define rX1     %xmm12
#define rMSK4   %ymm13   /* all ones if N%8 >= 4 */
#define rMASK   %ymm14   /* each word says if N%4 includes that word */
#define rNP1    %ymm15

#define rA0_    %xmm0
#define rX0_    %xmm1
#define rx0_    %xmm2
#define rt0_    %xmm3
#define rY0r_   %xmm4
#define rY0i_   %xmm5
#define rY1r_   %xmm6
#define rY1i_   %xmm7
#define rY2r_   %xmm8
#define rY2i_   %xmm9
#define rY3r_   %xmm10
#define rY3i_   %xmm11
#define rNP1_   %xmm15
/*
 *                      %rdi        %rsi           %rdx          %rcx
 * void ATL_UGEMV(ATL_CINT M, ATL_CINT N, const TYPE *A, ATL_CINT lda,
 *                          %r8      %r9
 *                const TYPE *X, TYPE *Y)
 */
.text
.global ATL_asmdecor(ATL_UGEMV)
ALIGN64
ATL_asmdecor(ATL_UGEMV):
/*
 * Construct {-1,1,-1,1} in memory, and duplicate into rNP1
 */
   mov $0xBF800000, %eax        /* IEEE -1.0 */
   mov %eax, -12(%rsp)
   mov %eax, -4(%rsp)
   shl $1, %eax
   shr $1, %eax                 /* IEEE 1.0 */
   mov %eax, -16(%rsp)
   mov %eax, -8(%rsp)
   vbroadcastf128 -16(%rsp), rNP1
      /* rNP1 = {-1.0,+1.0,-1.0,+1.0,-1.0,+1.0-1.0,+1.0} */
/*
 * Save callee-saved iregs
 */
   movq %rbp, -8(%rsp)
   movq %rbx, -16(%rsp)
   movq %r12, -24(%rsp)
   movq %r13, -32(%rsp)
   movq %r14, -40(%rsp)
   movq %r15, -48(%rsp)

   movslq %edi, %rdi
   movslq %esi, %rsi
   movslq %ecx, %rcx
   mov M, Mr
   andw $0xFFF8, M_w            /* M is now a multiple of 8 */
   sub M, Mr                    /* Mr = M % 8 */
/*
 *
 */
   xor FLAGS, FLAGS             /* assume M%8 = 0 */
   vxorps rMSK4, rMSK4, rMSK4   /* all-zeros: assuming M%8 < 4 */
   vxorps rMASK, rMASK, rMASK   /* all-zeros: assuming no cleanup */
/*
 * If (Mr >= 4) rMSK4={all 1s}; Mr -= 4
 */
   bt $2, Mr                    /* CF=1 if Mr >= 4 */
   jnc DONE_MSK4
      mov $1, FLAGS             /* will peel first 4 cplx numbers */
      sub $4, Mr
      mov $0xFFFFFFFF, %ebx
      movl %ebx, -56(%rsp)
      vbroadcastss -56(%rsp), rMSK4     /* rMSK4 = all 1s -> peel 4 its */
      add $32, pA                       /* will be peeled */
      add $32, pX                       /* will be peeled */
   DONE_MSK4:                           /* Mr known <= 3 */
   cmp $0, Mr
   je DONE_MASK
/*
 * Construct rMASK based on Mr, rMASK starts out all-zeros (no cleanup)
 * If we reach here, there is some cleanup to do (1 <= Mr <= 3)
 */
   mov $0xFFFFFFFF, %ebx
   movl %ebx, -56(%rsp)
   vbroadcastss -56(%rsp), rt0   /* rt0 = all 1s */
   add $2, FLAGS
/*
 * If Mr == 1, have first two words (imag,real) all 1s, others 0 (from MASK)
 */
   cmp $1, Mr
   jne DONEM1
      vblendps $0x03, rt0, rMASK, rMASK   /* 0b0000 0011 */
      jmp DONE_MASK
   DONEM1:
/*
 * If Mr == 2, have first 4 words all 1s, others 0 (from MASK)
 */
   cmp $2, Mr
   jne DONEM2
      vblendps $0x0F, rt0, rMASK, rMASK   /* 0b0000 1111 */
      jmp DONE_MASK
   DONEM2:
/*
 * If we reach here, Mr == 3
 */
   vblendps $0x3F, rt0, rMASK, rMASK   /* 0b0011 1111 */

DONE_MASK:
   shl $3, lda          /* lda *= sizeof */
/*
 * Jump to special case code if M < 8
 */
   cmp $8, M
   jl MLT8
/*
 * Otherwise, set up for normal NU=4, Mu=8 unrolled loop
 */
   lea (pA,M,8), pA     /* A += M */
   lea (pX,M,8), pX     /* X += M */
   lea (pA,lda), pA1
   lea (pA,lda,2), pA2
   lea (pA1,lda,2), pA3
   shl $3, M            /* M *= sizeof */
   neg M
   mov M, M0
   mov N, Nr
   andl $0xFFFFFFFC, N_l        /* N is now a multiple of 4 */
   jz LOOPN1
   sub N, Nr                    /* Nr = N % 4 */
   LOOPN4:
      bt $0, FLAGS      /* CF=1 if Mr >= 4 */
      jc PEEL4
      vxorps rY0r, rY0r, rY0r
      vxorps rY0i, rY0i, rY0i
      vxorps rY1r, rY1r, rY1r
      vxorps rY1i, rY1i, rY1i
      vxorps rY2r, rY2r, rY2r
      vxorps rY2i, rY2i, rY2i
      vxorps rY3r, rY3r, rY3r
      vxorps rY3i, rY3i, rY3i
      LOOPM8:
         vmovaps (pX,M), rX0
            /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
         vshufps $0xB1, rX0, rX0, rx0
            /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
         vmovaps (pA,M), rA0
            /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
         vmulps rX0, rA0, rt0
            /* rt0 = {x3i*a3i,x3r*a3r,...,  x0i*a0i, x0r*a0r} */
         vaddps  rt0, rY0r, rY0r
            /* rY0r= {s3ii, s3rr, s2ii, s2rr, s1ii, s1rr, s0ii, s0rr} */
            prefetchnta 512(pA,M)
         vmulps rx0, rA0, rt0
            /* rt0 = {x3r*a3i, x3i*a3r, ..., x0r*a0i, x0i*a0r} */
         vaddps  rt0, rY0i, rY0i
            /* rY0i= {s3ri, s3ir, s2ri, s2ir, s1ri, s1ir, s0ri, s0ir} */

         vmovaps (pA1,M), rA0
         vmulps rX0, rA0, rt0
         vaddps  rt0, rY1r, rY1r
            prefetchnta 448(pA1,M)
         vmulps rx0, rA0, rt0
         vaddps  rt0, rY1i, rY1i

         vmovaps (pA2,M), rA0
         vmulps rX0, rA0, rt0
         vaddps  rt0, rY2r, rY2r
            prefetchnta 448(pA2,M)
         vmulps rx0, rA0, rt0
         vaddps  rt0, rY2i, rY2i

         vmovaps (pA3,M), rA0
         vmulps rX0, rA0, rt0
         vaddps  rt0, rY3r, rY3r
            prefetchnta 448(pA3,M)
         vmulps rx0, rA0, rt0
         vaddps  rt0, rY3i, rY3i

         vmovaps 32(pX,M), rX0
            /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
         vshufps $0xB1, rX0, rX0, rx0
            /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
         vmovaps 32(pA,M), rA0
            /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
         vmulps rX0, rA0, rt0
            /* rt0 = {x3i*a3i,x3r*a3r,...,  x0i*a0i, x0r*a0r} */
         vaddps  rt0, rY0r, rY0r
            /* rY0r= {s3ii, s3rr, s2ii, s2rr, s1ii, s1rr, s0ii, s0rr} */
         vmulps rx0, rA0, rt0
            /* rt0 = {x3r*a3i, x3i*a3r, ..., x0r*a0i, x0i*a0r} */
         vaddps  rt0, rY0i, rY0i
            /* rY0i= {s3ri, s3ir, s2ri, s2ir, s1ri, s1ir, s0ri, s0ir} */

         vmovaps 32(pA1,M), rA0
         vmulps rX0, rA0, rt0
         vaddps  rt0, rY1r, rY1r
         vmulps rx0, rA0, rt0
         vaddps  rt0, rY1i, rY1i
            prefetcht0 256(pX,M)

         vmovaps 32(pA2,M), rA0
         vmulps rX0, rA0, rt0
         vaddps  rt0, rY2r, rY2r
         vmulps rx0, rA0, rt0
         vaddps  rt0, rY2i, rY2i

         vmovaps 32(pA3,M), rA0
         vmulps rX0, rA0, rt0
         vaddps  rt0, rY3r, rY3r
         vmulps rx0, rA0, rt0
         vaddps  rt0, rY3i, rY3i

         add $8*8, M
      jnz LOOPM8
/*
 *    If we have a M%4 remainder, jump to code that will handle it
 */
      bt $1, FLAGS
      jc CLEANN4_M
      DONE_MCU:  /* jump back to here if we left loop for M cleanup */
/*
 *    Sum up all these vecs
 */
      lea (pA,lda,4), pA
      vmulps rNP1, rY0r, rY0r   /* negate all imag*imag entries */
      vhaddps rY0i, rY0r, rY0r
         /* rY0r = {s0iD,s0iC,s0rD,s0rC,s0iB,s0iA,s0rB,s0rA} */
      lea (pA1,lda,4), pA1
      vmulps rNP1, rY1r, rY1r   /* negate all imag*imag entries */
      vhaddps rY1i, rY1r, rY1r  /* same for 0 but for Y[1] */
      prefetchnta (pA,M0)
      lea (pA2,lda,4), pA2
      vmulps rNP1, rY2r, rY2r   /* negate all imag*imag entries */
      vhaddps rY2i, rY2r, rY2r  /* same as for 0 but for Y[2] */
      lea (pA3,lda,4), pA3
      prefetchnta (pA1,M0)
      vmulps rNP1, rY3r, rY3r   /* negate all imag*imag entries */
      vhaddps rY3i, rY3r, rY3r  /* same as for 0 but for Y[3] */

      prefetchnta (pA2,M0)
      vhaddps rY1r, rY0r, rY0r
           /* rY0r = {s1iCD,s1rCD,s0iCD,s0rCD,s1iAB,s1rAB,s0iAB,s0rAB} */
      prefetchnta (pA3,M0)
      vhaddps rY3r, rY2r, rY2r
           /* rY2r = {s3iCD,s3rCD,s2iCD,s2rCD,s3iAB,s3rAB,s2iAB,s2rAB} */
      prefetchnta 64(pA,M0)
      vperm2f128 $0x20, rY2r, rY0r, rY0i
           /* rY0i = {s3iAB,s3rAB,s2iAB,s2rAB,s1iAB,s1rAB,s0iAB,s0rAB} */
      prefetchnta 64(pA1,M0)
      vperm2f128 $0x31, rY2r, rY0r, rY2i
           /* rY2i = {s3iCD,s3rCD,s2iCD,s2rCD,s1iCD,s1rCD,s0iCD,s0rCD} */
      vaddps rY2i, rY0i, rY0r /* rY0r= {s3i,s3r,s2i,s2r,s1i,s1r,s0i,s0r} */
      prefetchnta 64(pA2,M0)
      mov M0, M
      #ifndef BETA0
         vaddps (pY), rY0r, rY0r
      #endif
      vmovaps rY0r, (pY)
      add $32, pY
      prefetchnta 64(pA3,M0)

      sub $4, N
   jnz LOOPN4
/*
 * Do N-loop cleanup if necessary
 */
   cmp $0, Nr
   jne LOOPN1

/*
 * EPILOGUE: restore registers and return
 */
DONE:
   movq -8(%rsp), %rbp
   movq -16(%rsp), %rbx
   movq -24(%rsp), %r12
   movq -32(%rsp), %r13
   movq -40(%rsp), %r14
   movq -48(%rsp), %r15
   ret
/*
 *    Peel first 4 iterations if M%8 >= 4; if M%8 < 4, then this will
 *    zero the Y vectors
 */
PEEL4:
   vmovaps -32(pX,M), rX0   /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
   vshufps $0xB1, rX0, rX0, rx0
      /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
   vmovaps -32(pA,M), rA0 /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
   vmulps rX0, rA0, rY0r
   vmulps rx0, rA0, rY0i

   vmovaps -32(pA1,M), rA0
   vmulps rX0, rA0, rY1r
   vmulps rx0, rA0, rY1i

   vmovaps -32(pA2,M), rA0
   vmulps rX0, rA0, rY2r
   vmulps rx0, rA0, rY2i

   vmovaps -32(pA3,M), rA0
   vmulps rX0, rA0, rY3r
   vmulps rx0, rA0, rY3i

   jmp LOOPM8
/*
 *    Handles M%4 component for LOOPN4.
 */
CLEANN4_M:
   vmaskmovps (pX), rMASK, rX0
      /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
   vshufps $0xB1, rX0, rX0, rx0
      /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
   vmaskmovps (pA), rMASK, rA0
      /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
   vmulps rX0, rA0, rt0
      /* rt0 = {x3i*a3i,x3r*a3r,...,  x0i*a0i, x0r*a0r} */
   vaddps  rt0, rY0r, rY0r
      /* rY0r= {s3ii, s3rr, s2ii, s2rr, s1ii, s1rr, s0ii, s0rr} */
   vmulps rx0, rA0, rt0
      /* rt0 = {x3r*a3i, x3i*a3r, ..., x0r*a0i, x0i*a0r} */
   vaddps  rt0, rY0i, rY0i
      /* rY0i= {s3ri, s3ir, s2ri, s2ir, s1ri, s1ir, s0ri, s0ir} */
   vmaskmovps (pA1), rMASK, rA0
   vmulps rX0, rA0, rt0
   vaddps  rt0, rY1r, rY1r
   vmulps rx0, rA0, rt0
   vaddps  rt0, rY1i, rY1i
   vmaskmovps (pA2), rMASK, rA0
   vmulps rX0, rA0, rt0
   vaddps  rt0, rY2r, rY2r
   vmulps rx0, rA0, rt0
   vaddps  rt0, rY2i, rY2i
   vmaskmovps (pA3), rMASK, rA0
   vmulps rX0, rA0, rt0
   vaddps  rt0, rY3r, rY3r
   vmulps rx0, rA0, rt0
   vaddps  rt0, rY3i, rY3i
   jmp DONE_MCU

/*
 * this case can handle any N value, as long as M >= 4
 */
LOOPN1:  /* rolled loop for N cleanup */
   mov M0, M
   bt $0, FLAGS      /* CF=1 if Mr >= 4 */
   jc PEEL4_N1
   vxorps rY0r, rY0r, rY0r
   vxorps rY0i, rY0i, rY0i
   LOOPM4_N1:
      vmovaps (pX,M), rX0   /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
      vshufps $0xB1, rX0, rX0, rx0/* rx0 = {x3r,x3i,x2r,x2i,x1r,x1i,x0r,x0i} */
      vmovaps (pA,M), rA0   /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
      vmulps rX0, rA0, rt0  /* rt0 = {x3i*a3i,x3r*a3r,...,  x0i*a0i, x0r*a0r} */
      vaddps  rt0, rY0r, rY0r
         /* rY0r= {s3ii, s3rr, s2ii, s2rr, s1ii, s1rr, s0ii, s0rr} */
      vmulps rx0, rA0, rt0
         /* rt0 = {x3r*a3i, x3i*a3r, ..., x0r*a0i, x0i*a0r} */
         prefetchnta 512(pA,M)
      vaddps  rt0, rY0i, rY0i
         /* rY0i= {s3ri, s3ir, s2ri, s2ir, s1ri, s1ir, s0ri, s0ir} */
      add $4*8, M
   jnz LOOPM4_N1
/*
 * If M%4 > 0, handle it
 */
   bt $1, FLAGS
   jnc DONE_N1MCU
      vmaskmovps (pX,M), rMASK, rX0
         /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
      vshufps $0xB1, rX0, rX0, rx0
         /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
      vmaskmovps (pA,M), rMASK, rA0
         /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
      vmulps rX0, rA0, rt0
         /* rt0 = {x3i*a3i,x3r*a3r,...,  x0i*a0i, x0r*a0r} */
      vaddps  rt0, rY0r, rY0r
         /* rY0r= {s3ii, s3rr, s2ii, s2rr, s1ii, s1rr, s0ii, s0rr} */
      vmulps rx0, rA0, rt0
         /* rt0 = {x3r*a3i, x3i*a3r, ..., x0r*a0i, x0i*a0r} */
      vaddps  rt0, rY0i, rY0i
         /* rY0i= {s3ri, s3ir, s2ri, s2ir, s1ri, s1ir, s0ri, s0ir} */
   DONE_N1MCU:
/*
 * sum up results
 */
   #ifndef BETA0
      movlps (pY), rA0_
   #endif
   vmulps rNP1, rY0r, rY0r   /* negate all imag*imag entries */
   vhaddps rY0i, rY0r, rY0r  /* rY0r = {s3i,s2i,s3r,s2r,s1i,s0i,s1r,s0r} */
   vextractf128 $1,rY0r,rY0i_/* rY0i = {XXX,XXX,XXX,XXX,s3i,s2i,s3r,s2r} */
   addps rY0i_, rY0r_        /* rY0r = {s31i,s20i,s31r,s20r} */
   haddps rY0r_, rY0r_       /* rY0r = {s0-3i,s0-3r,s0-3i,s0-3r} */
   #ifndef BETA0
      addps rA0_, rY0r_
   #endif
   movlps rY0r_, (pY)

   lea (pA,lda), pA
   add $8, pY
   sub $1, Nr
jnz LOOPN1
jmp DONE
/*
 * Handle peel of first iteration if M%8 >= 4
 */
PEEL4_N1:
   vmovaps -32(pX,M), rX0   /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
   vshufps $0xB1, rX0, rX0, rx0/* rx0 = {x3r,x3i,x2r,x2i,x1r,x1i,x0r,x0i} */
   vmovaps -32(pA,M), rA0   /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
   vmulps rX0, rA0, rY0r /* rt0 = {x3i*a3i,x3r*a3r,...,  x0i*a0i, x0r*a0r} */
   vmulps rx0, rA0, rY0i
      /* rt0 = {x3r*a3i, x3i*a3r, ..., x0r*a0i, x0i*a0r} */
      prefetchnta 512(pA,M)
   jmp LOOPM4_N1

/*
 * Special case code for M < 8; start by loading loop-invariant X
 */
#define rX1 rY3i
#define rx1 rY3r
#define ry0 rY2r_
MLT8:
   xorps ry0, ry0
   bt $0, FLAGS      /* CF=1 if Mr >= 4 */
   jnc MLT4

   vmovaps -32(pX), rX0
      /* rX0 = {x3i, x3r, x2i, x2r, x1i, x1r, x0i, x0r} */
   vshufps $0xB1, rX0, rX0, rx0
      /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
   vmulps rNP1, rX0, rX0
      /* rX0 ={-x3i, x3r,-x2i, x2r,-x1i, x1r,-x0i, x0r} */
   vmaskmovps (pX), rMASK, rX1
      /* rX1 = {x7i, x7r, x6i, x6r, x5i, x5r, x4i, x4r} */
   vshufps $0xB1, rX1, rX1, rx1
      /* rx1 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
   vmulps rNP1, rX1, rX1
      /* rX1 ={-x7i, x7r,-x6i, x6r,-x5i, x5r,-x4i, x4r} */
LOOPN1_MLE8:
   vmovaps -32(pA), rA0
      /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
   vmulps rX0, rA0, rY0r
      #ifndef BETA0
         movlps (pY), ry0
      #endif
   vmulps rx0, rA0, rY0i
   vmaskmovps 0(pA), rMASK, rA0
      /* rA0 = {a7i, a7r, a6i, a6r, a5i, a5r, a4i, a4r} */
   vmulps rX1, rA0, rt0
   vaddps rt0, rY0r, rY0r
      /* rY0r = {yr7, yr6, yr5, yr4, yr3, yr2, yr1, yr0} */
   vmulps rx1, rA0, rt0
   vaddps rt0, rY0i, rY0i
      prefetcht0  (pA,lda,8)
      /* rY0i = {yi7, yi6, yi5, yi4, yi3, yi2, yi1, yi0} */
   vhaddps rY0i, rY0r, rY0r
      /* rY0r = {yi6-7,yi4-5,yr6-7,yr4-5,yi2-3,yi0-1,yr2-3,yr0-1} */
   vextractf128 $1, rY0r, rY0i_ /* y0i_={yi6-7,yi4-5,yr6-7,yr4-5} */
      add lda, pA
   addps rY0i_, rY0r_           /* y0r ={yi2367,yi0145,yr2367,yr0145 */
      add $8, pY
   haddps rY0r_, rY0r_          /* y0r ={yi0-7,y0-7,yi0-7,yr0-7} */
   #ifndef BETA0
      addps rY0r_, ry0
      movlps ry0, -8(pY)
   #else
      movlps rY0r_, -8(pY)
   #endif
   sub $1, N
jne LOOPN1_MLE8
jmp DONE
/*
 * Special case code for M < 3
 */
MLT4:
   vmaskmovps (pX), rMASK, rX0
   vshufps $0xB1, rX0, rX0, rx0
      /* rx0 = {x3r, x3i, x2r, x2i, x1r, x1i, x0r, x0i} */
   vmulps rNP1, rX0, rX0
      /* rX0 ={-x3i, x3r,-x2i, x2r,-x1i, x1r,-x0i, x0r} */

LOOPN1_MLT4:
   vmaskmovps (pA), rMASK, rA0
      /* rA0 = {a3i, a3r, a2i, a2r, a1i, a1r, a0i, a0r} */
   vmulps rX0, rA0, rY0r
      #ifndef BETA0
         movlps (pY), ry0
      #endif
   vmulps rx0, rA0, rY0i
   vhaddps rY0i, rY0r, rY0r
      /* rY0r = {yi6-7,yi4-5,yr6-7,yr4-5,yi2-3,yi0-1,yr2-3,yr0-1} */
      prefetcht0  (pA,lda,8)
   vextractf128 $1, rY0r, rY0i_ /* y0i_={yi6-7,yi4-5,yr6-7,yr4-5} */
      add lda, pA
   addps rY0i_, rY0r_           /* y0r ={yi2367,yi0145,yr2367,yr0145 */
      add $8, pY
   haddps rY0r_, rY0r_          /* y0r ={yi0-7,y0-7,yi0-7,yr0-7} */
   #ifndef BETA0
      addps rY0r_, ry0
      movlps ry0, -8(pY)
   #else
      movlps rY0r_, -8(pY)
   #endif
   sub $1, N
jne LOOPN1_MLT4
jmp DONE
