/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2011 R. Clint Whaley
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
   #error "This kernel requires AVX!"
#endif
#ifdef ATL_GAS_x8632
   #define NN %edi
   #define N  %esi
   #define N_w %si
   #define XX  %ebp
   #define X   %edx
   #define X_b %dl
   #define Imax %eax
   #define bitreg %ecx
   #define bitreg_b %cl
#else
   #define NN      %rdi
   #define N       %rsi
   #define N_w     %si
   #define XX      %r8
   #define X       %rdx
   #define X_b     %dl
   #define Imax    %rax
   #define bitreg  %rcx
   #define bitreg_b %cl
#endif

#define maxval  %ymm0
#define maxval_ %xmm0
#define absval  %ymm1
#define absval_ %xmm1
#define x0      %ymm2
#define x0_     %xmm2
#define x1      %ymm3
#define x1_     %xmm3
#define up      %ymm4
#define up_     %xmm4
#include "atlas_asm.h"
/*                   rdi/4          rsi/8
int ATL_UIAMAX(const int N, const TYPE *X, const int incX) */
.text
.globl ATL_asmdecor(ATL_UIAMAX)
ATL_asmdecor(ATL_UIAMAX):
#ifdef ATL_GAS_x8632
   #define FSIZE 12
   sub $FSIZE, %esp
   mov $0x7FFFFFFF, %eax   /* all 1s except sign bit 0 */
   movl %eax, (%esp)
   vbroadcastss (%esp), absval
/*
 * Save registers then load input arguments
 */
   movl %edi, (%esp)
   movl %esi, 4(%esp)
   movl %ebp, 8(%esp)
   movl FSIZE+4(%esp), NN
   movl FSIZE+8(%esp), X
   mov X, XX
#else
/*
 * Construct 32 bit constant with 0 in 31st bit, 1s elsewhere; can AND floats
 * with this value to get absolute value
 */
   mov $0x7FFFFFFF, %eax
   movl %eax, -8(%rsp)
   vbroadcastss -8(%rsp), absval

   mov %rsi, X
   mov %rsi, XX
#endif
/*
 * Start out assuming 1st elt is max, move ptr and dec N
 */
   mov X, Imax
   movss (X), maxval_
   andps absval_, maxval_
   movss 4(X), x1_
   andps absval_, x1_
   addps x1_, maxval_
   shufps $0x00, maxval_, maxval_   /* maxval, maxval, maxval, maxval */
   vinsertf128 $1, maxval_, maxval, maxval /* maxval in all 8 entries */
   add $8, X
   sub $1, NN
   jz DONE
/*
 * Don't even start vector loops unless we have at least 2 full iterations left
 */
   cmp $16, NN
   jb CLEANUP
/*
 * If X is only 4-byte aligned, then it cannot be aligned
 */
   test $0x3, X
   jnz UNALIGNED
/*
 * Find the first 32-byte aligned X address, and do scalar ops until we reach it
 */
   lea 31(X), N
   andw $0xFFE0, N_w  /* N = ((X+31)/32)*32 */
   cmp X, N
   jne FORCE_ALIGN
/*
 * After any peeling, X is aligned and remaining vector length in NN
 */
XALIGNED:
   mov NN, N
   and $0xFFFFFF8, N       /* make N a multiple of 8 */
   jz CLEANUP
   sub N, NN
   shl $3, N            /* N *= sizeof */
   lea (X,N), X
   neg N
   ALOOP:
      vandps (X,N), absval, x0 		/* abs(x3i) ... abs(x0r) */
      vandps 32(X,N), absval, x1 	/* abs(x7i) ... abs(x4r) */
      vhaddps x1, x0, x0   		/* abs(x7i)+abs(x7r) ... abs(x0i)+abs(x0r) */
      prefetchnta 1024(X,N)
      vcmpLEps maxval, x0, x1  		/* all 1s if maxval already has max */
      vmovmskps x1, bitreg
      cmp $0xFF, bitreg_b
      jnz VNEWMAX
AGOTMAX:
      add $64, N
   jnz ALOOP
   cmp $0, NN
   jnz CLEANUP

DONE:
   sub XX, Imax  /* # of bytes away from start */
   shr $3, Imax  /* # of elts (index) where max was found */
#ifdef ATL_GAS_x8632
   movl (%esp), %edi
   movl 4(%esp), %esi
   movl 8(%esp), %ebp
   add $FSIZE, %esp
#endif
   ret

UNALIGNED:
   mov NN, N
   shr $3, N          /* N/8 */
   jz  CLEANUP
   shl $3, N
   sub N, NN
   lea (X,N,8), X     /* X += N */
   shl $3, N          /* N *= sizeof */
   neg N
   UALOOP:
      vmovups (X,N), x0
      vandps absval, x0, x0
      vmovups 32(X,N), x1
      vandps absval, x1, x1
      prefetchnta 1024(X,N)
      vhaddps x1, x0, x0   /* abs(x7i)+abs(x7r) ... abs(x0i)+abs(x0r) */
      vcmpLEps maxval, x0, x1  /* all 1s if maxval already has max */
      vmovmskps x1, bitreg
      cmp $0xFF, bitreg_b
      jnz VNEWMAX
      UGOTMAX:
      add $64, N
   jnz UALOOP
   cmp $0, NN
   jnz CLEANUP
   jmp DONE


/*
 * When we jump to this label, we know that a new max can be found somewhere
 * in the sums stored in x0 in the following order:
 *   {x7, x6, x3, x2, x5, x4, x1, x0}
 * So, ignore old max, and just find the max of these 8 elts
 */
VNEWMAX:                                /* x7, x6, x3, x2, x5, x4, x1, x0 */
   vextractf128 $1, x0, up_             /* XX XX XX XX x7 x6 x3 x2 */
   movss x0_, maxval_
   lea (X,N), Imax
   vshufps $0x01, x0, x0, x1
   vcomiss x1_, maxval_  /* newmax if ZF=PF=0, CF=1 */
   jnc DONE1
   movss x1_, maxval_
   lea 8(X,N), Imax
DONE1:
   vcomiss up_, maxval_
   jnc DONE2
   movss up_, maxval_
   lea 16(X,N), Imax
DONE2:
   vshufps $0x01, up, up, x1
   vcomiss x1_, maxval_   /* newmax if ZF=PF=0, CF=1 */
   jnc DONE3
   movss x1_, maxval_
   lea 24(X,N), Imax
DONE3:                                /* x7, x6, x3, x2, x5, x4, x1, x0 */
   vshufps $0x02, x0, x0, x1
   vcomiss x1_, maxval_   /* newmax if ZF=PF=0, CF=1 */
   jnc DONE4
   movss x1_, maxval_
   lea 32(X,N), Imax
DONE4:                                /* x7, x6, x3, x2, x5, x4, x1, x0 */
   vshufps $0x03, x0, x0, x1
   vcomiss x1_, maxval_   /* newmax if ZF=PF=0, CF=1 */
   jnc DONE5
   movss x1_, maxval_
   lea 40(X,N), Imax
DONE5:                                  /* XX XX XX XX x7 x6 x3 x2 */
   vshufps $0x02, up, up, x1
   vcomiss x1_, maxval_   /* newmax if ZF=PF=0, CF=1 */
   jnc DONE6
   movss x1_, maxval_
   lea 48(X,N), Imax
DONE6:
   vshufps $0x03, up, up, x1
   vcomiss x1_, maxval_   /* newmax if ZF=PF=0, CF=1 */
   jnc DONE7
   movss x1_, maxval_
   lea 56(X,N), Imax
DONE7:
   vshufps $0x00, maxval, maxval, maxval
   vinsertf128 $1, maxval_, maxval, maxval
   test $0x1F, X
   jz AGOTMAX
   jmp UGOTMAX

CLEANUP:
   lea (X,NN,8), X
   neg NN
   CULOOP:
      movss (X,NN,8), x0_
      andps absval_, x0_
      movss 4(X,NN,8), x1_
      andps absval_, x1_
      addss x1_, x0_
      comiss x0_, maxval_   /* need new max if ZF=PF=0, CF=1 */
      jc SNEWMAX
      add $1, NN
   jnz CULOOP
   jmp DONE
SNEWMAX:
   movss x0_, maxval_
   lea (X,NN,8), Imax
   add $1, NN
   jnz CULOOP
   jmp DONE
/*
 * N must hold aligned X value
 */
FORCE_ALIGN:
   movss (X), x0_
   andps absval_, x0_
   movss 4(X), x1_
   andps absval_, x1_
   addss x1_, x0_
   comiss x0_, maxval_   /* need new max if ZF=PF=0, CF=1 */
   jc FA_NEWMAX
   sub $1, NN
   add $8, X
   cmp X, N
jnz FORCE_ALIGN
   vshufps $0x00, maxval, maxval, maxval  /* XX,XX,XX,XX, max,max,max,max */
   vinsertf128 $1, maxval_, maxval, maxval /* max in all 8 values */
   cmp $0, NN
   jnz XALIGNED
   jmp DONE
FA_NEWMAX:
   movss x0_, maxval_
   mov X, Imax
   sub $1, NN
   add $8, X
   cmp X, N
   jnz FORCE_ALIGN
   vshufps $0x00, maxval, maxval, maxval  /* XX,XX,XX,XX, max,max,max,max */
   vinsertf128 $1, maxval_, maxval, maxval /* max in all 8 values */
   cmp $0, NN
   jnz XALIGNED
   jmp DONE
