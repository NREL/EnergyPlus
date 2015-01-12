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
#include "atlas_asm.h"

#ifndef ATL_AVX
   #error "This kernel requires AVX"
#endif

/*
 * vector registers
 */
#define ralp %ymm0
   #define ralp_ %xmm0
#define ialp %ymm1
   #define ialp_ %xmm1
#define y0   %ymm2
   #define y0_ %xmm2
#define x0   %ymm3
   #define x0_ %xmm3
#define x1   %ymm4
   #define x1_ %xmm4
/*
 * Integer registers
 */
#ifdef ATL_GAS_x8632
   #define N  %ebx
   #define X  %edx
   #define Y  %ecx
   #define II %eax
#elif defined(ATL_GAS_x8664)
   #define N       %rdi
   #define X       %rdx
   #define Y       %rcx
   #define II      %rax
#else
   #error "This routine requires x86 assembly!"
#endif
#define Y_b  %cl
#define X_b  %dl
#define II_b %al

#ifndef PFW
   #define PFW prefetchnta
#endif
#ifndef PFR
   #define PFR prefetchnta
#endif
#ifndef PFDIST
   #define PFDIST 768
#endif
/*
                    rdi/ 4              rsi/8         rdx/12          rcx/16
void ATL_UAXPY(const int N, const TYPE *alpha, const TYPE *X, const int incX,
                 r8/20          r9/24
               TYPE *Y, const int incY)
*/
.text
.globl ATL_asmdecor(ATL_UAXPY)
ATL_asmdecor(ATL_UAXPY):

#ifdef ATL_GAS_x8632
   #define FSIZE 16
   sub $FSIZE, %esp
   movl %ebx, (%esp)
   movl %esi, 4(%esp)
   movl FSIZE+4(%esp), N
   movl FSIZE+8(%esp), %esi
   movl FSIZE+12(%esp), %edx
   movl FSIZE+20(%esp), Y
   #define TMPOFF 8(%esp)
   #define rsi esi
#else
   mov %r8, Y
   #define TMPOFF -8(%rsp)
#endif
   fld1                                 /* ST = {1.0} */
   fldz                                 /* ST = {0.0, 1.0} */
   PFR (X)
   fsubp                                /* ST = {-1.0} */
   fmull 8(%rsi)                        /* ST = {-ai} */
   fstpl TMPOFF                         /* ST={}, store -ia to tmp */
   PFW (Y)
   vbroadcastsd 8(%rsi), ialp           /*  ai  ai  ai  ai */
   vbroadcastsd TMPOFF, ralp          /* -ia -ia -ia -ia */
   vblendpd $0x5, ralp, ialp, ialp      /*  ai -ai  ai -ai */
   vbroadcastsd (%rsi), ralp            /*  ar  ar  ar  ar */
/*
 * If Y is not 16-byte aligned, then it can never be 32-byte aligned
 */
   test $0x0F, Y_b
   jnz UNALIGNED
   test $0x1F, Y_b
   jz YALIGNED    /* jump to Y known to 32-byte aligned */
/*
 * If we reach here, Y is 16-byte aligned, so peel 1 iteration to make 32-byte
 */
   movupd (X), x0_              /* x0 = {xi, xr} */
   pshufd $0x4E, x0_, x1_       /* x1 = {xr, xi} */
   movapd (Y), y0_              /* y0 = {yi, yr} */
   mulpd ralp_, x0_             /* x0 = {ar*xi, ar*xr} */
   addpd x0_, y0_
   mulpd ialp_, x1_             /* x1 = {ai*xr,-ai*xi} */
   addpd x1_, y0_
   movapd y0_, (Y)
   add $16, X
   add $16, Y
   sub $1, N
   jz DONE
YALIGNED:   /* Y is known to be 32-byte aligned */
   cmp $4, N
   jb CLEANUP
   mov N, II
   andb $0xFE, II_b                     /* make II a multiple of veclen */
   sub II, N                            /* N now has how much must be cleaned */
   shl $4, II                           /* II = N*sizeof(DCPLX) */
   lea (X, II), X                       /* X += N */
   lea (Y, II), Y                       /* Y += N */
   neg II                               /* II = -II */
   test $0x1F, X_b                      /* if X not 32-byte aligned */
   jnz YAXULOOP                         /* jump to unaligned X loop */
   YAXALOOP:
      vmovapd (X,II), x0                /* x1i x1r x0i x0r */
      vshufpd $0x5, x0, x0, x1           /* x1r x1i x0r x0i */
      vmulpd ralp, x0, x0               /* ar*x1i, ar*x1r, ar*x0i, ar*x0r */
      vaddpd (Y,II), x0, y0
      PFR PFDIST(X,II)
      vmulpd ialp, x1, x1               /* ai*x1r,-ai*x1i, ai*x0r,-ai*x0i */
      vaddpd x1, y0, y0
      PFW PFDIST(Y,II)
      vmovapd y0, (Y, II)
      add $32, II
   jnz YAXALOOP

   cmp $0, N
   jnz CLEANUP
DONE:
#ifdef ATL_GAS_x8632
   movl (%esp), %ebx
   movl 4(%esp), %esi
   add $FSIZE, %esp
#endif
   ret

   YAXULOOP:
      vmovupd (X,II), x0                /* x1i x1r x0i x0r */
      vshufpd $0x5, x0, x0, x1           /* x1r x1i x0r x0i */
      vmulpd ralp, x0, x0               /* ar*x1i, ar*x1r, ar*x0i, ar*x0r */
      vaddpd (Y,II), x0, y0
      PFR PFDIST(X,II)
      vmulpd ialp, x1, x1               /* ai*x1r,-ai*x1i, ai*x0r,-ai*x0i */
      vaddpd x1, y0, y0
      PFW PFDIST(Y,II)
      vmovapd y0, (Y, II)
      add $32, II
   jnz YAXULOOP
   cmp $0, N
   jz DONE
   jmp CLEANUP
UNALIGNED:
   cmp $4, N
   jb CLEANUP
   mov N, II
   andb $0xFE, II_b                     /* make II a multiple of veclen */
   sub II, N                            /* N now has how much must be cleaned */
   shl $4, II                           /* II = N*sizeof(DCPLX) */
   lea (X, II), X                       /* X += N */
   lea (Y, II), Y                       /* Y += N */
   neg II                               /* II = -II */
   YUXULOOP:
      vmovupd (X,II), x0                /* x1i x1r x0i x0r */
      vshufpd  $0x5, x0, x0, x1         /* x1r x1i x0r x0i */
      vmovupd (Y,II), y0
      vmulpd ralp, x0, x0               /* ar*x1i, ar*x1r, ar*x0i, ar*x0r */
      vaddpd x0, y0, y0
      PFR PFDIST(X,II)
      vmulpd ialp, x1, x1               /* ai*x1r,-ai*x1i, ai*x0r,-ai*x0i */
      vaddpd x1, y0, y0
      PFW PFDIST(Y,II)
      vmovupd y0, (Y, II)
      add $32, II
   jnz YUXULOOP
   cmp $0, N
   jz DONE

CLEANUP:
CULOOP:
   movupd (X), x0_              /* x0 = {xi, xr} */
   pshufd $0x4E, x0_, x1_       /* x1 = {xr, xi} */
   movupd (Y), y0_              /* y0 = {yi, yr} */
   mulpd ralp_, x0_             /* x0 = {ar*xi, ar*xr} */
   addpd x0_, y0_
   mulpd ialp_, x1_             /* x1 = {ai*xr,-ai*xi} */
   addpd x1_, y0_
   movupd y0_, (Y)
   add $16, X
   add $16, Y
   sub $1, N
jnz CULOOP
jmp DONE
