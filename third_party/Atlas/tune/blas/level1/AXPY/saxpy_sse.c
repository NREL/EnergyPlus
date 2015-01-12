#include "atlas_asm.h"

#ifndef ATL_SSE2
   #error "This kernel requires SSE2"
#endif
#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define X    %ebp
   #define Y    %edx
   #define N    %ecx
   #define Nr   %eax
   #define Nr_b %al
   #define JTRG %ebx
#elif defined(ATL_GAS_x8664)
   #define N    %rdi
   #define X    %rsi
   #define Y    %rcx
   #define Nr   %rax
   #define Nr_b %al
   #define JTRG %rdx
#else
   #error "This kernel requires x86 assembly!"
#endif

#define alpha %xmm0
#define rY0   %xmm1
#define rX0   %xmm2

#ifndef PFDIST
   #ifdef ATL_ARCH_P4E
      #define PFDIST 192
   #else
      #define PFDIST 3072
   #endif
#endif

# byte offset              4                  8              12             16
# void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
#                TYPE *Y, const int incY)
        .text
.global ATL_asmdecor(ATL_UAXPY)
ATL_asmdecor(ATL_UAXPY):
#ifdef ATL_GAS_x8632
   #define OFF 12
        subl    $OFF, %esp
        movl    %ebp, (%esp)
        movl    %ebx, 4(%esp)

        movl    OFF+4(%esp), N
        movss   OFF+8(%esp),  alpha
        movl    OFF+12(%esp), X
        movl    OFF+20(%esp), Y
#endif
        prefetchw       (Y)
        prefetcht0      (X)
        shufps  $0x00, alpha, alpha   # alpha = {alpha,alpha,alpha,alpha}

        movq    N, Nr
        xor     JTRG, JTRG
        cmp     $7, N
        jbe     SCALAR_TEST
#
#       Nr = (((char*)Y+15)/16)*16 - Y
#
        movq    $1, JTRG
        lea     15(Y), Nr
        andb    $0xF0, Nr_b
        subq    Y, Nr
        jnz     FORCE_ALIGN
YALIGNED:
        test    $0xF, X
        jnz     XUNALIGNED

        movq    N, Nr
        shr     $2, N
        shl     $2, N
        sub     N, Nr
        lea     (X,N,4), X
        lea     (Y,N,4), Y
        neg     N
NLOOP:
        movaps  (X,N,4), rX0
        movaps  (Y,N,4), rY0
        mulps   alpha, rX0
                                prefetchw      PFDIST(Y,N,8)
        addps   rX0, rY0
        movaps  rY0, (Y,N,4)
        addq    $4, N
        jnz     NLOOP

        xor     JTRG, JTRG
        cmp     $0, Nr
        jne     SCALAR_TEST
#
#       Epilogue
#
DONE:
#ifdef ATL_GAS_x8632
        movl    (%esp), %ebp
        movl    4(%esp), %ebx
        addl    $OFF, %esp
#endif
        ret
XUNALIGNED:
        movq    N, Nr
        shr     $2, N
        shl     $2, N
        sub     N, Nr
        lea     (X,N,4), X
        lea     (Y,N,4), Y
        neg     N
UNLOOP:
        movups  (X,N,4), rX0
        movaps  (Y,N,4), rY0
        mulps   alpha, rX0
                                prefetchw      PFDIST(Y,N,8)
        addps   rX0, rY0
        movaps  rY0, (Y,N,4)
        addq    $4, N
        jnz     UNLOOP
        xor     JTRG, JTRG
        jmp     SCALAR_TEST
#
#       Assumes Nr has number of bytes until aligned
#
FORCE_ALIGN:
        shr     $2, Nr    # Nr = (Ya-Y)/sizeof(float)
        cmp     N, Nr
        cmova   N, Nr     # Nr = MIN(N,Nr)
        sub     Nr, N
#
#  This loop assumes num of iterations is in Nr, return @ in JTRG
#  NOTE: to aid portability, changed JTRG to boolean, 0 means jump to DONE,
#        1 means jump to  YALIGNED
#
SCALAR_TEST:
        cmp     $0, Nr
        je      DONE
        lea     (X,Nr,4), X
        lea     (Y,Nr,4), Y
        neg     Nr
SLOOP:
        movss   (X,Nr,4), rX0
        mulss   alpha, rX0
        movss   (Y,Nr,4), rY0
        addss   rX0, rY0
        movss   rY0, (Y,Nr,4)
        addq    $1, Nr
        jnz     SLOOP
        cmp     $0, JTRG
        je      DONE
        jmp     YALIGNED
