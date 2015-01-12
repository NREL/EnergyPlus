#include "atlas_asm.h"

#ifndef ATL_SSE2
   #error "This kernel requires SSE2"
#endif
#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define X    %eax
   #define Y    %edx
   #define N    %ecx
   #define Nr   %ebp
#elif defined(ATL_GAS_x8664)
   #define N    %rdi
   #define X    %rsi
   #define Y    %rcx
   #define Nr   %rdx
#else
   #error "This kernel requires x86 assembly!"
#endif

#define alpha %xmm0
#define rY0   %xmm1
#define rX0   %xmm2

#ifndef PFDIST
   #ifdef ATL_ARCH_P4E
      #define PFDIST 384
   #else
      #define PFDIST 416
   #endif
#endif

# byte offset              4                  8              16             20
# void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
#                TYPE *Y, const int incY)
        .text
.global ATL_asmdecor(ATL_UAXPY)
ATL_asmdecor(ATL_UAXPY):
#ifdef ATL_GAS_x8632
   #define OFF 8
        subl    $OFF, %esp
        movl    %ebp, (%esp)
        movl    %ebx, 4(%esp)

        movl    OFF+4(%esp), N
        movlpd  OFF+8(%esp),  alpha
        movl    OFF+16(%esp), X
        movl    OFF+24(%esp), Y
#endif
        unpcklpd       alpha, alpha

        movq    N, Nr
        cmp     $4, N
        jbe     SCALAR_TEST

        movq    Y, Nr
        shr     $4, Nr
        shl     $4, Nr
        cmp     Nr, Y
        je      YALIGNED
        movlpd  (X), rX0
        mulsd   alpha, rX0
        movlpd  (Y), rY0
        subq    $1, N
        addsd   rX0, rY0
        addq    $8, X
        movlpd  rY0, (Y)
        addq    $8, Y
YALIGNED:
        movq    X, Nr
        shr     $4, Nr
        shl     $4, Nr
        cmp     Nr, X
        jne     XUNALIGNED

        movq    N, Nr
        shr     $1, N
        shl     $1, N
        sub     N, Nr
        lea     (X, N, 8), X
        lea     (Y, N, 8), Y
        neg     N
NLOOP:
        movapd  (X,N,8), rX0
        movapd  (Y,N,8), rY0
        mulpd   alpha, rX0
                                        prefetchw       PFDIST(Y,N,8)
        addpd   rX0, rY0
        movapd  rY0, (Y,N,8)
                                        prefetchnta     PFDIST(X,N,8)
        addq    $2, N
        jnz     NLOOP
#
#  This loop assumes num of iterations is in Nr
#
SCALAR_TEST:
        cmp     $0, Nr
        je      DONE
SLOOP:
        movlpd  (X), rX0
        mulsd   alpha, rX0
        movlpd  (Y), rY0
        addsd   rX0, rY0
        addq    $8, X
        movlpd  rY0, (Y)
        addq    $8, Y
        subq    $1, Nr
        jnz     SLOOP
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
        shr     $1, N
        shl     $1, N
        sub     N, Nr
        lea     (X, N, 8), X
        lea     (Y, N, 8), Y
        neg     N
UNLOOP:
        movupd  (X,N,8), rX0
        movapd  (Y,N,8), rY0
        mulpd   alpha, rX0
        addpd   rX0, rY0
        movapd  rY0, (Y,N,8)
        addq    $2, N
        jnz     UNLOOP
        jmp     SCALAR_TEST
