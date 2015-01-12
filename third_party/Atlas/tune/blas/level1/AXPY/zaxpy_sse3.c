#include "atlas_asm.h"

#ifndef ATL_SSE3
   #error "This kernel requires SSE3"
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
   #define X    %rdx
   #define Y    %rcx
   #define Nr   %rax
#else
   #error "This kernel requires x86 assembly!"
#endif

#define alpha1  %xmm0
#define alpha2  %xmm1
#define rY0     %xmm2
#define rX0     %xmm3
#define rX1     %xmm4
#ifndef PFDIST
   #define PFDIST 512   /* optimized for 32-bit P4E/Athlon-64 X2 */
#endif

# byte offset        %rdi  4        %rsi       8     %rdx    12      %rcx    16
# void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
#                TYPE *Y, const int incY)
#                %r8
        .text
.global ATL_asmdecor(ATL_UAXPY)
ATL_asmdecor(ATL_UAXPY):
#ifdef ATL_GAS_x8632
   #define OFF 16
        subl    $OFF, %esp
#
#       Put hi{1.0,-1.0}lo in rX0
#
        fld1
        fldz
        fsub    %st(1), %st
        fstpl   0(%esp)
        fstpl   8(%esp)
        movupd  (%esp), rX0        # rX0 = {1.0, -1.0}
#
#       Store regs to stack and load parameters
#
        movl    %ebp, (%esp)
        movl    %ebx, 4(%esp)

        movl    OFF+4(%esp), N
        movl    OFF+8(%esp), Nr   # address of alpha
        movupd  (Nr),  alpha1
        movl    OFF+12(%esp), X
        movl    OFF+20(%esp), Y
#else
        movq    %r8, Y
        movupd  (%rsi), alpha1
#
#       Put hi{1.0,-1.0}lo in rX0
#
        fld1
        fldz
        fsub    %st(1), %st
        fstpl   -16(%rsp)
        fstpl   -8(%rsp)
        movupd  -16(%rsp), rX0         # rX0 = {1.0, -1.0}
#endif
                                       # alpha1 = {ialpha, ralpha}
        pshufd  $0x4E,alpha1,alpha2    # alpha2 = {ralpha, ialpha}
        mulsd   rX0, alpha2            # alpha2 = {ralpha, -ialpha}
#
#       Move to unaligned loop if alignment is not 16-byte
#
        test    $0xF, Y
        jnz     UNALIGNED

        add     N, N         # double N so we can use it to index by 8
        lea     (X,N,8), X
        lea     (Y,N,8), Y
        neg     N
NLOOP:
                                        # alp2 = {ralpha, -ialpha}
                                        # alp1 = {ialpha, ralpha}
        movddup (X,N,8), rX0            # rX0  = {rx,    rx}
        movddup 8(X,N,8), rX1           # rX1  = {ix,    ix}
        movapd  (Y,N,8), rY0            # rY0  = {iy,    ry}
        mulpd   alpha1, rX0             # rX0  = {rx*ia, rx*ra}
        prefetchw      PFDIST(Y,N,8)
        addpd   rX0, rY0                # rY0  = {iy+rx*ia, ry+rx*ra}
        mulpd   alpha2, rX1             # rX1  = {ra*ix, -ia*ix}
        addpd   rX1, rY0                # rY0 = {iy+rx*ia+ra*ix, ry+rx*ra-ia*ix}
        prefetchnta    PFDIST(X,N,8)
        movapd  rY0, (Y,N,8)
        addq    $2, N
        jnz     NLOOP
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
UNALIGNED:
        add     N, N
        lea     (X,N,8), X
        lea     (Y,N,8), Y
        neg     N
UNLOOP:
                                        # alp2 = {ralpha, -ialpha}
                                        # alp1 = {ialpha, ralpha}
        movddup (X,N,8), rX0            # rX0  = {rx,    rx}
        movddup 8(X,N,8), rX1           # rX1  = {ix,    ix}
        movupd  (Y,N,8), rY0            # rY0  = {iy,    ry}
        mulpd   alpha1, rX0             # rX0  = {rx*ia, rx*ra}
        prefetchw      PFDIST(Y,N,8)
        addpd   rX0, rY0                # rY0  = {iy+rx*ia, ry+rx*ra}
        mulpd   alpha2, rX1             # rX1  = {ra*ix, -ia*ix}
        addpd   rX1, rY0                # rY0 = {iy+rx*ia+ra*ix, ry+rx*ra-ia*ix}
        prefetchnta    PFDIST(X,N,8)
        movupd  rY0, (Y,N,8)
        addq    $2, N
        jnz     UNLOOP
        jmp     DONE
