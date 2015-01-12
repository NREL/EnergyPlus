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

#define alp1    %xmm0
#define alp2    %xmm1
#define rY0     %xmm2
#define rX0     %xmm3
#define rX1     %xmm4
#define salp    %xmm5
#ifndef PFDIST
   #ifdef ATL_ARCH_P4E
      #define PFDIST 256   /* optimized for 32-bit P4E */
   #else
      #define PFDIST 512  /* opt for Athlon 64 X2 */
   #endif
#endif

# byte offset       %rdi   4          %rsi     8     %rdx    12      %rcx    16
# void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
#                TYPE *Y, const int incY)
#                %r8
        .text
.global ATL_asmdecor(ATL_UAXPY)
ATL_asmdecor(ATL_UAXPY):
#ifdef ATL_GAS_x8632
   #define OFF 8
        subl    $OFF, %esp
#
#       Put hi{1.0,-1.0}lo in rX0
#
        fld1
        fldz
        fsub    %st(1), %st
        fstps   0(%esp)
        fstps   4(%esp)
        movlps  (%esp), rX0        # rX0 = {XXX, XXX, 1.0, -1.0}
#
#       Store regs to stack and load parameters
#
        movl    %ebp, (%esp)
        movl    %ebx, 4(%esp)

        movl    OFF+4(%esp), N
        movl    OFF+8(%esp), Nr   # address of alpha
        movlps  (Nr),  alp1
        movl    OFF+12(%esp), X
        movl    OFF+20(%esp), Y
#else
        movq    %r8, Y
        movlps  (%rsi), alp1     # Load alpha
#
#       Put hi{1.0,-1.0}lo in rX0
#
        fld1
        fldz
        fsub    %st(1), %st
        fstps   -8(%rsp)
        fstps   -4(%rsp)
        movlps  -8(%rsp), rX0        # rX0 = {XXX, XXX, 1.0, -1.0}
#endif
        movlhps alp1, alp1             # alp1 = {ialpha, ralpha, ialpha, ralpha}
        prefetchw (Y)
        prefetchnta (X)
        movlhps rX0, rX0               # rX0  = {1.0   , -1.0  , 1.0   , -1.0  }
        pshufd  $0x11,alp1,alp2        # alp2 = {ralpha, ialpha, ralpha, ialpha}
        movaps  alp2, salp             # salp = {ralpha, ialpha, ralpha, ialpha}
        mulps   rX0, alp2              # alp2 = {ralpha, -ialph, ralpha, -ialph}
        mulss   rX0, salp              # salp = {ralpha, ialpha, ralpha, -ialph}
        pshufd  $0xE1,salp,salp        # salp = {ralpha, ialpha, -ialph, ralpha}
#
#       If X is only 4-byte aligned, it's alignment cannot be fixed,
#       so go to crap code
#
        test    $0x7, X
        jnz     NOXALIGN
#
#       Force X to 16-byte boundary so we can use MOVSxDUP
#
        test    $0xF, X
        jz      XALIGNED
#
#       One peeled iteration to force X to 16-byte alignment
#
                                # salp = { ra,  ia, -ia,  ra}
        movlps  (X), rX0        # rX0  = { XX,  XX,  ix,  rx}
        xorps   rY0, rY0        # get rid of junk in top 64 bits
        movlhps rX0, rX0        # rX0  = { ix,  rx,  ix,  rx}
        movlps  (Y), rY0        # rY0  = {  0,   0,  iy,  ry}
        mulps   salp, rX0       # rX0  = {ra*ix, ia*rx, -ia*ix, ra*rx}
        haddps  rX0, rX0        # rX0  = {XX,XX, ra*ix+ia*rx, ra*rx-iaix}
        addps   rX0, rY0        # rY0  = {XX,XX, iyN, ryN}
        movlps  rY0, (Y)
        sub     $1, N
        add     $8, X
        add     $8, Y
XALIGNED:
        test    $0xF, Y
        jnz     YUNALIGNED

        mov     N, Nr
        shr     $1, N
        shl     $1, N
        lea     (X, N, 8), X
        lea     (Y, N, 8), Y
        neg     N
        add     N, Nr
        cmp     $0, N
        je      CLEANUP
        ALIGN16
NLOOP:
                                        # alp1 = {ia,   ra,  ia,  ra}
                                        # alp2 = {ra,  -ia,  ra, -ia}
        movsldup (X,N,8), rX0           # rX0  = {rx1, rx1, rx0, rx0}
        movshdup (X,N,8), rX1           # rX1  = {ix1, ix1, ix0, ix0}
        movaps  (Y,N,8), rY0            # rY0  = {iy1, ry1, iy0, ry0}
        mulps   alp1, rX0               # rX0  = {ia*rx1,ra*rx1,ia*rx0,ra*rx0}
        prefetchw      PFDIST(Y,N,8)
        addps   rX0, rY0                # rY0  gets 1st part of results
        mulps   alp2, rX1               # rX1  = {ra*ix1,-ia*ix1,ra*ix0,-ia*ix0}
        prefetcht0     PFDIST(X,N,8)
        addps   rX1, rY0                # rY0 gets last part of results
        movapd  rY0, (Y,N,8)
        addq    $2, N
        jnz     NLOOP
#
#       Do one more scalar iteration if there's a remainder
#
CLEANUP:
        cmp     $0, Nr
        je      DONE
                                # salp = { ra,  ia, -ia,  ra}
        movlps  (X), rX0        # rX0  = { XX,  XX,  ix,  rx}
        xorps   rY0, rY0        # get rid of junk in top 64 bits
        movlhps rX0, rX0        # rX0  = { ix,  rx,  ix,  rx}
        movlps  (Y), rY0        # rY0  = {  0,   0,  iy,  ry}
        mulps   salp, rX0       # rX0  = {ra*ix, ia*rx, -ia*ix, ra*rx}
        haddps  rX0, rX0        # rX0  = {XX,XX, ra*ix+ia*rx, ra*rx-iaix}
        addps   rX0, rY0        # rY0  = {XX,XX, iyN, ryN}
        movlps  rY0, (Y)
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
#
# This code assumes aligned X, but unaligned Y
#
YUNALIGNED:
        mov     N, Nr
        shr     $1, N
        shl     $1, N
        lea     (X,N,8), X
        lea     (Y,N,8), Y
        neg     N
        add     N, Nr
        cmp     $0, N
        je      CLEANUP
YUNLOOP:
                                        # alp1 = {ia,   ra,  ia,  ra}
                                        # alp2 = {ra,  -ia,  ra, -ia}
        movsldup (X,N,8), rX0           # rX0  = {rx1, rx1, rx0, rx0}
        movshdup (X,N,8), rX1           # rX1  = {ix1, ix1, ix0, ix0}
        movups  (Y,N,8), rY0            # rY0  = {iy1, ry1, iy0, ry0}
        mulps   alp1, rX0               # rX0  = {ia*rx1,ra*rx1,ia*rx0,ra*rx0}
        prefetchw      PFDIST(Y,N,8)
        addps   rX0, rY0                # rY0  gets 1st part of results
        mulps   alp2, rX1               # rX1  = {ra*ix1,-ia*ix1,ra*ix0,-ia*ix0}
        prefetcht0     PFDIST(X,N,8)
        addps   rX1, rY0                # rY0 gets last part of results
        movupd  rY0, (Y,N,8)
        addq    $2, N
        jnz     YUNLOOP
        jmp     CLEANUP
#
# X is not aligned even on 8-byte boundary, so cannot align it at all
# This shouldn't happen much, so just implement the unaligned Y case,
# so this case implements neither vector aligned
#
NOXALIGN:
        mov     N, Nr
        shr     $1, N
        shl     $1, N
        lea     (X, N, 8), X
        lea     (Y, N, 8), Y
        neg     N
        add     N, Nr
        cmp     $0, N           # alp1 = { ia,  ra,  ia,  ra}
        je      CLEANUP         # salp = { ra,  ia, -ia,  ra}

        movsldup alp1, alp1     # alp1 = { ra,  ra,  ra,  ra}
        pshufd  $0x99,salp,alp2 # alp2 = { ia, -ia,  ia, -ia}
NOALOOP:
        movups  (X,N,8), rX0    # rX0  = {ix1, rx1, ix0, rx0}
        pshufd  $0xB1,rX0,rX1   # rX1  = {rx1, ix1, rx0, ix0}
        movups  (Y,N,8), rY0    # rY0  = {iy1, ry1, iy0, ry0}
        mulps   alp1, rX0       # rX0  = {ix1*ra, rx1*ra,ix0*ra, rx1*ra}
        prefetchw      PFDIST(Y,N,8)
        addps   rX0, rY0
        mulps   alp2, rX1       # rX1  = {rx1*ia,-ix1*ia,rx0*ia,-ix0*ia}
        prefetcht0     PFDIST(X,N,8)
        addps   rX1, rY0
        movups  rY0, (Y,N,8)
        add     $2, N
        jnz     NOALOOP
        jmp     CLEANUP
