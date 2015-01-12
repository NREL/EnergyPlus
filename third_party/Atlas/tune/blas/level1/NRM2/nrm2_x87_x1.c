#include "atlas_asm.h"
#if !defined(ATL_GAS_x8632) && !defined(ATL_GAS_x8664)
   #error "This kernel requires x86 gas 32 or 64 bit x86 assembler!"
#endif

#ifdef ATL_GAS_x8664
   #define N            %rdi
   #define X            %rsi
#else
   #define N            %eax
   #define X            %edx
#endif

#define pref(mem)       prefetcht0 mem
#if defined(ATL_ARCH_HAMMER) && !defined(SREAL)
   #define PFDIST 72
#else
   #define PFDIST 192
#endif

/*                  rdi/ 4         rsi/ 8          rdx/12
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
*/
        .text
.global ATL_asmdecor(ATL_UNRM2)
ATL_asmdecor(ATL_UNRM2):
   #ifdef ATL_GAS_x8632
#
#       Load operands to registers
#
        movl    4(%esp), N
        movl    8(%esp), X
   #endif

        fldz
   #ifdef SREAL
        #define fldl flds
        shl     $2, N
   #else
        shl     $3, N
   #endif
        add     N, X
        neg     N
ALIGN4
LOOP1:
        fldl    (X, N)
        fmul    %st, %st
        faddp
        pref(PFDIST(X,N))
   #ifdef SREAL
        add     $4, N
   #else
        add     $8, N
   #endif
ALIGN4
        jnz     LOOP1

        fsqrt
#
#       Put return val in right register and return
#
#ifdef ATL_GAS_x8664
   #ifdef SREAL
        fstps   -4(%rsp)
        movss   -4(%rsp), %xmm0
   #else
        fstpl   -8(%rsp)
        movsd   -8(%rsp), %xmm0
   #endif
#endif
        ret
