#include "atlas_asm.h"
#if !defined(ATL_GAS_x8632) && !defined(ATL_GAS_x8664)
   #error "This kernel requires x86 gas 32 or 64 bit x86 assembler!"
#endif

#ifdef ATL_GAS_x8664
   #define N            %rdi
   #define X            %rsi
   #define incX         %rdx
#else
   #define N            %eax
   #define X            %edx
   #define incX         %ecx
   #define movq         movl
#endif

#define pref(mem) prefetcht0        mem

/*                  rdi/ 4         rsi/ 8          rdx/12
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
*/
        .text
.global ATL_asmdecor(ATL_UNRM2)
ATL_asmdecor(ATL_UNRM2):
   #ifdef ATL_GAS_x8664
#
#       Since incX might be negative, must convert it to 64 bit
#
        movl    %edx, %eax
        cltq
        movq    %rax, incX
   #else
#
#       Load operands to regs
#
        movl    4(%esp), N
        movl    8(%esp), X
        movl    12(%esp), incX
   #endif

   #if SREAL
        #define fldl flds
        shl     $2, incX        # incX *= sizeof(float)
   #else
        shl     $3, incX        # incX *= sizeof(double)
   #endif
        fldz
ALIGN4
LOOP1:
        pref((X,incX,8))
        fldl    (X)
        fmul    %st, %st
        faddp
        add     incX, X
        sub     $1, N
        jnz     LOOP1
        fsqrt
#
#       Put return val in right reg, and return
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
