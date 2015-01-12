#include "atlas_asm.h"

#ifdef SREAL

#ifndef ATL_SSE1
   #error "This routine requires SSE1"
#endif

#ifdef ATL_GAS_x8632
   #define X	%edx
   #define N	%ecx
   #define N2   %eax
   #define rsp  esp
#elif defined(ATL_GAS_x8664)
   #define X	%rsi
   #define N	%edi
   #define N2   %eax
#else
   #error "This kernel requires a gas x86 assembler!"
#endif


#                %edi         %xmm0     %rsi
# void ATL_USET(int N, float alpha, float *X, int incX)
#
        .text
.global	ATL_asmdecor(ATL_USET)
ATL_asmdecor(ATL_USET):
#ifdef ATL_GAS_x8632
        subl    $8, %esp
	movl	12(%esp), N
	movl	16(%esp), N2
	movl	20(%esp), X
        movl    N2, (%esp)
        movl    N2, 4(%esp)
        movq    (%esp), %mm0
   #define OFF 0
#else
	movss	%xmm0, -8(%rsp)
	movss	%xmm0, -4(%rsp)
	movq	-8(%rsp), %mm0
   #define OFF -8
#endif
#
#       If N is not divisable by two, peal first iteration
#
        movl    N, N2
        shr     $1, N2
        shl     $1, N2
        cmp     N, N2
        je      GOGO
        movl    OFF(%rsp), N2
        movl    N2, (X)
        add     $4, X
GOGO:
        shr     $1, N
        jz      DONE
	ALIGN16
LOOP1:
	movntq	%mm0, (X)
	add 	$8, X
	dec 	N
	jnz LOOP1
#
#	All done here
#
DONE:
	sfence
	emms
#ifdef ATL_GAS_x8632
        addl    $8, %esp
#endif
	ret

#else

#ifdef ATL_GAS_x8632
   #define X	%edx
   #define N	%ecx
#elif defined(ATL_GAS_x8664)
   #define X	%rsi
   #define N	%edi
#else
   #error "This kernel requires a gas x86 assembler!"
#endif
#ifndef ATL_SSE1
   #error "This routine requires SSE1"
#endif

#                %edi         %xmm0       %rsi
# void ATL_USET(int N, double alpha, double *X, int incX)
#
        .text
.global	ATL_asmdecor(ATL_USET)
ATL_asmdecor(ATL_USET):
#ifdef ATL_GAS_x8632
	movl	4(%esp), N
	movq	8(%esp), %mm0
	movl	16(%esp), X
#else
	movlpd	%xmm0, -8(%rsp)
	movq	-8(%rsp), %mm0
#endif
	ALIGN16
LOOP1:
	movntq	%mm0, (X)
	add 	$8, X
	decl	N
	jnz LOOP1
#
#	All done here
#
	sfence
	emms
	ret

#endif
