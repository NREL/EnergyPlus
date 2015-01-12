#include "atlas_asm.h"

#ifdef ATL_GAS_x8632
   #define movQ movl
   #define addq addl
   #define subq subl
   #define rsp  esp
#elif !defined(ATL_GAS_x8664)
   #error "This kernel requires a gas x86 assembler!"
#endif

#ifdef ATL_GAS_x8632
   #define nblk	%ebx
   #define N	%eax
   #define X	%esi
   #define Y	%ecx
   #define stX	%edx
   #define stXF	%edi
#else
   #define movQ movq
   #define nblk	%r8
   #define N	%rax
   #define X	%rsi
   #define Y	%rcx
   #define stX	%rdx
   #define stXF	%rdi
#endif
#define NB 512
#ifndef NB
   #error "Undefined NB!"
#endif

#if NB == 8192
    #define SH 13
#elif NB == 4096
    #define SH 12
#elif NB == 2048
    #define SH 11
#elif NB == 1024
    #define SH 10
#elif NB == 512
    #define SH 9
#elif (NB == 256)
    #define SH 8
#endif
#                %edi     %rsi      %rdx     %rcx       %r8
#void ATL_UCOPY(int N, TYPE *X, int incX, TYPE *Y, int incY)
#
        .text
.global	ATL_asmdecor(ATL_UCOPY)
ATL_asmdecor(ATL_UCOPY):
#ifdef ATL_GAS_x8632
        subl    $16, %esp
        movl    %ebx, (%esp)
        movl    %esi, 4(%esp)
        movl    %edi, 8(%esp)
        movl    %ebp, 12(%esp)
        movl    20(%esp), N
        movl    24(%esp), X
        movl    32(%esp), Y
#else
	movq	%rbp, %r11
	movslq	%edi, N
#endif
	movQ	N, stXF
	shl	$3, stXF
	addq	X, stXF
#
#       Find how many NB-size chunks we have got, bail if 0
#
	movQ	N, nblk
	shr	$SH, nblk
	jz	LOOP1

LOOPB:
#
#	Burst load X
#
	movQ	X, stX
	addq	$NB*8, stX
	ALIGN16
BURST:
	movl	-64(stX), %ebp
	movl	-128(stX), %ebp
	subq	$128, stX
	cmp	X, stX
	jne	BURST

	addq	$NB*8, stX
	ALIGN16
LOOP8:
#	prefetchnta	1024(X)
	movq	(X), %mm0
	movq	8(X), %mm1
	movq	16(X), %mm2
	movq	24(X), %mm3
	movq	32(X), %mm4
	movq	40(X), %mm5
	movq	48(X), %mm6
	movq	56(X), %mm7

	movntq	%mm0, (Y)
	movntq	%mm1, 8(Y)
	movntq	%mm2, 16(Y)
	movntq	%mm3, 24(Y)
	movntq	%mm4, 32(Y)
	movntq	%mm5, 40(Y)
	movntq	%mm6, 48(Y)
	movntq	%mm7, 56(Y)

	addq	$64, Y
	addq	$64, X
	cmp	X, stX
	jne	LOOP8
#
#       Keep going until out of blocks
#
	subq	$1, nblk
	jnz	LOOPB

	cmp X, stXF
	je	DONE
LOOP1:
	movq	(X), %mm0
	movntq	%mm0, (Y)
	addq	$8, Y
	addq	$8, X
	cmp	X, stXF
	jne	LOOP1
DONE:
	sfence
	emms
#ifdef ATL_GAS_x8632
        movl    (%esp), %ebx
        movl    4(%esp), %esi
        movl    8(%esp), %edi
        movl    12(%esp), %ebp
        addl    $16, %esp
#else
	movq	%r11, %rbp
#endif
	ret
