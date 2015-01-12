#include "atlas_asm.h"

#ifdef SREAL

#ifndef ATL_SSE1
   #error "This kernel requires SSE1"
#endif
#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define rsp  esp
   #define rax  eax
#elif !defined(ATL_GAS_x8664)
   #error "This kernel requires a gas x86 assembler!"
#endif
#ifdef ATL_GAS_x8632
   #define N    %eax
   #define X    %edx
   #define stX  %ecx
   #define stXF %ebx
#else
   #define N	%rax
   #define X	%rsi
   #define stX	%rdi
   #define stXF	%rdx
#endif

#define absval  %xmm0
#define rX0     %xmm1
#define rX1     %xmm2
#define rX2     %xmm3
#define rX3     %xmm4
#define sum0    %xmm5
#define sum1    %xmm6
#define sum2    %xmm7

# BYTE:                    4              8
# TYPE ATL_UASUM(const int N, const TYPE *X, const int incX)
        .text
.global	ATL_asmdecor(ATL_UASUM)
ATL_asmdecor(ATL_UASUM):
#ifdef ATL_GAS_x8632
	subl	$16, %esp
   #define OFF 0
#else
   #define OFF -16
#endif
#
#       Temporarily store 1.0 and -1.0 to stack
#
	fld1
	fldz
	fsub 	%st(1), %st
	fstps	OFF(%rsp)
	fstps	OFF+4(%rsp)
#
#	absval = (-1.0 ^ 1.0) = sign bit
#
	movss	OFF(%rsp), absval
	movss	OFF+4(%rsp), rX1
	xorps	rX1, absval
#
#       eax = all bits set
#
	xorl 	%eax, %eax
	notl	%eax
	movl	%eax, OFF(%rsp)
	movss	OFF(%rsp), rX1
	andnps	rX1, absval
	shufps	$0x00, absval, absval
#ifdef ATL_GAS_x8632
#
#       Save iregs
#
	movl	%ebx, (%esp)
#
#       N = N, X = X, stXF = X + N
#
	movl	20(%esp), N
	movl	24(%esp), X
#else
	movl	%edi, %eax
	cltq
#endif
	movq	N, stXF
	shl	$2, stXF
	addq	X, stXF
#
#	Get X aligned to 16 byte boundary
#
        xorps   sum0, sum0
	movq	X, stX
	shr	$4, stX
	shl	$4, stX
	cmp	X, stX
	jne	FORCE_ALIGN
ALIGNED_START:
	movq	N, stX
	shr	$4, stX
	jz	UNALIGNED
	shl	$6, stX
	addq	X, stX
	xorps	sum1, sum1
	xorps	sum2, sum2
ALIGNED_LOOP:
	movaps	(X), rX0
	movaps	16(X), rX1
	movaps	32(X), rX2
	movaps	48(X), rX3
	andps	absval, rX0
   #if defined(ATL_ARCH_HAMMER64) || defined(ATL_ARCH_HAMMER32)
	        prefetchnta 396(X)
   #else
	        prefetchnta 296(X)
   #endif
	andps	absval, rX1
	addps	rX0, sum0
	andps	absval, rX2
	addps	rX1, sum1
	andps	absval, rX3
	addps	rX2, sum2
        addps   rX3, sum0
	addq	$64, X
	cmp	X, stX
	jne	ALIGNED_LOOP
#
	addps	sum1, sum0
        addps   sum2, sum0
	movhlps	sum0, sum1
	addps	sum1, sum0
	movss	sum0, sum1
	shufps	$0x55, sum0, sum0
	addss	sum1, sum0
        cmp     X, stXF
        jne     UNALIGNED_LOOP
#
#	Restore iregs, return value
#
DONE:
#ifdef ATL_GAS_x8632
	movl	(%esp), %ebx
	movss	sum0, (%esp)
	flds	(%esp)
	addl	$16, %esp
#else
	movss	sum0, %xmm0
#endif
	ret
FORCE_ALIGN:
	movss	(X), rX0
	andps	absval, rX0
	addss	rX0, sum0
	addq	$4, X
	movq	X, stX
	shr	$4, stX
	shl	$4, stX
	dec	N
	cmp	X, stX
	je	ALIGNED_START
	cmp	X, stXF
	jne	FORCE_ALIGN
	jmp	DONE
UNALIGNED:
	cmp	X, stXF
	je	DONE
UNALIGNED_LOOP:
	movss	(X), rX0
	andps	absval, rX0
	addss	rX0, sum0
	addq	$4, X
	cmp	X, stXF
	jne	UNALIGNED_LOOP
        jmp     DONE

#else

#ifndef ATL_SSE2
   #error "This kernel requires SSE2"
#endif
#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define rsp  esp
   #define rax  eax
#elif !defined(ATL_GAS_x8664)
   #error "This kernel requires a gas x86 assembler!"
#endif
#ifdef ATL_GAS_x8632
   #define N    %eax
   #define X    %edx
   #define stX  %ecx
   #define stXF %ebx
#else
   #define N	%rax
   #define X	%rsi
   #define stX	%rdi
   #define stXF	%rdx
#endif

#define absval  %xmm0
#define rX0     %xmm1
#define rX1     %xmm2
#define rX2     %xmm3
#define rX3     %xmm4
#define sum0    %xmm5
#define sum1    %xmm6
#define sum2    %xmm7

# BYTE:                    4              8
# TYPE ATL_UASUM(const int N, const TYPE *X, const int incX)
        .text
.global	ATL_asmdecor(ATL_UASUM)
ATL_asmdecor(ATL_UASUM):
#ifdef ATL_GAS_x8632
	subl	$16, %esp
   #define OFF 0
#else
   #define OFF -16
#endif
#
#       Temporarily store 1.0 and -1.0 to stack
#
	fld1
	fldz
	fsub 	%st(1), %st
	fstpl	OFF(%rsp)
	fstpl	OFF+8(%rsp)
#
#	absval = (-1.0 ^ 1.0) = sign bit
#
	movlpd	OFF(%rsp), absval
	movlpd	OFF+8(%rsp), rX1
	xorpd	rX1, absval
#
#       eax = all bits set
#
	xorl 	%eax, %eax
	notl	%eax
	movl	%eax, OFF(%rsp)
	movl	%eax, OFF+4(%rsp)
	movlpd	OFF(%rsp), rX1
	andnpd	rX1, absval
	unpcklpd	absval, absval
#ifdef ATL_GAS_x8632
#
#       Save iregs
#
	movl	%ebx, (%esp)
#
#       N = N, X = X, stXF = X + N
#
	movl	20(%esp), N
	movl	24(%esp), X
#else
	movl	%edi, %eax
	cltq
#endif
	movq	N, stXF
	shl	$3, stXF
	addq	X, stXF
#
#	If X is not aligned to 16 byte boundary, peel 1 iteration
#
        xorpd   sum0, sum0
	movq	X, stX
	shr	$4, stX
	shl	$4, stX
	cmp	X, stX
	je	ALIGNED_START
	movlpd	(X), sum0
	andpd	absval, sum0
	addq	$8, X
	dec	N
	jz	DONE
#
#       If still not aligned after peeling, go to unaligned loop
#
	movq	X, stX
	shr	$4, stX
	shl	$4, stX
	cmp	X, stX
	jne	UNALIGNED_LOOP
ALIGNED_START:
	movq	N, stX
	shr	$3, stX
	jz	UNALIGNED_LOOP
	shl	$6, stX
	addq	X, stX
	xorpd	sum1, sum1
	xorpd	sum2, sum2
ALIGNED_LOOP:
	movapd	(X), rX0
	movapd	16(X), rX1
	movapd	32(X), rX2
	movapd	48(X), rX3
	andpd	absval, rX0
   #if defined(ATL_ARCH_HAMMER64) || defined(ATL_ARCH_HAMMER32)
	        prefetchnta 640(X)
   #else
	        prefetchnta 1024(X)
   #endif
	andpd	absval, rX1
	addpd	rX0, sum0
	andpd	absval, rX2
	addpd	rX1, sum1
	andpd	absval, rX3
	addpd	rX2, sum2
        addpd   rX3, sum0
	addq	$64, X
	cmp	X, stX
	jne	ALIGNED_LOOP
#
	addpd	sum1, sum0
        addpd   sum2, sum0
	movapd	sum0, sum1
	unpckhpd	sum1, sum1
	addsd	sum1, sum0
        cmp     X, stXF
        jne     UNALIGNED_LOOP
#
#	Restore iregs, return value
#
DONE:
#ifdef ATL_GAS_x8632
	movl	(%esp), %ebx
	movlpd	sum0, (%esp)
	fldl	(%esp)
	addl	$16, %esp
#else
	movsd	sum0, %xmm0
#endif
	ret
UNALIGNED_LOOP:
	movlpd	(X), rX0
	andpd	absval, rX0
	addsd	rX0, sum0
	addq	$8, X
	cmp	X, stXF
	jne	UNALIGNED_LOOP
        jmp     DONE

#endif
