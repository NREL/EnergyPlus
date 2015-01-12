#include "atlas_asm.h"

#ifdef SREAL
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
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
   #define N	%eax
   #define X	%edx
   #define maxX	%ecx
   #define X0	%edi
   #define N8   %ebp
   #define reg1 %ebx
   #define reg2 %esi
#else
   #define N	%rax
   #define X	%rsi
   #define maxX	%rcx
   #define X0	%rdi
   #define N8   %rdx
   #define reg1	%r8
   #define reg2 %r9
#endif

#define maxval  %xmm0
#define absval	%xmm1
#define rX0     %xmm2
#define rX1     %xmm3
#define rX2     %xmm4
#define rX3     %xmm5

# IREG                   rdi            rsi
# int ATL_UIAMAX(const int N, const TYPE *X, const int incX)

# IREG                   rdi            rsi
# int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
        .text
.global	ATL_asmdecor(ATL_UIAMAX)
ATL_asmdecor(ATL_UIAMAX):
#ifdef ATL_GAS_x8632
	subl	$16, %esp
   #define SOFF 0
#else
   #define SOFF -8
#endif
#
#	Temporarily store 1.0 and -1.0 to stack
#
	fld1
	fldz
	fsub	%st(1), %st
	fstps	SOFF(%rsp)
	fstps	SOFF+4(%rsp)
#
#       eax = all bits 1
#
	xorl	%eax, %eax
	notl	%eax
#
#	absval = (-1.0 ^ 1.0) = sign bit only
#
	movss	SOFF(%rsp), absval
	movss	SOFF+4(%rsp), rX0
	xorps	rX0, absval
#
#       absval = NOT(sign bit) & (all ones) == all bits but sign bet set
#
	movl	%eax, SOFF(%rsp)
	movss	SOFF(%rsp), rX0
	andnps	rX0, absval
	shufps	$0x00, absval, absval
#ifdef ATL_GAS_x8632
#
#       Save iregs
#
	movl	%edi, (%esp)
	movl	%ebp, 4(%esp)
        movl    %ebx, 8(%esp)
	movl	%esi, 12(%esp)
#
	movl	20(%esp), N
	movl	24(%esp), X
#
	movl	X, X0
	movl	X, maxX
	cmp	$1, N
	jbe	DONE
#else
#
#       X already in right register, X0 = X, maxX = X, init N
#
	movl	%edi, %eax
	movq	X, X0
	movq	X, maxX
	cmp	$1,%eax
	jbe	DONE
	cltq
#endif
        xorps   maxval, maxval
#
#       Get X aligned to 16 byte boundary
#
	test	$15, X
	jnz	FORCEALIGN

ALIGNED_STARTUP:
        movq    N, N8
        shr     $4, N8
        jz      LOOP1
	shl	$4, N8
	subq	N8, N
	shr	$4, N8
LOOP8:
   #if defined(ATL_ARCH_HAMMER64) || defined(ATL_ARCH_HAMMER32)
		prefetchnta	608(X)
   #elif defined(ATL_ARCH_P4)
                prefetchnta     464(X)
   #else
		prefetchnta	128(X)
		prefetchnta	160(X)
   #endif
        movaps  (X), rX0
        movaps  16(X), rX1
	movaps	32(X), rX2
	movaps	48(X), rX3
	andps	absval, rX0
	andps	absval, rX1
	andps	absval, rX2
	andps	absval, rX3
        cmpps   $6, maxval, rX0
        cmpps   $6, maxval, rX1
        cmpps   $6, maxval, rX2
        cmpps   $6, maxval, rX3
        movmskps        rX0, reg1
        movmskps        rX1, reg2
	shl	$4, reg1
	or	reg2, reg1
	movmskps	rX2, reg2
	shl	$4, reg1
	or	reg2, reg1
	movmskps	rX3, reg2
	shl	$4, reg1
	or	reg2, reg1

	cmp	$0, reg1
	jne	LOOP8_NEWMAX
LOOP8INC:
	addq    $64, X
	dec	N8
	jnz	LOOP8
#
#
#	Find which of 16 possible vals created maxval
#
FIND:
	movups	(maxX), rX0
	movups	16(maxX), rX1
	andps	absval, rX0
	andps	absval, rX1
	cmpps	$0, maxval, rX0
	cmpps	$0, maxval, rX1
	movmskps	rX0, reg1
	movmskps	rX1, reg2
	test	$15, reg1
	jnz	FIND_0
	test	$15, reg2
	jnz	FIND_4

	movups	32(maxX), rX0
	movups	48(maxX), rX1
	andps	absval, rX0
	andps	absval, rX1
	cmpps	$0, maxval, rX0
	cmpps	$0, maxval, rX1
	movmskps	rX0, reg1
	movmskps	rX1, reg2
	test	$15, reg1
	jnz	FIND_8
	addq	$48, maxX
	test	$1, reg2
	jnz	FIND_CU
	addq	$4, maxX
	test	$2, reg2
	jnz	FIND_CU
	addq	$4, maxX
	test	$4, reg2
	jnz	FIND_CU
	addq	$4, maxX
	jmp	FIND_CU

FIND_0:
	test	$1, reg1
	jnz	FIND_CU
	addq	$4, maxX
	test	$2, reg1
	jnz	FIND_CU
	addq	$4, maxX
	test	$4, reg1
	jnz	FIND_CU
	addq	$4, maxX
	jmp	FIND_CU
FIND_4:
	addq	$16, maxX
	test	$1, reg2
	jnz	FIND_CU
	addq	$4, maxX
	test	$2, reg2
	jnz	FIND_CU
	addq	$4, maxX
	test	$4, reg2
	jnz	FIND_CU
	addq	$4, maxX
	jmp	FIND_CU
FIND_8:
	addq	$32, maxX
	test	$1, reg1
	jnz	FIND_CU
	addq	$4, maxX
	test	$2, reg1
	jnz	FIND_CU
	addq	$4, maxX
	test	$4, reg1
	jnz	FIND_CU
	addq	$4, maxX
FIND_CU:
	cmp	$0, N
	jnz	LOOP1
DONE:
	movq	maxX, %rax
	subq	X0, %rax
	shr	$2, %rax
#ifdef ATL_GAS_x8632
	movl	(%esp), %edi
	movl	4(%esp), %ebp
	movl	8(%esp), %ebx
	movl	12(%esp), %esi
        addl    $16, %esp
#endif
	ret

LOOP8_NEWMAX:
	movq	X, maxX
	test	$0xFF00, reg1
	jz	L8NM_8
	movaps	(X), rX0
	movaps	16(X), rX1
	andps	absval, rX0
	andps	absval, rX1
	maxps	rX1, rX0
	movhlps	rX0, rX1
	maxps	rX1, rX0
	movaps	rX0, maxval
	shufps	$0x11,	maxval, maxval
	maxps	rX0, maxval
	movlhps	maxval, maxval
	test	$0x00FF, reg1
	jz	LOOP8INC
L8NM_8:
	movaps	32(X), rX0
	movaps	48(X), rX1
	andps	absval, rX0
	andps	absval, rX1
	maxps	rX1, rX0
	movhlps	rX0, rX1
	maxps	rX1, rX0
	movaps	rX0, rX1
	shufps	$0x11,	rX1, rX1
	maxps	rX0, rX1
	movlhps	rX1, rX1
	maxps	rX1, maxval
	jmp	LOOP8INC
#
#  Assumes X at start, and N # of iterations
#
LOOP1:
	movss	(X), rX0
	andps	absval, rX0
	comiss	rX0, maxval
	jb	NEWMAX1
LOOPINC1:
	addq	$4, X
        dec     N
	jnz	LOOP1
        shufps  $0x00, maxval, maxval
	jmp	DONE
NEWMAX1:
	movss	rX0, maxval
	movq	X, maxX
	jmp	LOOPINC1
FORCEALIGN:
	movss	(X), rX0
	andps	absval, rX0
	comiss	rX0, maxval
	jb FA_NEWMAX
FA_INC:
	dec	N
	jz	DONE
	addq	$4, X
	test	$15, X
	jnz	FORCEALIGN
#
        shufps  $0x00, maxval, maxval
        jmp     ALIGNED_STARTUP
FA_NEWMAX:
	movss	rX0, maxval
	movq	X, maxX
	jmp	FA_INC
#else

#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
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
   #define N	%eax
   #define X	%edx
   #define maxX	%ecx
   #define X0	%edi
   #define N4   %ebp
   #define reg1 %ebx
   #define reg2 %esi
#else
   #define N	%rax
   #define X	%rsi
   #define maxX	%rcx
   #define X0	%rdi
   #define N4   %rdx
   #define reg1	%r8
   #define reg2 %r9
#endif

#define maxval  %xmm0
#define absval	%xmm1
#define rX0     %xmm2
#define rX1     %xmm3
#define rX2     %xmm4
#define rX3     %xmm5

# IREG                   rdi            rsi
# int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
        .text
.global	ATL_asmdecor(ATL_UIAMAX)
ATL_asmdecor(ATL_UIAMAX):
#ifdef ATL_GAS_x8632
	subl	$16, %esp
   #define SOFF 0
#else
   #define SOFF -16
#endif
#
#	Temporarily store 1.0 and -1.0 to stack
#
	fld1
	fldz
	fsub	%st(1), %st
	fstpl	SOFF(%rsp)
	fstpl	SOFF+8(%rsp)
#
#       eax = all bits 1
#
	xorl	%eax, %eax
	notl	%eax
#
#	absval = (-1.0 ^ 1.0) = sign bit only
#
	movlpd	SOFF(%rsp), absval
	movlpd	SOFF+8(%rsp), rX0
	xorpd	rX0, absval
#
#       absval = NOT(sign bit) & (all ones) == all bits but sign bet set
#
	movl	%eax, SOFF(%rsp)
	movl	%eax, 4+SOFF(%rsp)
	movlpd	SOFF(%rsp), rX0
	andnpd	rX0, absval
	unpcklpd	absval, absval
#ifdef ATL_GAS_x8632
#
#       Save iregs
#
	movl	%edi, (%esp)
	movl	%ebp, 4(%esp)
        movl    %ebx, 8(%esp)
	movl	%esi, 12(%esp)
#
	movl	20(%esp), N
	movl	24(%esp), X
#
	movl	X, X0
	movl	X, maxX
	cmp	$1, N
	jbe	DONE
#else
#
#       X already in right register, X0 = X, maxX = X, init N
#
	movl	%edi, %eax
	movq	X, X0
	movq	X, maxX
	cmp	$1,%eax
	jbe	DONE
	cltq
#endif
        xorpd   maxval, maxval
#
#       Get X aligned to 16 byte boundary
#
	test	$15, X
	jnz	FORCEALIGN

ALIGNED_STARTUP:
        movq    N, N4
        shr     $3, N4
        jz      LOOP1
	shl	$3, N4
	subq	N4, N
	shr	$3, N4
LOOP4:
        movapd  (X), rX0
        movapd  16(X), rX1
#if defined(ATL_ARCH_HAMMER64) || defined(ATL_ARCH_HAMMER32)
					prefetchnta	608(X)
#else
					prefetchnta	464(X)
					prefetchnta	496(X)
#endif
	andpd	absval, rX0
        movapd  32(X), rX2
	andpd	absval, rX1
	cmppd	$6, maxval, rX0
        movapd  48(X), rX3
	andpd	absval, rX2
        cmppd   $6, maxval, rX1
	andpd	absval, rX3
        cmppd   $6, maxval, rX2
        movmskpd        rX0, reg1
        cmppd   $6, maxval, rX3
        movmskpd        rX1, reg2
	shl	$2, reg1
	or 	reg2, reg1
        movmskpd        rX2, reg2
	shl	$2, reg1
	or 	reg2, reg1
        movmskpd        rX3, reg2
	shl	$2, reg1
	or 	reg2, reg1

        cmp     $0, reg1
        jne     LOOP4_NEWMAX
LOOP4INC:
	addq    $64, X
	dec	N4
	jnz	LOOP4
#
#
#	Find which of 8 possible vals is maxval
#
	movupd	(maxX), rX0
	movupd	16(maxX), rX1
	andpd	absval, rX0
	andpd	absval, rX1
	cmppd	$0, maxval, rX0
	cmppd	$0, maxval, rX1
	movmskpd	rX0, reg1
	movmskpd	rX1, reg2
	test	$3, reg1
	jnz	DONE_0
	test	$3, reg2
	jnz	DONE_2
	movupd	32(maxX), rX0
	movupd	48(maxX), rX1
	andpd	absval, rX0
	andpd	absval, rX1
	cmppd	$0, maxval, rX0
	cmppd	$0, maxval, rX1
	movmskpd	rX0, reg1
	movmskpd	rX1, reg2
	test	$3, reg1
	jnz	DONE_4
	addq	$48, maxX
	test	$1, reg2
	jnz	DONE_CU
	addq	$8, maxX
	jmp	DONE_CU
DONE_0:
	test	$1, reg1
	jnz	DONE_CU
	add	$8, maxX
	jmp	DONE_CU
DONE_2:
	addq	$16, maxX
	test	$1, reg2
	jnz	DONE_CU
	add	$8, maxX
	jmp	DONE_CU
DONE_4:
	addq	$32, maxX
	test	$1, reg1
	jnz	DONE_CU
	add	$8, maxX
DONE_CU:
	cmp	$0, N
	jnz	LOOP1
DONE:
	finit
	movq	maxX, %rax
	subq	X0, %rax
	shr	$3, %rax
#ifdef ATL_GAS_x8632
	movl	(%esp), %edi
	movl	4(%esp), %ebp
	movl	8(%esp), %ebx
	movl	12(%esp), %esi
        addl    $16, %esp
#endif
	ret
LOOP4_NEWMAX:
	movq	X, maxX
	movapd	(X), rX0
	movapd	16(X), rX1
	andpd	absval, rX0
	movapd	32(X), rX2
	andpd	absval, rX1
	movapd	48(X), rX3
	andpd	absval, rX2
	maxpd	rX1, rX0
	andpd	absval, rX3
	maxpd	rX3, rX2
	maxpd	rX2, rX0
	movapd	rX0, maxval
	unpcklpd rX0, rX0
	unpckhpd maxval, maxval
	maxpd	rX0, maxval
	jmp	LOOP4INC
#
#  Assumes X at start, and N # of iterations,
#
LOOP1:
	movlpd	(X), rX0
	andpd	absval, rX0
	comisd	rX0, maxval
	jb	NEWMAX1
LOOPINC1:
	addq	$8, X
        dec     N
	jnz	LOOP1
	jmp	DONE
NEWMAX1:
	movlpd	(X), maxval
	unpcklpd	maxval, maxval
	andpd	absval, maxval
	movq	X, maxX
	jmp	LOOPINC1
FORCEALIGN:
	movlpd	(X), maxval
	dec	N
	unpcklpd	maxval, maxval
	andpd	absval, maxval
	addq	$8, X
	test	$15, X
	jnz	LOOP1
	jmp	ALIGNED_STARTUP
#endif
