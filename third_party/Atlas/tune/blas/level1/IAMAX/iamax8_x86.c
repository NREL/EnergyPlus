
#include "atlas_asm.h"

#ifdef SREAL

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
#else
   #define N	%rax
   #define X	%rsi
   #define maxX	%rcx
   #define X0	%rdi
   #define N8   %rdx
   #define reg1	%r8
#endif

#define maxval  %xmm0
#define rX0     %xmm1
#define rX1     %xmm2
#define absval	%xmm3

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
        movq    X, N8
        shr     $4, N8
        shl     $4, N8
        cmp     X, N8
        jne     FORCEALIGN

ALIGNED_STARTUP:
        movq    N, N8
        shr     $3, N8
        jz      CLEANUP
	shl	$3, N8
	subq	N8, N
	shr	$3, N8
LOOP8:
        movaps  (X), rX0
        movaps  16(X), rX1
	andps	absval, rX0
	andps	absval, rX1
        maxps   maxval, rX0
        maxps   maxval, rX1
        cmpps   $4, maxval, rX0
   #if defined(ATL_ARCH_HAMMER64) || defined(ATL_ARCH_HAMMER32)
		prefetchnta	320(X)
   #elif defined(ATL_ARCH_P4)
                prefetchnta     464(X)
   #else
		prefetchnta	192(X)
   #endif
        cmpps   $4, maxval, rX1
        movmskps        rX0, reg1
        cmp     $0, reg1
        jne     LOOP8_1
        movmskps        rX1, reg1
        cmp     $0, reg1
        jne     LOOP8_2
LOOP8INC:
	addq    $32, X
	dec	N8
	jnz	LOOP8
#
	cmp	$0, N
	jnz	CLEANUP
DONE:
	finit
	movq	maxX, %rax
	subq	X0, %rax
	shr	$2, %rax
#ifdef ATL_GAS_x8632
	movl	(%esp), %edi
	movl	4(%esp), %ebp
	movl	8(%esp), %ebx
        addl    $16, %esp
#endif
	ret

LOOP8_1:
	flds	(X)
	fabs
	movq	$-12, reg1
	movq	$-16, maxX
LOOP8NML:
	flds	16(X,reg1)
	fabs
	fcomi	%st(1), %st
	jbe	LOOP8NMLINC
	mov	reg1, maxX
	fxch
LOOP8NMLINC:
	fstp	%st
	addq	$4, reg1
	jnz	LOOP8NML
#
	fstp	%st
	addq	$16, maxX
	addq	X, maxX
	movss	(maxX), maxval
	shufps	$0x00, maxval, maxval
	andps	absval, maxval
        movmskps        rX1, reg1
        cmp     $0, reg1
        jz      LOOP8INC
	movaps	16(X), rX1
	andps	absval, rX1
        maxps   maxval, rX1
        cmpps   $4, maxval, rX1
        movmskps        rX1, reg1
        cmp     $0, reg1
	je	LOOP8INC
	jmp	LOOP8_2
LOOP8_2:
	flds	16(X)
	fabs
	movq	$-12, reg1
	movq	$-16, maxX
LOOP8NML2:
	flds	32(X,reg1)
	fabs
	fcomi	%st(1), %st
	jbe	LOOP8NML2INC
	mov	reg1, maxX
	fxch
LOOP8NML2INC:
	fstp	%st
	addq	$4, reg1
	jnz	LOOP8NML2
#
	fstp	%st
	addq	$32, maxX
	addq	X, maxX
	movss	(maxX), maxval
	shufps	$0x00, maxval, maxval
	andps	absval, maxval
        jmp     LOOP8INC
CLEANUP:
	flds	(maxX)
	fabs

#
#  Assumes X at start, and N # of iterations, %st(0) has max so far
#
LOOP1:
	flds	(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX1
LOOPINC1:
	addq	$4, X
        dec     N
	jnz	LOOP1
	jmp	DONE
NEWMAX1:
	fstp	%st(0)
	flds	(X)
	fabs
	movq	X, maxX
	jmp	LOOPINC1
FORCEALIGN:
	flds	(X)
	fabs

LOOPALIGN:
	flds	(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAXA
ALIGNINC:
	addq	$4, X
        movq    X, N8
        shr     $4, N8
        shl     $4, N8
        cmp     X, N8
        jz      ALIGNED
        dec     N
	jnz	LOOPALIGN
	jmp	DONE

NEWMAXA:
	fstp	%st(0)
	flds	(X)
	fabs
	movq	X, maxX
	jmp	ALIGNINC
ALIGNED:
        dec     N
	jz	DONE
        movss   (maxX), maxval
        shufps  $0x00, maxval, maxval
	andps	absval, maxval
        jmp     ALIGNED_STARTUP

#else

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
   #define X	%esi
   #define stX	%edx
   #define stX4	%ebx
   #define maxX	%ecx
   #define X0	%edi
#else
   #define N	%rax
   #define X	%rsi
   #define stX	%rdx
   #define stX4	%r8
   #define maxX	%rcx
   #define X0	%rdi
#endif

# int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
        .text
.global	ATL_asmdecor(ATL_UIAMAX)
ATL_asmdecor(ATL_UIAMAX):
#ifdef ATL_GAS_x8632
	subl	$12, %esp
	movl	%ebx, (%esp)
	movl	%esi, 4(%esp)
	movl	%edi, 8(%esp)
#
	movl	16(%esp), N
	movl	20(%esp), X
#
	movl	X, X0
	movl	X, maxX
	cmp	$1, N
	jbe	DONE
#else
#
#       X already in right register, init N, stX = X + N
#
	movl	%edi, %eax
	movq	X, X0
	movq	X, maxX
	cmp	$1,%eax
	jbe	DONE
	cltq
#endif
	movq	N, stX
	shl	$3, stX
	addq	X, stX
	cmp	X, stX

	fldl	(X)
	fabs
	addq	$8, X
	movq	N, stX4
	subq	$1, stX4
	shr	$3, stX4
	shl	$6, stX4
	addq	X, stX4
	cmp	stX4, X
	je	LOOP1
	ALIGN16
LOOP:
	fldl	(X)
	fabs
	fcomip	%st(1), %st
   #if defined(ATL_ARCH_P4)
	        prefetchnta	768(X)
   #else
	        prefetchnta	572(X)
   #endif
	ja	NEWMAX_1
LOOP_2:
	fldl	8(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_2
LOOP_3:
	fldl	16(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_3
LOOP_4:
	fldl	24(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_4
LOOP_5:
	fldl	32(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_5
LOOP_6:
	fldl	40(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_6
LOOP_7:
	fldl	48(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_7
LOOP_8:
	fldl	56(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX_8
LOOP_9:
	addq	$64, X
	cmp	stX4, X
	jne	LOOP

	cmp	stX4, stX
	jne	LOOP1

DONE:
	finit
	movq	maxX, %rax
	subq	X0, %rax
	shr	$3, %rax
#ifdef ATL_GAS_x8632
        movl    (%esp), %ebx
        movl    4(%esp), %esi
        movl    8(%esp), %edi
        addl    $12, %esp
#endif
	ret
NEWMAX_1:
	fstp	%st(0)
	fldl	(X)
	fabs
	movq	X, maxX
	jmp	LOOP_2
NEWMAX_2:
	fstp	%st(0)
	fldl	8(X)
	fabs
	movq	X, maxX
	addq	$8, maxX
	jmp	LOOP_3
NEWMAX_3:
	fstp	%st(0)
	fldl	16(X)
	fabs
	movq	X, maxX
	addq	$16, maxX
	jmp	LOOP_4
NEWMAX_4:
	fstp	%st(0)
	fldl	24(X)
	fabs
	movq	X, maxX
	addq	$24, maxX
	jmp	LOOP_5
NEWMAX_5:
	fstp	%st(0)
	fldl	32(X)
	fabs
	movq	X, maxX
	addq	$32, maxX
	jmp	LOOP_6
NEWMAX_6:
	fstp	%st(0)
	fldl	40(X)
	fabs
	movq	X, maxX
	addq	$40, maxX
	jmp	LOOP_7
NEWMAX_7:
	fstp	%st(0)
	fldl	48(X)
	fabs
	movq	X, maxX
	addq	$48, maxX
	jmp	LOOP_8
NEWMAX_8:
	fstp	%st(0)
	fldl	56(X)
	fabs
	movq	X, maxX
	addq	$56, maxX
	jmp	LOOP_9
#
#  Assumes X at start, and stX where to quit, %st(0) has max so far
#
LOOP1:
	fldl	(X)
	fabs
	fcomip	%st(1), %st
	ja	NEWMAX1
LOOPINC1:
	addq	$8, X
	cmp	stX, X
	jne	LOOP1
	jmp	DONE
NEWMAX1:
	fstp	%st(0)
	fldl	(X)
	fabs
	movq	X, maxX
	jmp	LOOPINC1

#endif
