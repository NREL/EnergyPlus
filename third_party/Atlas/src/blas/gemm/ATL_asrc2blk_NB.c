#include "atlas_asm.h"
#include "dmm.h"

#ifdef ATL_GAS_x8664
#ifndef NB
   #error "NB must be compile-time constant"
#elif (NB/8)*8 != NB
   #error "NB must be multiple of 8!!"
#endif

#define NBso (NB*8)

#ifdef ALPHAX
   #define R2B_NB ATL_drow2blkT_NB_aX
   #define C2BN   ATL_dcol2blk_NB_aX
#else
   #define R2B_NB ATL_drow2blkT_NB_a1
   #define C2BN   ATL_dcol2blk_NB_a1
#endif

#define alpha	%xmm0
#define rA0	%xmm1
#define rA1	%xmm2
#define rA2	%xmm3
#define rA3	%xmm4

#define pV	%rdi
#define pA0	%rdx
#define it2	%ebp
#define ii  	%rbp
#define jj	%rax
#define lda	%rbx
#define lda8	%rcx
#define ld_nb	%rsi
#define incA	%r8
#define nn	%r9
#define JJ	%r10
#define nblk	%r11
#define mr	%r12
#define pAn	%r13
#define pVn	%r14

#               %edi   %esi           %rdx     %rcx      %r8       %xmm0
# void col2blk(int M, int N, const TYPE *A, int lda, TYPE *V, SCALAR alpha)
#    NOTE: N is ignored, assumed to be NB
#
.global ATL_asmdecor(C2BN)
ATL_asmdecor(C2BN):
	movq	%rbp, -8(%rsp)
	movq	%rbx, -16(%rsp)
	movq	%r12, -24(%rsp)
	movq	%r13, -32(%rsp)
	movq	%r14, -40(%rsp)
#
#       Convert lda to 64 bit, then figure nblk (both ops require %rax)
#
	movl	%ecx, %eax
	cltq
	movq	%rax, lda
	movl	%edi, %eax
	cltq
	movq	%rdx, %r14   # div overwrites rdx and rax, save pA
	movq	$0, %rdx
	movq	$NB, %rcx
	idivq	%rcx
	movq	%rdx, mr
	movq	%rax, nblk
	movq	%r14, %rdx
#
#       Init pV, and alpha,
#       lda *= sizeof;  lda8 = 8 * lda;  ld_nb = lda - NB;
#
	movq	%r8, pV
	shl	$3, lda
	movq	lda, lda8
	shl	$3, lda8
	movq	lda, ld_nb
	subq	$NBso, ld_nb
	movq	lda8, incA
	subq	$NBso, incA
	neg	incA
   #ifdef ALPHAX
	unpcklpd	alpha, alpha
   #endif
	cmp	$0, nblk
	je 	CUC2B_chk
#
#	Set pAn = pA, pVn = pV
#
	movq	pA0, pAn
	movq	pV, pVn
#
#	for (JJ=NB/8; JJ; JJ--)
#
	movq	$NB/8, JJ
JJLOOP:
	addq	lda8, pAn
	addq	$8*NBso, pVn
#
#	for (nn=nblk; n; n--)
#
	movq	nblk, nn
NNLOOP:

#
#	Burst load 8 cols of A
#
	movq	$8, jj
CBURST:
	movl	NBso-64(pA0), it2
	movl	NBso-128(pA0), it2
   #if NB >= 24
	movl	NBso-192(pA0), it2
   #endif
   #if NB >= 32
	movl	NBso-256(pA0), it2
   #endif
   #if NB >= 40
	movl	NBso-320(pA0), it2
   #endif
   #if NB >= 48
	movl	NBso-384(pA0), it2
   #endif
   #if NB >= 56
	movl	NBso-448(pA0), it2
   #endif
   #if NB >= 64
	movl	NBso-512(pA0), it2
   #endif
   #if NB >= 72
	movl	NBso-576(pA0), it2
   #endif
   #if NB >= 80
	movl	NBso-640(pA0), it2
   #endif
	addq	lda, pA0
#
#       while (jj)
#
	subq	$1, jj
	jnz CBURST
	subq	lda8, pA0

#
#	for (jj=8; jj; jj--)
#
	movq	$8, jj
JILOOP:
#
#       for (ii=NB/8; ii; ii--)
#
	movq	$NB/8, ii
ILOOP:
   #ifdef ALPHAX
	movupd	(pA0), rA0
	mulpd	alpha, rA0
	movupd	16(pA0), rA1
	mulpd	alpha, rA1
	movupd	32(pA0), rA2
	mulpd	alpha, rA2
	movupd	48(pA0), rA3
	mulpd	alpha, rA3
   #else
	movupd	(pA0), rA0
	movupd	16(pA0), rA1
	movupd	32(pA0), rA2
	movupd	48(pA0), rA3
   #endif

	movntpd	rA0, (pV)
	movntpd	rA1, 16(pV)
	movntpd	rA2, 32(pV)
	movntpd	rA3, 48(pV)

	addq	$64, pA0
	addq	$64, pV
#
#	while (ii)
#
	subq	$1, ii
	jnz	ILOOP
#
#	pA0 += lda - NB
#
	addq	ld_nb, pA0
#
#	while(jj)
#
	subq	$1, jj
	jnz	JILOOP
#
#	pA -= (lda*8 - NB);  pV += NBNB - 8*NB;
#
	addq	incA, pA0
	addq	$(NB-8)*NBso, pV
#
#       while (nn)
#
	subq	$1, nn
	jnz	NNLOOP
#
	movq	pAn, pA0
	movq	pVn, pV
#
#       while (JJ)
#
	subq	$1, JJ
	jnz	JJLOOP
#
#	If we have a partial block, copy it as well
#
	cmp	$0, mr
	je	DONE_C2B
#
#	pV = pV - NBNB + nblk*NBNB
#
	subq	$NB*NBso, pV
	imulq	$NB*NBso, nblk, JJ
	addq	JJ, pV
#
#	pA0 -= NB*lda
#
	imulq	$NB, lda, JJ
	subq	JJ, pA0
	imulq	$NBso, nblk, JJ
	addq	JJ, pA0
CUC2B:
	movq	pA0, pAn
#
#       for (jj=NB; jj; jj--)
#
	movq	$NB, JJ
JJCU:
#
#       for (ii=mr; ii; ii--)
#
	movq	mr, ii
	addq	lda, pAn
IICU:
   #ifdef ALPHAX
	movlpd	(pA0), rA0
	mulsd	alpha, rA0
	movlpd	rA0, -48(%rsp)
	movq	-48(%rsp), pVn
	movnti	pVn, (pV)
   #else
	movq	(pA0), pVn
	movnti	pVn, (pV)
   #endif
	addq	$8, pV
	addq	$8, pA0
	subq	$1, ii
	jnz	IICU
	movq	pAn, pA0
#
#	while(JJ)
#
	subq	$1, JJ
	jnz	JJCU
#
#	Done col2blk
#
DONE_C2B:
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
	movq	-24(%rsp), %r12
	movq	-32(%rsp), %r13
	movq	-40(%rsp), %r14
	sfence
	ret
CUC2B_chk:
	cmp	$0, mr
	je	DONE_C2B
	jmp	CUC2B

#undef  pV
#undef  pA0
#undef  it2
#undef  ii
#undef  jj
#undef  lda
#undef  lda8
#undef  ld_nb
#undef  incA
#undef  nn
#undef  JJ
#undef  nblk
#undef  mr
#undef  pAn
#undef  pVn

#define N	%esi
#define pA0	%rdx
#define lda	%rax
#define pV	%rcx
#define itmp	%edi
#define it2	%ebp
#define ltmp	%rbp
#define pA1	%rbx
#define pA2	%r8
#define pA3	%r9
#define pA4	%r10
#define pA5	%r11
#define pA6	%r12
#define pA7	%r13

#                      %edi   %esi           %rdx     %rcx      %r8       %xmm0
#void ATL_row2blkT_NB(int M, int N, const TYPE *A, int lda, TYPE *V, TYPE alpha)
#
#  NOTE : can ignore M and N: they are NB, a compile-time constant (cpp macro)
.global ATL_asmdecor(R2B_NB)
ATL_asmdecor(R2B_NB):
	movq	%rbp, -8(%rsp)
	movq	%rbx, -16(%rsp)
	movq	%r12, -24(%rsp)
	movq	%r13, -32(%rsp)
#
#	lda = lda * sizeof;  Init pV, and get alpha in both slots of xmm0
#
	movl	%ecx, %eax
	cltq
	shl	$3, lda
	movq	%r8, pV
   #ifdef ALPHAX
	unpcklpd	alpha, alpha
   #endif
#
#	Set pA[1-7]
#
	movq	pA0, pA1
	addq	lda, pA1
	movq	pA1, pA2
	addq	lda, pA2
	movq	pA2, pA3
	addq	lda, pA3
	movq	pA3, pA4
	addq	lda, pA4
	movq	pA4, pA5
	addq	lda, pA5
	movq	pA5, pA6
	addq	lda, pA6
	movq	pA6, pA7
	addq	lda, pA7
NLOOP:
#if 1
#
#	Burst load 8 cols of A
#
	movl	$8, itmp
BURST:
	movl	NBso-64(pA0), it2
	movl	NBso-128(pA0), it2
   #if NB >= 24
	movl	NBso-192(pA0), it2
   #endif
   #if NB >= 32
	movl	NBso-256(pA0), it2
   #endif
   #if NB >= 40
	movl	NBso-320(pA0), it2
   #endif
   #if NB >= 48
	movl	NBso-384(pA0), it2
   #endif
   #if NB >= 56
	movl	NBso-448(pA0), it2
   #endif
   #if NB >= 64
	movl	NBso-512(pA0), it2
   #endif
   #if NB >= 72
	movl	NBso-576(pA0), it2
   #endif
   #if NB >= 80
	movl	NBso-640(pA0), it2
   #endif
	addq	lda, pA0
	subl	$1, itmp
	jnz BURST
	movq	pA1, pA0
	subq	lda, pA0
#endif
#
#       Scale the 8 preloaded cols, and write them as 8 rows of V
#
	movq	$(-NBso), ltmp
COPYLOOP:
	movlpd	NBso(pA0,ltmp), rA0
	movhpd	NBso(pA1,ltmp), rA0
   #ifdef ALPHAX
	mulpd	alpha, rA0
   #endif
	movlpd	NBso(pA2,ltmp), rA1
	movhpd	NBso(pA3,ltmp), rA1
   #ifdef ALPHAX
	mulpd	alpha, rA1
   #endif
	movlpd	NBso(pA4,ltmp), rA2
	movhpd	NBso(pA5,ltmp), rA2
   #ifdef ALPHAX
	mulpd	alpha, rA2
   #endif
	movlpd	NBso(pA6,ltmp), rA3
	movhpd	NBso(pA7,ltmp), rA3
   #ifdef ALPHAX
	mulpd	alpha, rA3
   #endif

	movntpd	rA0, (pV)
	movntpd	rA1, 16(pV)
	movntpd	rA2, 32(pV)
	movntpd	rA3, 48(pV)

	addq	$NBso, pV
	addq	$8, ltmp
	jnz	COPYLOOP

	subq	$NBso*NB-64, pV
	shl	$3, lda
	addq	lda, pA0
	addq	lda, pA1
	addq	lda, pA2
	addq	lda, pA3
	addq	lda, pA4
	addq	lda, pA5
	addq	lda, pA6
	addq	lda, pA7
	shr	$3, lda
	subl	$8, N
	jnz	NLOOP
#
#	All done here
#
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
	movq	-24(%rsp), %r12
	movq	-32(%rsp), %r13
	sfence
	ret
#endif
