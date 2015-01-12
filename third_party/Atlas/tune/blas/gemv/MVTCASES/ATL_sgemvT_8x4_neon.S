/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2011 Md. Majedul Haque Sujon
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
#ifndef ATL_GAS_ARM
   #error "This routine requires GAS/ARM assembly"
#endif
#ifndef ATL_NEON
   #error "This routine requires an ARM NEON SIMD unit!"
#endif
#ifndef ATL_NONIEEE
   #error "This NEON routine requires turning off IEEE compliance!"
#endif


/*  Written and Submitted by Md Majedul Haque Sujon  */

/* Unroll and Scalar expansion:
 *	I have tried following unroll factors (MxN): 4x2, 4x4, 8x2, 8x4, 8x6
 * 	I found better performance by using 8x4 (better than 8x6). In case of
 *	8x6, I need to reuse fp registers which reduce the scalar expansion as
 * 	well as increase the data dependency.
 *
 * Prefetch:
 * 	I have tried prefetch with offset 128, 64, 32, 16. Surprisingly, I
 *	found little variation in performance with the parameter.
 *
 *
 * Splitting clean up into 2 cases:
 *	I have splitted into two separate cases" TALL and FAT. I provided
 *	separate implementations for each case.
 *
 *      --------------------------------
 *      |                       |      |
 *      |                       |      |
 *      |                       |TALL  |
 *      |                       |CASE  |
 *      |                       |      |
 *      |-----------------------       |
 *      |       FAT CASE        |      |
 *      |                       |      |
 *      --------------------------------
 *
 *	For cleanup, I first called TALL case and then FAT case which I beleive
 *	provide better performance. When lda is not very larger than M,prefetch
 * 	helps TALL case.
 *
 *	a) TALL case:
 *		For remaining N<4, TALL case will execute. To provie better
 * 	performance, I want to use maximum the vector operations as much as
 * 	possible. So, 	I implement 3 blocks: N==3, N==2 and N==1. Each block
 *	uses M unrolled by 8 (remaining M%8 elements will fall into scalar loop)
 *
 *	b) FAT case:
 *		For remaining M < 8, the program will execute FAT cases. I use
 *	saxpy like implementation (loading X in outer loop and loading A and Yin
 * 	innerloop) for this case for better performance.  It provides decent
 * 	performance in comparison with the scalar implementation.
 * 	Yet, I want to maximumize the use of vector ops. So, I implemented 3
 *      blocks: for M==4, M==2, M==1 (where in M==4, I used unroll of 4 and
 *      M==2, I used unroll of 2). Any case with 1<=M<8 will execute like this:
 *      7=4+2+1, 6=4+2, 5=4+1 ... ...
 *
 * Alignment:
 * 	Here, A is the main bottle neck. So, aligning X or Y would not work. I
 * 	beleive, striding access of A ( by lda*4 element) and aligning each
 * 	stride of A would improve the performance. I have implemented code for
 * 	strided access but can't complete the cleanup part.
 *	-----------------------------------------
 *	|  |  |  |  |   |  |  |  |    |
 *	|A |  |  |  |A2 |  |  |  | A3 |
 *	|  |  |  |  |   |  |  |  |    |
 *	|  |A |  |  |   |A2|  |  |    |
 *	-----------------------------------------
 *
 *  Future work:
 * 	I will incorporate memory alignment optimization with this code.
 */


/* KNOWN ISSUE:
 * 	I found a strange problem with the vector operation. In my TALL case,
 * When M is very large and N=odd like, 1,2,3 there is a floating point
 * precision problem. ATLAS tester fails with diff=0.000001 ~ 0.000005.
 * I have checked my code for errors but failed to find any. If I am right,
 * there may be 2 sources : a) using VMLA instruction. b) using vector
 * register as scalar (like: d1[0]) to store and load.
 * I want to use VFMA(Vector Fused Multiply Accumulate) which would provide
 * better precision but the tested hardware doesn't support this. Reference for
 * VFMA:
 *	http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0489c/CIHEJBIE.html
 *
 * Please let me know if you find any problem in my code.
 * RCW: caused by non-IEEE complaint arithmetic of NEON.
 *
 *
 * -- Thanks,
 *    Md Majedul Haque Sujon.
 *
 */


#define M r0
#define N r1
#define A r2
#define lda r3

#define X r4
#define Y r5

#define A2 r6
#define A3 r7
#define A4 r8

#define lastX r9
#define lastY r10

/* mainly used for indexing, used as other purpose if needed*/
#define II r11
/* used mainly to check X addr or Y addr*/
#define CHKXY r12

/* mainly used for jump location, used for
 *other purpose if regs are not available
 */

#define JTARGCY r14

#define SP r13

#define FSIZE_X 100
#define FSIZE_Y 104


/*			r0	r1		r2		r3
void ATL_UGEMV(const int M, const int N, const float *A, const int lda, const
    1st overflow		2nd overflow arg
float *X		,float *Y)
*/

.code 32
.fpu neon
.text
.align 2
/* .arm */
.globl ATL_asmdecor(ATL_UGEMV)
ATL_asmdecor(ATL_UGEMV):
.type ATL_asmdecor(ATL_UGEMV), %function

/* save regs */

push {r4-r11,r14}
vpush {q4-q7}
/* stmDB SP!, {r4-r11,r14} */

#################################
/* just for a test. ommited  */
#################################
//fmrx lastX, FPSCR
//push {lastX} // need to add 4 to FSIZE_X, FSIZE_Y
//mvn II, #0xF
//and II, II, lastX    /* zero exception bits*/
//bic II, II, #(1<<24) /* turn off flush-to-zero*/
//fmxr FPSCR, II

##############################

/* load parameter */
ldr X,[SP,#FSIZE_X]
ldr Y,[SP, #FSIZE_Y]


/* calculate end of X and Y adddress*/
add lastX, X, M, LSL #2
add lastY, Y, N, LSL #2

PLD [X]
PLD [A]

/* calculate address of As*/
add A2, A, lda, LSL #2
add A3, A2,lda, LSL #2
add A4, A3, lda, LSL #2

PLD [A2]
PLD [A3]
PLD [A4]

/* set jump location for TALL case*/
ldr JTARGCY, =DONE

/* N<4?  goto TALL case directly */
cmp N, #4
BLT N_LESS_4

/* flag whether it is not only FAT case, need to track for BETA0 */
EOR JTARGCY, JTARGCY /* JTARGCY =0 */

/* M<8? goto FAT case directly*/
cmp M, #8
BLT M_LESS_8_N_GE_4

################################################
/* M >= 8 and N >= 4 */
/* M_GE8_NGE4: */
#################################################


/* M remaining (M/8)*8 */
mov II, M, LSR #3
LSL II, II, #3
/* CHKXY contains X address upto remainder*/
add CHKXY, X, II, LSL #2

/* here JTARGCY is used as end of rounded Y address*/
mov JTARGCY, N, LSR #2
LSL JTARGCY, JTARGCY, #2
add JTARGCY, Y, JTARGCY, LSL #2

/* calculate distance to point next A-s*/
LSL lda, lda, #2
sub lda, lda, II
LSL lda, lda, #2

/* no available int reg, but A is needed to be saved
 * otherwise need to use complex calc with multplication
 * so I saved it  in stack. ....
 */
push {A}


N4_LOOP:

/* to ommit the EOR in each N loop, I peel one iteration out, and multiply
 * and save result to these regs. But the effect is so negligible that it
 * doesn't improve the performance. For simplicity, I skipped those codes here.
 */

/* clear reg */
VEOR q10, q10, q10
VEOR q11, q11, q11
VEOR q12, q12, q12
VEOR q13, q13, q13

M8_LOOP:
	/* load x and all 4 A addr*/
	VLD1.32 {d0,d1,d2,d3}, [X]! 	/* q0, q1 */
	VLD1.32 {d4,d5,d6,d7},[A]! 	/* q2, q3 */
	VLD1.32 {d8,d9,d10,d11},[A2]!	/* q4, q5 */
	VLD1.32 {d12,d13,d14,d15},[A3]!	/* q6, q7 */
	VLD1.32 {d16,d17,d18,d19},[A4]!	/* q8, q9 */

	/* prefetch*/
	PLD [X, #64]
	PLD [A, #64]
	PLD [A2,#64]

	VMLA.F32 q10, q2, q0
	VMLA.F32 q11, q4, q0
	VMLA.F32 q12, q6, q0
	VMLA.F32 q13, q8, q0

	PLD [A3, #64]
	PLD [A4, #64]

	VMLA.F32 q10, q3, q1
	VMLA.F32 q11, q5, q1
	VMLA.F32 q12, q7, q1
	VMLA.F32 q13, q9, q1

	cmp X, CHKXY
BNE M8_LOOP

/*  horizontal pairwise add to add all the scalar expansion */
VPADD.F32 d28,d20,d21
VPADD.F32 d29,d22,d23
VPADD.F32 d30,d24,d25
VPADD.F32 d31,d26,d27

VPADD.F32 d20,d28,d29
VPADD.F32 d21,d30,d31

/* Store result*/

#ifdef BETA0
	VST1.32 {d20-d21},[Y]!
#else
	VLD1.32 {d22-d23},[Y]
	VADD.F32 q10, q10, q11
	VST1.32 {d20-d21},[Y]!
#endif

/* add (lda-M)*4 to A-s, here is the effective distanc here */

add A, A, lda
add A2,A2,lda
add A3,A3,lda
add A4,A4,lda

/* position X */
sub X, X, II, LSL#2

cmp Y, JTARGCY
BNE N4_LOOP


#############################################################
/* Now, there would be two remaining cases: TALL and FAT
 * I will call TALL case first
 *
 *
 *      --------------------------------
 *      |                       |      |
 *      |                       |      |
 *      |                       |TALL  |
 *      |                       |CASE  |
 *      |                       |      |
 *      |-----------------------       |
 *      |       FAT CASE        |      |
 *      |                       |      |
 *      --------------------------------
 *
 *
 */

/* what happens to the registers: */

/* Now, X == init X + {(M/8)*8}*4 , Y== init Y + ((N/4)*4)*4
 * lda is changed.. need to restore
 * M, N = unchanged
 * A = changed...to next position
 */

/* CALL the TALL  case: set parameter*/
/* restore lda:  lda = (lda + II*4 ) / 16  */
add lda, lda, II, LSL #2
LSR lda, lda, #4

/* X, Y is already set, M would be same, N need to set as remainder*/
subs N, lastY, JTARGCY
LSR N, N, #2

/* set A4 as the original A, as A4 is not used and we may need it in FAT case*/
pop {A4}

/* parameter for TALL call: */

/* Now, X == init X + {(M/8)*8}*4 , Y== init Y + ((N/4)*4)*4
 * lastX, lastY = unchanged
 * M, lda = original value
 * N = N % 4
 * A4 = original A
 */

ldr JTARGCY, =FAT_REM
BNE N_LESS_4    /* N!=0, then goto TALL CASE*/


/* CALL the FAT case: */

FAT_REM:

/* First check whether there is any FAT case, if no.. no need to arrange param*/

/* M is unchanged from previous operation in TALL*/
mov II, M, LSR #3
subs M, M, II, LSL #3	/* M = remainder of M... set flags to check later*/

BEQ DONE	/* M==0, no FAT case, goto done*/

/* arrange parameters */

/* Now, Y==lastY
 * X == lastX or init X + {(M/8)*8}*4 depending upon the prev call !!!
 * lastX, lastY = unchanged
 * M = original M % 8
 * lda is unchanged
 * N = remainder,
 * A = undef ... depend on condition... need to save before
 * but A4 can be used to save A, as it is not used in TALL case
 */

 mov A, A4

/* parameter for FAT case should be:
 * X = initial X + {(M/8)*8}*4
 * Y = initial Y
 * lda = original lda
 * A, A2, A3, A4 = follow X
 * lastX =original.. not changed
 * lastY = Y + ((N/4)*4)*4
 * M = original M % 8
 * N = (original N/4)* 4
 * Need to handle BETA0
 */

ldr X,[SP, #FSIZE_X]	/* load X again, as X is undefined */
ldr Y,[SP, #FSIZE_Y]	/* load Y again as Y and N is changed */


add X, X, II, LSL #5	/* X = X + II* 32 */
add A, A, II, LSL #5	/* A = A + II* 32 */

add A2, A, lda, LSL #2
add A3, A2,lda, LSL #2
add A4, A3, lda, LSL #2

sub II, lastY, Y        /* II = original N * 4  */
mov N, II, LSR #4	/* N = original N/4 */
LSL N, N, #2		/* N = (N/4)*4  */

add lastY, Y, N , LSL #2


/* flag for FAT cases to avoid to store for BETA0 */
mov JTARGCY, #1
B M_LESS_8_N_GE_4

###############################################################

DONE:

/*
//pop {lastX}
//fmxr FPSCR, lastX
*/


/* resotore regs */
vpop {q4-q7}
pop {r4-r11,r14}

/* ldmIA can be used instead of pop*/
/* ldmIA SP!, {r4-r11,r14} */

bx lr


###############################################################
/*  Special case M>=8(currently M is multiple of 8) N<4  */
/* TALL A */

/* Handled each case separately: n=3,2,1*/

###############################################################


N_LESS_4:

/* A4 is not used in this case, I reuse it between two calls*/

/* M<8 ? goto scalar block*/
cmp M,#8
BLT M_LESS8_N_LESS4

/* II =  (M/8)*8 */
mov II, M, LSR #3
LSL II, II, #3

/* CHKXY contains X address upto remainder*/
add CHKXY, X, II, LSL #2


/* N < 3? goto N<=2 test */
cmp N,#3
BLT N_LESS_3

######################################
/* N == 3 */
######################################

/* assuming lda, A, A2, A3 in correct position*/
/* lastX = last element*/

/* clear regs*/
VEOR q2, q2, q2
VEOR q3, q3, q3
VEOR q4, q4, q4
VEOR q5, q5, q5
VEOR q6, q6, q6
VEOR q7, q7, q7

M_N3_LOOP:
	/* load x and all 4 A addr*/
	VLD1.32 {d0,d1,d2,d3}, [X]!	/* q0, q1 */
	VLD1.32 {d16,d17,d18,d19},[A]!  /* q8, q9 */
	VLD1.32 {d20,d21,d22,d23},[A2]!	/* q10, q11 */
	VLD1.32 {d24,d25,d26,d27},[A3]!  /* q12, q13 */

	PLD [X, #32]
        PLD [A, #32]
        PLD [A2,#32]
        PLD [A3,#32]

	VMLA.F32 q2, q8,  q0
	VMLA.F32 q3, q9, q1
	VMLA.F32 q4, q10, q0
	VMLA.F32 q5, q11, q1
	VMLA.F32 q6, q12,  q0
	VMLA.F32 q7, q13, q1

     	cmp X, CHKXY
BNE M_N3_LOOP

/*add up regs */

VADD.F32 q14,q2,q3
VADD.F32 q15,q4,q5
VADD.F32 q0,q6,q7

/*  horizontal pairwise add to addup scalar expansion */

VPADD.F32 d20, d28, d29
VPADD.F32 d21, d30, d31
VPADD.F32 d23, d0, d1

VPADD.F32 d25, d20, d21
VPADD.F32 d26, d23, d24  /* d24 is garbase, not saved in Y*/

#ifdef BETA0
	VST1.32 {d25},[Y]!
	VST1.32 {d26[0]}, [Y]!
#else
	VLD1.32 {d28},[Y]!
	VLD1.32 {d29[0]}, [Y]!

	VADD.F32 d28, d28, d25
	VADD.F32 d29,d29,d26
	sub Y, Y, #12 			/* restore back the prev value*/

	VST1.32 {d28},    [Y]!
	VST1.32 {d29[0]}, [Y]!
#endif

cmp CHKXY,lastX
BxEQ JTARGCY

/* set Y to initial position*/
sub Y, Y, #12

/* set effective lda to move around A in loop*/
sub II, M, II /* number of remaining element*/

/* call clean up to complete */
B CLEANUP_M_LESS_8


N_LESS_3:

cmp N, #2
BLT N_EG_1

###################################################
/* N == 2 */

###################################################

/* assuming X, A, A2, in correct position*/

/* clear regs*/
VEOR q2, q2, q2
VEOR q3, q3, q3
VEOR q4, q4, q4
VEOR q5, q5, q5


M_N2_LOOP:
	/* load x and all 4 A addr*/
	VLD1.32 {d0,d1,d2,d3}, [X]!	/* q0, q1 */
	VLD1.32 {d18,d19,d20,d21},[A]!  /* q9, q10 */
	VLD1.32 {d22,d23,d24,d25},[A2]!	/* q11, q12 */

	PLD [X, #32]
	PLD [A, #32]
	PLD [A2,#32]

	VMLA.F32 q2, q9,  q0
	VMLA.F32 q3, q10, q1
	VMLA.F32 q4, q11, q0
	VMLA.F32 q5, q12, q1

	cmp X, CHKXY
BNE M_N2_LOOP

VADD.F32 q6, q2, q3
VADD.F32 q7, q4, q5

/*  horizontal pairwise add using vpadd */
/* horizontal add, rd = d18*/

VPADD.F32 d17, d14, d15
VPADD.F32 d16, d12, d13
VPADD.F32 d18, d16, d17


#ifdef BETA0
	VST1.32 {d18},[Y]!
#else
	VLD1.32 {d4},[Y]
	VADD.F32 d18, d18, d4
	VST1.32 {d18},[Y]!
#endif

cmp CHKXY,lastX
BxEQ JTARGCY

/* set Y to initial position*/
sub Y, Y, #8


/* number of remaining element*/
sub II, M, II

/* goto cleanup*/

B CLEANUP_M_LESS_8

N_EG_1:

######################################################
/* N == 1 */

######################################################

/* assuming X, A, in correct position*/


VEOR q2, q2, q2
VEOR q3, q3, q3

M_N1_LOOP:
	/* load x and all 4 A addr*/
	VLD1.32 {d0,d1,d2,d3}, [X]!	/* q0, q1 */
	VLD1.32 {d18,d19,d20,d21},[A]!  /* q9, q10 */

	PLD [X, #32]
	PLD [A, #32]

	VMLA.F32 q2, q9,  q0
	VMLA.F32 q3, q10, q1

	cmp X, CHKXY
BNE M_N1_LOOP

VADD.F32 q6, q2, q3
VADD.F32 d14,d12,d13

VPADD.F32 d16, d14, d13 /* d13 garbase but not used to save in Y*/


#ifdef BETA0
	VST1.32 {d16[0]},[Y]!
#else
	VLD1.32 {d17[0]},[Y]
	VADD.F32 d18, d16, d17

	VST1.32 {d18[0]},[Y]!
#endif

cmp CHKXY,lastX
BxEQ JTARGCY

/* set Y to initial position*/
sub Y, Y, #4

sub II, M, II 	/* number of remaining element*/

/* for BETA0, there may be 2 cases:
 * 	1. 1st case falls through from above cases: need to load Y even in BETA0
 *	2. 2nd case occurs for the 1st time: no load of Y;
 */

/* to reduce extra condition check and simplicity
 * I separated out this two cases
 */

CLEANUP_M_LESS_8:

/* II is the remainder for X */

/* use A2 to precalculate lda distance*/
sub A2, lda, II
LSL A2, A2, #2

CLEANUP_NL4_LOOP:
/* clear reg */
VEOR d2, d2, d2

CLEANUP_ML8_LOOP:

    VLD1.32 {d0[0]},[X]!
    VLD1.32 {d1[0]}, [A]!
    VMLA.F32 d2, d1, d0

    cmp X, lastX
BNE CLEANUP_ML8_LOOP

/* No need to use BETA0 macro as it's cleanup case  */

VLD1.32 {d3[0]}, [Y]

VADD.F32 d3, d3, d2
VST1.32 {d3[0]},[Y]!


add A, A, A2
sub X, X, II, LSL #2

cmp Y, lastY
BNE CLEANUP_NL4_LOOP

Bx JTARGCY


###############################################
/* M<8 and N<4 and M_N_scalar loop*/
/* scaler implementation , not a fall through case*/


M_LESS8_N_LESS4:

/* calculate effective lda once before loop */
sub lda,lda,M
LSL lda,lda,#2


NL4_LOOP:
/* clear reg */
VEOR d2, d2, d2
ML8_LOOP:
    VLD1.32 {d0[0]},[X]!
    VLD1.32 {d1[0]}, [A]!
    VMLA.F32 d2, d1, d0
    cmp X, lastX
BNE ML8_LOOP

#ifdef BETA0

VST1.32 {d2[0]},[Y]!

#else
VLD1.32 {d3[0]}, [Y]
VADD.F32 d3, d3, d2
VST1.32 {d3[0]},[Y]!

#endif

add A, A, lda
sub X, X, M, LSL #2

cmp Y, lastY
BNE NL4_LOOP

B DONE



###############################################################
/* FAT CASE ........... M < 8  N>=4*/
###############################################################


/* All of the below implementation is like SAXPY calculation
 * main idea:  Load all the X at once, now in inner loop load Y,
 * do computation and store back to Y....
 * BETA0 need to be handled carefully
 */

###############################################################


/* need to handle Y clean up for 4,2 case */
/* Special case M<8 and N>=4, saxpy like implementation*/

M_LESS_8_N_GE_4:

/* BETA0, then peel 1st iteration,
 * for 1st iteration, we need to store without load
 */

#ifdef BETA0

/* JTARGCY==1, it is not direct case, FAT is called for cleanup
 * so, skipped it
 */

cmp JTARGCY, #1
BEQ STR_SKIPPED

mov CHKXY, Y
mov II, A

VLD1.32 {d0[0]},[X]!

BETA0_M1_PEEL_LOOP:

    VLD1.32 {d1[0]}, [A]
    VMUL.F32 d2, d1, d0
    VST1.32 {d2[0]},[Y]!

    add A, A, lda, LSL #2
    cmp Y, lastY
    BNE BETA0_M1_PEEL_LOOP

/* restore Y */
mov Y, CHKXY

/* restore A,A2,A3,A4 to their updated position */
mov A, II
add A, A, #4
add A2, A2, #4
add A3, A3, #4
add A4, A4, #4

/* 1 M iteration is done*/
subs M, M, #1
BEQ DONE       /* M==0, got to DONE */

STR_SKIPPED:

#endif


/* N remaining II= (N/4)*4 */
mov II, N, LSR #2
LSL II, II, #2
/* CHKXY contains Y address upto remainder*/
add CHKXY, Y, II, LSL #2


/* M < 4 ? goto M2 case*/
cmp M, #4
BLT M2_N_GE4

/* M >= 4*/
############################################################

M4_N_GE4:

/* M==4, this implementation is better than M4 x N4 unroll... 2 times better*/

mov JTARGCY, A /* save A in JTARGCY*/


/* load all X at once*/
VLD1.32 {d0-d1}, [X]!


M4_N_Y_LOOP:
	VLD1.32 {d2-d3},[A]
	VLD1.32 {d4-d5},[A2]
	VLD1.32 {d6-d7},[A3]
	VLD1.32 {d8-d9},[A4]

	VLD1.32 {d10,d11},[Y]
	PLD [Y, #32]

	VMUL.F32 q1,q1,q0
	VMUL.F32 q2,q2,q0
	VMUL.F32 q3,q3,q0
	VMUL.F32 q4,q4,q0

	VADD.F32 d12,d2,d3
	VADD.F32 d13,d4,d5
	VADD.F32 d14,d6,d7
	VADD.F32 d15,d8,d9

	VPADD.F32 d16,d12,d13
	VPADD.F32 d17,d14,d15

	VADD.F32 q8,q8,q5

	VST1.32 {d16,d17},[Y]!


	add A, A, lda, LSL #4
	add A2,A2,lda, LSL #4
	add A3,A3,lda, LSL #4
	add A4,A4,lda, LSL #4

    cmp Y, CHKXY
BNE M4_N_Y_LOOP


/* no cleanup? goto M remaining check */
cmp Y,lastY
BEQ M4_M_REM_CHECK

/* cleanup for M4 case*/

/* Restore X to previous position*/
sub X,X, #16

VLD1.32 {d0-d1}, [X]!
M4_N1_Y_LOOP:
    VLD1.32 {d2-d3},[A]
    VMUL.F32 q1,q1,q0

    VPADD.F32 d4,d2,d3
    VPADD.F32 d6,d4,d5  /* d5 garbase, but not used for Y*/

    VLD1.32 {d8[0]}, [Y]
    VADD.F32 d6, d6, d8
    VST1.32 {d6[0]},[Y]!

    add A, A, lda, LSL #2
    cmp Y, lastY
BNE M4_N1_Y_LOOP



M4_M_REM_CHECK:

/* restore Y and position A, A2.... */

sub Y, CHKXY, II, LSL#2

mov A, JTARGCY /* restore A*/

add A, A, #16
add A2, A, lda, LSL #2

/* find remaining element for X */
subs II, M, #4
BEQ DONE       /* M==0, goto DONE*/


/* II<2? got to M==1 case*/
cmp II, #2
BLT M1_N_GE4




M2_N_GE4:

/* M<2? then goto SCALAR implementation*/
cmp M, #2
BLT M1_N_GE4

#################################################
/* case: for M = 7 (4+2+1), 6 (4+2), 2 and N>>4 */
/* CHKXY ----> has the addr multiple of 4 of Y */

/* A3, A4 is not used,
 * A, Y can be stored in A3, A4
 */

/* save A and Y*/
Mov A3, A
mov A4, Y

VLD1.32 {d0}, [X]!

/* X, A multiple of 2 floats*/
M2_N_Y_LOOP:

	VLD1.32 {d2},[A]
	VLD1.32 {d3},[A2]

	VLD1.32 {d10},[Y]

	PLD [Y, #32]
	VMUL.F32 d4,d2,d0
	VMUL.F32 d5,d3,d0

	VPADD.F32 d6,d4,d5
	VADD.F32 d6,d6,d10
	VST1.32 {d6},[Y]!

	add A, A, lda, LSL #3
	add A2,A2,lda, LSL #3

    cmp Y, CHKXY
BNE M2_N_Y_LOOP


/* cleanup for N
 */

/* check no cleanup for Y, goto M check*/
cmp Y,lastY
BEQ M2_M_REM_CHECK


/* Restore X to previous position */
sub X,X, #8

VLD1.32 {d0}, [X]!

M2_N1_Y_LOOP:
    VLD1.32 {d1},[A]
    VMUL.F32 d1,d1,d0

    VPADD.F32 d3,d1,d2  // d2 garbase

    VLD1.32 {d4[0]}, [Y]
    VADD.F32 d3, d3, d4
    VST1.32 {d3[0]},[Y]!

    add A, A, lda, LSL #2
    cmp Y, lastY
BNE M2_N1_Y_LOOP

/* remaining M check*/

M2_M_REM_CHECK:
/* restote A and Y */
/* mov A to appropriate position... A += 8*/
mov A, A3
add A, A, #8
mov Y, A4

/* check whether M is done, if not goto SCALAR loops*/
cmp lastX,X
BNE M1_N_GE4

B DONE

############################################################
/* Complete SCALAR implementation for saxpy like case*/
/* needed for odd case of M*/

M1_N_GE4:

VLD1.32 {d0[0]},[X]!

NY_LOOP_SCALAR:

    VLD1.32 {d1[0]}, [A]
    VMUL.F32 d2, d1, d0

    VLD1.32 {d3[0]}, [Y]
    VADD.F32 d3, d3, d2
    VST1.32 {d3[0]},[Y]!

    add A, A, lda, LSL #2
    cmp Y, lastY

BNE NY_LOOP_SCALAR

B DONE

