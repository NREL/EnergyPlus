
/*
 * 90x90 Single Precision Matrix Multiply using 3DNow! instructions
 *  (C) Copyright 2003 Tim Mattox & Hank Dietz
 *
 * The authors and University of Kentucky make this software freely
 * available as a PUBLIC DOMAIN release.  None of the authors nor
 * University of Kentucky can be held responsible for any problems
 * deriving from use of this software.
 *
 * The primary author is:
 *
 *         Tim Mattox
 *         Department of Electrical and Computer Engineering
 *         University of Kentucky
 *         Lexington, KY  40506-0046
 *         email: tmattox@engr.uky.edu
 *         URL: http://aggregate.org/
 *
 * Theory of operation:
 *
 * The Athlon processor has "interesting" instruction decode/issue
 * properties.  To maximize the throughput, each aligned 16 byte window of
 * instructions should contain exactly a multiple of 3 instructions.
 * Thus, there are alignment directives placed thoughout this code,
 * rep() macros and "interesting" addressing mode selections going on to
 * force this 3 instructions per 16 bytes property.  Yes, adding NOPs
 * and extranous do-nothing prefixes can speed up things dramatically!
 *
 * As for as the 3DNow! algorithm we tried a variety of instruction mixes,
 * unrollings, etc., and this appeared to give the best performance,
 * as shown from the results of "make ummcase pre=s nb=90 ...":
 * moves="-DMoveA -DMoveB" beta=1 ldc=180
 *  7.31 GFLOPS on an Athlon XP 2600+ (2075 MHz), which is 88% of peak.
 * moves="" beta=1 ldc=180
 *  7.97 GFLOPS on an Athlon XP 2600+ (2075 MHz), which is 96% of peak.
 *
 */

#define THREEDNOW
#include "SSE3Dnow.h"
#include "atlas_misc.h"
#include "atlas_prefetch.h"

#define alignN(n)               __asm__ __volatile__ (".align " n)

#define	ABIAS	(-20*KB)
#define	BBIAS0	(31)
#define	BBIAS1	(91)
#define	CBIAS	(1)

#define	PA0_INC	(6*NB)


void ATL_USERMM
(const int M, const int N, const int K, const float alpha, const float *A, const int lda, const float *B, const int ldb, const float beta, float *C, const int ldc)
{
   const float *stM;
   const float *stN;
   const int incCn = (ldc) - NB;
   float *pC0;
   const float *pA0;
   const float *pB0;
   const float *pB1;
#ifdef BETAX
static   vector locbeta;
#endif

#define First12As(i, ib, ic) \
	align(); \
	vec_mov_mr(&(pBX[  ib + 0 - BBIASX]), reg7); \
	rep(); vec_mov_mr(&(pA0[0*KB + 0 + i - ABIAS]), reg4); \
	vec_mul_rr(reg7, reg4); \
	\
	rep(); vec_mov_mr(&(pA0[1*KB + 0 + i - ABIAS]), reg5); \
	vec_mul_rr(reg7, reg5); \
	vec_acc_rr(reg5, reg4); \
	\
	rep(); vec_mov_mr(&(pA0[2*KB + 0 + i - ABIAS]), reg5); \
	vec_mul_rr(reg7, reg5); \
	vec_mov_mr(&(pBX[  ib + 2 - BBIASX]), reg3); \
	\
	rep(); vec_mov_mr(&(pA0[3*KB + 0 + i - ABIAS]), reg6); \
	vec_mul_rr(reg7, reg6); \
	vec_acc_rr(reg6, reg5); \
	\
	rep(); vec_mov_mr(&(pA0[4*KB + 0 + i - ABIAS]), reg6); \
	vec_mul_rr(reg7, reg6); \
	ATL_pfl1W(&(pC0[6*(ic) + 4 - CBIAS])); \
	\
	vec_mul_mr(&(pA0[5*KB + 0 + i - ABIAS]), reg7); \
	rep(); vec_mov_mr(&(pA0[0*KB + 2 + i - ABIAS]), reg0); \
	\
	vec_acc_rr(reg7, reg6); \
	vec_mul_rr(reg3, reg0); \
	rep(); vec_mov_mr(&(pA0[1*KB + 2 + i - ABIAS]), reg7); \
	\
	vec_mul_rr(reg3, reg7); \
	vec_acc_rr(reg7, reg0); \
	rep(); vec_mov_mr(&(pA0[2*KB + 2 + i - ABIAS]), reg1); \
	\
	vec_mul_rr(reg3, reg1); \
	rep(); vec_mov_mr(&(pA0[3*KB + 2 + i - ABIAS]), reg7); \
	vec_mul_rr(reg3, reg7); \
	\
	vec_acc_rr(reg7, reg1); \
	rep(); vec_mov_mr(&(pA0[4*KB + 2 + i - ABIAS]), reg2); \
	vec_mul_rr(reg3, reg2); \
	\
	vec_mul_mr(&(pA0[5*KB + 2 + i - ABIAS]), reg3); \
	vec_acc_rr(reg3, reg2); \
	vec_mov_mr(&(pBX[  ib + 4 - BBIASX]), reg7); \
	\
	;

#define Do6As(i, ib) \
	align(); \
	vec_add_rr(reg4, reg0); \
	rep(); vec_mov_mr(&(pA0[0*KB + i - ABIAS]), reg4); \
	vec_mul_rr(reg7, reg4); \
	\
	rep(); vec_mov_mr(&(pA0[1*KB + i - ABIAS]), reg3); \
	vec_mul_rr(reg7, reg3); \
	vec_acc_rr(reg3, reg4); \
	\
	vec_add_rr(reg5, reg1); \
	rep(); vec_mov_mr(&(pA0[2*KB + i - ABIAS]), reg5); \
	vec_mul_rr(reg7, reg5); \
	\
	rep(); vec_mov_mr(&(pA0[3*KB + i - ABIAS]), reg3); \
	vec_mul_rr(reg7, reg3); \
	vec_acc_rr(reg3, reg5); \
	\
	vec_add_rr(reg6, reg2); \
	rep(); vec_mov_mr(&(pA0[4*KB + i - ABIAS]), reg6); \
	vec_mul_rr(reg7, reg6); \
	\
	vec_mul_mr(&(pA0[5*KB     + i - ABIAS]), reg7); \
	vec_acc_rr(reg7, reg6); \
	vec_mov_mr(&(pBX[  ib + 2 - BBIASX]), reg7); \
	\
	;

#define Last6As(i, ib) \
	align(); \
	vec_add_rr(reg4, reg0); \
	rep(); vec_mov_mr(&(pA0[0*KB + i - ABIAS]), reg4); \
	vec_mul_rr(reg7, reg4); \
	\
	rep(); vec_mov_mr(&(pA0[1*KB + i - ABIAS]), reg3); \
	vec_mul_rr(reg7, reg3); \
	vec_acc_rr(reg3, reg4); \
	\
	vec_add_rr(reg5, reg1); \
	rep(); vec_mov_mr(&(pA0[2*KB + i - ABIAS]), reg5); \
	vec_mul_rr(reg7, reg5); \
	\
	rep(); vec_mov_mr(&(pA0[3*KB + i - ABIAS]), reg3); \
	vec_mul_rr(reg7, reg3); \
	vec_acc_rr(reg3, reg5); \
	\
	vec_add_rr(reg6, reg2); \
	rep(); vec_mov_mr(&(pA0[4*KB + i - ABIAS]), reg6); \
	vec_mul_rr(reg7, reg6); \
	\
	vec_mul_mr(&(pA0[5*KB     + i - ABIAS]), reg7); \
	vec_acc_rr(reg7, reg6); \
	vec_add_rr(reg4, reg0); \
	\
	;

#ifdef BETA0
#define StoreResults(ic) \
	align(); \
	vec_add_rr(reg5, reg1); \
	vec_add_rr(reg6, reg2); \
	vec_mov_rm(reg0, &(pC0[6*(ic) + 0 - CBIAS])); \
	vec_mov_rm(reg1, &(pC0[6*(ic) + 2 - CBIAS])); \
	\
	vec_mov_rm(reg2, &(pC0[6*(ic) + 4 - CBIAS])); \
	;
#elif defined(BETA1)
#define StoreResults(ic) \
	align(); \
	rep(); vec_add_mr(&(pC0[6*(ic) + 0 - CBIAS]), reg0); \
	rep(); vec_add_mr(&(pC0[6*(ic) + 2 - CBIAS]), reg1); \
	vec_add_rr(reg5, reg1); \
	\
	rep(); vec_add_mr(&(pC0[6*(ic) + 4 - CBIAS]), reg2); \
	rep(); vec_add_rr(reg6, reg2); \
	rep(); vec_mov_rm(reg0, &(pC0[6*(ic) + 0 - CBIAS])); \
	\
	vec_mov_rm(reg1, &(pC0[6*(ic) + 2 - CBIAS])); \
	vec_mov_rm(reg2, &(pC0[6*(ic) + 4 - CBIAS])); \
	;
#else
#define StoreResults(ic) \
	align(); \
	rep(); vec_mov_mr(locbeta, reg3); \
	vec_mov_mr(&(pC0[6*(ic) + 0 - CBIAS]), reg7); \
	vec_mov_mr(&(pC0[6*(ic) + 2 - CBIAS]), reg4); \
	\
	rep(); vec_mul_rr(reg3, reg7); \
	rep(); vec_mul_rr(reg3, reg4); \
	rep(); vec_mul_mr(&(pC0[6*(ic) + 4 - CBIAS]), reg3); \
	\
	vec_add_rr(reg7, reg0); \
	vec_add_rr(reg5, reg1); \
	vec_add_rr(reg6, reg2); \
	vec_add_rr(reg4, reg1); \
	\
	vec_add_rr(reg3, reg2); \
	vec_mov_rm(reg0, &(pC0[6*(ic) + 0 - CBIAS])); \
	vec_mov_rm(reg1, &(pC0[6*(ic) + 2 - CBIAS])); \
	vec_mov_rm(reg2, &(pC0[6*(ic) + 4 - CBIAS])); \
	\
	;
#endif

#define	FirstSteps(i, ib, stage) \
	First12As((((i) - 6) + ((stage - 0) * PA0_INC)), ((ib) - 6), stage) \
	Do6As((((i) - 2) + ((stage - 0) * PA0_INC)), ((ib) - 2)) \
	;

#define	Steps(i, ib, stage) \
	Do6As((((i) - 6) + ((stage - 0) * PA0_INC)), ((ib) - 6)) \
	Do6As((((i) - 4) + ((stage - 0) * PA0_INC)), ((ib) - 4)) \
	Do6As((((i) - 2) + ((stage - 0) * PA0_INC)), ((ib) - 2)) \
	;

#define	LastSteps(i, ib, stage) \
	Do6As((((i) - 6) + ((stage - 0) * PA0_INC)), ((ib) - 6)) \
	Do6As((((i) - 4) + ((stage - 0) * PA0_INC)), ((ib) - 4)) \
	Last6As((((i) - 2) + ((stage - 0) * PA0_INC)), ((ib) - 2)) \
	;

   vec_enter();

#ifdef BETAX
   vec_splat(&beta, reg3);
   vec_mov_rm(reg3, locbeta);
#endif

   { /* block "prefetch" the A submatrix */
      register const long long *pAd=(const long long *) A;
      register const long long *pAe=pAd + (506*8);
      align();
      do {
	rep(); vec_mov_mr(&(pAd[ 0]), reg0);
	vec_mov_mr(&(pAd[ 8]), reg1);
	rep(); pAd += 8*2;
      } while (pAd != pAe);
   }

   stM = A + NB*NB + ABIAS;
   stN = B + NB*NB + BBIAS0;
   pC0=C + CBIAS;
   pA0=A + ABIAS;
   pB0=B + BBIAS0;
   pB1=B + BBIAS1;

   /* block "prefetch" some of the B submatrix */
   align();
          vec_mov_mr(&(pB0[  0 - BBIAS0]), reg0);
          vec_mov_mr(&(pB0[ 16 - BBIAS0]), reg1);
   rep(); vec_mov_mr(&(pB1[ 32 - BBIAS1]), reg2);

   rep(); vec_mov_mr(&(pB1[ 48 - BBIAS1]), reg3);
          vec_mov_mr(&(pB1[ 64 - BBIAS1]), reg4);
          vec_mov_mr(&(pB1[ 80 - BBIAS1]), reg5);

          vec_mov_mr(&(pB1[ 96 - BBIAS1]), reg6);
          vec_mov_mr(&(pB1[112 - BBIAS1]), reg7);
   rep(); vec_mov_mr(&(pB0[128 - BBIAS0]), reg0);

   rep(); vec_mov_mr(&(pB1[144 - BBIAS1]), reg1);
   rep(); vec_mov_mr(&(pB1[160 - BBIAS1]), reg2);

   rep(); vec_mov_mr(&(pB1[176 - BBIAS1]), reg3);
   rep(); vec_mov_mr(&(pB1[192 - BBIAS1]), reg4);

   align();
   do { /* N-loop */
      rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 +  0]));
      rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 + 16]));

      align();
      do { /* M-loop */

#undef	pBX
#undef	BBIASX
#define	pBX	pB0
#define	BBIASX	BBIAS0

	FirstSteps(6, 6, 0);
	Steps(12, 12, 0);
	Steps(18, 18, 0);
	Steps(24, 24, 0);
	Steps(30, 30, 0);
	Steps(36, 36, 0);
	Steps(42, 42, 0);
	Steps(48, 48, 0);
	Steps(54, 54, 0);
	Steps(60, 60, 0);

#undef	pBX
#undef	BBIASX
#define	pBX	pB1
#define	BBIASX	BBIAS1

	Steps(66, 66, 0);
	Steps(72, 72, 0);
	Steps(78, 78, 0);
	Steps(84, 84, 0);
	LastSteps(90, 90, 0);
	StoreResults(0);

	alignN("8");
	rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 + 32]));

#undef	pBX
#undef	BBIASX
#define	pBX	pB0
#define	BBIASX	BBIAS0

	FirstSteps(6, 6, 1);
	Steps(12, 12, 1);
	Steps(18, 18, 1);
	Steps(24, 24, 1);
	Steps(30, 30, 1);
	Steps(36, 36, 1);
	Steps(42, 42, 1);
	Steps(48, 48, 1);
	Steps(54, 54, 1);
	Steps(60, 60, 1);

#undef	pBX
#undef	BBIASX
#define	pBX	pB1
#define	BBIASX	BBIAS1

	Steps(66, 66, 1);
	Steps(72, 72, 1);
	Steps(78, 78, 1);
	Steps(84, 84, 1);
	LastSteps(90, 90, 1);
	StoreResults(1);

	alignN("8");
	rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 + 48]));

#undef	pBX
#undef	BBIASX
#define	pBX	pB0
#define	BBIASX	BBIAS0

	FirstSteps(6, 6, 2);
	Steps(12, 12, 2);
	Steps(18, 18, 2);
	Steps(24, 24, 2);
	Steps(30, 30, 2);
	Steps(36, 36, 2);
	Steps(42, 42, 2);
	Steps(48, 48, 2);
	Steps(54, 54, 2);
	Steps(60, 60, 2);

#undef	pBX
#undef	BBIASX
#define	pBX	pB1
#define	BBIASX	BBIAS1

	Steps(66, 66, 2);
	Steps(72, 72, 2);
	Steps(78, 78, 2);
	Steps(84, 84, 2);
	LastSteps(90, 90, 2);

	rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 + 64]));
	rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 + 80]));
        pA0 += PA0_INC*3;
	rep(); ATL_pfl1R(&(pB0[2*KB - BBIAS0 + 88]));

	StoreResults(2);

        pC0 += 6*3;

#if (NB != 90)
 #error "NB must be 90"
#endif

      } while(pA0 != stM);

      pB0 += NB;
      pB1 += NB;
      pC0 += incCn;
      pA0 -= NB*NB;

   } while(pB0 != stN);

   vec_exit();
}
