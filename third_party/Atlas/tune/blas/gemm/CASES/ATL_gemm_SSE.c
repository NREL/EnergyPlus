
#include "camm_util.h"
#ifndef ATL_GAS_x8632
   #error "This kernel requires gas x86-32 assembler!"
#endif

/*
 *
 * Low level Strategies
 *
 */


#undef p1_1x4_1
#define p1_1x4_1(a_) \
      pla(a_,ax,1) \
      pla(a_,bx,0) \
      pm(0,1) \
      pa(1,4) \
      pla(SS(a_,NR),ax,2) \
      pm(0,2) \
      pa(2,5) \
      pla(SS(a_,SS(NR,NR)),ax,1) \
      pm(0,1) \
      pa(1,6) \
      pmm(SS(a_,SS(NR,SS(NR,NR))),ax,0) \
      pa(0,7)
#undef p1_2_1x4_1
#define p1_2_1x4_1(a_) \
      px(0) \
      pld(a_,bx,0)  \
      px(1) \
      pld(a_,ax,1) \
      pm(0,1) \
      pa(1,4) \
      px(2) \
      pld(SS(a_,NR),ax,2) \
      pm(0,2) \
      pa(2,5) \
      px(1) \
      pld(SS(a_,SS(NR,NR)),ax,1) \
      pm(0,1) \
      pa(1,6) \
      px(2) \
      pld(SS(a_,SS(NR,SS(NR,NR))),ax,2) \
      pm(0,2) \
      pa(2,7)
#undef p1_4_1x4_1
#define p1_4_1x4_1(a_) \
      pls(a_,bx,0)  \
      pls(a_,ax,1) \
      pmsr(0,1) \
      pasr(1,4) \
      pls(SS(a_,NR),ax,2) \
      pmsr(0,2) \
      pasr(2,5) \
      pls(SS(a_,SS(NR,NR)),ax,1) \
      pmsr(0,1) \
      pasr(1,6) \
      pls(SS(a_,SS(NR,SS(NR,NR))),ax,2) \
      pmsr(2,0) \
      pasr(0,2)
#undef lp1x4_1
#define lp1x4_1(a_)
#undef dp1x4_1
#define dp1x4_1(a_)
#undef pl1x4_1
#define pl1x4_1 0


#undef p1_1x2_1
#define p1_1x2_1(a_) \
      pla(a_,ax,1) \
      pla(a_,bx,0) \
      pm(0,1) \
      pa(1,4) \
      pla(SS(a_,NR),ax,2) \
      pm(0,2) \
      pa(2,5)
#undef p1_2_1x2_1
#define p1_2_1x2_1(a_) \
      px(0) \
      pld(a_,bx,0)  \
      px(1) \
      pld(a_,ax,1) \
      pm(0,1) \
      pa(1,4) \
      px(2) \
      pld(SS(a_,NR),ax,2) \
      pm(0,2) \
      pa(2,5)
#undef p1_4_1x2_1
#define p1_4_1x2_1(a_) \
      pls(a_,bx,0)  \
      pls(a_,ax,1) \
      pmsr(0,1) \
      pasr(1,4) \
      pls(SS(a_,NR),ax,2) \
      pmsr(0,2) \
      pasr(2,5)
#undef lp1x2_1
#define lp1x2_1(a_)
#undef dp1x2_1
#define dp1x2_1(a_)
#undef pl1x2_1
#define pl1x2_1 0


#undef p1_1x1_1
#define p1_1x1_1(a_) \
      pla(a_,ax,1) \
      pla(a_,bx,0) \
      pm(0,1) \
      pa(1,4)
#undef p1_2_1x1_1
#define p1_2_1x1_1(a_) \
      px(0) \
      pld(a_,bx,0)  \
      px(1) \
      pld(a_,ax,1) \
      pm(0,1) \
      pa(1,4)
#undef p1_4_1x1_1
#define p1_4_1x1_1(a_) \
      pls(a_,bx,0)  \
      pls(a_,ax,1) \
      pmsr(0,1) \
      pasr(1,4)
#undef lp1x1_1
#define lp1x1_1(a_)
#undef dp1x1_1
#define dp1x1_1(a_)
#undef pl1x1_1
#define pl1x1_1 0


#undef p1_1x4_2
#define p1_1x4_2(a_)  \
      pmm(SS(SS(a_,NR),NR),ax,2) \
      pa(0,4) \
      pla(SS(a_,RS4),bx,0) \
      pmm(SS(SS(SS(a_,NR),NR),NR),ax,3) \
      pa(1,5) \
      pa(2,6) \
      pc(0,2) \
      pmm(SS(a_,RS4),ax,0) \
      pa(3,7) \
      pla(SS(a_,RS4),bx,1) \
      pc(1,3) \
      pmm(SS(SS(a_,RS4),NR),ax,1)
#undef lp1x4_2
#define lp1x4_2(a_) \
      pla(a_,bx,0) \
      pla(a_,bx,1) \
      pc(0,2) \
      pc(1,3) \
      pmm(a_,ax,0) \
      pmm(SS(a_,NR),ax,1)
#undef dp1x4_2
#define dp1x4_2(a_) \
      pmm(SS(SS(a_,NR),NR),ax,2) \
      pa(0,4) \
      pmm(SS(SS(SS(a_,NR),NR),NR),ax,3) \
      pa(1,5) \
      pa(2,6) \
      pa(3,7)
#undef pl_1x4_2
#define pl_1x4_2 RS


#undef p1_1x2_2
#define p1_1x2_2(a_)  \
      pa(0,4) \
      pla(SS(a_,RS4),bx,0) \
      pa(1,5) \
      pc(0,2) \
      pmm(SS(a_,RS4),ax,0) \
      pla(SS(a_,RS4),bx,1) \
      pc(1,3) \
      pmm(SS(SS(a_,RS4),NR),ax,1)
#undef lp1x4_2
#define lp1x4_2(a_) \
      pla(a_,bx,0) \
      pla(a_,bx,1) \
      pc(0,2) \
      pc(1,3) \
      pmm(a_,ax,0) \
      pmm(SS(a_,NR),ax,1)
#undef dp1x4_2
#define dp1x4_2(a_) \
      pmm(SS(SS(a_,NR),NR),ax,2) \
      pa(0,4) \
      pmm(SS(SS(SS(a_,NR),NR),NR),ax,3) \
      pa(1,5) \
      pa(2,6) \
      pa(3,7)
#undef pl_1x4_2
#define pl_1x4_2 RS


#undef p1_1x4_2
#define p1_1x4_2(a_)  \
      pmm(SS(SS(a_,NR),NR),ax,2) \
      pa(0,4) \
      pla(SS(a_,RS4),bx,0) \
      pmm(SS(SS(SS(a_,NR),NR),NR),ax,3) \
      pa(1,5) \
      pa(2,6) \
      pc(0,2) \
      pmm(SS(a_,RS4),ax,0) \
      pa(3,7) \
      pla(SS(a_,RS4),bx,1) \
      pc(1,3) \
      pmm(SS(SS(a_,RS4),NR),ax,1)
#undef lp1x4_2
#define lp1x4_2(a_) \
      pla(a_,bx,0) \
      pla(a_,bx,1) \
      pc(0,2) \
      pc(1,3) \
      pmm(a_,ax,0) \
      pmm(SS(a_,NR),ax,1)
#undef dp1x4_2
#define dp1x4_2(a_) \
      pmm(SS(SS(a_,NR),NR),ax,2) \
      pa(0,4) \
      pmm(SS(SS(SS(a_,NR),NR),NR),ax,3) \
      pa(1,5) \
      pa(2,6) \
      pa(3,7)
#undef pl_1x4_2
#define pl_1x4_2 RS


#undef p1_1x4_3
#define p1_1x4_3(a_) \
      pla(SS(SS(a_,NR),NR),ax,2) \
      pm(0,1) \
      pa(1,5) \
      pla(SS(SS(SS(a_,NR),NR),NR),ax,1) \
      pm(0,2) \
      pa(2,6) \
      pla(SS(a_,RS4),ax,2) \
      pm(0,1) \
      pa(1,7) \
      pla(SS(a_,RS4),bx,0) \
      pla(SS(SS(a_,NR),RS4),ax,1) \
      pm(0,2) \
      pa(2,4)
#undef lp1x4_3
#define lp1x4_3(a_) \
      pla(a_,bx,0) \
      pla(a_,ax,2) \
      pla(SS(a_,NR),ax,1) \
      pm(0,2) \
      pa(2,4)
#undef dp1x4_3
#define dp1x4_3(a_) \
      pla(SS(SS(a_,NR),NR),ax,2) \
      pm(0,1) \
      pa(1,5) \
      pla(SS(SS(SS(a_,NR),NR),NR),ax,1) \
      pm(0,2) \
      pa(2,6) \
      pm(0,1) \
      pa(1,7)
#undef pl1x4_3
#define pl1x4_3 RS

#undef p1_1x2_3
#define p1_1x2_3(a_) \
      pm(0,1) \
      pa(1,5) \
      pla(SS(a_,RS4),ax,2) \
      pla(SS(a_,RS4),bx,0) \
      pla(SS(SS(a_,NR),RS4),ax,1) \
      pm(0,2) \
      pa(2,4)
#undef lp1x2_3
#define lp1x2_3(a_) \
      pla(a_,bx,0) \
      pla(a_,ax,2) \
      pla(SS(a_,NR),ax,1) \
      pm(0,2) \
      pa(2,4)
#undef dp1x2_3
#define dp1x2_3(a_) \
      pm(0,1) \
      pa(1,5)
#undef pl1x2_3
#define pl1x2_3 RS


#undef p1_1x1_3
#define p1_1x1_3(a_) \
      pla(SS(a_,RS4),bx,0) \
      pla(SS(SS(a_,NR),RS4),ax,1) \
      pm(0,2) \
      pa(2,4)
#undef lp1x1_3
#define lp1x1_3(a_) \
      pla(a_,bx,0) \
      pla(a_,ax,2) \
      pm(0,2) \
      pa(2,4)
#undef dp1x1_3
#define dp1x1_3(a_)
#undef pl1x1_3
#define pl1x1_3 RS


/*
 *
 *  End of low level strategies
 *
 */


#define VERS 1

#if defined(DREAL) || defined(DCPLX)
#define NR KB8
#else
#define NR KB4
#endif

#define pf(a_,b_)  /*  f(nta,a_,b_) */



#if defined(DREAL) || defined(DCPLX)
#define Z1(a_,b_) pc(a_,b_) ps(1,b_,b_) pasr(b_,a_)
#else
#define Z1(a_,b_) phl(a_,b_) pa(b_,a_) pc(a_,b_) ps(1,b_,b_) pasr(b_,a_)
#endif
#if defined(DREAL) || defined (SREAL)
#ifdef DREAL
#define Z1x4    f(t0,0,cx) pc(4,0) pul(5,4) pc(6,1) puh(5,0) pul(7,6)  \
                puh(7,1) pa(0,4) pa(1,6) pu(4,0,cx) pu(6,SS(CS,CS),cx)
#define Z1x2    f(t0,0,cx) pc(4,0) pul(5,4) puh(5,0)  \
                pa(0,4) pu(4,0,cx)
#else
#define Z1x4    f(t0,0,cx) pc(4,0) pul(5,4) pc(6,1) puh(5,0) pul(7,6)  \
                pa(0,4) puh(7,1) pc(4,2) pa(1,6) ps(68,6,4) ps(238,6,2) pa(4,2) pu(2,0,cx)
#define Z1x2    f(t0,0,cx) pc(4,0) pul(5,4) puh(5,0)  \
                pa(0,4) phl(4,2) pa(2,4) pud(4,0,cx)
#endif
#else
#define Z1x4    Z1(4,0) pus(4,0,cx) Z1(5,1) pus(5,CS,cx) \
                Z1(6,0) pus(6,SS(CS,CS),cx) Z1(7,1) pus(7,SS(SS(CS,CS),CS),cx)
#define Z1x2    Z1(4,0) pus(4,0,cx) Z1(5,1) pus(5,CS,cx)
#endif
#define Z1x1    Z1(4,0) pus(4,0,cx)

#ifdef BETA0
#define W1x4    px(4) px(5) px(6) px(7)
#define W1x2    px(4) px(5)
#define W1x1    px(4)
#endif
#ifdef BETA1
#define W1x4    pls(0,cx,4) pls(CS,cx,5) pls(SS(CS,CS),cx,6) \
                pls(SS(SS(CS,CS),CS),cx,7)
#define W1x2    pls(0,cx,4) pls(CS,cx,5)
#define W1x1    pls(0,cx,4)
#endif
#ifdef BETAX
#define W1x4    pls(0,cx,4) pls(CS,cx,5) pls(SS(CS,CS),cx,6) \
                pls(SS(SS(CS,CS),CS),cx,7) \
                pmsr(3,4) pmsr(3,5) pmsr(3,6) pmsr(3,7)
#define W1x2    pls(0,cx,4) pls(CS,cx,5) pmsr(3,4) pmsr(3,5)
#define W1x1    pls(0,cx,4) pmsr(3,4)
#endif

#if defined(DREAL) || defined(SREAL)
#ifdef DREAL
#define CS 8
#else
#define CS 4
#endif
#define LDCM 1
#else
#ifdef DCPLX
#define CS 16
#else
#define CS 8
#endif
#define LDCM 2
#endif



#if defined(SREAL) || defined(SCPLX)
#define MTYPE float
#else
#define MTYPE double
#endif


#undef MY_FUNCTION
#define MY_FUNCTION ATL_USERMM


void
MY_FUNCTION (int m, int n, int k, MTYPE alpha, const MTYPE *a,
	    int lda,const MTYPE *b, int ldb, MTYPE beta, MTYPE *c,
	    int ldc) {

  const MTYPE *bbp=&beta;
  const MTYPE *t1=a+m*KB,*t2=b+n*KB,*t3=a+((m>>2)<<2)*KB;

#undef N
#define N main

  ASM (

#if KB % 4
#error KB must be divisible by four -- m n cleanup needs alignment
#endif

#if !defined(SINGLE)
#undef KB
#define KB KB2
#endif

#ifdef BETAX
       pls(0,di,3)
#endif

       "pushl %%ebx\n\t"
       "movl  %%esi,%%ebx\n\t"

#if MB == 0 || NB == 0
       a(4,sp)

#if MB == 0
       "movl %4,%%esi\n\t"
#endif
#if NB == 0
       "movl %5,%%edi\n\t"
#endif
       a(-4,sp)
#endif

       "pushl %%ebp\n\t"
#if NB == 0
       "movl %%edi,%%ebp\n\t"
#else
       mm(MM(NR,NB),bp)
       ra(bx,bp)
#endif

#if MB == 0
       a(8,sp)
       "movl %6,%%edi\n\t"
       a(-8,sp)
#else
       mm(MM(NR,E4(MB)),di)
       ra(ax,di)
#endif

       lab(loopb)

#if NB == 0
       cmp(bx,bp)
       je(end)
#endif

       "pushl %%edi\n\t"
       "pushl %%eax\n\t"

       lab(loopa)

#if MB == 0
       cmp(ax,di)
       je(2)
#endif

#if MB == 0 ||  MB >= 4
#undef N
#define N Mjoin(1x4_,VERS)
#include "camm_pipe3.h"

       W1x4
       KB_block
       Z1x4

       a(SS(SS(NR,NR),SS(NR,NR)),ax)
       a(SS(SS(CS,CS),SS(CS,CS)),cx)

#undef N
#define N main

#endif

#if MB == 0
       jmp(loopa)
#else
       cmp(ax,di)
       jne(loopa)
#endif

#if MB == 0
       lab(2)
       a(SS(NR,NR),di)
       cmp(di,si)
       jl(1)
#endif

#if MB == 0 || ( MB / 2 ) % 2
#undef N
#define N Mjoin(1x2_,VERS)
#include "camm_pipe3.h"

       W1x2
       KB_block
       Z1x2

       a(SS(NR,NR),ax)
       a(SS(CS,CS),cx)

#undef N
#define N main

#endif

#if MB == 0
       lab(1)
       cmp(ax,si)
       je(stop)
#endif

#if MB == 0 || MB % 2

#undef N
#define N Mjoin(1x1_,VERS)
#include "camm_pipe3.h"

       W1x1
       KB_block
       Z1x1

/*         a(NR,ax) */
       a(CS,cx)

#undef N
#define N main

#endif

#if MB == 0
       lab(stop)
#endif

       "popl %%eax\n\t"
       "popl %%edi\n\t"
       ra(dx,cx)
       a(NR,bx)

#if NB == 0
       jmp(loopb)
       lab(end)
#else
       cmp(bx,bp)
       jne(loopb)
#endif

       "popl %%ebp\n\t"
       "popl %%ebx\n\t"


       ::"a" (a),"S" (b),"c" (c),"d" ((ldc-m)*LDCM*sizeof(*c)),
       "m" (t1),"m" (t2),"m" (t3)
#ifdef BETAX
       ,"D" (bbp):"memory");
#else
       :"di","memory");
#endif

}
