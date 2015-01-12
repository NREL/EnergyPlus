#ifndef CAMM_SCALE_H
#define CAMM_SCALE_H    /*+ To stop multiple inclusions. +*/

#include "camm_util.h"

#undef spf
#define spf(a_,b_)  f(t0,a_,b_)

#ifdef SCPLX
#ifdef BETAX
#undef SSREG
#define SSREG      2
#undef lbx
#define lbx        pls(4,ax,1) ps(0,1,1) pm(SSREG,1)
#undef cxx
#define cxx        pm(1,3) ps(177,3,3) pa(3,2)
#undef pcx
#define pcx        pc(2,3)
#else
#undef lbx
#define lbx
#undef cxx
#define cxx
#undef pcx
#define pcx
#endif
#undef lb
#define lb         pls(0,ax,0) ps(0,0,0) lbx 
#undef c
#define c(a_)      pl(a_ ## 0,si,2) pcx pm(0,2) cxx pu(2,a_ ## 0,si)
#undef cp
#define cp(a_,b_)  pl(a_ ## 0,si,2) pcx pm(0,2) spf(b_,si) cxx pu(2,a_ ## 0,si)
#undef c1_2
#define c1_2(a_)   px(2) pld(a_ ## 0,si,2) pcx pm(0,2) cxx pud(2,a_ ## 0,si)
#undef ub
#define ub
#endif

#ifdef SREAL
#undef lb
#define lb         pls(0,ax,0) ps(0,0,0)
#undef c
#define c(a_)      pl(a_ ## 0,si,2) pm(0,2) pu(2,a_ ## 0,si)
#undef cp
#define cp(a_,b_)  pl(a_ ## 0,si,2) spf(b_,si) pm(0,2) pu(2,a_ ## 0,si)
#undef c1_2
#define c1_2(a_)   px(2) pld(a_ ## 0,si,2) pm(0,2) pud(2,a_ ## 0,si)
#undef c1_4
#define c1_4(a_)   pls(a_ ## 0,si,2) pm(0,2) pus(2,a_ ## 0,si)
#undef ub
#define ub
#endif

#ifdef DREAL
#undef lb
#define lb        fl(0,ax)
#undef c
#define c(a_)     fl(a_ ## 0,si) fm(1,0) fl(a_ ## 8,si) fm(2,0) fx1 \
                  fp(a_ ## 0,si) fp(a_ ## 8,si)
#undef cp
#define cp(a_,b_) fl(a_ ## 0,si) fm(1,0) fl(a_ ## 8,si) spf(b_,si) fm(2,0) fx1 \
                  fp(a_ ## 0,si) fp(a_ ## 8,si)
#undef c1_2
#define c1_2(a_)  fl(a_ ## 0,si) fm(1,0) fp(a_ ## 0,si) 
#undef ub
#define ub        fc(0)
#endif

#ifdef DCPLX
#undef lb
#define lb        fl(0,ax) fl(8,ax)
#undef c
#define c(a_)     fl(a_ ## 0,si) fl(a_ ## 8,si) fd(3) fm(2,0) fd(3) \
                  fm(2,0) fx(3) fm(4,0) fx(2) fm(5,0) fap(0,2) fx(2) fsp(2) fx1 \
                  fp(a_ ## 0,si) fp(a_ ## 8,si)
#undef cp
#define cp(a_,b_) fl(a_ ## 0,si) fl(a_ ## 8,si) fd(3) fm(2,0) fd(3) \
                  fm(2,0) fx(3) spf(b_,si) fm(4,0) fx(2) fm(5,0) fap(0,2) fx(2) \
                  fsp(2) fx1 fp(a_ ## 0,si) fp(a_ ## 8,si)
#undef ub
#define ub        fc(0) fc(0)
#endif

#undef sbl1
#define sbl1       c1_4(0x0)
#undef sbl2
#define sbl2       c1_2(0x0)
#undef sbl4
#define sbl4       cp(0x0,0x40)
#undef sbl8
#define sbl8       sbl4 c(0x1)
#undef sbl16
#define sbl16      sbl8 cp(0x2,0x60) c(0x3)

#undef sinc16
#define sinc16    a(0x40,si)
#undef sinc8
#define sinc8     a(0x20,si)
#undef sinc4
#define sinc4     a(0x10,si)
#undef sinc2
#define sinc2     a(0x8,si)
#undef sinc1
#define sinc1     a(0x4,si)

#undef SCALE
#define SCALE Mjoin(Mjoin(PREC,Mjoin(scale,BLC)),FEXT)

#undef MY_FUNCTION
#define MY_FUNCTION SCALE

static void
MY_FUNCTION(const TYPE *b,TYPE *c,int len) {

  const TYPE *ce=c+len;
#if defined(BETAX) && defined(SCPLX)
  const TYPE z1[2]={{1.0,-1.0},{1.0,-1.0}},*z=z1;
#endif
  NO_INLINE

#ifndef SREAL
  len+=len;
#endif
#ifdef DCPLX
  len+=len;
#endif


  ASM(

      "pushl %%ebx\n\t"
      a(4,sp)


      "movl %0,%%esi\n\t"

      spf(0x00,si)
      spf(0x20,si)

      "movl %1,%%eax\n\t"
      "movl %2,%%edi\n\t"

#if defined(BETAX) && defined(SCPLX)
      "movl %3,%%ebx\n\t"
      pl(0,bx,SSREG)
#endif

      lb

      lab(loop)

      test(-16,di)
      je(8)
      sub(16,di)
      align

      sbl16
      sinc16

      jmp(loop)
      align

      lab(8)

      test(8,di)
      je(4)

      sbl8
      sinc8

      lab(4)

      test(4,di)
      je(2)

      sbl4
      sinc4

      lab(2)

#ifndef DCPLX
      test(2,di)
      je(1)

      sbl2
      sinc2

      lab(1)

#ifdef SREAL
      test(1,di)
      je(stop)

      sbl1
      sinc1

      lab(stop)
#endif
#endif

      ub

      a(-4,sp)
      "popl %%ebx\n\t"


      ::"m" (c),"m" (b), "m" (len)
#if defined(BETAX) && defined(SCPLX)
      ,"m" (z)
#endif
      : "si","ax","di");


}
#endif /* CAMM_SCALE_H */
