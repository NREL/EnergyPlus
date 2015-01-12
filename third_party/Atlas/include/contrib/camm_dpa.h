#include <stdlib.h>
#include <sys/time.h>
#include <stdio.h>

#include "camm_util.h"


#if defined(ALIGN) 
#if( defined(SCPLX) || defined(DCPLX))
#error Cannot align complex routines
#endif
#if defined(SREAL) && ( NDPM != 1 ) && ( STRIDE % 4 != 0)
#error Can only align SREAL with NDPM 1 or STRIDE % 4 = 0
#endif
#if defined(DREAL) && ( NDPM != 1 ) && ( STRIDE % 2 != 0)
#error Can only align DREAL with NDPM 1 or STRIDE % 2 = 0
#endif
#endif

/******************************************************************************
 *  Single Precision Complex Macros
 ******************************************************************************/  

#ifdef SCPLX

#ifdef NO_TRANSPOSE

#if NDPM > 3 
#error Max NDPM is 3 for SCPLX NO_TRANSPOSE
#endif

#undef plax
#define plax

#undef R1
#define R1 2
#undef R2
#define R2 4
#undef R3
#define R3 6
#undef R4
#define R4 6

#undef TREG
#define TREG 1
#undef SREG
#define SREG 0
#undef CREG
#define CREG 0

#ifdef GER
#undef AREG
#define AREG 0
#undef targ
#define targ(a_)        AREG
#undef wb
#define wb(a_,b_)       pu(AREG,a_,b_)
#undef wbd
#define wbd(a_,b_)      pud(AREG,a_,b_)
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#else
#undef AREG
#define AREG TREG
#undef targ
#define targ(a_)        CREG
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef w
#define w(a_)           pu(CREG,a_ ## 0,si)
#undef w1_2
#define w1_2(a_)        pud(CREG,a_ ## 0,si)
#endif

#undef src
#define src(a_)         a_
#undef mpx
#define mpx(a_)         pls(0,si,a_) ps(0,a_,a_) pls(4,si,P(a_,1)) \
                        ps(0,P(a_,1),P(a_,1)) sign(a_)
#undef madd
#define madd(a_,b_,c_)  pas(a_,b_,c_)
#undef ulfa
#define ulfa(a_)

#else

#undef R1
#define R1 4
#undef R2
#define R2 5
#undef R3
#define R3 6
#undef R4
#define R4 7

#undef TREG
#define TREG 3
#undef SREG
#define SREG 2
#undef CREG
#define CREG 0
#undef targ
#define targ(a_)        a_
#undef src
#define src(a_)         0
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#undef mpx
#define mpx(a_)        px(a_)
#ifdef BETA0
#undef ulfa
#define ulfa(a_)       phl(a_,0) pa(0,a_) pud(a_,0,si)
#else
#undef ulfa
#define ulfa(a_)       pld(0,si,TREG) phl(a_,0) pa(0,a_) pa(TREG,a_) pud(a_,0,si)
#endif
#undef AREG
#define AREG TREG
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef wbs
#define wbs(a_,b_)


#undef plax
#define plax       pc(CREG,1) ps(160,CREG,CREG) ps(245,1,1) sign(CREG)



#endif

#if defined(Conj_) && ! defined(GER) 
#undef sign
#define sign(a_)       pm(SREG,a_)
#else		   
#undef sign
#define sign(a_)       pm(SREG,P(a_,1))
#endif



#undef plb
#define plb(a_,b_)           pl(a_,b_,AREG)
#undef plbd
#define plbd(a_,b_)          px(AREG) pld(a_,b_,AREG)

#undef dpr
#define dpr(a_)              pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dprp
#define dprp(a_,b_,c_)       pf(b_,c_) pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dpi
#define dpi(a_)              pm(P(src(a_),1),TREG) ps(177,TREG,TREG) pa(TREG,targ(a_))

#ifndef GER


#undef plaa
#define plaa(a_)                pl(a_ ## 0,si,CREG) plax
#undef wa
#define wa(a_)                  w(a_)
#undef dp
#define dp(a_,b_,c_)            plb(a_ ## 0,b_) dpr(c_) plb(a_ ## 0,b_) dpi(c_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)     plb(a_ ## 0,b_) dprp(c_,d_,e_) plb(a_ ## 0,b_) dpi(c_)
#undef ddp
#define ddp(a_,b_,c_)           dp(a_,b_,c_)       
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_)    dpp(a_,b_,c_,d_,e_)

#undef plaa1_2
#define plaa1_2(a_)             px(CREG) pld(a_ ## 0,si,CREG) plax
#undef wa1_2
#define wa1_2(a_)               w1_2(a_)
#undef dp1_2
#define dp1_2(a_,b_,c_)         plbd(a_ ## 0,b_) dpr(c_) plbd(a_ ## 0,b_) dpi(c_)
#undef dpp1_2
#define dpp1_2(a_,b_,c_,d_,e_)  plbd(a_ ## 0,b_) dprp(c_,d_,e_) plbd(a_ ## 0,b_) dpi(c_)
#undef ddp1_2
#define ddp1_2(a_,b_,c_)        dp1_2(a_,b_,c_)       
#undef ddpp1_2
#define ddpp1_2(a_,b_,c_,d_,e_) dpp1_2(a_,b_,c_,d_,e_)


#else

#undef lqc
#define lqc(a_)              pl(a_ ## 0,si,TREG)
#undef lqc1
#define lqc1_2(a_)           px(TREG) pld(a_ ## 0,si,TREG)


#undef plaa
#define plaa(a_) 
#undef wa
#define wa(a_)
#undef dp
#define dp(a_,b_,c_)         lqc(a_) plb(a_ ## 0,b_) dpr(c_) \
                             lqc(a_) dpi(c_) wb(a_ ## 0,b_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)  lqc(a_) plb(a_ ## 0,b_) dpr(c_) pf(d_,e_) \
                             lqc(a_) dpi(c_) wb(a_ ## 0,b_)
#undef ddp
#define ddp(a_,b_,c_)        dp(a_,b_,c_)       
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_) dpp(a_,b_,c_,d_,e_)

#undef plaa1_2
#define plaa1_2(a_)
#undef wa1_2
#define wa1_2(a_)
#undef dp1_2
#define dp1_2(a_,b_,c_)         lqc1_2(a_) plbd(a_ ## 0,b_) dpr(c_) \
                                lqc1_2(a_) dpi(c_) wbd(a_ ## 0,b_)
#undef dpp1_2
#define dpp1_2(a_,b_,c_,d_,e_)  lqc1_2(a_) plbd(a_ ## 0,b_) dpr(c_) pf(d_,e_) \
                                lqc1_2(a_) dpi(c_) wbd(a_ ## 0,b_)
#undef ddp1_2
#define ddp1_2(a_,b_,c_)        dp1_2(a_,b_,c_)       
#undef ddpp1_2
#define ddpp1_2(a_,b_,c_,d_,e_) dpp1_2(a_,b_,c_,d_,e_)

#endif

#endif

/******************************************************************************
 *  Single Precision Real Macros
 ******************************************************************************/  

#ifdef SREAL

#ifdef NO_TRANSPOSE

#undef mpx
#define mpx(a_)        pls(0,si,a_) ps(0,a_,a_)
#undef madd
#define madd(a_,b_,c_) pas(a_,b_,c_)
#undef TREG
#define TREG 1
#undef targ
#define targ(a_)        0
#undef src
#define src(a_)         a_
#undef ulfa
#define ulfa(a_)

#ifdef GER
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#undef w1_4
#define w1_4(a_)
#undef CREG
#define CREG 2
#undef AREG
#define AREG 0
#undef cp
#define cp pc(CREG,TREG)
#undef wb
#define wb(a_,b_) pu(AREG,a_,b_)
#undef wbd
#define wbd(a_,b_) pud(AREG,a_,b_)
#undef wbs
#define wbs(a_,b_) pus(AREG,a_,b_)
#else
#undef CREG
#define CREG 0
#undef AREG
#define AREG TREG
#undef cp
#define cp
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef wbs
#define wbs(a_,b_)
#undef w
#define w(a_)           pu(CREG,a_ ## 0,si)
#undef w1_2
#define w1_2(a_)        pud(CREG,a_ ## 0,si)
#undef w1_4
#define w1_4(a_)        pus(CREG,a_ ## 0,si)
#endif

#else

#undef mpx
#define mpx(a_)        px(a_)
#ifdef BETA0
#undef madd
#define madd(a_,b_,c_)
#else
#undef madd
#define madd(a_,b_,c_) pas(a_,b_,c_)
#endif
#undef TREG
#define TREG 3
#undef targ
#define targ(a_)        a_
#undef src
#define src(a_)         0
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#undef w1_4
#define w1_4(a_)
#undef ulfa
#undef ulfa
#define ulfa(a_)       phl(a_,0) pa(0,a_) pc(a_,0) ps(1,0,0) pa(0,a_) \
                       madd(0,si,a_) pus(a_,0,si) 

#undef CREG
#define CREG 0
#undef AREG
#define AREG TREG
#undef cp
#define cp
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef wbs
#define wbs(a_,b_)

#endif

#if defined(ALIGN)
#undef plb
#define plb(a_,b_)           pla(a_,b_,AREG)
#else
#undef plb
#define plb(a_,b_)           pl(a_,b_,AREG)
#endif
#undef plbd
#define plbd(a_,b_)          px(AREG) pld(a_,b_,AREG)
#undef plbs
#define plbs(a_,b_)          pls(a_,b_,AREG)
#undef dpr
#define dpr(a_)              pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dprp
#define dprp(a_,b_,c_)       pf(b_,c_) pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dprs
#define dprs(a_)             pmsr(src(a_),TREG) pasr(TREG,targ(a_))
#undef dprps
#define dprps(a_,b_,c_)      pf(b_,c_) pmsr(src(a_),TREG) pasr(TREG,targ(a_))

#undef plaa
#define plaa(a_)             pl(a_ ## 0,si,CREG) 
#undef wa
#define wa(a_)               w(a_)
#undef dp
#define dp(a_,b_,c_)         cp plb(a_ ## 0,b_) dpr(c_) wb(a_ ## 0,b_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)  cp plb(a_ ## 0,b_) dprp(c_,d_,e_) wb(a_ ## 0,b_)
#undef ddp
#define ddp(a_,b_,c_)        dp(a_,b_,c_)       
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_) dpp(a_,b_,c_,d_,e_)

#undef plaa1_2
#define plaa1_2(a_)             px(CREG) pld(a_ ## 0,si,CREG) 
#undef wa1_2
#define wa1_2(a_)               w1_2(a_)
#undef dp1_2
#define dp1_2(a_,b_,c_)         cp plbd(a_ ## 0,b_) dpr(c_) wbd(a_ ## 0,b_)
#undef dpp1_2
#define dpp1_2(a_,b_,c_,d_,e_)  cp plbd(a_ ## 0,b_) dprp(c_,d_,e_) wbd(a_ ## 0,b_)
#undef ddp1_2
#define ddp1_2(a_,b_,c_)        dp1_2(a_,b_,c_)       
#undef ddpp1_2
#define ddpp1_2(a_,b_,c_,d_,e_) dpp1_2(a_,b_,c_,d_,e_)

#undef plaa1_4
#define plaa1_4(a_)             pls(a_ ## 0,si,CREG) 
#undef wa1_4
#define wa1_4(a_)               w1_4(a_)
#undef dp1_4
#define dp1_4(a_,b_,c_)         cp plbs(a_ ## 0,b_) dprs(c_) wbs(a_ ## 0,b_)
#undef dpp1_4
#define dpp1_4(a_,b_,c_,d_,e_)  cp plbs(a_ ## 0,b_) dprps(c_,d_,e_) wbs(a_ ## 0,b_)
#undef ddp1_4
#define ddp1_4(a_,b_,c_)        dp1_4(a_,b_,c_)       
#undef ddpp1_4
#define ddpp1_4(a_,b_,c_,d_,e_) dpp1_4(a_,b_,c_,d_,e_)



#undef R1
#define R1 4
#undef R2
#define R2 5
#undef R3
#define R3 6
#undef R4
#define R4 7

#endif

/******************************************************************************
 *  Double Precision Real Macros
 ******************************************************************************/  

#ifdef DREAL

#ifdef ATL_SSE2

#ifdef NO_TRANSPOSE

#undef mpx
#define mpx(a_)        pls(0,si,a_) ps(0,a_,a_)
#undef madd
#define madd(a_,b_,c_) pas(a_,b_,c_)
#undef TREG
#define TREG 1
#undef targ
#define targ(a_)        0
#undef src
#define src(a_)         a_
#undef ulfa
#define ulfa(a_)

#ifdef GER
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#undef w1_4
#define w1_4(a_)
#undef CREG
#define CREG 2
#undef AREG
#define AREG 0
#undef cp
#define cp pc(CREG,TREG)
#undef wb
#define wb(a_,b_) pu(AREG,a_,b_)
#undef wbd
#define wbd(a_,b_) pus(AREG,a_,b_)
#undef wbs
/* #define wbs(a_,b_) pus(AREG,a_,b_) */
#else
#undef CREG
#define CREG 0
#undef AREG
#define AREG TREG
#undef cp
#define cp
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef wbs
/* #define wbs(a_,b_) */
#undef w
#define w(a_)           pu(CREG,a_ ## 0,si)
#undef w1_2
#define w1_2(a_)        pus(CREG,a_ ## 0,si)
#undef w1_4
/* #define w1_4(a_)        pus(CREG,a_ ## 0,si) */
#endif

#else

#undef mpx
#define mpx(a_)        px(a_)
#ifdef BETA0
#undef madd
#define madd(a_,b_,c_)
#else
#undef madd
#define madd(a_,b_,c_) pas(a_,b_,c_)
#endif
#undef TREG
#define TREG 3
#undef targ
#define targ(a_)        a_
#undef src
#define src(a_)         0
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#undef w1_4
#define w1_4(a_)
#undef ulfa
#undef ulfa
#define ulfa(a_)       /* phl(a_,0) pa(0,a_) */ pc(a_,0)  ps(1,0,0) pa(0,a_) \
                       madd(0,si,a_) pus(a_,0,si) 

#undef CREG
#define CREG 0
#undef AREG
#define AREG TREG
#undef cp
#define cp
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef wbs
#define wbs(a_,b_)

#endif

#if defined(ALIGN)
#undef plb
#define plb(a_,b_)           pla(a_,b_,AREG)
#else
#undef plb
#define plb(a_,b_)           pl(a_,b_,AREG)
#endif
#undef plbd
#define plbd(a_,b_)          /* px(AREG)  */pls(a_,b_,AREG)
#undef plbs
/* #define plbs(a_,b_)          pls(a_,b_,AREG) */
#undef dpr
#define dpr(a_)              pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dprp
#define dprp(a_,b_,c_)       pf(b_,c_) pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dprs
#define dprs(a_)             pmsr(src(a_),TREG) pasr(TREG,targ(a_))
#undef dprps
#define dprps(a_,b_,c_)      pf(b_,c_) pmsr(src(a_),TREG) pasr(TREG,targ(a_))

#undef plaa
#define plaa(a_)             pl(a_ ## 0,si,CREG) 
#undef wa
#define wa(a_)               w(a_)
#undef dp
#define dp(a_,b_,c_)         cp plb(a_ ## 0,b_) dpr(c_) wb(a_ ## 0,b_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)  cp plb(a_ ## 0,b_) dprp(c_,d_,e_) wb(a_ ## 0,b_)
#undef ddp
#define ddp(a_,b_,c_)        dp(a_,b_,c_)       
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_) dpp(a_,b_,c_,d_,e_)

#undef plaa1_2
#define plaa1_2(a_)             /* px(CREG)  */pls(a_ ## 0,si,CREG) 
#undef wa1_2
#define wa1_2(a_)               w1_2(a_)
#undef dp1_2
#define dp1_2(a_,b_,c_)         cp plbd(a_ ## 0,b_) dprs(c_) wbd(a_ ## 0,b_)
#undef dpp1_2
#define dpp1_2(a_,b_,c_,d_,e_)  cp plbd(a_ ## 0,b_) dprps(c_,d_,e_) wbd(a_ ## 0,b_)
#undef ddp1_2
#define ddp1_2(a_,b_,c_)        dp1_2(a_,b_,c_)       
#undef ddpp1_2
#define ddpp1_2(a_,b_,c_,d_,e_) dpp1_2(a_,b_,c_,d_,e_)

#undef plaa1_4
/* #define plaa1_4(a_)             pls(a_ ## 0,si,CREG)  */
#undef wa1_4
/* #define wa1_4(a_)               w1_4(a_) */
#undef dp1_4
/* #define dp1_4(a_,b_,c_)         cp plbs(a_ ## 0,b_) dprs(c_) wbs(a_ ## 0,b_) */
#undef dpp1_4
/* #define dpp1_4(a_,b_,c_,d_,e_)  cp plbs(a_ ## 0,b_) dprps(c_,d_,e_) wbs(a_ ## 0,b_) */
#undef ddp1_4
/* #define ddp1_4(a_,b_,c_)        dp1_4(a_,b_,c_)        */
#undef ddpp1_4
/* #define ddpp1_4(a_,b_,c_,d_,e_) dpp1_4(a_,b_,c_,d_,e_) */



#undef R1
#define R1 4
#undef R2
#define R2 5
#undef R3
#define R3 6
#undef R4
#define R4 7

#else

#ifdef NO_TRANSPOSE

#undef t0
#define t0(a_)         1
#undef s0
#define s0(a_)         a_
#undef t8
#define t8(a_)         2
#undef s8
#define s8(a_)         a_
#undef w
#define w(a_)          fp(a_ ## 0,si) fp(a_ ## 8,si)
#undef w1_2
#define w1_2(a_)       fp(a_ ## 0,si)
#undef mpx
#define mpx(a_)        fl(0,si) fc(M(a_,2))
#undef madd
#define madd(a_,b_,c_) faa(a_,b_)
#undef ulfa
#define ulfa(a_)       fc(0)

#else

#undef t0
#define t0(a_)         a_
#undef s0
#define s0(a_)         1
#undef t8
#define t8(a_)         a_
#undef s8
#define s8(a_)         2
#undef w
#define w(a_)           
#undef w1_2
#define w1_2(a_)           
#undef mpx
#define mpx(a_)        fz
#ifdef BETA0
#undef madd
#define madd(a_,b_,c_)
#else
#undef madd
#define madd(a_,b_,c_) faa(a_,b_)
#endif
#undef ulfa
#define ulfa(a_)       madd(0,si,a_) fp(0,si)

#endif


#ifndef GER

#undef plaa1_2
#define plaa1_2(a_)              fl(a_ ## 0,si) 
#undef wa1_2
#define wa1_2(a_)                w1_2(a_)
#ifdef NO_TRANSPOSE
#undef ddp1_2
#define ddp1_2(a_,b_,c_)         fl(a_ ## 0,b_) fm(M(s0(c_),1),0) fap(0,t0(c_)) 
#undef dp1_2
#define dp1_2(a_,b_,c_)          ddp1_2(a_,b_,c_)
#else
#undef ddp1_2
#define ddp1_2(a_,b_,c_)         fl(a_ ## 0,b_) fm(s0(c_),0) fap(0,M(t0(c_),1)) 
#undef dp1_2
#define dp1_2(a_,b_,c_)          fl(a_ ## 0,b_) fmp(0,s0(c_)) fap(0,M(t0(c_),2))
#endif

#else

#undef plaa1_2
#define plaa1_2(a_)              fl(a_ ## 0,si) 
#undef wa1_2
#define wa1_2(a_)
#undef ddp1_2
#define ddp1_2(a_,b_,c_)         fd(M(s0(c_),2)) fm(t0(c_),0) faa(a_ ## 0,b_) fp(a_ ## 0,b_) 
#undef dp1_2
#define dp1_2(a_,b_,c_)          fm(M(s0(c_),2),0) faa(a_ ## 0,b_) fp(a_ ## 0,b_) 

#endif



#undef plaa
#define plaa(a_)                 fl(a_ ## 0,si) fl(a_ ## 8,si) fx1

#ifndef GER


#undef wa
#define wa(a_)                   w(a_)


#undef ddp
#define ddp(a_,b_,c_)            fl(a_ ## 0,b_) fm(s0(c_),0) fl(a_ ## 8,b_) \
                                 fm(P(s8(c_),1),0) fx1 fap(0,P(t0(c_),1)) \
                                 fap(0,t8(c_))
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_)     fl(a_ ## 0,b_) fm(s0(c_),0) fl(a_ ## 8,b_) \
                                 fm(P(s8(c_),1),0)  pf(d_,e_) fx1 fap(0,P(t0(c_),1)) \
                                 fap(0,t8(c_))

/* #define ddp(a_,b_,c_)            fd(M(s0(c_),1)) fma(a_ ## 0,b_) fap(0,t0(c_)) \ */
/*                                  fd(M(s8(c_),1)) fma(a_ ## 8,b_) fap(0,t8(c_)) */
/* #define ddpp(a_,b_,c_,d_,e_)     fd(M(s0(c_),1)) fma(a_ ## 0,b_) fap(0,t0(c_)) \ */
/*                                   \ */
/*                                  fd(M(s8(c_),1)) fma(a_ ## 8,b_) fap(0,t8(c_)) pf(d_,e_) */

#ifdef NO_TRANSPOSE

#undef dp
#define dp(a_,b_,c_)             ddp(a_,b_,c_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)      ddpp(a_,b_,c_,d_,e_)

#else

#undef dp
#define dp(a_,b_,c_)             fl(a_ ## 0,b_) fmp(0,s0(c_)) fl(a_ ## 8,b_) \
                                 fmp(0,s8(c_)) fap(0,M(t0(c_),1)) fap(0,M(t8(c_),2))
#undef dpp
#define dpp(a_,b_,c_,d_,e_)      fl(a_ ## 0,b_)  pf(d_ ,e_) fmp(0,s0(c_)) fl(a_ ## 8,b_) \
                                 fmp(0,s8(c_)) fap(0,M(t0(c_),1)) fap(0,M(t8(c_),2))

/* #define dp(a_,b_,c_)             fma(a_ ## 0,b_) fap(0,M(t0(c_),1))  \ */
/*                                  fma(a_ ## 8,b_) fap(0,M(t8(c_),2)) */
/* #define dpp(a_,b_,c_,d_,e_)      fma(a_ ## 0,b_) fap(0,M(t0(c_),1))  \ */
/*                                   \ */
/* 			         fma(a_ ## 8,b_) fap(0,M(t8(c_),2)) pf(d_,e_) */

#endif


#else

#undef wa
#define wa(a_)
#undef ddp
#define ddp(a_,b_,c_)            fd(M(s0(c_),1)) fm(t0(c_),0) faa(a_ ## 0,b_) fp(a_ ## 0,b_) \
                                 fd(M(s8(c_),1)) fm(t8(c_),0) faa(a_ ## 8,b_) fp(a_ ## 8,b_)
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_)     fd(M(s0(c_),1)) fm(t0(c_),0) faa(a_ ## 0,b_) fp(a_ ## 0,b_) \
                                 fd(M(s8(c_),1)) fm(t8(c_),0) faa(a_ ## 8,b_) fp(a_ ## 8,b_) pf(d_,e_)

#undef dp
#define dp(a_,b_,c_)             fm(M(s0(c_),1),0) faa(a_ ## 0,b_) fp(a_ ## 0,b_) \
                                 fm(M(s8(c_),2),0) faa(a_ ## 8,b_) fp(a_ ## 8,b_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)      fm(M(s0(c_),1),0) faa(a_ ## 0,b_) fp(a_ ## 0,b_) \
                                 fm(M(s8(c_),2),0) faa(a_ ## 8,b_) fp(a_ ## 8,b_) pf(d_,e_)

#endif


#undef R1
#define R1 3
#undef R2
#define R2 4
#undef R3
#define R3 5
#undef R4
#define R4 6

#endif

#endif

/******************************************************************************
 *  Double Precision Complex Macros
 ******************************************************************************/  

#ifdef DCPLX

#ifdef ATL_SSE2
#ifdef NO_TRANSPOSE

#if NDPM > 3 
#error Max NDPM is 3 for DCPLX NO_TRANSPOSE
#endif

#undef plax
#define plax

#undef R1
#define R1 2
#undef R2
#define R2 4
#undef R3
#define R3 6
#undef R4
#define R4 6

#undef TREG
#define TREG 1
#undef SREG
#define SREG 0
#undef CREG
#define CREG 0

#ifdef GER
#undef AREG
#define AREG 0
#undef targ
#define targ(a_)        AREG
#undef wb
#define wb(a_,b_)       pu(AREG,a_,b_)
#undef wbd
/* #define wbd(a_,b_)      pud(AREG,a_,b_) */
#undef w
#define w(a_)
#undef w1_2
/* #define w1_2(a_) */
#else
#undef AREG
#define AREG TREG
#undef targ
#define targ(a_)        CREG
#undef wb
#define wb(a_,b_)
#undef wbd
/* #define wbd(a_,b_) */
#undef w
#define w(a_)           pu(CREG,a_ ## 0,si)
#undef w1_2
/* #define w1_2(a_)        pud(CREG,a_ ## 0,si) */
#endif

#undef src
#define src(a_)         a_
#undef mpx
#define mpx(a_)         pls(0,si,a_) ps(0,a_,a_) pls(8,si,P(a_,1)) \
                        ps(0,P(a_,1),P(a_,1)) sign(a_)
#undef madd
#define madd(a_,b_,c_)  pas(a_,b_,c_)
#undef ulfa
#define ulfa(a_)

#else

#undef R1
#define R1 4
#undef R2
#define R2 5
#undef R3
#define R3 6
#undef R4
#define R4 7

#undef TREG
#define TREG 3
#undef SREG
#define SREG 2
#undef CREG
#define CREG 0
#undef targ
#define targ(a_)        a_
#undef src
#define src(a_)         0
#undef w
#define w(a_)
#undef w1_2
#define w1_2(a_)
#undef mpx
#define mpx(a_)        px(a_)
#ifdef BETA0
#undef ulfa
#define ulfa(a_)       /* phl(a_,0) pa(0,a_)  */pu(a_,0,si)
#else
#undef ulfa
#define ulfa(a_)       pl(0,si,TREG) /* phl(a_,0) pa(0,a_) */ pa(TREG,a_) pu(a_,0,si)
#endif
#undef AREG
#define AREG TREG
#undef wb
#define wb(a_,b_)
#undef wbd
#define wbd(a_,b_)
#undef wbs
#define wbs(a_,b_)


#undef plax
#define plax       pc(CREG,1) ps(0,CREG,CREG) ps(3,1,1) sign(CREG)



#endif

#if defined(Conj_) && ! defined(GER) 
#undef sign
#define sign(a_)       pm(SREG,a_)
#else		   
#undef sign
#define sign(a_)       pm(SREG,P(a_,1))
#endif



#undef plb
#define plb(a_,b_)           pl(a_,b_,AREG)
#undef plbd
/* #define plbd(a_,b_)          px(AREG) pld(a_,b_,AREG) */

#undef dpr
#define dpr(a_)              pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dprp
#define dprp(a_,b_,c_)       pf(b_,c_) pm(src(a_),TREG) pa(TREG,targ(a_))
#undef dpi
#define dpi(a_)              pm(P(src(a_),1),TREG) ps(1,TREG,TREG) pa(TREG,targ(a_))

#ifndef GER

#undef plaa
#define plaa(a_)                pl(a_ ## 0,si,CREG) plax
#undef wa
#define wa(a_)                  w(a_)
#undef dp
#define dp(a_,b_,c_)            plb(a_ ## 0,b_) dpr(c_) plb(a_ ## 0,b_) dpi(c_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)     plb(a_ ## 0,b_) dprp(c_,d_,e_) plb(a_ ## 0,b_) dpi(c_)
#undef ddp
#define ddp(a_,b_,c_)           dp(a_,b_,c_)       
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_)    dpp(a_,b_,c_,d_,e_)

#undef plaa1_2
/* #define plaa1_2(a_)             px(CREG) pld(a_ ## 0,si,CREG) plax */
#undef wa1_2
/* #define wa1_2(a_)               w1_2(a_) */
#undef dp1_2
/* #define dp1_2(a_,b_,c_)         plbd(a_ ## 0,b_) dpr(c_) plbd(a_ ## 0,b_) dpi(c_) */
#undef dpp1_2
/* #define dpp1_2(a_,b_,c_,d_,e_)  plbd(a_ ## 0,b_) dprp(c_,d_,e_) plbd(a_ ## 0,b_) dpi(c_) */
#undef ddp1_2
/* #define ddp1_2(a_,b_,c_)        dp1_2(a_,b_,c_)        */
#undef ddpp1_2
/* #define ddpp1_2(a_,b_,c_,d_,e_) dpp1_2(a_,b_,c_,d_,e_) */


#else

#undef lqc
#define lqc(a_)              pl(a_ ## 0,si,TREG)
#undef lqc1
/* #define lqc1_2(a_)           px(TREG) pld(a_ ## 0,si,TREG) */


#undef plaa
#define plaa(a_) 
#undef wa
#define wa(a_)
#undef dp
#define dp(a_,b_,c_)         lqc(a_) plb(a_ ## 0,b_) dpr(c_) \
                             lqc(a_) dpi(c_) wb(a_ ## 0,b_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)  lqc(a_) plb(a_ ## 0,b_) dpr(c_) pf(d_,e_) \
                             lqc(a_) dpi(c_) wb(a_ ## 0,b_)
#undef ddp
#define ddp(a_,b_,c_)        dp(a_,b_,c_)       
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_) dpp(a_,b_,c_,d_,e_)

#undef plaa1_2
/* #define plaa1_2(a_) */
#undef wa1_2
/* #define wa1_2(a_) */
#undef dp1_2
/* #define dp1_2(a_,b_,c_)         lqc1_2(a_) plbd(a_ ## 0,b_) dpr(c_) \ */
/*                                 lqc1_2(a_) dpi(c_) wbd(a_ ## 0,b_) */
#undef dpp1_2
/* #define dpp1_2(a_,b_,c_,d_,e_)  lqc1_2(a_) plbd(a_ ## 0,b_) dpr(c_) pf(d_,e_) \ */
/*                                 lqc1_2(a_) dpi(c_) wbd(a_ ## 0,b_) */
#undef ddp1_2
/* #define ddp1_2(a_,b_,c_)        dp1_2(a_,b_,c_)        */
#undef ddpp1_2
/* #define ddpp1_2(a_,b_,c_,d_,e_) dpp1_2(a_,b_,c_,d_,e_) */

#endif

#else 

#if NDPM > 2
#error Max NDPM is 2 for DCPLX
#endif

#undef TREG
#define TREG           2

#ifdef NO_TRANSPOSE

#undef w
#define w(a_)          fp(a_ ## 0,si) fp(a_ ## 8,si)
#undef plax
#define plax           fx1
#undef srr
#define srr(a_)        a_
#undef sri
#define sri(a_)        a_
#undef sir
#define sir(a_)        a_
#undef sii
#define sii(a_)        a_
#undef trr
#define trr(a_)        P(TREG,1)
#undef tri
#define tri(a_)        M(TREG,1)
#undef tir
#define tir(a_)        TREG
#undef tii
#define tii(a_)        TREG
#undef mpx
#define mpx(a_)        fl(0,si) fl(8,si) fc(M(a_,2)) fc(M(a_,2)) 
#undef madd
#define madd(a_,b_,c_) faa(a_,b_)
#undef ulfa
#define ulfa(a_)       fc(0) fc(0)

#else

#undef srr
#define srr(a_)       P(TREG,1)
#undef sri
#define sri(a_)       M(TREG,1)
#undef sir
#define sir(a_)       TREG
#undef sii
#define sii(a_)       TREG
#undef trr
#define trr(a_)       a_
#undef tri
#define tri(a_)       a_
#undef tir
#define tir(a_)       a_
#undef tii
#define tii(a_)       a_
#undef w
#define w(a_)           
#undef plax
#define plax  
#undef mpx
#define mpx(a_)        fz fz
#ifdef BETA0
#undef madd
#define madd(a_,b_,c_)
#else
#undef madd
#define madd(a_,b_,c_) faa(a_,b_)
#endif
#undef ulfa
#define ulfa(a_)       madd(0,si,a_) fp(0,si) madd(8,si,a_) fp(8,si)

#endif



#ifdef Conj_
#undef fapi
#define fapi(a_,b_)   fsp(b_)
#undef fspi
#define fspi(a_,b_)   fap(a_,b_)
#else
#undef fapi
#define fapi(a_,b_)   fap(a_,b_)
#undef fspi
#define fspi(a_,b_)   fsp(b_)
#endif

#ifndef GER


#undef plaa
#define plaa(a_)             fl(a_ ## 0,si) fl(a_ ## 8,si) plax
#undef wa
#define wa(a_)               w(a_)
#undef ddp
#define ddp(a_,b_,c_)        fl(a_ ## 0,b_) fd(0) fm(srr(c_),0) fap(0,trr(c_)) \
                                                  fm(sri(c_),0) fap(0,tri(c_))\
                             fl(a_ ## 8,b_) fd(0) fm(sir(c_),0) fspi(0,tir(c_)) \
                                                  fm(sii(c_),0) fapi(0,tii(c_))
#undef ddpp
#define ddpp(a_,b_,c_,d_,e_) fl(a_ ## 0,b_) fd(0) fm(srr(c_),0) fap(0,trr(c_)) \
                                                  fm(sri(c_),0) fap(0,tri(c_))\
                             fl(a_ ## 8,b_) fd(0) pf(d_,e_) fm(sir(c_),0) fspi(0,tir(c_))\
                                                  fm(sii(c_),0) fapi(0,tii(c_))



#ifdef NO_TRANSPOSE



#undef dp
#define dp(a_,b_,c_)         ddp(a_,b_,c_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)  ddpp(a_,b_,c_,d_,e_)



#else

#undef dp
#define dp(a_,b_,c_)        fl(a_ ## 0,b_) fd(0) fm(srr(c_),0) fap(0,trr(c_)) \
                                                 fm(sri(c_),0) fap(0,tri(c_))\
                            fl(a_ ## 8,b_)       fm(0,sir(c_)) fmp(0,M(sir(c_),1)) \
                                                 fspi(0,M(tir(c_),2)) fapi(0,M(tii(c_),2))

#undef dpp
#define dpp(a_,b_,c_,d_,e_) fl(a_ ## 0,b_) fd(0) fm(srr(c_),0) fap(0,trr(c_)) \
                                                 pf(d_,e_) fm(sri(c_),0) fap(0,tri(c_))\
                            fl(a_ ## 8,b_)       fm(0,sir(c_)) fmp(0,M(sir(c_),1)) \
                                                 fspi(0,M(tir(c_),2)) fapi(0,M(tii(c_),2))


#endif

#else

#undef plaa
#define plaa(a_)            fl(a_ ## 0,si) fl(a_ ## 8,si) plax
#undef wa
#define wa(a_)

#undef ddprr
#define ddprr(a_,b_,c_)     fl(a_ ## 0,b_) \
                                              fd(tri(c_))           fm(P(sri(c_),1),0)      fap(0,1) \
                                              fd(M(trr(c_),1))      fm(srr(c_),0)           fspi(0,1) \
                            fp(a_ ## 0,b_) 
#undef ddpri
#define ddpri(a_,b_,c_)     fl(a_ ## 8,b_) \
                                              fd(tii(c_))           fm(P(sii(c_),1),0)      fap(0,1) \
                                              fd(M(tir(c_),1))      fm(sir(c_),0)           fapi(0,1) \
                            fp(a_ ## 8,b_) 
#undef dpri
#define dpri(a_,b_,c_)      fl(a_ ## 8,b_) \
                                              fx(2)                 fm(sir(c_),0)           fap(0,2) \
                                                                    fm(M(sii(c_),2),0)      fapi(0,1) \
                            fp(a_ ## 8,b_)


#undef ddpp
#define ddpp(a_,b_,c_,d_,e_) ddprr(a_,b_,c_) pf(d_,e_) ddpri(a_,b_,c_)
#undef ddp
#define ddp(a_,b_,c_)        ddprr(a_,b_,c_)           ddpri(a_,b_,c_)
#undef dpp
#define dpp(a_,b_,c_,d_,e_)  ddprr(a_,b_,c_) pf(d_,e_) dpri(a_,b_,c_)
#undef dp
#define dp(a_,b_,c_)         ddprr(a_,b_,c_)           dpri(a_,b_,c_)

#endif


#undef R1
#define R1 4
#undef R2
#define R2 6
#undef R3
#define R3 6
#undef R4
#define R4 6

#endif

#endif


/******************************************************************************
 *  General Macros
 ******************************************************************************/  




#undef bla1
#define bla1(a_,b_)          plaa(a_) dpp(a_,ax,R1,b_,si) wa(a_) 
#undef blb1
#define blb1(a_,b_)          plaa(a_) dpp(a_,ax,R1,b_,ax) wa(a_)
			     
#undef bla2
#undef bla2
#define bla2(a_,b_)          pf(b_,si) plaa(a_) ddp(a_,ax,R1)        pf(b_,ax) dp(a_,bx,R2) wa(a_)
#undef blb2
#undef blb2
#define blb2(a_,b_)                    plaa(a_) ddpp(a_,ax,R1,b_,bx)           dp(a_,bx,R2) wa(a_) 
			     
#undef bla3
#define bla3(a_,b_)          plaa(a_) ddpp(a_,ax,R1,b_,si) ddp(a_,bx,R2) \
                             dpp(a_,cx,R3,b_,ax) wa(a_)
#undef blb3
#define blb3(a_,b_)          plaa(a_) ddpp(a_,ax,R1,b_,bx) ddp(a_,bx,R2) \
                             dpp(a_,cx,R3,b_,cx) wa(a_)
			     
#undef bla4
#define bla4(a_,b_)          plaa(a_) ddpp(a_,ax,R1,b_,si) ddpp(a_,bx,R2,b_,ax) \
                             ddp(a_,cx,R3) dpp(a_,dx,R4,b_,bx) wa(a_)
#undef blb4
#define blb4(a_,b_)          plaa(a_) ddp(a_,ax,R1)        ddpp(a_,bx,R2,b_,cx) \
                             ddp(a_,cx,R3) dpp(a_,dx,R4,b_,dx) wa(a_)

#undef bla
#define bla(a_,b_)      Mjoin(bla,NDP)(a_,b_)
#undef blb
#define blb(a_,b_)      Mjoin(blb,NDP)(a_,b_)



#undef bla11_2
#define bla11_2(a_)    plaa1_2(a_) dp1_2(a_,ax,R1) wa1_2(a_) 
#undef bla21_2
#define bla21_2(a_)    plaa1_2(a_) ddp1_2(a_,ax,R1) dp1_2(a_,bx,R2) wa1_2(a_)
#undef bla31_2
#define bla31_2(a_)    plaa1_2(a_) ddp1_2(a_,ax,R1) ddp1_2(a_,bx,R2) \
                          dp1_2(a_,cx,R3) wa1_2(a_)
#undef bla41_2
#define bla41_2(a_)    plaa1_2(a_) ddp1_2(a_,ax,R1) ddp1_2(a_,bx,R2) \
                          ddp1_2(a_,cx,R3) dp1_2(a_,dx,R4) wa1_2(a_)

#undef bla1_2
#define bla1_2(a_)     Mjoin(Mjoin(bla,NDP),1_2)(a_)



#undef bla11_4
#define bla11_4(a_)    plaa1_4(a_) dp1_4(a_,ax,R1) wa1_4(a_) 
#undef bla21_4
#define bla21_4(a_)    plaa1_4(a_) ddp1_4(a_,ax,R1) dp1_4(a_,bx,R2) wa1_4(a_)
#undef bla31_4
#define bla31_4(a_)    plaa1_4(a_) ddp1_4(a_,ax,R1) ddp1_4(a_,bx,R2) \
                          dp1_4(a_,cx,R3) wa1_4(a_)
#undef bla41_4
#define bla41_4(a_)    plaa1_4(a_) ddp1_4(a_,ax,R1) ddp1_4(a_,bx,R2) \
                          ddp1_4(a_,cx,R3) dp1_4(a_,dx,R4) wa1_4(a_)

#undef bla1_4
#define bla1_4(a_)     Mjoin(Mjoin(bla,NDP),1_4)(a_)



#undef inc1
#define inc1(a_)        a(a_,si) a(a_,ax)
#undef inc2
#define inc2(a_)        inc1(a_) a(a_,bx)
#undef inc3
#define inc3(a_)        inc2(a_) a(a_,cx)
#undef inc4
#define inc4(a_)        inc3(a_) a(a_,dx)

#undef inc
#define inc(a_)         Mjoin(inc,NDP)(a_)


#ifdef PREFETCH
/* #include "camm_arith.h" */
#undef S
#define S(a_,b_) (a_) + (b_)
#undef PF1
#define PF1 PREFETCH
#undef PF2
#define PF2 S(PF1,32)
#undef PF3
#define PF3 S(PF1,64)
#undef PF4
#define PF4 S(PF1,96)
#undef PF5
#define PF5 S(PF1,128)
#undef PF6
#define PF6 S(PF1,160)
#undef PF7
#define PF7 S(PF1,192)
#undef PF8
#define PF8 S(PF1,224)
#else
#undef PF1
#define PF1 64
#undef PF2
#define PF2 96
#undef PF3
#define PF3 128
#undef PF4
#define PF4 160
#undef PF5
#define PF5 192
#undef PF6
#define PF6 224
#undef PF7
#define PF7 256
#undef PF8
#define PF8 288
#endif


#if defined(NO_TRANSPOSE) && !defined(SREAL) && !defined(GER)
#undef pf
#define pf(a_,b_)  f(t0,a_,b_)
#else
#undef pf
#define pf(a_,b_)  f(nta,a_,b_)
#endif

#undef bl1
#define bl1            bla1_4(0x0) inc(4)
#undef bl2
#define bl2            bla1_2(0x0) inc(8)
#undef bl4
#define bl4            bla(0x0,PF1) inc(16)
#undef bl8
#define bl8            bla(0x0,PF1) blb(0x1,PF1) inc(32) 
#undef bl16
#define bl16           bla(0x0,PF1) blb(0x1,PF1) bla(0x2,PF2) blb(0x3,PF2) inc(64)
#undef bl32
#define bl32           bla(0x0,PF1) blb(0x1,PF1) bla(0x2,PF2) blb(0x3,PF2) \
                       bla(0x4,PF3) blb(0x5,PF3) bla(0x6,PF4) blb(0x7,PF4) inc(128)
#undef bl64
#define bl64           bla(0x0,PF1) blb(0x1,PF1) bla(0x2,PF2) blb(0x3,PF2) \
                       bla(0x4,PF3) blb(0x5,PF3) bla(0x6,PF4) blb(0x7,PF4) \
                       bla(0x8,PF5) blb(0x9,PF5) bla(0xa,PF6) blb(0xb,PF6) \
                       bla(0xc,PF7) blb(0xd,PF7) bla(0xe,PF8) blb(0xf,PF8) inc(256)

/* #define in2           inc(8) */
/* #define in4           inc(16) */
/* #define in8           inc(32) */
/* #define in16          inc(64) */

#undef in2
#define in2  
#undef in4
#define in4  
#undef in8
#define in8  
#undef in16
#define in16 

#ifdef NO_TRANSPOSE
#undef incf
#define incf           ra(di,si)
#else
#undef incf
#define incf
#endif

#undef lf1
#define lf1            mpx(R1)
#undef lf2
#define lf2            lf1 incf mpx(R2)
#undef lf3
#define lf3            lf2 incf mpx(R3)
#undef lf4
#define lf4            lf3 incf mpx(R4)

#undef lf
#define lf             Mjoin(lf,NDP)


#undef ulf1
#define ulf1           ulfa(R1)
#undef ulf2
#define ulf2           ulf1 ra(di,si) ulfa(R2) 
#undef ulf3
#define ulf3           ulf2 ra(di,si) ulfa(R3) 
#undef ulf4
#define ulf4           ulf3 ra(di,si) ulfa(R4) 

#undef ulf
#define ulf            Mjoin(ulf,NDP)

#undef lpba
#define lpba(a_)      "movl %%esi,%%e" #a_ "\n\t"

#undef lpb1
#define lpb1          lpba(ax)
#undef lpb2
#define lpb2          lpb1 ra(di,si) lpba(bx)
#undef lpb3
#define lpb3          lpb2 ra(di,si) lpba(cx)
#undef lpb4
#define lpb4          lpb3 ra(di,si) lpba(dx)

#undef lpb
#define lpb           Mjoin(lpb,NDP)

#undef ipf1
#define ipf1(a_)   pf(a_,si) pf(a_,ax)
#undef ipf2
#define ipf2(a_)   ipf1(a_)  pf(a_,bx) 
#undef ipf3
#define ipf3(a_)   ipf2(a_)  pf(a_,cx) 
#undef ipf4
#define ipf4(a_)   ipf3(a_)  pf(a_,dx) 

#undef ipf
#define ipf(a_)     Mjoin(ipf,NDP)(a_)

#ifdef LUNROLL
#undef UNROLL
#ifdef SREAL
#undef UNROLL
#define UNROLL LUNROLL
#elif defined(DREAL) || defined(SCPLX)
#undef UNROLL
#define UNROLL LUNROLL*2
#elif defined(DCPLX)
#undef UNROLL
#define UNROLL LUNROLL*4
#endif
#else
#undef UNROLL
#define UNROLL 16
#endif

#undef UNROLL1_2
#if UNROLL == 64
#undef blUNROLL
#define blUNROLL bl64
#undef UNROLL1_2
#define UNROLL1_2 32
#elif UNROLL == 32
#undef blUNROLL
#define blUNROLL bl32
#undef UNROLL1_2
#define UNROLL1_2 16
#elif UNROLL == 16
#undef blUNROLL
#define blUNROLL bl16
#undef UNROLL1_2
#define UNROLL1_2 8
#elif UNROLL == 8
#undef blUNROLL
#define blUNROLL bl8
#undef UNROLL1_2
#define UNROLL1_2 4
#elif UNROLL == 4
#undef blUNROLL
#define blUNROLL bl4
#undef UNROLL1_2
#define UNROLL1_2 2
#elif UNROLL == 2
#undef blUNROLL
#define blUNROLL bl2
#undef UNROLL1_2
#define UNROLL1_2 1
#elif UNROLL == 1
#undef blUNROLL
#define blUNROLL bl1
#undef UNROLL1_2
#define UNROLL1_2 stop
#endif
#ifndef UNROLL1_2
#error UNROLL must be set to power of 2 < 128
#endif


#ifdef GER
#undef aconst
#define aconst
#undef cconst
#define cconst const
#else
#undef aconst
#define aconst const
#undef cconst
#define cconst
#endif

#undef MY_FUNCTION
#define MY_FUNCTION Mjoin(dp,EXT)

static void
MY_FUNCTION(aconst TYPE *a,int lda,
	      const TYPE *b,
	      cconst TYPE *c,int stride,int len) {

#ifdef SCPLX
#if defined(GER) && defined(Conj_)
    const TYPE w1[2]={{-1.0,1.0},{-1.0,1.0}},*w=w1;
#else
    const TYPE w1[2]={{1.0,-1.0},{1.0,-1.0}},*w=w1;
#endif
#endif

#if defined(DCPLX) && defined(ATL_SSE2)
#if defined(GER) && defined(Conj_)
    const TYPE w1[1]={{-1.0,1.0}},*w=w1;
#else
    const TYPE w1[1]={{1.0,-1.0}},*w=w1;
#endif
#endif

#ifdef NO_TRANSPOSE
#undef movm
#define movm c
#undef fixm
#define fixm b
#else
#undef movm
#define movm b
#undef fixm
#define fixm c
#endif    
    NO_INLINE
    unsigned u1=stride*sizeof(*fixm),u2=lda*sizeof(*a),u3=len*sizeof(*movm)/sizeof(float);

    ASM (

	 "pushl %%ebx\n\t"
	 a(4,sp)

#if defined(SCPLX) || (defined(DCPLX) && defined(ATL_SSE2))
	 "movl %6,%%esi\n\t"
	 pl(0,si,SREG)
#endif
	 
#ifdef NO_TRANSPOSE
	 "movl %1,%%esi\n\t"  /* fixm */
	 "movl %2,%%edi\n\t"  /* fixm2fixm */
#endif

	 lf

	 "movl %3,%%esi\n\t"  /* a */
	 "movl %4,%%edi\n\t"  /* a2a */

	 lpb

	 ipf(0)

	 "movl %0,%%esi\n\t"  /* movm */
	 "movl %5,%%edi\n\t"  /* len */

#if defined(ALIGN)

#if defined(SREAL)

	 test(4,ax)
	 je(Mjoin(a1,EXT))
	 test(-1,di)
	 je(Mjoin(a1,EXT))
	 sub(1,di)
	 bl1

	 lab(Mjoin(a1,EXT))

#endif

#if defined(DREAL) || defined(SREAL)

	 test(8,ax)
	 je(Mjoin(as,EXT))
	 test(-2,di)
	 je(Mjoin(as,EXT))
	 sub(2,di)
	 bl2

	 lab(Mjoin(as,EXT))

#endif

#endif
	      

	 ipf(32)

	 lab(Mjoin(loop,EXT))

	 test(-UNROLL,di)
	 je(Mjoin(UNROLL1_2,EXT))
	 sub(UNROLL,di)

	 blUNROLL
	 
	 jmp(Mjoin(loop,EXT))

#if UNROLL > 32
	 lab(Mjoin(32,EXT))
	 test(32,di)
	 je(Mjoin(16,EXT))
	 bl32
#endif	 

#if UNROLL > 16
	 lab(Mjoin(16,EXT))
	 test(16,di)
	 je(Mjoin(8,EXT))
	 bl16
#endif	 

#if UNROLL > 8
	 lab(Mjoin(8,EXT))
	 test(8,di)
	 je(Mjoin(4,EXT))
	 bl8
#endif	 

#if UNROLL > 4
	 lab(Mjoin(4,EXT))
	 test(4,di)
	 je(Mjoin(2,EXT))
	 bl4
#endif

#if UNROLL > 2	 
	 lab(Mjoin(2,EXT))
#ifndef DCPLX
	 test(2,di)
	 je(Mjoin(1,EXT))
	 bl2
#endif
#endif

#if UNROLL > 1
	 lab(Mjoin(1,EXT))
#ifdef SREAL
	 test(1,di)
	 je(Mjoin(stop,EXT))
	 bl1
#endif
#endif

	 lab(Mjoin(stop,EXT))

#ifndef NO_TRANSPOSE
	 "movl %1,%%esi\n\t"  /* fixm */
	 "movl %2,%%edi\n\t"  /* fixm2fixm */
#endif

	 ulf

	 a(-4,sp)
	 "popl %%ebx\n\t"


	 ::"m" (movm),"m" (fixm),"m" (u1),"m" (a),"m" (u2),"m" (u3)

#if defined(SCPLX) || (defined(DCPLX) && defined(ATL_SSE2))
	 ,"m" (w)
#endif
	 :"ax","bx","cx","dx","si","di");


}

