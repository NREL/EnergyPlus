#ifdef GER
#undef NO_TRANSPOSE
#define NO_TRANSPOSE
#endif


#if NDPM > 4
#error Max NDPM is 4 
#endif

#if !defined(ATL_SSE1) && ( defined(SREAL) || defined(SCPLX) )
#error This routine needs ATL_SSE1 defined
#endif

#if !defined(ATL_SSE2) && ( defined(DREAL) || defined(DCPLX) )
#error This routine needs ATL_SSE2 defined
#endif

#include <stdio.h>
#include <stdlib.h>
#if defined(NO_TRANSPOSE) && defined(BETA0)
#include <string.h>
#endif

#include "camm_util.h"

#ifndef GER
#if defined(BETAX) || defined(BETAXI0)
#include "camm_scale.h"
#endif
#endif

#if NDPM >= 4
#define EXT4 Mjoin(4dp,BLC)
#undef NDP
#define NDP 4
#undef EXT
#define EXT EXT4
#include "camm_dpa.h"
#endif

#if NDPM >= 3
#define EXT3 Mjoin(3dp,BLC)
#undef NDP
#define NDP 3
#undef EXT
#define EXT EXT3
#include "camm_dpa.h"
#endif

#if NDPM >= 2
#define EXT2 Mjoin(2dp,BLC)
#undef NDP
#define NDP 2
#undef EXT
#define EXT EXT2
#include "camm_dpa.h"
#endif

#define EXT1 Mjoin(1dp,BLC)
#undef NDP
#define NDP 1
#undef EXT
#define EXT EXT1
#include "camm_dpa.h"

#undef NDP
#define NDP NDPM
#undef EXT
#define EXT Mjoin(Mjoin(NDP,Mjoin(dp,BLC)),m)
#include "camm_dpa.h"

#ifdef GER
#if defined(SCPLX) || defined(DCPLX)
#ifdef Conj_
#define IM 1c
#else
#define IM 1u
#endif
#else
#define IM 1
#endif


#define FN Mjoin(Mjoin(Mjoin(ATL_,PREC),Mjoin(ger,IM)),_a1_x1_yX)

#undef MY_FUNCTION
#define MY_FUNCTION FN

void 
MY_FUNCTION(int m,int n, const SCALAR alpha,const TYPE *c,
   int cinc,const TYPE *b,int binc,
   TYPE *a,int lda) {

#else


#define FN Mjoin(Mjoin(Mjoin(ATL_,PREC),gemv),Mjoin(FEXT,Mjoin(_a1_x1_,Mjoin(BL,_y1))))

#undef MY_FUNCTION
#define MY_FUNCTION FN

void 
MY_FUNCTION(int m,int n, const SCALAR alpha,const TYPE *a,
   int lda,const TYPE *b,int binc,
   const SCALAR beta,TYPE *c,int cinc) {

#endif

  int i,mm,nn;
  const TYPE *ae;
#ifdef NO_TRANSPOSE
  int len=m,w=n;
#define zz b
#else
  int len=n,w=m;
#define zz c
#endif

#ifdef GER
#define zzinc binc
#else
#define zzinc 1


#if defined(NO_TRANSPOSE) && defined(BETA0)
  memset(c,0,m*sizeof(*c));
#endif

#if defined(BETAX) || defined(BETAXI0)
#if defined(SCPLX) || defined(DCPLX)
  SCALE(beta,c,m);
#endif
#if defined(SREAL) || defined(DREAL)
  SCALE(&beta,c,m);
#endif
#endif

#endif

  ae=a+w*lda;
  nn=STRIDE*lda;


#if NDPM == 1
  for (;a<ae;a+=lda,zz+=zzinc)
    Mjoin(dp,EXT)(a,nn,b,c,STRIDE*zzinc,len);

#else

  while (a+NDPM*nn<=ae) {
    for (i=0;i<STRIDE;i++,a+=lda,zz+=zzinc) 
      Mjoin(dp,EXT)(a,nn,b,c,STRIDE*zzinc,len);

    a+=(NDPM-1)*nn;
    zz+=(NDPM-1)*STRIDE*zzinc;
  }

  for (i=0;a<ae && i<STRIDE;i++,a+=lda,zz+=zzinc) {

    mm=(ae-a)/nn;
#if STRIDE > 1
    if (((ae-a)/lda)%STRIDE)
      mm++;
#endif
    
    if (mm == 1)
      Mjoin(dp,EXT1)(a,nn,b,c,STRIDE*zzinc,len);

#if ( NDPM == 2 && STRIDE > 1 ) || NDPM > 2
    else if (mm == 2)
      Mjoin(dp,EXT2)(a,nn,b,c,STRIDE*zzinc,len);
#endif

#if ( NDPM == 3 && STRIDE > 1 ) || NDPM > 3
    else if (mm == 3)
      Mjoin(dp,EXT3)(a,nn,b,c,STRIDE*zzinc,len);
#endif

#if ( NDPM == 4 && STRIDE > 1 ) || NDPM > 4
    else if (mm == 4)
      Mjoin(dp,EXT4)(a,nn,b,c,STRIDE*zzinc,len);
#endif


  }

#endif

}

