/***************************************
					$Header: /cvsroot/math-atlas/AtlasBase/kernel/CammMaguire/camm_tpipe.h,v 1.2 2003/10/18 18:13:30 yycamm Exp $

					
***************************************/


/* #ifndef CAMM_TPIPE_H */
/* #define CAMM_TPIPE_H */    /*+ To stop multiple inclusions. +*/

#ifndef BITS
#error BITS must be defined in camm_tpipe.h
#endif
#ifndef DIV
#error DIV must be defined in camm_tpipe.h
#endif
#ifndef INC
#error INC(a_) must be defined in camm_tpipe.h
#endif
#ifndef LR
#error LR must be defined in camm_tpipe.h
#endif

#ifdef ALIGN

#if defined(SREAL)

     test(4,ax) 
     je(a2)
      
#undef KB
#define KB ( 1 /* / DIV */ )
#include "camm_pipe3.h"

     KB_block 
     INC(4) 
     sub(1,LR) 

     lab(a2)

#endif

#if defined(SREAL) || defined(DREAL)

     test(8,ax) 
     je(a4) 
     test(-2,LR) 
     je(a4)

#undef KB
#define KB ( 2 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(8) 
     sub(2,LR) 
     
     lab(a4)

#endif
#endif

/*      "movl %%edx,%%edi\n\t"  */
     push(LR)
     shr(BITS,LR) 
     shl(BITS,LR) 
     m(4,LR) 
     ra(ax,LR) 

#if defined(ALIGN) && ( defined(SCPLX) || defined(DCPLX) )
     test(12,ax)
     je(loopa)
#endif

#if !defined(ALIGN) || defined(SCPLX) || defined(DCPLX)
#undef plq
#define plq(a_,b_,c_) pl(a_,b_,c_)
#undef puq
#define puq(a_,b_,c_)  pu(a_,b_,c_)
#undef plqx
#define plqx(a_,b_,c_,d_,e_) plx(a_,b_,c_,d_,e_)
#undef puqx
#define puqx(a_,b_,c_,d_,e_)  pux(a_,b_,c_,d_,e_)
#else
#undef plq
#define plq(a_,b_,c_) pla(a_,b_,c_)
#undef puq
#define puq(a_,b_,c_)  punt(a_,b_,c_)
#undef plqx
#define plqx(a_,b_,c_,d_,e_) plax(a_,b_,c_,d_,e_)
#undef puqx
#define puqx(a_,b_,c_,d_,e_)  puax(a_,b_,c_,d_,e_)
#endif

     align
     lab(loop) 
     cmp(ax,LR) 
     je(stop)

#undef KB
#define KB ( (1 << BITS) /* / DIV */ )
#include "camm_pipe3.h"
     KB_block  
     INC(4*KB/**DIV*/) 

     jmp(loop) 

     lab(stop)
     pop(LR)

#if ( 1 << BITS ) > 128
     test(128,LR) 
     je(64)
#undef KB
#define KB ( 128 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(512) 
     
     lab(64)
#endif

#if ( 1 << BITS ) > 64
     test(64,LR) 
     je(32)
#undef KB
#define KB ( 64 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(256) 

     lab(32)
#endif

#if ( 1 << BITS ) > 32
     test(32,LR) 
     je(16)
#undef KB
#define KB ( 32 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(128) 

     lab(16)
#endif

#if ( 1 << BITS ) > 16
     test(16,LR) 
     je(8)
#undef KB
#define KB ( 16 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(64) 

     lab(8)
#endif

#if ( 1 << BITS ) > 8
     test(8,LR) 
     je(4)
#undef KB
#define KB ( 8 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(32) 

     lab(4)
#endif

#if ( 1 << BITS ) > 4
     test(4,LR) 
     je(2)
#undef KB
#define KB ( 4 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(16) 

     lab(2)
#endif

#if DIV != 4 && ( 1 << BITS ) > 2
     test(2,LR) 
     je(1)
#undef KB
#define KB ( 2 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(8) 

     lab(1)
#endif

#if DIV == 1 && ( 1 << BITS ) > 1
     test(1,LR) 
     je(end)
#undef KB
#define KB ( 1 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     lab(end)
#endif

#if defined (ALIGN) && ( defined(SCPLX) || defined(DCPLX) )

     jmp(tend)

#undef plq
#define plq(a_,b_,c_) pla(a_,b_,c_)
#undef puq
#define puq(a_,b_,c_) punt(a_,b_,c_)
#undef plqx
#define plqx(a_,b_,c_,d_,e_) plax(a_,b_,c_,d_,e_)
#undef puqx
#define puqx(a_,b_,c_,d_,e_)  puax(a_,b_,c_,d_,e_)

     align
     lab(loopa) 
     cmp(ax,LR) 
     je(stopa)

#undef KB
#define KB ( (1 << BITS) /* / DIV */ )
#include "camm_pipe3.h"
     KB_block  
     INC(4*KB/**DIV*/) 

     jmp(loopa) 

     lab(stopa)
     pop(LR)

#if ( 1 << BITS ) > 128
     test(128,LR) 
     je(64a)
#undef KB
#define KB ( 128 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(512) 
     
     lab(64a)
#endif

#if ( 1 << BITS ) > 64
     test(64,LR) 
     je(32a)
#undef KB
#define KB ( 64 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(256) 

     lab(32a)
#endif

#if ( 1 << BITS ) > 32
     test(32,LR) 
     je(16a)
#undef KB
#define KB ( 32 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(128) 

     lab(16a)
#endif

#if ( 1 << BITS ) > 16
     test(16,LR) 
     je(8a)
#undef KB
#define KB ( 16 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(64) 

     lab(8a)
#endif

#if ( 1 << BITS ) > 8
     test(8,LR) 
     je(4a)
#undef KB
#define KB ( 8 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(32) 

     lab(4a)
#endif

#if ( 1 << BITS ) > 4
     test(4,LR) 
     je(2a)
#undef KB
#define KB ( 4 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(16) 

     lab(2a)
#endif

#if DIV != 4 && ( 1 << BITS ) > 2
     test(2,LR) 
     je(1a)
#undef KB
#define KB ( 2 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     INC(8) 

     lab(1a)
#endif

#if DIV == 1 && ( 1 << BITS ) > 1
     test(1,LR) 
     je(enda)
#undef KB
#define KB ( 1 /* / DIV */ )
#include "camm_pipe3.h"
     KB_block 
     lab(enda)
#endif

     lab(tend)

#endif

/* #endif */ /* CAMM_TPIPE_H */
