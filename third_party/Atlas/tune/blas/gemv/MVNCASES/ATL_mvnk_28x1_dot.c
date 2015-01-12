/*
 * This kernel does GEMV by performing 28 simultaneous dot-products along
 * the rows of the no-transpose matrix A.  It assumes M is a multiple of 28.
 */
#include "atlas_asm.h"
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
/*
 * Integer register usage
 */
#define MM      %rdi
#define NN      %rsi
#define pA0     %rdx
#define pA      %rax
#define lda     %rcx
#define pX      %r8
#define pY      %r9
#define incAm   %rbp
#define N0      %r11
/*
 * Floating point vector register usage
 */
#define rX0     %xmm0
#define rA0     %xmm1
#define rY0     %xmm2
#define rY2     %xmm3
#define rY4     %xmm4
#define rY6     %xmm5
#define rY8     %xmm6
#define rY10    %xmm7
#define rY12    %xmm8
#define rY14    %xmm9
#define rY16    %xmm10
#define rY18    %xmm11
#define rY20    %xmm12
#define rY22    %xmm13
#define rY24    %xmm14
#define rY26    %xmm15

/*
 *
 *                      rdi         rsi            rdx           rcx
 *void ATL_UGEMV(ATL_CINT M, ATL_CINT N, const TYPE *A, ATL_CINT lda,
 *               const TYPE *X, TYPE *Y)
 *                        r8       r9
 *
 *  y = [0,1]*y + A*x, A is MxN,  len(X) = N, len(Y) = M
 */
#ifdef BETA0
   #define AddYtoDot(addr_, reg_)
#else
   #define AddYtoDot(addr_, reg_) addpd addr_, reg_
#endif
#ifdef ATL_3DNow
   #define prefY(addr_) prefetchw addr_
#else
   #define prefY(addr_) prefetcht0 addr_
#endif
.text
.global ATL_asmdecor(ATL_UGEMV)
ALIGN64
ATL_asmdecor(ATL_UGEMV):
   movq %rbp, -8(%rsp)

   neg NN                  /* make N negative for update X */
   mov NN, N0              /* backup of N so we can restore at end of M loop */
   mov $28*8, incAm
   sub $-128, pA0          /* pA0 += 128 bytes */
   sub $-128, pY           /* pY  += 128 bytes */
   shl $3, lda             /* lda *= sizeof */
   mov pA0, pA
   MLOOP:
      #ifdef ATL_SSE3
         movddup (pX), rX0
      #else
         movlpd  (pX), rX0
         unpcklpd rX0, rX0
      #endif
      add $8, pX
      movapd -128(pA0), rY0
      mulpd   rX0, rY0
      movapd -112(pA0), rY2
      mulpd   rX0, rY2
      movapd  -96(pA0), rY4
      mulpd   rX0, rY4
      movapd  -80(pA0), rY6
      mulpd   rX0, rY6
      movapd  -64(pA0), rY8
      mulpd   rX0, rY8
      movapd  -48(pA0), rY10
      mulpd   rX0, rY10
      movapd  -32(pA0), rY12
      mulpd   rX0, rY12
      movapd  -16(pA0), rY14
      mulpd   rX0, rY14
      movapd     (pA0), rY16
      mulpd   rX0, rY16
      movapd   16(pA0), rY18
      mulpd   rX0, rY18
      movapd   32(pA0), rY20
      mulpd   rX0, rY20
      movapd   48(pA0), rY22
      mulpd   rX0, rY22
      movapd   64(pA0), rY24
      mulpd   rX0, rY24
      movapd   80(pA0), rY26
      mulpd   rX0, rY26
      add lda, pA0
      add $1, NN
      jz DONENLOOP
      NLOOP:
         #ifdef ATL_SSE3
            movddup (pX), rX0
         #else
            movlpd  (pX), rX0
            unpcklpd rX0, rX0
         #endif
         add $8, pX   /* pX++ */

         movapd -128(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY0
         movapd -112(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY2
         movapd -96(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY4
         movapd -80(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY6
         movapd -64(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY8
         movapd -48(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY10
         movapd -32(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY12
         movapd -16(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY14
         movapd (pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY16
         movapd 16(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY18
         movapd 32(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY20
         movapd 48(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY22
         movapd 64(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY24
         movapd 80(pA0), rA0
         mulpd  rX0, rA0
         addpd  rA0, rY26
         add lda, pA0
      add $1, NN
      jnz NLOOP
      DONENLOOP:
      AddYtoDot(-128(pY), rY0)
      movapd rY0, -128(pY)
      AddYtoDot(-112(pY), rY2)
      movapd rY2, -112(pY)
      AddYtoDot(-96(pY), rY4)
      movapd rY4, -96(pY)
      AddYtoDot(-80(pY), rY6)
      movapd rY6, -80(pY)
      AddYtoDot(-64(pY), rY8)
      movapd rY8, -64(pY)
      AddYtoDot(-48(pY), rY10)
      movapd rY10, -48(pY)
      AddYtoDot(-32(pY), rY12)
      movapd rY12, -32(pY)
      AddYtoDot(-16(pY), rY14)
      movapd rY14, -16(pY)
      AddYtoDot((pY), rY16)
      movapd rY16, (pY)
      AddYtoDot(16(pY), rY18)
      movapd rY18, 16(pY)
      AddYtoDot(32(pY), rY20)
      movapd rY20, 32(pY)
      AddYtoDot(48(pY), rY22)
      movapd rY22, 48(pY)
      AddYtoDot(64(pY), rY24)
      movapd rY24, 64(pY)
      AddYtoDot(80(pY), rY26)
      movapd rY26, 80(pY)

      lea (pX, N0, 8), pX   /* pX -= N*sizeof */
      prefY(-80(pY,incAm))
      mov N0, NN
      prefY(-16(pY,incAm))
      add incAm, pA
      prefY(48(pY,incAm))
      mov pA, pA0
      prefY(112(pY,incAm))
      add incAm, pY
   sub $28, MM
   jnz MLOOP

   movq -8(%rsp), %rbp
   ret
