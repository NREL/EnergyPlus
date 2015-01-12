#include "atlas_asm.h"

#ifndef ATL_SSE2
   #error "This kernel requires SSE2"
#endif
#ifdef ATL_GAS_x8632
   #define FSIZE 4
   #define movq movl
   #define addq addl
   #define subq subl
   #define rsp  esp
   #define rax  eax
   #define N    %edi
   #define X    %edx
   #define Y    %ecx
   #define II   %eax
#else
   #define N    %rdi
   #define X    %rsi
   #define Y    %rcx
   #define II   %rax
#endif

#define rSr     %xmm0
#define rSi     %xmm1
#define rX      %xmm2
#define rY      %xmm3
#define rZ      %xmm4

#ifdef ATL_ARCH_Core2
   #define MOVAPD movaps
   #define MOVUPD movups
#else
   #define MOVAPD movapd
   #define MOVUPD movupd
#endif
#define PREFX   prefetchnta
#define PREFY   PREFX
#ifndef YDIST
   #define YDIST   1152
#endif
#ifndef XDIST
   #define XDIST   YDIST
#endif
/*
                rdi/4              rsi/8          rdx/12
void ATL_UDOT(const int N, const TYPE *X, const int incx,
                     rcx/16           r8/20   r9/24
              const TYPE *Y, const int incy, TYPE *dot)
 */
.text
.global  ATL_asmdecor(ATL_UDOT)
ATL_asmdecor(ATL_UDOT):
#ifdef ATL_GAS_x8632
        subl    $FSIZE, %esp
        movl    %edi, (%esp)
        movl    FSIZE+4(%esp), N
        movl    FSIZE+8(%esp), X
        movl    FSIZE+16(%esp), Y
#endif
        xorpd   rSr, rSr        /* zero running sum for real components */
        xorpd   rSi, rSi        /* zero running sum for imaginary comps */
        mov     N, II                   /* II = N */
        shl     $4, II                  /* II = N * sizeof */
        add     II, X                   /* X += N */
        add     II, Y                   /* Y += N */
        neg     II                      /* II = -N * sizeof */
        test    $0xF, X
        jnz     UNALIGNED
        test    $0xF, Y
        jnz     UNALIGNED

        MOVAPD  (X,II), rX              /* rX = {Xi, Xr} */
        pshufd  $0x4E, rX, rZ           /* rZ = {Xr, Xi} */
        MOVAPD  (Y,II), rY              /* rY = {Yi, Yr} */
        add     $16, II
        jz      LP1DONE
LOOP1:
        mulpd   rY, rX                /* rX = {Xi*Yi, Xr*Yr} */
        addpd   rX, rSr                /* real part of sum */
                MOVAPD  (X,II), rX              /* rX = {Xi, Xr} */
        mulpd   rY, rZ                  /* rZ = {Xr*Yi, Xi*Yr} */
                MOVAPD  (Y,II), rY              /* rY = {Yi, Yr} */
        addpd   rZ, rSi                 /* imag part of sum */
                pshufd  $0x4E, rX, rZ           /* rZ = {Xr, Xi} */
        add     $16, II
        jnz LOOP1
LP1DONE:
        mulpd   rY, rX                  /* rX = {Xi*Yi, Xr*Yr} */
        addpd   rX, rSr                 /* real part of sum */
        mulpd   rY, rZ                  /* rZ = {Xr*Yi, Xi*Yr} */
        addpd   rZ, rSi                 /* imag part of sum */

DONE:
#ifdef ATL_SSE3                         /* rSr= {Xi*Yi, Xr*Yr} */
        hsubpd  rSr, rSr                /* rSr= {XXXXX, Xr*Yr-Xi*Yi} */
                                        /* rSi= {Xr*Yi, Xi*Yr} */
        haddpd  rSi, rSi                /* rSi= {XXXXX, Xi*Yr+Xr*Yi} */
#else
        pshufd  $EE, rSr, rX            /* rX = {XXXXX, Xi*Yi} */
        subps   rX, rSr                 /* rSr= {XXXXX, Xr*Yr-Xi*Yi} */
        pushfd  $EE, rSi, rY            /* rY = {XXXXX, Xr*Yi} */
        addps   rY, rSi                 /* rSi= {XXXXX, Xi*Yr+Xr*Yi} */
#endif
#ifdef ATL_GAS_x8632
        movl    FSIZE+24(%esp), II
        movsd   rSr, (II)
        movsd   rSi, 8(II)
        movl    (%esp), %edi
        addl    $FSIZE, %esp
#else
        movsd  rSr, (%r9)
        movsd  rSi, 8(%r9)
#endif
        ret
/*
 *      Loop for when X or Y is not aligned to 16-byte boundary
 */
UNALIGNED:
        MOVUPD  (X,II), rX              /* rX = {Xi, Xr} */
        pshufd  $0x4E, rX, rZ           /* rZ = {Xr, Xi} */
        MOVUPD  (Y,II), rY              /* rY = {Yi, Yr} */
        add     $16, II
        jz      LPUDONE
ULOOP:
        mulpd   rY, rX                /* rX = {Xi*Yi, Xr*Yr} */
        addpd   rX, rSr                /* real part of sum */
                MOVUPD  (X,II), rX              /* rX = {Xi, Xr} */
        mulpd   rY, rZ                  /* rZ = {Xr*Yi, Xi*Yr} */
                MOVUPD  (Y,II), rY              /* rY = {Yi, Yr} */
        addpd   rZ, rSi                 /* imag part of sum */
                pshufd  $0x4E, rX, rZ           /* rZ = {Xr, Xi} */
        add     $16, II
        jnz ULOOP
LPUDONE:
        mulpd   rY, rX                  /* rX = {Xi*Yi, Xr*Yr} */
        addpd   rX, rSr                 /* real part of sum */
        mulpd   rY, rZ                  /* rZ = {Xr*Yi, Xi*Yr} */
        addpd   rZ, rSi                 /* imag part of sum */
        jmp DONE



