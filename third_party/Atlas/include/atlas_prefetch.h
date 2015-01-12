#ifndef ATLAS_PREFETCH_H
#define ATLAS_PREFETCH_H
/*
 * Altivec prefetch model not well utilized by SSE-like prefetch, so have
 * special commands for it.
 */
#if defined(ATL_AltiVec)
   #include "atlas_altivec.h"
#endif
/*
 *
 * ATL_pfl1R(mem)  : fetch location mem to L1, with intent to read *only*
 * ATL_pfl1W(mem)  : fetch location mem to L1, with intent to read/write
 * ATL_pfl1WO(mem) : fetch location mem to L1, with intent to write ONLY
 */

#if defined(ATL_3DNow)
   #ifdef __GNUC__
      #define ATL_pfl1R(mem) \
         __asm__ __volatile__ ("prefetch %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1W(mem) \
         __asm__ __volatile__ ("prefetchw %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1WO ATL_pfl1W
      #define ATL_GOT_L1PREFETCH
      #ifdef ATL_SSE1
         #define ATL_pfl2R(mem) \
            __asm__ __volatile__ ("prefetcht1 %0" : : "m" (*((char *)(mem))))
         #define ATL_pfl2W(mem) \
            __asm__ __volatile__ ("prefetcht1 %0" : : "m" (*((char *)(mem))))
         #define ATL_pfl2WO ATL_pfl2W
         #define ATL_GOT_L2PREFETCH
      #endif
   #endif
#elif defined(ATL_SSE1) || defined (ATL_SSE2) /* SSE prefetch is available */
   #ifdef __GNUC__
      #define ATL_pfl1R(mem) \
         __asm__ __volatile__ ("prefetchnta %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1W(mem) \
         __asm__ __volatile__ ("prefetchnta %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1WO ATL_pfl1W
      #define ATL_GOT_L1PREFETCH

      #define ATL_pfl2R(mem) \
         __asm__ __volatile__ ("prefetcht1 %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl2W(mem) \
         __asm__ __volatile__ ("prefetcht1 %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl2WO ATL_pfl2W
      #define ATL_GOT_L2PREFETCH
   #endif
#elif defined(__SUNPRO_C) && defined(__sparc) /* && __SUNPRO_CC > 0x600 */
   #include <sun_prefetch.h>
   #define ATL_pfl1R(mem) sparc_prefetch_read_many((void*)(mem))
   #define ATL_pfl1W(mem) sparc_prefetch_write_many((void*)(mem))
   #define ATL_GOT_L1PREFETCH
   #define ATL_pfl2R(mem) sparc_prefetch_read_many((void*)(mem))
   #define ATL_pfl2W(mem) sparc_prefetch_write_many((void*)(mem))
   #define ATL_GOT_L2PREFETCH
#elif defined(ATL_GAS_ARM)
   #define ATL_pfl1R(mem) \
      __asm__ __volatile__ ("pld %0" : : "m" (*((char *)(mem))))
   #define ATL_pfl1W(mem)  /* seems to run slower if you pref writes */
   #define ATL_GOT_L1PREFETCH
#elif defined(ATL_ARCH_21264)
   #ifdef __GNUC__
      #define ATL_pfl1R(mem) \
         __asm__ __volatile__ ("ldt $f31, %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1W(mem) \
         __asm__ __volatile__ ("lds $f31, %0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1WO(mem) \
         __asm__ __volatile__ ("wh64 %0" : : "m" (*((char *)(mem))))
      #define ATL_GOT_L1PREFETCH
   #elif defined(__DECC)
      #include "c_asm.h"
      #define ATL_pfl1R(mem) asm ("ldt %f31,(%a0) ;", mem)
      #define ATL_pfl1W(mem) asm ("lds %f31,(%a0) ;", mem)
      #define ATL_pfl1WO(mem) asm ("wh64 (%a0) ;", mem)
      #define ATL_GOT_L1PREFETCH
   #endif
/*
 * Note: SunUS5/10 seems to get no benefit from prefetch, so don't enable
 */
#elif defined(ATL_ARCH_USIV) || defined(ATL_ARCH_SunUSIII) || \
      defined(ATL_ARCH_SunUSII) || defined(ATL_ARCH_SunUSI) || \
      defined(ATL_ARCH_SunUST1) || defined(ATL_ARCH_SunUST2)
   #ifdef __GNUC__
      #define ATL_pfl1R(mem) \
         __asm__ __volatile__ ("prefetch %0,0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1W(mem) \
         __asm__ __volatile__ ("prefetch %0,2" : : "m" (*((char *)(mem))))
      #define ATL_GOT_L1PREFETCH
      #define ATL_pfl2R(mem) \
         __asm__ __volatile__ ("prefetch %0,3" : : "m" (*((char *)(mem))))
      #define ATL_pfl2W(mem) \
         __asm__ __volatile__ ("prefetch %0,2" : : "m" (*((char *)(mem))))
      #define ATL_GOT_L2PREFETCH
   #endif
/*
 * Gives gigantic slowdown on POWER4, so don't enable there, just use gcc
 * builtin
 */
#elif defined(ATL_GAS_PPC) && !defined(ATL_ARCH_POWER4)
   #if defined(__GNUC__) || defined(__IBM_GCC_ASM)
      #define ATL_pfl1R(mem) \
         __asm__ __volatile__ ("dcbt  0, %0, 0" : : "r" ((mem)))
      #define ATL_pfl1W(mem) \
         __asm__ __volatile__ ("dcbtst  0, %0" : : "r" ((mem)))
      #define ATL_pfST(mem) \
         __asm__ __volatile__ ("dcbt  0, %0, 1" : : "r" ((mem)))
      #define ATL_pfl1STi(mem, str) \
        __asm__ __volatile__ ("rlwinm %0, %0, 0, 0, 24\n\t" \
                              "ori %0, %0, 96+%2\n\t" \
                              "dcbt 0, %0, 8"  \
                              : "=r" (mem) \
                              : "0" (mem), "i" (str))

      #define ATL_GOT_L1PREFETCH
      #define ATL_L1LS 128
   #endif
#elif defined(ATL_ARCH_IA64Itan) || defined(ATL_ARCH_IA64Itan2)
/*
 * Have to use nt2, 'cause fpu ignored L1.
 * NOTE: just let icc to prefetch, keep inst here for reference
 */
   #if defined(__ECC) && 0
      #include "ia64intrin.h"
      #define ATL_pfl1R(mem) __lfetch(2, (mem))
      #define ATL_pfl1W(mem)  __lfetch_excl(2, (mem))
      #define ATL_GOT_L1PREFETCH
   #elif defined(__GNUC__) && !defined(__ECC)
      #define ATL_pfl1R(mem) \
         __asm__ ("    lfetch.nt2  [%0]": : "r"((void *)(mem)))
      #define ATL_pfl1W(mem) \
         __asm__ ("    lfetch.excl     [%0]": : "r"((void *)(mem)))
      #define ATL_GOT_L1PREFETCH
   #endif
#elif defined(ATL_ARCH_HPPA20) && defined(__GNUC__)
      #define ATL_pfl1R(mem) \
         __asm__ __volatile__ ("ldw %0, %%r0" : : "m" (*((char *)(mem))))
      #define ATL_pfl1W(mem) \
         __asm__ __volatile__ ("ldd %0, %%r0" : : "m" (*((char *)(mem))))
      #define ATL_GOT_L1PREFETCH
#elif defined(ATL_AltiVec) && !defined(ATL_pfl1R)
   #ifndef ATL_NoFakePF
      /* 33619968 is ATL_GetCtrl(0, 1, 2), or fetch 1 32-byte block */
      #define ATL_pfl1R(mem) ATL_pfavR(mem, 33619968, 3)
      #define ATL_pfl1W(mem) ATL_pfavW(mem, 33619968, 2)
      #define ATL_GOT_L1PREFETCH
   #endif
#elif defined(ATL_ARCH_MIPSICE9) && (defined(__GNUC__) || defined(__PATHCC__))
   #define ATL_pfl1R(mem) \
      __asm__ __volatile__ ("pref 6,%0" : : "m" (*((char *)(mem))))
   #define ATL_pfl1W(mem) \
      __asm__ __volatile__ ("pref 7,%0" : : "m" (*((char *)(mem))))
   #define ATL_GOT_L1PREFETCH
   #define ATL_L1LS 32
   #define ATL_L2LS 64
#elif defined(ATL_ARCH_IbmZ196) || defined(ATL_ARCH_IbmZ10)
   #define ATL_pfl1R(mem) __builtin_prefetch(mem, 0, 3)
   #define ATL_pfl1W(mem) __builtin_prefetch(mem, 1, 3)
   #define ATL_GOT_L1PREFETCH
   #define ATL_L1LS 256
   #define ATL_L2LS 256
/*
 * As last resort, try using gcc intrinsic, but only if we aren't on a
 * generic architecture that does not possess prefetch
 */
#elif defined(__GNUC__) && !defined(ATL_ARCH_x86x87)
   #define ATL_pfl1R(mem) __builtin_prefetch(mem, 0, 3)
   #define ATL_pfl1W(mem) __builtin_prefetch(mem, 1, 3)
   #define ATL_GOT_L1PREFETCH
#endif
#if defined(ATL_pfl1W) && !defined(ATL_pfl1WO)
   #define ATL_pfl1WO ATL_pfl1W
#endif

#ifdef ATL_NOL1PREFETCH
   #ifdef ATL_GOT_L1PREFETCH
      #undef ATL_pfl1R
      #undef ATL_pfl1W
      #undef ATL_pfl1WO
      #undef ATL_GOT_L1PREFETCH
   #endif
#endif
#ifdef ATL_NOL2PREFETCH
   #ifdef ATL_GOT_L2PREFETCH
      #undef ATL_pfl2R
      #undef ATL_pfl2W
      #undef ATL_pfl2WO
      #undef ATL_GOT_L2PREFETCH
   #endif
#endif
#ifndef ATL_GOT_L1PREFETCH  /* dummy calls cpp takes out of code */
   #define ATL_pfl1R(mem)
   #define ATL_pfl1W(mem)
   #define ATL_pfl1WO(mem)
#endif
#ifndef ATL_GOT_L2PREFETCH  /* dummy calls cpp takes out of code */
   #define ATL_pfl2R(mem)
   #define ATL_pfl2W(mem)
#endif

/*
 * Define Cache line sizes for L1 and L2
 */
#ifndef ATL_L1LS
   #define ATL_L1LS 64
#endif
#ifndef ATL_L2LS
   #define ATL_L2LS ATL_L1LS
#endif

#endif
