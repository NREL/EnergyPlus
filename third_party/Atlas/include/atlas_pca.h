#ifndef ATLAS_PCA_H
   #define ATLAS_PCA_H
/*
 * PowerPCs, POWERs and ARMs are weakly ordered, meaning that a given
 * processor's writes  may appear out-of-order to other processors,
 * which breaks PCA's syncs since PCA depends on in-order writes.
 * To fix, we must issue a memory barrier call before giving the go-ahead.
 * PowerPC: SYNC ensures that all prior stores complete before the next one.
 * POWER: DCS waits until all pending writes are written before preceeding
 * ARM: DMB (data mem barrier) - all prior mem accesses (in program order)
 *      complete before DMB returns
 *
 * Older x86's have a special mode where stores can become out-of-order, but
 * it was rarely enabled and does not seem to exist on modern hardware, so
 * we don't have to bother there.
 *
 * SPARCs do not change the order of stores.
 *
 * PowerPC and ARM syncs do not fix problem, so don't allow PCA on machines
 * with out-of-order write schemes.
 */
#if defined(ATL_ARCH_PPCG4) || defined(ATL_ARCH_PPCG5)
   #ifdef __GNUC__
      #define ATL_membarrier __asm__ __volatile__ ("sync")
/*      #define ATL_USEPCA 1 */
   #endif
#elif defined(ATL_ARCH_POWER3) || defined(ATL_ARCH_POWER4) || \
      defined(ATL_ARCH_POWER5) || defined(ATL_ARCH_POWER6) || \
      defined(ATL_ARCH_POWER7)
   #ifdef __GNUC__
      #define ATL_membarrier __asm__ __volatile__ ("dcs")
/*      #define ATL_USEPCA 1 */
   #endif
/*
 * Unfortunately, none of the memory fence instructions seems to work
 * adequately on ARM
 */
#elif defined(ATL_ARCH_ARMv7)
   #ifdef __GNUC__
      #define ATL_membarrier __asm__ __volatile__ ("dmb")
/*      #define ATL_USEPCA 1 */
   #endif
#elif defined(ATL_ARCH_IA64Itan) || defined(ATL_ARCH_IA64Itan2)
   #ifdef __GNUC__
      #define ATL_membarrier __asm__ __volatile__ ("mf")
/*      #define ATL_USEPCA 1 */
   #endif
#else
   #define ATL_membarrier
   #define ATL_USEPCA 1
#endif

#endif
