#include "atlas_asm.h"
.text
#ifdef ATL_AS_OSX_PPC
   .globl _ATL_ResetAtomicCount
   _ATL_ResetAtomicCount:
#else
   #if defined(ATL_USE64BITS) && _CALL_ELF != 2
/*
 *      Official Program Descripter section, seg fault w/o it on Linux/PPC64
 */
        .section        ".opd","aw"
        .align 2
	.globl  ATL_USERMM
        .align  3
ATL_ResetAtomicCount:
        .quad   Mjoin(.,ATL_ResetAtomicCount),.TOC.@tocbase,0
        .previous
        .type   Mjoin(.,ATL_ResetAtomicCount),@function
        .text
	.globl  Mjoin(.,ATL_ResetAtomicCount)
.ATL_ResetAtomicCount:
   #else
	.globl  ATL_ResetAtomicCount
ATL_ResetAtomicCount:
   #endif
#endif
/* r3                                 r3       r4 */
/* int int ATL_ResetAtomicCount(void *vp, int cnt) */
RETRY:
   lwarx r5, 0, r3    /* Read int from mem, place reservation */
   stwcx. r4, 0, r3   /* attempt to store new value back to mem */
   bne-  RETRY        /* If store failed, retry */
   mr r3, r5
   blr
