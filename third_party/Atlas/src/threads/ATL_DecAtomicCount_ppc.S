#include "atlas_asm.h"
.text
#ifdef ATL_AS_OSX_PPC
   .globl _ATL_DecAtomicCount
   _ATL_DecAtomicCount:
#else
   #if defined(ATL_USE64BITS) && _CALL_ELF != 2
/*
 *      Official Program Descripter section, seg fault w/o it on Linux/PPC64
 */
        .section        ".opd","aw"
        .align 2
	.globl  ATL_USERMM
        .align  3
ATL_DecAtomicCount:
        .quad   Mjoin(.,ATL_DecAtomicCount),.TOC.@tocbase,0
        .previous
        .type   Mjoin(.,ATL_DecAtomicCount),@function
        .text
	.globl  Mjoin(.,ATL_DecAtomicCount)
.ATL_DecAtomicCount:
   #else
	.globl  ATL_DecAtomicCount
ATL_DecAtomicCount:
   #endif
#endif
#error "Code is not reliable on PPC, don't know why"
/* r3                           r3  */
/* int ATL_DecAtomicCount(void *vp) */
RETRY:
   lwarx r5, 0, r3    /* Read int from mem, place reservation */
   addi  r5, r5, -1   /* decrement value */
   stwcx. r5, 0, r3   /* attempt to store decremented value back to mem */
   bne-  RETRY        /* If store failed, retry */
   mr r3, r5
   blr
