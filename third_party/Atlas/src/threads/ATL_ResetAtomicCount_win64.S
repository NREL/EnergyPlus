#include "atlas_asm.h"
/*
 * rax                       rcx         rdx
 * int ATL_ResetAtomicCount(void *vp, int cnt)
 * Sets vp's acnt=cnt.
 * RETURNS: acnt before the reset
 */
.text
.global ATL_asmdecor(ATL_ResetAtomicCount)
ATL_asmdecor(ATL_ResetAtomicCount):
   sub $-128, %rcx            /* skip false sharing guard zone */
   ATOMIC_LOOP:
      movl (%rcx), %eax       /* read acnt from memory */
      lock                    /* make cmpxchg atomic */
      cmpxchg %edx, (%rcx)   /* put cnt in mem if mem still == acnt in eax */
      je DONE                 /* ZF set if cmpxchg wrote to mem */
   jmp ATOMIC_LOOP            /* ZF=0 means cmpxch failed, try again */
DONE:
   ret
