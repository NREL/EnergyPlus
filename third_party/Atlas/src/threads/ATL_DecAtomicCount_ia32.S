#include "atlas_asm.h"
/* rax                  %rdi/rcx/4  */
/* int ATL_DecAtomicCount(void *vp) */
.text
.global ATL_asmdecor(ATL_DecAtomicCount)
ATL_asmdecor(ATL_DecAtomicCount):
   movl 4(%esp), %edx
   sub $-128, %edx            /* skip false sharing guard zone */
   ATOMIC_LOOP:
      movl (%edx), %eax       /* read cnt from memory */
      movl %eax, %ecx         /* ecx = cnt */
      subl $1, %ecx           /* ecx = cnt-1 */
      jl ZERO_RET             /* return 0 if count already below 1 */
      lock                    /* make cmpxchg atomic */
      cmpxchg %ecx, (%edx)    /* put cnt-1 in mem if mem still == cnt in eax */
      je DONE                 /* ZF set if cmpxchg wrote to mem */
   jmp ATOMIC_LOOP            /* ZF=0 means cmpxch failed, try again */

ZERO_RET:
   xor %eax, %eax
DONE:
   ret
