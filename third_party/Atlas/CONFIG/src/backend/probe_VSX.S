#include "atlas_asm.h"
/*

 * void do_vsum(z, x, y)
 * RETURNS: z = x + y
 *
 */
.text
#if defined(ATL_USE64BITS) && defined (ATL_OS_Linux) && _CALL_ELF != 2
        .align 2
        .globl  ATL_asmdecor(do_vsum)
        .section        ".opd","aw"
        .align 3
#else
.globl ATL_asmdecor(do_vsum)
#endif
ATL_asmdecor(do_vsum):
        lxvd2x  v0,0,r5
        lxvd2x  v1,0,r4
        xvadddp v3,v0,v1
        stxvd2x  v3,0,r3
        blr
