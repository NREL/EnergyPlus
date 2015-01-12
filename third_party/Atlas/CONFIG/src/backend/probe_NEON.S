#include "atlas_asm.h"
#
# ARM NEON assembler for:
# void do_vsum(float* z,float* x,float* y)
#  where x, y, and z are vectors of length 4
# RETURNS: z = x + y;
#
.code   32
.fpu    neon
.text
.align 2
.globl  ATL_asmdecor(do_vsum)
.type   ATL_asmdecor(do_vsum), %function
ATL_asmdecor(do_vsum):
   vldm     r1,     {d0-d1}
   vldm     r2,     {d2-d3}
   vadd.f32 d0,     d0,      d2
   vadd.f32 d1,     d1,      d3
   vstm     r0,     {d0-d1}
   bx lr
.size ATL_asmdecor(do_vsum),.-ATL_asmdecor(do_vsum)
