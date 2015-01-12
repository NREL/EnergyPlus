9
1 2 0 0 axpy1_x0y0.c       "R. Clint Whaley"
2 2 1 1 axpy1_x1y1.c       "R. Clint Whaley"
3 2 1 1 axpy32_x1y1.c      "R. Clint Whaley"
4 2 1 1 axpy32p32_x1y1.c   "R. Clint Whaley"
5 2 1 1 axpy8p8m0_x1y1.c   "R. Clint Whaley"
6 2 1 1 axpy16p4x16_x1y1.c "R. Clint Whaley"
7 2 1 1 axpy16p4m0_x1y1.c  "R. Clint Whaley"
8 2 1 1 axpy4p40_x1y1.c    "R. Clint Whaley"
9 2 1 1 daxpy_sse2.c        "R. Clint Whaley" \
gcc
-x assembler-with-cpp

<ID> <alpha> <incX> <incY> <rout> <author> [\
 <CC>
  <CCFLAGS>]

  ID : unique num > 0
  <alpha> valid values are 1, -1; all others are X.

<ID> <alpha> <incX> <incY> <rout> <author> [\
 <CC>
  <CCFLAGS>]

  ID : unique num > 0
  <alpha> valid values are 1, -1; all others are X.
  <incX> : 0 - any inc, all other is fixed incX
  <incY> : 0 - any inc, all other is fixed incY

          name key: axpy<unroll>p<prefetch>m<muladd>_x<incX>y<incY>.c
          if p is not there, no prefetch
          if m not there, muladd=1 (combined multiply/add instruction
