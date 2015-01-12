7
1 2 0 0 caxpy1_x0y0.c       "R. Clint Whaley"
2 2 1 1 caxpy1_x1y1.c       "R. Clint Whaley"
3 2 1 1 caxpy8p1_x1y1.c     "R. Clint Whaley"
4 2 1 1 caxpy8p4m0_x1y1.c   "R. Clint Whaley"
5 0 1 1 caxpy1_a0x0y0.c     "R. Clint Whaley"
6 2 1 1 caxpy2p32_x1y1.c    "R. Clint Whaley"
7 2 1 1 caxpy_sse3.c        "R. Clint Whaley" \
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
  <alpha> valid values are 1, -1, and 0 (means imag component 0, real X);
          all others are X.
  <incX> : 0 - any inc, all other is fixed incX
  <incY> : 0 - any inc, all other is fixed incY

          name key: axpy<unroll>p<prefetch>m<muladd>_x<incX>y<incY>.c
          if p is not there, no prefetch
          if m not there, muladd=1 (combined multiply/add instruction
