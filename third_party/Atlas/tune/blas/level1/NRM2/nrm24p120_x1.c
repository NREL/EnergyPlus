#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
/*
 * Only machines like x86 with extended precision (both arithmetic and sqrt)
 * will be able to use this kernel.  On machines with standard 64/32 bit
 * precision, this will fail the overflow/underflow tests.
 */
{
   int n;
   #if defined(SREAL) || defined(SCPLX) || defined(ATL_OS_WinNT) || \
       defined(ATL_OS_Win64)
      register double t0=0.0, t1=0.0, t2=0.0, t3=0.0;
   #else
      #define sqrt sqrtl
      register long double t0=0.0, t1=0.0, t2=0.0, t3=0.0;
   #endif
   const TYPE *stX, *stX0 = X+N;

   n = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(4));
   if (n)  /* not aligned */
   {
      stX = X + n;
      do t0 += *X * *X; while(++X != stX);
   }
   n = N - n;

   stX = X + ((n>>2)<<2);
   if (X != stX)
   {
      do
      {
          ATL_pfl1R(X+120);
          t0 += *X   * *X;
          t1 += X[1] * X[1];
          t2 += X[2] * X[2];
          t3 += X[3] * X[3];
          X += 4;
      }
      while (X != stX);
      t0 += t1;
      t2 += t3;
      t0 += t2;
   }
   if (X != stX0)
   {
      do t0 += *X * *X; while(++X != stX0);
   }
   t0 = sqrt(t0);
   return(t0);
}
