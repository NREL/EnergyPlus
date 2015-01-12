#include "atlas_misc.h"
#include <math.h>
TYPE ATL_UNRM2(const int N, const TYPE *X, const int incX)
/*
 * Only machines like x86 with extended precision (both arithmetic and sqrt)
 * will be able to use this kernel.  On machines with standard 64/32 bit
 * precision, this will fail the overflow/underflow tests.
 */
{
   int i;
   #if defined(SREAL) || defined(SCPLX) || defined(ATL_OS_WinNT) || \
       defined(ATL_OS_Win64)
      double t0=ATL_rzero;
   #else
      #define sqrt sqrtl
      long double t0=ATL_rzero;
   #endif
   for (i=0; i < N; i++) t0 += X[i]*X[i];
   t0 = sqrt(t0);
   return(t0);
}
