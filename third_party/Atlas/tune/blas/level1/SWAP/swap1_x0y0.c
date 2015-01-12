#include "atlas_misc.h"
void ATL_USWAP(const int N, TYPE *X, const int incX, TYPE *Y, const int incY)
{
   int i;
   TYPE tmp;
   for (i=N; i; i--, X += incX, Y += incY)
   {
      tmp = *Y;
      *Y = *X;
      *X = tmp;
   }
}
