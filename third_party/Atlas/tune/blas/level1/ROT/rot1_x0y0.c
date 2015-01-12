#include "atlas_misc.h"
void ATL_UROT(const int N, TYPE *X, const int incX, TYPE *Y, const int incY,
              const TYPE c, const TYPE s)
/*
 * rot, no unrolling, arbitrary incX, incY, S & C
 */
{
   int i;
   TYPE tmp;

   for (i=N; i; i--, Y += incY, X += incX)
   {
      tmp = c * *X + s * *Y;
      *Y = c * *Y - s * *X;
      *X = tmp;
   }
}
