#include "atlas_misc.h"
void ATL_UCOPY(const int N, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int i;
   for (i=0; i < N; i++, X += incX, Y += incY) *Y = *X;
}
