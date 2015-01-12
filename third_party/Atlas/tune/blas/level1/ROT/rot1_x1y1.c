#include "atlas_misc.h"
void ATL_UROT(const int N, TYPE *X, const int incX, TYPE *Y, const int incY,
              const TYPE c, const TYPE s)
/*
 * rot, no unrolling, incX=incY=1, arbitrary S & C
 */
{
   int i;
   TYPE tmp;

   for (i=0; i < N; i++)
   {
      tmp = c * X[i] + s * Y[i];
      Y[i] = c * Y[i] - s * X[i];
      X[i] = tmp;
   }
}
