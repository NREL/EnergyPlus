#include "atlas_misc.h"
void ATL_UCPSC(const int N, const SCALAR alpha, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int i;
   for (i=0; i < N; i++) Y[i] = alpha * X[i];
}
