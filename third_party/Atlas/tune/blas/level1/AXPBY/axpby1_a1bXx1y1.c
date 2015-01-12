#include "atlas_misc.h"
void ATL_UAXPBY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
                const SCALAR beta, TYPE *Y, const int incY)
{
   int i;
   for (i=0; i < N; i++) Y[i] = X[i] + beta * Y[i];
}
