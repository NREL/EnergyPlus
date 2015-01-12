#include "atlas_misc.h"
void ATL_USET(const int N, const SCALAR alpha, TYPE *X, const int incX)
{
   int i;
   for (i=0; i < N; i++) X[i] = alpha;
}
