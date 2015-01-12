#include "atlas_misc.h"
TYPE ATL_UDOT(const int N, const TYPE *X, const int incX,
             const TYPE *Y, const int incY)
{
   register TYPE dot=ATL_rzero;
   int i;
   for (i=0; i < N; i++) dot += X[i] * Y[i];
   return(dot);
}
