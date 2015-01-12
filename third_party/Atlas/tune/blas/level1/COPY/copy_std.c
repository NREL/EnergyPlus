#include "atlas_misc.h"
#include <string.h>
void ATL_UCOPY(const int N, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   memcpy(Y, X, ATL_MulBySize((size_t)N));
}
