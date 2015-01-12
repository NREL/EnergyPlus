#include "atlas_misc.h"
void ATL_USCAL(const int N, const SCALAR alpha, TYPE *X, const int incX)
{
   int i;
   const int incx = incX+incX;
   const register TYPE ra=(*alpha), ia=alpha[1];
   register TYPE rx, ix;
   for (i=N; i; i--, X += incx)
   {
      rx = *X; ix = X[1];
      *X   = rx*ra - ix*ia;
      X[1] = rx*ia + ix*ra;
   }
}
