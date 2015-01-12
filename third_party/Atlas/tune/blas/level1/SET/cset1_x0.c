#include "atlas_misc.h"
void ATL_USET(const int N, const SCALAR alpha, TYPE *X, const int incx)
{
   int i;
   const register TYPE ra=(*alpha), ia=alpha[1];
   const int incX=incx+incx;

   for (i=N; i; i--, X += incX)
   {
      *X = ra;
      X[1] = ia;
   }
}
