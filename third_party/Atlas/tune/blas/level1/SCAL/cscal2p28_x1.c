#include "atlas_misc.h"
#include "atlas_prefetch.h"

void ATL_USCAL(const int N, const SCALAR alpha, TYPE *X, const int incX)
{
   int n;
   const int incx = incX+incX;
   const register TYPE ra=(*alpha), ia=alpha[1];
   register TYPE rx, ix, rx1, ix1;
   TYPE *stX, *stX0 = X + N + N;

   n = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(2));
   if (n==1)  /* not aligned */
   {
      rx = *X; ix = X[1];
      *X   = rx*ra - ix*ia;
      X[1] = rx*ia + ix*ra;
      X += 2;
      n = N - 1;
   }
   else n = N;

   stX = X + ((n>>1)<<2);
   if (X != stX)
   {
      do
      {
         ATL_pfl1W(X+56);
         rx = *X; ix = X[1];
         rx1 = X[2]; ix1 = X[3];
         *X   = rx*ra - ix*ia;
         X[1] = rx*ia + ix*ra;
         X[2] = rx1*ra - ix1*ia;
         X[3] = rx1*ia + ix1*ra;
         X += 4;
      }
      while(X != stX);
   }
   if (X != stX0)
   {
      rx = *X; ix = X[1];
      *X   = rx*ra - ix*ia;
      X[1] = rx*ia + ix*ra;
   }
}
