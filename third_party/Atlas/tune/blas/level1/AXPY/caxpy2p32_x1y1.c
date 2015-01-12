#include "atlas_misc.h"
#include "atlas_prefetch.h"

void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   int i;
   const register TYPE ra=(*alpha), ia=alpha[1];
   register TYPE rx, ix;
   const TYPE *stX, *stX0=X+(N<<1);

   stX = X + ((N>>1)<<2);
   if (X != stX)
   {
      do
      {
         rx = X[0]; ix = X[1];
         ATL_pfl1W(Y+64);
         ATL_pfl1R(X+64);
         *Y   += ra*rx - ia*ix;
         Y[1] += ra*ix + ia*rx;
         rx = X[2]; ix = X[3];
         Y[2] += ra*rx - ia*ix;
         Y[3] += ra*ix + ia*rx;
         X += 4; Y += 4;
      }
      while (X != stX);
   }
   while (X != stX0)
   {
      rx = X[0]; ix = X[1];
      *Y   += ra*rx - ia*ix; X += 2;
      Y[1] += ra*ix + ia*rx; Y += 2;
   }
}
