#include "atlas_misc.h"
#define ATL_NoFakePF
#include "atlas_prefetch.h"

void ATL_UDOT(const int N, const TYPE *X, const int incx,
              const TYPE *Y, const int incy, SCALAR dot)
{
   register TYPE rx, ix, ry, iy, rdot=ATL_rzero, idot=ATL_rzero;
   const int incX=incx+incx, incY=incy+incy;
   const TYPE *stX=X+((N>>1)<<2), *stX0 = X + N + N;
   #ifdef ATL_AltiVec
      int cwrd = ATL_MulBySize(N)>>4;
   #endif

   if (X != stX)
   {
      #ifdef ATL_AltiVec
         if (cwrd >= 64)
         {
            cwrd = (cwrd+31)>>5;
            if (cwrd <= 256) cwrd = ATL_GetCtrl(512, cwrd <= 255 ? cwrd : 0, 0);
            else /* use all pipes */
            {
               cwrd >>= 1;
               cwrd = ATL_GetCtrl(1024, cwrd <= 255 ? cwrd : 0, 0);
               ATL_pfavR(X+128, cwrd, 2);
               ATL_pfavR(Y+128, cwrd, 3);
            }
         }
         else cwrd = ATL_GetCtrl(64, (cwrd+3)>>2, 4);
         ATL_pfavR(X, cwrd, 0);
         ATL_pfavR(Y, cwrd, 1);
      #endif
      do
      {
         ATL_pfl1R(X+48);
         ATL_pfl1R(Y+48);
         rx = *X; ix = X[1];
         ry = *Y; iy = Y[1];
         #ifndef Conj_
            rdot += rx*ry - ix*iy;
            idot += rx*iy + ix*ry;
         #else
            rdot += rx*ry + ix*iy;
            idot += rx*iy - ix*ry;
         #endif
         rx = X[2]; ix = X[3];
         ry = Y[2]; iy = Y[3];
         #ifndef Conj_
            rdot += rx*ry - ix*iy;
            idot += rx*iy + ix*ry;
         #else
            rdot += rx*ry + ix*iy;
            idot += rx*iy - ix*ry;
         #endif
         X += 4;
         Y += 4;
      }
      while (X != stX);
   }
   while (X != stX0)
   {
      rx = *X; ix = X[1];
      ry = *Y; iy = Y[1];
      #ifndef Conj_
         rdot += rx*ry - ix*iy;
         idot += rx*iy + ix*ry;
      #else
         rdot += rx*ry + ix*iy;
         idot += rx*iy - ix*ry;
      #endif
      X += 2; Y += 2;
   }
   dot[0] = rdot;
   dot[1] = idot;
}
