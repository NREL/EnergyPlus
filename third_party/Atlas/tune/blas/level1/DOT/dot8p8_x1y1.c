#include "atlas_misc.h"
TYPE ATL_UDOT(const int N, const TYPE *X, const int incX,
             const TYPE *Y, const int incY)
{
   int nr;
   const TYPE *stX, *stX0 = X + N;
   register TYPE m0, m1, m2, m3;
   register TYPE dot0=ATL_rzero, dot1=ATL_rzero, dot2=ATL_rzero, dot3=ATL_rzero;
   register TYPE x0, x1, x2, x3, x4, x5, x6, x7, x8;
   register TYPE y0, y1, y2, y3, y4, y5, y6, y7, y8;

   if (N >= 20)
   {
      nr = N - 12;
      stX = X + 12 + ((nr>>3)<<3);
      x0 =   *X; x1 = X[1]; x2 = X[2]; x3 = X[3];
      y0 =   *Y; y1 = Y[1]; y2 = Y[2]; y3 = Y[3];
      x4 = X[4]; x5 = X[5]; x6 = X[6]; x7 = X[7];
      y4 = Y[4]; y5 = Y[5]; y6 = Y[6]; y7 = Y[7];
      m0 = x0 * y0; x0 = X[ 8]; y0 = Y[ 8];
      m1 = x1 * y1; x1 = X[ 9]; y1 = Y[ 9];
      m2 = x2 * y2; x2 = X[10]; y2 = Y[10];
      m3 = x3 * y3; x3 = X[11]; y3 = Y[11]; X += 12; Y += 12;
      do
      {
         dot0 += m0; m0 = x4 * y4; x4 =   *X; y4 =   *Y;
         dot1 += m1; m1 = x5 * y5; x5 = X[1]; y5 = Y[1];
         dot2 += m2; m2 = x6 * y6; x6 = X[2]; y6 = Y[2];
         dot3 += m3; m3 = x7 * y7; x7 = X[3]; y7 = Y[3];

         dot0 += m0; m0 = x0 * y0; x0 = X[4]; y0 = Y[4];
         dot1 += m1; m1 = x1 * y1; x1 = X[5]; y1 = Y[5];
         dot2 += m2; m2 = x2 * y2; x2 = X[6]; y2 = Y[6];
         dot3 += m3; m3 = x3 * y3; x3 = X[7]; y3 = Y[7]; X += 8; Y += 8;
      }
      while (X != stX);
      dot0 += m0; m0 = x4 * y4; x4 =   *X; y4 =   *Y;
      dot1 += m1; m1 = x5 * y5; x5 = X[1]; y5 = Y[1];
      dot2 += m2; m2 = x6 * y6; x6 = X[2]; y6 = Y[2];
      dot3 += m3; m3 = x7 * y7; x7 = X[3]; y7 = Y[3];

      dot0 += m0; m0 = x0 * y0;
      dot1 += m1; m1 = x1 * y1;
      dot2 += m2; m2 = x2 * y2;
      dot3 += m3; m3 = x3 * y3;

      dot0 += m0;
      dot1 += m1;
      dot2 += m2;
      dot3 += m3;

      dot0 += dot1;
      dot2 += dot3;

      dot0 += dot2;
   }
   if (X != stX0)
   {
      do
      {
         dot0 += *X++ * *Y++;
      }
      while(X != stX0);
   }
   return(dot0);
}
