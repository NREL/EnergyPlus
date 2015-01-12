#include "atlas_misc.h"

static TYPE *axpy8(const int N, const SCALAR alpha0, const TYPE *X, TYPE *Y)
/*
 * Uses 8-register prefetch along X and Y, 4 length pipeline for seperate
 * multiply and add
 */
{
   int i;
   const register TYPE alpha=alpha0;
   const TYPE *stX = X + N;
   register TYPE m0, m1, m2, m3, a0, a1, a2, a3;
   register TYPE x0, x1, x2, x3, x4, x5, x6, x7;
   register TYPE y0, y1, y2, y3, y4, y5, y6, y7;

   x0 = *X;   x1 = X[1]; x2 = X[2]; x3 = X[3];
   x4 = X[4]; x5 = X[5]; x6 = X[6]; x7 = X[7]; X += 8;
   y0 = *Y;   y1 = Y[1]; y2 = Y[2]; y3 = Y[3];
   y4 = Y[4]; y5 = Y[5]; y6 = Y[6]; y7 = Y[7];
   m0 = x0 * alpha; x0 = *X;
   m1 = x1 * alpha; x1 = X[1];
   m2 = x2 * alpha; x2 = X[2];
   m3 = x3 * alpha; x3 = X[3];
   a0 = y0 + m0; y0 = Y[ 8]; m0 = x4 * alpha; x4 = X[4];
   a1 = y1 + m1; y1 = Y[ 9]; m1 = x5 * alpha; x5 = X[5];
   a2 = y2 + m2; y2 = Y[10]; m2 = x6 * alpha; x6 = X[6];
   a3 = y3 + m3; y3 = Y[11]; m3 = x7 * alpha; x7 = X[7]; X += 8;
   do
   {
      *Y   = a0; a0 = y4 + m0; y4 = Y[12]; m0 = x0 * alpha; x0 = *X;
      Y[1] = a1; a1 = y5 + m1; y5 = Y[13]; m1 = x1 * alpha; x1 = X[1];
      Y[2] = a2; a2 = y6 + m2; y6 = Y[14]; m2 = x2 * alpha; x2 = X[2];
      Y[3] = a3; a3 = y7 + m3; y7 = Y[15]; m3 = x3 * alpha; x3 = X[3];
      Y[4] = a0; a0 = y0 + m0; y0 = Y[16]; m0 = x4 * alpha; x4 = X[4];
      Y[5] = a1; a1 = y1 + m1; y1 = Y[17]; m1 = x5 * alpha; x5 = X[5];
      Y[6] = a2; a2 = y2 + m2; y2 = Y[18]; m2 = x6 * alpha; x6 = X[6];
      Y[7] = a3; a3 = y3 + m3; y3 = Y[19]; m3 = x7 * alpha; x7 = X[7]; X += 8;
      Y += 8;
   }
   while (X != stX);
   *Y   = a0; a0 = y4 + m0; y4 = Y[12]; m0 = x0 * alpha;
   Y[1] = a1; a1 = y5 + m1; y5 = Y[13]; m1 = x1 * alpha;
   Y[2] = a2; a2 = y6 + m2; y6 = Y[14]; m2 = x2 * alpha;
   Y[3] = a3; a3 = y7 + m3; y7 = Y[15]; m3 = x3 * alpha;
   Y[4] = a0; a0 = y0 + m0; m0 = x4 * alpha;
   Y[5] = a1; a1 = y1 + m1; m1 = x5 * alpha;
   Y[6] = a2; a2 = y2 + m2; m2 = x6 * alpha;
   Y[7] = a3; a3 = y3 + m3; m3 = x7 * alpha; Y += 8;

   *Y   = a0; a0 = y4 + m0;
   Y[1] = a1; a1 = y5 + m1;
   Y[2] = a2; a2 = y6 + m2;
   Y[3] = a3; a3 = y7 + m3;
   Y[4] = a0;
   Y[5] = a1;
   Y[6] = a2;
   Y[7] = a3;
   return(Y+8);
}

void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   TYPE *y=Y, *stY = Y+N;
   if (N >= 24) y = axpy8((N>>3)<<3, alpha, X, Y);
   if (y != stY)
   {
      X += (y-Y);
      do *y++ += *X++ * alpha; while (y != stY);
   }
}
