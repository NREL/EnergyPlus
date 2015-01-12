#include "atlas_misc.h"
static void axpyCU(const int N, const SCALAR alpha0, const TYPE *X, TYPE *Y)
{
   const TYPE *stX;
   int nr = N;
   register TYPE alpha=alpha0;

   if (nr >= 16)
   {
      *Y   += alpha * *X;
      Y[1] += alpha * X[1];
      Y[2] += alpha * X[2];
      Y[3] += alpha * X[3];
      Y[4] += alpha * X[4];
      Y[5] += alpha * X[5];
      Y[6] += alpha * X[6];
      Y[7] += alpha * X[7];
      Y[8] += alpha * X[8];
      Y[9] += alpha * X[9];
      Y[10] += alpha * X[10];
      Y[11] += alpha * X[11];
      Y[12] += alpha * X[12];
      Y[13] += alpha * X[13];
      Y[14] += alpha * X[14];
      Y[15] += alpha * X[15];
      X += 16;
      Y += 16;
      nr -= 16;
   }
   if (nr >= 8)
   {
      *Y   += alpha * *X;
      Y[1] += alpha * X[1];
      Y[2] += alpha * X[2];
      Y[3] += alpha * X[3];
      Y[4] += alpha * X[4];
      Y[5] += alpha * X[5];
      Y[6] += alpha * X[6];
      Y[7] += alpha * X[7];
      X += 8;
      Y += 8;
      nr -= 8;
   }
   if (nr >= 4)
   {
      *Y   += alpha * *X;
      Y[1] += alpha * X[1];
      Y[2] += alpha * X[2];
      Y[3] += alpha * X[3];
      X += 4;
      Y += 4;
      nr -= 4;
   }
   if (nr >= 2)
   {
      *Y   += alpha * *X;
      Y[1] += alpha * X[1];
      X += 2;
      Y += 2;
      nr -= 2;
   }
   if (nr >= 1)
   {
      *Y   += alpha * *X;
   }
}
#include "atlas_misc.h"
void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *x, const int incX,
               TYPE *y, const int incY)
/*
 * 4 register prefetch on X (assumed to be in L1), 16 register prefetch on
 * Y (L2 or main), with 8-cycle muladd.  Unrolled by 16 to ensure multiple
 * cacheline usage for both single and double.  This kernel may blow for
 * axpy, but it'll rock for GER.
 */
{
   const int N16 = (N>>4)<<4;
   int i, j;
   const TYPE *stX = x + N16 - 32;
   const register TYPE alp = alpha;
   register TYPE m0, m1, m2, m3, m4, m5, m6, m7;
   register TYPE x0, x1, xx0, xx1;
   register TYPE y0, y1, y2, y3, y4, y5, y6, y7;
   register TYPE yy0, yy1, yy2, yy3, yy4, yy5, yy6, yy7;

   if (N16 > 16)
   {
      x0 = *x; xx0 = x[8];
      x1 = x[1]; xx1 = x[9];
      y0 = *y;   yy0 = y[8];
      y1 = y[1]; yy1 = y[9];
      y2 = y[2]; yy2 = y[10];
      y3 = y[3]; yy3 = y[11];
      y4 = y[4]; yy4 = y[12];
      y5 = y[5]; yy5 = y[13];
      y6 = y[6]; yy6 = y[14];
      y7 = y[7]; yy7 = y[15];
      m0 = y0  + alp * x0;   x0 = x[2];  y0  = y[16];
      m1 = yy0 + alp * xx0; xx0 = x[10]; yy0 = y[24];
      m2 = y1  + alp * x1;   x1 = x[3];  y1  = y[17];
      m3 = yy1 + alp * xx1; xx1 = x[11]; yy1 = y[25];
      m4 = y2  + alp * x0;   x0 = x[4];  y2  = y[18];
      m5 = yy2 + alp * xx0; xx0 = x[12]; yy2 = y[26];
      m6 = y3  + alp * x1;   x1 = x[5];  y3  = y[19];
      m7 = yy3 + alp * xx1; xx1 = x[13]; yy3 = y[27];
      if (N16 != 32)
      {
         do
         {
            *y    = m0; m0 =  y4 + alp *  x0;  x0 = x[ 6];  y4 = y[20];
            y[ 8] = m1; m1 = yy4 + alp * xx0; xx0 = x[14]; yy4 = y[28];
            y[ 1] = m2; m2 =  y5 + alp *  x1;  x1 = x[ 7];  y5 = y[21];
            y[ 9] = m3; m3 = yy5 + alp * xx1; xx1 = x[15]; yy5 = y[29]; x += 16;
            y[ 2] = m4; m4 =  y6 + alp *  x0;  x0 = *x;     y6 = y[22];
            y[10] = m5; m5 = yy6 + alp * xx0; xx0 = x[ 8]; yy6 = y[30];
            y[ 3] = m6; m6 =  y7 + alp *  x1;  x1 = x[ 1];  y7 = y[23];
            y[11] = m7; m7 = yy7 + alp * xx1; xx1 = x[ 9]; yy7 = y[31];

            y[ 4] = m0; m0 = y0  + alp * x0;   x0 = x[2];  y0  = y[32];
            y[12] = m1; m1 = yy0 + alp * xx0; xx0 = x[10]; yy0 = y[40];
            y[ 5] = m2; m2 = y1  + alp * x1;   x1 = x[3];  y1  = y[33];
            y[13] = m3; m3 = yy1 + alp * xx1; xx1 = x[11]; yy1 = y[41];
            y[ 6] = m4; m4 = y2  + alp * x0;   x0 = x[4];  y2  = y[34];
            y[14] = m5; m5 = yy2 + alp * xx0; xx0 = x[12]; yy2 = y[42];
            y[ 7] = m6; m6 = y3  + alp * x1;   x1 = x[5];  y3  = y[35];
            y[15] = m7; m7 = yy3 + alp * xx1; xx1 = x[13]; yy3 = y[43];
            y += 16;
         }
         while (x != stX);
      }
      *y    = m0; m0 =  y4 + alp *  x0;  x0 = x[ 6];  y4 = y[20];
      y[ 8] = m1; m1 = yy4 + alp * xx0; xx0 = x[14]; yy4 = y[28];
      y[ 1] = m2; m2 =  y5 + alp *  x1;  x1 = x[ 7];  y5 = y[21];
      y[ 9] = m3; m3 = yy5 + alp * xx1; xx1 = x[15]; yy5 = y[29]; x += 16;
      y[ 2] = m4; m4 =  y6 + alp *  x0;  x0 = *x;     y6 = y[22];
      y[10] = m5; m5 = yy6 + alp * xx0; xx0 = x[ 8]; yy6 = y[30];
      y[ 3] = m6; m6 =  y7 + alp *  x1;  x1 = x[ 1];  y7 = y[23];
      y[11] = m7; m7 = yy7 + alp * xx1; xx1 = x[ 9]; yy7 = y[31];

      y[ 4] = m0; m0 = y0  + alp * x0;   x0 = x[2];
      y[12] = m1; m1 = yy0 + alp * xx0; xx0 = x[10];
      y[ 5] = m2; m2 = y1  + alp * x1;   x1 = x[3];
      y[13] = m3; m3 = yy1 + alp * xx1; xx1 = x[11];
      y[ 6] = m4; m4 = y2  + alp * x0;   x0 = x[4];
      y[14] = m5; m5 = yy2 + alp * xx0; xx0 = x[12];
      y[ 7] = m6; m6 = y3  + alp * x1;   x1 = x[5];
      y[15] = m7; m7 = yy3 + alp * xx1; xx1 = x[13];
      y += 16;

      *y    = m0; m0 =  y4 + alp *  x0;  x0 = x[ 6];
      y[ 8] = m1; m1 = yy4 + alp * xx0; xx0 = x[14];
      y[ 1] = m2; m2 =  y5 + alp *  x1;  x1 = x[ 7];
      y[ 9] = m3; m3 = yy5 + alp * xx1; xx1 = x[15]; x += 16;
      y[ 2] = m4; m4 =  y6 + alp *  x0;
      y[10] = m5; m5 = yy6 + alp * xx0;
      y[ 3] = m6; m6 =  y7 + alp *  x1;
      y[11] = m7; m7 = yy7 + alp * xx1;

      y[ 4] = m0;
      y[12] = m1;
      y[ 5] = m2;
      y[13] = m3;
      y[ 6] = m4;
      y[14] = m5;
      y[ 7] = m6;
      y[15] = m7;
      y += 16;
      axpyCU(N-N16, alpha, x, y);
   }
   else axpyCU(N, alpha, x, y);
}
