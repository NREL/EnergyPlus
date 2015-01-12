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
static void axpy_16(const int N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 * 8 register prefetch on X & Y, with 4 cycle multiply & 4 cycle add,
 * unrolled by 16 to ensure multiple cacheline usage for both singe & double
 */
{
   const register TYPE alp = alpha;
   register TYPE x0, x1, x2, x3, xx0, xx1, xx2, xx3;
   register TYPE y0, y1, y2, y3, yy0, yy1, yy2, yy3;
   register TYPE m0, m1, m2, m3, a0, a1, a2, a3;
   const TYPE *stX = x + N;

/*   ATL_assert( ((N>>4)<<4) == N  && N); */
   x0 = *x;    xx0 = x[8];
   x1 = x[1];  xx1 = x[9];
   x2 = x[2];  xx2 = x[10];
   x3 = x[3];  xx3 = x[11];
   y0 = *y;    yy0 = y[8];
   y1 = y[1];  yy1 = y[9];
   y2 = y[2];  yy2 = y[10];
   y3 = y[3];  yy3 = y[11];

   m0 = alp * x0;  x0 = x[4];
   m1 = alp * xx0; xx0 = x[12];
   m2 = alp * x1;  x1 = x[5];
   m3 = alp * xx1; xx1 = x[13];

   a0 = y0 + m0;  m0 = alp * x2;  y0 = y[4];   x2 = x[6];
   a1 = yy0 + m1; m1 = alp * xx2; yy0 = y[12]; xx2 = x[14];
   a2 = y1 + m2;  m2 = alp * x3;  y1 = y[5];   x3 = x[7];
   a3 = yy1 + m3; m3 = alp * xx3; yy1 = y[13]; xx3 = x[15];
   x += 16;
   if (N != 16)
   {
      do
      {
         *y    = a0; a0 =  y2 + m0;  y2 = y[ 6]; m0 = alp *  x0;  x0 = *x;
         y[ 8] = a1; a1 = yy2 + m1; yy2 = y[14]; m1 = alp * xx0; xx0 = x[ 8];
         y[ 1] = a2; a2 =  y3 + m2;  y3 = y[7];  m2 = alp *  x1;  x1 = x[ 1];
         y[ 9] = a3; a3 = yy3 + m3; yy3 = y[15]; m3 = alp * xx1; xx1 = x[ 9];
         y[ 2] = a0; a0 =  y0 + m0;  y0 = y[16]; m0 = alp *  x2;  x2 = x[ 2];
         y[10] = a1; a1 = yy0 + m1; yy0 = y[24]; m1 = alp * xx2; xx2 = x[10];
         y[ 3] = a2; a2 =  y1 + m2;  y1 = y[17]; m2 = alp *  x3;  x3 = x[ 3];
         y[11] = a3; a3 = yy1 + m3; yy1 = y[25]; m3 = alp * xx3; xx3 = x[11];

         y[ 4] = a0; a0 =  y2 + m0;  y2 = y[18]; m0 = alp *  x0;  x0 = x[ 4];
         y[12] = a1; a1 = yy2 + m1; yy2 = y[26]; m1 = alp * xx0; xx0 = x[12];
         y[ 5] = a2; a2 =  y3 + m2;  y3 = y[19]; m2 = alp *  x1;  x1 = x[ 5];
         y[13] = a3; a3 = yy3 + m3; yy3 = y[27]; m3 = alp * xx1; xx1 = x[13];
         y[ 6] = a0; a0 =  y0 + m0;  y0 = y[20]; m0 = alp *  x2;  x2 = x[ 6];
         y[14] = a1; a1 = yy0 + m1; yy0 = y[28]; m1 = alp * xx2; xx2 = x[14];
         y[ 7] = a2; a2 =  y1 + m2;  y1 = y[21]; m2 = alp *  x3;  x3 = x[ 7];
         y[15] = a3; a3 = yy1 + m3; yy1 = y[29]; m3 = alp * xx3; xx3 = x[15];
         x += 16;
         y += 16;
      }
      while (x != stX);
   }
/*
 * Drain pipes
 */
   *y    = a0; a0 =  y2 + m0;  y2 = y[ 6]; m0 = alp *  x0;
   y[ 8] = a1; a1 = yy2 + m1; yy2 = y[14]; m1 = alp * xx0;
   y[ 1] = a2; a2 =  y3 + m2;  y3 = y[7];  m2 = alp *  x1;
   y[ 9] = a3; a3 = yy3 + m3; yy3 = y[15]; m3 = alp * xx1;

   y[ 2] = a0; a0 =  y0 + m0;              m0 = alp *  x2;
   y[10] = a1; a1 = yy0 + m1;              m1 = alp * xx2;
   y[ 3] = a2; a2 =  y1 + m2;              m2 = alp *  x3;
   y[11] = a3; a3 = yy1 + m3;              m3 = alp * xx3;

   y[ 4] = a0; a0 =  y2 + m0;
   y[12] = a1; a1 = yy2 + m1;
   y[ 5] = a2; a2 =  y3 + m2;
   y[13] = a3; a3 = yy3 + m3;

   y[ 6] = a0;
   y[14] = a1;
   y[ 7] = a2;
   y[15] = a3;
}

void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *x, const int incX,
               TYPE *y, const int incY)
{
   const int inb =
             ATL_DivBySize(((size_t)y)) - ((ATL_DivBySize(((size_t)y))>>4)<<4);
   int n16, nr;

   if (inb < N)
   {
      n16 = ((N - inb)>>4)<<4;
      nr = N - inb - n16;
      if (inb)
      {
         axpyCU(inb, alpha, x, y);
         x += inb; y += inb;
      }
      if (n16)
      {
         axpy_16(n16, alpha, x, y);
         x += n16; y += n16;
      }
      if (nr) axpyCU(nr, alpha, x, y);
   }
   else axpyCU(N, alpha, x, y);
}
