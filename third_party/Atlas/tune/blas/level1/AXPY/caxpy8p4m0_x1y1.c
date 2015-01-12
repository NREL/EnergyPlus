#include "atlas_misc.h"

static void axpyCU(const int N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 *  For cleanup, see if we can get compiler to do the work, use constant loops
 */
{
   int i;
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   register TYPE xr, xi;
   switch(N)
   {
   case 1:
      xr = *x; xi = x[1];
      *y   += ralpha * xr - ialpha * xi;
      y[1] += ialpha * xr + ralpha * xi;
      break;
   case 2:
      for (i=2; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 3:
      for (i=3; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 4:
      for (i=4; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 5:
      for (i=5; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 6:
      for (i=6; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 7:
      for (i=7; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   default:
      for (i=N; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
   }
}
static void axpy_8(const int N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 * 8 register prefetch on X & Y, with 4 cycle multiply & 4 cycle add,
 * unrolled by 16 to ensure multiple cacheline usage for both singe & double
 */
{
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   register TYPE xr0, xi0, xr1, xi1, xxr0, xxi0, xxr1, xxi1;
   register TYPE yr0, yi0, yr1, yi1, yyr0, yyi0, yyr1, yyi1;
   register TYPE m0, m1, m2, m3, a0, a1, a2, a3;
   const TYPE *stX = x + (N<<1) - 16;

/*   ATL_assert( (N == (N>>3)<<3) && N ); */

   xr0  = *x;   xxr0 = x[8];
   xi0  = x[1]; xxi0 = x[9];
   xr1  = x[2]; xxr1 = x[10];
   xi1  = x[3]; xxi1 = x[11];

   yr0  = *y;   yyr0 = y[8];
   yi0  = y[1]; yyi0 = y[9];
   yr1  = y[2]; yyr1 = y[10];
   yi1  = y[3]; yyi1 = y[11];

   m0 = ralpha * xr0;
   m1 = ralpha * xxr0;
   m2 = ialpha * xr0; xr0  = x[4];
   m3 = ialpha *xxr0; xxr0 = x[12];

   a0 = yr0  + m0; m0 = ialpha *  xi0; yr0  = y[4];
   a1 = yyr0 + m1; m1 = ialpha * xxi0; yyr0 = y[12];
   a2 = yi0  + m2; m2 = ralpha *  xi0;  xi0  = x[5]; yi0  = y[5];
   a3 = yyi0 + m3; m3 = ralpha * xxi0; xxi0 = x[13]; yyi0 = y[13];

   a0 -= m0; m0 = ralpha * xr1;
   a1 -= m1; m1 = ralpha * xxr1;
   a2 += m2; m2 = ialpha *  xr1; xr1  = x[6];
   a3 += m3; m3 = ialpha * xxr1; xxr1 = x[14];
   if (N != 8)
   {
      do
      {
         *y   = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1;  yr1 = y[6];
         y[8] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1; yyr1 = y[14];
         y[1] = a2; a2 =  yi1 + m2; m2 = ralpha *  xi1;  xi1 = x[7];
                    yi1  = y[7];
         y[9] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1; xxi1 = x[15];
                    yyi1 = y[15]; x += 16;
         a0 -= m0; m0 = ralpha *  xr0;
         a1 -= m1; m1 = ralpha * xxr0;
         a2 += m2; m2 = ialpha *  xr0; xr0 = *x;
         a3 += m3; m3 = ialpha * xxr0; xxr0 = x[8];
         y[ 2] = a0; a0 =  yr0 + m0; m0 = ialpha *  xi0; yr0  = y[16];
         y[10] = a1; a1 = yyr0 + m1; m1 = ialpha * xxi0; yyr0 = y[24];
         y[ 3] = a2; a2 = yi0  + m2; m2 = ralpha *  xi0; xi0  = x[1];
                     yi0  = y[17];
         y[11] = a3; a3 = yyi0 + m3; m3 = ralpha * xxi0; xxi0 = x[9];
                     yyi0 = y[25];

         a0 -= m0; m0 = ralpha *  xr1;
         a1 -= m1; m1 = ralpha * xxr1;
         a2 += m2; m2 = ialpha *  xr1; xr1  = x[2];
         a3 += m3; m3 = ialpha * xxr1; xxr1 = x[10];
         y[ 4] = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1; yr1  = y[18];
         y[12] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1; yyr1 = y[26];
         y[ 5] = a2; a2 = yi1  + m2; m2 = ralpha *  xi1; xi1  = x[3];
                     yi1  = y[19];
         y[13] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1; xxi1 = x[11];
                     yyi1 = y[27];
         a0 -= m0; m0 = ralpha *  xr0;
         a1 -= m1; m1 = ralpha * xxr0;
         a2 += m2; m2 = ialpha *  xr0; xr0 = x[4];
         a3 += m3; m3 = ialpha * xxr0; xxr0 = x[12];
         y[ 6] = a0; a0 =  yr0 + m0; m0 = ialpha *  xi0; yr0  = y[20];
         y[14] = a1; a1 = yyr0 + m1; m1 = ialpha * xxi0; yyr0 = y[28];
         y[ 7] = a2; a2 = yi0  + m2; m2 = ralpha *  xi0; xi0  = x[5];
                     yi0  = y[21];
         y[15] = a3; a3 = yyi0 + m3; m3 = ralpha * xxi0; xxi0 = x[13];
                     yyi0 = y[29];
         y += 16;
         a0 -= m0; m0 = ralpha *  xr1;
         a1 -= m1; m1 = ralpha * xxr1;
         a2 += m2; m2 = ialpha *  xr1; xr1  = x[6];
         a3 += m3; m3 = ialpha * xxr1; xxr1 = x[14];
      }
      while (x != stX);
   }
/*
 * Drain pipe, store last 8 elts of Y
 */
   *y   = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1;  yr1 = y[6];
   y[8] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1; yyr1 = y[14];
   y[1] = a2; a2 =  yi1 + m2; m2 = ralpha *  xi1;  xi1 = x[7]; yi1  = y[7];
   y[9] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1; xxi1 = x[15]; yyi1 = y[15];
   a0 -= m0; m0 = ralpha *  xr0;
   a1 -= m1; m1 = ralpha * xxr0;
   a2 += m2; m2 = ialpha *  xr0;
   a3 += m3; m3 = ialpha * xxr0;
   y[ 2] = a0; a0 =  yr0 + m0; m0 = ialpha *  xi0;
   y[10] = a1; a1 = yyr0 + m1; m1 = ialpha * xxi0;
   y[ 3] = a2; a2 = yi0  + m2; m2 = ralpha *  xi0;
   y[11] = a3; a3 = yyi0 + m3; m3 = ralpha * xxi0;

   a0 -= m0; m0 = ralpha *  xr1;
   a1 -= m1; m1 = ralpha * xxr1;
   a2 += m2; m2 = ialpha *  xr1;
   a3 += m3; m3 = ialpha * xxr1;
   y[ 4] = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1;
   y[12] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1;
   y[ 5] = a2; a2 = yi1  + m2; m2 = ralpha *  xi1;
   y[13] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1;
   a0 -= m0;
   a1 -= m1;
   a2 += m2;
   a3 += m3;
   y[ 6] = a0;
   y[14] = a1;
   y[ 7] = a2;
   y[15] = a3;
}

void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   const int n8 = (N>>3)<<3, nr = N - n8;

   if (n8)
   {
      axpy_8(n8, alpha, X, Y);
      X += n8<<1;
      Y += n8<<1;
   }
   if (nr) axpyCU(nr, alpha, X, Y);
}
