#include "atlas_misc.h"
#include "atlas_prefetch.h"
#include <math.h>
float ATL_UNRM2(const int N, const float *X, const int incX)
/*
 * uses double arithmetic so ddot-like algorithm can safely be employed
 */
{
   int n;
   register double t0=ATL_rzero, t1=ATL_rzero, t2=ATL_rzero, t3=ATL_rzero;
   register double x0, x1, x2, x3, x4, x5, x6, x7;
   register double x8, x9, x10, x11, x12, x13, x14, x15;
   const float *stX, *stX0 = X+N;

   n = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(16));
   if (n)  /* not aligned */
   {
      stX = X + n;
      do
      {
         x0 = *X++;
         t0 += x0*x0;
      }
      while (X != stX);
   }
   n = N - n;

   if (n >= 32)
   {
      stX = X + ((n>>4)<<4);
      x0 = *X; x1 = X[1]; x2 = X[2]; x3 = X[3];
      x4 = X[4]; x5 = X[5]; x6 = X[6]; x7 = X[7];
      x8 = X[8]; x9 = X[9]; x10 = X[10]; x11 = X[11];
      x12 = X[12]; x13 = X[13]; x14 = X[14]; x15 = X[15]; X += 16;
      do
      {
         ATL_pfl1R(X+256);
         t0 += x0*x0; x0 = *X;
         t1 += x1*x1; x1 = X[1];
         t2 += x2*x2; x2 = X[2];
         t3 += x3*x3; x3 = X[3];
         t0 += x4*x4; x4 = X[4];
         t1 += x5*x5; x5 = X[5];
         t2 += x6*x6; x6 = X[6];
         t3 += x7*x7; x7 = X[7];

         t0 += x8 *x8;  x8  = X[ 8];
         t1 += x9 *x9;  x9  = X[ 9];
         t2 += x10*x10; x10 = X[10];
         t3 += x11*x11; x11 = X[11];
         t0 += x12*x12; x12 = X[12];
         t1 += x13*x13; x13 = X[13];
         t2 += x14*x14; x14 = X[14];
         t3 += x15*x15; x15 = X[15];
         X += 16;
      }
      while (X != stX);
      t0 += x0*x0;
      t1 += x1*x1;
      t2 += x2*x2;
      t3 += x3*x3;
      t0 += x4*x4;
      t1 += x5*x5;
      t2 += x6*x6;
      t3 += x7*x7;
      t0 += x8 *x8;
      t1 += x9 *x9;
      t2 += x10*x10;
      t3 += x11*x11;
      t0 += x12*x12;
      t1 += x13*x13;
      t2 += x14*x14;
      t3 += x15*x15;
      t0 += t1;
      t2 += t3;
      t0 += t2;
   }
   if (X != stX0)
   {
      do
      {
         x0 = *X++;
         t0 += x0*x0;
      }
      while (X != stX0);
   }
   t0 = sqrt(t0);
   return(t0);
}
