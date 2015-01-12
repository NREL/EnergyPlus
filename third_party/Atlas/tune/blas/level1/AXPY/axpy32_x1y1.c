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
void ATL_UAXPY(const int N, const SCALAR alpha0, const TYPE *X, const int incX,
               TYPE *Y, const int incY)
{
   const int n = (N/32)*32;
   const TYPE *stX;
   int nr = N-n;
   register TYPE alpha=alpha0;

   if (n)
   {
      stX = X + n;
      do
      {
         Y[0] += alpha * X[0];
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
         Y[16] += alpha * X[16];
         Y[17] += alpha * X[17];
         Y[18] += alpha * X[18];
         Y[19] += alpha * X[19];
         Y[20] += alpha * X[20];
         Y[21] += alpha * X[21];
         Y[22] += alpha * X[22];
         Y[23] += alpha * X[23];
         Y[24] += alpha * X[24];
         Y[25] += alpha * X[25];
         Y[26] += alpha * X[26];
         Y[27] += alpha * X[27];
         Y[28] += alpha * X[28];
         Y[29] += alpha * X[29];
         Y[30] += alpha * X[30];
         Y[31] += alpha * X[31];
         X += 32;
         Y += 32;
      }
      while (X != stX);
   }
   if (nr) axpyCU(nr, alpha0, X, Y);
}
