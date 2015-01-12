#include "atlas_misc.h"
void ATL_UAXPBY(const int N, const SCALAR alpha, const TYPE *X, const int incX,
                const SCALAR beta, TYPE *Y, const int incY)
{
   const TYPE *stX=X+((N>>5)<<5), *stX0 = X+N;
   if (X != stX)
   {
      do
      {
         *Y = beta * *Y + *X;
         Y[ 1] = beta * Y[ 1] + X[ 1];
         Y[ 2] = beta * Y[ 2] + X[ 2];
         Y[ 3] = beta * Y[ 3] + X[ 3];
         Y[ 4] = beta * Y[ 4] + X[ 4];
         Y[ 5] = beta * Y[ 5] + X[ 5];
         Y[ 6] = beta * Y[ 6] + X[ 6];
         Y[ 7] = beta * Y[ 7] + X[ 7];
         Y[ 8] = beta * Y[ 8] + X[ 8];
         Y[ 9] = beta * Y[ 9] + X[ 9];
         Y[10] = beta * Y[10] + X[10];
         Y[11] = beta * Y[11] + X[11];
         Y[12] = beta * Y[12] + X[12];
         Y[13] = beta * Y[13] + X[13];
         Y[14] = beta * Y[14] + X[14];
         Y[15] = beta * Y[15] + X[15];
         Y[16] = beta * Y[16] + X[16];
         Y[17] = beta * Y[17] + X[17];
         Y[18] = beta * Y[18] + X[18];
         Y[19] = beta * Y[19] + X[19];
         Y[20] = beta * Y[20] + X[20];
         Y[21] = beta * Y[21] + X[21];
         Y[22] = beta * Y[22] + X[22];
         Y[23] = beta * Y[23] + X[23];
         Y[24] = beta * Y[24] + X[24];
         Y[25] = beta * Y[25] + X[25];
         Y[26] = beta * Y[26] + X[26];
         Y[27] = beta * Y[27] + X[27];
         Y[28] = beta * Y[28] + X[28];
         Y[29] = beta * Y[29] + X[29];
         Y[30] = beta * Y[30] + X[30];
         Y[31] = beta * Y[31] + X[31];
         X += 32;
         Y += 32;
      }
      while (X != stX);
   }
   while (X != stX0)
   {
      *Y = beta * *Y + *X++;
      Y++;
   }
}
