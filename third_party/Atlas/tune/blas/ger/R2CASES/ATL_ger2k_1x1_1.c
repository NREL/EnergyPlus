#include "atlas_misc.h"

void ATL_UGER2K
   (ATL_CINT M, ATL_CINT N, const TYPE *X0, const TYPE *Y0,
    const TYPE *X1, const TYPE *Y1, TYPE *A, ATL_CINT lda)
{
   register TYPE y0, y1;
   register ATL_INT i, j;

   for (j=0; j < N; j++)
   {
      y0 = Y0[j];
      y1 = Y1[j];
      for (i=0; i < M; i++)
         A[i+j*lda] += X0[i] * y0 + X1[i] * y1;
   }
}
