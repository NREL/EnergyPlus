#include "atlas_misc.h"

void ATL_UGER2K
   (ATL_CINT M, ATL_CINT N, const TYPE *X0, const TYPE *Y0,
    const TYPE *X1, const TYPE *Y1, TYPE *A, ATL_CINT lda)
{
   const TYPE *x0, *x1;
   register TYPE y0r, y0i, y1r, y1i, x0r, x0i, x1r, x1i;
   register ATL_INT i, j;
   const ATL_INT incA = (lda-M)<<1;

   for (j=0; j < N; j++, A += incA)
   {
      y0r = *Y0;
      y0i = Y0[1];
      Y0 += 2;
      y1r = *Y1;
      y1i = Y1[1];
      Y1 += 2;
      x0 = X0;
      x1 = X1;
      for (i=0; i < M; i++)
      {
         x0r = *x0;
         x0i = x0[1];
         x0 += 2;
         x1r = *x1;
         x1i = x1[1];
         x1 += 2;
         *A += x0r*y0r - x0i*y0i + x1r*y1r - x1i*y1i;
         A[1] += x0r*y0i + x0i*y0r + x1r*y1i + x1i*y1r;
         A += 2;
      }
   }
}
