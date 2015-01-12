#include "atlas_misc.h"
#include "atlas_level1.h"
#ifdef Conj_
void Mjoin(PATL,gerck_axpy)
#else
void Mjoin(PATL,gerk_axpy)
#endif
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
/*
 * This routine typically called when N is very small, and so we can't afford
 * to copy the vectors even if M is large; in this case we simply loop over
 * the appropriate calls to the AXPY
 */
{
#ifdef TCPLX
   const int incy = incY+incY, lda2 = lda+lda;
   TYPE y[2];
   const TYPE ralp=(*alpha), ialp=alpha[1];
   TYPE ry, iy;
   int j;

   for (j=0; j < N; j++, A += lda2, Y += incy)
   {
      ry = *Y; iy = Y[1];
      #ifdef Conj_
         y[0] = ry*ralp + iy*ialp;
         y[1] = ry*ialp - iy*ralp;
      #else
         y[0] = ry*ralp - iy*ialp;
         y[1] = ry*ialp + iy*ralp;
      #endif
      Mjoin(PATL,axpy)(M, y, X, incX, A, 1);
   }
#else
   int j;
   for (j=0; j < N; j++, A += lda, Y += incY)
      Mjoin(PATL,axpy)(M, *Y*alpha, X, incX, A, 1);
#endif
}
