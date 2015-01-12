/*
 * c_dblas1.c
 *
 * The program is a C wrapper for dcblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
void   F77_dasum(const int *N, double *X, const int *incX, double *tmp)
{
   *tmp = cblas_dasum(*N, X, *incX);
}

void F77_daxpy(const int *N, const double *alpha, const double *X,
                    const int *incX, double *Y, const int *incY)
{
   cblas_daxpy(*N, *alpha, X, *incX, Y, *incY);
   return;
}

void F77_dcopy(const int *N, double *X, const int *incX, 
                    double *Y, const int *incY)
{
   cblas_dcopy(*N, X, *incX, Y, *incY);
   return;
}

void   F77_ddot(const int *N, const double *X, const int *incX,
                const double *Y, const int *incY, double *tmp)
{
   *tmp = cblas_ddot(*N, X, *incX, Y, *incY);
}

void   F77_dnrm2(const int *N, const double *X, const int *incX, double *tmp)
{
   *tmp = cblas_dnrm2(*N, X, *incX);
}

void F77_drotg( double *a, double *b, double *c, double *s)
{
   cblas_drotg(a,b,c,s);
   return;
}

void F77_drot( const int *N, double *X, const int *incX, double *Y,
       const int *incY, const double *c, const double *s)
{

   cblas_drot(*N,X,*incX,Y,*incY,*c,*s);
   return;
}

void F77_dscal(const int *N, const double *alpha, double *X,
                         const int *incX)
{
   cblas_dscal(*N, *alpha, X, *incX);
   return;
}

void F77_dswap( const int *N, double *X, const int *incX,
                          double *Y, const int *incY)
{
   cblas_dswap(*N,X,*incX,Y,*incY);
   return;
}

#if 0
double F77_dzasum(const int *N, void *X, const int *incX)
{
   return cblas_dzasum(*N, X, *incX);
}

double F77_dznrm2(const int *N, const void *X, const int *incX)
{
   return cblas_dznrm2(*N, X, *incX);
}
#endif

int F77_idamax(const int *N, const double *X, const int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return(cblas_idamax(*N, X, *incX)+1);
}

/*
 * The following routines are not tested by the tester, but added by RCW
 * to force link test of ATLAS reference BLAS replacements
 */
void F77_drotmg(double *d1, double *d2, double *b1, const double *b2, double *P)
{
   cblas_drotmg(d1, d2, b1, *b2, P);
}

void F77_drotm(const int *N, double *X, const int *incX,
               double *Y, const int *incY, const double *P)
{
   cblas_drotm(*N, X, *incX, Y, *incY, P);
}

