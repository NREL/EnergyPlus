/*
 * c_zblas1.c
 *
 * The program is a C wrapper for zcblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
void F77_zaxpy(const int *N, const void *alpha, void *X,
                    const int *incX, void *Y, const int *incY)
{
   cblas_zaxpy(*N, alpha, X, *incX, Y, *incY);
   return;
}

void F77_zcopy(const int *N, void *X, const int *incX, 
                    void *Y, const int *incY)
{
   cblas_zcopy(*N, X, *incX, Y, *incY);
   return;
}

void F77_zdotc(const int *N, const void *X, const int *incX, 
                     const void *Y, const int *incY,void *dotc)
{
   cblas_zdotc_sub(*N, X, *incX, Y, *incY, dotc);
   return;
}

void F77_zdotu(const int *N, void *X, const int *incX, 
                        void *Y, const int *incY,void *dotu)
{
   cblas_zdotu_sub(*N, X, *incX, Y, *incY, dotu);
   return;
}

void F77_zdscal(const int *N, const double *alpha, void *X,
                         const int *incX)
{
   cblas_zdscal(*N, *alpha, X, *incX);
   return;
}

void F77_zscal(const int *N, const void * *alpha, void *X,
                         const int *incX)
{
   cblas_zscal(*N, alpha, X, *incX);
   return;
}

void F77_zswap( const int *N, void *X, const int *incX,
                          void *Y, const int *incY)
{
   cblas_zswap(*N,X,*incX,Y,*incY);
   return;
}

int F77_izamax(const int *N, const void *X, const int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return(cblas_izamax(*N, X, *incX)+1);
}

void   F77_dznrm2(const int *N, const void *X, const int *incX, double *tmp)
{
   *tmp = cblas_dznrm2(*N, X, *incX);
}

void   F77_dzasum(const int *N, void *X, const int *incX, double *tmp)
{
   *tmp = cblas_dzasum(*N, X, *incX);
}
/* 
 * The following routines are not tested by the tester, and not part of
 * standard, but added by RCW in order to force link test of ref blas
 * provided by ATLAS
 */
void F77_zrotg(void *a, void *b, void *c, void *s)
{
   cblas_zrotg(a, b, c, s);
}
void F77_zdrot(const int *N, void *X, const int *incX,
               void *Y, const int *incY, const double *c, const double *s)
{
   cblas_zdrot(*N, X, *incX, Y, *incY, *c, *s);
}
