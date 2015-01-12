/*
 * c_cblas1.c
 *
 * The program is a C wrapper for ccblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
void F77_caxpy(const int *N, const void *alpha, void *X,
                    const int *incX, void *Y, const int *incY)
{
   cblas_caxpy(*N, alpha, X, *incX, Y, *incY);
   return;
}

void F77_ccopy(const int *N, void *X, const int *incX, 
                    void *Y, const int *incY)
{
   cblas_ccopy(*N, X, *incX, Y, *incY);
   return;
}

void F77_cdotc(const int *N, void *X, const int *incX, 
                        void *Y, const int *incY, void *dotc)
{
   cblas_cdotc_sub(*N, X, *incX, Y, *incY, dotc);
   return;
}

void F77_cdotu(const int *N, void *X, const int *incX, 
                        void *Y, const int *incY,void *dotu)
{
   cblas_cdotu_sub(*N, X, *incX, Y, *incY, dotu);
   return;
}

void F77_cscal(const int *N, const void * *alpha, void *X,
                         const int *incX)
{
   cblas_cscal(*N, alpha, X, *incX);
   return;
}

void F77_csscal(const int *N, const float *alpha, void *X,
                         const int *incX)
{
   cblas_csscal(*N, *alpha, X, *incX);
   return;
}

void F77_cswap( const int *N, void *X, const int *incX,
                          void *Y, const int *incY)
{
   cblas_cswap(*N,X,*incX,Y,*incY);
   return;
}

int F77_icamax(const int *N, const void *X, const int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return (cblas_icamax(*N, X, *incX)+1);
}

void  F77_scnrm2(const int *N, const void *X, const int *incX, float *tmp)
{
   *tmp = cblas_scnrm2(*N, X, *incX);
}

void  F77_scasum(const int *N, void *X, const int *incX, float *tmp)
{
   *tmp = cblas_scasum(*N, X, *incX);
}

/*
 * The following routines are not tested by the tester, and not part of
 * standard, but added by RCW in order to force link test of ref blas
 * provided by ATLAS
 */
void F77_crotg(void *a, void *b, void *c, void *s)
{
   cblas_crotg(a, b, c, s);
}
void F77_csrot(const int *N, void *X, const int *incX,
               void *Y, const int *incY, const float *c, const float *s)
{
   cblas_csrot(*N, X, *incX, Y, *incY, *c, *s);
}
