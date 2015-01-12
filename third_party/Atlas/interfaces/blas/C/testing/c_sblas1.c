/*
 * c_sblas1.c
 *
 * The program is a C wrapper for scblat1.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_test.h"
#include "cblas.h"
void  F77_sasum(const int *N, float *X, const int *incX, float *tmp)
{
   *tmp = cblas_sasum(*N, X, *incX);
}

void F77_saxpy(const int *N, const float *alpha, const float *X,
                    const int *incX, float *Y, const int *incY)
{
   cblas_saxpy(*N, *alpha, X, *incX, Y, *incY);
   return;
}

#if 0
float F77_scasum(const int *N, void *X, const int *incX)
{
   return cblas_scasum(*N, X, *incX);
}

void  F77_scnrm2(const int *N, const void *X, const int *incX)
{
   return cblas_scnrm2(*N, X, *incX);
}
#endif

void F77_scopy(const int *N, const float *X, const int *incX, 
                    float *Y, const int *incY)
{
   cblas_scopy(*N, X, *incX, Y, *incY);
   return;
}

void  F77_sdot(const int *N, const float *X, const int *incX, 
                        const float *Y, const int *incY, float *tmp)
{
   *tmp = cblas_sdot(*N, X, *incX, Y, *incY);
}

void F77_snrm2(const int *N, const float *X, const int *incX, float *tmp)
{
   *tmp = cblas_snrm2(*N, X, *incX);
}

void F77_srotg( float *a, float *b, float *c, float *s)
{
   cblas_srotg(a,b,c,s);
   return;
}

void F77_srot( const int *N, float *X, const int *incX, float *Y,
              const int *incY, const float  *c, const float  *s)
{
   cblas_srot(*N,X,*incX,Y,*incY,*c,*s);
   return;
}

void F77_sscal(const int *N, const float *alpha, float *X,
                         const int *incX)
{
   cblas_sscal(*N, *alpha, X, *incX);
   return;
}

void F77_sswap( const int *N, float *X, const int *incX,
                          float *Y, const int *incY)
{
   cblas_sswap(*N,X,*incX,Y,*incY);
   return;
}

int F77_isamax(const int *N, const float *X, const int *incX)
{
   if (*N < 1 || *incX < 1) return(0);
   return(cblas_isamax(*N, X, *incX)+1);
}

/*
 * The following routines are not tested by the tester, but added by RCW
 * to force link test of ATLAS reference BLAS replacements
 */
void F77_srotmg(float *d1, float *d2, float *b1, const float *b2, float *P)
{
   cblas_srotmg(d1, d2, b1, *b2, P);
}

void F77_srotm(const int *N, float *X, const int *incX,
               float *Y, const int *incY, const float *P)
{
   cblas_srotm(*N, X, *incX, Y, *incY, P);
}

float F77_sdsdot(const int *N, const float *alpha, const float *X,
                 const int *incX, const float *Y, const int *incY)
{
   return(cblas_sdsdot(*N, *alpha, X, *incX, Y, *incY));
}

double F77_dsdot(const int *N, const float *X, const int *incX,
                 const float *Y, const int *incY)
{
   return(cblas_dsdot(*N, X, *incX, Y, *incY));
}
