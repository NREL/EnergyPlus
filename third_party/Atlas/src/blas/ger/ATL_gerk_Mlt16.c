#include "atlas_misc.h"
#include "atlas_lvl2.h"
#ifdef TCPLX
   #include "atlas_reflevel2.h"
#endif

typedef void (*gerk_t)
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda);

#ifdef TREAL

static void ATL_gerk_Meq1
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq2
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq3
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq4
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq5
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq6
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq7
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq8
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq9
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq10
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
   x9 = -X[9*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
      A[9] += x9 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   y0 = alpha;
   x9 = X[9*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   x9 = X[9*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq11
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
   x9 = -X[9*incX];
   x10 = -X[10*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
      A[9] += x9 * y0;
      A[10] += x10 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   y0 = alpha;
   x9 = X[9*incX] * y0;
   y0 = alpha;
   x10 = X[10*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   x9 = X[9*incX];
   x10 = X[10*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq12
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
   x9 = -X[9*incX];
   x10 = -X[10*incX];
   x11 = -X[11*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
      A[9] += x9 * y0;
      A[10] += x10 * y0;
      A[11] += x11 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   y0 = alpha;
   x9 = X[9*incX] * y0;
   y0 = alpha;
   x10 = X[10*incX] * y0;
   y0 = alpha;
   x11 = X[11*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   x9 = X[9*incX];
   x10 = X[10*incX];
   x11 = X[11*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq13
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
   x9 = -X[9*incX];
   x10 = -X[10*incX];
   x11 = -X[11*incX];
   x12 = -X[12*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
      A[9] += x9 * y0;
      A[10] += x10 * y0;
      A[11] += x11 * y0;
      A[12] += x12 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   y0 = alpha;
   x9 = X[9*incX] * y0;
   y0 = alpha;
   x10 = X[10*incX] * y0;
   y0 = alpha;
   x11 = X[11*incX] * y0;
   y0 = alpha;
   x12 = X[12*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   x9 = X[9*incX];
   x10 = X[10*incX];
   x11 = X[11*incX];
   x12 = X[12*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq14
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
   x9 = -X[9*incX];
   x10 = -X[10*incX];
   x11 = -X[11*incX];
   x12 = -X[12*incX];
   x13 = -X[13*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
      A[9] += x9 * y0;
      A[10] += x10 * y0;
      A[11] += x11 * y0;
      A[12] += x12 * y0;
      A[13] += x13 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   y0 = alpha;
   x9 = X[9*incX] * y0;
   y0 = alpha;
   x10 = X[10*incX] * y0;
   y0 = alpha;
   x11 = X[11*incX] * y0;
   y0 = alpha;
   x12 = X[12*incX] * y0;
   y0 = alpha;
   x13 = X[13*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   x9 = X[9*incX];
   x10 = X[10*incX];
   x11 = X[11*incX];
   x12 = X[12*incX];
   x13 = X[13*incX];
   goto X_IS_LOADED;
}
static void ATL_gerk_Meq15
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12;
   register TYPE x13, x14;

   if (alpha == ATL_rone) goto ALPHA1;
   else if (alpha != ATL_rnone) goto ALPHAX;
   x0 = -X[0*incX];
   x1 = -X[1*incX];
   x2 = -X[2*incX];
   x3 = -X[3*incX];
   x4 = -X[4*incX];
   x5 = -X[5*incX];
   x6 = -X[6*incX];
   x7 = -X[7*incX];
   x8 = -X[8*incX];
   x9 = -X[9*incX];
   x10 = -X[10*incX];
   x11 = -X[11*incX];
   x12 = -X[12*incX];
   x13 = -X[13*incX];
   x14 = -X[14*incX];
X_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0 = Y[incY*j];
      A[0] += x0 * y0;
      A[1] += x1 * y0;
      A[2] += x2 * y0;
      A[3] += x3 * y0;
      A[4] += x4 * y0;
      A[5] += x5 * y0;
      A[6] += x6 * y0;
      A[7] += x7 * y0;
      A[8] += x8 * y0;
      A[9] += x9 * y0;
      A[10] += x10 * y0;
      A[11] += x11 * y0;
      A[12] += x12 * y0;
      A[13] += x13 * y0;
      A[14] += x14 * y0;
   }
   return;

ALPHAX:
   y0 = alpha;
   x0 = X[0*incX] * y0;
   y0 = alpha;
   x1 = X[1*incX] * y0;
   y0 = alpha;
   x2 = X[2*incX] * y0;
   y0 = alpha;
   x3 = X[3*incX] * y0;
   y0 = alpha;
   x4 = X[4*incX] * y0;
   y0 = alpha;
   x5 = X[5*incX] * y0;
   y0 = alpha;
   x6 = X[6*incX] * y0;
   y0 = alpha;
   x7 = X[7*incX] * y0;
   y0 = alpha;
   x8 = X[8*incX] * y0;
   y0 = alpha;
   x9 = X[9*incX] * y0;
   y0 = alpha;
   x10 = X[10*incX] * y0;
   y0 = alpha;
   x11 = X[11*incX] * y0;
   y0 = alpha;
   x12 = X[12*incX] * y0;
   y0 = alpha;
   x13 = X[13*incX] * y0;
   y0 = alpha;
   x14 = X[14*incX] * y0;
   goto X_IS_LOADED;
ALPHA1:
   x0 = X[0*incX];
   x1 = X[1*incX];
   x2 = X[2*incX];
   x3 = X[3*incX];
   x4 = X[4*incX];
   x5 = X[5*incX];
   x6 = X[6*incX];
   x7 = X[7*incX];
   x8 = X[8*incX];
   x9 = X[9*incX];
   x10 = X[10*incX];
   x11 = X[11*incX];
   x12 = X[12*incX];
   x13 = X[13*incX];
   x14 = X[14*incX];
   goto X_IS_LOADED;
}

void Mjoin(PATL,gerk_Mlt16)
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
/*
 * ATLAS's normal GER kernels are optimized for long-M, and loop over rows in
 * the inner loop.  To avoid this killing us on short, wide matrices, have
 * special case code for M < 16.  This also allows kernels to assume M >= 16.
 */
{
   static gerk_t gerks[15]={ATL_gerk_Meq1, ATL_gerk_Meq2, ATL_gerk_Meq3,
                            ATL_gerk_Meq4, ATL_gerk_Meq5, ATL_gerk_Meq6,
                            ATL_gerk_Meq7, ATL_gerk_Meq8, ATL_gerk_Meq9,
                            ATL_gerk_Meq10, ATL_gerk_Meq11, ATL_gerk_Meq12,
                            ATL_gerk_Meq13, ATL_gerk_Meq14, ATL_gerk_Meq15};
   if (M < 1 || N < 1 || SCALAR_IS_ZERO(alpha))
      return;
   #ifdef ATL_GAS_x8664
   if (M > 14)
   #elif defined(ATL_GAS_x8632)
   if (M > 6)
   #else
   if (M > 15)
   #endif
   {
      Mjoin(PATL,gerk_axpy)(M, N, alpha, X, incX, Y, incY, A, lda);
      return;
   }
   gerks[M-1](M, N, alpha, X, incX, Y, incY, A, lda);
}

#else /* complex type */
#ifdef Conj_
void Mjoin(PATL,gerck_Mlt16)
#else
void Mjoin(PATL,gerk_Mlt16)
#endif
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda)
{
/*
 * For now, complex simply calls refblas for short M, axpy-based for large.
 * Probably not worth additional instruction load for complex to unroll.
 */
   #ifndef TUNING
   if (M < 8)
#ifdef Conj_
      Mjoin(PATL,refgerc)(M, N, alpha, X, incX, Y, incY, A, lda);
#else
      Mjoin(PATL,refgeru)(M, N, alpha, X, incX, Y, incY, A, lda);
#endif
   else
   #endif
#ifdef Conj_
      Mjoin(PATL,gerck_axpy)(M, N, alpha, X, incX, Y, incY, A, lda);
#else
      Mjoin(PATL,gerk_axpy)(M, N, alpha, X, incX, Y, incY, A, lda);
#endif
}
#endif
