/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include "atlas_kern3.h"

#include "atlas_prefetch.h"
#ifdef Left_
static void trsmLU_2(const int N, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Left, 'Upper', with 1 col prefetch, written with all dependencies shown,
 * so that compiler can optimize.
 * A is known to be 2x2, with 1/alpha already applied, diagonals already
 * inverted
 */
{
   const TYPE ar11=*A, ai11=A[1], ar12=A[4], ai12=A[5];
   const TYPE ar22=A[6], ai22=A[7];
   TYPE xr1, xi1, xr2, xi2;
   TYPE t0, p0;
   const int ldb2=ldb+ldb;
   TYPE *bn=B+ldb2;
   const int pfd=ldb2+ldb2;
   int j;

   p0 = B[2];
   for (j=N-1; j; j--) /* stop 1 iteration early to stop prefetch */
   {
      xr2 = p0  ; xi2 = B[3];
      xr1 = *B  ; xi1 = B[1];

      t0 = xr2;
      xr2 = ar22*xr2 - ai22*xi2;
      xi2 = ar22*xi2 + ai22*t0;     p0 = bn[2];

      xr1 -= ar12*xr2 - ai12*xi2;
      xi1 -= ar12*xi2 + ai12*xr2;   ATL_pfl1W(bn+pfd);
      t0 = xr1;
      xr1 = ar11*xr1 - ai11*xi1;
      xi1 = ar11*xi1 + ai11*t0;

      *B   = xr1; B[1] = xi1;
      B[2] = xr2; B[3] = xi2;
      B = bn;
      bn += ldb2;
   }
   xr2 = p0  ; xi2 = B[3];
   xr1 = *B  ; xi1 = B[1];

   t0 = xr2;
   xr2 = ar22*xr2 - ai22*xi2;
   xi2 = ar22*xi2 + ai22*t0;

   xr1 -= ar12*xr2 - ai12*xi2;
   xi1 -= ar12*xi2 + ai12*xr2;
   t0 = xr1;
   xr1 = ar11*xr1 - ai11*xi1;
   xi1 = ar11*xi1 + ai11*t0;

   *B   = xr1; B[1] = xi1;
   B[2] = xr2; B[3] = xi2;
}
static void trsmLU_3(const int N, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Left, 'Upper', with 1 col prefetch, written with all dependencies shown,
 * so that compiler can optimize.
 * A is known to be 3x3, with 1/alpha already applied, diagonals already
 * inverted
 */
{
   const TYPE ar11=*A, ai11=A[1], ar12=A[6], ai12=A[7], ar13=A[12], ai13=A[13];
   const TYPE ar22=A[ 8], ai22=A[ 9], ar23=A[14], ai23=A[15];
   const TYPE ar33=A[16], ai33=A[17];
   TYPE xr1, xi1, xr2, xi2, xr3, xi3;
   TYPE t0, p0;
   const int ldb2=ldb+ldb;
   TYPE *bn=B+ldb2;
   const int pfd=ldb2+ldb2;
   int j;

   p0 = B[4];
   for (j=N-1; j; j--)
   {
      xr3 = p0  ; xi3 = B[5];
      xr1 = *B  ; xi1 = B[1];
      xr2 = B[2]; xi2 = B[3];

      t0 = xr3;
      xr3 = ar33*xr3 - ai33*xi3;
      xi3 = ar33*xi3 + ai33*t0;

      xr2 -= ar23*xr3 - ai23*xi3;     p0 = bn[4];
      xi2 -= ar23*xi3 + ai23*xr3;
      t0 = xr2;
      xr2 = ar22*xr2 - ai22*xi2;
      xi2 = ar22*xi2 + ai22*t0;

      xr1 -= ar13*xr3 - ai13*xi3;     ATL_pfl1W(bn+pfd);
      xi1 -= ar13*xi3 + ai13*xr3;     ATL_pfl1W(bn+pfd+4);
      xr1 -= ar12*xr2 - ai12*xi2;
      xi1 -= ar12*xi2 + ai12*xr2;
      t0 = xr1;
      xr1 = ar11*xr1 - ai11*xi1;
      xi1 = ar11*xi1 + ai11*t0;

      *B   = xr1; B[1] = xi1;
      B[2] = xr2; B[3] = xi2;
      B[4] = xr3; B[5] = xi3;
      B = bn;
      bn += ldb2;
   }
   xr3 = p0  ; xi3 = B[5];
   xr1 = *B  ; xi1 = B[1];
   xr2 = B[2]; xi2 = B[3];

   t0 = xr3;
   xr3 = ar33*xr3 - ai33*xi3;
   xi3 = ar33*xi3 + ai33*t0;

   xr2 -= ar23*xr3 - ai23*xi3;
   xi2 -= ar23*xi3 + ai23*xr3;
   t0 = xr2;
   xr2 = ar22*xr2 - ai22*xi2;
   xi2 = ar22*xi2 + ai22*t0;

   xr1 -= ar13*xr3 - ai13*xi3;
   xi1 -= ar13*xi3 + ai13*xr3;
   xr1 -= ar12*xr2 - ai12*xi2;
   xi1 -= ar12*xi2 + ai12*xr2;
   t0 = xr1;
   xr1 = ar11*xr1 - ai11*xi1;
   xi1 = ar11*xi1 + ai11*t0;

   *B   = xr1; B[1] = xi1;
   B[2] = xr2; B[3] = xi2;
   B[4] = xr3; B[5] = xi3;
}
static void trsmLU_4(const int N, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Left, 'Upper', with 1 col prefetch, written with all dependencies shown,
 * so that compiler can optimize.
 * A is known to be 4x4, with 1/alpha already applied, diagonals already
 * inverted
 */
{
   const TYPE ar11=*A, ai11=A[1], ar12=A[8], ai12=A[9], ar13=A[16], ai13=A[17],
              ar14=A[24], ai14=A[25];
   const TYPE ar22=A[10], ai22=A[11], ar23=A[18], ai23=A[19],
              ar24=A[26], ai24=A[27];
   const TYPE ar33=A[20], ai33=A[21], ar34=A[28], ai34=A[29];
   const TYPE ar44=A[30], ai44=A[31];
   TYPE xr1, xi1, xr2, xi2, xr3, xi3, xr4, xi4;
   TYPE t0, p0;
   const int ldb2=ldb+ldb;
   TYPE *bn=B+ldb2;
   const int pfd = ldb2+ldb2;
   int j;

   p0 = B[6];
   for (j=N-1; j; j--)
   {
      xr4 = p0  ; xi4 = B[7];
      xr1 = *B; xi1 = B[1];
      xr3 = B[4]; xi3 = B[5];
      xr2 = B[2]; xi2 = B[3];

      t0 = xr4;
      xr4 = ar44*xr4 - ai44*xi4;
      xi4 = ar44*xi4 + ai44*t0;

      xr3 -= ar34*xr4 - ai34*xi4;
      xi3 -= ar34*xi4 + ai34*xr4;
      t0 = xr3;
      xr3 = ar33*xr3 - ai33*xi3;
      xi3 = ar33*xi3 + ai33*t0;

      xr2 -= ar24*xr4 - ai24*xi4;     p0 = bn[6];
      xi2 -= ar24*xi4 + ai24*xr4;
      xr2 -= ar23*xr3 - ai23*xi3;
      xi2 -= ar23*xi3 + ai23*xr3;
      t0 = xr2;
      xr2 = ar22*xr2 - ai22*xi2;      ATL_pfl1W(bn+pfd);
      xi2 = ar22*xi2 + ai22*t0;       ATL_pfl1W(bn+pfd+4);

      xr1 -= ar14*xr4 - ai14*xi4;
      xi1 -= ar14*xi4 + ai14*xr4;
      xr1 -= ar13*xr3 - ai13*xi3;
      xi1 -= ar13*xi3 + ai13*xr3;
      xr1 -= ar12*xr2 - ai12*xi2;
      xi1 -= ar12*xi2 + ai12*xr2;
      t0 = xr1;
      xr1 = ar11*xr1 - ai11*xi1;
      xi1 = ar11*xi1 + ai11*t0;

      *B   = xr1; B[1] = xi1;
      B[2] = xr2; B[3] = xi2;
      B[4] = xr3; B[5] = xi3;
      B[6] = xr4; B[7] = xi4;
      B = bn;
      bn += ldb2;
   }
   xr4 = p0  ; xi4 = B[7];
   xr1 = *B; xi1 = B[1];
   xr3 = B[4]; xi3 = B[5];
   xr2 = B[2]; xi2 = B[3];

   t0 = xr4;
   xr4 = ar44*xr4 - ai44*xi4;
   xi4 = ar44*xi4 + ai44*t0;

   xr3 -= ar34*xr4 - ai34*xi4;
   xi3 -= ar34*xi4 + ai34*xr4;
   t0 = xr3;
   xr3 = ar33*xr3 - ai33*xi3;
   xi3 = ar33*xi3 + ai33*t0;

   xr2 -= ar24*xr4 - ai24*xi4;
   xi2 -= ar24*xi4 + ai24*xr4;
   xr2 -= ar23*xr3 - ai23*xi3;
   xi2 -= ar23*xi3 + ai23*xr3;
   t0 = xr2;
   xr2 = ar22*xr2 - ai22*xi2;
   xi2 = ar22*xi2 + ai22*t0;

   xr1 -= ar14*xr4 - ai14*xi4;
   xi1 -= ar14*xi4 + ai14*xr4;
   xr1 -= ar13*xr3 - ai13*xi3;
   xi1 -= ar13*xi3 + ai13*xr3;
   xr1 -= ar12*xr2 - ai12*xi2;
   xi1 -= ar12*xi2 + ai12*xr2;
   t0 = xr1;
   xr1 = ar11*xr1 - ai11*xi1;
   xi1 = ar11*xi1 + ai11*t0;

   *B   = xr1; B[1] = xi1;
   B[2] = xr2; B[3] = xi2;
   B[4] = xr3; B[5] = xi3;
   B[6] = xr4; B[7] = xi4;
}
static void trsmLL_2(const int N, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Left', 'Lower', with 1 column prefetch, written with all dependencies
 * shown, so that the compiler can optimize.
 * A is known to be 2x2, with 1/alpha already applied, diagonals already
 * inverted
 */
{
   const TYPE ar11=*A, ai11=A[1], ar21=A[2], ai21=A[3];
   const TYPE ar22=A[6], ai22=A[7];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2;
   TYPE t0, p0;
   TYPE *pBn=B+ldb2;
   const int pfd=ldb2+ldb2;
   int j;

   p0 = *B;
   for (j=N-1; j; j--)
   {
      xr1 = p0; xi1 = B[1];
      xr2 = B[2]; xi2 = B[3];

      t0 = xr1;
      xr1 = ar11 * xr1 - ai11 * xi1;
      xi1 = ar11 * xi1 + ai11 * t0;     p0 = *pBn;

      xr2 -= ar21*xr1 - ai21*xi1;
      xi2 -= ar21*xi1 + ai21*xr1;       ATL_pfl1W(pBn+pfd);
      t0 = xr2;
      xr2 = ar22*xr2 - ai22*xi2;
      xi2 = ar22*xi2 + ai22*t0;

      *B   = xr1; B[1] = xi1;
      B[2] = xr2; B[3] = xi2;
      B = pBn;
      pBn += ldb2;
   }
   xr1 = p0; xi1 = B[1];
   xr2 = B[2]; xi2 = B[3];

   t0 = xr1;
   xr1 = ar11 * xr1 - ai11 * xi1;
   xi1 = ar11 * xi1 + ai11 * t0;

   xr2 -= ar21*xr1 - ai21*xi1;
   xi2 -= ar21*xi1 + ai21*xr1;
   t0 = xr2;
   xr2 = ar22*xr2 - ai22*xi2;
   xi2 = ar22*xi2 + ai22*t0;

   *B   = xr1; B[1] = xi1;
   B[2] = xr2; B[3] = xi2;
}
static void trsmLL_3(const int N, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Left', 'Lower', with 1 column prefetch, written with all dependencies
 * shown, so that the compiler can optimize.
 * A is known to be 3x3, with 1/alpha already applied, diagonals already
 * inverted
 */
{
   const TYPE ar11=*A, ai11=A[1], ar21=A[2], ai21=A[3], ar31=A[4], ai31=A[5];
   const TYPE ar22=A[ 8], ai22=A[ 9], ar32=A[10], ai32=A[11];
   const TYPE ar33=A[16], ai33=A[17];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, xr3, xi3;
   TYPE t0, p0;
   TYPE *pBn=B+ldb2;
   const int pfd=ldb2+ldb2;
   int j;

   p0 = *B;
   for (j=N-1; j; j--)
   {
      xr1 = p0; xi1 = B[1];
      xr3 = B[4]; xi3 = B[5];
      xr2 = B[2]; xi2 = B[3];

      t0 = xr1;
      xr1 = ar11 * xr1 - ai11 * xi1;
      xi1 = ar11 * xi1 + ai11 * t0;

      xr2 -= ar21*xr1 - ai21*xi1;
      xi2 -= ar21*xi1 + ai21*xr1;
      t0 = xr2;
      xr2 = ar22*xr2 - ai22*xi2;
      xi2 = ar22*xi2 + ai22*t0;     p0 = *pBn;

      xr3 -= ar31*xr1 - ai31*xi1;
      xi3 -= ar31*xi1 + ai31*xr1;
      xr3 -= ar32*xr2 - ai32*xi2;      ATL_pfl1W(pBn+pfd);
      xi3 -= ar32*xi2 + ai32*xr2;      ATL_pfl1W(pBn+pfd+4);
      t0 = xr3;
      xr3 = ar33*xr3 - ai33*xi3;
      xi3 = ar33*xi3 + ai33*t0;

      *B   = xr1; B[1] = xi1;
      B[2] = xr2; B[3] = xi2;
      B[4] = xr3; B[5] = xi3;
      B = pBn;
      pBn += ldb2;
   }
   xr1 = p0; xi1 = B[1];
   xr3 = B[4]; xi3 = B[5];
   xr2 = B[2]; xi2 = B[3];

   t0 = xr1;
   xr1 = ar11 * xr1 - ai11 * xi1;
   xi1 = ar11 * xi1 + ai11 * t0;

   xr2 -= ar21*xr1 - ai21*xi1;
   xi2 -= ar21*xi1 + ai21*xr1;
   t0 = xr2;
   xr2 = ar22*xr2 - ai22*xi2;
   xi2 = ar22*xi2 + ai22*t0;

   xr3 -= ar31*xr1 - ai31*xi1;
   xi3 -= ar31*xi1 + ai31*xr1;
   xr3 -= ar32*xr2 - ai32*xi2;
   xi3 -= ar32*xi2 + ai32*xr2;
   t0 = xr3;
   xr3 = ar33*xr3 - ai33*xi3;
   xi3 = ar33*xi3 + ai33*t0;

   *B   = xr1; B[1] = xi1;
   B[2] = xr2; B[3] = xi2;
   B[4] = xr3; B[5] = xi3;
}
static void trsmLL_4(const int N, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Left', 'Lower', with 1 column prefetch, written with all dependencies
 * shown, so that the compiler can optimize.
 * A is known to be 4x4, with 1/alpha already applied, diagonals already
 * inverted
 */
{
   const TYPE ar11=*A, ai11=A[1], ar21=A[2], ai21=A[3], ar31=A[4], ai31=A[5],
              ar41=A[6], ai41=A[7];
   const TYPE ar22=A[10], ai22=A[11], ar32=A[12], ai32=A[13],
              ar42=A[14], ai42=A[15];
   const TYPE ar33=A[20], ai33=A[21], ar43=A[22], ai43=A[23];
   const TYPE ar44=A[30], ai44=A[31];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, xr3, xi3, xr4, xi4;
   TYPE t0, p0;
   TYPE *pBn=B+ldb2;
   const int pfd = ldb2+ldb2;
   int j;

   p0 = *B;
   for (j=N-1; j; j--)
   {
      xr1 = p0; xi1 = B[1];
      xr3 = B[4]; xi3 = B[5];
      xr2 = B[2]; xi2 = B[3];
      xr4 = B[6]; xi4 = B[7];

      t0 = xr1;
      xr1 = ar11 * xr1 - ai11 * xi1;
      xi1 = ar11 * xi1 + ai11 * t0;

      xr2 -= ar21*xr1 - ai21*xi1;
      xi2 -= ar21*xi1 + ai21*xr1;
      t0 = xr2;
      xr2 = ar22*xr2 - ai22*xi2;
      xi2 = ar22*xi2 + ai22*t0;

      xr3 -= ar31*xr1 - ai31*xi1;
      xi3 -= ar31*xi1 + ai31*xr1;
      xr3 -= ar32*xr2 - ai32*xi2;     p0 = *pBn;
      xi3 -= ar32*xi2 + ai32*xr2;
      t0 = xr3;
      xr3 = ar33*xr3 - ai33*xi3;
      xi3 = ar33*xi3 + ai33*t0;

      xr4 -= ar41*xr1 - ai41*xi1;     ATL_pfl1W(pBn+pfd);
      xi4 -= ar41*xi1 + ai41*xr1;     ATL_pfl1W(pBn+pfd+4);
      xr4 -= ar42*xr2 - ai42*xi2;
      xi4 -= ar42*xi2 + ai42*xr2;
      xr4 -= ar43*xr3 - ai43*xi3;
      xi4 -= ar43*xi3 + ai43*xr3;
      t0 = xr4;
      xr4 = ar44*xr4 - ai44*xi4;
      xi4 = ar44*xi4 + ai44*t0;
      *B   = xr1; B[1] = xi1;
      B[2] = xr2; B[3] = xi2;
      B[4] = xr3; B[5] = xi3;
      B[6] = xr4; B[7] = xi4;
      B = pBn;
      pBn += ldb2;
   }
   xr1 = p0; xi1 = B[1];
   xr3 = B[4]; xi3 = B[5];
   xr2 = B[2]; xi2 = B[3];
   xr4 = B[6]; xi4 = B[7];

   t0 = xr1;
   xr1 = ar11 * xr1 - ai11 * xi1;
   xi1 = ar11 * xi1 + ai11 * t0;

   xr2 -= ar21*xr1 - ai21*xi1;
   xi2 -= ar21*xi1 + ai21*xr1;
   t0 = xr2;
   xr2 = ar22*xr2 - ai22*xi2;
   xi2 = ar22*xi2 + ai22*t0;

   xr3 -= ar31*xr1 - ai31*xi1;
   xi3 -= ar31*xi1 + ai31*xr1;
   xr3 -= ar32*xr2 - ai32*xi2;
   xi3 -= ar32*xi2 + ai32*xr2;
   t0 = xr3;
   xr3 = ar33*xr3 - ai33*xi3;
   xi3 = ar33*xi3 + ai33*t0;

   xr4 -= ar41*xr1 - ai41*xi1;
   xi4 -= ar41*xi1 + ai41*xr1;
   xr4 -= ar42*xr2 - ai42*xi2;
   xi4 -= ar42*xi2 + ai42*xr2;
   xr4 -= ar43*xr3 - ai43*xi3;
   xi4 -= ar43*xi3 + ai43*xr3;
   t0 = xr4;
   xr4 = ar44*xr4 - ai44*xi4;
   xi4 = ar44*xi4 + ai44*t0;
   *B   = xr1; B[1] = xi1;
   B[2] = xr2; B[3] = xi2;
   B[4] = xr3; B[5] = xi3;
   B[6] = xr4; B[7] = xi4;
}
#endif

#ifdef Right_
static void trsmRU_2(const int M, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Right', 'Upper', written with all dependencies shown, so that the
 * compiler can optimize.  A is known to be 2x2, with 1/alpha already applied,
 * diagonals already inverted.
 */
{
   const TYPE ar11=*A, ai11=A[1], ar12=A[4], ai12=A[5];
   const TYPE ar22=A[6], ai22=A[7];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, t0;
   TYPE *pB0=B, *pB1 = B+ldb2;
   int i;
   #define PFD 8

   for (i=M; i; i--)
   {
      xr1 = *pB0; xr2 = *pB1;
      xi1 = pB0[1]; xi2 = pB1[1];
/*
 *    real sequence:
 *    x1 *= a11;
 *    x2 = (x2 - x1*a12) * a22;
 */
      t0 = xr1;
      xr1 = xr1*ar11 - xi1*ai11;
      xi1 = t0 *ai11 + xi1*ar11;

      xr2 -= xr1*ar12 - xi1*ai12;
      xi2 -= xr1*ai12 + xi1*ar12;     ATL_pfl1W(pB0+PFD);
      t0 = xr2;                       ATL_pfl1W(pB1+PFD);
      xr2 = xr2*ar22 - xi2*ai22;
      xi2 = t0 *ai22 + xi2*ar22;

      *pB0 = xr1; pB0[1] = xi1; pB0 += 2;
      *pB1 = xr2; pB1[1] = xi2; pB1 += 2;
   }
   #undef PFD
}

static void trsmRU_3(const int M, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Right', 'Upper', written with all dependencies shown, so that the
 * compiler can optimize.  A is known to be 3x3, with 1/alpha already applied,
 * diagonals already inverted.
 */
{
   const TYPE ar11=*A, ai11=A[1], ar12=A[6], ai12=A[7], ar13=A[12], ai13=A[13];
   const TYPE ar22=A[ 8], ai22=A[ 9], ar23=A[14], ai23=A[15];
   const TYPE ar33=A[16], ai33=A[17];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, xr3, xi3, t0;
   TYPE *pB0=B, *pB1 = B+ldb2, *pB2=pB1+ldb2;
   int i;
   #define PFD 8

   for (i=M; i; i--)
   {
      xr1 = *pB0; xr2 = *pB1; xr3 = *pB2;
      xi1 = pB0[1]; xi2 = pB1[1]; xi3 = pB2[1];
/*
 *    real sequence:
 *    x1 *= a11;
 *    x2 = (x2 - x1*a12) * a22;
 *    x3 = (x3 - x1*a13 - x2*a23) * a33;
 */
      t0 = xr1;
      xr1 = xr1*ar11 - xi1*ai11;
      xi1 = t0 *ai11 + xi1*ar11;

      xr2 -= xr1*ar12 - xi1*ai12;
      xi2 -= xr1*ai12 + xi1*ar12;
      t0 = xr2;
      xr2 = xr2*ar22 - xi2*ai22;     ATL_pfl1W(pB0+PFD);
      xi2 = t0 *ai22 + xi2*ar22;     ATL_pfl1W(pB1+PFD);

      xr3 -= xr1*ar13 - xi1*ai13;
      xi3 -= xr1*ai13 + xi1*ar13;
      xr3 -= xr2*ar23 - xi2*ai23;
      xi3 -= xr2*ai23 + xi2*ar23;     ATL_pfl1W(pB2+PFD);
      t0 = xr3;
      xr3 = xr3*ar33 - xi3*ai33;
      xi3 = t0 *ai33 + xi3*ar33;

      *pB0 = xr1; pB0[1] = xi1; pB0 += 2;
      *pB1 = xr2; pB1[1] = xi2; pB1 += 2;
      *pB2 = xr3; pB2[1] = xi3; pB2 += 2;
   }
   #undef PFD
}

static void trsmRU_4(const int M, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Right', 'Upper', written with all dependencies shown, so that the
 * compiler can optimize.  A is known to be 4x4, with 1/alpha already applied,
 * diagonals already inverted.
 */
{
   const TYPE ar11=*A, ai11=A[1], ar12=A[8], ai12=A[9], ar13=A[16], ai13=A[17],
              ar14=A[24], ai14=A[25];
   const TYPE ar22=A[10], ai22=A[11], ar23=A[18], ai23=A[19],
              ar24=A[26], ai24=A[27];
   const TYPE ar33=A[20], ai33=A[21], ar34=A[28], ai34=A[29];
   const TYPE ar44=A[30], ai44=A[31];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, xr3, xi3, xr4, xi4, t0;
   TYPE *pB0=B, *pB1 = B+ldb2, *pB2=pB1+ldb2, *pB3=pB2+ldb2;
   int i;
   #define PFD 8

   for (i=M; i; i--)
   {
      xr1 = *pB0; xr2 = *pB1; xr3 = *pB2; xr4 = *pB3;
      xi1 = pB0[1]; xi2 = pB1[1]; xi3 = pB2[1]; xi4 = pB3[1];
/*
 *    real sequence:
 *    x1 *= a11;
 *    x2 = (x2 - x1*a12) * a22;
 *    x3 = (x3 - x1*a13 - x2*a23) * a33;
 *    x4 = (x4 - x1*a14 - x2*a24 - x3*a34) * a44;
 */
      t0 = xr1;
      xr1 = xr1*ar11 - xi1*ai11;
      xi1 = t0 *ai11 + xi1*ar11;

      xr2 -= xr1*ar12 - xi1*ai12;
      xi2 -= xr1*ai12 + xi1*ar12;
      t0 = xr2;
      xr2 = xr2*ar22 - xi2*ai22;
      xi2 = t0 *ai22 + xi2*ar22;

      xr3 -= xr1*ar13 - xi1*ai13;
      xi3 -= xr1*ai13 + xi1*ar13;
      xr3 -= xr2*ar23 - xi2*ai23;     ATL_pfl1W(pB0+PFD);
      xi3 -= xr2*ai23 + xi2*ar23;     ATL_pfl1W(pB1+PFD);
      t0 = xr3;
      xr3 = xr3*ar33 - xi3*ai33;
      xi3 = t0 *ai33 + xi3*ar33;

      xr4 -= xr1*ar14 - xi1*ai14;
      xi4 -= xr1*ai14 + xi1*ar14;
      xr4 -= xr2*ar24 - xi2*ai24;
      xi4 -= xr2*ai24 + xi2*ar24;
      xr4 -= xr3*ar34 - xi3*ai34;     ATL_pfl1W(pB2+PFD);
      xi4 -= xr3*ai34 + xi3*ar34;     ATL_pfl1W(pB3+PFD);
      t0 = xr4;
      xr4 = xr4*ar44 - xi4*ai44;
      xi4 = t0 *ai44 + xi4*ar44;

      *pB0 = xr1; pB0[1] = xi1; pB0 += 2;
      *pB1 = xr2; pB1[1] = xi2; pB1 += 2;
      *pB2 = xr3; pB2[1] = xi3; pB2 += 2;
      *pB3 = xr4; pB3[1] = xi4; pB3 += 2;
   }
   #undef PFD
}

static void trsmRL_2(const int M, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Right', 'Lower', written with all dependencies shown, so that the
 * compiler can optimize.  A is known to be 2x2, with 1/alpha already applied,
 * diagonals already inverted.
 */
{
   const TYPE ar11=*A, ai11=A[1], ar21=A[2], ai21=A[3];
   const TYPE ar22=A[6], ai22=A[7];
   const int ldb2 = ldb+ldb;
   TYPE *pB0=B, *pB1 = B+ldb2;
   TYPE xr1, xi1, xr2, xi2, t0;
   int i;
   #define PFD 8

   for (i=M; i; i--)
   {
      xr2 = *pB1; xr1 = *pB0;
      xi2 = pB1[1]; xi1 = pB0[1];
/*
 *    REAL SEQUENCE :
 *
 *    x2 *= a22;
 *    x1 = (x1 - x2*a21) * a11;
 */
      t0 = xr2;
      xr2 = xr2*ar22 - xi2*ai22;
      xi2 = t0 *ai22 + xi2*ar22;

      xr1 -= xr2*ar21 - xi2*ai21; ATL_pfl1W(pB1+PFD);
      xi1 -= xr2*ai21 + xi2*ar21; ATL_pfl1W(pB0+PFD);
      t0 = xr1;
      xr1 = xr1*ar11 - xi1*ai11;
      xi1 = t0 *ai11 + xi1*ar11;
      *pB1 = xr2; pB1[1] = xi2; pB1 += 2;
      *pB0 = xr1; pB0[1] = xi1; pB0 += 2;
   }
   #undef PFD
}
static void trsmRL_3(const int M, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Right', 'Lower', written with all dependencies shown, so that the
 * compiler can optimize.  A is known to be 3x3, with 1/alpha already applied,
 * diagonals already inverted.
 */
{
   const TYPE ar11=*A, ai11=A[1], ar21=A[2], ai21=A[3], ar31=A[4], ai31=A[5];
   const TYPE ar22=A[ 8], ai22=A[ 9], ar32=A[10], ai32=A[11];
   const TYPE ar33=A[16], ai33=A[17];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, xr3, xi3, t0;
   TYPE *pB0=B, *pB1 = B+ldb2, *pB2=pB1+ldb2;
   int i;
   #define PFD 8

   for (i=M; i; i--)
   {
      xr3 = *pB2; xr2 = *pB1; xr1 = *pB0;
      xi3 = pB2[1]; xi2 = pB1[1]; xi1 = pB0[1];
/*
 *    REAL SEQUENCE :
 *
 *    x3 *= a33;
 *    x2 = (x2 - x3*a32) * a22;
 *    x1 = (x1 - x3*a31 - x2*a21) * a11;
 */
      t0 = xr3;
      xr3 = ar33*xr3 - ai33*xi3;
      xi3 = ar33*xi3 + ai33*t0;

      xr2 -= xr3*ar32 - xi3*ai32;
      xi2 -= xr3*ai32 + xi3*ar32;
      t0 = xr2;
      xr2 = xr2*ar22 - xi2*ai22; ATL_pfl1W(pB2+PFD);
      xi2 = t0 *ai22 + xi2*ar22; ATL_pfl1W(pB1+PFD);

      xr1 -= xr3*ar31 - xi3*ai31;
      xi1 -= xr3*ai31 + xi3*ar31;
      xr1 -= xr2*ar21 - xi2*ai21;
      xi1 -= xr2*ai21 + xi2*ar21; /* ATL_pfl1W(pB0+PFD); */
      t0 = xr1;
      xr1 = xr1*ar11 - xi1*ai11;
      xi1 = t0 *ai11 + xi1*ar11;
      *pB2 = xr3; pB2[1] = xi3; pB2 += 2;
      *pB1 = xr2; pB1[1] = xi2; pB1 += 2;
      *pB0 = xr1; pB0[1] = xi1; pB0 += 2;
   }
   #undef PFD
}

static void trsmRL_4(const int M, const TYPE *A, TYPE *B, const int ldb)
/*
 * 'Right', 'Lower', written with all dependencies shown, so that the
 * compiler can optimize.  A is known to be 4x4, with 1/alpha already applied,
 * diagonals already inverted.
 */
{
   const TYPE ar11=*A, ai11=A[1], ar21=A[2], ai21=A[3], ar31=A[4], ai31=A[5],
              ar41=A[6], ai41=A[7];
   const TYPE ar22=A[10], ai22=A[11], ar32=A[12], ai32=A[13],
              ar42=A[14], ai42=A[15];
   const TYPE ar33=A[20], ai33=A[21], ar43=A[22], ai43=A[23];
   const TYPE ar44=A[30], ai44=A[31];
   const int ldb2 = ldb+ldb;
   TYPE xr1, xi1, xr2, xi2, xr3, xi3, xr4, xi4, t0;
   TYPE *pB0=B, *pB1 = B+ldb2, *pB2=pB1+ldb2, *pB3=pB2+ldb2;
   int i;
   #define PFD 8

   for (i=M; i; i--)
   {
      xr4 = *pB3; xr3 = *pB2; xr2 = *pB1; xr1 = *pB0;
      xi4 = pB3[1]; xi3 = pB2[1]; xi2 = pB1[1]; xi1 = pB0[1];

/*
 *    REAL SEQUENCE :
 *
 *    x4 *= a11;
 *    x3 = (x3 - x4*a43) * a33;
 *    x2 = (x2 - x4*a42 - x3*a32) * a22;
 *    x1 = (x1 - x4*a41 - x3*a31 - x2*a21) * a11;
 */
      t0 = xr4;
      xr4 = ar44*xr4 - ai44*xi4;
      xi4 = ar44*xi4 + ai44*t0;

      xr3 -= xr4*ar43 - xi4*ai43;
      xi3 -= xr4*ai43 + xi4*ar43;
      t0 = xr3;
      xr3 = ar33*xr3 - ai33*xi3;
      xi3 = ar33*xi3 + ai33*t0;

      xr2 -= xr4*ar42 - xi4*ai42;
      xi2 -= xr4*ai42 + xi4*ar42; ATL_pfl1W(pB3+PFD);
      xr2 -= xr3*ar32 - xi3*ai32; ATL_pfl1W(pB2+PFD);
      xi2 -= xr3*ai32 + xi3*ar32;
      t0 = xr2;
      xr2 = xr2*ar22 - xi2*ai22;
      xi2 = t0 *ai22 + xi2*ar22;

      xr1 -= xr4*ar41 - xi4*ai41;
      xi1 -= xr4*ai41 + xi4*ar41;
      xr1 -= xr3*ar31 - xi3*ai31;
      xi1 -= xr3*ai31 + xi3*ar31;
      xr1 -= xr2*ar21 - xi2*ai21; ATL_pfl1W(pB1+PFD);
      xi1 -= xr2*ai21 + xi2*ar21; ATL_pfl1W(pB0+PFD);
      t0 = xr1;
      xr1 = xr1*ar11 - xi1*ai11;
      xi1 = t0 *ai11 + xi1*ar11;
      *pB3 = xr4; pB3[1] = xi4; pB3 += 2;
      *pB2 = xr3; pB2[1] = xi3; pB2 += 2;
      *pB1 = xr2; pB1[1] = xi2; pB1 += 2;
      *pB0 = xr1; pB0[1] = xi1; pB0 += 2;
   }
   #undef PFD
}
#endif

static void trsmcpUN
   (const int N, const TYPE *alpha, const TYPE *A, const int lda, TYPE *C)
/*
 * copies Upper, NoTranspose matrix to Upper, NoTrans matrix, applying 1/alpha
 */
{
   const int lda2 = lda+lda, N2 = N+N;
   const TYPE *a=A+lda2;
   TYPE *c=C+N2;
   TYPE tmp[2];
   int j;

   tmp[0] = *alpha; tmp[1] = alpha[1];
   Mjoin(PATL,cplxinvert)(1, tmp, 1, tmp, 1); /* safe cplx inversion */
/*
 * copy non-diagonal portion of matrix, scaled by 1/alpha
 */
   for (j=1; j < N; j++)
   {
      Mjoin(PATL,cpsc)(j, tmp, a, 1, c, 1);
      a += lda2;
      c += N2;
   }
}

static void trsmcpUT
   (const enum ATLAS_TRANS Trans, const int N, const TYPE *alpha,
    const TYPE *A, const int lda, TYPE *C)
/*
 * copies Upper, Transpose matrix to Lower, NoTrans matrix, applying 1/alpha
 */
{
   const int lda2 = lda+lda;
   const TYPE *a=A+lda2;
   TYPE *c=C+2;
   TYPE tmp[2];
   int j;
   void (*move)(const int N, const SCALAR alpha, const TYPE *X, const int incX,
                TYPE *Y, const int incY);

   if (Trans == AtlasConjTrans) move = Mjoin(PATL,moveConj);
   else move = Mjoin(PATL,cpsc);

   tmp[0] = *alpha; tmp[1] = alpha[1];
   Mjoin(PATL,cplxinvert)(1, tmp, 1, tmp, 1); /* safe cplx inversion */
/*
 * copy non-diagonal portion of matrix, scaled by 1/alpha
 */
   for (j=1; j < N; j++)
   {
      move(j, tmp, a, 1, c, N);
      a += lda2;
      c += 2;
   }
}

static void trsmcpLT(const enum ATLAS_TRANS Trans, const int N,
                     const TYPE *alpha, const TYPE *A, const int lda, TYPE *C)
/*
 * copies Lower, Trans matrix to Upper, Notrans matrix, applies 1/alpha
 */
{
   const int lda2 = lda+lda+2, N2 = N+N+2;
   const TYPE *a=A+2;
   TYPE *c=C+N+N;
   TYPE tmp[2];
   int j;
   void (*move)(const int N, const SCALAR alpha, const TYPE *X, const int incX,
                TYPE *Y, const int incY);

   if (Trans == AtlasConjTrans) move = Mjoin(PATL,moveConj);
   else move = Mjoin(PATL,cpsc);

   tmp[0] = *alpha; tmp[1] = alpha[1];
   Mjoin(PATL,cplxinvert)(1, tmp, 1, tmp, 1); /* safe cplx inversion */
/*
 * copy non-diagonal portion of matrix, scaled by 1/alpha
 */
   for (j=0; j != N; j++)
   {
      move(N-j-1, tmp, a, 1, c, N);
      a += lda2;
      c += N2;
   }
}
static void trsmcpLN(const int N, const TYPE *alpha, const TYPE *A,
                     const int lda, TYPE *C)
/*
 * copies Lower, NoTrans matrix to Lower, Notrans matrix, applies 1/alpha
 */
{
   const int lda2 = lda+lda+2, N2 = N+N+2;
   const TYPE *a=A+2;
   TYPE *c=C+2;
   TYPE tmp[2];
   int j;

   tmp[0] = *alpha; tmp[1] = alpha[1];
   Mjoin(PATL,cplxinvert)(1, tmp, 1, tmp, 1); /* safe cplx inversion */
/*
 * copy non-diagonal portion of matrix, scaled by 1/alpha
 */
   for (j=0; j != N; j++)
   {
      Mjoin(PATL,cpsc)(N-j-1, tmp, a, 1, c, 1);
      a += lda2;
      c += N2;
   }
}

static enum ATLAS_DIAG trsmcopy
   (enum ATLAS_UPLO Uplo, enum ATLAS_TRANS Trans, enum ATLAS_DIAG Diag,
    const int N, const TYPE *alpha, const TYPE *A, const int lda, TYPE *C)
{
   enum ATLAS_DIAG diag=AtlasNonUnit;
   if (Uplo == AtlasLower)
   {
      if (Trans == AtlasNoTrans) trsmcpLN(N, alpha, A, lda, C);
      else trsmcpLT(Trans, N, alpha, A, lda, C);
   }
   else
   {
      if (Trans == AtlasNoTrans) trsmcpUN(N, alpha, A, lda, C);
      else trsmcpUT(Trans, N, alpha, A, lda, C);
   }
/*
 * Handle main diagonal of matrix
 */
   if (Diag == AtlasUnit)
   {
      if (*alpha == ATL_rone && alpha[1] == ATL_rzero) diag = AtlasUnit;
      Mjoin(PATLU,set)(N, *alpha, C, N+N+2);
      Mjoin(PATLU,set)(N, alpha[1], C+1, N+N+2);
   }
   else
   {
      if (Trans != AtlasConjTrans) Mjoin(PATL,copy)(N, A, lda+1, C, N+1);
      else Mjoin(PATL,copyConj)(N, A, lda+1, C, N+1);
      Mjoin(PATL,cplxinvert)(N, C, N+1, C, N+1); /* safe cplx inversion */
      Mjoin(PATL,scal)(N, alpha, C, N+1);
   }
   return(diag);
}

/*
 * In unbelievably beautiful kludge from the depths of hell, reuse the
 * 'Left' code for 'Right' by swapping M & N, and renaming routs; if you
 * must be ill, please don't get it on the code . . .
 */
#ifdef Right_
   #define trsmLU_4 trsmRU_4
   #define trsmLU_3 trsmRU_3
   #define trsmLU_2 trsmRU_2
   #define trsmLL_4 trsmRL_4
   #define trsmLL_3 trsmRL_3
   #define trsmLL_2 trsmRL_2
void Mjoin(PATL,CtrsmKR)
   (enum ATLAS_UPLO Uplo, enum ATLAS_TRANS Trans, enum ATLAS_DIAG Diag,
    const int N, const int M, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *B, const int ldb)
#else
void Mjoin(PATL,CtrsmKL)
   (enum ATLAS_UPLO Uplo, enum ATLAS_TRANS Trans, enum ATLAS_DIAG Diag,
    const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *B, const int ldb)
#endif
{
   TYPE tmp[2], ra, ia;
   void *vp;
   TYPE *a;

   if (N > 0)
   {
      if (M > 1)
      {
         vp = malloc(ATL_Cachelen + ATL_MulBySize(M)*M);
         ATL_assert(vp);
         a = ATL_AlignPtr(vp);
         Diag = trsmcopy(Uplo, Trans, Diag, M, alpha, A, lda, a);
         if (Trans != AtlasNoTrans)
         {
            if (Uplo == AtlasLower) Uplo = AtlasUpper;
            else Uplo = AtlasLower;
         }
         switch(M)
         {
         case 2:
            if (Uplo == AtlasLower) trsmLL_2(N, a, B, ldb);
            else trsmLU_2(N, a, B, ldb);
            break;
         case 3:
            if (Uplo == AtlasLower) trsmLL_3(N, a, B, ldb);
            else trsmLU_3(N, a, B, ldb);
            break;
         case 4:
            if (Uplo == AtlasLower) trsmLL_4(N, a, B, ldb);
            else trsmLU_4(N, a, B, ldb);
            break;
         default: /* this crap should never be used */
               tmp[0] = ATL_rone; tmp[1] = ATL_rzero;
               Mjoin(PATL,cplxinvert)(M, a, M+1, a, M+1);
               Mjoin(PATL,reftrsm)(AtlasLeft, Uplo, AtlasNoTrans, Diag, M, N,
                                   tmp, a, M, B, ldb);
         }
         free(vp);
      }
      else if (M == 1)
      {
         if (Diag == AtlasUnit)
         #ifdef Right_
            Mjoin(PATL,scal)(N, alpha, B, 1);
         #else
            Mjoin(PATL,scal)(N, alpha, B, ldb);
         #endif
         else
         {
            tmp[0] = A[0];
            if (Trans != AtlasConjTrans) tmp[1] = A[1];
            else tmp[1] = -A[1];
            Mjoin(PATL,cplxinvert)(1, tmp, 1, tmp, 1); /* safe cplx invers */
            ra = tmp[0]; ia = tmp[1];
            tmp[0] = *alpha * ra - alpha[1] * ia;
            tmp[1] = *alpha * ia + alpha[1] * ra;
            #ifdef Right_
               Mjoin(PATL,scal)(N, tmp, B, 1);
            #else
               Mjoin(PATL,scal)(N, tmp, B, ldb);
            #endif
         }
      }
   }
}
#ifdef Right_
   #undef  trsmLU_4
   #undef  trsmLU_3
   #undef  trsmLU_2
   #undef  trsmLL_4
   #undef  trsmLL_3
   #undef  trsmLL_2
#endif
