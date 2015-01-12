/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010, 2012 R. Clint Whaley
 *
 * Code contributers : R. Clint Whaley, Antoine P. Petitet
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
#include "atlas_misc.h"
#include "atlas_level1.h"
#include "atlas_kernel2.h"
#include "atlas_lvl2.h"

#include "atlas_reflvl2.h"          /* temporary for building purposes */
#include "atlas_reflevel2.h"        /* used for gbmv, gpmv and gpr.    */
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))
#include "atlas_cacheedge.h"

#ifdef CacheEdge
   #if !defined(CacheEdge) || CacheEdge > 4194304 || CacheEdge == 0
      #define MY_CE (4*ATL_MulBySize(ATL_L1elts))
   #else
      #define MY_CE CacheEdge
   #endif
#else
   #define MY_CE (4*ATL_MulBySize(ATL_L1elts))
#endif

static void ATL_trmvUTNk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,  /* aligned with A */
   TYPE *Y         /* 16-byte aligned */
)
{
   register ATL_INT j;
#ifdef TREAL
   for (j=0; j < N; j++)
      Y[j] += Mjoin(PATL,dot)(j+1, X, 1, A+j*lda, 1);
#else
   for (j=0; j < N; j++)
   {
      const register ATL_INT j2 = j + j;
      register TYPE ry=Y[j2], iy=Y[j2+1];
      Mjoin(PATL,dotu_sub)(j+1, X, 1, A+j2*lda, 1, Y+j2);
      Y[j2] += ry;
      Y[j2+1] += iy;
   }
#endif
}

static void ATL_trmvUTUk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,  /* aligned with A */
   TYPE *Y         /* 16-byte aligned */
)
{
   register ATL_INT j;
#ifdef TREAL
   *Y += *X; A += lda;
   for (j=1; j < N; j++, A += lda)
      Y[j] += X[j] + Mjoin(PATL,dot)(j, X, 1, A, 1);
#else
   register ATL_CINT lda2=lda+lda;
   *Y += *X;
   Y[1] += X[1];
   A += lda2;
   for (j=1; j < N; j++, A += lda2)
   {
      const register ATL_INT j2 = j + j;
      register TYPE ry=Y[j2], iy=Y[j2+1];
      ry += X[j2];
      iy += X[j2+1];
      Mjoin(PATL,dotu_sub)(j, X, 1, A, 1, Y+j2);
      Y[j2] += ry;
      Y[j2+1] += iy;
   }
#endif
}

static void ATL_trmvUNNk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,
   TYPE *Y
)
{
   ATL_INT j;
   #ifdef TREAL
      for (j=0; j < N; j++, A += lda)
         Mjoin(PATL,axpy)(j+1, X[j], A, 1, Y, 1);
   #else
      ATL_CINT lda2=lda+lda;
      for (j=0; j < N; j++, A += lda2)
         Mjoin(PATL,axpy)(j+1, X+j+j, A, 1, Y, 1);
   #endif
}
static void ATL_trmvUNUk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,
   TYPE *Y
)
{
#ifdef TREAL
   ATL_INT j;
   *Y += *X; A += lda;
   for (j=1; j < N; j++, A += lda)
   {
      register TYPE x=X[j];
      Mjoin(PATL,axpy)(j, x, A, 1, Y, 1);
      Y[j] += x;
   }
#else
   register ATL_INT j;
   register ATL_CINT lda2 = lda+lda;
   *Y += *X;
   Y[1] += X[1]; A += lda2;
   for (j=1; j < N; j++, A += lda2)
   {
      register ATL_CINT j2 = j + j;
      Mjoin(PATL,axpy)(j, X+j2, A, 1, Y, 1);
      Y[j2] += X[j2];
      Y[j2+1] += X[j2+1];
   }
#endif
}


static void ATL_trmvLTNk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,  /* aligned with A */
   TYPE *Y         /* 16-byte aligned */
)
{
   register ATL_INT j;
   ATL_CINT ldap1 = lda+1;
#ifdef TREAL
   for (j=0; j < N; j++)
      Y[j] += Mjoin(PATL,dot)(N-j, X+j, 1, A+j*ldap1, 1);
#else
   for (j=0; j < N; j++)
   {
      const register ATL_INT j2 = j + j;
      register TYPE ry=Y[j2], iy=Y[j2+1];
      Mjoin(PATL,dotu_sub)(N-j, X+j2, 1, A+j2*ldap1, 1, Y+j2);
      Y[j2] += ry;
      Y[j2+1] += iy;
   }
#endif
}

static void ATL_trmvLTUk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,  /* aligned with A */
   TYPE *Y         /* 16-byte aligned */
)
{
   register ATL_INT j;
   register ATL_CINT ldap1 = lda+1, Nm1 = N-1;
#ifdef TREAL
   for (j=0; j < Nm1; j++)
   {
      register TYPE t0 = X[j];
      t0 += Mjoin(PATL,dot)(N-j-1, X+j+1, 1, A+1+j*ldap1, 1);
      Y[j] += t0;
   }
   Y[Nm1] += X[Nm1];
#else
   for (j=0; j < Nm1; j++)
   {
      const register ATL_INT j2 = j + j;
      register TYPE ry=Y[j2], iy=Y[j2+1];
      ry += X[j2];
      iy += X[j2+1];
      Mjoin(PATL,dotu_sub)(N-j-1, X+j2+2, 1, A+2+j2*ldap1, 1, Y+j2);
      Y[j2] += ry;
      Y[j2+1] += iy;
   }
   j = Nm1+Nm1;
   Y[j] += X[j];
   Y[j+1] += X[j+1];
#endif
}

static void ATL_trmvLNNk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,
   TYPE *Y
)
{
   ATL_INT j;
   ATL_CINT ldap1 = (lda+1)SHIFT;
   #ifdef TREAL
      for (j=0; j < N; j++)
         Mjoin(PATL,axpy)(N-j, X[j], A+j*ldap1, 1, Y+j, 1);
   #else
      for (j=0; j < N; j++)
         Mjoin(PATL,axpy)(N-j, X+j+j, A+j*ldap1, 1, Y+j+j, 1);
   #endif
}
static void ATL_trmvLNUk
(
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   const TYPE *X,
   TYPE *Y
)
{
#ifdef TREAL
   ATL_INT j;
   ATL_CINT ldap1 = lda+1, Nm1=N-1;
   for (j=0; j < Nm1; j++)
   {
      Y[j] += X[j];
      Mjoin(PATL,axpy)(N-j-1, X[j], A+1+j*ldap1, 1, Y+j+1, 1);
   }
   Y[N-1] += X[N-1];
#else
   ATL_INT j, n=N-1;
   ATL_CINT ldap1 = lda+1, Nm1 = N+N-2;
   for (j=0,n=N-1; n; n--)
   {
      register ATL_CINT jn = j + 2;
      Y[j] += X[j];
      Y[j+1] += X[j+1];
      Mjoin(PATL,axpy)(n, X+j, A+2+j*ldap1, 1, Y+jn, 1);
      j = jn;
   }
   Y[Nm1] += X[Nm1];
   Y[Nm1+1] += X[Nm1+1];
#endif
}

static int ATL_trmvUN
(
   const enum ATLAS_DIAG  Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
/*
 * RETURNS: 0 if TRMV was performed, non-zero if nothing done
 */
{
   static void (*trmvK)(ATL_CINT, const TYPE*, ATL_CINT, const TYPE*, TYPE*);
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp;
   TYPE *x, *y;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      size_t N2=N+N, lda2 = lda+lda;
      TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
   #endif
   const size_t incA = ((size_t)lda2)*nb;
   ATL_INT j;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvN);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvN_L1) :
             Mjoin(PATL,gemvN_L2);
   trmvK = (Diag == AtlasNonUnit) ? ATL_trmvUNNk : ATL_trmvUNUk;
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
   {
      ATL_INT i;
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      y = X;
      for (i=0; i < N2; i++)
      {
         x[i] = X[i];
         X[i] = ATL_rzero;
      }

   }
   else  /* allocate both X and Y */
   {
      vp = malloc((ATL_Cachelen+ATL_MulBySize(N))<<1);
      if (!vp)
         return(3);
      x = ATL_AlignPtr(vp);
      y = x + N2;
      y = ATL_AlignPtr(y);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
      Mjoin(PATL,zero)(N, y, 1);
   }
   trmvK(nb, A, lda, x, y);
   A += incA;
   for (j=nb; j < N; j += nb, A += incA)
   {
      int kb = N-j;
      #ifdef TCPLX
         const register size_t j2 = j + j;
      #else
         #define j2 j
      #endif
      kb = Mmin(nb, kb);
      gemv(j, kb, one, A, lda, x+j2, 1, one, y, 1);
      trmvK(kb, A+j2, lda, x+j2, y+j2);
      #ifndef TCPLX
         #undef j2
      #endif
   }
   if (y != X)
      Mjoin(PATL,copy)(N, y, 1, X, incX);
   free(vp);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef one
#endif

static int ATL_trmvUT
(
   const enum ATLAS_DIAG  Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
/*
 * RETURNS: 0 if TRMV was performed, non-zero if nothing done
 */
{
   static void (*trmvK)(ATL_CINT, const TYPE*, ATL_CINT, const TYPE*, TYPE*);
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp;
   TYPE *x, *y;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      size_t N2=N+N, lda2 = lda+lda;
      TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
   #endif
   const size_t incA = ((size_t)lda2)*nb;
   ATL_INT j;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvT);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvT_L1) :
             Mjoin(PATL,gemvT_L2);
   trmvK = (Diag == AtlasNonUnit) ? ATL_trmvUTNk : ATL_trmvUTUk;
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
   {
      ATL_INT i;
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      y = X;
      for (i=0; i < N2; i++)
      {
         x[i] = X[i];
         X[i] = ATL_rzero;
      }

   }
   else  /* allocate both X and Y */
   {
      vp = malloc((ATL_Cachelen+ATL_MulBySize(N))<<1);
      if (!vp)
         return(3);
      x = ATL_AlignPtr(vp);
      y = x + N2;
      y = ATL_AlignPtr(y);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
      Mjoin(PATL,zero)(N, y, 1);
   }
   trmvK(nb, A, lda, x, y);
   A += incA;
   for (j=nb; j < N; j += nb, A += incA)
   {
      int kb = N-j;
      #ifdef TCPLX
         const register size_t j2 = j + j;
      #else
         #define j2 j
      #endif
      kb = Mmin(nb, kb);
      gemv(j, kb, one, A, lda, x, 1, one, y+j2, 1);
      trmvK(kb, A+j2, lda, x+j2, y+j2);
      #ifndef TCPLX
         #undef j2
      #endif
   }
   if (y != X)
      Mjoin(PATL,copy)(N, y, 1, X, incX);
   free(vp);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef one
#endif

static int ATL_trmvLN
(
   const enum ATLAS_DIAG  Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
/*
 * RETURNS: 0 if TRMV was performed, non-zero if nothing done
 */
{
   static void (*trmvK)(ATL_CINT, const TYPE*, ATL_CINT, const TYPE*, TYPE*);
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp;
   TYPE *x, *y;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      size_t N2=N+N, lda2 = lda+lda;
      TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
   #endif
   const size_t incA = ((size_t)lda+1)*(nb SHIFT);
   ATL_CINT Nnb = ((N-1)/nb)*nb, Nr = N-Nnb;
   ATL_INT j;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvN);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvN_L1) :
             Mjoin(PATL,gemvN_L2);
   trmvK = (Diag == AtlasNonUnit) ? ATL_trmvLNNk : ATL_trmvLNUk;
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
   {
      ATL_INT i;
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      y = X;
      for (i=0; i < N2; i++)
      {
         x[i] = X[i];
         X[i] = ATL_rzero;
      }

   }
   else  /* allocate both X and Y */
   {
      vp = malloc((ATL_Cachelen+ATL_MulBySize(N))<<1);
      if (!vp)
         return(3);
      x = ATL_AlignPtr(vp);
      y = x + N2;
      y = ATL_AlignPtr(y);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
      Mjoin(PATL,zero)(N, y, 1);
   }
   for (j=0; j < Nnb; j += nb, A += incA)
   {
      #ifdef TCPLX
         const register size_t j2=j+j, nb2=nb+nb;
      #else
         #define j2 j
         #define nb2 nb
      #endif
      trmvK(nb, A, lda, x+j2, y+j2);
      gemv(N-j-nb, nb, one, A+nb2, lda, x+j2, 1, one, y+j2+nb2, 1);
      #ifndef TCPLX
         #undef j2
         #undef nb2
      #endif
   }
   #ifdef TCPLX
      j += j;
   #endif
   trmvK(Nr, A, lda, x+j, y+j);
   if (y != X)
      Mjoin(PATL,copy)(N, y, 1, X, incX);
   free(vp);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef one
#endif

static int ATL_trmvLT
(
   const enum ATLAS_DIAG  Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
/*
 * RETURNS: 0 if TRMV was performed, non-zero if nothing done
 */
{
   static void (*trmvK)(ATL_CINT, const TYPE*, ATL_CINT, const TYPE*, TYPE*);
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp;
   TYPE *x, *y;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      size_t N2=N+N, lda2 = lda+lda;
      TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
   #endif
   const size_t incA = ((size_t)lda+1)*(nb SHIFT);
   ATL_CINT Nnb = ((N-1)/nb)*nb, Nr = N-Nnb;
   ATL_INT j;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvT);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvT_L1) :
             Mjoin(PATL,gemvT_L2);
   trmvK = (Diag == AtlasNonUnit) ? ATL_trmvLTNk : ATL_trmvLTUk;
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
   {
      ATL_INT i;
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      y = X;
      for (i=0; i < N2; i++)
      {
         x[i] = X[i];
         X[i] = ATL_rzero;
      }

   }
   else  /* allocate both X and Y */
   {
      vp = malloc((ATL_Cachelen+ATL_MulBySize(N))<<1);
      if (!vp)
         return(3);
      x = ATL_AlignPtr(vp);
      y = x + N2;
      y = ATL_AlignPtr(y);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
      Mjoin(PATL,zero)(N, y, 1);
   }
   for (j=0; j < Nnb; j += nb, A += incA)
   {
      #ifdef TCPLX
         const register size_t j2=j+j, nb2=nb+nb;
      #else
         #define j2 j
         #define nb2 nb
      #endif
      trmvK(nb, A, lda, x+j2, y+j2);
      gemv(N-j-nb, nb, one, A+nb2, lda, x+j2+nb2, 1, one, y+j2, 1);
      #ifndef TCPLX
         #undef j2
         #undef nb2
      #endif
   }
   #ifdef TCPLX
      j += j;
   #endif
   trmvK(Nr, A, lda, x+j, y+j);
   if (y != X)
      Mjoin(PATL,copy)(N, y, 1, X, incX);
   free(vp);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef one
#endif
void Mjoin(PATL,trmv)
(
   const enum ATLAS_UPLO  Uplo,
   const enum ATLAS_TRANS Trans,
   const enum ATLAS_DIAG  Diag,
   const int              N,
   const TYPE             *A,
   const int              lda,
   TYPE                   *X,
   const int              incX
)
/*
 * Purpose
 * =======
 *
 * Mjoin( PATL, trmv ) performs one of the matrix-vector operations
 *
 *    x := A * x,   or   x := conjg( A  ) * x,   or
 *
 *    x := A'* x,   or   x := conjg( A' ) * x,
 *
 * where x is an n-element vector and  A is an n by n unit, or non-unit,
 * upper or lower triangular matrix.
 *
 * This is a blocked version of the algorithm.  For a more detailed des-
 * cription of  the arguments of this function, see the reference imple-
 * mentation in the ATLAS/src/blas/reference directory.
 *
 * ---------------------------------------------------------------------
 */
{
   int ierr=1;
   #define NBMV 120   /* LCM(1,2,3,4,5,6,8); no NU cleanup most kerns */
/*
 * Small-case code just calls reference to avoid copies and setup costs
 */
   if (N < NBMV+NBMV)
   {
      Mjoin(PATL,reftrmv)(Uplo, Trans, Diag, N, A, lda, X, incX);
      return;
   }
/*
 * Handle conjugate transpose by conjugating input/output vector
 */
   #ifdef TCPLX
   if (Trans == AtlasConjTrans || Trans == AtlasConj)
   {
      Mjoin(PATLU,scal)(N, ATL_rnone, X+1, incX+incX);
      Mjoin(PATL,trmv)(Uplo, (Trans == AtlasConj) ? AtlasNoTrans:AtlasTrans,
                       Diag, N, A, lda, X, incX);
      Mjoin(PATLU,scal)(N, ATL_rnone, X+1, incX+incX);
      return;
   }
   #endif
   if (Uplo == AtlasUpper)
   {
      if (Trans == AtlasNoTrans)
         ierr = ATL_trmvUN(Diag, NBMV, N, A, lda, X, incX);
      else
         ierr = ATL_trmvUT(Diag, NBMV, N, A, lda, X, incX);
   }
   else if (Trans == AtlasNoTrans)
      ierr = ATL_trmvLN(Diag, NBMV, N, A, lda, X, incX);
   else /* if (Trans == AtlasTrans) */
      ierr = ATL_trmvLT(Diag, NBMV, N, A, lda, X, incX);
   if (ierr)
      Mjoin(PATL,reftrmv)(Uplo, Trans, Diag, N, A, lda, X, incX);
}
