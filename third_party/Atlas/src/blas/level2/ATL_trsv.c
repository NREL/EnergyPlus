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


static int ATL_trsvUN_k
(
   const enum ATLAS_DIAG Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
{
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp=NULL;
   TYPE *x, *a;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      size_t N2=N+N, lda2 = lda+lda;
      TYPE one[2] = {ATL_rone, ATL_rzero}, none[2] = {ATL_rnone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
      #define none ATL_rnone
   #endif
   const size_t incA = ((size_t)lda2)*nb;
   ATL_CINT Nnb = ((N-1)/nb)*nb, Nr = N-Nnb;
   ATL_INT j;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvN);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvN_L1) :
             Mjoin(PATL,gemvN_L2);
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
      x = X;
   else  /* allocate aligned X to hopefully avoid GEMV copying */
   {
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
   }

   a = A + ((size_t)lda2)*(N-nb);
   for (j=N-nb; j >= Nr; j -= nb, a -= incA)
   {
      #ifdef TCPLX
         const register size_t j2 = j + j;
      #else
         #define j2 j
      #endif
      Mjoin(PATL,reftrsv)(AtlasUpper, AtlasNoTrans, Diag, nb, a+j2, lda,
                          x+j2, 1);
      Mjoin(PATL,scal)(nb, none, x+j2, 1);
      gemv(j, nb, one, a, lda, x+j2, 1, one, x, 1);
      #ifndef TCPLX
         #undef j2
      #endif
   }
   Mjoin(PATL,reftrsv)(AtlasUpper, AtlasNoTrans, Diag, Nr, A, lda, x, 1);
   if (x != X)
   {
      Mjoin(PATL,scal)(Nr, none, x, 1);
      Mjoin(PATL,cpsc)(N, none, x, 1, X, incX);
      free(vp);
   }
   else
      Mjoin(PATL,scal)(Nnb, none, x+(Nr SHIFT), 1);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef one
   #undef none
#endif

static int ATL_trsvUT_k
(
   const enum ATLAS_DIAG Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
{
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp=NULL;
   TYPE *x;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      size_t N2=N+N, lda2 = lda+lda;
      const int nb2 = nb+nb;
      TYPE one[2] = {ATL_rone, ATL_rzero}, none[2] = {ATL_rnone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
      #define none ATL_rnone
      #define nb2 nb
   #endif
   const size_t incA = ((size_t)lda2)*nb;
   ATL_CINT Nnb = ((N-1)/nb)*nb, Nr = N-Nnb;
   ATL_INT j;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvT);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvT_L1) :
             Mjoin(PATL,gemvT_L2);
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
      x = X;
   else  /* allocate aligned X to hopefully avoid GEMV copying */
   {
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
   }

   Mjoin(PATL,reftrsv)(AtlasUpper, AtlasTrans, Diag, Mmin(N,nb), A, lda, x, 1);
   A += incA;
   for (j=nb; j < N; j += nb, A += incA)
   {
      #ifdef TCPLX
         const register size_t j2 = j + j;
      #else
         #define j2 j
      #endif
      register int kb = N-j;
      kb = (kb >= nb) ? nb : kb;
      Mjoin(PATL,scal)(nb, none, x+j2-nb2, 1);
      gemv(j, kb, one, A, lda, x, 1, one, x+j2, 1);
      Mjoin(PATL,reftrsv)(AtlasUpper, AtlasTrans, Diag, kb, A+j2, lda,
                          x+j2, 1);
      #ifndef TCPLX
         #undef j2
      #endif
   }
   j = Nnb SHIFT;
   if (x != X)
   {
      if (Nnb)
      {
         Mjoin(PATL,scal)(Nr, none, x+j, 1);
         Mjoin(PATL,cpsc)(N, none, x, 1, X, incX);
      }
      else
         Mjoin(PATL,copy)(N, x, 1, X, incX);
      free(vp);
   }
   else if (Nnb)
      Mjoin(PATL,scal)(Nnb, none, x, 1);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef nb2
   #undef lda2
   #undef one
   #undef none
#endif

static int ATL_trsvLN_k
(
   const enum ATLAS_DIAG Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
{
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp=NULL;
   TYPE *x, *a;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      const size_t N2=N+N, lda2 = lda+lda, nb2 = nb+nb;
      const TYPE one[2] = {ATL_rone, ATL_rzero},
                 none[2] = {ATL_rnone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
      #define none ATL_rnone
      #define nb2 nb
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
/*
 * If X is aligned to Cachelen wt inc=1, use it as y
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
      x = X;
   else  /* allocate aligned X to hopefully avoid GEMV copying */
   {
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
   }

   for (j=0; j < Nnb; j += nb, A += incA)
   {
      #ifdef TCPLX
         const register size_t j2 = j + j;
      #else
         #define j2 j
      #endif
      Mjoin(PATL,reftrsv)(AtlasLower, AtlasNoTrans, Diag, nb, A, lda, x+j2, 1);
      Mjoin(PATL,scal)(nb, none, x+j2, 1);
      gemv(N-j-nb, nb, one, A+nb2, lda, x+j2, 1, one, x+j2+nb2, 1);
      #ifndef TCPLX
         #undef j2
      #endif
   }
   #ifdef TCPLX
      j += j;
   #endif
   Mjoin(PATL,reftrsv)(AtlasLower, AtlasNoTrans, Diag, Nr, A, lda, x+j, 1);
   if (x != X)
   {
      Mjoin(PATL,scal)(Nr, none, x+j, 1);
      Mjoin(PATL,cpsc)(N, none, x, 1, X, incX);
      free(vp);
   }
   else
      Mjoin(PATL,scal)(Nnb, none, x, 1);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef nb2
   #undef one
   #undef none
#endif

static int ATL_trsvLT_k
(
   const enum ATLAS_DIAG Diag,
   const int nb,
   ATL_CINT N,
   const TYPE *A,
   ATL_CINT lda,
   TYPE *X,
   ATL_CINT incX
)
{
   void (*gemv)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*, ATL_CINT,
                const TYPE*, ATL_CINT, const SCALAR, TYPE*, ATL_CINT);
   void *vp=NULL;
   TYPE *x, *a;
   const size_t opsize = (N*N+N+N)*sizeof(TYPE)SHIFT;
   size_t t0;
   #ifdef TCPLX
      const size_t N2=N+N, lda2 = lda+lda, nb2 = nb+nb;
      const TYPE one[2] = {ATL_rone, ATL_rzero},
                 none[2] = {ATL_rnone, ATL_rzero};
   #else
      #define N2 N
      #define lda2 lda
      #define one ATL_rone
      #define none ATL_rnone
      #define nb2 nb
   #endif
   const size_t incA = ((size_t)lda+1)*(nb SHIFT);
   ATL_CINT Nnb = ((N-1)/nb)*nb, Nr = N-Nnb;
   ATL_INT j;
   int pnb;

   if (N < nb+nb)
      return(1);
   if (opsize > MY_CE)
      gemv = Mjoin(PATL,gemvT);
   else
      gemv = (opsize <= ATL_MulBySize(ATL_L1elts)) ? Mjoin(PATL,gemvT_L1) :
             Mjoin(PATL,gemvT_L2);
/*
 * If X is aligned to Cachelen wt inc=1, use it as x
 */
   t0 = (size_t) X;
   if (incX == 1 && (ATL_MulByCachelen(ATL_DivByCachelen(t0)) == t0))
      x = X;
   else  /* allocate aligned X to hopefully avoid GEMV copying */
   {
      vp = malloc(ATL_Cachelen+ATL_MulBySize(N));
      if (!vp)
         return(2);
      x = ATL_AlignPtr(vp);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
   }
   j = (N-Nr)SHIFT;
   A += (lda+1)*j;
   Mjoin(PATL,reftrsv)(AtlasLower, AtlasTrans, Diag, Nr, A, lda, x+j, 1);
   pnb = Nr;
   A -= incA;

   for (j=N-Nr-nb; j >= 0; j -= nb, A -= incA)
   {
      #ifdef TCPLX
         const register size_t j2 = j + j;
      #else
         #define j2 j
      #endif
      Mjoin(PATL,scal)(pnb, none, x+j2+nb2, 1);
      pnb = nb;
      gemv(N-j-nb, nb, one, A+nb2, lda, x+j2+nb2, 1, one, x+j2, 1);
      Mjoin(PATL,reftrsv)(AtlasLower, AtlasTrans, Diag, nb, A, lda, x+j2, 1);
      #ifndef TCPLX
         #undef j2
      #endif
   }
   if (x != X)
   {
      if (Nnb)
      {
         Mjoin(PATL,scal)(nb, none, x, 1);
         Mjoin(PATL,cpsc)(N, none, x, 1, X, incX);
      }
      else
         Mjoin(PATL,copy)(N, x, 1, X, incX);
      free(vp);
   }
   else if (Nnb)
      Mjoin(PATL,scal)(N-nb, none, x+nb2, 1);
   return(0);
}
#ifndef TCPLX
   #undef N2
   #undef lda2
   #undef nb2
   #undef one
   #undef none
#endif

void Mjoin(PATL,trsv)
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
 * Mjoin( PATL, trsv ) solves one of the systems of equations
 *
 *    A * x = b,   or   conjg( A  ) * x = b,   or
 *
 *    A'* x = b,   or   conjg( A' ) * x = b,
 *
 * where b and x are n-element vectors and  A is an n by n unit, or non-
 * unit, upper or lower triangular matrix.
 *
 * No test for  singularity  or  near-singularity  is included  in  this
 * routine. Such tests must be performed before calling this routine.
 *
 * This is a blocked version of the algorithm.  For a more detailed des-
 * cription of  the arguments of this function, see the reference imple-
 * mentation in the ATLAS/src/blas/reference directory.
 *
 * ---------------------------------------------------------------------
 */
{
   int ierr=1;
   #define NBSV 120  /* LCM(1,2,3,4,5,6,8), min NU cleanup in gemv kerns */
   if (N >= NBSV+NBSV)
   {
      #ifdef TCPLX
      if (Trans == AtlasConjTrans)
      {
         Mjoin(PATLU,scal)(N, ATL_rnone, X+1, incX+incX);
         Mjoin(PATL,trsv)(Uplo, AtlasTrans, Diag, N, A, lda, X, incX);
         Mjoin(PATLU,scal)(N, ATL_rnone, X+1, incX+incX);
         return;
      }
      else if (Trans == AtlasConj)
      {
         Mjoin(PATLU,scal)(N, ATL_rnone, X+1, incX+incX);
         Mjoin(PATL,trsv)(Uplo, AtlasNoTrans, Diag, N, A, lda, X, incX);
         Mjoin(PATLU,scal)(N, ATL_rnone, X+1, incX+incX);
         return;
      }

      #endif
      if (Uplo == AtlasUpper)
      {
         if (Trans == AtlasNoTrans)
            ierr = ATL_trsvUN_k(Diag, NBSV, N, A, lda, X, incX);
         else
            ierr = ATL_trsvUT_k(Diag, NBSV, N, A, lda, X, incX);
      }
      else if (Trans == AtlasNoTrans)
         ierr = ATL_trsvLN_k(Diag, NBSV, N, A, lda, X, incX);
      else
         ierr = ATL_trsvLT_k(Diag, NBSV, N, A, lda, X, incX);
   }
   if (ierr)
      Mjoin(PATL,reftrsv)(Uplo, Trans, Diag, N, A, lda, X, incX);
}
