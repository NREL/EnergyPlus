/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
#include "atlas_lvl2.h"
#include "atlas_lvl3.h"
#if defined(ATL_INL1)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvt_L1.h))
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvt_L2.h))
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvt.h))
#endif

#ifdef ATL_INL1
   #define ATL_gemv Mjoin(PATL,gemvT_L1)
#elif defined(ATL_INL2)
   #define ATL_gemv Mjoin(PATL,gemvT_L2)
#else
   #define ATL_gemv Mjoin(PATL,gemvT)
#endif
void ATL_gemv
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta0, TYPE *Y, ATL_CINT incY)
/*
 * y = alpha*A*x + beta*y
 */
{
   void (*getX)(const int N, const SCALAR alpha, const TYPE *X,
                const int incX, TYPE *Y, const int incY);
   ATL_mvkern_t mvtk, mvtk_b1, mvtk_b0;
   void *vp=NULL;
   TYPE *x = (TYPE*)X, *y = (TYPE*)Y;
   size_t t1, t2;
   ATL_INT m, Nm, nr, CacheElts, mb, imb, incy=1;
   int mu, nu, alignX, alignY, ALIGNX2A, ForceNU, COPYX, COPYY, APPLYALPHAX;
   int minM, minN;
   #ifdef TREAL
      #define one ATL_rone
      #define Zero ATL_rzero
      TYPE alpha = alpha0, beta = beta0;
      const int ALPHA_IS_ONE = (alpha0 == ATL_rone);
   #else
      TYPE one[2] = {ATL_rone, ATL_rzero};
      #ifdef Conj_
         TYPE *alpha=one;
      #else
         TYPE *alpha=(TYPE*)alpha0;
      #endif
      TYPE Zero[2] = {ATL_rzero, ATL_rzero};
      TYPE *beta = (TYPE*) beta0;
      const int ALPHA_IS_ONE = (alpha0[0] == ATL_rone && alpha[1] == ATL_rzero);
   #endif

   if (M < 1 || N < 1)          /* F77 BLAS doesn't scale in either case */
      return;
   if (SCALAR_IS_ZERO(alpha))   /* No contrib from alpha*A*x */
   {
      if (!SCALAR_IS_ONE(beta))
      {
         if (SCALAR_IS_ZERO(beta))
            Mjoin(PATL,zero)(N, Y, incY);
         else
            Mjoin(PATL,scal)(N, beta, Y, incY);
      }
      return;
   }
/*
 * ATLAS's MVT kernels loop over M in inner loop, which is bad news if M is
 * very small.  Call code that requires no copy of X & Y for these degenerate
 * cases
 */
   if (M < 16)
   {
      Mjoin(PATL,mvtk_Mlt16)(M, N, alpha0, A, lda, X, incX, beta0, Y, incY);
      return;
   }
/*
 * Get mvtk kernel pointer along with any usage guidelines, and use the
 * optimized CacheElts to compute the correct blocking factor
 */
   mvtk_b1 = ATL_GetMVTKern(M, N, A, lda, &mvtk_b0, &mu, &nu,
                            &minM, &minN, &alignX, &ALIGNX2A, &alignY,
                            &ForceNU, &CacheElts);
/*
 * Set up to handle case where kernel requres N to be a multiple if NU
 */
   if (ForceNU)
   {
      Nm = (N/nu)*nu;
      nr = N - Nm;
   }
   else
   {
      Nm = N;
      nr = 0;
   }
/*
 * For very small N, we can't afford the data copy, so call special case code
 */
   if (N < 4 || Nm < 1)
   {
      Mjoin(PATL,mvtk_smallN)(M, N, alpha0, A, lda, X, incX, beta0, Y, incY);
      return;
   }
   if (CacheElts)
   {
      mb = (CacheElts - 2*nu) / (2*(nu+1));
      mb = (mb > mu) ? (mb/mu)*mu : M;
      mb = (mb > M) ? M : mb;
   }
   else
      mb = M;
/*
 *****************************************************************************
 Figure out whether vecs need be copied, and which one will be scaled by alpha
 *****************************************************************************
 */
   COPYY = (incY != 1);
   if (!COPYY && alignY)
   {
      t1 = (size_t) Y;
      COPYY = ((t1/alignY)*alignY != t1);
   }
   COPYX = (incX != 1);
   if (!COPYX)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make X's % with Cachelen match that of A if you want A & X to have
 *    the same alignment
 */
      if (ALIGNX2A)
      {
         t1 = (size_t) A;
         t2 = (size_t) X;
         COPYX = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                 (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignX)
      {
         t1 = (size_t) X;
         COPYX = ((t1/alignX)*alignX != t1);
      }
   }
   if (COPYX != COPYY)         /* if only one of them is being copied */
      APPLYALPHAX = COPYX;     /* apply alpha to that one */
   else if (!COPYY && !COPYX)  /* nobody currently being copied means */
   {                           /* we need to force a copy to apply alpha */
      APPLYALPHAX = (M < N);   /* apply alpha to shorter vector */
      if (!ALPHA_IS_ONE)       /* force copy if alpha != 1.0 */
      {
         COPYX = APPLYALPHAX;
         COPYY = !APPLYALPHAX;
      }
   }
   else                        /* if both are being copied anyway */
      APPLYALPHAX = 0;         /* apply alpha during update of Y */

   if (COPYX | COPYY)         /* if I need to copy either vector */
   {                          /* allocate & align them */
      vp = malloc(ATL_MulBySize(COPYX*mb+COPYY*N) + 2*ATL_Cachelen);
/*
 *    If we cannot allocate enough space to copy the vectors, give up and
 *    call the simple loop-based implementation
 */
      if (!vp)
      {
         Mjoin(PATL,mvtk_smallN)(M, N, alpha0, A, lda, X, incX, beta0, Y, incY);
         return;
      }
      if (COPYY)
      {
         y = ATL_AlignPtr(vp);
         x = y + (N SHIFT);
         x = (ALIGNX2A) ? ATL_Align2Ptr(x, A) : ATL_AlignPtr(x);
         beta = Zero;
         alpha = one;
      }
      else if (ALIGNX2A)
         x = ATL_Align2Ptr(vp, A);
      else
         x = ATL_AlignPtr(vp);
   }
/*
 * Apply beta to Y if we aren't copying Y
 */
   if (!COPYY && !SCALAR_IS_ONE(beta0))
   {
      if (SCALAR_IS_ZERO(beta0))
         beta = Zero;
      else
      {
         Mjoin(PATL,scal)(N, beta0, Y, incY);
         beta = one;
      }
   }
   getX = (COPYX) ? Mjoin(PATL,cpsc) : NULL;
   mvtk = (COPYY || SCALAR_IS_ZERO(beta)) ? mvtk_b0 : mvtk_b1;
   m = M;
   do
   {
      imb = Mmin(mb, m);
      if (getX)    /* copy X if necessary */
         getX(imb, alpha, X, incX, x, 1);
      else
         x = (TYPE*) X;
/*
 *    Call optimized kernel (can be restricted or general)
 */
      if (imb >= minM)
         mvtk(imb, Nm, A, lda, x, y);
      else
         Mjoin(PATL,mvtk_Mlt16)(imb, Nm, one, A, lda, x, 1, beta, y, 1);
/*
 *    Some kernels require N%NU=0; if so nr is remainder, do cleanup with axpy
 */
      if (nr)
         Mjoin(PATL,mvtk_smallN)(imb, nr, one, A+((size_t)lda)*(Nm SHIFT), lda,
                                 x, 1, beta, y+(Nm SHIFT), 1);
      beta = one;
      mvtk = mvtk_b1;
      A += imb SHIFT;
      X += (imb*incX)SHIFT;
      m -= imb;
      imb = Mmin(m,mb);
   }
   while(m);

   if (COPYY)  /* move Y back to user's vector */
      Mjoin(PATL,axpby)(N, alpha0, y, 1, beta0, Y, incY);
   if (vp)
      free(vp);
}
