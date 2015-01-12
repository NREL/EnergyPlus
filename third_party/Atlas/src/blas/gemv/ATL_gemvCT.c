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
#include "atlas_reflevel2.h"
#if defined(ATL_INL1)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvt_L1.h))
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvt_L2.h))
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvt.h))
#endif

#ifdef ATL_INL1
   #define ATL_gemv Mjoin(PATL,gemvCT_L1)
#elif defined(ATL_INL2)
   #define ATL_gemv Mjoin(PATL,gemvCT_L2)
#else
   #define ATL_gemv Mjoin(PATL,gemvCT)
#endif
void ATL_gemv
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta0, TYPE *Y, ATL_CINT incY)
/*
 * Y = alpha*conj(A)*X + beta*Y
 * For Conjugate transpose, first form x = conj(X), y = A^T * conj(X),
 * then use axpbyConj to add this to original Y in the operation
 * Y = beta*Y + alpha*conj(y) = beta*Y + alpha*(A^H * X), which is
 * Y = beta*Y + alpha * A^H * X.
 */
{
   ATL_mvkern_t mvtk, mvtk_b1, mvtk_b0;
   void *vp=NULL;
   TYPE *x = (TYPE*)X, *y = (TYPE*)Y;
   size_t t1, t2;
   ATL_INT m, Nm, nr, CacheElts, mb, imb, incy=1;
   int mu, nu, alignX, alignY, ALIGNX2A, ForceNU, COPYX, COPYY, APPLYALPHAX;
   int minM, minN;
   TYPE one[2] = {ATL_rone, ATL_rzero};
   TYPE Zero[2] = {ATL_rzero, ATL_rzero};
   TYPE *beta = (TYPE*) beta0;
   const int ALPHA_IS_ONE = (alpha0[0] == ATL_rone && alpha0[1] == ATL_rzero);

   if (M < 1 || N < 1)          /* F77 BLAS doesn't scale in either case */
      return;
   if (SCALAR_IS_ZERO(alpha0))   /* No contrib from alpha*A*x */
   {
      if (!SCALAR_IS_ONE(beta0))
      {
         if (SCALAR_IS_ZERO(beta0))
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
      Mjoin(PATL,refgemv)(AtlasConjTrans, N, M, alpha0, A, lda, X, incX,
                          beta0, Y, incY);
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
      Mjoin(PATL,refgemv)(AtlasConjTrans, N, M, alpha0, A, lda, X, incX,
                          beta0, Y, incY);
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
   vp = malloc(ATL_MulBySize(mb+N) + 2*ATL_Cachelen);
/*
 * If we cannot allocate enough space to copy the vectors, give up and
 * call the simple loop-based implementation
 */
   if (!vp)
   {
      Mjoin(PATL,refgemv)(AtlasConjTrans, N, M, alpha0, A, lda, X, incX,
                          beta0, Y, incY);
      return;
   }
   y = ATL_AlignPtr(vp);
   x = y + (N SHIFT);
   x = (ALIGNX2A) ? ATL_Align2Ptr(x, A) : ATL_AlignPtr(x);
   beta = Zero;
/*
 * In this step, we form y = A^T * conj(X)
 */
   mvtk = mvtk_b0;
   m = M;
   do
   {
      imb = Mmin(mb, m);
      Mjoin(PATL,copyConj)(imb, X, incX, x, 1);  /* x = conj(X) */
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

/*
 * Given y = A^T * conj(X) from above, now do:
 *    Y = beta*Y + alpha*conj(y) = beta*Y + alpha*(A^H * x), which is
 *    Y = beta*Y + alpha * A^H * x.
 */
   Mjoin(PATL,axpbyConj)(N, alpha0, y, 1, beta0, Y, incY);
   free(vp);
}
