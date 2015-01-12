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
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvn_L1.h))
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvn_L2.h))
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvn.h))
#endif

#ifdef ATL_INL1
   #define ATL_gemv Mjoin(PATL,gemvCN_L1)
#elif defined(ATL_INL2)
   #define ATL_gemv Mjoin(PATL,gemvCN_L2)
#else
   #define ATL_gemv Mjoin(PATL,gemvCN)
#endif
void ATL_gemv
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta0, TYPE *Y, ATL_CINT incY)
/*
 * Y = alpha*conj(A)*X + beta*Y, A is MxN, len(X) = N, len(Y) = M
 * We don't have a conj(A) kernel, so form this with O(N) extra flops:
 *    x = conj(X); y = A * conj(X),
 * then add this into Y using axpbyConj:
 *    Y = alpha*conj(y) + beta*Y = alpha*(conj(A)*X) + beta*Y
 * providing:
 *    Y = alpha*conj(A)*X + beta*Y
 */
{
   ATL_mvkern_t mvnk, mvnk_b1, mvnk_b0;
   void *vp=NULL;
   TYPE *x = (TYPE*)X, *y = (TYPE*)Y, *p;
   size_t t1, t2;
   ATL_INT m, Nm, nr, CacheElts, mb, imb, incy=1;
   int mu, nu, alignX, alignY, ALIGNY2A, ForceNU, COPYX, COPYY, APPLYALPHAX;
   int minM, minN, DOTBASED;
   TYPE one[2] = {ATL_rone, ATL_rzero}, *alpha=(TYPE*)alpha0;
   TYPE Zero[2] = {ATL_rzero, ATL_rzero};
   TYPE *beta = (TYPE*) beta0;
   const int ALPHA_IS_ONE = (alpha0[0] == ATL_rone && alpha[1] == ATL_rzero);

   if (M < 1 || N < 1)          /* F77BLAS doesn't scale in either case */
      return;
   if (SCALAR_IS_ZERO(alpha))   /* No contrib from alpha*A*x */
   {
      if (!SCALAR_IS_ONE(beta))
      {
         if (SCALAR_IS_ZERO(beta))
            Mjoin(PATL,zero)(M, Y, incY);
         else
            Mjoin(PATL,scal)(M, beta, Y, incY);
      }
      return;
   }
/*
 * ATLAS's mvn kernels loop over M in inner loop, which is bad news if M is
 * very small.  Call code that requires no copy of X & Y for these degenerate
 * cases
 */
   if (M < 16)
   {
      Mjoin(PATL,refgemv)(AtlasConj, M, N, alpha0, A, lda, X, incX,
                          beta0, Y, incY);
      return;
   }
/*
 * Get mvnk kernel pointer along with any usage guidelines, and use the
 * optimized CacheElts to compute the correct blocking factor
 * For no transpose, X alignment args really apply to Y, and vice versa.
 */
   mvnk_b1 = ATL_GetMVNKern(M, N, A, lda, &mvnk_b0, &DOTBASED, &mu, &nu,
                            &minM, &minN, &alignY, &ALIGNY2A, &alignX,
                            &ForceNU, &CacheElts);
/*
 * Set up to handle case where kernel requires N to be a multiple if NU
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
      Mjoin(PATL,refgemv)(AtlasConj, M, N, alpha0, A, lda, X, incX,
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
/*
 *****************************************************************************
 Figure out whether vecs need be copied, and which one will be scaled by alpha
 *****************************************************************************
 */
   vp = malloc(ATL_MulBySize(mb+N) + 2*ATL_Cachelen);
/*
 * If we cannot allocate enough space to copy the vectors, give up and
 * call the simple loop-based implementation
 */
   if (!vp)
   {
      Mjoin(PATL,refgemv)(AtlasConj, M, N, alpha0, A, lda, X, incX,
                          beta0, Y, incY);
      return;
   }
/*
 * Form x = conj(X)
 */
   x = ATL_AlignPtr(vp);
   Mjoin(PATL,copyConj)(N, X, incX, x, 1);
   y = x + (N SHIFT);
   y = (ALIGNY2A) ? ATL_Align2Ptr(y, A) : ATL_AlignPtr(y);
/*
 * Loop over cache-blocked (for vector reuse) gemv calls
 */
   mvnk = mvnk_b0;
   beta = Zero;
   m = M;
   do
   {
/*
 *    Form y = A * conj(X)
 */
      imb = Mmin(mb, m);
/*
 *    Call optimized kernel (can be restricted or general)
 */
      if (imb >= minM)
         mvnk(imb, Nm, A, lda, x, y);
      else
         Mjoin(PATL,mvnk_Mlt16)(imb, Nm, one, A, lda, x, 1, beta, y, 1);
/*
 *    Some kernels require N%NU=0; if so nr is remainder, do cleanup with axpy
 */
      if (nr)
         Mjoin(PATL,mvnk_smallN)(imb, nr, one, A+((size_t)lda)*(Nm SHIFT), lda,
                                 x+(Nm SHIFT), 1, one, y, 1);
/*
 *    Given y = A * conj(X) from above, now do:
 *       Y = beta*Y + alpha*conj(y) = beta*Y + alpha*(conj(A)*X)
 *         = beta*Y + alpha * conj(A) * X
 */
      Mjoin(PATL,axpbyConj)(imb, alpha0, y, 1, beta0, Y, incY);
      A += imb SHIFT;
      Y += (imb*incY)SHIFT;
      m -= imb;
   }
   while(m);
   free(vp);
}
