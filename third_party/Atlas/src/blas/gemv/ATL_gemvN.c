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
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvn_L1.h))
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvn_L2.h))
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),mvn.h))
#endif

#ifdef ATL_INL1
   #define ATL_gemv Mjoin(PATL,gemvN_L1)
#elif defined(ATL_INL2)
   #define ATL_gemv Mjoin(PATL,gemvN_L2)
#else
   #define ATL_gemv Mjoin(PATL,gemvN)
#endif
void ATL_gemv
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta0, TYPE *Y, ATL_CINT incY)
/*
 * y = alpha*A*x + beta*y, A is MxN, len(X) = N, len(Y) = M
 */
{
   ATL_mvkern_t mvnk, mvnk_b1, mvnk_b0;
   void *vp=NULL;
   TYPE *x = (TYPE*)X, *y = (TYPE*)Y, *p;
   size_t t1, t2;
   ATL_INT m, Nm, nr, CacheElts, mb, imb, incy=1;
   int mu, nu, alignX, alignY, ALIGNY2A, ForceNU, COPYX, COPYY, APPLYALPHAX;
   int minM, minN, DOTBASED;
   #ifdef TREAL
      #define one ATL_rone
      #define Zero ATL_rzero
      TYPE alpha = alpha0, beta = beta0;
      const int ALPHA_IS_ONE = (alpha0 == ATL_rone);
   #else
      TYPE one[2] = {ATL_rone, ATL_rzero}, *alpha=(TYPE*)alpha0;
      TYPE Zero[2] = {ATL_rzero, ATL_rzero};
      TYPE *beta = (TYPE*) beta0;
      const int ALPHA_IS_ONE = (alpha0[0] == ATL_rone && alpha[1] == ATL_rzero);
   #endif

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
      Mjoin(PATL,mvnk_Mlt16)(M, N, alpha0, A, lda, X, incX, beta0, Y, incY);
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
      Mjoin(PATL,mvnk_smallN)(M, N, alpha0, A, lda, X, incX, beta0, Y, incY);
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
   COPYX = (incX != 1);
   if (!COPYX && alignX)
   {
      t1 = (size_t) X;
      COPYX = ((t1/alignX)*alignX != t1);
   }
   COPYY = (incY != 1);
   if (!COPYY)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make X's % with Cachelen match that of A if you want A & X to have
 *    the same alignment
 */
      if (ALIGNY2A)
      {
         t1 = (size_t) A;
         t2 = (size_t) Y;
         COPYY = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                 (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignY)
      {
         t1 = (size_t) Y;
         COPYY = ((t1/alignY)*alignY != t1);
      }
   }
   if (COPYX != COPYY)         /* if only one of them is already being copied */
      APPLYALPHAX = COPYX;     /* apply alpha to that one */
   else if (!COPYY && !COPYX)  /* nobody currently being copied means */
   {                           /* we'll need to force a copy to apply alpha */
      APPLYALPHAX = (M < N);   /* apply alpha to vector requiring least */
      if (!ALPHA_IS_ONE)       /* workspace if alpha != 1.0 */
      {
         COPYX = APPLYALPHAX;
         COPYY = !APPLYALPHAX;
      }
   }
   else                        /* if both are being copied anyway */
      APPLYALPHAX = 0;         /* apply alpha during update of Y */

   if (COPYX | COPYY)         /* if I need to copy either vector */
   {                          /* allocate & align them */
      vp = malloc(ATL_MulBySize(COPYY*mb+COPYX*N) + 2*ATL_Cachelen);
/*
 *    If we cannot allocate enough space to copy the vectors, give up and
 *    call the simple loop-based implementation
 */
      if (!vp)
      {
         Mjoin(PATL,mvnk_smallN)(M, N, alpha0, A, lda, X, incX, beta0, Y, incY);
         return;
      }
      if (COPYX)
      {
         x = ATL_AlignPtr(vp);
         if (APPLYALPHAX && !ALPHA_IS_ONE)
            Mjoin(PATL,cpsc)(N, alpha, X, incX, x, 1);
         else
            Mjoin(PATL,copy)(N, X, incX, x, 1);
         if (COPYY)
            y = x + (N SHIFT);
      }
      else /* if (COPYY)  known true by surrounding if */
         y = vp;
      if (COPYY)
      {
         y = (ALIGNY2A) ? ATL_Align2Ptr(y, A) : ATL_AlignPtr(y);
         beta = Zero;
         alpha = one;
      }
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
         Mjoin(PATL,scal)(M, beta0, Y, incY);
         beta = one;
      }
   }
   mvnk = (COPYY || SCALAR_IS_ZERO(beta)) ? mvnk_b0 : mvnk_b1;
   m = M;
   do
   {
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
 *    If we are copying Y, we have formed A*x into y, so scale it by the
 *    original alpha, by using axpby: Y = beta0*Y + alpha0*y
 */
      if (COPYY)
         Mjoin(PATL,axpby)(imb, alpha0, y, 1, beta0, Y, incY);
      else
         y += imb SHIFT;
      A += imb SHIFT;
      Y += (imb*incY)SHIFT;
      m -= imb;
   }
   while(m);
   if (vp)
      free(vp);
}
