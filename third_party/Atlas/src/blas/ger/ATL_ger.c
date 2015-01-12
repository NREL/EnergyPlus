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
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),r1_L1.h))
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),r1_L2.h))
#elif defined(ATL_INOOC) || defined(ATL_TUNING)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),r1.h))
#else
   #define ATL_CHOICE
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))
   #include "atlas_cacheedge.h"
/*
 * If I don't believe CacheEdge setting (or not set), set L2 size to 4*L1
 */
   #ifdef CacheEdge
      #if CacheEdge > 4194304 || CacheEdge == 0
         #define MY_CE (4*ATL_MulBySize(ATL_L1elts))
      #else
         #define MY_CE CacheEdge
      #endif
   #else
      #define MY_CE (4*ATL_MulBySize(ATL_L1elts))
   #endif
#endif

#ifdef TREAL
   #define MY_GER Mjoin(PATL,ger)
   #define MY_GERK_AXPY Mjoin(PATL,gerk_axpy)
   #define MY_GERK_MLT16 Mjoin(PATL,gerk_Mlt16)
#else
   #ifdef Conj_
      #define MY_GER Mjoin(PATL,gerc)
      #define MY_GERK_AXPY Mjoin(PATL,gerck_axpy)
      #define MY_GERK_MLT16 Mjoin(PATL,gerck_Mlt16)
   #else
      #define MY_GER Mjoin(PATL,geru)
      #define MY_GERK_AXPY Mjoin(PATL,gerk_axpy)
      #define MY_GERK_MLT16 Mjoin(PATL,gerk_Mlt16)
   #endif
#endif
#ifdef Conj_
void Mjoin(PATL,gerk_axpy)
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda);
void Mjoin(PATL,gerk_Mlt16)
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda);
#endif
void MY_GERK_AXPY
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda);
void MY_GERK_MLT16
   (const int M, const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const TYPE *Y, const int incY, TYPE *A, const int lda);

#ifdef ATL_INL1
   #define ATL_ger Mjoin(MY_GER,_L1)
#elif defined(ATL_INL2)
   #define ATL_ger Mjoin(MY_GER,_L2)
#elif defined(ATL_INOOC)
   #define ATL_ger Mjoin(MY_GER,_OOC)
#else
   #define ATL_ger MY_GER
#endif

void ATL_ger
   (const int M, const int N, const SCALAR alpha0, const TYPE *X,
    const int incX, const TYPE *Y, const int incY, TYPE *A, const int lda)
{
#ifdef ATL_CHOICE
   const size_t opsize = (M*N+M+N)*sizeof(TYPE)SHIFT;

   if (opsize <= ATL_MulBySize(ATL_L1elts))
      Mjoin(MY_GER,_L1)(M, N, alpha0, X, incX, Y, incY, A, lda);
   else if (opsize <= MY_CE)
      Mjoin(MY_GER,_L2)(M, N, alpha0, X, incX, Y, incY, A, lda);
   else
      Mjoin(MY_GER,_OOC)(M, N, alpha0, X, incX, Y, incY, A, lda);
#else
   void (*getX)(const int N, const SCALAR alpha, const TYPE *X,
                const int incX, TYPE *Y, const int incY);
   ATL_r1kern_t gerk;
   void *vp=NULL;
   TYPE *x = (TYPE*)X, *y = (TYPE*)Y;
   size_t t1, t2;
   ATL_INT m, Nm, nr, CacheElts, mb, imb, incy=1;
   int mu, nu, alignX, alignY, ALIGNX2A, ForceNU, COPYX, COPYY, APPLYALPHAX;
   int minM, minN;
   #ifdef TREAL
      #define one ATL_rone
      TYPE alpha = alpha0;
      const int ALPHA_IS_ONE = (alpha0 == ATL_rone);
   #else
      TYPE one[2] = {ATL_rone, ATL_rzero}, *alpha=(TYPE*)alpha0;
      const int ALPHA_IS_ONE = (alpha0[0] == ATL_rone && alpha[1] == ATL_rzero);
   #endif

   if (M < 1 || N < 1 || SCALAR_IS_ZERO(alpha))
      return;
/*
 * Get gerk kernel pointer along with any usage guidelines, and use the
 * optimized CacheElts to compute the correct blocking factor
 */
   gerk = ATL_GetR1Kern(M, N, A, lda, &mu, &nu, &minM, &minN, &alignX,
                         &ALIGNX2A, &alignY, &ForceNU, &CacheElts);
   if (CacheElts)
   {
      mb = (CacheElts - 2*nu) / (2*(nu+1));
      mb = (mb > mu) ? (mb/mu)*mu : M;
      mb = (mb > M) ? M : mb;
   }
   else
      mb = M;
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
 * For very small N, we can't afford the data copy, so call AXPY-based routine
 */
   if (N < 4 || Nm < 1)
   {
      MY_GERK_AXPY(M, N, alpha0, X, incX, Y, incY, A, lda);
      return;
   }
/*
 * ATLAS's GER kernels loop over M in inner loop, which is bad news if M is
 * very small.  Call code that requires no copy of A & B for these degenerate
 * cases
 */
   if (M < 16 || M < minM)
   {
      MY_GERK_MLT16(M, N, alpha0, X, incX, Y, incY, A, lda);
      return;
   }
/*
 *****************************************************************************
 Figure out whether vecs need be copied, and which one will be scaled by alpha
 *****************************************************************************
 */
   #ifdef Conj_
      COPYY = 1;
   #else
      COPYY = (incY != 1);
      if (!COPYY && alignY)
      {
         t1 = (size_t) Y;
         COPYY = ((t1/alignY)*alignY != t1);
      }
   #endif
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
   if (COPYX != COPYY)         /* if only one of them is already being copied */
      APPLYALPHAX = COPYX;     /* apply alpha to that one */
   else if (!COPYY && !COPYX)  /* nobody currently being copied means */
   {                           /* we'll need to force a copy to apply alpha */
      APPLYALPHAX = (M < N);   /* apply alpha to shorter vector */
      if (!ALPHA_IS_ONE)       /* force copy if alpha != 1.0 */
      {
         COPYX = APPLYALPHAX;
         COPYY = !APPLYALPHAX;
      }
   }
   else                        /* if both are being copied anyway */
      APPLYALPHAX = (M < N);   /* apply alpha to shorter vector */

   if (COPYX | COPYY)         /* if I need to copy either vector */
   {                          /* allocate & align them */
      vp = malloc(ATL_MulBySize(COPYX*mb+COPYY*N) + 2*ATL_Cachelen);
/*
 *    If we cannot allocate enough space to copy the vectors, give up and
 *    call the simple loop-based implementation
 */
      if (!vp)
      {
         MY_GERK_AXPY(M, N, alpha0, X, incX, Y, incY, A, lda);
         return;
      }
      if (COPYY)
      {
         y = ATL_AlignPtr(vp);
         x = y + (N SHIFT);
         x = (ALIGNX2A) ? ATL_Align2Ptr(x, A) : ATL_AlignPtr(x);
         if (!APPLYALPHAX && !ALPHA_IS_ONE)  /* need to apply alpha to Y */
         {
            #ifdef Conj_
               Mjoin(PATL,moveConj)(N, alpha, Y, incY, y, 1);
            #else
               Mjoin(PATL,cpsc)(N, alpha, Y, incY, y, 1);
            #endif
            alpha = one;
         }
         else  /* do not apply alpha */
         #ifdef Conj_
            Mjoin(PATL,copyConj)(N, Y, incY, y, 1);
         #else
            Mjoin(PATL,copy)(N, Y, incY, y, 1);
         #endif
      }
      else if (ALIGNX2A)
         x = ATL_Align2Ptr(vp, A);
      else
         x = ATL_AlignPtr(vp);
   }
   getX = (COPYX) ? Mjoin(PATL,cpsc) : NULL;
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
      if (imb > minM)
         gerk(imb, Nm, x, y, A, lda);
      else
         Mjoin(PATL,gerk_Mlt16)(imb, Nm, one, x, 1, y, 1, A, lda);
/*
 *    Some kernels require N%NU=0; if so nr is remainder, do cleanup with axpy
 */
      if (nr)
         Mjoin(PATL,gerk_axpy)(imb, nr, one, x, 1, y+(Nm SHIFT), 1,
                               A+((size_t)lda)*(Nm SHIFT), lda);
      A += imb SHIFT;
      X += (imb*incX)SHIFT;
      m -= imb;
      imb = Mmin(m,mb);
   }
   while(m);
   if (vp)
      free(vp);
#endif
}
