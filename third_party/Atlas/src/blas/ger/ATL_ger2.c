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
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),r2_L1.h))
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),r2_L2.h))
#elif defined(ATL_INOOC) || defined(ATL_TUNING)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),r2.h))
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
   #define MY_GER2 Mjoin(PATL,ger2)
   #define MY_GER  Mjoin(PATL,ger)
#else
   #ifdef Conj_
      #define MY_GER2 Mjoin(PATL,ger2c)
      #define MY_GER  Mjoin(PATL,gerc)
   #else
      #define MY_GER2 Mjoin(PATL,ger2u)
      #define MY_GER  Mjoin(PATL,geru)
   #endif
#endif
#ifdef ATL_INL1
   #define ATL_ger2 Mjoin(MY_GER2,_L1)
   #define ATL_ger Mjoin(MY_GER,_L1)
#elif defined(ATL_INL2)
   #define ATL_ger2 Mjoin(MY_GER2,_L2)
   #define ATL_ger Mjoin(MY_GER,_L2)
#elif defined(ATL_INOOC)
   #define ATL_ger2 Mjoin(MY_GER2,_OOC)
   #define ATL_ger Mjoin(MY_GER,_OOC)
#else
   #define ATL_ger2 MY_GER2
#endif

#ifdef Conj_
void Mjoin(PATL,ger2ck_Nlt8)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *X,
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, const SCALAR beta0,
    const TYPE *W, ATL_CINT incW, const TYPE *Z, ATL_CINT incZ,
    TYPE *A, const int lda);
void Mjoin(PATL,ger2ck_Mlt16)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *X,
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, const SCALAR beta0,
    const TYPE *W, ATL_CINT incW, const TYPE *Z, ATL_CINT incZ,
    TYPE *A, const int lda);
#endif
void Mjoin(PATL,ger2k_Nlt8)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *X,
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, const SCALAR beta0,
    const TYPE *W, ATL_CINT incW, const TYPE *Z, ATL_CINT incZ,
    TYPE *A, const int lda);
void Mjoin(PATL,ger2k_Mlt16)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *X,
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, const SCALAR beta0,
    const TYPE *W, ATL_CINT incW, const TYPE *Z, ATL_CINT incZ,
    TYPE *A, const int lda);

void ATL_ger2
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha0, const TYPE *X,
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, const SCALAR beta0,
    const TYPE *W, ATL_CINT incW, const TYPE *Z, ATL_CINT incZ,
    TYPE *A, const int lda)
{
#ifdef ATL_CHOICE
    const size_t opsize = (M*N + ((N+M)<<1))*sizeof(TYPE)SHIFT;

   if (opsize > MY_CE)
      Mjoin(MY_GER2,_OOC)(M, N, alpha0, X, incX, Y, incY, beta0, W, incW,
                          Z, incZ, A, lda);
   else if (opsize > ATL_MulBySize(ATL_L1elts))
       Mjoin(MY_GER2,_L2)(M, N, alpha0, X, incX, Y, incY, beta0, W, incW,
                          Z, incZ, A, lda);
   else /* if (opsize <= ATL_MulBySize(ATL_L1elts)) */
       Mjoin(MY_GER2,_L1)(M, N, alpha0, X, incX, Y, incY, beta0, W, incW,
                          Z, incZ, A, lda);
#else
   void (*getX)(const int N, const SCALAR alpha, const TYPE *X,
                const int incX, TYPE *Y, const int incY);
   void (*getW)(const int N, const SCALAR alpha, const TYPE *X,
                const int incX, TYPE *Y, const int incY);
   ATL_r2kern_t ger2k;
   void *vp=NULL, *vp2;
   TYPE *x = (TYPE*)X, *y = (TYPE*)Y;
   TYPE *w = (TYPE*)W, *z = (TYPE*)Z;
   size_t t1, t2;
   ATL_INT m, Nm, nr, CacheElts, mb, imb, incy=1;
   int mu, nu, alignX, alignY, ALIGNX2A, ForceNU, minM, minN;
   int COPYX, COPYY, COPYW, COPYZ, APPLYALPHAX, APPLYBETAW;
   #ifdef TREAL
      #define one ATL_rone
      TYPE alpha = alpha0;
      TYPE beta  = beta0;
      const int ALPHA_IS_ONE = (alpha0 == ATL_rone);
      const int BETA_IS_ONE = (beta0 == ATL_rone);
   #else
      TYPE one[2]={ATL_rone,ATL_rzero};
      TYPE *alpha=(TYPE*)alpha0, *beta=(TYPE*)beta0;
      const int ALPHA_IS_ONE = (alpha0[0] == ATL_rone && alpha0[1]==ATL_rzero);
      const int BETA_IS_ONE = (beta0[0] == ATL_rone && beta0[1] == ATL_rzero);
   #endif

   if (M < 1 || N < 1)
      return;
#ifndef TUNING
   if (SCALAR_IS_ZERO(alpha))
   {
      if (!SCALAR_IS_ZERO(beta))
         ATL_ger(M, N, beta, W, incW, Z, incZ, A, lda);
      return;
   }
   if (SCALAR_IS_ZERO(beta))
   {
      ATL_ger(M, N, alpha, X, incX, Y, incY, A, lda);
      return;
   }
#endif
/*
 * Get gerk kernel pointer along with any usage guidelines, and use the
 * optimized CacheElts to compute the correct blocking factor
 */
   ger2k = ATL_GetR2Kern(M, N, A, lda, &mu, &nu, &minM, &minN, &alignX,
                         &ALIGNX2A, &alignY, &ForceNU, &CacheElts);
   if (CacheElts)
   {
      mb = (CacheElts - 4*nu) / (4*(nu+1));
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
 * For very small N, we can't afford the data copy, so call cleanup routine
 */
   if (N < 4 || Nm < 1)
   {
      #ifdef Conj_
         Mjoin(PATL,ger2ck_Nlt8)(M, N, alpha0, X, incX, Y, incY,
                                 beta0, W, incW, Z, incZ, A, lda);
      #else
         Mjoin(PATL,ger2k_Nlt8)(M, N, alpha0, X, incX, Y, incY,
                                 beta0, W, incW, Z, incZ, A, lda);
      #endif
      return;
   }
/*
 * ATLAS's GER2 kernels loop over M in inner loop, which is bad news if M is
 * very small.  Call code that requires no copy of A & B for these degenerate
 * cases
 */
   if (M < 16 || M < minM)
   {
      #ifdef Conj_
         Mjoin(PATL,ger2ck_Mlt16)(M, N, alpha0, X, incX, Y, incY,
                                  beta0, W, incW, Z, incZ, A, lda);
      #else
         Mjoin(PATL,ger2k_Mlt16)(M, N, alpha0, X, incX, Y, incY,
                                 beta0, W, incW, Z, incZ, A, lda);
      #endif
      return;
   }
/*
 *********************************************************************
 Figure out whether vecs need be copied, and which ones will be scaled
 *********************************************************************
 */
   #ifdef Conj_
      COPYY = COPYZ = 1;
   #else
      COPYY = (incY != 1);
      if (!COPYY && alignY)
      {
         t1 = (size_t) Y;
         COPYY = ((t1/alignY)*alignY != t1);
      }
      COPYZ = (incZ != 1);
      if (!COPYZ && alignY)
      {
         t1 = (size_t) Z;
         COPYZ = ((t1/alignY)*alignY != t1);
      }
   #endif
   COPYW = (incW != 1);
   if (!COPYW)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make X's modulo Cachelen match that of A if you want A & X to have
 *    the same alignment
 */
      if (ALIGNX2A)
      {
         t1 = (size_t) A;
         t2 = (size_t) W;
         COPYW = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                 (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignX)
      {
         t1 = (size_t) W;
         COPYW = ((t1/alignX)*alignX != t1);
      }
   }
   COPYX = (incX != 1);
   if (!COPYX)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make X's modulo Cachelen match that of A if you want A & X to have
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

   if (COPYW != COPYZ)         /* if only one of them is already being copied */
      APPLYBETAW = COPYW;      /* apply alpha to that one */
   else if (!COPYZ && !COPYW)  /* nobody currently being copied means */
   {                           /* we'll need to force a copy to apply beta */
      APPLYBETAW = (M < N);    /* apply beta to shorter vector */
      if (!BETA_IS_ONE)        /* force copy if beta != 1.0 */
      {
         COPYW = APPLYBETAW;
         COPYZ = !APPLYBETAW;
      }
   }
   else                        /* if both are being copied anyway */
      APPLYBETAW = (M < N);    /* apply beta to shorter vector */

   if (COPYX | COPYY | COPYW | COPYZ)  /* if I need to copy any vector */
   {                          /* allocate & align them */
      vp = malloc(ATL_MulBySize((COPYX+COPYW)*mb+(COPYY+COPYZ)*N)
                  + 4*ATL_Cachelen);
/*
 *    If we cannot allocate enough space to copy the vectors, give up and
 *    call the simple loop-based implementation
 */
      if (!vp)
      {
         #ifdef Conj_
            Mjoin(PATL,ger2ck_Nlt8)
         #else
            Mjoin(PATL,ger2k_Nlt8)
         #endif
            (M, N, alpha0, X, incX, Y, incY, beta0, W, incW, Z, incZ, A, lda);
         return;
      }
      vp2 = vp;
      if (COPYY)
      {
         y = ATL_AlignPtr(vp);
         vp2 = y + (N SHIFT);
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
      if (COPYZ)
      {
         z = ATL_AlignPtr(vp2);
         vp2 = z + (N SHIFT);
         if (!APPLYBETAW && !BETA_IS_ONE)  /* need to apply beta  to Z */
         {
            #ifdef Conj_
               Mjoin(PATL,moveConj)(N, beta, Z, incZ, z, 1);
            #else
               Mjoin(PATL,cpsc)(N, beta, Z, incZ, z, 1);
            #endif
            beta = one;
         }
         else  /* do not apply beta */
         #ifdef Conj_
            Mjoin(PATL,copyConj)(N, Z, incZ, z, 1);
         #else
            Mjoin(PATL,copy)(N, Z, incZ, z, 1);
         #endif
      }
      if (COPYX)
      {
         x = (ALIGNX2A) ? ATL_Align2Ptr(vp2, A) : ATL_AlignPtr(vp2);
         vp2 = x + (mb SHIFT);
      }
      if (COPYW)
      {
         w = (ALIGNX2A) ? ATL_Align2Ptr(vp2, A) : ATL_AlignPtr(vp2);
         vp2 = w + (mb SHIFT);
      }
   }
   getX = (COPYX) ? Mjoin(PATL,cpsc) : NULL;
   getW = (COPYW) ? Mjoin(PATL,cpsc) : NULL;
   m = M;
   do
   {
      imb = Mmin(mb, m);
      if (getX)    /* copy X if necessary */
         getX(imb, alpha, X, incX, x, 1);
      else
         x = (TYPE*) X;
      if (getW)    /* copy X if necessary */
         getW(imb, beta, W, incW, w, 1);
      else
         w = (TYPE*) W;

/*
 *    Call optimized kernel (can be restricted or general)
 */
      if (imb >= minM)
         ger2k(imb, Nm, x, y, w, z, A, lda);
      else
         Mjoin(PATL,ger2k_Mlt16)(imb, Nm, one, x, 1, y, 1, one, w, 1, z, 1,
                                 A, lda);
/*
 *    Some kernels require N%NU=0; if so nr is remainder, do cleanup with axpy
 */
      if (nr)
         Mjoin(PATL,ger2k_Nlt8)(imb, nr, one, x, 1, y+(Nm SHIFT), 1, one,
                                w, 1, z+(Nm SHIFT), 1,
                                A+((size_t)lda)*(Nm SHIFT), lda);
      A += imb SHIFT;
      X += (imb*incX)SHIFT;
      W += (imb*incW)SHIFT;
      m -= imb;
      imb = Mmin(m,mb);
   }
   while(m);
   if (vp)
      free(vp);
#endif
}
