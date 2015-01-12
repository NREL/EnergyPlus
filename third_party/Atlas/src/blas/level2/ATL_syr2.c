#include "atlas_misc.h"
#include "atlas_level1.h"
#include "atlas_lvl2.h"
#include "atlas_reflvl2.h"
#include "atlas_reflevel2.h"
#if defined(ATL_INL1)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr2_L1.h))
   #define ATL_syr2 Mjoin(PATL,syr2_L1)
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr2_L2.h))
   #define ATL_syr2 Mjoin(PATL,syr2_L2)
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr2.h))
   #define ATL_syr2 Mjoin(PATL,syr2)
#endif

#ifdef ATL_NXTUNE
   extern int ATL_KERN_NX;
   #define ATL_S2NX ATL_KERN_NX
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr2NX.h))
   #ifndef ATL_S2NX
      #define ATL_S2NX 128
   #endif
#endif


void Mjoin(PATL,syr2_kU)
(
   ATL_r2kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const TYPE *x,               /* vector x -- alpha applied to x or y */
   const TYPE *y,               /* vector y -- alpha applied to x or y */
   TYPE *A,                     /* symmetric matrix, A = A + x*y^T + y*x^T */
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r2kern_t gerk=gerk0;
   ATL_INT nx=(ATL_S2NX/ATL_s2U_NU)*ATL_s2U_NU, j;
   ATL_CINT NN = (N/ATL_s2U_NU)*ATL_s2U_NU;

   nx = (ATL_S2NX >= ATL_s2U_NU) ? (ATL_S2NX/ATL_s2U_NU)*ATL_s2U_NU
        : ATL_s2U_NU;
   nx = Mmin(nx,N);
   Mjoin(PATL,refsyr2U)(nx, ATL_rone, x, 1, y, 1, A, lda);
   if (nx == N)
     return;
   for (j=nx; j < NN; j += ATL_s2U_NU)
   {
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (j >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(j, ATL_s2U_NU, x, y+j, y, x+j, A+j*lda, lda);
      ATL_SYR2U_nu(A+j*(lda+1), lda, x+j, y+j);
   }
   nx = N - NN;
   if (nx)
   {
      ATL_GENGERK(NN, nx, x, y+NN, y, x+NN, A+NN*lda, lda);
      Mjoin(PATL,refsyr2U)(nx, ATL_rone, x+NN, 1, y+NN, 1, A+NN*(lda+1), lda);
   }
}

void Mjoin(PATL,syr2_kL)
(
   ATL_r2kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const TYPE *x,               /* vector x -- alpha applied to x or y */
   const TYPE *y,               /* vector y -- alpha applied to x or y */
   TYPE *A,                     /* symmetric matrix, A = A + x*y^T + y*x^T */
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r2kern_t gerk=gerk0;
   ATL_INT nx=Mmin(ATL_S2NX,N), i, NN, n;
   ATL_CINT incA = ATL_s2L_NU*(lda+1);

   i = N - nx;
   i = (i/ATL_s2L_NU)*ATL_s2L_NU;
   if (i != N-nx)
      nx += N-nx-i;
   NN = N - nx;
   for (i=0; i < NN; i += ATL_s2L_NU)
   {
      ATL_SYR2L_nu(A, lda, x, y);
      n = N-i-ATL_s2L_NU;
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (n >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(n, ATL_s2L_NU, x+ATL_s2L_NU, y, y+ATL_s2L_NU, x, A+ATL_s2L_NU, lda);
      A += incA;
      x += ATL_s2L_NU;
      y += ATL_s2L_NU;
   }
   Mjoin(PATL,refsyr2L)(nx, ATL_rone, x, 1, y, 1, A, lda);
}

void Mjoin(PATL,syr2)(const enum ATLAS_UPLO Uplo, ATL_CINT N,
                      const SCALAR alpha, const TYPE *X, ATL_CINT incX,
                      const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda)
{
   void *vp=NULL;
   TYPE *x, *xt, *y, *yt;
   ATL_r2kern_t gerk;
   ATL_INT CacheElts;
   const int ALP1 = (alpha == ATL_rone);
   int nu = (Uplo == AtlasUpper) ? ATL_s2U_NU : ATL_s2L_NU;
   int mu, minM, minN, alignX, alignXt, FNU, ALIGNX2A, COPYX, COPYY;
   int ApplyAlphaToXt=0, ApplyAlphaToYt=0, YisYt, XisXt, COPYXt, COPYYt;
   size_t len;

   if (N < 1 || SCALAR_IS_ZERO(alpha))
      return;
/*
 * For small problems, avoid overhead of func calls & data copy
 */
   if (N <= ATL_S2NX)
   {
      Mjoin(PATL,refsyr2)(Uplo, N, alpha, X, incX, Y, incY, A, lda);
      return;
   }
/*
 * Determine the GER kernel to use, and its parameters
 */
   gerk = ATL_GetR2Kern(N-nu, nu, A, lda, &mu, &nu, &minM, &minN, &alignX,
                        &ALIGNX2A, &alignXt, &FNU, &CacheElts);
/*
 * See if it is OK to have transpose vectors same as no-transpose
 */
   YisYt = XisXt = ALP1;
   if (!YisYt && alignXt > sizeof(TYPE)) /* align rest may prevent */
   {
      if (ALIGNX2A)
      {
         const size_t t1 = (size_t) A;
         if ((t1/alignXt)*alignXt != t1)
            YisYt = XisXt = 0;
      }
      else if (alignXt > alignX)
      {
         if ((alignXt/alignX)*alignX != alignXt)
            YisYt = XisXt = 0;
         else
            alignX = alignXt;
      }
      else if ((alignX/alignXt)*alignXt != alignX)
         YisYt = XisXt = 0;
   }
/*
 * See if we have to copy the no-transpose vectors
 */
   COPYY = (incY != 1);
   if (!COPYY)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make Y's modulo with Cachelen match that of A if you want A & Y
 *    to have the same alignment
 */
      if (ALIGNX2A)
      {
         const size_t t1 = (size_t) A, t2 = (size_t) Y;
         COPYY = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                 (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignX)
      {
         size_t t1 = (size_t) Y;
         COPYY = ((t1/alignX)*alignX != t1);
      }
   }
   COPYX = (incX != 1);
   if (!COPYX)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make X's modulo with Cachelen match that of A if you want A & X
 *    to have the same alignment
 */
      if (ALIGNX2A)
      {
         size_t t1 = (size_t) A, t2 = (size_t) X;
         COPYX = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                 (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignX)
      {
         size_t t1 = (size_t) X;
         COPYX = ((t1/alignX)*alignX != t1);
      }
   }
/*
 * See if we have to copy the transpose vectors
 */
   COPYYt = (incY != 1);
   if (!COPYYt && alignXt > sizeof(TYPE))
   {                /* may still need copy due to alignment issues */
      size_t t1 = (size_t) Y;
      COPYYt = ((t1/alignXt)*alignXt != t1);
   }
   COPYXt = (incX != 1);
   if (!COPYXt && alignXt > sizeof(TYPE))
   {                /* may still need copy due to alignment issues */
      size_t t1 = (size_t) X;
      COPYXt = ((t1/alignXt)*alignXt != t1);
   }
/*
 * See if applying alpha will force a copy; must apply alpha to either
 * no-transpose or transpose vectors, not mixture
 */
   if (!ALP1)
   {
      if (!COPYX && !COPYXt)
         COPYX = 1;
      else
         ApplyAlphaToXt = !COPYX;
      if (ApplyAlphaToXt)
         COPYYt = ApplyAlphaToYt = 1;
      else   /* must apply alpha to Y */
         COPYY = 1;
   }
/*
 * Compute amount of space necessary to allocate any needed vectors
 */
   len = (!YisYt) ? (COPYY + COPYYt) : (COPYY || COPYYt);
   len += (!XisXt) ? (COPYX + COPYXt) : (COPYX || COPYXt);
   len *= ATL_MulBySize(N) + ATL_Cachelen;
   x = xt = (TYPE*) X;
   y = yt = (TYPE*) Y;
   if (len)
   {
      TYPE *tp;
      tp = vp = malloc(len);
      if (!vp)
      {
         Mjoin(PATL,refsyr2)(Uplo, N, alpha, X, incX, Y, incY, A, lda);
         return;
      }
      if (COPYYt)
      {
         if (YisYt)
         {
            tp = y = yt = (ALIGNX2A)?ATL_Align2Ptr(tp, A):ATL_AlignPtr(tp);
            COPYY = 0;
         }
         else
            tp = yt = ATL_AlignPtr(tp);
         tp += N;
      }
      if (COPYY)
      {
         tp = y = ALIGNX2A ? ATL_Align2Ptr(tp, A) : ATL_AlignPtr(tp);
         tp += N;
      }
      if (COPYXt)
      {
         if (XisXt)
         {
            tp = x = xt = (ALIGNX2A)?ATL_Align2Ptr(tp, A):ATL_AlignPtr(tp);
            COPYX = 0;
         }
         else
            tp = xt = ATL_AlignPtr(tp);
         tp += N;
      }
      if (COPYX)
         x = ALIGNX2A ? ATL_Align2Ptr(tp, A) : ATL_AlignPtr(tp);
   }
/*
 * Copy vector(s) to workspace with one pass through the input vectors
 */
   if (COPYX || COPYXt)
   {
      if (COPYX && COPYXt)
      {
         if (ALP1)  /* no scaling */
         {
            register ATL_INT i;
            for (i=0; i < N; i++, X += incX)
               x[i] = xt[i] = *X;
         }
         else  /* when both vecs copied, apply alpha to one of them */
         {
            register ATL_INT i;
            const register TYPE ra=alpha;
            TYPE *v, *z;
            if (ApplyAlphaToXt)
            {
               z = xt;
               v = x;
            }
            else
            {
               z = x;
               v = xt;
            }
            for (i=0; i < N; i++, X += incX)
            {
               const register TYPE rx=(*X);
               z[i] = rx * ra;
               v[i] = rx;
            }
         }
      }
      else if (COPYXt)
      {
         if (!ALP1)
            Mjoin(PATL,cpsc)(N, alpha, X, incX, xt, 1);
         else
            Mjoin(PATL,copy)(N, X, incX, xt, 1);
      }
      else if (COPYX)
      {
         if (!ALP1)
            Mjoin(PATL,cpsc)(N, alpha, X, incX, x, 1);
         else
            Mjoin(PATL,copy)(N, X, incX, x, 1);
      }
   }
   if (COPYY || COPYYt)
   {
      if (COPYY && COPYYt)
      {
         if (ALP1)  /* no scaling */
         {
            register ATL_INT i;
            for (i=0; i < N; i++, Y += incY)
               y[i] = yt[i] = *Y;
         }
         else  /* when both vecs copied, apply alpha to transposed vec */
         {
            register ATL_INT i;
            const register TYPE ra=alpha;
            TYPE *v, *z;
            if (ApplyAlphaToYt)
            {
               z = yt;
               v = y;
            }
            else
            {
               z = y;
               v = yt;
            }
            for (i=0; i < N; i++, Y += incY)
            {
               const register TYPE ry=(*Y);
               z[i] = ry * ra;
               v[i] = ry;
            }
         }
      }
      else if (COPYYt)
      {
         if (!ALP1)
            Mjoin(PATL,cpsc)(N, alpha, Y, incY, yt, 1);
         else
            Mjoin(PATL,copy)(N, Y, incY, yt, 1);
      }
      else if (COPYY)
      {
         if (!ALP1)
            Mjoin(PATL,cpsc)(N, alpha, Y, incY, y, 1);
         else
            Mjoin(PATL,copy)(N, Y, incY, y, 1);
      }
   }
   if (Uplo == AtlasUpper)
      Mjoin(PATL,syr2_kU)(gerk, N, x, yt, A, lda);
   else
      Mjoin(PATL,syr2_kL)(gerk, N, x, yt, A, lda);
   if (vp)
      free(vp);
}
