#include "atlas_misc.h"
#include "atlas_level1.h"
#include "atlas_lvl2.h"
#include "atlas_reflvl2.h"
#include "atlas_reflevel2.h"
#if defined(ATL_INL1)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),her2_L1.h))
   #define ATL_her2 Mjoin(PATL,her2_L1)
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),her2_L2.h))
   #define ATL_her2 Mjoin(PATL,her2_L2)
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),her2.h))
   #define ATL_her2 Mjoin(PATL,her2)
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


void Mjoin(PATL,her2_kU)
(
   ATL_r2kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const SCALAR alpha,          /* need orig alpha to pass to ref blas */
   const TYPE *x,               /* input vector x */
   const TYPE *xh,              /* conj(alpha)*x^H */
   const TYPE *y,               /* input vector y */
   const TYPE *yh,              /* alpha * y^H */
   TYPE *A,                     /* hermitian matrix, A = A + x*y^H + y*x^H*/
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r2kern_t gerk=gerk0;
   ATL_INT nx=(ATL_S2NX/ATL_s2U_NU)*ATL_s2U_NU, j;
   TYPE one[2] = {ATL_rone, ATL_rzero};
   ATL_CINT lda2 = lda+lda;
   ATL_CINT NN = (N/ATL_s2U_NU)*ATL_s2U_NU;

   nx = (ATL_S2NX >= ATL_s2U_NU) ? (ATL_S2NX/ATL_s2U_NU)*ATL_s2U_NU
        : ATL_s2U_NU;
   nx = Mmin(nx,N);
   Mjoin(PATL,refher2U)(nx, alpha, x, 1, y, 1, A, lda);
   if (nx == N)
     return;
   for (j=nx; j < NN; j += ATL_s2U_NU)
   {
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (j >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(j, ATL_s2U_NU, x, yh+j+j, y, xh+j+j, A+j*lda2, lda);
      ATL_HER2U_nu(A+j*(lda2+2), lda, x+j+j, y+j+j, xh+j+j, yh+j+j);
   }
   nx = N - NN;
   if (nx)
   {
      ATL_GENGERK(j, nx, x, yh+j+j, y, xh+j+j, A+j*lda2, lda);
      Mjoin(PATL,refher2U)(nx, alpha, x+j+j, 1, y+j+j, 1, A+j*(lda2+2), lda);
   }
}

void Mjoin(PATL,her2_kL)
(
   ATL_r2kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const SCALAR alpha,          /* need orig alpha to pass to ref blas */
   const TYPE *x,               /* input vector x */
   const TYPE *xh,              /* conj(alpha)*x^H */
   const TYPE *y,               /* input vector y */
   const TYPE *yh,              /* alpha * y^H */
   TYPE *A,                     /* hermitian matrix, A = A + x*y^H + y*x^H*/
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r2kern_t gerk=gerk0;
   ATL_INT nx=Mmin(ATL_S2NX,N), i, NN, n;
   ATL_CINT lda2 = lda+lda, incA =  ATL_s2L_NU*(lda2+2);
   const TYPE one[2] = {ATL_rone, ATL_rzero};

   i = N - nx;
   i = (i/ATL_s2L_NU)*ATL_s2L_NU;
   if (i != N-nx)
      nx += N-nx-i;
   NN = N - nx;
   for (i=0; i < NN; i += ATL_s2L_NU)
   {
      ATL_HER2L_nu(A, lda, x, y, xh, yh);
      n = N-i-ATL_s2L_NU;
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (n >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(n, ATL_s2L_NU, x+ATL_s2L_NU+ATL_s2L_NU, yh, y+ATL_s2L_NU+ATL_s2L_NU,
           xh, A+ATL_s2L_NU+ATL_s2L_NU, lda);
      A += incA;
      xh += ATL_s2L_NU+ATL_s2L_NU;
      x += ATL_s2L_NU+ATL_s2L_NU;
      yh += ATL_s2L_NU+ATL_s2L_NU;
      y += ATL_s2L_NU+ATL_s2L_NU;
   }
   Mjoin(PATL,refher2L)(nx, alpha, x, 1, y, 1, A, lda);
}

void Mjoin(PATL,her2)(const enum ATLAS_UPLO Uplo, ATL_CINT N,
                      const SCALAR alpha, const TYPE *X, ATL_CINT incX,
                      const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda)
{
   const TYPE calpha[2]={*alpha, -alpha[1]};
   void *vp;
   TYPE *x, *xh, *y, *yh;
   ATL_r2kern_t gerk;
   ATL_INT CacheElts;
   ATL_CINT N2 = N+N, incX2=incX+incX, incY2=incY+incY;
   const int ALP1 = (alpha[0] == ATL_rone && alpha[1] == ATL_rzero);
   int nu = (Uplo == AtlasUpper) ? ATL_s2U_NU : ATL_s2L_NU;
   int mu, minM, minN, alignX, alignY, FNU, ALIGNX2A, COPYX, COPYY;
   size_t len;

   if (N < 1 || SCALAR_IS_ZERO(alpha))
      return;
/*
 * For small problems, avoid overhead of func calls & data copy
 */
   if (N <= ATL_S2NX)
   {
      Mjoin(PATL,refher2)(Uplo, N, alpha, X, incX, Y, incY, A, lda);
      return;
   }

   gerk = ATL_GetR2Kern(N-nu, nu, A, lda, &mu, &nu, &minM, &minN, &alignX,
                        &ALIGNX2A, &alignY, &FNU, &CacheElts);
   COPYX = (incX != 1);
   if (!COPYX)  /* may still need to copy due to alignment issues */
   {
      if (ALIGNX2A)
      {
         const size_t t1 = (size_t) A, t2 = (size_t) X;
         COPYX = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                  (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignX)
      {
         const size_t t1 = (size_t) X;
         COPYX = ((t1/alignX)*alignX != t1);
      }
   }
   COPYY = (incY != 1);
   if (!COPYY)  /* may still need to copy due to alignment issues */
   {
/*
 *    ATL_Cachelen is the highest alignment that can be requested, so
 *    make Y's modulo with Cachelen match that of A if you want A & Y to have
 *    the same alignment;  We use Y in the same way as X in GER2, so
 *    its alignment is controlled by the X align settings, while
 *    Y^H's alignment is controlled by the Y align settings.
 */
      if (ALIGNX2A)
      {
         const size_t t1 = (size_t) A, t2 = (size_t) Y;
         COPYY = (t1 - ATL_MulByCachelen(ATL_DivByCachelen(t1))) !=
                  (t2 - ATL_MulByCachelen(ATL_DivByCachelen(t2)));
      }
      else if (alignY)
      {
         const size_t t1 = (size_t) Y;
         COPYY = ((t1/alignX)*alignX != t1);
      }
   }
/*
 * Allocate work vectors, and setup vector pointers
 */
   len = ATL_Cachelen + ATL_MulBySize(N);
   len *= 2+COPYY+COPYX;
   vp = malloc(len);
   if (!vp)
   {
      Mjoin(PATL,refher2)(Uplo, N, alpha, X, incX, Y, incY, A, lda);
      return;
   }
   xh = ATL_AlignPtr(vp);
   yh = xh + N + N;
   yh = ATL_AlignPtr(yh);
   y = yh + N + N;
   if (COPYX)
   {
      x = ALIGNX2A ? ATL_Align2Ptr(y, A) : ATL_AlignPtr(y);
      y = x + N + N;
   }
   else
      x = (TYPE*)X;
   if (COPYY)
      y = ALIGNX2A ? ATL_Align2Ptr(y, A) : ATL_AlignPtr(y);
   else
      y = (TYPE*)Y;
/*
 * Copy/scale work vectors using only one pass through input vectors
 */
   if (ALP1)  /* no scaling required, just conjugation */
   {
      if (COPYX)
      {
         register ATL_INT i;
         for (i=0; i < N2; i += 2, X += incX2)
         {
            const register TYPE ix = X[1];
            x[i] = xh[i] = *X;
            x[i+1] = ix;
            xh[i+1] = -ix;
         }
      }
      else
         Mjoin(PATL,copyConj)(N, X, incX, xh, 1);
      if (COPYY)
      {
         register ATL_INT i;
         for (i=0; i < N2; i += 2, Y += incY2)
         {
            const register TYPE iy = Y[1];
            y[i] = yh[i] = *Y;
            y[i+1] = iy;
            yh[i+1] = -iy;
         }
      }
      else
         Mjoin(PATL,copyConj)(N, Y, incY, yh, 1);
   }
   else if (alpha[1] == ATL_rzero) /* must apply real scalar to conjvecs */
   {
      const register TYPE ra = *alpha;
      if (COPYX)
      {
         register ATL_INT i;
         for (i=0; i < N2; i += 2, X += incX2)
         {
            const register TYPE rx = *X, ix = X[1];
            x[i] = rx;
            x[i+1] = ix;
            xh[i] = ra * rx;
            xh[i+1] = ra * (-ix);
         }
      }
      else /* xh = conj(alpha*x) */
         Mjoin(PATL,moveConj)(N, calpha, X, incX, xh, 1);
      if (COPYY)
      {
         register ATL_INT i;
         for (i=0; i < N2; i += 2, Y += incY2)
         {
            const register TYPE ry = *Y, iy = Y[1];
            y[i] = ry;
            y[i+1] = iy;
            yh[i] = ra * ry;
            yh[i+1] = ra * (-iy);
         }
      }
      else /* yh = alpha*conj(y) */
         Mjoin(PATL,moveConj)(N, alpha, Y, incY, yh, 1);
   }
   else /* must apply complex scalar to conjugated vecs */
   {
      const register TYPE ra = *alpha, ia = alpha[1];
      if (COPYX)
      {
         register ATL_INT i;
         for (i=0; i < N2; i += 2, X += incX2)
         {
            const register TYPE rx = *X, ix = X[1];
            x[i] = rx;
            x[i+1] = ix;
            xh[i] = ra * rx - ia * ix;
            xh[i+1] = -(ra * ix + ia * rx);
         }
      }
      else /* xh = conj(alpha*x) */
         Mjoin(PATL,moveConj)(N, calpha, X, incX, xh, 1);
      if (COPYY)
      {
         register ATL_INT i;
         for (i=0; i < N2; i += 2, Y += incY2)
         {
            const register TYPE ry = *Y, iy = Y[1];
            y[i] = ry;
            y[i+1] = iy;
            yh[i] = ra * ry + ia * iy;
            yh[i+1] = ia * ry - ra * iy;
         }
      }
      else /* yh = alpha*conj(y) */
         Mjoin(PATL,moveConj)(N, alpha, Y, incY, yh, 1);
   }
   if (Uplo == AtlasUpper)
      Mjoin(PATL,her2_kU)(gerk, N, alpha, x, xh, y, yh, A, lda);
   else
      Mjoin(PATL,her2_kL)(gerk, N, alpha, x, xh, y, yh, A, lda);

   free(vp);
}

