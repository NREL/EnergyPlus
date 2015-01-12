#include "atlas_misc.h"
#include "atlas_level1.h"
#include "atlas_reflvl2.h"
#include "atlas_reflevel2.h"
#include "atlas_lvl2.h"
#if defined(ATL_INL1)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr_L1.h))
   #define ATL_her Mjoin(PATL,her_L1)
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr_L2.h))
   #define ATL_her Mjoin(PATL,her_L2)
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr.h))
   #define ATL_her Mjoin(PATL,her)
#endif

#ifdef ATL_NXTUNE
   extern int ATL_KERN_NX;
   #define ATL_S1NX ATL_KERN_NX
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syrNX.h))
   #ifndef ATL_S1NX
      #define ATL_S1NX 128
   #endif
#endif
void Mjoin(PATL,her_kU)
(
   ATL_r1kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const TYPE alpha,            /* alpha */
   const TYPE *x,               /* input vector X */
   const TYPE *xh,              /* alpha*X^H */
   TYPE *A,                     /* hermitian matrix, A = A + x*xh */
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r1kern_t gerk=gerk0;
   ATL_INT nx, j;
   TYPE one[2] = {ATL_rone, ATL_rzero};
   ATL_CINT lda2 = lda+lda;
   ATL_CINT NN = (N/ATL_s1U_NU)*ATL_s1U_NU;

   nx = (ATL_S1NX >= ATL_s1U_NU) ? (ATL_S1NX/ATL_s1U_NU)*ATL_s1U_NU : ATL_s1U_NU;
   nx = Mmin(nx, N);
   Mjoin(PATL,refherU)(nx, alpha, x, 1, A, lda);
   for (j=nx; j < NN; j += ATL_s1U_NU)
   {
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (j >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(j, ATL_s1U_NU, x, xh+j+j, A+j*lda2, lda);
      ATL_HER1U_nu(A+j*(lda2+2), lda, x+j+j, xh+j+j);
   }
   nx = N - j;
   if (nx)
   {
      ATL_GENGERK(j, nx, x, xh+j+j, A+j*lda2, lda);
      Mjoin(PATL,refherU)(nx, alpha, x+j+j, 1, A+j*(lda2+2), lda);
   }
}

void Mjoin(PATL,her_kL)
(
   ATL_r1kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const TYPE alpha,            /* alpha */
   const TYPE *x,               /* input vector X */
   const TYPE *xh,              /* alpha*X^H */
   TYPE *A,                     /* hermitian matrix, A = A + x*xh */
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r1kern_t gerk=gerk0;
   ATL_INT nx=Mmin(ATL_S1NX,N), i, NN, n;
   ATL_CINT lda2 = lda+lda;
   const TYPE one[2] = {ATL_rone, ATL_rzero};

   i = N - nx;
   i = (i/ATL_s1L_NU)*ATL_s1L_NU;
   if (i != N-nx)
      nx += N-nx-i;
   NN = N - nx;
   for (i=0; i < NN; i += ATL_s1L_NU)
   {
      ATL_HER1L_nu(A, lda, x, xh);
      n = N-i-ATL_s1L_NU;
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (n >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(n, ATL_s1L_NU, x+ATL_s1L_NU+ATL_s1L_NU, xh, A+ATL_s1L_NU+ATL_s1L_NU, lda);
      A += ATL_s1L_NU*(lda2+2);
      xh += ATL_s1L_NU+ATL_s1L_NU;
      x += ATL_s1L_NU+ATL_s1L_NU;
   }
   Mjoin(PATL,refher)(AtlasLower, nx, alpha, x, 1, A, lda);
}

void ATL_her(const enum ATLAS_UPLO Uplo, ATL_CINT N, const TYPE alpha,
               const TYPE *X, ATL_CINT incX, TYPE *A, ATL_CINT lda)
{
   const TYPE calpha[2] = {alpha, ATL_rzero};
   void *vp=NULL;
   TYPE *x, *xt;
   ATL_r1kern_t gerk;
   ATL_INT CacheElts;
   const int ALP1 = (alpha == ATL_rone);
   int COPYX, COPYXt;
   int mu, nu, minM, minN, alignX, alignXt, FNU, ALIGNX2A;
   if (N < 1 || (alpha == ATL_rzero))
      return;
/*
 * For very small problems, avoid overhead of func calls & data copy
 */
   if (N < 50)
   {
      Mjoin(PATL,refher)(Uplo, N, alpha, X, incX, A, lda);
      return;
   }
/*
 * Determine the GER kernel to use, and its parameters
 */
   gerk = ATL_GetR1Kern(N-ATL_s1L_NU, ATL_s1L_NU, A, lda, &mu, &nu,
                        &minM, &minN, &alignX, &ALIGNX2A, &alignXt,
                        &FNU, &CacheElts);
/*
 * Determine if we need to copy the vectors
 */
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
   vp = malloc((ATL_Cachelen+ATL_MulBySize(N))*(1+COPYX));
   if (!vp)
   {
      Mjoin(PATL,refher)(Uplo, N, alpha, X, incX, A, lda);
      return;
   }
   xt = ATL_AlignPtr(vp);
   if (COPYX)
   {
      x = xt + N+N;
      x = ALIGNX2A ? ATL_Align2Ptr(x, A) : ATL_AlignPtr(x);
      Mjoin(PATL,copy)(N, X, incX, x, 1);
   }
   else
      x = (TYPE*) X;
   if (ALP1)
      Mjoin(PATL,copyConj)(N, X, incX, xt, 1);
   else
      Mjoin(PATL,moveConj)(N, calpha, X, incX, xt, 1);
   if (Uplo == AtlasUpper)
      Mjoin(PATL,her_kU)(gerk, N, alpha, x, xt, A, lda);
   else
      Mjoin(PATL,her_kL)(gerk, N, alpha, x, xt, A, lda);
   if (vp)
     free(vp);
}
