#include "atlas_misc.h"
#include "atlas_level1.h"
#include "atlas_reflvl2.h"
#include "atlas_reflevel2.h"
#include "atlas_lvl2.h"
#if defined(ATL_INL1)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr_L1.h))
   #define ATL_syr Mjoin(PATL,syr_L1)
#elif defined(ATL_INL2)
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr_L2.h))
   #define ATL_syr Mjoin(PATL,syr_L2)
#else
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),syr.h))
   #define ATL_syr Mjoin(PATL,syr)
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
void Mjoin(PATL,syr_kU)
(
   ATL_r1kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const TYPE alpha,            /* alpha */
   const TYPE *x,               /* vector X -- may have alpha applied */
   const TYPE *xt,              /* X^T */
   TYPE *A,                     /* symmetric matrix, A = A + x*xt */
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r1kern_t gerk=gerk0;
   ATL_INT nx, j;
   ATL_CINT NN = (N/ATL_s1U_NU)*ATL_s1U_NU;

   nx = (ATL_S1NX >= ATL_s1U_NU) ? (ATL_S1NX/ATL_s1U_NU)*ATL_s1U_NU : ATL_s1U_NU;
   nx = Mmin(nx, N);
   Mjoin(PATL,refsyr)(AtlasUpper, nx, alpha, xt, 1, A, lda);
   for (j=nx; j < NN; j += ATL_s1U_NU)
   {
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (j >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(j, ATL_s1U_NU, x, xt+j, A+j*lda, lda);
      ATL_SYR1U_nu(A+j*(lda+1), lda, x+j, xt+j);
   }
   nx = N - j;
   if (nx)
   {
      ATL_GENGERK(j, nx, x, xt+j, A+j*lda, lda);
      Mjoin(PATL,refsyrU)(nx, alpha, xt+j, 1, A+j*(lda+1), lda);
   }
}

void Mjoin(PATL,syr_kL)
(
   ATL_r1kern_t gerk0,          /* func ptr to selected GER kernel */
   ATL_CINT N,                  /* size of prob to solve */
   const TYPE alpha,            /* alpha */
   const TYPE *x,               /* vector X -- may have alpha applied */
   const TYPE *xt,              /* X^T */
   TYPE *A,                     /* symmetric matrix, A = A + x*xt */
   ATL_CINT lda                 /* row stride of A */
)
{
   ATL_r1kern_t gerk=gerk0;
   ATL_INT nx=Mmin(ATL_S1NX,N), i, NN, n;

   i = N - nx;
   i = (i/ATL_s1L_NU)*ATL_s1L_NU;
   if (i != N-nx)
      nx += N-nx-i;
   NN = N - nx;
   for (i=0; i < NN; i += ATL_s1L_NU)
   {
      ATL_SYR1L_nu(A, lda, x, xt);
      n = N-i-ATL_s1L_NU;
      #if ATL_MIN_RESTRICTED_M > 0
         gerk = (n >= ATL_MIN_RESTRICTED_M) ? gerk0 : ATL_GENGERK;
      #endif
      gerk(n, ATL_s1L_NU, x+ATL_s1L_NU, xt, A+ATL_s1L_NU, lda);
      A += ATL_s1L_NU*(lda+1);
      xt += ATL_s1L_NU;
      x += ATL_s1L_NU;
   }
   Mjoin(PATL,refsyr)(AtlasLower, nx, alpha, xt, 1, A, lda);
}

void ATL_syr(const enum ATLAS_UPLO Uplo, ATL_CINT N, const TYPE alpha,
               const TYPE *X, ATL_CINT incX, TYPE *A, ATL_CINT lda)
{
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
      Mjoin(PATL,refsyr)(Uplo, N, alpha, X, incX, A, lda);
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
   COPYXt = COPYX = (incX != 1);
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
   if (!COPYXt && alignXt)  /* alignment might still force a copy */
   {
      size_t t1 = (size_t) X;
      COPYXt = ((t1/alignX)*alignX != t1);
   }
   if (!COPYX && !COPYXt && !ALP1)
      COPYX=1;
/*
 * See if X and Xt can legally be the same vector
 */
   if (ALP1 && (!alignXt || alignX == alignXt))
   {
      if (COPYX)
      {
         vp = malloc(ATL_MulBySize(N)+ATL_Cachelen);
         if (!vp)
         {
            Mjoin(PATL,refsyr)(Uplo, N, alpha, X, incX, A, lda);
            return;
         }
         x = xt = ALIGNX2A ? ATL_Align2Ptr(vp, A) : ATL_AlignPtr(vp);
         Mjoin(PATL,copy)(N, X, incX, x, 1);
         COPYX = 0;
      }
      else
         x = xt = (TYPE*) X;
   }
   else if (COPYX || COPYXt)
   {
      vp = malloc((COPYX+COPYXt)*(ATL_Cachelen + ATL_MulBySize(N)));
      if (!vp)
      {
         Mjoin(PATL,refsyr)(Uplo, N, alpha, X, incX, A, lda);
         return;
      }
      if (!COPYXt)      /* apply alpha to X, orig vec Xt */
      {
         xt = (TYPE*) X;
         x = (ALIGNX2A) ? ATL_Align2Ptr(vp, A) : ATL_AlignPtr(vp);
         if (ALP1)
            Mjoin(PATL,copy)(N, X, incX, x, 1);
         else
            Mjoin(PATL,cpsc)(N, alpha, X, incX, x, 1);
      }
      else if (!COPYX)  /* apply alpha to Xt, orig vec X */
      {
         x = (TYPE*) X;
         xt = ATL_AlignPtr(vp);
         if (ALP1)
            Mjoin(PATL,copy)(N, X, incX, xt, 1);
         else
            Mjoin(PATL,cpsc)(N, alpha, X, incX, xt, 1);
      }
      else /* copy both vectors */
      {
         xt = ATL_AlignPtr(vp);
         x = xt + N;
         x = (ALIGNX2A) ? ATL_Align2Ptr(x, A) : ATL_AlignPtr(x);
         if (ALP1)
         {
            register int i;
            for (i=0; i < N; i++, X += incX)
               xt[i] = x[i] = *X;
         }
         else
         {
            register int i;
            for (i=0; i < N; i++, X += incX)
            {
               const register TYPE rx = *X;
               xt[i] = rx;
               x[i] = alpha * rx;
            }
         }
      }
   }
   else
      x = xt = (TYPE*)X;
   if (Uplo == AtlasUpper)
      Mjoin(PATL,syr_kU)(gerk, N, alpha, x, xt, A, lda);
   else
      Mjoin(PATL,syr_kL)(gerk, N, alpha, x, xt, A, lda);
   if (vp)
     free(vp);
}
