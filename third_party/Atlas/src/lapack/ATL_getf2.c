#include "atlas_lapack.h"
#include "atlas_level2.h"
#include "atlas_lamch.h"

#ifdef TCPLX
   #define my_ger2 Mjoin(PATL,ger2u)
#else
   #define my_ger2 Mjoin(PATL,ger2)
#endif
static int LU1(ATL_CINT M, ATL_CINT N, ATL_CINT j, TYPE *A, ATL_CINT lda,
               int *ipiv)
/*
 * Performs an LU factorization on jth column.  N is the full width of
 * column panel, A is ptr to beginning of panel.
 * RETURNS: 0 on success, non-zero if no non-zero pivot exists
 */
{
   #ifdef TCPLX
      ATL_CINT lda2 = lda+lda;
      TYPE invs[2];
      const TYPE none[2] = {ATL_rnone, ATL_rzero};
   #else
      #define lda2 lda
      #define none ATL_rnone
   #endif
   TYPE *Ac = A + j*lda2;  /* active column */
   TYPE pivval=Ac[j];
   ATL_INT ip;

   ipiv[j] = ip = j + cblas_iamax(M-j, Ac+(j SHIFT), 1);
   #ifdef TCPLX
      pivval = Mabs(Ac[ip+ip]) + Mabs(Ac[ip+ip+1]);
   #else
      pivval = Ac[ip];
   #endif
   if (pivval != ATL_rzero)
   {
      if (ip != j)
         cblas_swap(N, A+(j SHIFT), lda, A+(ip SHIFT), lda);
      #ifdef TCPLX
         if (pivval >= ATL_laSAFMIN)
         {
            TYPE invs[2];
            Mjoin(PATL,cplxinvert)(1, Ac+j+j, 1, invs, 1);
            cblas_scal(M-j-1, invs, Ac+j+j+2, 1);
         }
         else
            Mjoin(PATL,cplxdivide)(M-j-1, Ac+j+j, Ac+j+j+2, 1, Ac+j+j+2, 1);
      #else
         if (Mabs(pivval) >= ATL_laSAFMIN)
            cblas_scal(M-j-1, ATL_rone/pivval, Ac+j+1, 1);
         else
         {
            ATL_INT i;
            for (i=j+1; i < M; i++)
               Ac[j] /= pivval;
         }
      #endif
      return(0);
   }
   return(1);
}

#ifdef TCPLX
   #define my_ger Mjoin(PATL,geru)
#else
   #define my_ger Mjoin(PATL,ger)
#endif
int Mjoin(PATL,getf2)(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT lda, int *ipiv)
{
   ATL_CINT MN = Mmin(M,N);
   ATL_INT ip;
   TYPE *Ac = A;
   ATL_INT j, iret=0;
   #ifdef TCPLX
      ATL_CINT lda2 = lda+lda;
      TYPE invs[2];
      const TYPE none[2] = {ATL_rnone, ATL_rzero};
   #else
      #define lda2 lda
      #define none ATL_rnone
   #endif

   if (M < 1 || N < 1)
      return(0);

   for (j=0; j < MN; j++, Ac += lda2)
   {
      if (LU1(M, N, j, A, lda, ipiv) && !iret)
         iret = j + 1;

      my_ger(M-j-1, N-j-1, none, Ac+((j+1)SHIFT), 1, Ac+((j+lda)SHIFT), lda,
             Ac+((lda+j+1)SHIFT), lda);
   }
   return(iret);
}
