/*
 * HERK actually uses real gemm, so use real blocking factors
 */
#ifdef SCPLX
   #define CMM_H
   #include "smm.h"
#elif defined(DCPLX)
   #define ZMM_H
   #include "zmm.h"
#endif
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include "atlas_level3.h"
#include "atlas_level1.h"
#include "atlas_lapack.h"
#include <math.h>

#ifdef TCPLX
   #define llt_syrk cblas_herk
   #define llt_trans AtlasConjTrans
   #define llt_dot  Mjoin(Mjoin(cblas_,UPR),dot)
   #define llt_scal Mjoin(Mjoin(cblas_,UPR),scal)
   #define llt_rscal Mjoin(Mjoin(cblas_,UPR),scal)
#else
   #define llt_syrk cblas_syrk
   #define llt_trans AtlasTrans
#endif
/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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
#ifdef TCPLX
static int ATL_potrf2(const int N, TYPE *A, const int lda)
{
   int j, k;
   static const TYPE one[2] = {ATL_rone, ATL_rzero};
   static const TYPE none[2] = {ATL_rnone, ATL_rzero};
   const size_t lda2 = lda+lda;
   TYPE Ajj, *Ac=A, *An=A+lda2;

   for (k=j=0; j != N; j++, k += 2)
   {
      Ajj = Ac[k] - llt_dot(k, Ac, 1, Ac, 1);
      Ac[k+1] = ATL_rzero;
      if (Ajj > ATL_rzero)
      {
         Ac[k] = Ajj = sqrt(Ajj);
         if (j != N-1)
         {
            llt_scal(j, ATL_rnone, Ac+1, 2);
            cblas_gemv(CblasColMajor, CblasTrans, j, N-j-1, none,
                       An, lda, Ac, 1, one, An+k, lda);
            llt_scal(j, ATL_rnone, Ac+1, 2);
            llt_rscal(N-j-1, ATL_rone/Ajj, An+k, lda);
            Ac = An;
            An += lda2;
         }
      }
      else
      {
         Ac[k] = Ajj;
         return(j+1);
      }
   }
   return(0);
}
#else  /* real version */
static int ATL_potrf2(const int N, TYPE *A, const int lda)
{
   int j;
   TYPE Ajj, *Ac=A, *An=A+lda;

   for (j=0; j != N; j++)
   {
      Ajj = Ac[j] - cblas_dot(j, Ac, 1, Ac, 1);
      if (Ajj > ATL_rzero)
      {
         Ac[j] = Ajj = sqrt(Ajj);
         if (j != N-1)
         {
            cblas_gemv(CblasColMajor, CblasTrans, j, N-j-1, ATL_rnone,
                       An, lda, Ac, 1, ATL_rone, An+j, lda);
            cblas_scal(N-j-1, ATL_rone/Ajj, An+j, lda);
            Ac = An;
            An += lda;
         }
      }
      else
      {
         Ac[j] = Ajj;
         return(j+1);
      }
   }
   return(0);
}
#endif

static int ATL_potrfU_4(TYPE *A, const int lda)
{
   TYPE *pA1=A+lda, *pA2=pA1+lda, *pA3=pA2+lda;
   TYPE L11 = *A, L21 = *pA1, L31 = *pA2, L41 = *pA3;
   TYPE L22 = pA1[1], L32 = pA2[1], L42 = pA3[1];
   TYPE L33 = pA2[2], L43 = pA3[2];
   TYPE L44 = pA3[3];
   int iret=0;

   if (L11 > ATL_rzero)
   {
      *A = L11 = sqrt(L11);
      L11 = ATL_rone / L11;
      L21 *= L11;
      L31 *= L11;
      L41 *= L11;
      *pA1 = L21; *pA2 = L31; *pA3 = L41;
      L22 -= L21*L21;
      if (L22 > ATL_rzero)
      {
         pA1[1] = L22 = sqrt(L22);
         L22 = ATL_rone / L22;
         L32 = (L32 - L31*L21) * L22;
         L42 = (L42 - L41*L21) * L22;
         L33 -= L31*L31 + L32*L32;
         pA2[1] = L32; pA3[1] = L42;
         if (L33 > ATL_rzero)
         {
            pA2[2] = L33 = sqrt(L33);
            L43 = (L43 - L41*L31 - L42*L32) / L33;
            L44 -= L41*L41 + L42*L42 + L43*L43;
            pA3[2] = L43;
            if (L44 > ATL_rzero)
            {
               pA3[3] = sqrt(L44);
               return(0);
            }
            else iret=4;
         }
         else iret=3;
      }
      else iret=2;
   }
   else iret=1;
   return(iret);
}
static int ATL_potrfU_3(TYPE *A, const int lda)
{
   TYPE *pA1=A+lda, *pA2=pA1+lda;
   register TYPE L11 = *A, L21 = *pA1, L31 = *pA2;
   register TYPE L22=pA1[1], L32=pA2[1];
   register TYPE L33=pA2[2];
   int iret=0;

   if (L11 > ATL_rzero)
   {
      *A = L11 = sqrt(L11);
      L11 = ATL_rone / L11;
      L21 *= L11;
      L31 *= L11;
      *pA1 = L21; *pA2 = L31;
      L22 -= L21*L21;
      if (L22 > ATL_rzero)
      {
         L22 = sqrt(L22);
         L32 = (L32 - L31*L21) / L22;
         L33 -= L31*L31 + L32*L32;
         pA1[1] = L22; pA2[1] = L32;
         if (L33 > ATL_rzero)
         {
            pA2[2] = sqrt(L33);
            return(0);
         }
         else iret=3;
      }
      else iret=2;
   }
   else iret=1;
   return(iret);
}

static int ATL_potrfU_2(TYPE *A, const int lda)
{
   TYPE *pA1 = A+lda;
   register TYPE L11=*A, L21=*pA1, L22 = pA1[1];

   if (L11 > ATL_rzero)
   {
      *A = L11 = sqrt(L11);
      *pA1 = L21 = L21 / L11;
      L22 -= L21*L21;
      if (L22 > ATL_rzero)
      {
         pA1[1] = sqrt(L22);
         return(0);
      }
      else return(2);
   }
   else return(1);
}

int ATL_potrfU(const int N, TYPE *A, const int lda)
{
   TYPE *An, *Ac;
   int Nleft, Nright, ierr;
   const size_t lda2 = lda SHIFT;
   #ifdef TREAL
      #define ONE ATL_rone
   #else
      static const TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #endif

   #ifdef TREAL
      if (N > 4)
   #else
      if (N > 1)
   #endif
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB<<1) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      ierr = ATL_potrfU(Nleft, A, lda);
      if (!ierr)
      {
         Ac = A + lda2 * Nleft;
         An = Ac + (Nleft SHIFT);
         cblas_trsm(CblasColMajor, CblasLeft, CblasUpper, llt_trans,
                    CblasNonUnit, Nleft, Nright, ONE, A, lda, Ac, lda);
         llt_syrk(CblasColMajor, CblasUpper, llt_trans, Nright, Nleft,
                  ATL_rnone, Ac, lda, ATL_rone, An, lda);
         ierr = ATL_potrfU(Nright, An, lda);
         if (ierr) return(ierr+Nleft);
      }
      else return(ierr);
   }
   #ifdef TREAL
      else if (N==4) return(ATL_potrfU_4(A, lda));
      else if (N==3) return(ATL_potrfU_3(A, lda));
      else if (N==2) return(ATL_potrfU_2(A, lda));
      else if (N==1)
      {
         if (*A > ATL_rzero) *A = sqrt(*A);
         else return(1);
      }
   #else
      else if (N == 1)
      {
         if (*A > ATL_rzero)
         {
            *A = sqrt(*A);
            A[1] = ATL_rzero;
         }
         else return(1);
      }
   #endif
   return(0);
}
