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
static int ATL_potrf2(const int N, TYPE *A, const int lda)
{
#ifdef TREAL
    TYPE Ajj, *Ac=A;
    const int N_1 = N-1;
    int j;

    for (j=0; j != N_1; j++, Ac += lda)
    {
       Ajj = Ac[j] - cblas_dot(j, A+j, lda, A+j, lda);

       if (Ajj > ATL_rzero)
       {
          Ac[j] = Ajj = sqrt(Ajj);
          cblas_gemv(CblasColMajor, CblasNoTrans, N-j-1, j, ATL_rnone,
                     A+j+1, lda, A+j, lda, ATL_rone, Ac+j+1, 1);
          cblas_scal(N-j-1, ATL_rone/Ajj, Ac+j+1, 1);
       }
       else
       {
          Ac[j] = Ajj;
          return(j+1);
       }
    }
    Ajj = Ac[j] - cblas_dot(j, A+j, lda, A+j, lda);
    if (Ajj > ATL_rzero) Ac[j] = Ajj = sqrt(Ajj);
    else
    {
       Ac[j] = Ajj;
       return(N);
    }
#else
    TYPE Ajj, *Ac=A;
    TYPE one[2] = {ATL_rone, ATL_rzero};
    TYPE none[2] = {ATL_rnone, ATL_rzero};
    const int N_1 = N-1, lda2 = lda<<1;
    int j, j2;

    for (j2=j=0; j != N_1; j++, j2 += 2, Ac += lda2)
    {
       Ajj = Ac[j2];
       cblas_dotc_sub(j, A+j2, lda, A+j2, lda, Ac+j2);
       Ajj -= Ac[j2];

       if (Ajj > ATL_rzero)
       {
          Ac[j2] = Ajj = sqrt(Ajj);
          llt_scal(j, ATL_rnone, A+j2+1, lda2);
          cblas_gemv(CblasColMajor, CblasNoTrans, N-j-1, j, none,
                     A+j2+2, lda, A+j2, lda, one, Ac+j2+2, 1);
          llt_scal(j, ATL_rnone, A+j2+1, lda2);
          llt_scal((N-j-1)<<1, ATL_rone/Ajj, Ac+j2+2, 1);
       }
       else
       {
          Ac[j2] = Ajj;
          return(j+1);
       }
    }
    Ajj = Ac[j2];
    cblas_dotc_sub(j, A+j2, lda, A+j2, lda, Ac+j2);
    Ajj -= Ac[j2];
    if (Ajj > ATL_rzero) Ac[j2] = sqrt(Ajj);
    else
    {
       Ac[j2] = Ajj;
       return(N);
    }
#endif
   return(0);
}

static int ATL_potrfL_4(TYPE *A, const int lda)
{
   TYPE *pA1=A+lda+1, *pA2=pA1+lda+1, *pA3=pA2+lda+1;
   TYPE L11=*A, L21=A[1], L31 = A[2], L41 = A[3];
   TYPE L22=*pA1, L32=pA1[1], L42 = pA1[2];
   TYPE L33=*pA2, L43 = pA2[1];
   TYPE L44=*pA3;
   int iret=0;

   if (L11 > ATL_rzero)
   {
      *A = L11 = sqrt(L11);
      L11 = ATL_rone / L11;
      L21 *= L11;
      L31 *= L11;
      L41 *= L11;
      A[1] = L21; A[2] = L31; A[3] = L41;
      L22 -= L21*L21;
      if (L22 > ATL_rzero)
      {
         *pA1 = L22 = sqrt(L22);
         L22 = ATL_rone / L22;
         L32 = (L32 - L31*L21) * L22;
         L42 = (L42 - L41*L21) * L22;
         L33 -= L31*L31 + L32*L32;
         pA1[1] = L32; pA1[2] = L42;
         if (L33 > ATL_rzero)
         {
            *pA2 = L33 = sqrt(L33);
            L43 = (L43 - L41*L31 - L42*L32) / L33;
            L44 -= L41*L41 + L42*L42 + L43*L43;
            pA2[1] = L43;
            if (L44 > ATL_rzero)
            {
               *pA3 = sqrt(L44);
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
static int ATL_potrfL_3(TYPE *A, const int lda)
{
   TYPE *pA1=A+lda+1, *pA2=pA1+lda+1;
   register TYPE L11=*A, L21=A[1], L31 = A[2];
   register TYPE L22=*pA1, L32=pA1[1];
   register TYPE L33=*pA2;
   int iret=0;

   if (L11 > ATL_rzero)
   {
      *A = L11 = sqrt(L11);
      L11 = ATL_rone / L11;
      L21 *= L11;
      L31 *= L11;
      A[1] = L21; A[2] = L31;
      L22 -= L21*L21;
      if (L22 > ATL_rzero)
      {
         L22 = sqrt(L22);
         L32 = (L32 - L31*L21) / L22;
         L33 -= L31*L31 + L32*L32;
         *pA1 = L22; pA1[1] = L32;
         if (L33 > ATL_rzero)
         {
            *pA2 = sqrt(L33);
            return(0);
         }
         else iret=3;
      }
      else iret=2;
   }
   else iret=1;
   return(iret);
}

static int ATL_potrfL_2(TYPE *A, const int lda)
{
   register TYPE L11=*A, L21=A[1], L22 = A[lda+1];

   if (L11 > ATL_rzero)
   {
      *A = L11 = sqrt(L11);
      A[1] = L21 = L21 / L11;
      L22 -= L21*L21;
      if (L22 > ATL_rzero)
      {
         A[lda+1] = sqrt(L22);
         return(0);
      }
      else return(2);
   }
   else return(1);
}

int ATL_potrfL(const int N, TYPE *A, const int lda)
{
   TYPE *An, *Ar;
   const size_t lda2=(lda SHIFT);
   int Nleft, Nright, ierr;
   #ifdef TREAL
      #define lda2 lda
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
      ierr = ATL_potrfL(Nleft, A, lda);
      if (!ierr)
      {
         Ar = A + (Nleft SHIFT);
         An = Ar + lda2 * Nleft;
         cblas_trsm(CblasColMajor, CblasRight, CblasLower, llt_trans,
                    CblasNonUnit, Nright, Nleft, ONE, A, lda, Ar, lda);
         llt_syrk(CblasColMajor, CblasLower, CblasNoTrans, Nright, Nleft,
                  ATL_rnone, Ar, lda, ATL_rone, An, lda);
         ierr = ATL_potrfL(Nright, An, lda);
         if (ierr) return(ierr+Nleft);
      }
      else return(ierr);
   }
   #ifdef TREAL
      else if (N==4) return(ATL_potrfL_4(A, lda));
      else if (N==3) return(ATL_potrfL_3(A, lda));
      else if (N==2) return(ATL_potrfL_2(A, lda));
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
