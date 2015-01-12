/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 2001 Peter Soendergaard
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
#include "atlas_lapack.h"
#include "atlas_lvl3.h"

#define REAL_RECURSE_LIMIT 4

#ifdef TREAL

#define SAFE_INVERT(A_) *(A_) = ATL_rone / *(A_);
#define TRI_MUL(A_, B_) \
{ \
     *(B_) = (*(A_)) * (*(B_)); \
}
#define TRI_NEG(A_) \
{ \
     *(A_) = - *(A_); \
}
#else

#define SAFE_INVERT(A_) Mjoin(PATL,cplxinvert)(1, A_, 1, A_, 1);
#define TRI_MUL(A_, B_) \
{ \
    (B_)[0] = ((A_)[0])*((B_)[0])-((A_)[1])*((B_)[1]); \
    (B_)[1] = ((A_)[1])*((B_)[0])+((A_)[0])*((B_)[1]); \
}
#define TRI_NEG(A_) \
{ \
     (A_)[0] = - (A_)[0]; \
     (A_)[1] = - (A_)[1]; \
}
#endif

static int ATL_trtriCU_4(const enum ATLAS_DIAG Diag, TYPE *A, const int lda)
{
    TYPE *pA0=A, *pA1=A+lda, *pA2=A+2*lda, *pA3=A+3*lda;
    TYPE A01=pA1[0], A02=pA2[0], A03=pA3[0];
    TYPE             A12=pA2[1], A13=pA3[1];
    TYPE                         A23=pA3[2];
    TYPE tmp;

    if (Diag == AtlasNonUnit)
    {
       SAFE_INVERT(pA0);
       SAFE_INVERT(pA1+1);
       SAFE_INVERT(pA2+2);
       SAFE_INVERT(pA3+3);

       pA1[0] = -A01*pA1[1]*pA0[0];
       pA2[1] = -A12*pA2[2]*pA1[1];
       pA3[2] = -A23*pA3[3]*pA2[2];

       pA2[0] = -(A01*pA2[1]+A02*pA2[2])*pA0[0];
       pA3[1] = -(A12*pA3[2]+A13*pA3[3])*pA1[1];

       pA3[0] = -(A01*pA3[1]+A02*pA3[2]+A03*pA3[3])*pA0[0];
    }
    else
    {
       pA1[0] = -A01;
       pA2[1] = -A12;
       pA3[2] = -A23;

       pA2[0] = -(A01*pA2[1]+A02);
       pA3[1] = -(A12*pA3[2]+A13);

       pA3[0] = -(A01*pA3[1]+A02*pA3[2]+A03);
    }
    return(0);
}


static int ATL_trtriCU_3(const enum ATLAS_DIAG Diag, TYPE *A, const int lda)
{
    TYPE *pA0=A, *pA1=A+lda, *pA2=A+2*lda;
    TYPE A01=pA1[0], A02=pA2[0];
    TYPE             A12=pA2[1];

    TYPE *B01 = pA1;
    TYPE *B02 = pA2;
    TYPE *B12 = pA2+1;

    TYPE tmp;

    if (Diag == AtlasNonUnit)
    {
       SAFE_INVERT(pA0);
       SAFE_INVERT(pA1+1);
       SAFE_INVERT(pA2+2);
       *B01 = -A01*pA1[1]*pA0[0];
       *B12 = -A12*pA2[2]*pA1[1];
       *B02 = -(A01*(*B12)+A02*pA2[2])*pA0[0];
    }
    else
    {
       *B01 = -A01;
       *B12 = -A12;
       *B02 = -(A01*(*B12)+A02);
    }
    return(0);
}

int ATL_trtriCU(const enum ATLAS_DIAG Diag, const int N, TYPE *A, const int lda)
{
  int ierr = 0;

   TYPE *Age, *Atr;
   TYPE tmp;
   int Nleft, Nright;
   #ifdef TREAL
      #define one ATL_rone
      #define mone -ATL_rone
      #define none ATL_rnone
   #else
      static const TYPE one[2] = {ATL_rone, ATL_rzero};
      static const TYPE mone[2] = {-ATL_rone, ATL_rzero};
      static const TYPE none[2] = {ATL_rnone, ATL_rzero};
   #endif

#ifdef TREAL
   if (N > REAL_RECURSE_LIMIT)
#else
   if (N > 1)
#endif
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;

      Age = A + ((Nleft * lda) SHIFT);
      Atr = A + (Nleft * (lda+1) SHIFT);

      cblas_trsm(AtlasColMajor, AtlasRight, AtlasUpper, AtlasNoTrans, Diag,
                  Nleft, Nright, one, Atr, lda, Age, lda);

      cblas_trsm(AtlasColMajor, AtlasLeft, AtlasUpper, AtlasNoTrans, Diag,
                  Nleft, Nright, mone, A, lda, Age, lda);

      ierr = ATL_trtriCU(Diag, Nleft, A, lda);
      if (ierr!=0) return(ierr);
      ierr = ATL_trtriCU(Diag, Nright, Atr, lda);
      if (ierr!=0) return(ierr+Nleft);
   }
   else
   {
#ifdef TREAL
     if (N==4) return(ATL_trtriCU_4(Diag,A,lda));
     else if (N==3) return(ATL_trtriCU_3(Diag,A,lda));
     else
     {
       if ( N == 2)
       {
         if (Diag == AtlasNonUnit)
         {
            SAFE_INVERT(A);
            SAFE_INVERT(A+((lda+1) SHIFT));
            TRI_MUL(A,A+((lda) SHIFT));
            TRI_MUL(A+((lda+1) SHIFT),A+((lda) SHIFT));
         }
         TRI_NEG(A+((lda) SHIFT));
       }
       else
#endif
       {
         if (Diag == AtlasNonUnit) SAFE_INVERT(A);
       }
#ifdef TREAL
     }
#endif
   }

   return(ierr);

}

