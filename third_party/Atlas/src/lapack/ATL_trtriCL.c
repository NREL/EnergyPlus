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

static int ATL_trtriCL_4(const enum ATLAS_DIAG Diag, TYPE *A, const int lda)
{
    TYPE *pA0=A, *pA1=A+lda, *pA2=A+2*lda, *pA3=A+3*lda;
    TYPE A10=pA0[1];
    TYPE A20=pA0[2], A21=pA1[2];
    TYPE A30=pA0[3], A31=pA1[3], A32=pA2[3];

    TYPE *B10 = pA0+1;
    TYPE *B20 = pA0+2;
    TYPE *B30 = pA0+3;
    TYPE *B21 = pA1+2;
    TYPE *B31 = pA1+3;
    TYPE *B32 = pA2+3;

    TYPE tmp;

    if (Diag == AtlasNonUnit)
    {
       SAFE_INVERT(pA0);
       SAFE_INVERT(pA1+1);
       SAFE_INVERT(pA2+2);
       SAFE_INVERT(pA3+3);
       *B10 = -A10*pA0[0]*pA1[1];
       *B21 = -A21*pA1[1]*pA2[2];
       *B32 = -A32*pA2[2]*pA3[3];
       *B20 = -(A20*pA0[0]+A21*(*B10))*pA2[2];
       *B31 = -(A31*pA1[1]+A32*(*B21))*pA3[3];
       *B30 = -(A30*pA0[0]+A31*(*B10)+A32*(*B20))*pA3[3];

    }
    else
    {
       *B10 = -A10;
       *B21 = -A21;
       *B32 = -A32;
       *B20 = -(A20+A21*(*B10));
       *B31 = -(A31+A32*(*B21));
       *B30 = -(A30+A31*(*B10)+A32*(*B20));

    }
    return(0);
}

static int ATL_trtriCL_3(const enum ATLAS_DIAG Diag, TYPE *A, const int lda)
{
    TYPE *pA0=A, *pA1=A+lda, *pA2=A+2*lda;
    TYPE A10=pA0[1];
    TYPE A20=pA0[2], A21=pA1[2];

    TYPE *B10 = pA0+1;
    TYPE *B20 = pA0+2;
    TYPE *B21 = pA1+2;

    TYPE tmp;

    if (Diag == AtlasNonUnit)
    {
       SAFE_INVERT(pA0);
       SAFE_INVERT(pA1+1);
       SAFE_INVERT(pA2+2);
       *B10 = -A10*pA0[0]*pA1[1];
       *B21 = -A21*pA1[1]*pA2[2];
       *B20 = -(A20*pA0[0]+A21*(*B10))*pA2[2];
    }
    else
    {
       *B10 = -A10;
       *B21 = -A21;
       *B20 = -(A20+A21*(*B10));
    }
    return(0);
}

int ATL_trtriCL(const enum ATLAS_DIAG Diag, const int N, TYPE *A, const int lda)
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

      Age = A + (Nleft SHIFT);
      Atr = A + (Nleft * (lda+1) SHIFT);

      cblas_trsm(AtlasColMajor, AtlasRight, AtlasLower, AtlasNoTrans, Diag,
                 Nright, Nleft, one, A, lda, Age, lda);

      cblas_trsm(AtlasColMajor, AtlasLeft, AtlasLower, AtlasNoTrans, Diag,
                 Nright, Nleft, mone, Atr, lda, Age, lda);

      ierr = ATL_trtriCL(Diag, Nleft, A, lda);
      if (ierr!=0) return(ierr);
      ierr = ATL_trtriCL(Diag, Nright, Atr, lda);
      if (ierr!=0) return(ierr+Nleft);

   }
   else
   {
#ifdef TREAL
     if (N==4) return(ATL_trtriCL_4(Diag,A,lda));
     if (N==3) return(ATL_trtriCL_3(Diag,A,lda));
     else

     if ( N == 2)
     {
       if (Diag == AtlasNonUnit)
       {
          SAFE_INVERT(A);
          SAFE_INVERT(A+((lda+1) SHIFT));
          TRI_MUL(A,A+(1 SHIFT));
          TRI_MUL(A+((lda+1) SHIFT),A+(1 SHIFT));
       }
       TRI_NEG(A+(1 SHIFT));

     }
     else
#endif
     if (Diag == AtlasNonUnit) SAFE_INVERT(A);
   }

  return(ierr);

}

