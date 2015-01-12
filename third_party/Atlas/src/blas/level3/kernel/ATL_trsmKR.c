/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 2000 Antoine P. Petitet
 *
 * Code contributers : Antoine P. Petitet, R. Clint Whaley
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
#include "atlas_kern3.h"


void Mjoin(Mjoin(Mjoin(Mjoin(Mjoin(PATL,trsmK),SideNM),UploNM),N),UnitNM)
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *B, const int ldb)
/*
 * This source file is multiply compiled to create the following routines:
 * ATL_trsmKRUNU : trsm Side='Right', Uplo='Upper', Trans='N', Unit='Unit'
 * ATL_trsmKRUNN : trsm Side='Right', Uplo='Upper', Trans='N', Unit='NonUnit'
 * ATL_trsmKRLNU : trsm Side='Right', Uplo='Lower', Trans='N', Unit='Unit'
 * ATL_trsmKRLNN : trsm Side='Right', Uplo='Lower', Trans='N', Unit='NonUnit'
 *
 * The transpose cases are handled by a higher level routine that copies A
 */
{
#ifdef Left_

#define nA        M  /* size of A */
#define incA    lda  /* dot product algorithm */

#define n1B      nA  /* 1st dim of rhs: length of each rhs */
#define inc1B     1  /* Offset between two successive rhs entries  */
#define n2B       N  /* 2nd dim of rhs: Number of rhs to loop over */
#define inc2B   ldb  /* Offset between two successive rhs */

#define Minc1B(i_, inc_) ((i_))

#else

#define nA        N
#define incA      1

#define n1B      nA  /* 1st dim of rhs: length of each rhs */
#define inc1B   ldb  /* Offset between two successive rhs entries  */
#define n2B       M  /* 2nd dim of rhs: Number of rhs to loop over */
#define inc2B     1  /* Offset between two successive rhs */

#define Minc1B(i_, inc_) ((i_)*(inc_))

#endif

   register TYPE x0, x1, x2, x3, x4, x5, x6, x7, a0;

   #ifndef UnitDiag_
      const TYPE one=1.0;
      void *vp;
      TYPE *diag;
   #endif

   TYPE *X0=B, *X1=B+inc2B, *X2=X1+inc2B, *X3=X2+inc2B, *X4=X3+inc2B;
   TYPE *X5=X4+inc2B, *X6=X5+inc2B, *X7=X6+inc2B;

   const TYPE *a;

   int inc, m, n2Bs = (n2B>>3)<<3;
   register int k, kA, k1B, k1B_, k2B;


#ifdef Left_
   #ifdef Upper_
      const int ldap1 = lda + 1;
      const TYPE * Ad;
   #endif
#else
   #ifdef Lower_
      const int ldap1 = lda + 1;
      const TYPE * Ad;
   #endif
#endif

/*
 * If non-unit, precompute inverse of diagonal entries
 */
   #ifndef UnitDiag_
      vp = malloc(ATL_Cachelen+ATL_MulBySize(nA));
      ATL_assert(vp);
      diag = ATL_AlignPtr(vp);
      a = A;
      inc = lda + 1;
      for (kA=0; kA != nA; kA++)
      {
         diag[kA] = one / *a;
         a += inc;
      }
   #endif

   inc = inc2B << 3;                    /* rhs loop increment */

   for (k2B=0; k2B != n2Bs; k2B += 8)  /* Loop over the 2nd dim of rhs  */
   {
                                       /* Loop over A and B */
#ifdef Left_
   #ifdef Upper_
      Ad = A + nA*lda + nA-1;          /*    D  Ad  */
                                       /*    x  D   */
      for (kA=nA-1, k1B=(nA-1)*inc1B; kA >= 0;  kA--, k1B-=inc1B)
   #else
      for (kA=0,    k1B=0;            kA != nA; kA++, k1B+=inc1B)
   #endif
#else
   #ifdef Lower_
      Ad = A + (nA-1)*lda + nA;        /*    D  x   */
                                       /*    Ad D   */

      for (kA=nA-1, k1B=(nA-1)*inc1B; kA >= 0;  kA--, k1B-=inc1B)
   #else
      for (kA=0,    k1B=0;            kA != nA; kA++, k1B+=inc1B)
   #endif
#endif
      {
         x0 = X0[k1B];
         x0 *=  alpha;
         x1 = X1[k1B];
         x1 *=  alpha;
         x2 = X2[k1B];
         x2 *=  alpha;

#ifdef Left_
         #ifdef Upper_
            a = Ad;
            Ad -= ldap1;
         #else
            a = A + kA;
         #endif
#else
         #ifdef Lower_
            a = Ad;
            Ad -= ldap1;
         #else
            a = A + kA * lda;
         #endif
#endif

         x3 = X3[k1B];
         x3 *=  alpha;
         x4 = X4[k1B];
         x4 *=  alpha;
         x5 = X5[k1B];
         x5 *=  alpha;
         x6 = X6[k1B];
         x6 *=  alpha;
         x7 = X7[k1B];
         x7 *=  alpha;

#ifdef Left_
      #ifdef Upper_
         for (k=kA+1, k1B_=k1B+inc1B; k != nA; k++, k1B_+=inc1B)
      #else
         for (k=0,    k1B_=0;         k != kA; k++, k1B_+=inc1B)
      #endif
#else
      #ifdef Lower_
         for (k=kA+1, k1B_=k1B+inc1B; k != nA; k++, k1B_+=inc1B)
      #else
         for (k=0,    k1B_=0;         k != kA; k++, k1B_+=inc1B)
      #endif
#endif
         {
            a0 = *a;
            x0 -= a0 * X0[k1B_];
            x1 -= a0 * X1[k1B_];
            a += incA;
            x2 -= a0 * X2[k1B_];
            x3 -= a0 * X3[k1B_];
            x4 -= a0 * X4[k1B_];
            x5 -= a0 * X5[k1B_];
            x6 -= a0 * X6[k1B_];
            x7 -= a0 * X7[k1B_];
         }
         #ifndef UnitDiag_
            a0 = diag[kA];
            x0 *= a0;
            x1 *= a0;
            x2 *= a0;
            x3 *= a0;
            x4 *= a0;
            x5 *= a0;
            x6 *= a0;
            x7 *= a0;
         #endif
         X0[k1B] = x0;
         X1[k1B] = x1;
         X2[k1B] = x2;
         X3[k1B] = x3;
         X4[k1B] = x4;
         X5[k1B] = x5;
         X6[k1B] = x6;
         X7[k1B] = x7;
      }
      X0 += inc;
      X1 += inc;
      X2 += inc;
      X3 += inc;
      X4 += inc;
      X5 += inc;
      X6 += inc;
      X7 += inc;
   }

   if ( (n2Bs=n2B-n2Bs) )                     /* clean-up rhs */
   {
      inc = incA << 3;

      B = X0;

      for (k2B=0; k2B != n2Bs; k2B++)   /* Loop over the 2nd dim of rhs  */
      {
#ifdef Left_
   #ifdef Upper_
         Ad = A + nA*lda + nA-1;          /*    D  Ad  */
                                          /*    x  D   */
         for (kA=nA-1, k1B=(nA-1)*inc1B; kA >= 0;  kA--, k1B-=inc1B)
   #else
         for (kA=0,    k1B=0;            kA != nA; kA++, k1B+=inc1B)
   #endif
#else
   #ifdef Lower_
         Ad = A + (nA-1)*lda + nA;        /*    D  x   */
                                          /*    Ad D   */

         for (kA=nA-1, k1B=(nA-1)*inc1B; kA >= 0;  kA--, k1B-=inc1B)
   #else
         for (kA=0,    k1B=0;            kA != nA; kA++, k1B+=inc1B)
   #endif
#endif
         {
#ifdef Left_
         #ifdef Upper_
            X0 = (TYPE*) Ad;
            Ad -= ldap1;
         #else
            X0 = (TYPE*) A+kA;
         #endif
#else
         #ifdef Lower_
            X0 = (TYPE*) Ad;
            Ad -= ldap1;
         #else
            X0 = (TYPE*) A+kA*lda;
         #endif
#endif
            X1 = X0+incA;
            X2 = X1+incA;
            X3 = X2+incA;
            X4 = X3+incA;
            X5 = X4+incA;
            X6 = X5+incA;
            X7 = X6+incA;

            x0 = B[k1B];
            x0 *= alpha;
            x1 = x2 = x3 = x4 = x5 = x6 = x7 = ATL_rzero;

#ifdef Left_
         #ifdef Upper_
            k = kA + 1;
            m = nA - k;
            m = (m >> 3)<<3;
            for (m += k, k1B_=k1B+inc1B; k != m; k += 8, k1B_+=Minc1B(8,inc1B))
         #else
            m = (kA >> 3)<<3;
            for (k=0, k1B_=0; k != m; k += 8,  k1B_+=Minc1B(8,inc1B))
         #endif
#else
         #ifdef Lower_
            k = kA + 1;
            m = nA - k;
            m = (m >> 3)<<3;
            for (m += k, k1B_=k1B+inc1B; k != m; k += 8, k1B_+=Minc1B(8,inc1B))
         #else
            m = (kA >> 3)<<3;
            for (k=0, k1B_=0; k != m; k += 8,  k1B_+=Minc1B(8,inc1B))
         #endif
#endif
            {
               x0 -= *X0 * B[k1B_];
               X0 += inc;
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               X1 += inc;
               x2 -= *X2 * B[k1B_+Minc1B(2,inc1B)];
               X2 += inc;
               x3 -= *X3 * B[k1B_+Minc1B(3,inc1B)];
               X3 += inc;
               x4 -= *X4 * B[k1B_+Minc1B(4,inc1B)];
               X4 += inc;
               x5 -= *X5 * B[k1B_+Minc1B(5,inc1B)];
               X5 += inc;
               x6 -= *X6 * B[k1B_+Minc1B(6,inc1B)];
               X6 += inc;
               x7 -= *X7 * B[k1B_+Minc1B(7,inc1B)];
               X7 += inc;
            }

            k1B_ = Minc1B(m,inc1B);
#ifdef Left_
         #if Upper_
            switch(nA-m)
         #else
            switch(kA-m)
         #endif
#else
         #if Lower_
            switch(nA-m)
         #else
            switch(kA-m)
         #endif
#endif
            {
            case 1:
               x0 -= *X0 * B[k1B_];
               break;
            case 2:
               x0 -= *X0 * B[k1B_];
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               break;
            case 3:
               x0 -= *X0 * B[k1B_];
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               x2 -= *X2 * B[k1B_+Minc1B(2,inc1B)];
               break;
            case 4:
               x0 -= *X0 * B[k1B_];
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               x2 -= *X2 * B[k1B_+Minc1B(2,inc1B)];
               x3 -= *X3 * B[k1B_+Minc1B(3,inc1B)];
               break;
            case 5:
               x0 -= *X0 * B[k1B_];
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               x2 -= *X2 * B[k1B_+Minc1B(2,inc1B)];
               x3 -= *X3 * B[k1B_+Minc1B(3,inc1B)];
               x4 -= *X4 * B[k1B_+Minc1B(4,inc1B)];
               break;
            case 6:
               x0 -= *X0 * B[k1B_];
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               x2 -= *X2 * B[k1B_+Minc1B(2,inc1B)];
               x3 -= *X3 * B[k1B_+Minc1B(3,inc1B)];
               x4 -= *X4 * B[k1B_+Minc1B(4,inc1B)];
               x5 -= *X5 * B[k1B_+Minc1B(5,inc1B)];
               break;
            case 7:
               x0 -= *X0 * B[k1B_];
               x1 -= *X1 * B[k1B_+Minc1B(1,inc1B)];
               x2 -= *X2 * B[k1B_+Minc1B(2,inc1B)];
               x3 -= *X3 * B[k1B_+Minc1B(3,inc1B)];
               x4 -= *X4 * B[k1B_+Minc1B(4,inc1B)];
               x5 -= *X5 * B[k1B_+Minc1B(5,inc1B)];
               x6 -= *X6 * B[k1B_+Minc1B(6,inc1B)];
               break;
            default:;
            }

            x0 += x1;
            x2 += x3;
            x4 += x5;
            x6 += x7;

            x0 += x2;
            x4 += x6;
            x0 += x4;
            #ifndef UnitDiag_
               x0 *= diag[kA];
            #endif
            B[k1B] = x0;
         }

         B += inc2B;

      }
   }
   #ifndef UnitDiag_
      free(vp);
   #endif
}

