/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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

#ifdef TREAL

#ifdef SDREAL
   #ifdef UnitDiag_
   void Mjoin(ATL_sdtrcopyU2L_U),NM)
   #else
   void Mjoin(PATL_sdtrcopyU2L_N,NM)
   #endif
   (const int N, const double alpha0, const float *A, const int lda, double *C)
#else
   #ifdef UnitDiag_
   void Mjoin(Mjoin(PATL,trcopyU2L_U),NM)
   #else
   void Mjoin(Mjoin(PATL,trcopyU2L_N),NM)
   #endif
      (const int N, const SCALAR alpha0, const TYPE *A, const int lda, TYPE *C)
#endif
/*
 * Takes an Upper matrix, transposes it, and copies it to a dense matrix with
 * zeroes above the diagonal (i.e., lower form)
 */
{
   int i, j;
   const int ldap1 = lda+1;
   const register TYPE alpha=alpha0;
   const TYPE *Ac = A, *Ar;
   TYPE *rC, *cC=C;

   if (N > 1)
   {
      for (j=0; j != N; j++)
      {
         for (i=0; i != j; i++) C[i] = 0.0;
         #ifdef UnitDiag_
            C[j] = alpha;
         #else
            C[j] = ATL_MulByALPHA(*Ac);
         #endif
         Ar = Ac + lda;
         for (i=j+1; i < N; i++, Ar += lda) C[i] = ATL_MulByALPHA(*Ar);
         C += N;
         Ac += ldap1;
      }
   }
   else if (N == 1)
   {
      #ifdef UnitDiag_
         *C = alpha;
      #else
         *C = ATL_MulByALPHA(*A);
      #endif
   }
}

#else

#ifdef SDCPLX
#ifdef UnitDiag_
   #ifdef ConjTrans_
      void ATL_cztrcopyU2Lc_U
   #else
      void ATL_cztrcopyU2L_U
   #endif
#else
   #ifdef ConjTrans_
      void ATL_cztrcopyU2Lc_N
   #else
      void ATL_cztrcopyU2L_N
   #endif
#endif
   (const int N, const float *A, const int lda, TYPE *C)
#else
#ifdef UnitDiag_
   #ifdef ConjTrans_
      void Mjoin(PATL,trcopyU2Lc_U)
   #else
      void Mjoin(PATL,trcopyU2L_U)
   #endif
#else
   #ifdef ConjTrans_
      void Mjoin(PATL,trcopyU2Lc_N)
   #else
      void Mjoin(PATL,trcopyU2L_N)
   #endif
#endif
   (const int N, const TYPE *A, const int lda, TYPE *C)
#endif
/*
 * Takes an Upper matrix, transposes it, and copies it to a dense matrix with
 * zeroes above the diagonal (i.e., lower form)
 */
{
   int i, j2;
   const int lda2=lda<<1, N2=N<<1, ldap1=lda2+2;
   #define ldc N2
   const TYPE *a;
   #ifdef UnitDiag_
      const TYPE one=1.0, zero=0.0;
   #else
      const TYPE zero=0.0;
   #endif

   for (j2=0; j2 != N2; j2 += 2)
   {
      for (i=0; i != j2; i += 2) C[i] = C[i+1] = zero;
      #ifdef UnitDiag_
         C[j2] = one;
         C[j2+1] = zero;
      #else
         C[j2] = *A;
         #ifdef ConjTrans_
            C[j2+1] = -A[1];
         #else
            C[j2+1] = A[1];
         #endif
      #endif
      a = A + lda2;
      if (j2 != N2)
      {
         for (i=j2+2; i != N2; i += 2)
         {
            C[i] = *a;
            #ifdef ConjTrans_
               C[i+1] = -a[1];
            #else
               C[i+1] = a[1];
            #endif
            a += lda2;
         }
      }
      A += ldap1;
      C += ldc;
   }
}

#endif

