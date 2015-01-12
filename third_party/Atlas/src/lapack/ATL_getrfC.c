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
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include "atlas_level3.h"
#include "atlas_level1.h"
#include "atlas_lapack.h"
#include "atlas_lamch.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))
#ifdef ATL_USEPTHREADS
   #include "atlas_pthreads.h"
   #include "atlas_tcacheedge.h"
#else
   #include "atlas_cacheedge.h"
#endif


#ifdef TREAL
   #define ATL_luMmin 2
   #define ATL_PCAMin 1500
#else
   #define ATL_luMmin 1
   #ifdef SCPLX
      #define ATL_PCAMin 256
   #else
      #define ATL_PCAMin 512
   #endif
#endif

#ifdef TCPLX
   #define ATL_CplxInv(in, out) Mjoin(PATL,cplxinvert)(1, in, 1, out, 1);
#endif

#ifdef TREAL

static int LU2(const int M, TYPE *A, const int lda, int *ipiv)
/*
 * factors 2 cols of LU, applies all updated to 2nd call during ininitial
 * iamax to save time
 */
{
   int ip, iret=0;
   TYPE *A1 = A + lda;
   register TYPE t0, t1, scal0, scal1, amax=ATL_rzero;
   int i, imax=(-1);

   *ipiv = ip = cblas_iamax(M, A, 1);
   t0 = A[ip];
   if (t0 != ATL_rzero)
   {
      if (Mabs(t0) >= ATL_laSAFMIN)
      {
         t1 = A1[ip];
         A[ip] = *A; A1[ip] = *A1;
         *A = t0; *A1 = t1;
         scal0 = ATL_rone / t0; scal1 = -t1;
         for (i=1; i != M; i++)
         {
            t0 = A[i]; t1 = A1[i];
            t0 *= scal0;
            t1 += t0 * scal1;
            A[i] = t0; A1[i] = t1;
            if (t1 < ATL_rzero) t1 = -t1;
            if (t1 > amax) { amax = t1; imax = i; }
         }
      }
      else  /* can't safely invert pivot, so must actually divide */
      {
         t1 = A1[ip];
         A[ip] = *A; A1[ip] = *A1;
         *A = t0; *A1 = t1;
         scal0 = t0; scal1 = -t1;
         for (i=1; i != M; i++)
         {
            t0 = A[i]; t1 = A1[i];
            t0 /= scal0;
            t1 += t0 * scal1;
            A[i] = t0; A1[i] = t1;
            if (t1 < ATL_rzero) t1 = -t1;
            if (t1 > amax) { amax = t1; imax = i; }
         }
      }
      if (amax != ATL_rzero)
      {
         ipiv[1] = imax;
         t0 = A[imax]; t1 = A1[imax];
         A[imax] = A[1]; A1[imax] = A1[1];
         A[1] = t0; A1[1] = t1;
         if (Mabs(t1) >= ATL_laSAFMIN)
            cblas_scal(M-2, ATL_rone/t1, A1+2, 1);
         else
            for (i=2; i < M; i++)
               A1[i] /= t1;
      }
      else
      {
         if (imax != -1) ipiv[1] = imax;
         else ipiv[1] = 1;
         iret = 2;
      }
   }
   else
   {
      imax = 1 + cblas_iamax(M-1, A1+1, 1);
      amax = A1[imax];
      iret=1;
      if (amax != ATL_rzero)
      {
         ipiv[1] = imax;
         t0 = A[imax]; t1 = A1[imax];
         A[imax] = A[1]; A1[imax] = A1[1];
         A[1] = t0; A1[1] = t1;
         if (Mabs(t1) >= ATL_laSAFMIN)
            cblas_scal(M-2, ATL_rone/t1, A1+2, 1);
         else
            for (i=2; i < M; i++)
               A1[i] /= t1;
      }
      else
      {
         if (imax != -1) ipiv[1] = imax;
         else ipiv[1] = 1;
      }
   }
   return(iret);
}
#define MySwap(N_, A_, lda_, ip_) \
{ \
   TYPE t0_, t1_, t2_, t3_; \
   TYPE *A0_=(A_), *A1_, *A2_, *A3_; \
   int ip0_ = (ip_); \
   switch(N_) \
   { \
   case 4: \
      A1_ = A0_ + (lda_); A2_ = A1_ + (lda_); A3_ = A2_ + (lda_); \
      t0_ = *A0_; t1_ = *A1_; t2_ = *A2_; t3_ = *A3_; \
      *A0_ = A0_[ip0_]; *A1_ = A1_[ip0_]; *A2_ = A2_[ip0_]; *A3_ = A3_[ip0_]; \
      A0_[ip0_] = t0_; A1_[ip0_] = t1_; A2_[ip0_] = t2_; A3_[ip0_] = t3_; \
      break; \
   case 3: \
      A1_ = A0_ + (lda_); A2_ = A1_ + (lda_); \
      t0_ = *A0_; t1_ = *A1_; t2_ = *A2_; \
      *A0_ = A0_[ip0_]; *A1_ = A1_[ip0_]; *A2_ = A2_[ip0_]; \
      A0_[ip0_] = t0_; A1_[ip0_] = t1_; A2_[ip0_] = t2_; \
      break; \
   case 2: \
      A1_ = A0_ + (lda_); \
      t0_ = *A0_; t1_ = *A1_; \
      *A0_ = A0_[ip0_]; *A1_ = A1_[ip0_]; \
      A0_[ip0_] = t0_; A1_[ip0_] = t1_; \
      break; \
   case 1: \
      t0_ = *A0_; \
      *A0_ = A0_[ip0_]; \
      A0_[ip0_] = t0_; \
      break; \
   default: \
      cblas_swap(N_, A0_, lda_, A0_+ip0_, lda_); \
   } \
}

static int L2LU(const int M, const int N, TYPE *A, const int lda, int *ipiv)
/*
 * Level 2 based left-looking LU
 */
{
   TYPE *Ac=A;
   TYPE t0;
   const int MN=Mmin(M,N), MN_1=MN-1;
   int ip, j, jn, iret=0;

   if (N == 2) return(LU2(M, A, lda, ipiv));
   for (j=0, jn=1; j != MN; j=jn++)
   {
      ipiv[j] = ip = j + cblas_iamax(M-j, Ac+j, 1);
      t0 = Ac[ip];
      if (t0 != ATL_rzero)
      {
         MySwap(N, A+j, lda, ip-j);
         cblas_scal(M-jn, ATL_rone/t0, Ac+jn, 1);
         if (j != MN_1)
         {
            Ac += lda;
            cblas_trsv(CblasColMajor, CblasLower, CblasNoTrans, CblasUnit, jn,
                       A, lda, Ac, 1);
            cblas_gemv(CblasColMajor, CblasNoTrans, M-jn, jn, ATL_rnone,
                       A+jn, lda, Ac, 1, ATL_rone, Ac+jn, 1);
         }
      }
      else if (!iret) iret = jn;
   }
   return(iret);
}

#else

static int L2LU(const int M, const int N, TYPE *A, const int lda, int *ipiv)
/*
 * Complex Level 2 based left-looking LU
 */
{
   TYPE *Ac=A;
   TYPE t0, tmp[2];
   const TYPE one[2] = {ATL_rone, ATL_rzero}, none[2] = {ATL_rnone, ATL_rzero};
   const int MN=Mmin(M,N), MN_1=MN-1, lda2=lda+lda;
   int ip, ip2, j, j2, jn, jn2, iret=0;

   for (j=0, j2=0, jn=1, jn2=2; j != MN; j=jn++, j2 += 2, jn2 += 2)
   {
      ipiv[j] = ip = j + cblas_iamax(M-j, Ac+j2, 1);
      ip2 = ip + ip;
      if (Ac[ip2] != ATL_rzero || Ac[ip2+1] != ATL_rzero)
      {
         Mjoin(PATL,cplxinvert)(1, Ac+ip2, 1, tmp, 1);
         cblas_swap(N, A+j2, lda, A+ip2, lda);
         cblas_scal(M-jn, tmp, Ac+jn2, 1);
         if (j != MN_1)
         {
            Ac += lda2;
            cblas_trsv(CblasColMajor, CblasLower, CblasNoTrans, CblasUnit, jn,
                       A, lda, Ac, 1);
            cblas_gemv(CblasColMajor, CblasNoTrans, M-jn, jn, none,
                       A+jn2, lda, Ac, 1, one, Ac+jn2, 1);
         }
      }
      else if (!iret) iret = jn;
   }
   return(iret);
}
#endif

int ATL_getrfC(const int M, const int N, TYPE *A, const int lda, int *ipiv)
/*
 * Column-major factorization of form
 *   A = P * L * U
 * where P is a row-permutation matrix, L is lower triangular with unit diagonal
 * elements (lower trapazoidal if M > N), and U is upper triangular (upper
 * trapazoidal if M < N).  This is the recursive Level 3 BLAS version.
 */
{
   const int MN = Mmin(M, N);
   int Nleft, Nright, k, i, ierr=0;
   #ifdef TCPLX
      const TYPE one[2] = {ATL_rone, ATL_rzero};
      const TYPE none[2] = {ATL_rnone, ATL_rzero};
      TYPE inv[2], tmp[2];
   #else
      #define one ATL_rone
      #define none ATL_rnone
      TYPE tmp;
   #endif
   TYPE *Ac, *An;

   if (((size_t)M)*N <= ATL_L1elts)
      return(Mjoin(PATL,getf2)(M, N, A, lda, ipiv));
   #if defined(ATL_USEPTHREADS) && defined(ATL_USEPCA)
      if (N <= (NB<<2) && N >= 16 && M-N >= ATL_PCAMin &&
          ((size_t)ATL_MulBySize(M)*N) <= CacheEdge*ATL_NTHREADS)
      {
         if (N >= 16)
            ierr = Mjoin(PATL,tgetf2)(M, N, A, lda, ipiv);
         else
            ierr = Mjoin(PATL,tgetf2_nocp)(M, N, A, lda, ipiv);
         return(ierr);
      }
   #endif
   if (MN > ATL_luMmin)
   {
      Nleft = MN >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      i = ATL_getrfC(M, Nleft, A, lda, ipiv);  /* factor left to L & U */
      if (i) if (!ierr) ierr = i;
/*
 *    Update trailing submatrix
 */
      Ac = A + (Nleft * lda SHIFT);
      An = Ac + (Nleft SHIFT);
      ATL_laswp(Nright, Ac, lda, 0, Nleft, ipiv, 1);
      cblas_trsm(CblasColMajor, CblasLeft, CblasLower, CblasNoTrans, CblasUnit,
                 Nleft, Nright, one, A, lda, Ac, lda);
      cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M-Nleft, Nright,
                 Nleft, none, A+(Nleft SHIFT), lda, Ac, lda, one, An, lda);
      i = ATL_getrfC(M-Nleft, Nright, An, lda, ipiv+Nleft);
      if (i) if (!ierr) ierr = i + Nleft;
      for (i=Nleft; i != MN; i++) ipiv[i] += Nleft;
      ATL_laswp(Nleft, A, lda, Nleft, MN, ipiv, 1);
   }
#ifdef TREAL
   else if (MN == 2)
   {
      Nleft = 2;
      Nright = N - 2;
      ierr = LU2(M, A, lda, ipiv);
      if (!Nright) return(ierr);
/*
 *    OK, we're now in case M=2, N > 2, Nleft = 2, Nright = N-2
 */
      Ac = A + ((lda+lda)SHIFT);
      ATL_laswp(Nright, Ac, lda, 0, 2, ipiv, 1);
      cblas_trsm(CblasColMajor, CblasLeft, CblasLower, CblasNoTrans, CblasUnit,
                 Nleft, Nright, one, A, lda, Ac, lda);
   }
#endif
   else if (MN == 1)
   {
      *ipiv = i = cblas_iamax(M, A, 1);  /* find pivot */
      #ifdef TREAL
         tmp = A[i];
         if (tmp != ATL_rzero)
         {
            if (Mabs(tmp) > ATL_laSAFMIN)
               cblas_scal(M, ATL_rone/tmp, A, 1);
            else
               for (k=0; k < N; k++)
                  A[k] /= tmp;
            A[i] = *A;
            *A = tmp;
         }
         else ierr = 1;
      #else
         i <<= 1;
         tmp[0] = A[i];
         tmp[1] = A[i+1];
         if (tmp[0] != ATL_rzero || tmp[1] != ATL_rzero)
         {
            if (ATL_lapy2(tmp[0], tmp[1]) >= ATL_laSAFMIN)
            {
               ATL_CplxInv(tmp, inv);
               cblas_scal(M, inv, A, 1);
            }
            else
               Mjoin(PATL,cplxdivide)(M, tmp, A, 1, A, 1);
            A[i] = *A;
            A[i+1] = A[1];
            *A = tmp[0];
            A[1] = tmp[1];
         }
         else ierr = 1;
      #endif
   }
   return(ierr);
}

