/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2001 R. Clint Whaley
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
#include "atlas_lapack.h"
#include "cblas.h"
#include "atlas_cblastypealias.h"
#include "atlas_tst.h"
#include "atlas_level3.h"

#ifdef TimeC
   #include "clapack.h"
   #define CLP Mjoin(clapack_,PRE)
#endif
#ifdef GCCWIN
   ___main(){} __main(){} MAIN__(){} _MAIN_(){}
#endif
#define CBP Mjoin(cblas_,PRE)

double time00();

enum TEST_UPLO {TestGE=0, TestUpper=121, TestLower=122};

static void geinv
   (const enum CBLAS_ORDER Order, const int N, TYPE *A, const int lda)
{
   int *ipiv;
   TYPE *wrk;
   int lwrk;

   ipiv = malloc(sizeof(int)*N);
   ATL_assert(ipiv);
   #ifdef TimeF77
      lwrk = N * Mjoin(PATL,GetNB)();
      wrk = malloc(ATL_MulBySize(lwrk));
      if (Order == AtlasRowMajor) Mjoin(PATL,tstsqtran)(N, A, lda);
      ATL_assert(Mjoin(PATL,f77getrf)(AtlasColMajor, N, N, A, lda, ipiv) == 0);
      ATL_assert(Mjoin(PATL,f77getri)
         (AtlasColMajor, N, A, lda, ipiv, wrk, &lwrk) == 0);
      if (Order == AtlasRowMajor) Mjoin(PATL,tstsqtran)(N, A, lda);
      free(wrk);
   #elif defined(TimeC)
      ATL_assert(Mjoin(CLP,getrf)(Order, N, N, A, lda, ipiv) == 0);
      ATL_assert(Mjoin(CLP,getri)(Order, N, A, lda, ipiv) == 0);
  #else
      lwrk = N * Mjoin(PATL,GetNB)();
      wrk = malloc(ATL_MulBySize(lwrk));
      ATL_assert(Mjoin(PATL,getrf)(Order, N, N, A, lda, ipiv) == 0);
      ATL_assert(Mjoin(PATL,getri)(Order, N, A, lda, ipiv, wrk, &lwrk) == 0);
      free(wrk);
   #endif
   free(ipiv);
}

static void tsthetran(const int N, TYPE *A, const int lda)
{
   int i;
   const int lda2=lda SHIFT;
   Mjoin(PATL,tstsqtran)(N, A, lda);
#ifdef TCPLX
   for (i=0; i < N; i++) Mjoin(PATLU,scal)(N, ATL_rnone, A+1+i*lda2, 2);
#endif
}
static void poinv(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo,
                  const int N, TYPE *A, const int lda)
{
   #ifdef TimeF77
      if (Order == AtlasRowMajor) tsthetran(N, A, lda);
      ATL_assert(Mjoin(PATL,f77potrf)(Uplo, N, A, lda) == 0);
      ATL_assert(Mjoin(PATL,f77trtri)(Uplo, CblasNonUnit, N, A, lda) == 0);
      ATL_assert(Mjoin(PATL,f77lauum)(Uplo, N, A, lda) == 0);
      if (Order == AtlasRowMajor) tsthetran(N, A, lda);
   #elif defined(TimeC)
      ATL_assert(Mjoin(CLP,potrf)(Order, Uplo, N, A, lda) == 0);
      ATL_assert(Mjoin(CLP,trtri)(Order, Uplo, CblasNonUnit, N, A, lda) == 0);
      ATL_assert(Mjoin(CLP,lauum)(Order, Uplo, N, A, lda) == 0);
   #else
      ATL_assert(Mjoin(PATL,potrf)(Order, Uplo, N, A, lda) == 0);
      ATL_assert(Mjoin(PATL,trtri)(Order, Uplo, CblasNonUnit, N, A, lda) == 0);
      Mjoin(PATL,lauum)(Order, Uplo, N, A, lda);
   #endif
}

static void test_inv(const enum CBLAS_ORDER Order, const enum TEST_UPLO Uplo,
                     const int N, TYPE *A, const int lda)
{
   if (Uplo == TestGE) geinv(Order, N, A, lda);
   else poinv(Order, Uplo, N, A, lda);
}

TYPE *GetGE(int M, int N, int lda)
{
   TYPE *A;
   A = malloc(ATL_MulBySize(lda)*N);
   if (A) Mjoin(PATL,gegen)(M, N, A, lda, M*N+lda);
   return(A);
}

static void CrapUpTri
   (enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, TYPE *A, int lda)
/*
 * Puts crap on opposite triangle to Uplo, so as to ensure error on use
 */
{
   const int lda2=(lda SHIFT), ldap1=((lda+1)SHIFT);
   int j;

   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasLower) Uplo = CblasUpper;
      else Uplo = CblasLower;
   }
   if (Uplo == CblasLower)
   {
      A += lda2;
      for (j=1; j < N; j++, A += lda2)
         Mjoin(PATLU,set)(j SHIFT, -50000000.0, A, 1);
   }
   else
   {
      for (j=0; j < N; j++, A += ldap1)
         Mjoin(PATLU,set)((N-j-1)SHIFT, -5500000.0, A+(1 SHIFT), 1);
   }
}

static TYPE *DupMat(enum ATLAS_ORDER Order, int M, int N, TYPE *A, int lda,
                    int ldc)
/*
 * returns a duplicate of the A matrix, with new leading dimension
 */
{
   int i, j, M2;
   const int ldc2 = (ldc SHIFT), lda2 = (lda SHIFT);
   TYPE *C;
   if (Order == CblasRowMajor)
   {
      i = M;
      M = N;
      N = i;
   }
   M2 = M SHIFT;
   ATL_assert(ldc >= M);
   C = malloc(ATL_MulBySize(ldc)*N);
   ATL_assert(C);
   for (j=0; j != N; j++)
   {
      for (i=0; i != M2; i++) C[i] = A[i];
      C += ldc2;
      A += lda2;
   }
   return(C-N*ldc2);
}

#include <math.h>
static void PosDefGen
   (enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, TYPE *A, int lda)
/*
 * Generates a reasonably conditioned positive definite matrix
 */
{
   TYPE *aa, *L;
   TYPE val, bias, sign;
   int j;
   const int lda2=(lda SHIFT), ldap1=((lda+1)SHIFT);

   Mjoin(PATL,gegen)(N, N, A, lda, N*N+lda);
   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasLower) Uplo = CblasUpper;
      else Uplo = CblasLower;
   }
/*
 * It should be enough to make diagonal non-zero, but small numbers are very
 * ill-conditioned, and therefore may not be solvable in practice.  Therefore,
 * scale the diagonal by log(N).
 */
   bias = log(N);
   bias = (bias < 1.0) ? 1.0 : bias;
   for (aa=A,j=0; j < N; j++, aa += ldap1)
   {
      val = *aa;
      sign = (val < 0.0) ? -1.0 : 1.0;
      val = (val < 0.0) ? -val : val;
      val = (val+bias)*sign;
      *aa = val;
   }
/*
 * For imaginary numbers, force zero imaginary component on diagonal
 */
   #ifdef TCPLX
      Mjoin(Mjoin(ATL_,UPR),set)(N, 0.0, A+1, ldap1);
   #endif
/*
 * Zero non-active portion of matrix
 */
   if (Uplo == CblasLower)
   {
      for (j=0, aa=A; j < N; j++, aa += lda2)
         Mjoin(PATL,zero)(j, aa, 1);
   }
   else
   {
      for (j=0, aa=A+(1 SHIFT); j < N; j++, aa += ldap1)
         Mjoin(PATL,zero)(N-j-1, aa, 1);
   }
/*
 * Force A = L * L', where L is Lower or Upper as requested, to make pos def
 */
   L = DupMat(CblasColMajor, N, N, A, lda, N);
   #ifdef TCPLX
      Mjoin(CBP,herk)(CblasColMajor, Uplo, CblasNoTrans, N, N, ATL_rone, L, N,
                      ATL_rzero, A, lda);
   #else
      Mjoin(CBP,syrk)(CblasColMajor, Uplo, CblasNoTrans, N, N, ATL_rone, L, N,
                      ATL_rzero, A, lda);
#endif
   free(L);
/*
 * Make sure non-triangular elements are bad for error detection
 */
   CrapUpTri(CblasColMajor, Uplo, N, A, lda);
}

static void MakeHEDiagDom
   (enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, TYPE *A, int lda)
/*
 * Makes hermitian matrix diagonally dominant
 */
{
   int j;
   const int lda2=(lda SHIFT), ldap1=((lda+1)SHIFT);

   Mjoin(PATL,gegen)(N, N, A, lda, N*N+lda);
   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasLower) Uplo = CblasUpper;
      else Uplo = CblasLower;
   }
   if (Uplo == CblasLower)
   {
      for (j=0; j < N; j++, A += ldap1)
      {
         #ifdef TREAL
            *A = 1.0 + cblas_asum(N-j, A, 1);
            *A += cblas_asum(j, A-lda*j, lda);
         #elif defined(SCPLX)
            *A = 1.0 + cblas_scasum(N-j, A, 1);
            *A += cblas_scasum(j, A-lda2*j, lda);
         #else
            *A = 1.0 + cblas_dzasum(N-j, A, 1);
            *A += cblas_dzasum(j, A-lda2*j, lda);
         #endif
         #ifdef TCPLX
            A[1] = ATL_rzero;
         #endif
      }
   }
   else /* Upper */
   {
      for (j=0; j < N; j++, A += ldap1)
      {
         #ifdef TREAL
            *A = 1.0 + cblas_asum(N-j, A, lda);
            *A += cblas_asum(j, A-j, 1);
         #else
            #ifdef SCPLX
               *A = 1.0 + cblas_scasum(N-j, A, lda);
               *A += cblas_scasum(j, A-j*2, 1);
            #else
               *A = 1.0 + cblas_dzasum(N-j, A, lda);
               *A += cblas_dzasum(j, A-j*2, 1);
            #endif
            A[1] = ATL_rzero;
         #endif
      }
   }
}

static void hegen
   (enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, TYPE *A, int lda)
{
#ifdef POSDEFGEN
   PosDefGen(Order, Uplo, N, A, lda);
#else
   MakeHEDiagDom(Order, Uplo, N, A, lda);
   CrapUpTri(Order, Uplo, N, A, lda);
#endif
}

static TYPE *GetHE(enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, int lda)
/*
 * Gets symm/hemm matrix, and puts a bunch of crap in other side to make
 * sure factorization doesn't use it, and makes pos def by making it
 * diag dominant
 */
{
   TYPE *A;

   A = GetGE(N, N, lda);
   if (!A) return(NULL);
   hegen(Order, Uplo, N, A, lda);
   return(A);
}

static void ReflectMat
   (enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, TYPE *A, int lda)
/*
 * Takes a symmetric matrix, and makes it general by reflecting across diagonal
 */
{
   const int lda2 = (lda SHIFT);
   int j;

   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasUpper) Uplo = CblasLower;
      else Uplo = CblasUpper;
   }
   if (Uplo == CblasUpper)
   {
      for (j=0; j < N; j++) cblas_copy(j, A+j*lda2, 1, A+(j SHIFT), lda);
   }
   else /* lower matrix */
   {
      for (j=0; j < N; j++) cblas_copy(j, A+(j SHIFT), lda,  A+j*lda2, 1);
   }
}
static void ReflectHE
   (enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo, int N, TYPE *A, int lda)
/*
 * Reflects matrix, makes it hermitian
 */
{
   #ifdef TREAL
      ReflectMat(Order, Uplo, N, A, lda);
   #else
   const int lda2 = (lda SHIFT);
   int j;

   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasUpper) Uplo = CblasLower;
      else Uplo = CblasUpper;
   }
   if (Uplo == CblasUpper)
   {
      for (j=0; j < N; j++)
      {
         cblas_copy(j, A+j*lda2, 1, A+(j SHIFT), lda);
         Mjoin(PATLU,scal)(j, ATL_rnone, A+(j SHIFT)+1, lda*2);
      }
   }
   else /* lower matrix */
   {
      for (j=0; j < N; j++)
      {
         cblas_copy(j, A+(j SHIFT), lda,  A+j*lda2, 1);
         Mjoin(PATLU,scal)(j, ATL_rnone, A+j*lda2+1, 2);
      }
   }
   Mjoin(PATLU,zero)(N, A+1, (lda+1)SHIFT);
   #endif
}

static TYPE *GetMat(enum CBLAS_ORDER Order, enum TEST_UPLO Uplo, int N, int lda)
{
   if (Uplo == TestGE) return(GetGE(N, N, lda));
   return(GetHE(Order, (enum CBLAS_UPLO)Uplo, N, lda));
}

static TYPE geresid(enum CBLAS_ORDER Order, int N, TYPE *A, int lda,
                    TYPE *AI, int ldi)
/*
 * returns ||A - AI|| / (N * eps * ||A|| * ||AI||);
 * for row-major, we are not using 1-norm, since we are adding rows instead
 * of cols, but it should be an equally good norm, so don't worry about it.
 */
{
   TYPE numer, denom, eps;
   const int ldcp1 = (N+1)SHIFT;
   TYPE *C;
   int i;

   #ifdef TREAL
      TYPE one = ATL_rone, zero = ATL_rzero;
   #else
      TYPE one[2] = {ATL_rone, ATL_rzero}, zero[2] = {ATL_rzero, ATL_rzero};
   #endif

   eps = Mjoin(PATL,epsilon)();
   C = malloc(N*ATL_MulBySize(N));
   ATL_assert(C);
   cblas_gemm(Order, CblasNoTrans, CblasNoTrans, N, N, N, one, A, lda,
              AI, ldi, zero, C, N);                /* C now has A*inv(A) */
   for (i=0; i != N; i++) C[i*ldcp1] -= ATL_rone;  /* C now has A*inv(A)-I */
   numer = Mjoin(PATL,genrm1)(N, N, C, N);
   denom = Mjoin(PATL,genrm1)(N, N, A, lda) *
           Mjoin(PATL,genrm1)(N, N, AI, ldi) * N * eps;
   free(C);
   return(numer/denom);
}

static TYPE poresid(enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo,
                    int N, TYPE *A, int lda, TYPE *AI, int ldi)
/*
 * returns ||A - AI|| / (N * eps * ||A|| * ||AI||);
 */
{
   enum CBLAS_UPLO uplo=Uplo;
   TYPE numer, denom, eps;
   const int ldcp1 = (N+1)SHIFT;
   int i;
   #ifdef TREAL
      TYPE one = ATL_rone, zero = ATL_rzero;
   #else
      TYPE one[2] = {ATL_rone, ATL_rzero}, zero[2] = {ATL_rzero, ATL_rzero};
   #endif
   TYPE *C, *B;

   C = malloc(N*ATL_MulBySize(N));
   ATL_assert(C);
   B = DupMat(Order, N, N, AI, ldi, N);
   ReflectHE(Order, Uplo, N, B, N);
   #ifdef TREAL
      cblas_symm(Order, CblasRight, Uplo, N, N, one, A, lda, B, N, zero, C, N);
   #else
      cblas_hemm(Order, CblasRight, Uplo, N, N, one, A, lda, B, N, zero, C, N);
   #endif
   free(B);
   eps = Mjoin(PATL,epsilon)();
   if (Order == CblasRowMajor)
      uplo = (Uplo == CblasUpper) ? CblasLower : CblasUpper;
   for (i=0; i != N; i++) C[i*ldcp1] -= ATL_rone;  /* C now has A*inv(A)-I */
   numer = Mjoin(PATL,genrm1)(N, N, C, N);
   #ifdef TREAL
      denom = Mjoin(PATL,synrm)(uplo, N, A, lda) *
              Mjoin(PATL,synrm)(uplo, N, AI, ldi) * N * eps;
   #else
      denom = Mjoin(PATL,henrm)(uplo, N, A, lda) *
              Mjoin(PATL,henrm)(uplo, N, AI, ldi) * N * eps;
   #endif
   free(C);
   return(numer/denom);
}

static TYPE GetResid(enum CBLAS_ORDER Order, enum TEST_UPLO Uplo,
                     int N, TYPE *A, int lda, TYPE *AI, int ldi)
{
   TYPE ret;
   if (Uplo == TestGE) ret = geresid(Order, N, A, lda, AI, ldi);
   else ret = poresid(Order, (enum CBLAS_UPLO)Uplo, N, A, lda, AI, ldi);
   return(ret);
}

static double RunTest
   (enum CBLAS_ORDER Order, enum TEST_UPLO Uplo, int N, int lda,
    int CacheSize, TYPE *res)
{
   TYPE *A, *AI, *C;
   int ierr;
   double t0, t1;

   A  = GetMat(Order, Uplo, N, lda);
   #ifdef DEBUG
      Mjoin(PATL,geprint)("A0", N, N, A, lda);
   #endif
   AI = DupMat(Order, N, N, A, lda, lda);
   t0 = ATL_flushcache(CacheSize);

   t0 = ATL_flushcache(-1);
   t0 = time00();
   test_inv(Order, Uplo, N, AI, lda); /* AI should now have inverse(A) */
   t1 = time00() - t0;
   t0 = ATL_flushcache(0);
   #ifdef DEBUG
      Mjoin(PATL,geprint)("A ", N, N, A, lda);
      Mjoin(PATL,geprint)("AI", N, N, AI, lda);
   #endif

   *res = GetResid(Order, Uplo, N, A, lda, AI, lda);
   free(AI);
   free(A);
   return(t1);
}

static double RunTiming
   (enum CBLAS_ORDER Order, enum TEST_UPLO Uplo, int N, int lda,
    size_t CacheSize, int nreps)
{
   TYPE *A, *a;
   const size_t incA = N*lda SHIFT;
   size_t i, k;
   double t0, t1=0.0;

   if (nreps < 1) nreps = 1;
   i = ATL_DivBySize(2*CacheSize) ATL_PTCACHEMUL;
   k = i = (i + N*N-1) / (N*N);
   if (nreps > i) k = i = nreps;
   a = A = malloc(i * ATL_MulBySize(incA));
   if (A)
   {
      if (Uplo == TestGE)
         for (i=0; i < k; i++)
            Mjoin(PATL,gegen)(N, N, A+i*incA, lda, N+lda);
      else
         for (i=0; i < k; i++)
            hegen(Order, (enum CBLAS_UPLO)Uplo, N, A+i*incA, lda);

      t0 = time00();
      for (i=nreps; i; i--, a += incA) test_inv(Order, Uplo, N, a, lda);
      t1 = time00() - t0;
      free(A);
   }
   else fprintf(stderr, "   WARNING: not enough mem to run timings!\n");
   return(t1/nreps);
}

static int RunCase
   (enum CBLAS_ORDER Order, enum TEST_UPLO Uplo, int N, int lda,
    int CacheSize, int MFLOP, double thresh)
{
   double mflops, mflop, t0, mfb=0.0;
   TYPE resid;
   int nreps, iret=(-1);
   char *cuplo, *cord;

   if (Order == CblasRowMajor) cord = "Row";
   else cord = "Col";
   if (Uplo == TestGE)
   {
      cuplo = "   GE";
      #ifdef TREAL
         mflops = ((2.0*N)*N*N)/3.0 - (0.5*N)*N;    /* LU flops */
         mflops += (1.0*N)*N*N;                     /* getri flops */
      #else
         mflops = ((8.0*N)*N*N)/3.0 - (1.0*N)*N;    /* LU flops */
         mflops += (4.0*N)*N*N;                     /* getri flops */
      #endif
   }
   else
   {
      if (Uplo == TestUpper) cuplo = "Upper";
      else cuplo = "Lower";
      mflops = N;
   #ifdef TREAL
      mflops = (mflops*mflops*mflops) / 3.0 + 0.5*(mflops*mflops); /* LLt */
      mflops += (0.25*N)*N*N;                     /* lauum flops */
   #else
      mflops = (4.0/3.0)*(mflops*mflops*mflops) + 3.0 * (mflops*mflops);
      mflops += (1.0*N)*N*N;                     /* lauum flops */
   #endif
   }
   #ifdef TREAL
      mflops += ((1.0*N)*N*N)/3.0;               /* trtri flops */
   #else
      mflops += ((4.0*N)*N*N)/3.0 - (2.0*N)*N;   /* trtri flops */
   #endif
   mflops /= 1000000.0;
   if (mflops < 1.0) nreps = 1;
   else nreps = mflops;
/*
 * The line below replaces this line:
   nreps = MFLOP / nreps;
 * they are the same, since we ensure nreps >= 1 in above statement, but
 * on the SGI, the original causes an error, must be a compiler error of
 * some sort -- RCW.
 */
   nreps = MFLOP / Mmax(1,nreps);
   if (thresh >= ATL_rzero) /* perform residual check */
   {
      t0 = RunTest(Order, Uplo, N, lda, CacheSize, &resid);
      if (resid <= thresh) iret=1;
      else iret=0;
   }
   else resid = -1.0;
   if (nreps > 0 || thresh < 0.0)
      t0 = RunTiming(Order, Uplo, N, lda, CacheSize, nreps);
   if (t0 > 0.0) mflop = mflops / t0;
   else mflop = -1.0;
   fprintf(stdout, "%5d    %3s %6s %6d %6d %9.3f %9.2f  %e\n",
           nreps, cord, cuplo, N, lda, t0, mflop, resid);
   return(iret);
}

void RunCases(int CacheSize, TYPE thresh, int MFLOP, int ldagap, int norder,
              enum ATLAS_ORDER *Orders, int nuplo, enum ATLAS_UPLO *Uplos,
              int N0, int NN, int incN)
{
   int i, lda, n, io, iu, ns=0, np=0, nc=0;

   fprintf(stdout,
"\nNREPS  ORDER   UPLO      N    LDA      TIME     MFLOP         RESID\n");
   fprintf(stdout,
  "=====  =====  =====  =====  =====  ========  ========  ============\n\n");

   for (n=N0; n <= NN; n += incN)
   {
      if (ldagap >= 0) lda = ldagap+n;
      else lda = NN;
      for (io=0; io < norder; io++)
      {
         for (iu=0; iu < nuplo; iu++)
         {
            i = RunCase(Orders[io], Uplos[iu], n, lda, CacheSize, MFLOP,
                        thresh);
            if (i > 0) np++;
            else if (i < 0) ns++;
            nc++;
         }
      }
   }
   if (thresh >= ATL_rzero)
      fprintf(stdout, "\n%d cases: %d passed, %d skipped, %d failed\n",
              nc, np, ns, nc-np-ns);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "\nUSAGE: %s -n <n> -N <N0 NN incN> -T <thresh> -F <mflop> -l <ldagap> -C <cache size> -U <nuplos> <uplo1> ... <uploN> -O <norder> ...\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *MFLOP, int *CacheSize, TYPE *thresh,
              int *ldagap, int *nord, enum ATLAS_ORDER **Ord,
              int *nuplo, enum TEST_UPLO **Uplo, int *N0, int *NN, int *incN)
{
   int i, j, n;
   char ch;

   *MFLOP = 0;
   #ifdef L2SIZE
      *CacheSize = L2SIZE;
   #else
      *CacheSize = 4*1024*1024;
   #endif
   *thresh = 100.0;
   *N0 = *NN = *incN = -1;
   *ldagap = 0;
   *nord = *nuplo = -1;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'T':
         *thresh = atof(args[++i]);
         break;
      case 'C':
         *CacheSize = 1024*atoi(args[++i]);
         break;
      case 'l':
         *ldagap = atoi(args[++i]);
         break;
      case 'n':
         *N0 = *NN = *incN = atoi(args[++i]);
         break;
      case 'N':
         *N0 = atoi(args[++i]);
         *NN = atoi(args[++i]);
         *incN = atoi(args[++i]);
         break;
      case 'F':
         *MFLOP = atoi(args[++i]);
         break;
      case 'O':
         *nord  = atoi(args[++i]);
         if (*nord  <= 0) PrintUsage(args[0]);
         *Ord  = malloc(*nord  * sizeof(enum ATLAS_ORDER));
         ATL_assert(*Ord);
         for (j=0; j != *nord; j++)
         {
            if (args[i] == NULL) PrintUsage(args[0]);
            ch = *args[++i];
            if (ch == 'c' || ch == 'C') (*Ord)[j] = AtlasColMajor;
            else if (ch == 'r' || ch == 'R') (*Ord)[j] = AtlasRowMajor;
            else PrintUsage(args[0]);
         }
         break;
      case 'U':
         *nuplo = atoi(args[++i]);
         if (*nuplo <= 0) PrintUsage(args[0]);
         *Uplo = malloc(*nuplo * sizeof(enum TEST_UPLO));
         ATL_assert(*Uplo);
         for (j=0; j != *nuplo; j++)
         {
            if (args[i] == NULL) PrintUsage(args[0]);
            ch = *args[++i];
            if (ch == 'u' || ch == 'U') (*Uplo)[j] = AtlasUpper;
            else if (ch == 'l' || ch == 'L') (*Uplo)[j] = AtlasLower;
            else (*Uplo)[j] = TestGE;
         }
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   if (*N0 == -1)
   {
      *N0 = 100;
      *NN = 1000;
      *incN = 100;
   }
   if (*nord  == -1)
   {
      *nord  = 1;
      *Ord  = malloc(sizeof(enum ATLAS_ORDER));
      ATL_assert(*Ord);
      **Ord = AtlasColMajor;
   }
   if (*nuplo == -1)
   {
      *nuplo = 1;
      *Uplo = malloc(sizeof(enum TEST_UPLO));
      ATL_assert(*Uplo);
      **Uplo = TestGE;
   }
}

main(int nargs, char **args)
{
   int MFLOP, CacheSize, ldagap, nord, nuplo, N0, NN, incN;
   enum TEST_UPLO *Uplo;
   enum ATLAS_ORDER *Ord;
   TYPE thresh;
   GetFlags(nargs, args, &MFLOP, &CacheSize, &thresh, &ldagap, &nord, &Ord,
            &nuplo, &Uplo, &N0, &NN, &incN);
   RunCases(CacheSize, thresh, MFLOP, ldagap, nord, Ord, nuplo,
            (enum ATLAS_UPLO*) Uplo, N0, NN, incN);
   exit(0);
}
