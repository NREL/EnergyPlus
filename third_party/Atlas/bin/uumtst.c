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
#include "atlas_level1.h"
#ifdef GCCWIN
   ___main(){} __main(){} MAIN__(){} _MAIN_(){}
#endif


double time00();

#ifdef TimeF77
static void test_lauum(const enum ATLAS_ORDER Order, const enum ATLAS_UPLO Uplo,
                       const int N, TYPE *A, const int lda)
{
   if (Order == AtlasRowMajor) Mjoin(PATL,tstsqtran)(N, A, lda);
   ATL_assert(Mjoin(PATL,f77lauum)(Uplo, N, A, lda) == 0)
   if (Order == AtlasRowMajor) Mjoin(PATL,tstsqtran)(N, A, lda);
}
#elif defined(TimeC)
   #include "clapack.h"
   #define Clauum Mjoin(Mjoin(clapack_,PRE),lauum)
   #define test_lauum(Maj_, Uplo_, N_, A_, lda_) \
      ATL_assert(Clauum(Maj_, Uplo_, N_, A_, lda_) == 0)
#else
   #define test_lauum(Maj_, Uplo_, N_, A_, lda_) \
      ATL_lauum(Maj_, Uplo_, N_, A_, lda_)
#endif

static void ATL_L2GE(const enum CBLAS_ORDER, const int, const TYPE*,
                     const int, TYPE*, const int);

static void ATL_U2GE
   (const enum CBLAS_ORDER Order, const int N, const TYPE *U, const int ldu,
    TYPE *C, const int ldc)
{
   int j;

   if (Order == CblasRowMajor) ATL_L2GE(CblasColMajor, N, U, ldu, C, ldc);
   else
   {
      for (j=0; j < N; j++)
      {
         cblas_copy(j+1, U+j*(ldu SHIFT), 1, C+j*(N SHIFT), 1);
         if (j != N-1) Mjoin(PATL,zero)(N-j-1, C+((1+j*(N+1))SHIFT), 1);
      }
   }
}

static void ATL_L2GE
   (const enum CBLAS_ORDER Order, const int N, const TYPE *L, const int ldl,
    TYPE *C, const int ldc)
{
   int j;
   if (Order == CblasRowMajor) ATL_U2GE(CblasColMajor, N, L, ldl, C, ldc);
   else
   {
      for (j=0; j < N; j++)
      {
         if (j > 1) Mjoin(PATL,zero)(j-1, C+j*(N SHIFT), 1);
         cblas_copy(N-j, L+j*((ldl+1)SHIFT), 1, C+j*((N+1)SHIFT), 1);
      }
   }
}

static TYPE *ATL_LtmulL
   (const enum CBLAS_ORDER Order, const int N, const TYPE *L, const int ldl)
{
   TYPE *C;
   #ifdef TREAL
      const TYPE one=ATL_rone, zero=ATL_rzero;
   #else
      const TYPE one[2] = {ATL_rone,ATL_rzero}, zero[2] = {ATL_rzero,ATL_rzero};
   #endif
   C = malloc(N*ATL_MulBySize(N));
   ATL_assert(C);
   ATL_L2GE(Order, N, L, ldl, C, N);
   cblas_trmm(Order, CblasLeft, CblasLower, CblasConjTrans, CblasNonUnit,
              N, N, one, L, ldl, C, N);
   return(C);
}

static TYPE *ATL_UmulUt
   (const enum CBLAS_ORDER Order, const int N, const TYPE *U, const int ldu)
{
   TYPE *C;
   #ifdef TREAL
      const TYPE one=ATL_rone, zero=ATL_rzero;
   #else
      const TYPE one[2] = {ATL_rone,ATL_rzero}, zero[2] = {ATL_rzero,ATL_rzero};
   #endif
   C = malloc(N*ATL_MulBySize(N));
   ATL_assert(C);
   ATL_U2GE(Order, N, U, ldu, C, N);
   cblas_trmm(Order, CblasRight, CblasUpper, CblasConjTrans, CblasNonUnit,
              N, N, one, U, ldu, C, N);
   return(C);
}
#if 0
#define PADVAL ATL_typify(-973200000.0)
#else
#define PADVAL ATL_rzero
#endif

static void ATL_checkpad(enum ATLAS_UPLO Uplo, int N, TYPE *A, int lda)
{
   const int lda2 = lda SHIFT, N2 = N SHIFT;
   int i, j, k;
   TYPE *a;

   a = A;
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
      {
         for (i=(j+1) SHIFT; i < N2; i++)
         {
            if (a[i] != PADVAL)
            #ifdef TREAL
               fprintf(stderr, "   OVERWRITE at A(%d,%d) of %f!!!\n",
                       i+1, j+1, a[i]);
            #else /* possibly generates 2 warning per element */
               if (i % 2)
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i-1], a[i]);
               else
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i], a[i+1]);
            #endif
         }
         a += lda2;
      }
   }
   else
   {
      for (j=0; j < N; j++)
      {
         k = j SHIFT;
         for (i=0; i < k; i++)
         {
            if (a[i] != PADVAL)
            #ifdef TREAL
               fprintf(stderr, "   OVERWRITE at A(%d,%d) of %f!!!\n",
                       i, j, a[i]);
            #else /* possibly generates 2 warning per element */
               if (i % 2)
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i-1], a[i]);
               else
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i], a[i+1]);
            #endif
         }
         a += lda2;
      }
   }
}

#ifdef TREAL
static TYPE lltnrm1(enum ATLAS_UPLO Uplo, int N, TYPE *A, int lda)
{
   int i, j;
   TYPE tmp, nrm=0.0;

   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
      {
         tmp = Mjoin(PATL,asum)(j, A+j*lda, 1);
         tmp += Mjoin(PATL,asum)(N-j, A+j*lda+j, lda);
         if (tmp > nrm) nrm = tmp;
      }
   }
   else
   {
      for (i=0; i < N; i++)
      {
         tmp = Mjoin(PATL,asum)(i, A+i, lda);
         tmp += Mjoin(PATL,asum)(N-i, A+i*lda+i, 1);
         if (tmp > nrm) nrm = tmp;
      }
   }
   return(nrm);
}
#else
static TYPE lltnrm1(enum ATLAS_UPLO Uplo, int N, TYPE *A, int lda)
{
   const int lda2 = lda<<1;
   int i, j;
   TYPE tmp, nrm=0.0;

   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
      {
         tmp = A[(j*(lda+1))<<1];
         if (tmp < ATL_rzero) tmp = -tmp;
         tmp += Mjoin(PATL,asum)(j, A+j*lda2, 1);
         tmp += Mjoin(PATL,asum)(N-j-1, A+(((j+1)*lda+j)<<1), lda);
         if (tmp > nrm) nrm = tmp;
      }
   }
   else
   {
      for (i=0; i < N; i++)
      {
         tmp = A[(i*(lda+1))<<1];
         if (tmp < ATL_rzero) tmp = -tmp;
         tmp += Mjoin(PATL,asum)(i, A+(i<<1), lda);
         tmp += Mjoin(PATL,asum)(N-i-1, A+((i*lda+i+1)<<1), 1);
         if (tmp > nrm) nrm = tmp;
      }
   }
   return(nrm);
}
#endif

static void lltgen(enum ATLAS_UPLO Uplo, int N, TYPE *A, int lda, int seed)
{
   const int lda2 = lda SHIFT;
   int j;
   TYPE t0, t1;
   #ifdef TREAL
      const TYPE padval = PADVAL;
   #else
      const TYPE padval[2] = {PADVAL, PADVAL};
   #endif

   Mjoin(PATL,gegen)(N, N, A, lda, seed);
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
         Mjoin(PATL,set)(N-j-1, padval, A+((j*lda+j+1)SHIFT), 1);
   }
   else
   {
      for (j=0; j < N; j++) Mjoin(PATL,set)(j, padval, A+j*lda2, 1);
   }
   #ifdef TCPLX   /* imag part of diag should be assumed to be zero */
       Mjoin(Mjoin(ATL_,UPR),set)(N, *padval, A+1, (lda+1)<<1);
   #endif
/*
 * Make diagonally dominant, and positive diagonal
 */
   t1 = lltnrm1(Uplo, N, A, lda);
   for (j=0; j < N; j++)
   {
      t0 = A[(j+j*lda)SHIFT]*5.3;
      if (t0 < ATL_rzero) t0 = -t0;
      A[(j+j*lda)SHIFT] = t0 + t1;
   }
}

void lltdiff(const enum ATLAS_UPLO Uplo, const int N,
             const TYPE *A, const int lda, TYPE *C, const int ldc)
/*
 * C <- C - A, A & C symmetric (real) or hermition (complex)
 */
{
   int j;
#ifdef TREAL
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
         Mjoin(PATL,axpy)(j+1, ATL_rnone, A+j*lda, 1, C+j*ldc, 1);
   }
   else
   {
      for (j=0; j < N; j++)
         Mjoin(PATL,axpy)(N-j, ATL_rnone, A+j*(lda+1), 1, C+j*(ldc+1), 1);
   }
#else
   const int lda2 = lda<<1, ldc2 = ldc<<1;
   const TYPE none[2] = {ATL_rnone, ATL_rzero};

   Mjoin(Mjoin(ATL_,UPR),axpy)(N, ATL_rnone, A, (lda+1)<<1, C, (ldc+1)<<1);
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
         Mjoin(PATL,axpy)(j, none, A+j*lda2, 1, C+j*ldc2, 1);
   }
   else
   {
      for (j=0; j < N; j++)
         Mjoin(PATL,axpy)(N-j-1, none, A+((j*(lda+1)+1)<<1), 1,
                          C+((j*(ldc+1)+1)<<1), 1);
   }
#endif
}

static TYPE uumtest(enum ATLAS_ORDER Order, enum ATLAS_UPLO Uplo,
                    int CacheSize, int N, int lda, double *tim)
{
   TYPE *A, *Ag, *LmLt;
   double t0, t1;
   TYPE normA, eps, resid;
   enum ATLAS_UPLO MyUplo = Uplo;

   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasUpper) MyUplo = CblasLower;
      else MyUplo = CblasUpper;
   }
   eps = Mjoin(PATL,epsilon)();
   A = malloc(ATL_MulBySize(lda)*N + ATL_MulBySize(N)*N);
   if (A == NULL) return(-1);
   Ag = A + lda*(N SHIFT);
   t0 = ATL_flushcache(CacheSize);
   lltgen(MyUplo, N, A, lda, N*1029+lda);
   lltgen(MyUplo, N, Ag, N, N*1029+lda);
   normA = lltnrm1(MyUplo, N, A, lda);
   #ifdef DEBUG
      Mjoin(PATL,geprint)("A", N, N, A, lda);
      Mjoin(PATL,geprint)("Ag", N, N, Ag, N);
   #endif

   t0 = ATL_flushcache(-1);

   t0 = time00();
   test_lauum(Order, Uplo, N, A, lda);
   t1 = time00() - t0;
   *tim = t1;

   t0 = ATL_flushcache(0);

   ATL_checkpad(MyUplo, N, A, lda);
   if (Uplo == CblasUpper) LmLt = ATL_UmulUt(Order, N, Ag, N);
   else LmLt = ATL_LtmulL(Order, N, Ag, N);
   #ifdef DEBUG
      Mjoin(PATL,geprint)("A", N, N, A, lda);
      Mjoin(PATL,geprint)("Ag", N, N, LmLt, N);
   #endif
   lltdiff(MyUplo, N, A, lda, LmLt, N);
   #ifdef DEBUG
      Mjoin(PATL,geprint)("A-L*Lt", N, N, LmLt, N);
   #endif
   resid = lltnrm1(MyUplo, N, LmLt, N) / (normA * eps * N);
   if (resid > 10.0 || resid != resid)
      fprintf(stderr, "normA=%e, eps=%e, num=%e\n", normA, eps, resid);

   free(LmLt);
   free(A);

   return(resid);
}

int RunCase(int CacheSize, TYPE thresh, int MFLOP, enum ATLAS_ORDER Order,
            enum ATLAS_UPLO Uplo, int N, int lda)
{
   char *Ups, *Ord;
   TYPE resid = 0.0;
   double mflop, mflops, t0, tim=0.0;
   int nreps=1, passed, i, imem;
   const int incA = lda*N;
   TYPE *a, *A;

   mflops = N;
   mflops = (mflops*mflops*mflops) / 4.0;
   #ifdef TCPLX
      mflops *= 4.0;
   #endif
   mflops /= 1000000.0;

   if (thresh > ATL_rzero) resid =
      uumtest(Order, Uplo, CacheSize, N, lda, &tim);
   else resid = -1.0;

   if (MFLOP > mflops || thresh <= ATL_rzero) /* need to time repetitively */
   {
      nreps = (mflops * 1000000);
      nreps = (MFLOP*1000000 + nreps-1) / nreps;
      if (nreps < 1) nreps = 1;
      imem = ATL_DivBySize(CacheSize) ATL_PTCACHEMUL;
      imem = (imem + 2*N*N-1) / (N*N);
      if (imem < nreps) imem = nreps;
      a = A = malloc(imem * ATL_MulBySize(incA));
      if (A != NULL)
      {
         for (i=0; i < imem; i++) lltgen(Uplo, N, A+i*incA, lda, N*1029+lda);
         t0 = time00();
         for (i=nreps; i; i--, a += incA)
            test_lauum(Order, Uplo, N, a, lda);
         tim = time00() - t0;
         tim /= nreps;
         free(A);
      }
      else fprintf(stderr, "   WARNING: not enough mem to run timings!\n");
   }

   if (tim > 0.0) mflop = mflops / tim;
   else mflop = 0.0;
   if (Uplo == AtlasUpper) Ups = "Upper";
   else Ups = "Lower";
   if (Order == CblasColMajor) Ord = "Col";
   else Ord = "Row";
   fprintf(stdout, "%5d  %3s  %5s %6d %6d  %12.5f  %12.3f  %12e\n",
           nreps, Ord, Ups, N, lda, tim, mflop, resid);
   if (resid > thresh || resid != resid) passed = 0;
   else if (resid < 0.0) passed = -1;
   else passed = 1;
   return(passed);
}

void RunCases(const int CacheSize, const TYPE thresh, const int MFLOP,
              const int ldagap, const int norder, const enum ATLAS_UPLO *Ords,
              const int nuplo, const enum ATLAS_UPLO *Uplos,
              const int N0, const int NN, const int incN)
{
   int i, lda, n, iup, io, np=0, nc=0, ns=0;

   fprintf(stdout,
"NREPS  ORD   UPLO      N    lda          TIME        MFLOPS         RESID\n");
   fprintf(stdout,
"=====  ===  =====  =====  =====  ============  ============  ============\n");
   for (n=N0; n <= NN; n += incN)
   {
      if (ldagap >= 0) lda = ldagap + n;
      else lda = NN;
      for (io=0; io < norder; io++)
      {
         for (iup=0; iup < nuplo; iup++)
         {
            i = RunCase(CacheSize, thresh, MFLOP, Ords[io], Uplos[iup], n, lda);
            if (i > 0) np++;
            else if (i < 0) ns++;
            nc++;
         }
      }
   }
   if (thresh > ATL_rzero)
      fprintf(stdout, "\n%d cases: %d passed, %d skipped, %d failed\n",
              nc, np, ns, nc-np-ns);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -n <n> -N <N0 NN incN> -T <thresh> -F <mflop> -l <lagap> -C <cache size> -U <nuplos> <uplo1> ... <uploN>\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *MFLOP, int *CacheSize, TYPE *thresh,
              int *ldagap, int *norders, enum ATLAS_UPLO **Order,
              int *nuplo, enum ATLAS_UPLO **Uplo,
              int *N0, int *NN, int *incN)
{
   int i, j, n;
   char ch;

   *norders = -1;
   *MFLOP = 0;
   #ifdef L2SIZE
      *CacheSize = L2SIZE;
   #else
      *CacheSize = 4*1024*1024;
   #endif
   *thresh = 100.0;
   *N0 = *NN = *incN = -1;
   *ldagap = 0;
   *nuplo = -1;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'O':
         *norders = n = atoi(args[++i]);
         if (n < 1) PrintUsage(args[0]);
         *Order = malloc(n * sizeof(enum ATLAS_ORDER));
         ATL_assert(*Order);
         for (j=0; j != n; j++)
         {
            ch = *args[++i];
            if (ch == 'R' || ch == 'r') (*Order)[j] = AtlasRowMajor;
            else if (ch == 'C' || ch == 'c') (*Order)[j] = AtlasColMajor;
            else PrintUsage(args[0]);
         }
         break;
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
      case 'U':
         *nuplo = atoi(args[++i]);
         if (*nuplo <= 0) PrintUsage(args[0]);
         *Uplo = malloc(*nuplo * sizeof(enum ATLAS_UPLO));
         ATL_assert(*Uplo);
         for (j=0; j != *nuplo; j++)
         {
            if (args[i] == NULL) PrintUsage(args[0]);
            ch = *args[++i];
            if (ch == 'u' || ch == 'U') (*Uplo)[j] = AtlasUpper;
            else if (ch == 'l' || ch == 'L') (*Uplo)[j] = AtlasLower;
            else PrintUsage(args[0]);
         }
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   if (*norders == -1)
   {
      *norders = 1;
      *Order = malloc(sizeof(enum ATLAS_ORDER));
      **Order = AtlasColMajor;
   }
   if (*N0 == -1)
   {
      *N0 = 100;
      *NN = 1000;
      *incN = 100;
   }
   if (*nuplo == -1)
   {
      *nuplo = 1;
      *Uplo = malloc(sizeof(enum ATLAS_UPLO));
      ATL_assert(*Uplo);
      **Uplo = AtlasLower;
   }
}

main(int nargs, char **args)
{
   int MFLOP, CacheSize, ldagap, nuplo, N0, NN, incN;
   TYPE thresh;
   enum ATLAS_UPLO *Uplos;
   enum ATLAS_UPLO *Orders;
   int norders;
   GetFlags(nargs, args, &MFLOP, &CacheSize, &thresh, &ldagap,
            &norders, &Orders, &nuplo, &Uplos, &N0, &NN, &incN);
   RunCases(CacheSize, thresh, MFLOP, ldagap, norders, Orders, nuplo, Uplos,
            N0, NN, incN);
   exit(0);
}
