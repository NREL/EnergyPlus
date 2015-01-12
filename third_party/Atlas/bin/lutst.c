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
#include "atlas_lapack.h"
#include "cblas.h"
#include "atlas_cblastypealias.h"
#include "atlas_tst.h"
#ifdef GCCWIN
   ___main(){} __main(){} MAIN__(){} _MAIN_(){}
#endif

double time00();
#ifdef TimeF77
   #define test_getrf(Major_, M_, N_, A_, lda_, ipiv_) \
      ATL_assert(Mjoin(PATL,f77getrf)(Major_, M_, N_, A_, lda_, ipiv_) == 0)
#elif defined(TimeC)
   #include "clapack.h"
   #define Cgetrf Mjoin(Mjoin(clapack_,PRE),getrf)
   #define test_getrf(Major_, M_, N_, A_, lda_, ipiv_) \
      ATL_assert(Cgetrf(Major_, M_, N_, A_, lda_, ipiv_) == 0)
#else
   #define test_getrf(Major_, M_, N_, A_, lda_, ipiv_) \
      ATL_assert(ATL_getrf(Major_, M_, N_, A_, lda_, ipiv_) == 0)
#endif

static TYPE *ATL_LmulUR(const int M, const int N, const TYPE *LU, const int ldl)
{
   const int lda = ldl SHIFT, ldc = N SHIFT, MN = Mmin(M,N);
   int i, j, m;
   TYPE *C, *c;
   #ifdef TREAL
      const TYPE ONE=ATL_rone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #endif

   C = c = malloc(M*ATL_MulBySize(N));
   ATL_assert(c);
   if (M >= N)
   {
      for (i=0; i != N; i++, LU += lda, C += ldc)
      {
         Mjoin(PATL,copy)(i+1, LU, 1, C, 1);
         Mjoin(PATL,zero)(N-i-1, C+((i+1)SHIFT), 1);
      }
      for(; i != M; i++, LU += lda, C += ldc) Mjoin(PATL,copy)(N, LU, 1, C, 1);
      LU -= lda * M;
      C -= ldc * M;
      cblas_trmm(CblasRowMajor, CblasRight, CblasUpper, CblasNoTrans, CblasUnit,
                 M, N, ONE, LU, ldl, C, N);
   }
   else /* N > M */
   {
      for (i=0; i != M; i++, C += ldc, LU += lda)
      {
         Mjoin(PATL,zero)(i, C, 1);
         C[i SHIFT] = ATL_rone;
         #ifdef TCPLX
            C[(i SHIFT)+1] = ATL_rzero;
         #endif
         Mjoin(PATL,copy)(N-i-1, LU+((i+1)SHIFT), 1, C+((i+1)SHIFT), 1);
      }
      LU -= lda * M;
      C -= ldc * M;
      cblas_trmm(CblasRowMajor, CblasLeft, CblasLower, CblasNoTrans,
                 CblasNonUnit, M, N, ONE, LU, ldl, C, N);
   }
   return(C);
}

static TYPE *ATL_LmulUC(const int M, const int N, const TYPE *LU, const int ldl)
{
   const int lda = ldl SHIFT, MN = Mmin(M,N);
   int i, j, m;
   TYPE *C, *c;
   #ifdef TREAL
      const TYPE ONE=ATL_rone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #endif

   C = c = malloc(M*ATL_MulBySize(N));
   ATL_assert(c);
   if (M >= N)
   {
      for (j=0; j < MN; j++)
      {
         m = j SHIFT;
         for (i=0; i < m; i++) c[i] = ATL_rzero;
         #ifdef TCPLX
            c[i++] = ATL_rone;
            c[i++] = ATL_rzero;
         #else
            c[i++] = ATL_rone;
         #endif
         for (m=M SHIFT; i < m; i++) c[i] = LU[i];
         c += m;
         LU += lda;
      }
      LU -= MN * lda;
      for (m=M SHIFT; j < N; j++, c += m) Mjoin(PATL,zero)(M, c, 1);
      cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                 CblasNonUnit, M, N, ONE, LU, ldl, C, M);
   }
   else /* M < N */
   {
      for (j=0; j < M; j++)
      {
         m = (j+1) SHIFT;
         for (i=0; i < m; i++) c[i] = LU[i];
         for (m=M SHIFT; i < m; i++) c[i] = ATL_rzero;
         c += m;
         LU += lda;
      }
      Mjoin(PATL,gecopy)(M, N-M, LU, ldl, c, M);
      LU -= M * lda;
      cblas_trmm(CblasColMajor, CblasLeft, CblasLower, CblasNoTrans,
                 CblasUnit, M, N, ONE, LU, ldl, C, M);
   }
   return(C);
}

int findnpvt(const int N, const int *ipiv)
{
   int i, n=0;
   for (i=0; i != N; i++) if (ipiv[i] != i) n++;
   return(n);
}

static TYPE lutestR(int CacheSize, int M, int N, int lda, int *npiv,
                    double *tim)
{
   TYPE *A, *LmU;
   int *ipiv;
   const int MN = Mmin(M,N);
   int i;
   double t0, t1;
   TYPE normA, eps, resid;

   eps = Mjoin(PATL,epsilon)();
   A = malloc(ATL_MulBySize(lda)*M);
   if (A == NULL) return(-1);
   ipiv = malloc( MN * sizeof(int) );
   if (ipiv == NULL)
   {
      free(A);
      return(-1);
   }
   t0 = ATL_flushcache(CacheSize);

   Mjoin(PATL,gegen)(N, M, A, lda, M*N+lda);
   #ifdef DEBUG
      Mjoin(PATL,geprint)("A0", N, M, A, lda);
   #endif
   normA = Mjoin(PATL,genrm1)(N, M, A, lda); /* actually infnrm, but OK */

   t0 = ATL_flushcache(-1);

   t0 = time00();
   test_getrf(CblasRowMajor, M, N, A, lda, ipiv);
   t1 = time00() - t0;
   *tim = t1;

   t0 = ATL_flushcache(0);

   #ifdef DEBUG
      Mjoin(PATL,geprint)("LU", N, M, A, lda);
   #endif
   LmU = ATL_LmulUR(M, N, A, lda);  /* LmU contains L * U */
   #ifdef DEBUG
      Mjoin(PATL,geprint)("L*U", N, M, LmU, N);
   #endif
   Mjoin(PATL,gegen)(N, M, A, lda, M*N+lda);  /* regenerate A, overwriting LU */
   ATL_laswp(M, A, lda, 0, MN, ipiv, 1);  /* apply swaps to A */
   resid = Mjoin(PATL,gediffnrm1)(N, M, A, lda, LmU, N);
   resid /= (normA * eps * Mmin(M,N));
   *npiv = findnpvt(MN, ipiv);

   free(LmU);
   free(A);
   free(ipiv);

   return(resid);
}

static TYPE lutestC(int CacheSize, int M, int N, int lda, int *npiv,
                    double *tim)
{
   TYPE *A, *LmU;
   int *ipiv;
   const int MN = Mmin(M,N);
   int i;
   double t0, t1;
   TYPE normA, eps, resid;

   eps = Mjoin(PATL,epsilon)();
   A = malloc(ATL_MulBySize(lda)*N);
   if (A == NULL) return(-1);
   ipiv = malloc( MN * sizeof(int) );
   if (ipiv == NULL)
   {
      free(A);
      return(-1);
   }
   t0 = ATL_flushcache(CacheSize);

   Mjoin(PATL,gegen)(M, N, A, lda, M*N+lda);
   normA = Mjoin(PATL,genrm1)(M, N, A, lda);

   t0 = ATL_flushcache(-1);

   t0 = time00();
   test_getrf(CblasColMajor, M, N, A, lda, ipiv);
   t1 = time00() - t0;
   *tim = t1;

   t0 = ATL_flushcache(0);

   LmU = ATL_LmulUC(M, N, A, lda);  /* LmU contains L * U */
   Mjoin(PATL,gegen)(M, N, A, lda, M*N+lda);  /* regenerate A, overwriting LU */
   ATL_laswp(N, A, lda, 0, MN, ipiv, 1);  /* apply swaps to A */
   resid = Mjoin(PATL,gediffnrm1)(M, N, A, lda, LmU, M);
   resid /= (normA * eps * Mmin(M,N));
   *npiv = findnpvt(MN, ipiv);

   free(LmU);
   free(A);
   free(ipiv);

   return(resid);
}

int RunCase(int CacheSize, TYPE thresh, int MFLOP, enum ATLAS_ORDER Order,
            int M, int N, int lda)
{
   char *cord = (Order == AtlasColMajor ? "Col" : "Row");
   const double maxMN = Mmax(M,N), minMN = Mmin(M,N);
   unsigned long nreps=0;
   int npiv=(-1), *ipiv;
   const int incA = (Order == AtlasColMajor ? N*lda : M*lda);
   double mflops, mflop, resid, tim=(-1.0), t0;
   TYPE *A, *a;
   int i;

   #ifdef TREAL
      mflops = maxMN * minMN * minMN - ((minMN*minMN*minMN) / 3.0) -
               (minMN*minMN) / 2.0;
   #else
      mflops = (maxMN * minMN * minMN - ((minMN*minMN*minMN) / 3.0) +
                (maxMN*minMN) / 2.0)*4.0 - 3.0 * minMN*minMN;
   #endif
   mflops /= 1000000.0;

   if (thresh > ATL_rzero)
   {
      if (Order == AtlasColMajor)
         resid = lutestC(CacheSize, M, N, lda, &npiv, &tim);
      else resid = lutestR(CacheSize, M, N, lda, &npiv, &tim);
   }
   else resid = -1.0;
   if (MFLOP > mflops || thresh <= ATL_rzero) /* need to time repetitively */
   {
      nreps = (mflops*1000000);
      nreps = (MFLOP*1000000 + nreps-1) / nreps;
      if (nreps < 1) nreps = 1;
      i = ATL_DivBySize(2*CacheSize) ATL_PTCACHEMUL;
      i = (i + M*N) / (M*N);
      if (i < nreps) i = nreps;  /* don't reuse mem or no pivoting */
      a = A = malloc(i * ATL_MulBySize(incA));
      if (A != NULL)
      {
         ipiv = malloc(Mmin(M,N)*sizeof(int));  /* what the hell - reuse ipiv */
         if (ipiv)
         {
            Mjoin(PATL,gegen)(i*incA, 1, A, i*incA, incA+M+3012);
            t0 = time00();
            for (i=nreps; i; i--, a += incA)
               test_getrf(Order, M, N, a, lda, ipiv);
            tim = time00() - t0;
            tim /= nreps;
            if (npiv == 0) npiv = findnpvt(Mmin(M,N), ipiv);
            free(ipiv);
         }
         else fprintf(stderr, "   WARNING: not enough mem to run timings!\n");
         free(A);
      }
      else fprintf(stderr, "   WARNING: not enough mem to run timings!\n");
   }
   if (tim > 0.0) mflop = mflops / tim;
   else mflop = 0.0;
   fprintf(stdout, "%5d  %3s   %6d %6d %6d %6d %9.3f %9.3f %9.3e\n",
           nreps, cord, M, N, lda, npiv, tim, mflop, resid);
   return(resid <= thresh);
}

void RunCases(int CacheSize, TYPE thresh, int MFLOP, int norder,
              enum ATLAS_ORDER *Orders, int LdaIsM, int MisN, int NisM,
              int M0, int MN, int incM, int N0, int NN, int incN)
{
   int i, j, m, n, io, lda, np=0, nc=0;

   fprintf(stdout,
"NREPS  Major      M      N    lda  NPVTS      TIME     MFLOP     RESID\n");
   fprintf(stdout,
"=====  =====  =====  =====  =====  =====  ========  ========  ========\n");


   for (j = N0; j <= NN; j += incN)
   {
      n = j;
      for (i = M0; i <= MN; i += incM)
      {
         for (io=0; io < norder; io++)
         {
            lda = -1;
            if (MisN)
            {
               m = n;
               if (!LdaIsM) lda = NN;
            }
            else if (NisM)
            {
               m = n = i;
               if (!LdaIsM) lda = MN;
            }
            else
            {
               m = i;
               n = j;
               if (!LdaIsM)
               {
                  if (Orders[io] == AtlasRowMajor) lda = NN;
                  else lda = MN;
               }
            }
            if (lda == -1)
            {
               if (Orders[io] == AtlasRowMajor) lda = n;
               else lda = m;
            }
            if (RunCase(CacheSize, thresh, MFLOP, Orders[io], m, n, lda)) np++;
            nc++;
         }
      }
   }
   if (thresh > ATL_rzero)
   {
      if (nc == np)
         fprintf(stdout, "\nALL %d CASES PASSED\n", nc);
      else
         fprintf(stdout, "\n%d cases ran, %d cases passed, %d failed\n\n",
                 nc, np, nc-np);
   }
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -m <m> -n <n> -M <M0 MN incM> -N <N0 NN incN> -T <thresh> -F <mflop> -l <LdaIsM> -C <cache size> -O <norder> <o1> ... <oN>\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *MFLOP, int *CacheSize, TYPE *thresh,
              int *norders, enum ATLAS_ORDER **Order, int *LdaIsM, int *MisN,
              int *NisM, int *M0, int *MN, int *incM,
              int *N0, int *NN, int *incN)
{
   char ch;
   int i, j, n;

   *MFLOP = 0;
   #ifdef L2SIZE
      *CacheSize = L2SIZE;
   #else
      *CacheSize = 4*1024*1024;
   #endif
   *thresh = 100.0;
   *LdaIsM = 1;
   *MisN = *NisM = 0;
   *M0 = *MN = *incM = -1;
   *N0 = *NN = *incN = -1;
   *norders = -1;
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
         *LdaIsM = atoi(args[++i]);
         break;
      case 'm':
         *M0 = *MN = *incM = atoi(args[++i]);
         break;
      case 'n':
         *N0 = *NN = *incN = atoi(args[++i]);
         break;
      case 'M':
         *M0 = atoi(args[++i]);
         *MN = atoi(args[++i]);
         *incM = atoi(args[++i]);
         break;
      case 'N':
         *N0 = atoi(args[++i]);
         *NN = atoi(args[++i]);
         *incN = atoi(args[++i]);
         break;
      case 'F':
         *MFLOP = atoi(args[++i]);
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
      if (*M0 > 0) *N0 = *NN = *incN = *NisM = 1;
      else
      {
         *N0 = 100;
         *NN = 1000;
         *incN = 100;
      }
   }
   if (*M0 == -1) *M0 = *MN = *incM = *MisN = 1;
}

main(int nargs, char **args)
{
   int MFLOP, CacheSize, LdaIsM, MisN, NisM, M0, MN, incM, N0, NN, incN;
   int norders;
   enum ATLAS_ORDER *Orders;
   TYPE thresh;

   GetFlags(nargs, args, &MFLOP, &CacheSize, &thresh, &norders, &Orders,
            &LdaIsM, &MisN, &NisM, &M0, &MN, &incM, &N0, &NN, &incN);
   RunCases(CacheSize, thresh, MFLOP, norders, Orders, LdaIsM, MisN, NisM,
            M0, MN, incM, N0, NN, incN);
   free(Orders);
   exit(0);
}
