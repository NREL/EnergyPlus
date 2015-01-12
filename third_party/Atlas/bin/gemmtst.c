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
#if defined(ATL_AFFINITY) || defined(ATL_NOAFFINITY)
   #ifndef ATL_USEPTHREADS
      #define ATL_USEPTHREADS
   #endif
#endif

/* #define TEST_GPMM */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include "atlas_tst.h"
#include "cblas.h"
#ifdef ATL_USEPTHREADS
   #include "atlas_tlvl3.h"
#endif
#if defined(ATL_RKXOVER) || defined(TEST_RANKK)
   #define TRUST_BIGNORK
   void Mjoin(PATL,bignork_mm)
      (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
       const int M, const int N, const int K, const SCALAR alpha,
       const TYPE *A, const int lda, const TYPE *B, const int ldb,
       const SCALAR beta, TYPE *C, const int ldc);
   #ifdef TCPLX
      #error "ATLAS's rank-K update does not yet support complex types!"
   #endif
#endif
#if defined(ATL_RKXOVER) && !defined(TEST_RANKK)
   #define TEST_RANKK
#endif
#ifdef TEST_GPMM
   #include "atlas_pkblas.h"
#endif
#ifdef TEST_RANKK
   #include Mstr(Mjoin(Mjoin(atlas_,PRE),vkbmms.h))
int Mjoin(PATL,vrankK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                       ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, const TYPE *B,
                       ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
   #ifdef ATL_RKXOVER
      FILE *fprk=NULL;
      int speedup_rk[ATL_vMAX_KB+1], speedup_rk_b0[ATL_vMAX_KB+1];
      int MAX_vKB = ATL_vMAX_KB;
   #endif
#elif defined(TEST_GEMM_RK)
int Mjoin(PATL,gemm_rK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                        ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
                        const TYPE *A, ATL_CINT lda, const TYPE *B,
                        ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
#endif

#ifndef L2SIZE
   #define L2SIZE 4194304
#endif

#ifdef ATL_DeclareSlens
F77_INTEGER ATL_Slen1, ATL_Slen2;
#endif
double time00();

void printmat(char *mat, int M, int N, TYPE *A, int lda)
{
   int i, j;

#ifdef TCPLX
   lda *= 2;
#endif
   printf("\n%s = \n",mat);
   for (i=0; i != M; i++)
   {
#ifdef TREAL
      for (j=0; j != N; j++) printf("%f  ",A[i+j*lda]);
#else
      for (j=0; j != N; j++) printf("(%f,%f)  ",A[2*i+j*lda], A[1+2*i+j*lda]);
#endif
      printf("\n");
   }
}

void matgen(int M, int N, TYPE *A, int lda, int seed)
{
   int i, j;

#ifdef TCPLX
   M *= 2;
   lda *= 2;
#endif
   dumb_seed(seed);
   for (j=N; j; j--)
   {
      for (i=0; i != M; i++) A[i] = dumb_rand();
      for (; i < lda; i++) A[i] = -9e27;
      A += lda;
   }
}

#ifdef ATL_NANC
void matnan(int M, int N, TYPE *C, int ldc, int seed)
/*
 * Fills matrix with NaNs
 */
{
   int i, j;
   unsigned int *ip;
   TYPE MyNaN;
   #ifdef TCPLX
      const int m=M+M, ldC=ldc+ldc;
   #else
      const int m=M, ldC=ldc;
   #endif
#if 0
   ip = (unsigned int*) &MyNaN;
   ip[0] = 1;
   ip[1] = 2047 << (31-11);
#else
   MyNaN = 0.0/0.0;
#endif
   for (j=0; j < N; j++, C += ldC)
   {
      for (i=0; i < m; i++)
         C[i] = MyNaN;
   }
}
#endif

#if defined(TRUST_C)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(Mjoin(cblas_,PRE),gemm) \
         (CblasColMajor, TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(ATL_NOAFFINITY) || defined(ATL_AFFINITY)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,tgemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TRUST_SMALL)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,small_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TRUST_BIGNORK)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,bignork_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TRUST_BIG)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,big_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TRUST_BPP)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,mmBPP)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TRUST_JIT)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,mmJITcp)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(ATL_USEPTHREADS) && !defined(TEST_F77)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,gemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#else
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,f77gemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#endif

#ifdef TEST_GPMM
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,gpmm)(PackGen, TA, PackGen, TB, PackGen, m, n, k, al, \
                       A, 0, 0, lda, B, 0, 0, ldb, be, C, 0, 0, ldc)
#elif defined(ATL_NOAFFINITY) || defined(ATL_AFFINITY)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,tgemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_F77)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,f77gemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_BIG)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,big_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_SMALLK)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,smallK_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_SMALL)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,small_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_BPP)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,mmBPP)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_JIT)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,mmJITcp)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TEST_RANKK)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
   { \
      if (k <= ATL_vMAX_KB) \
      { \
         ATL_assert(!Mjoin(PATL,vrankK)(TA, TB, m, n, k, al, A, lda, B, ldb, \
                                        be, C, ldc));\
      } \
      else \
      { \
         fprintf(stderr, "maxK exceeded at %d\n", k); \
         exit(k); \
      } \
   }
#elif defined(TEST_GEMM_RK)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      ATL_assert(!Mjoin(PATL,gemm_rK)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc))
#elif defined(TEST_ALIASED)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,aliased_gemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(ATL_USEPTHREADS)
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,tgemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#else
   #define test_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,gemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#endif

int mmcase(int TEST, int CACHESIZE, char TA, char TB, int M, int N, int K,
	   SCALAR alpha, TYPE *A, size_t lda, TYPE *B, size_t ldb, SCALAR beta,
	   TYPE *C, size_t ldc, TYPE *D, size_t ldd)
{
   char *pc;
#ifdef TREAL
   char *form="%4d   %c   %c %4d %4d %4d  %5.1f  %5.1f  %6.2f %5.1f %5.2f   %3s\n";
   #define MALPH alpha
   #define MBETA beta
#else
   #define MALPH *alpha, alpha[1]
   #define MBETA *beta, beta[1]
   char *form="%4d   %c   %c %4d %4d %4d  %5.1f %5.1f  %5.1f %5.1f  %6.2f %6.1f %4.2f   %3s\n";
#endif
   int ii, jj, i, j=0, PASSED, nerrs, Na, Nb;
   double t0, t1, t2, t3, mflop;
   TYPE maxval, f1, ferr;
   static TYPE feps=0.0;
   static int itst=1;
   /*int *L2, nL2=(1.3*L2SIZE)/sizeof(int);*/
   enum ATLAS_TRANS TAc, TBc;
   double l2ret;

   if (!TEST) D = C;
   /*if (nL2) L2 = malloc(nL2*sizeof(int));*/
   #if !ATL_LINEFLUSH
      l2ret = ATL_flushcache( CACHESIZE );
   #endif
   if (TA == 'n' || TA == 'N')
   {
      matgen(M, K, A, lda, K*1112);
      TAc = AtlasNoTrans;
      Na = K;
   }
   else
   {
      matgen(K, M, A, lda, K*1112);
      if (TA == 'c' || TA == 'C') TAc = AtlasConjTrans;
      else TAc = AtlasTrans;
      Na = M;
   }
   if (TB == 'n' || TB == 'N')
   {
      matgen(K, N, B, ldb, N*2238);
      TBc = AtlasNoTrans;
      Nb = N;
   }
   else
   {
      matgen(N, K, B, ldb, N*2238);
      if (TB == 'c' || TB == 'C') TBc = AtlasConjTrans;
      else TBc = AtlasTrans;
      Nb = K;
   }
#ifdef ATL_NANC
   matnan(M, N, C, ldc, M*N);
#else
   matgen(M, N, C, ldc, M*N);
#endif

#ifdef DEBUG
   printmat("A0", M, K, A, lda);
   printmat("B0", K, N, B, ldb);
   printmat("C0", M, N, C, ldc);
#endif

   /*
     if (L2)
     {
     for (i=0; i != nL2; i++) L2[i] = 0.0;
     for (i=0; i != nL2; i++) j += L2[i];
     }*/

   /* invalidate L2 cache */
   #if !ATL_LINEFLUSH
      l2ret = ATL_flushcache( -1 );
   #else
      ATL_flushCacheByAddr(ldc*N, C);
      ATL_flushCacheByAddr(ldb*Nb, B);
      ATL_flushCacheByAddr(lda*Na, A);
   #endif

   t0 = time00();
   trusted_gemm(TAc, TBc, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   t1 = time00() - t0;
   if (t1 <= 0.0) mflop = t1 = 0.0;
   else   /* flop rates actually 8MNK+12MN & 2MNK + 2MN, resp */
      #ifdef TCPLX
         mflop = ( ((8.0*M)*N)*K ) / (t1*1000000.0);
      #else
         mflop = ( ((2.0*M)*N)*K ) / (t1*1000000.0);
      #endif
   printf(form, itst, TA, TB, M, N, K, MALPH, MBETA, t1, mflop, 1.0, "---");

#ifdef DEBUG
   printmat("C", M, N, C, ldc);
#endif

#ifndef TIMEONLY
   #ifdef ATL_NANC
      matnan(M, N, D, ldd, M*N);
   #else
      matgen(M, N, D, ldd, M*N);
   #endif

   /* invalidate L2 cache */
   #if !ATL_LINEFLUSH
      l2ret = ATL_flushcache( -1 );
   #else
      ATL_flushCacheByAddr(ldc*N, D);
      ATL_flushCacheByAddr(ldb*Nb, B);
      ATL_flushCacheByAddr(lda*Na, A);
   #endif
   t0 = time00();
   test_gemm(TAc, TBc, M, N, K, alpha, A, lda, B, ldb, beta, D, ldd);

   t2 = time00() - t0;
   if (t2 <= 0.0) t2 = mflop = 0.0;
   else
      #ifdef TCPLX
         mflop = ( ((8.0*M)*N)*K ) / (t2*1000000.0);
      #else
         mflop = ( ((2.0*M)*N)*K ) / (t2*1000000.0);
      #endif
#ifdef DEBUG
   printmat("D", M, N, D, ldd);
#endif
   if (TEST)
   {
      if (feps == 0.0)
      {
#if 0
         f1 = feps = 0.5;
         do
         {
            feps = f1;
            f1 *= 0.5;
            maxval = 1.0 + f1;
         }
         while (maxval != 1.0);
         printf("feps=%e\n",feps);
#else
         feps = EPS;
#endif
#ifdef DEBUG
         printf("feps=%e\n",feps);
#endif
      }
#ifdef TREAL
      ferr = 2.0 * (Mabs(alpha) * 2.0*K*feps + Mabs(beta) * feps) + feps;
#else
      f1 = Mabs(*alpha) + Mabs(alpha[1]);
      maxval = Mabs(*beta) + Mabs(beta[1]);
      ferr = 2.0 * (f1*8.0*K*feps + maxval*feps) + feps;
#endif
      PASSED = 1;
      maxval = 0.0;
      pc = "YES";
      nerrs = ii = jj = 0;
      for (j=0; j != N; j++)
      {
         for (i=0; i != M SHIFT; i++)
         {
            #ifdef ATL_NANC
               if (D[i] != D[i])
               {
                  nerrs++;
                  PASSED = 0;
                  printf("NaN in test_gemm at C(%d,%d)\n", i, j);
               }
               if (C[i] != C[i])
               {
                  nerrs++;
                  PASSED = 0;
                  printf("NaN in trusted_gemm at C(%d,%d)\n", i, j);
               }
            #endif
            f1 = D[i] - C[i];
            if (f1 < 0.0) f1 = -f1;
            if (f1 > ferr)
            {
               nerrs++;
               PASSED = 0;
               pc = "NO!";
               if (f1 > maxval)
               {
                  maxval=f1;
                  ii = i+1;
                  jj = j+1;
               }
            }
            else if (D[i] != D[i])
            {
               nerrs++;
               PASSED = 0;
               pc = "NaN";
               maxval = D[i];
            }
         }
         D += ldd SHIFT;
         C += ldc SHIFT;
      }
      if (maxval != 0.0)
         fprintf(stderr, "ERROR: nerr=%d, i=%d, j=%d, maxval=%e\n", nerrs, ii,jj, maxval);
   }
   else pc = "---";
   if (t1 == t2) t3 = 1.0;
   else if (t2 != 0.0) t3 = t1/t2;
   else t3 = 0.0;
   #ifdef ATL_RKXOVER
      if (K <= ATL_vMAX_KB)
      {
         if (SCALAR_IS_ZERO(beta))
            speedup_rk_b0[K] = (t3 >= 1.0) ? t3*100 : 0;
         else
            speedup_rk[K] = (t3 >= 1.0) ? t3*100 : 0;
      }
   #endif
   printf(form, itst++, TA, TB, M, N, K, MALPH, MBETA, t2, mflop, t3, pc);
#else
   itst++;
   PASSED = 1;
#endif
   /*free(L2);*/
   #if !ATL_LINEFLUSH
      l2ret = ATL_flushcache( 0 );
   #endif
   return(PASSED);
}

int mmcase0(int MFLOP, size_t CACHESIZE, char TA, char TB, int M, int N, int K,
	    SCALAR alpha, size_t lda, size_t ldb, SCALAR beta, size_t ldc)
{
   char *pc;
#ifdef TREAL
   char *form="%4d   %c   %c %4d %4d %4d  %5.1f  %5.1f  %6.2f %5.1f %5.2f   %3s\n";
   #define MALPH alpha
   #define MBETA beta
   TYPE betinv, bet=beta;
#else
   #define MALPH *alpha, alpha[1]
   #define MBETA *beta, beta[1]
   char *form="%4d   %c   %c %4d %4d %4d  %5.1f %5.1f  %5.1f %5.1f  %6.2f %6.1f %4.2f   %3s\n";
   TYPE betinv[2], *bet=beta;
#endif
   #ifdef ATL_USETHREADS
      const size_t cs=ATL_NTHREADS *
                      ATL_MulBySize(ATL_DivBySize(CACHESIZE+ATL_sizeof-1));
   #else
      const size_t cs=ATL_MulBySize(ATL_DivBySize(CACHESIZE+ATL_sizeof-1));
   #endif
   const size_t setsz = M*N + M*K + N*K;
   size_t nset;
   size_t i, incA, incB, incC, inc, k;
   TYPE *c, *C, *a, *A, *b, *B, *st;
   int nreps, ii, jj, j=0, PASSED, nerrs;
   double t0, t1, t2, t3, mflop, mf, mops;
   TYPE maxval, f1, ferr;
   static TYPE feps=0.0;
   static int itst=1;
   enum ATLAS_TRANS TAc, TBc;
   void *vp;
   #ifdef TCPLX
      if (*beta == 0.0 && beta[1] == 0.0) betinv[0] = betinv[1] = 0.0;
      else if (beta[1] == 0.0) { betinv[0] = 1 / *beta;  betinv[1] = 0.0; }
      else
      {
         t0 = *beta;
         t1 = beta[1];
         if (Mabs(t1) <= Mabs(t0))
         {
            t2 = t1 / t0;
            betinv[0] = t0 = 1.0 / (t0 + t1*t2);
            betinv[1] = -t0 * t2;
         }
         else
         {
            t2 = t0 / t1;
            betinv[1] = t0 = -1.0 / (t1 + t0*t2);
            betinv[0] = -t2 * t0;
         }
      }
      mops = ( ((8.0*M)*N)*K ) / 1000000.0;
   #else
      if (beta != 0.0) betinv = 1.0 / beta;
      else betinv = beta;
      mops = ( ((2.0*M)*N)*K ) / 1000000.0;
   #endif
   nreps = MFLOP / mops;
   if (nreps < 1) nreps = 1;
   if (TA == 'n' || TA == 'N')
   {
      TAc = AtlasNoTrans;
      incA = lda * K;
   }
   else
   {
      if (TA == 'c' || TA == 'C') TAc = AtlasConjTrans;
      else TAc = AtlasTrans;
      incA = lda * M;
   }
   if (TB == 'n' || TB == 'N')
   {
      incB = ldb * N;
      TBc = AtlasNoTrans;
   }
   else
   {
      incB = ldb * K;
      if (TB == 'c' || TB == 'C') TBc = AtlasConjTrans;
      else TBc = AtlasTrans;
   }
   incC = ldc*N;
   inc = incA + incB + incC;   /* amount actually allocated */
   nset = (cs+setsz-1)/setsz;
   if (nset < 2)
      nset = 2;
   vp = malloc(ATL_MulBySize(nset*inc)+ATL_Cachelen);
   ATL_assert(vp);
   C = c = ATL_AlignPtr(vp);
   a = A = C + incC;
   b = B = A + incA;
   st = C + nset*inc;
   #ifdef ATL_NANC
      matnan(inc, nset, C, inc, M*N);
   #else
      matgen(inc, nset, C, inc, M*N);
   #endif

#ifdef DEBUG
   printmat("A0", M, K, A, lda);
   printmat("B0", K, N, B, ldb);
   printmat("C0", M, N, C, ldc);
#endif

   t0 = time00();
   for (k=nreps; k; k--)
   {
      trusted_gemm(TAc, TBc, M, N, K, alpha, a, lda, b, ldb, bet, c, ldc);
      c += inc; a += inc; b += inc;
      if (c == st)
      {
         c = C; a = A; b = B;
         if (bet == beta) bet = betinv;
         else bet = beta;
      }
   }
   t1 = time00() - t0;
   t1 /= nreps;
   if (t1 <= 0.0) mflop = t1 = 0.0;
   else   /* flop rates actually 8MNK+12MN & 2MNK + 2MN, resp */
      mflop = mops / t1;
   printf(form, itst, TA, TB, M, N, K, MALPH, MBETA, t1, mflop, 1.0, "---");

#ifdef DEBUG
   printmat("C", M, N, C, ldc);
#endif

   matgen(inc, nset, C, inc, M*N);
   t0 = time00();
   for (k=nreps; k; k--)
   {
      test_gemm(TAc, TBc, M, N, K, alpha, a, lda, b, ldb, bet, c, ldc);
      c += inc; a += inc; b += inc;
      if (c == st)
      {
         c = C; a = A; b = B;
         if (bet == beta) bet = betinv;
         else bet = beta;
      }
   }

   t2 = time00() - t0;
   t2 /= nreps;
   if (t2 <= 0.0) t2 = mflop = 0.0;
   else mflop = mops / t2;

   pc = "---";
   if (t1 == t2) t3 = 1.0;
   else if (t2 != 0.0) t3 = t1/t2;
   else t3 = 0.0;
   #ifdef ATL_RKXOVER
      if (K <= ATL_vMAX_KB)
      {
         if (SCALAR_IS_ZERO(beta))
            speedup_rk_b0[K] = (t3 >= 1.0) ? t3*100 : 0;
         else
            speedup_rk[K] = (t3 >= 1.0) ? t3*100 : 0;
      }
   #endif
   printf(form, itst-4, TA, TB, M, N, K, MALPH, MBETA, t2, mflop, t3, pc);
   itst++;
   free(vp);
   return(1);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -Side <nsides> L/R -Uplo <nuplo> L/U -Atrans <ntrans> n/t/c -Btrans <ntrans> n/t/c -Diag <ndiags> N/U -M <m1> <mN> <minc> -N <n1> <nN> <ninc> <k1> <kN> <kinc> -n <n> -m <m> -k <k> -a <nalphas> <alpha1> ... <alphaN> -b <nbetas> <beta1> ... <betaN> -Test <0/1> -F <mflops> -C <cachesize>\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char *args[], int *TEST, int *nside,
              enum ATLAS_SIDE **Side, int *nuplo, enum ATLAS_UPLO **Uplo,
              int *nta, enum ATLAS_TRANS **TransA,
              int *ntb, enum ATLAS_TRANS **TransB, int *ndiag,
              enum ATLAS_DIAG **Diag, int *M0, int *MN, int *Minc,
              int *N0, int *NN, int *Ninc, int *K0, int *KN, int *Kinc,
              int *nalphas, TYPE **alphas, int *nbetas, TYPE **betas,
              int *LDA_IS_M, int *MFLOP, int *CACHESIZE, int *nrep)

{
   char ch;
   int i, j;
   char *outfile=NULL;
/*
 * Set up defaults
 */
   *nrep = 1;
   *TEST = 1;
   *M0 = *N0 = *K0 = -1;
   *nuplo = *nta = *ntb = *nside = *ndiag = *nalphas = *nbetas = -1;
   *MFLOP = *LDA_IS_M = 0;
#ifdef L2SIZE
   *CACHESIZE = L2SIZE;               /* Size of largest cache to flush */
#else
   *CACHESIZE = 4*1024*1024;
#endif

   for (i=1; i < nargs; i++)
   {
      switch(args[i][1])
      {
      case '#':
         *nrep = atoi(args[++i]);
         break;
      case 'F':
         *MFLOP = atoi(args[++i]);
         break;
      case 'C':
            if( args[i+1] == NULL ) PrintUsage( args[0] );
	    *CACHESIZE = 1024*atoi(args[++i]);
            break;
      case 'S':
         *nside = atoi(args[++i]);
         *Side = malloc(*nside * sizeof(int));
         assert(*Side);
         for (j=0; j != *nside; j++)
         {
            ch = *args[++i];
            if (ch == 'r' || ch == 'R') (*Side)[j] = AtlasRight;
            else if (ch == 'l' || ch == 'L') (*Side)[j] = AtlasLeft;
            else PrintUsage(args[0]);
         }
         break;
      case 'U':
         *nuplo = atoi(args[++i]);
         *Uplo = malloc(*nuplo * sizeof(int));
         assert(*Uplo);
         for (j=0; j != *nuplo; j++)
         {
            ch = *args[++i];
            if (ch == 'u' || ch == 'U') (*Uplo)[j] = AtlasUpper;
            else if (ch == 'l' || ch == 'L') (*Uplo)[j] = AtlasLower;
            else PrintUsage(args[0]);
         }
         break;
      case 'D':
         *ndiag = atoi(args[++i]);
         *Diag = malloc(*ndiag * sizeof(int));
         assert(*Diag);
         for (j=0; j != *ndiag; j++)
         {
            ch = *args[++i];
            if (ch == 'u' || ch == 'U') (*Diag)[j] = AtlasUnit;
            else if (ch == 'n' || ch == 'N') (*Diag)[j] = AtlasNonUnit;
            else PrintUsage(args[0]);
         }
         break;
      case 'A':
         *nta   = atoi(args[++i]);
         *TransA = malloc(*nta * sizeof(int));
         assert(*TransA);
         for (j=0; j != *nta; j++)
         {
            ch = *args[++i];
            if (ch == 'n' || ch == 'N') (*TransA)[j] = AtlasNoTrans;
            else if (ch == 't' || ch == 'T') (*TransA)[j] = AtlasTrans;
            else if (ch == 'c' || ch == 'C') (*TransA)[j] = AtlasConjTrans;
            else PrintUsage(args[0]);
         }
         break;
      case 'B':
         *ntb   = atoi(args[++i]);
         *TransB = malloc(*ntb * sizeof(int));
         assert(*TransB);
         for (j=0; j != *ntb; j++)
         {
            ch = *args[++i];
            if (ch == 'n' || ch == 'N') (*TransB)[j] = AtlasNoTrans;
            else if (ch == 't' || ch == 'T') (*TransB)[j] = AtlasTrans;
            else if (ch == 'c' || ch == 'C') (*TransB)[j] = AtlasConjTrans;
            else PrintUsage(args[0]);
         }
         break;
      case 'M':
         *M0 = atoi(args[++i]);
         *MN = atoi(args[++i]);
         *Minc = atoi(args[++i]);
         break;
      case 'N':
         *N0 = atoi(args[++i]);
         *NN = atoi(args[++i]);
         *Ninc = atoi(args[++i]);
         break;
      case 'K':
         *K0 = atoi(args[++i]);
         *KN = atoi(args[++i]);
         *Kinc = atoi(args[++i]);
         break;
      case 'T':
         *TEST = atoi(args[++i]);
         break;
      case 'm':
         *M0 = *MN = *Minc = atoi(args[++i]);
         break;
      case 'n':
         *N0 = *NN = *Ninc = atoi(args[++i]);
         break;
      case 'k':
         *K0 = *KN = *Kinc = atoi(args[++i]);
         break;
      case 'a':
         *nalphas = atoi(args[++i]);
         *alphas = malloc(ATL_MulBySize(*nalphas));
         assert(*alphas);
         for (j=0; j < *nalphas SHIFT; j++) (*alphas)[j] = atof(args[++i]);
         break;
      case 'b':
         *nbetas  = atoi(args[++i]);
         *betas  = malloc(ATL_MulBySize(*nbetas ));
         assert(*betas );
         for (j=0; j < *nbetas SHIFT; j++) (*betas)[j] = atof(args[++i]);
         break;
      case 'd':
         *LDA_IS_M  = atoi(args[++i]);
         break;
      #ifdef ATL_RKXOVER
      case 'o':
         outfile = args[++i];
         break;
      #endif
      default:
         PrintUsage(args[0]);
         break;
      }
   }
/*
 * Finish setting up defaults if the user has not selected
 */
   if (*N0 == -1)
   {
      #ifdef ATL_RKXOVER
         #ifdef SCPLX
            *N0 = *NN = *Ninc = 3000;
         #elif defined(DCPLX)
            *N0 = *NN = *Ninc = 2000;
         #elif defined(SREAL)
            *N0 = *NN = *Ninc = 4000;
         #else
            *N0 = *NN = *Ninc = 3000;
         #endif
      #else
         *N0 = 100;
         *NN = 1000;
         *Ninc = 100;
      #endif
   }
#ifdef ATL_RKXOVER
   *K0 = 1;
   *KN = ATL_vMAX_KB;
   *Kinc = 1;
   if (!outfile)
      outfile = Mstr(Mjoin(Mjoin(atlas_,PRE),rkXover.h));
   fprk = fopen(outfile, "w");
   assert(fprk);
   *nbetas  = 2;
   *betas  = malloc(ATL_MulBySize(1));
   assert(*betas );
   #ifdef TREAL
      **betas  = 1.0;
      (*betas)[1] = 0.0;
   #else
      **betas  = 1.0;
      (*betas)[2] = (*betas)[3] = (*betas)[1] = 0.0;
   #endif
   *TEST = 0;
#endif
   if (*nside == -1)
   {
      *nside = 1;
      *Side = malloc(sizeof(int));
      assert(*Side);
      **Side = AtlasLeft;
   }
   if (*nuplo == -1)
   {
      *nuplo = 1;
      *Uplo = malloc(sizeof(int));
      assert(*Uplo);
      **Uplo = AtlasLower;
   }
   if (*nta == -1)
   {
      *nta = 1;
      *TransA = malloc(sizeof(int));
      assert(*TransA);
      **TransA = AtlasNoTrans;
   }
   if (*ntb == -1)
   {
      *ntb = 1;
      *TransB = malloc(sizeof(int));
      assert(*TransB);
      **TransB = AtlasNoTrans;
   }
   if (*ndiag == -1)
   {
      *ndiag = 1;
      *Diag = malloc(sizeof(int));
      assert(*Diag);
      **Diag = AtlasNonUnit;
   }
   if (*nalphas == -1)
   {
      *nalphas = 1;
      *alphas = malloc(ATL_MulBySize(1));
      assert(*alphas);
      #ifdef TREAL
         **alphas = 1.0;
      #else
         **alphas = 1.0;
         (*alphas)[1] = 0.0;
      #endif
   }
   if (*nbetas  == -1)
   {
      *nbetas  = 1;
      *betas  = malloc(ATL_MulBySize(1));
      assert(*betas );
      #ifdef TREAL
         **betas  = 1.0;
      #else
         **betas  = 1.0;
         (*betas)[1] = 0.0;
      #endif
      #ifdef ATL_NANC
         **betas = 0.0;
      #endif
   }
}
int ___main(){return(-1);}   /* this nonsense helps */
int __main(){return(-1);}    /* when you link with certain */
int MAIN__(){return(-1);}    /* fortran compilers */
int _MAIN_(){return(-1);}

int main(int nargs, char *args[])
/*
 *  tst <tst> <# TA> <TA's> <# TB's> <TB's> <M0> <MN> <incM> <N0> <NN> <incN>
 *      <K0> <KN> <incK> <# alphas> <alphas> <# betas> <betas>
 *
 */
{
   int M0, MN, incM, N0, NN, incN, K0, KN, incK, lda, ldb, ldc, MFLOP;
   int i, k, m, n, im, in, ik, ita, itb, ia, ib, nTA, nTB, nalph, nbeta;
   int itst=0, ipass=0, TEST, LDA_IS_M, MSAME=0, KSAME=0, r;
   int ndiag, nuplo, nside, nrep;
   TYPE *alph, *beta, *A, *B, *C, *D=NULL;
   #ifdef TREAL
      TYPE bet1 = 1.0, alp1 = -1.0;
   #else
      TYPE bet1[2] = {1.0, 0.0}, alp1[2] = {-1.0, 0.0};
   #endif
   char TA, TB;
   enum ATLAS_SIDE *Side;
   enum ATLAS_UPLO *Uplo;
   enum ATLAS_TRANS *TransA, *TransB, TAc, TBc;
   enum ATLAS_DIAG *Diag;
   int CACHESIZE;

   GetFlags(nargs, args, &TEST, &nside, &Side, &nuplo, &Uplo,
            &nTA, &TransA, &nTB, &TransB, &ndiag, &Diag,
            &M0, &MN, &incM, &N0, &NN, &incN, &K0, &KN, &incK,
            &nalph, &alph, &nbeta, &beta, &LDA_IS_M, &MFLOP,&CACHESIZE, &nrep);

   if (M0 == -1)
   {
      MSAME = 1;
      M0 = MN = incM = NN;
   }
   if (K0 == -1)
   {
      KSAME = 1;
      K0 = KN = incK = NN;
   }

   if (!MFLOP)
   {
      A = malloc(MN*KN*ATL_sizeof);
      B = malloc(NN*KN*ATL_sizeof);
      C = malloc(MN*NN*ATL_sizeof);
      if (TEST) D = malloc(MN*NN*ATL_sizeof);
      else D = NULL;
      if (!A || !B || !C || (TEST && !D))
      {
         fprintf(stderr, "Not enough memory to run tests!!\n");
         exit(-1);
      }
   }
/*
 * Page the code in from disk, so first timing doesn't blow
 */
   if (MFLOP)
   {
      #ifdef ATL_RKXOVER
         k = Mmin(100, KN);
      #else
         k = 100;
      #endif
      mmcase0(10, 1, 'n', 'n', 100, 100, k, alp1, 100, 100, bet1, 100);
      mmcase0(10, 1, 'n', 't', 100, 100, k, alp1, 100, 100, bet1, 100);
      mmcase0(10, 1, 't', 'n', 100, 100, k, alp1, 100, 100, bet1, 100);
      mmcase0(10, 1, 't', 't', 100, 100, k, alp1, 100, 100, bet1, 100);
   }
   else
   {
      m = Mmin(100, MN);
      k = Mmin(100, KN);
      n = Mmin(100, NN);
      matgen(m, k, A, m, m*k);
      matgen(k, n, B, k, n*k);
      matgen(m, n, C, m, m*n);
      TA = TB = 'N';
      TAc = TBc = AtlasNoTrans;
      trusted_gemm(TAc, TBc, m, n, k, alp1, A, m, B, k, bet1, C, m);
      test_gemm(TAc, TBc, m, n, k, alp1, A, m, B, k, bet1, C, m);
   }

#ifdef TREAL
   printf("\nTEST  TA  TB    M    N    K  alpha   beta    Time  Mflop  SpUp  PASS\n");
   printf("====  ==  ==  ===  ===  ===  =====  =====  ======  =====  ====  ====\n\n");
#else
   printf("\nTEST  TA  TB    M    N    K  ralph ialph  rbeta ibeta    Time  Mflop  SpUp  PASS\n");
   printf("====  ==  ==  ===  ===  ===  ===== =====  ===== =====  ======  =====  ====  ====\n\n");
#endif
   for (im=M0; im <= MN; im += incM)
   {
      for (n=N0; n <= NN; n += incN)
      {
         if (MSAME) m = n;
         else m = im;
         for (ik=K0; ik <= KN; ik += incK)
         {
            if (KSAME) k = n;
            else k = ik;
            for (ita=0; ita != nTA; ita++)
            {
               if (TransA[ita] == AtlasNoTrans) TA = 'N';
               else if (TransA[ita] == AtlasTrans) TA = 'T';
               else if (TransA[ita] == AtlasConjTrans) TA = 'C';

               for (itb=0; itb != nTB; itb++)
               {
                  if (TransB[itb] == AtlasNoTrans) TB = 'N';
                  else if (TransB[itb] == AtlasTrans) TB = 'T';
                  else if (TransB[itb] == AtlasConjTrans) TB = 'C';
                  for (ia=0; ia != nalph; ia++)
                  {
                     for (ib=0; ib != nbeta; ib++)
                     {
                        itst++;
                        if (LDA_IS_M)
                        {
                           if (TA == 'n' || TA == 'N') lda = m;
                           else lda = k;
                           if (TB == 'n' || TB == 'N') ldb = k;
                           else ldb = n;
                           ldc = m;
                        }
                        else
                        {
                           if (TA == 'n' || TA == 'N') lda = MN;
                           else lda = KN;
                           if (TB == 'n' || TB == 'N') ldb = KN;
                           else ldb = NN;
                           ldc = MN;
                        }
                        for (r=0; r < nrep; r++)
                        {
                           if (MFLOP)
                           {
                              ipass++;
#ifdef TREAL
                                 mmcase0(MFLOP, CACHESIZE, TA, TB, m, n, k,
				         alph[ia], lda, ldb, beta[ib], ldc);
#else
                                 mmcase0(MFLOP, CACHESIZE, TA, TB, m, n, k,
				         alph+(ia SHIFT), lda, ldb,
				         beta+(ib SHIFT), ldc);
#endif
                           }
                           else
                           {
#ifdef TREAL
                                 ipass += mmcase(TEST, CACHESIZE, TA, TB, m,
					         n, k, alph[ia], A, lda, B, ldb,
                                                 beta[ib], C, ldc, D,ldc);
#else
                                 ipass += mmcase(TEST, CACHESIZE, TA, TB, m,
					         n, k, alph+(ia SHIFT), A,
					         lda, B, ldb, beta+(ib SHIFT),
					         C, ldc, D,ldc);
#endif
                           }
/*
 *                         If we lose 5 times in a row, quit search
 */
                           #ifdef ATL_RKXOVER
                              if (k > 16 &&    /* don't quit too early */
                                  !(speedup_rk_b0[k] | speedup_rk_b0[k-1] |
                                    speedup_rk_b0[k-2] | speedup_rk_b0[k-3] |
                                    speedup_rk_b0[k-4] | speedup_rk[k] |
                                    speedup_rk_b0[k-1] | speedup_rk_b0[k-2] |
                                    speedup_rk_b0[k-3] | speedup_rk[k-4] |
                                    speedup_rk_b0[k-6] | speedup_rk[k-7] |
                                    speedup_rk_b0[k-8] | speedup_rk[k-9] |
                                    speedup_rk_b0[k-10] | speedup_rk[k-11]
                                   )
                                 )
                              {
                                 MAX_vKB = k-5;
                                 goto RANK_K;
                              }
                              #endif
                        }
                     }
                  }
               }
            }
         }
      }
   }
   #ifdef ATL_RKXOVER
RANK_K:
      fprintf(fprk, "#ifndef ATLAS_RKXOVER\n   #define ATLAS_RKXOVER\n\n");

/*
 *    First, find last size where we got speedup.  Then, if KB > 80, demand
 *    at > 3% speedup before declaring it worth the extra code
 */
      for (speedup_rk_b0[0]=0, n=MAX_vKB; !speedup_rk_b0[n]; n--);
      while(n > 80 && speedup_rk_b0[n] < 104) n--;
      fprintf(fprk, "#define ATL_RK_MAXK_B0 %d\n", n);
      fprintf(fprk, "static int ATL_rkXover_b0[%d] = {0", n+1);
      for (i=1; i <= n; i++)
         fprintf(fprk, ",%d", speedup_rk_b0[i]);
      fprintf(fprk, "};\n");

      for (speedup_rk[0]=0, m=MAX_vKB; !speedup_rk[m]; m--);
      while(m > 80 && speedup_rk[m] < 104) m--;
      fprintf(fprk, "#define ATL_RK_MAXK_BX %d\n", m);
      fprintf(fprk, "static int ATL_rkXover_bX[%d] = {0", m+1);
      for (i=1; i <= m; i++)
         fprintf(fprk, ",%d", speedup_rk[i]);
      fprintf(fprk, "};\n");
      fprintf(fprk, "#define ATL_RK_MAXK %d\n", m >= n ? m : n);

      fprintf(fprk, "\n#endif\n");
      fclose(fprk);
   #endif
   if (TEST && !MFLOP)
      printf("\nNTEST=%d, NUMBER PASSED=%d, NUMBER FAILURES=%d\n",
             itst, ipass, itst-ipass);
   else printf("\nDone with %d timing runs\n",itst);
   free(Side);
   free(Uplo);
   free(TransA);
   free(TransB);
   free(Diag);
   free(alph);
   free(beta);
   if (!MFLOP)
   {
      free(A);
      free(B);
      free(C);
      if (D) free(D);
   }
   return(0);
}
