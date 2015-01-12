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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "atlas_misc.h"

#define dumb_seed(iseed_) srand(iseed_)
#ifndef RAND_MAX  /* rather dangerous non-ansi workaround */
   #define RAND_MAX ((unsigned long)(1<<30))
#endif
#define dumb_rand() ( 0.5 - ((double)rand())/((double)RAND_MAX) )
#if defined(PentiumCPS) && !defined(WALL)
   #define WALL
#endif
#if defined(WALL)
   #define time00 ATL_walltime
#else
   #define time00 ATL_cputime
#endif

#define SAFE_ALPHA -3
#ifndef REPS
   #define REPS 1500
#endif

#ifndef L2SIZE
   #define L2SIZE 4194304
#endif

#define Mjoin(pre, nam) my_join(pre, nam)
#define my_join(pre, nam) pre ## nam
#define Mjoin(pre, nam) my_join(pre, nam)
#define my_join(pre, nam) pre ## nam
#define Mstr2(m) # m
#define Mstr(m) Mstr2(m)
#define Mmin(x, y) ( (x) > (y) ? (y) : (x) )

#ifdef FULLMM
   #define LANG 'M'
#elif !defined(LangF77)
   #define LANG 'C'
#else
   #define LANG 'F'
#endif
#if defined(sREAL)
   #include "atlas_ssysinfo.h"
   #define PRE 's'
   #define pre s
   #define TYPE float
   #define SCALAR float
   #define ATL_sizeof sizeof(TYPE)
   #define TREAL
   #define SHIFT
   #define EPS 1.0e-7
#elif defined(dREAL)
   #include "atlas_dsysinfo.h"
   #define PRE 'd'
   #define pre d
   #define TYPE double
   #define SCALAR double
   #define ATL_sizeof sizeof(TYPE)
   #define TREAL
   #define SHIFT
   #define EPS 1.0e-16
#elif defined (qREAL)
   #include "atlas_qsysinfo.h"
   #define PRE 'q'
   #define pre q
   #define TYPE long double
   #define SCALAR long double
   #define ATL_sizeof sizeof(TYPE)
   #define TREAL
   #define SHIFT
#elif defined(sCPLX) || defined(cCPLX) || defined(cREAL)
   #include "atlas_csysinfo.h"
   #define PRE 'c'
   #define pre c
   #define TYPE float
   #define ATL_sizeof (sizeof(TYPE)<<1)
   #define SCALAR float *
   #define TCPLX
   #define SHIFT <<1
   #define EPS 1.0e-7
#else
   #include "atlas_zsysinfo.h"
   #define PRE 'z'
   #define pre z
   #define TYPE double
   #define ATL_sizeof (sizeof(TYPE)<<1)
   #define SCALAR double*
   #define TCPLX
   #define SHIFT <<1
   #define EPS 1.0e-16
#endif

#ifdef TCPLX
   #define bn1 bX  /* don't use bn1 case anymore */
#endif
#ifndef MB
   #define MB NB
#endif
#ifndef KB
   #define KB NB
#endif

#ifndef csA
   #define csA 2
#endif
#ifndef csB
   #define csB 2
#endif

#if defined(tranAt) || defined(tranAT)
   #define TransA
#elif defined(tranAc) || defined(tranAC)
   #define ConjTransA
#else
   #define NoTransA
#endif
#if defined(tranBt) || defined(tranBT)
   #define TransB
#elif defined(tranBc) || defined(tranBC)
   #define ConjTransB
#else
   #define NoTransB
#endif
#ifdef NoTransA
   #define Ma MB
   #define Na KB
#else
   #define Ma KB
   #define Na MB
#endif
#ifdef NoTransB
   #define Mb KB
   #define Nb NB
#else
   #define Mb NB
   #define Nb KB
#endif

#ifdef IJK
   #undef IJK
   #define LOOPO IJK
#else
   #undef JIK
   #define  LOOPO JIK
#endif

#ifdef LDA2
   #if (LDA2 <= 0)
      #undef LDA2
   #endif
#endif
#ifdef LDB2
   #if (LDB2 <= 0)
      #undef LDB2
   #endif
#endif
#ifdef LDC2
   #if (LDC2 <= 0)
      #undef LDC2
   #endif
#endif

#ifdef TransA
   #define TA T
   #ifndef LDA2
      #define LDA2 KB
   #endif
#elif defined(ConjTransA)
   #define TA C
   #ifndef LDA2
      #define LDA2 KB
   #endif
#else
   #define TA N
   #ifndef LDA2
      #define LDA2 MB
   #endif
#endif
#ifdef TransB
   #define TB T
   #ifndef LDB2
      #define LDB2 NB
   #endif
#elif defined(ConjTransB)
   #define TB C
   #ifndef LDB2
      #define LDB2 NB
   #endif
#else
   #define TB N
   #ifndef LDB2
      #define LDB2 KB
   #endif
#endif

#ifndef LDC2
   #define LDC2 MB
#endif
#if (ALPHA == 1)
   #define ALPHAnam _a1
#elif (ALPHA == -1)
   #define ALPHAnam _an1
#elif (ALPHA == SAFE_ALPHA)
   #define ALPHAnam _aXX
#else
   #define ALPHAnam _aX
#endif
#if (BETA == 1)
   #define BETAnam _b1
   #define NBETAnam _bn1
#elif (BETA == 0)
   #define BETAnam _b0
   #define NBETAnam _b0
#elif (BETA == -1)
   #define BETAnam _bn1
   #define NBETAnam _b1
#else
   #define BETAnam _bX
   #define NBETAnam _bX
#endif


#define ppre Mjoin(Mjoin(ATL_,pre),LOOPO)
#define MNKnam Mjoin(Mjoin(Mjoin(Mjoin(MB0,x),NB0),x),KB0)
#define TRnam Mjoin(TA, TB)
#define ldnam Mjoin(Mjoin(Mjoin(Mjoin(LDA,x),LDB),x),LDC)

#ifdef TREAL
   #define NBmm Mjoin(Mjoin(Mjoin(Mjoin(Mjoin(ppre,MNKnam), TRnam),ldnam), ALPHAnam), BETAnam)

#ifdef NEWMM
   #ifndef ATL_CINT
      #define ATL_CINT const int
   #endif
   void NBmm(ATL_CINT M, ATL_CINT N, ATL_CINT K, TYPE *A, TYPE *B, TYPE *C);
#else
   void NBmm(const int, const int, const int, const SCALAR, const TYPE*,
             const int, const TYPE*, const int, const SCALAR, TYPE*, const int);
#endif
#else
   #define NBmm0 Mjoin(Mjoin(Mjoin(Mjoin(ppre,MNKnam), TRnam),ldnam), ALPHAnam)
   #define _bn1 _bX

   void Mjoin(NBmm0,BETAnam)(const int, const int, const int, const TYPE,
                             const TYPE*, const int, const TYPE*, const int,
                             const TYPE, TYPE*, const int);
   void Mjoin(NBmm0,_bn1)(const int, const int, const int, const TYPE,
                        const TYPE*, const int, const TYPE*, const int,
                        const TYPE, TYPE*, const int);
   void Mjoin(NBmm0,_b1)(const int, const int, const int, const TYPE,
                       const TYPE*, const int, const TYPE*, const int,
                       const TYPE, TYPE*, const int);
#if csA == 1 && csB == 1
   #if csC == 2
      #define NBmm(m_, n_, k_, alp_, a_, lda_, b_, ldb_, bet_, c_, ldc_) \
      { \
         Mjoin(NBmm0,NBETAnam)(m_, n_, k_, *(alp_), (a_), lda_, (b_), \
                               ldb_, -(*(bet_)), c_, ldc_); \
         Mjoin(NBmm0,BETAnam)(m_, n_, k_, *(alp_), a_, lda_, (b_)+incb, ldb_, \
                              *(bet_), (c_)+1, ldc_); \
         Mjoin(NBmm0,_bn1)(m_, n_, k_, *(alp_), (a_)+inca, lda_, (b_)+incb, \
                           ldb_, rnone, c_, ldc_); \
         Mjoin(NBmm0,_b1)(m_, n_, k_, *(alp_), (a_)+inca, lda_, (b_), ldb_, \
                          rone, (c_)+1, ldc_); \
      }
   #elif csC == 1
      #define NBmm(m_, n_, k_, alp_, a_, lda_, b_, ldb_, bet_, c_, ldc_) \
      { \
         Mjoin(NBmm0,NBETAnam)(m_, n_, k_, *(alp_), (a_), lda_, (b_), \
                               ldb_, -(*(bet_)), c_, ldc_); \
         Mjoin(NBmm0,BETAnam)(m_, n_, k_, *(alp_), a_, lda_, (b_)+incb, ldb_, \
                              *(bet_), (c_)+incc, ldc_); \
         Mjoin(NBmm0,_bn1)(m_, n_, k_, *(alp_), (a_)+inca, lda_, (b_)+incb, \
                           ldb_, rnone, c_, ldc_); \
         Mjoin(NBmm0,_b1)(m_, n_, k_, *(alp_), (a_)+inca, lda_, (b_), ldb_, \
                          rone, (c_)+incc, ldc_); \
      }
   #endif
#else /* csA == 2 && csB == 2 && csC == 2 */
   #define NBmm(m_, n_, k_, alp_, a_, lda_, b_, ldb_, bet_, c_, ldc_) \
   { \
      Mjoin(NBmm0,NBETAnam)(m_, n_, k_, *(alp_), (a_)+1, lda_, (b_)+1, ldb_, \
                            -(*(bet_)), c_, ldc_); \
      Mjoin(NBmm0,BETAnam)(m_, n_, k_, *(alp_), (a_)+1, lda_, b_, ldb_, \
                           *(bet_), (c_)+1, ldc_); \
      Mjoin(NBmm0,_bn1)(m_, n_, k_, *(alp_), a_, lda_, b_, ldb_, \
                        rnone, c_, ldc_); \
      Mjoin(NBmm0,_b1)(m_, n_, k_, *(alp_), a_, lda_, (b_)+1, ldb_, \
                       rone, (c_)+1, ldc_); \
   }
#endif
#if 0
   #undef NBmm
   #define NBmm(m_, n_, k_, alp_, a_, lda_, b_, ldb_, bet_, c_, ldc_) \
   { \
      zgemm_("T", "N", &(m_), &(n_), &(k_), alp_, a_, &(lda_), b_, \
             &(ldb_), bet_, c_, &(ldc_)); \
   }
#endif
#endif

void tst_mm(const int M, const int N, const int K, const SCALAR alpha,
            const TYPE *A, const int lda0, const TYPE *B, const int ldb0,
            const SCALAR beta, TYPE *C, const int ldc0)
{
   int i, j, k;
   int lda = lda0 SHIFT, ldb = ldb0 SHIFT, ldc = ldc0 SHIFT;
   register TYPE c0;
   #ifdef TREAL
      for (j=0; j < N; j++)
      {
         for (i=0; i < M; i++)
         {
            c0 = 0.0;
            for (k=0; k < K; k++)
            {
               #if defined(NoTransA) && defined(NoTransB)
                  c0 += A[i+k*lda] * B[j*ldb+k];
               #elif defined(NoTransA) && defined(TransB)
                  c0 += A[i+k*lda] * B[j+k*ldb];
               #elif defined(TransA) && defined(NoTransB)
                  c0 += A[i*lda+k] * B[j*ldb+k];
               #elif defined(TransA) && defined(TransB)
                  c0 += A[i*lda+k] * B[j+k*ldb];
               #endif
            }
            C[i+j*ldc] = beta*C[i+j*ldc] + alpha*c0;
         }
      }
   #else
      register TYPE cr, ci, ar, ai, br, bi;
/*
 *    If matrix is stored split into real & imaginary parts, allocate some
 *    matrices and intermix them for f77-like imaginary matrices
 */
      #if csA == 1 && csB == 1
         int inc, ldaa, ldbb, lda2=lda/2, ldb2=ldb/2;
         TYPE *aa, *bb;
         aa = malloc(M*K*sizeof(TYPE)*2);
         bb = malloc(K*N*sizeof(TYPE)*2);
         assert(aa && bb);
         #ifdef NoTransA
            ldaa = M*2;
            inc = K*lda2;
            for (k=0; k < K; k++)
            {
               j = k*lda;
               for (i=0; i < M; i++)
               {
                  aa[k*ldaa+2*i]   = A[inc+k*lda2+i];
                  aa[k*ldaa+2*i+1] = A[k*lda2+i];
               }
            }
         #else
            ldaa = K*2;
            inc = M*lda2;
            for (i=0; i < M; i++)
            {
               for (k=0; k < K; k++)
               {
                  aa[i*ldaa+2*k]   = A[inc+i*lda2+k];
                  aa[i*ldaa+2*k+1] = A[i*lda2+k];
               }
            }
         #endif
         #ifdef NoTransB
            inc = N*ldb2;
            ldbb = K*2;
            for (j=0; j < N; j++)
            {
               for (k=0; k < K; k++)
               {
                  bb[j*ldbb+2*k]   = B[inc+j*ldb2+k];
                  bb[j*ldbb+2*k+1] = B[j*ldb2+k];
               }
            }
         #else
            inc = K*ldb2;
            ldbb = N*2;
            for (k=0; k < K; k++)
            {
               for (j=0; j < N; j++)
               {
                  bb[k*ldbb+2*j]   = B[inc+k*lda2+j];
                  bb[k*ldbb+2*j+1] = B[k*lda2+j];
               }
            }
         #endif
         A = (const TYPE *) aa;
         B = (const TYPE *) bb;
         lda = ldaa;
         ldb = ldbb;
      #endif

      for (j=0; j < N; j++)
      {
         for (i=0; i < M; i++)
         {
            cr = ci = 0.0;
            for (k=0; k < K; k++)
            {
               #if defined(NoTransA) && defined(NoTransB)
                  ar = A[2*i+k*lda];
                  ai = A[2*i+k*lda+1];
                  br = B[j*ldb+2*k];
                  bi = B[j*ldb+2*k+1];
               #elif defined(NoTransA) && !defined(NoTransB)
                  ar = A[2*i+k*lda] ;
                  ai = A[2*i+k*lda+1];
                  br = B[2*j+k*ldb];
                  bi = B[2*j+k*ldb+1];
               #elif !defined(NoTransA) && defined(NoTransB)
                  ar = A[i*lda+k*2];
                  ai = A[i*lda+k*2+1];
                  br = B[j*ldb+k*2];
                  bi = B[j*ldb+k*2+1];
               #elif !defined(NoTransA) && !defined(NoTransB)
                  ar = A[i*lda+k*2];
                  ai = A[i*lda+k*2+1];
                  br = B[2*j+k*ldb];
                  bi = B[2*j+k*ldb+1];
               #endif
               #ifdef ConjTransA
                  ai = -ai;
               #endif
               #ifdef ConjTransB
                  bi = -bi;
               #endif
               cr += ar * br - ai * bi;
               ci += ar * bi + ai * br;
            }
/*
 *          Scale by alpha
 */
            ar = *alpha;
            ai = alpha[1];
            br = cr;
            bi = ci;
            cr =  br * ar;
            ci =  bi * ar;
            cr -= bi * ai;
            ci += br * ai;
/*
 *          Scale C by beta
 */
            br = *beta;
            bi = beta[1];
            ar = C[2*i+j*ldc];
            ai = C[2*i+j*ldc+1];
            C[2*i+j*ldc]   = ar*br - ai * bi;
            C[2*i+j*ldc+1] = ai*br + ar * bi;
/*
 *          Store answer back to C
 */
            C[2*i+j*ldc]   += cr;
            C[2*i+j*ldc+1] += ci;

         }
      }
      #if csA == 1 && csB == 1
         free(aa);
         free(bb);
      #endif
   #endif
}
int mmtst(void)
{
   char fnam[80];
#if defined(LDA) && LDA != 0
      const int lda=LDA;
#else
      const int lda=2*LDA2;
#endif
#if defined(LDB) && LDB != 0
   const int ldb=LDB;
#else
   const int ldb=2*LDB2;
#endif
#if defined(LDC) && LDC != 0
   const int ldc=LDC;
#else
   const int ldc=2*LDC2;
#endif
   int nA, nB;
   #ifdef TCPLX
      int inca, incb, incc;
      const TYPE one=1.0, none=(-1.0);
      #if (ALPHA == 1)
         TYPE alpha[2] = {1.0, 0.0};
      #elif (ALPHA == -1)
         TYPE alpha[2] = {-1.0, 0.0};
      #else
         TYPE alpha[2] = {2.3, 0.0};
      #endif
      #if (BETA == 1)
         TYPE beta[2] = {1.0, 0.0};
      #elif (BETA == -1)
         TYPE beta[2] = {-1.0, 0.0};
      #elif (BETA == 0)
         TYPE beta[2] = {0.0, 0.0};
      #else
         TYPE beta[2] = {1.3, 0.0};
      #endif
   #else
      #ifdef ALPHA
         TYPE alpha=ALPHA;
      #else
         TYPE alpha=1.0;
      #endif
      #ifdef BETA
         TYPE beta=BETA;
      #else
         TYPE beta=1.0;
      #endif
   #endif
   const TYPE rone=1.0, rnone=(-1.0);
   void *va=NULL, *vb=NULL, *vc=NULL;
   TYPE *C0, *C1, *A, *B;
   TYPE diff, tmp;
   int i, j, k, n, nerr;
   int M=MB, N=NB, K=KB;
   TYPE ErrBound;

   if (!M) M = MB0;
   if (!N) N = NB0;
   if (!K) K = KB0;
   #ifdef TREAL
      ErrBound = 2.0 * (Mabs(alpha) * 2.0*K*EPS + Mabs(beta) * EPS) + EPS;
   #else
      diff = Mabs(*alpha) + Mabs(alpha[1]);
      tmp = Mabs(*beta) + Mabs(beta[1]);
      ErrBound =  2.0 * (diff*8.0*K*EPS + tmp*EPS) + EPS;
   #endif
   #ifdef NoTransA
      nA = K;
   #else
      nA = M;
   #endif
   #ifdef NoTransB
      nB = N;
   #else
      nB = K;
   #endif
   #ifdef TCPLX
      inca = lda*nA;
      incb = ldb*nB;
   #endif
   #ifdef ATL_MinMMAlign
      va = malloc(ATL_MinMMAlign + lda*nA*ATL_sizeof);
      vb = malloc(ATL_MinMMAlign + ldb*nB*ATL_sizeof);
      vc = C0 = malloc(2*ldc*N*ATL_sizeof);
      assert(va && vb && C0);
      A = (TYPE *) ( ( ((size_t) va)/ATL_MinMMAlign ) * ATL_MinMMAlign
                     + ATL_MinMMAlign );
      B = (TYPE *) ( ( ((size_t) vb)/ATL_MinMMAlign ) * ATL_MinMMAlign
                     + ATL_MinMMAlign );
   #else
      C0 = vc = malloc( (2*ldc*N + lda*nA + ldb*nB) * ATL_sizeof);
      assert(vc);
      A = C1 + (ldc * N SHIFT);
      B = A + (lda * nA SHIFT);
   #endif
   C1 = C0 + (ldc * N SHIFT);
   for (n=lda*nA SHIFT, i=0; i < n; i++) A[i] = dumb_rand();
   for (n=ldb*nB SHIFT, i=0; i < n; i++) B[i] = dumb_rand();
   for (n=ldc*N SHIFT, i=0; i < n; i++) C0[i] = C1[i] = dumb_rand();
   tst_mm(M, N, K, alpha, A, lda, B, ldb, beta, C0, ldc);
   NBmm(M, N, K, alpha, A, lda, B, ldb, beta, C1, ldc);
   nerr = 0;
   for (j=0; j < N; j++)
   {
      for (i=0; i < M SHIFT; i++)
      {
         k = i + j*(ldc SHIFT);
         diff = C0[k] - C1[k];
         if (diff < 0.0) diff = -diff;
         if (diff > ErrBound)
         {
            fprintf(stderr, "C(%d,%d) : expected=%f, got=%f\n",
                    i, j, C0[k], C1[k]);
            nerr++;
         }
         else if (C1[k] != C1[k])   /* test for NaNs in test answer */
         {
            fprintf(stderr, "C(%d,%d) : expected=%f, got=%f\n",
                    i, j, C0[k], C1[k]);
            nerr++;
         }
      }
   }
   free(vc);
   if (va) free(va);
   if (vb) free(vb);
   return(nerr);
}

int main()
{
   int ierr;
   ierr = mmtst();
   if (!ierr) fprintf(stdout, "PASSED TEST\n");
   return(ierr);
}

