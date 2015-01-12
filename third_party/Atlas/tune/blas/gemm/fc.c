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
#define Mabs(x) ( (x) < 0 ? (x) * -1 : (x) )
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

void SortDoubles(int N, double *X)
/*
 * Simple selection sort on X
 */
{
   double small;
   int ismall, i, j;
   for (i=0; i < N; i++)
   {
      ismall = i;
      small = X[i];
      for (j=i+1; j < N; j++)
      {
         if (X[j] < small)
         {
            ismall = j;
            small = X[j];
         }
      }
     if (ismall != i)
      {
         X[ismall] = X[i];
         X[i] = small;
      }
   }
}

#ifndef NSAMPLE
   #define NSAMPLE 3
#endif
void time_mm(char *fnam0)
{
   char fnam[80];
#if defined(LDA) && LDA != 0
      const int lda=LDA;
#else
      const int lda=LDA2;
#endif
#if defined(LDB) && LDB != 0
   const int ldb=LDB;
#else
   const int ldb=LDB2;
#endif
#if defined(LDC) && LDC != 0
   const int ldc=LDC;
#else
   const int ldc=LDC2;
#endif
   int nA, nB;
   int restarts=0;
   int i, j, k, reps, len;
   int incA, incB, incC, nmov;
   double t0, t1, mflop;
   double times[NSAMPLE];
   void *va=NULL, *vb=NULL, *vc=NULL, *vp=NULL;
   TYPE *A, *B, *C, *a, *b, *c, *stA, *stB, *stC;
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
   FILE *fpout;
   double time00();

   #ifdef NoTransA
      nA = KB;
   #else
      nA = MB;
   #endif
   #ifdef NoTransB
      nB = NB;
   #else
      nB = KB;
   #endif
   #ifdef ATL_nkflop
      #ifdef TCPLX
         t0 = 8.0*MB;
      #else
         t0 = 2.0*MB;
      #endif
      t0 *= NB;
      t0 *= KB;
      t1 = ATL_nkflop * 1000.0;
      reps = t1 / t0;
   #else
      reps = REPS * ( (40.0/MB) * (40.0/NB) * (40.0/KB) );
      #ifdef TCPLX
         reps /= 4;
      #endif
   #endif
   if (reps < 1) reps = 1;

   incA = incB = incC = nmov = 0;
   #ifdef MoveA
      incA = lda*nA;
      nmov++;
   #else
      va = malloc(128+ATL_sizeof*lda*nA);
      assert(va);
      a = A = (void*) ( 128 + ((((size_t)va)>>7)<<7) );
      stA = A + (lda*nA SHIFT);
   #endif
   #ifdef MoveB
      incB = ldb*nB;
      nmov++;
   #else
      vb = malloc(128+ATL_sizeof*ldb*nB);
      assert(vb);
      b = B = (void*) ( 128 + ((((size_t)vb)>>7)<<7) );
      stB = B + (ldb*nB SHIFT);
   #endif
   #ifdef MoveC
      assert(ldc == MB);
      incC = ldc*NB;
      nmov++;
   #else
      vc = malloc(128+ATL_sizeof*ldc*NB);
      assert(vc);
      c = C = (void*) ( 128 + ((((size_t)vc)>>7)<<7) );
      stC = C + (ldc * NB SHIFT);
   #endif

/*   sprintf(fnam, "res/%s", Mstr(NBmm)); */
   if (fnam0) strcpy(fnam, fnam0);
   else
      sprintf(fnam,
              "res/%c%smm%s%s%d_%dx%dx%d_%dx%dx%d_%dx%dx%d%s%s_%dx%d_%d_pf%d",
              PRE, Mstr(LOOPO), Mstr(TA), Mstr(TB), NB, MB0, NB0, KB0,
              LDA, LDB, LDC, MU, NU, KU, Mstr(ALPHAnam), Mstr(BETAnam),
              MULADD, LAT, CLEANUP, PREFA);
   fpout = fopen(fnam, "w");
   #if 1 /* def ATL_NEWTIME */
      #ifdef WALL
         fprintf(fpout, "%d %d\n", NSAMPLE, 1);
      #else
         fprintf(fpout, "%d %d\n", NSAMPLE, 0);
      #endif
   #endif

   if (nmov != 0)  /* need to allocate space */
   {
      len = (1.2*L2SIZE) / ATL_sizeof;
      /* total moving length */
      i = Mmin(incA, MB*KB) + Mmin(incB, NB*KB) + Mmin(incC, MB*NB);
      j = (len+i-1) / i;       /* number of reps to cause flush */
      len = (incA+incB+incC) * j;
      vp = malloc(128+len*ATL_sizeof);
      assert(vp);
      #ifdef TCPLX
         inca = incA;
         incb = incB;
         incc = incC;
         incA *= 2;
         incB *= 2;
         incC *= 2;
      #endif
      a = (void*) ( 128 + ((((size_t)vp)>>7)<<7) );
      if (incA)
      {
         A = a;
         stA = a + incA * j;
      }
      if (incB)
      {
         b = B = a + j * incA;
         stB = b + j * incB;
         if (incC)
         {
            c = C = stB;
            stC = C + j * incC;
         }
      }
      else if (incC)
      {
         c = C = a + j * incA;
         stC = C + j * incC;
      }
      a = A;
   }
   #ifdef TCPLX
   else
   {
         inca = incA;
         incb = incB;
         incc = incC;
         incA *= 2;
         incB *= 2;
         incC *= 2;
   }
   #endif

   while (a != stA) *a++ = dumb_rand();  a = A;
   while (b != stB) *b++ = dumb_rand();  b = B;
   while (c != stC) *c++ = 0.0;          c = C;

   for (i=0; i != NSAMPLE; i++)
   {
      t0 = time00();
      for (k=reps; k; k--)
      {
         #ifdef NEWMM
            NBmm(MB, NB, KB, a, b, c);
         #else
            NBmm(MB, NB, KB, alpha, a, lda, b, ldb, beta, c, ldc);
         #endif
         #ifdef MoveA
            a += incA;
            if (a == stA) a = A;
         #endif
         #ifdef MoveB
            b += incB;
            if (b == stB) b = B;
         #endif
         #ifdef MoveC
            c += incC;
            if (c == stC)
            {
               c = C;
            #ifdef TREAL
               if (beta != 0.0) beta = 1.0 / beta;
               if (alpha != 0.0) alpha = -alpha;
            #else
               if (*beta != 0.0) *beta = 1.0 / (*beta);
               if (*alpha != 0.0) *alpha = -(*alpha);
            #endif
            }
         #else
            #ifdef TREAL
               if (beta != 0.0) beta = 1.0 / beta;
               if (alpha != 0.0) alpha = -alpha;
            #else
               if (*beta != 0.0) *beta = 1.0 / (*beta);
               if (*alpha != 0.0) *alpha = -(*alpha);
            #endif
         #endif
      }
      t1 = time00() - t0;
/*
 *    Workaround for mystery prob Windows/icc, where first return val is
 *    always 0
 */
      if (t1 >= 0.005)
      {
         mflop = ( (((2.0*MB)*NB)*KB)*reps ) / (t1 * 1000000.0);
         #ifdef TCPLX
            mflop *= 4.0;
         #endif
         fprintf(stderr,
   "%cNB=%d, ld=%d,%d,%d, mu=%d, nu=%d, ku=%d, lat=%d, pf=%d: time=%.3f, mflop=%.2f\n",
                 PRE, NB, lda, ldb, ldc, MU, NU, KU, LAT, PREFA, t1, mflop);
         #if 1 /* def ATL_NEWTIME */
            if (fpout) fprintf(fpout, "%le\n",mflop);
         #else
            if (fpout) fprintf(fpout, "%lf\n",mflop);
         #endif
         times[i] = t1;
      }
      else
      {
         if (++restarts > 5)
         {
            fclose(fpout);
            remove(fnam);
            fprintf(stderr, "Too many zero-time values, dying\n");
            exit(-1);
         }
         fprintf(stderr, "Near-zero time %e rejected\n", t0);
         i--;
      }
   }
   SortDoubles(NSAMPLE, times);
   #ifdef WALL
      t1 = times[0];
   #else
      t1 = times[NSAMPLE/2];
   #endif
   mflop = ( (((2.0*MB)*NB)*KB)*reps ) / (t1 * 1000000.0);
   #ifdef TCPLX
      mflop *= 4.0;
   #endif
   fprintf(stdout, "%cNB=%d, time=%.3f, mflop=%.2f\n", PRE, NB, t1, mflop);
   if (fpout) fclose(fpout);
   if (vp) free(vp);
   if (va) free(va);
   if (vb) free(vb);
   if (vc) free(vc);
}

int main(int nargs, char **args)
{
   char *fnam;
   if (nargs > 1) fnam = args[1];
   else fnam = NULL;
   time_mm(fnam);
   exit(0);
}
