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
#ifdef ATL_THREADED_CE
   #define ATL_USEPTHREADS
#endif
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))

#define dumb_seed(iseed_) srand(iseed_)
#ifndef RAND_MAX  /* rather dangerous non-ansi workaround */
   #define RAND_MAX ((unsigned long)(1<<30))
#endif
#define dumb_rand() ( 0.5 - ((double)rand())/((double)RAND_MAX) )


int FoundCE;
int CompCE=0;
double time00();
void Mjoin(PATL,FindCE_mm)(enum ATLAS_TRANS TA, enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           const TYPE *C, const int ldc);
#ifdef ATL_THREADED_CE
   #include "atlas_threads.h"
   void Mjoin(PATL,tgemm)(enum ATLAS_TRANS TA, enum ATLAS_TRANS TB,
                          const int M, const int N, const int K,
                          const SCALAR alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const SCALAR beta,
                          const TYPE *C, const int ldc);
#endif

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
      A += lda;
   }
}


double mmcase(enum ATLAS_TRANS TA, enum ATLAS_TRANS TB, int M, int N, int K,
              SCALAR alpha, SCALAR beta, int CE)
{
   char cTA, cTB;
   int nL2 = (1.3*L2SIZE)/sizeof(int);
   int *iL2=NULL, j=0, i, n;
   int lda, ldb, ldc=M;
   void *vA, *vB, *vC;
   TYPE *A, *B, *C;
   double t0, t1;

/*
 * Make sure CE will be different than 0, if CE is not 0
 */
   if (CE)
   {
      FoundCE = CE;
      CompCE = 1;
      Mjoin(PATL,FindCE_mm)(TA, TB, M, N, K, alpha, NULL, lda, NULL, ldb, beta,
                            NULL, ldc);
      if (CompCE < KB)
         return(-2.0);
   }
   CompCE = 0;
/*
 * Blow off cache flushing if C is already twice as large as L2
 */
   if (M*N*sizeof(TYPE) >= 2*L2SIZE) nL2 = 0;
   if (nL2) iL2 = malloc(nL2 * sizeof(int));
   vA = malloc(ATL_Cachelen+ATL_MulBySize(M)*K);
   vB = malloc(ATL_Cachelen+ATL_MulBySize(N)*K);
   vC = malloc(ATL_Cachelen+ATL_MulBySize(M)*N);
   if (!vA || !vB || !vC || (nL2 && !iL2))
   {
      if (iL2) free(iL2);
      if (vA) free(vA);
      if (vB) free(vB);
      if (vC) free(vC);
      return(-1.0);
   }
   ATL_assert(vA && vB && vC);
   if (nL2) ATL_assert(iL2);
   A = ATL_AlignPtr(vA);
   B = ATL_AlignPtr(vB);
   C = ATL_AlignPtr(vC);
   if (TA == AtlasNoTrans)
   {
      lda = M;
      matgen(M, K, A, lda, 271*M*K);
      cTA = 'N';
   }
   else
   {
      lda = K;
      matgen(K, M, A, lda, 271*M*K);
      if (TA == AtlasTrans) cTA = 'T';
      else cTA = 'C';
   }
   if (TB == AtlasNoTrans)
   {
      ldb = K;
      matgen(K, N, B, ldb, 99876*N*K);
      cTB = 'N';
   }
   else
   {
      ldb = N;
      matgen(N, K, B, ldb, 99876*N*K);
      if (TB == AtlasTrans) cTB = 'T';
      else cTB = 'C';
   }
   matgen(M, N, C, M, 81*M*N);

/*
 * invalidate L2 cache
 */
   if (nL2)
   {
      for (i=0; i != nL2; i++) iL2[i] = 0.0;
      for (i=0; i != nL2; i++) j += iL2[i];
   }

   FoundCE = CE;

   t0 = time00();
#ifdef ATL_THREADED_CE
   Mjoin(PATL,tgemm)(TA, TB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
#else
   Mjoin(PATL,FindCE_mm)(TA, TB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
#endif
   t1 = time00() - t0;

   if (iL2) free(iL2);
   free(vA);
   free(vB);
   free(vC);
   return(t1);
}

double mmcases(int nsamples, enum ATLAS_TRANS TA, enum ATLAS_TRANS TB,
               int M, int N, int K, SCALAR alpha, SCALAR beta, int CE)
{
   double d=0.0;
   int i;

   for (i=0; i < nsamples; i++)
      d += mmcase(TA, TB, M, N, K, alpha, beta, CE);

   return(d/(double)nsamples);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -n <N> -m <M> -k <K> -f <include filename>\n",
           nam);
   exit(-1);
}
void GetFlags(int nargs, char *args[], enum ATLAS_TRANS *TA, enum ATLAS_TRANS *TB,
              int *M, int *N, int *K, TYPE *alpha, TYPE *beta,
              int *CE0, int *CEN, int *incCE, FILE **fpout)
{
   int i, n;
   double mf;

   *TB = *TA = AtlasNoTrans;
   *TA = AtlasTrans;
   *M = MB+MB;
   #ifdef ATL_THREADED_CE
      *M *= Mmin(8, ATL_NTHREADS);
      *K = (32*1024*1024)/((sizeof(TYPE) SHIFT)* (*M));
   #else
      *K = (8*1024*1024)/((sizeof(TYPE) SHIFT)* (*M));
   #endif
   #ifdef ATL_nkflop
      mf = 2.0*ATL_nkflop * 1000.0;
   #else
      mf = 2500000.0;
      #if defined(SREAL) || defined(SCPLX)
         mf *= 4;
      #else
         mf *= 2;
      #endif
   #endif
   *N = mf/(2.0 * *M * *K);
   if (*N < 2000)
      *N = 2000;
   *N = (*N/NB)*NB;
   *CEN = *CE0 = -1;
   *alpha = *beta = 1.0;
   #ifdef TCPLX
      beta[1] = alpha[1] = 0.0;
   #endif
   *fpout = NULL;

   for (i=1; i < nargs; i++)
   {
      if (*args[i] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'C':
         *CE0 = atoi(args[++i]);
         *CEN = atoi(args[++i]);
         *incCE = atoi(args[++i]);
         break;
      case 'f':
         *fpout = fopen(args[++i], "w");
         ATL_assert(fpout);
         break;
      case 'n':
         *N = atoi(args[++i]);
         break;
      case 'k':
         *K = atoi(args[++i]);
         break;
      case 'm':
         *M = atoi(args[++i]);
         break;
      default :
         PrintUsage(args[0]);
      }
   }

   if (*N != -1)
   {
      if (*M == -1) *M = *N;
      if (*K == -1) *K = *N;
   }
   if (*CE0 == -1)
   {
      n = ATL_MulBySize(NBNB);
      for (i=1; i < n; i <<= 1);
      i = i / 1024;
      if (!i) i = 1;
      *CE0 = i;
      *CEN = (2*L2SIZE) / 1024;
      if (*CEN > 8*1024) *CEN = 8*1024;
      *incCE = -2;
   }
}

double tloop(enum ATLAS_TRANS TA, enum ATLAS_TRANS TB, int M, int N, int K,
             SCALAR alpha, SCALAR beta, int CE00, int CE0, int CEN, int incCE,
             int *CEout, double *mflop)
{
   char cTA, cTB;
   int i, CE;
   double t1, mf, mmf=0.0, mf0;

   if (TA == AtlasNoTrans) cTA = 'N';
   else if (TA == AtlasTrans) cTA = 'T';
   else cTA = 'C';
   if (TB == AtlasNoTrans) cTB = 'N';
   else if (TB == AtlasTrans) cTB = 'T';
   else cTB = 'C';

   i = CE = CE00;
   do
   {
      t1 = mmcase(TA, TB, M, N, K, alpha, beta, i*1024);
      if (t1 == -2.0)
         mf = 0.0;
      else
      {
         ATL_assert(t1 > 0.0);
         #ifdef TREAL
            mf = (((2.0*M)*N)*K) / (1000000.0 * t1);
         #else
            mf = (((8.0*M)*N)*K) / (1000000.0 * t1);
         #endif
      }
      if (mf > mmf)
      {
         CE = i;
         mmf = mf;
         if (CE00 == i) mf0 = mf;
      }
      #ifdef TREAL
         fprintf(stdout,
                 " %c   %c %7d %7d %7d  %6.2f  %6.2f  %9d %10.3f %9.2f\n",
                 cTA, cTB, M, N, K, alpha, beta, i, t1, mf);
      #else
         fprintf(stdout,
            " %c  %c %6d %6d %6d %5.1f %5.1f %5.1f %5.1f %9d %9.3f %8.2f\n",
                 cTA, cTB, M, N, K, *alpha, alpha[1], *beta, beta[1],
                 i, t1, mf);
      #endif
      if (i == CE00 && CE0 != CE00) i = CE0;
      else if (incCE == -2) i <<= 1;
      else i += incCE;
   }
   while (i <= CEN);
   *CEout = CE;
   *mflop = mmf;
   return(mf0);
}

void refineCE(enum ATLAS_TRANS TA, enum ATLAS_TRANS TB, int M, int N, int K,
              SCALAR alpha, SCALAR beta, int prevCE, int bestCE, int nextCE,
              double bestMF, int *CEout, double *mflop)
{

   int newCE, CE;
   double mf;
/*
 * See if true max is less than one we have so far
 */
   if (bestCE != prevCE)
   {
      newCE = bestCE - (bestCE - prevCE)/2;
      if (bestCE - newCE <= 1)  /* return if we've found CE within 1K */
      {
         *CEout = bestCE;
         *mflop = bestMF;
         return;
      }
      tloop(TA, TB, M, N, K, alpha, beta, newCE, newCE, newCE, 1, &CE, &mf);
      if (mf > bestMF)
      {
         refineCE(TA, TB, M, N, K, alpha, beta, prevCE, newCE, bestCE, mf,
                  CEout, mflop);
         return;
      }
   }
/*
 * See if best CE is greater than what has been tried so far
 */
   if (bestCE != nextCE)
   {
      newCE = bestCE + (nextCE - bestCE)/2;
      if (newCE - bestCE <= 1)  /* return if we've found CE within 1K */
      {
         *CEout = bestCE;
         *mflop = bestMF;
         return;
      }
      tloop(TA, TB, M, N, K, alpha, beta, newCE, newCE, newCE, 1, &CE, &mf);
      if (mf >= bestMF)
      {
         refineCE(TA, TB, M, N, K, alpha, beta, bestCE, newCE, nextCE, mf,
                  CEout, mflop);
         return;
      }
   }
   *CEout = bestCE;
   *mflop = bestMF;
   return;
}
int main(int nargs, char *args[])
{
   enum ATLAS_TRANS TA, TB;
   int i, M, N, K, CE0, CEN, incCE, CE=0, nextCE, prevCE;
   #ifdef TREAL
      TYPE alpha, beta;
   #else
      TYPE alpha[2], beta[2];
   #endif
   char *sp;
   double mf, mf0;
   FILE *fpout;

   GetFlags(nargs, args, &TA, &TB, &M, &N, &K, SADD alpha, SADD beta, &CE0, &CEN, &incCE,
            &fpout);
   if (M == -1)
   {
/*
 *    Blocking for very large caches problematic due to line conflicts, so no
 *    use going above 1MB or so . . .
 */
      K = 1024*1024;
      if (L2SIZE < K) K = L2SIZE;
      K /= ATL_sizeof;
      K = 1.15*((K-NBNB)/(2.0*NB));
      K = ((K+NB-1)/NB)*NB;
   }

   #ifdef TREAL
      fprintf(stdout, "TA  TB       M       N       K   alpha    beta  CacheEdge       TIME    MFLOPS\n");
      fprintf(stdout, "==  ==  ======  ======  ======  ======  ======  =========  =========  ========\n\n");
   #else
      fprintf(stdout, "TA TB      M      N      K    alpha       beta     CacheEdge      TIME   MFLOPS\n");
      fprintf(stdout, "== == ====== ====== ====== ===== ===== ===== ===== ========= ========= ========\n\n");
   #endif
/*
 * Determine rough flop rate, so we can see how big a problem to do
 */
   if (M == -1)
   {
      #ifdef ATL_nkflop
         mf = ATL_nkflop * 1000.0;
      #else
         #ifdef TREAL
            mf = mmcase(TA, TB, 450, 450, 450, alpha, beta, 0);
            mf = (2.0*450.0*450.0*450.0) / mf;
         #else
            mf = mmcase(TA, TB, 200, 200, 200, alpha, beta, 0);
            mf = (8.0*200.0*200.0*200.0) / mf;
         #endif
      #endif
      mf = (mf*4.0) / K;
      for (i=8*NB; i*i < mf; i += NB);
      M = N = i;
   }
/*
 * preload instructions, and ensure we can allocate the memory
 */
   do
   {
      mf0 = mmcase(TA, TB, M, N, K, alpha, beta, 0);
      if (mf0 <= 0.0)
      {
         if (K > (Mmax(M,N)<<3)) K >>= 1;
         if (M > N) M >>= 1;
         else N >>= 1;
      }
   }
   while(mf0 <= 0.0);
   mf0 = tloop(TA, TB, M, N, K, alpha, beta, 0, CE0, CEN, incCE, &CE, &mf);
/*
 * If best CacheEdge not 3% better than no cachedge,
 * its probably clock resolution
 * Go ahead and accept any CacheEdge that gets the same performance, though,
 * since it will use less memory.
 */
   if (mf >= mf0)
   {
      fprintf(stdout, "\nInitial CE=%dKB, mflop=%.2f\n\n", CE, mf);
/*
 *    If CacheEdge not already at extremum, refine it
 */
      if (CE != 0 && CE != CEN && CE != CE0)
      {
         if (incCE == -2)
         {
            prevCE = CE / 2;
            nextCE = CE * 2;
         }
         else
         {
            prevCE = CE - incCE;
            nextCE = CE+incCE;
         }
         if (prevCE < 0) prevCE = 0;
         if (nextCE > CEN) nextCE = CEN;
         refineCE(TA, TB, M,N,K, alpha, beta, prevCE, CE, nextCE, mf, &CE, &mf);
      }
      fprintf(stdout, "\nBest CE=%dKB, mflop=%.2f\n", CE, mf);
   }
   else
   {
      fprintf(stdout,
              "Best CE=%dKB, mflop=%.2f, might as well set to 4MB (%.2f)\n",
              CE, mf, mf0);
      CE = 4096;
   }
   if (fpout)
   {
#ifdef ATL_JITcp
      if (CE)
      {
         FoundCE = CE;
         CompCE = 1;
         Mjoin(PATL,FindCE_mm)(TA, TB, M, N, K, alpha, NULL, 1, NULL, 1, beta,
                               NULL, 1);
      }
      else CompCE = 0;
   #ifdef DCPLX
      sp = "ZD";
   #else
      sp = "CS";
   #endif
      fprintf(fpout, "#ifndef ATLAS_%sNKB_H\n", sp);
      fprintf(fpout, "   #define ATLAS_%sNKB_H\n", sp);
      fprintf(fpout, "   #define ATL_%sNKB %d\n", sp, CompCE/KB);
      fprintf(fpout, "#endif\n");
#else
      fprintf(fpout, "#ifndef ATLAS_CACHEEDGE_H\n");
      fprintf(fpout, "   #define ATLAS_CACHEEDGE_H\n");
      /* if (mf > 1.04*mf0) */
         fprintf(fpout, "   #define CacheEdge %d\n", CE*1024);
      fprintf(fpout, "#endif\n");
#endif
      fclose(fpout);
   }
   return(0);
}
