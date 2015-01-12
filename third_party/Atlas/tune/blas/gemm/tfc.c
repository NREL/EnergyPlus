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
#include <math.h>
#include <limits.h>
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include "atlas_f77.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))

#define dumb_seed(iseed_) srand(iseed_)
#ifndef RAND_MAX  /* rather dangerous non-ansi workaround */
   #define RAND_MAX ((unsigned long)(1<<30))
#endif
#define dumb_rand() ( 0.5 - ((double)rand())/((double)RAND_MAX) )


int ___main(){return(-1);}   /* this nonsense helps */
int __main(){return(-1);}    /* when you link with certain */
int MAIN__(){return(-1);}    /* fortran compilers */
int _MAIN_(){return(-1);}

double time00();
#define nshape 5
enum ATLAS_MATSHAPE {AtlasM_NB=0, AtlasN_NB=1, AtlasMN_NB=2, AtlasK_NB=3,
                     Atlas0_NB=4, AtlasMN_REST};

#define DENMAT 175
#define MAXALLOC (8*1024*1024*8)

#ifdef ATL_DeclareSlens
F77_INTEGER ATL_Slen1, ATL_Slen2;
#endif

void Mjoin(PATL,small_mm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,big_mm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,dbig_mm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,sbig_mm)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
typedef void (*GEMMPTR)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
static GEMMPTR big_gemm, small_gemm;

void matgen(ATL_INT M0, ATL_INT N, TYPE *A, ATL_INT lda0, int seed)
{
   ATL_INT i, j;
   ATL_CINT M = M0 SHIFT, lda = lda0 SHIFT;

   dumb_seed(seed);
   for (j=N; j; j--)
   {
      for (i=0; i != M; i++) A[i] = dumb_rand();
      A += lda;
   }
}

int mmcase(char TA, char TB, int M, int N, int K, SCALAR alpha, SCALAR beta,
           double *mf0, double *mf1)
{
#ifdef TREAL
   char *form="%4d   %c   %c %4d %4d %4d  %5.1f  %5.1f  %6.2f %6.1f  %4.2f\n";
   #define MALPH alpha
   #define MBETA beta
#else
   #define MALPH *alpha, alpha[1]
   #define MBETA *beta, beta[1]
   char *form="%4d   %c   %c %4d %4d %4d  %5.1f %5.1f  %5.1f %5.1f  %6.2f %6.1f %4.2f\n";
#endif
   int lda, ldb, ldc, n, reps, i, j, incA, incB, incC;
   int la, lb, lc;
   double t0, t1, t2, t3, mflop;
   TYPE *A, *B, *C, *a, *b, *c, *ast, *bst, *cst;
   TYPE maxval, f1, ferr;
   static TYPE feps=0.0;
   static int itst=1;
   enum ATLAS_TRANS TAc, TBc;
   extern int ATL_bigmmOutOfMem;

   if (TA == 'n' || TA == 'N')
   {
      lda = Mmax(M,100);
      incA = la = lda * K;
      TAc = AtlasNoTrans;
   }
   else
   {
      lda = Mmax(K,100);
      incA = la = lda * M;
      TAc = AtlasTrans;
   }
   if (TB == 'n' || TB == 'N')
   {
      ldb = Mmax(K,100);
      incB = lb = ldb * N;
      TBc = AtlasNoTrans;
   }
   else
   {
      ldb = Mmax(N,100);
      incB = lb = ldb * K;
      TBc = AtlasTrans;
   }
   ldc = Mmax(M,100);
   j = ATL_DivBySize(L2SIZE);
   i = Mmax(ldc*N, j);
   i += Mmax(la, j);
   i += Mmax(lb, j);
   if (i*ATL_sizeof > MAXALLOC) return(1);

   i = la;
   j = (ATL_DivBySize(L2SIZE) / i);
   if (j < 1) j = 1;
   j *= i;
   la = j;
   a = A = malloc(j*ATL_sizeof);
   if (a == NULL) return(1);
   ast = A + (j SHIFT);
   matgen(j, 1, A, j, K*1112);

   i = lb;
   j = (ATL_DivBySize(L2SIZE) / i);
   if (j < 1) j = 1;
   j *= i;
   lb = j;
   b = B = malloc(j*ATL_sizeof);
   if (b == NULL)
   {
      free(A);
      return(2);
   }
   matgen(j, 1, B, j, N*1287);
   bst = B + (j SHIFT);

   incC = i = ldc * N;
   lc = j = ((ATL_DivBySize(L2SIZE) + i-1) / i)*i;
   c = C = malloc(j*ATL_sizeof);
   if (c == NULL)
   {
      free(A);
      free(B);
      return(3);
   }
   matgen(j, 1, C, j, M*N);
   cst = C + (j SHIFT);
   #ifdef TCPLX
      incA *= 2; incB *= 2; incC *= 2;
   #endif
/*
 * preload instructions from disk
 */
   small_gemm(TAc, TBc, 80, 80, 80, alpha, a, 80, b, 80, beta, c, 80);
   big_gemm(TAc, TBc, 80, 80, 80, alpha, a, 80, b, 80, beta, c, 80);
   matgen(la, 1, A, la, K*1112);
   matgen(lb, 1, B, lb, N*1287);
   matgen(lc, 1, C, lc, M*N);
/*
 * Insist we have at least as many flops as doing a matmul of order DENMAT
 */
   t1 = (((double) M) * N) * K;
   #ifdef ATL_nkflop
      t0 = 1000.0 * ATL_nkflop;
      t1 *= 2.0;
   #else
      t0 = DENMAT;
      t0 *= t0*t0;
   #endif
   t0 /= t1;
   reps = t0 + 0.9;
   if (!reps) reps = 1;

   i = reps;
   t0 = time00();
   do
   {
      small_gemm(TAc, TBc, M, N, K, alpha, a, lda, b, ldb, beta, c, ldc);
      a += incA;
      b += incB;
      c += incC;
      if (a == ast) a = A;
      if (b == bst) b = B;
      if (c == cst) c = C;
   }
   while (--i);
   t1 = time00() - t0;
   if (t1 <= 0.0) mflop = t1 = 0.0;
   else mflop = ( ((2.0*M)*N)*K*reps ) / (t1*1000000.0);
   #ifdef TCPLX
      mflop *= 4.0;
   #endif
   *mf0 = mflop;
   printf(form, itst, TA, TB, M, N, K, MALPH, MBETA, t1, mflop, 1.0);

   a = A;  b = B;  c = C;
   matgen(la, 1, A, la, K*1112);
   matgen(lb, 1, B, lb, N*1287);
   matgen(lc, 1, C, lc, M*N);

   i = reps;
   t0 = time00();
   do
   {
      big_gemm(TAc, TBc, M, N, K, alpha, a, lda, b, ldb, beta, c, ldc);
      if (ATL_bigmmOutOfMem)
      {
         free(A);
         free(B);
         free(C);
         ATL_bigmmOutOfMem = 0;
         return(4);
      }
      a += incA;
      b += incB;
      c += incC;
      if (a == ast) a = A;
      if (b == bst) b = B;
      if (c == cst) c = C;
   }
   while (--i);
   t2 = time00() - t0;
   if (t2 <= 0.0) t2 = mflop = 0.0;
   else mflop = ( ((2.0*M)*N)*K*reps ) / (t2*1000000.0);
   #ifdef TCPLX
      mflop *= 4.0;
   #endif
   *mf1 = mflop;

   if (t1 == t2) t3 = 1.0;
   else if (t2 != 0.0) t3 = t1/t2;
   else t3 = 0.0;
   printf(form, itst++, TA, TB, M, N, K, MALPH, MBETA, t2, mflop, t3);
   free(A);
   free(B);
   free(C);
   return(0);
}

void GetDims(enum ATLAS_MATSHAPE shape, int n, int nb, int *M, int *N, int *K)
{
   *M = *N = *K = n;
   switch(shape)
   {
   case AtlasM_NB:
      *M = nb;
      break;
   case AtlasN_NB:
      *N = nb;
      break;
   case AtlasMN_NB:
      *M = *N = nb;
      break;
   case AtlasK_NB:
      *K = nb;
      break;
   case AtlasMN_REST:  /* restricted M & N, but basically square */
      if (n > 6*nb)
         *M = *N = 6*nb;
      break;
   case Atlas0_NB:
     break;
   default:
      fprintf(stderr, "SHAPE=%d\n", shape);
      exit(-1);
   }
}

void SortPoints(int N, int *Ns, double *mfs0, double *mfs1)
/*
 * simple selection sort for data points, sorting on Ns
 */
{
   int i, j, itmp;
   double tmp;

   for (i=0; i != N; i++)
   {
      for (j=i+1; j < N; j++)
      {
         if (Ns[j] < Ns[i])
         {
            itmp = Ns[i];
            Ns[i] = Ns[j];
            Ns[j] = itmp;

            tmp = mfs0[i];
            mfs0[i] = mfs0[j];
            mfs0[j] = tmp;

            tmp = mfs1[i];
            mfs1[i] = mfs1[j];
            mfs1[j] = tmp;
         }
      }
   }
}

unsigned long PredictNcross(enum ATLAS_MATSHAPE shape, int nb, int N, int *Ns,
                            double *mfs0, double *mfs1)
{
   double slope0, slope1, tmp;
   int m, n, k;
   unsigned long NN;

   SortPoints(N, Ns, mfs0, mfs1);
   slope0 = mfs0[N-1] - mfs0[0] / (double) (Ns[N-1] - Ns[0]);
   slope1 = mfs1[N-1] - mfs1[0] / (double) (Ns[N-1] - Ns[0]);
   if (slope1 < slope0) return(LONG_MAX);
   tmp = (mfs1[N-1] - mfs0[N-1]) / (slope0 - slope1);
   NN = Ns[N-1] + tmp;
   GetDims(shape, NN, nb, &m, &n, &k);
   return(m*n*k);
}

void *GetLongerVector(int N, int chunk, int size, void *v0)
/*
 * Allocates new vector of size N+chunk, copies old v0, and frees it
 * RETURNS: ptr to new expanded vector
 */
{
   void *v;
   double *dv, *dv0;
   int *iv, *iv0;
   int i, n = N + chunk;

   v = malloc(size*n);
   assert(v);

   if (size == sizeof(int))
   {
      iv = v; iv0 = v0;
      for (i=0; i < N; i++)
         iv[i] = iv0[i];
   }
   else
   {
      assert(size == sizeof(double));
      dv = v; dv0 = v0;
      for (i=0; i < N; i++)
         dv[i] = dv0[i];
   }
   if (v0) free(v0);
   return(v);
}

#define CHUNKSIZE 128
unsigned long tloop(enum ATLAS_MATSHAPE shape, char TA, char TB,
                    int nb, SCALAR alpha, SCALAR beta)
{
   int smallN = 10, bigN = 750, stepN=10, MinDist=5;
   int i, j, n, n0, nn, M, N, K, Ncross=0;
   long MNK=0;
   double *mfs0, *mfs1;
   int *Ns, len=0;
   double mf0, mf1, tmp;

   mfs0 = malloc(CHUNKSIZE*sizeof(double));
   mfs1 = malloc(CHUNKSIZE*sizeof(double));
   Ns = malloc(CHUNKSIZE*sizeof(int));
   assert(mfs0 && mfs1 && Ns);
   len = CHUNKSIZE;
#ifdef TREAL
   printf("\nTEST  TA  TB    M    N    K  alpha   beta    Time  Mflop  SpUp\n");
   printf("====  ==  ==  ===  ===  ===  =====  =====  ======  =====  ==== \n\n");
#else
   printf("\nTEST  TA  TB    M    N    K        alpha         beta    Time  Mflop  SpUp\n");
   printf("====  ==  ==  ===  ===  ===  ===== =====  ===== =====  ======  =====  ====\n\n");
#endif
   GetDims(shape, smallN, nb, &M, &N, &K);
   assert(mmcase(TA, TB, M, N, K, alpha, beta, &mf0, &mf1) == 0);
   Ns[0] = smallN; mfs0[0] = mf0; mfs1[0] = mf1;
   if (mf0 < mf1) return(M*N*K);
/*
 * Find crossover point
 */
   n = bigN;
   i = 1;
   do
   {
      GetDims(shape, n, nb, &M, &N, &K);
      if (mmcase(TA, TB, M, N, K, alpha, beta, &mf0, &mf1))
      {
         n = n - stepN;
         bigN -= stepN;
         if (bigN < stepN) break;
      }
      else
      {
         Ns[i] = n; mfs0[i] = mf0; mfs1[i] = mf1;
         i++;
         if (mf0 < mf1) Ncross = n;
         else
         {
            if (i == len)
            {
               Ns = GetLongerVector(len, CHUNKSIZE, sizeof(int), Ns);
               mfs0 = GetLongerVector(len, CHUNKSIZE, sizeof(double), mfs0);
               mfs1 = GetLongerVector(len, CHUNKSIZE, sizeof(double), mfs1);
               len += CHUNKSIZE;
            }
            n += bigN;
         }
      }
   }
   while (!Ncross);
   if (!Ncross)
   {
      MNK = PredictNcross(shape, nb, i, Ns, mfs0, mfs1);
      free(Ns);
      free(mfs0);
      free(mfs1);
      return(MNK);
   }
/*
 * Refine Xover point using recursive halving
 */
   nn = Ns[i-1];
   n0 = Ns[i-2];
   n = n0 + (nn - n0) / 2;
   while (n - n0 > MinDist)
   {
      GetDims(shape, n, nb, &M, &N, &K);
      assert(mmcase(TA, TB, M, N, K, alpha, beta, &mf0, &mf1) == 0);
      if (mf0 < mf1) nn = n;
      else n0 = n;
      n = n0 + (nn - n0) / 2;
   }
   GetDims(shape, n, nb, &M, &N, &K);
   free(Ns);
   free(mfs0);
   free(mfs1);
   return(shape == AtlasMN_REST ? K : M*N*K);
}


void DoShapes(FILE *fpout, char TA, char TB, int nb, int N0, int NN, int incN,
              SCALAR alpha, SCALAR beta)
{
   enum ATLAS_MATSHAPE shape;
   unsigned long n, nn;
   int M, N, K;
   static char *nm[nshape] = {"M", "N", "MN", "K", "GE"};

   big_gemm    = Mjoin(PATL,big_mm);
   small_gemm  = Mjoin(PATL,small_mm);
   for (shape=AtlasM_NB; shape <= Atlas0_NB; shape++)
   {
      n = tloop(shape, TA, TB, nb, alpha, beta);
      fprintf(fpout, "#define %c%c_MNK_%s %ld\n", TA, TB, nm[shape], n);
   }
}

int main(int nargs, char *args[])
{
   char TA='n', TB='n';
   int nb, N0, NN, incN, jstop=0;
   long icross;
   #ifdef TREAL
      TYPE alpha=1.0, beta=1.0;
   #else
      TYPE alpha[2] = {-1.0, 0.0}, beta[2] = {1.0, 0.0};
   #endif
   FILE *fp;

   big_gemm    = Mjoin(PATL,big_mm);
   small_gemm  = Mjoin(PATL,small_mm);
#ifndef SM_FOUT
   if (nargs < 5)
   {
      fprintf(stderr, "usage: %s <NB> <N0> <NN> <incN> [TA TB alpha beta]\n", args[0]);
      exit(-1);
   }
   nb = atoi(args[1]);
   N0 = atoi(args[2]);
   NN = atoi(args[3]);
   incN = atoi(args[4]);
   if (nargs > 5) TA = *args[5];
   if (nargs > 6) TA = *args[6];
   if (nargs > 7) alpha = atof(args[7]);
   if (nargs > 8) beta  = atof(args[8]);
   tloop(jstop, TA, TB, nb, N0, NN, incN, alpha, beta);
#else
   fprintf(stderr, "\n\nFinding crossover point for small case algorithms:\n");
   fp = fopen(Mstr(SM_FOUT), "w");
   nb = NB;
   N0 = 10;
   NN = 100;
   incN = 5;

   fprintf(fp, "#ifndef %sXOVER_H\n#define %sXOVER_H\n\n",
           Mstr(PREU), Mstr(PREU));
   fprintf(fp, "#define ATL_3NB %d\n", 3*NB);
   DoShapes(fp, 'N', 'N', nb, N0, NN, incN, alpha, beta);
   DoShapes(fp, 'N', 'T', nb, N0, NN, incN, alpha, beta);
   DoShapes(fp, 'T', 'N', nb, N0, NN, incN, alpha, beta);
   DoShapes(fp, 'T', 'T', nb, N0, NN, incN, alpha, beta);
#ifdef TCPLX
   small_gemm  = Mjoin(PATL,big_mm);
   big_gemm  = Mjoin(PATL,Mjoin(UPR,big_mm));
   icross = tloop(AtlasMN_REST, 'N', 'T', nb, alpha, beta);
   fprintf(fp, "#define C2R_K %ld\n", icross);
#endif
   fprintf(fp, "\n#endif\n");
   fclose(fp);
#endif
   return(0);
}
