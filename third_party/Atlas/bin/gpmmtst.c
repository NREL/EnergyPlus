/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2003 R. Clint Whaley
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

#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include "atlas_tst.h"
#include "cblas.h"
#include "atlas_pkblas.h"

#ifndef L2SIZE
   #define L2SIZE 4194304
#endif

#ifdef ATL_DeclareSlens
F77_INTEGER ATL_Slen1, ATL_Slen2;
#endif
double time00();

#if defined(TRUST_C)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(Mjoin(cblas_,PRE),gemm) \
         (CblasColMajor, TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#elif defined(TRUST_SMALL)
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,small_mm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#else
   #define trusted_gemm(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc) \
      Mjoin(PATL,f77gemm)(TA, TB, m, n, k, al, A, lda, B, ldb, be, C, ldc)
#endif

#define test_gpmm(UA, TA, UB, TB, UC, m, n, k, al, A, IA, JA, lda, \
                  B, IB, JB, ldb, be, C, IC, JC, ldc) \
   Mjoin(PATL,gpmm)(UA, TA, UB, TB, UC, m, n, k, al, A, IA, JA, lda, \
                    B, IB, JB, ldb, beta, C, IC, JC, ldc);


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

void ge2tp(enum PACK_UPLO uplo, int M, int N, TYPE *A, int lda,
           TYPE *P, int ldp)
{
   int i, j;

   if (uplo == PackUpper)
   {
      for (j=0; j < N; j++)
      {
         for (i=0; i <= j; i++) P[i] = A[i];
         P += ldp--;
         A += lda;
      }
   }
   else /* if (uplo == PackLower) */
   {
      assert(uplo == PackLower);
      for (j=0; j < N; j++)
      {
         for (i=j; i < N; i++) P[i] = A[i-j];
         P += ldp--;
         A += lda;
      }
   }
}

void tp2ge(enum PACK_UPLO uplo, int M, int N, TYPE *P, int ldp, TYPE *A,int lda)
{
   int i, j;

   if (uplo == PackUpper)
   {
      for (j=0; j < N; j++)
      {
         for (i=0; i <= j; i++) A[i] = P[i];
         for (i=j+1; i < N; i++) A[i] = 0.0;
         P += ldp--;
         A += lda;
      }
   }
   else /* if (uplo == PackLower) */
   {
      assert(uplo == PackLower);
      for (j=0; j < N; j++)
      {
         for (i=0; i < j; i++) A[i] = 0.0;
         for (i=j; i < N; i++) A[i] = P[i-j];
         P += ldp--;
         A += lda;
      }
   }
}

void sp2ge(enum PACK_UPLO uplo, int M, int N, TYPE *P, int ldp, TYPE *A,int lda)
{
   int i, j;

   if (uplo == PackUpper)
   {
      for (j=0; j < N; j++)
      {
         for (i=0; i <= j; i++) A[j+i*lda] = A[i+j*lda] = P[i];
         P += ldp--;
      }
   }
   else /* if (uplo == PackLower) */
   {
      assert(uplo == PackLower);
      for (j=0; j < N; j++)
      {
         for (i=j; i < N; i++) A[j+i*lda] = A[i+j*lda] = P[i-j];
         P += ldp--;
      }
   }
}

static void gpgen(enum PACK_UPLO uplo, int M, int N, TYPE *P, int ldp)
{
   int ma, mi, m;
   void matgen(int M, int N, TYPE *A, int lda, int seed);

   if (uplo == PackGen) matgen(M, N, P, ldp, (M<<16)+N*ldp);
   else
   {
      mi = Mmin(M,N);
      ma = Mmax(M,N);
      m = ((mi * (mi+1))>>1) + (ma-mi) * mi;
      matgen(m, 1, P, m, (M<<16)+N*ldp);
   }
}

int checkC(enum PACK_UPLO uplo, int M, int N, int K, int I, int J,
           TYPE *P, int ldp, TYPE *C, int ldc, SCALAR alpha, SCALAR beta)
{
   int nerr=0, ii, jj, i, j=0;
   static TYPE feps=0.0;
   TYPE maxval, f1, ferr;

   if (feps == 0.0) feps = EPS;
   #ifdef TREAL
      ferr = 2.0 * (Mabs(alpha) * 2.0*K*feps + Mabs(beta) * feps) + feps;
   #else
      f1 = Mabs(*alpha) + Mabs(alpha[1]);
      maxval = Mabs(*beta) + Mabs(beta[1]);
      ferr = 2.0 * (f1*8.0*K*feps + maxval*feps) + feps;
   #endif
   M += I;
   N += J;
   if (uplo == PackGen)
   {
      for (j=0; j != N; j++)
      {
         if (j >= J)
         {
            for (i=0; i != M SHIFT; i++)
            {
               if (i >= I)
               {
                  f1 = P[i] - C[i];
                  if (f1 < 0.0) f1 = -f1;
                  if (f1 > ferr)
                  {
                     nerr++;
                     if (f1 > maxval)
                     {
                        maxval=f1;
                        ii = i+1;
                        jj = j+1;
                     }
                  }
               }
            }
         }
         P += ldp SHIFT;
         C += ldc SHIFT;
      }
   }
/*
 * These guys will not work for complex
 */
   else if (uplo == PackUpper)
   {
      for (j=0; j < N; j++, P += ldp SHIFT, C += ldc SHIFT)
      {
         if (j < J) continue;
         for (i=0; i <= j; i++)
         {
            if (i < I) continue;
            f1 = P[i] - C[i];
            if (f1 < 0.0) f1 = -f1;
            if (f1 > ferr)
            {
               nerr++;
               if (f1 > maxval)
               {
                  maxval=f1;
                  ii = i+1;
                  jj = j+1;
               }
            }
         }
      }
   }
   else
   {
      for (j=0; j < N; j++, P += ldp SHIFT, C += ldc SHIFT)
      {
         if (j < J) continue;
         for (i=j; i < N; j++)
         {
            if (i < I) continue;
            f1 = P[i-j] - C[i];
            if (f1 < 0.0) f1 = -f1;
            if (f1 > ferr)
            {
               nerr++;
               if (f1 > maxval)
               {
                  maxval=f1;
                  ii = i+1;
                  jj = j+1;
               }
            }
         }
      }
   }
   if (maxval != 0.0)
      fprintf(stderr, "ERROR: nerr=%d, i=%d, j=%d, maxval=%e\n",
              nerr, ii,jj, maxval);
   return(nerr);
}

int mmcase(int TEST, int CACHESIZE, char UA, char TA, char UB, char TB, char UC,
           int M, int N, int K, SCALAR alpha, int IA, int JA, int lda,
           int IB, int JB, int ldb, SCALAR beta,
           int IC, int JC, int ldc)
{
   char *pc;
#ifdef TREAL
   char *form="%5d %c  %c  %c  %c  %c %4d %4d %4d %5.1f %4d %4d %4d %4d %4d %4d %5.1f %4d %4d %4d %6.2f %6.1f %4.2f  %3s\n";
   #define MALPH alpha
   #define MBETA beta
#else
   #define MALPH *alpha, alpha[1]
   #define MBETA *beta, beta[1]
   char *form="%5d %c  %c  %c  %c  %c %4d %4d %4d %5.1f %5.1f %4d %4d %4d %4d %4d %4d %5.1f %5.1f %4d %4d %4d %6.2f %6.1f %4.2f  %3s\n";
#endif
   int ii, jj, i, j=0, PASSED=1, nerrs;
   double t0, t1, t2, t3, mflop;
   static TYPE feps=0.0;
   static int itst=1;
   /*int *L2, nL2=(1.3*L2SIZE)/sizeof(int);*/
   enum ATLAS_TRANS TAe, TBe;
   enum PACK_UPLO UAe, UBe, UCe;
   double l2ret;
   TYPE *A, *B, *C, *pA, *pB, *pC;

fprintf(stderr, "M=%d, N=%d, K=%d, IA=%d, JA=%d, lda=%d, IB=%d, JB=%d, ldb=%d IC
=%d, JC=%d, ldc=%d\n",
        M, N, K, IA, JA, lda, IB, JB, ldb, IC, JC, ldc);

   if (UA == 'u' || UA == 'U') UAe = PackUpper;
   else if (UA == 'l' || UA == 'L') UAe = PackLower;
   else UA = PackGen;
   if (UB == 'u' || UB == 'U') UBe = PackUpper;
   else if (UB == 'l' || UB == 'L') UBe = PackLower;
   else UB = PackGen;
   if (UC == 'u' || UC == 'U') UCe = PackUpper;
   else if (UC == 'l' || UC == 'L') UCe = PackLower;
   else UC = PackGen;

   if (TA == 'n' || TA == 'N') TAe = AtlasNoTrans;
   else TAe = AtlasTrans;
   if (TB == 'n' || TB == 'N') TBe = AtlasNoTrans;
   else TBe = AtlasTrans;

/*
 * See if case needs to be skipped
 */
   if (TAe == PackNoTrans || TAe == AtlasConj)
   {
      if (IA+M <= lda && JA+K <= lda && IA >=0 && JA >= 0) return(-1);
      if (UAe == PackUpper) { if (JA+K <= lda && IA+M <= JA) return(-1); }
      else if (UAe == PackLower) { if (JA+M <= lda && IA+K <= JA) return(-1); }
   }
   else
   {
      if (IA+K <= lda && JA+M <= lda && IA >=0 && JA >= 0) return(-1);
      if (UAe == PackUpper) { if(IA >= JA+K && IA+M <= lda) return(-1); }
      else if (UAe == PackLower) { if (IA >= JA+M && IA+K <= lda) return(-1); }
   }

   if (TBe == PackNoTrans || TBe == AtlasConj)
   {
      if (IB+K <= ldb && JB+N <= ldb && IB >=0 && JB >= 0) return(-1);
      if (UBe == PackUpper) { if (JB+N <= lda && IB+N <= JB) return(-1); }
      else if (UBe == PackLower) { if (IB >= JB+N && IB+K <= ldb) return(-1); }
   }
   else
   {
      if (IB+K <= ldb && JB+M <= ldb && IB >=0 && JB >= 0) return(-1);
      if (UBe == PackUpper) { if (JB+K <= ldb && IB+N <= JB) return(-1); }
      else if (UBe == PackLower) { if (IB >= JB+K && IB+N <= ldb) return(-1); }
   }

   l2ret = ATL_flushcache( CACHESIZE );
   A = malloc(lda*lda*ATL_sizeof);
   assert(A);
   B = malloc(ldb*ldb*ATL_sizeof);
   assert(B);
   C = malloc(ldc*ldc*ATL_sizeof);
   assert(C);

   if (UAe == PackGen)
   {
      pA = A;
      matgen(lda, lda, A, lda, K*1112);
   }
   else
   {
      pA = malloc( ((lda * (lda+1))>>1)*ATL_sizeof );
      assert(pA);
      gpgen(UAe, lda, lda, pA, lda);
      tp2ge(UAe, lda, lda, pA, lda, A, lda);
   }

   if (UBe == PackGen)
   {
      pB = B;
      matgen(lda, lda, B, ldb, N*2238);
   }
   else
   {
      pB = malloc( ((ldb * (ldb+1))>>1)*ATL_sizeof );
      assert(pB);
      gpgen(UBe, ldb, ldb, pB, ldb);
      tp2ge(UBe, ldb, ldb, pB, ldb, B, ldb);
   }
   if (UCe == PackGen)
   {
      matgen(ldc, ldc, C, ldc, M*N);
      pC = malloc(ldc*ldc*ATL_sizeof);
      assert(pC);
      matgen(ldc, ldc, C, ldc, M*N);
   }
   else
   {
      pC = malloc( ((ldc * (ldc+1))>>1)*ATL_sizeof );
      assert(pC);
      gpgen(UCe, ldc, ldc, pC, ldc);
      tp2ge(UCe, ldc, ldc, pC, ldc, C, ldc);
   }

   l2ret = ATL_flushcache( -1 );

   t0 = time00();
   trusted_gemm(TAe, TBe, M, N, K, alpha, A+IA+JA*lda, lda, B+IB+JB*ldb, ldb,
                beta, C+IC+JC*ldc, ldc);
   t1 = time00() - t0;
   if (t1 <= 0.0) mflop = t1 = 0.0;
   else   /* flop rates actually 8MNK+12MN & 2MNK + 2MN, resp */
      #ifdef TCPLX
         mflop = ( ((8.0*M)*N)*K ) / (t1*1000000.0);
      #else
         mflop = ( ((2.0*M)*N)*K ) / (t1*1000000.0);
      #endif
   if (TEST != -99)
      printf(form, itst, 'G', TA, 'G', TB, 'G', M, N, K, MALPH, IA, JA, lda,
             IB, JB, ldb, MBETA, IC, JC, ldc, t1, mflop, 1.0, "---");

   /* invalidate L2 cache */
   l2ret = ATL_flushcache( -1 );

   t0 = time00();
   test_gpmm(UAe, TAe, UBe, TBe, UCe, M, N, K, alpha, A, IA, JA, lda,
             B, IB, JB, ldb, beta, C, IC, JC, ldc);
   t2 = time00() - t0;
   if (t2 <= 0.0) t2 = mflop = 0.0;
   else
      #ifdef TCPLX
         mflop = ( ((8.0*M)*N)*K ) / (t2*1000000.0);
      #else
         mflop = ( ((2.0*M)*N)*K ) / (t2*1000000.0);
      #endif

   if (TEST && TEST != -99)
   {
      nerrs = checkC(UC, M, N, K, IC, JC, pC, ldc, C, ldc, alpha, beta);
      if (nerrs)
      {
         pc = "NO!";
         PASSED = 0;
      }
      else pc = "YES";
   }
   else pc = "---";
   if (t1 == t2) t3 = 1.0;
   else if (t2 != 0.0) t3 = t1/t2;
   else t3 = 0.0;
   if (TEST != -99)
      printf(form, itst++, UA, TA, UB, TB, UC, M, N, K, MALPH, IA, JA, lda,
             IB, JB, ldb, MBETA, IC, JC, ldc, t2, mflop, t3, pc);
   l2ret = ATL_flushcache( 0 );
   return(PASSED);
}

void PrintUsage(char *name)
{
   fprintf(stderr, "USAGE: %s \n", name);
   fprintf(stderr,
           "   -UA <nuplo> L/U/G -UB <nuplo> L/U/G -UC <nuplo> L/U/G \n");
   fprintf(stderr, "   -Atrans <ntrans> n/t/c -Btrans <ntrans> n/t/c \n");
   fprintf(stderr, "   -Diag <ndiags> N/U \n");
   fprintf(stderr,
           "   -M <m1> <mN> <minc> -N <n1> <nN> <ninc> <k1> <kN> <kinc> \n");
   fprintf(stderr, "   -n <n> -m <m> -k <k> \n");
   fprintf(stderr, "   -ia <i> -ja <j> -ib <i> -jb <j> -ic <i> -jc <j>\n");
   fprintf(stderr,
           "   -I[A,B,C] <i1> <iN> <iinc> -J[A,B,C] <j1> <jN> <jinc>\n");
   fprintf(stderr,
   "   -a <nalphas> <alpha1> ... <alphaN> -b <nbetas> <beta1> ... <betaN>\n");
   fprintf(stderr, "   -Test <0/1> -F <mflops> -C <cachesize>\n");
   exit(-1);
}

void GetFlags(int nargs, char *args[], int *TEST,
              int *nua, enum PACK_UPLO **UAS, int *nub, enum PACK_UPLO **UBS,
              int *nuc, enum PACK_UPLO **UCS,
              int *nta, enum ATLAS_TRANS **TransA,
              int *ntb, enum ATLAS_TRANS **TransB, int *ndiag,
              enum ATLAS_DIAG **Diag, int *M0, int *MN, int *Minc,
              int *N0, int *NN, int *Ninc, int *K0, int *KN, int *Kinc,
              int *IA0, int *IAN, int *IAINC, int *JA0, int *JAN, int *JAINC,
              int *IB0, int *IBN, int *IBINC, int *JB0, int *JBN, int *JBINC,
              int *IC0, int *ICN, int *ICINC, int *JC0, int *JCN, int *JCINC,
              int *nalphas, TYPE **alphas, int *nbetas, TYPE **betas,
              int *LDA_IS_M, int *MFLOP, int *CACHESIZE)

{
   char ch;
   int i, j;
   int *ni, **is;
   int *nuplo;
   enum PACK_UPLO **Uplo;
/*
 * Set up defaults
 */
   *TEST = 1;
   *IA0 = *JA0 = *IB0 = *JB0 = *IC0 = *JC0 = 0;
   *IAN = *JAN = *IBN = *JBN = *ICN = *JCN = 0;
   *IAINC = *JAINC = *IBINC = *JBINC = *ICINC = *JCINC = 1;
   *M0 = *N0 = *K0 = -1;
   *nuc = *nub = *nua = *nta = *ntb = *ndiag = *nalphas = *nbetas = -1;
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
      case 'F':
         *MFLOP = atoi(args[++i]);
         break;
      case 'C':
            if( args[i+1] == NULL ) PrintUsage( args[0] );
	    *CACHESIZE = 1024*atoi(args[++i]);
            break;
      case 'U':
         if (args[i][2] == 'A')
         {
            nuplo = nua;
            Uplo = UAS;
         }
         else if (args[i][2] == 'B')
         {
            nuplo = nub;
            Uplo = UBS;
         }
         else
         {
            nuplo = nuc;
            Uplo = UCS;
         }
         *nuplo = atoi(args[++i]);
         *Uplo = malloc(*nuplo * sizeof(int));
         assert(*Uplo);
         for (j=0; j != *nuplo; j++)
         {
            ch = *args[++i];
            if (ch == 'u' || ch == 'U') (*Uplo)[j] = PackUpper;
            else if (ch == 'l' || ch == 'L') (*Uplo)[j] = PackLower;
            else  (*Uplo)[j] = PackGen;
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
      case 'I':
         switch(args[i][2])
         {
         case 'a':
         case 'A':
            *IA0 =  atoi(args[++i]);
            *IAN =  atoi(args[++i]);
            *IAINC = atoi(args[++i]);
            break;
         case 'b':
         case 'B':
            *IB0 =  atoi(args[++i]);
            *IBN =  atoi(args[++i]);
            *IBINC = atoi(args[++i]);
            break;
         case 'c':
         case 'C':
            *IC0 =  atoi(args[++i]);
            *ICN =  atoi(args[++i]);
            *ICINC = atoi(args[++i]);
            break;
         default:
            PrintUsage(args[0]);
         }
         break;
      case 'J':
         switch(args[i][2])
         {
         case 'a':
         case 'A':
            *JA0 =  atoi(args[++i]);
            *JAN =  atoi(args[++i]);
            *JAINC = atoi(args[++i]);
            break;
         case 'b':
         case 'B':
            *JB0 =  atoi(args[++i]);
            *JBN =  atoi(args[++i]);
            *JBINC = atoi(args[++i]);
            break;
         case 'c':
         case 'C':
            *JC0 =  atoi(args[++i]);
            *JCN =  atoi(args[++i]);
            *JCINC = atoi(args[++i]);
            break;
         default:
            PrintUsage(args[0]);
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
      case 'i':
         switch(args[i][2])
         {
         case 'a':
         case 'A':
            *IA0 = *IAN = *IAINC = atoi(args[++i]);
            break;
         case 'b':
         case 'B':
            *IB0 = *IBN = *IBINC = atoi(args[++i]);
            break;
         case 'c':
         case 'C':
            *IC0 = *ICN = *ICINC = atoi(args[++i]);
            break;
         }
         break;
      case 'j':
         switch(args[i][2])
         {
         case 'a':
         case 'A':
            *JA0 = *JAN = *JAINC = atoi(args[++i]);
            break;
         case 'b':
         case 'B':
            *JB0 = *JBN = *JBINC = atoi(args[++i]);
            break;
         case 'c':
         case 'C':
            *JC0 = *JCN = *JCINC = atoi(args[++i]);
            break;
         }
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
      *N0 = 100;
      *NN = 1000;
      *Ninc = 100;
   }
   if (*nua == -1)
   {
      *nua = 1;
      *UAS = malloc(sizeof(int));
      assert(*UAS);
      **UAS = PackGen;
   }
   if (*nub == -1)
   {
      *nub = 1;
      *UBS = malloc(sizeof(int));
      assert(*UBS);
      **UBS = PackGen;
   }
   if (*nuc == -1)
   {
      *nuc = 1;
      *UCS = malloc(sizeof(int));
      assert(*UCS);
      **UCS = PackGen;
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
   }
}
___main(){}
__main(){}
MAIN__(){}
_MAIN_(){}
main(int nargs, char *args[])
/*
 *  tst <tst> <# TA> <TA's> <# TB's> <TB's> <M0> <MN> <incM> <N0> <NN> <incN>
 *      <K0> <KN> <incK> <# alphas> <alphas> <# betas> <betas>
 *
 */
{
   int IA0, IAN, IAinc, IB0, IBN, IBinc, IC0, ICN, ICinc;
   int JA0, JAN, JAinc, JB0, JBN, JBinc, JC0, JCN, JCinc;
   int M0, MN, incM, N0, NN, incN, K0, KN, incK, lda, ldb, ldc, MFLOP;
   int i, k, m, n, im, in, ik, ita, itb, ia, ib, nTA, nTB, nalph, nbeta;
   int iua, iub, iuc, iia, iib, iic, ija, ijb, ijc;
   int itst=0, ipass=0, iskip=0, itmp, TEST, LDA_IS_M, MSAME=0, KSAME=0;
   int ndiag, nua, nub, nuc;
   TYPE *alph, *beta, *A, *B, *C, *D=NULL;
   #ifdef TREAL
      TYPE bet1 = 1.0, alp1 = -1.0;
   #else
      TYPE bet1[2] = {1.0, 0.0}, alp1[2] = {-1.0, 0.0};
   #endif
   char TA, TB, UA, UB, UC;
   enum ATLAS_SIDE *Side;
   enum PACK_UPLO *UAs, *UBs, *UCs;
   enum ATLAS_TRANS *TransA, *TransB, TAc, TBc;
   enum ATLAS_DIAG *Diag;
   int CACHESIZE;

   GetFlags(nargs, args, &TEST, &nua, &UAs, &nub, &UBs, &nuc, &UCs,
            &nTA, &TransA, &nTB, &TransB, &ndiag, &Diag,
            &M0, &MN, &incM, &N0, &NN, &incN, &K0, &KN, &incK,
            &IA0, &IAN, &IAinc, &JA0, &JAN, &JAinc,
            &IB0, &IBN, &IBinc, &JB0, &JBN, &JBinc,
            &IC0, &ICN, &ICinc, &JC0, &JCN, &JCinc,
            &nalph, &alph, &nbeta, &beta, &LDA_IS_M, &MFLOP,&CACHESIZE);

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

/*
 * Page in routs so first timing is more reliable
 */
   mmcase(-99, CACHESIZE, 'U', 'N', 'U', 'N', 'U', 100, 100, 100, 2.0,
          0, 100, 200, 0, 100, 200, 2.0, 0, 100, 200);

#ifdef TREAL
   printf(
"\nTEST# UA TA UB TB UC    M    N    K ALPHA   IA   JA  LDA   IB   JB  LDB  BETA   IC   JC  LDC   TIME  MFLOP SpUp PASS\n");
   printf(
  "===== == == == == == ==== ==== ==== ===== ==== ==== ==== ==== ==== ==== ===== ==== ==== ==== ====== ====== ==== ====\n");
#else
   printf(
"\nTEST# UA TA UB TB UC    M    N    K    ALPHA      IA   JA  LDA   IB   JB  LDB     BETA      IC   JC  LDC   TIME  MFLOP SpUp PASS\n");
   printf(
"===== == == == == == ==== ==== ==== ===== ===== ==== ==== ==== ==== ==== ==== ===== ===== ==== ==== ==== ====== ====== ==== ====\n");
#endif
   for (iua=0; iua != nua; iua++)
   {
      if (UAs[iua] == PackUpper) UA = 'U';
      else if (UAs[iua] == PackLower) UA = 'L';
      else UA = 'G';
   for (iub=0; iub != nub; iub++)
   {
      if (UBs[iub] == PackUpper) UB = 'U';
      else if (UBs[iub] == PackLower) UB = 'L';
      else UB = 'G';
   for (iuc=0; iuc != nuc; iuc++)
   {
      if (UCs[iuc] == PackUpper) UC = 'U';
      else if (UCs[iuc] == PackLower) UC = 'L';
      else UC = 'G';
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
            for (iia=IA0; iia <= IAN; iia += IAinc)
            {
            for (ija=JA0; ija <= JAN; ija += JAinc)
            {
            for (iib=IB0; iib <= IBN; iib += IBinc)
            {
            for (ijb=JB0; ijb <= JBN; ijb += JBinc)
            {
            for (iic=IC0; iic <= ICN; iic += IBinc)
            {
            for (ijc=JC0; ijc <= JCN; ijc += JBinc)
            {
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
                        lda += iia;
                        ldb += iib;
                        ldc += iic;

                        #ifdef TREAL
                           itmp = mmcase(TEST, CACHESIZE, UA, TA, UB, TB, UC,
                                         m, n, k, alph[ia], iia, ija, lda,
                                         iib, ijb, ldb, beta[ib],
                                         iic, ijc, ldc);
                        #else
                           itmp = mmcase(TEST, CACHESIZE, UA, TA, UB, TB, UC,
                                         m, n, k, alph+(ia SHIFT), iia, ija,
                                         lda, iib, ijb, ldb, beta+(ib SHIFT),
                                         iic, ijc, ldc);
                        #endif
                        if (itmp == -1) iskip++;
                        else if (itmp) ipass++;
                        }
                     }
                  }
               }
            }
            }
            }
            }
            }
            }
         }
      }
   }
   }
   }
   }
   if (TEST)
      printf("\nNTEST=%d, NUMBER PASSED=%d, NUMBER SKIPPED=%d, NUMBER FAILURES=%d\n",
             itst, ipass, iskip, itst-ipass-iskip);
   else printf("\nDone with %d timing runs\n",itst);
   free(UAs);
   free(UBs);
   free(UCs);
   free(TransA);
   free(TransB);
   free(Diag);
   free(alph);
   free(beta);
   exit(0);
}
