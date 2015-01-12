/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2000 R. Clint Whaley
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

#define CBP Mjoin(cblas_,PRE)


double time00();

#ifdef TimeF77
   #define test_posv(Ord_, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
    ATL_assert(Mjoin(PATL,f77posv)(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) == 0)
   #define test_gesv(Ord_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
    ATL_assert(Mjoin(PATL,f77gesv)(N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) == 0)
   #define test_gels(Ord_, TA_, M_, N_, NRHS_, A_, lda_, B_, ldb_) \
      ATL_assert(Mjoin(PATL,f77gels)(TA_, M_, N_, NRHS_, A_, lda_, B_, ldb_)==0)
#else
   #include "clapack.h"
   #ifdef ATL_FULL_LAPACK
      #include "C_lapack.h"
   #endif
   #define Cgesv Mjoin(Mjoin(clapack_,PRE),gesv)
   #define Cposv Mjoin(Mjoin(clapack_,PRE),posv)
   #define Cgels Mjoin(Mjoin(clapack_,PRE),gels)
//   #define Cgels Mjoin(PATL,gels)
   #define test_posv(Ord_, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   { \
      int ii; \
      ii = Cposv(Ord_, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_); \
      if (ii) fprintf(stderr, "posv returns %d!!\n", ii); \
      fflush(stderr); \
   }
   #define test_gesv(Ord_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
      ATL_assert(Cgesv(Ord_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) == 0)
   #define test_gels(Ord_, TA_, M_, N_, NRHS_, A_, lda_, B_, ldb_) \
       ATL_assert(!Cgels(Ord_,TA_, M_, N_, NRHS_, A_, lda_, B_, ldb_));
#endif

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -n <n> -N <N0 NN incN> -r <nrhs> -R <R0 RN, incR> -T <thresh> -F <mflop> -a <lagap> -b <ldbgap> -C <cache size> -O <norders> <ord1> ... <ordN> -U <nuplos> <uplo1> ... <uploN>\n", nam);
   fprintf(stderr, "   uplos can be 'u'/'l' for LLt, 'q' for QR, else LU\n");
   exit(-1);
}

void GetFlags(int nargs, char **args, int *N0, int *NN, int *incN,
              int *R0, int *RN, int *incR, int *ldagap, int *ldbgap,
              double *thresh, int *nord, enum CBLAS_ORDER **Ord,
              int *nuplo, int **Uplo, int *CS, int *MFLOP, int *QR, int *nrep)
{
   int i, j;
   char ch;

   *nrep = 1;
   *QR = 0;
   *N0 = 100; *NN = 1000; *incN = 100;
   *R0 = *RN = *incR = 1;
   *ldagap = *ldbgap = 0;
   *nord = *nuplo = 0;
   *Ord = NULL;
   *Uplo = NULL;
   *thresh = 100.0;
   #ifdef L2SIZE
      *CS = L2SIZE;
   #else
      *CS = 4*1024*1024;
   #endif
   *MFLOP = 0;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case '#':
         *nrep = atoi(args[++i]);
         break;
      case 'Q':
         *QR = atoi(args[++i]);
         break;
      case 'T':
         *thresh = atof(args[++i]);
         break;
      case 'C':
         *CS = 1024*atoi(args[++i]);
         break;
      case 'a':
         *ldagap = atoi(args[++i]);
         break;
      case 'b':
         *ldbgap = atoi(args[++i]);
         break;
      case 'n':
         *N0 = *NN = *incN = atoi(args[++i]);
         break;
      case 'r':
         *R0 = *RN = *incR = atoi(args[++i]);
         break;
      case 'R':
         *R0 = atoi(args[++i]);
         *RN = atoi(args[++i]);
         *incR = atoi(args[++i]);
         break;
      case 'N':
         *N0 = atoi(args[++i]);
         *NN = atoi(args[++i]);
         *incN = atoi(args[++i]);
         break;
      case 'F':
         fprintf(stderr, "This tester cannot time with multiple calls!!\n");
         exit(-1);
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
         *Uplo = malloc(*nuplo * sizeof(int));
         ATL_assert(*Uplo);
         for (j=0; j != *nuplo; j++)
         {
            if (args[i] == NULL) PrintUsage(args[0]);
            ch = *args[++i];
            if (ch == 'u' || ch == 'U') (*Uplo)[j] = AtlasUpper;
            else if (ch == 'l' || ch == 'L') (*Uplo)[j] = AtlasLower;
            else if (ch == 'q' || ch == 'Q') (*Uplo)[j] = 1;
            else (*Uplo)[j] = 0;
         }
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   if (*nuplo == 0)
   {
     *nuplo = 1;
      *Uplo = malloc(sizeof(int));
      ATL_assert(*Uplo);
      **Uplo = 0;
   }
   if (*nord == 0)
   {
      *nord = 1;
      *Ord = malloc(sizeof(enum CBLAS_ORDER));
      ATL_assert(*Ord);
      **Ord = AtlasColMajor;
   }
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

static TYPE diffnrm(const int N, const TYPE *X1, const TYPE *X2)
{
   TYPE t0, t1, max=0.0;
   int i;

   for (i=0; i < N; i++) /* compute ||x-xc|| */
   {
      #ifdef TREAL
         t0 = X1[i] - X2[i];
         t0 = Mabs(t0);
         if (t0 > max) max = t0;
      #else
         t0 = X1[2*i] - X2[2*i]; t1 = X1[2*i+1] - X2[2*i+1];
         t0 = Mabs(t0); t1 = Mabs(t1);
         t0 += t1;
         if (t0 > max) max = t0;
      #endif
   }
   return(max);
}

static TYPE slvtst(int N, int NRHS, TYPE nrmA, TYPE *XC, int ldxc,
                   TYPE *X, int ldx, double thresh)
/*
 * Figures ||x - xc|| / (N*eps*||A||*||xc||), where xc is known correct
 * answer, and x is computed result
 */
{
   TYPE eps, nrmD, nrmX, t0, max=0.0;
   int j;

   eps = Mjoin(PATL,epsilon)();
   for (j=0; j < NRHS; j++)
   {
      nrmX = Mjoin(PATL,infnrm)(N, XC, 1);
      nrmD = diffnrm(N, XC, X);
      t0 = nrmD / (N*eps*nrmA*nrmX);
      if (t0 > thresh) fprintf(stderr, "RHS=%d, nrm=%f\n", j+1,t0);
      if (t0 > max) max = t0;
      XC += ldxc SHIFT;
      X  += ldx SHIFT;
   }
   return(max);
}

static TYPE geslvtst(enum CBLAS_ORDER Order, int N, int NRHS, TYPE nrmA,
                     TYPE *XC, int ldxc, TYPE *X, int ldx, double thresh)
/*
 * Figures ||x - xc|| / (N*eps*||A||*||xc||), where xc is known correct
 * answer, and x is computed result
 */
{
   return(slvtst(N, NRHS, nrmA, XC, ldxc, X, ldx, thresh));
}

static TYPE syslvtst(enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo,
                     int N, int NRHS, TYPE *A, int lda, TYPE *XC, int ldxc,
                     TYPE *X, int ldx, double thresh)
/*
 * Figures ||x - xc|| / (N*eps*||A||*||xc||), where xc is known correct
 * answer, and x is computed result
 */
{
   TYPE nrmA, nrmX, t0, max=0.0;
   TYPE *xc=XC, *x=X;

   if (Order == CblasRowMajor)
   {
      if (Uplo == CblasUpper) Uplo = CblasLower;
      else Uplo = CblasUpper;
   }
   #ifdef TREAL
      nrmA = Mjoin(PATL,synrm)(Uplo, N, A, lda);
   #else
      nrmA = Mjoin(PATL,henrm)(Uplo, N, A, lda);
   #endif
   return(slvtst(N, NRHS, nrmA, XC, ldxc, X, ldx, thresh));
}

static int posvtst(enum CBLAS_ORDER Order, enum CBLAS_UPLO Uplo,
                   int N, int NRHS, int lda, int ldb,
                   int CS, double thresh)
{
   TYPE *A, *X, *B, *F, nrm;
   double t0, t1, mflop;
   int ierr=0;
   char cord='C', cuplo='U';
   #ifdef TCPLX
      const TYPE one[2]={ATL_rone, ATL_rzero};
      const TYPE zero[2]={ATL_rzero, ATL_rzero};
   #endif

   if (Order == CblasRowMajor) cord = 'R';
   if (Uplo == CblasLower) cuplo = 'L';
/*
 * Generate A and X, and figure B with matmul
 */
   A = GetHE(Order, Uplo, N, lda);
   X = GetGE(N, NRHS, ldb);
   B = GetGE(N, NRHS, ldb);
   ATL_assert(A && X && B);
/*
 *    For ColMajor, we do b = A * x, but for row we have b' and x', so we
 *    trans both sides of equation, and do b' = x' * A' = x' * A instead
 */
   #ifdef TREAL
      if (Order == CblasColMajor)
         Mjoin(CBP,symm)(Order, CblasLeft, Uplo, N, NRHS, ATL_rone, A, lda,
                         X, ldb, ATL_rzero, B, ldb);
      else
         Mjoin(CBP,symm)(Order, CblasRight, Uplo, NRHS, N, ATL_rone, A, lda,
                         X, ldb, ATL_rzero, B, ldb);
   #else
      if (Order == CblasColMajor)
         Mjoin(CBP,hemm)(Order, CblasLeft, Uplo, N, NRHS, one, A, lda, X, ldb,
                         zero, B, ldb);
/*
 *    For RowMajor, we need x' & b', so we get b' = x' * A'.  In order to
 *    get A', we conjugate it.  Afterwords, put it back to A for factor.
 */
      else
      {
         if (Uplo == AtlasUpper)
            for (ierr=0; ierr < N-1; ierr++)
               Mjoin(PATLU,scal)(N-1-ierr, ATL_rnone, A+3+ierr*((lda+1)<<1),2);
         else
            for (ierr=1; ierr < N; ierr++)
               Mjoin(PATLU,scal)(ierr, ATL_rnone, A+1+ierr*(lda<<1), 2);
         Mjoin(CBP,hemm)(Order, CblasRight, Uplo, NRHS, N, one, A, lda, X, ldb,
                         zero, B, ldb);
         if (Uplo == AtlasUpper)
            for (ierr=0; ierr < N-1; ierr++)
               Mjoin(PATLU,scal)(N-1-ierr, ATL_rnone, A+3+ierr*((lda+1)<<1),2);
         else
            for (ierr=1; ierr < N; ierr++)
               Mjoin(PATLU,scal)(ierr, ATL_rnone, A+1+ierr*(lda<<1), 2);
         ierr = 0;
      }
   #endif

   CS = ATL_DivBySize(CS);
   F = GetGE(CS, 1, CS); /* flush cache */
   if (F) free(F);

   t0 = time00();
   test_posv(Order, Uplo, N, NRHS, A, lda, B, ldb);
   t1 = time00() - t0;
   mflop = N;
   if (F == NULL || t1 <= 0.0) t1 = mflop = -1.0;
#ifdef TREAL
   else mflop = ( ((1.0*N)*N*N) + (3.0*NRHS)*N*N) / (t1*3000000.0);
#else
   else mflop = ( (4.0/3.0)*(mflop*mflop*mflop) + 3.0 * (mflop*mflop)
                  + NRHS*mflop*(4*mflop+2) ) / (t1*1000000.0);
#endif

   nrm = syslvtst(Order, Uplo, N, NRHS, A, lda, X, ldb, B, ldb, thresh);
   if (nrm > thresh) ierr=1;

   fprintf(stdout, "  %c     %c %7d %7d %7d %7d %10.3f %9.2f  %e\n",
           cord, cuplo, N, NRHS, lda, ldb, t1, mflop, nrm);
   free(A);
   free(X);
   free(B);
   return(ierr);
}

static int gelstst(enum CBLAS_ORDER Order, int N, int NRHS, int lda, int ldb,
                   int CS, double thresh)
{
   TYPE *A, *X, *B, *F, nrm;
   #ifdef TREAL
      const TYPE one=ATL_rone, zero=ATL_rzero;
   #else
      const TYPE one[2]={ATL_rone, ATL_rzero};
      const TYPE zero[2]={ATL_rzero, ATL_rzero};
   #endif
   double t0, t1, mflop;
   char cord='C';
   int ierr=0;

/*
 * This is really only a guestimate on the flop count
 */
   mflop = N;
   #ifdef TREAL
      mflop = ((4.0/3.0)*mflop*mflop*mflop) + NRHS*mflop*mflop;
   #else
      mflop = ((16.0/3.0)*mflop*mflop*mflop) + NRHS*mflop*(4.0*mflop+2);
   #endif
   if (Order == CblasRowMajor) cord = 'R';
/*
 * Generate A and X, and figure B with matmul
 */
   A = GetGE(N, N, lda);
   ATL_assert(A);

   nrm = Mjoin(PATL,genrm1)(N, N, A, lda);
   X = GetGE(N, NRHS, ldb);
   B = GetGE(N, NRHS, ldb);
   ATL_assert(X && B);
   if (Order == CblasColMajor)
      Mjoin(CBP,gemm)(Order, CblasNoTrans, CblasNoTrans, N, NRHS, N, one,
                      A, lda, X, ldb, zero, B, ldb);
   else
      Mjoin(CBP,gemm)(Order, CblasNoTrans, CblasTrans, NRHS, N, N, one,
                      X, ldb, A, lda, zero, B, ldb);

   CS = ATL_DivBySize(CS);
   F = GetGE(CS, 1, CS); /* flush cache */
   if (F) free(F);

   t0 = time00();
   test_gels(Order, AtlasNoTrans, N, N, NRHS, A, lda, B, ldb);
   t1 = time00() - t0;
   if (F == NULL || t1 <= 0.0) t1 = mflop = -1.0;
   else mflop = mflop / (t1*1000000.0);

   nrm = geslvtst(Order, N, NRHS, nrm, X, ldb, B, ldb, thresh);
   if (nrm > thresh) ierr=1;

   fprintf(stdout, "  %c     %c %7d %7d %7d %7d %10.3f %9.2f  %e\n",
           cord, 'Q', N, NRHS, lda, ldb, t1, mflop, nrm);
   free(A);
   free(X);
   free(B);
   return(ierr);
}

static int gesvtst(enum CBLAS_ORDER Order, int N, int NRHS, int lda, int ldb,
                   int CS, double thresh)
{
   TYPE *A, *X, *B, *F, nrm;
   #ifdef TREAL
      const TYPE one=ATL_rone, zero=ATL_rzero;
   #else
      const TYPE one[2]={ATL_rone, ATL_rzero};
      const TYPE zero[2]={ATL_rzero, ATL_rzero};
   #endif
   double t0, t1, mflop;
   char cord='C';
   int ierr=0;
   int *ipiv;

   mflop = N;
   #ifdef TREAL
      mflop = ((2.0/3.0)*mflop*mflop*mflop) + mflop*mflop*(NRHS+0.5);
   #else
      mflop = ((8.0/3.0)*mflop*mflop*mflop) - (mflop*mflop)
            + NRHS*mflop*(4.0*mflop+2);
   #endif
   if (Order == CblasRowMajor) cord = 'R';
/*
 * Generate A and X, and figure B with matmul
 */
   A = GetGE(N, N, lda);
   ATL_assert(A);
   nrm = Mjoin(PATL,genrm1)(N, N, A, lda);
   X = GetGE(N, NRHS, ldb);
   B = GetGE(N, NRHS, ldb);
   ipiv = malloc(N*sizeof(int));
   ATL_assert(X && B && ipiv);
   if (Order == CblasColMajor)
      Mjoin(CBP,gemm)(Order, CblasNoTrans, CblasNoTrans, N, NRHS, N, one,
                      A, lda, X, ldb, zero, B, ldb);
   else
      Mjoin(CBP,gemm)(Order, CblasNoTrans, CblasTrans, NRHS, N, N, one,
                      X, ldb, A, lda, zero, B, ldb);

   CS = ATL_DivBySize(CS);
   F = GetGE(CS, 1, CS); /* flush cache */
   if (F) free(F);

   t0 = time00();
   test_gesv(Order, N, NRHS, A, lda, ipiv, B, ldb);
   t1 = time00() - t0;
   if (F == NULL || t1 <= 0.0) t1 = mflop = -1.0;
   else mflop = mflop / (t1*1000000.0);

   nrm = geslvtst(Order, N, NRHS, nrm, X, ldb, B, ldb, thresh);
   if (nrm > thresh) ierr=1;

   fprintf(stdout, "  %c     %c %7d %7d %7d %7d %10.3f %9.2f  %e\n",
           cord, 'G', N, NRHS, lda, ldb, t1, mflop, nrm);
   free(ipiv);
   free(A);
   free(X);
   free(B);
   return(ierr);
}

static int DoAllTests(int nrep, int N0, int NN, int incN, int R0, int RN,
                      int incR, int ldagap, int ldbgap, double thresh,
                      int nord, enum CBLAS_ORDER *Orders,
                      int nuplo, int *Uplos, int CS, int MFLOP,
                      int QR)
{
   int i=0, ierr=0, n, r, lda, ldb, io, iu, in;

   fprintf(stdout, "ORD  UPLO       N    NRHS     lda     ldb       TIME     MFLOP        RESID\n");
   fprintf(stdout, "===  ====  ======  ======  ======  ======  =========  ========  ===========\n\n");

   for (n=N0; n <= NN; n += incN)
   {
      lda = n + ldagap;
      ldb = n + ldbgap;
      for (r=R0; r <= RN; r += incR)
      {
         for (io=0; io < nord; io++)
         {
            for (iu=0; iu < nuplo; iu++, i++)
            {
               for (in=0; in < nrep; in++)
               {
                  if (Uplos[iu] == 0)
                     ierr += gesvtst(Orders[io], n, r, lda, ldb, CS, thresh);
                  else if (Uplos[iu] == 1)
                     ierr += gelstst(Orders[io], n, r, lda, ldb, CS, thresh);
                  else
                     ierr += posvtst(Orders[io], Uplos[iu], n, r, lda, ldb,
                                     CS, thresh);
               }
            }
         }
      }
   }
   if (ierr == 0) fprintf(stdout, "\n%d TESTS RUN, ALL PASSED.\n\n", i);
   else fprintf(stdout, "\n%d TESTS RUN, %d FAILED!!\n\n", i, ierr);
   return(ierr);
}
main(int nargs, char **args)
{
   int ierr, N0, NN, incN, R0, RN, incR, ldagap, ldbgap, nord, nuplo;
   int CS, MFLOP, QR, nrep;
   enum CBLAS_ORDER *Order;
   int *Uplo;
   double thresh;
   GetFlags(nargs, args, &N0, &NN, &incN, &R0, &RN, &incR, &ldagap, &ldbgap,
            &thresh, &nord, &Order, &nuplo, &Uplo, &CS, &MFLOP, &QR, &nrep);
   ierr = DoAllTests(nrep, N0, NN, incN, R0, RN, incR, ldagap, ldbgap, thresh,
                     nord, Order, nuplo, Uplo, CS, MFLOP, QR);
   if (Order) free(Order);
   if (Uplo) free(Uplo);
   exit(ierr);
}
