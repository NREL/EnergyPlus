/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2008 R. Clint Whaley
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

#define CAN_NB 32
#include "C_lapack.h"

static int Uplo_LA2ATL(int i)
{
   return( (i & LAUpper) ? AtlasUpper : AtlasLower);
}
static int Side_LA2ATL(int i)
{
   return( (i & LARight) ? AtlasRight : AtlasLeft);
}

#ifdef TimeF77
   #define test_potrf(Uplo_, N_, A_, lda_) \
      ATL_assert(Mjoin(PATL,f77potrf)(Uplo_LA2ATL(Uplo_), N_, A_, lda_) == 0)
#elif defined(TimeC)
   #include "clapack.h"
   #define Cpotrf Mjoin(Mjoin(clapack_,PRE),potrf)
   #define test_potrf(Uplo_, N_, A_, lda_) \
      ATL_assert(Cpotrf(AtlasColMajor, Uplo_LA2ATL(Uplo_), N_, A_, lda_) == 0)
#else
   #define test_potrf(Uplo_, N_, A_, lda_) \
      ATL_assert(ATL_potrf(AtlasColMajor, Uplo_LA2ATL(Uplo_), N_, A_, lda_)==0)
#endif
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
#ifndef TimeF77
   #include "atlas_lapack.h"
#endif
#ifdef TimeF77
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   ATL_assert(!Mjoin(PATL,f77gelqf)(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_gelqf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #include "clapack.h"
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_gelqf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
#ifdef TimeF77
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   ATL_assert(!Mjoin(PATL,f77gerqf)(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_gerqf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #include "clapack.h"
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_gerqf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
#ifdef TimeF77
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   ATL_assert(!Mjoin(PATL,f77geqlf)(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_geqlf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #include "clapack.h"
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_geqlf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
#ifdef TimeF77
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   ATL_assert(!Mjoin(PATL,f77geqrf)(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_geqrf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #include "clapack.h"
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!ATL_geqrf(M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif


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

static FILE *my_fopen(char *name, char *acc)
{
   if (!name)
      return(*acc == 'r' ? stdin : stdout);
   if (!strncmp(name, "stderr", 6))
      return(stderr);
   if (!strncmp(name, "stdout", 6))
      return(stdout);
   if (!strncmp(name, "stdin", 5))
      return(stdin);
   return(fopen(name, acc));
}

static void my_fclose(FILE *fp)
{
   if (fp && fp != stderr && fp != stdout && fp != stdin)
      fclose(fp);
}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);

   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -R <#> <rout1> ... <rout#>\n");
   fprintf(stderr, "      routs: getrf, potrf, geqrf\n");
   fprintf(stderr, "   -F <mflop> : force <mflops> of timed computation\n");
   fprintf(stderr, "   -# <#> : repeat each timing # times\n");
   fprintf(stderr, "   -n <#> <N1> ... <N#>\n");
   fprintf(stderr, "   -N <Nstart> <Nend> <Ninc>\n");
   fprintf(stderr, "   -m <#> <M1> ... <M#>\n");
   fprintf(stderr, "   -M <Mstart> <Mend> <Minc>\n");
   fprintf(stderr, "   -a <ldagap> : lda = M + <ldagap> foreach M\n");
   fprintf(stderr, "   -f <flushKB> : flush at least this mem in LRU timers\n");
   fprintf(stderr, "   -S <#> <side1> ... <side#>\n");
   fprintf(stderr, "   -U <nuplo> <up1> ... <upN> : Vals: [u,l,q,g]\n");
   fprintf(stderr, "   -#t # N1 reps1 ... Nt reps_t: for N >= Nx repeat each timing repsx times\n");
   exit(ierr ? ierr : -1);
}

int *GetIntList1(int ival)
/*
 * returns integer array with iarr[0] = 1, iarr[1] = ival
 */
{
   int *iarr;
   iarr = malloc(2*sizeof(int));
   ATL_assert(iarr);
   iarr[0] = 1;
   iarr[1] = ival;
   return(iarr);
}

int *GetIntList2(int ival1, int ival2)
/*
 * returns integer array with iarr[0] = 1, iarr[1] = ival1, ival[2] = ival2
 */
{
   int *iarr;
   iarr = malloc(3*sizeof(int));
   ATL_assert(iarr);
   iarr[0] = 1;
   iarr[1] = ival1;
   iarr[2] = ival2;
   return(iarr);
}

int *GetIntList(int nargs, char **args, int i, int nmul)
/*
 * Gets a list of integers, whose length is given by atoi(args[i])*nmul
 * list is this length+1, since 0'th location gets atoi(args[i])
 */
{
   int n, *iarr, k;

   if (++i >= nargs)
      PrintUsage(args[0], i, NULL);
   n = atoi(args[i]) * nmul;
   ATL_assert(n > 0);
   iarr = malloc(sizeof(int)*(n+1));
   ATL_assert(iarr);

   iarr[0] = n / nmul;
   for (k=0; k < n; k++)
   {
      if (++i >= nargs)
         PrintUsage(args[0], i, NULL);
      iarr[k+1] = atoi(args[i]);
   }
   return(iarr);
}

int *RoutNames2IntList(int nargs, char **args, int i)
{
   int n, *iarr, k;

   if (++i >= nargs)
      PrintUsage(args[0], i, NULL);
   n = atoi(args[i]);
   ATL_assert(n > 0);
   iarr = malloc(sizeof(int)*(n+1));
   ATL_assert(iarr);

   iarr[0] = n;
   for (k=0; k < n; k++)
   {
      if (++i >= nargs)
         PrintUsage(args[0], i, NULL);
      if (!strcmp(args[i], "getrf") || !strcmp(args[i], "GETRF"))
         iarr[k+1] = LAgetrf;
      else if (!strcmp(args[i], "potrf") || !strcmp(args[i], "POTRF"))
         iarr[k+1] = LApotrf;
      else if (!strcmp(args[i], "geqrf") || !strcmp(args[i], "GEQRF"))
         iarr[k+1] = LAgeqrf;
      else if (!strcmp(args[i], "geqlf") || !strcmp(args[i], "GEQLF"))
         iarr[k+1] = LAgeqrf;
      else if (!strcmp(args[i], "gerqf") || !strcmp(args[i], "GERQF"))
         iarr[k+1] = LAgeqrf;
      else if (!strcmp(args[i], "gelqf") || !strcmp(args[i], "GELQF"))
         iarr[k+1] = LAgeqrf;
      else
         PrintUsage(args[0], i, args[i]);
   }
   return(iarr);
}

int *IntRange2IntList(int N0, int NN, int incN)
{
   int i, n;
   int *iarr;

   for (i=N0, n=0; i <= NN; i += incN) n++;
   iarr = malloc(sizeof(int)*(n+1));
   ATL_assert(iarr);
   iarr[0] = n;
   for (i=N0, n=1 ; i <= NN; i += incN, n++)
      iarr[n] = i;
   return(iarr);
}

void GetFlags(int nargs, char **args, int **nreps, int *flsizeKB, int *mflop,
              int **ROUTs, int *ldagap, int **Ns, int **Ms, int **UPLOs,
              int **SDs)
{
   int *NBs=NULL, *ns=NULL, *ms=NULL, *ups=NULL, *sds=NULL, *ip;
   int i, k, n;

   *ROUTs = NULL;
   *ldagap = 0;
   *flsizeKB = L2SIZE/1024;
   *nreps = NULL;
   *mflop = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'n':         /* -n  or -nb */
         ns = GetIntList(nargs, args, i, 1);
         i += ns[0] + 1;
         break;
      case 'm':         /* -m # <M1> ... <M#> */
         ms = GetIntList(nargs, args, i, 1);
         i += ms[0] + 1;
         break;
      case 'N':         /* -N or -NB */
      case 'M':                         /* -M <Mstart> <Mend> <Minc>\n"); */
         if (i+3 >= nargs)
            PrintUsage(args[0], i, NULL);
         ip = IntRange2IntList(atoi(args[i+1]),atoi(args[i+2]),atoi(args[i+3]));
         if (args[i][0] == 'M')
            ms = ip;
         else                           /* -N <Nstart> <Nend> <Ninc>\n"); */
            ns = ip;
         i += 3;
         break;
      case 'R':        /* -R # <rout1> ... <routN#>  */
         *ROUTs = RoutNames2IntList(nargs, args, i);
         i += (*ROUTs)[0] + 1;
         break;
      case '#':                         /* set nreps */
         if (args[i][2] == 't')         /* -#t N1 reps1 ... Nt repst */
         {
            *nreps = GetIntList(nargs, args, i, 2);
            i += ((*nreps)[0] << 1) + 1;
         }
         else                           /* -# <reps> */
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            *nreps = GetIntList2(0, atoi(args[i]));
         }
         break;
      case 'f':                         /* -f <flushKB> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *flsizeKB = atoi(args[i]);
         break;
      case 'F':                         /* -F <mflop> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *mflop = atoi(args[i]);
         break;
      case 'U':                         /* -U <nup> <u1> ... <uN>;[u,l,q,g] */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         n = atoi(args[i]);
         ATL_assert(n > 0);
         ups = malloc(sizeof(int)*(n+1));
         ups[0] = n;
         for (k=0; k < n; k++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            switch(args[i][0])
            {
            case 'U':
            case 'u':
               ups[k+1] = LAUpper;
               break;
            case 'l':
            case 'L':
            default:
               ups[k+1] = LALower;
               break;
            }
         }
         break;
      case 'S':                         /* -S <#> <side1> ... <sideN> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         n = atoi(args[i]);
         ATL_assert(n > 0);
         sds = malloc(sizeof(int)*(n+1));
         sds[0] = n;
         for (k=0; k < n; k++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            switch(args[i][0])
            {
            case 'L':
            case 'l':
               sds[k+1] = LALeft;
               break;
            default:
               sds[k+1] = LARight;
               break;
            }
         }
         break;
      case 'a':                         /* -a <ldagap> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *ldagap = atoi(args[i]);
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
/*
 * Take default values
 */
   if (!(*ROUTs))
      *ROUTs = GetIntList1(LAgeqrf);
   if (!(*nreps))
      *nreps = GetIntList2(0, 1);
   if (!ns)
      ns = GetIntList1(1000);
   if (!ms)
      ms = GetIntList1(0);
   if (!ups)
      ups = GetIntList1(LAUpper);
   if (!sds)
      sds = GetIntList1(LARight);

   *Ns = ns;
   *Ms = ms;
   *UPLOs = ups;
   *SDs = sds;
}

double GetFlopCount(enum ATL_LAROUT rout, enum ATL_LAFLG flags,
                    int M, int N, int KL, int KU, int nb)
/*
 * These numbers copied from LAPACK timer routines TIMING/[EIG,LIN]/dopla[2].f
 */
{
    double m=(M?M:N), n=N, kl=KL, ku=ku, mn;
    double adds=0.0, muls=0.0;

    if (rout & LAgetrf)
    {
       mn = (m >= n) ? n : m;   /* mn = MIN(M,N) */
       adds = mn * ( m*n-(m+n)*(mn+1.0)/2.0 + (mn+1.0)*(2.0*mn+1.0)/6.0 );
       muls = adds + mn * ( m-(mn+1.0)/2.0 );
    }
    else if (rout & LAgeqrf)
    {
       if (flags & LARight)  /* LAgeqrf || LAgeqlf */
       {
          if (M >= N)
          {
             muls = n*( ((23.0/6.0)+m+(n/2.0)) + n*(m-(n/3.0)) );
             adds = n*( (5.0/6.0) + n*((1.0/2.0) + (m-(n/3.0))) );
          }
          else
          {
             muls = m*( ((23.0/6.0) + 2.0*n - (m/2.0)) + m*(n-(m/3.0)) );
             adds = m*( (5.0/6.0) + n - (m/2.0) + m*(n-(m/3.0)) );
          }
       }
       else  /* LAgerqf || LAgelqf */
       {
          if (M >= N)
          {
             muls = n*( ((29.0/6.0) + m + n/2.0) + n*(m-n/3.0) );
             adds = n*( (5.0/6.0) + m + n*(-0.5 + (m - n/3.0)) );
          }
          else
          {
             muls = m*( ((29.0/6.0) + 2.0*n - 0.5*m) +m*(n - m/3.0) );
             adds = m*( (5.0/6.0) + 0.5*m + m*(n - m/3.0) );
          }
       }
    }
    else if (rout & LApotrf)
    {
       muls = m*( (1.0/3.0) + m*((1.0/2.0) + (m/6.0)) );
       adds = (1.0/6.0)*m*(-1.0+m*m);
    }
    #if defined (SCPLX) || defined(DCPLX)
       return(2.0*adds + 6.0*muls);
    #else
       return(adds+muls);
    #endif
}


double Time2Flops(int rout, int UPLO, int M, int N, double time)
{
   double mflop;
   mflop = GetFlopCount(rout, UPLO, M, N, 0, 0, CAN_NB);
   if (mflop > 0)
      mflop /= time*1e6;  /* translate flops & time to MFLOPS */
   return(mflop);
}

double GetTimeWithReps_LU
   (int mflopF, int lda, int M, int N, int nb, int Uplo, int Side, int flsizeKB)
{
   double mflop, t0, t1, drep;
   char *wrksets;       /* working sets for kernel calls */
#ifdef TCPLX
   const int lda2 = lda+lda;
#else
   const int lda2 = lda;
#endif
   size_t setsz, setszT;   /* work set size in memory, and amnt of it touched */
   size_t nrep;            /* # of reps required to force mflopF flops */
   size_t nset;            /* # of working sets allocated */
   int i;
/*
 * Keep setsz a multiple of TYPE size for alignment reasons.  LU only accesses
 * M*N of matrix and all of IPIV.
 */
   setsz = lda*N*ATL_sizeof +
           ((M*sizeof(int)+ATL_sizeof-1)/ATL_sizeof)*ATL_sizeof;
   setszT = M*N*ATL_sizeof + M*sizeof(int);
   mflop = GetFlopCount(LAgetrf, 0, M, N, 0, 0, CAN_NB);
/*
 * Cannot reuse matrices (bogus to factor an already factored matrix), so we
 * must take as our total memspace MAX(nrep,nset)*setsz
 */
   ATL_assert(mflop > 0.0);
   drep = (mflopF*1.0e6) / mflop;
   nrep = (int)(drep+0.999999);
/*
 * If cacheline flush doesn't work, then we must use this method
 */
   #if ATL_LINEFLUSH
      if (nrep < 2)
         return(-1.0);                                /* do wt normal timer */
   #else
      nrep = (nrep >= 1) ? nrep : 1;
   #endif

   nset = (flsizeKB*1024+setszT-1)/setszT;
   if (nset < nrep)
      nset = nrep;
   wrksets = malloc(nset * setsz);
   ATL_assert(wrksets);

   for (i=0; i < nset; i++)
      Mjoin(PATL,gegen)(M, N, (TYPE*)(wrksets+i*setsz), lda, M*N+lda);

   t0 = time00();
   for (i=0; i < nrep; i++)
   {
      test_getrf(CblasColMajor, M, N, (TYPE*)(wrksets+i*setsz), lda,
                 (int*)(wrksets+i*setsz+lda*N*ATL_sizeof));
   }
   t1 = time00();
   free(wrksets);

   return((t1-t0)/((double)nrep));
}

double GetTimeWithReps_LLT
   (int mflopF, int lda, int M, int N, int nb, int Uplo, int Side, int flsizeKB)
{
   double mflop, t0, t1, drep;
   char *wrksets;       /* working sets for kernel calls */
#ifdef TCPLX
   const int lda2 = lda+lda;
#else
   const int lda2 = lda;
#endif
   size_t setsz, setszT;   /* work set size in memory, and amnt of it touched */
   size_t nrep;            /* # of reps required to force mflopF flops */
   size_t nset;            /* # of working sets allocated */
   int i;
   setsz=lda*N*ATL_sizeof;   /* matrix is entire working set of LLt */
   setszT=N*N*ATL_sizeof;    /* only touch N*N portion */
   mflop = GetFlopCount(LApotrf, Uplo, M, N, 0, 0, CAN_NB);
/*
 * Cannot reuse matrices (bogus to factor an already factored matrix), so we
 * must take as our total memspace MAX(nrep,nset)*setsz
 */
   ATL_assert(mflop > 0.0);
   drep = (mflopF*1.0e6) / mflop;
   nrep = (int)(drep+0.999999);
/*
 * If cacheline flush doesn't work, then we must use this method
 */
   #if ATL_LINEFLUSH
      if (nrep < 2)
         return(-1.0);                                /* do wt normal timer */
   #else
      nrep = (nrep >= 1) ? nrep : 1;
   #endif

   nset = (flsizeKB*1024+setszT-1)/setszT;
   if (nset < nrep)
      nset = nrep;
   wrksets = malloc(nset * setsz);
   ATL_assert(wrksets);

   for (i=0; i < nset; i++)
      PosDefGen(CblasColMajor, Uplo_LA2ATL(Uplo), N,
                (TYPE*)(wrksets+i*setsz), lda);

   t0 = time00();
   for (i=0; i < nrep; i++)
   {
      test_potrf(Uplo, N, (TYPE*)(wrksets+i*setsz), lda);
   }
   t1 = time00();
   free(wrksets);

   return((t1-t0)/((double)nrep));
}

double GetTimeWithReps_LQ
   (int mflopF, int lda, int M, int N, int nb, int Uplo, int Side, int flsizeKB)
{
   double mflop, t0, t1, drep;
   TYPE dtmp, dtmp1;
   char *wrksets;       /* working sets for kernel calls */
#ifdef TCPLX
   const int lda2 = lda+lda;
#else
   const int lda2 = lda;
#endif
   size_t setsz, setszT;   /* work set size in memory, and amnt of it touched */
   size_t nrep;            /* # of reps required to force mflopF flops */
   size_t nset;            /* # of working sets allocated */
   int wlen;            /* length of QR's workspace */
   int i;
/*
 * Figure out how much workspace is required, and allocate it
 */
   test_gelqf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);
   wlen = dtmp;
/*
 * QR accesses matrix, Min(M,N)-length tau & workspace, but for flush purposes
 * be conservative and say it only accesses A
 */
   setsz = (lda*N + wlen + Mmin(M,N)) * ATL_sizeof;
   setszT = M*N*ATL_sizeof;

   mflop = GetFlopCount(LAgeqrf, LALeft+LALower, M, N, 0, 0, CAN_NB);
/*
 * Cannot reuse matrices (bogus to factor an already factored matrix), so we
 * must take as our total memspace MAX(nrep,nset)*setsz
 */
   ATL_assert(mflop > 0.0);
   drep = (mflopF*1.0e6) / mflop;
   nrep = (int)(drep+0.999999);
/*
 * If cacheline flush doesn't work, then we must use this method
 */
   #if ATL_LINEFLUSH
      if (nrep < 2)
         return(-1.0);                                /* do wt normal timer */
   #else
      nrep = (nrep >= 1) ? nrep : 1;
   #endif

   nset = (flsizeKB*1024+setszT-1)/setszT;
   if (nset < nrep)
      nset = nrep;
   wrksets = malloc(nset * setsz);
   ATL_assert(wrksets);

   for (i=0; i < nset; i++)
      Mjoin(PATL,gegen)(M, N, (TYPE*)(wrksets+i*setsz), lda, M*N+lda);

   t0 = time00();
   for (i=0; i < nrep; i++)
   {
      test_gelqf(CblasColMajor, M, N, (TYPE*)(wrksets+i*setsz), lda,
                 (TYPE*)(wrksets+i*setsz+(N*lda+wlen)*ATL_sizeof),
                 (TYPE*)(wrksets+i*setsz+N*lda*ATL_sizeof), wlen);
   }
   t1 = time00();
   free(wrksets);

   return((t1-t0)/((double)nrep));
}

double GetTimeWithReps_RQ
   (int mflopF, int lda, int M, int N, int nb, int Uplo, int Side, int flsizeKB)
{
   double mflop, t0, t1, drep;
   TYPE dtmp, dtmp1;
   char *wrksets;       /* working sets for kernel calls */
#ifdef TCPLX
   const int lda2 = lda+lda;
#else
   const int lda2 = lda;
#endif
   size_t setsz, setszT;   /* work set size in memory, and amnt of it touched */
   size_t nrep;            /* # of reps required to force mflopF flops */
   size_t nset;            /* # of working sets allocated */
   int wlen;            /* length of QR's workspace */
   int i;
/*
 * Figure out how much workspace is required, and allocate it
 */
   test_gerqf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);
   wlen = dtmp;
/*
 * QR accesses matrix, Min(M,N)-length tau & workspace, but for flush purposes
 * be conservative and say it only accesses A
 */
   setsz = (lda*N + wlen + Mmin(M,N)) * ATL_sizeof;
   setszT = M*N*ATL_sizeof;

   mflop = GetFlopCount(LAgeqrf, LALeft+LAUpper, M, N, 0, 0, CAN_NB);
/*
 * Cannot reuse matrices (bogus to factor an already factored matrix), so we
 * must take as our total memspace MAX(nrep,nset)*setsz
 */
   ATL_assert(mflop > 0.0);
   drep = (mflopF*1.0e6) / mflop;
   nrep = (int)(drep+0.999999);
/*
 * If cacheline flush doesn't work, then we must use this method
 */
   #if ATL_LINEFLUSH
      if (nrep < 2)
         return(-1.0);                                /* do wt normal timer */
   #else
      nrep = (nrep >= 1) ? nrep : 1;
   #endif

   nset = (flsizeKB*1024+setszT-1)/setszT;
   if (nset < nrep)
      nset = nrep;
   wrksets = malloc(nset * setsz);
   ATL_assert(wrksets);

   for (i=0; i < nset; i++)
      Mjoin(PATL,gegen)(M, N, (TYPE*)(wrksets+i*setsz), lda, M*N+lda);

   t0 = time00();
   for (i=0; i < nrep; i++)
   {
      test_gerqf(CblasColMajor, M, N, (TYPE*)(wrksets+i*setsz), lda,
                 (TYPE*)(wrksets+i*setsz+(N*lda+wlen)*ATL_sizeof),
                 (TYPE*)(wrksets+i*setsz+N*lda*ATL_sizeof), wlen);
   }
   t1 = time00();
   free(wrksets);

   return((t1-t0)/((double)nrep));
}

double GetTimeWithReps_QL
   (int mflopF, int lda, int M, int N, int nb, int Uplo, int Side, int flsizeKB)
{
   double mflop, t0, t1, drep;
   TYPE dtmp, dtmp1;
   char *wrksets;       /* working sets for kernel calls */
#ifdef TCPLX
   const int lda2 = lda+lda;
#else
   const int lda2 = lda;
#endif
   size_t setsz, setszT;   /* work set size in memory, and amnt of it touched */
   size_t nrep;            /* # of reps required to force mflopF flops */
   size_t nset;            /* # of working sets allocated */
   int wlen;            /* length of QR's workspace */
   int i;
/*
 * Figure out how much workspace is required, and allocate it
 */
   test_geqlf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);
   wlen = dtmp;
/*
 * QR accesses matrix, Min(M,N)-length tau & workspace, but for flush purposes
 * be conservative and say it only accesses A
 */
   setsz = (lda*N + wlen + Mmin(M,N)) * ATL_sizeof;
   setszT = M*N*ATL_sizeof;

   mflop = GetFlopCount(LAgeqrf, LARight+LALower, M, N, 0, 0, CAN_NB);
/*
 * Cannot reuse matrices (bogus to factor an already factored matrix), so we
 * must take as our total memspace MAX(nrep,nset)*setsz
 */
   ATL_assert(mflop > 0.0);
   drep = (mflopF*1.0e6) / mflop;
   nrep = (int)(drep+0.999999);
/*
 * If cacheline flush doesn't work, then we must use this method
 */
   #if ATL_LINEFLUSH
      if (nrep < 2)
         return(-1.0);                                /* do wt normal timer */
   #else
      nrep = (nrep >= 1) ? nrep : 1;
   #endif

   nset = (flsizeKB*1024+setszT-1)/setszT;
   if (nset < nrep)
      nset = nrep;
   wrksets = malloc(nset * setsz);
   ATL_assert(wrksets);

   for (i=0; i < nset; i++)
      Mjoin(PATL,gegen)(M, N, (TYPE*)(wrksets+i*setsz), lda, M*N+lda);

   t0 = time00();
   for (i=0; i < nrep; i++)
   {
      test_geqlf(CblasColMajor, M, N, (TYPE*)(wrksets+i*setsz), lda,
                 (TYPE*)(wrksets+i*setsz+(N*lda+wlen)*ATL_sizeof),
                 (TYPE*)(wrksets+i*setsz+N*lda*ATL_sizeof), wlen);
   }
   t1 = time00();
   free(wrksets);

   return((t1-t0)/((double)nrep));
}

double GetTimeWithReps_QR
   (int mflopF, int lda, int M, int N, int nb, int Uplo, int Side, int flsizeKB)
{
   double mflop, t0, t1, drep;
   TYPE dtmp, dtmp1;
   char *wrksets;       /* working sets for kernel calls */
#ifdef TCPLX
   const int lda2 = lda+lda;
#else
   const int lda2 = lda;
#endif
   size_t setsz, setszT;   /* work set size in memory, and amnt of it touched */
   size_t nrep;            /* # of reps required to force mflopF flops */
   size_t nset;            /* # of working sets allocated */
   int wlen;            /* length of QR's workspace */
   int i;
/*
 * Figure out how much workspace is required, and allocate it
 */
   test_geqrf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);
   wlen = dtmp;
/*
 * QR accesses matrix, Min(M,N)-length tau & workspace, but for flush purposes
 * be conservative and say it only accesses A
 */
   setsz = (lda*N + wlen + Mmin(M,N)) * ATL_sizeof;
   setszT = M*N*ATL_sizeof;

   mflop = GetFlopCount(LAgeqrf, LARight+LAUpper, M, N, 0, 0, CAN_NB);
/*
 * Cannot reuse matrices (bogus to factor an already factored matrix), so we
 * must take as our total memspace MAX(nrep,nset)*setsz
 */
   ATL_assert(mflop > 0.0);
   drep = (mflopF*1.0e6) / mflop;
   nrep = (int)(drep+0.999999);
/*
 * If cacheline flush doesn't work, then we must use this method
 */
   #if ATL_LINEFLUSH
      if (nrep < 2)
         return(-1.0);                                /* do wt normal timer */
   #else
      nrep = (nrep >= 1) ? nrep : 1;
   #endif

   nset = (flsizeKB*1024+setszT-1)/setszT;
   if (nset < nrep)
      nset = nrep;
   wrksets = malloc(nset * setsz);
   ATL_assert(wrksets);

   for (i=0; i < nset; i++)
      Mjoin(PATL,gegen)(M, N, (TYPE*)(wrksets+i*setsz), lda, M*N+lda);

   t0 = time00();
   for (i=0; i < nrep; i++)
   {
      test_geqrf(CblasColMajor, M, N, (TYPE*)(wrksets+i*setsz), lda,
                 (TYPE*)(wrksets+i*setsz+(N*lda+wlen)*ATL_sizeof),
                 (TYPE*)(wrksets+i*setsz+N*lda*ATL_sizeof), wlen);
   }
   t1 = time00();
   free(wrksets);

   return((t1-t0)/((double)nrep));
}


double GetTime(int rout, int mflopF, int lda, int M, int N, int nb, int Uplo,
               int Side, int flsizeKB)
{
#if ATL_LINEFLUSH
   FLSTRUCT *flp;
#endif
   TYPE *A, *wrk=NULL, dtmp, dtmp1, *tau=NULL;
   int *ipiv=NULL, itmp, wlen;
   double t0, t1;
/*
 * Call routs that force particular flop count if requested; they return -1.0
 * if one invocation will suffice to force mflopF, in which case do the timing
 * in this routine, which is simpler & doesn't require LRU & as much workspace
 * If we don't have the ability to do cacheline flushing, must use LRU rout!
 */
#if ATL_LINEFLUSH
   if (mflopF > 0)
   {
#endif
      if (rout == LApotrf)
         t1 = GetTimeWithReps_LLT(mflopF, lda, M, N, nb, Uplo, Side, flsizeKB);
      else if (rout == LAgeqrf)
      {
         if (Side == LARight)
         {
            if (Uplo == LAUpper)
               t1 = GetTimeWithReps_QR(mflopF, lda, M, N, nb, Uplo, Side,
                                       flsizeKB);
            else
               t1 = GetTimeWithReps_QL(mflopF, lda, M, N, nb, Uplo, Side,
                                       flsizeKB);
         }
         else if (Uplo == LAUpper)
            t1 = GetTimeWithReps_RQ(mflopF, lda, M, N, nb, Uplo, Side,
                                    flsizeKB);
         else
            t1 = GetTimeWithReps_LQ(mflopF, lda, M, N, nb, Uplo, Side,
                                    flsizeKB);
      }
      else
         t1 = GetTimeWithReps_LU(mflopF, lda, M, N, nb, Uplo, Side, flsizeKB);
#if ATL_LINEFLUSH == 0
      return(t1);
#else
      if (t1 >= 0.0)
         return(t1);
   }
#endif
#if ATL_LINEFLUSH != 0
/*
 * Generate operands
 */
   A = GetGE(M, N, lda);
   ATL_assert(A);
   flp = ATL_GetFlushStruct(A, N*lda*ATL_sizeof, NULL);
   if (rout == LApotrf)
      PosDefGen(CblasColMajor, Uplo_LA2ATL(Uplo), N, A, lda);
   else if (rout & LAgeqrf)
   {                            /* QR must allocate workspace */
      if (Side == LARight)
      {
         if (Uplo == LAUpper)
         {
            test_geqrf(CblasColMajor, M, N, A, lda, &dtmp1, &dtmp, -1);
         }
         else
         {
            test_geqlf(CblasColMajor, M, N, A, lda, &dtmp1, &dtmp, -1);
         }
      }
      else if (Uplo == LAUpper)
      {
         test_gerqf(CblasColMajor, M, N, A, lda, &dtmp1, &dtmp, -1);
      }
      else
      {
         test_gelqf(CblasColMajor, M, N, A, lda, &dtmp1, &dtmp, -1);
      }
      wlen = dtmp;
      wrk = calloc(wlen, ATL_sizeof);
      ATL_assert(wrk);
      flp = ATL_GetFlushStruct(wrk, wlen*ATL_sizeof, flp);
      itmp = (M >= N) ? M : N;
      tau = calloc(itmp, ATL_sizeof);
      flp = ATL_GetFlushStruct(tau, itmp*ATL_sizeof, flp);
   }
   else
   {
      ipiv = calloc(M, sizeof(int));
      ATL_assert(ipiv);
      flp = ATL_GetFlushStruct(ipiv, M*sizeof(int), flp);
   }
/*
 * Flush cache, and do timing
 */
   ATL_FlushAreasByCL(flp);
   if (rout == LApotrf)
   {
      t0 = time00();
      test_potrf(Uplo, N, A, lda);
      t1 = time00();
   }
   else if (rout == LAgeqrf)
   {
      if (Side == LARight)
      {
         if (Uplo == LAUpper)
         {
            t0 = time00();
            test_geqrf(CblasColMajor, M, N, A, lda, tau, wrk, wlen);
            t1 = time00();
         }
         else
         {
            t0 = time00();
            test_geqlf(CblasColMajor, M, N, A, lda, tau, wrk, wlen);
            t1 = time00();
         }
      }
      else if (Uplo == LAUpper)
      {
         t0 = time00();
         test_gerqf(CblasColMajor, M, N, A, lda, tau, wrk, wlen);
         t1 = time00();
      }
      else
      {
         t0 = time00();
         test_gelqf(CblasColMajor, M, N, A, lda, tau, wrk, wlen);
         t1 = time00();
      }
   }
   else
   {
      t0 = time00();
      test_getrf(CblasColMajor, M, N, A, lda, ipiv);
      t1 = time00();
   }
   if (tau)
      free(tau);
   if (wrk)
      free(wrk);
   if (ipiv)
      free(ipiv);
   free(A);
   ATL_KillAllFlushStructs(flp);
   return(t1 - t0);
#endif
}

static char Uplo2Char(enum ATL_LAROUT rout, enum ATL_LAFLG flags)
{
   switch (rout)
   {
   case LAgeqrf:
      if (flags & LARight)
         return('Q');
      return( (flags & LAUpper) ? 'R' : 'L');
   case LApotrf:
      return( (flags & LAUpper) ? 'U' : 'L' );
   default:
      return('G');
   }
}

static char Side2Char(enum ATL_LAROUT rout, enum ATL_LAFLG i)
{
   if (rout == LAgeqrf)
   {
      if (i & LARight) return( (i & LAUpper) ? 'R' : 'L');
      else return('Q');
   }
   return( (i & LARight) ? 'R' : 'L');
}

static int Flags2Bitmap(int Rout, int Upper, int Side)
{
   return((Rout<<8)|Upper|Side);
}
enum FLGSEL {FSUplo, FSSide, FSRout};

char *Bitmap2Char(enum FLGSEL wch, int flags)
{
   int rout;

   switch(wch)
   {
   case FSUplo:
      if (flags & LAUpper)
         return("UPPER");
      else if (flags & LALower)
         return("LOWER");
      else
         return("GEREC");
   case FSSide:
      if (flags & LARight)
         return("RIGHT");
      else if (flags & LALeft)
         return("LEFT");
      else
         return("N/A");
   case FSRout:
      rout = flags >> 8;
      if (rout & LApotrf)
         return("POTRF");
      else if (rout & LAgetrf)
         return("GETRF");
      else if (rout & LAgeqrf)
      {
         if (flags & LARight)
         {
            if (flags & LALower)
               return("GEQLF");
            else
               return("GEQRF");
         }
         else if (flags & LAUpper)
            return("GERQF");
         else
            return("GELQF");
      }
   }
   return("UNKNOWN");
}

char *NameExcludingPath(char *path)
{
   char *sp=path;
   int i;
   for (i=0; path[i]; i++)
      if (path[i] == '/')
         sp = path+i+1;
   return(sp);
}

int UseN(int N, int *Ms, int *Ns)
/*
 * RETURNS: 0 if all Ns are same value, 1 else
 */
{
   int i;
   for (i=0; i < N-1 && Ns[i] == Ns[i+1]; i++);
   return(Ns[i] != Ns[i+1]);
}

void PrintIntArr(char *name, int N, int *iarr)
{
   int i;
   if (N > 0)
   {
      fprintf(stdout, "%s = %d", name, iarr[0]);
      for (i=1; i < N; i++)
         fprintf(stdout, ", %d", iarr[i]);
      fprintf(stdout, "\n");
   }
   else
      fprintf(stderr, "%s has no entries!\n", name);
}
int GetMyReps(int N, int *nreps)
/*
 * Finds the correct nreps for this N
 */
{
   int n, i;

   n = *nreps++;
   for (i=n+n-2; i>=0; i -= 2)
   {
      if (N >= nreps[i])
         return(nreps[i+1]);
   }
   return(nreps[1]);
}

void GoToTown(int *nreps, int flsizeKB, int mflopF, int ldagap,  int rout,
              int *Ns, int *Ms, int *UPLOs, int *SDs)
{
   FILE *fpout=stdout;
   double time, mflop, mfB;
   int *nbs, *flgs, *ms, *ns;
   int itst=0, lda, n, m, u, s, b, r, M, kk, nb0, nrep;

   fprintf(fpout, "***     TUNING FOR %10s ***\n",
           Bitmap2Char(FSRout, (rout<<8)+UPLOs[1]+SDs[1]));
   fprintf(fpout, "*********************************\n");
   fprintf(fpout, "TST     REP  UP  SD       M       N     LDA           TIME          MFLOP\n");
   fprintf(fpout, "======  ===  ==  ==  ======  ======  ======  =============  =============\n");

   for (n=1; n <= Ns[0]; n++)
   {
      for (m=1; m <= Ms[0]; m++)
      {
         M = (Ms[m]) ? Ms[m]:Ns[n];
         for (u=1; u <= UPLOs[0]; u++)
         {
            for (s=1; s <= SDs[0]; s++)
            {
               lda = ldagap + M;
               nrep = GetMyReps(Mmin(M,Ns[n]), nreps);
               for (r=1; r <= nrep; r++)
               {
                  time = GetTime(rout, mflopF, lda, M, Ns[n], CAN_NB,
                                 UPLOs[u], SDs[s], flsizeKB);
                  mflop = Time2Flops(rout, UPLOs[u], M, Ns[n], time);
                  fprintf(fpout,
                          "%6d %4d   %c   %c %7d %7d %7d   %13e %14.2f\n",
                           itst++, r, Uplo2Char(rout, UPLOs[u]+SDs[s]),
                           Side2Char(rout, SDs[s]+UPLOs[u]),
                           M, Ns[n], lda, time, mflop);
               }                                /* end of reps loop */
            }                                   /* end of Side loop */
         }                                      /* end of Uplo loop */
      }                                         /* end of M loop */
   }                                            /* end of N loop */
}

int main(int nargs, char **args)
{
   int *Ns, *Ms, *UPLOs, *SDs, *ROUTs, *nreps;
   int flsizeKB, mflop, ldagap, r;

   GetFlags(nargs, args, &nreps, &flsizeKB, &mflop, &ROUTs, &ldagap,
            &Ns, &Ms, &UPLOs, &SDs);
   for (r=1; r <= ROUTs[0]; r++)
      GoToTown(nreps, flsizeKB, mflop, ldagap, ROUTs[r], Ns, Ms, UPLOs, SDs);
   return(0);
}
