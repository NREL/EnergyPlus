/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 2001 Peter Soendergaard
 *
 * Code contributers : Peter Soendergaard, R. Clint Whaley
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
#include "atlas_aux.h"

#ifdef GCCWIN
   ___main(){} __main(){} MAIN__(){} _MAIN_(){}
#endif

double time00();

#if 1
#define PADVAL ATL_typify(-973200000.0)
#else
#define PADVAL ATL_rzero
#endif

#ifdef TimeF77
   #define test_trtri(Order_, Uplo_, Diag_, N_, A_, lda_) \
      ATL_assert(Mjoin(PATL,f77trtri)(Uplo_, Diag_, N_, A_, lda_) == 0)
#elif defined(TimeC)
   #include "clapack.h"
   #define Ctrtri Mjoin(Mjoin(clapack_,PRE),trtri)
   #define test_trtri(Order_, Uplo_, Diag_, N_, A_, lda_) \
      ATL_assert(Ctrtri(Order_, Uplo_, Diag_, N_, A_, lda_) == 0)
#else
   #define test_trtri(Order_, Uplo_, Diag_, N_, A_, lda_) \
      ATL_assert(ATL_trtri(Order_, Uplo_, Diag_, N_, A_, lda_) == 0)
#endif

static void ATL_checkpad(enum ATLAS_ORDER Order,
                         enum ATLAS_UPLO Uplo,
                         enum ATLAS_DIAG Diag,
			 int N, TYPE *A, int lda)
{
   const int lda2 = lda SHIFT, N2 = N SHIFT;
   int i, j, k, skipdiag;
   TYPE *a;
   enum ATLAS_UPLO use_uplo;

   a = A;

   if (Order==AtlasColMajor)
     use_uplo=Uplo;
   else
   {
     if (Uplo==AtlasUpper)
        use_uplo=AtlasLower;
     else
        use_uplo=AtlasUpper;
   }

   /* Determines if the diagonal should be checked for overwrites */
   if (Diag == AtlasNonUnit)
     skipdiag = 1;
   else
     skipdiag = 0;

   if (use_uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
      {
         for (i=(j+skipdiag) SHIFT; i < N2; i++)
         {
            if (a[i] != PADVAL)
	    {
            #ifdef TREAL
               fprintf(stderr, "   OVERWRITE at A(%d,%d) of %f!!!\n",
                       i+1, j+1, a[i]);
            #else /* possibly generates 2 warning per element */
               if (i % 2)
               {
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i-1], a[i]);
               }
               else
               {
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i], a[i+1]);
               }
            #endif
            }
         }
         a += lda2;
      }
   }
   else
   {
      for (j=0; j < N; j++)
      {
         k = (j+1-skipdiag) SHIFT;
         for (i=0; i < k; i++)
         {
            if (a[i] != PADVAL)
            {
            #ifdef TREAL
               fprintf(stderr, "   OVERWRITE at A(%d,%d) of %f!!!\n",
                       i, j, a[i]);
            #else /* possibly generates 2 warning per element */
               if (i % 2)
               {
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i-1], a[i]);
               }
               else
               {
                  fprintf(stderr, "   OVERWRITE at A(%d,%d) of (%f,%f)!!!\n",
                          i/2, j, a[i], a[i+1]);
               }
            #endif
            }
         }
         a += lda2;
      }
   }
}

/* Calculates 1-nrm of triangolar matrix. If Diag=AtlasUnit then the
   diagonal will be excluded from the calculation. */
static TYPE trinrm1(enum ATLAS_ORDER Order,
                    enum ATLAS_UPLO Uplo,
                    enum ATLAS_DIAG Diag,
		    int N, TYPE *A, int lda)
{
   const int lda2 = lda SHIFT;
   int j, skipdiag;
   TYPE max=0.0, t0;
   enum ATLAS_UPLO use_uplo;

   if (Order==AtlasColMajor)
     use_uplo=Uplo;
   else
   {
     if (Uplo==AtlasUpper)
        use_uplo=AtlasLower;
     else
        use_uplo=AtlasUpper;
   }

   if (Diag == AtlasNonUnit)
     skipdiag = 0;
   else
     skipdiag = 1;

   if (use_uplo == AtlasUpper)
     {
       for (j=0; j < N; j++)
	 {
	   t0 = Mjoin(PATL,asum)(j+1-skipdiag, A, 1);
	   if (t0 > max) max = t0;
	   A += lda2;
	 }
     }
   else
     {
       for (j=0; j < N; j++)
	 {
	   t0 = Mjoin(PATL,asum)(N-j-skipdiag, A+((j+skipdiag)SHIFT), 1);
	   if (t0 > max) max = t0;
	   A += lda2;
	 }
     }

   return(max);
}

void trddom
(
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_UPLO      Diag,
   const int                  N,
   TYPE                       * A,
   const int                  LDA
)
{
/*
 * Scale strictly lower (resp. upper) part of triangular matrix by 1 / N
 * to make it diagonally dominant.
 */
   int                        i, iaij, j, jaj, lda2 = ( LDA SHIFT ),
                              ldap12 = (( LDA + 1 ) SHIFT);
   TYPE                       alpha;
   TYPE                       smallest;


   if( N <= 0 ) return;

   /* 0.9 is a safety factor for making sure it is really
      diagonally dominant. */
   alpha = ATL_rone * 0.9 / (TYPE)(N);

   /* Find smallest element on the diagonal */

   if (Diag == AtlasNonUnit)
   {
#ifdef TCPLX
        smallest=Mmin(Mabs(A[0]),Mabs(A[1]));
#else
        smallest=Mabs(A[0]);
#endif
	for (i=1;i<N;i++)
	{
#ifdef TCPLX
           smallest=Mmin(smallest,Mmin(Mabs(A[i*ldap12]),Mabs(A[1+i*ldap12])));
#else
           smallest=Mmin(smallest,Mabs(A[i*ldap12]));
#endif
	}
      alpha *= smallest;
   }

   if( UPLO == AtlasUpper )
   {
      for( j = 0, jaj = 0; j < N; j++, jaj += lda2 )
      {
         for( i = 0, iaij = jaj; i < j; i++, iaij += (1 SHIFT) )
         {
            A[iaij  ] *= alpha;
#ifdef TCPLX
            A[iaij+1] *= alpha;
#endif
         }
      }
   }
   else
   {
      for( j = N-1, jaj = (N-1)*ldap12; j >= 0; j--, jaj -= ldap12 )
      {
         for( i = j+1, iaij = jaj+(1 SHIFT); i < N; i++, iaij += (1 SHIFT) )
         {
            A[iaij  ] *= alpha;
#ifdef TCPLX
            A[iaij+1] *= alpha;
#endif
         }
      }
   }
}


static void trigen(enum ATLAS_ORDER Order,
                   enum ATLAS_UPLO Uplo,
                   enum ATLAS_DIAG Diag,
		   int N, TYPE *A, int lda, const TYPE padval, int seed)
{
   const int lda2 = lda SHIFT;
   int j, skipdiag;
   enum ATLAS_UPLO use_uplo;

   if (Order==AtlasColMajor)
     use_uplo=Uplo;
   else
   {
     if (Uplo==AtlasUpper)
        use_uplo=AtlasLower;
     else
        use_uplo=AtlasUpper;
   }

   if (Diag == AtlasNonUnit)
     skipdiag = 1;
   else
     skipdiag = 0;


   /* Fill the full matrix with random values. */
   Mjoin(PATL,gegen)(N, N, A, lda, seed);

   /* Fill the untouched part with paddings. */
   if (use_uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
         Mjoin(PATLU,set)((N-j-skipdiag)SHIFT, padval, A+((j*lda+j+skipdiag)SHIFT), 1);
   }
   else
   {
      for (j=0; j < N; j++)
	Mjoin(PATLU,set)((j+1-skipdiag)SHIFT, padval, A+j*lda2, 1);
   }

   /* Make it diagonally dominant */
   trddom(use_uplo,Diag,N,A,lda);

   /* Introduce singularity error */
   /*   A[(5+5*lda) SHIFT] = 0.0;
   #ifdef TCPLX
      A[((5+5*lda) SHIFT)+1] = 0.0;
   #endif
   */
}


static TYPE trtritest(enum ATLAS_ORDER Order, enum ATLAS_UPLO Uplo,
                      enum ATLAS_DIAG Diag, int CacheSize, int N, int lda,
		      double *tim)
{
   TYPE *A, *Acompare;
   int i;
   double t0, t1;
   TYPE normA, eps, resid;
   /*int ierr;*/

   #ifdef TCPLX
      const TYPE one[2]={ATL_rone, ATL_rzero};
   #else
      const TYPE one = ATL_rone;
   #endif


   eps = Mjoin(PATL,epsilon)();
   A = malloc(ATL_MulBySize(lda)*N);
   Acompare = malloc(ATL_MulBySize(lda)*N);
   if (A == NULL) return(-1);
   if (Acompare == NULL) return(-1);
   t0 = ATL_flushcache(CacheSize);

   /* create random, diagonally dominant matrix with magic value at
      unused places. Last number is just the random seed. */
   trigen(Order, Uplo, Diag, N, A, lda, PADVAL, N*1029+lda);

   /* Create backup to calculate residual. This one has to be used
      as a full matrix, so it has zero fills and correct diagonal. */
   trigen(Order, Uplo, Diag, N, Acompare, lda, ATL_rzero, N*1029+lda);
   if (Diag==AtlasUnit)
     for (i=0; i < N; i++)
       Acompare[(i*(lda+1)) SHIFT] = ATL_rone;


   normA = trinrm1(Order,Uplo, Diag, N, A, lda);
#ifdef DEBUG
   Mjoin(PATL,geprint)("A0", N, N, A, lda);
#endif

   t0 = ATL_flushcache(-1);

   /* Calculate and time a solution */
   t0 = time00();
   test_trtri(Order, Uplo, Diag, N, A, lda);
   t1 = time00() - t0;
   *tim = t1;

/*   if (ierr != 0)
   {
     fprintf(stderr, "Return values != 0 : %d \n",ierr);
     return(9999.9999);
   }*/


   t0 = ATL_flushcache(0);

   /* Instroduce a padding error. */
   /* A[(5+5*lda)SHIFT]=114.0; */

#ifdef DEBUG
   Mjoin(PATL,geprint)("L", N, N, A, lda);
#endif
   ATL_checkpad(Order, Uplo, Diag, N, A, lda);

   /* Calculate A^{-1}*A */
   cblas_trmm(Order,CblasLeft,Uplo,AtlasNoTrans,Diag,
		    N,N,one,A,lda,Acompare,lda);

#ifdef DEBUG
     Mjoin(PATL,geprint)("A^{-1}*A", N, N, Acompare, N);
#endif

   /* Subtract diagonal */
   for (i=0; i < N; i++)
     Acompare[i*((lda+1) SHIFT)] -= ATL_rone;

/*
   resid = trinrm1(Order, Uplo,AtlasNonUnit,N,Acompare,lda);
   fprintf(stderr, "normA=%e, eps=%e, num=%e\n", normA, eps, resid);
*/

   resid = Mjoin(PATL,genrm1)(N, N, Acompare, lda);


#ifdef DEBUG
   if (resid/(normA*eps*N) > 10.0)
     fprintf(stderr, "normA=%e, eps=%e, num=%e\n", normA, eps, resid);
#endif
   resid /= (normA * eps * N);

   free(Acompare);
   free(A);

   return(resid);
}

int RunCase(int CacheSize, TYPE thresh, int MFLOP,
            enum ATLAS_ORDER Order, enum ATLAS_UPLO Uplo,
            enum ATLAS_DIAG Diag, int N, int lda)
{
   char *Ors;
   char *Ups;
   char *Diags;
   TYPE resid = 0.0;
   double mflop, mflops, t0, tim=0.0;
   int nreps=1, passed, i, imem;
   const int incA = lda*N;
   TYPE *a, *A;

   mflops = N;
#ifdef TREAL
   mflops = 1.0/3.0 * mflops*mflops*mflops;
#else
   mflops = 4.0/3.0 * mflops*mflops*mflops - 2.0*mflops*mflops;
#endif
   mflops /= 1000000.0;

   if (thresh > ATL_rzero) resid = trtritest(Order, Uplo, Diag, CacheSize, N, lda, &tim);
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
         for (i=0; i < imem; i++) trigen(Order, Uplo, Diag, N, A+i*incA, lda,
					 PADVAL, N*1029+lda);
         t0 = time00();
         for (i=nreps; i; i--, a += incA) test_trtri(Order, Uplo, Diag, N, a, lda);
         tim = time00() - t0;
         tim /= nreps;
         free(A);
      }
      else fprintf(stderr, "   WARNING: not enough mem to run timings!\n");
   }

   if (tim > 0.0) mflop = mflops / tim;
   else mflop = 0.0;
   if (Order == AtlasColMajor) Ors = "Col ";
   else Ors = "Row ";
   if (Uplo == AtlasUpper) Ups = "Upper";
   else Ups = "Lower";
   if (Diag == AtlasNonUnit) Diags = "NonUn";
   else Diags = "Unit ";
   fprintf(stdout, "%5d  %4s %5s %5s %6d %6d  %12.5f  %12.3f  %12e\n",
           nreps, Ors, Ups, Diags, N, lda, tim, mflop, resid);
   if (resid > thresh || resid != resid) passed = 0;
   else if (resid < 0.0) passed = -1;
   else passed = 1;
   return(passed);
}

void RunCases(const int CacheSize, const TYPE thresh, const int MFLOP,
              const int ldagap,
              const int norder, const enum ATLAS_ORDER *Orders,
              const int nuplo, const enum ATLAS_UPLO *Uplos,
	      const int ndiag, const enum ATLAS_DIAG *Diags,
              const int N0, const int NN, const int incN)
{
   int i, lda, n, ior, iup, idiag, np=0, nc=0, ns=0;

   fprintf(stdout,
      "NREPS   Ord. UPLO DIAG       N    lda          TIME        MFLOPS         RESID\n");
   fprintf(stdout,
      "=====  ==== =====  ===== =====  =====  ============  ============  ============\n");
   for (n=N0; n <= NN; n += incN)
   {
     if (ldagap >= 0) lda = ldagap + n;
     else lda = NN;
     for (ior=0; ior < norder; ior++)
     {
       for (iup=0; iup < nuplo; iup++)
       {
         for (idiag=0; idiag < ndiag; idiag++)
         {
	   i = RunCase(CacheSize, thresh, MFLOP, Orders[ior], Uplos[iup], Diags[idiag], n, lda);
	   if (i > 0)
	     np++;
	   else if (i < 0)
	     ns++;
	   nc++;
         }
       }
     }
   }
   if (thresh > ATL_rzero)
      fprintf(stdout, "\n%d cases: %d passed, %d skipped, %d failed\n",
              nc, np, ns, nc-np-ns);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -n <n> -N <N0 NN incN> -T <thresh> -F <mflop> -l <lagap> -C <cache size> -O <norders> <order1> ... <orderN> -U <nuplos> <uplo1> ... <uploN> -D <ndiags> <diag1> <diag2> ...\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *MFLOP, int *CacheSize, TYPE *thresh,
              int *ldagap,
              int *norder, enum ATLAS_ORDER **Order,
              int *nuplo, enum ATLAS_UPLO **Uplo,
	      int *ndiag, enum ATLAS_DIAG **Diag,
              int *N0, int *NN, int *incN)
{
   int i, j;
   char ch;

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
   *ndiag = -1;
   *norder = -1;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
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
      case 'O':
         *norder =  atoi(args[++i]);
         if (*norder <= 0) PrintUsage(args[0]);
         *Order = malloc(*norder * sizeof(enum ATLAS_ORDER));
         ATL_assert(*Order);
         for (j=0; j != *norder; j++)
         {
            ch = *args[++i];
            if (ch == 'R' || ch == 'r') (*Order)[j] = AtlasRowMajor;
            else if (ch == 'C' || ch == 'c') (*Order)[j] = AtlasColMajor;
            else PrintUsage(args[0]);
         }
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
      case 'D':
         *ndiag = atoi(args[++i]);
         if (*ndiag <= 0) PrintUsage(args[0]);
         *Diag = malloc(*ndiag * sizeof(enum ATLAS_DIAG));
         ATL_assert(*Diag);
         for (j=0; j != *ndiag; j++)
         {
            if (args[i] == NULL) PrintUsage(args[0]);
            ch = *args[++i];
            if (ch == 'n' || ch == 'N') (*Diag)[j] = AtlasNonUnit;
            else if (ch == 'u' || ch == 'U') (*Diag)[j] = AtlasUnit;
            else PrintUsage(args[0]);
         }
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   if (*N0 == -1)
   {
      *N0 = 100;
      *NN = 1000;
      *incN = 100;
   }
   if (*norder == -1)
   {
      *norder = 1;
      *Order = malloc(sizeof(enum ATLAS_ORDER));
      ATL_assert(*Order);
      **Order = AtlasColMajor;
   }
   if (*nuplo == -1)
   {
      *nuplo = 1;
      *Uplo = malloc(sizeof(enum ATLAS_UPLO));
      ATL_assert(*Uplo);
      **Uplo = AtlasLower;
   }
   if (*ndiag == -1)
   {
      *ndiag = 1;
      *Diag = malloc(sizeof(enum ATLAS_DIAG));
      ATL_assert(*Diag);
      **Diag = AtlasNonUnit;
   }
}

int main(int nargs, char **args)
{
  int MFLOP;     /* Number of mlops to minimum do in each test */
  int CacheSize;
  int ldagap;
  int norder;    /* Number of data lyouts to test */
  int nuplo;     /* Number of upper and lower matrices to test */
  int ndiag;     /* -- diagonals -- */
  int N0;        /* Beginning blocksize */
  int NN;        /* Ending blocksize */
  int incN;      /* Blocksize stride */

  TYPE thresh;

  /* GetFlags will allocate an enum-array where each position indicates
     which type of matrix is to be used: AtlasUpper or AtlasLower */
  enum ATLAS_UPLO *Uplos;
  /* Same deal with Diags: Indicate if diagonal is one or not: AtlasNonUnit
     or AtlasUnit */
  enum ATLAS_DIAG *Diags;
  enum ATLAS_ORDER *Orders;

  GetFlags(nargs, args, &MFLOP, &CacheSize, &thresh, &ldagap, &norder,
           &Orders,&nuplo, &Uplos,&ndiag, &Diags, &N0, &NN, &incN);
  RunCases(CacheSize, thresh, MFLOP, ldagap, norder, Orders,
           nuplo, Uplos, ndiag, Diags, N0, NN, incN);
  exit(0);
}
