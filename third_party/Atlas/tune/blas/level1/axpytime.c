#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
double ATL_flushcache(long long size);


int FAx=0, MAx=0, FAy=0, MAy=0;
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct FA_allocs
{
   void *mem, *memA;
   struct FA_allocs *next;
} *allocQ=NULL;

struct FA_allocs *NewAlloc(size_t size, struct FA_allocs *next,
                           int align, int misalign)
/*
 * Allocates size allocation that is aligned to [align], but not aligned
 * to [misalign].  Therefore, misalign > align.  Align must minimally be sizeof
 * while misalign may be 0 if we don't need to avoid a particular alignment.
 */
{
   void *vp;
   char *cp;
   struct FA_allocs *ap;
   int n, i;
   const int malign = align >= misalign ? align : misalign;

   n = size + align + align + malign;
   i = (n >> 3)<<3;
   if (n != i)
      n += n - i;
   cp = malloc(n + sizeof(struct FA_allocs));
   assert(cp);
   ap = (struct FA_allocs *) (cp + n);
   ap->mem = cp;
/*
 * Align to min alignment
 */
   ap->memA = align ? (void*) ((((size_t) cp)/align)*align + align) : cp;
/*
 * Misalign to misalign
 * We often need to make sure to unaligned addresses share the same modulo
 * so that they have the *same* degree of misalignment (so that their alignment
 * can be fixed by simple peeling), and so in this case force the address
 * modulo the misalign to be the exact align value.
 */
   if (misalign)
      ap->memA = (void*)((((size_t)ap->memA)/malign)*malign + malign + align);
   ap->next = next;
   return(ap);
}

/*
 * no-align malloc free retaining system default behavior
 */
void *NA_malloc(size_t size)
{
   return(malloc(size));
}
void *NA_calloc(size_t n, size_t size)
{
   return(calloc(n, size));
}
void NA_free(void *ptr)
{
   free(ptr);
}


/*
 * malloc/free pair that aligns data to align, but not to misalign
 */
void *FA_malloc(size_t size, int align, int misalign)
{
   if ((!misalign && align <= 8) || !size)
      return(malloc(size));
   else
   {
      allocQ = NewAlloc(size, allocQ, align, misalign);
      return(allocQ->memA);
   }
}
void *FA_calloc(size_t n, size_t size, int align, int misalign)
{
   char *cp;
   int *ip;
   double *dp;
   size_t i;
   size_t tsize;
   tsize = n * size;
   cp = FA_malloc(tsize, align, misalign);
   if (size == sizeof(int))
      for (ip=(int*)cp,i=0; i < n; i++)
        ip[i] = 0;
   else if (size == sizeof(double))
      for (dp=(double*)cp,i=0; i < n; i++)
        dp[i] = 0.0;
   else
      for (i=0; i < tsize; i++)
        cp[i] = 0;
   return(cp);
}

void FA_free(void *ptr, int align, int misalign)
/*
 * Part of malloc/free pair that aligns data to FALIGN
 */
{
   struct FA_allocs *ap, *prev;
   if (ptr)
   {
      if ((!misalign && align <= 8))
         free(ptr);
      else
      {
         for (ap=allocQ; ap && ap->memA != ptr; ap = ap->next) prev = ap;
         if (!ap)
         {
            fprintf(stderr, "Couldn't find mem=%ld\nmemQ=\n", (size_t)ptr);
            for (ap=allocQ; ap; ap = ap->next)
               fprintf(stderr, "   %ld, %ld\n", (size_t)ap->memA,
                       (size_t)ap->mem);
         }
         assert(ap);
         if (ap == allocQ)
            allocQ = allocQ->next;
         else
            prev->next = ap->next;
         free(ap->mem);
      }
   }
}

#define dumb_seed(iseed_) srand(iseed_)
#ifndef RAND_MAX  /* rather dangerous non-ansi workaround */
   #define RAND_MAX ((unsigned long)(1<<30))
#endif
#define dumb_rand() ( 0.5 - ((double)rand())/((double)RAND_MAX) )

#if defined(WALL) || defined(PentiumCPS)
   #ifndef WALL
      #define WALL
   #endif
   #define time00 ATL_walltime
#else
   #define time00 ATL_cputime
#endif
double time00();

#define Mjoin(pre, nam) my_join(pre, nam)
#define my_join(pre, nam) pre ## nam
#define Mstr2(m) # m
#define Mstr(m) Mstr2(m)
#define Mmin(x, y) ( (x) > (y) ? (y) : (x) )
#define Mmax(x, y) ( (x) < (y) ? (y) : (x) )
#define Mabs(x) ( (x) >= 0.0 ? (x) : -(x) )

#ifdef sREAL
   #define SREAL
#elif defined(dREAL)
   #define DREAL
#elif defined(sCPLX)
   #define SCPLX
#elif defined(dCPLX)
   #define DCPLX
#endif

#if defined (SREAL)
   #define TYPE float
   #define SCALAR float
   #define PRE s
#elif defined (DREAL)
   #define TYPE double
   #define SCALAR double
   #define PRE d
#elif defined (SCPLX)
   #define TYPE float
   #define SCALAR float*
   #define PRE c
#elif defined (DCPLX)
   #define TYPE double
   #define SCALAR double*
   #define PRE z
#endif

#if defined(SREAL) || defined(DREAL)
   #define TREAL
   #define SHIFT
   #define ATL_sizeof sizeof(TYPE)
#elif defined(SCPLX) || defined(DCPLX)
   #define TCPLX
   #define ATL_sizeof (2*sizeof(TYPE))
   #define SHIFT <<1
#endif

#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))
#ifndef TEST_AXPY
   #define TEST_AXPY ATL_AXPY
#endif

double DoTiming(int N, int nkflop, int cachesize, SCALAR alpha,
                int incX, int incY)
{
   void TEST_AXPY(const int N, const SCALAR alpha, const TYPE *X,
                  const int incX, TYPE *Y, const int incY);
   int nrep, i, n, nvec, NN, ix=0, iy=0, ii, jj;
   const int incx=Mabs(incX)SHIFT, incy=Mabs(incY)SHIFT;
   TYPE *X, *Y, *x, *y, *stX;
   double t0, t1;

   #ifdef TREAL
      nrep = (nkflop*1000) / (N*2);
   #else
      nrep = (nkflop*1000) / (N*8);
   #endif
   if (nrep < 1) nrep = 1;
   i = cachesize / ATL_sizeof;
   nvec = (i + 2*N) / N;
   if (nvec < 2) nvec = 2;
   if (nvec/2) nvec++;
   NN = (nvec/2) * N;
   if (incx > 4) ii = (incx + 3)/4;
   else ii = incx;
   if (incy > 4) jj = (incy + 3)/4;
   else jj = incy;
   NN *= (ii + jj);
   ii = N*(incx+incy);
   if (NN / ii) NN += ii - (NN % ii);
   x = X = FA_malloc(ATL_sizeof*NN, FAx, MAx);
   assert(X);
   stX = X + NN;
   dumb_seed(NN);
   for (i=0, n=NN; i < n; i++) X[i] = dumb_rand();
   if (incX < 0) ix = (N-1) * incx;
   if (incY < 0) iy = (N-1) * incy;
   ii = N * incx;
   jj = N * incy;
   t0 = time00();
   for (i=nrep; i; i--)
   {
      y = x + ii;
      TEST_AXPY(N, alpha, x+ix, incX, y+iy, incY);
      x = y + jj;
      if (x == stX) x = X;
   }
   t1 = time00() - t0;
   FA_free(X, FAx, MAx);
   return(t1/nrep);
}
void GetTimes(int nrep, double *tims, int N, int nkflop, int cachesize,
              SCALAR alpha, int incX, int incY)
/*
 * Gets nrep times
 */
{
   int i;
   for (i=0; i < nrep; i++)
   {
      tims[i] = DoTiming(N, nkflop, cachesize, alpha, incX, incY);
      fprintf(stdout, "      N=%d, tim=%e\n", N, tims[i]);
   }
}

void PutClosestFirst(int nrep, double *tims, double t0)
/*
 * examines all nrep entries, and puts one closest to t0 first
 */
{
   int i, im=0;
   double diff, dm=100000000.0;
   for (i=0; i < nrep; i++)
   {
      diff = tims[i] - t0;
      diff = Mabs(diff);
      if (diff < dm)
      {
         dm = diff;
         im = i;
      }
   }
   if (im)
   {
      diff = tims[0];
      tims[0] = tims[im];
      tims[im] = diff;
   }
}

void CrunchTims(int nrep, double *tims)
/*
 * orders times from closest to furthest
 */
{
   int i, j, im=0, in=0;
   double df, dfm=1000000.0;

   if (nrep < 3) return;
/*
 * Find two closest timings, take them as basis
 */
   for (i=0; i < nrep; i++)
   {
       for (j=0; j < nrep; j++)
       {
          if (i != j)
          {
             df = tims[i] - tims[j];
             df = Mabs(df);
             if (df < dfm)
             {
                im = i; in = j;
                dfm = df;
             }
          }
       }
   }
   df = tims[0];
   tims[0] = tims[im];
   tims[im] = df;
   df = tims[1];
   tims[1] = tims[in];
   tims[in] = df;
   df = (tims[0] + tims[1]) / 2.0;
   for (i=2; i < nrep; i++)
   {
      PutClosestFirst(nrep-i, tims+i, df);
      df = (i*df + tims[i]) / (i+1.0);
   }
}

int ApplyIntolerance(int nrep, double *tims, double TOL)
/*
 * Takes list ordered by closeness, and throws out outliers
 */
{
   int i, j=nrep;
   double t0, t1;

   t0 = tims[0];
   assert(t0 > 0.0);
   for (i=1; i < nrep; i++)
   {
      t1 = tims[i] - t0;
      t1 = Mabs(t1);
      if (t1 > t0*TOL) return(i);
   }
   return(nrep);
}

void SortTimes(int n, double *times)
/*
 * Simple selection sort,
 * RETURNS: times sorted from least to greatest
 */
{
   int i, j;
   double t0;
   for (i=0; i < n; i++)
   {
      t0 = times[i];
      for (j=i+1; j < n; j++)
      {
         if (t0 > times[j])
         {
            times[i] = times[j];
            times[j] = t0;
            t0 = times[i];
         }
      }
   }
}
double GetAvgMf(int N, int nkflop, int cachesize, char *fout,
                 SCALAR alpha, int incX, int incY, int nrep, double TOL)
/*
 * Gets nrep timings within TOL of each other.
 */
{
   double t0;
   #ifdef TREAL
      double mf = (2.0*N)/1000000.0;
   #else
      double mf = (8.0*N)/1000000.0;
   #endif
   double *tims;
   int n, i, j, k;
   FILE *fpout=NULL;

   tims = malloc(sizeof(double)*nrep);
   assert(tims);
   GetTimes(nrep, tims, N, nkflop, cachesize, alpha, incX, incY);
   SortTimes(nrep, tims);
/*
 * Take smallest time if we are using walltime, median value if CPUtime
 */
   #ifdef WALL
      t0 = tims[0];
   #else
      t0 = tims[nrep/2];
   #endif

   mf /= t0;
   fprintf(stdout, "   N=%d, time=%e, mflop=%f\n", N, t0, mf);
   if (fout)
   {
      fpout = fopen(fout, "w");
      assert(fpout);
      fprintf(fpout, "%lf\n", mf);
      fclose(fpout);
   }
   return(mf);
}

void DoTimings(int nN, int *Ns, int nkflop, int cachesize, char *fout,
               SCALAR alpha, int incX, int incY)
{

   int i;
   double t0;
   for (i=0; i < nN; i++)
   {
      t0 = GetAvgMf(Ns[i], nkflop, cachesize, fout, alpha, incX, incY, 3, 0.1);
      fprintf(stdout, "N=%d, incX=%d, incY=%d, mflop = %f\n",
              Ns[i], incX, incY, t0);
   }
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -n <N> -N <#> N1 ... N# <-F kflops> -C <cacheflush> -f <fout> -a <alpha> -X <incX> -Y <incY>\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *nN, int **Ns, int *nkflops,
              int *cachesize, char **fnam, int *incX, int *incY, TYPE *alpha)
{
   int i, j, k;

   *incY = 1;
   *alpha = 1.1;
   #ifdef TCPLX
      alpha[1] = 0.9;
   #endif
   *nN = -1;
   *nkflops = ATL_nkflop / 10;
   *cachesize = 4*L2SIZE;
   *fnam = NULL;
   *incX = 1;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      if (i == nargs-1) PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'b':
         i += 1 SHIFT;
         break;
      case 'a':
         #ifdef TREAL
            *alpha = atof(args[++i]);
         #else
            *alpha = atof(args[++i]);
            alpha[1] = atof(args[++i]);
         #endif
         break;
      case 'Y':
         *incY = atoi(args[++i]);
         break;
      case 'X':
         *incX = atoi(args[++i]);
         break;
      case 'C':
         *cachesize = atoi(args[++i]);
         break;
      case 'f':
         *fnam = args[++i];
         break;
      case 'F':
         if (args[i][2] == 'y' || args[i][2] == 'x')
         {
            j = args[i][2] != 'y';
            k = atoi(args[++i]);
            if (j)
            {
               if (k < 0)
                  MAx = -k;
               else
                  FAx = k;
            }
            else
            {
               if (k < 0)
                  MAy = -k;
               else
                  FAy = k;
            }
         }
         else
            *nkflops = atoi(args[++i]);
         break;
      case 'N':
         *nN = atoi(args[++i]);
         if (*nN > nargs-i) PrintUsage(args[0]);
         *Ns = malloc((*nN)*sizeof(int));
         assert(*Ns);
         for (j=0; j < *nN; j++) (*Ns)[j] = atoi(args[++i]);
         break;
      case 'n':
         *nN = 1;
         *Ns = malloc(sizeof(int));
         assert(*Ns);
         **Ns = atoi(args[++i]);
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   if (*nN < 0)
   {
      *nN = 1;
      *Ns = malloc(sizeof(int));
      assert(*Ns);
      **Ns = 777;
   }
   if (FAx < sizeof(TYPE))
      FAx = sizeof(TYPE);
   if (FAy < sizeof(TYPE))
      FAy = sizeof(TYPE);
}

int main(int nargs, char **args)
{
   char *fout;
   int nN, nkflops, cachesize, incX, incY;
   int *Ns;
   #ifdef TREAL
      TYPE alpha, *al=&alpha, beta, *be=&beta;
   #else
      TYPE alpha[2], *al=alpha, beta[2], *be=beta;
   #endif
   GetFlags(nargs, args, &nN, &Ns, &nkflops, &cachesize, &fout,
            &incX, &incY, al);
   DoTimings(nN, Ns, nkflops, cachesize, fout, alpha, incX, incY);
   free(Ns);
   exit(0);
}
