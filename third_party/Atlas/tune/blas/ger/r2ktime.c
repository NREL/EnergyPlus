/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010, 2009 R. Clint Whaley
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
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))

static char *resfile=NULL;
static FILE *fpres=NULL;
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
#include "atlas_r2testtime.h"

#ifdef TIME_KERNEL
   void ATL_UGER2K(ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y,
                   const TYPE *X1, const TYPE *Y1, TYPE *A, ATL_CINT lda);
#else
   void Mjoin(PATL,ger2)(ATL_CINT, ATL_CINT, const SCALAR, const TYPE*,
                         ATL_CINT, const TYPE *, ATL_CINT, const SCALAR,
                         const TYPE*, ATL_CINT, const TYPE *, ATL_CINT,
                         TYPE *, ATL_CINT);
   #ifdef TREAL
      #define test_ger2(M, N, alpha, X, incX, Y, incY, \
                        alpha1, X1, incX1, Y1, incY1, A, lda) \
         Mjoin(PATL,ger2)(M, N, *alpha, X, incX, Y, incY, \
                          *alpha1, X1, incX1, Y1, incY1, A, lda)
   #else
      #define test_ger2(M, N, alpha, X, incX, Y, incY, \
                        alpha1, X1, incX1, Y1, incY1, A, lda) \
         Mjoin(PATL,ger2u)(M, N, alpha, X, incX, Y, incY, \
                           alpha1, X1, incX1, Y1, incY1, A, lda)
   #endif
#endif

double Time2Flop(ATL_INT M, ATL_INT N, double time)
{
   if (time == 0.0 || time != time)
      return(0.0);
   #ifdef TREAL
      return(2*((1.0*M)*(2.0*N)+Mmin(M,N))/(time*1.0e6));
   #else
      return(2*((((6.0*M)*N+Mmin(M,N)) + (2.0*M)*N)*1.0e-6)/time);
   #endif
}

void Times2Flops(ATL_INT M, ATL_INT N, ATL_INT ntim, double *mf)
/*
 * Converts time to MFLOP
 */
{
   int i;

   for (i=0; i < ntim; i++)
      mf[i] = Time2Flop(M, N, mf[i]);
}

static double mysum(ATL_CINT N, double *d)
{
   int i;
   double sum;

   sum = d[0];
   for (i=1; i < N; i++)
      sum += d[i];
   return(sum);
}

   #define NX M
   #define NY N
#ifdef TIME_KERNEL
double r2time(
   int verb,            /* verbosity */
   int nreps,           /* number of reps to do for one timing sample */
   size_t flushelts,    /* size of area to flush to avoid cache reuse */
   ATL_INT M,           /* # of rows of array A */
   ATL_INT N,           /* # of cols of array A */
   TYPE *alpha,
   ATL_INT lda,         /* leading dim */
   int incX,            /* ignored, assumed 1 by definition */
   int incY,            /* increment on Y; can be non-1 */
   int FAa,             /* if (FA. = 0) enforce no alignment */
   int MAa,             /* else force op to be aligned to at least FA bytes */
   int FAx,             /* if MA. != 0, disallow op to be aligned to MA. byts */
   int MAx,
   int FAy,
   int MAy)
/*
 * This function directly calls the kernel routine on data that has been
 * preloaded (through initialization) to any cache large enough to hold it.
 */
{
   double t0, t1;
   TYPE *A, *X, *Y;
   ATL_INT i;
   int k;
   TYPE *X1, *Y1;
   #ifdef TREAL
      const TYPE negalpha[1] = {-alpha[0]};
   #else
      const TYPE negalpha[2] = {-alpha[0], -alpha[1]};
   #endif

   A = FA_malloc(ATL_MulBySize(lda)*N, FAa, MAa);
   X = FA_malloc(ATL_MulBySize(NX), FAx, MAx);
   Y = FA_malloc(ATL_MulBySize(NY*incY), FAy, MAy);
   ATL_assert(A && X && Y);
   X1 = FA_malloc(ATL_MulBySize(NX), FAx, MAx);
   Y1 = FA_malloc(ATL_MulBySize(NY*incY), FAy, MAy);
   ATL_assert(X1 && Y1);

   Mjoin(PATL,gegen)(1, NY, Y, incY, M);
   Mjoin(PATL,gegen)(NX, 1, X, NX, N+127*50+77);
   Mjoin(PATL,gegen)(1, NY, Y1, incY, M);         /* for timing, just use */
   Mjoin(PATL,gegen)(NX, 1, X1, M, N+127*50+77);  /* same data for X1/Y1 */
   Mjoin(PATL,gegen)(M, N, A, lda, N*M+513*7+90);
/*
 * NOTE: if nreps too high this could lead to under/overflow
 */
   for (k=0; k < 8; k++)  /* loop until results are believable, or give up */
   {
      t0 = time00();
      for (i=nreps; i; i--)
      {
         ATL_UGER2K(M, N, X, Y, X1, Y1, A, lda);
      }
      t1 = time00();
      if (t1 > t0) break;
      nreps = (nreps) ? nreps+nreps : 1;
   }
   t1 = (t1 - t0)/(1.0*nreps);
   if (verb)
      fprintf(stdout, "   M=%d, N=%d, lda=%d, nreps=%d, time=%e, mflop=%.2f\n",
              M, N, lda, nreps, t1, Time2Flop(M, N, t1));
   FA_free(A, FAa, MAa);
   FA_free(X, FAx, MAx);
   FA_free(Y, FAy, MAy);
   FA_free(X1, FAx, MAx);
   FA_free(Y1, FAy, MAy);
   return(t1);
}
#else
double r2time(
   int verb,            /* verbosity */
   int nreps,           /* number of reps to do for one timing sample */
   ATL_INT flushelts,   /* size of area to flush to avoid cache reuse */
   ATL_INT M,           /* # of rows of array A */
   ATL_INT N,           /* # of cols of array A */
   TYPE *alpha,
   ATL_INT lda,         /* leading dim */
   int mu,              /* unrolling on M */
   int nu,              /* unrolling on N */
   int FAa,             /* if (FA. = 0) enforce no alignment */
   int MAa,             /* else force op to be aligned to at least FA bytes */
   int FAx,             /* if MA. != 0, disallow op to be aligned to MA. bytes*/
   int MAx,
   int FAy,
   int MAy)
/*
 * Times the kernel for out-of-cache (where flushelts sets the cache that it
 * is not allowed to be in) use.
 * RETURNS: elapsed time in seconds to average repitition of indicated problem.
 * NOTE: This timer adapted from r1ktime, and just reuses x & y as X1, Y1
 *       for rank-2 update.  This may affect timings for highly rectangular
 *       matrices with one small dim where the vector costs are important!
 */
{
   #ifdef TREAL
      TYPE NONE = -1.0;
   TYPE nalpha[1] = {-alpha[0]};
   #else
      TYPE NONE[2] = {-1.0, 0.0};
   TYPE nalpha[2] = {-alpha[0], -alpha[1]};
   #endif
   double t0, t1;
   TYPE *A, *X, *Y, *a, *x, *y;
   void *vmem;
   size_t Aelts, Xelts, Yelts, setspan, ygap, xgap, agap, pregap, setsz, nsets;
   size_t ptr_st;
   ATL_INT i, j;
   int k, maxalign;

   if (MAx)
      assert(MAx != FAx);
   if (MAy)
      assert(MAy != FAy);
   if (MAa)
      assert(MAa != FAa);
/*
 * Find basic length of each operand in elements
 */
   Aelts = lda * N;
   Xelts = M;
   Yelts = N;
/*
 * Map memory so that we can enforce all required alignments while moving
 * through memory; mem starts with maxalign-aligned memory, so that we can
 * guarantee all further alignments
 */
   maxalign = (MAx) ? MAx : 1;
   if (MAy)
      maxalign = ATL_lcm(MAy,maxalign);
   if (MAa)
      maxalign = ATL_lcm(MAa,maxalign);
   if (FAx)
      maxalign = ATL_lcm(FAx,maxalign);
   if (FAy)
      maxalign = ATL_lcm(FAy,maxalign);
   if (FAa)
      maxalign = ATL_lcm(FAa,maxalign);
   if (maxalign == 1)
      maxalign = 0;
   j = (FAx) ? FAx : sizeof(TYPE);
   if (MAx)
      for (i=0; (i % j != 0 || i%MAx == 0); i += sizeof(TYPE));
   else if (FAx)
      for (i=0; i % j != 0 ; i += sizeof(TYPE));
   else
      i = 0;
   pregap = i;
   xgap = ATL_MulBySize(Xelts);
   if (FAy || MAy)
   {
      j = (FAy) ? FAy : sizeof(TYPE);
      if (MAy)
         for (i=pregap+xgap; (i%j != 0 || i%MAy == 0); i += sizeof(TYPE));
      else
         for (i=pregap+xgap; (i%j != 0); i += sizeof(TYPE));
      xgap = i - pregap;
   }
   ygap = ATL_MulBySize(Yelts);
   if (FAa || MAa)
   {
      j = (FAa) ? FAa : sizeof(TYPE);
      if (MAa)
         for (i=pregap+xgap+ygap; (i%j != 0 || i%MAa == 0); i += sizeof(TYPE));
      else
         for (i=pregap+xgap+ygap; (i%j != 0); i += sizeof(TYPE));
      ygap = i - pregap - xgap;
   }
   agap = ATL_MulBySize(Aelts);

   if (maxalign)
   {
      j = pregap;
      for (i=pregap+xgap+ygap+agap; i%maxalign != 0; i++);
      agap = i-pregap-xgap-ygap;
   }
   setspan = pregap + xgap + ygap + agap;
   assert(setspan%sizeof(TYPE) == 0);
   setsz = ATL_MulBySize(M+N+M*N);
   nsets = (ATL_MulBySize(flushelts)+setsz-1)/setsz;
   if (!nsets)
      nsets = 1;
   vmem = malloc(maxalign + nsets*setspan);
   assert(vmem);
   if (maxalign)   /* start maxaligned to guarantee all alignments */
      for (ptr_st = (size_t)vmem; ptr_st%maxalign; ptr_st++);
   else ptr_st = (size_t) vmem;
   X = (TYPE*) (ptr_st + pregap);
   Y = (TYPE*) (ptr_st + pregap + xgap);
   A = (TYPE*) (ptr_st + pregap + xgap + ygap);
/*
 * Set ptrs to last set in memory
 */
   setspan /= sizeof(TYPE);
   a = A += (nsets-1) * setspan;
   x = X += (nsets-1) * setspan;
   y = Y += (nsets-1) * setspan;
   for (i=nsets; i; i--)
   {
      #define DEBUG_FA
      #ifdef DEBUG_FA
         if (FAa)
            assert(((size_t)a)%FAa == 0);
         if (FAx)
            assert(((size_t)x)%FAx == 0);
         if (FAy)
            assert(((size_t)y)%FAy == 0);
         if (MAa)
            assert(((size_t)a)%MAa != 0);
         if (MAx)
            assert(((size_t)x)%MAx != 0);
         if (MAy)
            assert(((size_t)y)%MAy != 0);
      #endif
      Mjoin(PATL,gegen)(Yelts, 1, y, Yelts, M);
      Mjoin(PATL,gegen)(Xelts, 1, x, Xelts, N+127*50+77);
      if (i&1)
         Mjoin(PATL,scal)(Xelts, NONE, x, 1);
      Mjoin(PATL,gegen)(M, N, A, lda, N*M+513*7+90);
      a -= setspan; x -= setspan; y -= setspan;
   }
   a = A; x = X; y = Y;

   j=0;
   for (k=0; k < 8; k++) /* loop until good timing or too many trips */
   {
      t0 = time00();
      for (i=nreps; i; i--)
      {
         test_ger2(M, N, alpha, x, 1, y, 1, nalpha, x, 1, y, 1, A, lda);
         if (++j < nsets) { a -= setspan; x -= setspan; y -= setspan; }
         else  { a = A; x = X; y = Y; j=0; }
      }
      t1 = time00();
      if (t1 > t0)
         break;
      nreps = (nreps) ? nreps+nreps : 1;
   }
   free(vmem);
   t1 = (t1-t0) / (1.0*nreps);
   if (verb)
      fprintf(stdout, "   M=%d, N=%d, lda=%d, nreps=%d, time=%e, mflop=%.2f\n",
              M, N, lda, nreps, t1, Time2Flop(M, N, t1));
   return(t1);
}
#endif

void DoTimes(int verb, size_t flshelts, ATL_INT ntim, ATL_INT nrep,
             ATL_INT mu, ATL_INT nu, ATL_INT M, ATL_INT N, TYPE *alpha,
             ATL_INT lda, int FAa, int MAa, int FAx, int MAx, int FAy, int MAy)
{
   double *times;
   int i;

   times = malloc(ntim * sizeof(double));
   assert(times);

#ifdef TREAL
   fprintf(stdout,
   "GER2: M=%d, N=%d, lda=%d, AF=[%d,%d,%d], AM=[%d,%d,%d], alpha=%e:\n",
           M, N, lda, FAa, FAx, FAy, MAa, MAx, MAy, *alpha);
#else
   fprintf(stdout, "GER2: M=%d, N=%d, lda=%d, AF=[%d,%d,%d], AM=[%d,%d,%d], alpha=[%e,%e]:\n",
           M, N, lda, FAa, FAx, FAy, MAa, MAx, MAy, *alpha, alpha[1]);
#endif
   for (i=0; i < ntim; i++)
      times[i] = r2time(verb, nrep, flshelts, M, N, alpha, lda,
                        mu, nu, FAa, MAa, FAx, MAx, FAy, MAy);
   SortDoubles(ntim, times);
   Times2Flops(M, N, ntim, times);
   if (fpres)
   {
      #if defined(PentiumCPS) || defined(WALL)
         fprintf(fpres, "%d 1\n", ntim);
      #else
         fprintf(fpres, "%d 0\n", ntim);
      #endif
      for (i=0; i < ntim; i++)
         fprintf(fpres, "%le\n", times[i]);
      fclose(fpres);
   }
   fprintf(stdout, "NREPS=%d, MAX=%.2f, MIN=%.2f, AVG=%.2f, MED=%.2f\n",
           ntim, times[0], times[ntim-1], mysum(ntim, times)/ntim,
           times[ntim>>1]);
   free(times);
}

void PrintUsage(char *name, char *arg, int i)
{
   if (i > 0)
      fprintf(stderr, "BAD ARG '%s' on %dth FLAG\n", arg, i);
   fprintf(stderr, "USAGE: %s [flags], where flags are:\n", name);
   fprintf(stderr, "   -v <#> : set verbosity level\n");
   fprintf(stderr, "   -C <#> : set flushsz = # (kbytes)\n");
   fprintf(stderr, "   -x <#> : unrolling for X in kernel is #\n");
   fprintf(stderr, "   -y <#> : unrolling for Y in kernel is #\n");
   fprintf(stderr, "   -m <#> : set # of rows of matrix to #\n");
   fprintf(stderr, "   -n <#> : set # of cols of matrix to #\n");
   fprintf(stderr, "   -l <#> : set leading dimension of array A to #\n");
   fprintf(stderr, "   -F <#> : do at least # MFLOPS for each timing interval\n");
   fprintf(stderr, "   -f <file> : output timing summary in <file>; if file exists read & report\n");
   fprintf(stderr,
           "   -r <#> : do # repetitions of the call for each timing interval\n");
   fprintf(stderr,
      "   -# <#> : report # timings (each interval may have multiple calls)\n");
   fprintf(stderr,
"   -F[x,y,a] <#> : if(# > 0) -> force op to be aligned to at least # bytes\n");
   fprintf(stderr,
"                   if(# < 0) -> force op to be aligned to < # bytes.\n");
   fprintf(stderr, "   -b <beta> : 2 floats for complex, one for real.\n");
   exit(i ? i : -1);
}

void GetFlags(int nargs, char **args, int *verb,
              size_t *flushelts, ATL_INT *celts, ATL_INT *pgelts,
              ATL_INT *mu, ATL_INT *nu, ATL_INT *ntim, ATL_INT *nrep,
              enum ATLAS_TRANS *TA, ATL_INT *m, ATL_INT *n, ATL_INT *lda,
              TYPE *beta,
              int *FAa, int *MAa, int *FAx, int *MAx, int *FAy, int *MAy)
{
   double mfF=ATL_nkflop/1000.0, flops;
   ATL_INT j, h;
   size_t il;
   int i;
   char ch;

   *verb = 1;
   #ifdef ATL_PAGESZ
      *pgelts = ATL_DivBySize(ATL_PAGESZ);
   #else
      *pgelts = 4*ATL_DivBySize(1024);
   #endif
   *celts = 0.75*ATL_L1elts;
   #ifdef L2SIZE
      *flushelts = L2SIZE;
   #else
      *flushelts = 8*1024*ATL_DivBySize(1024);
   #endif
   *mu = *nu = 1;
   *m = 800;
   *n = 200;
   *nrep = *lda = 0;
   *ntim = 3;
   *FAa = *MAa = *FAx = *MAx = *FAy = *MAy = 0;
   *beta = 1.0;
   #ifdef TCPLX
      beta[1] = 0.0;
   #endif

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], "No '-' preceeding flag!", i);
      switch(args[i][1])
      {
      case 'f' :  /* set resfile output */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -f ", i-1);
         resfile = args[i];
         break;
      case 'v' :  /* set verbosity */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -v ", i-1);
         *verb = atoi(args[i]);
         break;
      case 'G' :  /* set GEMV blocking cache size in KB */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -G ", i-1);
         j = atoi(args[i]);
         *celts = j*ATL_DivBySize(1024);
         break;
      case 'A' :  /* set transpose */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -A ", i-1);
         ch = args[i][0];
         if (ch == 't' || ch == 'T')
            *TA = AtlasTrans;
         else if (ch == 'c' || ch == 'C')
            *TA = AtlasConjTrans;
         else if (ch == 'z' || ch == 'Z')
            *TA = AtlasConj;
         else
            *TA = AtlasNoTrans;
         break;
      case 'C' :  /* set flushsz in KB */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -C ", i-1);
         il = atol(args[i]);
         if (il >= 0)
            *flushelts = il*ATL_DivBySize(1024);
         break;
      case 'p' :  /* set pagesz in KB */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -p ", i-1);
         j = atoi(args[i]);
         *pgelts = j*ATL_DivBySize(1024);
         break;
      case 'x' :  /* set mu */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -x ", i-1);
         *mu = atoi(args[i]);
         break;
      case 'y' :  /* set nu */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -y ", i-1);
         *nu = atoi(args[i]);
         break;
      case 'm' :  /* set M */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -m ", i-1);
         *m = atoi(args[i]);
         break;
      case 'n' :  /* set N */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -n ", i-1);
         *n = atoi(args[i]);
         break;
      case 'l' :  /* set lda */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -l ", i-1);
         *lda = atoi(args[i]);
         break;
      case 'a' : /* alias for setting alpha in r1ktime */
      case 'b' : /* set beta */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -b ", i-1);
         *beta = atof(args[i]);
         #ifdef TCPLX
            if (++i >= nargs)
               PrintUsage(args[0], "out of flags in -b ", i-1);
            beta[1] = atof(args[i]);
         #endif
         break;
      case 'F' :  /* set nrep by specifying MFLOPS, or force alignment */
         ch = args[i][2];
         if (ch == '\0')   /* specifying MFLOPS */
         {
            if (++i >= nargs)
               PrintUsage(args[0], "out of flags in -F ", i-1);
            j = atoi(args[i]);
            mfF = j;
         }
         else
         {
            if (ch != 'a' && ch != 'y' && ch != 'x')
               PrintUsage(args[0], args[i], i);
            if (++i >= nargs)
               PrintUsage(args[0], args[i-1], i-1);
            j = atoi(args[i]);
            if (j < 0)
            {
               if (ch == 'a')
                  *MAa = -j;
               else if (ch == 'y')
                  *MAy = -j;
               else if (ch == 'x')
                  *MAx = -j;
            }
            else
            {
               if (ch == 'a')
                  *FAa = j;
               else if (ch == 'y')
                  *FAy = j;
               else if (ch == 'x')
                  *FAx = j;
            }
         }
         break;
      case 'r' :  /* set nrep directly as integer */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -r ", i-1);
         *nrep = atoi(args[i]);
         break;
      case '#' :  /* set number of timings to report */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -# ", i-1);
         *ntim = atoi(args[i]);
         break;
      default:
         PrintUsage(args[0], args[i], i);
      }
   }
   if (!(*nrep))
   {
      flops = Time2Flop(*m, *n, 1.0) * 1000.0;  /* Get kiloFLOPS in GEMV */
      *nrep = (mfF+flops-1)/flops;
      if (*nrep < 1) *nrep = 1;
   }
   if (!(*lda))
      *lda = *m + 8;
}
int main(int nargs, char **args)
{
   size_t flushelts;
   ATL_INT celts, pgelts, mu, nu, ntim, nrep, m, n, lda;
   int FAa, MAa, FAx, MAx, FAy, MAy;    /* Force & Max align for ops */
   int verb;
   enum ATLAS_TRANS TA;
   double *dres;
   #ifdef TREAL
      TYPE beta;
   #else
      TYPE beta[2];
   #endif

   GetFlags(nargs, args, &verb, &flushelts, &celts, &pgelts, &mu, &nu, &ntim,
            &nrep, &TA, &m, &n, &lda, SADD beta,
            &FAa, &MAa, &FAx, &MAx, &FAy, &MAy);
   if (resfile)
   {
      dres = ReadResultsFile(1, ntim, resfile);
      if (dres)
      {
         fprintf(stdout, "TIMINGS READ IN FROM '%s':\n", resfile);
         PrintResultsFromFile(stdout, dres);
         free(dres);
         exit(0);
      }
      fpres = fopen(resfile, "w");
      assert(fpres);
   }
   DoTimes(verb, flushelts, ntim, nrep, mu, nu, m, n, SADD beta, lda,
           FAa, MAa, FAx, MAx, FAy, MAy);
   exit(0);
}
