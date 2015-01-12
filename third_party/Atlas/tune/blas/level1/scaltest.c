#include "atlas_misc.h"
#include <assert.h>

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
#ifndef TEST_SCAL
   #define TEST_SCAL ATL_USCAL
#endif
void TEST_SCAL(const int, const SCALAR, TYPE *, const int incX);

#ifdef TREAL
void good_scal(const int N, const SCALAR alpha, TYPE *X, const int incX)
{
   int i;
   if (alpha == ATL_rzero)
      for (i=0; i < N; i++, X += incX) *X = ATL_rzero;
   else
      for (i=0; i < N; i++, X += incX) *X *= alpha;
}
int checkY(int N, TYPE *Yg, int incYg, TYPE *Yc, int incYc)
{
   int i, iret=0;
   TYPE eps, diff;
   TYPE Mjoin(PATL, epsilon)(void);
   eps = Mjoin(PATL,epsilon)();

   for (i=0; i < N; i++, Yg += incYg, Yc += incYc)
   {
      diff = *Yg - *Yc;
      diff = Mabs(diff);
      if (diff > 2*eps)
      if (*Yg != *Yc)
      {
         iret = i;
         fprintf(stderr, "ERROR: Y[%d], correct=%f, computed=%f\n",
                 i, *Yg, *Yc);
      }
   }
   return(iret);
}
int CheckPad(int npad, TYPE padval, int N, TYPE *Y, int incY)
{
   int i, iret=0;

   incY = Mabs(incY);
   for (i=0; i < npad; i++)
   {
      if (Y[i] != padval)
      {
         iret = i;
         fprintf(stderr, "OVERWRITE %f IN PREPAD %d before beginning of Y!!\n",
                 Y[i], npad-i);
      }
   }
   Y += npad;
   if (incY != 1)
   {
      for (i=0; i < N*incY; i++)
      {
         if (i%incY)
         {
            if (Y[i] != padval)
            {
               iret = i;
               fprintf(stderr, "INTERNAL OVERWRITE %f AT POSITION %d!!\n",
                       Y[i], i);
            }
         }
      }
   }
   Y += 1 + (N-1)*incY;
   for (i=0; i < npad; i++)
   {
      if (Y[i] != padval)
      {
         iret = i;
         fprintf(stderr, "OVERWRITE %f IN POSTPAD %d past end of Y!!\n",
                 Y[i], i+1);
      }
   }
   return(iret);
}
#else
void good_scal(const int N, const SCALAR alpha, TYPE *X, const int incx)
{
   int i;
   const int incX=incx+incx;
   const register TYPE ra=*alpha, ia=alpha[1];
   register TYPE rx, ix;

   if ( SCALAR_IS_ZERO(alpha) )
   {
      for (i=0; i < N; i++, X += incX) *X = X[1] = ATL_rzero;
   }
   else
   {
      for (i=0; i < N; i++, X += incX)
      {
         rx = *X; ix = X[1];
         *X   = rx * ra - ix * ia;
         X[1] = rx * ia + ix * ra;
      }
   }
}
int checkY(int N, TYPE *Yg, int incYg, TYPE *Yc, int incYc)
{
   int i, iret=0;
   TYPE rdiff, idiff, eps, maxerr;
   TYPE Mjoin(PATL, epsilon)(void);
   eps = Mjoin(PATL,epsilon)();
   maxerr = 6*eps;
   incYg *= 2; incYc *= 2;
   for (i=0; i < N; i++, Yg += incYg, Yc += incYc)
   {
      rdiff = *Yg - *Yc;
      idiff = Yg[1] - Yc[1];
      rdiff = Mabs(rdiff);
      idiff = Mabs(idiff);
      if (rdiff > maxerr)
      {
         iret = i;
         fprintf(stderr, "ERROR: Y[%d], real, correct=%e, computed=%e\n",
                 i, *Yg, *Yc);
      }
      if (idiff > maxerr)
      {
         iret = i;
         fprintf(stderr, "ERROR: Y[%d], imag, correct=%e, computed=%e\n",
                 i, Yg[1], Yc[1]);
      }
   }
   return(iret);
}
int CheckPad(int npad, TYPE padval, int N, TYPE *Y, int incY)
{
   int i, n, iret=0;

   incY = Mabs(incY);
   npad *= 2;
   for (i=0; i < npad; i++)
   {
      if (Y[i] != padval)
      {
         iret = i;
         fprintf(stderr, "OVERWRITE %f IN PREPAD %d before beginning of Y!!\n",
                 Y[i], npad-i);
      }
   }
   Y += npad;
   if (incY != 1)
   {
      for (i=0; i < N*incY; i++)
      {
         if (i%incY)
         {
            if (Y[2*i] != padval)
            {
               iret = i;
               fprintf(stderr, "INTERNAL REAL OVERWRITE %f AT POSITION %d!!\n",
                       Y[2*i], i);
            }
            if (Y[2*i+1] != padval)
            {
               iret = i+1;
               fprintf(stderr, "INTERNAL IMAG OVERWRITE %f AT POSITION %d!!\n",
                       Y[2*i+1], i);
            }
         }
      }
   }
   Y += 2 + 2*(N-1)*incY;
   for (i=0; i < npad; i++)
   {
      if (Y[i] != padval)
      {
         iret = i;
         fprintf(stderr, "OVERWRITE %f IN POSTPAD %d past end of Y!!\n",
                 Y[i], i+1);
      }
   }
   return(iret);
}
#endif

static int CheckY(int npad, TYPE padval, int N, TYPE *Yg, int incYg,
                  TYPE *Yt, int incYt)
{
   int i0, i1;
   incYg = Mabs(incYg);
   incYt = Mabs(incYt);
   i0 = checkY(N, Yg+(npad SHIFT), incYg, Yt+(npad SHIFT), incYt);
   i1 = CheckPad(npad, padval, N, Yt, incYt);
   if (!i0 && !i1) return(0);
   return(1);
}
static void vecset(int N, TYPE alpha, TYPE *X)
{
   int i;
   #ifdef TCPLX
      N *= 2;
   #endif
   for (i=0; i < N; i++) X[i] = alpha;
}

static TYPE *getvec(int npad, TYPE padval, int N, int incX, int FA, int MA)
{
   TYPE *X, *x;
   int i, n;

   if (N <= 0) return(NULL);
   incX = Mabs(incX);
   n = 2*npad + 1+(N-1)*incX;
   X = FA_malloc(ATL_sizeof*n, FA, MA);
   assert(X);
   vecset(n, padval, X);
   #ifdef TCPLX
      npad *= 2;
      incX *= 2;
   #endif
   x = X + npad;
   for (i=0; i < N; i++, x += incX)
   {
      #ifdef TREAL
         *x = dumb_rand();
      #else
         *x   = dumb_rand();
         x[1] = dumb_rand();
      #endif
   }
   return(X);
}

static void copyvec(int N, const TYPE *X, int incX, TYPE *Y, int incY)
{
   int i;
   #ifdef TREAL
      for (i=0; i < N; i++, X += incX, Y += incY) *Y = *X;
   #else
      incX *= 2; incY *= 2;
      for (i=0; i < N; i++, X += incX, Y += incY) { *Y = *X; Y[1] = X[1]; }
   #endif
}

static TYPE *dupvec(int npad, int N, TYPE *X, int incX, int FA, int MA)
{
   int i, n;
   TYPE *y;

   incX = Mabs(incX);
   n = 1+(N-1)*incX + 2*npad;
   y = FA_malloc(ATL_sizeof*n, FA, MA);
   assert(y);
   #ifdef TCPLX
      n *= 2;
   #endif
   for (i=0; i < n; i++) y[i] = X[i];
   return(y);
}

static TYPE *gen_dupvec(int N, TYPE padval, int npadX, TYPE *X, int incX,
                        int npadY, int incY, int FA, int MA)
{
   int i, n;
   TYPE *y, *yy, *xx=X+(npadX SHIFT);

   y = getvec(npadY, padval, N, incY, FA, MA);
   yy = y + (npadY SHIFT);
   if (incY < 1) yy -= ((N-1)SHIFT) * incY;
   if (incX < 1) xx -= ((N-1)SHIFT) * incX;
   copyvec(N, xx, incX, yy, incY);
   return(y);
}

int DoTest(int N, TYPE *alpha0, int incY)
{
   int iret;
   const int npad=Mmax(4*Mabs(incY), 16);
   const TYPE padval=(-2271.0);
   TYPE *Yg, *Yt, *y;
   #ifdef TREAL
      TYPE alpha = *alpha0;
   #else
      TYPE *alpha = alpha0;
   #endif

   Yg = getvec(npad, padval, N, incY, FAy, MAy);
   Yt = dupvec(npad, N, Yg, incY, FAy, MAy);

   y = Yg + (npad SHIFT);
   if (incY < 1) y -= ((N-1)SHIFT) * incY;
   good_scal(N, alpha, y, incY);
   y = Yt + (npad SHIFT);
   if (incY < 1) y -= ((N-1)SHIFT) * incY;
   TEST_SCAL(N, alpha, y, incY);
   iret = CheckY(npad, padval, N, Yg, incY, Yt, incY);
   FA_free(Yg, FAx, MAx);
   FA_free(Yt, FAy, MAy);
   return(iret);
}
int DoAllTests(int nN, int *Ns, int nX, int *Xs, int nY, int *Ys,
               int nalpha, TYPE *alp)
{
   int in, ix, iy, ia, ib, iret=0, i=0, j, k;
   char *passfail;

#ifdef TREAL
   char *t1="  ITST         N     alpha  incX  incY    TEST";
   char *t2="======  ========  ========  ====  ====  ======";
#else
   char *t1="  ITST         N    ralpha   ialpha  incX  incY    TEST";
   char *t2="======  ========  ======== ========  ====  ====  ======";
#endif
   fprintf(stdout, "%s\n", t1);
   fprintf(stdout, "%s\n", t2);
   for (in=0; in < nN; in++)
   {
      for (ix=0; ix < nX; ix++)
      {
         for (ia=0; ia < nalpha; ia++)
         {
            j = DoTest(Ns[in], alp+(ia SHIFT), Xs[ix]);
            iret += j;
            if (j == 0) passfail = "PASSED";
            else passfail = "FAILED";
            #ifdef TREAL
               fprintf(stdout, "%6d %9d %9.2f %5d %5d  %s\n",
                       i, Ns[in], alp[ia], Xs[ix], 0, passfail);
            #else
               fprintf(stdout, "%6d %9d  %8.2f %8.2f %5d %5d  %s\n",
                       i, Ns[in], alp[2*ia], alp[2*ia+1], Xs[ix], 0, passfail);
            #endif
            i++;
         }
      }
   }
   if (iret == 0) fprintf(stdout, "ALL SCAL SANITY TESTS PASSED.\n\n");
   else fprintf(stdout, "%d of %d SCAL TESTS FAILED!!\n\n", iret, i);
   return(iret);
}

void PrintUsage(char *nam)
{
   fprintf(stderr,
      "USAGE: %s -N # n1 ... n# -n <n> -X # x1 ... x# -a # alpha1 ... alpha#\n",
           nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *nN, int **Ns, int *nX, int **incXs,
              int *nY, int **incYs, int *nal, TYPE **alphas)
{
   int i, j, k, ig;

   *nal = -1;
   *nY = *nX = *nN = -1;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      if (i == nargs-1) PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'F':
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
         break;
      case 'a':
         *nal = atoi(args[++i]);
         if (*nal > nargs-i) PrintUsage(args[0]);
         *alphas = malloc(ATL_MulBySize(*nal));
         assert(*alphas);
         for (j=0; j < *nal SHIFT; j++) (*alphas)[j] = atof(args[++i]);
         break;
      case 'b':
         ig = atoi(args[++i]);
         if (ig > nargs-i) PrintUsage(args[0]);
         i += ig SHIFT;
         break;
      case 'Y':
         *nY = atoi(args[++i]);
         if (*nY > nargs-i) PrintUsage(args[0]);
         *incYs = malloc((*nY)*sizeof(int));
         assert(*incYs);
         for (j=0; j < *nY; j++) (*incYs)[j] = atoi(args[++i]);
         break;
      case 'X':
         *nX = atoi(args[++i]);
         if (*nX > nargs-i) PrintUsage(args[0]);
         *incXs = malloc((*nX)*sizeof(int));
         assert(*incXs);
         for (j=0; j < *nX; j++) (*incXs)[j] = atoi(args[++i]);
         break;
      case 'N':
         *nN = atoi(args[++i]);
         if (*nN > nargs-i) PrintUsage(args[0]);
         *Ns = malloc((*nN)*sizeof(int));
         assert(*Ns);
         for (j=0; j < *nN; j++) (*Ns)[j] = atoi(args[++i]);
         break;
      case 'y':
         *nY = 1;
         *incYs = malloc(sizeof(int));
         assert(*incYs);
         **incYs = atoi(args[++i]);
         break;
      case 'x':
         *nX = 1;
         *incXs = malloc(sizeof(int));
         assert(*incXs);
         **incXs = atoi(args[++i]);
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
      *nN = 4;
      *Ns = malloc((*nN)*sizeof(int));
      assert(*Ns);
      **Ns = 777;
      (*Ns)[1] = 1;
      (*Ns)[2] = 3;
      (*Ns)[3] = 7;
   }
   if (*nX < 0)
   {
      *nX = 1;
      *incXs = malloc((*nX)*sizeof(int));
      assert(*incXs);
      **incXs = 1;
   }
   if (*nY < 0)
   {
      *nY = 1;
      *incYs = malloc((*nY)*sizeof(int));
      assert(*incYs);
      **incYs = 1;
   }
   if (*nal < 1)
   {
      #ifdef TREAL
         *nal = 3;
         *alphas = malloc(ATL_MulBySize(3));
         assert(*alphas);
         (*alphas)[0] = 1.0;
         (*alphas)[1] = -1.0;
         (*alphas)[2] = 0.9;
      #else
         *nal = 4;
         *alphas = malloc(ATL_MulBySize(4));
         assert(*alphas);
         (*alphas)[0] = 1.0;
         (*alphas)[1] = 0.0;
         (*alphas)[2] = -1.0;
         (*alphas)[3] = 0.0;
         (*alphas)[4] = 1.3;
         (*alphas)[5] = 0.0;
         (*alphas)[6] = 0.9;
         (*alphas)[7] = 1.1;
      #endif
   }
   if (FAx < sizeof(TYPE))
      FAx = sizeof(TYPE);
   if (FAy < sizeof(TYPE))
      FAy = sizeof(TYPE);
   FAy = FAx;
   MAy = MAx;
}

int main(int nargs, char **args)
{
   int nN, nX, nY, nal, nbe;
   int *Ns, *incXs, *incYs;
   TYPE *alphas=NULL, *betas=NULL;
   int ierr;
   GetFlags(nargs, args, &nN, &Ns, &nX, &incXs, &nY, &incYs, &nal, &alphas);
   ierr = DoAllTests(nN, Ns, nX, incXs, nY, incYs, nal, alphas);
   if (alphas) free(alphas);
   free(incXs);
   free(incYs);
   return(ierr);
}
