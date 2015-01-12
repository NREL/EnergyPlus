#include "atlas_misc.h"
#include "atlas_tst.h"
#include "atlas_lvl2.h"
#include "atlas_level1.h"
#include <ctype.h>
int FAx=0, MAx=0, FAy=0, MAy=0, FAa=0, MAa=0;
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

static void dumb_ger(int Conj, int M, int N, SCALAR alpha, TYPE *X, int incX,
                     TYPE *Y, int incY, TYPE *A, int lda)
{
   #ifdef TCPLX
      TYPE tmp[2];
   #endif
   int j;
   ATL_assert(incX == 1);
   ATL_assert( SCALAR_IS_ONE(alpha) );
   for (j=0; j < N; j++)
   {
   #ifdef TREAL
      Mjoin(PATL,axpy)(M, Y[j*incY], X, 1, A+((size_t)lda)*j, 1);
   #else
      tmp[0] = Y[2*j*incY];
      tmp[1] = Y[2*j*incY+1];
      if (Conj) tmp[1] = -tmp[1];
      Mjoin(PATL,axpy)(M, tmp, X, 1, A+2*((size_t)lda)*j, 1);
   #endif
   }
}
static void dumb_ger2(int Conj, int M, int N,
                      SCALAR alpha0, TYPE *X0, int incX0, TYPE *Y0, int incY0,
                      SCALAR alpha1, TYPE *X1, int incX1, TYPE *Y1, int incY1,
                      TYPE *A, int lda)
{
   dumb_ger(Conj, M, N, alpha0, X0, incX0, Y0, incY0, A, lda);
   dumb_ger(Conj, M, N, alpha1, X1, incX1, Y1, incY1, A, lda);
}

static int CheckAns(int M, int N, TYPE *G, int ldg, TYPE *U, int ldu)
{
   TYPE diff, eps;
   int i, j, ierr=0;
   #ifdef TREAL
      const int M2 = M, emul=8;
   #else
      const int M2 = M<<1, emul=4*8;
      ldg <<= 1; ldu <<= 1;
   #endif

   eps = Mjoin(PATL,epsilon)();
   for (j=0; j < N; j++, G += ldg, U += ldu)
   {
      for (i=0; i < M2; i++)
      {
         diff = G[i] - U[i];
         if (diff < ATL_rzero) diff = -diff;
         if (diff > emul*eps)
         {
            fprintf(stderr, "A(%d,%d): Good=%f, Computed=%f\n",
                    i, j, G[i], U[i]);
            if (!ierr) ierr = i+j*M+1;
         }
      }
   }
   return(ierr);
}

#define NX M
#define NY N
void ATL_UGER2K(ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y,
                const TYPE *W, const TYPE *Z, TYPE *A, ATL_CINT lda);

static int RunTest(int CONJ, int M, int N, int incY, int lda)
{
   #ifdef TCPLX
      TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      TYPE one = ATL_rone;
   #endif
   TYPE *A, *A0, *X, *Y, *y;
   TYPE *X1, *Y1;
   ATL_CINT aincY = Mabs(incY), incX=1, aincX=1;
   int ierr;

   fprintf(stdout, "   TEST CONJ=%d, M=%d, N=%d, lda=%d, incY=%d, STARTED\n",
           CONJ, M, N, lda, incY);
   A = FA_malloc(ATL_MulBySize(lda)*N, FAa, MAa);
   A0 = FA_malloc(ATL_MulBySize(M)*N, FAa, MAa);
   Y = FA_malloc(ATL_MulBySize(NY)*aincY, FAy, MAy);
   X = FA_malloc(ATL_MulBySize(NX), FAx, MAx);
   ATL_assert(A && A0 && X && Y);
   X1 = FA_malloc(ATL_MulBySize(NX), FAx, MAx);
   Y1 = FA_malloc(ATL_MulBySize(NY)*aincY, FAy, MAy);
   ATL_assert(X1 && Y1);
   if (aincY != 1)
      Mjoin(PATLU,set)(NY*aincY SHIFT, 1000000000.0, Y1, 1);
   if (aincX != 1)
      Mjoin(PATLU,set)(NY*aincX SHIFT, 5000000000.0, X1, 1);
   Mjoin(PATL,gegen)(1, NY, Y1, aincY, ((N*aincY)<<4)+0xFFABCD);
   Mjoin(PATL,gegen)(1, NX, X1, aincX, NY*aincY+0xAC3955F);
   Mjoin(PATL,gegen)(1, NY, Y, aincY, NY*aincY);
   Mjoin(PATL,gegen)(1, NX, X, aincX, NY*aincY+127*50+77);
   Mjoin(PATL,gegen)(M, N, A0, M, N*M+513*7+90);
   Mjoin(PATL,gegen)(M, N, A, lda, N*M+513*7+90);
   if (incY < 0) Y += (NY-1) * (aincY SHIFT);
   if (incY < 0) Y1 += (NY-1) * (aincY SHIFT);

   ATL_UGER2K(M, N, X, Y, X1, Y1, A, lda);
   dumb_ger2(CONJ, M, N, one, X, 1, Y, incY, one, X1, 1, Y1, incY, A0, M);
   if (incY < 0) Y1 -= (N-1) * (aincY SHIFT);
   FA_free(Y1, FAy, MAy);
   FA_free(X1, FAx, MAx);

   if (incY < 0) Y -= (N-1) * (aincY SHIFT);
   FA_free(Y, FAy, MAy);
   FA_free(X, FAx, MAx);
   ierr = CheckAns(M, N, A0, M, A, lda);
   FA_free(A, FAa, MAa);
   FA_free(A0, FAa, MAa);

   if (ierr)
      fprintf(stdout, "   TEST CONJ=%d, M=%d, N=%d, lda=%d, incY=%d, FAILED\n",
              CONJ, M, N, lda, incY);
   else fprintf(stdout,
                "   TEST CONJ=%d, M=%d, N=%d, lda=%d, incY=%d, PASSED\n",
                CONJ, M, N, lda, incY);
   return(ierr);
}
#undef NX
#undef NY

int RunTests(int verb, int *CONJs, int *Ms, int *Ns, int *incYs, int *ldas)
{
   int iy, ix, ic, in, im;
   ATL_INT m, n, lda, conj, incy;
   int nerr=0;
   assert(ldas[0] == Ms[0]);
   for (in=1; in <= Ns[0]; in++)
   {
      n = Ns[in];
      for (im=1; im <= Ms[0]; im++)
      {
         m = Ms[im];
         lda = ldas[im];
         for (iy=1; iy <= incYs[0]; iy++)
         {
            incy = incYs[iy];
            for (ic=1; ic <= CONJs[0]; ic++)
            {
               conj = CONJs[ic];
               nerr += RunTest(conj, m, n, incy, lda);
               if (nerr && !verb)
                  return(nerr);
            }
         }
      }
   }
   return(nerr);
}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);

   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -n <#> <N1> ... <N#>\n");
   fprintf(stderr, "   -N <Nstart> <Nend> <Ninc>\n");
   fprintf(stderr, "   -m <#> <M1> ... <M#>\n");
   fprintf(stderr, "   -M <Mstart> <Mend> <Minc>\n");
   fprintf(stderr, "   -l <#> <lda1> ... <lda#>\n");
   fprintf(stderr, "   -g <ldagap> : lda = M + <ldagap> foreach M\n");
   fprintf(stderr, "   -y <#> <incY1> ... <incY#>\n");
   fprintf(stderr, "   -x <#> <incX1> ... <incX#>\n");
   fprintf(stderr, "   -C <#> <conj1> ... <conj#>\n");
   fprintf(stderr,
           "   -v [0,1] : 0 - stop on first error, else keep testing\n");
   fprintf(stderr,
"   -F[x,y,a] <#> : if(# > 0) -> force op to be aligned to at least # bytes\n");
   fprintf(stderr,
      "                   if(# < 0) -> force op to be aligned to < # bytes.\n");

   exit(ierr ? ierr : -1);
}


/* procedure 1 */
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

#ifdef TYPE
/* procedure 2 */
TYPE *GetTypeList1(const SCALAR val)
/*
 * Returns a TYPE array with arr[0] = 1.0, arr[1] = val
 */
{
   TYPE *arr;
   arr = malloc(ATL_MulBySize(2));
   ATL_assert(arr);
   arr[0] = 1;
   #ifdef TCPLX
      arr[2] = *val;
      arr[3] = val[1];
   #else
      arr[1] = val;
   #endif
   return(arr);
}
#endif

/* procedure 3 */
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

/* procedure 4 */
int *DupIntList(int *list)
/*
 * Duplicates list of integers, list[0] holds the length, not including 0
 */
{
   int i, n, *ip;

   assert(list);
   n = list[0] + 1;
   ip = malloc(sizeof(int)*n);
   assert(ip);
   for (i=0; i < n; i++)
      ip[i] = list[i];
   return(ip);
}

/* procedure 5 */
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

#ifdef TYPE
/* procedure 6 */
TYPE *GetTypeList(int nargs, char **args, int i, int nmul)
/*
 * Gets a list of TYPEs, whose length is given by atoi(args[i])*nmul
 * list is this length+1, since 0'th location gets atof(args[i])
 */
{
   int n, k;
   TYPE *arr;

   if (++i >= nargs)
      PrintUsage(args[0], i, NULL);
   n = atoi(args[i]) * nmul;
   ATL_assert(n > 0);
   arr = malloc(ATL_MulBySize(n+1));
   ATL_assert(arr);

   arr[0] = n / nmul;
   for (k=0; k < n; k++)
   {
      if (++i >= nargs)
         PrintUsage(args[0], i, NULL);
      arr[k+(1 SHIFT)] = atof(args[i]);
   }
   return(arr);
}
#endif

/* procedure 7 */
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

int GetFlags(int nargs, char **args, int **CONJs, int **Ms, int **Ns,
             int **LDAs, int **incYs, int **incXs)
{
   int verb, i, k, *ip;
   char ch;
   int ldagap = 8;

   *Ns = *Ms = *LDAs = *incYs = *incXs = *CONJs = NULL;
   verb = 0;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      ch = args[i][1];
      switch(ch)
      {
      case 'v':
        if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -g ");
         verb = atoi(args[i]);
         break;
      case 'g':
        if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -g ");
         ldagap = atoi(args[i]);
         break;
      case 'M':
      case 'N':
         if (i+3 >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -N/M ");
         ip = IntRange2IntList(atoi(args[i+1]),atoi(args[i+2]),atoi(args[i+3]));
         if (ch == 'M')
            *Ms = ip;
         else
            *Ns = ip;
         i += 3;
         break;
      case 'n':
      case 'm':
      case 'l':
      case 'y':
      case 'x':
         ip = GetIntList(nargs, args, i, 1);
         i += ip[0] + 1;
         switch(ch)
         {
         case 'n':
            *Ns = ip;
            break;
         case 'm':
            *Ms = ip;
            break;
         case 'l':
            *LDAs = ip;
            break;
         case 'y':
            *incYs = ip;
            break;
         case 'x':
            *incXs = ip;
            break;
         }
         break;
      case 'F':
         ch = tolower(args[i][2]);
         k = atoi(args[++i]);
         if (ch == 'a')
         {
            if (k < 0)
              MAa = -k;
            else
              FAa = k;
         }
         else if (ch == 'y')
         {
            if (k < 0)
              MAy = -k;
            else
              FAy = k;
         }
         else
         {
            if (k < 0)
              MAx = -k;
            else
              FAx = k;
         }
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   if (*CONJs == NULL)
   #ifdef TCPX
      *CONJs = GetIntList2(0, 1);
   #else
      *CONJs = GetIntList1(0);
   #endif
   if (*incXs == NULL)
      *incXs = GetIntList1(1);
   if (*incYs == NULL)
      *incYs = GetIntList1(1);
   if (*Ms == NULL)
      *Ms = GetIntList1(977);
   if (*Ns == NULL)
      *Ns = GetIntList1(77);
   if (*LDAs == NULL)
   {
      *LDAs = DupIntList(*Ms);
      for (i=1; i <= (*LDAs)[0]; i++)
         (*LDAs)[i] += ldagap;
   }
   assert((*LDAs)[0] == (*Ms)[0]);
   return(verb);
}

int main(int nargs, char **args)
{
   int *Ms, *Ns, *LDAs, *incYs, *incXs, *CONJs;
   int verb, ierr=0;

   verb = GetFlags(nargs, args, &CONJs, &Ms, &Ns, &LDAs, &incYs, &incXs);
   ierr = RunTests(verb, CONJs, Ms, Ns, incYs, LDAs);
   free(CONJs);
   free(incXs);
   free(incYs);
   free(Ms);
   free(Ns);
   free(LDAs);
   exit(ierr);
}
