#include "atlas_taffinity.h"
#include "atlas_threads.h"
#define DREAL
#include "atlas_misc.h"

void ATL_goparallel_noaff
   (const unsigned int P, void *DoWork, void *opstruct, void *DoComb);

typedef struct
{
   size_t nflops;               /* number of flops to perform */
   volatile double *V;          /* 16-length array of zeros */
   int rank, nthr;
} ATL_TUNE_T;


void InCacheGemm
(
   size_t nflops,               /* how many flops to do */
   volatile double *V           /* 16-length array of zeros */
)
/*
 * This routine emulates an in-cache 4x4 GEMM, but using only 16 registers
 * V is declared volatile so compiler doesn't get rid of the loop.
 */
{
   size_t i;
   register double c0, c1, c2, c3, c4, c5, c6, c7;
   register double a0, a1, a2, a3, b0, b1, b2, b3;

   a0 = *V;   b0 = V[4];
   a1 = V[1]; a2 = V[2];
   c0 = c1 = c2 = c3 = c4 = c5 = c6 = c7 = ATL_rzero;
   for (i=(nflops>>5); i; i--)
   {
      c0 += a0*b0; a3 = V[3];
      c1 += a1*b0;
      c2 += a2*b0; b1 = V[5];
      c3 += a3*b0;
      c4 += a0*b1; b2 = V[6];
      c5 += a1*b1;
      c6 += a2*b1;
      c7 += a3*b1;
      c0 += a0*b2; b3 = V[7];
      c1 += a1*b2;
      c2 += a2*b2; b0 = V[4];
      c3 += a3*b2;
      c4 += a0*b3; a0 = *V;
      c5 += a1*b3; a1 = V[1];
      c6 += a2*b3; a2 = V[2];
      c7 += a3*b3;
   }
   *V = c0;   V[1] = c1; V[2] = c2; V[3] = c3;
   V[4] = c4; V[5] = c5; V[6] = c6; V[7] = c7;
}

void TuneDoWork_gp(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_TUNE_T *tp = lp->opstruct;
   int i;
   InCacheGemm(tp->nflops, tp->V);
}

void TuneDoWork(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_TUNE_T *tp = lp->opstruct;
   int i;
   InCacheGemm(tp->nflops, tp->V);
}

void PrintUsage(char *exe)
{
   fprintf(stderr, "USAGE: %s [-r <reps>] -m/k/f [m/k/flops] -o outfile\n",
           exe);
   exit(-1);
}

int GetFlags(int nargs, char **args, size_t *nflop, char **outfile)
{
   int i, reps=50, imul;

   *outfile = NULL;
   *nflop = 2*300 * 300 * 300;  /* emulate 300x300 DGEMM */
   for (i=1; i < nargs; i++)
   {
      imul = 1;
      if (args[i][0] != '-')
         PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'r':
         if (++i >= nargs)
            PrintUsage(args[0]);
         reps = atoi(args[i]);
         break;
      case 'm':
         imul *= 1000;
      case 'k':
         imul *= 1000;
      case 'f':
         if (++i >= nargs)
            PrintUsage(args[0]);
         *nflop = atoll(args[i]) * imul;
         break;
      case 'o':
         if (++i >= nargs)
            PrintUsage(args[0]);
         *outfile = args[i];
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   return(reps);
}
int main(int nargs, char **args)
{
#ifndef ATL_OMP_THREADS
   size_t nflops;
   int i, k, nreps = 200, opstride, which;
   double t0, taff, tnoa;
   ATL_TUNE_T ta[ATL_NTHREADS];
   volatile double *V;
   void *vp[ATL_NTHREADS];
   char *outfile;


   taff = tnoa = 0.0;
   nreps = GetFlags(nargs, args, &nflops, &outfile);

   for (i=0; i < ATL_NTHREADS; i++)
   {
      ta[i].rank = i;
      ta[i].nthr = ATL_NTHREADS;
      ta[i].nflops = nflops;
      vp[i] = malloc(sizeof(double)*16 + ATL_Cachelen);
      ATL_assert(vp[i]);
      ta[i].V = ATL_AlignPtr(vp[i]);
      ATL_dzero(16, (double*)ta[i].V, 1);  /* zero w/o telling compiler */
   }
   opstride = (int) ( ((char*)(ta+1)) - (char*)(ta) );

   printf("FINDING WHETHER AFFINITY IS HELPFUL USING FLOPS=%e NREPS=%d\n",
          (double)nflops, nreps);

   t0 = ATL_walltime();
   for (k=0; k < nreps; k++)
      ATL_goparallel(ATL_NTHREADS, TuneDoWork, ta, NULL);
   taff = ATL_walltime() - t0;
   printf("   Affinity    time = %e\n", (float)taff);

   t0 = ATL_walltime();
   for (k=0; k < nreps; k++)
      ATL_goparallel_noaff(ATL_NTHREADS, TuneDoWork, ta, NULL);
   tnoa = ATL_walltime() - t0;
   printf("   NO affinity time = %e\n", (float)tnoa);

   printf("Affinity speedup = %.2f\n", (float)(tnoa / taff));

   for (i=0; i < ATL_NTHREADS; i++)
      free(vp[i]);

   if (outfile)  /* if this is a real run where we want to change things */
   {
      if (tnoa*1.04 < taff)
      {
         FILE *fpout;
         printf(
       "Affinity is not helpful on your system, forcing ATLAS not to use it\n");
         fpout = fopen(outfile, "w");
         ATL_assert(fpout);
         fprintf(fpout, "#ifndef ATL_TAFFINITY_H\n   #define ATL_TAFFINITY_H\n");
         fprintf(fpout, "   #define ATL_NOAFFINITY 1\n");
         fprintf(fpout, "#endif\n");
         fclose(fpout);
         fpout = fopen("res/aff.h", "w");
         fprintf(fpout, "#define ATL_TAFFINITY 0\n");
         fclose(fpout);
      }
      else /* affinity was a win */
      {
         FILE *fpout;
         fpout = fopen("res/aff.h", "w");
         fprintf(fpout, "#define ATL_TAFFINITY 1\n");
         fclose(fpout);
      }
   }
#else
   FILE *fpout;
   char *outfile;
   size_t nflops;
   int nreps;

   nreps = GetFlags(nargs, args, &nflops, &outfile);
   if (outfile)
   {
      printf(
      "Not good idea to set affinity wt OpenMP; forcing ATLAS not to use it\n");
      fpout = fopen(outfile, "w");
      ATL_assert(fpout);
      fprintf(fpout, "#ifndef ATL_TAFFINITY_H\n   #define ATL_TAFFINITY_H\n");
      fprintf(fpout, "   #define ATL_NOAFFINITY 1\n");
      fprintf(fpout, "#endif\n");
      fclose(fpout);
      fpout = fopen("res/aff.h", "w");
      fprintf(fpout, "#define ATL_TAFFINITY 0\n");
      fclose(fpout);
   }
#endif
   return(0);
}
