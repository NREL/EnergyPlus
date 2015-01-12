#include "atlas_threads.h"
#define DREAL
#include "atlas_misc.h"

void goparallel_dyn
   (const unsigned int P, void *DoWork, void *opstruct, void *DoComb);
void goparallel_lin
   (const unsigned int P, void *DoWork, void *opstruct, void *DoComb);
void goparallel_log2
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
   fprintf(stderr,
"USAGE: %s [-r <reps>] [-f flops] [-k <kflops>] [-m <mflops>] -W [which]\n",
           exe);
   fprintf(stderr,
   "   which: bitfield, 1st bit is dyn, 2nd is lg2, 3rd is linear\n");
   exit(-1);
}

int GetFlags(int nargs, char **args, int *which, size_t *flops)
{
   int i, reps=1;
   int imul=1;

   *flops = 1000000;
   *which = 7;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'r':
         if (++i >= nargs)
            PrintUsage(args[0]);
         reps = atoi(args[i]);
         break;
      case 'W':
         if (++i >= nargs)
            PrintUsage(args[0]);
         *which = atoi(args[i]);
         break;
      case 'm':
         imul *= 1000;
      case 'k':
         imul *= 1000;
      case 'f':
         if (++i >= nargs)
            PrintUsage(args[0]);
         *flops = imul * atoi(args[i]);
         imul = 1;
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   return(reps);
}
int main(int nargs, char **args)
{
   int i, k, nreps = 200, opstride, which;
   double t0, tlin, tlg2, tdyn, trnk;
   ATL_TUNE_T ta[ATL_NTHREADS];
   volatile double VV[16];
   size_t flops;

   tlg2 = tdyn = tlin = 0.0;
   nreps = GetFlags(nargs, args, &which, &flops);

   for (i=0; i < ATL_NTHREADS; i++)
   {
      ta[i].rank = i;
      ta[i].nthr = ATL_NTHREADS;
      ta[i].nflops = flops;
      ta[i].V = VV;
   }
   for (i=0; i < 16; i++)
      VV[i] = ATL_rzero;
   opstride = (int) ( ((char*)(ta+1)) - (char*)(ta) );

   printf("FINDING SPEED OF CREATE/DGEMM/JOIN USING %d REPITITIONS:\n",
          nreps);
   if (which & 1)
   {
      t0 = ATL_walltime();
      for (k=0; k < nreps; k++)
      {
         ATL_goparallel_dyn(ATL_NTHREADS, TuneDoWork, ta, NULL);
      }
      tdyn = ATL_walltime() - t0;
      printf("   dyn time = %e\n", (float)tdyn);
   }

   if (which & 2)
   {
      t0 = ATL_walltime();
      for (k=0; k < nreps; k++)
      {
         ATL_goparallel_log2(ATL_NTHREADS, TuneDoWork, ta, NULL);
      }
      tlg2 = ATL_walltime() - t0;
      printf("   lg2 time = %e\n", (float)tlg2);
   }

   if (which & 4)
   {
      t0 = ATL_walltime();
      for (k=0; k < nreps; k++)
      {
         ATL_goparallel_lin(ATL_NTHREADS, TuneDoWork, ta, NULL);
      }
      tlin = ATL_walltime() - t0;
      printf("   lin time = %e\n", (float)tlin);
   }
   if (which & 8)
   {
      t0 = ATL_walltime();
      for (k=0; k < nreps; k++)
      {
         ATL_goparallel_prank(ATL_NTHREADS, TuneDoWork_gp, ta, NULL);
      }
      trnk = ATL_walltime() - t0;
      printf("   rnk time = %e\n", (float)trnk);
   }
   if ((which | 7) == which)
      printf("DYNAMIC is %.2f%% of LINEAR and %.2f%% of LOG2 SPEED.\n",
             (tdyn/tlin)*100.0, (tdyn/tlg2)*100.0);
   if ((which & 1) && (which & 8))
      printf("rank dynamic is %.2f%% of affinity dynamic\n", (trnk/tdyn)*100.0);
   return(0);
}
