#include "atlas_threads.h"
#include "atlas_misc.h"
void *ATL_SetAtomicCount_mut(long long);

static volatile int count=0;
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static void *acnt;
static void **acnts;
static double *timearr=NULL;

int ATL_DecAtomicCount_ser(void)
{
   int iret=0;
   if (count)
   {
      iret = count;
      count--;
   }
   return(iret);
}


void TuneDoWork_ser(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   double t0, t1;
   ATL_thread_t *tp = vp;
   ATL_CINT iam = tp->rank;
   int lcount, i;

   t0 = ATL_walltime();
#ifdef UNSAFE
   while (ATL_DecAtomicCount_ser(acnt));
#else
   lcount = ATL_GetAtomicCount(acnt);
   if (iam)
      lcount = lcount / ATL_NTHREADS;
   else
      lcount = lcount / ATL_NTHREADS + lcount % ATL_NTHREADS;
   while (lcount--)
      i = ATL_GetAtomicCount(acnt);
#endif
   timearr[iam] = ATL_walltime() - t0;
}

void TuneDoWork_mut(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   double t0, t1;
   ATL_thread_t *tp = vp;
   ATL_CINT iam = tp->rank;

   t0 = ATL_walltime();
   while (ATL_DecAtomicCount_mut(acnt));
   timearr[iam] = ATL_walltime() - t0;
}

void TuneDoWork_loc(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   double t0, t1;
   ATL_thread_t *tp = vp;
   ATL_CINT iam = tp->rank;
   int i;

   t0 = ATL_walltime();
   for (i=0; i < ATL_NTHREADS; i++)
   {
      void *lacnt = acnts[(iam+i)%ATL_NTHREADS];
      while (ATL_DecAtomicCount(lacnt));
   }
   t1 = ATL_walltime();
   timearr[iam] = t1 - t0;
}

void TuneDoWork(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   double t0, t1;
   ATL_thread_t *tp = vp;
   ATL_CINT iam = tp->rank;

   t0 = ATL_walltime();
   while (ATL_DecAtomicCount(acnt));
   t1 = ATL_walltime();
   timearr[iam] = t1 - t0;
}

void TuneDoWork_gc(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   double t0, t1;
   ATL_thread_t *tp = vp;
   ATL_CINT iam = tp->rank;

   t0 = ATL_walltime();
   while (ATL_DecGlobalAtomicCount(acnt, iam));
   t1 = ATL_walltime();
   timearr[iam] = t1 - t0;
}


void PrintRank(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   printf("%d: awake with vp=%p\n", tp->rank, lp->vp);
}

void PrintUsage(char *exe)
{
   fprintf(stderr, "USAGE: %s [-r <reps>] -o outfile\n", exe);
   exit(-1);
}

size_t GetFlags(int nargs, char **args, char **outfile)
{
   int i;
   size_t reps = 1000000;

   *outfile = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'r':
         if (++i >= nargs)
            PrintUsage(args[0]);
         reps = atoll(args[i]);
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
   double t0, tmut_s, tdec_s, tser_s, tmut, tdec, tser, tldec;
   size_t nreps, i, lcnt;
   ATL_thread_t ts;
   char *outfile;

   nreps = GetFlags(nargs, args, &outfile);

   printf("FINDING SPEED OF SERIAL COUNTER CHANGING USING %ld REPS:\n",
          (long int) nreps);
   ts.rank = 0;
   timearr = malloc(sizeof(double)*ATL_NTHREADS);
   ATL_assert(timearr);

   acnt = ATL_SetAtomicCount(nreps);
   TuneDoWork(NULL, &ts);
   tdec_s = timearr[0];
   printf("   serial AtoDec time = %e\n", tdec_s);
   ATL_FreeAtomicCount(acnt);
   acnt = NULL;

   acnt = ATL_SetAtomicCount_mut(nreps);
   TuneDoWork_mut(NULL, &ts);
   tmut_s = timearr[0];
   ATL_FreeAtomicCount_mut(acnt);
   printf("   serial mutex  time = %e\n", tmut_s);

   acnt = ATL_SetAtomicCount_mut(nreps);
   count = nreps;
   TuneDoWork_ser(NULL, &ts);
   tser_s = timearr[0];
   ATL_FreeAtomicCount_mut(acnt);
   printf("   serial/read unsafe time = %e\n", tser_s);

   #ifdef PentiumCPS
      t0 = (1000000.0/nreps)*PentiumCPS;
      printf("   CYCLES PER CALL: SER=%.1f, DEC=%.1f, MUT=%.1f\n",
             t0*tser_s, t0*tdec_s, t0*tmut_s);
   #endif
   t0 = 1000000.0 / nreps;
   printf("   MICROSECONDS PER CALL: SER=%.2f DEC=%.2f, MUT=%.2f\n",
          tser_s*t0, tdec_s*t0, tmut_s*t0);
   printf("DEC TIME SPEEDUP OVER MUTEX   = %.2f\n", tmut_s / tdec_s);
   printf("UNSAFE/READ SPEEDUP OVER DEC = %.2f\n\n", tdec_s / tser_s);

   printf("FINDING SPEED OF PARALLEL COUNTER CHANGING USING %ld REPS %d PROC\n",
          (long int) nreps, ATL_NTHREADS);


   acnt = ATL_SetGlobalAtomicCount(ATL_NTHREADS, nreps, 0);
   ATL_goparallel(ATL_NTHREADS, TuneDoWork_gc, NULL, NULL);
   for (tldec=0.0,i=0; i < ATL_NTHREADS; i++)
      tldec = Mmax(tldec,timearr[i]);
   ATL_FreeGlobalAtomicCount(acnt);
   printf("   parallel GblDec time = %e (par/ser = %.2f)\n",
          tldec, tldec/tdec_s);

   acnt = ATL_SetAtomicCount_mut(nreps);
   acnt = ATL_SetAtomicCount(nreps);
   ATL_goparallel(ATL_NTHREADS, TuneDoWork, NULL, NULL);
   for (tdec=0.0,i=0; i < ATL_NTHREADS; i++)
      tdec = Mmax(tdec,timearr[i]);
   ATL_FreeAtomicCount(acnt);
   printf("   parallel AtoDec time = %e (par/ser = %.2f)\n", tdec, tdec/tdec_s);

   acnt = ATL_SetAtomicCount_mut(nreps);
   ATL_goparallel(ATL_NTHREADS, TuneDoWork_mut, NULL, NULL);
   for (tmut=0.0,i=0; i < ATL_NTHREADS; i++)
      tmut = Mmax(tmut,timearr[i]);
   ATL_FreeAtomicCount_mut(acnt);
   printf("   parallel mutex  time = %e (par/ser = %.2f)\n", tmut, tmut/tmut_s);

   count = nreps;
   acnt = ATL_SetAtomicCount_mut(nreps);
   ATL_goparallel(ATL_NTHREADS, TuneDoWork_ser, NULL, NULL);
   for (tser=0.0,i=0; i < ATL_NTHREADS; i++)
      tser = Mmax(tser,timearr[i]);
   ATL_FreeAtomicCount_mut(acnt);
   printf("   parallel unsafe time = %e (par/ser = %.2f)\n", tser, tser/tser_s);

   #ifdef PentiumCPS
      t0 = 1000000.0*PentiumCPS;
      printf("   CYCLES PER CALL: SER=%.1f, DEC=%.1f, GBLDEC=%.1f, MUT=%.1f\n",
             t0*(tser/nreps), t0*(tdec/nreps), t0*(tldec/nreps),
             t0*(tmut/nreps));
   #endif
   t0 = 1000000.0 / nreps;
   printf(
   "   MICROSECONDS PER CALL: SER=%.2f DEC=%.2f, GBLDEC=%.2f, MUT=%.2f\n",
          tser*t0, tdec*t0, tldec*t0, tmut*t0);
   printf("DEC TIME SPEEDUP OVER MUTEX   = %.2f\n", tmut / tdec);
   printf("GBLDEC TIME SPEEDUP OVER MUTEX   = %.2f\n", tmut / tldec);
/*
 * Change nothing unless outfile is non-NULL
 */
   if (outfile)
   {
      FILE *fpout;
/*
 *    If my assembly isn't noticably faster than the mutex code, just use the
 *    the mutex code
 */
      if (tmut < tldec*1.02)
      {
         printf("\nNO REAL ADVANTAGE TO ASSEMBLY, FORCING USE OF MUTEX\n");
         ATL_assert(!system("make iForceUseMutex"));
      }
   }
   free(timearr);
   return(0);
}
