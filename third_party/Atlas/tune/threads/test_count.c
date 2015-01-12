#include "atlas_threads.h"
#include "atlas_misc.h"
#include "assert.h"

static volatile char *checkin;

void PrintUsage(char *name)
{
   fprintf(stderr, "USAGE: %s [-r <reps>] [-c <cnt>]\n", name);
   exit(-1);
}

int GetFlags(int nargs, char **args, int *nreps)
{
   int i, cnt=16384;

   *nreps = 20;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'r':
         if (++i == nargs)
            PrintUsage(args[0]);
         *nreps = atoi(args[i]);
         break;
      case 'c':
         if (++i == nargs)
            PrintUsage(args[0]);
         cnt = atoi(args[i]);
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   return(cnt);
}

void TestDoWork(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   int i;
   ATL_thread_t *tp = vp;
   void *acnt = lp->opstruct;
   #ifdef ATL_GLOBAL
      const int iam = tp->rank;
   #endif
   do
   {
      #ifdef ATL_GLOBAL
         i = ATL_DecGlobalAtomicCount(acnt, iam);
      #else
         i = ATL_DecAtomicCount(acnt);
      #endif
      if (i < 1)
         break;
      checkin[i-1]++;
   }
   while(1);
}

int main(int nargs, char **args)
{
   int cnt, nreps, i, k;
   void *vp;

   cnt = GetFlags(nargs, args, &nreps);

   checkin = malloc(cnt*sizeof(char));
   assert(checkin);
   for (i=0; i < nreps; i++)
   {
      #ifdef ATL_GLOBAL
         vp = ATL_SetGlobalAtomicCount(ATL_NTHREADS, cnt, 0);
      #else
         vp = ATL_SetAtomicCount(cnt);
      #endif
      for (k=0; k < cnt; k++)
         checkin[k] = 0;
      ATL_goparallel(ATL_NTHREADS, TestDoWork, vp, NULL);
      for (k=0; k < cnt; k++)
         assert(checkin[k] == 1);
      #ifdef ATL_GLOBAL
         ATL_FreeGlobalAtomicCount(vp);
      #else
         ATL_FreeAtomicCount(vp);
      #endif
   }
   printf("TEST PASSED\n");
   return(0);
}
