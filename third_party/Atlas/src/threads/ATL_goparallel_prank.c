#include "atlas_misc.h"
#include "atlas_threads.h"
#if defined(ATL_GAS_x8664) || defined(ATL_GAS_x8632)
   #define ATL_HAS_COREID
static unsigned int ATL_coreID(void)
{

  int myRetn=-1;
  __asm__ __volatile__ ("\n"
    "movl $1, %%eax\n"
    "cpuid\n"
    "shrl $24, %%ebx\n"
    "movl %%ebx, %0\n"
    : "=m" (myRetn)
    :
#if defined(ATL_GAS_x8632)
    :"%eax", "%ebx", "%edx", "%ecx"
#elif defined(ATL_GAS_x8664)
    :"%rax", "%rbx", "%rdx", "%rcx"
#endif
    );
  return(myRetn);
}
#else
   #define ATL_HAS_COREID
   #define _GNU_SOURCE 1
   #define __USE_GNU   1
   #include <sched.h>
   #define ATL_coreID sched_getcpu
#endif
#ifndef ATL_HAS_COREID
void ATL_goparallel_prank
(
   const unsigned int P, /* # of cores to use */
   void *DoWork,         /* func ptr to work function */
   void *opstruct,       /* will be stashed in launchstruct's vp */
   void *DoComb
)
{
   fprintf(stderr, "Hey chief, you are screwed:\n");
   fprintf(stderr,
      "  someone called goparallel_ptrank wt no way to determine prank!\n");
   exit(-1);
}
#else

typedef struct
{
   volatile int *coreIDs;   /* NTHR-len array providing non-unique coreIDs */
   volatile int *thrrnks;   /* NP-len array of chosen thread ranks */
   void *Tcnt;              /* atomic counter of # of threads launched */
   void *Trankcnt;          /* atomic ctr providing thread rank */
   int NT, NP;              /* # of threads & processors */
   int NLC;                 /* # of local cntrs in global acnts Tcnt/Trankcnt */
   pthread_attr_t attr;     /* attribute for pthread_create */
   ATL_LAUNCHSTRUCT_t *lp;  /* to pretend we've been launched normally */
} ATL_ranklaunch_t;


/*
 * Selects an ID from list A which does not appear in C
 * RETURNS: first unique ID, or -1 if no such ID found
 */
static int GetUniqueID
(
   int Na,   /* number of accepted unique IDs in U */
   int *A,   /* list of accepted unique IDs found so far */
   int Nc,   /* number of candidates left */
   int *C    /* non-unique candidates */
)
{
   int ic, ia;
   for (ic=0; ic < Nc; ic++)
   {
      for (ia=0; ia < Na && A[ia] != C[ic]; ia++);
      if (ia == Na)  /* found unique one */
         return(ic);
   }
   return(-1);
}

void *ATL_DoRankLaunch(void *vp)
{
   ATL_ranklaunch_t *rl = vp;
   ATL_LAUNCHSTRUCT_t *lp = rl->lp;
   const int T = rl->NT, P = rl->NP;
   int trank;   /* thread rank between 0 and NT-1 */
   int prank;   /* processor rank between 0 and NP-1 */
   int i, coreID;
   pthread_t pt;
/*
 * Cooperate with master to launch NT threads
 */
   coreID = ATL_coreID();
   trank = coreID % rl->NLC;
   #ifdef ATL_GLOBAL
   while(ATL_DecGlobalAtomicCount(rl->Tcnt, trank))
   #else
   while(ATL_DecAtomicCount(rl->Tcnt))
   #endif
      pthread_create(&pt, &rl->attr, ATL_DoRankLaunch, rl);
/*
 * Get my coreID, and tell master I'm alive by writing it to coreID array
 */
   #ifdef ATL_GLOBAL
      trank = T - ATL_DecGlobalAtomicCount(rl->Trankcnt, trank);
   #else
      trank = T - ATL_DecAtomicCount(rl->Trankcnt);
   #endif
   rl->coreIDs[trank] = coreID;
/*
 * Wait on master to signal ranking have been established
 */
   while(rl->coreIDs[0] == -1)
      ATL_thread_yield();
/*
 * See if my core has been selected for survival
 */
   for (i=0; i < P; i++)
      if (rl->thrrnks[i] == trank)
         break;
/*
 * If I'm not in worker list, signal completion by writing -2 to coreID array,
 * and quit
 */
   if (i == P)
   {
      rl->coreIDs[trank] = -2;  /* signal thread has completed */
      pthread_exit(NULL);
   }
/*
 * i is actually now my processor rank, fill my thread info in
 */
   prank = i;
/*   lp->rank2thr[prank].thrH = pthread_self(); */ /* don't need this */
   lp->rank2thr[prank].rank = prank;

   lp->DoWork(lp, lp->rank2thr+prank);  /* do work */

   rl->coreIDs[trank] = -2 ;    /* signal thread completion for master */
}

/*
 * This function is used when we don't have affinity, but do have some way
 * to determine the coreID, which must be a unique non-negative int.
 * It will launch 4*P threads in a detached state; all those threads that
 * start on unique cores will work on the problem, as will some on non-unique
 * cores that are necessary to get P threads out of the 4P created
 */
void ATL_goparallel_prank
(
   const unsigned int P, /* # of cores to use */
   void *DoWork,         /* func ptr to work function */
   void *opstruct,       /* will be stashed in launchstruct's vp */
   void *DoComb
)
{
   ATL_LAUNCHSTRUCT_t ls;
   ATL_ranklaunch_t rl;
   int T, t, i, j, prank, nunique, coreID;
   int *uids;    /* unique coreIDs */
   volatile int *coreIDs;
   pthread_t pt;
   pthread_attr_t *attr;
   void *vp;

   attr = &rl.attr;
   ls.DoWork = DoWork;
   ls.vp = opstruct;
   ls.DoComb = NULL;
   ls.chkin = NULL;
   ls.acounts = NULL;
   T = (P >= 8) ? P<<2 : P+P;
   rl.NT = T;
   rl.NP = P;
   #ifdef ATL_GLOBAL
      rl.NLC = P;
      rl.Tcnt = ATL_SetGlobalAtomicCount(rl.NLC, T-1, 0);
      rl.Trankcnt = ATL_SetGlobalAtomicCount(rl.NLC, T-1, 0);
   #else
      rl.NLC = T >> 2;
      rl.Tcnt = ATL_SetAtomicCount(T-1);
      rl.Trankcnt = ATL_SetAtomicCount(T-1);
   #endif
   rl.lp = &ls;
   uids = malloc(ATL_Cachelen+sizeof(int)*(T+P+P)+sizeof(ATL_thread_t)*P);
   ATL_assert(uids);
   coreIDs = rl.coreIDs = (volatile int*) (uids + P);
   rl.thrrnks = (volatile int*) (rl.coreIDs + T);
   vp = (void*) (rl.thrrnks + P);
   ls.rank2thr = ATL_AlignPtr(vp);
/*
 * Initialize attribute: detached with system scope
 */
   ATL_assert(!pthread_attr_init(attr));
   ATL_assert(!pthread_attr_setdetachstate(attr,PTHREAD_CREATE_DETACHED));
   pthread_attr_setscope(attr, PTHREAD_SCOPE_SYSTEM); /* no chk, OK to fail */
/*
 * Initialize rank arrays with -1; negative #s are codes, -1 means not init,
 * -2: started and then died.
 */
   for (i=0; i < P; i++)
      coreIDs[i] = rl.thrrnks[i] = -1;
   for (i=P; i < T; i++)
      coreIDs[i] = -1;
/*
 * Cooperate with worker threads to spawn all T threads
 */
   #ifdef ATL_GLOBAL
      while(ATL_DecGlobalAtomicCount(rl.Tcnt, 0))
   #else
      while(ATL_DecAtomicCount(rl.Tcnt))
   #endif
         pthread_create(&pt, attr, ATL_DoRankLaunch, &rl);
/*
 * Wait for all created threads to checkin; worker threads will all write
 * their coreID to their entry in the coreIDs array.  Their index in this array
 * is therefore their thread rank, which everyone agrees on due to using
 * the atomic counter.
 */
   coreID = ATL_coreID();  /* get my core ID */
   for (i=1; i < T; i++)
      while(coreIDs[i] == -1)
         ATL_thread_yield();
/*
 * All workers are spinning on coreIDs[0] awaiting my OK, so it is safe
 * to build all processor/thread ranking arrays
 */
   uids[0] = coreID;
   rl.thrrnks[0] = 0;   /* master is always first worker */
   for (i=1; i < P; i++)
   {
      j = GetUniqueID(i, uids, T-1, (int*)coreIDs+1) + 1;
      if (!j)
        break;
      rl.thrrnks[i] = j;
      uids[i] = coreIDs[j];
   }
   nunique = i;
/*
 * We didn't get P unique coreIDs, so choose some coreIDs to get extra threads
 * Try to map all extra threads to same cores as much as possible, to make
 * it more likely OS gets off its ass and reschedules; also, since we are
 * using dynamically scheduled ops, only a few processors will be running
 * at low speeds.
 */
   while (i < P)
   {
      int k;
      for (k=0; k < i; k++)
      {
         for (j=0; j < T; j++)
            if (coreIDs[j] == uids[k])
               break;
         if (j < T)
         {
            rl.thrrnks[i] = j;
            uids[i] = coreIDs[j];
            i++;
            break;
         }
      }
   }
/*
 * Signal to workers that thread mapping is complete, then do my portion of work
 */
   coreIDs[0] = coreID;
   ls.rank2thr[0].rank = 0;
   ls.DoWork(&ls, ls.rank2thr);  /* do work */
/*
 * I could have freed these resources after first checkin, but have delayed
 * until now to avoid possible context switch due to system call.  Free some
 * resources I'm no longer using
 */
   ATL_assert(!pthread_attr_destroy(attr));  /* spawning complete, release */
   #ifdef ATL_GLOBAL
      ATL_FreeGlobalAtomicCount(rl.Tcnt);
      ATL_FreeGlobalAtomicCount(rl.Trankcnt);
   #else
      ATL_FreeAtomicCount(rl.Tcnt);
      ATL_FreeAtomicCount(rl.Trankcnt);
   #endif
/*
 * Wait for all threads to complete before returning
 */
   if (nunique < P)
      printf("Node 0 awaits completion on %d unique cores; P=%d\n", nunique, P);
   for (i=1; i < T; i++)
      while(coreIDs[i] != -2)
        ATL_thread_yield();
   free(uids);
}
#endif
