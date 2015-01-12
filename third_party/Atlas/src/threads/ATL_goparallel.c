#ifndef ATL_NOAFFINITY
   #include "atlas_taffinity.h"  /* include this file first! */
#endif
#include "atlas_misc.h"
#include "atlas_threads.h"

#if !defined(ATL_NOAFFINITY) && defined(ATL_PAFF_SELF) && defined(ATL_USEOPENMP)
static int ATL_setmyaffinity()
/*
 * Attempts to sets the affinity of an already-running thread.  The
 * aff_set flag is set to true whether we succeed or not (no point in
 * trying multiple times).
 * RETURNS: 0 on success, non-zero error code on error
 */
{
   int bindID;
   bindID = omp_get_thread_num();
   #ifdef ATL_RANK_IS_PROCESSORID
      bindID = bindID % ATL_AFF_NUMID;
   #else
      bindID = ATL_affinityIDs[bindID%ATL_AFF_NUMID];
   #endif
#ifdef ATL_PAFF_PLPA
   plpa_cpu_set_t cpuset;
   PLPA_CPU_ZERO(&cpuset);
   PLPA_CPU_SET(bindID, &cpuset);
   if (me->paff_set)
      return(0);
   me->paff_set = 1;
   return(plpa_sched_setaffinity((pid_t)0, sizeof(cpuset), &cpuset));
#elif defined(ATL_PAFF_PBIND)
   return(processor_bind(P_LWPID, P_MYID, bindID, NULL));
#elif defined(ATL_PAFF_SCHED)
   cpu_set_t cpuset;
   CPU_ZERO(&cpuset);
   CPU_SET(bindID, &cpuset);
   if (me->paff_set)
      return(0);
   me->paff_set = 1;
   return(sched_setaffinity(0, sizeof(cpuset), &cpuset));
#elif defined (ATL_PAFF_RUNON)
   if (me->paff_set)
      return(0);
   me->paff_set = 1;
   return(pthread_setrunon_np(bindID));
#elif defined(ATL_PAFF_BINDP)
   if (me->paff_set)
      return(0);
   me->paff_set = 1;
   return(bindprocessor(BINDTHREAD, thread_self(), bindID));
#elif defined(ATL_PAFF_CPUSET)  /* untried FreeBSD code */
   cpuset_t mycpuset;
   CPU_ZERO(&mycpuset);         /* no manpage, so guess works like linux */
   CPU_SET(bindID, &mycpuset);
   if (me->paff_set)
      return(0);
   me->paff_set = 1;
   return(cpuset_setaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1,
                             sizeof(mycpuset), &mycpuset));
#endif
   return(0);
}
#endif
#if defined(ATL_TUNING) && defined(ATL_NOAFFINITY)
   void *ATL_log2tlaunch_noaff(void *vp);
   #define ATL_goparallel ATL_goparallel_noaff
   #define ATL_dyntlaunch ATL_log2tlaunch_noaff
   #define ATL_USE_DYNAMIC 0
#elif defined(ATL_TUNING)
   #if defined(ATL_LAUNCH_LINEAR)
      #define ATL_goparallel ATL_goparallel_lin
      #define ATL_dyntlaunch ATL_lin0tlaunch
      #define ATL_USE_DYNAMIC 0
   #elif defined(ATL_LAUNCH_DYNAMIC)
      #define ATL_goparallel ATL_goparallel_dyn
      #define ATL_USE_DYNAMIC 1
   #else
      #ifdef ATL_LAUNCH_LOG2
         #define ATL_goparallel ATL_goparallel_log2
      #endif
      #define ATL_dyntlaunch ATL_log2tlaunch
      #define ATL_USE_DYNAMIC 0
   #endif
#else
   #define ATL_USE_DYNAMIC 1
#endif
void ATL_goparallel
/*
 * This function is used when you pass a single opstruct to all threads;
 * In this case, we stash opstruct in launchstruct's vp, and then use the
 * rank array as opstruct during the spawn.  Therefore, these routines
 * should expect to get their problem def from ls.vp, and their rank from
 * the second argument.  The DoWork function is the function that should
 * be called from each thread to do the parallel work.  This function should
 * look like:
 * void DoWork_example(ATL_LAUNCHSTRUCT_t *lp, void *vp)
 * {
 *    ATL_thread_t *tp = vp;
 *    const int myrank = tp->rank;
 *    my_prob_def_t *pd = lp->vp;
 *    ... do work based on info in struct pointed to by lp->vp ...
 * }
 * Your DoWork should perform any needed combine before finishing execution,
 * and any return values can be passed in the problem definition structure
 * that you define.
 */
(
   const unsigned int P, /* # of cores to use */
   void *DoWork,         /* func ptr to work function */
   void *opstruct,       /* structure giving tasks to threads */
   void *DoComb          /* function to combine two opstructs */
)
{
   ATL_thread_t *tp;
   int *chkin;
   void *vp, *lc;
   int i;
   ATL_LAUNCHSTRUCT_t ls;

   ls.OpStructIsInit = NULL;
   ls.DoWork = DoWork;
   ls.DoComb = DoComb;
   ls.opstruct = opstruct;
#ifdef ATL_OMP_THREADS
   tp = malloc(sizeof(ATL_thread_t)*P);
   ATL_assert(tp);
   for (i=0; i < P; i++)
   {
      tp[i].vp = &ls;
      tp[i].rank = i;
      tp[i].P = P;
   }
   ls.rank2thr = tp;
   omp_set_num_threads(P);
   #pragma omp parallel
   {
/*
 *    Make sure we got the requested nodes, and set affinity if supported
 */
      ATL_assert(omp_get_num_threads() == P);
      #ifdef ATL_PAFF_SELF
         ATL_setmyaffinity();
      #endif
      i = omp_get_thread_num();
      ls.DoWork(&ls, tp+i);
   }
/*
 * Do combine (linear) if requested
 */
   if (DoComb)
      for (i=1; i < P; i++)
         ls.DoComb(ls.opstruct, 0, i);
#else
   #if ATL_USE_DYNAMIC
      ls.acounts = &lc;
      ls.acounts[0] = ATL_SetGlobalAtomicCount(P>>1, P-1, 0);
      vp = malloc(P*(sizeof(ATL_thread_t)+sizeof(int)) + ATL_Cachelen);
      ATL_assert(vp);
      chkin = vp;
      tp = (ATL_thread_t*)(chkin+P);
      tp = ATL_AlignPtr(tp);
   #else
      vp = malloc(P*(sizeof(ATL_thread_t)) + ATL_Cachelen);
      tp = ATL_AlignPtr(vp);
   #endif
   ls.rank2thr = tp;

   for (i=0; i < P; i++)
   {
      tp[i].vp = &ls;
      tp[i].rank = i;
      tp[i].P = P;
      #if ATL_USE_DYNAMIC
         chkin[i] = 0;
      #endif
   }
   #if ATL_USE_DYNAMIC
      ls.chkin = (volatile int*) chkin;
   #endif
   ATL_thread_start(tp, 0, 1, ATL_dyntlaunch, tp);
   ATL_thread_join(tp);
   #if ATL_USE_DYNAMIC
      ATL_FreeGlobalAtomicCount(ls.acounts[0]);
   #endif
   free(vp);
#endif
}
