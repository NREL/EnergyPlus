#ifndef ATL_NOAFFINITY
   #include "atlas_taffinity.h"  /* include this file first! */
#endif
#include "atlas_misc.h"
#include "atlas_threads.h"

#if !defined(ATL_NOAFFINITY) && defined(ATL_PAFF_SELF)
static int ATL_setmyaffinity(ATL_thread_t *me)
/*
 * Attempts to sets the affinity of an already-running thread.  The
 * aff_set flag is set to true whether we succeed or not (no point in
 * trying multiple times).
 * RETURNS: 0 on success, non-zero error code on error
 */
{
   #ifdef ATL_RANK_IS_PROCESSORID
      const int bindID = me->rank % ATL_AFF_NUMID;
   #else
      const int bindID = ATL_affinityIDs[me->rank%ATL_AFF_NUMID];
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
#ifdef ATL_NOAFFINITY
   #define ATL_tDoWorkWrap ATL_tDoWorkWrap_noaff
#endif
void *ATL_tDoWorkWrap(void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_LAUNCHSTRUCT_t *lp = tp->vp;
   lp->DoWork(lp, tp);
   return(NULL);
}

#if defined(ATL_TUNING) && defined(ATL_NOAFFINITY)
   #define ATL_lin0tlaunch ATL_lin0tlaunch_noaff
   #define ATL_thread_start ATL_thread_start_noaff
#endif
void *ATL_lin0tlaunch(void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_LAUNCHSTRUCT_t *lp;
   const int P = tp->P;
   int i;
/*
 * Set my affinity if I haven't already
 */
   #ifdef ATL_PAFF_SELF
      if (!tp->paff_set)
          ATL_setmyaffinity(tp);
   #endif
/*
 * Spawn DoWork to all nodes
 */
   lp = tp->vp;
   for (i=1; i < P; i++)
   {
      ATL_thread_start(tp+i, i, 1, ATL_tDoWorkWrap, tp+i);
   }
/*
 * Thread 0 must also do the operation
 */
   lp->DoWork(lp, tp);
/*
 * Await completion of each task, and do combine (linear!) if requested
 */
   for (i=1; i < P; i++)
   {
      ATL_thread_join(tp+i);
      if (lp->DoComb)  /* do combine if necessary */
         lp->DoComb(lp->opstruct, 0, i);
   }
   return(NULL);
}
