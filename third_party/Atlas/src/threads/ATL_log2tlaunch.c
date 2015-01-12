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
#if defined(ATL_TUNING) && defined(ATL_NOAFFINITY)
void *ATL_log2tlaunch_noaff(void *vp)
#else
void *ATL_log2tlaunch(void *vp)
#endif
{
   ATL_thread_t *tp = vp, *btp;
   ATL_LAUNCHSTRUCT_t *lp;
   int i, iam, abit, mask, src, dest, nthrP2;
   const int P=tp->P;

   iam = tp->rank;
   for (i=0; (1<<i) < P; i++);
   nthrP2 = i;
/*
 * Set my affinity if I haven't already
 */
   #ifdef ATL_PAFF_SELF
      if (!tp->paff_set)
          ATL_setmyaffinity(tp);
   #endif
   btp = tp - iam;
   lp = tp->vp;
   mask = (1<<nthrP2) - 1;   /* no threads are in at beginning */
/*
 * Take log_2(NTHR) steps to do log_2 launch
 */
   for (i=nthrP2-1; i >= 0; i--)
   {
      abit = (1<<i);
      mask ^= abit;   /* double the # of threads participating */
      if (!(iam & mask))
      {
         if (!(iam & abit))
         {
            dest = iam ^ abit;
            if ( dest < P)
               ATL_thread_start(btp+dest, dest, 1, ATL_log2tlaunch, btp+dest);
         }
      }
   }
   lp->DoWork(lp, tp);   /* do the operation */
/*
 * Join tree back up, combining results as required
 */
   mask = 0;
   for (i=0; i < nthrP2; i++)
   {
      if (!(iam & mask))
      {
         abit = (1<<i);
         if (!(iam & abit))
         {
            src = iam ^ abit;
            if (src < P)
            {
               ATL_thread_join(btp+src);
               if (lp->DoComb)
                  lp->DoComb(lp->opstruct, iam, src);
            }
         }
         else
            ATL_thread_exit(NULL);
      }
      mask |= abit;
   }
   return(NULL);
}
