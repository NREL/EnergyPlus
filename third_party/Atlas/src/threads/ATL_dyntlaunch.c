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
void *ATL_dyntlaunch_noaff(void *vp)
#else
void *ATL_dyntlaunch(void *vp)
#endif
{
   ATL_thread_t *tp = vp, *btp;
   ATL_LAUNCHSTRUCT_t *lp;
   const int iam = tp->rank, P = tp->P;
   int i, src, dest, nthrP2, mask, abit;
   void *acnt;

   lp = tp->vp;
   acnt = lp->acounts[0];
   btp = tp - iam;
/*
 * Set my affinity if I haven't already
 */
   #ifdef ATL_PAFF_SELF
      if (!tp->paff_set)
          ATL_setmyaffinity(tp);
   #endif
   dest = ATL_DecGlobalAtomicCount(acnt, iam);
   while(dest)
   {
      dest = tp->P - dest;
      ATL_thread_start(btp+dest, dest, 0, ATL_dyntlaunch, btp+dest);
      dest = ATL_DecGlobalAtomicCount(acnt, iam);
   }
/*
 * Do the operation
 */
   lp->DoWork(lp, tp);
/*
 * Do combine in minimum spanning tree, combining results as required
 */
   for (i=0; (1<<i) < P; i++);
   nthrP2 = i;
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
               while (lp->chkin[src] != ATL_CHK_DONE_OP)
                  ATL_POLL;
               if (lp->DoComb)
                  lp->DoComb(lp->opstruct, iam, src);
            }
         }
         else
         {
            lp->chkin[iam] = ATL_CHK_DONE_OP;
            ATL_thread_exit(NULL);
         }
      }
      mask |= abit;
   }
   return(NULL);
}
