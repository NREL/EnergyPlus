#include "atlas_misc.h"
#include "atlas_threads.h"

void *ATL_SetGlobalAtomicCount
(
   int P,               /* # of Local counters to use to make global ctr */
   int cnt,             /* total count to start global count at */
   int percLoc          /* fraction of local work to reserve for callers */
)                       /* whose rank is exactly equal to the cnt index */
/*
 * This routine counts down from cnt to 0 for in a thread-safe way.
 * For scalability, the count is split up into P different counters
 * (this minimizes contention on atomic counters).  Further, if percLoc
 * is non-zero, then .01*fracLoc*local(cnt) numbers will be reserved exclusively
 * for callers that set their rank to the local counter index.  This allows
 * us to force a particular node to do at least that many columns, and for
 * those column accesses we can do it a non-rentrant read, which means it
 * runs much faster.  However, this means the caller will need to be sure
 * in this case that two processors cannot call with the same rank!
 */
{
   void **cnts;
   int *ip, *lcnts;
   int i, b, extra, nL, nG;

   b = cnt / P;
   extra = cnt - b*P;
   nL = (percLoc > 0) ? percLoc*.01*b : 0;
   nG = b - nL;
   i = ((P+3)>>2)<<2;
   ip = malloc(P*sizeof(void*)+(i+4)*sizeof(int));
   ATL_assert(ip);
   lcnts = ip+4;
   cnts = (void**)(lcnts + i);
   ip[0] = P;
   ip[1] = b;
   ip[2] = extra;
   ip[3] = nL;

   for (i=0; i < P; i++)
   {
      int n = nG;
      if (i < extra) n++;
      cnts[i] = ATL_SetAtomicCount(n);
      lcnts[i] = nL;
   }
   return((void*)ip);
}

