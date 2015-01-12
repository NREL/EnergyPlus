#include "atlas_misc.h"
#include "atlas_threads.h"

void ATL_ResetGlobalAtomicCount(void *vp, int cnt, int percLoc)
/*
 * This routine resets the global atomic counter vp to cnt
 */
{
   int *ip=vp, *lcnts = ip+4;
   const int P = *ip;
   void **cnts = (void**)(lcnts+(((P+3)>>2)<<2));
   int i, b, extra, nL, nG;

   b = cnt / P;
   extra = cnt - b*P;
   nL = (percLoc > 0) ? percLoc*.01*b : 0;
   nG = b - nL;
   ip[0] = P;
   ip[1] = b;
   ip[2] = extra;
   ip[3] = nL;

   for (i=0; i < P; i++)
   {
      int n = nG;
      if (i < extra) n++;
      ATL_ResetAtomicCount(cnts[i], n);
      lcnts[i] = nL;
   }
}

