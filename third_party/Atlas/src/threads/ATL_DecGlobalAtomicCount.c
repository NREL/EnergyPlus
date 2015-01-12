#include "atlas_misc.h"
#include "atlas_threads.h"
int ATL_DecGlobalAtomicCount(void *vp, int rank)
/*
 * This routine returns a global counter that has been distributed over
 * P local counters
 */
{
   int i, j, P, b, icnt, extra, nL, *ip=vp, *iloc;
   void **acnts;

   P = ip[0];
   b = ip[1];
   extra = ip[2];
   nL = ip[3];
   iloc = ip+4;
/*
 * See if I can get the index from purely local information
 */
   if (rank < P && rank >= 0 && nL)
   {
      j = iloc[rank];
      if (j)
      {
         iloc[rank]--;
         j += b * rank + Mmin(rank, extra);
//fprintf(stderr, "%d: j=%d, LRET\n", rank, j);
         return(j);
      }
   }
   acnts = (void**) (ip+4+(((P+3)>>2)<<2));
/*
 * Otherwise, find an atomic counter that still has count
 */
   for (i=0; i < P; i++)
   {
/*
 *    If I got a counter value, convert it from local to global
 */
      icnt = (rank+i)%P;
      if (j = ATL_DecAtomicCount(acnts[icnt]))
      {
         j += nL + b*icnt + Mmin(icnt,extra);
         break;
      }
   }
//fprintf(stderr, "%d: j=%d, icnt=%d, b=%d P=%d, e=%d\n", rank, j, icnt, b, P, extra);
   return(j);
}
