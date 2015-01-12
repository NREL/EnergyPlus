#include "atlas_misc.h"
#include "atlas_threads.h"

void ATL_FreeGlobalAtomicCount(void *vp)
{
   int *ip=vp;
   const int P=ip[0];
   void **acnts = (void**)(ip+4+(((P+3)>>2)<<2));
   int i;
   for (i=0; i < P; i++)
      ATL_FreeAtomicCount(acnts[i]);
   free(vp);
}

