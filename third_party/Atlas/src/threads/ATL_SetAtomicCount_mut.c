#include "atlas_misc.h"
#include "atlas_threads.h"

void *ATL_SetAtomicCount(int cnt)
{
#if defined(ATL_OMP_THREADS)
   char *cp;
   omp_lock_t *mp;
   int *cntp;
   cp = malloc(256+sizeof(int) + sizeof(omp_lock_t));
   ATL_assert(cp);
   cntp = (int*)(cp+128);  /* avoid false sharing wt 128-byte guard */
   mp = (omp_lock_t*)(cntp+2);
   omp_init_lock(mp);
   *cntp = cnt;
   return((void*)cp);
#else
   char *cp;
   pthread_mutex_t *mp;
   int *cntp;
   cp = malloc(256+sizeof(int) + sizeof(pthread_mutex_t));
   ATL_assert(cp);
   cntp = (int*)(cp+128);  /* avoid false sharing wt 128-byte guard */
   mp = (pthread_mutex_t*)(cntp+2);
   ATL_assert(!pthread_mutex_init(mp, NULL));
   *cntp = cnt;
   return((void*)cp);
#endif
}

