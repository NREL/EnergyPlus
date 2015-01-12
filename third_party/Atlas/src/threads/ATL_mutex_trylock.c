#include "atlas_misc.h"
#include "atlas_threads.h"

int ATL_mutex_trylock(void *vp)
/*
 * return 0 if lock not required, else return non-zero
 */
{
#if defined(ATL_WINTHREADS) || (defined(ATL_OS_OSX) && defined(ATL_SSE1))
   return(ATL_DecAtomicCount(vp));
#elif defined(ATL_OMP_THREADS)
   return(omp_test_lock(vp));
#else
   return(!pthread_mutex_trylock(vp));
#endif
}
