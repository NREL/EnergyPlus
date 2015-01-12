#include "atlas_misc.h"
#include "atlas_threads.h"

void ATL_mutex_lock(void *vp)
{
#ifdef ATL_WINTHREADS  /* if not using pthreads, use AtomicCount to sim mut */
   while(!ATL_DecAtomicCount(vp));
#elif defined(ATL_OS_OSX) && defined(ATL_SSE1)
   while(!ATL_DecAtomicCount(vp))
      ATL_thread_yield();
#elif defined(ATL_OMP_THREADS)
   omp_set_lock(vp);
#else
   ATL_assert(!pthread_mutex_lock(vp));
#endif
}
