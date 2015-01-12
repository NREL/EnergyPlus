#include "atlas_misc.h"
#include "atlas_threads.h"
void ATL_mutex_free(void *vp)
{
#if defined(ATL_WINTHREADS) || (defined(ATL_OS_OSX) && defined(ATL_SSE1))
   ATL_FreeAtomicCount(vp);
#elif defined(ATL_OMP_THREADS)
   omp_destroy_lock(vp);
   free(vp);
#else
   ATL_assert(!pthread_mutex_destroy(vp));
   free(vp);
#endif
}
