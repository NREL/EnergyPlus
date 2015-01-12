#include "atlas_misc.h"
#include "atlas_threads.h"

void ATL_mutex_unlock(void *vp)
{
#if defined(ATL_WINTHREADS) || (defined(ATL_OS_OSX) && defined(ATL_SSE1))
   ATL_ResetAtomicCount(vp, 1);
#elif defined(ATL_OMP_THREADS)
   omp_unset_lock(vp);
#else
   ATL_assert(!pthread_mutex_unlock(vp));
#endif
}
