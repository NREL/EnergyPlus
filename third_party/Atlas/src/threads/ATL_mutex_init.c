#include "atlas_misc.h"
#include "atlas_threads.h"
void *ATL_mutex_init(void)
{
/*
 * On Windows, use known-good x86 code.  OS X's mutex have horrible scaling,
 * so use homebrewed code instead
 */
#if defined(ATL_WINTHREADS) || (defined(ATL_OS_OSX) && defined(ATL_SSE1))
   return(ATL_SetAtomicCount(1));
#elif defined(ATL_OMP_THREADS)
   void *vp;
   vp = malloc(sizeof(omp_lock_t));
   ATL_assert(vp);
   omp_init_lock(vp);
   return(vp);
#else
   void *vp;
   vp = malloc(sizeof(pthread_mutex_t));
   ATL_assert(vp);
   ATL_assert(!pthread_mutex_init(vp, NULL));
   return(vp);
#endif
}
