#include "atlas_misc.h"
#include "atlas_threads.h"
int ATL_thread_join(ATL_thread_t *thr)   /* waits on completion of thread */
{
#ifdef ATL_WINTHREADS
   ATL_assert(WaitForSingleObject(thr->thrH, INFINITE) != WAIT_FAILED);
   ATL_assert(CloseHandle(thr->thrH));
#elif defined(ATL_OMP_THREADS)
   fprintf(stderr, "Cannot call thread_join using OpenMP!!\n");
   ATL_assert(0);  /* should never enter this rout when using OMP */
#else
   ATL_assert(!pthread_join(thr->thrH, NULL));
#endif
   return(0);
}
