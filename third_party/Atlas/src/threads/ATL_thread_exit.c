#include "atlas_misc.h"
#include "atlas_threads.h"
void ATL_thread_exit(void *retval)
{
#ifdef ATL_WINTHREADS
   ExitThread((DWORD)(retval));
#elif defined(ATL_OMP_THREADS)
   fprintf(stderr, "Cannot call thread_exit using OpenMP!!\n");
   ATL_assert(0);  /* should never enter this rout when using OMP */
#else
   pthread_exit(retval);
#endif
}
