#include <stdlib.h>
#ifdef ATL_OMP_THREADS
   #include <omp.h>
#else
   #include <pthread.h>
#endif
#include "atlas_misc.h"
void ATL_FreeAtomicCount(void *vp)
{
   char *cp=vp;

#ifdef ATL_OMP_THREADS
   omp_destroy_lock((omp_lock_t*)(cp+2*sizeof(int)+128));
#else
   ATL_assert(!pthread_mutex_destroy((pthread_mutex_t*)(cp+2*sizeof(int)+128)));
#endif
   free(vp);
}
