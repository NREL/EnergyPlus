#ifdef ATL_OMP_THREADS
   #include <omp.h>
#else
   #include <pthread.h>
#endif
int ATL_ResetAtomicCount(void *vp, int cnt)
{
   char *cp=vp;
   #ifdef ATL_OMP_THREADS
      omp_lock_t *mp;
   #else
      pthread_mutex_t *mp;
   #endif
   int *cntp;
   int iret;

   cntp = (int*)(cp+128);
   #ifdef ATL_OMP_THREADS
      mp = (omp_lock_t*)(cntp+2);
      omp_set_lock(mp);
   #else
      mp = (pthread_mutex_t*)(cntp+2);
      pthread_mutex_lock(mp);
   #endif
   iret = *cntp;
   *cntp = cnt;
   #ifdef ATL_OMP_THREADS
      omp_unset_lock(mp);
   #else
      pthread_mutex_unlock(mp);
   #endif
   return(iret);
}
