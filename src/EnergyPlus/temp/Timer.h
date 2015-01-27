
//#define TIMER_CPU_TIME
#define TIMER_F90_EPTIME

// if not using openmp, can't use the openmp timer
#if !defined(_OPENMP) && defined(TIMER_OMP_GET_WTIME)
#  undef TIMER_OMP_GET_WTIME
//#define TIMER_CPU_TIME
#  define TIMER_F90_EPTIME
#endif

#if defined(TIMER_F90_EPTIME)
#  define TSTART(x) x=epelapsedtime()
#  define TSTOP(x)  x=epelapsedtime()
#  define TSTAMP(x) x=epelapsedtime()
#elif defined(TIMER_CPU_TIME)
#  define TSTART(x) CPU_TIME(x)
#  define TSTOP(x)  CPU_TIME(x)
#  define TSTAMP(x) CPU_TIME(x)
#elif defined(TIMER_OMP_GET_WTIME)
#  define TSTART(x) x=OMP_get_wtime()
#  define TSTOP(x)  x=OMP_get_wtime()
#  define TSTAMP(x) x=OMP_get_wtime()
#else
  NEED_TO_SPECIFY_TIMER
#endif

// int OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS;
#ifdef _OPENMP
#define THREADID(a) OMP_GET_THREAD_NUM()
#define NUMTHREADS(a) OMP_GET_NUM_THREADS()
#define MAXTHREADS(a) OMP_GET_MAX_THREADS()
#else
#define THREADID(a) 1
#define NUMTHREADS(a) 1
#define MAXTHREADS(a) 1
#endif
