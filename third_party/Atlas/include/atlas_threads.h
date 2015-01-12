#ifndef ATLAS_THREADS_H
   #define ATLAS_THREADS_H

#ifndef ATL_PTMAXMALLOC
   #ifndef ATL_PTMAXMALLOC_MB
      #define ATL_PTMAXMALLOC_MB 128
   #endif
   #define ATL_PTMAXMALLOC (((size_t)(ATL_PTMAXMALLOC_MB)<<20)>>ATL_NTHRPOW2)
#endif
/*
 * If we don't have thread affinity, then the thread I'm waiting on may share
 * the core with me.  In this case, yield my time slice when polling
 */
#include "atlas_tsumm.h"
#if defined(ATL_TAFFINITY) && ATL_TAFFINITY
   #define ATL_POLL
#else
   #define ATL_POLL ATL_thread_yield()
#endif

#if defined(ATL_OS_Win64) || defined(ATL_OS_WinNT)
   #ifdef ATL_USE64BITS
      #define ATL_WIN64THREADS 1
      #define ATL_WINTHREADS 1
   #else
      #define ATL_WIN32THREADS 1
      #define ATL_WINTHREADS 1
   #endif
#endif
#include "atlas_pthreads.h" /* gened file defs ATL_NTHREADS & ATL_NTHRPOW2 */
#ifdef ATL_WINTHREADS
   #include <windows.h>
   typedef struct
   {
      void *vp;      /* ptr to extra info */
      HANDLE thrH;   /* handle to thread */
      int rank;      /* my rank */
      int P;         /* # of processors in this call */
   } ATL_thread_t;
   #ifndef CREATE_SUSPENDED
      #define CREATE_SUSPENDED 0x00000004
   #endif
   #ifndef WAIT_FAILED
      #define WAIT_FAILED (~0)
   #endif
#elif defined(ATL_OMP_THREADS)
   #include <omp.h>
   typedef struct
   {
      void *vp;      /* ptr to extra info */
      int rank;      /* my rank */
      int P;         /* # of processors in this call */
      int paff_set;  /* have I set my process affinity yet? */
   } ATL_thread_t;
#else
/*
 * Note that we only use paff_set when ATL_PAFF_SELF is defined, but
 * we define it all the time, since not everyone includes atlas_taffinity.h,
 * which is where this define comes from
 */
   #include <pthread.h>
   typedef struct
   {
      pthread_t thrH;/* handle of thread */
      void *vp;      /* ptr to extra info */
      int rank;      /* my rank */
      int P;         /* # of processors in this call */
      int paff_set;  /* have I set my process affinity yet? */
   } ATL_thread_t;
#endif

typedef struct ATL_LaunchStruct ATL_LAUNCHSTRUCT_t;
struct ATL_LaunchStruct
{
   ATL_thread_t *rank2thr;              /* index by rank to get thread handle */
   void *opstruct;
   int (*OpStructIsInit)(void*);        /* Query to see if struct is valid */
   void (*DoWork)(ATL_LAUNCHSTRUCT_t*, void*);
   void (*DoComb)(void*, const int, const int);  /* combine func */
   volatile int *chkin;                 /* nthr-len checkin array */
   void **acounts;                      /* var-len array of atomic counters */
   void *vp;                            /* misc. extra info ptr */
};

/*
 * Constants for use in chkin array
 */
#define ATL_CHK_DONE_OP   -2048
#define ATL_CHK_DONE_COMB -2049

/* Sets up ATL_LAUNCHSTRUCT_t var and ATL_thread_t array & starts threads*/
void ATL_thread_launch(void *opstruct, int opstructstride, void *OpStructIsInit,
                       void *DoWork, void *CombineOpStructs);
void ATL_goparallel(const unsigned int P, void *DoWork, void *opstruct, void*);
void ATL_goparallel_prank(const unsigned int P, void *DoWork, void *opstruct,
                          void *DoComb);
/*  Starts a thread running, and sets its affinity to proc if possible */
int ATL_thread_start(ATL_thread_t *thr, int proc, int JOINABLE,
                     void *(*rout)(void*), void*);
int ATL_thread_join(ATL_thread_t*); /* waits on completion of thread */
void ATL_thread_exit(void*);        /* exits currently executing thread */
void *ATL_log2tlaunch(void *vp);    /* min spanning tree launch */
void *ATL_lin0tlaunch(void *vp);    /* 0 linear launches all threads */
void *ATL_dyntlaunch(void *vp);     /* launch done as workpool */
/*
 * Atomic count functions; may be less overhead than mutex on some systems
 */
void *ATL_SetAtomicCount(int cnt);   /* allocates acnt, sets acnt=cnt */
int   ATL_ResetAtomicCount(void *vp, int cnt);  /* reset vp to cnt */
int   ATL_DecAtomicCount(void *vp);  /* returns acnt-- (not --acnt!) */
int   ATL_GetAtomicCount(void *vp);  /* returns acnt */
void  ATL_FreeAtomicCount(void *vp); /* free acount resources */
/*
 * Global count functions, built out of P local counters for good scaling
 */
void *ATL_SetGlobalAtomicCount(int P, int cnt, int percLoc);
void  ATL_ResetGlobalAtomicCount(void *vp, int cnt, int percLoc);
int   ATL_DecGlobalAtomicCount(void *vp, int rank);
int   ATL_GetGlobalAtomicCount(void *vp, int rank);
void  ATL_FreeGlobalAtomicCount(void *vp);
/*
 * If using pthreads, just wrapper around pthread mutex funcs, else written
 * in terms of the AtomicCount funcs with init value set to 1
 */
void *ATL_mutex_init(void);       /* returns mutex pointer */
void ATL_mutex_free(void *vp);    /* frees mutex vp */
void ATL_mutex_lock(void *vp);
void ATL_mutex_unlock(void *vp);
int  ATL_mutex_trylock(void *vp); /* opp pthreads: 0 means lock NOT acquired */
void ATL_thread_yield(void);      /* gives up time slice */


#define MindxT(A_,i_) ((void*)( ((char*)(A_)) + ((size_t)i_) ))
#define ATL_tlaunch ATL_log2tlaunch   /* may want linear launch later */
void ATL_tDistMemTouch(size_t N, void *vp);

#endif   /* end of #ifdef protecting include file from redundant inclusion */

