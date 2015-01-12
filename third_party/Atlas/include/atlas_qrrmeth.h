#ifndef ATLAS_QRRMETH_H
   #define ATLAS_QRRMETH_H
#include "atlas_pca.h"
/*
 * PCA does not work reliably on ARMv7
 */
#ifndef ATL_USEPCA
   #define FORCE_NO_PCA 1
#endif
#if !defined(ATL_USEPTHREADS) || defined(FORCE_NO_PCA)
/*
 * If PCA is not an option, we use unblocked if either diminsion is too small
 * to allow us to reliably register block the L3BLAS, or if we can fit the
 * entire problem into the L1 cache.  All other problems recur.
 */
   #define METHOD(METH_, M_, N_, LDA_) /* 0,1,2,3=Recur,Unblkd,Cp,NoC      */ \
      (METH_)=( (N_ < 8) || (M_ < 8) || (((size_t)M_)*N <= ATL_L1elts) ) ? 1:0;
#else
   #if !defined(CacheEdge) || CacheEdge <=0 || CacheEdge >= 8*1024*1024
      #define MyCacheEdge (ATL_NTHREADS*(size_t)256*1024) /* collective edge */
   #else
      #define MyCacheEdge (ATL_NTHREADS*(size_t)CacheEdge) /* collective edge */
   #endif
/*
 * BIGM is the # of elts we need to force no-copy-PCA even in when we don't
 * fit into the collective cache and N is very small.  In this rule of thumb,
 * we set it to roughly the page size
 */
   #define ATL_PCA_BIGM (ATL_DivBySize(4096))
/*
 * MINM is an estimate of the min local M we can have that will amortize
 * the parallel sync costs.  These numbers will need to be refined.
 */
   #ifdef SREAL
      #define ATL_PCA_MINM 256
   #elif defined(DREAL)
      #define ATL_PCA_MINM 128
   #elif defined(SCPLX)
      #define ATL_PCA_MINM 96
   #else
      #define ATL_PCA_MINM 64
   #endif
/*
 * Note: Expect M_ to be static dimension, N_ to be recursing dimension.
 */
   #define METHOD(METH_, M_, N_, LDA_) /* 0,1,2,3=Recur,unblck,Cp,NoC      */ \
   {                                                                          \
      METH_ = 0;                       /* default to recursion */ \
/* \
 *    If N_ is too small to get advantage from register blocking, stop recurs \
 *    and use NoCopy-PCA for huge M otherwise use unblocked \
 */ \
      if (N_ < 4) \
      { \
         METH_ = ((M_) > (ATL_PCA_BIGM<<Mmin(3,ATL_NTHRPOW2))) ? 3 : 1; \
      } \
/* \
 *    Don't consider PCA or unblocked unless problem fits in collective cache
 */ \
      if (MyCacheEdge >= ATL_MulBySize(M_)*N_) \
      { \
         const int zrows = Mmax(((M_)>>ATL_NTHRPOW2),Mmin(M_, N_));  \
                                            /* # of rows zero must take */ \
         const int rrows = (M_) - zrows;    /* # of rows for non-0 to take */ \
         METH_ = 2;   /* default to Cp-PCA if it fits in cache */ \
/* \
 *       If it fits in the L1, or if the number of local rows is too small
 *       to bear the cost of synchronization, use normal unblocked algorithm
 */ \
         if ( (((size_t)M_)*N_ <= ATL_L1elts) ||  /* Fits in L1, */ \
              (M_) < ATL_PCA_MINM ||           /* Total rows very small, */ \
              ((M_)>>ATL_NTHRPOW2) < 3 ||      /* Too few per core, */ \
              (rrows<<3) < zrows )             /* Rmndr too small % to help */ \
            METH_ = 1;                         /* Use unblocked. */ \
      } \
   } /* END METHOD MACRO */
#endif                          /* end if on threading or not */

#endif /* end multiple inclusion guard */

