/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
 * Originally developed at the University of Tennessee,
 * Innovative Computing Laboratory, Knoxville TN, 37996-1301, USA.
 *
 * ---------------------------------------------------------------------
 *
 * -- Copyright notice and Licensing terms:
 *
 *  Redistribution  and  use in  source and binary forms, with or without
 *  modification, are  permitted provided  that the following  conditions
 *  are met:
 *
 * 1. Redistributions  of  source  code  must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce  the above copyright
 *    notice,  this list of conditions, and the  following disclaimer in
 *    the documentation and/or other materials provided with the distri-
 *    bution.
 * 3. The name of the University,  the ATLAS group,  or the names of its
 *    contributors  may not be used to endorse or promote products deri-
 *    ved from this software without specific written permission.
 *
 * -- Disclaimer:
 *
 * THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
 * RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
 * CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ---------------------------------------------------------------------
 */
#ifndef ATLAS_PTMISC_H
#define ATLAS_PTMISC_H
/*
 * =====================================================================
 * Include Files
 * =====================================================================
 */
#include <math.h>
#include <pthread.h>

#include "atlas_misc.h"
#include "atlas_pthreads.h"
/*
 * =====================================================================
 * #define macro constants
 * =====================================================================
 *
 * ATL_XOVER_MI_DEFAULT is the  smallest  number of  NB-by-NB blocks for
 * which threading is enabled, where  NB  is  the  value returned by the
 * ATLAS function Mjoin( PATL, GetNB ).
 */
#ifdef TREAL
#define   ATL_XOVER_MI_DEFAULT      8     /* number of NB x NB blocks */
#else
#define   ATL_XOVER_MI_DEFAULT      4
#endif

#define   NOSPLIT                   0              /* For convenience */
#define   SPLIT_M                   1
#define   SPLIT_N                   2
#define   SPLIT_K                   3

/*
 * =====================================================================
 * macro functions
 * =====================================================================
 */
#define    Mptm(  a_, i_, siz_ ) ( ( (char*)(a_) + ( (i_) * (siz_) ) ) )
#define    Mvptm( a_, i_, siz_ ) ( (void *)(Mptm( (a_), (i_), (siz_) )))
/*
 * =====================================================================
 * typedef definitions
 * =====================================================================
 *
 * Definition of the Binary (recursive) task tree: Each node of the tree
 * mainly consist a node number,  a reference counter  to enforce depen-
 * dencies, a argument structure and a function to be applied.
 */
typedef           void *                 PT_DATA_T;
typedef           void *                 PT_FUN_VAL_T;
typedef           void *                 PT_FUN_ARG_T;
typedef           PT_FUN_VAL_T           (*PT_FUN_T) ( PT_FUN_ARG_T );

typedef           struct PT_node_T
{
   pthread_t          pid;
   pthread_mutex_t    mutex;
   pthread_cond_t     cond;
   struct PT_node_T   * left;
   struct PT_node_T   * right;
   PT_DATA_T          data;
   PT_FUN_VAL_T       * val;
   PT_FUN_T           fun;
   PT_FUN_ARG_T       arg;
   unsigned int       node;
   unsigned int       count;
} PT_NODE_T;

typedef           PT_NODE_T *            PT_TREE_T;
typedef           void                   (*PT_APPLY_FUN_T)( PT_TREE_T );

enum              DIM_1DSPLIT_E
{
   Atlas1dSplit     = 100,
   Atlas1dNoSplit   = 199
};

enum              DIM_TZSPLIT_E
{
   AtlasTzSplitMrow = 200,
   AtlasTzSplitKrow = 201,
   AtlasTzSplitKcol = 202,
   AtlasTzSplitNcol = 203,
   AtlasTzNoSplit   = 299
};

typedef           enum DIM_1DSPLIT_E     DIM_1DSPLIT_T;
typedef           enum DIM_TZSPLIT_E     DIM_TZSPLIT_T;

/*
 * Type definitions  for some auxiliaries that have been  multi-threaded
 * as well.
 */
typedef struct
{
   size_t                     size;
   PT_FUN_T                   fun;
} PT_MISC_TYPE_T;

typedef struct
{
   const void                 * al, * be;
   const void                 * a;
   void                       * c;
   int                        la, lc, m, n;
} PT_GEADD_ARGS_T;

typedef struct
{
   void                       * a;
   int                        la, m, n;
} PT_GEZERO_ARGS_T;

typedef struct
{
   const void                 * al;
   void                       * a;
   int                        la, m, n;
} PT_GESCAL_ARGS_T;

typedef struct
{
   enum ATLAS_UPLO            up;
   const void                 * al;
   void                       * a;
   int                        k, la, m, n;
} PT_TZSCAL_ARGS_T;


/*
 * =====================================================================
 * Function prototypes
 * =====================================================================
 */
int               ATL_sGetNB           ( void );
int               ATL_dGetNB           ( void );
int               ATL_cGetNB           ( void );
int               ATL_zGetNB           ( void );

DIM_1DSPLIT_T     ATL_1dsplit
(
   const unsigned int,
   const int,
   const int,
   unsigned int *,
   unsigned int *,
   int *,
   int *,
   double *
);

DIM_TZSPLIT_T     ATL_tzsplit
(
   const enum ATLAS_UPLO,
   const unsigned int,
   const int,
   const int,
   const int,
   const int,
   unsigned int *,
   unsigned int *,
   int *,
   int *
);
/*
 * Task tree management
 */
PT_TREE_T         ATL_init_node
(  unsigned int,      PT_TREE_T,         PT_TREE_T,         PT_DATA_T,
   PT_FUN_VAL_T *,    PT_FUN_T,          PT_FUN_ARG_T );

void              ATL_traverse_tree    ( PT_TREE_T );
void              ATL_apply_tree       ( PT_TREE_T,         PT_APPLY_FUN_T );
void              ATL_free_tree        ( PT_TREE_T );
void              ATL_free_node        ( PT_TREE_T );
void              ATL_print_node_id    ( PT_TREE_T );

void              ATL_thread_init_pa   (pthread_attr_t *, int);
void              ATL_thread_init      ( pthread_attr_t * );
void              ATL_thread_free      ( pthread_attr_t * );
void              ATL_wait_tree        ( PT_TREE_T );
void              ATL_signal_tree      ( PT_TREE_T );
void              ATL_thread_tree      ( PT_TREE_T,         pthread_attr_t * );
void              ATL_join_tree        ( PT_TREE_T );

PT_TREE_T         ATL_create_tree
(  unsigned int *,    const int,         const int );
/*
 * Typeless auxiliary functions
 */
PT_TREE_T         ATL_Sgeadd
(  const PT_MISC_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const int,         const void *,      const void *,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_Sgescal
(  const PT_MISC_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const int,         const void *,      void *,
   const int );
PT_TREE_T         ATL_Sgezero
(  const PT_MISC_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const int,         void *,            const int );
PT_TREE_T         ATL_Stzscal
(  const PT_MISC_TYPE_T *,               const unsigned int,
   const unsigned int,                   pthread_attr_t *,  const int,
   const enum ATLAS_UPLO,                const int,         const int,
   const int,         const void *,      void *,            const int );
/*
 * Single precision real auxiliary functions
 */
PT_FUN_ARG_T      ATL_sptgeadd0        ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_sptgescal0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_sptgezero0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_spttzscal0       ( PT_FUN_ARG_T );

PT_TREE_T         ATL_sptgeadd_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      const void *,      const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_sptgescal_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_sptgezero_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         void *,            const int );
PT_TREE_T         ATL_spttrscal_nt
(  const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const int,       const int,
   const void *,      void *,            const int );

void              ATL_sptgeadd
(  const int,       const int,       const float,     const float *,
   const int,       const float,     float *,         const int );
void              ATL_sptgescal
(  const int,       const int,       const float,     float *,
   const int );
void              ATL_sptgezero
(  const int,       const int,       float *,         const int );
void              ATL_spttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const float,     float *,         const int );

/*
 * Double precision real auxiliary functions
 */
PT_FUN_ARG_T      ATL_dptgeadd0        ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_dptgescal0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_dptgezero0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_dpttzscal0       ( PT_FUN_ARG_T );

PT_TREE_T         ATL_dptgeadd_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      const void *,      const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_dptgescal_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_dptgezero_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         void *,            const int );
PT_TREE_T         ATL_dpttrscal_nt
(  const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const int,       const int,
   const void *,      void *,            const int );

void              ATL_dptgeadd
(  const int,       const int,       const double,    const double *,
   const int,       const double,    double *,        const int );
void              ATL_dptgescal
(  const int,       const int,       const double,    double *,
   const int );
void              ATL_dptgezero
(  const int,       const int,       double *,        const int );
void              ATL_dpttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const double,    double *,        const int );
/*
 * Single precision complex auxiliary functions
 */
PT_FUN_ARG_T      ATL_cptgeadd0        ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_cptgescal0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_cptgezero0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_cpthescal0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_cpttzscal0       ( PT_FUN_ARG_T );

PT_TREE_T         ATL_cptgeadd_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      const void *,      const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_cptgescal_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_cptgezero_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         void *,            const int );
PT_TREE_T         ATL_cpttrscal_nt
(  const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const int,       const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_cpthescal_nt
(  const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const int,       const int,
   const void *,      void *,            const int );

void              ATL_cptgeadd
(  const int,       const int,       const float  *,  const float *,
   const int,       const float *,   float *,         const int );
void              ATL_cptgezero
(  const int,       const int,       float *,         const int );
void              ATL_cptgescal
(  const int,       const int,       const float  *,  float  *,
   const int );
void              ATL_cpttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const float  *,  float  *,        const int );
void              ATL_cpthescal
(  const enum ATLAS_UPLO,            const int,       const int,
   const float,     float  *,        const int );
/*
 * Double precision complex auxiliary functions
 */
PT_FUN_ARG_T      ATL_zptgeadd0        ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_zptgescal0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_zptgezero0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_zpthescal0       ( PT_FUN_ARG_T );
PT_FUN_ARG_T      ATL_zpttzscal0       ( PT_FUN_ARG_T );

PT_TREE_T         ATL_zptgeadd_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      const void *,      const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_zptgescal_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         const void *,      void *,            const int );
PT_TREE_T         ATL_zptgezero_nt
(  const unsigned int,                   pthread_attr_t *,  const int,
   const int,         void *,            const int );
PT_TREE_T         ATL_zpttrscal_nt
(  const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const int,       const int,
   const void *,      void *,            const int );
PT_TREE_T         ATL_zpthescal_nt
(  const unsigned int,                   pthread_attr_t *,
   const enum ATLAS_UPLO,                const int,       const int,
   const void *,      void *,            const int );

void              ATL_zptgeadd
(  const int,       const int,       const double *,  const double *,
   const int,       const double *,  double *,        const int );
void              ATL_zptgezero
(  const int,       const int,       double *,        const int );
void              ATL_zptgescal
(  const int,       const int,       const double *,  double *,
   const int );
void              ATL_zpttrscal
(  const enum ATLAS_UPLO,            const int,       const int,
   const double *,  double *,        const int );
void              ATL_zpthescal
(  const enum ATLAS_UPLO,            const int,       const int,
   const double,    double *,        const int );

#endif
/*
 * End of atlas_ptmisc.h
 */
