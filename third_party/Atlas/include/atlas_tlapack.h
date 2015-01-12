#ifndef ATLAS_TLAPACK_H
   #define ATLAS_TLAPACK_H

#include "atlas_threads.h"
#include "atlas_lapack.h"

typedef struct
{
   ATL_INT M;       /* matrix rows to distribute across processors */
   ATL_INT N;       /* matrix columns */
   volatile ATL_INT *maxindx;  /* this array starts wt all values -1 */
   volatile ATL_INT *stage;    /* this ptr starts wt all values -1 */
   void *A;
   ATL_INT lda;
   int *ipiv;
   int rank, p, info;
   void *works;    /* ptr to array of ptrs */
} ATL_TGETF2_M_t;

#endif                  /* end of ifndef ATLAS_TLAPACK_H */
