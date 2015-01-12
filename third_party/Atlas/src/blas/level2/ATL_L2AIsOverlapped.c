#include "atlas_misc.h"
/*
 * This function stores the matrix that was accesses last in the Level 2 BLAS.
 * It returns 1 if the current matrix is a proper subset of the prior matrix,
 * and 0 otherwise.
 * NOTE: may want to amend later to allow 1 for large % overlap
 */
#ifdef ATL_USEPTHREADS
/*
 * Need to write wrappers around mutex stuff so I can initialize
 * arrays on the first (and only first) call with thread safety
 */
#else
   int ATL_L2AIsOverlapped
      (int rank, int sz, size_t M, size_t N, size_t A, size_t lda)
   {
      static size_t Ao=0, Mo, No, ldao;
/*
 *    If original matrix could possibly contain this one; if lda is changing,
 *    this computation won't always work, but this case shouldn't be important
 *    enough to care about.
 */
      M *= sz;
      lda *= sz;
      if (Ao && Ao <= A && Mo >= M && No >= N && lda <= ldao)
      {
         size_t j, i;
         j = (A-Ao)/ldao;
         i = (A-(Ao+j*ldao));
         if (i+M <= Mo && j+N <= No)
            return(1);
      }
      Mo = M;
      No = N;
      ldao = lda;
      Ao = A;
      return(0);
   }
#endif
