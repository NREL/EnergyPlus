#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"

static int ATL_tsyr2kK(ATL_SYR2K_t *syp, ATL_CINT N, ATL_CINT K,
                       const void *A, const void *B, void *C)
/*
 * Attempts to allocate workspace W, then do:
 *   (1) W = alpha*A*B' or alpha*A'B (GEMM)
 *   (2) C <- beta*C + W + W'
 * RETURNS: 0 on success, nonzero if unable to allocate memory
 */
{
   void *v, *W;
   ATL_INT ldw;
   int i;
   size_t sz;
   const int eltsh = syp->eltsh;

/*
 * Make ldw a multiple of 4 that is not a power of 2
 */
   ldw = ((N+3)>>2)<<2;
   if (!(ldw&(ldw-1)))
      ldw += 4;
   sz = (ldw*N)<<eltsh;
   if (sz <= ATL_NTHREADS*ATL_PTMAXMALLOC)
      v = malloc(sz + ATL_Cachelen);
   if (!v)
      return(1);  /* signal we can't get memory */

   W = ATL_AlignPtr(v);
   syp->tvgemm(syp->TA, syp->TB, N, N, K, syp->alpha, A, syp->lda, B, syp->ldb,
               syp->zero, W, ldw);
   syp->tvApAt(syp->Uplo, N, W, ldw, syp->beta, C, syp->ldc);
   free(v);
   return(0);
}

void ATL_tvsyr2k_rec
   (ATL_SYR2K_t *syp, ATL_CINT Nblks, ATL_CINT nr, const void *A,
    const void *B, void *C)
/*
 * Do SYR2K/HER2K, either by mallocing space and calling GEMM, or recurring
 * until C is small enough that space can be allocated.  Gets its parallelism
 * from the calls to parallel GEMM
 */
{
   const int nb = syp->nb, eltsh = syp->eltsh;
   ATL_INT nL, nR, nbL, nbR, rL, rR;
   void *gc, *sc;   /* ptr to C to update with gemm & 2nd syr2k call, resp */
   void *A1, *B1;   /* ptr to 2nd block of a/b resp */
/*
 * Attempt to halt recursion by allocating workspace, and calling GEMM
 */
   if (!ATL_tsyr2kK(syp, Nblks*nb+nr, syp->K, A, B, C))
      return;
   ATL_assert(Nblks>1 || (Nblks==1 && nr));  /* must have something to split */
/*
 * Must recur in order to make problem small enough to allocate C workspace
 */
   nbR = Nblks>>1;
   nbL = Nblks - nbR;
   rL = (nL == nR) ? nr : 0;
   rR = nr - rL;
   nL = nbL*nb + rL;
   nR = nbR*nb + rR;
   sc = MindxT(C, (((size_t)nL*(syp->ldc+1))<<eltsh));
   if (syp->trans == AtlasNoTrans)
   {
      A1 = MindxT(A, ((size_t)nL<<eltsh));
      B1 = MindxT(B, ((size_t)nL<<eltsh));
   }
   else  /* index like transpose */
   {
      A1 = MindxT(A, (((size_t)nL*syp->lda)<<eltsh));
      B1 = MindxT(B, (((size_t)nL*syp->ldb)<<eltsh));
   }

   ATL_tvsyr2k_rec(syp, nbL, rL, A, B, C);
   if (syp->Uplo == AtlasUpper)
   {
      gc = MindxT(C, (((size_t)nL*syp->ldc)<<eltsh));
      syp->tvgemm(syp->TA, syp->TB, nL, nR, syp->K, syp->alpha, A, syp->lda,
                  B1, syp->ldb, syp->beta, gc, syp->ldc);
      syp->tvgemm(syp->TA, syp->TB, nL, nR, syp->K, syp->alpha2, B, syp->ldb,
                  A1, syp->lda, syp->one, gc, syp->ldc);
   }
   else
   {
      gc = MindxT(C, ((size_t)nL<<eltsh));
      syp->tvgemm(syp->TA, syp->TB, nR, nL, syp->K, syp->alpha, A1, syp->lda,
                  B, syp->ldb, syp->beta, gc, syp->ldc);
      syp->tvgemm(syp->TA, syp->TB, nR, nL, syp->K, syp->alpha2, B1, syp->ldb,
                  A, syp->lda, syp->one, gc, syp->ldc);
   }
   ATL_tvsyr2k_rec(syp, nbR, rR, A1, B1, sc);

}

