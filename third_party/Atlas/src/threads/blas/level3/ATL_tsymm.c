#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
int Mjoin(PATL,StructIsInitSYMM)(void *vp)
{
   return(((ATL_TSYMM_t*)vp)->M);
}

void Mjoin(PATL,DoWorkSYMM)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_TSYMM_t *sp=((ATL_TSYMM_t*)lp->opstruct)+tp->rank;
   Mjoin(PATL,symm)(sp->side, sp->uplo, sp->M, sp->N, SVVAL((TYPE*)sp->alpha),
                     sp->A, sp->lda, sp->B, sp->ldb, SVVAL((TYPE*)sp->beta),
                     sp->C, sp->ldc);
}

static void ATL_symmL_rec
   (ATL_TSYMM_t *syp, ATL_CINT Mblks, ATL_CINT mr, ATL_CINT Nblks, ATL_CINT nr,
    const TYPE *A, const TYPE *B, TYPE *C)
{
   const TYPE *A10, *A01, *B10;
   TYPE *C10;
   const int nb = syp->nb;
   ATL_INT nbR, nbL, rR, rL, nL, nR;
   #ifdef TCPLX
      TYPE ONE[2] = {ATL_rone,ATL_rzero};
   #else
      TYPE ONE = ATL_rone;
   #endif

   nbR = Mblks>>1;
   nbL = Mblks - nbR;
/*
 * Stop recursion once we are no longer getting parallelism
 */
   if (Mjoin(PATL,threadMM)(AtlasNoTrans, AtlasNoTrans,
                            nbR*nb, Nblks*nb+nr, nbR*nb) < 2)
   {
      Mjoin(PATL,symm)(syp->side, syp->uplo, Mblks*nb+mr, syp->N,
                       SVVAL((TYPE*)syp->alpha), A, syp->lda, B, syp->ldb,
                       SVVAL((TYPE*)syp->beta), C, syp->ldc);
      return;
   }
   rL = (nbR == nbL) ? mr : 0;
   rR = mr - rL;
   nL = nbL*nb + rL;
   nR = nbR*nb + rR;
   B10 = B + (nL SHIFT);
   C10 = C + (nL SHIFT);
   ATL_symmL_rec(syp, nbL, rL, Nblks, nr, A, B, C);
   ATL_symmL_rec(syp, nbR, rR, Nblks, nr, A+(syp->lda+1)*(nL SHIFT), B10, C10);
   if (syp->uplo == AtlasLower)
   {
      A10 = A + (nL SHIFT);
      Mjoin(PATL,tgemm)(AtlasTrans, AtlasNoTrans, nL, syp->N, nR,
                        SVVAL((TYPE*)syp->alpha), A10, syp->lda, B10, syp->ldb,
                        ONE, C, syp->ldc);
      Mjoin(PATL,tgemm)(AtlasNoTrans, AtlasNoTrans, nR, syp->N, nL,
                        SVVAL((TYPE*)syp->alpha), A10, syp->lda, B, syp->ldb,
                        ONE, C10, syp->ldc);
   }
   else
   {
      A01 = A + nL*(syp->lda SHIFT);
      Mjoin(PATL,tgemm)(AtlasNoTrans, AtlasNoTrans, nL, syp->N, nR,
                        SVVAL((TYPE*)syp->alpha), A01, syp->lda, B10, syp->ldb,
                        ONE, C, syp->ldc);
      Mjoin(PATL,tgemm)(AtlasTrans, AtlasNoTrans, nR, syp->N, nL,
                        SVVAL((TYPE*)syp->alpha), A01, syp->lda, B, syp->ldb,
                        ONE, C10, syp->ldc);
   }
}
static void ATL_symmR_rec
   (ATL_TSYMM_t *syp, ATL_CINT Mblks, ATL_CINT mr, ATL_CINT Nblks, ATL_CINT nr,
    const TYPE *A, const TYPE *B, TYPE *C)
{
   const TYPE *A10, *A01, *B01;
   TYPE *C01;
   const int nb = syp->nb;
   ATL_INT nbR, nbL, rR, rL, nL, nR;
   #ifdef TCPLX
      TYPE ONE[2] = {ATL_rone,ATL_rzero};
   #else
      TYPE ONE = ATL_rone;
   #endif

   nbR = Nblks>>1;
   nbL = Nblks - nbR;
/*
 * Stop recursion once we are no longer getting parallelism
 */
   if (Mjoin(PATL,threadMM)(AtlasNoTrans, AtlasNoTrans,
                            Mblks*nb+mr, nbR*nb, nbR*nb) < 2)
   {
      Mjoin(PATL,symm)(syp->side, syp->uplo, syp->M, Nblks*nb+nr,
                       SVVAL((TYPE*)syp->alpha), A, syp->lda, B, syp->ldb,
                       SVVAL((TYPE*)syp->beta), C, syp->ldc);
      return;
   }
   rL = (nbR == nbL) ? nr : 0;
   rR = nr - rL;
   nL = nbL*nb + rL;
   nR = nbR*nb + rR;
   B01 = B + (nL*syp->ldb SHIFT);
   C01 = C + (nL*syp->ldc SHIFT);
   ATL_symmL_rec(syp, Mblks, mr, nbL, rL, A, B, C);
   ATL_symmL_rec(syp, Mblks, mr, nbR, rR, A+(syp->lda+1)*(nL SHIFT), B01, C01);
   if (syp->uplo == AtlasLower)
   {
      A10 = A + (nL SHIFT);
      Mjoin(PATL,tgemm)(AtlasNoTrans, AtlasNoTrans, syp->M, nL, nR,
                        SVVAL((TYPE*)syp->alpha), B01, syp->ldb, A10, syp->lda,
                        ONE, C, syp->ldc);
      Mjoin(PATL,tgemm)(AtlasNoTrans, AtlasTrans, syp->M, nR, nL,
                        SVVAL((TYPE*)syp->alpha), B, syp->ldb, A10, syp->lda,
                        ONE, C01, syp->ldc);
   }
   else
   {
      A01 = A + (syp->lda SHIFT);
      Mjoin(PATL,tgemm)(AtlasNoTrans, AtlasTrans, syp->M, nL, nR,
                        SVVAL((TYPE*)syp->alpha), B01, syp->ldb, A01, syp->lda,
                        ONE, C, syp->ldc);
      Mjoin(PATL,tgemm)(AtlasNoTrans, AtlasNoTrans, syp->M, nR, nL,
                        SVVAL((TYPE*)syp->alpha), B, syp->ldb, A01, syp->lda,
                        ONE, C01, syp->ldc);
   }
}

static void ATL_tsymm_SYsplit
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc,
    ATL_CINT nb)
/*
 * This routine is specialized for the case where we cannnot split the
 * B matrix, and must instead split the symmetric matrix (A).  It calls
 * a recursive GEMM-based BLAS, that gets its parallel performance from
 * calling threaded GEMM.
 */
{
   ATL_TSYMM_t ss;
   ss.side = Side;
   ss.uplo = Uplo;
   ss.M = M;
   ss.N = N;
   ss.nb = nb;
   ss.alpha = SADD alpha;
   ss.beta  = SADD beta;
   ss.lda = lda;
   ss.ldb = ldb;
   ss.ldc = ldc;
   if (Side == AtlasLeft)
      ATL_symmL_rec(&ss, M/nb, M%nb, N/nb, N%nb, A, B, C);
   else
      ATL_symmR_rec(&ss, M/nb, M%nb, N/nb, N%nb, A, B, C);

}

/*
 * The XOVER is the min # of blocks required to do parallel operation
 */
#ifndef ATL_TSYMM_XOVER
   #ifdef ATL_TGEMM_XOVER
      #define ATL_TSYMM_XOVER ATL_TGEMM_XOVER
   #else
      #define ATL_TSYMM_XOVER 4  /* want 4 blocks for parallel execution */
   #endif
#endif
/*
 * Once you have achieved enough blocks to do computation, minimum number
 * of blocks to give each processor
 */
#ifndef ATL_TSYMM_ADDP
   #ifdef ATL_TGEMM_ADDP
      #define ATL_TSYMM_ADDP  ATL_TGEMM_ADDP
   #else
      #define ATL_TSYMM_ADDP  1  /* want 1 blocks to add proc to workers */
   #endif
#endif
void Mjoin(PATL,tsymm)
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   ATL_INT n, nblks, tblks, nr, minblks, extrablks, p, i, j;
   ATL_thread_t tp[ATL_NTHREADS];
   ATL_TSYMM_t symms[ATL_NTHREADS];
   ATL_LAUNCHSTRUCT_t ls;
   const TYPE *b;
   TYPE *c;
   static int nb=0;

   if (M < 1 || N < 1)
      return;
   if (SCALAR_IS_ZERO(alpha))
   {
      if (!SCALAR_IS_ONE(beta))
         Mjoin(PATL,gescal)(M, N, beta, C, ldc);
      return;
   }
   if (!nb) nb = Mjoin(PATL,GetNB());
   if (Side == AtlasLeft)
   {
      nblks = N / nb;
      nr = N - nblks*nb;
      tblks = ((double)(M*N)) / ( (double)nb * nb );
      p = (nblks+ATL_TSYMM_ADDP-1)/ATL_TSYMM_ADDP;
      if (p < ATL_NTHREADS)  /* N not big enough to give blk to each proc */
      {
/*
 *       If I can't split N, and M is the dominant cost, use recursion to
 *       decompose symmetric matrix; parallelism will come from TGEMM calls
 */
         if (M > (N<<(ATL_NTHRPOW2+2)))
         {
            ATL_tsymm_SYsplit(Side, Uplo, M, N, alpha, A, lda, B, ldb,
                              beta, C, ldc, nb);
            return;
         }
      }
      else
         p = ATL_NTHREADS;
      if (p < 2)
         goto SERIAL;
/*
 *    Distribute N over the processors
 */
      b = B;
      c = C;
      minblks = nblks / p;
      extrablks = nblks - minblks*p;
      for (i=0; i < p; i++)
      {
         if (i < extrablks)
            n = (minblks+1)*nb;
         else if (i == extrablks)
            n = minblks*nb + nr;
         else
            n = minblks*nb;
         symms[i].A = A;
         symms[i].B = b;
         symms[i].alpha = SADD alpha;
         symms[i].beta = SADD beta;
         symms[i].C = c;
         symms[i].M = M;
         symms[i].N = n;
         symms[i].lda = lda;
         symms[i].ldb = ldb;
         symms[i].ldc = ldc;
         symms[i].side = Side;
         symms[i].uplo = Uplo;
         b = MindxT(b, ATL_MulBySize((size_t)ldb)*n);
         c = MindxT(c, ATL_MulBySize((size_t)ldc)*n);
      }
      for (; i < ATL_NTHREADS; i++)  /* flag rest of struct as uninitialized */
         symms[i].M = 0;
   }
   else  /* Side == AtlasRight */
   {
      nblks = M / nb;
      nr = M - nblks*nb;
      tblks = ((double)(M*N)) / ( (double)nb * nb );
      p = (nblks+ATL_TSYMM_ADDP-1)/ATL_TSYMM_ADDP;
      if (p < ATL_NTHREADS)  /* N not big enough to give blk to each proc */
      {
/*
 *       If I can't split M, and N is the dominant cost, use recursion to
 *       decompose symmetric matrix; parallelism will come from TGEMM calls
 */
         if (N > (M<<(ATL_NTHRPOW2+2)))
         {
            ATL_tsymm_SYsplit(Side, Uplo, M, N, alpha, A, lda, B, ldb,
                              beta, C, ldc, nb);
            return;
         }
      }
      else
         p = ATL_NTHREADS;
      if (p < 2)
         goto SERIAL;
/*
 *    Distribute M over the processors
 */
      b = B;
      c = C;
      minblks = nblks / p;
      extrablks = nblks - minblks*p;
      for (i=0; i < p; i++)
      {
         if (i < extrablks)
            n = (minblks+1)*nb;
         else if (i == extrablks)
            n = minblks*nb + nr;
         else
            n = minblks*nb;
         symms[i].A = A;
         symms[i].B = b;
         symms[i].alpha = SADD alpha;
         symms[i].beta = SADD beta;
         symms[i].C = c;
         symms[i].M = n;
         symms[i].N = N;
         symms[i].lda = lda;
         symms[i].ldb = ldb;
         symms[i].ldc = ldc;
         symms[i].side = Side;
         symms[i].uplo = Uplo;
         b = MindxT(b, ATL_MulBySize((size_t)n));
         c = MindxT(c, ATL_MulBySize((size_t)n));
      }
      for (; i < ATL_NTHREADS; i++)  /* flag rest of struct as uninitialized */
         symms[i].M = 0;
   }
   if (p < 2)
   {
SERIAL:
      Mjoin(PATL,symm)(Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
      return;
   }
   ATL_goparallel(p, Mjoin(PATL,DoWorkSYMM), symms, NULL);
}

