#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
int Mjoin(PATL,StructIsInitTRSM)(void *vp)
{
   return(((ATL_TTRSM_t*)vp)->B != NULL);
}


void Mjoin(PATL,DoWorkTRSM)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *thp=vp;
   ATL_TTRSM_t *tp=((ATL_TTRSM_t*)lp->opstruct) + thp->rank;
   Mjoin(PATL,trsm)(tp->side, tp->uplo, tp->TA, tp->diag, tp->M, tp->N,
                    SVVAL((TYPE*)tp->alpha), tp->A, tp->lda, tp->B, tp->ldb);
}

#ifndef ATL_TTRSM_XOVER
   #define ATL_TTRSM_XOVER 4   /* want 4 total blocks before adding proc */
#endif
void Mjoin(PATL,ttrsm)(const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
                       const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
                       ATL_CINT M, ATL_CINT N, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, TYPE *B, ATL_CINT ldb)
{
   ATL_TTRSM_t trsms[ATL_NTHREADS];
   TYPE *b;
   ATL_INT n, nblks, minblks;
   double tblks;
   int nr, p, i, j, extrablks;
   static int nb=0;

   if (M < 1 || N < 1)
      return;
   if (SCALAR_IS_ZERO(alpha))
   {
      Mjoin(PATL,gezero)(M, N, B, ldb);
      return;
   }
/*
 * Distribute RHS over the processors
 */
   if (!nb) nb = Mjoin(PATL,GetNB)();
   if (side == AtlasLeft)
   {
      nblks = N/nb;
      nr = N - nblks*nb;
      tblks = ((double)(M*N)) / ( (double)nb * nb );
      p = (tblks+ATL_TTRSM_XOVER-1)/ATL_TTRSM_XOVER;
      p = Mmin(p, ATL_NTHREADS);
      p = p ? p : 1;

      b = B;
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
         trsms[i].A = A;
         trsms[i].M = M;
         trsms[i].N = n;
         trsms[i].lda = lda;
         trsms[i].ldb = ldb;
         trsms[i].B = b;
         trsms[i].alpha = SADD alpha;
         trsms[i].side = side;
         trsms[i].uplo = uplo;
         trsms[i].TA   = TA;
         trsms[i].diag = diag;
         n *= (ldb << Mjoin(PATL,shift));
         b = MindxT(b, n);
      }
   }
   else /* Side == AtlasRight */
   {
      nblks = M/nb;
      nr = M - nblks*nb;
      tblks = (N/nb)*nblks;
      p = (tblks+ATL_TTRSM_XOVER-1)/ATL_TTRSM_XOVER;
      p = Mmin(p, ATL_NTHREADS);
      p = p ? p : 1;

      b = B;
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
         trsms[i].A = A;
         trsms[i].M = n;
         trsms[i].N = N;
         trsms[i].lda = lda;
         trsms[i].ldb = ldb;
         trsms[i].B = b;
         trsms[i].alpha = SADD alpha;
         trsms[i].side = side;
         trsms[i].uplo = uplo;
         trsms[i].TA   = TA;
         trsms[i].diag = diag;
         n <<= Mjoin(PATL,shift);
         b = MindxT(b, n);
      }
   }
   if (p < 2)
   {
      Mjoin(PATL,trsm)(side, uplo, TA, diag, M, N, alpha, A, lda, B, ldb);
      return;
   }
   for (; i < ATL_NTHREADS; i++)  /* flag rest of struct as uninitialized */
      trsms[i].B = NULL;
   ATL_goparallel(p, Mjoin(PATL,DoWorkTRSM), trsms, NULL);
}
