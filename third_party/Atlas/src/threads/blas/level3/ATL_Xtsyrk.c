#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
#include "math.h"
/*
 * Recursive decompositon on trapazoidal-shaped matrix ($C$ after splitting)
 */
#ifndef ATL_MINL3THRFLOPS
   #ifdef ATL_TGEMM_ADDP
      #define ATL_MINL3THRFLOPS \
         (((2.0*ATL_TGEMM_ADDP)*ATL_TGEMM_ADDP)*ATL_TGEMM_ADDP)
   #else
      #define ATL_MINL3THRFLOPS (((2.0*MB)*NB)*KB)
   #endif
#endif

int ATL_tsyrkdecomp_K
   (ATL_TSYRK_K_t *psyrk,
    void (*syrkK)(const enum ATLAS_UPLO, const enum ATLAS_TRANS, ATL_CINT,
                  ATL_CINT, const void*, const void*, ATL_CINT, const void*,
                  void*, ATL_CINT),
    int np, const int eltsh, const int nb, const void *zero, const void *one,
    const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT Kblks, const int kr,
    const void *alpha, const void *A, ATL_CINT lda,
    const void *beta, void *C, ATL_CINT ldc)
{
   ATL_INT minblks, extrablks, j, k, ldcw;
   int i;

/*
 * Note that this routine is essentially for large K, so we don't consider
 * any K smaller than NB for a processor
 */
   minblks = Kblks / np;
   if (minblks)
      extrablks = Kblks - minblks*np;
   else
   {
      np = Kblks;
      minblks = 1;
      extrablks = 0;
   }
/*
 * Find a good ldcw: multiple of 4 that is not a power of two
 */
   ldcw = ((N+3)>>2)<<2;   /* multiple of 4 */
   if (!(ldcw&(ldcw-1)))
      ldcw += 4;
   if ((ldcw<<eltsh)*N > ATL_PTMAXMALLOC)
      return(0);
   for (i=0; i < np; i++)
   {
      if (i < extrablks)
         k = (minblks + 1)*nb;
      else if (i == extrablks)
         k = minblks*nb + kr;
      else
         k = minblks * nb;
      j = N;
      psyrk[i].alpha = alpha;
      psyrk[i].beta  = beta ;
      psyrk[i].one   = one  ;
      psyrk[i].zero  = zero ;
      psyrk[i].Uplo = Uplo;
      psyrk[i].Trans = Trans;
      psyrk[i].N = N;
      psyrk[i].K = k;
      psyrk[i].A = A;
      psyrk[i].C = C;
      psyrk[i].lda = lda;
      psyrk[i].ldc = ldc;
      psyrk[i].eltsh = eltsh;
      if (!i)
         psyrk[0].nCw = psyrk[0].ldcw = 0;
      else
      {
         psyrk[i].nCw = 1;
         psyrk[i].ldcw = ldcw;
      }
      psyrk[i].Cw = NULL;
      psyrk[i].Cinfp[0] = psyrk + i;
      psyrk[i].tvsyrk = syrkK;
      k = (Trans == AtlasNoTrans) ? lda * k : k;
      k <<= eltsh;
      A = MindxT(A,k);
   }
   for (; i < ATL_NTHREADS; i++)
      psyrk[i].N = 0;
   return(np);
}

void ATL_tsyrk_K(ATL_TSYRK_K_t *syp, int np, ATL_CINT N, ATL_CINT K,
                 const void *A, void *C)
{
   const int nb = syp->nb;
   void ATL_DoWorkSYRK_K(ATL_LAUNCHSTRUCT_t *lp, void *vp);

   if (np < 1 || Mmin(N,K) < 8)
      np = 1;
   else
      np = ATL_tsyrkdecomp_K(syp, syp->tvsyrk, np, syp->eltsh, nb, syp->zero,
                             syp->one, syp->Uplo, syp->Trans, N, K/nb, K%nb,
                             syp->alpha, A, syp->lda, syp->beta, C, syp->ldc);
   if (np < 2)
   {
      syp->tvsyrk(syp->Uplo, syp->Trans, N, K, syp->alpha, A, syp->lda,
                  syp->beta, C, syp->ldc);
      return;
   }
   ATL_goparallel(np, ATL_DoWorkSYRK_K, syp, syp->DoComb);
}

void ATL_tsyrk_K_rec(ATL_TSYRK_K_t *syp, int np, ATL_CINT Nblks, ATL_CINT nr,
                     ATL_CINT K, const void *A0, void *C00)
/*
 * This routine recurs on N until we can allocate the full NxN workspace,
 * at which point it stops the recursion and distributes K for parallel
 * operation
 */
{
   const enum ATLAS_TRANS TA = syp->Trans;
   ATL_CINT lda = syp->lda, ldc = syp->ldc, eltsh = syp->eltsh;
   ATL_CINT nb = syp->nb, N = Nblks*nb+nr;
   ATL_INT sz, nblksL, nblksR, nrL, nrR, nL, nR;
   const void *A1;
   void *C10, *C01, *C11;
/*
 * Stop recursion & call threaded SYRK if we can allocate workspace for all of C
 */
   sz = (N * N) << eltsh;
/*
 * Quit recurring if we can allocate space for C workspace and we can
 * no longer usefully split Nblks, or we can usefully split K
 */
   if (sz <= ATL_PTMAXMALLOC && (nb*ATL_NTHREADS < K || Nblks < ATL_NTHREADS))
   {
      ATL_tsyrk_K(syp, np, Nblks*nb+nr, K, A0, C00);
      return;
   }
   nblksL = (Nblks+1)>>1;
   nblksR = Nblks - nblksL;
   if (nblksL >= nblksR)
   {
      nrL = nr;
      nrR = 0;
   }
   else
   {
      nrL = 0;
      nrR = nr;
   }

   nL = nblksL * nb + nrL;
   nR = nblksR * nb + nrR;
   if (syp->Uplo == AtlasUpper)
   {
      sz = nL<<eltsh;
      C01 = MindxT(C00,sz*ldc);
      A1 = (TA == AtlasNoTrans) ? MindxT(A0,sz) : MindxT(A0,sz*lda);
      C11 = MindxT(C01,sz);
      ATL_tsyrk_K_rec(syp, np, nblksL, nrL, K, A0, C00);
      syp->gemmT(syp->Trans, syp->TB, nL, nR, K, syp->alpha, A0, lda, A1, lda,
                 syp->beta, C01, ldc);
      ATL_tsyrk_K_rec(syp, np, nblksR, nrR, K, A1, C11);
   }
   else /* Lower triangular matrix */
   {
      sz = nL<<eltsh;
      C10 = MindxT(C00,sz);
      A1 = (TA == AtlasNoTrans) ? MindxT(A0,sz) : MindxT(A0,sz*lda);
      sz += (ldc*nL)<<eltsh;
      C11 = MindxT(C00,sz);
      ATL_tsyrk_K_rec(syp, np, nblksL, nrL, K, A0, C00);
      syp->gemmT(syp->Trans, syp->TB, nR, nL, K, syp->alpha, A1, lda, A0, lda,
                 syp->beta, C10, ldc);
      ATL_tsyrk_K_rec(syp, np, nblksR, nrR, K, A1, C11);
   }
}

void ATL_DoWorkSYRK_K(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp=vp;
   ATL_TSYRK_K_t *syp = ((ATL_TSYRK_K_t *)lp->opstruct)+tp->rank;
/*
 * Allocate space if needed, and then do SYRK into it
 */
   if (syp->nCw)
   {
      syp->Cw = malloc((syp->ldcw << syp->eltsh)*syp->N+ATL_Cachelen);
      if (syp->Cw)
         syp->tvsyrk(syp->Uplo, syp->Trans, syp->N, syp->K, syp->alpha, syp->A,
                     syp->lda, syp->zero, ATL_AlignPtr(syp->Cw), syp->ldcw);
   }
   else /* do SYRK directly into original C: no poss of failure */
      syp->tvsyrk(syp->Uplo, syp->Trans, syp->N, syp->K, syp->alpha,
                  syp->A, syp->lda, syp->beta, syp->C, syp->ldc);
}

int ATL_IsInitSYRK_K(void *vp)
{
   return( ((ATL_TSYRK_K_t*)vp)->N );
}

int ATL_tsyrkdecomp_tr1D(int P, ATL_CINT N, ATL_CINT K,
                         ATL_CINT nb, ATL_CINT mu, double minmf, ATL_INT *Ms)
/*
 * Partitions triangular matrix from SYRK into roughly equal flop count
 * regions, with the first such region being strictly triangular, and the
 * rest trapazoidal row-panels.
 * Ms : must be of length P at least, on output contains the correct size
 *      matrix to give to each processor.
 * RETURNS: number of processors used
 */
{
   double Pflops, myflops, tflops, pinv;
   const int incM = (nb >= 60) ? ((24+mu-1)/mu)*mu :
                    ((nb < 16) ? nb : ((16+mu-1)/mu)*mu);
   ATL_INT n, m, j;
   int k, p;

   for (k=0; k < P; k++)
      Ms[k] = 0;
   tflops = (((double)N)*N)*K;
   while (P && (Pflops = tflops/((double)P)) < minmf) P--;
   if (P < 2)
      return(0);

   tflops /= K;
/*
 * For each processor, find m that balances the flop count
 */
   for (n=p=0; p < P; p++)
   {
      Pflops = tflops / (P-p);
      if (tflops*K < minmf)
      {
         if (p < 2)
            return(0);
         Ms[p-1] += N - n;
         return(p);
      }
/*
 *    Finds the largest m that is a multiple of nb that generates <= Pflops
 */
      m = nb;  /* number of rows in row-panel */
      k = 1;   /* number of blocks in m */
      do
      {
         myflops = m;
         myflops *= myflops + n + n;
         if (myflops == Pflops)
            break;
         else if (myflops > Pflops)
         {
            m -= nb;
            k--;
            myflops = m;
            myflops *= myflops + n + n;
            break;
         }
         m += nb;
         k++;
      }
      while (1);
/*
 *    If we are below target flop count, see how to adjust
 */
      if (myflops < Pflops)
      {
         j = (k < 4) ? incM : mu;  /* for small M, don't tolerate cleanup */
         while ((((double)m)*((((double)m)+n)+n)) < Pflops) m += j;
         myflops = (((double)m)*((((double)m)+n)+n));
      }
      j = N - n;
      if (m >= j)
      {
         if (j < incM)
         {
            if (p < 1)
               return(0);
            Ms[p-1] += j;
            return(p);
         }
         Ms[p] = j;
         return(p+1);
      }
      else if (p == P-1)
         m = N - n;
      n += m;
      Ms[p] = m;
      tflops -= myflops;
   }
   return(p);
}
int ATL_IsInitSYRK_M(void *vp)
{
   return( ((ATL_TSYRK_M_t*)vp)->K );
}

int ATL_tsyrkdecomp_M
(
   ATL_TSYRK_M_t *syp,          /* output: parallel decomposition structs */
   const enum ATLAS_UPLO Uplo,
   const enum ATLAS_TRANS TA,
   ATL_CINT N, ATL_CINT K,      /* original problem size */
   const void *alpha,
   const void *A,
   ATL_CINT lda,
   const void *beta,
   void *C,
   ATL_CINT ldc,
   ATL_CINT nb,                 /* MB of GEMM kernel */
   const int mu,                /* reg blking factor along M of MM kernel */
   const int eltsh,
   const enum ATLAS_TRANS TB,   /* Dual of TA (Conj for herk, trans for syrk) */
   double minmf,
   void (*gemmK)(ATL_CINT, ATL_CINT, ATL_CINT, const void*, const void *,
                 ATL_CINT, const void*, ATL_CINT, const void*, void*, ATL_CINT),
   void (*tvsyrk)(const enum ATLAS_UPLO, const enum ATLAS_TRANS, ATL_CINT,
                  ATL_CINT, const void*, const void*, ATL_CINT, const void*,
                  void*, ATL_CINT)
)
{
   ATL_INT Ms[ATL_NTHREADS];
   int k, j, p;
   ATL_CINT incA = lda << eltsh, incC = (ldc+1) << eltsh;
   ATL_INT n, m, JJ;
   const int ISNOTRANS = (TA == AtlasNoTrans);

   p = ATL_tsyrkdecomp_tr1D(ATL_NTHREADS, N, K, nb, mu, minmf, Ms);
   if (p < 2)
      return(0);
   if (Uplo == AtlasLower)
   {
      n = 0;
      for (k=0; k < p; k++)
      {
         m = Ms[k];
         syp[k].gemmK = gemmK;
         syp[k].tvsyrk = tvsyrk;
         syp[k].alpha = alpha;
         syp[k].beta  = beta ;
         syp[k].K = K;
         syp[k].lda = lda;
         syp[k].ldc = ldc;
         syp[k].nb = nb;
         syp[k].eltsh = eltsh;
         syp[k].Uplo = Uplo;
         syp[k].TA = TA;
         syp[k].TB = TB;
         syp[k].M = m;
         syp[k].N = n;
         syp[k].T = MindxT(C,((size_t)n*incC));
         syp[k].C = (n > 0) ? MindxT(C,((size_t)n<<eltsh)) : NULL;
         syp[k].A0 = (ISNOTRANS) ? MindxT(A,((size_t)n<<eltsh))
                                 : MindxT((size_t)A,n*incA);
         syp[k].A = syp[k].A0;
         syp[k].B = A;
         n += m;
      }
   }
   else  /* Uplo == AtlasUpper */
   {
      n = 0;
      for (k=0; k < p; k++)
      {
         m = Ms[k];
         syp[k].gemmK = gemmK;
         syp[k].tvsyrk = tvsyrk;
         syp[k].alpha = alpha;
         syp[k].beta  = beta ;
         syp[k].K = K;
         syp[k].lda = lda;
         syp[k].ldc = ldc;
         syp[k].nb = nb;
         syp[k].eltsh = eltsh;
         syp[k].Uplo = Uplo;
         syp[k].TA = TA;
         syp[k].TB = TB;
         syp[k].M = m;
         syp[k].N = n;
         JJ = N - n - m;
         syp[k].T = MindxT(C,((size_t)JJ*incC));
         syp[k].C = (n > 0) ? MindxT(C,((size_t)JJ*incC+m*(ldc<<eltsh))) : NULL;
         if (ISNOTRANS)
         {
            syp[k].A = syp[k].A0 = MindxT(A,((size_t)JJ<<eltsh));
            syp[k].B = MindxT(syp[k].A0, ((size_t)m<<eltsh));
         }
         else
         {
            syp[k].A = syp[k].A0 = MindxT(A,((size_t)JJ*incA));
            syp[k].B = MindxT(syp[k].A0, ((size_t)m*incA));
         }
         n += m;
      }
   }
   for (k=p; k < ATL_NTHREADS; k++)
      syp[k].K = 0;
   return(p);
}

void ATL_DoWorkSYRK_M(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp=vp;
   ATL_TSYRK_M_t *syp = ((ATL_TSYRK_M_t*)lp->opstruct) + tp->rank;

   syp->tvsyrk(syp->Uplo, syp->TA, syp->M, syp->K, syp->alpha,
               syp->A0, syp->lda, syp->beta, syp->T, syp->ldc);
   if (syp->C)
      syp->gemmK(syp->M, syp->N, syp->K, syp->alpha, syp->A, syp->lda,
                 syp->B, syp->lda, syp->beta, syp->C, syp->ldc);
}

