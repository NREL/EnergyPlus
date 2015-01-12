#include "atlas_misc.h"
#include "atlas_tcacheedge.h"
#ifndef CacheEdge
   #include "atlas_cacheedge.h"
   #ifndef CacheEdge
      #define CacheEdge 524288
   #endif
#endif
#include "atlas_tlvl3.h"
#include "atlas_threads.h"
#include "atlas_tsumm.h"


/*
 * Matmul driver, loops over pre-copied A & B, operands are preblocked
 * so a column panel of B, the entire A and needed portion of C fit in the L2,
 * Therefore, use all of A against a single column panel of B,
 * and thus do NMK loop order.
 */

#define genmm Mjoin(PATL,pKBmm)     /* cleans up any combin. of partial blks */
#define PKBmm Mjoin(PATL,pKBmm_b1)  /* cleans up full MB,NB, partial KB */
#define PNBmm Mjoin(PATL,pNBmm_b1)  /* cleans up full MB,KB, partial NB */
#define PMBmm Mjoin(PATL,pMBmm_b1)  /* cleans up full NB,KB, partial MB */

/*
 * This routine is a driver routine that makes all the appropriate calls
 * to the gemm kernel assuming both A & B are already copied to block-major
 * format and that you all you need is one traversal of the K loop.
 */
static void DoMM_K
(
   ATL_CINT mb,         /* # of rows in C <= MB */
   ATL_CINT nb,         /* # of cols in C <= NB */
   ATL_CINT nfKblks,    /* # of full blocks of K */
   ATL_CINT kr,         /* partial remainder block on K */
   const TYPE *A,       /* block-major A in (nfKblks*KB+kr) x NB panel */
   const TYPE *B,       /* block-major B in (nfKblks*KB+kr) x NB panel */
   const SCALAR beta,   /* scale C by this value */
   TYPE *C,             /* ldcxnb array for result */
   ATL_CINT ldc         /* stride betweent elts in a row of C */
)
{
#ifdef TREAL
   ATL_INT k;
   ATL_CINT incA = mb*KB, incB = KB*nb;
   NBMM0 mmk, mmk_kr=genmm, mmk_bX;

   ATL_assert(mb <= MB && nb <= NB);
   if (!nfKblks)  /* only partial block to do! */
   {
      if (mb != MB || nb != NB)
      {
         if (SCALAR_IS_ZERO(beta))
            Mjoin(PATL,gezero)(mb, nb, C, ldc);
         Mjoin(PATL,pKBmm)(mb, nb, kr, ATL_rone, A, kr, B, kr, beta, C, ldc);
      }
      else
      {
         if (SCALAR_IS_ONE(beta))
            Mjoin(PATL,pKBmm_b1)(mb, nb, kr, ATL_rone, A, kr, B, kr, beta,
                                 C, ldc);
         else if (SCALAR_IS_ZERO(beta))
            Mjoin(PATL,pKBmm_b0)(mb, nb, kr, ATL_rone, A, kr, B, kr, beta,
                                 C, ldc);
         else
            Mjoin(PATL,pKBmm_bX)(mb, nb, kr, ATL_rone, A, kr, B, kr, beta,
                                 C, ldc);
      }
      return;
   }
   if (mb != NB && nb != NB)
      mmk_bX = mmk = genmm;
   else if (mb != NB)
   {
      mmk = PMBmm;
      if (SCALAR_IS_ONE(beta))
         mmk_bX = Mjoin(PATL,pMBmm_b1);
      else if (SCALAR_IS_ZERO(beta))
         mmk_bX = Mjoin(PATL,pMBmm_b0);
      else
         mmk_bX = Mjoin(PATL,pMBmm_bX);
   }
   else if (nb != NB)
   {
      mmk = PNBmm;
      if (SCALAR_IS_ONE(beta))
         mmk_bX = Mjoin(PATL,pNBmm_b1);
      else if (SCALAR_IS_ZERO(beta))
         mmk_bX = Mjoin(PATL,pNBmm_b0);
      else
         mmk_bX = Mjoin(PATL,pNBmm_bX);
   }
   else
   {
      mmk = NBmm;
      mmk_kr = PKBmm;
      if (SCALAR_IS_ONE(beta))
         mmk_bX = NBmm_b1;
      else if (SCALAR_IS_ZERO(beta))
         mmk_bX = NBmm_b0;
      else
         mmk_bX = NBmm_bX;
   }
   mmk_bX(mb, nb, KB, ATL_rone, A, KB, B, KB, beta, C, ldc);  /* apply beta */
   A += incA; B += incB;
   for (k=1; k < nfKblks; k++, A += incA, B += incB)  /* full blocks */
      mmk(mb, nb, KB, ATL_rone, A, KB, B, KB, ATL_rone, C, ldc);
   if (kr)  /* partial remainder */
      mmk_kr(mb, nb, kr, ATL_rone, A, kr, B, kr, ATL_rone, C, ldc);
#else  /* complex code */
   ATL_INT k;
   ATL_CINT incA = mb*KB*2, incB = 2*KB*nb;
   const TYPE one[2] = {ATL_rone, ATL_rzero};
   const TYPE *bet = beta;
   NBMM0 mmk, mmk_kr=genmm, mmk_bX;

   ATL_assert(mb <= MB && nb <= NB);
   if (!nfKblks)  /* only partial block to do! */
   {
      if (beta[1] == ATL_rzero && beta[0] != ATL_rzero)
         Mjoin(PATL,pKBmm)(mb, nb, kr, ATL_rone, A, kr, B, kr, *beta, C, ldc);
      else
      {
         if (*beta == ATL_rzero && beta[1] == ATL_rzero)
            Mjoin(PATL,gezero)(mb, nb, C, ldc);
         else
            Mjoin(PATL,gescal)(mb, nb, beta, C, ldc);
         Mjoin(PATL,pKBmm)(mb, nb, kr, ATL_rone, A, kr, B, kr, ATL_rone,C, ldc);
      }
      return;
   }
   if (mb == MB && nb == NB)
   {
      if (beta[1] == ATL_rzero)  /* real scalar */
      {
         if (*beta == ATL_rone)
         {
            NBmm_b1(mb, nb, KB, ATL_rone, A, KB, B, KB, ATL_rone, C, ldc);
         }
         else if (*beta == ATL_rzero)
         {
            NBmm_b0(mb, nb, KB, ATL_rone, A, KB, B, KB, ATL_rzero, C, ldc);
         }
         else
         {
            NBmm_bX(mb, nb, KB, ATL_rone, A, KB, B, KB, *beta, C, ldc);
         }
      }
      else /* must scale for complex beta */
      {
         Mjoin(PATL,gescal)(mb, nb, beta, C, ldc);
         NBmm_b1(mb, nb, KB, ATL_rone, A, KB, B, KB, ATL_rone, C, ldc);
      }
      A += incA; B += incB;
      for (k=1; k < nfKblks; k++, A += incA, B += incB)  /* full blocks */
         NBmm_b1(mb, nb, KB, ATL_rone, A, KB, B, KB, ATL_rone, C, ldc);
      if (kr)  /* partial remainder */
         Mjoin(PATL,pKBmm)(mb, nb, kr, ATL_rone, A, kr, B, kr, ATL_rone,C, ldc);
      return;
   }
   if (mb != MB)
   {
      if (nb != NB)  /* both blocks partial */
      {
         if (*beta == ATL_rzero && beta[1] == ATL_rzero)
         {
            Mjoin(PATL,gezero)(mb, nb, C, ldc);
            bet = one;
         }
         else if (beta[1] != ATL_rzero)
         {
            Mjoin(PATL,gescal)(mb, nb, beta, C, ldc);
            bet = one;
         }
         mmk = mmk_bX =  Mjoin(PATL,pKBmm);
      }
      else  /* only M block is partial */
      {
         if (beta[1] != ATL_rzero)
         {
            Mjoin(PATL,gescal)(mb, nb, beta, C, ldc);
            bet = one;
         }
         mmk = Mjoin(PATL,pMBmm_b1);
         if (*bet == ATL_rone)
            mmk_bX = Mjoin(PATL,pMBmm_b1);
         else if (*bet == ATL_rzero)
            mmk_bX = Mjoin(PATL,pMBmm_b0);
         else
            mmk_bX = Mjoin(PATL,pMBmm_bX);
      }
   }
   else if (nb != MB)  /* only N block is partial */
   {
      if (beta[1] != ATL_rzero)
      {
         Mjoin(PATL,gescal)(mb, nb, beta, C, ldc);
         bet = one;
      }
      mmk = Mjoin(PATL,pNBmm_b1);
      if (*bet == ATL_rone)
         mmk_bX = Mjoin(PATL,pNBmm_b1);
      else if (*bet == ATL_rzero)
         mmk_bX = Mjoin(PATL,pNBmm_b0);
      else
         mmk_bX = Mjoin(PATL,pNBmm_bX);
   }
   mmk_bX(mb, nb, KB, ATL_rone, A, KB, B, KB, *bet, C, ldc);  /* apply beta */
   A += incA; B += incB;
   for (k=1; k < nfKblks; k++, A += incA, B += incB)  /* full blocks */
      mmk(mb, nb, KB, ATL_rone, A, KB, B, KB, ATL_rone, C, ldc);
   if (kr)  /* partial remainder */
      Mjoin(PATL,pKBmm)(mb, nb, kr, ATL_rone, A, kr, B, kr, ATL_rone, C, ldc);
#endif
}


/*
 * Takes a MxN block and expands it to an ldaxM block wt zero padding in-place
 */
#ifdef TREAL
static void ExpandBlock
(
   ATL_CINT M, /* the number of rows that should be expanded to lda */
   ATL_CINT N, /* number of columns in block */
   TYPE *A,    /* in: MxN block, out: ldaxN blk, wt zero-padding in lda-M gap */
   ATL_INT lda /* desired stride between columns */
)
{
   TYPE *a, *c;
   ATL_CINT gap = lda - M;
   ATL_INT j;

   if (gap < 1)   /* already done if lda == M */
      return;

//fprintf(stderr, "ExpandBlock, M=%d, N=%d, A=%p, lda=%d\n", M, N, A, lda);
   a = A + N*M - 1;
   c = A + N*lda - 1;
   for (j=N; j; j--)
   {
      TYPE *stop = c - gap;
      do
         *c-- = ATL_rzero;
      while (c != stop);
      stop =  c - M;
      do
         *c-- = *a--;
      while (c != stop);
   }
}
#endif

void Mjoin(PATL,DoWork_rkK)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
/*
 * This routine has everyone cooperate to copy row-panels of A, and then
 * loops over atomic counters on N & M to perform the rank-K update
 */
{
   ATL_thread_t *tp=vp;
   ATL_TGEMM_RKK_t *pd=lp->opstruct;
   ATL_CINT iam = tp->rank;
   volatile int *chkin = pd->chkin;
   TYPE *Bw=pd->Bws[iam], *Aw=pd->Aw;
   void *aMcnt=pd->aMcnt, *aNcnt=pd->aNcnt, **aMcnts = pd->aMcnts;
   ATL_CINT K = pd->K;
   ATL_CINT BNOTRANS = (pd->TB == AtlasNoTrans);
   ATL_CINT ANOTRANS = (pd->TA == AtlasNoTrans);
   const TYPE *A=pd->A, *B=pd->B;
   TYPE *C = pd->C;
   #ifdef TREAL
      TYPE alpha = pd->alpha;
      TYPE beta  = pd->beta;
   #else
      const SCALAR alpha = pd->alpha;
      const SCALAR beta  = pd->beta;
   #endif
   ATL_CINT nMb=pd->nMb, mr=pd->mr, nNb=pd->nNb, nr=pd->nr;
   ATL_CINT nKb=pd->nKb, kr=pd->kr, kr8=pd->kr8, krpad = kr8-kr;
   size_t incA = (nKb*KB + kr8)*(MB SHIFT), ldc = pd->ldc;
   size_t lda=pd->lda, amul = (ANOTRANS) ? (1 SHIFT) : (lda SHIFT);
   size_t ldb=pd->ldb, P=ATL_NTHREADS;
   ATL_CINT tnMblks = (mr) ? nMb+1 : nMb, tnNblks = (nr) ? nNb+1 : nNb;
   ATL_CINT kr8f = (kr8 >= NB) ? 0 : kr8;
   ATL_CINT nfKblks = (kr8 >= NB) ? nKb+1 : nKb;
   ATL_CINT bmul = (BNOTRANS) ? (ldb SHIFT) : (1 SHIFT);
   ATL_INT iblk, jblk;
/*   #define PRINTTOTALS */
   #ifdef PRINTTOTALS
      int myblks = 0, hisblks=0;
      static int myarr[ATL_NTHREADS], hisarr[ATL_NTHREADS];
   #endif
   MAT2BLK A2blk;
   #ifdef TREAL
      MAT2BLK B2blk = (BNOTRANS) ? Mjoin(PATL,col2blk_a1) :
                                   Mjoin(PATL,row2blkT_a1);
   #else
      MAT2BLK B2blk = (BNOTRANS) ? Mjoin(PATL,col2blk_a1) :
                                  (pd->TB == AtlasTrans) ?
                                   Mjoin(PATL,row2blkT_a1):
                                   Mjoin(PATL,row2blkC_a1);
   #endif
   int k;
   #ifdef TCPLX
      const TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define one ATL_rone
   #endif
#ifdef TREAL
   if (ANOTRANS)
      A2blk = (SCALAR_IS_ONE(alpha)) ?
              Mjoin(PATL,row2blkT_a1) : Mjoin(PATL,row2blkT_aX);
   else
      A2blk = (SCALAR_IS_ONE(alpha)) ?
              Mjoin(PATL,col2blk_a1) : Mjoin(PATL,col2blk_aX);
#else
   if (ANOTRANS)
   {
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone)
            A2blk = Mjoin(PATL,row2blkT_a1);
         else
            A2blk = Mjoin(PATL,row2blkT_aXi0);
      }
      else
         A2blk = Mjoin(PATL,row2blkT_aX);
   }
   else if (pd->TA == AtlasConjTrans)
   {
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone)
            A2blk = Mjoin(PATL,col2blkConj_a1);
         else
            A2blk = Mjoin(PATL,col2blkConj_aXi0);
      }
      else
         A2blk = Mjoin(PATL,col2blkConj_aX);
   }
   else  /* TA == AtlasTrans */
   {
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone)
            A2blk = Mjoin(PATL,col2blk_a1);
         else
            A2blk = Mjoin(PATL,col2blk_aXi0);
      }
      else
         A2blk = Mjoin(PATL,col2blk_aX);
   }
#endif
/*
 * Use the AtomicCounter aMcnt to copy the nMb full row panels of A,
 * and possibly one partial mr-wide row panel.  Since K is not long, copying
 * them a rowpanel at a time shouldn't kill us on the TLB.
 */
   while(iblk = ATL_DecGlobalAtomicCount(aMcnt, iam))
   {
      int mb;
      size_t i, ia;

      mb = (iblk-- == tnMblks && mr) ? mr : MB;
      i = iblk*NB;
      i *= amul;
      A2blk(K, mb, A+i, lda, Aw+iblk*incA, alpha);
      #ifdef TREAL
         if (kr8 != kr)
            ExpandBlock(kr, mb, Aw+iblk*incA+nKb*KB*mb, kr8);
      #endif
   }
/*
 * Tell everyone I have finished copying A, and then loop until everyone
 * else signals they've copied their pieces
 */
   if (iam == 0)
   {
      for (k=1; k < P; k++)
         while(!chkin[k])
            ATL_POLL;
      chkin[0] = 1;
   }
   else
   {
      chkin[iam] = 1;
      while (!chkin[0]);
   }
/*
 * Perform the rank-K update with a fully-copied A by looping over column-panels
 * of C in reverse order
 */
   ATL_mutex_lock(pd->Mlocks[iam]);
   while (jblk = ATL_DecGlobalAtomicCount(aNcnt, iam))
   {
      ATL_INT nb, pL;
      size_t j;
      void *aCrow;
      TYPE *c;

      nb = (jblk != tnNblks || !nr) ? NB : nr;
      pd->Js[iam] = j = (jblk-1)*NB;
      c = C + j*(ldc SHIFT);
/*
 *    Copy the specified column-panel of B to my private workspace
 */
      B2blk(K, nb, B+j*bmul, ldb, Bw, one);
/*
 *    If kr is nonzero and not a multiple of 8, pad K with K8-K zeros
 *    This allows us to cause less K-cleanup kernels to be loaded, as
 *    well as ensuring we keep things aligned for vectorized cleanup kernels
 *    This will tend to depress perf when kr is small, and improve it when
 *    kr is large and not a multiple of the vector length
 */
      #ifdef TREAL
      if (krpad)
         ExpandBlock(kr, nb, Bw+nKb*KB*nb, kr8);
      #endif
/*
 *    Given that I'm working on col-panel j, determine the percentage of its
 *    blocks that I reserve for myself based on how many columns are left.
 *    If there are plenty of columns left, do all local counters which will
 *    reduce the counter cost by something like a factor of 10.  If we are
 *    getting close to running out of col-panels, reserve less and less of
 *    the problem for my exclusive use.
 */
      if (jblk >= P+P)
         pL =  100;
      else if (jblk <= 2)
         pL = 0;
      else
         pL = (jblk > P) ? 50 : 100/P;
      aCrow = aMcnts[iam];
      ATL_ResetGlobalAtomicCount(aCrow, tnMblks, pL);
      ATL_mutex_unlock(pd->Mlocks[iam]);
      while (iblk = ATL_DecGlobalAtomicCount(aCrow, 0))
      {
         const int mb = (!mr || iblk != 1) ? MB : mr;
         const size_t i = (tnMblks-iblk)*(NB SHIFT);
         #ifdef PRINTTOTALS
            myblks++;
         #endif

         iblk = tnMblks - iblk;
         DoMM_K(mb, nb, nfKblks, kr8f, Aw+iblk*incA, Bw, beta, c+i, ldc);
      }
      ATL_mutex_lock(pd->Mlocks[iam]);
   }
   ATL_mutex_unlock(pd->Mlocks[iam]);
   chkin[iam] = -3;  /* let everyone know I've finished my columns */
/*
 * When no more col-panels of C are available, it is time to see if I can
 * help other workers finish their columns; As long as someone hasn't
 * signaled his completion (negative # in chkin), continue trying to steal.
 */
   do
   {
/*
 *    If anyone is still working, continue looking to steal his work
 */
      for (k=0; k < P && chkin[k] <= 0; k++);
      if (k == P)
         break;    /* everyone done, quit */
      for (; k < P; k++)
      {
         const int rk = k;
         void *aCrow = aMcnts[rk];

         Bw = pd->Bws[rk];
         ATL_mutex_lock(pd->Mlocks[rk]);
         if (ATL_GetGlobalAtomicCount(aCrow, 1))
         {
            TYPE *c = C + pd->Js[rk]*(((size_t)ldc)SHIFT);
            int nb;

            nb = pd->N - pd->Js[rk];
            nb = Mmin(NB, nb);
            while (iblk = ATL_DecGlobalAtomicCount(aCrow, 1))
            {
               const int mb = (iblk != 1 || !mr) ? MB : mr;
               const size_t i = (tnMblks-iblk)*(NB SHIFT);

               #ifdef PRINTTOTALS
                  hisblks++;
               #endif
               iblk = tnMblks - iblk;
               DoMM_K(mb, nb, nfKblks, kr8f, Aw+(iblk*incA), Bw, beta,
                      c+i, ldc);
            }
         }
         ATL_mutex_unlock(pd->Mlocks[rk]);
      }
   }
   while(1);
/*
 * Master process is waiting on thread 0, so 0 stays here until all nodes
 * complete their operations
 */
   chkin[iam] = -2;
   #ifdef PRINTTOTALS
      myarr[iam] = myblks;
      hisarr[iam] = hisblks;
   #endif
   if (pd->Sync0 && !iam)
   {
      #ifdef PRINTTOTALS
         int lblks, rblks;
      #endif
      for (k=1; k < P; k++)
         while(chkin[k] != -2)
            ATL_POLL;
      #ifdef PRINTTOTALS
          printf(" myblks : %4d", myarr[0]);
          lblks = myarr[0];
          for (k=1; k < P; k++)
          {
             lblks += myarr[k];
             printf(",%4d", myarr[k]);
          }
          printf(" = %d\n", lblks);
          printf("hisblks : %4d", hisarr[0]);
          rblks = hisarr[0];
          for (k=1; k < P; k++)
          {
             rblks += hisarr[k];
             printf(",%4d", hisarr[k]);
          }
          printf(" = %d\n", rblks);
          printf("Total = %d, expected=%d\n", lblks+rblks,
                 ((pd->M+NB-1)/NB)*((pd->N+NB-1)/NB));
      #endif
   }
}
int Mjoin(PATL,tgemm_rkK)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, ATL_CINT M,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * Does a rank-K update on dynamically scheduled column panels of C
 */
{
   ATL_TGEMM_RKK_t pd;   /* problem definition */
   size_t sz;
   volatile int *chkin;
   TYPE **Bws, *Aw;
   ATL_CINT nKb = ATL_DivByNB(K), kr = K - ATL_MulByNB(nKb);
   ATL_CINT K8 = ATL_MulByNB(nKb) + (((kr+7)>>3)<<3);
   ATL_INT nlblks, nrblks;
   int i, nDb, dr;
   void **acnts;

   sz = ATL_MulBySize(K8)*(M + NB*ATL_NTHREADS) + ATL_Cachelen +
        ATL_NTHREADS*(sizeof(TYPE*)+2*sizeof(int)+ATL_Cachelen+2*sizeof(void*));
   if (sz > ATL_NTHREADS*ATL_PTMAXMALLOC)
      return(1);
   Bws = malloc(sz);
   if (!Bws)
      return(2);
   chkin = (volatile int*) (Bws + ATL_NTHREADS);
   pd.Sync0 = 1;
   pd.Js = (int*) (chkin+ATL_NTHREADS);
   acnts = (void**) (pd.Js+ATL_NTHREADS);
   pd.Mlocks = acnts + ATL_NTHREADS;
   Aw = (TYPE*)(pd.Mlocks+ATL_NTHREADS);
   Aw = ATL_AlignPtr(Aw);
   Bws[0] = Aw + K8*(M SHIFT);
   Bws[0] = ATL_AlignPtr(Bws[0]);
   pd.Js[0] = 0;
   chkin[0] = 0;
   for (i=1; i < ATL_NTHREADS; i++)
   {
      Bws[i] = Bws[i-1] + K8*(NB SHIFT);
      Bws[i] = ATL_AlignPtr(Bws[i]);
      chkin[i] = 0;
      pd.Js[i] = 0;
   }
   pd.chkin = chkin;
   pd.Aw = Aw;
   pd.Bws = Bws;
   pd.nMb = nDb = ATL_DivByNB(M);
   pd.mr = dr = M - ATL_MulByNB(nDb);
   nDb = (dr) ? nDb+1 : nDb;
   pd.aMcnt = ATL_SetGlobalAtomicCount(ATL_NTHREADS, nDb, 0);
   for (i=0; i < ATL_NTHREADS; i++)
   {
      pd.Mlocks[i] = ATL_mutex_init();
      acnts[i] = ATL_SetGlobalAtomicCount(1, 0, 0);
   }
   pd.aMcnts = acnts;
   pd.nNb = nDb = ATL_DivByNB(N);
   pd.nr = dr = N - ATL_MulByNB(nDb);
   pd.aNcnt = ATL_SetGlobalAtomicCount(ATL_NTHREADS, dr ? nDb+1 : nDb, 0);
   pd.nKb = nKb; pd.kr = kr;
   #ifdef TREAL
      pd.kr8 = ((kr+7)>>3)<<3;
      if (pd.kr8 > KB)
         pd.kr8 = KB;
   #else
      pd.kr8 = kr;
   #endif
   pd.A = A; pd.B = B; pd.C = C;
   pd.lda = lda; pd.ldb = ldb; pd.ldc = ldc;
   pd.M = M; pd.N = N; pd.K = K;
   pd.TA = TA; pd.TB = TB;
   pd.alpha = alpha; pd.beta = beta;

   ATL_goparallel(ATL_NTHREADS, Mjoin(PATL,DoWork_rkK), &pd, NULL);

   ATL_FreeGlobalAtomicCount(pd.aMcnt);
   ATL_FreeGlobalAtomicCount(pd.aNcnt);
   for (i=0; i < ATL_NTHREADS; i++)
   {
      ATL_FreeGlobalAtomicCount(acnts[i]);
      ATL_mutex_free(pd.Mlocks[i]);
   }
   free(Bws);
   return(0);
}
