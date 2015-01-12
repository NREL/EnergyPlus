#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"

/*
 * Prototype functions in ATL_Xtsyrk
 */
int ATL_IsInitSYRK_M(void *vp);
void ATL_DoWorkSYRK_M(ATL_LAUNCHSTRUCT_t *lp, void *vp);
int ATL_tsyrkdecomp_M
   (ATL_TSYRK_M_t *syp, const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS TA,
    ATL_CINT N, ATL_CINT K, const void *alpha, const void *A, ATL_CINT lda,
    const void *beta, void *C, ATL_CINT ldc, ATL_CINT nb, const int mu,
    const int eltsh, const enum ATLAS_TRANS TB, double minmf,
    void (*gemmK)(ATL_CINT, ATL_CINT, ATL_CINT, const void*, const void *,
                  ATL_CINT,const void*, ATL_CINT, const void*, void*, ATL_CINT),
    void (*tvsyrk)(const enum ATLAS_UPLO, const enum ATLAS_TRANS, ATL_CINT,
                   ATL_CINT, const void*, const void*, ATL_CINT, const void*,
                   void*, ATL_CINT));
int ATL_tsyrkdecomp_K
   (ATL_TSYRK_K_t *psyrk,
    void (*syrkK)(const enum ATLAS_UPLO, const enum ATLAS_TRANS, ATL_CINT,
                  ATL_CINT, const void*, const void*, ATL_CINT, const void*,
                  void*, ATL_CINT),
    const int eltsh, const int nb, const void *zero, const void *one,
    const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT Kblks, const int kr,
    const void *alpha, const void *A, ATL_CINT lda,
    const void *beta, void *C, ATL_CINT ldc);
void ATL_tsyrk_K_rec(ATL_TSYRK_K_t *syp, int np, ATL_CINT Nblks, ATL_CINT nr,
                     ATL_CINT K, const void *A, void *C);
void ATL_DoWorkSYRK_K(ATL_LAUNCHSTRUCT_t *lp, void *vp);
int ATL_IsInitSYRK_K(void *vp);

void Mjoin(PATL,tvsyrk)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const void *alpha, const void *A, ATL_CINT lda,
    const void *beta, void *C, ATL_CINT ldc)
{
   Mjoin(PATL,syrk)(Uplo, Trans, N, K, SVVAL((TYPE*)alpha), A, lda,
                   SVVAL((TYPE*)beta), C, ldc);
}


static int CombineCw(ATL_TSYRK_K_t *me, ATL_TSYRK_K_t *him)
/*
 * This routine combines the data in him->Cw into my->Cw, if possible.
 * If his workspace is bigger than mine, I combine instead into his workspace,
 * and then set my pointer to his workspace.  The buffer that has been subsumed
 * is freed after the combine.
 * RETURNS: 0 if we are able to do the combine, non-zero if buffers are
 *          cannot be combined.
 */
{
   TYPE *w;
   size_t meB, meE, himB, himE, I, J;   /* begin,end of C range */
   #ifdef TREAL
      const TYPE ONE = 1.0;
   #else
      const TYPE ONE[2] = {1.0, 0.0};
   #endif

/*
 * If I'm the master (owner of original C), then I can always do combine
 * into the original C
 */
   if (me->nCw == 0)
   {
      if (him->Cw)  /* his malloc succeeded */
      {
         Mjoin(PATL,tradd)(him->Uplo, him->N, ATL_AlignPtr(him->Cw),
                           him->ldcw, ONE, (TYPE*)him->C, him->ldc);
         free(him->Cw);
      }
      else if (him->nCw)  /* must do GEMM since he couldn't malloc */
         him->tvsyrk(him->Uplo, him->Trans, him->N, him->K, him->alpha,
                     him->A, him->lda, him->one, him->C, him->ldc);
      return(0);        /* successful combine */
   }
/*
 * *************************************************************************
 * Otherwise, I don't own C, so must combine work into my or his buffer when
 * possible, and return failure when not
 * *************************************************************************
 */
   meB  = (size_t) me->C;
   meE  = meB + (((me->N*(me->ldc + 1)))<<(me->eltsh));
   himB = (size_t)him->C;
   himE = himB + (((him->N*(him->ldc + 1)))<<(me->eltsh));
/*
 * If my workspace is a superset of his, use my workspace as the target buffer
 * if I was able to allocate it
 */
   if (meB <= himB && meE >= himE && me->Cw)
   {
/*
 *    Determine where our overlap is
 */
      I = (himB - meB)>>(him->eltsh);           /* gap in elts */
      J = I / him->ldc;                         /* col coord */
      I -= J*him->ldc;                          /* row coord */
      ATL_assert(I == J);
      w = ATL_AlignPtr(me->Cw);
      w += J*me->ldcw + I;
      if (him->Cw)  /* if he succeeded in malloc, combine his op with mine */
      {
         Mjoin(PATL,tradd)(him->Uplo, him->N, ATL_AlignPtr(him->Cw),
                           him->ldcw, ONE, w, him->ldcw);
         free(him->Cw);
      }
      else          /* must do SYRK since he didn't */
         him->tvsyrk(him->Uplo, him->Trans, him->N, him->K, him->alpha,
                     him->A, him->lda, him->one, w, me->ldcw);
      return(0);        /* successful combine */
   }
/*
 * else if his workspace is a superset of mine, use his as target buffer if
 * he was able to allocate it
 */
   else if (himB <= meB && himE >= meE && him->Cw)
   {
/*
 *    Determine where our overlap is
 */
      I = (meB - himB)>>(him->eltsh);           /* gap in elements */
      J = I / him->ldc;                         /* column coord */
      I -= J*him->ldc;                          /* row coord */
      ATL_assert(I == J);
      w = ATL_AlignPtr(him->Cw);
      w += J*him->ldcw + I;
      if (me->Cw)  /* if I succeeded in malloc, combine my op with his */
      {
         Mjoin(PATL,tradd)(me->Uplo, me->N, ATL_AlignPtr(me->Cw),
                           me->ldcw, ONE, w, him->ldcw);
         free(me->Cw);
      }
      else          /* must do my SYRK into his workspace since I couldn't */
         him->tvsyrk(me->Uplo, me->Trans, me->N, me->K, me->alpha,
                     me->A, me->lda, me->one, w, him->ldcw);
      me->C = him->C;
      me->Cw = him->Cw;
      me->ldcw = him->ldcw;
      me->N = him->N;
      me->K = him->K;
      return(0);        /* successful combine */
   }
   return(1);           /* unsuccessful combine */
}

void Mjoin(PATL,CombineStructsSYRK)
   (void *opstruct, const int myrank, const int hisrank)
/*
 * This routine written like GEMM, so that SYRK can have been split
 * with N, even though present code only splits K (so everyone is writing
 * to entire C).  I may want the extra functionality later, so programmed
 * it using GEMM as model.
 * NOTE: this version actually wouldn't work if we split both N & K for
 *       all cases; I later had to redesign the GEMM combine to account
 *       for the fact that you have to sum up the pieces of the original C
 *       you own, instead of always modifying C when you own only  a piece
 *       of it.  This problem only shows up on systems with non-power-of-2
 *       # of processors, where the launch recursive distribution doesn't
 *       match the recursive launch/combine procedure.  Will need to rewrite
 *       based on present GEMM combine if I ever go to true recursive
 *       distribution on both N & K.
 */
{
   #ifdef TREAL
      TYPE ONE = ATL_rone;
   #else
      TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #endif
   ATL_TSYRK_K_t *me = ((ATL_TSYRK_K_t*)opstruct)+myrank;
   ATL_TSYRK_K_t *him = ((ATL_TSYRK_K_t*)opstruct)+hisrank, *himcp, *mycp;
   int i, j;

/*
 * Need to combine only if joining thread has C in workspace
 */
   if (him->nCw)
   {
/*
 *    For all his workspaces, find out where to combine them into
 */
      for (i=0; i < him->nCw; i++)
      {
/*
 *       If I can't combine his data into my primary workspace, see if it
 *       can be combined with any of my other workspaces
 */
         if (CombineCw(me, him->Cinfp[i]))
         {
            for (j=1; j < me->nCw; j++)
               if (!CombineCw(me->Cinfp[j], him->Cinfp[i]))
                  break;
/*
 *          If I can't combine his data into any existing auxiliary space,
 *          add his node to my list of workspaces to be combined later
 */
            if (j == me->nCw)
            {
               me->Cinfp[j] = him->Cinfp[i];
               me->nCw = j + 1;
            }
         }
      }
   }
}

void Mjoin(PATL,tsyrk_K_rec)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc, ATL_CINT nb)
/*
 * This typed wrapper routine sets up type-specific data structures, and
 * calls the appropriate typeless recursive routine in order to recursively
 * cut N until workspace can be allocated, and then the K-dimension will be
 * threaded.  During the recursion, parallel performance is achieved by
 * calling the threaded GEMM.
 */
{
   ATL_CINT Nblks = N/nb, nr = N - nb*Nblks;
   ATL_TSYRK_K_t syp[ATL_NTHREADS];
   #ifdef TCPLX
      TYPE ZERO[2] = {ATL_rzero, ATL_rzero}, ONE[2] = {ATL_rone, ATL_rzero};
   #else
      TYPE ZERO=ATL_rzero, ONE=ATL_rone;
   #endif
   int i;

   syp[0].DoComb = Mjoin(PATL,CombineStructsSYRK);
   syp[0].Uplo = Uplo;
   syp[0].Trans = Trans;
   syp[0].TB = (Trans == AtlasNoTrans) ? AtlasTrans : AtlasNoTrans;
   syp[0].K = K;
   syp[0].alpha = SADD alpha;
   syp[0].beta = SADD beta;
   syp[0].zero = SADD ZERO;
   syp[0].one  = SADD ONE;
   syp[0].lda = lda;
   syp[0].ldc = ldc;
   syp[0].gemmT = Mjoin(PATL,tvgemm);
   syp[0].tvsyrk = Mjoin(PATL,tvsyrk);
   syp[0].eltsh = Mjoin(PATL,shift);
   syp[0].nb = nb;
   ATL_tsyrk_K_rec(syp, Mjoin(PATL,threadMM)(Trans, syp[0].TB, N>>1, N>>1, K),
                   Nblks, nr, K, A, C);
}

static int ATL_tsyrk_M
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS TA, ATL_CINT N,
    ATL_CINT K, const void *alpha, const TYPE *A, ATL_CINT lda,
    const void *beta, TYPE *C, ATL_CINT ldc)
{
   ATL_TSYRK_M_t syp[ATL_NTHREADS];
   int i, p;
   p = ATL_tsyrkdecomp_M(syp, Uplo, TA, N, K, alpha, A, lda, beta, C, ldc,
                         MB, ATL_mmMU, Mjoin(PATL,shift),
                         (TA == AtlasNoTrans) ? AtlasTrans : AtlasNoTrans,
                         ATL_TGEMM_PERTHR_MF, (TA == AtlasNoTrans) ?
                         Mjoin(PATL,tsvgemmNT):Mjoin(PATL,tsvgemmTN),
                         Mjoin(PATL,tvsyrk));
   if (p < 2)
      return(0);
   ATL_goparallel(p, ATL_DoWorkSYRK_M, syp, NULL);
   return(p);
}


void Mjoin(PATL,tsyrk)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   #ifdef TREAL
      const TYPE ONE = ATL_rone, ZERO = ATL_rzero;
   #else
      const TYPE ONE[2]={ATL_rone, ATL_rzero}, ZERO[2]={ATL_rzero, ATL_rzero};
   #endif
   size_t nblksN;
   int i, np, nb;
   void Mjoin(PATL,ptsyrk)
      (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
       ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
       const SCALAR beta, TYPE *C, ATL_CINT ldc);

   if (Mjoin(PATL,threadMM)(Trans,
                            (Trans == AtlasNoTrans) ? AtlasTrans:AtlasNoTrans,
                            N, N>>1, K) < 2)
      goto DOSERIAL;
   if (N < 1)
      return;
   if (SCALAR_IS_ZERO(alpha) || K < 1)
   {
      if (!SCALAR_IS_ONE(beta))
         Mjoin(PATL,trscal)(Uplo, N, N, beta, C, ldc);
      return;
   }

   nb = MB;
   if (K > (N<<ATL_NTHRPOW2) && (((size_t)N)*N*sizeof(TYPE) <= ATL_PTMAXMALLOC))
   {
      Mjoin(PATL,tsyrk_K_rec)(Uplo, Trans, N, K, alpha, A, lda, beta,
                               C, ldc, nb);
      return;
   }
   np = ATL_tsyrk_M(Uplo, Trans, N, K, SADD alpha, A, lda,
                     SADD beta, C, ldc);
   if (np < 2)
   {
DOSERIAL:
      Mjoin(PATL,syrk)(Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
      return;
   }
}
