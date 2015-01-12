#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
/*
 * prototype the typeless tGEMM helper routines
 */
void ATL_DoWorkMM(ATL_LAUNCHSTRUCT_t *lp, void *vp);
int ATL_StructIsInitMM(void *vp);
int ATL_thrdecompMM
   (ATL_TMMNODE_t *ptmms, const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const void *A, ATL_INT lda,
    const void *B, const ATL_INT ldb, const void *C, ATL_CINT ldc, const int P,
    int *DivideK);

#ifdef TCPLX
void Mjoin(PATL,tsvgemmCC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasConjTrans, AtlasConjTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmCC)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
#endif  /* end ifdef TCPLX */
#ifdef TCPLX
void Mjoin(PATL,tsvgemmCN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasConjTrans, AtlasNoTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmCN)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
#endif  /* end ifdef TCPLX */
#ifdef TCPLX
void Mjoin(PATL,tsvgemmCT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasConjTrans, AtlasTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmCT)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
#endif  /* end ifdef TCPLX */
#ifdef TCPLX
void Mjoin(PATL,tsvgemmNC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasNoTrans, AtlasConjTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmNC)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
#endif  /* end ifdef TCPLX */
void Mjoin(PATL,tsvgemmNN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasNoTrans, AtlasNoTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmNN)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
void Mjoin(PATL,tsvgemmNT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasNoTrans, AtlasTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmNT)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
#ifdef TCPLX
void Mjoin(PATL,tsvgemmTC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasTrans, AtlasConjTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmTC)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
#endif  /* end ifdef TCPLX */
void Mjoin(PATL,tsvgemmTN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasTrans, AtlasNoTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmTN)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}
void Mjoin(PATL,tsvgemmTT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc)
{
#ifdef FindingCE
void Mjoin(PATL,FindCE_mm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                           const int M, const int N, const int K,
                           const SCALAR alpha, const TYPE *A, const int lda,
                           const TYPE *B, const int ldb, const SCALAR beta,
                           TYPE *C, const int ldc);
   Mjoin(PATL,FindCE_mm)(AtlasTrans, AtlasTrans, M, N, K,
                         SVVAL((TYPE*)alpha), A, lda, B, ldb,
                         SVVAL((TYPE*)beta), C, ldc);
#else
   Mjoin(PATL,tgemmTT)(M, N, K, SVVAL((TYPE*)alpha), A, lda, B, ldb,
                       SVVAL((TYPE*)beta), C, ldc);
#endif
}

#ifdef ATL_SERIAL_COMBINE
static ATL_combnode_t *ATL_NewCombnode
   (ATL_INT M, ATL_INT N, TYPE *W, ATL_INT ldw, TYPE *D, ATL_INT ldd,
    ATL_combnode_t *next)
{
   ATL_combnode_t *np;
   np = malloc(sizeof(ATL_combnode_t));
   ATL_assert(np);
   np->M = M;
   np->N = N;
   np->W = W;
   np->ldw = ldw;
   np->D = D;
   np->ldd = ldd;
   np->next = next;
   return(np);
}
#endif

void Mjoin(PATL,InitTMMNodes)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, const TYPE *alpha,
    const TYPE *beta, const TYPE *one, const TYPE *zero,
    ATL_thread_t *btp, ATL_TMMNODE_t *ptmms)
{
   int i;
   void (*gemmK)(ATL_CINT, ATL_CINT, ATL_CINT, const void*, const void *,
                 ATL_CINT, const void*, ATL_CINT, const void*, void*, ATL_CINT);

   if (TA == AtlasNoTrans)
   {
#ifdef TCPLX
      if (TB == AtlasConjTrans)
         gemmK = Mjoin(PATL,tsvgemmNC);
      else
#endif
      gemmK = (TB == AtlasNoTrans)?Mjoin(PATL,tsvgemmNN):Mjoin(PATL,tsvgemmNT);
   }
#ifdef TCPLX
   else if (TA == AtlasConjTrans)
   {
      if (TB == AtlasNoTrans)
         gemmK = Mjoin(PATL,tsvgemmCN);
      else if (TB == AtlasConjTrans)
         gemmK = Mjoin(PATL,tsvgemmCC);
      else
         gemmK = Mjoin(PATL,tsvgemmCT);
   }
#endif
   else
   {
#ifdef TCPLX
      if (TB == AtlasConjTrans)
         gemmK = Mjoin(PATL,tsvgemmTC);
      else
#endif
      gemmK = (TB == AtlasNoTrans)?Mjoin(PATL,tsvgemmTN):Mjoin(PATL,tsvgemmTT);
   }
   for (i=0; i < ATL_NTHREADS; i++)
   {
      ptmms[i].mb = MB;
      ptmms[i].nb = NB;
      ptmms[i].kb = KB;
      ptmms[i].gemmK = gemmK;
      ptmms[i].eltsz = ATL_sizeof;
      ptmms[i].eltsh = Mjoin(PATL,shift);
      ptmms[i].K = 0;
      ptmms[i].nCp = ptmms[i].nCw = 0;
      ptmms[i].ownC = 0;
      ptmms[i].rank = i;
      ptmms[i].alpha = (void*) alpha;
      ptmms[i].beta  = (void*) beta;
      ptmms[i].one = (void*) one;
      ptmms[i].zero  = (void*) zero;
      ptmms[i].Cinfp[0] = ptmms+i;
   }
}

int Mjoin(PATL,CombineCw)(ATL_TMMNODE_t *me, ATL_TMMNODE_t *him)
/*
 * This routine combines the data in him->Cw into my->Cw (or my->C), if poss.
 * If his workspace is bigger than mine, I combine instead into his workspace,
 * and then set my pointer to his workspace.  The buffer that has been subsumed
 * is freed after the combine.
 * NOTE: This routine assumes him is *not* an owner of C (i.e. he wrote to
 *       workspace, not to the original C)!
 * RETURNS: 0 if we are able to do the combine, non-zero if buffers are
 *          cannot be combined.
 */
{
   ATL_TMMNODE_t *myCp;
   TYPE *w;
   size_t meB, meE, himB, himE, I, J;   /* begin,end of C range */
   #ifdef TREAL
      const TYPE ONE = 1.0;
   #else
      const TYPE ONE[2] = {1.0, 0.0};
   #endif
   const int eltsh = me->eltsh;

   ATL_assert(!him->ownC);
/*
 * Find starting/ending points of our C partitions
 */
   himB = (size_t)him->C;
   himE = himB + ((him->N*(him->ldc) + him->M)<<eltsh);
   meB  = (size_t) me->C;
   meE  = meB + ((me->N*(me->ldc) + me->M)<<eltsh);
/*
 * If I own my piece of the original C, then I can combine any C that is
 * a proper subset of mine;
 */
   if (me->ownC)
   {
      ATL_assert(!him->ownC);  /* should never be true in this routine */
/*
 *    If his wrkspc is not a subset of mine, I can't combine it into the
 *    piece of C originally owned by me
 */
      if (himB < meB || himE > meE)
         return(1);       /* can't combine non subset of my C */
      else if (him->Cw)  /* his malloc succeeded */
      {
         Mjoin(PATL,geadd)(him->M, him->N, ONE, ATL_AlignPtr(him->Cw),
                           him->ldcw, ONE, (TYPE*)him->C, him->ldc);
         free(him->Cw);
      }
      else if (him->nCw)  /* must do GEMM since he couldn't malloc */
         him->gemmK(him->M, him->N, him->K, him->alpha, him->A, him->lda,
                    him->B, him->ldb, SADD ONE, him->C, him->ldc);
      return(0);        /* successful combine */
   }
/*
 * *************************************************************************
 * Otherwise, I don't own C, so must combine work into my or his buffer when
 * possible, and return failure when not
 * *************************************************************************
 */
/*
 * If my workspace is a superset of his, use my workspace as the target buffer
 * if I was able to allocate it
 */
   if (meB <= himB && meE >= himE && me->Cw)
   {
/*
 *    Determine where our overlap is
 */
      I = (himB - meB)>>eltsh;                    /* gap in elements */
      J = I / him->ldc;                           /* column coord */
      I -= J*him->ldc;                            /* row coord */
      if (I+him->M >= me->M || J+him->N >= me->N) /* no intersec after all! */
         return(1);                               /* so cannot combine */
      w = ATL_AlignPtr(me->Cw);
      w += J*me->ldcw + I;
      if (him->Cw)  /* if he succeeded in malloc, combine his op with mine */
      {
         Mjoin(PATL,geadd)(him->M, him->N, ONE, ATL_AlignPtr(him->Cw),
                           him->ldcw, ONE, w, me->ldcw);
         free(him->Cw);
      }
      else          /* must do GEMM since he didn't */
         him->gemmK(him->M, him->N, him->K, him->alpha, him->A, him->lda,
                    him->B, him->ldb, SADD ONE, w, me->ldcw);
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
      I = (meB - himB)>>eltsh;                    /* gap in elements */
      J = I / me->ldc;                            /* col coordinate */
      I -= J*me->ldc;                             /* row coordinate */
      if (I+me->M >= him->M || J+me->N >= him->N) /* no intersec after all! */
         return(1);                               /* so cannot combine */
      w = ATL_AlignPtr(him->Cw);
      w += J*him->ldcw + I;
      if (me->Cw)  /* if I succeeded in malloc, combine my op with his */
      {
         Mjoin(PATL,geadd)(me->M, me->N, ONE, ATL_AlignPtr(me->Cw),
                           me->ldcw, ONE, w, him->ldcw);
         free(me->Cw);
      }
      else          /* must do my GEMM into his workspace since I couldn't */
         him->gemmK(me->M, me->N, me->K, me->alpha, me->A, me->lda,
                    me->B, me->ldb, SADD ONE, w, him->ldcw);
      me->C = him->C;
      me->Cw = him->Cw;
      me->ldcw = him->ldcw;
      me->M = him->M;
      me->N = him->N;
      return(0);        /* successful combine */
   }
   return(1);           /* unsuccessful combine */
}

void Mjoin(PATL,HandleNewCp)(ATL_TMMNODE_t *me, ATL_TMMNODE_t *him)
/*
 * Handles joining a Cp to my list of C partitions
 */
{
  size_t himB, himE, meB, meE, B, E, I, J;
  ATL_TMMNODE_t *tp;
  ATL_INT ldc;
  int i, j;
  const int eltsh = me->eltsh;
/*
 * Find the extent of his C partition
 */
   himB = (size_t)him->C;
   himE = himB + ((him->N*(him->ldc) + him->M)<<eltsh);
   ldc = him->ldc;
/*
 * First, see if this partition can be joined to one I already own
 */
   for (i=0; i < me->nCp; i++)
   {
      tp = me->Cinfp[ATL_NTHREADS-1-i];
      if (tp)
      {
         meB  = (size_t) tp->C;
         meE  = meB + ((tp->N*(tp->ldc) + tp->M)<<eltsh);
         if (meB <= himB)  /* my partition has the base pointer */
         {
            I = (himB - meB)>>eltsh;    /* gap between our C's in elements */
            J = I / ldc;                /* column coord from meB */
            I -= J*ldc;                 /* row coord from meB */
/*
 *          If we have same row (col) coord and he starts at col (row) that
 *          I stop at, join column (row) panel
 */
            if (!I && J == tp->N)
               tp->N += him->N;
            else if (!J && I == tp->M)
               tp->M += him->M;
            else
               continue;                /* if unjoinable, go next candidate */
            break;   /* partitions joined, quit */
         }
         else              /* his partition has the base pointer */
         {
            I = (meB - himB)>>eltsh;    /* gap between our C's in elements */
            J = I / ldc;                /* column coord from himB */
            I -= J*ldc;                 /* row coord from himB */
/*
 *          If we have same row (col) coord and I start at col (row) that
 *          he stops at, join column (row) space
 */
            if (!I && J == him->N)
               tp->N += him->N;
            else if (!J && I == tp->M)
               tp->M += him->M;
            else
               continue;                /* if unjoinable, go next candidate */
            tp->C = him->C;
            break;   /* partitions joined, quit */
         }
      }
   }
/*
 * If I can't join his partition to any of mine, add his to list
 */
   if (i == me->nCp)
   {
      (me->nCp)++;
      me->Cinfp[ATL_NTHREADS-(me->nCp)] = him;
      tp = him;
   }
/*
 * Either new partition is in tp, or an expanded partition is.  In either
 * case, see if any of my workspaces can be combined into this new
 * (or newly expanded) area I own.
 */
   if (i < me->nCp)
   {
      for (i=0; i < me->nCw; i++)
      {
         if (!Mjoin(PATL,CombineCw)(tp, me->Cinfp[i]))
         {
            for (j=i+1; j < me->nCw; j++)
               me->Cinfp[j-1] = me->Cinfp[j];
            (me->nCw)--;
         }
      }
   }
}

void Mjoin(PATL,CombineStructsMM)
(
   void *vp,          /* void ptr to P MMNODE_t structs give tasks to threads */
   const int myrank,  /* my entry in MMNODE_t array */
   const int hisrank  /* array entry to be combined into mine */
)
{
   ATL_TMMNODE_t *mme = ((ATL_TMMNODE_t*)vp)+myrank;
   ATL_TMMNODE_t *mhim = ((ATL_TMMNODE_t*)vp)+hisrank;
   int i, j;
   #ifdef ATL_SERIAL_COMBINE  /* do nothing if combining serially */
      return;
   #endif

/*
 * First, for all of the partitions of the original C that he owns, either
 * join them with mine, or add them to my list of owned partitions
 */
   for (i=0; i < mhim->nCp; i++)
      Mjoin(PATL,HandleNewCp)(mme, mhim->Cinfp[ATL_NTHREADS-1-i]);
/*
 * For all of his workspaces, find out where to combine them into
 */
   for (i=0; i < mhim->nCw; i++)
   {
/*
 *    Look through my partitions of original C for combine partner
 */
      for (j=0; j < mme->nCp; j++)
         if (!Mjoin(PATL,CombineCw)(mme->Cinfp[ATL_NTHREADS-1-j],
                                    mhim->Cinfp[i]))
            break;
/*
 *    If I can't combine his data directly into C, see if it can be
 *    combined with any of my workspaces
 */
      if (j == mme->nCp)
      {
         for (j=0; j < mme->nCw; j++)
            if (!Mjoin(PATL,CombineCw)(mme->Cinfp[j], mhim->Cinfp[i]))
               break;
/*
 *       If I can't combine his data into any partition or workspace, add his
 *       node to my list of workspaces to be combined later
 */
         if (j == mme->nCw)
         {
            mme->Cinfp[j] = mhim->Cinfp[i];
            mme->nCw = j + 1;
         }
      }
   }
}

void Mjoin(PATL,tgemm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                       ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
                       const SCALAR beta, TYPE *C, ATL_CINT ldc)
{
   #ifdef ATL_SERIAL_COMBINE
      ATL_combnode_t *combb=NULL, *combp;
   #endif
   ATL_TMMNODE_t mms[ATL_NTHREADS];
   int i, np, DividedK=0;
   #ifdef TREAL
      TYPE ONE=ATL_rone, ZERO=ATL_rzero;
   #else
      TYPE ONE[2] = {ATL_rone, ATL_rzero}, ZERO[2] = {ATL_rzero, ATL_rzero};
   #endif

   if (M < 1 || N < 1)
      return;
   if (K < 1 || SCALAR_IS_ZERO(alpha))
   {
      if (!SCALAR_IS_ONE(beta))
         Mjoin(PATL,gescal)(M, N, beta, C, ldc);
      return;
   }
/*
 * See if we are in a case where we've implemented a dynamically scheduled
 * code.  Performance of the dynamically scheduled operations varies:
 * on Intel, its typically faster, and on AMD its slower than
 * statically scheduled *when run on an unloaded machine*.  The difference is
 * that if there is load on one or more processors, dynamically scheduled
 * code is almost twice as fast.  On unloaded machines, the performance
 * difference is due mostly to the differing partitioning, not the overhead
 * of dynamic scheduling, which always seems to pay for itself.
 * On unloaded AMD machines, the asymptotic loss is roughly 1-2%.
 * Dynamic scheduling seems to always be a performance loss for MAC OSX
 */
   #ifndef ATL_OS_OSX
      #ifdef FindingCE
         ATL_assert(!Mjoin(PATL,tgemm_bigMN_Kp)(TA, TB, M, N, K, alpha, A, lda,
                                                B, ldb, beta, C, ldc));
         return;
      #endif
/*
 *    Rank-K update has special case
 */
      i = Mmax(ATL_NTHREADS,4)*NB;
      if (K <= 4*NB && M >= 2*MB && N >= 2*NB && Mmax(M,N) >= i)
      {
         if (!Mjoin(PATL,tgemm_rkK)(TA, TB, M, N, K, alpha, A, lda, B, ldb,
                                    beta, C, ldc))
            return;
      }
/*
 *    Very large matrices loops over rank-Kp updates, where Kp is set by
 *    CacheEdge.  If any dim is very small, use one of the other cases.
 */
      i = Mmin(M,N);
      i = Mmin(i, K);
      if (i > Mmax(8,2*ATL_NTHREADS)*NB)
      {
         if (!Mjoin(PATL,tgemm_bigMN_Kp)(TA, TB, M, N, K, alpha, A, lda,
                                         B, ldb, beta, C, ldc))
            return;
      }
   #endif
/*
 * See how many processors are optimal for this problem
 */
   np = Mjoin(PATL,threadMM)(TA, TB, M, N, K);
   if (np > 1)
   {
      Mjoin(PATL,InitTMMNodes)(TA, TB, SADD alpha, SADD beta, SADD ONE,
                               SADD ZERO, NULL, mms);
      np = ATL_thrdecompMM(mms, TA, TB, M, N, K, A, lda, B, ldb, C, ldc,
                           np, &DividedK);
   }
#ifdef DEBUG
fprintf(stderr, "np=%d\n\n", np);
#endif
   if (np < 2)
   {
      Mjoin(PATL,gemm)(TA, TB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      return;
   }
/*
 * If we are debugging, set up serial combine queue
 */
   #ifdef ATL_SERIAL_COMBINE
      for (i=0; i < ATL_NTHREADS; i++)
      {
         if (mms[i].K)   /* if this struct being used */
         {
            if (!mms[i].ownC)   /* I need a workspace for C */
            {
               mms[i].Cw = calloc(mms[i].ldcw * mms[i].N, ATL_sizeof);
               ATL_assert(mms[i].Cw);
               combb = ATL_NewCombnode(mms[i].M, mms[i].N, mms[i].Cw,
                                       mms[i].ldcw, mms[i].C, mms[i].ldc,
                                       combb);
            }
         }
      }
   #endif

   ATL_goparallel(np, ATL_DoWorkMM, mms,
                  DividedK ? Mjoin(PATL,CombineStructsMM) : NULL);
/*
 * If we are debugging, serially combine all workspaces back to original C
 */
   #ifdef ATL_SERIAL_COMBINE
      while(combb)
      {
         Mjoin(PATL,geadd)(combb->M, combb->N, ONE, combb->W, combb->ldw,
                           ONE, combb->D, combb->ldd);
         free(combb->W);
         combp = combb;
         combb = combb->next;
         free(combp);
      }
   #endif
}

void Mjoin(PATL,tvgemm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                        ATL_CINT M, ATL_CINT N, ATL_CINT K, const void *alpha,
                        const void *A, ATL_CINT lda, const void *B,ATL_CINT ldb,
                        const void *beta, void *C, ATL_CINT ldc)
/*
 * This void wrapper for tgemm is used in some typeless structures
 */
{
   Mjoin(PATL,tgemm)(TA, TB, M, N, K, SVVAL((const TYPE*)alpha), A, lda,
                     B, ldb, SVVAL((const TYPE*)beta), C, ldc);
}
