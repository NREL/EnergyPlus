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

void Mjoin(PATL,DoWork_bigMN_Kp)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   const int iam = tp->rank, P = tp->P;
   ATL_TGEMM_RKK_t *pd = lp->opstruct;
   volatile int *chkin = pd->chkin+P, *hischk = pd->chkin;
   ATL_CINT K = pd->K, Kp = pd->kr, nkb = Kp / NB;
   ATL_CINT nnb = (pd->nr) ? pd->nNb+1 : pd->nNb;
   ATL_CINT nmb = (pd->mr) ? pd->nMb+1 : pd->nMb;
   const size_t incA = (pd->TA == AtlasNoTrans) ? (pd->lda SHIFT) : (1 SHIFT);
   const size_t incB = (pd->TB == AtlasNoTrans) ? (1 SHIFT) : (pd->ldb SHIFT);
   ATL_INT k, kb;
   int i, n;
   const TYPE *A = pd->A, *B = pd->B;
   void Mjoin(PATL,DoWork_rkK)(ATL_LAUNCHSTRUCT_t *lp, void *vp);
   #ifdef TCPLX
      const TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define one ATL_rone
   #endif

   for (k=0; k < K; k += Kp)
   {
      kb = K - k;
      kb = Mmin(kb, Kp);
/*
 *    To avoid race conditions, thread 0 sets everything up, and rest of
 *    threads await his signal to begin
 */
      if (iam == 0)
      {
         n = chkin[0] + 1;
         for (i=1; i < P; i++)
            while(chkin[i] < n)
               ATL_POLL;
         for (i=0; i < P; i++)
            hischk[i] = 0;
         pd->beta = (k) ? one : pd->beta;
         pd->A = A + k*incA;
         pd->B = B + k*incB;
         ATL_ResetGlobalAtomicCount(pd->aNcnt, nnb, 0);
         ATL_ResetGlobalAtomicCount(pd->aMcnt, nmb, 0);
         pd->K = kb;
         if (kb == Kp)
         {
            pd->kr = pd->kr8 = 0;
            pd->nKb = nkb;
         }
         else
         {
            pd->nKb = kb / KB;
            pd->kr = kb - pd->nKb * KB;
            #ifdef TREAL
               pd->kr = kb - pd->nKb * KB;
               pd->kr8 = ((pd->kr+7)>>3)<<3;
               if (pd->kr8 > KB)
                  pd->kr8 = KB;
            #else
               pd->kr8 = pd->kr = kb - pd->nKb * KB;
            #endif
         }
         chkin[0] = n;
      }
      else
      {
         n = ++chkin[iam];
         while (chkin[0] < n)
            ATL_POLL;
      }
      Mjoin(PATL,DoWork_rkK)(lp, vp);  /* do rank-K update */
   }
/*
 * Master process waiting on thread 0, who therefore must block until everyone
 * signals completion
 */
   n = ++chkin[iam];
   if (!iam)
      for (i=1; i < P; i++)
         while (chkin[i] < n)
            ATL_POLL;
}
#ifdef TREAL
   #undef one
#endif


int Mjoin(PATL,tgemm_bigMN_Kp)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, ATL_CINT M,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc)
/*
 * This routine handles the asymtotic case where A & B are too large to be
 * copied at once, and we loop over CacheEdge-sized partitions of K.
 * Implements very large gemm as a series of synchronized rank-Kp updates.
 */
{
   ATL_TGEMM_RKK_t pd;   /* problem definition */
   size_t sz, Kp;
   void *vp;
   int i;
   #ifdef FindingCE
      extern int FoundCE, CompCE;
      #define MY_CE FoundCE
   #else
      #define MY_CE CacheEdge
   #endif

   #ifdef TREAL
      Kp = (ATL_DivBySize(MY_CE) - MB*NB)/(MB + 2*NB);
   #else
      Kp = (ATL_DivBySize(MY_CE) - 2*MB*NB)/(2*MB + 4*NB);
   #endif
   Kp = (Kp/KB)*KB;
   #ifdef FindingCE
      if (MY_CE == 0)
         Kp = K;
      if (Kp < KB)
         Kp = KB;
      if (CompCE)
      {
         CompCE = Kp;
         return;
      }
   #else
      if (Kp < KB)
         return(1);  /* not going to be efficient */
   #endif

   pd.kr = Kp;
   pd.TA = TA;
   pd.TB = TB;
   pd.M = M;
   pd.N = N;
   pd.K = K;
   pd.alpha = alpha;
   pd.A = A;
   pd.lda = lda;
   pd.B = B;
   pd.ldb = ldb;
   pd.beta = beta;
   pd.C = C;
   pd.ldc = ldc;
   pd.nMb = M / MB;
   pd.mr = M - pd.nMb*MB;
   pd.nNb = N / NB;
   pd.nr = N - pd.nNb*NB;

   sz = ATL_MulBySize(Kp)*M + ATL_Cachelen;  /* sizeof A workspace */
   sz += ATL_NTHREADS*(Kp*ATL_MulBySize(NB)+ATL_Cachelen);  /* B workspaces */
   sz += ATL_NTHREADS*3*sizeof(int);  /* chkin1/2 & Js arrays */
   sz += ATL_NTHREADS*sizeof(TYPE*);  /* Bws array */
   sz += ATL_NTHREADS*2*sizeof(void*); /* aMcnts & Mlocks arrays */
   if (sz > ATL_NTHREADS*ATL_PTMAXMALLOC+ATL_MaxMalloc)
      return(2);  /* can't allocate space */
   pd.Bws = malloc(sz);
   if (!pd.Bws)
      return(3);
   pd.aMcnts = (void**)(pd.Bws+ATL_NTHREADS);
   pd.Mlocks = (pd.aMcnts+ATL_NTHREADS);
   pd.chkin = (volatile int*)(pd.Mlocks+ATL_NTHREADS);
   pd.Js = (int*)(pd.chkin + 2*ATL_NTHREADS);
   pd.Aw = (TYPE *) (pd.Js + ATL_NTHREADS);
   pd.Aw = ATL_AlignPtr(pd.Aw);
   pd.Sync0 = 0;
   vp = pd.Aw + M * (Kp SHIFT);
   pd.Bws[0] = ATL_AlignPtr(vp);
   for (i=1; i < ATL_NTHREADS; i++)
   {
      vp = pd.Bws[i-1] + (NB SHIFT)*Kp;
      pd.Bws[i] = ATL_AlignPtr(vp);
   }
   for (i=0; i < ATL_NTHREADS; i++)
   {
      pd.Mlocks[i] = ATL_mutex_init();
      pd.aMcnts[i] = ATL_SetGlobalAtomicCount(1, 0, 0);
      pd.chkin[i] = pd.chkin[ATL_NTHREADS+i] = 0;
      pd.Js[i] = 0;
   }
   pd.aMcnt = ATL_SetGlobalAtomicCount(ATL_NTHREADS, 1, 0);
   pd.aNcnt = ATL_SetGlobalAtomicCount(ATL_NTHREADS, 1, 0);

   ATL_goparallel(ATL_NTHREADS, Mjoin(PATL,DoWork_bigMN_Kp), &pd, NULL);
/*
 * Free allocated resources
 */
   ATL_FreeGlobalAtomicCount(pd.aMcnt);
   ATL_FreeGlobalAtomicCount(pd.aNcnt);
   for (i=0; i < ATL_NTHREADS; i++)
   {
      ATL_mutex_free(pd.Mlocks[i]);
      ATL_FreeGlobalAtomicCount(pd.aMcnts[i]);
   }
   free(pd.Bws);
   return(0);
}
