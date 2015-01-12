#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
/*
 * prototype the typeless tGEMM helper routines
 */
void ATL_DoWorkMM(ATL_LAUNCHSTRUCT_t *lp, void *vp);
int ATL_StructIsInitMM(void *vp);
void ATL_linearize_mmnodes(ATL_TMMNODE_t *ptmms, const int P);
int ATL_thrdecompMM_rMNK
   (ATL_TMMNODE_t *ptmms, const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT Mblks, const int mr, ATL_CINT Nblks, const int nr, ATL_CINT Kblks,
    const int kr, const void *A, ATL_INT lda, const void *B, const ATL_INT ldb,
    const void *C, ATL_CINT ldc, const int P, const int indx, const int COPYC);
int ATL_thrdecompMM_K
   (ATL_TMMNODE_t *ptmms, const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT Mblks, const int mr, ATL_CINT Nblks, const int nr, ATL_CINT Kblks,
    const int kr, const void *A, ATL_INT lda, const void *B, const ATL_INT ldb,
    const void *C, ATL_CINT ldc, const int P, const int indx, const int COPYC);
int ATL_thrdecompMM_N
   (ATL_TMMNODE_t *ptmms, const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT Mblks, const int mr, ATL_CINT Nblks, const int nr, ATL_CINT Kblks,
    const int kr, const void *A, ATL_INT lda, const void *B, const ATL_INT ldb,
    const void *C, ATL_CINT ldc, const int P, const int indx, const int COPYC);
int ATL_thrdecompMM_M
   (ATL_TMMNODE_t *ptmms, const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT Mblks, const int mr, ATL_CINT Nblks, const int nr, ATL_CINT Kblks,
    const int kr, const void *A, ATL_INT lda, const void *B, const ATL_INT ldb,
    const void *C, ATL_CINT ldc, const int P, const int indx, const int COPYC);
void Mjoin(PATL,InitTMMNodes)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, const TYPE *alpha,
    const TYPE *beta, const TYPE *one, const TYPE *zero,
    ATL_thread_t *btp, ATL_TMMNODE_t *ptmms);
void Mjoin(PATL,HandleNewCp)(ATL_TMMNODE_t *me, ATL_TMMNODE_t *him);
int Mjoin(PATL,CombineCw)(ATL_TMMNODE_t *me, ATL_TMMNODE_t *him);
void Mjoin(PATL,CombineStructsMM)(void *vp, const int myrank, const int herank);
int Mjoin(PATL,tgemm_rec)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
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
      return(0);
   if (K < 1 || SCALAR_IS_ZERO(alpha))
   {
      if (!SCALAR_IS_ONE(beta))
         Mjoin(PATL,gescal)(M, N, beta, C, ldc);
      return(0);
   }
   Mjoin(PATL,InitTMMNodes)(TA, TB, SADD alpha, SADD beta, SADD ONE,
                            SADD ZERO, NULL, mms);
   np = ATL_thrdecompMM_rMNK(mms, TA, TB, M/MB, M%MB, N/NB, N%NB, K/KB, K%KB,
                             A, lda, B, ldb, C, ldc, ATL_NTHREADS, 0, 0);
   if (np < ATL_NTHREADS)
      ATL_linearize_mmnodes(mms, np);
#ifdef DEBUG
fprintf(stderr, "np=%d\n\n", np);
#endif
   if (np < 2)
   {
      Mjoin(PATL,gemm)(TA, TB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      return(1);
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
   return(np);
}
