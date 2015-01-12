#include "atlas_misc.h"
#include "atlas_level2.h"
#include "atlas_threads.h"
#include "atlas_tcacheedge.h"
/*
 * If CacheEdge not set, default to 256K
 */
#if !defined(CacheEdge)
    #define CacheEdge 262144
/*
 * If we don't trust detected value, default to 256K
 */
#elif CacheEdge > 4194304 || CacheEdge <= 0
    #undef CacheEdge
    #define CacheEdge 262144
#endif

/*
 * Flag is a bitfield of info, let bx be the xth least sig bit:
 *   b0 : 0-Sticky last col of distro; 1-sticky first col
 *   b1 : 0-NoTrans, 1-Trans
 *   b2 : 0-NoConj, 1-Conj
 */
typedef struct
{
   int flg;
   ATL_INT M, N, incX, incY, lda, n, nr, P;
   #ifdef TREAL
      TYPE alpha, beta;
   #else
      const TYPE *alpha, *beta;
   #endif
   const TYPE *A, *X;
   TYPE *Y;
} ATL_TGEMV_t;

void Mjoin(PATL,DOMVNWORK_cols)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_TGEMV_t *pd = lp->opstruct;
   ATL_CINT N = pd->N, lda = pd->lda;
   const int P = tp->P;
   ATL_CINT nr = pd->nr;
   const int vrank = (!nr || (pd->flg & 1)) ? tp->rank : (P + tp->rank+nr-1)%P;
   ATL_INT n = pd->n;
   TYPE *y = pd->Y + (tp->rank)*((pd->M SHIFT)+ATL_Cachelen/sizeof(TYPE));
   const TYPE *a = pd->A + (lda SHIFT)*vrank;
   #ifdef TCPLX
      TYPE one[2] = {ATL_rone, ATL_rzero}, zero[2] = {ATL_rzero, ATL_rzero};
      const enum ATLAS_TRANS TA = (pd->flg & 4) ? AtlasConj : AtlasNoTrans;
   #else
      const enum ATLAS_TRANS TA = AtlasNoTrans;
      #define one ATL_rone
      #define zero ATL_rzero
   #endif

   y = ATL_Align2Ptr(y, a);
   if (vrank < nr)
      n++;

   Mjoin(PATL,gemv)(TA, pd->M, n, one, a, lda*P,
                    pd->X+vrank*((pd->incX)SHIFT), P*pd->incX, zero, y, 1);
}
#ifndef TCPLX
   #undef one
   #undef zero
#endif

void Mjoin(PATL,DOMVTWORK_cols)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_TGEMV_t *pd = lp->opstruct;
   ATL_CINT N = pd->N, lda = pd->lda;
   const int P = tp->P;
   int vrank = (!pd->nr || (pd->flg & 1)) ? tp->rank:(P + tp->rank+pd->nr-1)%P;
   size_t n = pd->n, nr = pd->nr;
   const TYPE *a = pd->A+(lda SHIFT)*vrank;
   TYPE *y = pd->Y + (pd->incY)*((tp->rank) SHIFT);
   #ifdef TCPLX
      const enum ATLAS_TRANS TA = (pd->flg & 4) ? AtlasConjTrans : AtlasTrans;
   #else
      const enum ATLAS_TRANS TA = AtlasTrans;
   #endif

   if (vrank < nr)
      n++;
   Mjoin(PATL,gemv)(TA, pd->M, n, pd->alpha, a, lda*P, pd->X, pd->incX,
                    pd->beta, pd->Y+vrank*((pd->incY)SHIFT), P*pd->incY);
}
void Mjoin(PATL,CombineMVN)
(
   void *vp,          /* void ptr to ATL_GEMV_t struct given to threads */
   const int myrank,  /* my entry in MMNODE_t array */
   const int hisrank  /* array entry to be combined into mine */
)
{
   ATL_TGEMV_t *pd = vp;
   ATL_CINT M = pd->M;
   ATL_INT i;
   const int P = pd->P;
   const int vrank = (!pd->nr || (pd->flg & 1)) ?  myrank :
                     (P + myrank+pd->nr-1)%P;
   const int hvrank = (!pd->nr || (pd->flg & 1)) ?  hisrank :
                      (P + hisrank+pd->nr-1)%P;
   const TYPE *a = pd->A + (pd->lda SHIFT)*vrank;
   const TYPE *ha = pd->A + (pd->lda SHIFT)*hvrank;
   TYPE *y = pd->Y + (myrank)*((M SHIFT)+ATL_Cachelen/sizeof(TYPE));
   TYPE *hy = pd->Y + (hisrank)*((M SHIFT)+ATL_Cachelen/sizeof(TYPE));
   #ifdef TCPLX
      ATL_CINT M2 = M+M;
      const TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      #define one ATL_rone
   #endif

   y = ATL_Align2Ptr(y, a);
   hy = ATL_Align2Ptr(hy, ha);

#ifdef TCPLX
   for (i=0; i < M2; i++)
#else
   for (i=0; i < M; i++)
#endif
      y[i] += hy[i];
}
#ifndef TCPLX
   #undef one
#endif
void Mjoin(PATL,tgemv)
   (const enum ATLAS_TRANS TA, ATL_CINT M, ATL_CINT N, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *X, ATL_CINT incX,
    const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   static size_t ALb=0, ALe=0;
   size_t at = (size_t) A;
   ATL_INT n, P, ldaP;
   ATL_TGEMV_t pd;
/*
 * quick return if possible.
 */
   if (M < 1 || N < 1)
      return;
   if (SCALAR_IS_ZERO(alpha))   /* No contrib from alpha*A*x */
   {
      ATL_CINT NY = (TA == AtlasTrans || TA == AtlasConjTrans) ? N : M;
      if (!SCALAR_IS_ONE(beta))
      {
         if (SCALAR_IS_ZERO(beta))
            Mjoin(PATL,zero)(NY, Y, incY);
         else
            Mjoin(PATL,scal)(NY, beta, Y, incY);
      }
      return;
   }
   pd.flg = (at >= ALb && at <= ALe) ? 1 : 0;
   ALb = (size_t)A;
   ALe = (size_t)(A+(M SHIFT));
   #ifdef TREAL
      pd.flg |= (TA == AtlasTrans || TA == AtlasConjTrans) ? 2 : 0;
   #else
      if (TA != AtlasNoTrans)
      {
         if (TA == AtlasConj)
            pd.flg |= 4;
         else if (TA == AtlasTrans)
            pd.flg |= 2;
         else /* if (TA == AtlasConjTrans) */
            pd.flg |= (2|4);
      }
   #endif
   P = ATL_DivBySize(CacheEdge);
   P = ((size_t)M*N+P-1) / P;   /* add more procs only when cache is full */
   P = (P&1 && P > 1)?P+1 : P;  /* don't use odd P; it hurts alignment */
   P = Mmin(ATL_NTHREADS, P);
/*
 * Make sure we don't overflow 32-bit integer lda
 */
   ldaP = P * lda;
   while ((size_t)ldaP != ((size_t)lda)*P)
   {
      P--;
      ldaP = P * lda;
   }
   if (P > 1)
   {
      pd.M = M; pd.N = N; pd.incX = incX; pd.incY = incY; pd.lda = lda;
      pd.alpha = alpha; pd.beta = beta;
      pd.X = X; pd.Y = Y; pd.A = A;
      pd.P = P;
      n = N / P;
      pd.n = n;
      pd.nr = N - n*P;
      if (pd.flg & 2)   /* Transpose case */
      {
         ATL_goparallel(P, Mjoin(PATL,DOMVTWORK_cols), &pd, NULL);
         return;
      }
/*
 *    For gemvN, everyone needs a private M-length y.  Don't do this unless
 *    we are sure the combine cost is likely dominated by the parallelism
 */
      else if (n > Mmax(P,8))
      {
         int vrank;
         const TYPE *a;
         TYPE *y, *y0;
         #ifdef TCPLX
            TYPE one[2] = {ATL_rone, ATL_rzero};
            TYPE zero[2] = {ATL_rzero, ATL_rzero};
         #endif

         y0 = y = malloc(P*(ATL_Cachelen+ATL_MulBySize(M)));
         ATL_assert(y);
         pd.Y = y;
         pd.incY = 1;
         #ifdef TREAL
            pd.alpha = ATL_rone;
            pd.beta  = ATL_rzero;
         #else
            pd.alpha = one;
            pd.beta  = zero;
         #endif
         ATL_goparallel(P, Mjoin(PATL,DOMVNWORK_cols), &pd,
                        Mjoin(PATL,CombineMVN));
/*
 *       goparallel reduces all node's Ys to node 0's.  Extract his from the
 *       work array, and combine it with input array, applying both alpha
 *       and beta in the process
 */
         vrank = (!pd.nr || (pd.flg & 1)) ? 0 : pd.nr-1;
         a = A + (lda SHIFT)*vrank;
         y = ATL_Align2Ptr(y, a);
         Mjoin(PATL,axpby)(M, alpha, y, 1, beta, Y, incY);
         free(y0);
         return;
      }
   }
/*
 * If we haven't parallelized this thing, just do it serial
 */
   Mjoin(PATL,gemv)(TA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
}
