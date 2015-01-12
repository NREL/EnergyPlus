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
#include Mstr(Mjoin(Mjoin(atlas_,PRE),r1_L2.h))

typedef struct
{
   ATL_INT M, N, incX, incY, lda, flg;
   ATL_INT nrblks, ncblks, nblks;
   #ifdef TREAL
      TYPE alpha;
   #else
      const TYPE *alpha;
   #endif
   const TYPE *X, *Y;
   TYPE *A;
} ATL_TGER_t;

#ifdef TREAL
   #define MY_GER ger
#elif defined(Conj_)
   #define MY_GER gerc
#else
   #define MY_GER geru
#endif
#define MY_DOWORK_cols Mjoin(Mjoin(Mjoin(PATL,DoWork),MY_GER),_cols)
#define MY_TGER Mjoin(Mjoin(PATL,t),MY_GER)
#define MY_GER1 Mjoin(PATL,MY_GER)
/*
 * This routine distributes the columns cyclicly over the processors.
 * If (pd->flg & 1), then we always assign the first column of the array
 * to P0, otherwise we always assign the LAST column to P0.  In both
 * cases, the idea is to make sure that on repetitive calls, the same
 * processor gets the same cols, in order to encourage cache reuse.
 * Most LAPACK calls will keep the end of the last column constant,
 * while a left-looking variant would probably keep the first column constant.
 * One idea would be to save the address of the
 * first and last cols between calls, and if the last col address is the
 * same, use vrank, else use actual rank.
 */
void MY_DOWORK_cols(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_TGER_t *pd = lp->opstruct;
   ATL_CINT N = pd->N, lda = pd->lda;
   const int P = tp->P;
   int vrank;
   size_t n, nr;
   const TYPE *y;

   n = N / P;
   nr = N - n*P;
   vrank = (!nr || (pd->flg & 1)) ? tp->rank : (P + tp->rank+nr-1)%P;
   y = pd->Y + (pd->incY)*(vrank SHIFT);
   if (vrank < nr)
      n++;
   MY_GER1(pd->M, n, pd->alpha, pd->X, pd->incX, pd->Y+(pd->incY)*(vrank SHIFT),
           P*pd->incY, pd->A+lda*(vrank SHIFT), lda*P);
}

void Mjoin(Mjoin(PATL,t),MY_GER)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *X,
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda)
{
   ATL_INT mb, nb, mu, nu, nblks, nrblks, ncblks, ldaP;
   ATL_TGER_t pd;
   int P;
   static TYPE *A0=NULL, *A0e=NULL;

   if (M < 1 || N < 1 || SCALAR_IS_ZERO(alpha))  /* quick return if no-op */
      return;

   pd.M = M; pd.N = N; pd.incX = incX; pd.incY = incY; pd.lda = lda;
   pd.alpha = alpha;
   pd.X = X; pd.Y = Y; pd.A = A;
   pd.flg = (A0 == A || A0e == A+(M SHIFT)) ? 1 : 2;
   A0 = A; A0e = A+(M SHIFT);

   P = ATL_DivBySize(CacheEdge);
   P = ((size_t)M*N+P-1) / P;   /* add more procs only when cache is full */
   P = (P&1 && P > 1)?P+1 : P;  /* don't use odd P, since it hurts alignment */
//   printf("TGER, P=%d\n", P);
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
      ATL_goparallel(P, MY_DOWORK_cols, &pd, NULL);
   else
      MY_GER1(M, N, alpha, X, incX, Y, incY, A, lda);
}
