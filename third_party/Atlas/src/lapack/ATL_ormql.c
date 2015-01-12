
#include "stdio.h"
#include "cblas.h"
#include "atlas_lapack.h"

#ifdef  SREAL
   #define MYOPT LASreal
#endif
#ifdef  DREAL
   #define MYOPT  LADreal
#endif
#ifdef  SCPLX
   #define MYOPT  LAScplx
#endif
#ifdef  DCPLX
   #define MYOPT  LADcplx
#endif

int ATL_ormql
   (const enum CBLAS_SIDE SIDE, const enum CBLAS_TRANSPOSE TRANS,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, TYPE *A, ATL_CINT lda,
    const TYPE * TAU, TYPE *C, ATL_CINT ldc, TYPE *WORK, ATL_CINT LWORK)
/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *      SUBROUTINE ATL_ormql( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
 *                        WORK, LWORK, INFO )
 *
 * ATL_ormql.c :
 * int ATL_ormql(const enum CBLAS_SIDE SIDE SIDE,
 *        const enum CBLAS_TRANSPOSE TRANS, ATL_CINT M, ATL_CINT N,
 *        ATL_CINT K, TYPE * A, ATL_CINT lda,TYPE * TAU, TYPE * C, ATL_CINT ldc,
 *                       TYPE * WORK, ATL_CINT LWORK)
 *
 *      NOTE :   ATL_ormql.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *
 *
 *  Purpose
 *  =======
 *
 *  ATL_ormql overwrites the general real M-by-N matrix C with
 *
 *                  SIDE = 'L'     SIDE = 'R'
 *  TRANS = 'N':      Q * C          C * Q
 *  TRANS = 'T':      Q**T * C       C * Q**T
 *
 *  where Q is,
 *        a real orthogonal matrix defined as the product of k
 *        elementary reflectors
 *
 *        Q = H(k) . . . H(2) H(1)
 *
 *   OR
 *        a complex unitary matrix defined as a product of k
 *        elementary reflectors
 *
 *        Q = H(k) . . . H(2) H(1)
 *
 *  as returned by ATLL_geqrf.c. Q is of order M if SIDE = 'L' and of order N
 *  if SIDE = 'R'.
 *
 *  Arguments
 *  =========
 *
 *  SIDE    (input) CHARACTER*1
 *          = 'L': apply Q or Q**T from the Left;
 *          = 'R': apply Q or Q**T from the Right.
 *
 *  TRANS   (input) CHARACTER*1
 *          = 'N':  No transpose, apply Q;
 *          = 'T':  Transpose, apply Q**T.
 *
 *  M       (input) INTEGER
 *          The number of rows of the matrix C. M >= 0.
 *
 *  N       (input) INTEGER
 *          The number of columns of the matrix C. N >= 0.
 *
 *  K       (input) INTEGER
 *          The number of elementary reflectors whose product defines
 *          the matrix Q.
 *          If SIDE = 'L', M >= K >= 0;
 *          if SIDE = 'R', N >= K >= 0.
 *
 *  A       (input) array, dimension (LDA,K)
 *          The i-th column must contain the vector which defines the
 *          elementary reflector H(i), for i = 1,2,...,k, as returned by
 *          DGEQRF in the first k columns of its array argument A.
 *          A is modified by the routine but restored on exit.
 *
 *  lda     (input) INTEGER
 *          The leading dimension of the array A.
 *          If SIDE = 'L', LDA >= max(1,M);
 *          if SIDE = 'R', LDA >= max(1,N).
 *
 *  TAU     (input)  array, dimension (K)
 *          TAU(i) must contain the scalar factor of the elementary
 *          reflector H(i), as returned by ATL_geqrf.c.
 *
 *  C       (input/output)  array, dimension (LDC,N)
 *          On entry, the M-by-N matrix C.
 *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
 *
 *  ldc     (input) INTEGER
 *          The leading dimension of the array C. LDC >= max(1,M).
 *
 *  WORK    (workspace/output) array, dimension (MAX(1,LWORK))
 *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 *
 *  LWORK   (input) INTEGER
 *          The dimension of the array WORK.
 *          If SIDE = 'L', LWORK >= max(1,N);
 *          if SIDE = 'R', LWORK >= max(1,M).
 *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
 *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
 *          blocksize.
 *
 *          If LWORK = -1, then a workspace query is assumed; the routine
 *          only calculates the optimal size of the WORK array, returns
 *          this value as the first entry of the WORK array, and no error
 *          message related to LWORK is issued by XERBLA.
 *
 *  INFO    (output) INTEGER
 *          = 0:  successful exit
 *          < 0:  if INFO = -i, the i-th argument had an illegal value
 *
 */
{
   ATL_CINT minMN = Mmin(M, N), maxMN = Mmax(M, N);
   ATL_INT n, nb, j, ib, mi, ni, ic, jc ;
   TYPE  *ws_QL2,  *ws_T, *ws_larfb;        /* Workspace for QR2,T, larfb     */
   void *vp=NULL;

   nb = clapack_ilaenv(LAIS_OPT_NB, LAormqr, MYOPT+LARight+LALower, M, N,K,-1);

/*
 * If it is a workspace query, return the size of work required.
 *    wrksz = wrksz of ATL_larfb + ATL_larft + ATL_geqr2
 */
   if (LWORK < 0)
   {
      if(SIDE == CblasLeft)
      {
         *WORK = ( N*nb + nb*nb + maxMN )  ;
      }
      else
      {
         *WORK = ( M*nb + nb*nb + maxMN )  ;
      }
      return(0);
   }
   else if (M < 1 || N < 1)                 /* quick return if no work to do  */
      return(0);
/*
 * If the user gives us too little space, see if we can allocate it ourselves
 */
   else
   {
      if(SIDE == CblasLeft)
      {
         if (LWORK < (N*nb + nb*nb + maxMN))
         {
            vp = malloc(ATL_MulBySize(N*nb + nb*nb + maxMN) + ATL_Cachelen);
            if (!vp)
               return(-7);
            WORK = ATL_AlignPtr(vp);
         }
      }
      else
      {
         if (LWORK < (M*nb + nb*nb + maxMN))
         {
            vp = malloc(ATL_MulBySize(M*nb + nb*nb + maxMN) + ATL_Cachelen);
            if (!vp)
               return(-7);
            WORK = ATL_AlignPtr(vp);
         }
      }                                     /* if CblasRight                  */
   }                                        /* if else                        */

/*
 * Assign workspace areas for ATL_larft, ATL_geql2, ATL_larfb
 */

   ws_T = WORK;                             /* T at begining of work          */
   ws_QL2 = WORK +(nb SHIFT)*nb;            /* After T Work space             */
   ws_larfb = ws_QL2 + (maxMN SHIFT);       /* After workspace for T and QR2  */


   if (SIDE == CblasLeft)
   {
      if (TRANS == CblasNoTrans)
      {
         for (j=0 ; j < K ; j = j + nb)
         {
            ib = Mmin(K-j, nb);
/*
 *          Form the triangular factor of the block reflector
 *          H = H(i+ib-1) . . . H(i+1) H(i)
 */
            ATL_larft(LABackward, LAColumnStore, M-K+j+ib, ib,
                      A+(j SHIFT)*lda , lda, TAU+(j SHIFT),
                                   ws_T, ib);
/*
 *          H or H' is applied to C(1:m-k+i+ib-1,1:n)
 */
            mi = M -K + j +ib ;

            ATL_larfb(SIDE, TRANS, LABackward, LAColumnStore,
                      mi, N, ib, A+(j SHIFT)*lda, lda, ws_T, ib,
                      C, ldc, ws_larfb, N);
         }
      }                                     /* CblasNoTrans                   */
      else
      {
         j = (K/nb)*nb;
         if (j == K)
         {
            j=K -nb;
         }

         for (j; j >= 0; j = j - nb)
         {
            ib = nb;
            if ((j+nb) > K)
            {
               ib = K - j;
            }
/*
 *          Form the triangular factor of the block reflector
 *          H = H(i+ib-1) . . . H(i+1) H(i)
 */
            ATL_larft(LABackward, LAColumnStore, M-K+j+ib, ib,
                      A+(j SHIFT)*(lda), lda, TAU+(j SHIFT), ws_T, ib);
/*
 *          H or H' is applied to C(1:m-k+i+ib-1,1:n)
 */
            mi = M -K + j +ib ;

            ATL_larfb(SIDE, TRANS, LABackward, LAColumnStore,
                      mi, N, ib, A+(j SHIFT)*(lda), lda, ws_T, ib,
                      C, ldc, ws_larfb, N);
         }                                  /* for                            */
      }
   }                                        /* cblasLeft main logic           */
   else
   {
      if ( TRANS == CblasNoTrans )
      {
         j = (K/nb)*nb;
         if (j == K)
         {
            j=K -nb;
         }
         for (j; j >= 0; j = j - nb)
         {

            ib = nb;
            if ((j+nb) > K)
            {
               ib = K - j;
            }
/*
 *          Form the triangular factor of the block reflector
 *          H = H(i+ib-1) . . . H(i+1) H(i)
 */
            ATL_larft(LABackward, LAColumnStore, N-K+j+ib, ib,
                      A+(j SHIFT)*(lda), lda, TAU+(j SHIFT), ws_T, ib);
/*
 *          H or H' is applied to C(1:m-k+i+ib-1,1:n)
 */
            ni = N - K + j +ib;

            ATL_larfb(SIDE, TRANS, LABackward, LAColumnStore,
                      M, ni, ib, A+(j SHIFT)*(lda), lda, ws_T, ib,
                      C, ldc, ws_larfb, M);
         }                                  /* for                            */

      }                                     /* Cblas Tran on Right            */
      else
      {
         for (j = 0 ; j < K; j = j + nb)
         {
            ib = Mmin(K-j, nb);
/*
 *          Form the triangular factor of the block reflector
 *          H = H(i+ib-1) . . . H(i+1) H(i)
 */
            ATL_larft(LABackward, LAColumnStore, N-K+j+ib, ib,
                      A+(j SHIFT)*lda , lda, TAU+(j SHIFT),
                                   ws_T, ib);
/*
 *          H or H' is applied to C(1:m-k+i+ib-1,1:n)
 */
            ni = N - K + j +ib;
            ATL_larfb(SIDE, TRANS, LABackward, LAColumnStore,
                      M, ni, ib, A+(j SHIFT)*lda, lda, ws_T, ib,
                      C, ldc, ws_larfb, M);
         }                                  /* for                            */
      }                                     /* Cblastrans                     */
   }                                        /* CblasRight                     */

   if (vp)
      free(vp);
   return(0);
}                                           /* END ATL_ormql                  */

