

/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 Siju Samuel
 *
 * Code contributers : Siju Samuel, Anthony M. Castaldo, R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_ptalias_lapack.h"
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

int ATL_gelqf(ATL_CINT M, ATL_CINT N, TYPE  *A, ATL_CINT lda, TYPE *TAU,
               TYPE *WORK, ATL_CINT LWORK)
/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *      SUBROUTINE gelqf( M, N, A, LDA, TAU, WORK, LWORK, INFO )
 *
 * ATL_gelqf.c :
 * int ATL_gelqf(int M, int N, TYPE  *A, int LDA, TYPE  *TAU,
 *              TYPE *WORK, int LWORK)
 *
 *  Purpose
 *  =======
 *
 *  ATL_gelqf  computes an LQ factorization of a real/complex M-by-N matrix A:
 *  A = L * Q.
 *
 *  Compared to LAPACK, here, a recursive panel factorization is implemented.
 *  Refer to ATL_gelqr.c andd ATL_larft.c for details.
 *
 *  Arguments
 *  =========
 *
 *  M       (input) INTEGER
 *          The number of rows of the matrix A.  M >= 0.
 *
 *  N       (input) INTEGER
 *          The number of columns of the matrix A.  N >= 0.
 *
 *  A       (input/output) array, dimension (LDA,N)
 *          On entry, the M-by-N matrix A.
 *          On exit, the elements on and above the diagonal of the array
 *          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
 *          upper triangular if m >= n); the elements below the diagonal,
 *          with the array TAU, represent the orthogonal matrix Q as a
 *          product of min(m,n) elementary reflectors (see Further
 *          Details).
 *
 *  LDA     (input) INTEGER
 *          The leading dimension of the array A.  LDA >= max(1,M).
 *
 *  TAU     (output) array, dimension (min(M,N))
 *          The scalar factors of the elementary reflectors (see Further
 *          Details).
 *
 *  WORK    (workspace/output) array, dimension (MAX(1,LWORK))
 *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 *
 *  LWORK   (input) INTEGER
 *          The dimension of the array WORK.  LWORK >= max(1,N).
 *          For optimum performance LWORK >= N*NB, where NB is
 *          the optimal blocksize.
 *
 *          If LWORK = -1, then a workspace query is assumed; the routine
 *          only calculates the optimal size of the WORK array, returns
 *          this value as the first entry of the WORK array, and no error
 *          message related to LWORK is issued .
 *
 *  INFO    (output) INTEGER
 *          = 0:  successful exit
 *          < 0:  if INFO = -i, the i-th argument had an illegal value
 *
 *  Further Details
 *  ===============
 *
 *  The matrix Q is represented as a product of elementary reflectors
 *
 *     Q = H(1) H(2) . . . H(k), where k = min(m,n).
 *
 *  Each H(i) has the form
 *
 *     H(i) = I - tau * v * v'                  (For Real precision)
 *     H(i) = I - tau * v * conjugate(v)'       (For Complex precision)
 *
 *  where tau is a real/complex scalar, and v is a real/complex vector with
 *  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
 *  and tau in TAU(i).
 *
 */
{
   ATL_CINT minMN = Mmin(M, N), maxMN = Mmax(M, N);
   ATL_INT n, nb, j;
   TYPE  *ws_LQ2,  *ws_T, *ws_larfb;        /* Workspace level 2, T, larfb.   */
   void *vp=NULL;

   /* For transpose function, may need type-appropriate 'ONE' for alpha. */
   #ifdef TREAL
      const TYPE ONE = ATL_rone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #endif
   TYPE *ws_CP=NULL, *ws_CPRaw=NULL;
   ATL_INT ldCP;

   #if defined(ATL_TUNING)
   /*-------------------------------------------------------------------------*/
   /* For tuning recursion crossover points, the blocking factor is set by    */
   /* la2xover, the tuning program for that purpose.                          */
   /*-------------------------------------------------------------------------*/
   if (ATL_PanelTune) nb=ATL_PanelTune; else
   #endif /* ATL_TUNING */

   nb = clapack_ilaenv(LAIS_OPT_NB, LAgeqrf, MYOPT+LALeft+LALower, M, N,-1,-1);

/*
 * If it is a workspace query, return the size of work required.
 *    wrksz = wrksz of ATL_larfb + ATL_larft + ATL_gelq2
 */
   if (LWORK < 0)
   {
      *WORK = ( maxMN*nb + nb*nb + maxMN )  ;
      return(0);
   }
   else if (M < 1 || N < 1)  /* quick return if no work to do */
      return(0);

/*
 * LQ is the transpose of QR: We use this to go from row-major LQ to
 * col-major QR, typically faster. Here, if we are square and large,
 * we transpose the whole matrix in-place and then transpose it back.
 * This should be a tunable parameter; perhaps if the matrix fits in
 * L1 or L2? (Note by Tony C, short on time to conduct tuning).
 */
   if (M == N && N >= 128)
   {
      Mjoin(PATL,sqtrans)(N, A, lda);
      n = ATL_geqrf(M, N, A, lda, TAU, WORK, LWORK);
      Mjoin(PATL,sqtrans)(N, A, lda);

      /* Take the conjugate for Complex TAU. */
      #ifdef TCPLX
      ATL_INT i;
      for (i=1; i<(minMN<<1); i+=2)
         *(TAU+i) = 0.-*(TAU+i);          /* Negate imaginary part. */
      #endif
      return(n);
   }
/*
 * If the user gives us too little space, see if we can allocate it ourselves
 */
   else if (LWORK < (maxMN*nb + nb*nb + maxMN))
   {
      vp = malloc(ATL_MulBySize(maxMN*nb + nb*nb + maxMN) + ATL_Cachelen);
      if (!vp)
         return(-7);
       WORK = ATL_AlignPtr(vp);
   }

/*
 * Assign workspace areas for ATL_larft, ATL_gelq2, ATL_larfb
 */
   ws_T = WORK;                         /* T at begining of work */
   ws_LQ2 = WORK +(nb SHIFT)*nb;        /* After T Work space             */
   ws_larfb = ws_LQ2 + (maxMN SHIFT);   /* After workspace for T and LQ2  */

/*
 * Leave one iteration to be done outside loop, so we don't build T
 * Any loop iterations are therefore known to be of size nb (no partial blocks)
 */
   n = (minMN / nb) * nb;
   if (n == minMN)
      n -= Mmin(nb, minMN);       /* when n is a multiple of nb, reduce by nb */
   #if !defined(ATL_USEPTHREADS)        /* If no PCA, try to copy up front. */
      j = M - n;
      j = Mmax(nb, j);
      ldCP = (N&7) ? (((N+7)>>3)<<3) : N;
      ws_CPRaw = malloc(ATL_MulBySize(ldCP)*j + ATL_Cachelen);
      if (ws_CPRaw) ws_CP=ATL_AlignPtr(ws_CPRaw);  /* Align if malloced. */
   #endif /* Serial Mode */


   for (j=0; j < n; j += nb)
   {
      #if !defined(ATL_USEPTHREADS) /* If no PCA it won't copy. Try it here. */
      /* If we got our copy workspace, transpose panel before recursion. */
      if (ws_CP)                             /* If workspace exists. */
      {
         int ci, cj;                         /* for conjugation.     */
         ldCP = N-j;
         if (ldCP&7)
            ldCP = ((ldCP+7)>>3)<<3;
         Mjoin(PATL,gemoveT)(N-j, nb, ONE, A+(j SHIFT)*(lda+1),
                             lda, ws_CP, ldCP);

         ATL_assert(!ATL_geqrr(N-j, nb, ws_CP, ldCP, TAU+(j SHIFT),
                               ws_LQ2, ws_T, nb, ws_larfb, 1));

         Mjoin(PATL,gemoveT)(nb, N-j, ONE, ws_CP, ldCP,
                             A+(j SHIFT)*(lda+1), lda);

         #if defined(TCPLX)               /* conj upTri T, TAU. */
         for (cj=0; cj<nb; cj++)          /* column loop... */
         {
            TAU[((j+cj) SHIFT)+1] = 0.-TAU[((j+cj) SHIFT)+1];
            for (ci=0; ci<=cj; ci++)      /* row loop... */
               ws_T[((ci+cj*nb) SHIFT)+1] = 0.-ws_T[((ci+cj*nb) SHIFT)+1];
         }
         #endif /* defined(TCPLX) */
      } else /* copy workspace was not allocated, use native. */
      #endif /* Serial Mode (No PCA) */
      {
         ATL_assert(!ATL_gelqr(nb, N-j,  A+(j SHIFT)*(lda+1), lda,
                               TAU+(j SHIFT), ws_LQ2, ws_T, nb, ws_larfb, 1));
      }

      if (j+nb < M)  /* if there are more cols left to bottom, update them */
      {
/*
 *       ======================================================================
 *       Form the triangular factor of the block reflector
 *       After gelqr, ws_T contains 'T', the nb x nb triangular factor 'T'
 *       of the block reflector. It is an output used in the next call, dlarfb.
 *          H = Id - Y'*T*Y, with Id=(N-j)x(N-j), Y=(N-j)xNB.
 *
 *       The ws_T array used above is an input to dlarfb; it is 'T' in
 *       that routine, and LDT x K (translates here to LDWORK x NB).
 *       WORK is an LDWORK x NB workspace (not input or output).
 *       ======================================================================
 */
         ATL_larfb(CblasRight, CblasNoTrans, LAForward, LARowStore,
                   M-j-nb, N-j, nb, A+(j SHIFT)*(lda+1), lda, ws_T, nb,
                   A+((j SHIFT)*(lda+1))+(nb SHIFT), lda, ws_larfb, M);
      }
   }

/*
 *  Build Last panel.  build T is set to 0
 *  RCW: changed nb = minMN-n to nb = M-n, since it is used as the # of rows!
 */
   nb = minMN - n;                            /* remaining factorization. */
   if (nb)                                    /* If we have any cleanup, */
   {
      ATL_CINT mr = M-n;
      #if !defined(ATL_USEPTHREADS)          /* If no PCA try up front copy. */
      if (ws_CP)                             /* If workspace exists. */
      {
         ATL_INT cj;                         /* for conjugation.     */
         ldCP = N-n;
         if (ldCP&7)
            ldCP = ((ldCP+7)>>3)<<3;
         Mjoin(PATL,gemoveT)(N-j, mr, ONE, A+(j SHIFT)*(lda+1),
                             lda, ws_CP, ldCP);

         /* Final parm is whether to build T. No need for T on final panel. */
         ATL_assert(!ATL_geqrr(N-j, mr,  ws_CP, ldCP, TAU+(j SHIFT),
                               ws_LQ2, ws_T, nb, ws_larfb, 0));

         Mjoin(PATL,gemoveT)(mr, N-j, ONE, ws_CP, ldCP,
                             A+(j SHIFT)*(lda+1), lda);

         /* We only need to conjugate final chunk of TAU. */
         #if defined(TCPLX)
         for (cj=0; cj<nb; cj++)
         {
            TAU[((j+cj) SHIFT)+1] = 0.-TAU[((j+cj) SHIFT)+1];
         }
         #endif /* defined(TCPLX) */
      } else /* copy workspace was not allocated, use native. */
      #endif /* Serial Mode (No PCA) */
      {
         /* Final parm is whether to build T. No need for T on final panel. */
         ATL_assert(!ATL_gelqr(M-n, N-n, A+(n SHIFT)*(lda+1), lda,
                               TAU+(n SHIFT), ws_LQ2, ws_T, nb, ws_larfb, 0));
      }
   } /* if we had any cleanup rows to do... */

   if (vp)
      free(vp);

   #if !defined(ATL_USEPTHREADS)             /* If copy workspace possible, */
   if (ws_CPRaw) free(ws_CPRaw);             /* Free any space allocated.   */
   #endif /* Serial Mode */

   return(0);
} /* END ATL_gelqf */

