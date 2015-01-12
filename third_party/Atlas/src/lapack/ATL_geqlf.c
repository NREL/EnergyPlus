
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

int ATL_geqlf(ATL_CINT M, ATL_CINT N, TYPE  *A, ATL_CINT lda, TYPE *TAU,
               TYPE *WORK, ATL_CINT LWORK)
/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *      SUBROUTINE geqlf( M, N, A, LDA, TAU, WORK, LWORK, INFO )
 *
 * ATL_geqlf.c :
 * int ATL_geqlf(int M, int N, TYPE  *A, int LDA, TYPE  *TAU,
 *              TYPE *WORK, int LWORK)
 *
 *  Purpose
 *  =======
 *
 *  ATL_geqlf  computes a QL factorization of a real/complex M-by-N matrix A:
 *  A = Q * L.
 *
 *  Compared to LAPACK, here, a recursive panel factorization is implemented.
 *  Refer to ATL_geqlr.c andd ATL_larft.c for details.
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
   TYPE  *ws_QL2,  *ws_T, *ws_larfb;        /* Workspace level 2, T, larfb.   */
   void *vp=NULL;

   #if defined(ATL_TUNING)
   /*-------------------------------------------------------------------------*/
   /* For tuning recursion crossover points, the blocking factor is set by    */
   /* la2xover, the tuning program for that purpose.                          */
   /*-------------------------------------------------------------------------*/
   if (ATL_PanelTune) nb=ATL_PanelTune; else
   #endif /* ATL_TUNING */

   nb = clapack_ilaenv(LAIS_OPT_NB, LAgeqrf, MYOPT+LARight+LALower, M, N,-1,-1);

/*
 * If it is a workspace query, return the size of work required.
 *    wrksz = wrksz of ATL_larfb + ATL_larft + ATL_geql2
 */
   if (LWORK < 0)
   {
      *WORK = ( N*nb + nb*nb + maxMN )  ;
      return(0);
   }
   else if (M < 1 || N < 1)                 /* quick return if no work to do  */
      return(0);
/*
 * If the user gives us too little space, see if we can allocate it ourselves
 */
   else if (LWORK < (N*nb + nb*nb + maxMN))
   {
      vp = malloc(ATL_MulBySize(N*nb + nb*nb + maxMN) + ATL_Cachelen);
      if (!vp)
         return(-7);
      WORK = ATL_AlignPtr(vp);
   }

/*
 * Assign workspace areas for ATL_larft, ATL_geql2, ATL_larfb
 */
   ws_T = WORK;                             /* T at begining of work          */
   ws_QL2 = WORK +(nb SHIFT)*nb;            /* After T Work space             */
   ws_larfb = ws_QL2 + (maxMN SHIFT);       /* After workspace for T and QL2  */

/*
 * Leave one iteration to be done outside loop, so we don't build T
 * Any loop iterations are therefore known to be of size nb (no partial blocks)
 */
   n = (minMN / nb) * nb;
   if (n == minMN)
      n -= Mmin(nb, minMN);

   for (j=0; j < n; j += nb)
   {
      ATL_assert(!ATL_geqlr(M-j, nb, A+(N SHIFT)*lda-((j + nb) SHIFT)*lda, lda,
                            TAU+( (minMN -(j +nb)) SHIFT), ws_QL2, ws_T, nb,
                            ws_larfb, 1));
      if (j+nb < N)     /* if there are more cols left to right, update them  */
      {
/*
 *       Form the triangular factor of the block reflector
 *          H = H(i) H(i+1) . . . H(i+ib-1)
 *       After geqlr, ws_T contains 'T', the nb x nb triangular factor 'T'
 *       of the block reflector. It is an output used in the next call, dlarfb.
 *          H = Id - Y*T*Y', with Id=(M-j)x(M-j), Y=(M-j)xNB.
 *
 *       Apply H' to A(j:m,j+nb:N) from the left
 *
 *       The ws_T array used above is an input to dlarfb; it is 'T' in
 *       that routine, and LDT x K (translates here to LDWORK x NB).
 *       WORK is an LDWORK x NB workspace (not input or output).
 */
         ATL_larfb(CblasLeft, CblasTrans, LABackward, LAColumnStore,
                   M-j, N-j-nb, nb, A+(N SHIFT)*lda-((j + nb) SHIFT)*lda, lda,
                   ws_T, nb, A, lda, ws_larfb, N);
      }
   }

/*
 * Build Last panel. buildT is passed as 0
 */
   nb = minMN - n;                              /* remaining columns.         */
   ATL_assert(!ATL_geqlr(M-n, N-n, A, lda, TAU, ws_QL2, ws_T, nb, ws_larfb, 0));

   if (vp)
      free(vp);
   return(0);
} /* END ATL_dgeqlf */

