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
#include "atlas_lvl3.h"
#include "atlas_qrrmeth.h"

#ifdef ATL_USEPTHREADS
   #include "atlas_threads.h"
   #include "atlas_taffinity.h"
   #include "atlas_tcacheedge.h"
#else
   #include "atlas_cacheedge.h"
#endif


int ATL_geqlr(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT LDA, TYPE  *TAU,
               TYPE *ws_QL2, TYPE *ws_T, ATL_CINT LDT,
               TYPE *WORKM, const int buildT)
{
/*
 * This is a recursive implementation of ATL_geqlf.c; it performs a QR
 * factorization of a panel (M > N) with a bottom level of ATL_geql2. The
 * recursion is on columns only; it divides by 2 until it reaches a
 * stopping point; at which time it calls ATL_geql2 to complete a sub-panel,
 * ATL_larft and ATL_larfb to propagate the results, etc.
 *
 * ATL_geqlr.c :
 * int ATL_geqlr(int M, int N, TYPE *A, int LDA, TYPE  *TAU,
 *               TYPE *ws_QL2, TYPE *ws_T, int LDT,
 *               TYPE *WORKM, int buildT)
 *      NOTE :   ATL_geql2.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *  Purpose
 *  =======
 *
 *  ATL_geqlr computes a QR factorization of a real M-by-N matrix A:
 *  A = Q * L.
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
 *  A       (input/output)  array, dimension (LDA,N)
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
 *
 *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
 *          The scalar factors of the elementary reflectors (see Further
 *          Details).
 *
 *  ws_QL2  (workspace) workspace for geql2 factorization. To be allocated
 *          with space of max(M,N)
 *
 *  ws_T    (input/output).  Is the size of T matrix. To be allocated
 *          with a space of min(M,N) X min(M,N). If buildT flag is true,
 *          T is computed and populated as output. If buildT is false,
 *          T must not be used as output
 *
 *  LDT     (input) INTEGER
 *          The leading dimension of the array T.  LDT >= max(1,min(M,N)).
 *
 *  WORKM   (workspace) Work space matrix, N rows by
 *          minMN columns; the amount used by larfb.
 *
 *  buildT  If non-zero, dgeqrr will build in ws_T the complete T necessary
 *          for the original panel it is passed;
 *          such that Q= I - transpose(Y) * T * Y.
 *          If zero, ws_T will contain only those elements of T necessary to
 *          complete the panel.
 */

   int left, right;
   int leftMN;
   int I, INFO, IINFO, lbuilt, rbuilt, method;
   int LDA2 = LDA SHIFT;                    /* for complex LDA *2             */
   int LDT2 = LDT SHIFT;                    /* for complex LDT *2             */
   ATL_CINT minMN = Mmin(M, N);

   if (M < 1 || N < 1) return(0);           /* Nothing to do.                 */
   METHOD(method, M, N, LDA);                   /* Find the method.           */
   #if !defined(ATL_USEPTHREADS)
   if (method == 2 || method == 3) method=1;    /* Don't PCA if no affinity.  */
   #endif

   switch(method)                               /* Based on method;           */
   {
      case 0:  /* RECURSION. */


      /*
       * Choose a smart recursive column partitioning based on N:
       * The mapping from this routs dim:GEMM is : right:M, left:N, K:M-left
       * For big probs, max M & K by making left small & a mult of NB.
       * For small problems, make M a mult of MU (many x86 have NU=1 anyway!).
       */

         if (minMN >= NB+NB) /* big prob, put remainder on right.             */
         {
            leftMN = ATL_MulByNB(ATL_DivByNB(minMN>>1));
            right = minMN - leftMN;
            left  = N -right;
         }
         else /* small prob, keep M mult of MU (MU more critical than NU)     */
         {
            right = ((minMN>>1)/ATL_mmMU)*ATL_mmMU;
            leftMN = minMN - right;
            left = N - right;
         }

         if (left==0 || right==0)               /* Stop trying to be fancy.   */
         {
            right = (minMN>>1);
            leftMN = minMN - right;
            left = N - right;
         }

      /*----------------------------------------------------------------------*/
      /* On the right half, we use the same workspaces.                       */
      /* Because we know we have a left hand side we must always              */
      /* build T, so we can multiply by Q before doing the left side.         */
      /* Build T @ T[left,left].                                              */
      /*----------------------------------------------------------------------*/
         ATL_geqlr(M, right, (A+(left*LDA2)), LDA, (TAU+(leftMN SHIFT)), ws_QL2,
                   (ws_T+(leftMN SHIFT)+leftMN*LDT2), LDT, WORKM, 1);

      /*----------------------------------------------------------------------*/
      /* Now we must adjust the left hand side according to our T.            */
      /* We must apply H'                                                     */
      /*----------------------------------------------------------------------*/

         ATL_larfb(CblasLeft, CblasTrans,
                   LABackward, LAColumnStore,
                   M, left, right, (A +(left*LDA2))  , LDA,
                   (ws_T+(leftMN SHIFT)+leftMN*LDT2),
                   LDT, A, LDA, WORKM, N);

      /*----------------------------------------------------------------------*/
      /* On the left  half, we must adjust all pointers.                      */
      /*----------------------------------------------------------------------*/
         ATL_geqlr(M-right, left, (A), LDA, (TAU), ws_QL2, ( ws_T),
                   LDT, WORKM, buildT);

      /*----------------------------------------------------------------------*/
      /* If we build T, the right side must be completely built, and          */
      /* the left side should be partially built. We need to fill in          */
      /* the lower  left  hand block, 'right' rows by 'left' columns.         */
      /* The formula is -T2 * (Y2^T * Y1) * T1.                               */
      /* The routine is in ATL_larft.c.                                       */
      /*----------------------------------------------------------------------*/

         if (buildT)
         {
            ATL_larft_block(LABackward, LAColumnStore,
                            M, minMN, minMN-right, right,
                            (A+((N-minMN)*LDA2)) , LDA,
                            ws_T, LDT);
         }

         return(0); /* END CASE RECURSION */


      case 1:  /* SERIAL. */

         ATL_geql2(M, minMN, A+((N-minMN)*LDA2), LDA, TAU, ws_QL2);

         if (buildT || (N > minMN) )
         {
            ATL_larft(LABackward, LAColumnStore, M, minMN, A+((N-minMN)*LDA2),
                      LDA, TAU, ws_T, LDT);
         }
         break; /* END CASE (Update T is after switch). */

      #if defined(ATL_USEPTHREADS)  /* Cases 2 & 3 only for parallel. */
      case 2: /* PCA COPY (last two parameters: BuildT, Copy) */
         ATL_tgeql2(M, minMN, A+((N-minMN)*LDA2), LDA, TAU, ws_QL2,
                    ws_T, LDT, WORKM, 1, 1);
         break; /* END CASE (Update T is after switch). */

      case 3: /* PCA NOCOPY (last two parameters: BuildT, Copy) */
         ATL_tgeql2(M, minMN, A+((N-minMN)*LDA2), LDA, TAU, ws_QL2,
                    ws_T, LDT, WORKM, 1, 0);
         break; /* END CASE (Update T is after switch). */
      #endif /* defined(ATL_USEPTHREADS) */
   } /* END SWITCH on method. */

   /*
    *   For cases Serial, PCA_Copy, PCA NoCopy, we must update T.
    *   Adjust remainder matrix according to T:  apply H' , if N > minMN
    */
   if (N > minMN )
   {
      ATL_larfb(CblasLeft, CblasTrans, LABackward, LAColumnStore,
                M, N-minMN, minMN, (A+((N-minMN)*LDA2))  , LDA, (ws_T),
                LDT, A, LDA, WORKM, N);
   }

   return(0);
} /* END ATL_geqlr */


