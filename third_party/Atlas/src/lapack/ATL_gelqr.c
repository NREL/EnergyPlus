
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


int ATL_gelqr(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT LDA, TYPE  *TAU,
               TYPE *ws_LQ2, TYPE *ws_T, ATL_CINT LDT,
               TYPE *WORKM, const int buildT)
{
   int top, bottom, buildT_temp;
   int bottomMN;
   int I, INFO, IINFO, lbuilt, rbuilt, method;
   int LDA2 = LDA SHIFT;                    /* for complex LDA *2             */
   int LDT2 = LDT SHIFT;                    /* for complex LDT *2             */
   ATL_CINT minMN = Mmin(M, N);

   #ifdef TCPLX
      TYPE ONE[2] = {ATL_rone, ATL_rzero};
   #else
      #define ONE ATL_rone
   #endif

   if (M < 1 || N < 1) return(0);           /* Nothing to do.                 */
   METHOD(method, N, M, LDA);                   /* Find the method.           */
   #if !defined(ATL_USEPTHREADS)
   if (method == 2 || method == 3) method=1;    /* Don't PCA if no affinity.  */
   #endif

   switch(method)                               /* Based on method;           */
   {
      case 0:  /* RECURSION. */

      /*
       * Choose a smart recursive column partitioning based on M:
       */
         if (minMN >= NB+NB)            /* big prob, put rmndr on bottom.     */
         {
            bottomMN = ATL_MulByNB(ATL_DivByNB(minMN>>1));
            top  = minMN - bottomMN;
            bottom  = M -top;
         }
         else                          /* small prob, keep M mult of MU.      */
         {
            top  = ((minMN>>1)/ATL_mmMU)*ATL_mmMU;
            bottomMN = minMN - top;
            bottom = M - top;
         }

         if (top==0 || bottom==0)      /* If too small for that,              */
         {
            top=(minMN>>1);            /* Get half for top.                   */
            bottomMN = minMN - top;
            bottom = M - top;          /* Rest for bottom.                    */
         }

         /*-------------------------------------------------------------------*/
         /* On the top half, we use the same workspaces.                      */
         /* Because we know we have a bottom hand side we must always         */
         /* build T, so we can multiply by Q before doing the bottom side.    */
         /*-------------------------------------------------------------------*/
            ATL_gelqr(top, N, A, LDA, TAU, ws_LQ2, ws_T, LDT, WORKM, 1);

         /*-------------------------------------------------------------------*/
         /* Now we must adjust the bottom hand side according to our T.       */
         /* We must apply H' to A[0:(M-1), top:(N-1)].                        */
         /*-------------------------------------------------------------------*/

            ATL_larfb(CblasRight, CblasNoTrans, LAForward, LARowStore,
                       bottom, N, top, A, LDA, ws_T, LDT, A+(top SHIFT),
                       LDA, WORKM, M);

         /*-------------------------------------------------------------------*/
         /* On the bottom half, we must adjust all pointers.                  */
         /*-------------------------------------------------------------------*/
            ATL_gelqr(bottom, N-top, (A+(top SHIFT)+top*LDA2), LDA,
                      (TAU+(top SHIFT)), ws_LQ2,
                      (ws_T+(top SHIFT)+top*LDT2), LDT, WORKM, buildT);

         /*-------------------------------------------------------------------*/
         /* If we build T, the left/top side must be completely built, and    */
         /* the right/bottom side should be partially built. We need to fill  */
         /* in the upper left hand block, 'top' rows by 'bottom' columns.     */
         /* The formula is -T1 * (Y1 * Y2^T) * T2.                            */
         /* The routine is in ATL_larft.c.                                    */
         /*-------------------------------------------------------------------*/

            if (buildT)
            {
               ATL_larft_block(LAForward, LARowStore, N, minMN, top, bottomMN,
                               A, LDA, ws_T, LDT);
            }
            return(0);
            break;

      case 1: /* SERIAL */
         /*
          *       ATL_gelq2(minMN, N, A, LDA, TAU, ws_LQ2);
          *       Transpose the input and sent to QR2 serial.
          */
         if (minMN >= 4)
         {
            Mjoin(PATL,gemoveT)(N, minMN, ONE, A, LDA, WORKM, N);
            ATL_geqr2(N, minMN, WORKM, N, TAU, ws_LQ2);
            Mjoin(PATL,gemoveT)(minMN, N, ONE, WORKM, N, A, LDA);
            #ifdef TCPLX
               Mjoin(PATLU,scal)(minMN, ATL_rnone, TAU+1, 2);
            #endif
         }
         else
         {
               ATL_gelq2(minMN, N, A, LDA, TAU, ws_LQ2);
         }

         if (buildT || (M > minMN) )
         {
/*          Build the T matrix.                                               */
            ATL_larft(LAForward, LARowStore, N, minMN, A, LDA,
                    TAU, ws_T, LDT);
         }
         break; /* END CASE */

      #if defined(ATL_USEPTHREADS)  /* Cases 2 & 3 only for parallel. */
      case 2:  /* PCA COPY */
      case 3:  /* PCA NOCOPY (does not exist for LQ) */

         if (buildT || (M > minMN) )
         {
            buildT_temp = 1;
         }
         else
         {
            buildT_temp = buildT;
         }
/*       call lq2 with '1' for copy.          */
         ATL_tgelq2(N, minMN, A, LDA, TAU, ws_LQ2, ws_T, LDT, WORKM,
                    buildT_temp, 1);

         break; /* END CASE */
      #endif /* defined(ATL_USEPTHREADS) */
   } /* END SWITCH on method */

   /* Common code for cases Serial, PCA COPY, PCA NOCOPY */
   /*
    *   Adjust bottom according to T:  apply H' , if M > minMN
    */
   if ( M > minMN )
   {
      ATL_larfb(CblasRight, CblasNoTrans,
                LAForward, LARowStore, M-minMN, N, minMN, A, LDA, ws_T, LDT,
                A+( minMN SHIFT), LDA, WORKM, M);
   }

   return(0);
} /* END ATL_gelqr */


