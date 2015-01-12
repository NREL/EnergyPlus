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


int ATL_gerqr(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT LDA, TYPE  *TAU,
               TYPE *ws_RQ2, TYPE *ws_T, ATL_CINT LDT,
               TYPE *WORKM, const int buildT)
{
   int top, bottom, buildT_temp;
   int topMN;
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
         if (minMN >= NB+NB)            /* big prob, put remainder on right   */
         {
            topMN = ATL_MulByNB(ATL_DivByNB(minMN>>1));
            bottom = minMN - topMN;
            top  = M - bottom;
         }
         else /* small prob, keep M mult of MU (MU more critical than NU)     */
         {
            bottom = ((minMN>>1)/ATL_mmMU)*ATL_mmMU;
            topMN = minMN - bottom;
            top = M - bottom;
         }

         if (top==0 || bottom==0)      /* If too small for that,              */
         {
            bottom = (minMN>>1);       /* Stop trying to be clever.           */
            topMN = minMN - bottom;
            top = M - bottom;
         }

      /*----------------------------------------------------------------------*/
      /* On the bottom half, we use the same workspaces.                      */
      /* Because we know we have a top hand side we must always               */
      /* build T, so we can multiply by Q before doing the top side.          */
      /*----------------------------------------------------------------------*/
         ATL_gerqr(bottom,N, (A+(top SHIFT)), LDA, (TAU+(topMN SHIFT)), ws_RQ2,
                   ( ws_T+(topMN SHIFT)+topMN*LDT2), LDT, WORKM, 1);

      /*----------------------------------------------------------------------*/
      /* Now we must adjust the top hand side according to our T.             */
      /* We must apply H'                                                     */
      /*----------------------------------------------------------------------*/

         ATL_larfb(CblasRight, CblasNoTrans, LABackward, LARowStore,
                   top, N, bottom, (A +(top SHIFT))  , LDA,
                   (ws_T+(topMN SHIFT)+topMN*LDT2), LDT, A, LDA, WORKM, M);

      /*----------------------------------------------------------------------*/
      /* On the top  half,                                                    */
      /*----------------------------------------------------------------------*/
         ATL_gerqr(top, N-bottom,(A), LDA, (TAU),
                   ws_RQ2,
                   ( ws_T), LDT, WORKM, buildT);

      /*----------------------------------------------------------------------*/
      /* If we build T, the bottom side must be completely built, and         */
      /* the top side should be partially built. We need to fill in           */
      /* the lower  left  hand block, 'bottom' rows by 'top' columns.         */
      /* The formula is -T2 * (Y2 * Y1^T) * T1.                               */
      /* The routine is in ATL_larft.c.                                       */
      /*----------------------------------------------------------------------*/

         if (buildT )
         {
            ATL_larft_block(LABackward, LARowStore,
                            N, minMN, minMN-bottom, bottom,
                            A+((M -minMN) SHIFT), LDA, ws_T, LDT);
         }

         return(0);

      case 1: /* SERIAL (single core mode) */
         if (minMN >= 4)
         {
            Mjoin(PATL,gemoveT)(N, minMN, ONE, A+((M-minMN) SHIFT),LDA,WORKM,N);
            ATL_geql2(N, minMN, WORKM, N, TAU, ws_RQ2);
            Mjoin(PATL,gemoveT)(minMN,N,ONE,WORKM, N, A+((M-minMN) SHIFT), LDA);
            // make conjugate  of TAU
            #ifdef TCPLX
               Mjoin(PATLU,scal)(minMN, ATL_rnone, TAU+1, 2);
            #endif
         }
         else
         {
            ATL_gerq2(minMN, N, A+((M -minMN) SHIFT) , LDA, TAU, ws_RQ2);
         }

         if (buildT  || M > minMN)
         {
            ATL_larft(LABackward, LARowStore, N, minMN,
                      A+((M -minMN) SHIFT), LDA, TAU, ws_T, LDT);
         }
         break; /* END CASE */

      #if defined(ATL_USEPTHREADS)  /* Cases 2 & 3 only for parallel. */
      case 2: /* PCA COPY */
      case 3: /* PCA NOCOPY (but does not exist for RQ) */
         if (buildT || (M > minMN) )
         {
            buildT_temp = 1;
         }
         else
         {
            buildT_temp = buildT;
         }

         /* Here minMN, N are reversed */
         ATL_tgerq2(N, minMN,  A+((M-minMN) SHIFT), LDA, TAU, ws_RQ2,
                    ws_T, LDT, WORKM, buildT_temp, 1);
         break; /* END CASE */
      #endif /* defined(ATL_USEPTHREADS) */
   } /* END SWITCH on method */

   /* Common code for cases Serial, PCA COPY, PCA NOCOPY */
   if (M > minMN)
   {
      ATL_larfb(CblasRight, CblasNoTrans,
                LABackward, LARowStore, M-minMN, N, minMN, A+((M -minMN) SHIFT),
                LDA, (ws_T), LDT, A, LDA, WORKM, M);
   }

   return(0);
} /* END ATL_gerqr */


