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
/*-----------------------------------------------------------------------------
 * This is the C translation of the standard LAPACK Fortran routine:
 *      SUBROUTINE DGEQL2( M, N, A, LDA, TAU, WORK, INFO )
 *
 * Reference :
 *      Scaling LAPACK Panel Operations Using Parallel Cache Assignment
 *          Principles and Practice of Parallel Programming (PPoPP)' 10
 *          Jan 9-14, 2010, Bangalore, India
 *            by   Anthony M. Castaldo,  R.Clint Whaley
 *
 * ATL_tgeql2.c :
 *
 * int ATL_geql2( const int M, const int N, TYPE *A, int LDA,
 *                             TYPE  *TAU, TYPE *WORK)
 *     NOTE :a)  ATL_geql2_t.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *
 *           b) This routine will not validate the input parameters.
 *
 *  Purpose
 *  =======
 *
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
 *  A       (input/output) array, dimension (LDA,N)
 *          On entry, the m by n matrix A.
 *          On exit, if m >= n, the lower triangle of the subarray
 *          A(m-n+1:m,1:n) contains the n by n lower triangular matrix L;
 *          if m <= n, the elements on and below the (n-m)-th
 *          superdiagonal contain the m by n lower trapezoidal matrix L;
 *          the remaining elements, with the array TAU, represent the
 *          orthogonal matrix Q ((unitary matrix incase of complex precision )
 *          as a product of elementary reflectors (see Further Details).
 *
 *  LDA     (input) INTEGER
 *          The leading dimension of the array A.  LDA >= max(1,M).
 *
 *  TAU     (output) array, dimension (min(M,N))
 *          The scalar factors of the elementary reflectors (see Further
 *          Details).
 *
 *  WORK    (workspace) array, dimension (N)
 *
 *  INFO    (output) INTEGER
 *          = 0: successful exit
 *          < 0: if INFO = -i, the i-th argument had an illegal value
 *
 *  Further Details
 *  ===============
 *
 *  The matrix Q is represented as a product of elementary reflectors
 *
 *     Q = H(k) . . . H(2) H(1), where k = min(m,n).
 *                                             (for Real/Complex Precisions)
 *
 *  Each H(i) has the form
 *
 *     H(i) = I - tau * v * v'                 (for Real Precision)
 *     H(i) = I - tau * v * conjugate(v)'      (for Complex  Precision)
 *
 *
 *  where tau is a real scalar, and v is a real vector with
 *  v(m-k+i+1:m) = 0 and v(m-k+i) = 1; v(1:m-k+i-1) is stored on exit in
 *  A(1:m-k+i-1,n-k+i), and tau in TAU(i).
 *
 *
 *-------------------------------------------------------------------------
 * Threads are managed as below.  Instead of having
 * thread zero manage the computation by specifying tasks to workers, we
 * have workers proceed automatically to their next task; looping through
 * each of the columns on their own and only synchronizing when necessary.
 *
 * We launch and join as below. For example:
 *
 * Core To Thread Mapping
 *
 * Core  Binary  Reversed  Decimal   Busy Map
 *  0     000      000      0        X.......
 *  1     001      100      4        X...X...
 *  2     010      010      2        X.X.X...
 *  3     011      110      6        X.X.X.X.
 *  4     100      001      1        XXX.X.X.
 *  5     101      101      5        XXX.XXX.
 *  6     110      011      3        XXXXXXX.
 *  7     111      111      7        XXXXXXXX
 *
 * Worker Thread assigned  as below :
 *
 * Thread Binary  #ofTrailing  ReplaceTrailing      Waits for
 *                zeroes       bits by 1            (+ core)
 *
 *  0     000      3          +1 +2 +4             1  2 4
 *  1     001      0              _                   _
 *  2     010      1          +1                   3
 *  3     011      0              _                   _
 *  4     100      2          +1 +2                5  6
 *  5     101      0              _                   _
 *  6     110      1          +1                   7
 *  7     111      0              _                   _
 *
 *  There is no trivial non-looping method of reversing the binary digits,
 *  so we use an integer array for the indices, and another array to keep
 *  track of working cores. The join operation for a node just skips the
 *  core if it is inactive.
 *
 *  For example, core 0 must join 1, 2, 4, but if only three threads were
 *  engaged, it skips the join on thread 1, joins 2, and then joins 4.
 *  Thread 2 would normally need to combine 3, but since 3 is inactive, it
 *  (Thread 2) will just post itself done immediately.
 *
 *  This approach balances the binary tree and lets us use our existing
 *  join method.
 *
 *----------------------------------------------------------------------------*/

#include "stdio.h"
#include "cblas.h"
#include "atlas_tlapack.h"
#include "atlas_lapack.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"
#include "atlas_level2.h"
#include "atlas_lamch.h"
#include "math.h"
#include "errno.h"

#ifdef TREAL
   static const TYPE ONE = ATL_rone;
#else
   static const TYPE ONE[2] = {ATL_rone, ATL_rzero};
#endif

#define  MY_align 64                        /* ONLY powers of 2 work.         */

#ifdef  SREAL
   #define  MY_gemv ATL_sgemv               /* L2 tuned dgemv                 */
   #define  MY_gemvT ATL_sgemvT_L2          /* L2 tuned dgemv                 */
   #define  ATL_axpy ATL_saxpy
   #define  MY_ger  ATL_sger_L2             /* L2 tuned dger                  */
   #define MY_gecopy ATL_sgecopy            /* Copy routine to use.           */
#endif
#ifdef  DREAL
   #define  MY_gemv ATL_dgemv               /* L2 tuned dgemv                 */
   #define  MY_gemvT ATL_dgemvT_L2          /* L2 tuned dgemv                 */
   #define  ATL_axpy ATL_daxpy
   #define  MY_ger  ATL_dger_L2             /* L2 tuned dger                  */
   #define MY_gecopy ATL_dgecopy            /* Copy routine to use.           */
#endif
#ifdef  SCPLX
   #define  ATL_axpy ATL_caxpy
   #define  MY_gemvT ATL_cgemvCT_L2         /* L2 tuned dgemv                 */
   #define  MY_gemv ATL_cgemv               /* L2 tuned dgemv                 */
   #define  MY_ger  ATL_cgerc_L2            /* L2 tuned dger                  */
   #define MY_gecopy ATL_cgecopy            /* Copy routine to use.           */
#endif
#ifdef  DCPLX
   #define  ATL_axpy ATL_zaxpy
   #define  MY_gemvT ATL_zgemvCT_L2         /* L2 tuned dgemv                 */
   #define  MY_gemv ATL_zgemv               /* L2 tuned dgemv                 */
   #define  MY_ger  ATL_zgerc_L2            /* L2 tuned dger                  */
   #define MY_gecopy ATL_zgecopy            /* Copy routine to use.           */
#endif

#ifdef TREAL
    #define MY_TRANS CblasTrans
#else
    #define MY_TRANS CblasConjTrans
#endif

#define ATL_ger_L2 Mjoin(PATL,ger_L2)
#define ATL_geql2_dnrm2 Mjoin(PATL,geql2_dnrm2)
#define ATL_geql2_dnrm2_ql Mjoin(PATL,geql2_dnrm2_ql)
#define ATL_geql2_LC_Setup Mjoin(PATL,geql2_LC_Setup)
#define ATL_geql2_DoCopy Mjoin(PATL,geql2_DoCopy)
#define ATL_geql2_UnCopy Mjoin(PATL,geql2_UnCopy)
#define ATL_geql2_Cache Mjoin(PATL,geql2_Cache)
#define ATL_geql2Worker Mjoin(PATL,geql2Worker)
#define ATL_geql2Worker_Zero Mjoin(PATL,geql2Worker_Zero)
#define ATL_tgeql2 Mjoin(PATL,tgeql2)
#define ATL_geql2_Order Mjoin(PATL,geql2_Order)


double time00(void);                        /* prototype external rtn.        */
/*----------------------------------------------------------------------------*/
/* rdtsc reads the time stamp counter into %rax, which will be aliased as a   */
/* temp for the memory location 'time' which MUST be a long long location!!!  */
/* This takes about 30 cycles to complete.                                    */
/*----------------------------------------------------------------------------*/
#define rdtsc(time)                 \
   __asm__ __volatile__ (           \
      "rdtsc;\n\t"                  \
      "shlq $32, %%rdx;\n\t"        \
      "addq %%rdx, %%rax;\n\t"      \
      : "=a"(time) : : "%rdx" );


typedef struct                              /* Threading structure.           */
{
   ATL_INT  fullM;
   ATL_INT  fullN;
   ATL_INT  myM;
   ATL_INT  myN;
   ATL_INT  myK;
   ATL_INT  lda;
   ATL_INT  rank;
   volatile ATL_INT  dnrm2;
   volatile ATL_INT  dgemv;
   ATL_INT  active;
   TYPE     *A;
#ifdef TREAL
   volatile TYPE     zDiag;
#else
   volatile TYPE  zDiag[2];
#endif
   TYPE     *TAU;
   TYPE     *oldA;
   ATL_INT  oldLDA;
   TYPE     *WORK;
   volatile TYPE     Scale;
   volatile TYPE     SSQ;
   TYPE     *T;
   ATL_INT  LDT;
   ATL_INT  buildT;
   volatile ATL_INT  dgemvt;
   ATL_INT  copy;
} ATL_DGEQL2_t;


/* This code is used three times below, macro to prevent bug propagation.     */
#define dnrm2_combine_ql \
   if (myTS->Scale > ptnr->Scale)   /* If my scale is bigger,  */ \
   {                                                              \
      w = (ptnr->Scale)/(myTS->Scale); /* Must scale his SSQ.  */ \
      myTS->SSQ += (ptnr->SSQ)*w*w; /* Add scaled version.     */ \
   } else                           /* His scale is bigger.    */ \
   {                                                              \
      w = (myTS->Scale)/(ptnr->Scale); /* Must scale my SSQ.   */ \
      myTS->SSQ *= (w*w);           /* Scale my SSQ first.     */ \
      myTS->SSQ += ptnr->SSQ;       /* Add his to mine.        */ \
      myTS->Scale = ptnr->Scale;    /* Replace my SSQ with his.*/ \
   } /* END #define dnrm2_combine */


//-----------------------------------------------------------------------------
//  ATL_dgeqr2_dnrm2: Compute a dnrm2, but retain scale and value.
// ---------------------------------------------------------------------------

static void ATL_geql2_dnrm2_ql(ATL_DGEQL2_t *ts)
{
   int i,  N=ts->myM, N2;
   #ifdef TREAL
      N2 = N ;
   #else
      N2 = N << 1;
   #endif
   TYPE  aX, sX, SSQ=0., Scale=1.0;
   TYPE *X=ts->A+(((ts->lda) SHIFT)*(ts->myN -1 -ts->myK ));
                                             /* Point at proper column. */

   if (ts->rank != 0)                       /* If a standard compute,         */
   {
      for (i=0; i<N2; i++)                  /* Do the full column.            */
      {
         if (X[i] != 0.)
         {
            aX = fabs(X[i]);
            if (Scale < aX)
            {
               SSQ = 1.+SSQ*(Scale/aX)*(Scale/aX);
               Scale = aX;
            } else
            {
               SSQ += (aX/Scale)*(aX/Scale);
            }
         } //for
      }
   } else                                   /* This is the Rank 0 Operation   */
   {

      N2 = ((N-ts->myK -1) SHIFT);          /* Not all the N is taken         */
      for (i=0; i<N2; i++)                  /* QL starts from zero.           */
      {
         if (X[i] != 0.)
         {
            aX = fabs(X[i]);
            if (Scale < aX)
            {
               sX = Scale/aX;               /* Do scale only once.            */
               SSQ = 1.+SSQ*sX*sX;          /* Scale SSQ.                     */
               Scale = aX;                  /* Use the new Scale.             */
            } else
            {
               sX = aX/Scale;               /* Scale the value.               */
               SSQ += sX*sX;                /* Square and add to SSQ.         */
            }
         }
      }//for
   }

   ts->Scale = Scale;
   ts->SSQ   = SSQ;
}                                           /* END ATL_dgeqr2_dnrm2           */


/*
 * LC_Setup sets up copy.
 */
static size_t ATL_geql2_LC_Setup(ATL_DGEQL2_t *myTS)
{
   int N = myTS->fullN;                     /* width of array.                */
   int M = myTS->myM;                       /* height of array.               */
   myTS->oldLDA = myTS->lda;                /* Remember the old one.          */
   myTS->oldA   = myTS->A;                  /* ...                            */
   int LDA = (M+1)&(-2);                    /* My new LDA, multiple 2.        */
   myTS->lda = LDA;                         /* Store new LDA.                 */
   size_t memSize = ((LDA SHIFT)*N*sizeof(TYPE));/* Room I need.              */
   memSize = (memSize+MY_align-1)&(-MY_align);   /* As multiple of MY_align.  */
   return(memSize);                         /* Exit with amt needed.          */
}                                           /* ** END ATL_geql2_LC_Setup **   */

/*
 * DoCopy works from LC_Setup.
 */
static void ATL_geql2_DoCopy(ATL_DGEQL2_t *myTS)
{
   int N = myTS->fullN;                     /* width of array.                */
   int M = myTS->myM;                       /* height of array.               */

   #ifdef BUILD_RQ2
   Mjoin(PATL,gemoveT)(M, N, ONE,  myTS->oldA,
         myTS->oldLDA, myTS->A, myTS->lda);
   #else
   MY_gecopy(M, N, myTS->oldA, myTS->oldLDA,
             myTS->A, myTS->lda);
   #endif
} /* END ATL_geql2_DoCopy. */

/*
 * UnCopy is just a copy back, no free.
 */
static void ATL_geql2_UnCopy(ATL_DGEQL2_t *myTS)
{
   int N = myTS->fullN;                     /* width of array.                */
   int M = myTS->myM;                       /* height of array.               */

   #ifdef BUILD_RQ2
   Mjoin(PATL,gemoveT)(N, M , ONE, myTS->A,
                       myTS->lda, myTS->oldA, myTS->oldLDA);
   #else
   MY_gecopy(M, N, myTS->A, myTS->lda,
             myTS->oldA, myTS->oldLDA);
   #endif

   myTS->lda = myTS->oldLDA;                /* Restore the old one.           */
   myTS->A = myTS->oldA;                    /* ...                            */
}                                           /* ** END ATL_geql2_UnCopy **     */

/*
 * Cache will force a read every 64th byte (eighth double) in an array,
 * following LDA.
 */
static void ATL_geql2_Cache(ATL_DGEQL2_t *myTS)
{
   int r, c;
   int N = myTS->fullN;                     /* width of array.                */
   int M = myTS->myM;                       /* height of array.               */
   int LDA = myTS->lda;
   TYPE *A = myTS->A;
   volatile TYPE x;

   for (c=N-1; c>=0; c--)                   /* Every column, backwards.       */
   {
      for (r=((M-1) SHIFT); r>=0; r-=8)     /* every eighth double.           */
      {
         x = *(A+(LDA SHIFT)*c+r);          /* read the variable.             */
      }
   }
}                                           /* ** END ATL_dgeql2_Cache **     */

/*
 * Callback for thread launcher
 */
static int Mjoin(PATL,StructIsInitGEQL2)(void *vp)
{
   //return 1;
   return(((ATL_DGEQL2_t*)vp)->active);
}


/*
 * ATL_geql2Worker: Persistent for the duration of the DGEQL2 operation.
 * Argument is pointer to a structure which contains our data section.
 */
//void* ATL_geql2Worker(void *myArg)
static void* ATL_geql2Worker(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp=vp;
   ATL_DGEQL2_t *myTS = ((ATL_DGEQL2_t*) lp->opstruct)+tp->rank;
   ATL_DGEQL2_t *zTS = myTS - (myTS->rank); /* Point at zero ts.              */
   int   zees, pair, myRank;                /* Log2 looping variables.        */
   int   i_loop;
   int   i,j,M,N,LDA,mScale,mUpdate,newN,KNT,LDT;
   int   idup;
   int   myCopy=myTS->copy;
   volatile ATL_DGEQL2_t *ptnr;             /* partner log2 combine.          */
   TYPE  *A, *scaleA, *T;                   /* Work variables.                */
   TYPE XNORM, BETA, BETAp;
   TYPE *TAU = myTS->TAU;                   /* Short cut.              */
   TYPE ALPHA;
   TYPE w;

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      TYPE AII ;
      TYPE TAUVAL ;
      TYPE myTAUi;
      TYPE sc;
      TYPE RSAFMN;
      const TYPE ZERO = ATL_rzero;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      TYPE AII[2];
      TYPE TAUVAL[2] ;
      TYPE myTAUi[2] ;
      TYPE negTAUi[2] ;

      TYPE ALPHAI, ALPHAR;
      TYPE ALPHADIV[2] ;
      TYPE sc[2];
      TYPE RSAFMN[2];
      const TYPE ZERO[2] = {ATL_rzero, ATL_rzero};
   #endif

   myRank = myTS->rank;                     /* Get my rank.                   */
   T = zTS->T;
   LDT = zTS->LDT;

/*----------------------------------------------------------------------------*/
/* Now we begin the real dgeql2.                                              */
/*----------------------------------------------------------------------------*/
   N = myTS->fullN;                         /* panel width.                   */

   if (myCopy)
   {
      ATL_geql2_DoCopy(myTS);                  /* Copy my data.               */
   }

   LDA = myTS->lda;                         /* Load AFTER local copy.         */
   M = myTS->myM;                           /* Shortcut to M.                 */

/* Here the computation should start from left to right.               */

   for (i=N-1; i >= 0; i--)                 /* Now, for each column,          */
   {
/*    i starts from N -1                                                      */
      myTS->myK =  N - i -1;                /* 0, 1, ... N-1  Set my K value. */
      idup =  N - i -1;                     /* 0, 1, ... N-1  Set my K value. */

      if (myRank == 0)                      /* Zero follows diagonal.         */
      {
         mScale = M-idup-1;                 /* Special scaling value.         */
         mUpdate = M-idup;                  /* Special gemv/ger size.         */
         A = myTS->A + (i* (LDA SHIFT));    /* Special pointer to A.          */

         /* Save diagonal entry, replace with 1.0. */
         #ifdef TREAL
            myTS->zDiag = *(A + ((M -1 - myTS->myK) SHIFT));
            *(A + M -1 - myTS->myK ) = 1.0;
         #else
            myTS->zDiag[0] = *(A + ( (M -1 - myTS->myK) SHIFT));
            myTS->zDiag[1] = *(A + ( (M -1 - myTS->myK) SHIFT) + 1);
            *(A + ((M -1 - myTS->myK) SHIFT) ) = 1.0;
            *(A + ((M -1 - myTS->myK) SHIFT) + 1 ) = 0.0;
         #endif

         scaleA = A;                        /* What to scale.                 */
      } else                                /* Others keep square.            */
      {
         mScale = M;                        /* Always scale full col.         */
         mUpdate = M;                       /* Always gemv/ger full co.       */
         A = myTS->A + (i* (LDA SHIFT));    /* Point at column.               */
         scaleA = A;                        /* What to scale.                 */
      }

      /* i starts from N -1. */

      ATL_geql2_dnrm2_ql(myTS);

      zees = myRank;                        /* Init the test flags.           */
      pair = 1;                             /* Starting pair.                 */
      while ( (zees & 1) == 0 &&            /* If I must wait on pair,        */
         (myRank)+pair < ATL_NTHREADS)      /* ..and pair exists,             */
      {
         ptnr = myTS+pair;                  /* Point at my partner.           */

         if (ptnr->active == 1)             /* If ptnr was used,              */
         {
            while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.              */
            dnrm2_combine_ql;
         }

         zees >>= 1;                        /* Shift a zero out.              */
         pair <<= 1;                        /* Double the pair idx.           */
      }

      myTS->dnrm2++;                        /* Signal I am done.              */


      /****************************************/
      /********  S Y N C   P O I N T  *********/
      /********  S Y N C   P O I N T  *********/
      /********  S Y N C   P O I N T  *********/
      /****************************************/

      /*-----------------------------------------------------------------*/
      /* Note: We can avoid syncing immediately after the DGER, but DGER */
      /* does use zTS->WORK as its vector. So here is the point where we */
      /* have to make sure everybody's DGER is done, and they have done  */
      /* their norm2 work. All threads converge here for the next column */
      /* and so will be done with their DGER, and zTS->WORK is free.     */
      /*-----------------------------------------------------------------*/
      while (zTS->dnrm2 < myTS->dnrm2);     /* Wait for zero to finish.       */

      /*-------------------------------------------------------------*/
      /* At this point, zTS->SSQ is computed. If it is zero, then no */
      /* rotation is needed, and TAU should be set to zero, and we   */
      /* just skip to the next column.                               */
      /* HOWEVER, core zero may be fast on the next compare, and     */
      /* change zTS->SSQ before some other core gets a chance to see */
      /* it was zero. So we must have a barrier here before we go on.*/
      /*                                                             */
      /* In Complex we require both SSQ == 0 and IMAG(A[i,i]) == 0.  */
      /*-------------------------------------------------------------*/
      /* If H should be Identity, set TAU to zero and go to next column. */
      #ifdef TREAL
      if (zTS->SSQ == 0.)
      #else /* COMPLEX */
      if (zTS->SSQ == 0. && zTS->zDiag[1] == 0.)
      #endif /* REAL or CPLX */
      {
         zees = myRank;                     /* Init the test flags.           */
         pair = 1;                          /* Starting pair.                 */
         while ( (zees & 1) == 0 &&         /* If I must wait on pair,        */
            (myRank)+pair < ATL_NTHREADS)   /* ..and pair exists,             */
         {
            ptnr = myTS+pair;               /* Point at my partner.           */

            if (ptnr->active == 1)          /* If ptnr was used,              */
            {
               while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.           */
            }

            zees >>= 1;                     /* Shift a zero out.              */
            pair <<= 1;                     /* Double the pair idx.           */
         }

         if (myRank == 0)                   /* Replace A[i,i] we repl with 1. */
         {
            #ifdef TREAL
            *(A + M -1 - idup) =  myTS->zDiag;
            TAU[i] = 0.;                    /* clear TAU so H[i]=Identity.    */
            #else
            *(A + ((M -1 - idup) SHIFT)) =  myTS->zDiag[0];
            *(A + ((M -1 - idup) SHIFT) +1) =  myTS->zDiag[1];
            TAU[(i SHIFT)] = 0.;            /* clear TAU so H[i]=Identity.    */
            TAU[(i SHIFT)+1] = 0.;
            #endif
         }

         myTS->dnrm2++;                     /* Signal I am done.              */
         while (zTS->dnrm2 < myTS->dnrm2);  /* Wait for zero to finish.       */
         continue;                          /* ..Proceed to next column.      */
      } /* END if H=Identity no need to process column. */

/*----------------------------------------------------------------------------*/
/* Here, H is not identity, we 'continued' the loop if it was.                */
/*----------------------------------------------------------------------------*/
      XNORM = (zTS->Scale)*sqrt(zTS->SSQ);  /* Compute the norm.              */

/*----------------------------------------------------------------------------*/
/* The following code is inlined from ATL_larfg.                              */
/* Here, SSQ is non-zero, we exited the loop above if that occurred.          */
/*----------------------------------------------------------------------------*/

   #ifdef TREAL
/*----------------------------------------------------------------------------*/
/*    The following code is inlined from ATL_larfg; the main difference is    */
/*    that we use zTS->zDiag instead of ALPHA, and recompute is parallel.     */
/*----------------------------------------------------------------------------*/
      BETAp = ATL_lapy2((zTS->zDiag), XNORM);    /* Get sqrt(a^2+b^2)         */
      BETA = BETAp;                              /* Assume diagonal < 0 ...   */
      if ((zTS->zDiag) >= 0.) BETA = -BETAp;     /* .. If >= 0, change sign.  */

      KNT = 0;                                   /* Init power to zero. */
      if (BETAp < ATL_laSAFMIN)
      {
         RSAFMN = ATL_rone / ATL_laSAFMIN;       /* Set a maximum. */

         /*---------------------------------------------------------------*/
         /* BETAp is the same for all cores, so this loop can be executed */
         /* independently. However, XNORM must be computed in concert.    */
         /* The new BETAp will be at most 1, at least SAFMIN.             */
         /*---------------------------------------------------------------*/
         while (BETAp < ATL_laSAFMIN)
         {
            KNT++;                              /* increment power. */
            if ( myTS->active == 1)             /* If I am active, */
            {
               cblas_scal(mScale, RSAFMN,
                                    scaleA, 1); /* Scale my share. */
            }

            BETA *= RSAFMN;
            BETAp *= RSAFMN;
            if (myRank==0) zTS->zDiag *= RSAFMN;   /* Only done by core 0! */
         }

         /* i starts from N -1. */
         ATL_geql2_dnrm2_ql(myTS);             /* Do my share of new norm2. */

         zees = myRank;                        /* Init the test flags.    */
         pair = 1;                             /* Starting pair.          */
         while ( (zees & 1) == 0 &&            /* If I must wait on pair, */
            (myRank)+pair < ATL_NTHREADS)      /* ..and pair exists,      */
         {
            ptnr = myTS+pair;                  /* Point at my partner.    */

            if (ptnr->active == 1)             /* If ptnr was used,       */
            {
               while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.       */
                  dnrm2_combine_ql;
            }

            zees >>= 1;                        /* Shift a zero out.       */
            pair <<= 1;                        /* Double the pair idx.    */
         }

         myTS->dnrm2++;                        /* Signal I am done.       */

         /*************************************/
         /******  S Y N C   P O I N T  ********/
         /******  S Y N C   P O I N T  ********/
         /******  S Y N C   P O I N T  ********/
         /*************************************/
         while (zTS->dnrm2 < myTS->dnrm2);         /* Wait on zero to finish. */

         XNORM = (zTS->Scale)*sqrt(zTS->SSQ);      /* Compute the norm. */
         BETAp = ATL_lapy2((zTS->zDiag), XNORM);   /* Get sqrt(a^2+b^2) */
         BETA = BETAp;                             /* Assume diagonal < 0 ... */
         if ((zTS->zDiag) >= 0.) BETA = 0.-BETAp;  /* ..If >= 0, change sign. */
      }

      myTAUi = (BETA-(zTS->zDiag)) / BETA;         /* Compute TAU[i]. */
      if (myRank == 0) TAU[i] = myTAUi;            /* Set if I own TAU. */

      sc = ATL_rone/((zTS->zDiag)-BETA);           /* Find scaling factor. */
      if ( myTS->active == 1)
      {
         cblas_scal(mScale, sc, scaleA, 1);        /* Scale the vector. */
      }

      if (myRank == 0)                             /* If I own diagonal, */
      {
         AII = BETA;                               /* Set new A[i,i] element. */
         for (j=0; j<KNT; j++)                     /* Rescaling loop...       */
            AII *= ATL_laSAFMIN;                   /* ...Adjust it.           */
      }

   #else /* COMPLEX VERSION of LARFG, modeled on clarfg.f. */
      ALPHAR =  zTS->zDiag[0];                     /* Real portion. */
      ALPHAI =  zTS->zDiag[1];                     /* Imaginary portion. */

      BETAp = ATL_lapy3(ALPHAR, ALPHAI, XNORM);    /* sqrt(a^2+b^2,c^2) */
      BETA = BETAp;                                /* Assume ALPHAR < 0.*/
      if (ALPHAR >= 0.) BETA = -BETAp;             /* If >=0, Change sign. */

      KNT = 0;                                     /* Init power to zero. */
      if (BETAp < ATL_laSAFMIN)
      {
         RSAFMN[0] = ATL_rone/ATL_laSAFMIN;        /* Set a maximum. */
         RSAFMN[1] = ATL_rzero;                    /* ..for scaling. */

         /*---------------------------------------------------------------*/
         /* BETAp is the same for all cores, so this loop can be executed */
         /* independently. However, XNORM must be computed in concert.    */
         /* The new BETAp will be at most 1, at least SAFMIN.             */
         /*---------------------------------------------------------------*/
         while (BETAp < ATL_laSAFMIN)
         {
            KNT++;                                 /* increment power. */
            if ( myTS->active == 1)                /* If I am active, */
            {
               cblas_scal(mScale, RSAFMN,
                                    scaleA, 1);    /* Scale my share. */
            }

            BETA *= RSAFMN[0];
            BETAp *= RSAFMN[0];
            ALPHAR *= RSAFMN[0];
            ALPHAI *= RSAFMN[0];
         }

         /* i starts from N -1. */
         ATL_geql2_dnrm2_ql(myTS);             /* Do my share of new norm2. */

         zees = myRank;                        /* Init the test flags.      */
         pair = 1;                             /* Starting pair.            */
         while ( (zees & 1) == 0 &&            /* If I must wait on pair,   */
            (myRank)+pair < ATL_NTHREADS)      /* ..and pair exists,        */
         {
            ptnr = myTS+pair;                  /* Point at my partner.      */

            if (ptnr->active == 1)             /* If ptnr was used,         */
            {
               while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.         */
                  dnrm2_combine_ql;
            }

            zees >>= 1;                        /* Shift a zero out.         */
            pair <<= 1;                        /* Double the pair idx.      */
         }

         myTS->dnrm2++;                        /* Signal I am done.         */

         /*************************************/
         /******  S Y N C   P O I N T  ********/
         /******  S Y N C   P O I N T  ********/
         /******  S Y N C   P O I N T  ********/
         /*************************************/
         while (zTS->dnrm2 < myTS->dnrm2);         /* Wait on zero to finish.*/

         XNORM = (zTS->Scale)*sqrt(zTS->SSQ);      /* Compute the norm. */
         BETAp = ATL_lapy3(ALPHAR, ALPHAI, XNORM); /* sqrt(a^2+b^2,c^2) */
         BETA = BETAp;                             /* Assume ALPHAR < 0.*/
         if (ALPHAR >= 0.) BETA = 0.-BETAp;        /* If >=0, Change sign. */
      }

      myTAUi[0] = (BETA-ALPHAR) / BETA;      /* Compute real part. */
      myTAUi[1] = (0.-ALPHAI) / BETA;        /* Compute imag part. */

      ALPHADIV[0] = ALPHAR - BETA;           /* prepare for 1/(alpha-beta) */
      ALPHADIV[1] = ALPHAI;                  /* ...                        */
      ATL_ladiv(ONE, ALPHADIV, sc);          /* compute scaling factor.    */
      if (myTS->active == 1)                 /* If I have some of vector,  */
      {
         cblas_scal(mScale, sc, scaleA, 1);  /* ..scale it. */
      }

      if (myRank == 0)                       /* If I own TAU, */
      {
         TAU[(i SHIFT)] = myTAUi[0];
         #ifdef BUILD_RQ2
            TAU[(i SHIFT) +1] = -myTAUi[1];  /* RQ2 needs conjugate. */
         #else
            TAU[(i SHIFT) +1] = myTAUi[1];   /* otherwise use normal. */
         #endif
      }

      if (myRank == 0)                       /* If I own diagonal, */
      {
         for (j=0; j<KNT; j++) BETA *= ATL_laSAFMIN;  /* Rescale BETA. */
         AII[0] = BETA;                               /* save for later. */
         AII[1] = 0.0;
      }
   #endif /* COMPLEX version of larfg. */

/*----------------------------------------------------------------------------*/
/* Now we apply dlarf, if we are not on the last column. This is              */
/* a dgemv, followed by a dger, all presuming TAU is non-zero.                */
/* The DGEMV: Column major, transpose A.                                      */
/*                                                                            */
/* We must compute H(i)*C, where C is the trailing part of our panel          */
/* at A[i..M-1, (i+1)..N-1]. So, C is (M-i) x (N-i-1).                        */
/* H(i) = (I-TAU[i]* u * (transpose u)), by definition.                       */
/* Where 'u' = A[i..M-1, i]. So, u is (M-i) x 1. We compute H(i)*C            */
/* as C - TAU[i] * u * (transpose w), where w = (transpose C) * u             */
/* (so that (transpose w) = (transpose u) * C.)                               */
/* Thus, w is (N-i-1) x 1.                                                    */
/*                                                                            */
/* Now, (transpose C) * u is just a GEMV. It produces a vector of             */
/* (N-i-1) elements. Every core will produce its own copy, and they           */
/* must be added together.                                                    */
/*                                                                            */
/* The second part, C += -TAU[i] * u * (transpose w), is just a GER.          */
/* Each core can do its part independently. We are essentially                */
/* dividing on M, not N, so every core needs (transpose w).                   */
/*----------------------------------------------------------------------------*/

   #ifdef TREAL
      if (myTS->myK < (N-1) && myTAUi != 0.)/* If dlarf necessary,            */
   #else
      if (myTS->myK < (N-1) )               /* If dlarf necessary,            */
   #endif
      {
         newN = N- myTS->myK  -1;           /* Width of update array & w.     */

         MY_gemvT ( mUpdate, newN, ONE, myTS->A, LDA,
                     A, 1, ZERO, myTS->WORK, 1);

/*----------------------------------------------------------------------------*/
/*       Now combine with other threads.                                      */
/*----------------------------------------------------------------------------*/

         zees = myRank;                     /* Init the test flags.           */
         pair = 1;                          /* Starting pair.                 */
         while ( (zees & 1) == 0 &&         /* If I must wait on pair,        */
                  (myRank)+pair <
                  ATL_NTHREADS)             /* ..and pair exists,             */
         {
            ptnr = myTS+pair;               /* Point at my partner.           */
            if (ptnr->active == 1)          /* If partner was used,           */
            {
               while (ptnr->dgemv < idup);  /* Wait for it.                   */
               ATL_axpy(newN, ONE, ptnr->WORK, 1, myTS->WORK, 1);
            }

            zees >>= 1;                     /* Shift a zero out.              */
            pair <<= 1;                     /* Double the pair idx.           */
         }

         myTS->dgemv = idup;                /* Say I finished dgemv.          */

/**********************************                                           */
/******  S Y N C   P O I N T  *****                                           */
/******  S Y N C   P O I N T  *****                                           */
/******  S Y N C   P O I N T  *****                                           */
/**********************************                                           */
/*----------------------------------------------------------------------------*/
/*       We can't start GER until all of                                      */
/*       'w' is finished. Wait for zero.                                      */
/*----------------------------------------------------------------------------*/
         while (zTS->dgemv < idup);         /* Wait for zero to build 'w'.    */

/*----------------------------------------------------------------------------*/
/*       'w' now in WORK. Use for GER.                                        */
/*----------------------------------------------------------------------------*/
         #ifdef TREAL
            MY_ger(
            mUpdate, newN, 0.-myTAUi,
            A, 1, zTS->WORK, 1,
            myTS->A, LDA);
         #else
            negTAUi[0]= 0.0 - myTAUi[0];
            negTAUi[1]= 0.0 + myTAUi[1];    /* conjugate for complex          */

            MY_ger(
            mUpdate, newN, negTAUi,
            A, 1, zTS->WORK, 1,
            myTS->A, LDA);
         #endif

         #ifdef TREAL
            if (myRank == 0) *(A + M -1 - idup) = AII;
                                            /* Core 0, restore diag now. */
         #else
            if (myRank == 0)
            {
               *(A + ((M -1 - idup) SHIFT) ) = AII[0];
                                            /* Core 0, restore diag now. */
               *(A + ((M -1 - idup) SHIFT)  + 1) = AII[1];
                                            /* Core 0, restore diag now. */
            }
         #endif
/*----------------------------------------------------------------------------*/
/*       Once we finish it is safe for us to start our                        */
/*       next column and dnrm2 on our share. We  will                         */
/*       sync up with other threads to complete that.                         */
/*----------------------------------------------------------------------------*/
      }
      else                        /* END IF we need to apply dlarf.           */
      {
         #ifdef TREAL
            if (myRank == 0) *(A + M -1 - idup) = AII;
                                                 /* Don't forget to set diag. */
         #else
            if (myRank == 0)
            {
               *(A + ((M -1 - idup) SHIFT) ) = AII[0];
                                                 /* Core 0, restore diag now. */
               *(A + ((M -1 - idup) SHIFT) +1  ) = AII[1];
                                                 /* Core 0, restore diag now. */
            }
         #endif
      }

      /*
       * for computing T,  for RQF replace myTAUi with correct TAU for the
       * complex part.
       */

      #ifdef TCPLX
          #ifdef BUILD_RQ2
                myTAUi[1] = 0.0 -  myTAUi[1];
          #endif
      #endif

      #ifdef TREAL
      if (myTS->buildT && idup == 0)        /* Simple store will work.        */
         *(T +i*LDT + i)  = myTAUi;         /* Just store it. NOte later      */
      #else
      if (myTS->buildT && idup == 0)        /* Simple store will work.        */
      {
         *(T +(i*(LDT SHIFT))+ (i SHIFT))  = myTAUi[0];
                                            /* Just store it. NOte later      */
         *(T +(i*(LDT SHIFT)) + (i SHIFT) + 1)  = myTAUi[1];
      }
      #endif
/*    change to min m, n                                                      */
      if (myTS->buildT && idup > 0)         /* If I must work for T,          */
      {

/*----------------------------------------------------------------------------*/
/*       Building T is very similar to DLARF, except we use the other         */
/*       side of A. Here is the picture:                                      */
/*                                                                            */
/*       B R R R R R R R R      Q = finished part of column                   */
/*       Q B R R R R R R R      R = upper triangular part for column          */
/*       Q Q B R R R R R R      B = Betas (norm 2) stored on diagonal,        */
/*       Q Q Q 1 C C C C C      1 = forced unit (normally assumed)            */
/*       Q Q Q u C C C C C      u = vector that H(i) is computed by,          */
/*       Q Q Q u C C C C C      C = portion of panel to be updated.           */
/*       .     .     .                                                        */
/*       .     .     .                                                        */
/*       .     .     .                                                        */
/*       Q Q Q u C C C C C                                                    */
/*                                                                            */
/*       We must compute Q^T times u, so each thread does its part, and       */
/*       then we add them together. We don't know at this point if all        */
/*       threads have completed their dger, so we can't use WORK. But         */
/*       we can use WORK+N-i, because it is not in use at this time. So       */
/*       we build the vector in zTS->WORK+N-i, it is 'i' elements long,       */
/*       and then zero will copy that into the column T[0,i].                 */
/*                                                                            */
/*       From there, we let thread 0 (alone) do a DTRMV to update that        */
/*       vector with the previous T, and then store TAU[i] at T[i,i].         */
/*                                                                            */
/*       This presumes that the DTRMV is too small to parallelize; but        */
/*       if that assumption is wrong the DTRMV could be parallelized as       */
/*       well, in future work.                                                */
/*                                                                            */
/*       We must sync to add up the DGEMV for T, not after that.              */
/*                                                                            */
/*       Rank 0: A points at A[i,i] mUpdate=M-i  A-i*LDA=A[i,0]               */
/*       Rank X: A points at A[0,i] mUpdate=M    A-i*LDA=A[0,0]               */
/*                                                                            */
/*----------------------------------------------------------------------------*/

         if (myRank == 0)                   /* Special case...                */
         {
            #ifdef TREAL
            AII = *(A + M -1 - idup );      /* Save diagonal element.         */
            *(A + M -1 - idup )  = 1.0;     /* Force to 1.                    */
            #else
            AII[0] = *(A + ((M -1 - idup) SHIFT) );  /* Save diagonal element.*/
            AII[1] = *(A + ((M -1 - idup ) SHIFT) +1);/* Save diagonal element*/
            *(A + ((M -1 - idup) SHIFT)) = 1.0;  /* Force to 1.               */
            *(A + ((M -1 - idup) SHIFT) + 1) = 0.0;   /* Force to 1.          */
            #endif
         }

         int os=(N+3)&(-4);                 /* Find even offset into work.    */


         #ifdef TREAL
         MY_gemvT(mUpdate, idup, 0.-myTAUi, A+LDA, LDA,
                  A, 1, 0.0, myTS->WORK+os, 1);
         #else
            #ifdef BUILD_RQ2
               negTAUi[0]= 0.0 + myTAUi[0];
               negTAUi[1]= 0.0 - myTAUi[1];
            #else
               negTAUi[0]= 0.0 - myTAUi[0];
               negTAUi[1]= 0.0 - myTAUi[1];
            #endif
            MY_gemvT(mUpdate, idup, negTAUi, A+(LDA SHIFT), LDA,
                     A, 1, ZERO, myTS->WORK+(os SHIFT), 1);
            #ifdef BUILD_RQ2
                for (i_loop = 0; i_loop < idup; i_loop++)
                {
                     (myTS->WORK+(os SHIFT))[(i_loop SHIFT)    ] =
                         -(myTS->WORK+(os SHIFT))[(i_loop SHIFT)    ];
                }
            #endif
         #endif

/*----------------------------------------------------------------------------*/
/* Now combine with other threads.                                            */
/*----------------------------------------------------------------------------*/
         zees = myRank;                     /* Init the test flags.           */
         pair = 1;                          /* Starting pair.                 */
         while ( (zees & 1) == 0 &&         /* If I must wait on pair,        */
                  (myRank)+pair <
                  ATL_NTHREADS)             /* ..and pair exists,             */
         {
            ptnr = myTS+pair;               /* Point at my partner.           */
            if (ptnr->active == 1)          /* If partner was used,           */
            {
               while (ptnr->dgemvt < idup); /* Wait for it.                   */
               ATL_axpy(idup, ONE, ptnr->WORK+(os SHIFT), 1,
                        myTS->WORK+(os SHIFT), 1);
            }

            zees >>= 1;                     /* Shift a zero out.              */
            pair <<= 1;                     /* Double the pair idx.           */
         }

         myTS->dgemvt = idup;               /* Post my completion.            */

/*----------------------------------------------------------------------------*/
/*       Done with dgemv part, rest is all for thread 0 to get done.          */
/*----------------------------------------------------------------------------*/
         if (myRank == 0)
         {
            TYPE *src=zTS->WORK+(os SHIFT); /* Source vector.                 */
            TYPE *dst= (T + (i*(LDT SHIFT)) +(i SHIFT) +(1 SHIFT));
                                                       /* Destination vector. */

            #ifdef TREAL
            *(A + M -1 - idup )  = AII;     /* Restore saved value.           */
            #else
            *(A + ((M -1 - idup) SHIFT) )  = AII[0];  /* Restore saved value. */
            *(A + ((M -1 - idup) SHIFT) +1 )  = AII[1]; /* Restore svd value. */
            #endif
            for (j=0; j<(idup SHIFT); j++)
            {
               *dst++ = *src++;             /* Copy value.                    */
            }

            cblas_trmv(CblasColMajor, CblasLower, CblasNoTrans, CblasNonUnit,
                        idup, (T + (i+1)*(LDT SHIFT)  + ((i+1) SHIFT) ),
                        LDT, T+i*(LDT SHIFT)+ ((i+1) SHIFT), 1);

            #ifdef TREAL
            *(T+i*LDT+i)=myTAUi;            /* Force TAU[i] on diagonal.      */
            #else
            *(T+i*(LDT SHIFT) +(i SHIFT))=myTAUi[0];
                                            /* Force TAU[i] on diagonal.      */
            *(T+i*(LDT SHIFT) +(i SHIFT) + 1)=myTAUi[1];
                                            /* Force TAU[i] on diagonal.      */
            #endif

         }                        /* ** END IF zero must update T. **         */

      }                                     /* ** END if building T **        */
   }                                        /* END FOR each column.           */

/*----------------------------------------------------------------------------*/
/* If we copied, this will copy back.                                         */
/*----------------------------------------------------------------------------*/
   if (myCopy)
   {
      ATL_geql2_UnCopy(myTS);               /* Do my copy back.               */
   }

   return(NULL);                            /* Implicit thread exit.          */
}                                           /* end *** ATL_geql2Worker **     */


/**************************************************************************** */
/********************** M A S T E R   C O N T R O L L E R ******************* */
/**************************************************************************** */

/*----------------------------------------------------------------------------*/
/* The break-up: We divide on M, simply enough, in multiples of 4. If there   */
/* are less than 32*ATL_NTHREADS elements, we will not divide at all.         */
/*                                                                            */
/* Processor 0 is our master combiner. The jobs are to compute the norm,      */
/* and then apply the reflector to the array.                                 */
/*                                                                            */
/* To compute the norm, threads need SAFMIN. We compute it before any thread  */
/* launches if it is not already computed.                                    */
/*                                                                            */
/*----------------------------------------------------------------------------*/
#ifdef BUILD_RQ2
#define ATL_tgexx2 Mjoin(PATL,tgerq2)
#else
#define ATL_tgexx2 Mjoin(PATL,tgeql2)
#endif

int ATL_tgexx2(int M, int N, TYPE *A, int LDA, TYPE *TAU, TYPE *WORK,
                 TYPE *ws_T, int LDT, TYPE *WORKM, int buildT, int myCopy)
{

   ATL_DGEQL2_t ts[ATL_NTHREADS];

   long long t0, t1;

   if (M < 0)
   {
      fprintf(stderr, "%s: M<0 (%i)\n", Mstr(ATL_tgexx2), M);
      return(-1);
   }

   if (N < 0)
   {
      fprintf(stderr, "%s: N<0 (%i)\n", Mstr(ATL_tgexx2), N);
      return(-2);
   }

   #ifdef BUILD_RQ2
   if (LDA < N)
   {
      fprintf(stderr, "%s: LDA<N (%i, %i)\n", Mstr(ATL_tgexx2), LDA, N);
      return(-4);
   }
   #else
   if (LDA < M)
   {
      fprintf(stderr, "%s: LDA<M (%i, %i)\n", Mstr(ATL_tgexx2), LDA, M);
      return(-4);
   }
   #endif

/*----------------------------------------------------------------------------*/
/* Preliminaries are done; now split up the problem.                          */
/* We use a data-owner split, each thread gets 1/p of the data,               */
/* and does all computation related to it.                                    */
/*----------------------------------------------------------------------------*/
   TYPE *myA = A, *allMem, *workMem;
   int i, j, k, b0, b, th;
   long unsigned int CPU;
   size_t mem[ATL_NTHREADS], totmem, workSize;

   th = ((M+N-1)/N);                        /* Max number of threads.         */
   if (th==0) th=1;                         /* Avoid divide by zero.          */
   if (th > ATL_NTHREADS) th=ATL_NTHREADS;  /* Limit on top.                  */

   b0 = (M/th);                             /* Find part for th zero.         */
   if (b0 < N) b0 = N;                      /* Take at least N.               */
   b = 0;
   if (th !=1)                              /* If multiple threads,           */
      b = ((M-b0) / (th-1) ) & (-4);        /* Split up the rest.             */
   b0 = M - (th-1)*b;                       /* Leftovers go to b0.            */
   if (b0 > b && b0 >= (N+(th-1)*4)) /* If b0 is biggest and can be smaller,  */
   {
      b += 4;                     /* Make the others slightly bigger.         */
      b0 -= (th-1)*4;             /* Core 0 has more overhead, do less.       */
   }

   if (b == 0) th = 1;
   if (th == 1 || (M < N) )       /* If impossible to split, use serial. */
   {

      #ifdef BUILD_RQ2
      ATL_gerq2(N, M, A, LDA, TAU, WORK);   /* Use serial version.            */
      if (buildT)
      {
         ATL_larft(LABackward, LARowStore, M, N, A, LDA, TAU, ws_T, LDT);
      }
      #else
      ATL_geql2(M, N, A, LDA, TAU, WORK);
      if (buildT)
      {
         ATL_larft(LABackward, LAColumnStore, M, N, A, LDA, TAU, ws_T, LDT);
      }
      #endif
      return(0);                            /* Exit after panel.              */
   }

/*----------------------------------------------------------------------------*/
/* Fill out the thread work areas.                                            */
/*----------------------------------------------------------------------------*/
   for (i=0; i<ATL_NTHREADS; i++)
   {
      ts[i].active=0;                       /* Nobody is active yet.          */
      mem[i] = 0;                           /* Nobody needs memory.           */
   }
/*
 * Note :
 *    For QL, the '0' denote the rows at the bottom. From bottom
 *    each row buld is allocated
 */

/* Modify myA  as   A + M - b0                                                */
   #ifdef BUILD_RQ2
   myA = A + ((M -b0) * (LDA SHIFT));
   #else
   myA = A + ((M -b0) SHIFT);
   #endif

   ts[0].fullM = M;                         /* Need full size M.              */
   ts[0].fullN = N;                         /* Need full size N.              */
   ts[0].myM = b0;                          /* Core 0 is special.             */
   ts[0].myN = N;                           /* Width is same for all.         */
   ts[0].myK = 0;                           /* First k for dnrm2.             */
   ts[0].lda = LDA;                         /* LDA is same for all.           */
   ts[0].rank = 0;                          /* Rank used by core 0.           */
   ts[0].A = myA;                           /* Core 0 gets bottom of A.       */
   ts[0].TAU = TAU;                         /* TAU is same for all.           */
   ts[0].dnrm2 = -1;                        /* Not done yet.                  */
   ts[0].dgemv = -1;                        /* Not done yet.                  */
   ts[0].active = 1;                        /* We are active.                 */
   ts[0].buildT = buildT;                   /* Pass in decision var.          */
   ts[0].T = ws_T;                          /* Pass in matrix addr.           */
   ts[0].LDT = LDT;                         /* And leading dimension.         */
   ts[0].dgemvt = -1;                       /* Not done yet.                  */
   ts[0].copy = myCopy;                     /* Whether worker should copy.    */
   #ifdef BUILD_RQ2
   myA -= (b SHIFT)*LDA;                    /* Point at next A, RQ.           */
   #else
   myA -= (b SHIFT);                        /* Point at next A, QL.           */
   #endif

   for (i=1; i < th; i++)
   {
      ts[i].fullM = b;                      /* Remember whole M.              */
      ts[i].fullN = N;                      /* Need full size N.              */
      ts[i].myM = b;                        /* 'b' entries for all.           */
      ts[i].myN = N;                        /* Width is same for all.         */
      ts[i].myK = 0;                        /* First k for dnrm2.             */
      ts[i].lda = LDA;                      /* LDA is same for all.           */
      ts[i].rank = i;                       /* Rank of process.               */
      ts[i].A = myA;                        /* Point at share of A.           */
      ts[i].TAU = TAU;                      /* TAU is same for all.           */
      ts[i].dnrm2 = -1;                     /* Not done yet.                  */
      ts[i].dgemv = -1;                     /* Not done yet.                  */
      ts[i].active = 1;                     /* Indicate active.               */
      ts[i].buildT = buildT;                /* Pass in decision var.          */
      ts[i].dgemvt = -1;                    /* Not done yet.                  */
      ts[i].copy = myCopy;                  /* Whether worker should copy.    */
      #ifdef BUILD_RQ2
      myA -= (b SHIFT)*LDA;                 /* Point at next A, RQ.           */
      #else
      myA -= (b SHIFT);                     /* Point at next A, QL.           */
      #endif
   }

/*----------------------------------------------------------------------------*/
/* Deal with memory.                                                          */
/*----------------------------------------------------------------------------*/
   if (myCopy)
   {
      totmem=MY_align;                          /* Needed for alignment.      */
      for (i=0; i<th; i++)
      {
         mem[i] = ATL_geql2_LC_Setup(ts+i);     /* Find necessary memory.     */
         totmem += mem[i];                      /* Add to total.              */
      }

      allMem = malloc(totmem);                  /* Allocate the memory.       */

      ts[0].A = (TYPE*) (((size_t) allMem+MY_align)&(-MY_align));
      for (i=1; i<th; i++)                      /* Each thread takes..        */
      {
         ts[i].A = (TYPE*) ((size_t) ts[i-1].A + mem[i-1]);/* ..next block.   */
      }
   }

   workSize = ((2 SHIFT)*(N+4)*sizeof(TYPE) +
                     MY_align-1)&(-MY_align);   /* aligned. */
   totmem = MY_align + workSize*ATL_NTHREADS;   /* Find mem to alloc.        */
   workMem = malloc(totmem);
   ts[0].WORK = (TYPE*) (((size_t) workMem + MY_align-1)&(-MY_align));

   for (i=1; i<th; i++)
      ts[i].WORK = (TYPE*) ((size_t) ts[i-1].WORK + workSize);

/*----------------------------------------------------------------------------*/
/* Call ATL_launcher to launch thread which runs in different CPUs   cores    */
/*----------------------------------------------------------------------------*/

   ATL_goparallel(th, ATL_geql2Worker, ts, NULL);


   if (myCopy) free(allMem);                /* release copied area.           */
   free(workMem);                           /* release work area.             */
   return(0);                               /* Done with dgeql2.              */
} /* END ATL_t_dgeql2 */
