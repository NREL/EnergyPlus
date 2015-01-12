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
/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *      SUBROUTINE DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
 *
 * Reference :
 *      Scaling LAPACK Panel Operations Using Parallel Cache Assignment
 *          Principles and Practice of Parallel Programming (PPoPP)' 10
 *          Jan 9-14, 2010, Bangalore, India
 *            by   Anthony M. Castaldo,  R.Clint Whaley
 *
 * ATL_tgeqr2.c :
 *
 * int ATL_geql2_t( const int M, const int N, TYPE *A, int LDA,
 *                             TYPE  *TAU, TYPE *WORK)
 *     NOTE :a)  ATL_tgeqr2.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *
 *           b) This routine will not validate the input parameters.
 *
 *
 *  Purpose
 *  =======
 *
 *  A = Q * R.
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
 *          On exit, the elements on and above the diagonal of the array
 *          contain the min(m,n) by n upper trapezoidal matrix R (R is
 *          upper triangular if m >= n); the elements below the diagonal,
 *          with the array TAU, represent the orthogonal matrix Q
 *          (unitary matrix incase of complex precision )  as a
 *          product of elementary reflectors (see Further Details).
 *
 *  LDA     (input) INTEGER
 *          The leading dimension of the array A.  LDA >= max(1,M).
 *
 *  TAU     (output) array, dimension (min(M,N))
 *          The scalar factors of the elementary reflectors (see Further
 *          Details).
 *
 *  WORK    (workspace)  array, dimension (N)
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
 *     Q = H(1) H(2) . . . H(k), where k = min(m,n).
 *                                             (for Real/Complex Precision)
 *  Each H(i) has the form
 *
 *     H(i) = I - tau * v * v'                 (for Real Precision)
 *     H(i) = I - tau * v * conjugate(v)'      (for Complex  Precision)
 *
 *  where tau is a real/complex  scalar, and v is a real/complex vector with
 *  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
 *  and tau in TAU(i).
 *
 *
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
 * Worker Thread assigned  as below : (siju check it is thread or core ? TODO)
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
 */


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
   #define  MY_trmv ATL_strmv               /* L2 tuned dgemv                 */
   #define  MY_gemvT  ATL_sgemvT_L2         /* L2 tuned dgemv                 */
   #define  MY_gemvN  ATL_sgemvN            /* L2 tuned dgemv                 */
   #define  ATL_axpy ATL_saxpy
   #define  MY_ger  ATL_sger_L2             /* L2 tuned dger                  */
   #define MY_gecopy ATL_sgecopy            /* Copy routine to use.           */
#endif
#ifdef  DREAL
   #define  MY_gemv_Tony  ATL_dgemv_L2      /* L2 tuned dgemv */
   #define  MY_trmv ATL_dtrmv               /* L2 tuned dgemv                 */
   #define  MY_gemvT  ATL_dgemvT_L2         /* L2 tuned dgemv                 */
   #define  MY_gemvN  ATL_dgemvN            /* L2 tuned dgemv                 */
   #define  ATL_axpy ATL_daxpy
   #define  MY_ger  ATL_dger_L2             /* L2 tuned dger                  */
   #define MY_gecopy ATL_dgecopy            /* Copy routine to use.           */
#endif
#ifdef  SCPLX
   #define  MY_trmv ATL_ctrmv               /* L2 tuned dgemv                 */
   #define  MY_gemvT  ATL_cgemvCT_L2        /* L2 tuned dgemv                 */
   #define  MY_gemvN  ATL_cgemvN            /* L2 tuned dgemv                 */
   #define  ATL_axpy ATL_caxpy
   #define  MY_gemv ATL_cgemv               /* L2 tuned dgemv                 */
   #define  MY_ger  ATL_cgerc_L2            /* L2 tuned dger                  */
   #define MY_gecopy ATL_cgecopy            /* Copy routine to use.           */
#endif
#ifdef  DCPLX
   #define  MY_trmv ATL_dtrmv               /* L2 tuned dgemv                 */
   #define  MY_gemvT  ATL_zgemvCT_L2        /* L2 tuned dgemv                 */
   #define  MY_gemvN  ATL_zgemvN            /* L2 tuned dgemv                 */
   #define  ATL_axpy ATL_zaxpy
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
#define ATL_geqr2_dnrm2 Mjoin(PATL,geqr2_dnrm2)
#define ATL_geqr2_LC_Setup Mjoin(PATL,geqr2_LC_Setup)
#define ATL_geqr2_DoCopy Mjoin(PATL,geqr2_DoCopy)
#define ATL_geqr2_UnCopy Mjoin(PATL,geqr2_UnCopy)
#define ATL_geqr2_Cache Mjoin(PATL,geqr2_Cache)
#define ATL_geqr2Worker Mjoin(PATL,geqr2Worker)
#define ATL_geqr2Worker_Zero Mjoin(PATL,geqr2Worker_Zero)
#define ATL_tgeqr2 Mjoin(PATL,tgeqr2)
#define ATL_geqr2_Order Mjoin(PATL,geqr2_Order)

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


/*----------------------------------------------------------------------------*/
/* Align the code.                                                            */
/*----------------------------------------------------------------------------*/
#define asmalign                    \
   __asm__ __volatile__ (           \
      ".align 4096;\n\t"            \
      : : : "%eax" )

//int ATL_geqr2_Order[ATL_NTHREADS]={0,4,2,6,1,5,3,7};
//int ATL_geqr2_Order[ATL_NTHREADS]={0,2,1,3};

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
} ATL_DGEQR2_t;

/* This code is used three times below, macro to prevent bug propagation.     */
#define dnrm2_combine \
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
   }                                        /* END #define dnrm2_combine      */


//double dgeqr2_SAFMIN=-1.;

/*-----------------------------------------------------------------------------
 * ATL_dgeqr2_dnrm2: Compute a dnrm2, but retain scale and value.
 *---------------------------------------------------------------------------*/

static void ATL_geqr2_dnrm2(ATL_DGEQR2_t *ts)
{
   int i,  N=ts->myM, N2;
   #ifdef TREAL
      N2 = N ;
   #else
      N2 = N << 1;
   #endif
   TYPE  aX, sX, SSQ=0., Scale=1.0;
   TYPE *X=ts->A+(((ts->lda) SHIFT)*(ts->myK));

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
      i = ((ts->myK) SHIFT) +(1 SHIFT);     /* Might as well shortcut.        */
      for (; i<N2; i++)                     /* 'i' already initialized.       */
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
//    fprintf(stderr, "scale %f ssq %f \n", Scale, SSQ );
}                                           /* END ATL_dgeqr2_dnrm2           */


/*
 * LC_Setup sets up copy.
 */
static size_t ATL_geqr2_LC_Setup(ATL_DGEQR2_t *myTS)
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
}                                           /* ** END ATL_geqr2_LC_Setup **   */


/*
 * DoCopy works from LC_Setup.
 */
static void ATL_geqr2_DoCopy(ATL_DGEQR2_t *myTS)
{
   int N = myTS->fullN;                         /* width of array.            */
   int M = myTS->myM;                           /* height of array.           */

   #ifdef BUILD_LQ2
   Mjoin(PATL,gemoveT)(M, N, ONE, myTS->oldA,   /* First arg is source,       */
         myTS->oldLDA, myTS->A, myTS->lda);     /* This is destination.       */
   #else
   MY_gecopy(M, N, myTS->oldA, myTS->oldLDA,    /* First arg is source,       */
   myTS->A, myTS->lda);                         /* This is destination.       */
   #endif
}  /* END ATL_geqr2_DoCopy. */


/*
 * UnCopy is just a copy back, no free.
 */
static void ATL_geqr2_UnCopy(ATL_DGEQR2_t *myTS)
{
   int N = myTS->fullN;                         /* width of array.            */
   int M = myTS->myM;                           /* height of array.           */
   #ifdef BUILD_LQ2
   Mjoin(PATL,gemoveT)(N, M , ONE,myTS->A,
                       myTS->lda, myTS->oldA, myTS->oldLDA);
   #else
   MY_gecopy(M, N, myTS->A, myTS->lda,
   myTS->oldA, myTS->oldLDA);
   #endif
   myTS->lda = myTS->oldLDA;                    /* Restore the old one.       */
   myTS->A = myTS->oldA;                        /* ...                        */
}  /* END ATL_geqr2_UnCopy. */


/*
 * Cache will force a read every 64th byte (eighth double) in an array,
 * following LDA.
 */
static void ATL_geqr2_Cache(ATL_DGEQR2_t *myTS)
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
}                                           /* ** END ATL_dgeqr2_Cache **     */


/*
 * Callback for thread launcher
 */
static int Mjoin(PATL,StructIsInitGEQR2)(void *vp)
{
   //return 1;
   return(((ATL_DGEQR2_t*)vp)->active);
}


/*
 * ATL_geqr2Worker: Persistent for the duration of the DGEQR2 operation.
 * Argument is pointer to a structure which contains our data section.
 */
//void* ATL_geqr2Worker(void *myArg)
static void* ATL_geqr2Worker(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp = vp;
   ATL_DGEQR2_t *myTS = ((ATL_DGEQR2_t*) lp->opstruct)+tp->rank;
   ATL_DGEQR2_t *zTS = myTS - (myTS->rank);     /* Point at zero ts.         */
   int   zees, pair, myRank;                    /* Log2 looping variables.   */
   int   i,j,M,N,LDA,mScale,mUpdate,newN,KNT,LDT;
   int i_loop;
   int   myCopy = myTS->copy;
   volatile ATL_DGEQR2_t *ptnr;                 /* partner log2 combine.     */
   TYPE  *A, *scaleA,  *T;                      /* Work variables.           */
   TYPE XNORM, BETA, BETAp;
   TYPE *TAU = myTS->TAU;                       /* Short cut.                */
   TYPE ALPHA;
   TYPE w;                                      /* Used in dnrm2_combine mac.*/
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
/* Now we begin the real dgeqr2.                                              */
/*----------------------------------------------------------------------------*/
   N = myTS->fullN;                         /* panel width.                   */

   if (myCopy)
   {
      ATL_geqr2_DoCopy(myTS);                  /* Execute it.                 */
   }

   LDA = myTS->lda;                         /* Load AFTER local copy.         */
   M = myTS->myM;                           /* Shortcut to M.                 */

   for (i=0; i<N; i++)                      /* Now, for each column,          */
   {
      if (myRank == 0)                      /* Zero follows diagonal.         */
      {
         mScale = M-i-1;                    /* Special scaling value.         */
         mUpdate = M-i;                     /* Special gemv/ger size.         */
         A = myTS->A + (i SHIFT)+ (i* (LDA SHIFT));  /* Special pointer to A. */
         #ifdef TREAL
            myTS->zDiag = (*A);             /* Get the diagonal.              */
	         *A = 1.;
         #else
            myTS->zDiag[0] = (*A);
            myTS->zDiag[1] = *(A+1);
/*          Replace with one now.                                             */
            *(A) = 1.0;
            *(A+1) = 0.0;
         #endif
         scaleA = A+(1 SHIFT);              /* What to scale.                 */
      } else                                /* Others keep square.            */
      {
         mScale = M;                        /* Always scale full col.         */
         mUpdate = M;                       /* Always gemv/ger full co.       */
         A = myTS->A + (i*(LDA SHIFT));     /* Point at column.               */
         scaleA = A;                        /* What to scale.                 */
      }

      myTS->myK = i;                        /* Set my K value.                */

      ATL_geqr2_dnrm2(myTS);

      zees = myRank;                        /* Init the test flags.           */
      pair = 1;                             /* Starting pair.                 */
      while ( (zees & 1) == 0 &&            /* If I must wait on pair,        */
         (myRank)+pair < ATL_NTHREADS)      /* ..and pair exists,             */
      {
         ptnr = myTS+pair;                  /* Point at my partner.           */

         if (ptnr->active == 1)             /* If ptnr was used,              */
         {
            while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.              */
               dnrm2_combine;
         }

         zees >>= 1;                        /* Shift a zero out.              */
         pair <<= 1;                        /* Double the pair idx.           */
      }

      myTS->dnrm2++;                        /* Signal I am done.              */

      /*************************************/
      /******  S Y N C   P O I N T  ********/
      /******  S Y N C   P O I N T  ********/
      /******  S Y N C   P O I N T  ********/
      /*************************************/

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

         if (myRank == 0)                   /* Restore A[i,i] we replaced w 1.*/
         {
            #ifdef TREAL
            *(A) =  zTS->zDiag;
            TAU[i] = 0.;                    /* clear TAU so H[i]=Identity.    */
            #else
            *(A )  = zTS->zDiag[0];
            *(A+1) = zTS->zDiag[1];
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

         ATL_geqr2_dnrm2(myTS);                /* Do my share of new norm2. */

         zees = myRank;                        /* Init the test flags.    */
         pair = 1;                             /* Starting pair.          */
         while ( (zees & 1) == 0 &&            /* If I must wait on pair, */
            (myRank)+pair < ATL_NTHREADS)      /* ..and pair exists,      */
         {
            ptnr = myTS+pair;                  /* Point at my partner.    */

            if (ptnr->active == 1)             /* If ptnr was used,       */
            {
               while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.       */
                  dnrm2_combine;
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

         ATL_geqr2_dnrm2(myTS);                /* Do my share of new norm2. */

         zees = myRank;                        /* Init the test flags.      */
         pair = 1;                             /* Starting pair.            */
         while ( (zees & 1) == 0 &&            /* If I must wait on pair,   */
            (myRank)+pair < ATL_NTHREADS)      /* ..and pair exists,        */
         {
            ptnr = myTS+pair;                  /* Point at my partner.      */

            if (ptnr->active == 1)             /* If ptnr was used,         */
            {
               while (ptnr->dnrm2 <= myTS->dnrm2);  /* Wait for it.         */
                  dnrm2_combine;
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
         #ifdef BUILD_LQ2
            TAU[(i SHIFT) +1] = -myTAUi[1];  /* LQ2 needs conjugate. */
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
/*   Now we apply dlarf, if we are not on the last column. This is            */
/*   a dgemv, followed by a dger, all presuming TAU is non-zero.              */
/*   The DGEMV: Column major, transpose A.                                    */
/*                                                                            */
/*   We must compute H(i)*C, where C is the trailing part of our panel        */
/*   at A[i..M-1, (i+1)..N-1]. So, C is (M-i) x (N-i-1).                      */
/*   H(i) = (I-TAU[i]* u * (transpose u)), by definition.                     */
/*   Where 'u' = A[i..M-1, i]. So, u is (M-i) x 1. We compute H(i)*C          */
/*   as C - TAU[i] * u * (transpose w), where w = (transpose C) * u           */
/*   (so that (transpose w) = (transpose u) * C.)                             */
/*   Thus, w is (N-i-1) x 1.                                                  */
/*                                                                            */
/*   Now, (transpose C) * u is just a GEMV. It produces a vector of           */
/*   (N-i-1) elements. Every core will produce its own copy, and they         */
/*   must be added together. Pictorially, the pieces look like this:          */
/*                                                                            */
/*   B R R R R R R R R      Q = finished part of column                       */
/*   Q B R R R R R R R      R = upper triangular part for column              */
/*   Q Q B R R R R R R      B = Betas (norm 2) stored on diagonal,            */
/*   Q Q Q 1 C C C C C      1 = forced unit (normally assumed)                */
/*   Q Q Q u C C C C C      u = vector that H(i) is computed by,              */
/*   Q Q Q u C C C C C      C = portion of panel to be updated.               */
/*   .     .     .                                                            */
/*   .     .     .          The '1' will later be replaced by BETA for        */
/*   .     .     .          the column 'u'.                                   */
/*   Q Q Q u C C C C C                                                        */
/*                                                                            */
/*   The second part, C += -TAU[i] * u * (transpose w), is just a GER.        */
/*   Each core can do its part independently. We are essentially              */
/*   dividing on M, not N, so every core needs (transpose w).                 */
/*                                                                            */
/*----------------------------------------------------------------------------*/

   #ifdef TREAL
      if (i < (N-1) && myTAUi != 0.)        /* If dlarf necessary,            */
   #else
      if (i < (N-1) )                       /* If dlarf necessary,            */
   #endif
      {
         newN = N-i-1;                      /* Width of update array & vect w.*/
         MY_gemvT(mUpdate, newN,
                  ONE, A+(LDA SHIFT), LDA, A, 1, ZERO, myTS->WORK, 1);

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
               while (ptnr->dgemv < i);     /* Wait for it.                   */
               ATL_axpy(newN, ONE, ptnr->WORK, 1, myTS->WORK, 1);
            }

            zees >>= 1;                     /* Shift a zero out.              */
            pair <<= 1;                     /* Double the pair idx.           */
         }

         myTS->dgemv = i;                   /* Say I finished dgemv.          */

         /***********************************/
         /******  S Y N C   P O I N T  ******/
         /******  S Y N C   P O I N T  ******/
         /******  S Y N C   P O I N T  ******/
         /***********************************/
         /*---------------------------------*/
         /* We can't start GER until all of */
         /* 'w' is finished. Wait for zero. */
         /*---------------------------------*/
         while (zTS->dgemv < i);            /* Wait for zero to build 'w'.    */

         /*---------------------------------*/
         /* 'w' now in WORK. Use for GER.   */
         /*---------------------------------*/
         #ifdef TREAL
         MY_ger(mUpdate, newN, 0.-myTAUi, A, 1, zTS->WORK, 1,
                A+(LDA SHIFT), LDA);
         #else
         negTAUi[0]= 0.0 - myTAUi[0];
         negTAUi[1]= 0.0 + myTAUi[1];       /* conjugate for complex          */

         MY_ger(mUpdate, newN, negTAUi, A, 1, zTS->WORK, 1,
                A+(LDA SHIFT), LDA);
         #endif

         /*-----------------------------------------------*/
         /* Once we finish it is safe for us to start our */
         /* next column and dnrm2 on our share. We  will  */
         /* sync up with other threads to complete that.  */
         /*-----------------------------------------------*/
      } /* END IF we needed to apply dlarf. */

      #ifdef TREAL
         if (myRank == 0) *A = AII;      /* Core 0, restore diag now.      */
      #else
         if (myRank == 0)
         {
            *(A ) = AII[0];              /* Core 0, restore diag now.      */
            *(A + 1) = AII[1];           /* Core 0, restore diag now.      */
         }
      #endif

      /*
       * for computing T,  for LQ replace myTAUi with correct TAU for the
       * complex part.
       */

      #ifdef TCPLX
          #ifdef BUILD_LQ2
                myTAUi[1] = 0.0 -  myTAUi[1];
          #endif
      #endif

      #ifdef TREAL                          /* TODO change later              */
      if (myTS->buildT && i == 0)           /* Simple store will work.        */
         *T = myTAUi;                       /* Just store it.                 */
      #else
      if(myTS->buildT && i == 0)            /* Simple store will work.        */
      {
         *(T) = myTAUi[0];
         *(T+1) = myTAUi[1];
      }
      #endif

      if (myTS->buildT && i > 0)            /* If I must work for T,          */
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
/*----------------------------------------------------------------------------*/

         if (myRank == 0)                   /* Special case...                */
         {
            #ifdef TREAL
            AII = *A;                       /* Save diagonal element.         */
            *A = 1.0;                       /* Force to 1.                    */
            #else
            AII[0] = *(A) ;
            AII[1] = *(A+1);
            *(A ) = 1.0;                    /* Force to 1.                    */
            *(A + 1) = 0.0;                 /* Force to 1.                    */
            #endif
         }

         int os=(N+3)&(-4);                 /* Find even offset into work.    */

         #ifdef TREAL
         MY_gemvT(mUpdate, i, 0.-myTAUi,
                  A-i*LDA, LDA, A, 1, 0.0, myTS->WORK+os, 1);
         #else
              #ifdef BUILD_LQ2
                 negTAUi[0]= 0.0 + myTAUi[0];
                 negTAUi[1]= 0.0 - myTAUi[1]; /* conj for cplx Not required */
              #else
                 negTAUi[0]= 0.0 - myTAUi[0];
                 negTAUi[1]= 0.0 - myTAUi[1]; /* conj for cplx Not required */
              #endif
              MY_gemvT(mUpdate,  i, negTAUi, A-(i*(LDA SHIFT)), LDA,
                       A, 1, ZERO, myTS->WORK+(os SHIFT), 1);

          #ifdef BUILD_LQ2
		          for (i_loop = 0; i_loop < i; i_loop++)
		          {
		              (myTS->WORK+(os SHIFT))[(i_loop SHIFT) + 0] =
		                   0.0 - (myTS->WORK+(os SHIFT))[(i_loop SHIFT) + 0];
		          }
			  #endif
         #endif

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
               while (ptnr->dgemvt < i);    /* Wait for it.                   */
               ATL_axpy(i, ONE, ptnr->WORK+(os SHIFT), 1,
                        myTS->WORK+(os SHIFT), 1);
            }

            zees >>= 1;                     /* Shift a zero out.              */
            pair <<= 1;                     /* Double the pair idx.           */
         }

         myTS->dgemvt = i;                  /* Post my completion.            */

/*----------------------------------------------------------------------------*/
/*       Done with dgemv part, rest is                                        */
/*       all for thread 0 to get done.                                        */
/*----------------------------------------------------------------------------*/
         if (myRank == 0)
         {
            TYPE *src=zTS->WORK+(os SHIFT); /* Source vector.                 */
            TYPE *dst=T+(i*(LDT SHIFT));    /* Destination vector.            */
            #ifdef TREAL
            *A = AII;                       /* Restore saved value.           */
            #else
            *(A) = AII[0];                  /* Restore saved value.           */
            *(A +1) = AII[1];               /* Restore saved value.           */
            #endif
            for (j=0; j<(i SHIFT); j++)
            {
               *dst++ = *src++;             /* Copy value.                    */
            }

            cblas_trmv(CblasColMajor, CblasUpper, CblasNoTrans, CblasNonUnit,
                        i, T, LDT, T+(i*(LDT SHIFT)), 1);

            /* Force TAU[i] on diagonal. */
            #ifdef TREAL
	         *(T+i+i*LDT)=myTAUi;
            #else
            *(T+(i SHIFT)+(i*(LDT SHIFT))    ) = myTAUi[0];
            *(T+(i SHIFT)+(i*(LDT SHIFT)) + 1) = myTAUi[1];
            #endif

         } /* END IF zero must update T. */
      } /* END IF building T. */
   } /* END FOR each column. */


/*----------------------------------------------------------------------------*/
/* If we copied, this will copy back.                                         */
/*----------------------------------------------------------------------------*/
   if (myCopy)
   {
      ATL_geqr2_UnCopy(myTS);               /* Do my copy back.               */
   }

   return(NULL);                            /* Implicit thread exit.          */
} /* END ATL_geqr2Worker() */


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
#ifdef BUILD_LQ2
#define ATL_tgexx2 Mjoin(PATL,tgelq2)
#else
#define ATL_tgexx2 Mjoin(PATL,tgeqr2)
#endif

int ATL_tgexx2(int M, int N, TYPE *A, int LDA, TYPE *TAU, TYPE *WORK,
                 TYPE *ws_T, int LDT, TYPE *WORKM, int buildT, int myCopy)
{

   ATL_DGEQR2_t ts[ATL_NTHREADS];
   long long t0, t1;

// fprintf (stderr,"______in %s M=%d  N=%d \n", Mstr(ATL_tgexx2), M,N);
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

   #ifdef BUILD_LQ2
   if (LDA < N) /*  N is the original M */
   {
      fprintf(stderr, "%s: LDA<M (%i, %i)\n", Mstr(ATL_tgexx2), LDA, M);
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
   TYPE *myA = A, *myOldA = A, *allMem, *workMem;
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
   if (th == 1 || (N> M))         /* If impossible to split, use serial. */
   {

      #ifdef BUILD_LQ2
      ATL_gelq2(N, M, A, LDA, TAU, WORK);   /* Use serial version.            */
      if (buildT)
      {
         ATL_larft(LAForward, LARowStore, M, N, A, LDA,  TAU, ws_T, LDT);
      }
      #else
      ATL_geqr2(M, N, A, LDA, TAU, WORK);
      if (buildT)
      {
         ATL_larft(LAForward, LAColumnStore, M, N, A, LDA, TAU, ws_T, LDT);
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

   ts[0].fullM = M;                         /* Need full size M.              */
   ts[0].fullN = N;                         /* Need full size N.              */
   ts[0].myM = b0;                          /* Core 0 is special.             */
   ts[0].myN = N;                           /* Width is same for all.         */
   ts[0].myK = 0;                           /* First k for dnrm2.             */
   ts[0].lda = LDA;                         /* LDA is same for all.           */
   ts[0].rank = 0;                          /* Rank used by core 0.           */
   ts[0].A = myA;                           /* Core 0 gets top of A.          */
   ts[0].TAU = TAU;                         /* TAU is same for all.           */
   ts[0].dnrm2 = -1;                        /* Not done yet.                  */
   ts[0].dgemv = -1;                        /* Not done yet.                  */
   ts[0].active = 1;                        /* We are active.                 */
   ts[0].buildT = buildT;                   /* Pass in decision var.          */
   ts[0].T = ws_T;                          /* Pass in matrix addr.           */
   ts[0].LDT = LDT;                         /* And leading dimension.         */
   ts[0].dgemvt = -1;                       /* Not done yet.                  */
   ts[0].copy   = myCopy;                   /* Whether PCA should copy.       */
   #ifdef BUILD_LQ2
   myA += (b0 SHIFT)*LDA;                   /* Point at next A, LQ.           */
   #else
   myA += (b0 SHIFT);                       /* Point at next A, QR.           */
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
      ts[i].copy   = myCopy;                /* Whether PCA should copy.       */
      #ifdef BUILD_LQ2
      myA += (b SHIFT)*LDA;                 /* Point at next share, LQ.       */
      #else
      myA += (b SHIFT);                     /* Point at next share, QR.       */
      #endif
   }

/*----------------------------------------------------------------------------*/
/* Deal with memory.                                                          */
/*----------------------------------------------------------------------------*/
   if (myCopy)
   {
      totmem=MY_align;                         /* Needed for alignment.       */
      for (i=0; i<th; i++)
      {
         mem[i] = ATL_geqr2_LC_Setup(ts+i);    /* Find necessary memory.      */
         totmem += mem[i];                     /* Add to total.               */
      }

      allMem = malloc(totmem);                 /* Allocate the memory.        */

      ts[0].A = (TYPE*) (((size_t) allMem+MY_align)&(-MY_align));
      for (i=1; i<th; i++)                     /* Each thread takes..         */
      {
         ts[i].A = (TYPE*) ((size_t) ts[i-1].A + mem[i-1]);  /* ..next block. */
      }
   }

   workSize = ((2 SHIFT)*(N+4)*sizeof(TYPE) +
                     MY_align-1)&(-MY_align);    /* aligned.                  */
   totmem = MY_align + workSize*ATL_NTHREADS;    /* Find mem to alloc.        */
   workMem = malloc(totmem);
   ts[0].WORK = (TYPE*) (((size_t) workMem + MY_align-1)&(-MY_align));
   for (i=1; i<th; i++)
      ts[i].WORK = (TYPE*) ((size_t) ts[i-1].WORK + workSize);

/*----------------------------------------------------------------------------*/
/* Call ATL_launcher to launch thread which runs in different CPUs   cores    */
/*----------------------------------------------------------------------------*/

   ATL_goparallel(th, ATL_geqr2Worker, ts, NULL);


   #if defined(local_copy)
   free(allMem);                            /* release copied area.           */
   #endif                                   /* defined(local_copy)            */
   free(workMem);                           /* release work area.             */
   return(0);                               /* Done with dgeqr2.              */
}  /* END ATL_t_dgeqr2. */

