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
 * This is a recursive C implementation of the standard LAPACK Fortran routine:
 *      SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
 *
 * ATL_larft.c :
 * void ATL_larft(const enum ATL_LADIRECT DIRECT,
 *                const enum ATL_LASTOREV STOREV, int N, int K, TYPE *V,
 *                int LDV, TYPE  *TAU, TYPE *T, int LDT )
 *
 *     NOTE :   ATL_larft.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *
 *  Purpose
 *  =======
 *
 *  ATL_larft forms the triangular factor T of a real/complex block reflector H
 *  of order n, which is defined as a product of k elementary reflectors.
 *  We only handle DIRECT='F' (forward) and STOREV='C' (columnwise).
 *
 *  N       (input) INTEGER
 *          The order of the block reflector H. N >= 0.
 *
 *  K       (input) INTEGER
 *          The order of the triangular factor T (= the number of
 *          elementary reflectors). K >= 1.
 *
 *  V       (input/output) DOUBLE PRECISION array, dimension (LDV, K)
 *
 *  LDV     (input) INTEGER
 *          The leading dimension of the array V.
 *          LDV >= max(1,N).
 *
 *  TAU     (input)  array, dimension (K)
 *          TAU(i) must contain the scalar factor of the elementary
 *          reflector H(i).
 *
 *  T       (output) array, dimension (LDT,K)
 *          The k by k upper triangular factor T of the block reflector.
 *
 *  LDT     (input) INTEGER
 *          The leading dimension of the array T. LDT >= K.
 *
 *  Further Details
 *  ===============
 *
 *  The shape of the matrix V and the storage of the vectors which define
 *  the H(i) is best illustrated by the following example with n = 5 and
 *  k = 3. The elements equal to 1 are not stored.
 *
 *  DIRECT = 'F' and STOREV = 'C':
 *
 *               V = (  1       )
 *                   ( v1  1    )
 *                   ( v1 v2  1 )
 *                   ( v1 v2 v3 )
 *                   ( v1 v2 v3 )
 *
 *  =====================================================================
 *  We are using the recursive formulation outlined in the paper
 *  "Applying recursion to serial and parallel QR factorization leads to
 *   better performance", by E. Elmroth and F.G. Gustavson, 2000 (IBM).
 *
 *  The YT(Y^T) formulation is by Schreiber and Van Loan, 1989; but is
 *  modified in two ways. LAPACK uses Q=(I-YT(Y^T)) instead of Q=(I+YT(Y^T)),
 *  and also does not use unit vectors: As you can see above, our V vectors
 *  begin with '1' and thus cannot have unit norm, and working out the
 *  necessary T and TAU produces TAU[i] on the diagonal of T. If we stop our
 *  recursion at 1, TAU[0] is the result. If we stop at 2, we have a 2x2
 *  upper triangular matrix; with TAU[0] and TAU[1] on the diagonal, and
 *  -TAU[0]*TAU[1]* dotproduct(V[0], V[1]) in the upper right hand corner.
 *
 *  The Elmroth+Gustavson algorithm permits updates with different sizes,
 *  we make our split to try and keep the update area a multiple of 4 when
 *  possible, we presume this will aid SSE usage in multiplication.
 *
 */
#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_lapack.h"

#define ATL_larftFC           Mjoin(PATL,larftFC)
#define ATL_larftBC           Mjoin(PATL,larftBC)
#define ATL_larftFR           Mjoin(PATL,larftFR)
#define ATL_larftBR           Mjoin(PATL,larftBR)
#define ATL_larft_blockFC     Mjoin(PATL,larft_blockFC)
#define ATL_larft_blockBC     Mjoin(PATL,larft_blockBC)
#define ATL_larft_blockFR     Mjoin(PATL,larft_blockFR)
#define ATL_larft_blockBR     Mjoin(PATL,larft_blockBR)

void ATL_larftBR(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT);
void ATL_larftFR(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT);
void ATL_larftBC(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT);
void ATL_larftFC(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT);

void ATL_larft_blockFC(int N, int K, int left, int right,
                      TYPE *V, int LDV, TYPE *T, int LDT );
void ATL_larft_blockBC(int N, int K, int left, int right,
                      TYPE *V, int LDV, TYPE *T, int LDT );
void ATL_larft_blockFR(int N, int K, int left, int right,
                      TYPE *V, int LDV, TYPE *T, int LDT );
void ATL_larft_blockBR(int N, int K, int left, int right,
                      TYPE *V, int LDV, TYPE *T, int LDT );

/* Use CBLAS routines.                                                        */

   #ifdef TREAL
      #define MY_DOT  cblas_dot
   #else
      #define MY_DOT  cblas_dotc_sub
      #define MY_DOTU cblas_dotu_sub
   #endif

   #define MY_LEFT  CblasLeft
   #define MY_RIGHT CblasRight
   #define MY_LOWER CblasLower
   #define MY_UPPER CblasUpper
   #ifdef TREAL
      #define MY_TRANS CblasTrans
   #else
      #define MY_TRANS CblasConjTrans
   #endif

   #define MY_NOTRANS CblasNoTrans
   #define MY_UNIT    CblasUnit
   #define MY_NONUNIT CblasNonUnit

void ATL_larft_block
   (const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
    ATL_CINT N, ATL_CINT K, ATL_CINT left, ATL_CINT right,
    TYPE *V, ATL_CINT LDV, TYPE *T, ATL_CINT LDT)
{
   if ( DIRECT == LAForward)
   {
      if ( STOREV == LAColumnStore )
      {
         ATL_larft_blockFC(N, K, left, right, V, LDV, T,  LDT );
      }
      else if (STOREV == LARowStore)
      {
         ATL_larft_blockFR(N, K, left, right, V, LDV, T,  LDT );
      }

   }
   else if (DIRECT == LABackward)
   {
      if ( STOREV == LAColumnStore)
      {
         ATL_larft_blockBC(N, K, left, right, V, LDV, T, LDT );
      }
      else if (STOREV == LARowStore)
      {
         ATL_larft_blockBR(N, K, left, right, V, LDV, T, LDT );
      }
   }
}

/*
 * This function produce the block portion of the T matrix in recursive
 * algorithm.
 */

void ATL_larft_blockFC(ATL_CINT N, ATL_CINT K, ATL_CINT left, ATL_CINT right,
                       TYPE *V, int LDV, TYPE *T, ATL_CINT LDT)
{
   TYPE *Tw, *Y2, *T2;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* For complex LDV * 2            */
   LDT2 = LDT SHIFT;                        /* For complex LDT * 2            */

    #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      const TYPE NONE = ATL_rnone;
    #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      const TYPE NONE[2] = {ATL_rnone, ATL_rzero};
    #endif

   Tw = T+left*LDT2;                        /* Work at T[0, left].            */
   Y2 = V+(left SHIFT)+LDV2*left;           /* Y2 at V[left, left].           */
   T2 = T+(left SHIFT) +LDT2*left;          /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Fill in the upper right hand block, which is                               */
/* 'left' rows and 'right' columns.                                           */
/*                                                                            */
/* -T1 * (Y1^T * Y2) * T2                                                     */
/* Block upper right hand element is T+(LDT*left).                            */
/*----------------------------------------------------------------------------*/
/* Step 1: Y1^T * Y2, Y2 upper unit triangular.                               */
/* We do this in two steps to use DTRMM.                                      */
/* We transpose by hand the first 'left' columns                              */
/* of Y1 into the workspace; then use DTRMM to                                */
/* multiply that on the right by the triangular                               */
/* part of Y2. Then we use DGEMM to compute                                   */
/* Workspace += Y1rest^T * Y2rest.                                            */
/*                                                                            */
/* Note Y1rest[0,0] is at V+K, and                                            */
/* Y2rest[0,0] is at Y2+right.                                                */
/*                                                                            */
/*----------------------------------------------------------------------------*/
   for (row=0; row<left; row++)             /* Each row of Tw                 */
   {
      for (col=0; col<right; col++)         /* Each column of Tw              */
      {
         #ifdef TREAL
            *(Tw + row + col*LDT) =         /* Work[row, col]                 */
            *((V+left)+col+row*LDV);        /* Y1[col, row].                  */
         #else
/*          Copy the  ConjTranspose for complex                               */
            *(Tw + (row SHIFT) + col*LDT2) =/* Work[row, col]                 */
            *((V+ (left SHIFT))+(col SHIFT)
                              +row*LDV2);   /* Y1[col, row].                  */
/*          copy the imaginary part.                                          */
            *(Tw + (row SHIFT) + 1 + col*LDT2) = /* Work[row, col]            */
            0.0 - ( *((V+ (left SHIFT))+(col SHIFT) +1 +
                              row*LDV2) );  /* Y1[col, row].                  */
         #endif
      }
   }

/*----------------------------------------------------------------------------*/
/* Copy complete, Tw is 'left' rows, 'right' cols.                            */
/* Tw = 1.0 * Tw * Y2.                                                        */
/* DTRMM: (right): B := alpha * B * triangular A.                             */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,                /* Y2 is on right, lwr tri.       */
             MY_RIGHT, MY_LOWER,            /* Y2 is on right, lwr tri.       */
             MY_NOTRANS, MY_UNIT,           /* As-is, with unit diag.         */
             left, right, ONE,              /* B rows, cols, alpha=1.         */
             Y2, LDV, Tw, LDT);             /* A and B.                       */
/*----------------------------------------------------------------------------*/
/* Dgemm can finish the multiply, if any is left.                             */
/* Tw += (Y1^T) * Y2, if N-K is > 0.                                          */
/* DGEMM: C = alpha * A * B + Beta * C.                                       */
/*----------------------------------------------------------------------------*/
   if (N > K)                               /* If any left below...           */
   {
     cblas_gemm(CblasColMajor,
                MY_TRANS,                   /* Y1 is transposed.              */
                MY_NOTRANS,                 /* Y2 is not.                     */
                left, right,                /* Dimensions of C.               */
                N-K,                        /* Height of columns.             */
                ONE,                        /* No special multiplier.         */
                (V+(K SHIFT)), LDV,         /* Y1[0,0], dist to Y1[0,1]       */
                (Y2+(right SHIFT) ), LDV,   /* Y2[0,0], dist to Y2[0,1]       */
                ONE,                        /* Add to existing data.          */
                Tw, LDT);                   /* Wk[0,0], dist to Wk[0,1]       */
   }

/*----------------------------------------------------------------------------*/
/* Y1^T * Y2 complete; now two triangular mults.                              */
/* Tw = -1.0 * T1 * Tw.                                                       */
/* DTRMM: (left): B:= alpha * triangular A * B                                */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
            MY_LEFT, MY_UPPER,              /* T1 on left, upr tri.           */
            MY_NOTRANS, MY_NONUNIT,         /* As-is, non-unit diag.          */
             left, right, NONE,             /* B rows, cols, alpha=-1.        */
             T, LDT, Tw, LDT);              /* A and B.                       */

/*----------------------------------------------------------------------------*/
/* -T1 * (Y1^T * Y2) complete. Final tri T2 Mult.                             */
/* Tw = 1.0 * Tw * T2.                                                        */
/* DTRMM: (right): B:= alpha * B * triangular A                               */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
           MY_RIGHT, MY_UPPER,              /* T2 on right, upr tri.          */
            MY_NOTRANS, MY_NONUNIT,         /* As-is, non-unit diag.          */
             left, right, ONE,              /* B rows, cols, alpha=1.         */
             T2, LDT, Tw, LDT);             /* A and B.                       */

   return;                                  /* All done.                      */
}                                           /* END ATL_dlarft_block           */

/*
 * This function produce the block portion of the T matrix in recursive
 * algorithm.
 */
void ATL_larft_blockFR(ATL_CINT N, ATL_CINT K, ATL_CINT top, ATL_CINT bottom,
                       TYPE *V, ATL_CINT LDV, TYPE *T, ATL_CINT LDT)
{
   TYPE *Tw, *Y2, *T2;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* For complex LDV * 2            */
   LDT2 = LDT SHIFT;                        /* For complex LDT * 2            */

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      const TYPE NONE = ATL_rnone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      const TYPE NONE[2] = {ATL_rnone, ATL_rzero};
   #endif
   Tw = T+top*LDT2;                         /* Work at T[0, top].             */
   Y2 = V+(top SHIFT)+LDV2*top;             /* Y2 at V[top, top].             */
   T2 = T+(top SHIFT) +LDT2*top;            /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Fill in the upper left hand block, which is                                */
/* 'top' rows and 'bottom' columns.                                           */
/*                                                                            */
/* -T1 * (Y1 * Y2^T) * T2                                                     */
/* Block upper left hand element is T+(LDT*top).                              */
/*----------------------------------------------------------------------------*/
/* Step 1: Y1 * Y2^T, Y2 upper unit triangular.                               */
/* We do this in two steps to use DTRMM.                                      */
/* We copy by hand the 'top' columns                                          */
/* of Y1 into the workspace; then use DTRMM to                                */
/* multiply that on the bottom by the triangular                              */
/* part of Y2. Then we use DGEMM to compute                                   */
/* Workspace += Y1rest* Y2^Trest.                                             */
/*                                                                            */
/* Note Y1rest[0,0] is at V+K, and                                            */
/* Y2rest[0,0] is at Y2+bottom.                                               */
/*----------------------------------------------------------------------------*/
   for (row=0; row<top; row++)              /* Each row of Tw                 */
   {
      for (col=0; col<bottom; col++)        /* Each column of Tw              */
      {
         #ifdef TREAL
            *(Tw + row + col*LDT) =         /* Work[row, col]                 */
            *((V+top*LDV)+row+col*LDV);     /* Y1[col, row].                  */
         #else

            *(Tw +( row SHIFT) + col*LDT2) =/* Work[row, col]                 */
            *((V+top*LDV2)+(row SHIFT)+col*LDV2);/* Y1[col, row].             */

/*          copy the imaginary part.                                          */
            *(Tw +( row SHIFT)+1 + col*LDT2) =   /* Work[row, col]            */
            *((V+top*LDV2)+(row SHIFT)+ 1+col*LDV2);  /* Y1[col, row].        */
         #endif
      }
   }
/*----------------------------------------------------------------------------*/
/* Copy complete, Tw is 'top' rows, 'bottom' cols.                            */
/* Tw = 1.0 * Tw * Y2^T.                                                      */
/* DTRMM: (right): B := alpha * B * triangular A^T.                           */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,                /* Y2 is on right, upr tri.       */
            MY_RIGHT, MY_UPPER,             /* Y2 is on right, upr tri.       */
            MY_TRANS, MY_UNIT,              /* As-is, with unit diag.         */
            top, bottom, ONE,               /* B rows, cols, alpha=1.         */
            Y2, LDV, Tw, LDT);              /* A and B.                       */
/*----------------------------------------------------------------------------*/
/* Dgemm can finish the multiply, if any is left.                             */
/* Tw += Y1 * (Y2^T), if N-K is > 0.                                          */
/* DGEMM: C = alpha * A * B + Beta * C.                                       */
/*----------------------------------------------------------------------------*/
   if (N > K)                               /* If any top below...            */
   {
     cblas_gemm(CblasColMajor,
               MY_NOTRANS,                  /* Y1 is not transposed.          */
               MY_TRANS,                    /* Y2 is                          */
               top, bottom,                 /* Dimensions of C.               */
               N-K,                         /* Height of columns.             */
               ONE,                         /* No special multiplier.         */
               (V+(K*LDV2)), LDV,           /* Y1[0,0], dist to Y1[0,1]       */
               (Y2+(bottom*LDV2)), LDV,     /* Y2[0,0], dist to Y2[0,1]       */
               ONE,                         /* Add to existing data.          */
               Tw, LDT);                    /* Wk[0,0], dist to Wk[0,1]       */
   }

/*----------------------------------------------------------------------------*/
/* Y1 * Y2^T complete; now two triangular mults.                              */
/* Tw = -1.0 * T1 * Tw.                                                       */
/* DTRMM: (left): B:= alpha * triangular A * B                                */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
            MY_LEFT, MY_UPPER,              /* T1 on top, upr tri.            */
            MY_NOTRANS, MY_NONUNIT,         /* As-is, non-unit diag.          */
            top, bottom, NONE,              /* B rows, cols, alpha=-1.        */
            T, LDT, Tw, LDT);               /* A and B.                       */

/*----------------------------------------------------------------------------*/
/* -T1 * (Y * Y2^T) complete. Final tri T2 Mult.                              */
/* Tw = 1.0 * Tw * T2.                                                        */
/* DTRMM: (right): B:= alpha * B * triangular A                               */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
           MY_RIGHT, MY_UPPER,              /* T2 on bottom, upr tri.         */
            MY_NOTRANS, MY_NONUNIT,         /* As-is, non-unit diag.          */
             top, bottom, ONE,              /* B rows, cols, alpha=1.         */
             T2, LDT, Tw, LDT);             /* A and B.                       */
   return;                                  /* All done.                      */
}                                           /* END ATL_dlarft_block           */

/*
 * This function produce the block portion of the T matrix in recursive
 * algorithm.
 */
void ATL_larft_blockBC(ATL_CINT N, ATL_CINT K, ATL_CINT left, ATL_CINT right,
                       TYPE *V, ATL_CINT LDV, TYPE *T, ATL_CINT LDT)
{
   TYPE *Tw, *Y2, *T2;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* For complex LDV * 2            */
   LDT2 = LDT SHIFT;                        /* For complex LDT * 2            */

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      const TYPE NONE = ATL_rnone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      const TYPE NONE[2] = {ATL_rnone, ATL_rzero};
   #endif

   Tw = T+(left SHIFT);                     /* Work at T[0 + left].           */
   Y2 = V+LDV2*left;                        /* Y2 at V[0,left].               */
   T2 = T+(left SHIFT) +LDT2*left;          /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Fill in the lower  left hand block, which is                               */
/* 'right' rows and 'left' columns.                                           */
/*                                                                            */
/* -T2 * (Y2^T * Y1) * T1                                                     */
/* Block upper right hand element is T+(LDT*left).                            */
/*----------------------------------------------------------------------------*/
/* TODO : check all thsis comments                                            */
/* Step 1: Y2^T * Y1, Y1 lower unit triangular.                               */
/* We do this in two steps to use DTRMM.                                      */
/* We transpose by hand the 'last' 'right' columns                            */
/* of Y2 into the workspace; then use DTRMM to                                */
/* multiply that on the right by the triangular                               */
/* part of Y1. Then we use DGEMM to compute                                   */
/* Workspace += Y2rest^T * Y1rest.                                            */
/*                                                                            */
/* Note Y1rest[0,0] is at V, and                                              */
/* Y2rest[0,0] is at Y2.                                                      */
/*                                                                            */
/*----------------------------------------------------------------------------*/
   for (row=0; row<right; row++)            /* Each row of Tw                 */
   {
      for (col=0; col<left; col++)          /* Each column of Tw              */
      {
         #ifdef TREAL
/*          Copy the  Transpose                                               */
            *(Tw + row + col*LDT) =         /* Work[row, col]                 */
            *((Y2+N-K) +row*LDV2 + col);    /* Y[col, row].                   */
         #else
/*          Copy the  ConjTranspose for complex                               */
            *(Tw + (row SHIFT) + col*LDT2) =/* Work[row, col]                 */
            *(Y2+ ((N-K) SHIFT) +row*LDV2 + (col SHIFT) ); /* Y[col, row].    */

/*          copy the imaginary part.                                          */
            *(Tw + (row SHIFT) + 1 + col*LDT2) = /* Work[row, col]            */
            0.0 - *(Y2+((N-K) SHIFT)+row*LDV2 + (col SHIFT)+1); /* Y[col,row].*/
         #endif
      }
   }

/*----------------------------------------------------------------------------*/
/* Copy complete, Tw is 'right' rows, 'left' cols.                            */
/* Tw = 1.0 * Tw * Y1.                                                        */
/* DTRMM: (right): B := alpha * B * triangular A.                             */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,                /* Y1 is on right, upr tri.       */
             MY_RIGHT, MY_UPPER,            /* Y1 is on right, upr tri.       */
             MY_NOTRANS, MY_UNIT,           /* As-is, with unit diag.         */
             right, left, ONE,              /* B rows, cols, alpha=1.         */
             (V + ((N -K) SHIFT)), LDV, Tw, LDT);/* A and B.                  */
/*----------------------------------------------------------------------------*/
/* Dgemm can finish the multiply, if any is left.                             */
/* Tw += (Y2^T) * Y1, if N-K is > 0.                                          */
/* DGEMM: C = alpha * A * B + Beta * C.                                       */
/*----------------------------------------------------------------------------*/
   if (N > K)                               /* If any left below...           */
   {
     cblas_gemm(CblasColMajor,
                MY_TRANS,                   /* Y2 is transposed.              */
                MY_NOTRANS,                 /* Y1 is not.                     */
                right, left,                /* Dimensions of C.               */
                N-K,                        /* Height of columns.             */
                ONE,                        /* No special multiplier.         */
                (Y2), LDV,                  /* Y2[0,0], dist to Y2[0,1]       */
                (V ), LDV,                  /* Y1[0,0], dist to Y1[0,1]       */
                ONE,                        /* Add to existing data.          */
                Tw, LDT);                   /* Wk[0,0], dist to Wk[0,1]       */
   }

/*----------------------------------------------------------------------------*/
/* Y2^T * Y1 complete; now two triangular mults.                              */
/* Tw = -1.0 * T2 * Tw.                                                       */
/* DTRMM: (left): B:= alpha * triangular A * B                                */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
            MY_LEFT, MY_LOWER,              /* T2 on left, lwr tri.           */
            MY_NOTRANS, MY_NONUNIT,         /* As-is, non-unit diag.          */
            right, left, NONE,              /* B rows, cols, alpha=-1.        */
            T2, LDT, Tw, LDT);              /* A and B.                       */

/*----------------------------------------------------------------------------*/
/* -T2 * (Y2^T * Y1) complete. Final tri T1 Mult.                             */
/* Tw = 1.0 * Tw * T1.                                                        */
/* DTRMM: (right): B:= alpha * B * triangular A                               */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
           MY_RIGHT, MY_LOWER,              /* T1 on right, lower tri.        */
           MY_NOTRANS, MY_NONUNIT,          /* As-is, non-unit diag.          */
           right, left, ONE,                /* B rows, cols, alpha=1.         */
           T, LDT, Tw, LDT);                /* A and B.                       */


   return;                                  /* All done.                      */
}                                           /* END ATL_dlarft_block_BC        */

void ATL_larft_blockBR(ATL_CINT N, ATL_CINT K, ATL_CINT top, ATL_CINT bottom,
                       TYPE *V, ATL_CINT LDV, TYPE *T, ATL_CINT LDT)
{
   TYPE *Tw, *Y2, *T2;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* For complex LDV * 2            */
   LDT2 = LDT SHIFT;                        /* For complex LDT * 2            */

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      const TYPE NONE = ATL_rnone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      const TYPE NONE[2] = {ATL_rnone, ATL_rzero};
   #endif

   Tw = T+(top SHIFT);                      /* Work at T[top,0].              */
   Y2 = V+(top SHIFT);                      /* Y2 at V[top, 0].               */
   T2 = T+(top SHIFT) +LDT2*top;            /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Fill in the lower  left hand block, which is                               */
/* 'bottom' rows and 'top' columns.                                           */
/*                                                                            */
/* -T2 * (Y2 * Y1^T) * T1                                                     */
/* Block  lower left hand elements                                            */
/*----------------------------------------------------------------------------*/
/* Step 1: Y2 * Y1^T, Y1 lower unit triangular.                               */
/* We do this in two steps to use DTRMM.                                      */
/* We copy by hand the 'last-bottom' 'top' columns                            */
/* of Y2 into the workspace; then use DTRMM to                                */
/* multiply that on the bottom by the triangular                              */
/* part of Y1. Then we use DGEMM to compute                                   */
/* Workspace += Y2rest * Y1rest^T.                                            */
/*                                                                            */
/* Note Y1rest[0,0] is at V, and                                              */
/* Y2rest[0,0] is at Y2.                                                      */
/*                                                                            */
/*----------------------------------------------------------------------------*/
   for (row=0; row<bottom; row++)           /* Each row of Tw                 */
   {
      for (col=0; col<top; col++)           /* Each column of Tw              */
      {
         #ifdef TREAL
            *(Tw + row + col*LDT) =         /* Work[row, col]                 */
            *((Y2+ (N-K)*LDV) +row + (col*LDV)); /* Y[col, row].              */
         #else
            *(Tw + (row SHIFT) + col*LDT2) =/* Work[row, col]                 */
            *((Y2+ (N-K)*LDV2) +(row SHIFT) + (col*LDV2)); /* Y[col, row].    */


/*          copy the imaginary part.                                          */
            *(Tw + (row SHIFT) +1  + col*LDT2) = /* Work[row, col]            */
            *((Y2+ (N-K)*LDV2) +(row SHIFT) +1 + (col*LDV2)); /* Y[col, row]. */

         #endif
      }
   }

/*----------------------------------------------------------------------------*/
/* Copy complete, Tw is 'bottom' rows, 'top' cols.                            */
/* Tw = 1.0 * Tw * Y1^T.                                                      */
/* DTRMM: (bottom): B := alpha*B * (op)triangular A.                          */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,                /* Y1 is on bottom, lower tri.    */
             MY_RIGHT, MY_LOWER,            /* Y1 is on bottom, lower tri.    */
             MY_TRANS, MY_UNIT,             /* As-is, with unit diag.         */
             bottom, top, ONE,              /* B rows, cols, alpha=1.         */
             (V + ((N -K)*LDV2)), LDV, Tw, LDT); /* A and B.                  */
/*----------------------------------------------------------------------------*/
/* Dgemm can finish the multiply, if any is top.                              */
/* Tw += Y2 * (Y1^T), if N-K is > 0.                                          */
/* DGEMM: C = alpha * A * B + Beta * C.                                       */
/*----------------------------------------------------------------------------*/
   if (N > K)                               /* If any top below...            */
   {
     cblas_gemm(CblasColMajor,
                MY_NOTRANS,                 /* Y2 is not transposed.          */
                MY_TRANS,                   /* Y1 is transposed.              */
                bottom, top,                /* Dimensions of C.               */
                N-K,                        /* Height of columns.             */
                ONE,                        /* No special multiplier.         */
                (Y2), LDV,                  /* Y2[0,0], dist to Y2[0,1]       */
                (V ), LDV,                  /* Y1[0,0], dist to Y1[0,1]       */
                ONE,                        /* Add to existing data.          */
                Tw, LDT);                   /* Wk[0,0], dist to Wk[0,1]       */
   }

/*----------------------------------------------------------------------------*/
/* Y2 * Y1^T complete; now two triangular mults.                              */
/* Tw = -1.0 * T2 * Tw.                                                       */
/* DTRMM: (top): B:= alpha * triangular A * B                                 */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
            MY_LEFT, MY_LOWER,              /* T2 on left, lwr tri.           */
            MY_NOTRANS, MY_NONUNIT,         /* As-is, non-unit diag.          */
            bottom, top, NONE,              /* B rows, cols, alpha=-1.        */
            T2, LDT, Tw, LDT);              /* A and B.                       */

/*----------------------------------------------------------------------------*/
/* -T2 * (Y2 * Y1^T) complete. Final tri T1 Mult.                             */
/* Tw = 1.0 * Tw * T1.                                                        */
/* DTRMM: (right): B:= alpha * B * triangular A                               */
/*----------------------------------------------------------------------------*/
   cblas_trmm(CblasColMajor,
           MY_RIGHT, MY_LOWER,              /* T1 on bottom, lower tri.       */
           MY_NOTRANS, MY_NONUNIT,          /* As-is, non-unit diag.          */
           bottom, top, ONE,                /* B rows, cols, alpha=1.         */
           T, LDT, Tw, LDT);                /* A and B.                       */

   return;                                  /* All done.                      */
}                                           /* END ATL_dlarft_block_BR        */

void ATL_larft(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
               ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
               const TYPE *TAU, TYPE *T, ATL_CINT LDT)
{

   if ( DIRECT == LAForward)
   {
      if ( STOREV == LAColumnStore)
      {
         ATL_larftFC(DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT );
      }
      else if (STOREV == LARowStore)
      {
         ATL_larftFR(DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT );
      }

   }
   else if (DIRECT == LABackward)
   {
      if ( STOREV == LAColumnStore)
      {
         ATL_larftBC(DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT );
      }
      else if (STOREV == LARowStore)
      {
         ATL_larftBR(DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT );
      }
   }
}                                           /* END ATL_dlarft                 */

void ATL_larftFC(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT)
{
   TYPE   *Y2, *T2;
   TYPE   dt;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* for complex LDV *2             */
   LDT2 = LDT SHIFT;                        /* for comlex LDT *2              */
   int right, left;

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      TYPE VII;
      TYPE  d01, d02, d12;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      TYPE VII[2];
      TYPE  d01[2], d02[2], d12[2];
   #endif

   if (K == 0) return;                      /* Nothing to do.                 */
   if (N == 0) return;                      /* Nothing to do.                 */
   if (DIRECT != LAForward || STOREV != LAColumnStore) /* If not my specialty */
   {
      fprintf(stderr, "ATL_dlarft called with DIRECT=%d, STOREV=%d.\n"
                     "Aborting.\n", DIRECT, STOREV);
      exit(1);
   }

   if (K == 1)                              /* Down to a scalar               */
   {
      #ifdef TREAL
         *T = *TAU;                         /* Y. Just copy it.               */
      #else
         *T = *TAU;                         /* Y. Just copy it.               */
         *(T+1) = *(TAU+1);                 /* Y. Just copy it.               */
      #endif
      return;                               /* ...All done!                   */
   }

/*----------------------------------------------------------------------------*/
/* K=2: We only need T[0,1], computed as: (zero-relative):                    */
/*                                                                            */
/* T[0, 1] = -tau[0] * d01 * tau[1].                                          */
/*----------------------------------------------------------------------------*/
   if (K == 2)
   {
      #ifdef TREAL
         *T = *TAU;                         /* Copy the diagonal...           */
         *(T+LDT+1) = *(TAU+1);             /* ...                            */
         d01 = MY_DOT( N-2, V+2, 1, V+2+LDV, 1); /* Main dot product.         */
         d01 += *(V+1);                     /* Part mult by assumed 1.0       */
         *(T+LDT) = -(*TAU) * (*(TAU+1)) * d01;  /* Fill in T[0,1].           */
      #else
         *T = *TAU;                         /* Copy the diagonal -real        */
         *(T+1) = *(TAU+1);                 /* Copy the diagonal. -real       */
         *(T+LDT2+(1 SHIFT) ) = *(TAU+(1 SHIFT));/* ... -imag                 */
         *(T+LDT2+(1 SHIFT)+1) = *(TAU+(1 SHIFT) +1 );/* ... -imag            */

         MY_DOT( N-2, V+(2 SHIFT), 1,
                  (V+(2 SHIFT )+LDV2), 1, d01);  /* Main dot product.         */

         *(d01)+=*(V+(1 SHIFT));            /* Part mult by assumed 1.0       */
         *(d01+1)-=*(V+(1 SHIFT) + 1) ;     /* taken conj                     */

/*       T[0,1] = -tau1*tau2*d01                                              */
/*       Perform                                                              */
/*       tau1( -a -ib) * tau2(c+id) = (-ac+bd)  -i(bc+ad)-> res               */
         *(T+LDT2)  = -1.0* (*TAU) * (*(TAU + 2))  +
                     (*(TAU + 1) ) * (*(TAU + 3));
         *(T+LDT2 + 1)  = -1.0* (  (*(TAU + 1) ) * (*(TAU + 2))  +
                     (*TAU) * (*(TAU + 3))   ) ;

/*       res = res* d01                                                       */
         MY_DOTU( 1, (T+LDT2), 1, (d01), 1, (T+LDT2) ); /* T[0,1]             */
      #endif

      return;
   }

   right = (K>>3)<<2;
   left = K -right;
   if (right == 0)                          /* If not enough,                 */
   {
      left  = K>>1;                         /* Take smaller half.             */
      right = K-left;                       /* right is the rest.             */
   }

   Y2 = V+(left SHIFT) +LDV2*left;          /* Y2 at V[left, left].           */
   T2 = T+(left SHIFT)+LDT2*left;           /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Recurse on the left, no change to T or V.                                  */
/*----------------------------------------------------------------------------*/
   ATL_larftFC(DIRECT, STOREV, N, left,     /* K changes for left.            */
               V, LDV, TAU, T, LDT);        /* Left uses same V,Tau,T.        */


/*----------------------------------------------------------------------------*/
/* Recurse on the right, T2 is always below and to                            */
/* The right of T1 (adjacent with no overlap).                                */
/*----------------------------------------------------------------------------*/
   ATL_larftFC(DIRECT, STOREV, N-left, right,    /* N and K change on right.  */
            Y2, LDV, TAU+(left SHIFT), T2, LDT); /* Right new V, Tau, T.      */

   ATL_larft_blockFC(N, K, left, right,
                     V, LDV, T, LDT);       /* Fill in urh block.             */

}                                           /* END ATL_dlarft                 */

void ATL_larftBC(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT)
{
   TYPE   *Y2, *T2;
   TYPE   dt;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* for complex LDV *2             */
   LDT2 = LDT SHIFT;                        /* for comlex LDT *2              */
   int left, right;

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      TYPE VII;
      TYPE  d10, d20, d21;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      TYPE VII[2];
      TYPE  d10[2], d02[2], d12[2];
   #endif

   if (K == 0) return;                      /* Nothing to do.                 */
   if (N == 0) return;                      /* Nothing to do.                 */
   if (DIRECT != LABackward || STOREV != LAColumnStore) /* If not my specialty*/
   {
      fprintf(stderr, "ATL_dlarft called with DIRECT=%d, STOREV=%d.\n"
                     "Aborting.\n", DIRECT, STOREV);
      exit(1);
   }

   if (K == 1)                              /* Down to a scalar?              */
   {
     #ifdef TREAL
        *T = *TAU;                          /* Y. Just copy it.               */
     #else
        *T = *TAU;                          /* Y. Just copy it.               */
        *(T+1) = *(TAU+1);                  /* Y. Just copy it.               */
     #endif
      return;                               /* ...All done!                   */
   }

/*----------------------------------------------------------------------------*/
/* K=2: We only need T[1,0], computed as: (zero-relative):                    */
/*                                                                            */
/* T[0, 1] = -tau[0] * d10 * tau[1].        d10 : v0^T V1                     */
/*----------------------------------------------------------------------------*/
   if (K == 2)
   {
      #ifdef TREAL
         *T = *TAU;                         /* Copy the diagonal...           */
         *(T+LDT+1) = *(TAU+1);             /* ...                            */
         d10 = MY_DOT( N-2, V+LDV, 1, V, 1);/* Main dot product.              */
         d10 += *(V+LDV+N-2);               /* Part mult by assumed 1.0       */
         *(T+1) = -(*(TAU+1)) * (*TAU)* d10;/* Fill in T[0,1].                */
      #else
         *T = *TAU;                         /* Copy the diagonal -real        */
         *(T+1) = *(TAU+1);                 /* Copy the diagonal. -real       */
         *(T+LDT2+(1 SHIFT) ) = *(TAU+(1 SHIFT));/* ... -imag                 */
         *(T+LDT2+(1 SHIFT)+1) = *(TAU+(1 SHIFT) +1 );/* ... -imag            */

/*       Y is  conjugate transposed                                           */
         MY_DOT( N-2, (V+LDV2), 1, (V), 1, d10); /* Main dot product.         */

         *(d10)+=*(V+LDV2+((N-2) SHIFT));   /* Part mult by assumed 1.0       */
         *(d10+1)-=*(V+LDV2+((N-2) SHIFT) + 1) ; /* taken conj                */

/*       T[1,0] = -tau1*tau2*d10                                              */
/*       Perform                                                              */
/*       tau1( -a -ib) * tau2(c+id) = (-ac+bd)  -i(bc+ad)-> res               */
         *(T+ (1 SHIFT) )  = -1.0* (*TAU) * (*(TAU + 2))  +
                     (*(TAU + 1) ) * (*(TAU + 3));
         *(T+(1 SHIFT) + 1)  = -1.0* (  (*(TAU + 1) ) * (*(TAU + 2))  +
                     (*TAU) * (*(TAU + 3))   ) ;

/*  res = res* d01                                                            */
         MY_DOTU( 1, (T+ ( 1 SHIFT) ), 1,
                  (d10), 1, (T + (1 SHIFT)) );   /* T[0,1]                    */

      #endif

      return;
   }

   left = (K>>3)<<2;
   right = K -left;
   if (left == 0)                           /* If not enough,                 */
   {
      right  = K>>1;                        /* Take smaller half.             */
      left = K-right;                       /* right is the rest.             */
   }

   Y2 = V+LDV2*left;                        /* Y2 at V[0, left].              */
   T2 = T+(left SHIFT)+LDT2*left;           /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Recurse on the right, no change to T or V.                                 */
/*----------------------------------------------------------------------------*/
   ATL_larftBC(DIRECT, STOREV, N, right,    /* K changes for left.            */
     Y2, LDV, (TAU+(left SHIFT)) , T2, LDT);/* Left uses same V,Tau,T.        */

/*----------------------------------------------------------------------------*/
/* Recurse on the left, T1 is always above and to                             */
/* The left of T1 (adjacent with no overlap).                                 */
/*----------------------------------------------------------------------------*/
   ATL_larftBC(DIRECT, STOREV, N-right, left,    /* N and K change on right.  */
            V, LDV, TAU, T, LDT);           /* Left same V, Tau, T.           */

   ATL_larft_blockBC(N, K, left, right,
                    V, LDV, T, LDT);        /* Fill in urh block.             */

}                                           /* END ATL_dlarft                 */


void ATL_larftFR(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT)
{
   TYPE   *Y2, *T2;
   TYPE   dt;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* for complex LDV *2             */
   LDT2 = LDT SHIFT;                        /* for comlex LDT *2              */
   int bottom, top;

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      TYPE VII;
      TYPE  d01, d02, d12;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      TYPE VII[2];
      TYPE  d01[2], d02[2], d12[2];
   #endif

   if (K == 0) return;                      /* Nothing to do.                 */
   if (N == 0) return;                      /* Nothing to do.                 */
   if (DIRECT != LAForward || STOREV != LARowStore)   /* If not my specialty, */
   {
      fprintf(stderr, "ATL_dlarft called with DIRECT=%d, STOREV=%d.\n"
                     "Aborting.\n", DIRECT, STOREV);
      exit(1);
   }

   if (K == 1)                              /* Down to a scalar?              */
   {
     #ifdef TREAL
        *T = *TAU;                          /* Y. Just copy it.               */
     #else
        *T = *TAU;                          /* Y. Just copy it.               */
        *(T+1) = *(TAU+1);                  /* Y. Just copy it.               */
     #endif
      return;                               /* ...All done!                   */
   }

/*----------------------------------------------------------------------------*/
/* K=2: We only need T[0,1], computed as: (zero-relative):                    */
/*                                                                            */
/* T[0, 1] = -tau[0] * d01 * tau[1].   ( z = -tau.T.Y.v')                     */
/*----------------------------------------------------------------------------*/
   if (K == 2)
   {
      #ifdef TREAL
         *T = *TAU;                         /* Copy the diagonal...           */
         *(T+LDT+1) = *(TAU+1);             /* ...                            */
         d01 = MY_DOT( N-2, (V+2*LDV), LDV,
               V+2*LDV+1, LDV);             /* Main dot product.              */
         d01 += *(V+LDV);                   /* Part mult by assumed 1.0       */
         *(T+LDT) = -(*TAU) * (*(TAU+1)) * d01;  /* Fill in T[0,1].           */
      #else
         *T = *TAU;                         /* Copy the diagonal -real        */
         *(T+1) = *(TAU+1);                 /* Copy the diagonal. -real       */
         *(T+LDT2+(1 SHIFT) ) = *(TAU+(1 SHIFT));/* ... -imag                 */
         *(T+LDT2+(1 SHIFT)+1) = *(TAU+(1 SHIFT) +1 );/* ... -imag            */

/*       conjugate of V is taken                                              */
         MY_DOT( N-2, (V+2*LDV2 +(1 SHIFT)), LDV,
               (V+2*LDV2), LDV, d01);       /* Main dot product.              */


        *( d01)  += *(V+LDV2);              /* Part mult by assumed 1.0       */
        *(d01+1) += *(V+LDV2 + 1);          /* Part mult by assumed 1.0       */

/*      T[0,1] = -tau1*tau2*d0                                                */
/*      Perform                                                               */
/*       tau1( -a -ib) * tau2(c+id) = (-ac+bd)  -i(bc+ad)-> res               */
         *(T+ LDT2 )  = -1.0* (*TAU) * (*(TAU + 2))  +
                     (*(TAU + 1) ) * (*(TAU + 3));
         *(T+ LDT2 + 1)  = -1.0* (  (*(TAU + 1) ) * (*(TAU + 2))  +
                     (*TAU) * (*(TAU + 3))   ) ;

/*       res = res* d01                                                       */
         MY_DOTU( 1, (T+LDT2), 1,
                  (d01), 1, (T+ LDT2) );    /* T[1,0]                         */
      #endif

      return;
   }

   bottom = (K>>3)<<2;
   top = K -bottom;
   if (bottom == 0)                         /* If not enough,                 */
   {
      top  = K>>1;                          /* Take smaller half.             */
      bottom = K-top;                       /* bottom is the rest.            */
   }

   Y2 = V+(top SHIFT) +LDV2*top;            /* Y2 at V[top, top].             */
   T2 = T+(top SHIFT)+LDT2*top;             /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Recurse on the top, no change to T or V.                                   */
/*----------------------------------------------------------------------------*/
   ATL_larftFR(DIRECT, STOREV, N, top,      /* K changes for top.             */
               V, LDV, TAU, T, LDT);        /* Left uses same V,Tau,T.        */


/*----------------------------------------------------------------------------*/
/* Recurse on the bottom, T2 is always below and to                           */
/* The bottom of T1 (adjacent with no overlap).                               */
/*----------------------------------------------------------------------------*/
   ATL_larftFR(DIRECT, STOREV, N-top, bottom,    /* N and K change on bottom. */
            Y2, LDV, TAU+(top SHIFT), T2, LDT);  /* Right new V, Tau, T.      */

   ATL_larft_blockFR(N, K, top, bottom,
                     V, LDV, T, LDT);       /* Fill in urh block.             */

}                                           /* END ATL_dlarft                 */

void ATL_larftBR(const enum ATL_LADIRECT DIRECT, const enum ATL_LASTOREV STOREV,
                 ATL_CINT N, ATL_CINT K, TYPE *V, ATL_CINT LDV,
                 const TYPE *TAU, TYPE *T, ATL_CINT LDT)
{
   TYPE   *Y2, *T2;
   TYPE   dt;
   int    row, col;
   int LDV2, LDT2;
   LDV2 = LDV SHIFT;                        /* for complex LDV *2             */
   LDT2 = LDT SHIFT;                        /* for comlex LDT *2              */
   int top, bottom;

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      TYPE VII;
      TYPE  d10, d20, d21;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
      TYPE VII[2];
      TYPE  d10[2], d02[2], d12[2];
   #endif

   if (K == 0) return;                      /* Nothing to do.                 */
   if (N == 0) return;                      /* Nothing to do.                 */
   if (DIRECT != LABackward || STOREV != LARowStore)  /* If not my specialty, */
   {
      fprintf(stderr, "ATL_dlarft called with DIRECT=%d, STOREV=%d.\n"
                     "Aborting.\n", DIRECT, STOREV);
      exit(1);
   }

   if (K == 1)                              /* Down to a scalar               */
   {
     #ifdef TREAL
        *T = *TAU;                          /* Y. Just copy it.               */
     #else
        *T = *TAU;                          /* Y. Just copy it.               */
        *(T+1) = *(TAU+1);                  /* Y. Just copy it.               */
     #endif
      return;                               /* ...All done!                   */
   }

/*----------------------------------------------------------------------------*/
/* K=2: We only need T[1,0], computed as: (zero-relative):                    */
/*                                                                            */
/* T[0, 1] = -tau[0] *  tau[1] * d10        d10 : V1*v0^T                     */
/*                                   => -tau*T*Y*v^T                          */
/* T is a lower triangular matrix                                             */
/*----------------------------------------------------------------------------*/
   if (K == 2)
   {
      #ifdef TREAL
         *T = *TAU;                         /* Copy the diagonal...           */
         *(T+LDT+1) = *(TAU+1);             /* ...                            */
         d10 = MY_DOT( N-2, V, LDV,(V+1), LDV);  /* Main dot product.         */
         d10 += *(V+1+(N-2)*LDV);           /* Part mult by assumed 1.0       */
         *(T+1) = -(*(TAU)) * (*(TAU+1))* d10;   /* Fill in T[1,0].           */
      #else
         *T = *TAU;                         /* Copy the diagonal -real        */
         *(T+1) = *(TAU+1);                 /* Copy the diagonal. -real       */
         *(T+LDT2+(1 SHIFT) ) = *(TAU+(1 SHIFT));/* ... -imag                 */
         *(T+LDT2+(1 SHIFT)+1) = *(TAU+(1 SHIFT) +1 );/* ... -imag            */

/*       conjugate of V is taken                                              */
         MY_DOT( N-2, V, LDV, (V+( 1 SHIFT) ), LDV, d10); /* Main dot product.*/

         *(d10)+=*(V+(1 SHIFT) + (N-2)*LDV2);    /* Part mult by assumed 1.0  */
         *(d10+1)+=*(V+(1 SHIFT)+1 + (N-2)*LDV2 ); /* Part mult by assumed 1.0*/

/*       T[1,0] = -tau1*tau2*d10                                              */
/*       Perform                                                              */
/*       tau1( -a -ib) * tau2(c+id) = (-ac+bd)  -i(bc+ad)-> res               */
         *(T+ ( 1 SHIFT) )  = -1.0* (*TAU) * (*(TAU + 2))  +
                     (*(TAU + 1) ) * (*(TAU + 3));
         *(T+ ( 1 SHIFT) + 1)  = -1.0* (  (*(TAU + 1) ) * (*(TAU + 2))  +
                     (*TAU) * (*(TAU + 3))   ) ;

/*       res = res* d10                                                       */
         MY_DOTU( 1, (T+(1 SHIFT)), 1,
                  (d10), 1, (T+ ( 1 SHIFT) ) );  /* T[1,0]                    */

      #endif

      return;
   }

   top = (K>>3)<<2;
   bottom = K -top;
   if (top == 0)                            /* If not enough,                 */
   {
      bottom  = K>>1;                       /* Take smaller half.             */
      top = K-bottom;                       /* bottom is the rest.            */
   }

   Y2 = V+(top SHIFT) ;                     /* Y2 at V[top,0].                */
   T2 = T+(top SHIFT)+LDT2*top;             /* Address of new T2.             */

/*----------------------------------------------------------------------------*/
/* Recurse on the bottom, no change to T or V.                                */
/*----------------------------------------------------------------------------*/
   ATL_larftBR(DIRECT, STOREV, N, bottom,   /* K changes for top.             */
     Y2, LDV, (TAU+(top SHIFT)) , T2, LDT); /* Left uses same V,Tau,T.        */

/*----------------------------------------------------------------------------*/
/* Recurse on the top, T1 is always above and to                              */
/* The top of T1 (adjacent with no overlap).                                  */
/*----------------------------------------------------------------------------*/
   ATL_larftBR(DIRECT, STOREV, N-bottom, top,    /* N and K change on bottom. */
               V, LDV, TAU, T, LDT);             /* Left same V, Tau, T.      */

   ATL_larft_blockBR(N, K, top, bottom,
                    V, LDV, T, LDT);        /* Fill in urh block.             */

}                                           /* END ATL_dlarft                 */

