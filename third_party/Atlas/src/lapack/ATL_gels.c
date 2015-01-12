/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
#include "atlas_lapack.h"
#include "atlas_lamch.h"

#ifdef TCPLX
   #define mytrans AtlasConjTrans
#else
   #define mytrans AtlasTrans
#endif
int Mjoin(PATL,gels)
   (const enum ATLAS_TRANS TA, ATL_CINT M, ATL_CINT N, ATL_CINT NRHS,
    TYPE *A, ATL_CINT lda, TYPE *B, ATL_CINT ldb, TYPE *work, ATL_CINT lwork)
/*
 *  GELS solves overdetermined or underdetermined linear systems
 *  involving an M-by-N matrix A, or its conjugate-transpose, using a QR
 *  or LQ factorization of A.  It is assumed that A has full rank.
 *
 *  This is a straight translation from LAPACK 3.2.1; the only performance
 *  improvements come from using ATLAS's improved QR (and slighly ORMQR)
 *  implementations.
 *
 *  The following options are provided:
 *
 *  1. If TRANS = 'N' and m >= n:  find the least squares solution of
 *     an overdetermined system, i.e., solve the least squares problem
 *                  minimize || B - A*X ||.
 *
 *  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
 *     an underdetermined system A * X = B.
 *
 *  3. If TRANS = 'C/T' and m >= n:  find the minimum norm solution of
 *     an undetermined system A**H * X = B.
 *
 *  4. If TRANS = 'C/T' and m < n:  find the least squares solution of
 *     an overdetermined system, i.e., solve the least squares problem
 *                  minimize || B - A**H * X ||.
 *
 *  Several right hand side vectors b and solution vectors x can be
 *  handled in a single call; they are stored as the contiguously in the
 *  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
 *  matrix X.
 *
 *  TRANS   (input) CHARACTER*1
 *          = 'N': the linear system involves A;
 *          = 'C': the linear system involves A**H (complex only).
 *          = 'T': the linear system involves A**T (real only).
 *
 *  M       (input) INTEGER
 *          The number of rows of the matrix A.  M >= 0.
 *
 *  N       (input) INTEGER
 *          The number of columns of the matrix A.  N >= 0.
 *
 *  NRHS    (input) INTEGER
 *          The number of right hand sides, i.e., the number of
 *          columns of the matrices B and X. NRHS >= 0.
 *
 *  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
 *          On entry, the M-by-N matrix A.
 *            if M >= N, A is overwritten by details of its QR
 *                       factorization as returned by GEQRF;
 *            if M <  N, A is overwritten by details of its LQ
 *                       factorization as returned by GELQF.
 *  LDA     (input) INTEGER
 *          The leading dimension of the array A.  LDA >= max(1,M).
 *
 *  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
 *          On entry, the matrix B of right hand side vectors, stored
 *          columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
 *          if TRANS = 'C/T'.
 *          On exit, if INFO = 0, B is overwritten by the solution
 *          vectors, stored columnwise:
 *          if TRANS = 'N' and m >= n, rows 1 to n of B contain the least
 *          squares solution vectors; the residual sum of squares for the
 *          solution in each column is given by the sum of squares of the
 *          modulus of elements N+1 to M in that column;
 *          if TRANS = 'N' and m < n, rows 1 to N of B contain the
 *          minimum norm solution vectors;
 *          if TRANS = 'C' and m >= n, rows 1 to M of B contain the
 *          minimum norm solution vectors;
 *          if TRANS = 'C' and m < n, rows 1 to M of B contain the
 *          least squares solution vectors; the residual sum of squares
 *          for the solution in each column is given by the sum of
 *          squares of the modulus of elements M+1 to N in that column.
 *
 *  LDB     (input) INTEGER
 *          The leading dimension of the array B. LDB >= MAX(1,M,N).
 *
 * RETURNS:
 *          = 0:  successful exit
 *          < 0:  if INFO = -i, the i-th argument had an illegal value
 *          > 0:  if INFO =  i, the i-th diagonal element of the
 *                triangular factor of A is zero, so that A does not have
 *                full rank; the least squares solution could not be
 *                computed.
 */
{
   TYPE *TAU;
   #ifdef TCPLX
      const enum ATLAS_TRANS RTRAN = (TA == AtlasNoTrans) ?
                                     AtlasConjTrans : AtlasNoTrans;
      const TYPE one[3] = {ATL_rone, ATL_rzero, ATL_rzero}, *zero=one+1;
      TYPE wsq[4];
   #else
      const enum ATLAS_TRANS RTRAN = (TA == AtlasNoTrans) ?
                                     AtlasTrans : AtlasNoTrans;
      #define one ATL_rone
      #define zero ATL_rzero
      TYPE wsq[2];
   #endif
   TYPE *free0=NULL;
   TYPE anrm, bnrm;
   ATL_INT scalN, wlen;
   ATL_CINT MN = Mmin(M,N);
   int iascal=0, ibscal=0, ierr;
/*
 * Quick return for degenerate cases
 */
   if (!NRHS)
      return(0);
   else if (!M || !N)
   {
      Mjoin(PATL,geset)(Mmax(M,N), NRHS, zero, zero, B, ldb);
      return(0);
   }
/*
 * If no workspace given, routines will simply allocate their own, we need TAU
 */
   if (lwork == 0 || lwork < -1)
   {
      free0 = TAU = malloc(MN*ATL_sizeof);
      ATL_assert(TAU);
      work = NULL;
      wlen = 0;
   }
/*
 * If the user is providing workspace, or doing a workspace query, we must
 * compute the required workspace
 */
   else
   {
      wlen = MN;    /* space needed for TAU */
      if (M >= N)
      {
         ATL_assert(!Mjoin(PATL,geqrf)(M, N, A, lda, NULL, wsq, -1));
         ATL_assert(!Mjoin(PATL,ormqr)(AtlasLeft, RTRAN, M, NRHS, N, A, lda,
                                   NULL, B, ldb, wsq+(1 SHIFT), -1));
      }
      else
      {
         ATL_assert(!Mjoin(PATL,gelqf)(M, N, A, lda, NULL, wsq, -1));
         ATL_assert(!Mjoin(PATL,ormqr)(AtlasLeft, RTRAN, N, NRHS, M, A, lda,
                                   NULL, B, ldb, wsq+(1 SHIFT), -1));
      }
      if (wsq[1 SHIFT] > wsq[0])
         wsq[0] = wsq[1 SHIFT];
      wlen += wsq[0];
/*
 *    If this was a workspace query, return optimal workspace in *work
 */
      if (lwork == -1)
      {
         *work = wlen;
         return(0);
      }
/*
 *    Otherwise, take action if user's workspace is inadequate
 */
     if (lwork < wlen)
     {
        if (lwork >= wlen-MN) /* users space is work, we alloc TAU */
        {
           free0 = TAU = malloc(MN*ATL_sizeof);
           wlen -= MN;
           ATL_assert(TAU);
           work = work;
        }
        else if (lwork < MN) /* can't even use workspace for TAU */
        {
           free0 = TAU = malloc(MN*ATL_sizeof);
           ATL_assert(TAU);
           work = NULL;
           wlen = 0;
        }
        else  /* user's workspace becomes TAU; let worker routs alloc work */
        {
           TAU = work;
           work = NULL;
           wlen = 0;
        }
     }
     else  /* user provided adequate workspace for everything */
     {
        wlen = lwork - MN;
        TAU = work;
        work += MN SHIFT;
     }
   }
// TPSD is (TA != AtlasNoTrans)
/*
 * ===============================================================
 * Scale if max elt in A is outside safe range, return if nrm is 0
 * ===============================================================
 */
   anrm = Mjoin(PATL,gemaxnrm)(M, N, A, lda);
/*
 * If it is below it, scale matrix norm up to smallest safe number
 */
   if (anrm > ATL_rzero && anrm < ATL_labadUNDERTHRESH)
   {
      Mjoin(PATL,lascl)(LAMATG, 0, 0, anrm, ATL_labadUNDERTHRESH, M, N, A, lda);
      iascal = 1;
   }
/*
 * If matrix norm huge, scale it down by largest safe number
 */
   else if (anrm > ATL_labadOVERTHRESH)
   {
      Mjoin(PATL,lascl)(LAMATG, 0, 0, anrm, ATL_labadOVERTHRESH, M, N, A, lda);
      iascal = 2;
   }
/*
 * If norm is 0, entire matrix is 0, return zero solution
 */
   else if (anrm == ATL_rzero)
   {
      Mjoin(PATL,geset)(Mmax(M,N), NRHS, zero, zero, B, ldb);
      if (free0)
         free(free0);
      return(0);
   }
/*
 * ===============================================================
 * Scale if max elt in B is outside safe range, return if nrm is 0
 * ===============================================================
 */
   scalN = (TA != AtlasNoTrans) ? N : M;
   bnrm = Mjoin(PATL,gemaxnrm)(scalN, NRHS, B, ldb);
/*
 * If it is below it, scale matrix norm up to smallest safe number
 */
   if (bnrm > ATL_rzero && bnrm < ATL_labadUNDERTHRESH)
   {
      Mjoin(PATL,lascl)(LAMATG, 0, 0, bnrm, ATL_labadUNDERTHRESH,
                        scalN, NRHS, B, ldb);
      ibscal = 1;
   }
/*
 * If matrix norm huge, scale it down by largest safe number
 */
   else if (bnrm > ATL_labadOVERTHRESH)
   {
      Mjoin(PATL,lascl)(LAMATG, 0, 0, bnrm, ATL_labadOVERTHRESH,
                        scalN, NRHS, B, ldb);
      ibscal = 2;
   }
   if (M >= N)  /* overdetermined system */
   {
/*
 *    Compute QR factorization of A
 */
      ATL_assert(!Mjoin(PATL,geqrf)(M, N, A, lda, TAU, work, wlen));
/*
 *    Least-squares problem min || A * X - B ||
 */
      if (TA == AtlasNoTrans)
      {
         ATL_assert(!Mjoin(PATL,ormqr)(AtlasLeft, RTRAN, M, NRHS, N, A, lda,
                                       TAU, B, ldb, work, wlen));
         ierr = Mjoin(PATL,trtrs)(AtlasUpper, AtlasNoTrans, AtlasNonUnit,
                                  N, NRHS, A, lda, B, ldb);
         if (ierr)
         {
            if (free0)
               free(free0);
           return(ierr);
         }
         scalN = N;
      }
/*
 *    Overdetermined system of equations A' * X = B
 */
      else  /* transposed case */
      {
         ierr = Mjoin(PATL,trtrs)(AtlasUpper, TA, AtlasNonUnit,
                                  N, NRHS, A, lda, B, ldb);
         if (ierr)
         {
            if (free0)
               free(free0);
            return(ierr);
         }
         Mjoin(PATL,gezero)(M-N, NRHS, B+(N SHIFT), ldb);
          ATL_assert(!Mjoin(PATL,ormqr)(AtlasLeft, AtlasNoTrans, M, NRHS, N,
                                        A, lda, TAU, B, ldb, work, wlen));
         scalN = M;
      }
   }
/*
 * Compute LQ factorization of A
 */
   else    /* M < N */
   {
      ATL_assert(!Mjoin(PATL,gelqf)(M, N, A, lda, TAU, work, wlen));
/*
 *    Underdetermined system of equations A * X = B
 */
      if (TA == AtlasNoTrans)
      {
/*
 *       B(1:M,1:NRHS) = inv(L) * B(1:M,1:NRHS)
 */
         ierr = Mjoin(PATL,trtrs)(AtlasLower, AtlasNoTrans, AtlasNonUnit,
                                  M, NRHS, A, lda, B, ldb);
         if (ierr)
         {
            if (free0)
               free(free0);
            return(ierr);
         }
         Mjoin(PATL,gezero)(N-M, NRHS, B+(M SHIFT), ldb);
         ATL_assert(!Mjoin(PATL,ormlq)(AtlasLeft, RTRAN, N, NRHS, M, A, lda,
                                       TAU, B, ldb, work, wlen));
         scalN = N;
      }
/*
 *    Overdetermined system min || A' * X - B ||
 */
      else
      {
         ATL_assert(!Mjoin(PATL,ormlq)(AtlasLeft, AtlasNoTrans, N, NRHS, M,
                                       A, lda, TAU, B, ldb, work, wlen));
/*
 *       B(1:M,1:NRHS) := inv(L') * B(1:M,1:NRHS)
 */
         ierr = Mjoin(PATL,trtrs)(AtlasLower, mytrans, AtlasNonUnit,
                                  M, NRHS, A, lda, B, ldb);
         if (ierr)
         {
            if (free0)
               free(free0);
            return(ierr);
         }
         scalN = M;
      }
   }
/*
 * Undo scaling
 */
   if (iascal == 1)
      Mjoin(PATL,lascl)(LAMATG, 0, 0, anrm, ATL_labadUNDERTHRESH,
                        scalN, NRHS, B, ldb);
   else if (iascal == 2)
      Mjoin(PATL,lascl)(LAMATG, 0, 0, anrm, ATL_labadOVERTHRESH,
                        scalN, NRHS, B, ldb);
   if (ibscal == 1)
      Mjoin(PATL,lascl)(LAMATG, 0, 0, ATL_labadUNDERTHRESH, bnrm,
                        scalN, NRHS, B, ldb);
   else if (ibscal == 2)
      Mjoin(PATL,lascl)(LAMATG, 0, 0, ATL_labadOVERTHRESH, bnrm,
                        scalN, NRHS, B, ldb);
   if (free0)
      free(free0);
   return(0);
}
#undef mytrans
