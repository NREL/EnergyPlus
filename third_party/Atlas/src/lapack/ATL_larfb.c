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
 *     SUBROUTINE DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
 *     $                   T, LDT, C, LDC, WORK, LDWORK )
 *
 * ATL_larfb.c :
 *
 * void ATL_larfb(const enum CBLAS_SIDE SIDE, const enum CBLAS_TRANSPOSE TRANS,
 *     const enum ATL_LADIRECT  DIRECT, const enum ATL_LASTOREV STOREV,
 *                int M, int N, int K, TYPE *V, int LDV, TYPE *T, int LDT,
 *               TYPE *C, int LDC, TYPE *WORK, int LDWORK)
 *
 *
 *     NOTE :   ATL_larfb.c will get compiled to four precisions
 *                    single precision real,      double precision real
 *                    single precision complex,   double precision complex
 *  Purpose
 *  =======
 *
 *  ATL_larf  applies a real block reflector H or its transpose H' to a
 *  real/complex m by n matrix C, from either the left or the right.
 *
 *  Arguments
 *  =========
 *
 *  SIDE    (input) CHARACTER*1
 *          = 'L': apply H or H' from the Left
 *          = 'R': apply H or H' from the Right
 *
 *  TRANS   (input) CHARACTER*1
 *          = 'N': apply H (No transpose)
 *          = 'T': apply H' (Transpose for real)
 *          = 'C': apply H' (Conjugate Transpose for
 *                   complex)
 *
 *  DIRECT  (input) CHARACTER*1
 *          Indicates how H is formed from a product of elementary
 *          reflectors
 *          = 'F': H = H(1) H(2) . . . H(k) (Forward)
 *          = 'B': H = H(k) . . . H(2) H(1) (Backward)
 *
 *  STOREV  (input) CHARACTER*1
 *          Indicates how the vectors which define the elementary
 *          reflectors are stored:
 *          = 'C': Columnwise
 *          = 'R': Rowwise
 *
 *  M       (input) INTEGER
 *          The number of rows of the matrix C.
 *
 *  N       (input) INTEGER
 *          The number of columns of the matrix C.
 *
 *  K       (input) INTEGER
 *          The order of the matrix T (= the number of elementary
 *          reflectors whose product defines the block reflector).
 *
 *  V       (input)  array, dimension
 *                                (LDV,K) if STOREV = 'C'
 *                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
 *                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
 *          The matrix V. See further details.
 *
 *  LDV     (input) INTEGER
 *          The leading dimension of the array V.
 *          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
 *          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
 *          if STOREV = 'R', LDV >= K.
 *
 *  T       (input)  array, dimension (LDT,K)
 *          The triangular k by k matrix T in the representation of the
 *          block reflector.
 *
 *  LDT     (input) INTEGER
 *          The leading dimension of the array T. LDT >= K.
 *
 *  C       (input/output)  array, dimension (LDC,N)
 *          On entry, the m by n matrix C.
 *          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
 *
 *  LDC     (input) INTEGER
 *          The leading dimension of the array C. LDA >= max(1,M).
 *
 *  WORK    (workspace) array, dimension (LDWORK,K)
 *
 *  LDWORK  (input) INTEGER
 *          The leading dimension of the array WORK.
 *          If SIDE = 'L', LDWORK >= max(1,N);
 *          if SIDE = 'R', LDWORK >= max(1,M).
 *
 *  =====================================================================
 *  TonyC: When called by dgeqrf, V=&A(I,I), LDV=LDA. T=WORK, LDT=LDWORK.
 *  C=&A(I,I+IB), LDC=LDA. WORK=&WORK(IB+1), LDWORK=LDWORK. K=IB.
 */
#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_lapack.h"

#ifdef TREAL
    #define MY_TRANS CblasTrans
#else
    #define MY_TRANS CblasConjTrans
#endif

void ATL_larfb(const enum CBLAS_SIDE SIDE, const enum CBLAS_TRANSPOSE TRANS,
               const enum ATL_LADIRECT  DIRECT, const enum ATL_LASTOREV STOREV,
               ATL_CINT M, ATL_CINT N, ATL_CINT K, const TYPE *V, ATL_CINT LDV,
               const TYPE *T, ATL_CINT LDT, TYPE *C, ATL_CINT LDC,
               TYPE *WORK, ATL_CINT LDWORK)

{
   enum CBLAS_TRANSPOSE CTRANS, CTRANST;
   int    i, j;
   int  LDV2,LDC2, LDWORK2;
   LDV2 = LDV SHIFT;                        /* For complex, LDV*2             */
   LDC2 = LDC SHIFT;                        /* For complex, LDC*2             */
   LDWORK2 = LDWORK SHIFT;                  /* For complex, LDWORK * 2        */

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      const TYPE ZEROVAL = ATL_rzero;
      const TYPE NONE = ATL_rnone;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      const TYPE NONE[2] = {ATL_rnone, ATL_rzero};
      const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
   #endif

/* Quick return if possible                                                   */
   if (M <= 0 || N <= 0) return;            /* Early exit                     */

/*    Translate the Transpose and Transpose-Transpose settings                */
   if (TRANS == CblasNoTrans )
   {
      CTRANS = CblasNoTrans;
      #ifdef TREAL
         CTRANST = CblasTrans;
      #else
         CTRANST = CblasConjTrans;
      #endif
   } else
   {
      #ifdef TREAL
         CTRANS = CblasTrans;
      #else
         CTRANS = CblasConjTrans;
      #endif
      CTRANST = CblasNoTrans;
   }

   if (STOREV == LAColumnStore)
   {
      if (DIRECT == LAForward )
      {
/*
 *           Let  V =  ( V1 )    (first K rows)
 *                     ( V2 )
 *           where  V1  is unit lower triangular.
 */
            if (SIDE == CblasLeft)
            {
/*
 *             Form  H * C  or  H' * C  where  C = ( C1 )
 *                                                 ( C2 )
 *
 *             W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
 *
 *             W := C1'
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(N, C+(j SHIFT), LDC, WORK+j*LDWORK2, 1);
/*                For complex, make it conjugate                              */
                  #ifdef TCPLX
                     ATL_lacgv(N,  WORK+j*LDWORK2, 1);
                  #endif
               }

/*
 *             W := W * V1
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                          CblasUnit, N, K, ONE, V, LDV, WORK, LDWORK);

               if (M > K)
               {
/*
 *                 W := W + C2'*V2
 */

                  cblas_gemm(CblasColMajor, MY_TRANS, CblasNoTrans,
                             N, K, M-K, ONE, C+(K SHIFT), LDC, V+(K SHIFT),
                             LDV, ONE, WORK, LDWORK);
               }
/*
 *             W := W * T'  or  W * T
 *
 *             'T' is the non-unit upper triangular array, on the right.
 *             alpha is one.
 */
               cblas_trmm(CblasColMajor,
                          CblasRight, CblasUpper, CTRANST, CblasNonUnit,
                          N, K, ONE, T, LDT, WORK, LDWORK);
/*
 *             C := C - V * W'
 */
               if (M > K)
               {
/*
 *                 C2 := C2 - V2 * W'
 */
                  cblas_gemm(CblasColMajor,
                             CblasNoTrans, MY_TRANS, M-K, N, K, NONE,
                             V+(K SHIFT), LDV, WORK, LDWORK, ONE,
                             C+(K SHIFT), LDC);
               }
/*
 *             W := W * V1'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower,
                          MY_TRANS, CblasUnit, N, K, ONE, V, LDV, WORK, LDWORK);
/*
 *             C1 := C1 - W'
 */
               for (j=0; j< (K ) ; j++)
               {
                  for (i=0; i<N; i++)
                  {
                     #ifdef TREAL
                        C[j+i*LDC2] -= WORK[i+j*LDWORK2];
                     #else
                        C[(j SHIFT) +i*LDC2] -= WORK[(i SHIFT) +j*LDWORK2];
/*                      Conjugate is taken, hence the addition                */
                        C[(j SHIFT)+i*LDC2+1] += WORK[(i SHIFT)+j*LDWORK2+1];
                     #endif
                  }
               }                            /* for                            */
            } else if (SIDE == CblasRight)
            {
/*
 *             Form  C * H  or  C * H'  where  C = ( C1  C2 )
 *
 *             W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
 *
 *             W := C1
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(M, C+(j*LDC2), 1, WORK+j*LDWORK2, 1);
               }

/*
 *              W := W * V1
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower,
                          CblasNoTrans, CblasUnit, M, K, ONE, V, LDV,
                          WORK, LDWORK);
               if (N > K)
               {
/*
 *                 W := W + C2 * V2
 *
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                             M, K, N-K, ONE, C+K*LDC2, LDC,
                             V+(K SHIFT), LDV,ONE,WORK,LDWORK);
               }
/*
 *              W := W * T  or  W * T'
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CTRANS,
                          CblasNonUnit, M, K, ONE, T, LDT, WORK, LDWORK);
/*
 *              C := C - W * V'
 */
               if (N > K)
               {
/*
 *                 C2 := C2 - W * V2'
 *
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, MY_TRANS, M, N-K, K,
                             NONE, WORK, LDWORK, V+(K SHIFT), LDV, ONE,
                             C+K*LDC2, LDC);
               }
/*
 *              W := W * V1'
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, MY_TRANS,
                          CblasUnit, M, K, ONE, V, LDV, WORK, LDWORK);

/*
 *              C1 := C1 - W
 *
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<M; i++)
                  {
                     #ifdef TREAL
                        C[i+j*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[(i SHIFT)+j*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
                        C[(i SHIFT)+j*LDC2+1] -= WORK[(i SHIFT)+j*LDWORK2+1];
                     #endif
                  }
               }
            }                               /* SIDE == CblasRight             */
         } else                             /* DIRECT != LAForward            */
         {
/*
 *           Let  V =  ( V1 )
 *                     ( V2 )    (last K rows)
 *           where  V2  is unit upper triangular.
 */
            if (SIDE == CblasLeft)
            {
/*             Form  H * C  or  H' * C  where  C = ( C1 )
 *                                                  ( C2 )
 *
 *              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
 *
 *              W := C2'
 *
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(N, C +((M-K+j) SHIFT) , LDC, WORK+j*LDWORK2, 1);
/*                For complex, make it conjugate                              */
                  #ifdef TCPLX
                     ATL_lacgv(N,  WORK+j*LDWORK2, 1);
                  #endif
               }

/*
 *              W := W * V2
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                          CblasUnit, N, K, ONE, V+((M-K) SHIFT), LDV, WORK,
                          LDWORK);

               if (M > K)
               {

/*
 *                 W := W + C1'*V1
 *
 */
                  cblas_gemm(CblasColMajor, MY_TRANS, CblasNoTrans, N, K, M-K,
                             ONE, C, LDC, V, LDV, ONE, WORK, LDWORK);
               }

/*
 *              W := W * T'  or  W * T
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, CTRANST,
                          CblasNonUnit, N, K, ONE, T, LDT, WORK, LDWORK);

/*
 *             C := C - V * W'
 */
               if (M > K)
               {

/*
 *                 C1 := C1 - V1 * W'
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, MY_TRANS, M-K,
                             N, K, NONE, V, LDV, WORK, LDWORK, ONE, C, LDC);
               }

/*
 *              W := W * V2'
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, MY_TRANS,
                          CblasUnit, N, K, ONE, V+((M-K) SHIFT), LDV,
                          WORK, LDWORK);
/*
 *              C2 := C2 - W'
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<N; i++)
                  {
                     #ifdef TREAL
                        C[M-K+j+i*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[((M-K+j) SHIFT)+i*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
/*                      Conjugate is taken, hence the addition                */
                        C[((M-K+j) SHIFT)+1+i*LDC2] +=
                                                   WORK[(i SHIFT)+1+j*LDWORK2];
                    #endif
                  }
               }
            } else if (SIDE == CblasRight)
            {
/*
 *              Form  C * H  or  C * H'  where  C = ( C1  C2 )
 *
 *              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
 *
 *              W := C2
 *
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(M, C+(N-K+j)*LDC2, 1, WORK+(j*LDWORK2), 1);
               }

/*
 *              W := W * V2
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                          CblasUnit, M, K, ONE, V+((N-K) SHIFT), LDV, WORK,
                          LDWORK);
               if (N > K)
               {
/*
 *                 W := W + C1 * V1
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M,
                             K, N-K, ONE, C, LDC, V, LDV, ONE, WORK, LDWORK);
               }

/*
 *              W := W * T  or  W * T'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, CTRANS,
                          CblasNonUnit, M, K, ONE, T, LDT, WORK, LDWORK);

/*
 *              C := C - W * V'
 */

               if (N > K)
               {
/*
 *                 C1 := C1 - W * V1'
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, MY_TRANS, M, N-K,
                             K, NONE, WORK, LDWORK, V, LDV, ONE, C, LDC);
               }

/*
 *              W := W * V2'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, MY_TRANS,
                          CblasUnit, M, K, ONE, V+((N-K) SHIFT),
                          LDV, WORK, LDWORK);

/*
 *              C2 := C2 - W
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<M; i++)
                  {
                     #ifdef TREAL
                        C[i+(N-K+j)*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[(i SHIFT)+(N-K+j)*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
                        C[(i SHIFT)+(N-K+j)*LDC2+1] -=
                                                   WORK[(i SHIFT)+j*LDWORK2+1];
                     #endif
                  }
               }
            }                               /* END IF SIDE == CblasRight      */
         }
      } else if (STOREV == LARowStore)
      {
         if (DIRECT == LAForward)
         {

/*           Let  V =  ( V1  V2 )    (V1: first K columns)
 *           where  V1  is unit upper triangular.
 */
            if (SIDE == CblasLeft)
            {
/*
 *              Form  H * C  or  H' * C  where  C = ( C1 )
 *
 *              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
 *
 *              W := C1'
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(N, C+(j SHIFT), LDC, WORK+j*LDWORK2, 1);
                  #ifdef TCPLX
                     ATL_lacgv(N,  WORK+j*LDWORK2, 1);
                  #endif
               }
/*
 *              W := W * V1'
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, MY_TRANS,
                          CblasUnit, N, K, ONE, V, LDV, WORK, LDWORK);

               if (M > K)
               {
/*
 *                 W := W + C2'*V2'
 */
                  cblas_gemm(CblasColMajor, MY_TRANS, MY_TRANS, N, K, M-K,
                             ONE, C+(K SHIFT), LDC, V+(K*LDV2), LDV, ONE, WORK,
                             LDWORK);
               }

/*
 *              W := W * T'  or  W * T
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CTRANST,
                          CblasNonUnit, N, K, ONE, T, LDT, WORK, LDWORK);

/*
 *              C := C - V' * W'
 */
               if (M > K)
               {
/*
 *                 C2 := C2 - V2' * W'
 */
                  cblas_gemm(CblasColMajor, MY_TRANS, MY_TRANS, M-K, N, K,
                             NONE, V+(K*LDV2), LDV, WORK, LDWORK,
                             ONE, C+(K SHIFT), LDC);
               }

/*
 *              W := W * V1
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CblasNoTrans,
                          CblasUnit, N, K, ONE, V, LDV, WORK, LDWORK);

/*
 *              C1 := C1 - W'
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<N; i++)
                  {
                     #ifdef TREAL
                        C[j+i*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[(j SHIFT) +i*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
/*                      Conjugate is taken, hence the addition                */
                        C[(j SHIFT)+i*LDC2+1] += WORK[(i SHIFT)+j*LDWORK2+1];
                     #endif
                  }
               }
            } else if (SIDE == CblasRight)
            {

/*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
 *
 *              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
 *
 *              W := C1
 */

               for (j=0; j<K; j++)
               {
                  cblas_copy(M, C+j*LDC2, 1, WORK+j*LDWORK2, 1);
               }

/*
 *              W := W * V1'
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, MY_TRANS,
                          CblasUnit, M, K, ONE, V, LDV, WORK, LDWORK);

               if (N > K)
               {
/*
 *                 W := W + C2 * V2'
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, MY_TRANS,
                             M, K, N-K, ONE, C+(K*LDC2), LDC, V+(K*LDV2),
                             LDV, ONE, WORK, LDWORK);

               }
/*
 *              W := W * T  or  W * T'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper, CTRANS,
                          CblasNonUnit, M, K, ONE, T, LDT, WORK, LDWORK);
/*
 *              C := C - W * V
 */
               if (N > K)
               {
/*
 *                 C2 := C2 - W * V2
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans,
                             M, N-K, K, NONE, WORK, LDWORK, V+(K*LDV2),
                             LDV, ONE, C+(K*LDC2), LDC);
               }
/*
 *              W := W * V1
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasUpper,
                          CblasNoTrans, CblasUnit, M, K, ONE, V, LDV,
                          WORK, LDWORK);
/*
 *              C1 := C1 - W
 *
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<M; i++)
                  {
                     #ifdef TREAL
                        C[i+j*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[(i SHIFT)+j*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
                        C[(i SHIFT)+1+j*LDC2] -= WORK[(i SHIFT)+1+j*LDWORK2];
                     #endif
                  }
               }
            }                               /* END if SIDE == CblasRight      */
         } else                             /* DIRECT != LAForward            */
         {
/*          Let  V =  ( V1  V2 )    (V2: last K columns)
 *           where  V2  is unit lower triangular.
 */
            if (SIDE == CblasLeft)
            {

/*              Form  H * C  or  H' * C  where  C = ( C1 )
 *                                                  ( C2 )
 *
 *              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
 *
 *              W := C2'
 *
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(N, C+((M-K+j) SHIFT), LDC, WORK+j*LDWORK2, 1);
/*                For complex, make it conjugate                              */
                  #ifdef TCPLX
                     ATL_lacgv(N,  WORK+j*LDWORK2, 1);
                  #endif
               }

/*
 *              W := W * V2'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, MY_TRANS,
                          CblasUnit, N, K, ONE, V+(M-K)*LDV2, LDV,
                          WORK, LDWORK);

               if (M > K)
               {
/*
 *                 W := W + C1'*V1'
 */
                  cblas_gemm(CblasColMajor, MY_TRANS, MY_TRANS, N, K, M-K,
                             ONE, C, LDC, V, LDV, ONE, WORK, LDWORK);
               }

/*
 *              W := W * T'  or  W * T
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, CTRANST,
                          CblasNonUnit, N, K, ONE, T, LDT, WORK, LDWORK);

/*
 *              C := C - V' * W'
 */
               if (M > K)
               {
/*
 *                 C1 := C1 - V1' * W'
 *
 */
                  cblas_gemm(CblasColMajor, MY_TRANS, MY_TRANS, M-K, N, K,
                             NONE, V, LDV, WORK, LDWORK, ONE, C, LDC);
               }

/*
 *              W := W * V2
 *
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower,
                          CblasNoTrans, CblasUnit, N, K, ONE, V+(M-K)*LDV2,
                          LDV, WORK, LDWORK);
/*
 *              C2 := C2 - W'
 *
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<N; i++)
                  {
                     #ifdef TREAL
                        C[M-K+j+i*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[((M-K+j) SHIFT) +i*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
/*                      Conjugate is taken, hence the addition                */
                        C[((M-K+j) SHIFT)+i*LDC2+1] +=
                                                   WORK[(i SHIFT)+j*LDWORK2+1];
                     #endif
                  }
               }
            } else if (SIDE == CblasRight)
            {

/*             Form  C * H  or  C * H'  where  C = ( C1  C2 )
 *
 *              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
 *
 *              W := C2
 */
               for (j=0; j<K; j++)
               {
                  cblas_copy(M, C+(N-K+j)*LDC2, 1, WORK+j*LDWORK2, 1);
               }

/*
 *              W := W * V2'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, MY_TRANS,
                          CblasUnit, M, K, ONE, V+(N-K)*LDV2, LDV,
                          WORK, LDWORK);

               if (N > K)
               {
/*
 *                 W := W + C1 * V1'
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, MY_TRANS, M, K,
                             N-K, ONE, C, LDC, V, LDV, ONE, WORK, LDWORK);
               }
/*
 *              W := W * T  or  W * T'
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, CTRANS,
                          CblasNonUnit, M, K, ONE, T, LDT, WORK, LDWORK);
/*
 *              C := C - W * V
 */
               if (N > K)
               {
/*
 *                 C1 := C1 - W * V1
 */
                  cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M, N-K,
                             K, NONE, WORK, LDWORK, V, LDV, ONE, C, LDC);
               }

/*
 *              W := W * V2
 */
               cblas_trmm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                          CblasUnit, M, K, ONE, V+(N-K)*LDV2, LDV,
                          WORK, LDWORK);

/*
 *              C1 := C1 - W   TODO: check this  C2 : = C2 - W
 */
               for (j=0; j<K; j++)
               {
                  for (i=0; i<M; i++)
                  {
                     #ifdef TREAL
                        C[i+(N-K+j)*LDC] -= WORK[i+j*LDWORK];
                     #else
                        C[(i SHIFT)+(N-K+j)*LDC2] -= WORK[(i SHIFT)+j*LDWORK2];
                        C[(i SHIFT)+1+(N-K+j)*LDC2] -=
                                                   WORK[(i SHIFT)+1+j*LDWORK2];
                     #endif
                  }
               }
            }                               /* END IF SIDE == CblasRight      */
         }                                  /* END else DIRECT != LAForward   */
      }                                     /* END if STOREV == LARowwise     */
      return;
}                                           /* END ATL_larfb                  */

