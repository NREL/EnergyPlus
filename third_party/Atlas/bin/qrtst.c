/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 Siju Samuel
 *
 * Code contributers : Siju Samuel, R. Clint Whaley, Anthony M. Castaldo
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
 *  This program is a tester for QR, RQ, LQ and QL factorization routines.
 */

#include "atlas_misc.h"
#include "atlas_lapack.h"
#include "cblas.h"
#include "atlas_cblastypealias.h"
#include "atlas_tst.h"
#ifndef TimeF77
   #include "clapack.h"
#endif

#ifdef ATL_FULL_LAPACK
   #include "atlas_C2Flapack.h"

   #ifdef TREAL
      #define LA_LQ2Q Mjoin(Mjoin(ATL_C2F,PRE),ormlq)
   #else
      #define LA_LQ2Q Mjoin(Mjoin(ATL_C2F,PRE),unmlq)
   #endif
   #ifdef TREAL
      #define LA_RQ2Q Mjoin(Mjoin(ATL_C2F,PRE),ormrq)
   #else
      #define LA_RQ2Q Mjoin(Mjoin(ATL_C2F,PRE),unmrq)
   #endif
   #ifdef TREAL
      #define LA_QL2Q Mjoin(Mjoin(ATL_C2F,PRE),ormql)
   #else
      #define LA_QL2Q Mjoin(Mjoin(ATL_C2F,PRE),unmql)
   #endif
   #ifdef TREAL
      #define LA_QR2Q Mjoin(Mjoin(ATL_C2F,PRE),ormqr)
   #else
      #define LA_QR2Q Mjoin(Mjoin(ATL_C2F,PRE),unmqr)
   #endif
#endif

#ifdef GCCWIN
   ___main(){} __main(){} MAIN__(){} _MAIN_(){}
#endif
#define PRINT_1    0
#define TESTCORRECT  1

#ifdef TREAL
   static const TYPE ONE = ATL_rone;
   static const TYPE ZEROVAL = ATL_rzero;
   #define MY_TRANS CblasTrans
#else
   static const TYPE ONE[2] = {ATL_rone, ATL_rzero};
   static const TYPE ZEROVAL[2] = {ATL_rzero, ATL_rzero};
   #define MY_TRANS CblasConjTrans
#endif

#ifdef TimeF77
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,f77gelqf) \
         (Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   { \
      if ((lw_) != -1) \
      { \
         ATL_assert(!Mjoin(Mjoin(clapack_,PRE),gelqf) \
            (CblasColMajor, M_, N_, A_, lda_, tau_)) \
      } \
      else (wrk_)[0] = 0; \
   }
#elif defined(ATL_USEPTHREADS)
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,tgelqf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #define test_gelqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,gelqf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
#ifdef TimeF77
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,f77gerqf) \
         (Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   { \
      if ((lw_) != -1) \
      { \
         ATL_assert(!Mjoin(Mjoin(clapack_,PRE),gerqf) \
            (CblasColMajor, M_, N_, A_, lda_, tau_)) \
      } \
      else (wrk_)[0] = 0; \
   }
#elif defined(ATL_USEPTHREADS)
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,tgerqf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #define test_gerqf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,gerqf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
#ifdef TimeF77
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,f77geqlf) \
         (Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   { \
      if ((lw_) != -1) \
      { \
         ATL_assert(!Mjoin(Mjoin(clapack_,PRE),geqlf) \
            (CblasColMajor, M_, N_, A_, lda_, tau_)) \
      } \
      else (wrk_)[0] = 0; \
   }
#elif defined(ATL_USEPTHREADS)
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,tgeqlf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #define test_geqlf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,geqlf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
#ifdef TimeF77
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,f77geqrf) \
         (Major_, M_, N_, A_, lda_, tau_, wrk_, lw_))
#elif defined(TimeC)
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
   { \
      if ((lw_) != -1) \
      { \
         ATL_assert(!Mjoin(Mjoin(clapack_,PRE),geqrf) \
            (CblasColMajor, M_, N_, A_, lda_, tau_)) \
      } \
      else (wrk_)[0] = 0; \
   }
#elif defined(ATL_USEPTHREADS)
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,tgeqrf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#else
   #define test_geqrf(Major_, M_, N_, A_, lda_, tau_, wrk_, lw_) \
      ATL_assert(!Mjoin(PATL,geqrf) \
         (M_, N_, A_, lda_, tau_, wrk_, lw_))
#endif
/*
 * Print a Matrix
 */
void printMatrix(char *msg, TYPE *A, int M, int N, int lda)
{
   if(PRINT_1)
      Mjoin(PATL,geprint)(msg, M, N, A, lda);
}

/*
 * Print a Vector
 */
void printVector( char * msg, TYPE *X, int N)
{
   if(PRINT_1)
      Mjoin(PATL,geprint)(msg, N, 1, X, N);
}

/*
 * Copy Vector X1[1:n] to  -1.0*X1[1:n]
 */
void   makeVectorToNegVal( TYPE *X1, int size)
{
   int  i, size2;
   size2 = size SHIFT;                // 2*size, for  complex  numbers

   for ( i= 0; i < size2; i++)
   {
      X1[i] = -1.0*X1[i];
   }
}


int AllocationErr( char * msg)
{
   printf("Error while allocating %s\n", msg);
   exit(-1);
}

/*
 * Returns a duplicate of the A matrix, with new leading dimension
 * lda   : leading dimension of A
 * ldc   : leading dimension of Duplicate Matrix
 */
static TYPE *DupMat(enum ATLAS_ORDER Order, int M, int N, TYPE *A, int lda,
                     int ldc)
{
   int i, j, M2;
   const int ldc2 = (ldc SHIFT), lda2 = (lda SHIFT);
   TYPE *C;
   if (Order == CblasRowMajor)
   {
      i = M;
      M = N;
      N = i;
   }
   M2 = M SHIFT;
   ATL_assert(ldc >= M);
   C = malloc(ATL_MulBySize(ldc)*N);
   ATL_assert(C);
   for (j=0; j != N; j++)
   {
      for (i=0; i != M2; i++) C[i] = A[i];
      C += ldc2;
      A += lda2;
   }
   return(C-N*ldc2);
}

/*
 * Make input square matrix A[NxN] as Identity Matrix.
 */
void  makeIdenty(TYPE *A, int N, int lda)
{
   int i, j, lda2;

   lda2 = lda SHIFT;                  // 2*lda for complex

   for(i=0; i<N; i++)
   {
      for(j=0; j<N; j++)
      {
         #ifdef TREAL
            if(i==j){
               *(A+j*lda2+i) = 1.0;
            }
            else {
               *(A+j*lda2+i) = 0.0;
            }
         #else
            if(i==j){
               *(A+j*lda2 + (i SHIFT ) ) = 1.0;
               *(A+j*lda2+ (i SHIFT ) + 1) = 0.0;
            }
            else {
               *(A+j*lda2+ (i SHIFT ) ) = 0.0;
               *(A+j*lda2+ (i SHIFT ) + 1) = 0.0;
            }
          #endif
      }
   }
}

/*
 * Allocate a space for ldmxN for 'real'.
 * For Complex allocate space for (2*ldm)xN.
 * It also takes care of the precision.
 * Then generate matrix for MxN, real/complex values based on the precision.
 */
TYPE *GetGE(int M, int N, int lda)
{
   TYPE *A;
   A = malloc(ATL_MulBySize(lda)*N);
   if (A) Mjoin(PATL,gegen)(M, N, A, lda, M*N+lda);
   return(A);
}

/*
 * Populate Vi vector  from the given ROW of A Matrix.  A has the pointer
 * to the first element  in the row. N represent the size of row.
 * 'numberOfVielement' denotes  (N - number of zero elements in Vi)
 *
 *   if       ROW-> v v v v R R R (example)
 *   then     VI->  v v v v 1 (example)
 *
 *   NOTE : For Complex conjugate of v(i) is taken
 *
 */
void populateViForRQ (TYPE *Vi, TYPE * A, int lda,
                        int numberOfVielement)
{
   int j =0;
   int i =0;
   int lda2 = lda SHIFT;

   #ifdef TREAL
      for ( j = 0; j < (numberOfVielement -1 ); j++)
      {
         Vi[j]= *(A+j*lda);
      }
      Vi[numberOfVielement -1 ] = 1.0;
   #else
      for ( j = 0; j < ( (numberOfVielement -1)); j++)
      {
         Vi[(j SHIFT) ]= *(A+j*lda2);

/*       Conjugate is taken                                                   */
         Vi[(j SHIFT) +1 ]= 0.0 - *(A+j*lda2 + 1);
      }
      Vi[ ( (numberOfVielement -1) SHIFT)  ] = 1.0;
      Vi[ ( (numberOfVielement -1) SHIFT) + 1  ] = 0.0;
   #endif
}

/*
 * Populate Vi vector  from the given ROW of A Matrix.  A has the pointer
 * to the first element  in the row. N represent the size of row.
 * 'numberOfVielement' denotes  (N - number of zero elements in Vi)
 *
 *    vi -> L L v v v v v  (example)
 *           To
 *    vi -> 1 v v v v v  (example)
 *
 *     NOTE : Conjugate is taken for complex
 *
 */
void populateViForLQ (TYPE *Vi, TYPE * A, int N, int lda,
                                    int numberOfVielement )
{
   int j =0;
   int i =0;
   int lda2 = lda SHIFT;

   #ifdef TREAL
      Vi[0] = 1.0;
      for (j=1; j < numberOfVielement; j++)
      {
         Vi[j]= *(A+ (( N - numberOfVielement + j)*lda) );
      }
   #else
      Vi[0] = 1.0;
      Vi[1] = 0.0;
      for (j=1; j < numberOfVielement; j++)
      {
         Vi[(j SHIFT) ]= *(A+ (( N - numberOfVielement + j)*lda2) );
         Vi[(j SHIFT) + 1]= 0.0 - *(A+ (( N - numberOfVielement + j)*lda2)+1);
      }
   #endif
}

void populateViForQR(TYPE *Vi, TYPE * A, int M,  int numberOfVielement)
{
   int j = 0;

   #ifdef TREAL
      Vi[0] = 1.0;
   #else
      Vi[0] = 1.0;
      Vi[1] = 0.0;
   #endif
   for (j=1; j<numberOfVielement;  j++)
   {
      #ifdef TREAL
         Vi[j] = A[j+ M-numberOfVielement];
      #else
         Vi[(j SHIFT)] = A[((j+ M-numberOfVielement) SHIFT) ];
         Vi[(j SHIFT)+1] = A[((j+ M-numberOfVielement) SHIFT) + 1 ];
      #endif
   }
}

/*
 * Populate Vi vector  from the given COLUMN of A Matrix.  A has the pointer
 * to the top of the column.
 * 'numberOfVielement' denotes  (M - number of zero elements in Vi)
 *
 *                      v                        v
 *                      v                        v
 *                      v            TO          v
 *                      R                        1
 *                      R
 *                      R
 */
void populateViForQL(TYPE *Vi, TYPE * A,  int numberOfVielement)
{
   int j =0;

   #ifdef TREAL
      for (j=0; j< (numberOfVielement-1);  j++)
      {
         Vi[j] = A[j];
      }
      Vi[numberOfVielement -1 ] = 1.0;
   #else
      for (j=0; j< ( (numberOfVielement-1) SHIFT );  j++)
      {
         Vi[j ] = A[j];
      }
      Vi[(numberOfVielement -1) SHIFT ] = 1.0;
      Vi[ ((numberOfVielement -1) SHIFT ) + 1 ] = 0.0;
   #endif
}

/*  Make Matrix A to Upper Triangular or Upper Trapezoidal to make R matrix
 *  as per QR factorization. (Refer lapack for how the values are stored in A)
 */
void makeToUTForQR(TYPE *A , int lda, int M, int N)
{
   int i, j;
   int lda2 = lda SHIFT;              // 2*lda for complex

   for(i=0; i<M; i++)
   {
      for(j=0; (j<i && j < N) ; j++)
      {
         #ifdef TREAL
            *(A+j*lda2+i SHIFT) =  0.0;
         #else
            *(A+j*lda2+(i SHIFT)) =  0.0;
            *(A+j*lda2+(i SHIFT) + 1) =  0.0;
         #endif
      }
   }
}

/*
 * Make  the lower portion of the matrix to zero w.r.t to the
 * diagonal from bottom left (Note: change to standard naming)
 */
void makeToUTForRQ(TYPE *A , int lda, int M, int N )
{
   int i, j , lda2;
   int k =0;

   lda2 = lda SHIFT;

   int mn = Mmin(M,N) ;

   for(i=(M-1); i >= (M -mn) ; i--)
   {
      k++;

      for(j=(N-1-k ); j>=0  ; j--)
      {
         #ifdef TREAL
            *(A+j*lda2+i) =  0.0;
         #else
            *(A+j*lda2+ (i SHIFT) ) =  0.0;
            *(A+j*lda2+ (i SHIFT) + 1) =  0.0;
         #endif
      }
   }
}

/*
 * Make  the upper portion of the matrix to zero w.r.t to the
 * diagonal from upper right  (Note: change to standard naming)
 */
void makeToLTForLQ(TYPE *A , int lda, int M, int N )
{
   int i, j, lda2 ;
   int k =0;

   lda2 = lda SHIFT;
   int mn = Mmin(M,N) ;

   for(i=0;  i< mn  ; i++)
   {
      for(j= (i+1); j < N ; j++)
      {
         #ifdef TREAL
             *(A+j*lda2+(i SHIFT) ) =  0.0;
         #else
             *(A+j*lda2+(i SHIFT)) =  0.0;
             *(A+j*lda2+(i SHIFT) + 1) =  0.0;
         #endif
      }
   }
}

/*
 * Make  the upper portion of the matrix to zero w.r.t to the
 * diagonal from bottom left (Note: change to standard naming)
 */
void makeToLTForQL(TYPE *A , int lda, int M, int N )
{
   int i, j , lda2;
   int k =0;

   lda2 = lda SHIFT;

   int mn = Mmin(M,N) ;

   for(j=(N-1); j >= (N -mn) ; j--)
   {
      k++;

      for(i=(M-1-k ); i>=0  ; i--)
      {
         #ifdef TREAL
            *(A+j*lda2+i) =  0.0;
         #else
            *(A+j*lda2+ (i SHIFT) ) =  0.0;
            *(A+j*lda2+ (i SHIFT) + 1) =  0.0;
         #endif
      }
   }
}

static TYPE lqtest(int M, int N, int lda, int flushKB, double *time)
{
   TYPE *A, *TAU, *TAUNEG,  *WORK, *WORKF,  *L, *W ;
   TYPE *Q, *VI;
   TYPE *AORIG;
   int viCounter =0;
   TYPE dtmp, dtmp1;
   double t0;

   int ITER ;

   #ifdef TREAL
      TYPE NEGTAUVAL = ATL_rzero;
   #else
      TYPE NEGTAUVAL[2] = {ATL_rzero, ATL_rzero};
   #endif

   int LWORK;
   TYPE normA, eps, resid;
   int i, j, k, ldn, ldm, ldmn;

   ldm = M;
   ldn = N;
   const int MNmin = Mmin(M,N);
   const int Maxmn = Mmax(M,N);
   ldmn = MNmin;

   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize                                                  */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");

if(TESTCORRECT)
{
/* Allocate A Original and copy from A                                        */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);
}

/* Call to get the  size of workspace required                                */
   test_gelqf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);

/* Get Work and LWORK                                                         */
   LWORK = dtmp;
   WORK = malloc(ATL_MulBySize(LWORK));
   if (WORK == NULL) return(-1);

   TAU = malloc(ATL_MulBySize(MNmin));
   if (TAU == NULL) AllocationErr("TAU");
   if (flushKB > 0)
   {
      t0 = ATL_flushcache(flushKB*1024);
      t0 += ATL_flushcache(-1);
   }
   t0 = time00();
   test_gelqf(CblasColMajor, M, N, (TYPE*)(A), lda,
              (TYPE*)(TAU), (TYPE*)(WORK), LWORK);
   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);
if(TESTCORRECT)
{
/* Allocate and Copy TAU to TAUNEG                                            */
   TAUNEG = DupMat(CblasColMajor, MNmin, 1, TAU, ldmn, ldmn);

   makeVectorToNegVal(TAUNEG, MNmin);

/* Allocate Vi, based on Size of N                                            */
   VI = malloc(ATL_MulBySize(N));
   if (VI == NULL) AllocationErr("VI");

/* Allocate Q and Q-temp,   lda equals N                                      */
   Q = malloc(ATL_MulBySize(ldn)*N);
   if (Q == NULL) AllocationErr("Q");

   W = malloc(ATL_MulBySize(ldn));
   if (W == NULL) AllocationErr("W");

   makeIdenty(Q, N, ldn);              //Q: Size of Q mat  and lda is N

/* Copy A[M, N] to L and then Modify the upper part of DIAG w.r.t top left    */
   L= DupMat(CblasColMajor, M, N, A, lda, ldm);
   makeToLTForLQ(L , ldm, M, N );

   printMatrix("AORIG",AORIG, M, N, lda);
   printMatrix("A",A, M, N, lda);
   printMatrix("Q MARTIX ", Q, N, N, ldn);
   printMatrix("L matrix", L, M, N, ldm);
   printVector("TAU", TAU, MNmin);
   printVector("TAUNEG", TAUNEG, MNmin);

   #ifdef ATL_FULL_LAPACK
      ATL_assert(!LA_LQ2Q(CblasRight, CblasNoTrans, N, N, MNmin, A, lda, TAU,
                           Q, ldn));
   #else
/*
 *    Form Q Matrix
 *    Q = H(K).H(K-1)..........H(2).H(1).
 *        where H(i) = I -tau(i).v(i).v(i)^T.
 *
 *        Forming H Matrix from v and propogating the results, has resulted in
 *        taking a lot of CPU. So the following logic for forming Q is
 *        implemented
 *
 *          Q(Current) =   Q(Previous) * H (Current)
 *          Equivalent to,
 *              Q(K-1) =   Q(k) * H(K-1)
 *                        Note : Q matrix will get replaced in  each iterations
 *        In each Q(k) will hold the  previous Q values as below
 *                    1 0
 *                    0 Q
 *
 *          =>   Q(k) * (I - tau(k-1) v(k-1) v(k-1)^T)
 *          =>  Q(k) - tau(k-1)  w v(k-1)^T  where
 *                                w => Q(k) v(k-1)
 *
 *       NOTE : For complex
 *    Q = H(k)^T.H(k-1)^T..........H(2)^T.H(1)^T.
 *        Transpose is applied by taking the conjugate transpose of
 *        tau.
 */
      ITER = 1;
      for (i =  MNmin-1; i >= 0 ;  i-- )
      {
/*
 *       Make Vi And take its conjugate for Complex numbers. For complex,
 *       the conjugate value is stored in the output.
 *       In the first iteration, gets V(k)
 */

         populateViForLQ(VI, (A+((MNmin-ITER) SHIFT)), N, lda, (N-MNmin+ITER));
         printVector("VI",VI, M);

         #ifdef TREAL
               NEGTAUVAL=TAUNEG[i];
         #else
            NEGTAUVAL[0] = TAUNEG[( i SHIFT) ];
/*          Conjugate Transpose                                               */
            NEGTAUVAL[1] = 0.0 - TAUNEG[ (i SHIFT) + 1 ];
         #endif

/*
 * Q will be MxM matrix, intially populated as I matrix.
 * In each iteration Q will hold the  previous Q values as below
 *   Q 0
 *   0 1
 * Step 1
 * Q(k)*v
 */
         cblas_gemv(CblasColMajor, CblasNoTrans, (N - MNmin + ITER ),
                    (N - MNmin + ITER ) , ONE, (Q + ((MNmin -ITER)*(ldn SHIFT))
                     + ((MNmin - ITER) SHIFT)) , ldn, VI, 1, ZEROVAL, W, 1);

/* Perform Q(k) - tau W v^T                                                   */
         #ifdef TREAL
            cblas_ger(CblasColMajor, (N - MNmin + ITER ), (N - MNmin + ITER ),
                      NEGTAUVAL, W, 1, VI, 1, (Q + ((MNmin -ITER)*(ldn SHIFT))
                      + ((MNmin - ITER) SHIFT)) , ldn );
          #else
            cblas_gerc(CblasColMajor, (N - MNmin + ITER ), (N - MNmin + ITER ),
                      NEGTAUVAL, W, 1, VI, 1, (Q + ((MNmin -ITER)*(ldn SHIFT))
                      + ((MNmin - ITER) SHIFT)), ldn );
         #endif

         printMatrix("Q", Q, N, N, ldn);
         ITER++;
      }
   #endif

/*
 * Calculate LQ  and Compare with AORIG
 * L*Q -> A
 */
   cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M  , N,
              N , ONE, L, ldm, Q, ldn, ZEROVAL, A , lda);

   printMatrix("LQ Matrix ", A, M, N, lda);


/* Get the  Norm of RQ      s A has the result                                */
   normA = Mjoin(PATL,genrm1)(M, N, AORIG, lda);

/* Find residue || A - Q*L ||  /   || A ||                                    */
   resid = Mjoin(PATL,gediffnrm1)(M, N, AORIG, lda, A, lda);
   resid /= (normA * eps * Mmin(M,N));

}//(TESTCORRECT)
   if(A) free(A);
   if(TAU) free(TAU);
   if(TAUNEG) free(TAUNEG);
   if(WORK) free(WORK);
   if(L) free(L);
   if(Q) free(Q);
   if(VI) free (VI);
   if(AORIG) free(AORIG);

   return(resid);
}

static TYPE qltest(int M, int N, int lda, int flushKB, double *time)
{
   TYPE *A, *TAU, *TAUNEG,  *WORK, *L, *W ;
   TYPE *Q, *VI;
   TYPE *AORIG;
   TYPE dtmp, dtmp1;
   double t0;

   int ITER;

   #ifdef TREAL
      TYPE NEGTAUVAL = ATL_rzero;
   #else
      TYPE NEGTAUVAL[2] = {ATL_rzero, ATL_rzero};
   #endif

   int LWORK;
   TYPE normA, eps, resid;
   int i, j, k, ldm, ldmn;
   int lda2 = lda SHIFT;

   ldm = M;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize                                                  */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");

if(TESTCORRECT)
{
/* Allocate A Original and copy from A                                        */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);
}

/* Call to get the  size of workspace required                                */
   test_geqlf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);

/* Get Work and LWORK                                                         */
   LWORK = dtmp;
   WORK = malloc(ATL_MulBySize(LWORK));
   if (WORK == NULL) return(-1);

   TAU = malloc(ATL_MulBySize(MNmin));
   if (TAU == NULL) AllocationErr("TAU");

   if (flushKB)
   {
      t0 = ATL_flushcache(flushKB*1024);
      t0 += ATL_flushcache(-1);
   }
   t0 = time00();
   test_geqlf(CblasColMajor, M, N, (TYPE*)(A), lda, (TYPE*)(TAU),
              (TYPE*)(WORK), LWORK);
   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);
if(TESTCORRECT)
{
/* Allocate and Copy TAU to TAUNEG                                            */
   TAUNEG = DupMat(CblasColMajor, MNmin, 1, TAU, ldmn, ldmn);

   makeVectorToNegVal(TAUNEG, MNmin);

/* Allocate Vi, based on Size of M                                            */
   VI = malloc(ATL_MulBySize(M));
   if (VI == NULL) AllocationErr("VI");

/* Allocate Q and Q-temp,   lda equals M                                      */
   Q = malloc(ATL_MulBySize(ldm)*M);
   if (Q == NULL) AllocationErr("Q");

   W = malloc(ATL_MulBySize(ldm));
   if (W == NULL) AllocationErr("W");

   makeIdenty(Q, M, ldm);                   /* Q: Size of Q mat and lda is M  */


/* Make L Matrix  from A  and lda as M                                        */
   L = malloc(ATL_MulBySize(ldm)*N);        /* Has size [M, N]                */
   if (L == NULL) return(-1);

/*
 * Copy A[M, N] to L and then Modify the Tringular part
 * above the diagonal w.r.t bottom right diagonal
 */
   L = DupMat(CblasColMajor, M, N, A, lda, ldm);

   makeToLTForQL(L , ldm, M, N );

   printMatrix("AORIG",AORIG, M, N, lda);
   printMatrix("A",A, M, N, lda);
   printMatrix("Q ", Q, M, M, ldm);
   printMatrix("L matrix", L, M, N, ldm);
   printVector("TAU", TAU, MNmin);
   printVector("TAUNEG", TAUNEG, MNmin);

   #ifdef ATL_FULL_LAPACK
      ATL_assert(!LA_QL2Q(CblasLeft, CblasNoTrans, M, M, MNmin,
                  A+(N-MNmin)*lda2, lda, TAU, Q, ldm));
   #else
/*
 *    Form Q Matrix
 *    Q = H(K).H(K-1)..........H(2).H(1).
 *        where H(i) = I -tau(i).v(i).v(i)^T.
 *
 *        Forming H Matrix from v and propogating the results, has resulted in
 *        taking a lot of CPU. So the following logic for forming Q is
 *        implemented
 *
 *          Q(Current) =  (I - tau v v^T) * Q(Previous)
 *          Equivalent to,
 *              Q(K+1) =   H(K+1) * Q(k)
 *                        Note : Q matrix will get replaced in  each iterations
 *        In each Q(k) will hold the  previous Q values as below
 *                    Q 0
 *                    0 1
 *
 *          =>   (I - tau(k+1) v(k+1) v(k-1)^T) * Q(k)
 *          =>  Q(k) - tau(k+1)  v(k+1) w^T  where
 *                                w => Q(k)^T v(k+1)
 */
      ITER = 1;
      for (i = 0; i <  MNmin; i++)
      {
/*       Make Vi                                                              */
         populateViForQL(VI, (A+((N-MNmin+ITER-1)*lda2)), (M-MNmin+ITER));

         printVector("VI",VI, M);

         #ifdef TREAL
            NEGTAUVAL=TAUNEG[i];
         #else
            NEGTAUVAL[0] = TAUNEG[( i SHIFT) ];
            NEGTAUVAL[1] = TAUNEG[ (i SHIFT) + 1 ];
         #endif
/*
 *       Q will be MxM matrix, intially populated as I matrix.
 *       In each iteration Q will hold the  previous Q values as below
 *         Q 0
 *          0 1
 *       Step 1
 *       Q(k)^T*v
 */
         cblas_gemv(CblasColMajor, MY_TRANS, (M - MNmin + ITER ), (M - MNmin + ITER ),
                              ONE, (Q), ldm, VI, 1, ZEROVAL, W, 1);

/* Perform Q(k) - tau W v^T                                                   */
         #ifdef TREAL
            cblas_ger(CblasColMajor, (M - MNmin + ITER ), (M - MNmin + ITER ),
                      NEGTAUVAL, VI, 1, W, 1, (Q) ,ldm);
         #else
            cblas_gerc(CblasColMajor, (M - MNmin + ITER ), (M - MNmin + ITER ),
                       NEGTAUVAL, VI, 1, W, 1, (Q),ldm);
         #endif

         printMatrix("Q", Q, M, M, ldm);
         ITER++;

      }
   #endif

/*
 * Calculate Q*R  and Compare with AORIG
 * Q*L -> A
 */
   cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M  , N,
              M , ONE, Q, ldm, L, ldm, ZEROVAL, A , lda);

   printMatrix("QL Matrix ", A, M, N, lda);

/* Get the  Norm of QL      s A has the result                                */
   normA = Mjoin(PATL,genrm1)(M, N, AORIG, lda);

/* Find residue || A - Q*L ||  /   || A ||                                    */
   resid = Mjoin(PATL,gediffnrm1)(M, N, AORIG, lda, A, lda);
   resid /= (normA * eps * Mmin(M,N));

} /*(TESTCORRECT) */
   if(A) free(A);
   if(TAU) free(TAU);
   if(TAUNEG) free(TAUNEG);
   if(WORK) free(WORK);
   if(L) free(L);
   if(Q) free(Q);
   if(VI) free (VI);
   if(AORIG) free(AORIG);


   return(resid);
}


/*
 * Calls gerq ( single, double, single complex and double complex ).
 * From A[MxN] Matrix returned from geqr routines, compute Q and R matrix.
 * Find the residual for ( A_original - RQ  ) operation.
 *
 *   Also, make H(k) = i-tau*vv'
 *   and compute Q = H(1)*H(2)...H(k)  where k = Min(M,N)
 *
 *   VI has size of N
 *
 */
static TYPE rqtest(int M, int N, int lda, int flushKB, double *time)
{
   TYPE *A, *TAU, *TAUNEG,  *WORK, *WORKF, *R, *W ;
   TYPE *Q, *VI;
   TYPE *AORIG;
   TYPE dtmp, dtmp1;
   double t0;

   int ITER;

   #ifdef TREAL
      TYPE NEGTAUVAL = ATL_rzero;
   #else
      TYPE NEGTAUVAL[2] = {ATL_rzero, ATL_rzero};
   #endif

   int LWORK;
   TYPE normA, eps, resid;
   int i, j, k, ldn, ldm, ldmn;
   int lda2 = lda SHIFT;

   ldm = M;
   ldn = N;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

/* Epsilon                                                                    */
   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize                                                  */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");
if(TESTCORRECT)
{
/* Allocate A Original and copy from A                                        */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);
}

/* Call to get the  size of workspace required                                */
   test_gerqf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);

/* Get Work and LWORK                                                         */
   LWORK = dtmp;
   WORK = malloc(ATL_MulBySize(LWORK));
   if (WORK == NULL) return(-1);

   TAU = malloc(ATL_MulBySize(MNmin));
   if (TAU == NULL) AllocationErr("TAU");

   if (flushKB)
   {
      t0 = ATL_flushcache(flushKB*1024);
      t0 += ATL_flushcache(-1);
   }
   t0 = time00();
   test_gerqf(CblasColMajor, M, N, (TYPE*)(A), lda, (TYPE*)(TAU),
              (TYPE*)(WORK), LWORK);
   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);
if(TESTCORRECT)
{
/* Allocate and Copy TAU to TAUNEG                                            */
   TAUNEG = DupMat(CblasColMajor, MNmin, 1, TAU, ldmn, ldmn);

   makeVectorToNegVal(TAUNEG, MNmin);

/* Allocate Vi, based on Size of N                                            */
   VI = malloc(ATL_MulBySize(N));
   if (VI == NULL) AllocationErr("VI");

/* Allocate Q and Q-temp,   lda equals N                                      */
   Q = malloc(ATL_MulBySize(ldn)*N);
   if (Q == NULL) AllocationErr("Q");

   W = malloc(ATL_MulBySize(N));
   if (Q == NULL) AllocationErr("W");

   makeIdenty(Q, N, ldn);                   /* Q: Size of Q mat and lda is N  */

/* Copy A[M, N] to R and then Modify the Lower part of DIAG                   */
   R = DupMat(CblasColMajor, M, N, A, lda, ldm);
   makeToUTForRQ(R , ldm, M, N );

   printMatrix("AORIG",AORIG, M, N, lda);
   printMatrix("A",A, M, N, lda);
   printMatrix("Q MARTIX ", Q, N, N, ldn);
   printMatrix("R matrix", R, M, N, ldm);
   printVector("TAU", TAU, MNmin);
   printVector("TAUNEG", TAUNEG, MNmin);

   #ifdef ATL_FULL_LAPACK
      ATL_assert(!LA_RQ2Q(CblasRight, CblasNoTrans, N, N, MNmin,
                          A+((M-MNmin)SHIFT), lda, TAU, Q, ldn));
   #else
/*     Form Q Matrix
 *     Q = H(1).H(2)..........H(k-1).H(k).
 *         where H(i) = I -tau(i).v(i).v(i)^T.
 *
 *         Forming H Matrix from v and propogating the results, has resulted in
 *         taking a lot of CPU. So the following logic for forming Q is
 *         implemented
 *
 *           Q(Current) = (Q(previous)* I - tau v v^T)
 *           Equivalent to,
 *               Q(K+1) =   Q(k) * H(K+1)
 *                         Note : Q matrix will get replaced in  each iterations
 *         In each Q(k) will hold the  previous Q values as below
 *                     Q 0
 *                     0 1
 *
 *           =>  Q(k) * (I - tau(k+1) v(k+1) v(k-1)^T)
 *           =>  Q(k) - tau(k+1)  w   v(k+1)^T  where
 *                                 w => Q(k) v(k+1)
 *
 *
 *        NOTE : For complex
 *         Q = H(1)^T.H(2)^T..........H(k-1)^T.H(k)^T.
 *         Transpose is applied by taking the conjugate transpose of
 *         tau.
 */

      ITER = 1;

      for (i = 0  ; i < MNmin ; i++)
      {
/*
 *       Make Vi And take its conjugate for Complex numbers. For complex,
 *       the conjugate value is stored in the output.
 */
         populateViForRQ(VI,(A+((M-MNmin+ITER-1) SHIFT)), lda, (N-MNmin+ITER));
         printVector("VI______________ ",VI, N);

         #ifdef TREAL
            NEGTAUVAL=TAUNEG[i];
         #else
            NEGTAUVAL[0] = TAUNEG[(i SHIFT) ];
/*          Conjugate Transpose                                               */
            NEGTAUVAL[1] = 0.0 - TAUNEG[(i SHIFT) + 1 ];
         #endif
/*
 * Q will be MxM matrix, intially populated as I matrix.
 * In each iteration Q will hold the  previous Q values as below
 *   Q 0
 *   0 1
 *
 * Step 1
 * Q(k)*v
 */
         cblas_gemv(CblasColMajor, CblasNoTrans, (N - MNmin + ITER ) ,
                    (N - MNmin + ITER ), ONE, (Q), ldn, VI, 1,
                    ZEROVAL, W, 1);

/*       Perform Q(k) - tau W v^T                                             */
         #ifdef TREAL
            cblas_ger(CblasColMajor, (N - MNmin + ITER ), (N - MNmin + ITER ),
                      NEGTAUVAL, W, 1, VI, 1, (Q) ,ldn);
         #else
            cblas_gerc(CblasColMajor, (N - MNmin + ITER ), (N - MNmin + ITER ),
                       NEGTAUVAL, W, 1, VI, 1, (Q),ldn);
         #endif

         printMatrix("Q ", Q, N, N, ldn);
         ITER++;
      }
   #endif
/*
 * Calculate Q*R  and Compare with AORIG
 * R*Q -> A
 */
   cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M  , N,
              N, ONE, R, ldm, Q, ldn, ZEROVAL, A , lda);

   printMatrix("RQ Matrix ", A, M, N, lda);

/* Get the  Norm of RQ      s A has the result                                */
   normA = Mjoin(PATL,genrm1)(M, N, AORIG, lda);

/* Find residue || A - R*Q ||  /   || A ||                                    */
   resid = Mjoin(PATL,gediffnrm1)(M, N, AORIG, lda, A, lda);
   resid /= (normA * eps * Mmin(M,N));

}/*(TESTCORRECT) */
   if(A) free(A);
   if(TAU) free(TAU);
   if(TAUNEG) free(TAUNEG);
   if(WORK) free(WORK);
   if(R) free(R);
   if(Q) free(Q);
   if(VI)  free (VI);
   if(AORIG) free(AORIG);

   return(resid);
}

/*
 * Calls geqr ( single, double, single complex and double complex ).
 * From A[MxN] Matrix returned from geqr routines, compute Q and R matrix.
 * Find the residual for ( A_original - QR  ) operation.
 *
 *   Also, make H(k) = i-tau*vv'
 *   and compute Q = H(1)*H(2)...H(k)  where k = Min(M,N)
 *
 */
TYPE qrtest(int M, int N, int lda, int flushKB, double *time)
{
   TYPE *A, *TAU, *TAUNEG,  *WORK, *WORKF,  *R , *W ;
   TYPE *Q, *VI;
   TYPE *AORIG ;
   TYPE  dtmp, dtmp1;
   double t0;

   int ITER;

   #ifdef TREAL
      TYPE NEGTAUVAL = ATL_rzero;
      TYPE AII ;
   #else
      TYPE NEGTAUVAL[2] = {ATL_rzero, ATL_rzero};
      TYPE AII[2] ;
   #endif


   int LWORK;
   TYPE normA, eps, resid;
   int i, j, k, ldm, ldmn ;
   int lda2 = lda SHIFT;

   ldm = M;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

/* Epsilon                                                                    */
   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize                                                  */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");

   printMatrix("AORIG from A ", A, M, N, lda);
if(TESTCORRECT)
{
/* Allocate A Original and copy from A                                        */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);
}
   test_geqrf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);
   LWORK = dtmp;

   WORK = malloc(ATL_MulBySize(LWORK));
   if (WORK == NULL) return(-1);

   TAU = malloc(ATL_MulBySize(MNmin));
   if (TAU == NULL) AllocationErr("TAU");

   if (flushKB)
   {
      t0 = ATL_flushcache(flushKB*1024);
      t0 += ATL_flushcache(-1);
   }
   t0 = time00();
   test_geqrf(CblasColMajor, M, N, (TYPE*)(A), lda,
               (TYPE*)(TAU), (TYPE*)(WORK), LWORK);
   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);
if(TESTCORRECT)
{
/* Allocate and Copy TAU to TAUNEG                                            */
   TAUNEG = DupMat(CblasColMajor, MNmin, 1, TAU, ldmn, ldmn);

   makeVectorToNegVal(TAUNEG, MNmin);

/* Allocate Q and Q-Temp,   lda equals M(ldm)                                 */
   Q = malloc(ATL_MulBySize(ldm)*M);
   if (Q == NULL) AllocationErr("Q");

   W = malloc(ATL_MulBySize(M));
   if (W == NULL) AllocationErr("W");

   makeIdenty(Q, M, ldm);

/*
 * Copy A[M, N of lda] to R[M, N od ldm] and then make the elements
 * below diagonals as zero
 */
   R = DupMat(CblasColMajor, M, N, A, lda, ldm);
   makeToUTForQR(R , ldm, M,N);

   printMatrix("AORIG", AORIG, M, N, lda);
   printMatrix("A", A, M, N, lda);
   printMatrix("Q", Q, M, M, ldm);
   printMatrix("R", R, M, N, ldm);
   printVector("TAU", TAU, MNmin);
   printVector("TAUNEG", TAUNEG, MNmin);

   #ifdef ATL_FULL_LAPACK
      ATL_assert(!LA_QR2Q(CblasLeft, CblasNoTrans, M, M, MNmin, A, lda, TAU,
                          Q, ldm));
   #else
/*    Form Q Matrix
 *    Q = H(1).H(2)..........H(k-1).H(k).
 *        where H(i) = I -tau(i).v(i).v(i)^T.
 *
 *        Forming H Matrix from v and propogating the results, has resulted in
 *        taking a lot of CPU. So the following logic for forming Q is
 *        implemented
 *
 *          Q(Current) = (I - tau v v^T)*Q(previous)
 *          Equivalent to,
 *              Q(K-1) =  ( H(k-1))* Q(k)
 *                        Note : Q matrix will get replaced in  each iterations
 *          =>  (I - tau(k-1) v(k-1) v(k-1)^T) Q(k)
 *          =>  Q(k) - tau(k-1) w v(k+1)^T where
 *                                w => Q(k)^T v(k-1)
 */

/*    Make diagonal element of A, which  holds VI to 1.0                      */
      for (i = 0; i < MNmin; i++)
      {
         #ifdef TREAL
            *(A  + i*lda + i ) = 1.0;
         #else
            *(A  + (i * ( lda SHIFT)) + ( i SHIFT))   = 1.0;
            *(A  + (i * ( lda SHIFT)) + ( i SHIFT ) + 1) = 0.0;
         #endif
      }

      ITER = 1;
      for (i =( MNmin - 1); i >= 0; i--)

      {

/*       Point Vi to  A(diagonal element )                                    */
         VI = (A + (i*(lda SHIFT)) + ( i SHIFT));

         #ifdef TREAL
            NEGTAUVAL=TAUNEG[i];
         #else
            NEGTAUVAL[0] = TAUNEG[(i SHIFT) ];
            NEGTAUVAL[1] = TAUNEG[ (i SHIFT) + 1 ];
         #endif

/*
 * Q will be MxM matrix, intially populated as I matrix.
 * In each Q will hold the  previous Q values as below
 *  1 0
 *  0 Q
 */

/*
 * Step 1.
 * Q(k)^T*v
 */
         cblas_gemv(CblasColMajor, MY_TRANS, (M - MNmin + ITER ) ,
                    (M - MNmin + ITER ), ONE, (Q + ((MNmin - ITER)*(ldm SHIFT))
                    +( (MNmin - ITER) SHIFT )), ldm, VI, 1, ZEROVAL, W, 1);

/*       Perform Q(k) - tau v W^T                                             */
         #ifdef TREAL
            cblas_ger(CblasColMajor, (M - MNmin + ITER ), (M - MNmin + ITER ),
                      NEGTAUVAL, VI, 1, W, 1, (Q + ((MNmin - ITER)*(ldm SHIFT))
                                 +( (MNmin - ITER) SHIFT )) ,ldm);
         #else
            cblas_gerc(CblasColMajor, (M - MNmin + ITER ), (M - MNmin + ITER ),
                      NEGTAUVAL, VI, 1, W, 1, (Q + ((MNmin - ITER)*(ldm SHIFT))
                                 +( (MNmin - ITER) SHIFT )) ,ldm);
         #endif

         printMatrix("Q", Q, M, M, ldm);
         ITER++;
      }
   #endif

/*
 * Calculate Q*R  and Compare with AORIG
 * Q*R -> A       Reuse A space
 */

   cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, M  , N,
              M , ONE, Q, ldm, R, ldm, ZEROVAL, A , lda);

   printMatrix("QR  A matrix", A, M, N, lda);

/* Get the  Norm of QR      as A has the result                               */
   normA = Mjoin(PATL,genrm1)(M, N, AORIG, lda);

/* Find residue || A - Q*R ||  /   || A ||                                    */
   resid = Mjoin(PATL,gediffnrm1)(M, N, AORIG, lda, A, lda);
   resid /= (normA * eps * Mmin(M,N));

} /*(TESTCORRECT) */
   if (A)   free(A);
   if (TAU) free(TAU);
   if (TAUNEG)   free(TAUNEG);
   if (WORK)  free(WORK);
   if (R)   free(R);
   if (Q)   free(Q);
   if (W)   free(W);
   if (AORIG)  free(AORIG);

   return(resid);
}


int GetMyReps(int N, int *nreps)
/*
 * Finds the correct nreps for this N
 */
{
   int n, i;

   n = *nreps++;
   for (i=n+n-2; i>=0; i -= 2)
   {
      if (N >= nreps[i])
         return(nreps[i+1]);
   }
   return(nreps[1]);
}

int RunCase(TYPE thresh, int flushKB, int side, int uplo, int M, int N, int lda)
/*
 * RETURNS: 0 if residual is <= thresh, otherwise 1
 */
{
   TYPE resid;
   TYPE (*qtest)(int, int, int, int, double*);
   double time;
   double Time2Flops(int rout, int UPLO, int M, int N, double time);
   char qs[4];

   qs[2] = '\0';
   if (side == LARight)
   {
      qs[0] = 'Q';
      if (uplo == LALower) { qtest = qltest; qs[1] = 'L'; }
      else { qtest = qrtest; qs[1] = 'R'; }
   }
   else
   {
      qs[1] = 'Q';
      if (uplo == LALower) { qtest = lqtest; qs[0] = 'L'; }
      else { qtest = rqtest; qs[0] = 'R'; }
   }
   resid = qtest(M, N, lda, flushKB, &time);
   printf("%2s  %3s %6d %6d %6d  %10.4e %11.2f  %9.2e\n", qs, "Col", M, N, lda,
         time, Time2Flops(LAgeqrf, uplo+side, M, N, time), resid);
   /* A temporary fix */
   if(resid == 0. && M > 10 && N > 10)  resid = 99999999; // Setting to a big num
   return(resid <= thresh ? 1 : 0);
}

int RunCases(TYPE thresh, int flushKB, int ldagap, int *NREPS,
            int *sides, int *uplos, int *Ms, int *Ns)
/*
 * RETURNS: number of failed cases
 */
{
   int u, s, o, m, n, M, lda, npass=0, ntest=0, nreps, k;
   printf("Rt  Maj      M      N    lda        TIME       MFLOP   RESIDUAL\n");
   printf("==  ===  =====  =====  =====  ==========  ==========  =========\n");

   for (s=1; s <= sides[0]; s++)            /* loop over sides                */
   {
      for (u=1; u <= uplos[0]; u++)         /* loop over uplos                */
      {
         for (o=0; o < 1; o++)              /* useless order loop, add later  */
         {
            for (n=1; n <= Ns[0]; n++)
            {
               for (m=1; m <= Ms[0]; m++)
               {
                  M = (Ms[m]) ? Ms[m]:Ns[n];
                  nreps = GetMyReps(Mmin(M, Ns[n]), NREPS);
                  for (k=0; k < nreps; k++)
                  {
                     npass += RunCase(thresh, flushKB, sides[s], uplos[u],
                                       M, Ns[n], M+ldagap);
                     ntest++;
                  }
               }
            }
         }
      }
   }
   if (ntest == npass)
      printf("\n%d cases ran, %d cases passed\n\n", ntest, npass);
   else
      printf("\n%d cases ran, %d cases failed, %d cases passed\n\n", ntest,
             ntest-npass, npass);

   return(ntest-npass);
}

#define CAN_NB 0
void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);

   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "      routs: geqrf, geqlf, gerqf, gelqf\n");
   fprintf(stderr, "   -T <thresh> : set residual error threshold\n");
   fprintf(stderr, "   -# <#> : repeat each timing # times\n");
   fprintf(stderr, "   -n <#> <N1> ... <N#>\n");
   fprintf(stderr, "   -N <Nstart> <Nend> <Ninc>\n");
   fprintf(stderr, "   -m <#> <M1> ... <M#>\n");
   fprintf(stderr, "   -M <Mstart> <Mend> <Minc>\n");
   fprintf(stderr, "   -a <ldagap> : lda = M + <ldagap> foreach M\n");
   fprintf(stderr, "   -f <flushKB> : flush at least this mem in LRU timers\n");
   fprintf(stderr, "   -S <#> <side1> ... <side#>\n");
   fprintf(stderr, "   -U <nuplo> <up1> ... <upN> : Vals: [u,l,q,g]\n");
   exit(ierr ? ierr : -1);
}

int *GetIntList1(int ival)
/*
 * returns integer array with iarr[0] = 1, iarr[1] = ival
 */
{
   int *iarr;
   iarr = malloc(2*sizeof(int));
   ATL_assert(iarr);
   iarr[0] = 1;
   iarr[1] = ival;
   return(iarr);
}

int *GetIntList2(int ival1, int ival2)
/*
 * returns integer array with iarr[0] = 1, iarr[1] = ival1, ival[2] = ival2
 */
{
   int *iarr;
   iarr = malloc(3*sizeof(int));
   ATL_assert(iarr);
   iarr[0] = 1;
   iarr[1] = ival1;
   iarr[2] = ival2;
   return(iarr);
}

int *GetIntList(int nargs, char **args, int i, int nmul)
/*
 * Gets a list of integers, whose length is given by atoi(args[i])*nmul
 * list is this length+1, since 0'th location gets atoi(args[i])
 */
{
   int n, *iarr, k;

   if (++i >= nargs)
      PrintUsage(args[0], i, NULL);
   n = atoi(args[i]) * nmul;
   ATL_assert(n > 0);
   iarr = malloc(sizeof(int)*(n+1));
   ATL_assert(iarr);

   iarr[0] = n / nmul;
   for (k=0; k < n; k++)
   {
      if (++i >= nargs)
         PrintUsage(args[0], i, NULL);
      iarr[k+1] = atoi(args[i]);
   }
   return(iarr);
}

int *RoutNames2IntList(int nargs, char **args, int i)
{
   int n, *iarr, k;

   if (++i >= nargs)
      PrintUsage(args[0], i, NULL);
   n = atoi(args[i]);
   ATL_assert(n > 0);
   iarr = malloc(sizeof(int)*(n+1));
   ATL_assert(iarr);

   iarr[0] = n;
   for (k=0; k < n; k++)
   {
      if (++i >= nargs)
         PrintUsage(args[0], i, NULL);
      if (!strcmp(args[i], "getrf") || !strcmp(args[i], "GETRF"))
         iarr[k+1] = LAgetrf;
      else if (!strcmp(args[i], "potrf") || !strcmp(args[i], "POTRF"))
         iarr[k+1] = LApotrf;
      else if (!strcmp(args[i], "geqrf") || !strcmp(args[i], "GEQRF"))
         iarr[k+1] = LAgeqrf;
      else if (!strcmp(args[i], "geqlf") || !strcmp(args[i], "GEQLF"))
         iarr[k+1] = LAgeqrf;
      else if (!strcmp(args[i], "gerqf") || !strcmp(args[i], "GERQF"))
         iarr[k+1] = LAgeqrf;
      else if (!strcmp(args[i], "gelqf") || !strcmp(args[i], "GELQF"))
         iarr[k+1] = LAgeqrf;
      else
         PrintUsage(args[0], i, args[i]);
   }
   return(iarr);
}

int *IntRange2IntList(int N0, int NN, int incN)
{
   int i, n;
   int *iarr;

   for (i=N0, n=0; i <= NN; i += incN) n++;
   iarr = malloc(sizeof(int)*(n+1));
   ATL_assert(iarr);
   iarr[0] = n;
   for (i=N0, n=1 ; i <= NN; i += incN, n++)
      iarr[n] = i;
   return(iarr);
}

void GetFlags(int nargs, char **args, int *flsizeKB, TYPE *thresh,
              int **nreps, int *ldagap, int **Ms, int **Ns,
              int **UPLOs, int **SDs)
{
   int *NBs=NULL, *ns=NULL, *ms=NULL, *ups=NULL, *sds=NULL, *ip;
   int i, k, n;

   *ldagap = 0;
   *flsizeKB = L2SIZE/1024;
   *nreps = NULL;
    *thresh = 100.0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'T':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *thresh = atof(args[i]);
         break;
      case 'n':         /* -n  or -nb */
         ns = GetIntList(nargs, args, i, 1);
         i += ns[0] + 1;
         break;
      case 'm':         /* -m # <M1> ... <M#> */
         ms = GetIntList(nargs, args, i, 1);
         i += ms[0] + 1;
         break;
      case 'N':         /* -N or -NB */
      case 'M':                         /* -M <Mstart> <Mend> <Minc>\n"); */
         if (i+3 >= nargs)
            PrintUsage(args[0], i, NULL);
         ip = IntRange2IntList(atoi(args[i+1]),atoi(args[i+2]),atoi(args[i+3]));
         if (args[i][0] == 'M')
            ms = ip;
         else if (args[i][2] == 'B')    /* -NB <NBstart> <NBend> <NBinc> */
            NBs = ip;
         else                           /* -N <Nstart> <Nend> <Ninc>\n"); */
            ns = ip;
         i += 3;
         break;
      case '#':                         /* set nreps */
         if (args[i][2] == 't')         /* -#t N1 reps1 ... Nt repst */
         {
            *nreps = GetIntList(nargs, args, i, 2);
            i += ((*nreps)[0] << 1) + 1;
         }
         else                           /* -# <reps> */
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            *nreps = GetIntList2(0, atoi(args[i]));
         }
         break;
      case 'f':                         /* -f <flushKB> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *flsizeKB = atoi(args[i]);
         break;
      case 'U':                         /* -U <nup> <u1> ... <uN>;[u,l,q,g] */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         n = atoi(args[i]);
         ATL_assert(n > 0);
         ups = malloc(sizeof(int)*(n+1));
         ups[0] = n;
         for (k=0; k < n; k++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            switch(args[i][0])
            {
            case 'U':
            case 'u':
               ups[k+1] = LAUpper;
               break;
            case 'l':
            case 'L':
            default:
               ups[k+1] = LALower;
               break;
            }
         }
         break;
      case 'S':                         /* -S <#> <side1> ... <sideN> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         n = atoi(args[i]);
         ATL_assert(n > 0);
         sds = malloc(sizeof(int)*(n+1));
         sds[0] = n;
         for (k=0; k < n; k++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            switch(args[i][0])
            {
            case 'L':
            case 'l':
               sds[k+1] = LALeft;
               break;
            default:
               sds[k+1] = LARight;
               break;
            }
         }
         break;
      case 'a':                         /* -a <ldagap> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *ldagap = atoi(args[i]);
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
/*
 * Take default values
 */
   if (!(*nreps))
      *nreps = GetIntList2(0, 1);
   if (!ns)
      ns = GetIntList1(1000);
   if (!ms)
      ms = GetIntList1(0);
   if (!ups)
      ups = GetIntList1(LAUpper);
   if (!sds)
      sds = GetIntList1(LARight);

   *Ns = ns;
   *Ms = ms;
   *UPLOs = ups;
   *SDs = sds;
}

double GetFlopCount(enum ATL_LAROUT rout, enum ATL_LAFLG flags,
                    int M, int N, int KL, int KU, int nb)
/*
 * These numbers copied from LAPACK timer routines TIMING/[EIG,LIN]/dopla[2].f
 */
{
    double m=(M?M:N), n=N, kl=KL, ku=ku, mn;
    double adds=0.0, muls=0.0;

    if (rout & LAgetrf)
    {
       mn = (m >= n) ? n : m;   /* mn = MIN(M,N) */
       adds = mn * ( m*n-(m+n)*(mn+1.0)/2.0 + (mn+1.0)*(2.0*mn+1.0)/6.0 );
       muls = adds + mn * ( m-(mn+1.0)/2.0 );
    }
    else if (rout & LAgeqrf)
    {
       if (flags & LARight)  /* LAgeqrf || LAgeqlf */
       {
          if (M >= N)
          {
             muls = n*( ((23.0/6.0)+m+(n/2.0)) + n*(m-(n/3.0)) );
             adds = n*( (5.0/6.0) + n*((1.0/2.0) + (m-(n/3.0))) );
          }
          else
          {
             muls = m*( ((23.0/6.0) + 2.0*n - (m/2.0)) + m*(n-(m/3.0)) );
             adds = m*( (5.0/6.0) + n - (m/2.0) + m*(n-(m/3.0)) );
          }
       }
       else  /* LAgerqf || LAgelqf */
       {
          if (M >= N)
          {
             muls = n*( ((29.0/6.0) + m + n/2.0) + n*(m-n/3.0) );
             adds = n*( (5.0/6.0) + m + n*(-0.5 + (m - n/3.0)) );
          }
          else
          {
             muls = m*( ((29.0/6.0) + 2.0*n - 0.5*m) +m*(n - m/3.0) );
             adds = m*( (5.0/6.0) + 0.5*m + m*(n - m/3.0) );
          }
       }
    }
    else if (rout & LApotrf)
    {
       muls = m*( (1.0/3.0) + m*((1.0/2.0) + (m/6.0)) );
       adds = (1.0/6.0)*m*(-1.0+m*m);
    }
    #if defined (SCPLX) || defined(DCPLX)
       return(2.0*adds + 6.0*muls);
    #else
       return(adds+muls);
    #endif
}


double Time2Flops(int rout, int UPLO, int M, int N, double time)
{
   double mflop;
   mflop = GetFlopCount(rout, UPLO, M, N, 0, 0, CAN_NB);
   if (mflop > 0)
      mflop /= time*1e6;  /* translate flops & time to MFLOPS */
   return(mflop);
}

int main(int nargs, char **args)
{
   int *Ns, *Ms, *nreps, *UPLOs, *SDs;
   int flushKB, ldagap;
   TYPE thresh;

   GetFlags(nargs, args, &flushKB, &thresh, &nreps, &ldagap, &Ms, &Ns,
            &UPLOs, &SDs);
   return(RunCases(thresh, flushKB, ldagap, nreps, SDs, UPLOs, Ms, Ns));
}



