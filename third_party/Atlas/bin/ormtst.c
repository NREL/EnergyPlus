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
 *  This program is a tester for ORM routines.
 *  Logic:
 *       a) Based on the Input dimensions m and n, create a matrix A and perform
 *          QR(LQ,RQ,QL) factorization.
 *       b) Create the input matrix C for ORM routines
 *       c) Identify input dimension for call to ORM routines based on SIDE, TRANS
 *          and factorization variant.
 *       d) Call LAPACK Fortran version of ORM routine   with modified A & C
 *       d) Call ATLAS C version of ORM routine   with modified A & C
 *       e) Compare the results
 */

/************************   Some Sample Input :***********************
QR
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 R  -U 1 u -# 1   -x L -y N  -z 100
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 R  -U 1 u -# 1   -x L -y T  -z 100
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 R  -U 1 u -# 1   -x R -y N  -z 100
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 R  -U 1 u -# 1   -x R -y T  -z 100
RQ
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 L  -U 1 u -# 1   -x L -y N  -z 100

QL
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 R  -U 1 l -# 1   -x L -y N  -z 100

LQ
./xsormtst_pt  -m 1 800   -n 1 128 -a 200 -S 1 L  -U 1 l -# 1   -x L -y T  -z 100
 ***********************************************************************/


#include "atlas_misc.h"
#include "atlas_lapack.h"
#include "cblas.h"
#include "atlas_cblastypealias.h"
#include "atlas_tst.h"
#ifndef TimeF77
   #include "clapack.h"
#endif
//#define ormCtime 1
#define ormFtime 1

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
#define PRINT_2    1

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


static TYPE ORlqtest(int M, int N, int lda, int flushKB, double *time, char Cside, char Ctrans, int CsideSz)
{

   TYPE *A, *TAU,  *WORK, *WORKC ;
   TYPE *C, *CF;

   TYPE *AORIG ;
   TYPE  dtmp, dtmp1;
   double t0;

   int LWORK, LWORKC;
   TYPE normC, eps, resid;

   int  ldm, ldmn, ldn, cM, cN, ldc ;

   enum CBLAS_SIDE SIDE; enum CBLAS_TRANSPOSE TRANS ;

   ldm = M;
   ldn = N;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

/* Epsilon */
   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");

/* Allocate C  and CF  as the matrix to be multiplied with A                  */
/* and will get the same matrix                                               */
/* Note that ALWAYS Q will have size M since we are caling  ATL_ormqrf        */

   if( Cside == 'L')
   {
      if(Ctrans  ==  'N')
      {
         cM = N;
         cN = CsideSz;
         ldc = ldn;
         SIDE = CblasLeft;
         TRANS = CblasNoTrans;
      }
      else
      {
         cM = N;
         cN = CsideSz;
         ldc = ldn;
         SIDE = CblasLeft;
         #ifdef TREAL
            TRANS = CblasTrans;
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
   else
   {
      if(Ctrans  ==  'N')
      {
         cM = CsideSz;
         cN = N;
         ldc = CsideSz;
         SIDE = CblasRight;
         TRANS = CblasNoTrans;
      }
      else
      {
         cM = CsideSz;
         cN = N;
         ldc = CsideSz;
         SIDE = CblasRight;
         #ifdef TREAL
            TRANS = CblasTrans;
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
/* Allocate C and CF  for ormqr inp - will get modified */
   C  =  GetGE(cM,cN, ldc);                 /* Q will have side M             */
   CF =  GetGE(cM,cN, ldc);       /* Both C an CF are having the same values  */

/* Allocate A Original and copy from A */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);

/* Call to get the  size of workspace required */
   test_gelqf(CblasColMajor, M, N, &dtmp1, lda, &dtmp1, &dtmp, -1);

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

   test_gelqf(CblasColMajor, M, N, (TYPE*)(A), lda, (TYPE*)(TAU),
              (TYPE*)(WORK), LWORK);

   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);


   printMatrix("AORIG", AORIG, M, N, lda);
   printMatrix("A", A, M, N, lda);
   printVector("TAU", TAU, MNmin);


//   LWORKC = 1000000;            // TODO : Temporary Code  Modify  later
   LWORKC = Mjoin(PATL,ormlq)(SIDE, TRANS, cM, cN, MNmin, &dtmp1, lda, TAU, &dtmp1, ldc,
                     &dtmp,  -1);
   LWORKC = dtmp;
   WORKC = malloc(ATL_MulBySize(LWORKC));


   ATL_assert(!LA_LQ2Q(SIDE, TRANS, cM, cN, MNmin, A, lda, TAU,
                       CF, ldc));

   Mjoin(PATL,ormlq)(SIDE, TRANS, cM, cN, MNmin, A, lda, TAU, C, ldc,
                     WORKC,  LWORKC);


   normC = Mjoin(PATL,genrm1)(cM, cN, C, ldc);
   resid  = Mjoin(PATL,gediffnrm1)(cM, cN, C, ldc, CF, ldc);

   resid /= (normC * eps * Mmin(cM, cN));

   return(resid);
}

static TYPE ORqltest(int M, int N, int lda, int flushKB, double *time, char Cside, char Ctrans, int CsideSz)
{
   TYPE *A, *TAU,  *WORK, *WORKC ;
   TYPE *C, *CF;

   TYPE *AORIG ;
   TYPE  dtmp, dtmp1;
   double t0;


   int LWORK, LWORKC;
   TYPE normC, eps, resid;
   int  ldm, ldmn, ldn, cM, cN, ldc ;
   const int lda2 = (lda SHIFT);

   enum CBLAS_SIDE SIDE; enum CBLAS_TRANSPOSE TRANS ;


   ldm = M;
   ldn = N;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");

/* Allocate C  and CF  as the matrix to be multiplied with A
 * and will get the same matrix
 * Note that ALWAYS Q will have size M since we are caling  ATL_ormqrf
 */

   if( Cside == 'L')
   {
      if(Ctrans  ==  'N')
      {
         cM = M;
         cN = CsideSz;
         ldc = ldm;
         SIDE = CblasLeft;
         TRANS = CblasNoTrans;
      }
      else
      {
         cM = M;
         cN = CsideSz;
         ldc = ldm;
         SIDE = CblasLeft;
         #ifdef TREAL
            TRANS = CblasTrans;                 //or MY_TRAN?
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
   else
   {
      if(Ctrans  ==  'N')
      {
         cM = CsideSz;
         cN = M;
         ldc = CsideSz;
         SIDE = CblasRight;
         TRANS = CblasNoTrans;                 //or MY_TRAN?
      }
      else
      {
         cM = CsideSz;
         cN = M;
         ldc = CsideSz;
         SIDE = CblasRight;
         #ifdef TREAL
            TRANS = CblasTrans;                 //or MY_TRAN?
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
/* Allocate C and CF  for ormqr inp - will get modified */
   C  =  GetGE(cM,cN, ldc);                 /* Q will have side M             */
   CF =  GetGE(cM,cN, ldc);       /* Both C an CF are having the same values  */

/* Allocate A Original and copy from A */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);

/* Call to get the  size of workspace required */
   test_geqlf(CblasColMajor, M, N, &dtmp1, lda,
               &dtmp1,
               &dtmp, -1);

   LWORK = dtmp;

/* Get Work and LWORK   */
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
   test_geqlf(CblasColMajor, M, N, (TYPE*)(A), lda,
               (TYPE*)(TAU),
               (TYPE*)(WORK), LWORK);
   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);

   printMatrix("AORIG", AORIG, M, N, lda);
   printMatrix("A", A, M, N, lda);
   printVector("TAU", TAU, MNmin);


//   LWORKC = 1000000;
   LWORKC = Mjoin(PATL,ormql)(SIDE, TRANS, cM, cN, MNmin, &dtmp1, lda, TAU, &dtmp1, ldc,
                     &dtmp,  -1);
   LWORKC = dtmp;

   WORKC = malloc(ATL_MulBySize(LWORKC));

   ATL_assert(!LA_QL2Q(SIDE, TRANS, cM, cN, MNmin, A+(N-MNmin)*lda2, lda, TAU,
                                 CF, ldc));

   ATL_ormql(SIDE, TRANS, cM, cN, MNmin, A+(N-MNmin)*lda2, lda,
                                 TAU, C, ldc, WORKC,  LWORKC);

   normC = Mjoin(PATL,genrm1)(cM, cN, C, ldc);
   resid  = Mjoin(PATL,gediffnrm1)(cM, cN, C, ldc, CF, ldc);

   resid /= (normC * eps * Mmin(cM, cN));
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
static TYPE ORrqtest(int M, int N, int lda, int flushKB, double *time, char Cside, char Ctrans, int CsideSz )
{
   TYPE *A, *TAU,  *WORK, *WORKC ;
   TYPE *C, *CF;

   TYPE *AORIG ;
   TYPE  dtmp, dtmp1;
   double t0;


   int LWORK, LWORKC;
   TYPE normC, eps, resid;
   int  ldm, ldmn, ldn, cM, cN, ldc ;

   enum CBLAS_SIDE SIDE; enum CBLAS_TRANSPOSE TRANS ;


   ldm = M;
   ldn = N;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");

/* Allocate C  and CF  as the matrix to be multiplied with A
 * and will get the same matrix
 * Note that ALWAYS Q will have size M since we are caling  ATL_ormqrf
 *
 * fprintf(stderr, " inside rqtest  \n");
 * fprintf(stderr, " CSide %c  Ctrans %c \n", Cside, Ctrans);
 */

   if( Cside == 'L')
   {
      if(Ctrans  ==  'N')
      {
         cM = N;
         cN = CsideSz;
         ldc = ldn;
         SIDE = CblasLeft;
         TRANS = CblasNoTrans;
      }
      else
      {
         cM = N;
         cN = CsideSz;
         ldc = ldn;
         SIDE = CblasLeft;
         #ifdef TREAL
            TRANS = CblasTrans;                 //or MY_TRAN?
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
   else
   {
      if(Ctrans  ==  'N')
      {
         cM = CsideSz;
         cN = N;
         ldc = CsideSz;
         SIDE = CblasRight;
         TRANS = CblasNoTrans;                 //or MY_TRAN?
      }
      else
      {
         cM = CsideSz;
         cN = N;
         ldc = CsideSz;
         SIDE = CblasRight;
         #ifdef TREAL
            TRANS = CblasTrans;                 //or MY_TRAN?
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
/* Allocate C and CF  for ormqr inp - will get modified */
   C  =  GetGE(cM,cN, ldc);                 /* Q will have side M             */
   CF =  GetGE(cM,cN, ldc);       /* Both C an CF are having the same values  */

/* Allocate A Original and copy from A */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);

/* Call to get the  size of workspace required */
   test_gerqf(CblasColMajor, M, N, &dtmp1, lda,
               &dtmp1,
               &dtmp, -1);

   LWORK = dtmp;

/* Get Work and LWORK */
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

   test_gerqf(CblasColMajor, M, N, (TYPE*)(A), lda,
               (TYPE*)(TAU),
               (TYPE*)(WORK), LWORK);

   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);


   printMatrix("AORIG", AORIG, M, N, lda);
   printMatrix("A", A, M, N, lda);
   printVector("TAU", TAU, MNmin);


//   LWORKC = 10000000;            // TODO : Temporary Code  Modify  later
   LWORKC = Mjoin(PATL,ormrq)(SIDE, TRANS, cM, cN, MNmin, &dtmp1, lda, TAU, &dtmp1, ldc,
                     &dtmp,  -1);
   LWORKC = dtmp;
   WORKC = malloc(ATL_MulBySize(LWORKC));

   ATL_assert(!LA_RQ2Q(SIDE, TRANS, cM, cN, MNmin,
                       A+((M-MNmin)SHIFT), lda, TAU, CF, ldc));

   Mjoin(PATL,ormrq)(SIDE, TRANS,
                     cM, cN, MNmin, A+((M-MNmin) SHIFT) , lda, TAU, C, ldc,
                     WORKC,  LWORKC);

   normC = Mjoin(PATL,genrm1)(cM, cN, C, ldc);
   resid  = Mjoin(PATL,gediffnrm1)(cM, cN, C, ldc, CF, ldc);

   resid /= (normC * eps * Mmin(cM, cN));   /* Scale                          */

   return(resid);
}


/*
 * Calls geqr ( single, double, single complex and double complex ).
 * From A[MxN] Matrix returned from geqr routines, compute Q and R matrix.
 * Find the residual for ( A_original - QR  ) operation.
 *
 *   Also, make H(k) = i-tau*vv'
 *   and compute Q = H(1)*H(2)...H(k)  where k = Min(M,N)
 *    Cside is the column
 */
TYPE ORqrtest(int M, int N, int lda, int flushKB, double *time, char Cside, char Ctrans, int CsideSz )
{
   TYPE *A, *TAU,  *WORK, *WORKC ;
   TYPE *C, *CF;

   TYPE *AORIG ;
   TYPE  dtmp, dtmp1;
   double t0;


   int LWORK, LWORKC;
   TYPE normC, eps, resid;
   int  ldm, ldmn, ldn, cM, cN, ldc ;

   enum CBLAS_SIDE SIDE; enum CBLAS_TRANSPOSE TRANS ;

   ldm = M;
   ldn = N;
   const int MNmin = Mmin(M,N);
   ldmn = MNmin;

/* Epsilon                                                                    */
   eps = Mjoin(PATL,epsilon)();

/* Allocate A and initialize                                                  */
   A = GetGE(M, N, lda);
   if (A == NULL) AllocationErr("A");
/*
 * Allocate C  and CF  as the matrix to be multiplied with A
 * and will get the same matrix
 * Note that ALWAYS Q will have size M since we are caling  ATL_ormqrf
 */

   if( Cside == 'L')
   {
      if(Ctrans  ==  'N')
      {
         cM = M;
         cN = CsideSz;
         ldc = ldm;
         SIDE = CblasLeft;
         TRANS = CblasNoTrans;
      }
      else
      {
         cM = M;
         cN = CsideSz;
         ldc = ldm;
         SIDE = CblasLeft;
         #ifdef TREAL
            TRANS = CblasTrans;                 //or MY_TRAN?
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
   else
   {
      if(Ctrans  ==  'N')
      {
         cM = CsideSz;
         cN = M;
         ldc = CsideSz;
         SIDE = CblasRight;
         TRANS = CblasNoTrans;                 //or MY_TRAN?
      }
      else
      {
         cM = CsideSz;
         cN = M;
         ldc = CsideSz;
         SIDE = CblasRight;
         #ifdef TREAL
            TRANS = CblasTrans;                 //or MY_TRAN?
         #else
            TRANS = CblasConjTrans;
         #endif
      }
   }
/* Allocate C and CF  for ormqr inp - will get modified */
   C  =  GetGE(cM,cN, ldc);                 /* Q will have side M             */
   CF =  GetGE(cM,cN, ldc);       /* Both C an CF are having the same values  */

/* Allocate A Original and copy from A */
   AORIG = DupMat(CblasColMajor, M, N, A, lda, lda);

/* Call to get the  size of workspace required */
   test_geqrf(CblasColMajor, M, N, &dtmp1, lda,
               &dtmp1,
               &dtmp, -1);

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
               (TYPE*)(TAU),
               (TYPE*)(WORK), LWORK);
   *time = time00() - t0;
   if (flushKB)
      t0 += ATL_flushcache(0);


   printMatrix("AORIG", AORIG, M, N, lda);
   printMatrix("A", A, M, N, lda);
   printVector("TAU", TAU, MNmin);

//   LWORKC = 100000;            // TODO : Temporary Code  Modify  later
   LWORKC = Mjoin(PATL,ormqr)(SIDE, TRANS, cM, cN, MNmin, &dtmp1, lda, TAU, &dtmp1, ldc,
                     &dtmp,  -1);
   LWORKC = dtmp;
   WORKC = malloc(ATL_MulBySize(LWORKC));

   if(PRINT_2) printf("Input to ORM routine cM=%d cN=%d K=%d\n",
                                     cM, cN, MNmin);
   #ifdef ormFtime
   t0 = time00();
   #endif
   ATL_assert(!LA_QR2Q(SIDE, TRANS, cM, cN, MNmin, A, lda, TAU,
                                 CF, ldc));
   #ifdef ormFtime
   *time = time00() - t0;
   #endif

   #ifdef ormCtime
   t0 = time00();
   #endif
   ATL_ormqr(SIDE, TRANS, cM, cN, MNmin, A, lda,
                                 TAU, C, ldc, WORKC,  LWORKC);
   #ifdef ormCtime
   *time = time00() - t0;
   #endif

   normC = Mjoin(PATL,genrm1)(cM, cN, C, ldc);
   resid  = Mjoin(PATL,gediffnrm1)(cM, cN, C, ldc, CF, ldc);

   resid /= (normC * eps * Mmin(cM, cN));

/* free */
   if (A)   free(A);
   if (C)   free(C);
   if (CF) free(CF);
   if (WORK)  free(WORK);
   if (WORKC)  free(WORKC);
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

int RunCase(TYPE thresh, int flushKB, int side, int uplo, int M, int N, int lda, char Cside, char Ctrans, int CsideSz)
/*
 * RETURNS: 0 if residual is <= thresh, otherwise 1
 */
{
   TYPE resid;
   TYPE (*qtest)(int, int, int, int, double*, char, char, int);
   double time;
   double Time2FlopsNew(int rout, int UPLO, int M, int N, double time, char  QSIDE, int CsideSize);
   char qs[4];

   qs[2] = '\0';
   if (side == LARight)
   {
      qs[0] = 'Q';
      if (uplo == LALower) { qtest = ORqltest; qs[1] = 'L'; }
      else { qtest = ORqrtest; qs[1] = 'R'; }
   }
   else
   {
      qs[1] = 'Q';
      if (uplo == LALower) { qtest = ORlqtest; qs[0] = 'L'; }
      else { qtest = ORrqtest; qs[0] = 'R'; }
   }
   resid = qtest(M, N, lda, flushKB, &time, Cside, Ctrans, CsideSz);
   printf("%2s  %3s %6d %6d %6d  %10.4e %11.2f  %9.2e\n", qs, "Col", M, N, lda,
          time, Time2FlopsNew(LAgeqrf, uplo+side, M, N, time, Cside, CsideSz), resid);
   return(resid <= thresh ? 1 : 0);
}

int RunCases(TYPE thresh, int flushKB, int ldagap, int *NREPS,
              int *sides, int *uplos, int *Ms, int *Ns,
              char Cside, char Ctrans, int CsideSz)
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

//orig QR -QL
//                                      M, Ns[n], M+ldagap,  'L', 'N', 100);
//                                      M, Ns[n], M+ldagap,  'L', 'T', 100);
//orig RQ -LQ
//                                      M, Ns[n], M+ldagap,  'R', 'N', 100);
//                                      M, Ns[n], M+ldagap,  'R', 'T', 100);
                                      M, Ns[n], M+ldagap,  Cside, Ctrans, CsideSz);

                     ntest++;
                  }
               }
            }
         }
      }
   }
   printf("\n%d cases ran, %d cases passed\n\n", ntest, npass);
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
              int **UPLOs, int **SDs,
              char  *cside,
              char *ctrans,
              int *csideSz)
{
   int *NBs=NULL, *ns=NULL, *ms=NULL, *ups=NULL, *sds=NULL, *ip;
   int i, k, n;

   *ldagap = 0;
   *flsizeKB = L2SIZE/1024;
   *nreps = NULL;
    *thresh = 100.0;
   /* Temporary Setting For ormm Matrix  C */
   *cside  = 'L';
   *ctrans = 'N';
   *csideSz = 100;
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
      case 'n':                             /* -n or -nb                      */
         ns = GetIntList(nargs, args, i, 1);
         i += ns[0] + 1;
         break;
      case 'm':                             /* -m # <M1> ... <M#>             */
         ms = GetIntList(nargs, args, i, 1);
         i += ms[0] + 1;
         break;
      case 'N':                             /* -N or -NB                      */
      case 'M':                   /* -M <Mstart> <Mend> <Minc>\n");           */
         if (i+3 >= nargs)
            PrintUsage(args[0], i, NULL);
         ip = IntRange2IntList(atoi(args[i+1]),atoi(args[i+2]),atoi(args[i+3]));
         if (args[i][0] == 'M')
            ms = ip;
         else if (args[i][2] == 'B')        /* -NB <NBstart> <NBend> <NBinc>  */
            NBs = ip;
         else                     /* -N <Nstart> <Nend> <Ninc>\n");           */
            ns = ip;
         i += 3;
         break;
      case '#':                             /* set nreps                      */
         if (args[i][2] == 't')             /* -#t N1 reps1 ... Nt repst      */
         {
            *nreps = GetIntList(nargs, args, i, 2);
            i += ((*nreps)[0] << 1) + 1;
         }
         else                               /* -# <reps>                      */
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, NULL);
            *nreps = GetIntList2(0, atoi(args[i]));
         }
         break;
      case 'f':                             /* -f <flushKB>                   */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *flsizeKB = atoi(args[i]);
         break;
      case 'U':                   /* -U <nup> <u1> ... <uN>;[u,l,q,g]         */
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
      case 'S':                             /* -S <#> <side1> ... <sideN>     */
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
      case 'a':                             /* -a <ldagap>                    */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *ldagap = atoi(args[i]);
         break;
      case 'x':                         /* -x L      -x R      side for Q */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *cside = args[i][0];
         break;
      case 'y':                         /* -y T      -y T      trans  Q */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *ctrans = args[i][0];
         break;
      case 'z':                         /* -z <size> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *csideSz =  atoi(args[i]);
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
      mn = (m >= n) ? n : m;                /* mn = MIN(M,N)                  */
      adds = mn * ( m*n-(m+n)*(mn+1.0)/2.0 + (mn+1.0)*(2.0*mn+1.0)/6.0 );
      muls = adds + mn * ( m-(mn+1.0)/2.0 );
   }
   else if (rout & LAgeqrf)
   {
      if (flags & LARight)                  /* LAgeqrf || LAgeqlf             */
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
      else                                  /* LAgerqf || LAgelqf             */
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
    else if (rout & LAormqr)
    {
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


double Time2FlopsNew(int rout, int UPLO, int M, int N, double time, char QSIDE, int csideSz )
{
   double mflop;
   int cM, cN;


//ORlqtest
//   if( Cside == 'L')
//   {
//         cM = N;
//         cN = CsideSz;
//   }
//   else
//   {
//         cM = CsideSz;
//         cN = N;
//   }
//
//ORqltest
//   if( Cside == 'L')
//   {
//         cM = M;
//         cN = CsideSz;
//   }
//   else
//   {
//         cM = CsideSz;
//         cN = M;
//   }
//
//ORrqtest
//   if( Cside == 'L')
//   {
//         cM = N;
//         cN = CsideSz;
//   }
//   else
//   {
//         cM = CsideSz;
//         cN = N;
//   }
//
//ORqrtest
//   if( Cside == 'L')
//   {
//         cM = M;
//         cN = CsideSz;
//   }
//   else
//   {
//         cM = CsideSz;
//         cN = M;
//   }
   mflop = GetFlopCount(rout, UPLO, M, N, 0, 0, CAN_NB);
   if (mflop > 0)
      mflop /= time*1e6;          /* translate flops & time to MFLOPS         */
   return(mflop);
}

int main(int nargs, char **args)
{
   int *Ns, *Ms, *nreps, *UPLOs, *SDs;
   int flushKB, ldagap;
   TYPE thresh;

   char cside, ctrans;
   int  csideSz ;
   GetFlags(nargs, args, &flushKB, &thresh, &nreps, &ldagap, &Ms, &Ns,
            &UPLOs, &SDs, &cside, &ctrans, &csideSz);

  printf ("cside   = %c   ctrans = %c  csidesz = %d \n", cside, ctrans, csideSz);
   return(RunCases(thresh, flushKB, ldagap, nreps, SDs, UPLOs, Ms, Ns, cside, ctrans, csideSz));
}

