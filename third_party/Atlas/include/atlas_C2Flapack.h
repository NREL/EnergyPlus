/*
 * This routine prototypes the C wrappers to the Fortran77 LAPACK routines
 * (i.e., those LAPACK routines not natively provided by ATLAS).
 * See "clapack.h" for the prototypes of ATLAS's native LAPACK routines.
 */
#ifndef ATLAS_C2FLAPACK_H
   #define ATLAS_C2FLAPACK_H

int ATL_C2Fsgels(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                 ATL_CINT NRHS, float *A, ATL_CINT lda,
                 float *B, ATL_CINT ldb);
int ATL_C2Fsgels_wrk(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                    ATL_CINT NRHS, float *A, ATL_CINT lda,
                    float *B, ATL_CINT ldb, float *wrk, ATL_INT lwrk);
#define ATL_C2Fsgels_wrk__    /* signal that this func exists */
int ATL_C2Fdgels(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                 ATL_CINT NRHS, double *A, ATL_CINT lda,
                 double *B, ATL_CINT ldb);
int ATL_C2Fdgels_wrk(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                    ATL_CINT NRHS, double *A, ATL_CINT lda,
                    double *B, ATL_CINT ldb, double *wrk, ATL_INT lwrk);
#define ATL_C2Fdgels_wrk__    /* signal that this func exists */
int ATL_C2Fcgels(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                 ATL_CINT NRHS, float *A, ATL_CINT lda,
                 float *B, ATL_CINT ldb);
int ATL_C2Fcgels_wrk(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                    ATL_CINT NRHS, float *A, ATL_CINT lda,
                    float *B, ATL_CINT ldb, float *wrk, ATL_INT lwrk);
#define ATL_C2Fcgels_wrk__    /* signal that this func exists */
int ATL_C2Fzgels(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                 ATL_CINT NRHS, double *A, ATL_CINT lda,
                 double *B, ATL_CINT ldb);
int ATL_C2Fzgels_wrk(const enum CBLAS_TRANSPOSE TA, ATL_CINT M, ATL_CINT N,
                    ATL_CINT NRHS, double *A, ATL_CINT lda,
                    double *B, ATL_CINT ldb, double *wrk, ATL_INT lwrk);
#define ATL_C2Fzgels_wrk__    /* signal that this func exists */
/*
 * Routines in complex precisions only
 */
int ATL_C2Fcunmql
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fcunmql_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fcunmql_wrk__    /* signal that this func exists */
int ATL_C2Fcunmlq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fcunmlq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fcunmlq_wrk__    /* signal that this func exists */
int ATL_C2Fcunmrq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fcunmrq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fcunmrq_wrk__    /* signal that this func exists */
int ATL_C2Fcunmqr
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fcunmqr_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fcunmqr_wrk__    /* signal that this func exists */
int ATL_C2Fzunmql
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fzunmql_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fzunmql_wrk__    /* signal that this func exists */
int ATL_C2Fzunmlq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fzunmlq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fzunmlq_wrk__    /* signal that this func exists */
int ATL_C2Fzunmrq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fzunmrq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fzunmrq_wrk__    /* signal that this func exists */
int ATL_C2Fzunmqr
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fzunmqr_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fzunmqr_wrk__    /* signal that this func exists */
/*
 * Routines in real precisions only
 */
int ATL_C2Fsormql
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fsormql_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fsormql_wrk__    /* signal that this func exists */
int ATL_C2Fsormlq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fsormlq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fsormlq_wrk__    /* signal that this func exists */
int ATL_C2Fsormrq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fsormrq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fsormrq_wrk__    /* signal that this func exists */
int ATL_C2Fsormqr
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc);
int ATL_C2Fsormqr_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, float *A, ATL_CINT lda, float *TAU,
    float *C, ATL_CINT ldc, float *wrk, ATL_INT lwrk);
#define ATL_C2Fsormqr_wrk__    /* signal that this func exists */
int ATL_C2Fdormql
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fdormql_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fdormql_wrk__    /* signal that this func exists */
int ATL_C2Fdormlq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fdormlq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fdormlq_wrk__    /* signal that this func exists */
int ATL_C2Fdormrq
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fdormrq_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fdormrq_wrk__    /* signal that this func exists */
int ATL_C2Fdormqr
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc);
int ATL_C2Fdormqr_wrk
   (const enum CBLAS_SIDE S, const enum CBLAS_TRANSPOSE TA,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, double *A, ATL_CINT lda, double *TAU,
    double *C, ATL_CINT ldc, double *wrk, ATL_INT lwrk);
#define ATL_C2Fdormqr_wrk__    /* signal that this func exists */

#endif   /* end ifdef ATLAS_C2FLAPACK_H */
