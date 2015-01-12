#ifndef ATLAS_TLEVEL3_H
   #define  ATLAS_TLEVEL3_H
/*
 * ========================================
 * Threaded routines in all four precisions
 * ========================================
 */
int ATL_sthreadMM(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                  size_t M, size_t N, size_t K);
void ATL_stgemm
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, ATL_CINT M,
    ATL_CINT N, ATL_CINT K, const float alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float beta, float *C, ATL_CINT ldc);
void ATL_stsymm
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float beta, float *C, ATL_CINT ldc);
void ATL_sttrsm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    float *B, ATL_CINT ldb);
void ATL_sttrmm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const float alpha, const float *A, ATL_CINT lda,
    float *B, ATL_CINT ldb);
void ATL_stsyr2k
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const float alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float beta, float *C, ATL_CINT ldc);
void ATL_stsyrk
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const float alpha, const float *A, ATL_CINT lda,
    const float beta, float *C, ATL_CINT ldc);
int ATL_dthreadMM(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                  size_t M, size_t N, size_t K);
void ATL_dtgemm
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, ATL_CINT M,
    ATL_CINT N, ATL_CINT K, const double alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double beta, double *C, ATL_CINT ldc);
void ATL_dtsymm
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double beta, double *C, ATL_CINT ldc);
void ATL_dttrsm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    double *B, ATL_CINT ldb);
void ATL_dttrmm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const double alpha, const double *A, ATL_CINT lda,
    double *B, ATL_CINT ldb);
void ATL_dtsyr2k
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const double alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double beta, double *C, ATL_CINT ldc);
void ATL_dtsyrk
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const double alpha, const double *A, ATL_CINT lda,
    const double beta, double *C, ATL_CINT ldc);
int ATL_cthreadMM(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                  size_t M, size_t N, size_t K);
void ATL_ctgemm
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, ATL_CINT M,
    ATL_CINT N, ATL_CINT K, const float *alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float *beta, float *C, ATL_CINT ldc);
void ATL_ctsymm
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float *beta, float *C, ATL_CINT ldc);
void ATL_cttrsm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    float *B, ATL_CINT ldb);
void ATL_cttrmm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    float *B, ATL_CINT ldb);
void ATL_ctsyr2k
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const float *alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float *beta, float *C, ATL_CINT ldc);
void ATL_ctsyrk
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const float *alpha, const float *A, ATL_CINT lda,
    const float *beta, float *C, ATL_CINT ldc);
int ATL_zthreadMM(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                  size_t M, size_t N, size_t K);
void ATL_ztgemm
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, ATL_CINT M,
    ATL_CINT N, ATL_CINT K, const double *alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double *beta, double *C, ATL_CINT ldc);
void ATL_ztsymm
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double *beta, double *C, ATL_CINT ldc);
void ATL_zttrsm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    double *B, ATL_CINT ldb);
void ATL_zttrmm
   (const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
    const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
    ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    double *B, ATL_CINT ldb);
void ATL_ztsyr2k
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const double *alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double *beta, double *C, ATL_CINT ldc);
void ATL_ztsyrk
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const double *alpha, const double *A, ATL_CINT lda,
    const double *beta, double *C, ATL_CINT ldc);

/*
 * =======================================================
 * Threaded routines appearing only for complex precisions
 * =======================================================
 */
void ATL_cthemm
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const float *alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float *beta, float *C, ATL_CINT ldc);
void ATL_cther2k
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const float *alpha, const float *A, ATL_CINT lda,
    const float *B, ATL_CINT ldb, const float beta, float *C, ATL_CINT ldc);
void ATL_ctherk
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const float alpha, const float *A, ATL_CINT lda,
    const float beta, float *C, ATL_CINT ldc);
void ATL_zthemm
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const double *alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double *beta, double *C, ATL_CINT ldc);
void ATL_zther2k
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const double *alpha, const double *A, ATL_CINT lda,
    const double *B, ATL_CINT ldb, const double beta, double *C, ATL_CINT ldc);
void ATL_ztherk
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const double alpha, const double *A, ATL_CINT lda,
    const double beta, double *C, ATL_CINT ldc);
#endif
