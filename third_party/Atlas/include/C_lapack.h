#include "clapack.h"

/*
 ************************************************************
 * The following routines are provided natively by ATLAS in C
 ************************************************************
 */
#define C_sposv(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_sposv(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_sgesv(N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_sgesv(CblasColMajor, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_spotrf(Uplo_, N_, A_, lda_) \
   clapack_spotrf(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_sgetrf(M_, N_, A_, lda_, ipiv_) \
   clapack_sgetrf(CblasColMajor, M_, N_, A_, lda_, ipiv_)

#define C_spotrs(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_spotrs(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_sgetrs(Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_sgetrs(CblasColMajor, Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_spotri(Uplo_, N_, A_, lda_) \
   clapack_spotrs(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_strtri(Uplo_, Diag_, N_, A_, lda_) \
   clapack_spotrs(CblasColMajor, Uplo_, Diag_, N_, A_, lda_)
#define C_slaumm(Uplo_, N_, A_, lda_) \
   clapack_slaumm(CblasColMajor, Uplo_, N_, A_, lda_)

#define C_dposv(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_dposv(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_dgesv(N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_dgesv(CblasColMajor, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_dpotrf(Uplo_, N_, A_, lda_) \
   clapack_dpotrf(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_dgetrf(M_, N_, A_, lda_, ipiv_) \
   clapack_dgetrf(CblasColMajor, M_, N_, A_, lda_, ipiv_)

#define C_dpotrs(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_dpotrs(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_dgetrs(Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_dgetrs(CblasColMajor, Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_dpotri(Uplo_, N_, A_, lda_) \
   clapack_dpotrs(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_dtrtri(Uplo_, Diag_, N_, A_, lda_) \
   clapack_dpotrs(CblasColMajor, Uplo_, Diag_, N_, A_, lda_)
#define C_dlaumm(Uplo_, N_, A_, lda_) \
   clapack_dlaumm(CblasColMajor, Uplo_, N_, A_, lda_)

#define C_cposv(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_cposv(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_cgesv(N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_cgesv(CblasColMajor, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_cpotrf(Uplo_, N_, A_, lda_) \
   clapack_cpotrf(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_cgetrf(M_, N_, A_, lda_, ipiv_) \
   clapack_cgetrf(CblasColMajor, M_, N_, A_, lda_, ipiv_)

#define C_cpotrs(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_cpotrs(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_cgetrs(Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_cgetrs(CblasColMajor, Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_cpotri(Uplo_, N_, A_, lda_) \
   clapack_cpotrs(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_ctrtri(Uplo_, Diag_, N_, A_, lda_) \
   clapack_cpotrs(CblasColMajor, Uplo_, Diag_, N_, A_, lda_)
#define C_claumm(Uplo_, N_, A_, lda_) \
   clapack_claumm(CblasColMajor, Uplo_, N_, A_, lda_)

#define C_zposv(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_zposv(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_zgesv(N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_zgesv(CblasColMajor, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_zpotrf(Uplo_, N_, A_, lda_) \
   clapack_zpotrf(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_zgetrf(M_, N_, A_, lda_, ipiv_) \
   clapack_zgetrf(CblasColMajor, M_, N_, A_, lda_, ipiv_)

#define C_zpotrs(Uplo_, N_, NRHS_, A_, lda_, B_, ldb_) \
   clapack_zpotrs(CblasColMajor, Uplo_, N_, NRHS_, A_, lda_, B_, ldb_)
#define C_zgetrs(Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_) \
   clapack_zgetrs(CblasColMajor, Trans_, N_, NRHS_, A_, lda_, ipiv_, B_, ldb_)

#define C_zpotri(Uplo_, N_, A_, lda_) \
   clapack_zpotrs(CblasColMajor, Uplo_, N_, A_, lda_)
#define C_ztrtri(Uplo_, Diag_, N_, A_, lda_) \
   clapack_zpotrs(CblasColMajor, Uplo_, Diag_, N_, A_, lda_)
#define C_zlaumm(Uplo_, N_, A_, lda_) \
   clapack_zlaumm(CblasColMajor, Uplo_, N_, A_, lda_)


/*
 *****************************************************************************
 * The following routines are available in ATLAS only if a F77 LAPACK has been
 * provided to ATLAS.  These routines are accessed through the C-to-Fortran77
 * wrappers available in ATLAS/interfaces/lapack/C2F/src
 *****************************************************************************
 */
#include "atlas_C2Flapack.h"

/*
 * ----------------------------------------------------------------------------
 * These routines take workspace, and thus come in two flavors: rout & rout_wrk
 * ----------------------------------------------------------------------------
 */
#define C_sgels ATL_C2Fsgels
#define C_sgels_wrk ATL_C2Fsgels_wrk
#define C_dgels ATL_C2Fdgels
#define C_dgels_wrk ATL_C2Fdgels_wrk
#define C_cgels ATL_C2Fcgels
#define C_cgels_wrk ATL_C2Fcgels_wrk
#define C_zgels ATL_C2Fzgels
#define C_zgels_wrk ATL_C2Fzgels_wrk
