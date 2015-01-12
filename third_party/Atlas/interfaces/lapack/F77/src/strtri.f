      SUBROUTINE STRTRI( UPLO, DIAG, N, A, LDA, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*  -- Modified by Peter Soendergaard for ATLAS Fortran77 LAPACK interface,
*     2001
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      REAL    A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  STRTRI computes the inverse of an upper or lower triangular
*  matrix A.
*
*  This is the Level 3 BLAS version of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  A is upper triangular;
*          = 'L':  A is lower triangular.
*
*  DIAG    (input) CHARACTER*1
*          = 'N':  A is non-unit triangular;
*          = 'U':  A is unit triangular.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) REAL array, dimension (LDA,N)
*          On entry, the triangular matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of the array A contains
*          the upper triangular matrix, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of the array A contains
*          the lower triangular matrix, and the strictly upper
*          triangular part of A is not referenced.  If DIAG = 'U', the
*          diagonal elements of A are also not referenced and are
*          assumed to be 1.
*          On exit, the (triangular) inverse of the original matrix, in
*          the same storage format.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
*               matrix is singular and its inverse can not be computed.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL   ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      INTEGER ATLASROWMAJOR, ATLASCOLMAJOR
      PARAMETER (ATLASROWMAJOR=101, ATLASCOLMAJOR=102)
      INTEGER ATLASNOTRANS, ATLASTRANS, ATLASCONJTRANS
      PARAMETER (ATLASNOTRANS=111, ATLASTRANS=112, ATLASCONJTRANS=113)
      INTEGER ATLASUPPER, ATLASLOWER
      PARAMETER (ATLASUPPER=121, ATLASLOWER=122)
      INTEGER ATLASNONUNIT, ATLASUNIT
      PARAMETER (ATLASNONUNIT=131, ATLASUNIT=132)
      INTEGER ATLASLEFT, ATLASRIGHT
      PARAMETER (ATLASLEFT=141, ATLASRIGHT=142)
*     ..
*     .. Local Scalars ..
      LOGICAL            NOUNIT, UPPER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ATL_F77WRAP_STRTRI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'STRTRI', -INFO )
         RETURN
      END IF
*
      IF (UPPER) THEN
         IUPLO = ATLASUPPER
      ELSE
         IUPLO = ATLASLOWER
      ENDIF
*
      IF (NOUNIT) THEN
         IDIAG = ATLASNONUNIT
      ELSE
         IDIAG = ATLASUNIT
      ENDIF
*
      CALL ATL_F77WRAP_STRTRI( IUPLO, IDIAG, N, A, LDA, INFO )
*
      RETURN
      END
