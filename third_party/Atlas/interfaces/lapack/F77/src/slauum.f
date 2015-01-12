      SUBROUTINE SLAUUM( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*  -- Modified by R. Clint Whaley for ATLAS Fortran77 LAPACK interface,
*     2001
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      REAL         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  SLAUUM computes the product U * U' or L' * L, where the triangular
*  factor U or L is stored in the upper or lower triangular part of
*  the array A.
*
*  If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
*  overwriting the factor U in A.
*  If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
*  overwriting the factor L in A.
*
*  This is the blocked form of the algorithm, calling Level 3 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the triangular factor stored in the array A
*          is upper or lower triangular:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the triangular factor U or L.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the triangular factor U or L.
*          On exit, if UPLO = 'U', the upper triangle of A is
*          overwritten with the upper triangle of the product U * U';
*          if UPLO = 'L', the lower triangle of A is overwritten with
*          the lower triangle of the product L' * L.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*
*  =====================================================================
*
*    .. Parameters ..
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
*
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ATL_F77WRAP_SLAUUM
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
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SLAUUM', -INFO )
         RETURN
      END IF
*
      IF (UPPER) THEN
         IUPLO = ATLASUPPER
      ELSE
         IUPLO = ATLASLOWER
      ENDIF
      CALL ATL_F77WRAP_SLAUUM( IUPLO, N, A, LDA, INFO )
*
      RETURN
*
*     End of SLAUUM
*
      END
