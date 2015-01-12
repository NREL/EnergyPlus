      SUBROUTINE CLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
      IMPLICIT NONE
*
*  -- LAPACK auxiliary routine (version 3.2) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      COMPLEX   T( LDT, * ), TAU( * ), V( LDV, * )
*     ..
*
*  Purpose
*  =======
*
*  CLARFT forms the triangular factor T of a COMPLEX block reflector H
*  of order n, which is defined as a product of k elementary reflectors.
*
*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
*
*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
*
*  If STOREV = 'C', the vector which defines the elementary reflector
*  H(i) is stored in the i-th column of the array V, and
*
*     H  =  I - V * T * V**H
*
*  If STOREV = 'R', the vector which defines the elementary reflector
*  H(i) is stored in the i-th row of the array V, and
*
*     H  =  I - V**H * T * V
*
*  Arguments
*  =========
*
*  DIRECT  (input) CHARACTER*1
*          Specifies the order in which the elementary reflectors are
*          multiplied to form the block reflector:
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Specifies how the vectors which define the elementary
*          reflectors are stored (see also Further Details):
*          = 'C': columnwise
*          = 'R': rowwise
*
*  N       (input) INTEGER
*          The order of the block reflector H. N >= 0.
*
*  K       (input) INTEGER
*          The order of the triangular factor T (= the number of
*          elementary reflectors). K >= 1.
*
*  V       (input/output) COMPLEX array, dimension
*                               (LDV,K) if STOREV = 'C'
*                               (LDV,N) if STOREV = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*
*  TAU     (input) COMPLEX array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i).
*
*  T       (output) COMPLEX array, dimension (LDT,K)
*          The k by k triangular factor T of the block reflector.
*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
*          lower triangular. The rest of the array is not used.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  Further Details
*  ===============
*
*  The shape of the matrix V and the storage of the vectors which define
*  the H(i) is best illustrated by the following example with n = 5 and
*  k = 3. The elements equal to 1 are not stored; the corresponding
*  array elements are modified but restored on exit. The rest of the
*  array is not used.
*
*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
*
*               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
*                   ( v1  1    )                     (     1 v2 v2 v2 )
*                   ( v1 v2  1 )                     (        1 v3 v3 )
*                   ( v1 v2 v3 )
*                   ( v1 v2 v3 )
*
*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
*
*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
*                   (     1 v3 )
*                   (        1 )
*
*  =====================================================================
*
*     .. Parameters ..
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
*     .. Local variables ..
      INTEGER ISIDE, ITRANS, IDIRECT, ISTORE
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF (N .LE.0) RETURN
      IF (DIRECT.EQ.'F' .OR. DIRECT.EQ.'f') THEN
         IDIRECT = 1
      ELSE
         IDIRECT = 2
      ENDIF
      IF (STOREV.EQ.'R' .OR. STOREV.EQ.'r') THEN
         ISTORE = 1
      ELSE
         ISTORE = 2
      ENDIF
*
      CALL ATL_F77WRAP_CLARFT(IDIRECT, ISTORE, N, K, V, LDV,
     $                        TAU, T, LDT)
*
*
      RETURN
*
*     End of CLARFT
*
      END
