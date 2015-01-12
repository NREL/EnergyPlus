      SUBROUTINE CSYRK( UPLO, TRANS, N, K, ALPHA, A, LDA, BETA,
     $                  C, LDC )
*
*  -- Automatically Tuned Linear Algebra Software (ATLAS)
*     (C) Copyright 2000 All Rights Reserved
*
*  -- ATLAS routine -- F77 Interface -- Version 3.9.24 -- December 25, 2000
*
*  Author         : Antoine P. Petitet
*  Originally developed at the University of Tennessee,
*  Innovative Computing Laboratory,  Knoxville TN, 37996-1301, USA.
*
*  ---------------------------------------------------------------------
*
*  -- Copyright notice and Licensing terms:
*
*  Redistribution  and  use in  source and binary forms, with or without
*  modification, are  permitted provided  that the following  conditions
*  are met:
*
*  1. Redistributions  of  source  code  must retain the above copyright
*     notice, this list of conditions and the following disclaimer.
*  2. Redistributions in binary form must reproduce  the above copyright
*     notice,  this list of conditions, and the  following disclaimer in
*     the documentation and/or other materials provided with the distri-
*     bution.
*  3. The name of the University,  the ATLAS group,  or the names of its
*     contributors  may not be used to endorse or promote products deri-
*     ved from this software without specific written permission.
*
*  -- Disclaimer:
*
*  THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
*  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
*  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
*  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
*  CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
*  TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
*  OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
*  RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
*  CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
*  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
*  ---------------------------------------------------------------------
*
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO, TRANS
      INTEGER            N, K, LDA, LDC
      COMPLEX            ALPHA, BETA
*     ..
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CSYRK  performs one of the symmetric rank k operations
*
*     C := alpha * A * A' + beta * C,
*
*  or
*
*     C := alpha * A' * A + beta * C,
*
*  where alpha and beta are scalars, C is an n by n symmetric matrix and
*  A is an  n by k  matrix in the first case and a  k by n matrix in the
*  second case.
*
*  Arguments
*  =========
*
*  UPLO    (input)                       CHARACTER*1
*          On entry, UPLO  specifies whether the upper or lower triangu-
*          lar part of the array C is to be referenced as follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of C
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of C
*                                  is to be referenced.
*
*          Unchanged on exit.
*
*  TRANS   (input)                       CHARACTER*1
*          On entry,  TRANS  specifies the  operation to be performed as
*          follows:
*
*             TRANS = 'N' or 'n'  C := alpha * A * A' + beta * C,
*
*             TRANS = 'T' or 't'  C := alpha * A' * A + beta * C.
*
*          Unchanged on exit.
*
*  N       (input)                       INTEGER
*          On entry, N specifies the order of the matrix C. N must be at
*          least zero. Unchanged on exit.
*
*  K       (input)                       INTEGER
*          On entry, with TRANS = 'N' or 'n',  K specifies the number of
*          columns of the matrix A, and with TRANS = 'T' or 't', K  spe-
*          cifies  the  number of rows of the  matrix  A.  K  must be at
*          least zero. Unchanged on exit.
*
*  ALPHA   (input)                       COMPLEX
*          On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
*          supplied as zero  then  the  entries of the matrix A need not
*          be set on input. Unchanged on exit.
*
*  A       (input)                       COMPLEX array
*          On entry, A is an array of DIMENSION ( LDA, ka ), where ka is
*          k  when  TRANS = 'N' or 'n', and is n otherwise. Before entry
*          with TRANS = 'N' or 'n', the leading n by k part of the array
*          A must contain the matrix A,  otherwise  the  leading  k by n
*          part of the array A must contain the matrix A.  Unchanged  on
*          exit.
*
*  LDA     (input)                       INTEGER
*          On entry, LDA  specifies the first dimension of A as declared
*          in  the   calling  (sub)  program.   When  TRANS = 'N' or 'n'
*          then  LDA  must be at least  max( 1, n ), otherwise  LDA must
*          be at least  max( 1, k ). Unchanged on exit.
*
*  BETA    (input)                       COMPLEX
*          On entry,  BETA  specifies the scalar  beta.   When  BETA  is
*          supplied as zero  then  the  entries of the matrix C need not
*          be set on input. Unchanged on exit.
*
*  C       (input/output)                COMPLEX array
*          On entry, C is an array of DIMENSION ( LDC, n ). Before entry
*          with  UPLO = 'U' or 'u',  the leading n by n upper triangular
*          part of the array C must contain the upper triangular part of
*          the  symmetric matrix  and the strictly lower triangular part
*          of C is not referenced. On exit, the upper triangular part of
*          the array  C  is  overwritten by the upper triangular part of
*          the updated matrix. Before entry with  UPLO = 'L' or 'l', the
*          leading n by n lower triangular part of the array C must con-
*          tain the lower triangular part of the  symmetric  matrix  and
*          the strictly upper triangular part of C is not referenced. On
*          exit, the lower triangular part of the array C is overwritten
*          by the lower triangular part of the updated matrix.
*
*  LDC     (input)                       INTEGER
*          On entry,  LDC specifies the first dimension of C as declared
*          in   the  calling  (sub)  program.   LDC  must  be  at  least
*          max( 1, n ). Unchanged on exit.
*
*  Further Details
*  ===============
*
*  For further information on the Level 1 BLAS specification, see:
*
*  ``A Proposal for Standard Linear Algebra Subprograms''  by R. Hanson,
*  F. Krogh and C. Lawson, ACM SIGNUM Newsl., 8(16), 1973,
*
*  ``Basic Linear Algebra Subprograms for Fortran Usage''  by C. Lawson,
*  R. Hanson, D. Kincaid and F. Krogh,  ACM Transactions on Mathematical
*  Software, 5(3) pp 308-323, 1979.
*
*  For further information on the Level 2 BLAS specification, see:
*
*  ``An  Extended Set of  FORTRAN  Basic Linear Algebra Subprograms'' by
*  J. Dongarra,  J. Du Croz,  S. Hammarling and R. Hanson,  ACM Transac-
*  tions on Mathematical Software, 14(1) pp 1-17, 1988.
*
*  ``Algorithm 656: An extended Set of Basic Linear Algebra Subprograms:
*  Model Implementation and Test Programs''  by J. Dongarra, J. Du Croz,
*  S. Hammarling and R. Hanson,  ACM  Transactions on Mathematical Soft-
*  ware, 14(1) pp 18-32, 1988.
*
*  For further information on the Level 3 BLAS specification, see:
*
*  ``A Set of Level 3 Basic Linear Algebra Subprograms'' by J. Dongarra,
*  J. Du Croz, I. Duff and S. Hammarling, ACM Transactions on Mathemati-
*  cal Software, 16(1), pp 1-17, 1990.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            ILOWER, IUPPER
      PARAMETER          ( IUPPER = 121, ILOWER = 122 )
      INTEGER            ICOTRAN, INOTRAN, ITRAN
      PARAMETER          ( INOTRAN = 111, ITRAN = 112, ICOTRAN = 113 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, ITRANS, IUPLO, NROWA
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATL_F77WRAP_CSYRK, XERBLA
*     ..
*     .. External Functions ..
      EXTERNAL           LSAME
      LOGICAL            LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      IF(      LSAME( UPLO , 'L' ) ) THEN
         IUPLO = ILOWER
      ELSE IF( LSAME( UPLO , 'U' ) ) THEN
         IUPLO = IUPPER
      ELSE
         IUPLO = ILOWER
         INFO  = 1
      END IF
*
      IF(      LSAME( TRANS, 'N' ) ) THEN
         ITRANS = INOTRAN
         NROWA  = N
      ELSE IF( LSAME( TRANS, 'T' ) ) THEN
         ITRANS = ITRAN
         NROWA  = K
      ELSE IF( INFO.EQ.0 ) THEN
         ITRANS = INOTRAN
         NROWA  = 0
         INFO   = 2
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF(      N  .LT.0               ) THEN
            INFO = 3
         ELSE IF( K  .LT.0               ) THEN
            INFO = 4
         ELSE IF( LDA.LT.MAX( 1, NROWA ) ) THEN
            INFO = 7
         ELSE IF( LDC.LT.MAX( 1, N     ) ) THEN
            INFO = 10
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CSYRK ', INFO )
         RETURN
      END IF
*
      CALL ATL_F77WRAP_CSYRK( IUPLO, ITRANS, N, K, ALPHA, A, LDA,
     $                        BETA, C, LDC )
*
      RETURN
*
*     End of CSYRK
*
      END
