      SUBROUTINE SGER2( M, N, ALPHA, X, INCX, Y, INCY, BETA,
     $                  W, INCW, Z, INCZ, A, LDA )
*
*  -- Automatically Tuned Linear Algebra Software (ATLAS)
*     (C) Copyright 2010 All Rights Reserved
*
*  -- ATLAS routine -- F77 Interface -- Version 3.9.24 -- 2010
*
*  Author         : R. Clint Whaley
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
      INTEGER            INCX, INCY, INCW, INCZ, LDA, M, N
      REAL               ALPHA, BETA
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * ), W( * ), Z( * )
*     ..
*
*
*     .. Local Scalars ..
      INTEGER            INFO
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATL_F77WRAP_SGER2, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      IF(      M.LT.0 ) THEN
         INFO = 1
      ELSE IF( N.LT.0 ) THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 ) THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 ) THEN
         INFO = 7
      ELSE IF( INCW.EQ.0 ) THEN
         INFO = 10
      ELSE IF( INCZ.EQ.0 ) THEN
         INFO = 12
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = 14
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGER2 ', INFO )
         RETURN
      END IF
*
      CALL ATL_F77WRAP_SGER2( M, N, ALPHA, X, INCX, Y, INCY,
     $                        BETA, W, INCW, Z, INCZ, A, LDA )
*
      RETURN
*
*     End of SGER2
*
      END
