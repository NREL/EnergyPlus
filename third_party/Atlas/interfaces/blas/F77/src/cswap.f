      SUBROUTINE CSWAP( N, X, INCX, Y, INCY )
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
      INTEGER            INCX, INCY, N
*     ..
*     .. Array Arguments ..
      COMPLEX            X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  CSWAP swaps the entries of two n-vectors x and y.
*
*  Arguments
*  =========
*
*  N       (input)                       INTEGER
*          On entry, N specifies the length of the vector x. N  must  be
*          at least zero. Unchanged on exit.
*
*  X       (input/output)                COMPLEX array
*          On entry,  X  is an incremented array of dimension  at  least
*          ( 1 + ( n - 1 ) * abs( INCX ) )  that  contains the vector x.
*          On exit, the entries of the incremented array  X  are swapped
*          with the entries of the incremented array  Y.
*
*  INCX    (input)                       INTEGER
*          On entry, INCX specifies the increment for the elements of X.
*          INCX must not be zero. Unchanged on exit.
*
*  Y       (input/output)                COMPLEX array
*          On entry,  Y  is an incremented array of dimension  at  least
*          ( 1 + ( n - 1 ) * abs( INCY ) )  that  contains the vector y.
*          On exit, the entries of the incremented array  Y  are swapped
*          with the entries of the incremented array  X.
*
*  INCY    (input)                       INTEGER
*          On entry, INCY specifies the increment for the elements of Y.
*          INCY must not be zero. Unchanged on exit.
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
*     .. External Subroutines ..
      EXTERNAL           ATL_F77WRAP_CSWAP
*     ..
*     .. Executable Statements ..
*
      IF( N.GT.0 ) THEN
         CALL ATL_F77WRAP_CSWAP( N, X, INCX, Y, INCY )
      END IF
*
      RETURN
*
*     End of CSWAP
*
      END
