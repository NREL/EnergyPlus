      SUBROUTINE SROTMG( D1, D2, X1, Y1, PARAM )
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
      REAL               D1, D2, X1, Y1
*     ..
*     .. Array Arguments ..
      REAL               PARAM( * )
*     ..
*
*  Purpose
*  =======
*
*  SROTMG constructs the modified-Givens plane rotation.  The input sca-
*  lars d1, d2, x1 and y1 define a 2-vector [a1 a2]' such that
*
*     [ a1 ]   [ d1^{1/2}  0      ] [ x1 ]
*     [ a2 ] = [   0     d2^{1/2} ] [ y1 ].
*
*  This subroutine determines the modified Givens rotation matrix H that
*  transforms y1 and thus a2 to zero. A representation of this matrix is
*  stored in the output array PARAM.
*
*  Arguments
*  =========
*
*  D1      (input/output)                REAL
*          On entry, D1 specifies the scalar d1.
*
*  D2      (input/output)                REAL
*          On entry, D2 specifies the scalar d2.
*
*  X1      (input/output)                REAL
*          On entry, X1 specifies the scalar x1.
*
*  Y1      (input)                       REAL
*          On entry, Y1 specifies the scalar y1. Unchanged on exit.
*
*  PARAM   (output)                      REAL array
*          On entry, PARAM is an array of dimension at least 5. On exit,
*          the entries of this array have the following meaning:
*
*          if PARAM( 1 ) = 1,
*             h_12 = 1, h_21 = -1, PARAM( 2 ) = h_11, PARAM( 5 ) = h_22,
*             and the other entries of PARAM are left unchanged;
*          else if PARAM( 1 ) = 0,
*             h_11 = 1, h_22 =  1, PARAM( 3 ) = h_21, PARAM( 4 ) = h_12,
*             and the other entries of PARAM are left unchanged;
*          else if PARAM( 1 ) = -1, (case of re-scaling)
*             PARAM( 2 ) = h_11, PARAM( 3 ) = h_21,
*             PARAM( 4 ) = h_12, PARAM( 5 ) = h_22;
*          else if PARAM( 1 ) = -2,
*             H = I, and the other entries of PARAM are left unchanged;
*          end if
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
      EXTERNAL           ATL_F77WRAP_SROTMG
*     ..
*     .. Executable Statements ..
*
      CALL ATL_F77WRAP_SROTMG( D1, D2, X1, Y1, PARAM )
*
      RETURN
*
*     End of SROTMG
*
      END
