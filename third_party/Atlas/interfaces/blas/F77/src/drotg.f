      SUBROUTINE DROTG( A, B, C, S )
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
      DOUBLE PRECISION   C, A, B, S
*     ..
*
*  Purpose
*  =======
*
*  DROTG  constructs a Givens plane rotation. Given the scalars a and b,
*  this routine computes the following quantities:
*
*     sigma = sgn(a) if |a| > |b|, sgn(b) otherwise;
*     r     = sigma * sqrt( a^2 + b^2 );
*     c     = a / r if r <> 0, 1 otherwise;
*     s     = b / r if r <> 0, 0 otherwise.
*
*  The numbers c, s and r then satisfy the matrix equation:
*
*     [ c  s ] [ a ]    [ r ]
*     [ -s c ] [ b ] =  [ 0 ].
*
*  The introduction of  sigma  is not essential  to the computation of a
*  Givens rotation matrix, but it permits later stable reconstruction of
*  c and s from just one stored number.  For  this purpose, this routine
*  also computes
*
*          s        if |a| > |b|,
*     z =  1 / c    if |b| >= |a| and c <> 0,
*          1        if c = 0.
*
*  This subroutine returns r overwriting a, and z overwriting b, as well
*  as returning c and s. If one later wishes to reconstruct c and s from
*  z, it can be done as follows:
*
*     if  z  = 1,    set c = 0             and s = 1,
*     if |z| < 1,    set c = sqrt(1 - z^2) and s = z,
*     if |z| > 1,    set c = 1 / z         and s = sqrt(1 - c^2).
*
*  Arguments
*  =========
*
*  A       (input/output)                DOUBLE PRECISION
*          On entry, A specifies the scalar a. On exit, A is overwritten
*          by the scalar r defined above.
*
*  B       (input/output)                DOUBLE PRECISION
*          On entry, B specifies the scalar b. On exit, B is overwritten
*          by the scalar z defined above.
*
*  C       (output)                      DOUBLE PRECISION
*          On exit, C  specifies the scalar c defined above.
*
*  S       (output)                      DOUBLE PRECISION
*          On exit, S  specifies the scalar s defined above.
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
      EXTERNAL           ATL_F77WRAP_DROTG
*     ..
*     .. Executable Statements ..
*
      CALL ATL_F77WRAP_DROTG( A, B, C, S )
*
      RETURN
*
*     End of DROTG
*
      END
