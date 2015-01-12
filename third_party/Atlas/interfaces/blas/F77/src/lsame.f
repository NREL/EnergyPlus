      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- Automatically Tuned Linear Algebra Software (ATLAS)
*     (C) Copyright 2000 All Rights Reserved
*
*  -- ATLAS routine -- F77 Interface -- Version 3.9.24 -- December 25, 2000
*
*  Author         : LAPACK (version 3.0)
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
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of ca-
*  se.
*
*  Notes
*  =====
*
*  This routine is identical to the LSAME function provided in LAPACK.
*
*  Arguments
*  =========
*
*  CA, CB  (input)                       CHARACTER*1
*          CA and CB specify the single characters to be compared.
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
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Executable Statements ..
*
      LSAME = ( CA.EQ.CB )
*
      IF( LSAME )
     $   RETURN
*
      ZCODE = ICHAR( 'Z' )
      INTA  = ICHAR( CA  )
      INTB  = ICHAR( CB  )
*
      IF(      ( ZCODE.EQ.90  ).OR.( ZCODE.EQ.122 ) ) THEN
*
         IF( ( INTA.GE.97 ).AND.( INTA.LE.122 ) )
     $      INTA = INTA - 32
         IF( ( INTB.GE.97 ).AND.( INTB.LE.122 ) )
     $      INTB = INTB - 32
*
      ELSE IF( ( ZCODE.EQ.233 ).OR.( ZCODE.EQ.169 ) ) THEN
*
         IF( ( INTA.GE.129 ).AND.( INTA.LE.137 ).OR.
     $       ( INTA.GE.145 ).AND.( INTA.LE.153 ).OR.
     $       ( INTA.GE.162 ).AND.( INTA.LE.169 ) )
     $      INTA = INTA + 64
*
         IF( ( INTB.GE.129 ).AND.( INTB.LE.137 ).OR.
     $       ( INTB.GE.145 ).AND.( INTB.LE.153 ).OR.
     $       ( INTB.GE.162 ).AND.( INTB.LE.169 ) )
     $      INTB = INTB + 64
*
      ELSE IF( ( ZCODE.EQ.218 ).OR.( ZCODE.EQ.250 ) ) THEN
*
         IF( ( INTA.GE.225 ).AND.( INTA.LE.250 ) )
     $      INTA = INTA - 32
         IF( ( INTB.GE.225 ).AND.( INTB.LE.250 ) )
     $      INTB = INTB - 32
*
      END IF
*
      LSAME = ( INTA.EQ.INTB )
*
      RETURN
*
*     End of LSAME
*
      END
