/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
 * Originally developed at the University of Tennessee,
 * Innovative Computing Laboratory, Knoxville TN, 37996-1301, USA.
 *
 * ---------------------------------------------------------------------
 *
 * -- Copyright notice and Licensing terms:
 *
 *  Redistribution  and  use in  source and binary forms, with or without
 *  modification, are  permitted provided  that the following  conditions
 *  are met:
 *
 * 1. Redistributions  of  source  code  must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce  the above copyright
 *    notice,  this list of conditions, and the  following disclaimer in
 *    the documentation and/or other materials provided with the distri-
 *    bution.
 * 3. The name of the University,  the ATLAS group,  or the names of its
 *    contributors  may not be used to endorse or promote products deri-
 *    ved from this software without specific written permission.
 *
 * -- Disclaimer:
 *
 * THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
 * RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
 * CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ---------------------------------------------------------------------
 */
/*
 * Include files
 */
#include "atlas_refmisc.h"
#include "atlas_reflvl3.h"
#include "atlas_reflevel3.h"

void ATL_creftrsmRLNU
(
   const int                  M,
   const int                  N,
   const float                * ALPHA,
   const float                * A,
   const int                  LDA,
   float                      * B,
   const int                  LDB
)
{
/*
 * Purpose
 * =======
 *
 * ATL_creftrsmRLNU( ... )
 *
 * <=>
 *
 * ATL_creftrsm
 * ( AtlasRight, AtlasLower, AtlasNoTrans, AtlasUnit, ... )
 *
 * See ATL_creftrsm for details.
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   int                        i, iakj, ibij, ibik, j, jaj, jbj, jbk, k,
                              lda2 = ( LDA << 1 ), ldb2 = ( LDB << 1 );
/* ..
 * .. Executable Statements ..
 *
 */
   for( j = N-1,      jaj  = (N-1)*lda2, jbj  = (N-1)*ldb2;
        j >= 0;  j--, jaj -= lda2,       jbj -= ldb2 )
   {
      for( i = 0, ibij  = jbj; i < M; i++, ibij += 2 )
      { Msscl( ALPHA[0], ALPHA[1], B[ibij], B[ibij+1] ); }
      for( k = j+1,    iakj  = ((j+1) << 1)+jaj, jbk  = (j+1)*ldb2;
           k < N; k++, iakj += 2,                jbk += ldb2 )
      {
         for( i = 0, ibij = jbj, ibik = jbk; i < M; i++, ibij += 2, ibik += 2 )
         { Mmls( A[iakj], A[iakj+1], B[ibik], B[ibik+1], B[ibij], B[ibij+1] ); }
      }
   }
/*
 * End of ATL_creftrsmRLNU
 */
}
