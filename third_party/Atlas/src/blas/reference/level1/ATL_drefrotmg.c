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
#include "atlas_reflevel1.h"

#define   GAM           4096.0
#define   GAMSQ         16777216.0

void ATL_drefrotmg
(
   double                     * D1,
   double                     * D2,
   double                     * X1,
   const double               Y1,
   double                     * PARAM
)
{
/*
 * Purpose
 * =======
 *
 * ATL_drefrotmg  constructs the modified-Givens plane rotation. The in-
 * put scalars d1, d2, x1 and y1 define a 2-vector [a1 a2]' such that
 *
 *    [ a1 ]   [ d1^{1/2}  0      ] [ x1 ]
 *    [ a2 ] = [   0     d2^{1/2} ] [ y1 ].
 *
 * This subroutine determines the modified Givens rotation matrix H that
 * transforms y1 and thus a2 to zero. A representation of this matrix is
 * stored in the output array PARAM.
 *
 * See ``Basic Linear Algebra Subprograms for Fortran Usage'' by C. Law-
 * son, R. Hanson, D. Kincaid and F. Krogh, ACM Transactions on Mathema-
 * tical Software, 1979, 5(3) pp 308-323, for further information.
 *
 * Arguments
 * =========
 *
 * D1      (input/output)                double *
 *         On entry, D1 specifies the scalar d1.
 *
 * D2      (input/output)                double *
 *         On entry, D2 specifies the scalar d2.
 *
 * X1      (input/output)                double *
 *         On entry, X1 specifies the scalar x1.
 *
 * Y1      (input)                       const double
 *         On entry, Y1 specifies the scalar y1. Unchanged on exit.
 *
 * PARAM   (output)                      double *
 *         On entry, PARAM is an array of dimension at least 5. On exit,
 *         the entries of this array have the following meaning:
 *
 *         if PARAM[ 0 ] = 1,
 *            h_12 = 1, h_21 = -1, PARAM[ 1 ] = h_11, PARAM[ 4 ] = h_22,
 *            and the other entries of PARAM are left unchanged;
 *         else if PARAM[ 0 ] = 0,
 *            h_11 = 1, h_22 =  1, PARAM[ 2 ] = h_21, PARAM[ 3 ] = h_12,
 *            and the other entries of PARAM are left unchanged;
 *         else if PARAM[ 0 ] = -1, (case of re-scaling)
 *            PARAM( 2 ) = h_11, PARAM[ 2 ] = h_21,
 *            PARAM( 4 ) = h_12, PARAM[ 4 ] = h_22;
 *         else if PARAM[ 0 ] = -2,
 *            H = I, and the other entries of PARAM are left unchanged;
 *         end if
 *
 * ---------------------------------------------------------------------
 */
/*
 * .. Local Variables ..
 */
   static const double        gam   = GAM,   rgam   = ATL_dONE / GAM;
   static const double        gamsq = GAMSQ, rgamsq = ATL_dONE / GAMSQ;
   double                     d1 = (*D1), d2 = (*D2), flag, h11, h12, h21, h22,
                              p1, p2, q1, q2, tmp, u, x1 = (*X1);
/* ..
 * .. Executable Statements ..
 *
 */
   h11 = h12 = h21 = h22 = ATL_dZERO;

   if( d1 < ATL_dZERO )
   {
       PARAM[0] = ATL_dNONE;
       PARAM[1] = PARAM[2] = PARAM[3] = PARAM[4] = ATL_dZERO;
       *D1 = *D2 = *X1 = ATL_dZERO;
       return;
   }

   if( ( p2 = d2 * Y1 ) == ATL_dZERO )
   { PARAM[0] = ATL_dNTWO; return; }

   p1 = d1 * x1; q2 = p2 * Y1; q1 = p1 * x1;

   if( Mdabs( q1 ) > Mdabs( q2 ) )
   {
      h21 = - Y1 / x1; h12 = p2 / p1; u = ATL_dONE - h12 * h21;

      if( u <= ATL_dZERO )
      {
         PARAM[0] = ATL_dNONE;
         PARAM[1] = PARAM[2] = PARAM[3] = PARAM[4] = ATL_dZERO;
         *D1 = *D2 = *X1 = ATL_dZERO;
         return;
      }
      flag = ATL_dZERO; d1 /= u; d2 /= u; x1 *= u;
   }
   else
   {
      if( q2 < ATL_dZERO )
      {
         PARAM[0] = ATL_dNONE;
         PARAM[1] = PARAM[2] = PARAM[3] = PARAM[4] = ATL_dZERO;
         *D1 = *D2 = *X1 = ATL_dZERO;
         return;
      }
      flag = ATL_dONE;
      h11  = p1 / p2; h22  = x1 / Y1;
      u    = ATL_dONE + h11 * h22; tmp  = d2 / u;
      d2   = d1 / u; d1   = tmp; x1 = Y1 * u;
   }

   if( d1 <= rgamsq )
   {
      if( d1 != ATL_dZERO )                      /* scale d1 up */
      {
         if(      flag == ATL_dZERO )
         { h11  = ATL_dONE;  h22  = ATL_dONE; flag = ATL_dNONE; }
         else if( flag >  ATL_dZERO )
         { h21  = ATL_dNONE; h12  = ATL_dONE; flag = ATL_dNONE; }

         do
         {
            d1 *= gamsq; x1 *= rgam; h11 *= rgam; h12 *= rgam;
         } while( d1 <= rgamsq );
      }
   }
   else if( d1 >= gamsq )                            /* scale d1 down */
   {
      if(      flag == ATL_dZERO )
      { h11  = ATL_dONE;  h22  = ATL_dONE; flag = ATL_dNONE; }
      else if( flag > ATL_dZERO )
      { h21  = ATL_dNONE; h12  = ATL_dONE; flag = ATL_dNONE; }

      do
      {
         d1 *= rgamsq; x1 *= gam; h11 *= gam; h12 *= gam;
      } while( d1 >= gamsq );
   }

   if( ( tmp = Mdabs( d2 ) ) <= rgamsq )
   {
      if( d2 != ATL_dZERO )                      /* scale d2 up */
      {
         if(      flag == ATL_dZERO )
         { h11  = ATL_dONE;  h22  = ATL_dONE; flag = ATL_dNONE; }
         else if( flag > ATL_dZERO )
         { h21  = ATL_dNONE; h12  = ATL_dONE; flag = ATL_dNONE;
         }

         if( d2 > ATL_dZERO )
         {
            do
            {
               d2 *= gamsq; h21 *= rgam; h22 *= rgam;
            } while( d2 <=  rgamsq );
         }
         else
         {
            do
            {
               d2 *= gamsq; h21 *= rgam; h22 *= rgam;
            } while( d2 >= -rgamsq );
         }
      }
   }
   else if( tmp >= gamsq )                         /* scale d2 down */
   {
      if(      flag == ATL_dZERO )
      { h11  = ATL_dONE;  h22  = ATL_dONE; flag = ATL_dNONE; }
      else if( flag >  ATL_dZERO )
      { h21  = ATL_dNONE; h12  = ATL_dONE; flag = ATL_dNONE; }

      if( d2 > ATL_dZERO )
      {
         do
         {
            d2 *= rgamsq; h21 *= gam; h22 *= gam;
         } while( d2 >= gamsq );
      }
      else
      {
         do
         {
            d2 *= rgamsq; h21 *= gam; h22 *= gam;
         } while( d2 <= -gamsq );
      }
   }

   *D1 = d1; *D2 = d2; *X1 = x1;

   PARAM[0] = flag;
   if(      flag <  ATL_dZERO )
   { PARAM[1] = h11; PARAM[2] = h21; PARAM[3] = h12; PARAM[4] = h22; }
   else if( flag == ATL_dZERO )
   {                 PARAM[2] = h21; PARAM[3] = h12;                 }
   else
   { PARAM[1] = h11;                                 PARAM[4] = h22; }
/*
 * End of ATL_drefrotmg
 */
}
