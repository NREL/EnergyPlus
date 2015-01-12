/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 2000 Antoine P. Petitet
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include "atlas_misc.h"
#include "atlas_tst.h"

static void ATL_ladd
(
   int *                      J,
   int *                      K,
   int *                      I
)
{
/*
 * Purpose
 * =======
 *
 * ATL_ladd adds  without carry two long positive integers  K and J  an
 * put the result into I.  The long integers  I, J, K are encoded on 31
 * bits using an array of 2 integers.  The 16-lower bits  are stored  i
 * the  first  entry  of each array,  the 15-higher bits  in the second
 * entry.
 *
 * Arguments
 * =========
 *
 * J       (local input)                 int *
 *         On entry, J is an integer array of dimension 2 containing the
 *         encoded long integer J.
 *
 * K       (local input)                 int *
 *         On entry, K is an integer array of dimension 2 containing the
 *         encoded long integer K.
 *
 * I       (local output)                int *
 *         On entry, I is an integer array of dimension 2. On exit, this
 *         array contains the encoded long integer result.
 *
 * ---------------------------------------------------------------------
 */
   int                        itmp0 = K[0] + J[0], itmp1;
/*
 *    K[1] K[0] K  I[0]  = (K[0]+J[0]) % 2^16
 *    0XXX XXXX    carry = (K[0]+J[0]) / 2^16
 *
 * +  J[1] J[0] J  I[1] = K[1] + J[1] + carry
 *    0XXX XXXX    I[1] = I[1] % 2^15
 *    -------------
 *    I[1] I[0]
 *    0XXX XXXX I
 */
   itmp1 = itmp0 >> 16;         I[0] = itmp0 - ( itmp1 << 16 );
   itmp0 = itmp1 + K[1] + J[1]; I[1] = itmp0 - (( itmp0 >> 15 ) << 15);
}

static void ATL_lmul
(
   int *                      K,
   int *                      J,
   int *                      I
)
{
/*
 * Purpose
 * =======
 *
 * ATL_lmul multiplies  without carry two long positive integers K and J
 * and put the result into I.  The long integers  I, J, K are encoded on
 * 31 bits using an array of 2 integers. The 16-lower bits are stored in
 * the first entry of each array, the 15-higher bits in the second entry
 * of each array. For efficiency purposes, the  intrisic modulo function
 * is inlined.
 *
 * Arguments
 * =========
 *
 * K       (local input)                 int *
 *         On entry, K is an integer array of dimension 2 containing the
 *         encoded long integer K.
 *
 * J       (local input)                 int *
 *         On entry, J is an integer array of dimension 2 containing the
 *         encoded long integer J.
 *
 * I       (local output)                int *
 *         On entry, I is an integer array of dimension 2. On exit, this
 *         array contains the encoded long integer result.
 *
 * ---------------------------------------------------------------------
 */
   static int                 ipow30 = ( 1 << 30 );
   int                        kt, lt;
/*
 *    K[1] K[0] K  kt = K[0]*J[0]
 *    0XXX XXXX    if(kt < 0) kt += 2^31
 * x               I[0] = kt % 2^16
 *                 lt = K[0]*J[1] + K[1]*J[0]
 *    J[1] J[0] J  if(lt < 0) lt += 2^31
 *    0XXX XXXX    kt = (kt / 2^16) + lt
 * --------------  if(kt < 0) kt += 2^31
 *    I[1] I[0]    I[1] = kt % 2^15
 *    0XXX XXXX I
 */
   kt   = K[0] * J[0]; if( kt < 0 ) kt = ( kt + ipow30 ) + ipow30;
   I[0] = kt - ( ( kt >> 16 ) << 16 );
   lt   = K[0] * J[1] + K[1] * J[0];
   if( lt < 0 ) lt = ( lt + ipow30 ) + ipow30;
   kt = ( kt >> 16 ) + lt;
   if( kt < 0 ) kt = ( kt + ipow30 ) + ipow30;
   I[1] = kt - ( ( kt >> 15 ) << 15 );
}

static void ATL_setran
(
   const int                  OPTION,
   int *                      IRAN
)
{
/*
 * Purpose
 * =======
 *
 * ATL_setran initializes  the random generator with the encoding of the
 * first number X(0) in the sequence,  and the constants a and c used to
 * compute the next element in the sequence: X(n+1) = a*X(n) + c.  X(0),
 * a and c are stored in the static variables  irand, ias and ics.  When
 * OPTION is 0 (resp. 1 and 2),  irand  (resp. ia and ic)  is set to the
 * values of the input array IRAN.  When OPTION is 3, IRAN is set to the
 * current value of irand, and irand is then incremented.
 *
 * Arguments
 * =========
 *
 * OPTION  (local input)                 const int
 *         On entry, OPTION  is an integer that specifies the operations
 *         to be performed on the random generator as specified above.
 *
 * IRAN    (local input/output)          int *
 *         On entry,  IRAN is an array of dimension 2, that contains the
 *         16-lower and 15-higher bits of a random number.
 *
 * ---------------------------------------------------------------------
 */
   static int                 ias[2], ics[2], irand[2];
   int                        j[2];

   if(      OPTION == 3 )
   {                                       /* return current value */
      IRAN[0] = irand[0]; IRAN[1] = irand[1];
      ATL_lmul( irand, ias, j );         /* j     = irand * ias;   */
      ATL_ladd( j, ics, irand );         /* irand = j     + ics;   */
   }
   else if( OPTION == 0 ) { irand[0] = IRAN[0]; irand[1] = IRAN[1]; }
   else if( OPTION == 1 ) { ias  [0] = IRAN[0]; ias  [1] = IRAN[1]; }
   else if( OPTION == 2 ) { ics  [0] = IRAN[0]; ics  [1] = IRAN[1]; }
}

static void ATL_xjumpm
(
   const int                  JUMPM,
   int *                      MULT,
   int *                      IADD,
   int *                      IRANN,
   int *                      IRANM,
   int *                      IAM,
   int *                      ICM
)
{
/*
 * Purpose
 * =======
 *
 * ATL_xjumpm computes  the constants  A and C  to jump JUMPM numbers in
 * the random sequence: X(n+JUMPM) = A*X(n)+C.  The constants encoded in
 * MULT and IADD  specify  how to jump from one entry in the sequence to
 * the next.
 *
 * Arguments
 * =========
 *
 * JUMPM   (local input)                 const int
 *         On entry,  JUMPM  specifies  the  number  of entries  in  the
 *         sequence to jump over. When JUMPM is less or equal than zero,
 *         A and C are not computed, IRANM is set to IRANN corresponding
 *         to a jump of size zero.
 *
 * MULT    (local input)                 int *
 *         On entry, MULT is an array of dimension 2,  that contains the
 *         16-lower  and 15-higher bits of the constant  a  to jump from
 *         X(n) to X(n+1) = a*X(n) + c in the random sequence.
 *
 * IADD    (local input)                 int *
 *         On entry, IADD is an array of dimension 2,  that contains the
 *         16-lower  and 15-higher bits of the constant  c  to jump from
 *         X(n) to X(n+1) = a*X(n) + c in the random sequence.
 *
 * IRANN   (local input)                 int *
 *         On entry, IRANN is an array of dimension 2. that contains the
 *         16-lower and 15-higher bits of the encoding of X(n).
 *
 * IRANM   (local output)                int *
 *         On entry,  IRANM  is an array of dimension 2.   On exit, this
 *         array  contains respectively  the 16-lower and 15-higher bits
 *         of the encoding of X(n+JUMPM).
 *
 * IAM     (local output)                int *
 *         On entry, IAM is an array of dimension 2. On exit, when JUMPM
 *         is  greater  than  zero,  this  array  contains  the  encoded
 *         constant  A  to jump from  X(n) to  X(n+JUMPM)  in the random
 *         sequence. IAM(0:1)  contains  respectively  the  16-lower and
 *         15-higher  bits  of this constant  A. When  JUMPM  is less or
 *         equal than zero, this array is not referenced.
 *
 * ICM     (local output)                int *
 *         On entry, ICM is an array of dimension 2. On exit, when JUMPM
 *         is  greater  than  zero,  this  array  contains  the  encoded
 *         constant  C  to jump from  X(n)  to  X(n+JUMPM) in the random
 *         sequence. ICM(0:1)  contains  respectively  the  16-lower and
 *         15-higher  bits  of this constant  C. When  JUMPM  is less or
 *         equal than zero, this array is not referenced.
 *
 * ---------------------------------------------------------------------
 */
   int                        j[2], k;

   if( JUMPM > 0 )
   {
      IAM[0] = MULT[0]; IAM[1] = MULT[1];   /* IAM   = MULT;          */
      ICM[0] = IADD[0]; ICM[1] = IADD[1];   /* ICM   = IADD;          */
      for( k = 1; k <= JUMPM-1; k++ )
      {
         ATL_lmul( IAM, MULT, j );          /* j     = IAM   * MULT;  */
         IAM[0] = j[0]; IAM[1] = j[1];      /* IAM   = j;             */
         ATL_lmul( ICM, MULT, j );          /* j     = ICM   * MULT;  */
         ATL_ladd( IADD, j, ICM );          /* ICM   = IADD  + j;     */
      }
      ATL_lmul( IRANN, IAM, j );            /* j     = IRANN * IAM;   */
      ATL_ladd( j, ICM, IRANM );            /* IRANM = j     + ICM;   */
   }
   else
   {                                        /* IRANM = IRANN          */
      IRANM[0] = IRANN[0]; IRANM[1] = IRANN[1];
   }
}


void ATL_srand(int iseed)
{
   int iadd[2], ia1[2], ic1[2], iran1[2], jseed[2], mult[2];

   mult [0] = 20077; mult[1] = 16838;
   iadd [0] = 12345; iadd [1] = 0;
   jseed[0] = iseed; jseed[1] = (iseed>>16);

   ATL_xjumpm( 1, mult, iadd, jseed, iran1, ia1, ic1 );
   ATL_setran( 0, iran1 ); ATL_setran( 1, ia1 ); ATL_setran( 2, ic1 );
}

int ATL_rand(void)
{
   int j[2];

   ATL_setran( 3, j );
   return(j[0] + ((j[1])<<16));
}
