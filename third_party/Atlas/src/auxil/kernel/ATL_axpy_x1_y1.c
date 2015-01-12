/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009, 1999 R. Clint Whaley
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


#if (defined(ATL_MULADD) && ATL_mmnreg >= 29) || \
    (defined(ATL_NOMULADD) && ATL_mmnreg >= 25)
static void axpy_lt16(ATL_CINT N, const SCALAR alpha, const TYPE *x, TYPE *y)
{
   const register TYPE alp = alpha;
   ATL_INT i;
   switch(N)
   {
   case 1:
      *y += alp * *x;
      break;
   case 2:
      *y += alp * *x;
      y[1] += alp * x[1];
      break;
   case 3:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      break;
   case 4:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      break;
   case 5:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      break;
   case 6:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      break;
   case 7:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      break;
   case 8:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      break;
   case 9:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      break;
   case 10:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      y[9] += alp * x[9];
      break;
   case 11:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      y[9] += alp * x[9];
      y[10] += alp * x[10];
      break;
   case 12:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      y[9] += alp * x[9];
      y[10] += alp * x[10];
      y[11] += alp * x[11];
      break;
   case 13:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      y[9] += alp * x[9];
      y[10] += alp * x[10];
      y[11] += alp * x[11];
      y[12] += alp * x[12];
      break;
   case 14:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      y[9] += alp * x[9];
      y[10] += alp * x[10];
      y[11] += alp * x[11];
      y[12] += alp * x[12];
      y[13] += alp * x[13];
      break;
   case 15:
      *y += alp * *x;
      y[1] += alp * x[1];
      y[2] += alp * x[2];
      y[3] += alp * x[3];
      y[4] += alp * x[4];
      y[5] += alp * x[5];
      y[6] += alp * x[6];
      y[7] += alp * x[7];
      y[8] += alp * x[8];
      y[9] += alp * x[9];
      y[10] += alp * x[10];
      y[11] += alp * x[11];
      y[12] += alp * x[12];
      y[13] += alp * x[13];
      y[14] += alp * x[14];
      break;
   default:
      for (i=(N>>4); i; i--, y += 16, x += 16)
      {
         *y += alp * *x;
         y[1] += alp * x[1];
         y[2] += alp * x[2];
         y[3] += alp * x[3];
         y[4] += alp * x[4];
         y[5] += alp * x[5];
         y[6] += alp * x[6];
         y[7] += alp * x[7];
         y[8] += alp * x[8];
         y[9] += alp * x[9];
         y[10] += alp * x[10];
         y[11] += alp * x[11];
         y[12] += alp * x[12];
         y[13] += alp * x[13];
         y[14] += alp * x[14];
         y[15] += alp * x[15];
      }
      for(i=N-((N>>4)<<4); i; i--, y++, x++) *y += alp * *x;
   case 0:;
   }
}
#endif

#if defined(ATL_MULADD) && ATL_mmnreg >= 29
static void axpy_16(ATL_CINT N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 * 4 register prefetch on X (assumed to be in L1), 16 register prefetch on
 * Y (L2 or main), with 8-cycle muladd.  Unrolled by 16 to ensure multiple
 * cacheline usage for both single and double.
 */
{
   ATL_CINT N16 = (N>>4)<<4;
   ATL_INT i, j;
   const TYPE *stX = x + N16 - 32;
   const register TYPE alp = alpha;
   register TYPE m0, m1, m2, m3, m4, m5, m6, m7;
   register TYPE x0, x1, xx0, xx1;
   register TYPE y0, y1, y2, y3, y4, y5, y6, y7;
   register TYPE yy0, yy1, yy2, yy3, yy4, yy5, yy6, yy7;

   if (N16 > 16)
   {
      x0 = *x; xx0 = x[8];
      x1 = x[1]; xx1 = x[9];
      y0 = *y;   yy0 = y[8];
      y1 = y[1]; yy1 = y[9];
      y2 = y[2]; yy2 = y[10];
      y3 = y[3]; yy3 = y[11];
      y4 = y[4]; yy4 = y[12];
      y5 = y[5]; yy5 = y[13];
      y6 = y[6]; yy6 = y[14];
      y7 = y[7]; yy7 = y[15];
      m0 = y0  + alp * x0;   x0 = x[2];  y0  = y[16];
      m1 = yy0 + alp * xx0; xx0 = x[10]; yy0 = y[24];
      m2 = y1  + alp * x1;   x1 = x[3];  y1  = y[17];
      m3 = yy1 + alp * xx1; xx1 = x[11]; yy1 = y[25];
      m4 = y2  + alp * x0;   x0 = x[4];  y2  = y[18];
      m5 = yy2 + alp * xx0; xx0 = x[12]; yy2 = y[26];
      m6 = y3  + alp * x1;   x1 = x[5];  y3  = y[19];
      m7 = yy3 + alp * xx1; xx1 = x[13]; yy3 = y[27];
      if (N16 != 32)
      {
         do
         {
            *y    = m0; m0 =  y4 + alp *  x0;  x0 = x[ 6];  y4 = y[20];
            y[ 8] = m1; m1 = yy4 + alp * xx0; xx0 = x[14]; yy4 = y[28];
            y[ 1] = m2; m2 =  y5 + alp *  x1;  x1 = x[ 7];  y5 = y[21];
            y[ 9] = m3; m3 = yy5 + alp * xx1; xx1 = x[15]; yy5 = y[29]; x += 16;
            y[ 2] = m4; m4 =  y6 + alp *  x0;  x0 = *x;     y6 = y[22];
            y[10] = m5; m5 = yy6 + alp * xx0; xx0 = x[ 8]; yy6 = y[30];
            y[ 3] = m6; m6 =  y7 + alp *  x1;  x1 = x[ 1];  y7 = y[23];
            y[11] = m7; m7 = yy7 + alp * xx1; xx1 = x[ 9]; yy7 = y[31];

            y[ 4] = m0; m0 = y0  + alp * x0;   x0 = x[2];  y0  = y[32];
            y[12] = m1; m1 = yy0 + alp * xx0; xx0 = x[10]; yy0 = y[40];
            y[ 5] = m2; m2 = y1  + alp * x1;   x1 = x[3];  y1  = y[33];
            y[13] = m3; m3 = yy1 + alp * xx1; xx1 = x[11]; yy1 = y[41];
            y[ 6] = m4; m4 = y2  + alp * x0;   x0 = x[4];  y2  = y[34];
            y[14] = m5; m5 = yy2 + alp * xx0; xx0 = x[12]; yy2 = y[42];
            y[ 7] = m6; m6 = y3  + alp * x1;   x1 = x[5];  y3  = y[35];
            y[15] = m7; m7 = yy3 + alp * xx1; xx1 = x[13]; yy3 = y[43];
            y += 16;
         }
         while (x != stX);
      }
      *y    = m0; m0 =  y4 + alp *  x0;  x0 = x[ 6];  y4 = y[20];
      y[ 8] = m1; m1 = yy4 + alp * xx0; xx0 = x[14]; yy4 = y[28];
      y[ 1] = m2; m2 =  y5 + alp *  x1;  x1 = x[ 7];  y5 = y[21];
      y[ 9] = m3; m3 = yy5 + alp * xx1; xx1 = x[15]; yy5 = y[29]; x += 16;
      y[ 2] = m4; m4 =  y6 + alp *  x0;  x0 = *x;     y6 = y[22];
      y[10] = m5; m5 = yy6 + alp * xx0; xx0 = x[ 8]; yy6 = y[30];
      y[ 3] = m6; m6 =  y7 + alp *  x1;  x1 = x[ 1];  y7 = y[23];
      y[11] = m7; m7 = yy7 + alp * xx1; xx1 = x[ 9]; yy7 = y[31];

      y[ 4] = m0; m0 = y0  + alp * x0;   x0 = x[2];
      y[12] = m1; m1 = yy0 + alp * xx0; xx0 = x[10];
      y[ 5] = m2; m2 = y1  + alp * x1;   x1 = x[3];
      y[13] = m3; m3 = yy1 + alp * xx1; xx1 = x[11];
      y[ 6] = m4; m4 = y2  + alp * x0;   x0 = x[4];
      y[14] = m5; m5 = yy2 + alp * xx0; xx0 = x[12];
      y[ 7] = m6; m6 = y3  + alp * x1;   x1 = x[5];
      y[15] = m7; m7 = yy3 + alp * xx1; xx1 = x[13];
      y += 16;

      *y    = m0; m0 =  y4 + alp *  x0;  x0 = x[ 6];
      y[ 8] = m1; m1 = yy4 + alp * xx0; xx0 = x[14];
      y[ 1] = m2; m2 =  y5 + alp *  x1;  x1 = x[ 7];
      y[ 9] = m3; m3 = yy5 + alp * xx1; xx1 = x[15]; x += 16;
      y[ 2] = m4; m4 =  y6 + alp *  x0;
      y[10] = m5; m5 = yy6 + alp * xx0;
      y[ 3] = m6; m6 =  y7 + alp *  x1;
      y[11] = m7; m7 = yy7 + alp * xx1;

      y[ 4] = m0;
      y[12] = m1;
      y[ 5] = m2;
      y[13] = m3;
      y[ 6] = m4;
      y[14] = m5;
      y[ 7] = m6;
      y[15] = m7;
      y += 16;
      axpy_lt16(N-N16, alpha, x, y);
   }
   else axpy_lt16(N, alpha, x, y);
}
#elif defined(ATL_NOMULADD) && ATL_mmnreg >= 25
static void axpy_16(ATL_CINT N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 * 8 register prefetch on X & Y, with 4 cycle multiply & 4 cycle add,
 * unrolled by 16 to ensure multiple cacheline usage for both singe & double
 */
{
   const register TYPE alp = alpha;
   register TYPE x0, x1, x2, x3, xx0, xx1, xx2, xx3;
   register TYPE y0, y1, y2, y3, yy0, yy1, yy2, yy3;
   register TYPE m0, m1, m2, m3, a0, a1, a2, a3;
   const TYPE *stX = x + N;

   ATL_assert( ((N>>4)<<4) == N  && N);
   x0 = *x;    xx0 = x[8];
   x1 = x[1];  xx1 = x[9];
   x2 = x[2];  xx2 = x[10];
   x3 = x[3];  xx3 = x[11];
   y0 = *y;    yy0 = y[8];
   y1 = y[1];  yy1 = y[9];
   y2 = y[2];  yy2 = y[10];
   y3 = y[3];  yy3 = y[11];

   m0 = alp * x0;  x0 = x[4];
   m1 = alp * xx0; xx0 = x[12];
   m2 = alp * x1;  x1 = x[5];
   m3 = alp * xx1; xx1 = x[13];

   a0 = y0 + m0;  m0 = alp * x2;  y0 = y[4];   x2 = x[6];
   a1 = yy0 + m1; m1 = alp * xx2; yy0 = y[12]; xx2 = x[14];
   a2 = y1 + m2;  m2 = alp * x3;  y1 = y[5];   x3 = x[7];
   a3 = yy1 + m3; m3 = alp * xx3; yy1 = y[13]; xx3 = x[15];
   x += 16;
   if (N != 16)
   {
      do
      {
         *y    = a0; a0 =  y2 + m0;  y2 = y[ 6]; m0 = alp *  x0;  x0 = *x;
         y[ 8] = a1; a1 = yy2 + m1; yy2 = y[14]; m1 = alp * xx0; xx0 = x[ 8];
         y[ 1] = a2; a2 =  y3 + m2;  y3 = y[7];  m2 = alp *  x1;  x1 = x[ 1];
         y[ 9] = a3; a3 = yy3 + m3; yy3 = y[15]; m3 = alp * xx1; xx1 = x[ 9];
         y[ 2] = a0; a0 =  y0 + m0;  y0 = y[16]; m0 = alp *  x2;  x2 = x[ 2];
         y[10] = a1; a1 = yy0 + m1; yy0 = y[24]; m1 = alp * xx2; xx2 = x[10];
         y[ 3] = a2; a2 =  y1 + m2;  y1 = y[17]; m2 = alp *  x3;  x3 = x[ 3];
         y[11] = a3; a3 = yy1 + m3; yy1 = y[25]; m3 = alp * xx3; xx3 = x[11];

         y[ 4] = a0; a0 =  y2 + m0;  y2 = y[18]; m0 = alp *  x0;  x0 = x[ 4];
         y[12] = a1; a1 = yy2 + m1; yy2 = y[26]; m1 = alp * xx0; xx0 = x[12];
         y[ 5] = a2; a2 =  y3 + m2;  y3 = y[19]; m2 = alp *  x1;  x1 = x[ 5];
         y[13] = a3; a3 = yy3 + m3; yy3 = y[27]; m3 = alp * xx1; xx1 = x[13];
         y[ 6] = a0; a0 =  y0 + m0;  y0 = y[20]; m0 = alp *  x2;  x2 = x[ 6];
         y[14] = a1; a1 = yy0 + m1; yy0 = y[28]; m1 = alp * xx2; xx2 = x[14];
         y[ 7] = a2; a2 =  y1 + m2;  y1 = y[21]; m2 = alp *  x3;  x3 = x[ 7];
         y[15] = a3; a3 = yy1 + m3; yy1 = y[29]; m3 = alp * xx3; xx3 = x[15];
         x += 16;
         y += 16;
      }
      while (x != stX);
   }
/*
 * Drain pipes
 */
   *y    = a0; a0 =  y2 + m0;  y2 = y[ 6]; m0 = alp *  x0;
   y[ 8] = a1; a1 = yy2 + m1; yy2 = y[14]; m1 = alp * xx0;
   y[ 1] = a2; a2 =  y3 + m2;  y3 = y[7];  m2 = alp *  x1;
   y[ 9] = a3; a3 = yy3 + m3; yy3 = y[15]; m3 = alp * xx1;

   y[ 2] = a0; a0 =  y0 + m0;              m0 = alp *  x2;
   y[10] = a1; a1 = yy0 + m1;              m1 = alp * xx2;
   y[ 3] = a2; a2 =  y1 + m2;              m2 = alp *  x3;
   y[11] = a3; a3 = yy1 + m3;              m3 = alp * xx3;

   y[ 4] = a0; a0 =  y2 + m0;
   y[12] = a1; a1 = yy2 + m1;
   y[ 5] = a2; a2 =  y3 + m2;
   y[13] = a3; a3 = yy3 + m3;

   y[ 6] = a0;
   y[14] = a1;
   y[ 7] = a2;
   y[15] = a3;
}

#endif
void Mjoin(PATL,axpy_x1_y1)(ATL_CINT N, const SCALAR alpha, const TYPE *x,
                            ATL_CINT incX, TYPE *y, ATL_CINT incY)
/*
 * y <- alpha * x + y
 */
{
#if defined(ATL_MULADD) && ATL_mmnreg >= 29
   axpy_16(N, alpha, x, y);
#elif defined(ATL_NOMULADD) && (ATL_mmnreg >= 25)
   const int inb =
             ATL_DivBySize(((size_t)y)) - ((ATL_DivBySize(((size_t)y))>>4)<<4);
   ATL_INT n16, nr;

   if (inb < N)
   {
      n16 = ((N - inb)>>4)<<4;
      nr = N - inb - n16;
      if (inb)
      {
         axpy_lt16(inb, alpha, x, y);
         x += inb; y += inb;
      }
      if (n16)
      {
         axpy_16(n16, alpha, x, y);
         x += n16; y += n16;
      }
      if (nr) axpy_lt16(nr, alpha, x, y);
   }
   else axpy_lt16(N, alpha, x, y);
#else
#endif
}
