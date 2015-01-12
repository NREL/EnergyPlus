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


static void axpy_lt8(ATL_CINT N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 * For cleanup, see if we can get compiler to do the work, use constant loops
 */
{
   ATL_INT i;
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   register TYPE xr, xi;

   switch(N)
   {
   case 1:
      xr = *x; xi = x[1];
      #ifndef Conj_
         *y   += ralpha * xr - ialpha * xi;
         y[1] += ialpha * xr + ralpha * xi;
      #else
         *y   += ralpha * xr + ialpha * xi;
         y[1] += ialpha * xr - ralpha * xi;
      #endif
      break;
   case 2:
      for (i=0; i != 2; i++, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         #ifndef Conj_
            *y   += ralpha * xr - ialpha * xi;
            y[1] += ialpha * xr + ralpha * xi;
         #else
            *y   += ralpha * xr + ialpha * xi;
            y[1] += ialpha * xr - ralpha * xi;
         #endif
      }
      break;
   case 3:
      for (i=0; i != 3; i++, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         #ifndef Conj_
            *y   += ralpha * xr - ialpha * xi;
            y[1] += ialpha * xr + ralpha * xi;
         #else
            *y   += ralpha * xr + ialpha * xi;
            y[1] += ialpha * xr - ralpha * xi;
         #endif
      }
      break;
   case 4:
      for (i=0; i != 4; i++, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         #ifndef Conj_
            *y   += ralpha * xr - ialpha * xi;
            y[1] += ialpha * xr + ralpha * xi;
         #else
            *y   += ralpha * xr + ialpha * xi;
            y[1] += ialpha * xr - ralpha * xi;
         #endif
      }
      break;
   case 5:
      for (i=0; i != 5; i++, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         #ifndef Conj_
            *y   += ralpha * xr - ialpha * xi;
            y[1] += ialpha * xr + ralpha * xi;
         #else
            *y   += ralpha * xr + ialpha * xi;
            y[1] += ialpha * xr - ralpha * xi;
         #endif
      }
      break;
   case 6:
      for (i=0; i != 6; i++, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         #ifndef Conj_
            *y   += ralpha * xr - ialpha * xi;
            y[1] += ialpha * xr + ralpha * xi;
         #else
            *y   += ralpha * xr + ialpha * xi;
            y[1] += ialpha * xr - ralpha * xi;
         #endif
      }
      break;
   case 7:
      for (i=0; i != 7; i++, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         #ifndef Conj_
            *y   += ralpha * xr - ialpha * xi;
            y[1] += ialpha * xr + ralpha * xi;
         #else
            *y   += ralpha * xr + ialpha * xi;
            y[1] += ialpha * xr - ralpha * xi;
         #endif
      }
      break;
   default:;
   }
}
#if defined (ATL_MULADD) && ATL_mmnreg >= 26
#ifdef Conj_
   #define PEQ -=
   #define MEQ +=
#else
   #define PEQ +=
   #define MEQ -=
#endif
static void axpy_8(ATL_CINT N, const SCALAR alpha, const TYPE *x, TYPE *y)
{
   ATL_CINT n4 = N >> 2, N2 = (n4>>1)<<1, nr = n4 - N2, nn4 = n4<<1;
   TYPE *u = y+nn4, *v = u+nn4, *z = v+nn4;
   const TYPE *X1 = x + nn4, *X2 = X1 + nn4, *X3 = X2 + nn4;
   const TYPE *stX = x + ((N2-2)<<1);
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   register TYPE yr0, yi0, yr1, yi1;
   register TYPE ur0, ui0, ur1, ui1;
   register TYPE vr0, vi0, vr1, vi1;
   register TYPE zr0, zi0, zr1, zi1;
   register TYPE xr0, xi0, xr1, xi1;
   register TYPE xr2, xi2, xr3, xi3;

   if (N2)
   {
      yr0 = *y; ur0 = *u; vr0 = *v; zr0 = *z;
      yi0 = y[1]; ui0 = u[1]; vi0 = v[1]; zi0 = z[1];
      xr0 = *x; xr1 = *X1; xr2 = *X2; xr3 = *X3;

      yr0 += xr0 * ralpha; xi0 = x[1];
      ur0 += xr1 * ralpha; xi1 = X1[1];
      vr0 += xr2 * ralpha; xi2 = X2[1];
      zr0 += xr3 * ralpha; xi3 = X3[1];

      yi0 += xr0 * ialpha; yr1 = y[2];
      ui0 += xr1 * ialpha; ur1 = u[2];
      vi0 += xr2 * ialpha; vr1 = v[2];
      zi0 += xr3 * ialpha; zr1 = z[2];

      yr0 MEQ xi0 * ialpha; yi1 = y[3];
      ur0 MEQ xi1 * ialpha; ui1 = u[3];
      vr0 MEQ xi2 * ialpha; vi1 = v[3];
      zr0 MEQ xi3 * ialpha; zi1 = z[3];

      yi0 PEQ xi0 * ralpha; xr0 = x[2];
      ui0 PEQ xi1 * ralpha; xr1 = X1[2];
      vi0 PEQ xi2 * ralpha; xr2 = X2[2];
      zi0 PEQ xi3 * ralpha; xr3 = X3[2];

      if (N2 != 2)
      {
         do
         {
            *y = yr0; yr1 += xr0 * ralpha; xi0 = x[3]; x += 4;
            *u = ur0; ur1 += xr1 * ralpha; xi1 = X1[3]; X1 += 4;
            *v = vr0; vr1 += xr2 * ralpha; xi2 = X2[3]; X2 += 4;
            *z = zr0; zr1 += xr3 * ralpha; xi3 = X3[3]; X3 += 4;

            y[1] = yi0; yi1 += xr0 * ialpha; yr0 = y[4];
            u[1] = ui0; ui1 += xr1 * ialpha; ur0 = u[4];
            v[1] = vi0; vi1 += xr2 * ialpha; vr0 = v[4];
            z[1] = zi0; zi1 += xr3 * ialpha; zr0 = z[4];

            yr1 MEQ xi0 * ialpha; yi0 = y[5];
            ur1 MEQ xi1 * ialpha; ui0 = u[5];
            vr1 MEQ xi2 * ialpha; vi0 = v[5];
            zr1 MEQ xi3 * ialpha; zi0 = z[5];

            yi1 PEQ xi0 * ralpha; xr0 = *x;
            ui1 PEQ xi1 * ralpha; xr1 = *X1;
            vi1 PEQ xi2 * ralpha; xr2 = *X2;
            zi1 PEQ xi3 * ralpha; xr3 = *X3;

            y[2] = yr1; yr0 += xr0 * ralpha; xi0 = x[1];
            u[2] = ur1; ur0 += xr1 * ralpha; xi1 = X1[1];
            v[2] = vr1; vr0 += xr2 * ralpha; xi2 = X2[1];
            z[2] = zr1; zr0 += xr3 * ralpha; xi3 = X3[1];

            y[3] = yi1; yi0 += xr0 * ialpha; yr1 = y[6];
            u[3] = ui1; ui0 += xr1 * ialpha; ur1 = u[6];
            v[3] = vi1; vi0 += xr2 * ialpha; vr1 = v[6];
            z[3] = zi1; zi0 += xr3 * ialpha; zr1 = z[6];

            yr0 MEQ xi0 * ialpha; yi1 = y[7];
            ur0 MEQ xi1 * ialpha; ui1 = u[7]; y += 4;
            vr0 MEQ xi2 * ialpha; vi1 = v[7];
            zr0 MEQ xi3 * ialpha; zi1 = z[7]; u += 4;

            yi0 PEQ xi0 * ralpha; xr0 = x[2]; v += 4;
            ui0 PEQ xi1 * ralpha; xr1 = X1[2];
            vi0 PEQ xi2 * ralpha; xr2 = X2[2]; z += 4;
            zi0 PEQ xi3 * ralpha; xr3 = X3[2];
         }
         while (x != stX);
      }
      if (!nr) /* finish off this iteratation only */
      {
            *y = yr0; yr1 += xr0 * ralpha; xi0 = x[3];
            *u = ur0; ur1 += xr1 * ralpha; xi1 = X1[3];
            *v = vr0; vr1 += xr2 * ralpha; xi2 = X2[3];
            *z = zr0; zr1 += xr3 * ralpha; xi3 = X3[3]; X3 += 4;

            y[1] = yi0; yi1 += xr0 * ialpha;
            u[1] = ui0; ui1 += xr1 * ialpha;
            v[1] = vi0; vi1 += xr2 * ialpha;
            z[1] = zi0; zi1 += xr3 * ialpha;

            yr1 MEQ xi0 * ialpha;
            ur1 MEQ xi1 * ialpha;
            vr1 MEQ xi2 * ialpha;
            zr1 MEQ xi3 * ialpha;

            yi1 PEQ xi0 * ralpha;
            ui1 PEQ xi1 * ralpha;
            vi1 PEQ xi2 * ralpha;
            zi1 PEQ xi3 * ralpha;

            y[2] = yr1;
            u[2] = ur1;
            v[2] = vr1;
            z[2] = zr1;

            y[3] = yi1;
            u[3] = ui1;
            v[3] = vi1;
            z[3] = zi1; z += 4;
      }
      else     /* one iteration to do besides finishing off one from loop */
      {
            *y = yr0; yr1 += xr0 * ralpha; xi0 = x[3]; x += 4;
            *u = ur0; ur1 += xr1 * ralpha; xi1 = X1[3]; X1 += 4;
            *v = vr0; vr1 += xr2 * ralpha; xi2 = X2[3]; X2 += 4;
            *z = zr0; zr1 += xr3 * ralpha; xi3 = X3[3]; X3 += 4;

            y[1] = yi0; yi1 += xr0 * ialpha; yr0 = y[4];
            u[1] = ui0; ui1 += xr1 * ialpha; ur0 = u[4];
            v[1] = vi0; vi1 += xr2 * ialpha; vr0 = v[4];
            z[1] = zi0; zi1 += xr3 * ialpha; zr0 = z[4];

            yr1 MEQ xi0 * ialpha; yi0 = y[5];
            ur1 MEQ xi1 * ialpha; ui0 = u[5];
            vr1 MEQ xi2 * ialpha; vi0 = v[5];
            zr1 MEQ xi3 * ialpha; zi0 = z[5];

            yi1 PEQ xi0 * ralpha; xr0 = *x;
            ui1 PEQ xi1 * ralpha; xr1 = *X1;
            vi1 PEQ xi2 * ralpha; xr2 = *X2;
            zi1 PEQ xi3 * ralpha; xr3 = *X3;

            y[2] = yr1; yr0 += xr0 * ralpha; xi0 = x[1];
            u[2] = ur1; ur0 += xr1 * ralpha; xi1 = X1[1];
            v[2] = vr1; vr0 += xr2 * ralpha; xi2 = X2[1];
            z[2] = zr1; zr0 += xr3 * ralpha; xi3 = X3[1]; X3 += 2;

            y[3] = yi1; yi0 += xr0 * ialpha;
            u[3] = ui1; ui0 += xr1 * ialpha;
            v[3] = vi1; vi0 += xr2 * ialpha;
            z[3] = zi1; zi0 += xr3 * ialpha;

            yr0 MEQ xi0 * ialpha; y += 4;
            ur0 MEQ xi1 * ialpha; u += 4;
            vr0 MEQ xi2 * ialpha; v += 4;
            zr0 MEQ xi3 * ialpha; z += 4;

            yi0 PEQ xi0 * ralpha;
            ui0 PEQ xi1 * ralpha;
            vi0 PEQ xi2 * ralpha;
            zi0 PEQ xi3 * ralpha;

            *y = yr0;
            *u = ur0;
            *v = vr0;
            *z = zr0;

            y[1] = yi0;
            u[1] = ui0;
            v[1] = vi0;
            z[1] = zi0; z += 2;
      }
      if (N-(n4<<2)) axpy_lt8(N-(n4<<2), alpha, X3, z);
   }
   else axpy_lt8(N, alpha, x, y);
}
   #undef PEQ
   #undef MEQ
#elif defined(ATL_NOMULADD) && ATL_mmnreg >= 26
static void axpy_8(ATL_CINT N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 * 8 register prefetch on X & Y, with 4 cycle multiply & 4 cycle add,
 * unrolled by 16 to ensure multiple cacheline usage for both singe & double
 */
{
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   register TYPE xr0, xi0, xr1, xi1, xxr0, xxi0, xxr1, xxi1;
   register TYPE yr0, yi0, yr1, yi1, yyr0, yyi0, yyr1, yyi1;
   register TYPE m0, m1, m2, m3, a0, a1, a2, a3;
   const TYPE *stX = x + (N<<1) - 16;

   ATL_assert( (N == (N>>3)<<3) && N );

   xr0  = *x;   xxr0 = x[8];
   xi0  = x[1]; xxi0 = x[9];
   xr1  = x[2]; xxr1 = x[10];
   xi1  = x[3]; xxi1 = x[11];

   yr0  = *y;   yyr0 = y[8];
   yi0  = y[1]; yyi0 = y[9];
   yr1  = y[2]; yyr1 = y[10];
   yi1  = y[3]; yyi1 = y[11];

   m0 = ralpha * xr0;
   m1 = ralpha * xxr0;
   m2 = ialpha * xr0; xr0  = x[4];
   m3 = ialpha *xxr0; xxr0 = x[12];

   a0 = yr0  + m0; m0 = ialpha *  xi0; yr0  = y[4];
   a1 = yyr0 + m1; m1 = ialpha * xxi0; yyr0 = y[12];
   a2 = yi0  + m2; m2 = ralpha *  xi0;  xi0  = x[5]; yi0  = y[5];
   a3 = yyi0 + m3; m3 = ralpha * xxi0; xxi0 = x[13]; yyi0 = y[13];

   #ifndef Conj_
      a0 -= m0; m0 = ralpha * xr1;
      a1 -= m1; m1 = ralpha * xxr1;
      a2 += m2; m2 = ialpha *  xr1; xr1  = x[6];
      a3 += m3; m3 = ialpha * xxr1; xxr1 = x[14];
   #else
      a0 += m0; m0 = ralpha * xr1;
      a1 += m1; m1 = ralpha * xxr1;
      a2 -= m2; m2 = ialpha *  xr1; xr1  = x[6];
      a3 -= m3; m3 = ialpha * xxr1; xxr1 = x[14];
   #endif
   if (N != 8)
   {
      do
      {
         *y   = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1;  yr1 = y[6];
         y[8] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1; yyr1 = y[14];
         y[1] = a2; a2 =  yi1 + m2; m2 = ralpha *  xi1;  xi1 = x[7];
                    yi1  = y[7];
         y[9] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1; xxi1 = x[15];
                    yyi1 = y[15]; x += 16;
         #ifndef Conj_
            a0 -= m0; m0 = ralpha *  xr0;
            a1 -= m1; m1 = ralpha * xxr0;
            a2 += m2; m2 = ialpha *  xr0; xr0 = *x;
            a3 += m3; m3 = ialpha * xxr0; xxr0 = x[8];
         #else
            a0 += m0; m0 = ralpha *  xr0;
            a1 += m1; m1 = ralpha * xxr0;
            a2 -= m2; m2 = ialpha *  xr0; xr0 = *x;
            a3 -= m3; m3 = ialpha * xxr0; xxr0 = x[8];
         #endif
         y[ 2] = a0; a0 =  yr0 + m0; m0 = ialpha *  xi0; yr0  = y[16];
         y[10] = a1; a1 = yyr0 + m1; m1 = ialpha * xxi0; yyr0 = y[24];
         y[ 3] = a2; a2 = yi0  + m2; m2 = ralpha *  xi0; xi0  = x[1];
                     yi0  = y[17];
         y[11] = a3; a3 = yyi0 + m3; m3 = ralpha * xxi0; xxi0 = x[9];
                     yyi0 = y[25];

         #ifndef Conj_
            a0 -= m0; m0 = ralpha *  xr1;
            a1 -= m1; m1 = ralpha * xxr1;
            a2 += m2; m2 = ialpha *  xr1; xr1  = x[2];
            a3 += m3; m3 = ialpha * xxr1; xxr1 = x[10];
         #else
            a0 += m0; m0 = ralpha *  xr1;
            a1 += m1; m1 = ralpha * xxr1;
            a2 -= m2; m2 = ialpha *  xr1; xr1  = x[2];
            a3 -= m3; m3 = ialpha * xxr1; xxr1 = x[10];
         #endif
         y[ 4] = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1; yr1  = y[18];
         y[12] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1; yyr1 = y[26];
         y[ 5] = a2; a2 = yi1  + m2; m2 = ralpha *  xi1; xi1  = x[3];
                     yi1  = y[19];
         y[13] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1; xxi1 = x[11];
                     yyi1 = y[27];
         #ifndef Conj_
            a0 -= m0; m0 = ralpha *  xr0;
            a1 -= m1; m1 = ralpha * xxr0;
            a2 += m2; m2 = ialpha *  xr0; xr0 = x[4];
            a3 += m3; m3 = ialpha * xxr0; xxr0 = x[12];
         #else
            a0 += m0; m0 = ralpha *  xr0;
            a1 += m1; m1 = ralpha * xxr0;
            a2 -= m2; m2 = ialpha *  xr0; xr0 = x[4];
            a3 -= m3; m3 = ialpha * xxr0; xxr0 = x[12];
         #endif
         y[ 6] = a0; a0 =  yr0 + m0; m0 = ialpha *  xi0; yr0  = y[20];
         y[14] = a1; a1 = yyr0 + m1; m1 = ialpha * xxi0; yyr0 = y[28];
         y[ 7] = a2; a2 = yi0  + m2; m2 = ralpha *  xi0; xi0  = x[5];
                     yi0  = y[21];
         y[15] = a3; a3 = yyi0 + m3; m3 = ralpha * xxi0; xxi0 = x[13];
                     yyi0 = y[29];
         y += 16;
         #ifndef Conj_
            a0 -= m0; m0 = ralpha *  xr1;
            a1 -= m1; m1 = ralpha * xxr1;
            a2 += m2; m2 = ialpha *  xr1; xr1  = x[6];
            a3 += m3; m3 = ialpha * xxr1; xxr1 = x[14];
         #else
            a0 += m0; m0 = ralpha *  xr1;
            a1 += m1; m1 = ralpha * xxr1;
            a2 -= m2; m2 = ialpha *  xr1; xr1  = x[6];
            a3 -= m3; m3 = ialpha * xxr1; xxr1 = x[14];
         #endif
      }
      while (x != stX);
   }
/*
 * Drain pipe, store last 8 elts of Y
 */
   *y   = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1;  yr1 = y[6];
   y[8] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1; yyr1 = y[14];
   y[1] = a2; a2 =  yi1 + m2; m2 = ralpha *  xi1;  xi1 = x[7]; yi1  = y[7];
   y[9] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1; xxi1 = x[15]; yyi1 = y[15];
   #ifndef Conj_
      a0 -= m0; m0 = ralpha *  xr0;
      a1 -= m1; m1 = ralpha * xxr0;
      a2 += m2; m2 = ialpha *  xr0;
      a3 += m3; m3 = ialpha * xxr0;
   #else
      a0 += m0; m0 = ralpha *  xr0;
      a1 += m1; m1 = ralpha * xxr0;
      a2 -= m2; m2 = ialpha *  xr0;
      a3 -= m3; m3 = ialpha * xxr0;
   #endif
   y[ 2] = a0; a0 =  yr0 + m0; m0 = ialpha *  xi0;
   y[10] = a1; a1 = yyr0 + m1; m1 = ialpha * xxi0;
   y[ 3] = a2; a2 = yi0  + m2; m2 = ralpha *  xi0;
   y[11] = a3; a3 = yyi0 + m3; m3 = ralpha * xxi0;

   #ifndef Conj_
      a0 -= m0; m0 = ralpha *  xr1;
      a1 -= m1; m1 = ralpha * xxr1;
      a2 += m2; m2 = ialpha *  xr1;
      a3 += m3; m3 = ialpha * xxr1;
   #else
      a0 += m0; m0 = ralpha *  xr1;
      a1 += m1; m1 = ralpha * xxr1;
      a2 -= m2; m2 = ialpha *  xr1;
      a3 -= m3; m3 = ialpha * xxr1;
   #endif
   y[ 4] = a0; a0 =  yr1 + m0; m0 = ialpha *  xi1;
   y[12] = a1; a1 = yyr1 + m1; m1 = ialpha * xxi1;
   y[ 5] = a2; a2 = yi1  + m2; m2 = ralpha *  xi1;
   y[13] = a3; a3 = yyi1 + m3; m3 = ralpha * xxi1;
   #ifndef Conj_
      a0 -= m0;
      a1 -= m1;
      a2 += m2;
      a3 += m3;
   #else
      a0 += m0;
      a1 += m1;
      a2 -= m2;
      a3 -= m3;
   #endif
   y[ 6] = a0;
   y[14] = a1;
   y[ 7] = a2;
   y[15] = a3;
}
#endif

#ifdef Conj_
void Mjoin(PATL,axpyConj_x1_y1)
/*
 * y <- alpha * Conj(x) + y
 */
#else
void Mjoin(PATL,axpy_x1_y1)
/*
 * y <- alpha * x + y
 */
#endif
   (ATL_CINT N, const SCALAR alpha, const TYPE *X, ATL_CINT incX,
    TYPE *Y, ATL_CINT incY)
{
#if defined(ATL_NOMULADD) && ATL_mmnreg >= 26
   ATL_CINT n8 = (N>>3)<<3, nr = N - n8;

   if (n8)
   {
      axpy_8(n8, alpha, X, Y);
      X += n8<<1;
      Y += n8<<1;
   }
   if (nr) axpy_lt8(nr, alpha, X, Y);
#elif defined (ATL_MULADD) && ATL_mmnreg >= 26
   axpy_8(N, alpha, X, Y);
#endif
}
