#include "atlas_misc.h"

static void axpyCU(const int N, const SCALAR alpha, const TYPE *x, TYPE *y)
/*
 *  For cleanup, see if we can get compiler to do the work, use constant loops
 */
{
   int i;
   const register TYPE ralpha = *alpha, ialpha = alpha[1];
   register TYPE xr, xi;
   switch(N)
   {
   case 1:
      xr = *x; xi = x[1];
      *y   += ralpha * xr - ialpha * xi;
      y[1] += ialpha * xr + ralpha * xi;
      break;
   case 2:
      for (i=2; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 3:
      for (i=3; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 4:
      for (i=4; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 5:
      for (i=5; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 6:
      for (i=6; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   case 7:
      for (i=7; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
      break;
   default:
      for (i=N; i; i--, x += 2, y += 2)
      {
         xr = *x; xi = x[1];
         *y   += ralpha * xr - ialpha *xi;
         y[1] += ialpha * xr + ralpha *xi;
      }
   }
}

void ATL_UAXPY(const int N, const SCALAR alpha, const TYPE *x, const int incX,
               TYPE *y, const int incY)
/*
 * OK, this guy may look a little complicated.  It's for a combined muladd
 * arch, and the big trick is that it splits both X and Y in fourths, then
 * does 4 parallel AXPYs on the fourths.  This is to utilize up to eight
 * prefetch units/streams.  We then unroll the loop by 2 for each axpy,
 * and prefetch one iteration ahead.  I wrote this routine, and I gotta admit
 * my head aches when I look at it . . .
 */
{
   const int n4 = N >> 2, N2 = (n4>>1)<<1, nr = n4 - N2, nn4 = n4<<1;
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

      yr0 -= xi0 * ialpha; yi1 = y[3];
      ur0 -= xi1 * ialpha; ui1 = u[3];
      vr0 -= xi2 * ialpha; vi1 = v[3];
      zr0 -= xi3 * ialpha; zi1 = z[3];

      yi0 += xi0 * ralpha; xr0 = x[2];
      ui0 += xi1 * ralpha; xr1 = X1[2];
      vi0 += xi2 * ralpha; xr2 = X2[2];
      zi0 += xi3 * ralpha; xr3 = X3[2];

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

            yr1 -= xi0 * ialpha; yi0 = y[5];
            ur1 -= xi1 * ialpha; ui0 = u[5];
            vr1 -= xi2 * ialpha; vi0 = v[5];
            zr1 -= xi3 * ialpha; zi0 = z[5];

            yi1 += xi0 * ralpha; xr0 = *x;
            ui1 += xi1 * ralpha; xr1 = *X1;
            vi1 += xi2 * ralpha; xr2 = *X2;
            zi1 += xi3 * ralpha; xr3 = *X3;

            y[2] = yr1; yr0 += xr0 * ralpha; xi0 = x[1];
            u[2] = ur1; ur0 += xr1 * ralpha; xi1 = X1[1];
            v[2] = vr1; vr0 += xr2 * ralpha; xi2 = X2[1];
            z[2] = zr1; zr0 += xr3 * ralpha; xi3 = X3[1];

            y[3] = yi1; yi0 += xr0 * ialpha; yr1 = y[6];
            u[3] = ui1; ui0 += xr1 * ialpha; ur1 = u[6];
            v[3] = vi1; vi0 += xr2 * ialpha; vr1 = v[6];
            z[3] = zi1; zi0 += xr3 * ialpha; zr1 = z[6];

            yr0 -= xi0 * ialpha; yi1 = y[7];
            ur0 -= xi1 * ialpha; ui1 = u[7]; y += 4;
            vr0 -= xi2 * ialpha; vi1 = v[7];
            zr0 -= xi3 * ialpha; zi1 = z[7]; u += 4;

            yi0 += xi0 * ralpha; xr0 = x[2]; v += 4;
            ui0 += xi1 * ralpha; xr1 = X1[2];
            vi0 += xi2 * ralpha; xr2 = X2[2]; z += 4;
            zi0 += xi3 * ralpha; xr3 = X3[2];
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

            yr1 -= xi0 * ialpha;
            ur1 -= xi1 * ialpha;
            vr1 -= xi2 * ialpha;
            zr1 -= xi3 * ialpha;

            yi1 += xi0 * ralpha;
            ui1 += xi1 * ralpha;
            vi1 += xi2 * ralpha;
            zi1 += xi3 * ralpha;

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

            yr1 -= xi0 * ialpha; yi0 = y[5];
            ur1 -= xi1 * ialpha; ui0 = u[5];
            vr1 -= xi2 * ialpha; vi0 = v[5];
            zr1 -= xi3 * ialpha; zi0 = z[5];

            yi1 += xi0 * ralpha; xr0 = *x;
            ui1 += xi1 * ralpha; xr1 = *X1;
            vi1 += xi2 * ralpha; xr2 = *X2;
            zi1 += xi3 * ralpha; xr3 = *X3;

            y[2] = yr1; yr0 += xr0 * ralpha; xi0 = x[1];
            u[2] = ur1; ur0 += xr1 * ralpha; xi1 = X1[1];
            v[2] = vr1; vr0 += xr2 * ralpha; xi2 = X2[1];
            z[2] = zr1; zr0 += xr3 * ralpha; xi3 = X3[1]; X3 += 2;

            y[3] = yi1; yi0 += xr0 * ialpha;
            u[3] = ui1; ui0 += xr1 * ialpha;
            v[3] = vi1; vi0 += xr2 * ialpha;
            z[3] = zi1; zi0 += xr3 * ialpha;

            yr0 -= xi0 * ialpha; y += 4;
            ur0 -= xi1 * ialpha; u += 4;
            vr0 -= xi2 * ialpha; v += 4;
            zr0 -= xi3 * ialpha; z += 4;

            yi0 += xi0 * ralpha;
            ui0 += xi1 * ralpha;
            vi0 += xi2 * ralpha;
            zi0 += xi3 * ralpha;

            *y = yr0;
            *u = ur0;
            *v = vr0;
            *z = zr0;

            y[1] = yi0;
            u[1] = ui0;
            v[1] = vi0;
            z[1] = zi0; z += 2;
      }
      if (N-(n4<<2)) axpyCU(N-(n4<<2), alpha, X3, z);
   }
   else axpyCU(N, alpha, x, y);
}
