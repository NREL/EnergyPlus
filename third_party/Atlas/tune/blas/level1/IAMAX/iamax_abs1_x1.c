#include "atlas_misc.h"
#include <math.h>
#include "atlas_prefetch.h"

#if defined(ATL_AltiVec) && defined(SREAL)

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
/*
 * Special code for AltiVec, using vector instructions
 */
{
   register TYPE xmax, x0, x1, x2, x3;
   const TYPE *stX=X, *x, *xp=X, *vxp;
   int i, nr;
   vector float v0, v1, v2, vmax = VECTOR_INIT(0.0f, 0.0f, 0.0f, 0.0f);
   void *vp;
   float *tp;
   int cwrd = ATL_MulBySize(N)>>4;
   char ch[64];

   if (N > 0)
   {
      if (cwrd >= 64)
      {
         cwrd = (cwrd+31)>>5;
         if (cwrd <= 256) cwrd = ATL_GetCtrl(512, cwrd <= 255 ? cwrd : 0, 0);
         else /* use all pipes */
         {
            cwrd >>= 2;
            cwrd = ATL_GetCtrl(2048, cwrd <= 255 ? cwrd : 0, 0);
            ATL_pfavR(X+128, cwrd, 1);
            ATL_pfavR(X+256, cwrd, 2);
            ATL_pfavR(X+384, cwrd, 3);
         }
      }
      else cwrd = ATL_GetCtrl(64, (cwrd+3)>>2, 4);
      ATL_pfavR(X, cwrd, 0);

      nr = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(4));
      if (nr)
      {
         x0 = *X;
         xmax = fabs(x0);
         for (i=1; i < nr; i++)
         {
            x0 = fabs(X[i]);
            if (x0 > xmax) { xmax = x0; xp = X+i; }
         }
         x = X + i;
         nr = ((N - nr)>>2)<<2;
      }
      else
      {
         xmax = ATL_rzero;
         x = X;
         nr = (N>>2)<<2;
      }
/*      if ( (((size_t)x)>>4)<<4 != (size_t)x ) exit(-1); */
      if (nr)
      {
         stX = x + nr;
         vxp = x;
         do
         {
            v0 = vec_ldl(0, x); x += 4;
            v0 = vec_abs(v0);
            if (vec_all_ge(vmax, v0)) continue;
            vmax = vec_max(v0, vmax);
            v0 = vec_splat(vmax, 0);
            v1 = vec_splat(vmax, 1);
            v2 = vec_splat(vmax, 2);
            vmax = vec_splat(vmax, 3);
            v0 = vec_max(v0, v1);
            vmax = vec_max(v2, vmax);
            vmax = vec_max(v0, vmax);
            vxp = x - 4;
         }
         while (x != stX);
         tp = ATL_AlignPtr((void*)ch);
         vec_st(vmax, 0, tp);
         for (i=0; i < 4; i++)
         {
            if (tp[i] > xmax) { xmax = tp[i]; xp = vxp; }
         }
         if (xp == vxp)
         {
            for (i=0; i < 4; i++) if (fabs(xp[i]) == xmax) break;
            if (i == 4) exit(-1);
            xp += i;
         }
      }
      stX = X + N;
      while (x != stX)
      {
         x0 = fabs(*x);
         if (x0 > xmax) { xmax = x0; xp = x; }
         x++;
      }
   }
   return((int)(xp-X));
}

#else

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   register TYPE xmax, x0;
   int i, iret=0;
   if (N > 0)
   {
      xmax = *X;
      xmax = fabs(xmax);
      for (i=1; i < N; i++)
      {
         x0 = X[i];
         x0 = fabs(x0);
         if (x0 <= xmax) continue;
         else
         {
            xmax = x0;
            iret = i;
         }
      }
   }
   return(iret);
}

#endif
