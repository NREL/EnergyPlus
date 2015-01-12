#include "atlas_misc.h"
#include <math.h>

#if defined(ATL_AltiVec) && defined(SCPLX)

#define ATL_NoFakePF
#include "atlas_prefetch.h"

static int ATL_amax_av(const int N, const TYPE *X, const int incX)
/*
 * Assuming aligned X && N multiple of 4, finds abs max
*/
{
   const TYPE *stX = X + N+N;
#ifdef ATL_AVgcc
   const vector unsigned char vp0 = (vector unsigned char)
      {0, 1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19, 24, 25, 26, 27};
   const vector unsigned char vp1 = (vector unsigned char)
      {4, 5, 6, 7, 12, 13, 14, 15, 20, 21, 22, 23, 28, 29, 30, 31};
   vector float v0, v1, v2, v3, vmax = (vector float){0.0f, 0.0f, 0.0f, 0.0f};
#else
   const vector unsigned char vp0 = (vector unsigned char)
      (0, 1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19, 24, 25, 26, 27);
   const vector unsigned char vp1 = (vector unsigned char)
      (4, 5, 6, 7, 12, 13, 14, 15, 20, 21, 22, 23, 28, 29, 30, 31);
   vector float v0, v1, v2, v3, vmax = (vector float)(0.0f, 0.0f, 0.0f, 0.0f);
#endif
   const TYPE *xp=X, *x=X;
   char ch[128];
   TYPE *tp;
   int i;
   register TYPE r0, r1;

   tp = ATL_AlignPtr((void*)ch);
   do
   {
      v0 = vec_ldl(0, X);
      v0 = vec_abs(v0);
      v1 = vec_ldl(0, X+4); X += 8;
      v1 = vec_abs(v1);
      v2 = vec_perm(v0, v1, vp0);
      v3 = vec_perm(v0, v1, vp1);
      v0 = vec_add(v2, v3);
      if (vec_all_ge(vmax, v0)) continue;

      vmax = vec_max(v0, vmax);
      v0 = vec_splat(vmax, 0);
      v1 = vec_splat(vmax, 1);
      v2 = vec_splat(vmax, 2);
      vmax = vec_splat(vmax, 3);
      v0 = vec_max(v0, v1);
      vmax = vec_max(v2, vmax);
      vmax = vec_max(v0, vmax);
      xp = X - 8;
   }
   while (X != stX);
   vec_st(vmax, 0, tp);
   r1 = *tp;
   for (i=0; i < 4; i++)
   {
      r0 = fabs(*xp) + fabs(xp[1]);
      if (r0 == r1) break;
      xp += 2;
   }
   if (i == 4) exit(-1);
   return(((int)(xp-x))>>1);
}

static int ATL_amax(const int N, const TYPE *X, const int incX)
{
   const int incX2 = incX+incX;
   register TYPE xr, xi, yr, yi, xmax;
   int imax=0;
   const TYPE *stX = X + N+N, *xp=X, *x;
   int cwrd = ATL_MulBySize(N)>>4;

   switch(N)
   {
   case 1:
      return(0);
   case 2:
      xr = fabs(*X) + fabs(X[1]);
      xi = fabs(X[2]) + fabs(X[3]);
      if (xr >= xi) return(0);
      else return(1);
   case 3:
      xr = fabs(*X) + fabs(X[1]);
      xi = fabs(X[2]) + fabs(X[3]);
      yr = fabs(X[4]) + fabs(X[5]);
      if (xr >= xi && xr >= yr) return(0);
      else if (xi >= yr) return(1);
      else return(2);
   default:;
   }
   xr = *X;
   xi = X[1];
   xmax = fabs(xr) + fabs(xi);
   if ((N>>1)<<1 == N)
   {
      xr = X[2]; xi = X[3];
      xr = fabs(xr) + fabs(xi);
      if (xr > xmax) { xmax = xr; xp = X + 2; }
      x = X + 4;
   }
   else x = X + 2;
   if (N > 2)
   {
      do
      {
         ATL_pfl1R(x + 32);
         xr = *x; xi = x[1]; yr = x[2]; yi = x[3];  x += 4;
         xr = fabs(xr) + fabs(xi); yr = fabs(yr) + fabs(yi);
         if (xmax >= xr && xmax >= yr) continue;
         else if (xr >= yr) { xmax = xr; xp = x - 4; }
         else { xmax = yr; xp = x - 2; }
      }
      while(x != stX);
   }
   imax = (int) (xp - X);
   return(imax>>1);
}

static int GetMax(int I0, int I1, const TYPE *X)
/*
 * Returns max of two maxes, assuming i0 is smaller index
 */
{
   register TYPE r0, r1;
   const int i0 = I0<<1, i1 = I1<<1;
   r0 = fabs(X[i0]) + fabs(X[i0+1]);
   r1 = fabs(X[i1]) + fabs(X[i1+1]);
   if (r0 >= r1) return(I0);
   else return(I1);
}

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   int cwrd = ATL_MulBySize(N)>>4;
   int im=0;
   int n0, n1, n2;

   if (N > 64)
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
      n0 = ATL_AlignOffset(N, X, ATL_sizeof, ATL_MulBySize(4));
      if (n0) im = ATL_amax(n0, X, incX);
      n1 = ((N - n0)>>2)<<2;
      n2 = N - n0 - n1;
      if (n1) im = GetMax(im, n0+ATL_amax_av(n1, X+n0+n0, incX), X);
      if (n2) im = GetMax(im, n0+n1+ATL_amax(n2, X+n0+n0+n1+n1, incX), X);
   }
   else im = ATL_amax(N, X, incX);
   return(im);
}

#else

int ATL_UIAMAX(const int N, const TYPE *X, const int incX)
{
   const int incX2 = incX+incX;
   register TYPE xr, xi, yr, yi, xmax;
   int imax=0;
   const TYPE *stX = X + N+N, *xp=X, *x;

   if (N > 0)
   {
      xr = *X;
      xi = X[1];
      xmax = fabs(xr) + fabs(xi);
      if ((N>>1)<<1 == N)
      {
         xr = X[2]; xi = X[3];
         xr = fabs(xr) + fabs(xi);
         if (xr > xmax) { xmax = xr; xp = X + 2; }
         x = X + 4;
      }
      else x = X + 2;
   }
   if (N > 2)
   {
      do
      {
         xr = *x; xi = x[1]; yr = x[2]; yi = x[3];  x += 4;
         xr = fabs(xr) + fabs(xi); yr = fabs(yr) + fabs(yi);
         if (xmax >= xr && xmax >= yr) continue;
         else if (xr >= yr) { xmax = xr; xp = x - 4; }
         else { xmax = yr; xp = x - 2; }
      }
      while(x != stX);
   }
   imax = (int) (xp - X);
   return(imax>>1);
}

#endif
