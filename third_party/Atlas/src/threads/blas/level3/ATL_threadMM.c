#include "atlas_misc.h"
#include "atlas_threads.h"
#include "atlas_tlvl3.h"

#ifdef DEBUG
#define T2c(ta_) ((ta_) == AtlasNoTrans) ? 'N' : 'T'
#endif
#ifndef ATL_TXOVER_H
int Mjoin(PATL,threadMM)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                         size_t M, size_t N, size_t K)
/*
 * This dummy routine used when crossover is not tuned
 */
{
#if 0
   size_t minD, maxD;

   minD = Mmin(M,N);
   minD = Mmin(minD,K);
   maxD = Mmax(M,N);
   maxD = Mmax(maxD,K);
   if (M >= (NB<<(ATL_NTHRPOW2+2)))
      return(2);
   else if (minD >= 8 && maxD >= 2*NB)
      return(1);
   return(0);
#else
   int Mjoin(PATL,GemmWillThread)(ATL_CINT M, ATL_CINT N, ATL_CINT K);
   return(Mjoin(PATL,GemmWillThread)(M, N, K));
#endif
}
#else
int Mjoin(PATL,threadMM)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                         size_t M, size_t N, size_t K)
/*
 * RETURNS: number of threads matmul should use to paralellize the problem
 */
{
   size_t i, j, smp2, bip2, xo, xom, D;
   const int *xop;
   int k;
   if (M < 256 && N < 256 && K < 256)   /* small matrix */
   {
/*
 *    For really small problems, table lookups too expensive, so do a quick
 *    return
 */
      j = Mmax(M,N);
      i = Mmin(M,N);
      i = Mmin(i,K);
      if (j <= NB+NB || i < NB)
         return(1);    /* quick return */
/*
 *    Make choice based on most restricted dimension
 */
      if (M < N && M < K)   /* M most restricted dim */
         goto SMALLM;
      else if (K < M && K < N)  /* K most restricted dim */
         goto SMALLK;
      else if (M == N && M == K)
         goto SQUARE;
      else  /* N is most restricted dim */
         goto SMALLN;
   }
/*
 * The following three shapes model recursive factorizations where
 * two dimensions are cut during the recursion, and a third remains large
 */
   else if (N <= 256 && K <= 256)  /* recursive shape that doesn't cut M */
   {                               /* LU uses this shape */
      i = Mmin(N, K);
      j = Mmax(N, K);
      if (i >= NB)
         i = (i+j)>>1;
      else if (i >= 8)
         i = (i+i+i+j)>>2;  /* 3/4 MIN, 1/4 MAX */
      for (bip2=1; bip2 < i; bip2 <<= 1);
      smp2 = (bip2 == i) ? bip2 : (bip2>>1);
      i = (bip2-i < i-smp2 && i > 16) ? bip2 : smp2;
      for (j=0; j < 9; j++)
         if (i & (1<<j)) break;
      D = M;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SnkLm_XO : ATL_tmmNT_SnkLm_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SnkLm_XO : ATL_tmmTT_SnkLm_XO;
      #ifdef DEBUG
         printf("sNKlM_%c%c, M=%d, N=%d, K=%d rD=%d, D=%d\n",
                T2c(TA), T2c(TB), M, N, K, j, D);
      #endif
   }
   else if (M <= 256 && N <= 256)  /* recursive shape that doesn't cut K */
   {                               /* QR uses, maybe in LARFT? */
      i = Mmin(M, N);
      j = Mmax(M, N);
      if (i >= NB)
         i = (i+j)>>1;
      else if (i >= 8)
         i = (i+i+i+j)>>2;  /* 3/4 MIN, 1/4 MAX */
      for (bip2=1; bip2 < i; bip2 <<= 1);
      smp2 = (bip2 == i) ? bip2 : (bip2>>1);
      i = (bip2-i < i-smp2 && i > 16) ? bip2 : smp2;
      for (j=0; j < 9; j++)
         if (i & (1<<j)) break;
      D = K;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SmnLk_XO : ATL_tmmNT_SmnLk_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SmnLk_XO : ATL_tmmTT_SmnLk_XO;
      #ifdef DEBUG
         printf("sMNlK_%c%c, M=%d, N=%d, K=%d, rD=%d, D=%d\n",
                T2c(TA), T2c(TB), M, N, K, j, D);
      #endif
   }
   else if (M <= 256 && K <= 256) /* recursive shape that doesn't cut N */
   {                              /* UNCONFIRMED: QR variant uses */
      i = Mmin(M, K);
      j = Mmax(M, K);
      if (i >= NB)
         i = (i+j)>>1;
      else if (i >= 8)
         i = (i+i+i+j)>>2;  /* 3/4 MIN, 1/4 MAX */
      for (bip2=1; bip2 < i; bip2 <<= 1);
      smp2 = (bip2 == i) ? bip2 : (bip2>>1);
      for (j=0; j < 9; j++)
         if (i & (1<<j)) break;
      D = N;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SmkLn_XO : ATL_tmmNT_SmkLn_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SmkLn_XO : ATL_tmmTT_SmkLn_XO;
      #ifdef DEBUG
         printf("sNlMK_%c%c, M=%d, N=%d, K=%d, rD=%d, D=%d\n",
                T2c(TA), T2c(TB), M, N, K, j, D);
      #endif
   }
/*
 * The three following shapes model static blocking, where two dimensions
 * are full, and the third is blocked
 */
   else if (K <= 256)           /* K dim small, as in right-looking LU/QR */
   {
SMALLK:
      D = Mmin(M,N);
      if (D >= NB+NB)
         D = (M+N)>>1;
      i = K;
      for (bip2=1; bip2 < i; bip2 <<= 1);
      smp2 = (bip2 == i) ? bip2 : (bip2>>1);
      i = (bip2-i < i-smp2 && i > 16) ? bip2 : smp2;
      for (j=0; j < 9; j++)
         if (i & (1<<j)) break;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SkLmn_XO : ATL_tmmNT_SkLmn_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SkLmn_XO : ATL_tmmTT_SkLmn_XO;
      #ifdef DEBUG
         printf("sKlMN_%c%c, M=%d, N=%d, K=%d, rD=%d, D=%d\n",
                 T2c(TA), T2c(TB), M, N, K, j, D);
      #endif
   }
   else if (M <= 256)          /* M dim small */
   {
SMALLM:
      D = Mmin(N,K);
      if (D >= NB+NB)
         D = (N+K)>>1;
      i = M;
      for (bip2=1; bip2 < i; bip2 <<= 1);
      smp2 = (bip2 == i) ? bip2 : (bip2>>1);
      i = (bip2-i < i-smp2 && i > 16) ? bip2 : smp2;
      for (j=0; j < 9; j++)
         if (i & (1<<j)) break;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SmLnk_XO : ATL_tmmNT_SmLnk_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SmLnk_XO : ATL_tmmTT_SmLnk_XO;
      #ifdef DEBUG
         printf("sMlNK_%c%c, M=%d, N=%d, K=%d, rD=%d, D=%d\n",
                T2c(TA), T2c(TB), M, N, K, j, D);
      #endif
   }
   else if (N <= 256)          /* N dim small */
   {                           /* QR uses this */
SMALLN:
      D = Mmin(M,K);
      if (D >= NB+NB)
         D = (M+K)>>1;
      i = N;
      for (bip2=1; bip2 < i; bip2 <<= 1);
      smp2 = (bip2 == i) ? bip2 : (bip2>>1);
      i = (bip2-i < i-smp2 && i > 16) ? bip2 : smp2;
      for (j=0; j < 9; j++)
         if (i & (1<<j)) break;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SnLmk_XO : ATL_tmmNT_SnLmk_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SnLmk_XO : ATL_tmmTT_SnLmk_XO;
      #ifdef DEBUG
         printf("sNlMK_%c%c, M=%d, N=%d, K=%d, rD=%d, D=%d\n",
                T2c(TA), T2c(TB), M, N, K, j, D);
      #endif
   }
   else                        /* all dim > 256, call it square */
   {
SQUARE:   /* near-square shape, N <= 256 if jumped here */
      D = (M+N+K+1)/3;
      j = 0;
      if (TA == AtlasNoTrans)
         xop = (TB == AtlasNoTrans) ? ATL_tmmNN_SQmnk_XO : ATL_tmmNT_SQmnk_XO;
      else
         xop = (TB == AtlasNoTrans) ? ATL_tmmTN_SQmnk_XO : ATL_tmmTT_SQmnk_XO;
      #ifdef DEBUG
         printf("SQ_%c%c, M=%d, N=%d, K=%d, D=%d\n",
                T2c(TA), T2c(TB), M, N, K, D);
      #endif
   }

   xop += j*ATL_PDIM;
   for (k=ATL_PDIM-1; k >= 0; k--)
      if (xop[k] && D >= xop[k])
         return((k == ATL_PDIM-1) ? ATL_NTHREADS : (2<<k));
   return(1);
}
#endif
