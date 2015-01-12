/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 R. Clint Whaley
 *
 * Code contributers : R. Clint Whaley, Chad Zalkin
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
#define DEBUG
#ifdef DEBUG
   #define VERIFY 1
#endif
#include "atlas_mmtesttime.h"

static int Mylcm(const int M, const int N)
/*
 * Returns least common multiple (LCM) of two positive integers M & N by
 * computing greatest common divisor (GCD) and using the property that
 * M*N = GCD*LCM.
 */
{
   register int tmp, max, min, gcd=0;

   if (M != N)
   {
      if (M > N) { max = M; min = N; }
      else { max = N; min = M; }
      if (min > 0)  /* undefined for negative numbers */
      {
         do  /* while (min) */
         {
            if ( !(min & 1) ) /* min is even */
            {
               if ( !(max & 1) ) /* max is also even */
               {
                  do
                  {
                     min >>= 1;
                     max >>= 1;
                     gcd++;
                     if (min & 1) goto MinIsOdd;
                  }
                  while ( !(max & 1) );
               }
               do min >>=1 ; while ( !(min & 1) );
            }
/*
 *          Once min is odd, halve max until it too is odd.  Then, use
 *          property that gcd(max, min) = gcd(max, (max-min)/2)
 *          for odd max & min
 */
MinIsOdd:
            if (min != 1)
            {
               do  /* while (max >= min */
               {
                  max -= (max & 1) ? min : 0;
                  max >>= 1;
               }
               while (max >= min);
            }
            else return( (M*N) / (1<<gcd) );
            tmp = max;
            max = min;
            min = tmp;
         }
         while(tmp);
      }
      return( (M*N) / (max<<gcd) );
   }
   else return(M);
}

void FillInSSEGenString
(
   ATL_mmnode_t *mmp,           /* ptr to MMNODE to generate */
   char pre,                    /* precision/type prefix */
   int M, int N, int K,         /* problem dimensions */
   int ldc                      /* ldc to use */
)
/*
 * RETURNS: string holding the make call that will generate the kernel
 * specified by mmp
 */
{
   char ln[2048];
   int i;

   i = sprintf(ln, "make %cgensseK mu=%d nu=%d ku=%d",
               pre, mmp->mu, mmp->nu, mmp->ku);
   i += sprintf(ln+i, " MB=%d NB=%d, KB=%d",
                FLAG_IS_SET(mmp->flag, MMF_MRUNTIME) ? 0 : M,
                FLAG_IS_SET(mmp->flag, MMF_NRUNTIME) ? 0 : N,
                FLAG_IS_SET(mmp->flag, MMF_KRUNTIME) ? 0 : K);
   i += sprintf(ln+i, " %cssegenf=%s", pre, mmp->rout);
   if (FLAG_IS_SET(mmp->flag, MMF_LDISKB))
      i += sprintf(ln+i, " lda=%d ldb=%d", K, K);
/*
 * Need to modify generator so some flag (maybe lda=-2?) means that
 * lda/ldb are runtime constants, but lda == ldb --> saves registers
 */
   else if (FLAG_IS_SET(mmp->flag, MMF_LDAB))
      i += sprintf(ln+i, " lda=0 ldb=0");
   else
      i += sprintf(ln+i, " lda=0 ldb=0");
   i += sprintf(ln+i, " ldc=0");
   i += sprintf(ln+i, " ssegenflags=\"");
   if (FLAG_IS_SET(mmp->flag, MMF_LDFLOAT))
      i += sprintf(ln+i, "-treatLoadsAsFloat 1");
   if (FLAG_IS_SET(mmp->flag, MMF_STFLOAT))
      i += sprintf(ln+i, " -treatStoresAsFloat 1");
   if (FLAG_IS_SET(mmp->flag, MMF_PFACOLS))
      i += sprintf(ln+i, " -prefetchACols 1");
   if (FLAG_IS_SET(mmp->flag, MMF_PFABLK))
      i += sprintf(ln+i, " -prefetchABlock 1");
   if (FLAG_IS_SET(mmp->flag, MMF_PFBCOLS))
      i += sprintf(ln+i, " -prefetchBCols 1");
/*
 * You need an option that prefetches C at top of loop
 */
   if (FLAG_IS_SET(mmp->flag, MMF_PFCELTS))
      i += sprintf(ln+i, " -prefetchCelts 1");
   i += sprintf(ln+i, "\" > /dev/null 2>&1\n");
//   fprintf(stderr, "\n\nGENSTRING = %s\n", ln);
   if (mmp->genstr)
      free(mmp->genstr);
   mmp->genstr = DupString(ln);
}

static int FORCEFLAGS=0;   /* hack to get around bad comp/flags */
void FillInMMNode(ATL_mmnode_t *mmp, int ID, char pre, int M, int N, int K,
                  int mu, int nu, int ku, int lda, int ldb, int ldc,
                  int LDFLOAT, int STFLOAT,
                  int PFAcols, int PFAblk, int PFBcols, int PFc)
/*
 * Take mmgen_sse's flags and produce fill in the standard ATL_mmnode_t
 * structure pointed to by mmp.
 */
{
   char ln[256];
   int i;

   if (mmp->auth)
      free(mmp->auth);
   if (mmp->rout)
      free(mmp->rout);
   if (mmp->comp)
      free(mmp->comp);
   if (mmp->cflags)
      free(mmp->cflags);
   if (mmp->str)
      free(mmp->str);
   if (mmp->genstr)
      free(mmp->genstr);
   mmp->auth = mmp->rout = mmp->comp = mmp->cflags = mmp->str =
               mmp->genstr = NULL;
   mmp->mu = mu;
   mmp->nu = nu;
   mmp->ku = ku;
   mmp->ID = ID;
   if (K > 0)
   {
      mmp->kbmin = 0;
      mmp->kbmax = 2000;
   }
   mmp->SSE = 3;
   mmp->lat = 1;
   mmp->muladd = 0;
   mmp->fftch = mmp->iftch = mmp->nftch = mmp->clean = 0;
   mmp->TA = AtlasTrans;
   mmp->TB = AtlasNoTrans;
   mmp->asmbits = 0;
   mmp->auth = DupString("Zalkin & Whaley");
   sprintf(ln, "%cgenmm_sse.c", pre);
   mmp->rout = DupString(ln);
   if (FORCEFLAGS)
   {
      mmp->comp = DupString("gcc");
      #ifdef ATL_USE64BITS
         mmp->cflags = DupString(
            "-fomit-frame-pointer -mfpmath=sse -msse3 -O2 -m64");
      #else
         mmp->cflags = DupString(
            "-fomit-frame-pointer -mfpmath=sse -msse3 -O2 -m32");
      #endif
   }
   i = 0;
   SET_FLAG(i, MMF_MRUNTIME, (M == 0));
   SET_FLAG(i, MMF_NRUNTIME, (N == 0));
   SET_FLAG(i, MMF_KRUNTIME, (K == 0));
   if (K > 0)
   {
      SET_FLAG(i, MMF_KUISKB, (K == ku));
      SET_FLAG(i, MMF_LDISKB, (K == lda && K == ldb));
   }
   SET_FLAG(i, MMF_LDAB, (lda == ldb));
   SET_FLAG(i, MMF_PFACOLS, PFAcols);
   SET_FLAG(i, MMF_PFABLK, PFAblk);
   SET_FLAG(i, MMF_PFBCOLS, PFBcols);
   SET_FLAG(i, MMF_PFCELTS, PFc);
   mmp->flag = i;
}

#ifdef ATL_GAS_x8632
   #define MAXREGS 10
#else
   #define MAXREGS 16
#endif
#ifndef MAXNB
   #define MAXNB 80
#endif
static double TimeIt(ATL_mmnode_t *mmp, int verb, int ID, char pre,
                     int M, int N, int K, int mu, int nu, int ku,
                     int lda, int ldb, int ldc, int LDFLOAT, int STFLOAT,
                     int PFAcols, int PFAblk, int PFBcols, int PFc)
{
   int ierr=0;

   FillInMMNode(mmp, ID, pre, M, N, K, mu, nu, ku, lda, ldb, ldc,
                LDFLOAT, STFLOAT, PFAcols, PFAblk, PFBcols, PFc);
   FillInSSEGenString(mmp, pre, M, N, K, ldc ? ldc : M+8);
   #ifdef VERIFY
      if (MMKernelFailsTest(pre, M, N, K, 0, mmp))
         ierr = 1;
      else if (MMKernelFailsTest(pre, M, N, K, 1, mmp))
         ierr = 2;
      else if (MMKernelFailsTest(pre, M, N, K, 2, mmp))
         ierr = 3;
      if (ierr)
      {
         fprintf(stderr, "FAILED:  genstr='%s'\n", mmp->genstr);
//         return(0.0);  /* temp workaround until Chad fixes */
         assert(0);
      }
   #endif
   return(TimeMMKernel(verb, 0, mmp, pre, M, N, K, lda, ldb, ldc ? ldc : M+8,
                       1, -1, -1));
}

int FindWorkingFlags(char pre, int nb, int verb)
/*
 * If we can't compile & pass with default compiler and flags, we'll need
 * to manually set the compiler to gcc and the compiler to use sse!
 * RETURNS: 0 if generator will not work, 1 if we have found flags that do
 */
{
   ATL_mmnode_t *mmp;
   int ID=0;

   if (!nb) nb = 24;
   mmp = GetMMNode();
   FORCEFLAGS = 0;
   FillInMMNode(mmp, ID, pre, nb, nb, nb, 1, 1, 1, nb, nb, nb, 0, 0, 0, 0,
                0, 0);
   FillInSSEGenString(mmp, pre, nb, nb, nb, nb);
   if (MMKernelFailsTest(pre, nb, nb, nb, 0, mmp))
   {
      if (verb)
         printf("Default compiler & flags do not support SSEGEN!!\n\n");
      FORCEFLAGS = 1;
      FillInMMNode(mmp, ID, pre, nb, nb, nb, 1, 1, 1, nb, nb, nb, 0, 0, 0, 0,
                   0, 0);
      FillInSSEGenString(mmp, pre, nb, nb, nb, nb);
      return(!MMKernelFailsTest(pre, nb, nb, nb, 0, mmp));
   }
   if (verb)
      printf("Default compiler & flags work fine\n\n");
   return(1);
}

ATL_mmnode_t *findMUNU
(
   char pre,            /* precision/type specifier, one of [s,d,c,z] */
   int verb,            /* verbosity level */
   int nb3,             /* fitting 3 blocks in cache */
   int nb1,             /* fitting 1 block in cache */
   int *ID              /* on entry, ID to use, on exit, next ID to use */
)
/*
 * Given a guestimate of NB, find the best MU & NU to use
 */
{
   ATL_mmnode_t *mmp;
   double mf, best_mf=0.0, mfH;
   int mu, nu, nb, nbL, nbH, id=(*ID);
   int best_nb=nb1, best_mu=4, best_nu=2, best_id;
   const int vector_stride = (pre == 's' || pre == 'c') ? 4 : 2;

   fprintf(stdout, "\nFINDING SSEGEN REGISTER BLOCKING :\n");
   mmp = GetMMNode();
   for (nu=1; nu < MAXREGS; nu++)
   {
      for (mu=1; mu < MAXREGS; mu++)
      {
         if (1+nu+mu*nu > MAXREGS) continue;
         nb = Mylcm(mu, nu);
         nb = Mylcm(nb, vector_stride);
         nbL = (nb3/nb)*nb;
         nbH = (nb1/nb)*nb;
         assert(nbH > 0);
         if (nbL > 0)
         {
            nb = nbL;
            mf = TimeIt(mmp, verb, id, pre, nb, nb, nb, mu, nu, nb, nb, nb,
                        0, 0, 0, 0, 0, 0, 0);
            if (verb)
               fprintf(stdout, "   nb=%d, mu=%d, nu=%d, mf=%.2f\n",
                       nb, mu, nu, mf);
            if (mf > best_mf)
            {
               best_mu = mu;
               best_nu = nu;
               best_id = id;
               best_nb = nb;
               best_mf = mf;
            }
         }
         if (nbH != nbL)
         {
            mf = TimeIt(mmp, verb, id, pre, nbH, nbH, nbH, mu, nu, nbH,
                        nbH, nbH, 0, 0, 0, 0, 0, 0, 0);
            if (verb)
               fprintf(stdout, "   nb=%d, mu=%d, nu=%d, mf=%.2f\n",
                       nbH, mu, nu, mf);
            if (mf > best_mf)
            {
               best_mu = mu;
               best_nu = nu;
               best_id = id;
               best_nb = nbH;
               best_mf = mf;
            }
         }
      }
   }
   nb = best_nb;
   FillInMMNode(mmp, best_id, pre, nb, nb, nb, best_mu, best_nu, nb, nb, nb,
                0, 0, 0, 0, 0, 0, 0);
   mmp->mflop[0] = best_mf;
   fprintf(stdout, "\nBEST SSEGEN REGISTER BLOCK for NB=%d is %dx%d (%.2f)\n\n",
           best_nb, best_mu, best_nu, best_mf);
   *ID = id+1;
   return(mmp);
}

ATL_mmnode_t *findPF
/*
 * RETURNS: mmp modified with new best prefetch parameters
 */
(
   ATL_mmnode_t *mmp,   /* good MM kern; mu/nu must be set */
   char pre,            /* precision/type specifier, one of [s,d,c,z] */
   int verb,            /* verbosity level */
   int nb3,             /* fitting 3 blocks in cache */
   int nb1,             /* fitting 1 block in cache */
   int *ID              /* on entry, ID to use, on exit, next ID to use */
)
{
   int lcm, id=(*ID), mu, nu, i, j, k, h;
   int flagB, idB, nbB;
   const int veclen = (pre == 'c' || pre == 's') ? 4 : 2;
   double mf, mfB;

   mu = mmp->mu;
   nu = mmp->nu;
   lcm = Mylcm(mu, nu);
   lcm = Mylcm(lcm, veclen);
   nb3 = (nb3/lcm)*lcm;
   nb1 = (nb1/lcm)*lcm;

   if (verb > 0)
      printf("\nSEARCHING FOR BEST PREFETCH SETTING:\n");
   mfB = 0.0;
   flagB = mmp->flag;
   nbB = nb3;
   for (i=0; i < 2; i++)                /* loop over prefetchABlock */
   {
      for (j=0; j < 2; j++)             /* loop over prefetchAcols */
      {
         for (k=0; k < 2; k++)          /* loop over prefetchBcols */
         {
            for (h=0; h < 2; h++)       /* loop over prefetchC */
            {
               mf = TimeIt(mmp, verb, id++, pre, nb1, nb1, nb1, mu, nu, nb1,
                           nb1, nb1, 0, 0, 0, j, i, k, h);
               if (verb > 0)
                  printf("   nb=%d, pfAblk=%d, pfACols=%d, pfBCols=%d, pfC=%d, mf=%.2f\n",
                         nb1, i, j, k, h, mf);
               if (mf > mfB)
               {
                  flagB = mmp->flag;
                  nbB = nb1;
                  idB = id-1;
                  mfB = mf;
               }
               mf = TimeIt(mmp, verb, id++, pre, nb3, nb3, nb3, mu, nu, nb3,
                           nb3, nb3, 0, 0, 0, j, i, k, h);
               if (verb > 0)
                  printf("   nb=%d, pfAblk=%d, pfACols=%d, pfBCols=%d, pfC=%d, mf=%.2f\n",
                         nb3, i, j, k, h, mf);
               if (mf > mfB)
               {
                  flagB = mmp->flag;
                  nbB = nb3;
                  idB = id-1;
                  mfB = mf;
               }
            }
         }
      }
   }
   printf(
"BEST PRFTCH VALS: nb=%d, pfAblk=%d, pfACols=%d, pfBCols=%d, pfC=%d (%.2f)\n\n",
          nbB, FLAG_IS_SET(flagB, MMF_PFABLK), FLAG_IS_SET(flagB, MMF_PFACOLS),
          FLAG_IS_SET(flagB, MMF_PFBCOLS), FLAG_IS_SET(flagB, MMF_PFCELTS),
          mfB);
   *ID = id;
   mmp->mflop[0] = mfB;
   mmp->flag = flagB;
   mmp->mbB = mmp->nbB = mmp->kbB = nbB;
   mmp->ID = idB;
   return(mmp);
}

ATL_mmnode_t *findNB
/*
 * RETURNS: mmp modified with new NB
 */
(
   ATL_mmnode_t *mmp,   /* good MM kern; mu/nu/prefetch must be set */
   char pre,            /* precision/type specifier, one of [s,d,c,z] */
   int verb,            /* verbosity level */
   int *ID              /* on entry, ID to use, on exit, next ID to use */
)
{
   double mf, mfB;
   int mu, nu, nb, nbB, nbinc;

   printf("\nFINDING BEST NB:\n");
   mfB = mmp->mflop[0];
   nbB = mmp->nbB;
   mu = mmp->mu;
   nu = mmp->nu;
   nbinc = Mylcm(mu, nu);
   nbinc = Mylcm(nbinc, (pre == 'c' || pre == 's') ? 4 : 2);
   for (nb= (nbinc <= 8) ? (16/nbinc)*nbinc : nbinc; nb <= MAXNB; nb += nbinc)
   {
      mf = TimeIt(mmp, verb, *ID, pre, nb, nb, nb, mu, nu, nb,
                  nb, nb, 0, 0, 0, FLAG_IS_SET(mmp->flag, MMF_PFACOLS),
                  FLAG_IS_SET(mmp->flag, MMF_PFABLK),
                  FLAG_IS_SET(mmp->flag, MMF_PFBCOLS),
                  FLAG_IS_SET(mmp->flag, MMF_PFCELTS));
      if (verb > 0)
          printf("   nb=%d (%.2f)\n", nb, mf);
      if (mf > mfB)
      {
         mfB = mf;
         nbB = nb;
      }
   }
   printf("BEST NB=%d (%.2f)\n\n", nbB, mfB);
   mmp->mflop[0] = mfB;
   mmp->nbB = mmp->mbB = mmp->kbB = nbB;
   (*ID)++;
   return(mmp);
}

ATL_mmnode_t *findKU
/*
 * RETURNS: mmp modified with new best prefetch parameters
 */
(
   ATL_mmnode_t *mmp,   /* good MM kern; mu/nu/prefetch/nb must be set */
   char pre,            /* precision/type specifier, one of [s,d,c,z] */
   int verb,            /* verbosity level */
   int *ID              /* on entry, ID to use, on exit, next ID to use */
)
{
   const int nb = mmp->nbB, mu = mmp->mu, nu = mmp->nu;
   const int PFACOLS=FLAG_IS_SET(mmp->flag, MMF_PFACOLS);
   const int PFABLK =FLAG_IS_SET(mmp->flag, MMF_PFABLK);
   const int PFBCOLS=FLAG_IS_SET(mmp->flag, MMF_PFBCOLS);
   const int PFCELTS=FLAG_IS_SET(mmp->flag, MMF_PFCELTS);
   const int veclen = (pre == 'c' || pre == 's') ? 4 : 2;
   double mf, mfB;
   int kuB, k;

   printf("\nFINDING KU\n");
   mfB = mmp->mflop[0];
   kuB = nb;
   mf = TimeIt(mmp, verb, *ID, pre, nb, nb, nb, mu, nu, 1, nb, nb, 0, 0, 0,
              PFACOLS, PFABLK, PFBCOLS, PFCELTS);
   if (verb > 0)
      printf("   ku=%d (%.2f)\n", 1, mf);
   if (mf > mfB)
   {
      mfB = mf;
      kuB = 1;
   }
   for (k=veclen; k <= nb/2; k += veclen)
   {
      if ((nb/k)*k != nb)
         continue;
      mf = TimeIt(mmp, verb, *ID, pre, nb, nb, nb, mu, nu, k, nb, nb, 0, 0, 0,
                 PFACOLS, PFABLK, PFBCOLS, PFCELTS);
      if (verb > 0)
         printf("   ku=%d (%.2f)\n", k, mf);
      if (mf > mfB)
      {
         mfB = mf;
         kuB = k;
      }
   }
   printf("BEST KU=%d (%.2f)\n", kuB, mfB);
   mmp->ku = kuB;
   mmp->mflop[0] = mfB;
   SET_FLAG(mmp->flag, MMF_KUISKB, (mmp->ku == nb));
   return(mmp);
}

ATL_mmnode_t *findLDST
/*
 * RETURNS: mmp modified with new best prefetch parameters
 */
(
   ATL_mmnode_t *mmp,   /* good MM kern; mu/nu/prefetch/nb/ku must be set */
   char pre,            /* precision/type specifier, one of [s,d,c,z] */
   int verb,            /* verbosity level */
   int *ID              /* on entry, ID to use, on exit, next ID to use */
)
{
   const int nb = mmp->nbB, mu = mmp->mu, nu = mmp->nu, ku = mmp->ku;
   const int PFACOLS=FLAG_IS_SET(mmp->flag, MMF_PFACOLS);
   const int PFABLK =FLAG_IS_SET(mmp->flag, MMF_PFABLK);
   const int PFBCOLS=FLAG_IS_SET(mmp->flag, MMF_PFBCOLS);
   const int PFCELTS=FLAG_IS_SET(mmp->flag, MMF_PFCELTS);
   int LDFLOAT = 0, STFLOAT = 0, id=(*ID), i, j;
   const int veclen = (pre == 'c' || pre == 's') ? 4 : 2;
   double mf, mfB;

   if (pre == 's' || pre == 'c')
      return(mmp);
   printf("\nFINDING IF WE WANT TO LD/ST DOUBLES AS FLOATS:\n");
   mfB = mmp->mflop[0];
   for (i=0; i < 2; i++)        /* do loads as floats? */
   {
      for (j=0; j < 2; j++)     /* do stores as floats? */
      {
         mf = TimeIt(mmp, verb, id++, pre, nb, nb, nb, mu, nu, ku, nb, nb, 0,
                     i, j, PFACOLS, PFABLK, PFBCOLS, PFCELTS);
         if (verb > 0)
            printf("   ldf=%d, stf=%d (%.2f)\n", i, j, mf);
         if (mf > mfB)
         {
            LDFLOAT = i;
            STFLOAT = j;
         }
      }
   }
   printf("BEST CASE LDFLOAT=%d, STFLOAT=%d (%.2f)\n", LDFLOAT, STFLOAT, mfB);
   SET_FLAG(mmp->flag, MMF_LDFLOAT, LDFLOAT);
   SET_FLAG(mmp->flag, MMF_STFLOAT, STFLOAT);
   mmp->mflop[0] = mfB;
   *ID = id;
   return(mmp);
}

void GenFiles(char pre, ATL_mmnode_t *mmb)
/*
 * Generates kernel(s) into SSEGENOUTDIR for later use
 */
{
   ATL_mmnode_t *mmp;
   char ln[512];

   for (mmp=mmb; mmp; mmp = mmp->next)
   {
/*
 *    Add SSEGENOUTDIR to name if it is not already there
 */
      if (mmp->rout && strncmp("SSEGENOUTDIR/", mmp->rout, 13))
      {
         sprintf(ln, "SSEGENOUTDIR/%s", mmp->rout);
         free(mmp->rout);
         mmp->rout = DupString(ln);
      }
      FillInSSEGenString(mmp, pre, mmp->mbB, mmp->nbB, mmp->kbB, 0);
      if (system(mmp->genstr))
      {
         fprintf(stderr, "FAILED: '%s'\n", mmp->genstr);
         assert(0);
      }
   }
}
ATL_mmnode_t *mmksearch_sse
(
   char pre,     /* precision/type specifier, one of [s,d,c,z] */
   int verb      /* verbosity level */
)
/*
 * This search exercises the SSE matmul kernel generator mmgen_sse.c
 * RETURNS: best found case
 */
{

   int L1Elts;
   int nb1;             /* maxNB to fit one block in cache */
   int nb3;             /* maxNB to fit all three blocks in cache */
   int ID=10000;
   ATL_mmnode_t *mmp, *mm1;
   const int veclen = (pre == 's' || pre == 'c') ? 4 : 2;
   char upr;
   char ln[64];

   #ifndef ATL_SSE3  /* generator needs at least SSE3 to work */
      return(NULL);
   #endif

/*
 * If output file already exists, just retime kernels if necessary
 */
   sprintf(ln, "res/%cMMKSSE.sum", pre);
   mmp = ReadMMFile(ln);
   if (mmp)
   {
      GenFiles(pre, mmp);
      if (mmp->mflop[0] <= 0.0)
         TimeAllMMKernels(0, 1, 0, mmp, pre, 0, 0, 0, 1, 0, 0);
      return(mmp);
   }
   if (pre == 'z')
      upr = 'd';
   else if (pre == 'c')
      upr = 's';
   else
      upr = pre;
   L1Elts = GetL1CacheSize() * 1024 / pre2size(upr);
   for (nb3=16; 3*nb3*nb3 < L1Elts; nb3 += veclen);
   nb3 -= veclen;
   if (nb3 > MAXNB)
     nb3 = MAXNB;
   for (nb1=nb3; nb1*nb1 < L1Elts; nb1 += veclen);
   nb1 -= veclen;  /* don't completely fill cache */
   if (nb1 < 16)
      nb1 = nb3;
   if (nb1 > MAXNB)
     nb1 = MAXNB;

   mmp = findMUNU(pre, verb, nb3, nb1, &ID);
   mmp = findPF(mmp, pre, verb, nb3, nb1, &ID);
   mmp = findNB(mmp, pre, verb, &ID);
   mmp = findKU(mmp, pre, verb, &ID);
   mmp = findLDST(mmp, pre, verb, &ID);
/*
 * Make sure selected kernel gets the right answer
 */
   if (MMKernelFailsTest(pre, mmp->nbB, mmp->nbB, mmp->nbB, 0, mmp) ||
       MMKernelFailsTest(pre, mmp->nbB, mmp->nbB, mmp->nbB, 1, mmp) ||
       MMKernelFailsTest(pre, mmp->nbB, mmp->nbB, mmp->nbB, 2, mmp))
   {
      fprintf(stderr, "SSEGEN PRODUCED INVALID KERNEL, pre=%c:\n", pre);
      PrintMMLine(stderr, mmp);
      fprintf(stderr, "genstr='%s'\n", mmp->genstr);
      KillMMNode(mmp);
      return(NULL);
   }
   printf("\n\nSSEGEN SEARCH SUCCESSFULLY FINISHED, BEST CASE IS:\n");
   PrintMMNodes(stdout, mmp);
   return(mmp);
}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);
   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -p [s,d,c,z]: set precision prefix \n");
   fprintf(stderr, "   -b <nb> : starting blocking factor\n");
   fprintf(stderr, "   -v # : higher numbers print out more\n");
   exit(ierr ? ierr : -1);
}

int GetFlags(int nargs, char **args, char *pre, int *verb)
{
   int i;
   int nb = 48;
   char ch;

   *pre = 'd';
   *verb = 1;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'p':  /* -p <pre> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);

         ch = tolower(args[i][0]);
         assert(ch == 's' || ch == 'd' || ch == 'c' || ch == 'z');
         *pre = ch;
         break;
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *verb = atoi(args[i]);
         break;
      case 'b':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         nb = atoi(args[i]);
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   return(nb);
}

int main(int nargs, char **args)
{
   ATL_mmnode_t *mmp;
   char ln[64];
   int nb, verb;
   char pre = 'd';

   nb = GetFlags(nargs, args, &pre, &verb);

   nb = FindWorkingFlags(pre, nb, verb);
   mmp = mmksearch_sse(pre, verb);   /* find best kernel */
   if (!mmp)  /* quit if this arch doesn't support SSE3 */
      exit(0);
   GenFiles(pre, mmp);  /* generate kernel ino SSEGENOUTDIR for later use */
/*
 * Create final summary file describing found kernel
 */
   sprintf(ln, "res/%cMMKSSE.sum", pre);
   WriteMMFile(ln, mmp);
   KillMMNode(mmp);
   exit(0);
}
