/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "atlas_misc.h"
#include "atlas_mmtesttime.h"

#define MAXLAT 6
int GetGoodLat(int MULADD, int kb, int mu, int nu, int ku, int lat)
{
   int slat, blat, i, ii = mu*nu*ku;
   if (MULADD) return(lat);
   if ( (lat > 1) && (kb > ku) && ((ii/lat)*lat != ii) )  /* lat won't work */
   {
      for (i=lat; i; i--) if ( (ii/i) * i == ii ) break;
      slat = i;
      for (i=lat; i < MAXLAT; i++) if ( (ii/i) * i == ii ) break;
      blat = i;
      if ( (ii/blat)*blat != ii ) blat = slat;
      if (slat < 2) lat = blat;
      else if (lat-slat < blat-lat) lat = slat;
      else lat = blat;
   }
   return(lat);
}

int GetUniqueMuNus(int nregs, int muladd, int lat, int *mus, int *nus)
/*
 * RETURNS: number of unique MU,NU combos; always allow at least 1x1
 */
{
   int i, j, k, n=0;

   for (j=1; j <= nregs; j++)
   {
      for (i=1; i <= nregs; i++)
      {
         k = (muladd) ? 0 : lat;
         if ((i != 1 || j != 1) && i*j+i+1+k > nregs) continue;
         if (mus)
         {
            mus[n] = i;
            nus[n] = j;
         }
         n++;
      }
   }
   return(n);
}

#ifdef DEBUG
void PrintMUNUs(int N, int *mus, int *nus, double *fpls)
{
   int i;
   for (i=0; i < N; i++)
   {
      if (fpls)
         printf("%3d. MU=%d, NU=%d, fpl=%.3f\n", i, mus[i], nus[i], fpls[i]);
      else
         printf("%3d. MU=%d, NU=%d\n", i, mus[i], nus[i]);
   }
}
#endif

void SortByFlpLd(int N, int *mus, int *nus, double *FPL)
/*
 * Simple selection sort, sorting from best (greatest) flops/load to worst
 * ties in mflop are broken by taking the most square one, and if they
 * are equally square, then take the one with the bigger mu.
 */
{
   int i, j, imax, mindim, mindimB;
   double fpl, fplB;

   #ifdef DEBUG
      printf("\nUNSORTED:\n");
      PrintMUNUs(N, mus, nus, NULL);
   #endif
   for (i=0; i < N-1; i++)
   {
      imax = i;
      mindimB = (mus[i] <= nus[i]) ? mus[i] : nus[i];
      fplB = (2.0 * mus[i] * nus[i]) / (mus[i] + nus[i]);
      for (j=i+1; j < N; j++)
      {
          fpl = (2.0 * mus[j] * nus[j]) / (mus[j] + nus[j]);
          if (fpl > fplB)
          {
             imax = j;
             fplB = fpl;
             mindimB = (mus[j] <= nus[j]) ? mus[j] : nus[j];
          }
          else if (fpl == fplB)
          {
             mindim = (mus[j] <= nus[j]) ? mus[j] : nus[j];
             if (mindim > mindimB)
             {
                imax = j;
                mindimB = mindim;
             }
/*
 *           For symmetric shapes, choose the one with a bigger mu
 */
             else if (mindim == mindimB)
             {
                if (mus[j] > mindim)
                   imax = j;
             }
          }
      }
      if (imax != i)
      {
          j = mus[i];
          mus[i] = mus[imax];
          mus[imax] = j;
          j = nus[i];
          nus[i] = nus[imax];
          nus[imax] = j;
      }
      if (FPL)
         FPL[i] = fplB;
   }
   FPL[i] = (2.0 * mus[i] * nus[i]) / (mus[i] + nus[i]);
   #ifdef DEBUG
      printf("\n\nSORTED:\n");
      PrintMUNUs(N, mus, nus, FPL);
   #endif
}

#define LOWBOUND 0.6857
void GetMuNus(int nregs, int muladd, int lat, int *NGOOD, int *N0,
              int **mus, int **nus, double **fpls)
{
   int N, i;
   double fplB, *f;

   N = GetUniqueMuNus(nregs, muladd, lat, NULL, NULL);
   *mus = malloc(N*sizeof(int));
   *nus = malloc(N*sizeof(int));
   *fpls = malloc(N*sizeof(double));
   assert(*mus && *nus && *fpls);
   GetUniqueMuNus(nregs, muladd, lat, *mus, *nus);
   SortByFlpLd(N, *mus, *nus, *fpls);
   f = *fpls;
   fplB = LOWBOUND * f[0];
   for (i=1; i < N && f[i] >= fplB; i++);
   *NGOOD = i;
   *N0 = N;
}

int GetSafeGoodMuNu(int nreg, int muladd, int lat,
                    int N, int *mus, int *nus, double *fpls)
/*
 * Find the good value to compare agains the "bad" ones; should be safe on not
 * overflowing registers
 * NOTE : assumes mus/nus already sorted by flops/load
 */
{
   int k, i;
   k = (muladd) ? 0 : lat;
   for (i=0; i < N; i++)
      if (mus[i]*nus[i]+mus[i]+nus[i]+k+4 <= nreg)
         return(i);
   return(0);
}

void GetSafeMUNU(int nreg, int muladd, int lat, int *MU, int *NU)
{
   int N, Ng, i;
   int *mus, *nus;
   double *fpls;

   GetMuNus(nreg, muladd, lat, &Ng, &N, &mus, &nus, &fpls);
   i = GetSafeGoodMuNu(nreg, muladd, lat, N, mus, nus, fpls);
   *MU = mus[i];
   *NU = nus[i];
   free(mus);
   free(nus);
   free(fpls);
}
void GetMulAdd(char pre, int *MULADD, int *lat)
{
   char nam[64], ln[128];
   FILE *fp;

   sprintf(nam, "res/%cMULADD", pre);
   if (!FileExists(nam))
   {
      sprintf(ln, "make RunMulAdd pre=%c maxlat=%d mflop=%d\n", pre, 6, 200);
      assert(system(ln) == 0);
   }
   fp = fopen(nam, "r");
   assert(fp != NULL);
   fscanf(fp, "%d", MULADD);
   fscanf(fp, "%d", lat);
   fclose(fp);
}

void PrintUsage(char *name, int ierr, char *flag)
{
   fprintf(stderr,
           "%s searches for the best kernel that emit_mm.c can produce\n",
           name);
   fprintf(stderr, "For all gemm parameters (eg., nb) if they are not specified or\nspecified as 0, then the search determines them,\notherwise they are forced to the commandline specification.\n\n");

   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);
   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -v # : higher numbers print out more\n");
   fprintf(stderr, "   -p [s,d,c,z]: set precision prefix \n");
   fprintf(stderr, "   -b <nb> : blocking factor \n");
   fprintf(stderr, "   -r <nreg> : number of registers to assume\n");
   fprintf(stderr, "   -k <ku> : K unrolling factor \n");
   fprintf(stderr, "   -l <lat> : multiply latency to assume\n");
   fprintf(stderr, "   -M <muladd> : -1: search 0: separate mul&add : else MACC\n");
   fprintf(stderr, "   -o <outfile> : defaults to res/<pre>gMMRES.sum\n");
   exit(ierr ? ierr : -1);
}

char GetFlags(int nargs, char **args, int *verb, int *nregs, int *nb,
              int *ku, int *MACC, int *lat, char **outfile)
{
   char pre, ch;
   int i;

   *outfile = NULL;
   *verb = 1;
   *MACC = -1;
   *lat = *nregs = *nb = *ku = 0;
   pre = 'd';
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'o':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *outfile = DupString(args[i]);
         break;
      case 'p':  /* -p <pre> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);

         ch = tolower(args[i][0]);
         assert(ch == 's' || ch == 'd' || ch == 'c' || ch == 'z');
         pre = ch;
         break;
      case 'M':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *MACC = atoi(args[i]);
         break;
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *verb = atoi(args[i]);
         break;
      case 'b':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *nb = atoi(args[i]);
         break;
      case 'l':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *lat = atoi(args[i]);
         break;
      case 'r':
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *nregs = atoi(args[i]);
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   assert(*nb >= 0);
   if (*outfile == NULL)
   {
      *outfile = DupString("res/dgMMRES.sum");
      (*outfile)[4] = pre;
   }
   return(pre);
}

double TryKUs
(
   ATL_mmnode_t *mmp,
   char pre,                    /* precision */
   int verb,                    /* verbosity level */
   int MACC,                    /* 0 : separate mult&add, else MACC */
   int lat0,                    /* multiply latency */
   int beta,                    /* 0,1 beta, else beta=X */
   int nb,                      /* blocking factor */
   int mu, int nu, int ku,      /* unrolling factors */
   int fftch,                   /* do bogus fetch of C at top of loop? */
   int iftch,                   /* # of initial fetches to do */
   int nftch,                   /* # of fetches to do thereafter */
   int LDTOP,                   /* 1: load C at top, else at bottom */
   int pf                       /* prefetch strategy */
)
/*
 * If ku is set, times only that value, else tries both ku=1 & ku=nb
 * RETURNS: best performance of timed problems, with ku set correctly,
 *          but the generator flags may be bad!
 */
{
   double mf, mf1;
   int lat;

   assert(mu > 0 && nu > 0);
   if (ku)
   {
      mmp->ku = ku;
      lat = GetGoodLat(MACC, nb, mu, nu, ku, lat0);
      mf = TimeGMMKernel(verb, 0, pre, MACC, lat, beta, nb, mu, nu, ku,
                         fftch, iftch, nftch, LDTOP, pf, -1, -1);
   }
   else
   {
      lat = GetGoodLat(MACC, nb, mu, nu, 1, lat0);
      mf = TimeGMMKernel(verb, 0, pre, MACC, lat, beta, nb, mu, nu, nb,
                         fftch, iftch, nftch, LDTOP, pf, -1, -1);
      mf1 = TimeGMMKernel(verb, 0, pre, MACC, lat, beta, nb, mu, nu, 1,
                          fftch, iftch, nftch, LDTOP, pf, -1, -1);
      if (mf1 >= mf)
      {
         mmp->ku = 1;
         mf = mf1;
      }
   }
   return(mf);
}

double TryPFs
(
   ATL_mmnode_t *mmp,
   char pre,                    /* precision */
   int verb,                    /* verbosity level */
   int MACC,                    /* 0 : separate mult&add, else MACC */
   int lat,                     /* multiply latency */
   int beta,                    /* 0,1 beta, else beta=X */
   int nb,                      /* blocking factor */
   int mu, int nu, int ku,      /* unrolling factors */
   int fftch,                   /* do bogus fetch of C at top of loop? */
   int iftch,                   /* # of initial fetches to do */
   int nftch,                   /* # of fetches to do thereafter */
   int LDTOP                    /* 1: load C at top, else at bottom */
)
{
   double mf0, mf1;

   mf0 = TryKUs(mmp, pre, verb, MACC, lat, beta, nb, mu, nu, ku, fftch, iftch,
                nftch, LDTOP, 0);
   mf1 = TryKUs(mmp, pre, verb, MACC, lat, beta, nb, mu, nu, ku, fftch, iftch,
                nftch, LDTOP, 1);
   mmp->pref = (mf1 > mf0);
   return((mmp->pref) ? mf1 : mf0);
}

void ConfirmMACC(char pre, int verb, int nregs, int nb, int ku,
                 int *MACC, int *lat)
{
   char upr;
   int maccB, latB, latR, mu, nu;
   double mf, mfB;
   ATL_mmnode_t *mmp;

   mmp = GetMMNode();

   if (pre == 'd' || pre == 's')
      upr = pre;
   else if (pre == 'z')
      upr = 'd';
   else
      upr = 's';
   GetMulAdd(upr, MACC, lat);
   if (verb)
      printf("\nCONFIRMING MACC=%d AND LAT=%d WITH FORCED NREGS=%d\n",
             *MACC, *lat, nregs);
/*
 * Find performance of present MACC setting
 */
   maccB = *MACC;
   latB = *lat;
   GetSafeMUNU(nregs, maccB, latB, &mu, &nu);
   mfB = TryKUs(mmp, pre, verb, maccB, latB, 1, nb, mu, nu, ku, 0, mu+nu, 1,
                0, 0);
   if (verb)
      printf("   MACC=%1d, lat=%2d, mu=%2d, nu=%2d, mf=%.2f\n",
             maccB, latB, mu, nu, mfB);
/*
 * If no MACC, see if latency = 1 is just as good (dynamically scheduled mach)
 */
   if (!maccB)
   {
      GetSafeMUNU(nregs, 0, 1, &mu, &nu);
      mf = TryKUs(mmp, pre, verb, 0, 1, 1, nb, mu, nu, ku, 0, mu+nu, 1, 0, 0);
      if (verb)
         printf("   MACC=%1d, lat=%2d, mu=%2d, nu=%2d, mf=%.2f\n",
                0, 1, mu, nu, mf);
      if (mf > mfB)
      {
         mfB = mf;
         latB = 1;
      }
   }
/*
 * Find setting of reverse MACC setting, same latency
 */
   GetSafeMUNU(nregs, !maccB, *lat, &mu, &nu);
   mf = TryKUs(mmp, pre, verb, maccB, *lat, 1, nb, mu, nu, ku, 0, mu+nu, 1,
               0, 0);
   if (verb)
      printf("   MACC=%1d, lat=%2d, mu=%2d, nu=%2d, mf=%.2f\n",
             !maccB, *lat, mu, nu, mf);
   if (mf > mfB || (!maccB && mf == mfB))
   {
      maccB = !maccB;
      latB = *lat;
      mfB = mf;
   }
/*
 * Try to reverse MACC to 0, lat=1 (dynamically scheduled machines)
 */
   if (*MACC == 1)
   {
      GetSafeMUNU(nregs, 0, 1, &mu, &nu);
      mf = TryKUs(mmp, pre, verb, 0, 1, 1, nb, mu, nu, ku, 0, mu+nu, 1, 0, 0);
      if (verb)
         printf("   MACC=%1d, lat=%2d, mu=%2d, nu=%2d, mf=%.2f\n",
                0, 1, mu, nu, mf);
      if (mf > mfB)
      {
         mfB = mf;
         latB = 1;
         maccB = 0;
      }
   }
   if (verb)
      printf("CHOSE MACC=%d LAT=%d (%.2f)\n", maccB, latB, mfB);
   *MACC = maccB;
   *lat = latB;
}

double FindNumRegsByMACC(char pre, int verb, int nb, int ku, int MACC, int lat0,
                         int *NREGS, int *MU, int *NU)
{
   int i, mu, nu, muB, nuB, ForceMACC, lat;
   double mf, mfB;
   ATL_mmnode_t *mmp;

   mmp = GetMMNode();
   mfB = 0.0;
   for (i=8; i < 1024; i *= 2)
   {
      if (!MACC)
      {
         lat = i>>1;                       /* don't allow lat to take up */
         lat = (lat > lat0) ? lat0 : lat;  /* more than 1/2 the registers */
      }
      else
         lat = lat0;
      GetSafeMUNU(i, MACC, lat, &mu, &nu);
      mf = TryKUs(mmp, pre, verb, MACC, lat, 1, nb, mu, nu, ku, 0, mu+nu, 1,
                  0, 0);
      if (verb)
         printf(
 "   nreg=%3d: nb=%2d, mu=%2d, nu=%2d, ku=%2d, MACC=%1d, lat=%2d, mf=%.2f\n",
                i, nb, mu, nu, mmp->ku, MACC, lat, mf);
      if (mf > mfB)
      {
         mfB = mf;
         muB = mu;
         nuB = nu;
      }
/*
 *    Call a 8% decline in performance evidence of register overflow
 */

      else if (1.08*mf < mfB)
         break;
   }
   *NREGS = i>>1;
   *MU = muB;
   *NU = nuB;
   return(mfB);
}

int FindNumRegs(char pre, int verb, int nb, int ku, int *MACC, int *lat)
/*
 * Finds an estimate for the number of registers the compiler will let
 * you use in a generated matmul
 */
{
   int nregs, nr, ForceMACC, mu, nu, lat0;
   double mf, mf1, mfmacc;
   FILE *fp;
   char ln[128];

   sprintf(ln, "res/%cfpuMM", pre);
   fp = fopen(ln, "r");
   if (fp)
   {
      fgets(ln, 128, fp);  /* skip header */
      if (fscanf(fp, " %d %d %d", &nregs, MACC, lat) == 3)
      {
         fclose(fp);
         if (verb)
            printf("READ IN NUMBER OF GEMM REGISTERS = %d, MACC=%d, lat=%d:\n",
             nregs, *MACC, *lat);
         sprintf(ln, "res/%cnreg", pre);
         fp = fopen(ln, "w");
         fprintf(fp, "%d\n", nregs);
         fclose(fp);
         return(nregs);
      }
      fclose(fp);
   }

   ForceMACC = (*MACC >= 0);
   if (ForceMACC && *MACC && !(*lat))
   {
      fprintf(stderr,
              "If you force no MACC, then you must also force latency!\n");
      exit(-1);
   }
   if (pre == 'z')
      return(FindNumRegs('d', verb, nb, ku, MACC, lat));
   else if (pre == 'c')
      return(FindNumRegs('s', verb, nb, ku, MACC, lat));
   if (verb)
      printf("\nESTIMATING THE NUMBER OF USEABLE REGISTERS FOR GEMM:\n");
   if (!ForceMACC)
      GetMulAdd(pre, MACC, lat);
   lat0 = *lat;
   mf = FindNumRegsByMACC(pre, verb, nb, ku, *MACC, *lat, &nregs, &mu, &nu);
/*
 * Using separate multiply and add is expensive in terms of registers,
 * and is often messed up by compilers, so let's try lat=1 (for dynamically
 * scheduled machines), and using a MACC, and see what happens
 */
   if (!ForceMACC && *MACC == 0)
   {
      if (*lat > 1)
      {
         printf("\n");
         mf1 = FindNumRegsByMACC(pre, verb, nb, ku, 0, 1, &nr, &mu, &nu);
         if (mf1 >= mf)  /* latency of 1 just as good as longer latency */
         {
            nregs = nr;
            *lat = 1;
         }
      }
      printf("\n");
      mfmacc = FindNumRegsByMACC(pre, verb, nb, ku, 1, lat0, &nr, &mu, &nu);
      if (mfmacc > mf && mfmacc >= mf1) /* MACC is better */
      {
         nregs = nr;
         *MACC = 1;
         *lat = lat0;
      }
   }

   fp = fopen(ln, "w");
   assert(fp);
   fprintf(fp, "NREG  MACC  LAT\n%4d %5d %4d\n", nregs, *MACC, *lat);
   fclose(fp);
/*
 * Write # of registers to <pre>nreg for use by sysinfo tuning
 */
   sprintf(ln, "res/%cnreg", pre);
   fp = fopen(ln, "w");
   fprintf(fp, "%d\n", nregs);
   fclose(fp);

   if (verb)
      printf("NUMBER OF ESTIMATED GEMM REGISTERS = %d, MACC=%d, lat=%d:\n",
             nregs, *MACC, *lat);
   return(nregs);
}

int GetBigNB(char pre)
{
   int i, L1Elts;
   if (pre == 'd' || pre == 'z')
      L1Elts = 1024/8;
   else
      L1Elts = 1024/4;
   L1Elts *= GetL1CacheSize();
   for (i=16; i*i < L1Elts; i += 4);
   if (i*i > L1Elts)
      i -= 4;
   if (i > 80)
      i = 80;
   return(i);
}

int GetSmallNB(char pre)
{
   int i, L1Elts;
   const int imul = (pre == 'c' || pre == 'z') ? 6 : 3;
   if (pre == 'd' || pre == 'z')
      L1Elts = 1024/8;
   else
      L1Elts = 1024/4;
   L1Elts *= GetL1CacheSize();
   for (i=16; imul*i*i < L1Elts; i += 4);
   if (imul*i*i > L1Elts)
      i -= 4;
   i = Mmin(i,80);
   i = Mmax(i,16);
   return(i);
}

ATL_mmnode_t *FindBestNB
(
   char pre,   /* precision, one of s,d,c,z */
   int verb,   /* verbosity */
   ATL_mmnode_t *mmp,  /* input/output struct for best case found so far */
   int ku      /* 0: tune ku, else we must use this ku */
)
/*
 * This function tries to find the NB to use.  It varies NB, prefetch,
 * and ku (if allowed, but only between 1 and full unrolling)
 * RETURNS: matmul struct of best found case
 */
{
   int bN, b0, binc, nbB, muB, nuB, pfB, MACC, lat, KUISKB=0, i;
   double mf, mfB, mf1;

   nbB = mmp->nbB;
   mfB = mmp->mflop[0];
   muB = mmp->mu;
   nuB = mmp->nu;
   pfB = mmp->pref;
   MACC = mmp->muladd;
   lat = mmp->lat;
/*
 * Find largest block factor to tune; Since L1 estimate may be wrong,
 * make sure that larger block factors aren't competitive, but max
 * NB will be 80 regardless to avoid cleanup nightmare
 */
   if (verb)
      printf("\nFINDING UPPER BOUND ON NB:\n");
   bN = GetBigNB(pre);  /* our guess for largest useful NB */
   while (bN < 80)
   {
      mf = TryKUs(mmp, pre, verb, MACC, lat, 1, bN+4, muB, nuB, ku,
                  0, muB+nuB, 1, 0, 0);
      printf("   nb=%3d, mu=%3d, nu=%3d, ku=%3d, MACC=%d, lat=%d, mf=%.2f\n",
             bN+4, muB, nuB, mmp->ku, MACC, lat, mf);
      if (mf > mfB)
      {
         mfB = mf;
         nbB = bN+4;
      }
      else
         break;
      bN += 4;
   }
   if (bN > 80)
      bN = 80;
   if (verb)
      printf("NB UPPER BOUND CHOSEN AS : %d (%.2f)\n", bN, mfB);
/*
 * See if lowering NB past when all matrices should fit is useful
 * (again, L1 detection could be wrong)
 */
   if (verb)
      printf("\nFINDING LOWER BOUND ON NB:\n");
   b0 = GetSmallNB(pre);
   mf1 = TryKUs(mmp, pre, verb, MACC, lat, 1, b0, muB, nuB, ku,
                0, muB+nuB, 1, 0, 0);
   printf("   nb=%3d, mu=%3d, nu=%3d, ku=%3d, MACC=%d, lat=%d, mf=%.2f\n",
          b0, muB, nuB, mmp->ku, MACC, lat, mf1);
   while(b0 > 20)
   {
      mf = TryKUs(mmp, pre, verb, MACC, lat, 1, b0-4, muB, nuB, ku,
                  0, muB+nuB, 1, 0, 0);
      printf("   nb=%3d, mu=%3d, nu=%3d, ku=%3d, MACC=%d, lat=%d, mf=%.2f\n",
             b0-4, muB, nuB, mmp->ku, MACC, lat, mf);
      if (mf < mf1)
         break;
      else if (mf > mfB)
      {
         mfB = mf;
         nbB = b0-4;
      }
      b0 -= 4;
   }
   if (verb)
      printf("NB LOWER BOUND CHOSEN AS : %d\n", b0);

/*
 * Now try all NBs with varying prefetch
 */
   binc = (pre == 's' || pre == 'c') ? 4 : 2;
   KUISKB = (!ku && mmp->ku == mmp->nbB);
   b0 = (b0/binc)*binc;
   bN = (bN/binc)*binc;
   if (verb)
      printf("\nFINDING BEST NB AND PREFETCH SETTING IN RANGE [%d,%d,%d]:\n",
             b0, bN, binc);

   for (i=b0; i <= bN; i += binc)
   {
      mf = TryPFs(mmp, pre, verb, MACC, lat, 1, i, muB, nuB, KUISKB ? i:ku,
                  0, muB+nuB, 1, 0);
      printf(
      "   nb=%3d, pf=%d, mu=%3d, nu=%3d, ku=%3d, MACC=%d, lat=%d, mf=%.2f\n",
             i, mmp->pref, muB, nuB, mmp->ku, MACC, lat, mf);
      if (mf > mfB)
      {
         mfB = mf;
         nbB = i;
         pfB = mmp->pref;
      }
   }
   if (verb)
      printf("BEST NB=%d, BEST PREFETCH=%d (%.2f)\n", nbB, pfB, mfB);
   mmp->mflop[0] = mfB;
   mmp->mbB = mmp->nbB = mmp->kbB = nbB;
   mmp->pref = pfB;
   return(mmp);
}
ATL_mmnode_t *FindBestKU
(
   char pre,   /* precision, one of s,d,c,z */
   int verb,   /* verbosity */
   ATL_mmnode_t *mmp   /* input/output struct for best case found so far */
)
/*
 * Find best K unrolling.  There is no data cache dependence here, so time
 * with in-cache operands for increases speed and accuracy
 */
{
   int k, kuB, latB, kN, incK, lat;
   int nb, LAT, MACC, mu, nu, pf;
   double mf, mfB;

   LAT = mmp->lat;  /* canonical latency */
   MACC = mmp->muladd;
   mu = mmp->mu;
   nu = mmp->nu;
   nb = mmp->nbB;
   pf = mmp->pref;
   if (verb)
      printf("TRYING KUs FOR NB=%d, PF=%d, MU=%d, NU=%d MACC=%d, LAT=%d:\n",
             nb, pf, mu, nu, MACC, LAT);
/*
 * Try ku=1 as default
 */
   kuB = 1;
   latB = lat = GetGoodLat(MACC, nb, mu, nu, 1, LAT);
   mfB = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, 1,
                       0, mu+nu, 1, 0, pf, -1, -1);
   if (verb)
      printf("   KU=%d, lat=%d, mf=%.2f\n", 1, lat, mfB);
/*
 * Try NB/2 as maximal unrolling that actually has a loop
 */
   k = nb>>1;
   lat = GetGoodLat(MACC, nb, mu, nu, k, LAT);
   mf = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, k,
                      0, mu+nu, 1, 0, pf, -1, -1);
   if (verb)
      printf("   KU=%d, lat=%d, mf=%.2f\n", k, LAT, mf);
   if (mf > mfB)
   {
      mfB = mf;
      kuB = nb;
      latB = lat;
   }
/*
 * Try fully unrolled, give it .5% bonus since it is easier on lat, etc.
 */
   mf = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, nb,
                      0, mu+nu, 1, 0, pf, -1, -1);
   mf *= 1.005;
   if (verb)
      printf("   KU=%d, lat=%d, mf=%.2f\n", nb, LAT, mf);
   if (mf > mfB)
   {
      mfB = mf;
      kuB = nb;
      latB = LAT;
   }
/*
 * Have already tried 1 & KB, so now try 2, 4, 6, 8
 */
   for (k=2; k <= 8; k += 2)
   {
      lat = GetGoodLat(MACC, nb, mu, nu, k, LAT);
      mf = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, k,
                         0, mu+nu, 1, 0, pf, -1, -1);
      if (verb)
         printf("   KU=%d, lat=%d, mf=%.2f\n", k, LAT, mf);
      if (mf > mfB)
      {
         mfB = mf;
         kuB = k;
         latB = lat;
      }
   }
/*
 * Try all cases in range [8,nb/2,4]
 */
   kN = nb>>1;
   if (!mmp->muladd && mmp->lat > 2)
   {
      incK = mmp->lat;
      k = (incK >= 8) ? incK : (8/incK)*incK;
   }
   else
   {
      incK = 4;
      k = 8;
   }
   for (; k < kN; k += incK)
   {
      lat = GetGoodLat(MACC, nb, mu, nu, k, LAT);
      mf = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, k,
                         0, mu+nu, 1, 0, pf, -1, -1);
      if (verb)
         printf("   KU=%d, lat=%d, mf=%.2f\n", k, LAT, mf);
      if (mf > mfB)
      {
         mfB = mf;
         kuB = k;
         latB = LAT;
      }
   }
/*
 * Time the best found case out-of-cache so we it can be compared to others
 */
   lat = GetGoodLat(MACC, nb, mu, nu, kuB, LAT);
   mfB = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, kuB,
                       0, mu+nu, 1, 0, pf, -1, -1);
   mmp->ku = kuB;
   mmp->lat = latB;
   mmp->mflop[0] = mfB;
   if (verb)
      printf("BEST KU=%d, lat=%d (%.2f)\n", kuB, latB, mfB);
   return(mmp);
}

ATL_mmnode_t *FindBestRest
(
   char pre,   /* precision, one of s,d,c,z */
   int verb,   /* verbosity */
   ATL_mmnode_t *mmp   /* input/output struct for best case found so far */
)  /* tunes iftch, nftch, fftch, LDTOP, tries opposite muladd  */
{
   int i, j, n, nelts, nb, mu, nu, pf, MACC, lat, ku;
   int ifB, nfB, ffB, ldtopB;
   double mfB, mf, mf0;

   nb = mmp->nbB;
   pf = mmp->pref;
   mu = mmp->mu;
   nu = mmp->nu;
   ldtopB = 0;
   ifB = nelts = mu + nu;
   nfB = 1;
   ffB = 0;
   mfB = 0;
   MACC = mmp->muladd;
   lat = mmp->lat;
   ku = mmp->ku;
   if (verb)
      printf( "FINDING BEST FETCH PATTERN FOR nb=%d, mu=%d, nu=%d, ku=%d\n",
             nb, mu , nu, ku);
   for (i=2; i <= nelts; i++)
   {
      n = nelts - i;
      n = Mmin(i, n);
      if (!n)
         n = 1;
      for (j=1; j <= n; j++)
      {
         mf = TimeGMMKernel(verb, 1, pre, MACC, lat, 1, nb, mu, nu, ku,
                            0, i, j, 0, pf, -1, -1);
         if (verb)
            printf ("   ifetch=%2d, nfetch=%2d, mf=%.2f\n", i, j, mf);
         if (mf > mfB)
         {
            ifB = i;
            nfB = j;
            mfB = mf;
         }
      }
   }
/*
 * overwrite bad ifetch value output file with selected one
 */
   mfB = TimeGMMKernel(verb, 1, pre, MACC, lat, 1, nb, mu, nu, ku,
                       0, ifB, nfB, 0, pf, -1, -1);
   if (verb)
      printf("   best case retimed as mf=%.2f\n", mfB);
   mmp->mflop[0] = mfB;
   mmp->iftch = ifB;
   mmp->nftch = nfB;
   if (verb)
      printf("BEST IFETCH=%d, NFETCH=%d (%.2f)\n", ifB, nfB, mfB);
/*
 * Try force fetch for beta=0
 */
   if (verb > 1)
      printf("TRYING FALSE FETCH FOR BETA=0 CASES:\n");
   mf0 = TimeGMMKernel(verb, 1, pre, MACC, lat, 0, nb, mu, nu, ku,
                       0, ifB, nfB, 0, pf, -1, -1);
   if (verb > 1)
      printf("   noFF=%.2f\n", mf0);
   mf = TimeGMMKernel(verb, 1, pre, MACC, lat, 0, nb, mu, nu, ku,
                      1, ifB, nfB, 0, pf, -1, -1);
   if (verb > 1)
      printf("   yesFF=%.2f\n", mf);
   if (mf > mf0)
      ffB = 1;
   mmp->fftch = ffB;
/*
 * If loading C at top is 2% faster, take it despite error bound hit
 */
   if (verb)
      printf("TRYING LOAD-AT-TOP (load-at-bottom %.2f)\n", mfB);
   mf  = TimeGMMKernel(verb, 0, pre, MACC, lat, 1, nb, mu, nu, ku,
                       ffB, ifB, nfB, 1, pf, -1, -1);
   if (verb)
      printf("   load-at-top, mf=%.2f\n", mf);
   if (mfB*1.02 > mf)
   {
      if (verb)
         printf("STICKING WITH LOAD-AT-BOTTOM\n");
   }
   else
   {
      ldtopB = 1;
      if (verb)
         printf("SWITCHING TO LOAD-AT-TOP\n");
      mmp->flag |= (1<<MMF_LDCTOP);
      mfB = mf;
   }

/*
 * See if reversing muladd setting is helpful
 */
   if (verb)
      printf("TRYING SWAP OF MACC (present, madd=%d, lat=%d, mf=%.2f)\n",
             MACC, lat, mfB);
   if (MACC)
   {
      i = Mmax(mmp->lat, 4);
      i = GetGoodLat(0, nb, mu, nu, ku, i);
   }
   else
      i = mmp->lat;
   mf  = TimeGMMKernel(verb, 0, pre, !MACC, i, 1, nb, mu, nu, ku,
                       ffB, ifB, nfB, ldtopB, pf, -1, -1);
   if (verb)
      printf("   macc=%d, lat=%d, mf=%.2f\n", !MACC, i, mf);
   if (mf > mfB)
   {
      mmp->muladd = !MACC;
      mmp->lat = i;
      mfB = mf;
      if (verb)
         printf("SWITCHING TO NEW MACC SETTING!\n");
   }
   else if (verb)
      printf("KEEPING MACC SETTING.\n");
   mmp->mflop[0] = mfB;
   return(mmp);
}

ATL_mmnode_t *FindBestGenGemm
(
   char pre,   /* precision, one of s,d,c,z */
   int verb,   /* verbosity */
   int nregs,  /* max # of registers to use */
   int MACC,   /* 1: machine has multiply & accumulate, else separate */
   int lat,    /* latency on multiply */
   int FNB,    /* is it required to use NB, or can we tune? */
   int NB,     /* suggested nb */
   int ku      /* 0: tune ku, else we must use this ku */
)
/*
 * This routine finds the best copy matmul case that can be generated by
 * emit_mm.c.  It will search over the following parameters:
 *    (nb,pf), (mu,nu), ku, nftch, iftch, fftch, LDTOP
 *
 * pf is currently 1 or 0, and it controls whether the next block of A is
 * prefetched or not.
 *
 * LDTOP determines if we load C values before entering the K loop (TOP)
 * or after.  After gives better error bound, so give it slight advantage
 *
 * nftch,iftch are crude load scheduling parameters, and they tend to
 * have little affect on most machines (the compiler usually reschedules
 * the loads on its own).
 *
 * fftch causes the generator to load C at the top of the loop even
 * when we are don't need the values there, so that C is in cache at
 * the bottom of the loop when we need it.
 *
 * RETURNS: filled structure with best gemm case found
 */
{
   ATL_mmnode_t *mmp;
   int nb, N, Ng, i, j, mu, nu, nbB, muB, nuB;
   int *mus, *nus, *ip;
   double mf, mfB, mf1;
   double *fpls;
   #ifdef ATL_GAS_x8664
      #define NEXMU 5
      int exmu[NEXMU] = {4, 6, 8, 10, 12};
      int exnu[NEXMU] = {1, 1, 1, 1,  1};
   #elif defined(ATL_GAS_x8632)
      #define NEXMU 4
      int exmu[NEXMU] = {3, 4, 6, 2};
      int exnu[NEXMU] = {1, 1, 1, 2};
   #endif
   char upr;
   char ln[128];

/*
 * Use either required nb, or one that is a multiple of a lot of our
 * unrolling factors;  Use a big block factor so that our register blocking
 * matters more (cache is covering less of costs)
 */
   if (FNB)
      nb = NB;
   else
   {
      nb = (GetBigNB(pre)/12)*12;
      if (nb < 24)
         nb = 24;
   }
   if (pre == 'd' || pre == 's')
   {
      mmp = GetMMNode();
      FillInGMMNode(verb, mmp, pre, MACC, lat, 1, nb, 1, 1, 1, 0, 2, 1, 0, 0);

/*
 *    Get all MU/NU unrollings, Ng of them are competitive on flops/load ratio.
 *    For x86, always include extra 1-D blockings in mix, even if they
 *    are not judged competive (because if reg-reg moves aren't free, which
 *    is true for older x86 machines, 2-D register blocks don't really work
 *    due to 2-operand assembly)
 */
      GetMuNus(nregs, MACC, lat, &Ng, &N, &mus, &nus, &fpls);
      free(fpls);
      #ifdef NEXMU
         for (j=0; j < NEXMU; j++)
         {
            mu = exmu[j];
            nu = exnu[j];
            for (i=0; i < Ng; i++)
               if (mus[i] == mu && nus[i] == nu) break;
            if (i == Ng)
            {
               if (Ng >= N)
               {
                  ip = malloc((Ng+1)*sizeof(int));
                  assert(ip);
                  for (i=0; i < Ng; i++)
                     ip[i] = mus[i];
                  free(mus);
                  mus = ip;
                  ip = malloc((Ng+1)*sizeof(int));
                  assert(ip);
                  for (i=0; i < Ng; i++)
                     ip[i] = nus[i];
                  free(nus);
                  nus = ip;
               }
               mus[Ng] = mu;
               nus[Ng] = nu;
               Ng++;
            }
         }
      #endif
      if (verb)
         printf("PROBING FOR M AND N UNROLLING FACTORS:\n");
/*
 *    Try all competitive unrolling factors
 */
      mfB = 0;
      muB = nuB = 1;
      for (i=0; i < Ng; i++)
      {
         mf = TryKUs(mmp, pre, verb, MACC, lat, 1, nb, mus[i], nus[i], ku,
                     0, mus[i]+nus[i], 1, 0, 0);

         printf("   nb=%3d, mu=%3d, nu=%3d, ku=%3d, MACC=%d, lat=%d, mf=%.2f\n",
                nb, mus[i], nus[i], mmp->ku, MACC, lat, mf);
         if (mf > mfB)
         {
            muB = mus[i];
            nuB = nus[i];
            mfB = mf;
         }
      }
      mmp->mu = muB;
      mmp->nu = nuB;
      mmp->iftch = muB+nuB;
      mmp->mflop[0] = mfB;
      printf("SELECTED MU=%d, NU=%d (%.2f)\n", muB, nuB, mfB);
      free(mus);
      free(nus);
      nbB = nb;
   }
   else /* complex types gets their MU & NU from real cases */
   {
      upr = (pre == 'z') ? 'd' : 's';
      mmp = ReadMMFileWithPath(upr, "res", "gMMRES.sum");
      if (!mmp)
      {
         sprintf(ln, "make res/%cgMMRES.sum > /dev/null 2>&1", upr);
         assert(system(ln) == 0);
         mmp = ReadMMFileWithPath(upr, "res", "gMMRES.sum");
         assert(mmp);
      }
      muB = mmp->mu;
      nuB = mmp->nu;
      nbB = nb;
      mfB = TryKUs(mmp, pre, verb, MACC, lat, 1, nb, muB, nuB, ku,
                   0, muB+nuB, 1, 0, 0);
      mmp->mflop[0] = mfB;
      printf("READ IN MU=%d, NU=%d FROM REAL, nb=%d, mf=%.2f\n",
             muB, nuB, nb, mfB);
   }
/*
 * If we are allowed, try to tune NB
 */
   if (!FNB)
      mmp = FindBestNB(pre, verb, mmp, ku);
   else  /* still need to scope prefetch settings with required NB */
   {
      mmp->nbB = mmp->mbB = mmp->kbB = nb;
      mf = TryPFs(mmp, pre, verb, MACC, lat, 1, nb, muB, nuB, ku,
                  0, muB+nuB, 1, 0);
   }
/*
 * If we are allowed, tune ku
 */
   if (!ku)
      mmp = FindBestKU(pre, verb, mmp); /* tunes ku */
   mmp = FindBestRest(pre, verb, mmp);  /* tunes iftch, nftch, fftch, LDTOP */
   return(mmp);
}

int main(int nargs, char **args)
{
   char pre, *outfile;
   int verb, nregs, FNB, nb, ku, MACC, lat, mu, nu;
   ATL_mmnode_t *mmp, *mm;

   pre = GetFlags(nargs, args, &verb, &nregs, &nb, &ku, &MACC, &lat, &outfile);
   if (nregs == -1)  /* run # register probe only */
   {
      nb = GetBigNB(pre);
      nregs = FindNumRegs(pre, verb, nb, ku, &MACC, &lat);
      exit(0);
   }
   mmp = ReadMMFile(outfile);
   if (mmp)
   {
      if (mmp->mflop[0] <= 0)  /* need to retime */
      {
         for (mm=mmp; mm; mm = mm->next)
         {
            mm->mflop[0] = TimeGMMKernel(verb, 0, pre, mm->muladd, mm->lat,
                                         1, mm->nbB, mm->mu, mm->nu, mm->ku,
                                         mm->fftch, mm->iftch, mm->nftch,
                                         FLAG_IS_SET(mm->flag, MMF_LDCTOP),
                                         mm->pref, -1, -1);
         }
         WriteMMFile(outfile, mmp);
      }
      printf("\nSEARCH OUTPUT READ IN AS:\n");
      PrintMMNodes(stdout, mmp);
      exit(0);
   }
   if (nb > 0)
      FNB = 1;
   else
   {
      nb = GetBigNB(pre);
      FNB = 0;
   }
   if (!nregs)
      nregs = FindNumRegs(pre, verb, nb, ku, &MACC, &lat);
   else if (MACC < 0)
      ConfirmMACC(pre, verb, nregs, nb, ku, &MACC, &lat);
   mmp = FindBestGenGemm(pre, verb, nregs, MACC, lat, FNB, nb, ku);
   WriteMMFile(outfile, mmp);
   printf("\nSELECTED GENERATED KERNEL:\n");
   PrintMMNodes(stdout, mmp);
   KillMMNode(mmp);
   exit(0);
}
