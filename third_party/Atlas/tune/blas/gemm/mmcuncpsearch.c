/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
 *               2007, 2008, 2009, 2010 R. Clint Whaley
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
#define VERB 1

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "atlas_misc.h"
#include "atlas_fopen.h"
#include "atlas_prefetch.h"
#include "atlas_gentesttime.h"
#include "atlas_mmtesttime.h"

#define Mmin(x, y) ( (x) > (y) ? (y) : (x) )

#define TOLERANCE 1.2
#define REPS 4096
#define L1FNAME "L1CacheSize"
#define NTIM 3
#define MAXLAT 6
/*
 * For 2-operand assemblers, no benefit from 2-D register blocking, so flag
 * them;  If unknown arch is also 2-op, no problem will just search longer
 */

char LANG;

void PrintUsage(char *xnam)
{
   fprintf(stderr, "\n\nUsage: %s [-r #][-h][-f][-l #][-p s/d/c/z][-m #]\n",
           xnam);
   fprintf(stderr, "-h         : Print this help screen\n");
   fprintf(stderr, "-f         : Force complete search over given parameters\n");
   fprintf(stderr, "-p s/d/c/z : set the precision to search for\n");
   fprintf(stderr, "-r #       : Set max number of registers to use to # (default 32)\n");
   fprintf(stderr, "-m #       : Set max L1 cache size (kilobytes) to #\n");
   fprintf(stderr, "-L <c/f>   : Select what language to use (C or Fortran77)\n");
   fprintf(stderr, "-K #       : Set K-loop unrolling to # (-1 = K).\n");
   fprintf(stderr, "-l #       : Use latency factor #.  If set to 0,\n");
   fprintf(stderr,
"             do not do latency checking.  By default, latency checking is\n");
   fprintf(stderr,
"             done only if initial timings show it is a win.\n");
   exit(-1);
}

void GetSettings(int nargs, char *args[], char *pre, char *lang, int *ku,
                 int *LAT, int *FRC, int *nreg, int *MaxL1Size, int *ROUT)
{
   int i;

   *FRC = 0;
   *LAT = -1;
   *nreg = -1;
   *MaxL1Size = 128;
   *pre = 'd';
   *lang = 'C';
   *ku = 0;
   *ROUT = 0;
   for (i=1; i < nargs; i++)
   {
      if (*args[i] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'K':
         *ku = atoi(args[++i]);
         break;
      case 'L':
         i++;
         if ( (*args[i] == 'F') || (*args[i] == 'f') ) *lang = 'F';
         break;
      case 'm' :
         *MaxL1Size = atoi(args[++i]);
         break;
      case 'r' :
         *nreg = atoi(args[++i]);
         break;
      case 'f' :
         *FRC = atoi(args[++i]);
         break;
      case 'l' :
         *LAT = atoi(args[++i]);
         break;
      case 'p' :
         *pre = *args[++i];
         break;
      default:
      case 'R':
         *ROUT = atoi(args[++i]);
         break;
      case 'h' :
         PrintUsage(args[0]);
      }
   }
}

int L1Elts(char pre, int MaxL1Size)
{
   FILE *L1f;
   int L1Size, tsize;
   char ln[128];

   if (!FileExists("res/L1CacheSize"))
   {
      sprintf(ln, "make RunL1 MaxL1=%d\n",MaxL1Size);
      if (system(ln) != 0)
      {
         remove("res/L1CacheSize");
         fprintf(stderr, "Error in command: %s", ln);
         exit(-1);
      }
   }
   L1f = fopen("res/L1CacheSize", "r");
   assert(L1f != NULL);
   fscanf(L1f, "%d", &L1Size);
   fclose(L1f);
   switch (pre)
   {
      case 's':
         tsize = sizeof(float);
         break;
      case 'd':
         tsize = sizeof(double);
         break;
      case 'q':
         tsize = sizeof(long double);
         break;
      case 'c':
         tsize = sizeof(float);
         break;
      case 'z':
         tsize = sizeof(double);
         break;
   }
   return( (L1Size*1024) / tsize);
}

int GetCacheSize(int MaxL1Size)
/*
 * Returns L1 size in kilobytes
 */
{
   FILE *L1f;
   int L1Size;
   char ln[32];

   if (!FileExists("res/L1CacheSize"))
   {
      sprintf(ln, "make RunL1 MaxL1=%d\n",MaxL1Size);
      if (system(ln) != 0)
      {
         remove("res/L1CacheSize");
         fprintf(stderr, "Error in command: %s", ln);
         exit(-1);
      }
   }
   L1f = fopen("res/L1CacheSize", "r");
   assert(L1f != NULL);
   fscanf(L1f, "%d", &L1Size);
   fclose(L1f);
   fprintf(stderr, "\n      Read in L1 Cache size as = %dKB.\n",L1Size);
   return(L1Size);
}

int GetTypeSize(char pre)
{
   int tsize;
   if (pre == 'c' || pre == 's') tsize = ATL_ssize;
   else tsize = ATL_dsize;
   return(tsize);
}
void findNBs(char prec, char *NBnam, int MaxL1Size)
{
   FILE *NBf;
   char ln[80];
   int i, L1Size, tmp, tsize, tL1Size, CL, nNB;
   int NB[100];

   fprintf(stderr, "NB setting not supplied; calculating:\n");

   L1Size = GetCacheSize(MaxL1Size);
   tsize = GetTypeSize(prec);

   tL1Size = L1Size * (1024 / tsize);
   tmp = CL = ATL_Cachelen / tsize;
   if (!tmp) tmp=1;
   nNB = 0;
   fprintf(stderr, "tmp=%d, tL1size=%d\n",tmp, tL1Size);
   while (tmp*tmp <= tL1Size)
   {
      if (tmp >= 16)        /* no block sizes smaller than 16 */
         NB[nNB++] = tmp;
      if (tmp >= 80) break;  /* no block sizes bigger than 80 */
      tmp += CL;
   }
   if (!nNB)  /* this should never happen */
   {
      nNB = 3;
      NB[0] = 8;
      NB[1] = 4;
      NB[2] = 16;
   }
   else if (nNB > 2)  /* put second biggest blocking factor first in list */
   {
      tmp = NB[nNB-2];
      NB[nNB-2] = NB[0];
      NB[0] = tmp;
   }

   NBf = fopen(NBnam, "w");
   fprintf(NBf, "%d\n", nNB);
   for (i=0; i != nNB; i++) fprintf(NBf, "%d\n", NB[i]);
   fclose(NBf);
}

int GetSafeNB(char pre, int MaxL1)
{
   int i, L1, tsize, inc;

   tsize = GetTypeSize(pre);
   inc = ATL_MinMMAlign / tsize;
   if (inc < 4) inc = 4;
   L1 = (GetCacheSize(MaxL1) * 1024) / tsize;
   for (i=inc; i*i < L1; i += inc);
   if (i*i > L1) i -= inc;
   if (pre == 'd' || pre == 's')
   {
      if (i*i == L1) i -= inc;
   }
   else
   {
      if (i*i == L1) i -= 2*inc;
      else i -= inc;
   }
   if (i < 16) i = 16;
   if (i > 80) i = 80;
   return(i);
}

double GetAvg(int n, double tolerance, double *mflop)
{
   int i, j;
   double t0, tavg;
/*
 * Sort results, largest first
 */
   for (i=0; i != n; i++)
   {
      for (j=i+1; j < n; j++)
      {
         if (mflop[i] < mflop[j])
         {
            t0 = mflop[i];
            mflop[i] = mflop[j];
            mflop[j] = t0;
         }
      }
   }
/*
 * Not doing tolerance anymore, just take largest mflop rate if doing wall
 * times, or median value if doing CPU
 */

#if 1
   #ifdef WALL
      tavg = mflop[0];
   #else
      tavg = mflop[n/2];
   #endif
#else
/*
 * Throw out result if it is outside tolerance; rerun if two mflop not within
 * tolerance;  this code assumes n == 3
 */
   if (tolerance*mflop[1] < mflop[0])  /* too big a range in results */
   {
      if (tolerance*mflop[2] < mflop[1]) return(-1.0);
      tavg = (mflop[1] + mflop[2]) / 2.0;
   }
   else if (tolerance*mflop[2] < mflop[0]) tavg = (mflop[0] + mflop[1]) / 2.0;
   else tavg = (mflop[0] + mflop[1] + mflop[2]) / 3.0;
#endif

   return(tavg);
}

double mms_case(char pre, int MULADD, int NB, int mu, int nu, int ku,
                int pfA, int lat)
{
   char fnam[128], ln[256];
   int i;
   double mflop[NTIM], t0;
   FILE *fp;

   if (ku > NB) ku = NB;
   else if (ku == -1) ku = NB;
   sprintf(fnam,
           "res/%c%smm%c%c%d_%dx%dx%d_%dx%dx%d_%dx%dx%d%s%s_%dx%d_%d_pf%d",
           pre, "JIK", 'T', 'N', NB, NB, NB, NB, NB, NB, 0, mu, nu, ku,
           "_a1", "_b1", MULADD, lat, 1, pfA);
   if (!FileExists(fnam))
   {
      if (pre == 'c' || pre == 'z')
         sprintf(ln,
" make mmcase pre=%c loopO=%s ta=%c tb=%c mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d mu=%d nu=%d ku=%d alpha=%d beta=%d muladd=%d lat=%d csA=1 csB=1 csC=2 cleanup=%d pfA=%d > /dev/null 2>&1\n",
                   pre, "JIK", 'T', 'N', NB, NB, NB, NB, NB, 0, mu, nu, ku,
                   1, 1, MULADD, lat, 1, pfA);
      else sprintf(ln,
" make mmcase pre=%c loopO=%s ta=%c tb=%c mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d mu=%d nu=%d ku=%d alpha=%d beta=%d muladd=%d lat=%d cleanup=%d pfA=%d > /dev/null 2>&1\n",
                   pre, "JIK", 'T', 'N', NB, NB, NB, NB, NB, 0, mu, nu, ku,
                   1, 1, MULADD, lat, 1, pfA);
      if (system(ln) != 0)
      {
         fprintf(stderr, "ERROR IN COMMAND: %s", ln);
         fprintf(stderr, "   PROPOSED FILENAME: %s\n", fnam);
         sprintf(ln, "rm -f %s\n", fnam);
         system(ln);
         exit(-1);
      }
   }
   t0 = *((double*)ReadResultsFile(0, 3,fnam));
   fprintf(stdout,
"   pre=%c, muladd=%d, lat=%d, pf=%d, nb=%d, mu=%d, nu=%d, ku=%d, mflop=%.2f\n",
           pre, MULADD, lat, pfA, NB, mu, nu, ku, t0);
   return(t0);
}

double mms_caseIC(char pre, int MULADD, int NB, int mu, int nu, int ku,
                  int pfA, int lat)
/*
 * Do simple mmcase, where all operands are kept cache-resident
 * (useful for FPU optimization phases)
 */
{
   char fnam[128], ln[512];
   int i;
   double mflop[NTIM], t0;
   FILE *fp;

   if (ku > NB) ku = NB;
   else if (ku == -1) ku = NB;
   sprintf(fnam,
           "res/%c%smm%c%c%d_%dx%dx%d_%dx%dx%d_%dx%dx%d%s%s_%dx%d_%d_IC",
           pre, "JIK", 'T', 'N', NB, NB, NB, NB, NB, NB, 0, mu, nu, ku,
           "_a1", "_b1", MULADD, lat, 1);
   if (!FileExists(fnam))
   {
      if (pre == 'c' || pre == 'z')
         sprintf(ln,
" make mmcase pre=%c loopO=%s ta=%c tb=%c mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d mu=%d nu=%d ku=%d alpha=%d beta=%d muladd=%d lat=%d csA=1 csB=1 csC=2 cleanup=%d casnam=%s moves=\"\" > /dev/null 2>&1\n",
                   pre, "JIK", 'T', 'N', NB, NB, NB, NB, NB, 0, mu, nu, ku,
                   1, 1, MULADD, lat, 1, fnam);
      else sprintf(ln,
" make mmcase pre=%c loopO=%s ta=%c tb=%c mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d mu=%d nu=%d ku=%d alpha=%d beta=%d muladd=%d lat=%d cleanup=%d casnam=%s moves=\"\" > /dev/null 2>&1\n",
                   pre, "JIK", 'T', 'N', NB, NB, NB, NB, NB, 0, mu, nu, ku,
                   1, 1, MULADD, lat, 1, fnam);
      if (system(ln) != 0)
      {
         fprintf(stderr, "ERROR IN COMMAND: %s", ln);
         fprintf(stderr, "   PROPOSED FILENAME: %s\n", fnam);
         sprintf(ln, "rm -f %s\n", fnam);
         system(ln);
         exit(-1);
      }
   }
   t0 = *((double*)ReadResultsFile(0, 3, fnam));
   fprintf(stdout,
"   pre=%c, muladd=%d, lat=%d, pf=%d, nb=%d, mu=%d, nu=%d, ku=%d, mflop=%.2f\n",
           pre, MULADD, lat, pfA, NB, mu, nu, ku, t0);
   return(t0);
}

double mmcase0(char *nam, char pre, char *loopO, char ta, char tb,
              int M, int N, int K, int mb, int nb, int kb,
              int lda, int ldb, int ldc, int mu, int nu, int ku,
              int muladd, int pfA, int lat, int beta, int csA, int csB, int csC,
              int FFetch, int ifetch, int nfetch, char *mmnam)
{
   char fnam[128], ln[512], bnam[16], casnam[128], mmcase[128];
   int i, N0, lda2=lda, ldb2=ldb, ldc2=ldc;
   double mflop[NTIM], t0;
   FILE *fp;

   if (lda < 0) { lda2 = -lda; lda = 0; }
   if (ldb < 0) { ldb2 = -ldb; ldb = 0; }
   if (ldc < 0) { ldc2 = -ldc; ldc = 0; }
   if (mmnam) sprintf(mmcase, "mmucase mmrout=%s", mmnam);
   else sprintf(mmcase, "mmcase");
   if (ifetch == -1 || nfetch == -1) { ifetch = mu+nu; nfetch = 1; }
   if (beta == 1) sprintf(bnam, "_b1");
   else if (beta == -1) sprintf(bnam, "_bn1");
   else if (beta == 0) sprintf(bnam, "_b0");
   else sprintf(bnam, "_bX");
   N0 = Mmax(M,N);
   if (N0 < K) N0 = K;
   if (ku > K) ku = K;
   else if (ku == -1) ku = K;
   if (nam)
   {
      strcpy(fnam, nam);
      sprintf(casnam, "casnam=%s", nam);
   }
   else
   {
      sprintf(fnam,
              "res/%c%smm%c%c%d_%dx%dx%d_%dx%dx%d_%dx%dx%d%s%s_%dx%d_%d_pf%d",
              pre, loopO, ta, tb, N0, mb, nb, kb, lda, ldb, ldc, mu, nu, ku,
              "_a1", bnam, muladd, lat, 1, pfA);
      casnam[0] = '\0';
   }
   if (!FileExists(fnam))
   {
      if (pre == 'c' || pre == 'z')
         sprintf(ln,
" make %s pre=%c loopO=%s ta=%c tb=%c M=%d N=%d K=%d mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d lda2=%d ldb2=%d ldc2=%d mu=%d nu=%d ku=%d alpha=%d beta=%d muladd=%d lat=%d cleanup=%d csA=%d csB=%d csC=%d ff=%d if=%d nf=%d pfA=%d %s > /dev/null 2>&1\n",
                 mmcase,pre, loopO, ta, tb, M, N, K, mb, nb, kb, lda, ldb, ldc,
                 lda2, ldb2, ldc2, mu, nu, ku, 1, beta, muladd, lat, 1,
                 csA, csB, csC, FFetch, ifetch, nfetch, pfA, casnam);
      else sprintf(ln,
" make %s pre=%c loopO=%s ta=%c tb=%c M=%d N=%d K=%d mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d lda2=%d ldb2=%d ldc2=%d mu=%d nu=%d ku=%d alpha=%d beta=%d muladd=%d lat=%d cleanup=%d ff=%d if=%d nf=%d pfA=%d %s > /dev/null 2>&1\n",
                   mmcase, pre, loopO, ta, tb, M, N, K, mb, nb, kb, lda, ldb,
                   ldc, lda2, ldb2, ldc2, mu, nu, ku, 1, beta, muladd, lat, 1,
                   FFetch, ifetch, nfetch, pfA, casnam);
      if (system(ln) != 0)
      {
/*
 *       User cases, and large leading dimensions can fail to run
 */
         if (mmnam) return(-1.0);  /* user cases can fail to compile */
         if (lda2 != lda || ldb2 != ldb || ldc2 != ldc) return(-1);
         fprintf(stderr, "Error in command: %s", ln);
         sprintf(ln, "rm -f %s\n", fnam);
         system(ln);
         exit(-1);
      }
   }
   t0 = *((double*)ReadResultsFile(0, 3, fnam));
   fprintf(stdout,
"   pre=%c, loopO=%s, ta=%c tb=%c, mb=%d, nb=%d, kb=%d, lda=%d, ldb=%d, ldc=%d\n",
           pre, loopO, ta, tb, mb, nb, kb, lda, ldb, ldc);
  fprintf(stdout, "   mu=%d, nu=%d, ku=%d, muladd=%d, lat=%d ====> mflop=%f\n",
          mu, nu, ku, muladd, lat, t0);
   return(t0);
}

double mmucase(int ifile, char pre, int nb, int muladd, int lat,
               int mu, int nu, int ku, char *fnam)
{
   char fout[64];
   int iff;

   sprintf(fout, "res/%cuser%d", pre, ifile);
   if (mu == 1 && nu == 1) iff = 1;
   else iff = mu + nu;
   return(mmcase0(fout, pre, "JIK", 'T', 'N', nb, nb, nb, nb, nb, nb,
                  nb, nb, 0, mu, nu, ku, muladd, 0, lat, 1, 1, 1, 2, 0, iff, 1,
                  fnam));
}

enum CW {CleanM=0, CleanN=1, CleanK=2, CleanNot=3};
double mmclean(char pre, enum CW which, char *loopO, char ta, char tb,
               int M, int N, int K, int mb, int nb, int kb,
               int lda, int ldb, int ldc, int mu, int nu, int ku,
               int muladd, int pfA, int lat, int beta, int csA, int csB,
               int csC, int FFetch, int ifetch, int nfetch)
{
   char nam[128];
   char cwh[3] = {'M', 'N', 'K'};
   sprintf(nam, "res/%cClean%c_%dx%dx%d", pre, cwh[which], M, N, K);
   return(mmcase0(nam, pre, loopO, ta, tb, M, N, K, mb, nb, kb, lda, ldb, ldc,
                  mu, nu, ku, muladd, pfA, lat, beta, csA, csB, csC,
                  FFetch, ifetch, nfetch, NULL));
}

double mmcase(char *nam, char pre, char *loopO, char ta, char tb,
              int M, int N, int K, int mb, int nb, int kb,
              int lda, int ldb, int ldc, int mu, int nu, int ku,
              int muladd, int pfA, int lat, int beta,
              int csA, int csB, int csC, int FFetch, int ifetch, int nfetch)
{
   return(mmcase0(nam, pre, loopO, ta, tb, M, N, K, mb, nb, kb, lda, ldb, ldc,
                  mu, nu, ku, muladd, pfA, lat, beta, csA, csB, csC,
                  FFetch, ifetch, nfetch, NULL));
}

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


void FindMUNU(int muladd,  /* 0: use separate multiply and add inst */
              int lat,     /* pipe len for muladd=0 */
              int nr,      /* # of registers available */
              int FullTest, /* 0: use shortcut if available */
              int *MU,     /* suggested MU */
              int *NU)     /* suggested NU */
/*
 * Find near-square muxnu using nr registers or less
 */
{
   int i, j, mu, nu, Ng, N;
   int *mus, *nus;
   double *fpls;

   if (nr < 1)
   {
      *MU = lat;
      *NU = 1;
      return;
   }
   if (muladd) j = nr;
   else j = nr - lat;
   if (j < 3) mu = nu = 1;
   else
   {
/*
 *    For x86, two-operand assembler means 1-D case almost certainly best
 */
      #ifdef TWO_OP_ASM
          if (!FullTest)
          {
             #ifdef ATL_USE64BITS
                mu = 8;
                mu = ((8+lat-1)/lat)*lat;
                if (mu > 12)
                   mu = Mmin(lat, 8);
                nu = 1;
             #else
                if (lat > 2) mu = lat;
                else mu = 4;
                nu = 1;
             #endif
          }
          else {
      #endif
         GetMuNus(nr, muladd, lat, &Ng, &N, &mus, &nus, &fpls);
         i = GetSafeGoodMuNu(nr, muladd, lat, N, mus, nus, fpls);
         mu = mus[i];
         nu = mus[i];
         free(mus);
         free(nus);
         free(fpls);
      #ifdef TWO_OP_ASM
      }
      #endif
   }
   *MU = mu;
   *NU = nu;
}

void PutInstLogLine(FILE *fp, int muladd, int pfA, int lat, int nb,
                    int mu, int nu, int ku, int ForceFetch,
                    int ifetch, int nfetch, double mflop)
{
   fprintf(fp, "%6d  %3d %4d %3d %3d %3d %3d  %5d  %5d  %5d  %7.2lf\n",
           muladd, lat, pfA, nb, mu, nu, ku, ForceFetch, ifetch, nfetch, mflop);
}

void PutInstLogFile(FILE *fp, int muladd, int pfA, int lat, int nb,
                    int mu, int nu, int ku, int ForceFetch,
                    int ifetch, int nfetch, double mflop)
{
   fprintf(fp,
   "MULADD  LAT  PREF NB  MU  NU  KU  FFTCH  IFTCH  NFTCH    MFLOP\n");
   PutInstLogLine(fp, muladd, pfA, lat, nb, mu, nu, ku, ForceFetch,
                  ifetch, nfetch, mflop);
}

void PutInstLogFile1(char *fnam, char pre, int muladd, int pfA, int lat,
                     int nb, int mu, int nu, int ku,
                     int ForceFetch, int ifetch, int nfetch, double mflop)
{
   FILE *fp;

   fp = fopen(fnam, "w");
   assert(fp);
   PutInstLogFile(fp, muladd, pfA, lat, nb, mu, nu, ku, ForceFetch, ifetch,
                  nfetch, mflop);
   fclose(fp);
}

void GetInstLogLine(FILE *fp, int *muladd, int *pfA, int *lat, int *nb,
                    int *mu, int *nu, int *ku, int *ForceFetch,
                    int *ifetch, int *nfetch, double *mflop)
{
   assert(fscanf(fp, " %d %d %d %d %d %d %d %d %d %d %lf\n",
                 muladd, lat, pfA, nb, mu, nu, ku, ForceFetch,
                 ifetch, nfetch, mflop) == 11);
}

void GetInstLogFile(char *nam, char pre, int *muladd, int *pfA, int *lat,
                    int *nb, int *mu, int *nu, int *ku, int *ForceFetch,
                    int *ifetch, int *nfetch, double *mflop)
{
   char ln[128];
   FILE *fp;

   fp = fopen(nam, "r");
   if (fp == NULL) fprintf(stderr, "file %s not found!!\n\n", nam);
   assert(fp);
   fgets(ln, 128, fp);
   GetInstLogLine(fp, muladd, pfA, lat, nb, mu, nu, ku, ForceFetch,
                  ifetch, nfetch, mflop);
   fclose(fp);
}

void CreateFinalSumm(char pre, int muladd, int pfA, int lat, int nb, int mu,
                     int nu, int ku, int Ff, int If, int Nf, double gmf)
{
   char ln[64], auth[65];
   FILE *fp, *fp0;
   int icase, unb;
   double umf;

   sprintf(ln, "res/%cMMRES", pre);
   fp = fopen(ln, "w");
   PutInstLogFile(fp, muladd, pfA, lat, nb, mu, nu, ku, Ff, If, Nf, gmf);
   sprintf(ln, "res/%cuMMRES", pre);
   fp0 = fopen(ln, "r");
   assert(fp0);
   assert(fgets(ln, 64, fp0));
   assert(fscanf(fp0, " %d %d %lf \"%[^\"]\" \"%[^\"]", &icase, &unb, &umf,
                 ln, auth) == 5);
   fclose(fp0);
   fprintf(fp, "\nICASE  NB    MFLOP  ROUT  AUTHOR\n");
   fprintf(fp, "%5d %3d %8.2f  \"%.63s\" \"%.63s\"\n", icase, unb, umf,
           ln, auth);
   fclose(fp);
}


void FindFetch(char ta, char tb, char pre, int mb, int nb, int kb,
               int mu, int nu, int ku, int muladd, int pfA, int lat,
               int *FFetch0, int *ifetch0, int *nfetch0)
/*
 * See what fetch patterns are appropriate
 */
{
   char fnam[128];
   const int nelts = mu+nu;
   int csA=1, csB=1, csC=1, nleft, i, j;
   int ifetch = mu+nu, nfetch = 1;
   double mf, mf0;

   if (pre == 'c' || pre == 'z') csC = 2;

   mf0 = mmcase(NULL, pre, "JIK", ta,  tb, mb, nb, kb, mb, nb, kb,
                kb, kb, 0, mu, nu, ku, muladd, pfA, lat, 0, csA, csB, csC,
                0, ifetch, nfetch);

   for (i=2; i < nelts; i++)
   {
      nleft = nelts - i;
      for (j=1; j <= nleft; j++)
      {
         sprintf(fnam, "res/%cMMfetch%d_%d", pre, i, j);
         mf = mmcase(fnam, pre, "JIK", ta,  tb, mb, nb, kb, mb, nb, kb,
                     kb, kb, 0, mu, nu, ku, muladd, pfA, lat, 0, csA, csB, csC,
                     0, i, j);
         if (mf > mf0)
         {
            mf = mf0;
            ifetch = i;
            nfetch = j;
         }
      }
   }
/*
 * See if prefetching good idea for beta=0 case
 */
   sprintf(fnam, "res/%cMM_b0", pre);
   mf0 = mmcase(fnam, pre, "JIK", ta,  tb, mb, nb, kb, mb, nb, kb,
                kb, kb, 0, mu, nu, ku, muladd, pfA, lat, 0, csA, csB, csC,
                0, ifetch, nfetch);

   sprintf(fnam, "res/%cMM_b0_pref", pre);
   mf = mmcase(fnam, pre, "JIK", ta,  tb, mb, nb, kb, mb, nb, kb,
               kb, kb, 0, mu, nu, ku, muladd, pfA, lat, 0, csA, csB, csC,
               1, ifetch, nfetch);

   *FFetch0 = (mf > mf0);
   *ifetch0 = ifetch;
   *nfetch0 = nfetch;
   fprintf(stdout, "\n\nFORCEFETCH=%d, IFETCH = %d, NFETCH = %d\n\n",
           *FFetch0, *ifetch0, *nfetch0);
}

void kucases(char pre, int muladd, int nb, int mu, int nu, int pfA, int LAT,
             int Fku, double *mfB, int *nbB, int *muB, int *nuB, int *kuB,
             int *latB)
{
   double mf;
   int ku, lat;

   if (Fku == -1 || !Fku) ku = nb;
   else ku = Fku;
   if (ku != nb) lat = GetGoodLat(muladd, nb, mu, nu, ku, LAT);
   else lat = LAT;
   mf = mms_case(pre, muladd, nb, mu, nu, ku, pfA, lat);
   if (mf > *mfB)
   {
      *mfB = mf;
      *nbB = nb;
      *muB = mu;
      *nuB = nu;
      *kuB = ku;
      *latB = lat;
   }
   if (!Fku)
   {
      lat = GetGoodLat(muladd, nb, mu, nu, 1, LAT);
      mf = mms_case(pre, muladd, nb, mu, nu, 1, pfA, lat);
      if (mf > *mfB)
      {
         *mfB = mf;
         *nbB = nb;
         *muB = mu;
         *nuB = nu;
         *kuB = 1;
         *latB = lat;
      }
   }
}



void searchmu_nu
(
   char pre,
   int nb,
   int maxreg, /* # of registers usuable at once */
   int Fku,    /* 0: try KU=1&NB, else try only ku=Fku */
   int muladd, /* 0: machine lacks multiply & accumulate inst */
   int pfA,    /* prefetch setting */
   int LAT,    /* latency */
   int NO1D,   /* unused variable */
   /* best parameters found, init on entry, possibly changed on exit */
   double *mfB, int *nbB, int *muB, int *nuB, int *kuB, int *latB
)
{
   int N, Ng, i, j;
   int *mus, *nus;
   double mf, mfG, mf0;
   double *fpls;
   printf("\nTUNING MU & NU:\n");
/*
 * For the x86, always try some 1-D cases
 */
   #ifdef TWO_OP_ASM
      kucases(pre, muladd, nb, 6, 1, pfA, LAT, Fku,
              mfB, nbB, muB, nuB, kuB, latB);
      kucases(pre, muladd, nb, 4, 1, pfA, LAT, Fku,
              mfB, nbB, muB, nuB, kuB, latB);
      #ifdef ATL_USE64BITS
         kucases(pre, muladd, nb, 8, 1, pfA, LAT, Fku,
                 mfB, nbB, muB, nuB, kuB, latB);
         kucases(pre, muladd, nb, 10, 1, pfA, LAT, Fku,
                 mfB, nbB, muB, nuB, kuB, latB);
         kucases(pre, muladd, nb, 12, 1, pfA, LAT, Fku,
                 mfB, nbB, muB, nuB, kuB, latB);
      #endif
   #endif
/*
 * Find all possible Mu/Nu combos.  Only the first Ng of these should
 * be competitive performance-wise.
 */
   GetMuNus(maxreg, muladd, LAT, &Ng, &N, &mus, &nus, &fpls);
/*
 * If we get roughly as good a performance out of using only half the
 * registers available, the compiler is so screwing things up that we'd
 * better search the entire space
 */
   i = GetSafeGoodMuNu(maxreg, muladd, LAT, N, mus, nus, fpls);
   j = GetSafeGoodMuNu(maxreg/2, muladd, LAT, N, mus, nus, fpls);
   mfG = mms_case(pre, muladd, nb, mus[i], nus[i], nb, pfA, LAT);
   mf  = mms_case(pre, muladd, nb, mus[i], nus[i], 1, pfA,
                  GetGoodLat(muladd, nb, mus[i], nus[i], 1, LAT));
   mfG = Mmax(mfG, mf);
   mf0 = mms_case(pre, muladd, nb, mus[j], nus[j], nb, pfA, LAT);
   mf  = mms_case(pre, muladd, nb, mus[j], nus[j], 1, pfA,
                  GetGoodLat(muladd, nb, mus[i], nus[i], 1, LAT));
   mf = Mmax(mf0, mf);
   if (mf*1.02 >= mfG)
   {
      fprintf(stderr, "%d:USING HALF THE REGISTERS GETS YOU %.2f, FULL=%.2f!\n",
              __LINE__, mf, mfG);
      if (maxreg >= 64 && mf > mfG)
      {
         if (mf > mfG)
         {
            free(mus);
            free(nus);
            free(fpls);
            i = (maxreg+maxreg)/3;
            assert(i);
            fprintf(stderr, "   %d:CHANGING #REGS FROM %d TO %d!!\n",
                    __LINE__, maxreg, i);
            searchmu_nu(pre, nb, i, Fku, muladd, pfA, LAT, NO1D,
                        mfB, nbB, muB, nuB, kuB, latB);
            return;
         }
      }
      else
      {
         fprintf(stderr, "  %d:DOING FULL MU/NU SEARCH!\n", __LINE__);
         Ng = N;
      }
   }
   for (i=0; i < Ng; i++)
      kucases(pre, muladd, nb, Mmin(nb,mus[i]), Mmin(nb,nus[i]), pfA, LAT, Fku,
              mfB, nbB, muB, nuB, kuB, latB);
   free(mus);
   free(nus);
   free(fpls);
   printf("BEST MU=%d, NU=%d\n", *muB, *nuB);
}

void FindKU(char pre, int muladd, int pfA, int LAT, int nb, int mu, int nu,
            double *mfB, int *kuB, int *latB)
/*
 * For best case, try various ku's
 */
{
   int k, lat, size, linesize;
   double mf;

   fprintf(stdout, "\nCONFIRMING K-LOOP UNROLLING FOR CHOSEN NB:\n");
   printf("   pre=%c, nb=%d, mu=%d, nu=%d, muladd=%d, pfA=%d (%.2f)\n\n",
          pre, nb, mu, nu, muladd, pfA, *mfB);
   mf = mms_case(pre, muladd, nb, mu, nu, nb, pfA, LAT);
   if (mf > *mfB)
   {
      *kuB = nb;
      *mfB = mf;
      *latB = LAT;
   }
/*
 * Try 2, 4, 6, 8
 */
   for (k=2; k < 8; k += 2)
   {
      lat = GetGoodLat(muladd, nb, mu, nu, k, *latB);
      mf = mms_case(pre, muladd, nb, mu, nu, k, pfA, lat);
      if (mf > *mfB)
      {
         *latB = lat;
         *kuB = k;
         *mfB = mf;
      }
   }
/*
 * Try all unrollings between cache-line size and nb/2, in multiples of
 * the cacheline size
 */
   if (pre == 's' || pre == 'c')
      linesize = ATL_L1LS / sizeof(float);
   else
      linesize = ATL_L1LS / sizeof(double);
   if (linesize < 4) linesize = 4;
   for (k=linesize; k < nb; k += linesize)
   {
      if (k >= nb/2) k = nb;
      lat = GetGoodLat(muladd, nb, mu, nu, k, *latB);
      mf = mms_case(pre, muladd, nb, mu, nu, k, pfA, lat);
      if (mf > *mfB)
      {
         *latB = lat;
         *kuB = k;
         *mfB = mf;
      }
   }
   printf("K-LOOP UNROLLING SELECTED: %d (%.2f)\n\n", *kuB, *mfB);
}

void FindLAT(char pre, int pfA, int maxlat, int nb, int muladd,
             int mu, int nu, int ku, double *mfB, int *latB)

{
   int i, lat;
   double mf;
/*
 * Right now, search does not do accumulator expansion for small (mu, nu),
 * so there is no need to search for MAC
 */
   if (muladd) return;

   fprintf(stderr, "\nCONFIRMING LATENCY FACTOR FOR CHOSEN PARAMETERS:\n");
   for (i=1; i <= maxlat; i++)
   {
      lat = GetGoodLat(muladd, nb, mu, nu, ku, i);
      if (lat == i)
      {
         mf = mms_case(pre, muladd, nb, mu, nu, ku, pfA, lat);
         if (mf > *mfB)
         {
            *mfB = mf;
            *latB = i;
         }
      }
   }
   printf("\n\n   BEST LATENCY FACTOR=%d, (%.2f)\n\n", *latB, *mfB);
}

int ProbeLatency(char pre, int nr, int nb, int muladd, int mu, int nu)
/*
 * Finds a good setting for latency, assuming mu*nu > pipeline.  Uses
 * the minimum latency that gets good performance.  If latency has no
 * real affect on performance, returns latency of 1 (to avoid multiple probs
 * and minimize register waste)
 */
{
   int maxlat, i;
   double mf0, mf;

   if (muladd) return(1);
   maxlat = Mmin(nr/2, 16);
/*
 * Try latencies between 1 and 16, stopping anytime performance does not
 * improve; always unroll loop all way to help avoid having compiler pipeline
 * and so that all lats can be tried
 */
   fprintf(stderr, "\nPROBING FOR A GOOD LATENCY VALUE:\n");
   mf0 = mms_caseIC(pre, 0, nb, mu, nu, nb, 0, 1);
   fprintf(stderr, "   lat = %d, mf=%.2f\n", 1, mf0);
   for (i=2; i <= maxlat; i++)
   {
      mf = mms_caseIC(pre, 0, nb, mu, nu, nb, 0, 1);
      fprintf(stderr, "   lat = %d, mf=%.2f\n", i, mf);
      if (mf < mf0*1.01) /* w/o 1% improvement, latency not worth increasing */
         break;
      mf0 = mf;
   }
   i--;
   fprintf(stderr, "LATENCY %d (%.2f) SELECTED!\n", i, mf);
   return(i);
}

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

static int GuessSmallNB(char pre, int L1Size, int mu, int nu)
/*
 * Returns a small nb useful for in-cache timings
 */
{
   int imult, nb, size;

   size = (pre == 'd' || pre == 'z') ? ATL_dsize : ATL_ssize;
   L1Size /= size;
   imult = Mylcm(mu, nu);
/*
 * Try to get a block factor where A, B & C all fit into cache
 */
   for (nb=imult; 3*nb*nb < L1Size; nb += imult);
   nb -= imult;
/*
 * If block to small, settle for fitting one block comfortably in cache
 */
   if (nb < 28)
   {
      for (; nb*nb+(mu+nu)*nb*2 < L1Size; nb += imult);
      nb -= imult;
   }
   fprintf(stderr, "L1Size=%d, pre=%c, Smallnb=%d\n", L1Size, pre, nb);
   assert(nb);
   return(nb);
}

void ProbeFPU(char pre, int L1Size, int nreg, int *muladd0, int *lat0)
/*
 * Estimates good muladd and latency for matmul
 */
{
   double mf0, mf1;
   int i, mu, nu, muladd_r, lat_r, nb, imult, lat;
   char upre=pre;
   void GetMulAdd(char pre, int *MULADD, int *lat);

   if (pre == 'c') upre = 's';
   else if (pre == 'z') upre = 'd';
/*
 * Get muladd & latency for register-to-register code
 */
   GetMulAdd(upre, &muladd_r, &lat_r);
   FindMUNU(0, lat_r, (nreg > 16) ? nreg-2 : nreg, 0, &mu, &nu);
/*
 * Find good nb to use with these parameters
 */
   nb = GuessSmallNB(pre, L1Size, mu, nu);
/*
 * Compute best latency setting for separate multiply and add
 */
   lat = ProbeLatency(pre, nreg, nb, 0, mu, nu);
/*
 * Get mu,nu and nb to use with real matmul-detected latency
 */
   FindMUNU(0, lat, (nreg > 16) ? nreg-2 : nreg, 0, &mu, &nu);
   nb = GuessSmallNB(pre, L1Size, mu, nu);
/*
 * Time separate and combined mul/add.
 * NOTE: may slightly disadvantage muladd=1 case, as it uses the mu,nu set
 * with muladd=0 (using lat extra regs), but this gives us the same blocking
 * factor.  To offset this, require mf0 by 3% better to avoid using muladd=1
 */
   mf0 = mms_caseIC(pre, 0, nb, mu, nu, nb, 0, lat);
   mf1 = mms_caseIC(pre, 1, nb, mu, nu, nb, 0, lat);
   if (mf0 >= 1.03*mf1)
   {
      *muladd0 = 0;
      *lat0 = lat;
   }
   else
   {
      *muladd0 = 1;
      *lat0 = lat_r;
   }
   fprintf(stdout, "\n\nMATMUL FPU PROBE RESULTS: muladd=%d, lat=%d (%.2f) selected over (%.2f)!!\n",
           *muladd0, *lat0,
           *muladd0 == 0 ? mf0 : mf1, *muladd0 == 0 ? mf1 : mf0);
}

int GetSelectedNB(char pre)
/*
 * Returns ATLAS's NB.  This will be the best performing of the external
 * searches ran by ummsearch, and the generated files controlled here.
 */
{
   ATL_mmnode_t *mmp;
   int nb;

   mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
   assert(mmp);
   nb = mmp->next ? mmp->next->nbB : mmp->nbB;
   KillAllMMNodes(mmp);
   return(nb);
}
int GetNO1D(char pre, int nreg, int nb, int MULADD, int pfA, int LAT)
{
   int lat, NO1D=0;
   double mf0, mf1, mf;

/*
 * Always do 1-D cases for 2-op assembler!
 */
   #ifdef TWO_OP_ASM
      return(0);
   #endif
   if (pre == 'z') pre = 'd';
   else if (pre == 'c') pre = 's';

   lat = GetGoodLat(MULADD, nb, 3, 3, 1, LAT);
   if (nreg >= 15+(!MULADD)*Mmax(LAT,lat))
   {
      mf0 = mms_case(pre, MULADD, nb, 3, 3, 1, pfA, lat);
      mf1 = mms_case(pre, MULADD, nb, 3, 3, nb, pfA, LAT);
      mf = Mmax(mf1, mf0);
      mf0 = mms_case(pre, MULADD, nb, 9, 1, 1, pfA, lat);
      if (mf0 > mf) NO1D = 0;
      else if (mms_case(pre, MULADD, nb, 9, 1, nb, pfA, LAT) > mf) NO1D = 0;
      else if (mms_case(pre, MULADD, nb, 1, 9, nb, pfA, LAT) > mf) NO1D = 0;
      else if (mms_case(pre, MULADD, nb, 1, 9, 1, pfA, lat) > mf) NO1D = 0;
      else NO1D = 1;
   }
   return(NO1D);
}


double SearchNBs(char pre,      /* s, d */
                 int MA,        /* muladd */
                 int Fku,/* =0, try both ku=1 and ku=KB, else try only ku=Fku */
                 int nNBs,      /* # of NBs in NB array */
                 int *NBs,      /* array of NBs to search */
                 int mu,        /* M-loop unrolling to use */
                 int nu,        /* N-loop unrolling to use */
                 int pfA,       /* 0: no prefetch of A */
                 int lat,       /* approx latency to enforce */
                 int *latBo,    /* latency used by best kernel */
                 int *kuBo)     /* KU used by best kernel */
/*
 * RETURNS: Mflop of case that got the best performance (best NB is returned
 *          in first position of NBs array).
 */
{
   double mf, mfB;
   int i, j, k, tlat, nb, ku, kuB, latB;

   printf("\nFINDING BEST NB\n\n");
   if (Fku < 0) Fku = 0;
   tlat = lat; kuB = Fku; latB = lat;
   i = 0; mfB = 0.0;
   for (k=0; k != nNBs; k++)
   {
      nb = NBs[k];
      ku = Fku ? Fku : nb;
      mf = mms_case(pre, MA, nb, mu, nu, ku, pfA, lat);
      if (mf > mfB)
      {
         mfB = mf;
         kuB = ku;
         latB = lat;
         i = k;
      }
      if (Fku == 0)  /* try no K-loop unrolling */
      {
         tlat = GetGoodLat(MA, nb, mu, nu, 1, lat);
         mf = mms_case(pre, MA, nb, mu, nu, 1, pfA, tlat);
         if (mf > mfB)
         {
            mfB = mf;
            kuB = 1;
            latB = tlat;
            i = k;
         }
      }
   }
/*
 * Put best-performing NB first in NB array
 */
   if (i)
   {
      j = NBs[i];
      NBs[i] = NBs[0];
      NBs[0] = j;
   }
   printf("NB SEARCH DONE; NB=%d selected (%.2f).\n\n", NBs[0], mfB);
   *kuBo = kuB;
   *latBo = latB;
   return(mfB);
}

void gmmsearch(char pre, int MULADD, int Fku, int nNBs, int *NBs, int nreg,
               int LAT, int Fnb)
/*
 * Does real generated mmsearch
 */
{
   int latB, muB, nuB, kuB, nbB;
   int i, j, k, NB, ku, nb, lat=LAT, nNB=nNBs, NO1D=0;
   int FFetch, ifetch, nfetch, muladd, pfA;
   int ldbot=1;
   double mf, mfB, mf1;
   char ln[32];
   FILE *fp;

   printf("LINE %d of %s\n", __LINE__, __FILE__);
   sprintf(ln, "res/%cgMMRES", pre);
   if (FileExists(ln)) /* already have needed result */
   {
      GetInstLogFile(ln, pre, &muladd, &pfA, &lat, &nb, &muB, &nuB, &kuB,
                     &FFetch, &ifetch, &nfetch, &mf);
      if (mf <= 0.0)
      {
         mf = mmcase(NULL, pre, "JIK", 'T', 'N', nb, nb, nb, nb, nb, nb,
                     nb, nb, 0, muB, nuB, kuB, muladd, pfA, lat, 1, 1, 1, 2,
                     FFetch, ifetch, nfetch);
         PutInstLogFile1(ln, pre, muladd, pfA, lat, nb, muB, nuB, kuB, FFetch,
                         ifetch, nfetch, mf);
      }
      return;
   }
/*
 * Try not to tempt fate by using all registers
 */
   if (nreg > 16) i = nreg-2;
   else i = nreg;
   FindMUNU(MULADD, lat, i, 0, &muB, &nuB);
/*
 * First, find a good NB, with no prefetch
 */
   pfA = 0;
   mfB = SearchNBs(pre, MULADD,  Fku, nNBs, NBs, muB, nuB, (ldbot<<9),
                   lat, &latB, &kuB);
   nbB = NBs[0];

/*
 * Now, find if prefetching helps this kernel; may want to user different
 * NB if prefetch is a win (esp., smaller NB)
 */
   mf  = SearchNBs(pre, MULADD,  Fku, nNBs, NBs, muB, nuB, 1|(ldbot<<9),
                   lat, &lat, &ku);
   nbB = NBs[0];
   if (mf > mfB)
   {
      fprintf(stdout, "\nPrefetch kernel %.2f faster.\n", mf/mfB);
      mfB = mf;
      latB = lat;
      kuB = ku;
      pfA = 1;
   }
   else fprintf(stdout, "\nNon-prefetch kernel %.2f faster.\n", mfB/mf);
/*
 * With whatever the prefetch setting, try reversing load-C-at-top/bottom
 * After this, load-C setting stuck in pfA; give bottom preference due error
 */
   mf = mms_case(pre, MULADD, NBs[0], muB, nuB, kuB, pfA|((!ldbot)<<9), lat);
   if (ldbot? mf > mfB*1.005: mf*1.005 > mfB)
   {
      fprintf(stdout, "\nLoad-C-at-%s kernel %.2f faster.\n",
              (!ldbot)?"bottom":"top", mf/mfB);
      pfA |= (!ldbot)<<9;
   }
   else
   {
      fprintf(stdout, "\nLoad-C-at-%s kernel %.2f faster.\n",
              ldbot?"bottom":"top", mfB/mf);
      pfA |= ldbot<<9;
   }

   if (!Fnb) nNB = 1;
   if (MULADD)
      fprintf(stdout, "\nCombined multiply add, latency factor=%d, NB=%d ku=%d, chosen; initial MFLOP=%f.  Beginning unroll search:\n", latB, NBs[0], kuB, mfB);
   else
      fprintf(stdout, "\nSeparate multiply and add, latency factor=%d, NB=%d ku=%d, chosen; initial MFLOP=%f.  Beginning unroll search:\n", latB, NBs[0], kuB, mfB);

   NO1D = GetNO1D(pre, nreg, NBs[0], MULADD, pfA, LAT);
   if (NO1D) fprintf(stdout, "\n\nSkipping most 1D cases\n\n");
   else fprintf(stdout, "\n\nTiming 1D cases\n\n");
   for (k=0; k != nNB; k++)
   {
      NB = NBs[k];
      searchmu_nu(pre, NB, nreg, Fku, MULADD, pfA, LAT, NO1D,
                  &mfB, &nbB, &muB, &nuB, &kuB, &latB);
   }
   fprintf(stdout, "\n\nBest case so far: nb=%d, mu=%d, nu=%d, ku=%d, lat=%d; MFLOPS=%.2f.\n",
           nbB, muB, nuB, kuB, latB, mfB);
   fprintf(stdout, "Trying various other NB and KU settings:\n\n");
/*
 * If we haven't checked all permutations, try other blocking factors
 */
   nb = nbB;
   if (!Fnb)
   {
      if (nNBs > 1) fprintf(stdout, "Trying various blocking factors:\n");
      mf = mms_case(pre, MULADD, NBs[0], muB, nuB, kuB, pfA, latB);
      for (k=0; k < nNBs; k++)
      {
         NB = NBs[k];
         if (Fku == -1) ku = NB;
         else if (Fku) ku = Fku;
         else if (kuB == nbB) ku = NB;
         else ku = kuB;
         if (ku != NB) lat = GetGoodLat(MULADD, NB, muB, nuB, ku, latB);
         else lat = latB;
         mf = mms_case(pre, MULADD, NB, muB, nuB, ku, pfA, lat);
         if (mf > mfB)
         {
            kuB = ku;
            mfB = mf;
            nbB = NB;
            latB = lat;
         }
      }
   }
   if (nb != nbB) fprintf(stdout, "\nNew block factor of %d chosen!!\n\n", nbB);
   NB = nbB;

/*
 * Try all ku's, and then valid latencies
 */
   FindKU(pre, MULADD, pfA, LAT, nbB, muB, nuB, &mfB, &kuB, &latB);
   FindLAT(pre, pfA, MAXLAT, nbB, MULADD, muB, nuB, kuB, &mfB, &latB);

/*
 * Make sure MULADD is correct
 */
   lat = GetGoodLat(!MULADD, nbB, muB, nuB, kuB, latB);
   mf = mms_case(pre, !MULADD, nbB, muB, nuB, kuB, pfA, lat);
   if (mf > mfB*1.02)
   {
      fprintf(stdout, "\n\nMULADD MAY BE WRONG!!, old=%f, new=%f\n", mfB, mf);
      MULADD = !MULADD;
      latB = lat;
      mfB = mf;
   }
/*
 * See if swapping prefetch helps now, bust pfA into ld/pf again
 */
   ldbot = pfA & 512;
   pfA = pfA ^ ldbot;
   mf = mms_case(pre, MULADD, nbB, muB, nuB, kuB, (!pfA)|ldbot, lat);
   if (mf > mfB*1.01)
   {
      fprintf(stdout, "\n\nPREFETCH SWAPPED TO %d\n\n", pfA);
      pfA = !pfA;
      mfB = mf;
   }
/*
 * Try changing load at of C from top to bottom or vice versa; give
 * load-at-bottom benefit of doubt, since it is the least error-prone
 */
   ldbot >>= 9;
   mf = mms_case(pre, MULADD, nbB, muB, nuB, kuB, pfA|((!ldbot)<<9), lat);
   if (ldbot ? mf > mfB*1.01 : mf*1.01 > mfB)
   {
      ldbot = !ldbot;
      fprintf(stdout, "\n\nSWAPPED TO LOAD-AT-%s\n\n", ldbot?"BOTTOM":"TOP");
      mfB = mf;
   }
   pfA = pfA | (ldbot<<9);

/*
 * Try various fetch patterns
 */
   FindFetch('T', 'N', pre, nbB, nbB, nbB, muB, nuB, kuB, MULADD, pfA, latB,
             &FFetch, &ifetch, &nfetch);
   fprintf(stdout,
   "BEST GENERATED CASE: nb=%d, ma=%d, lat=%d mu=%d, nu=%d, ku=%d -- %.2f\n",
           nbB, MULADD, latB, muB, nuB, kuB, mfB);
   sprintf(ln, "res/%cgMMRES", pre);
   PutInstLogFile1(ln, pre, MULADD, pfA, latB, nbB, muB, nuB, kuB,
                   FFetch, ifetch, nfetch, mfB);
   printf("LINE %d of %s\n", __LINE__, __FILE__);
}


void TimeAllKernels
(
   char pre,            /* type/precision prefix */
   int itime,           /* which entry in mflop to overwrite */
   int FORCE,           /* 1: do not accept answer from pre-existing files */
   ATL_mmnode_t *mmb    /* base ptr to list matmul nodes */
)
/*
 * Times all kernels in mmb list
 */
{
   ATL_mmnode_t *mp;
   for (mp = mmb; mp; mp = mp->next)
   {
      if (mp->ID <= 0)  /* generated kernel */
      {
         if (FORCE)
            assert(!system("rm -f res/GENOUT\n"));

         mp->mflop[itime] = mmcase(FORCE ? "res/GENOUT" : NULL, pre,
            FLAG_IS_SET(mp->flag, MMF_AOUTER) ? "IJK" : "JIK",
            mp->TA == AtlasTrans ? 'T' : 'N', mp->TB == AtlasTrans ? 'T' : 'N',
            mp->mbB, mp->nbB, mp->kbB, mp->mbB, mp->nbB, mp->kbB,
            mp->kbB, mp->kbB, 0, mp->mu, mp->nu, mp->ku, mp->muladd, mp->pref,
            mp->lat, 1, 1, 1, (pre == 'c' || pre == 'z') ? 2 : 1,
            mp->fftch, mp->iftch, mp->nftch);
      }
      else
         mp->mflop[0] = TimeMMKernel(1, FORCE, mp, pre, mp->mbB, mp->nbB,
                                     mp->kbB, 0, 0, 0, 1, 0, 0);
   }
}

void CreateFinalSumFile
(
   char pre,
   ATL_mmnode_t *mmG,  /* best emit_mm.c generated file */
   ATL_mmnode_t *mmU   /* NULL or best user-contributed (external) file */
)
{
   char ln[64];
   FILE *fpout;

   sprintf(ln, "res/%cMMRES.sum", pre);
   fpout = fopen(ln, "w");
   assert(fpout);
   if (mmU && mmU->mflop[0] > mmG->mflop[0]*1.03)
   {
      fprintf(fpout,
      "#\n# Best generated case that matches the selected kernel's NB\n#\n");
      PrintMMLine(fpout, mmG);
      fprintf(fpout,
              "#\n# Selected main kernel (user-contributed/external)\n#\n");
      PrintMMLine(fpout, mmU);
   }
   else
   {
      fprintf(fpout,
         "#\n# No user-genned case selected, best from emit_mm.c below.\n#\n");
      PrintMMLine(fpout, mmG);
   }
   fclose(fpout);
}

ATL_mmnode_t *GetOrTransMMRES(char pre)
/*
 * Reads pre-existing file <pre>MMRES.sum, or creates it by translating
 * old-style <pre>MMRES.  If neither file exists, returns NULL
 * NOTE: does not read user-contrib case from this file, since that has
 *       been superceded by <pre>eMMRES.sum
 * RETURNS: list of two kernels.  First is generated kernel from MMRES.sum or
 *          MMRES, 2nd is best user-contrib from MMRES.sum or eMMRES.sum.
 */
{
   char ln[1024];
   ATL_mmnode_t *mmb;

   mmb = ReadMMFileWithPath(pre, "res", "MMRES.sum");
   if (!mmb)
   {
      sprintf(ln, "make res/%cMMRES.sum pre=%c\n", pre, pre);
      assert(system(ln) == 0);
      mmb = ReadMMFileWithPath(pre, "res", "MMRES.sum");
   }
   return(mmb);
}

ATL_mmnode_t *OldLogFile2MMNode(char pre, char *path, char *fnam)
{
   ATL_mmnode_t *mmp;
   char ln[1024];

   sprintf(ln, "%s/%c%s", path, pre, fnam);
   if (!FileExists(ln))
      return(NULL);
   mmp = GetMMNode();
   GetInstLogFile(ln, pre, &mmp->muladd, &mmp->pref, &mmp->lat, &mmp->nbB,
                  &mmp->mu, &mmp->nu, &mmp->ku,
                  &mmp->fftch, &mmp->iftch, &mmp->nftch, mmp->mflop);
   mmp->mbB = mmp->kbB = mmp->nbB;
   return(mmp);
}


void mmsearch(char pre, int MULADD, int Fku, int nNBs, int *NBs, int nreg,
              int LAT, int Fnb)
{
   int latB, muB, nuB, kuB, nbB;
   int muladd, nb, ifetch, nfetch, FFetch;
   int i, j, k, NB, pfA;
   int NO1D;
   int umb, unb, ukb, ma;
   double mfB, gmf;
   char fnam[128];
   FILE *fp;
   ATL_mmnode_t *mmU, *mmG;

   mmG = GetOrTransMMRES(pre);
   if (mmG) /* already have result */
   {
      nb = mmG->nbB;
      mmU = mmG->next;
      sprintf(fnam, "make RunUMMSearch pre=%c nb=%d", pre, nb);
      assert(!system(fnam));            /* update user & external searches */
      if (mmG->mflop[0] <= 0.0)
      {
         gmmsearch(pre, mmG->muladd, Fku, nNBs, NBs, nreg, mmG->lat, Fnb);
         TimeAllKernels(pre, 0, 0, mmG);
         CreateFinalSumFile(pre, mmG, mmU);
      }
      if (mmU && mmU->nbB != mmG->nbB)
         goto RECONCILE;
      nbB = mmU ? mmU->nbB : mmG->nbB;
      sprintf(fnam, "res/%cNB", pre);
      fp = fopen(fnam, "w");
      fprintf(fp, "%d\n%d\n", 1, nbB);
      fclose(fp);
      KillAllMMNodes(mmG);
      return;
   }
   gmmsearch(pre, MULADD, Fku, nNBs, NBs, nreg, LAT, Fnb);
   mmG = OldLogFile2MMNode(pre, "res",  "gMMRES");
   nbB = mmG->nbB;
   pfA = mmG->pref;
   muB = mmG->mu; nuB = mmG->nu; kuB = mmG->ku;
   gmf = mfB = mmG->mflop[0];
   mmU = ReadMMFileWithPath(pre, "res", "eMMRES.sum");
   if (!mmU)
   {
      printf("\nRUNNING EXTERNAL SEARCHES, PRE='%c', NB=%d:\n", pre, nbB);
      sprintf(fnam, "make RunUMMSearch pre=%c n=%d", pre, nbB);
      assert(!system(fnam));
      mmU = ReadMMFileWithPath(pre, "res", "eMMRES.sum");
      assert(mmU);
   }
RECONCILE:
   nbB = mmG->nbB;
   pfA = mmG->pref;
   muB = mmG->mu; nuB = mmG->nu; kuB = mmG->ku;
   gmf = mfB = mmG->mflop[0];

   printf("\nREAD IN BEST CONTRIBUTED KERNEL (ID=%d) AS:\n", mmU->ID);
   PrintMMLine(stdout, mmU);
   printf("\n");
/*
 * If user-written kernel is noticably better than generated, use it;
 * if it changes NB, we will need to retune the generated case to match new NB!
 */
   if (mmU->mflop[0] > 1.03*gmf)
   {
      nb = mmU->nbB;
      if (nb != nbB)
      {
         sprintf(fnam, "rm -f res/%cuClean* res/%cClean*", pre, pre);
         system(fnam);  /* if NB changed, must redu Cleanup searches! */
         if (kuB == nbB) kuB = nb;
         nbB = nb;
         if (nb % muB || nb % nuB)
         {
            NO1D = GetNO1D(pre, nreg, nb, MULADD, pfA, LAT);
            searchmu_nu(pre, nb, nreg, Fku, MULADD, pfA, LAT, NO1D,
                        &mfB, &nb, &muB, &nuB, &kuB, &mmG->lat);
         }
         FindKU(pre, MULADD, pfA, LAT, nbB, muB, nuB, &mfB, &kuB, &mmG->lat);
         FindLAT(pre, pfA, MAXLAT, nbB, MULADD, muB, nuB, kuB, &mfB, &mmG->lat);
         FindFetch('T', 'N', pre, nbB, nbB, nbB, muB, nuB, kuB, MULADD, pfA,
                   mmG->lat, &mmG->fftch, &mmG->iftch, &mmG->nftch);
         mmG->mu = muB; mmG->nu = nuB; mmG->ku = kuB;
         mmG->mflop[0] = mfB;
         mmG->nbB = mmG->mbB = mmG->kbB = nbB;
      }
   }
   else
   {
      KillAllMMNodes(mmU);
      mmU = NULL;
   }
/*
 * Save NB we've found
 */
   sprintf(fnam, "res/%cNB", pre);
   fp = fopen(fnam, "w");
   fprintf(fp, "%d\n%d\n", 1, nbB);
   fclose(fp);
/*
 * Save best case parameters we have found
 */
   CreateFinalSumFile(pre, mmG, mmU);
}

void FindNC_0(char ta, char tb, char pre, int N, int mb, int nb, int kb,
              int mu, int nu, int ku, int muladd, int pfA, int lat,
              int FFetch, int ifetch, int nfetch)
{
   int kuB=ku, latB=lat, lat0=lat, kb0=kb;
   int i, j, k, csA=1, csB=1, csC=1, kmax;
   double mf0, mf;
   char fnam[128];
   FILE *fp;

   sprintf(fnam, "res/%cbest%c%c_%dx%dx%d", pre, ta, tb, mb, nb, kb);
   if (FileExists(fnam)) /* default already exists */
   {
      GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                     &FFetch, &ifetch, &nfetch, &mf);
      if (mf < 0.0) /* need to retime */
      {
         mf = mmcase(NULL, pre, "JIK", ta, tb, nb, nb, nb,
                     nb, nb, nb, 0, 0, 0, mu, nu, ku, muladd, pfA, lat, 1,
                     1, 1, csC, FFetch, ifetch, nfetch);
         PutInstLogFile1(fnam, pre, muladd, pfA, lat, nb, mu, nu, ku,
                         FFetch, ifetch, nfetch, mf);
      }
      return;
   }
   if (pre == 'c' || pre == 'z') csA = csB = csC = 2;
   assert(N > 0);
   if (kb == 0)
   {
      kb0 = 100000;
      if ((mb*nb)/lat != lat) lat0 = GetGoodLat(muladd, kb0, mu, nu, 1, lat);
   }
   k = 1024 / (mu*nu);
   for (kmax=4; kmax*kmax < k; kmax += 4);
   if (pre == 'd' || pre == 's') kmax *= 2;
   if (kmax >= N) kmax = N;
   else if (kmax > N/2) kmax = N/2;
   if (kb == 0) kuB = k = Mmin(ku,kmax);
   else k = ku;
/*
 * Find best non-cleanup case
 */
   mf0 = mmcase(NULL, pre, "JIK", ta, tb, N, N, N, mb, nb, kb, 0, 0, 0,
                mu, nu, k, muladd, pfA, lat0, 1, csA, csB, csC,
                FFetch, ifetch, nfetch);
   latB = lat0;
/*
 * If kb is not known, try all available K unrollings; for large mu*nu*N
 * combinations, don't try maximal unrollings in order to avoid having
 * the compiler run out of space trying to optimize
 */
   if (kb == 0)
   {
      for (k=1; k < kmax; k += 4)
      {
         if (k == 5) k = 4;
         if (k > N/2) k = kmax;
         j = k;
         if (kb == 0) j = 1;
         i = GetGoodLat(muladd, kb0, mu, nu, j, lat);
         mf = mmcase(NULL, pre, "JIK", ta, tb, N, N, N, mb, nb, kb, 0, 0, 0,
                     mu, nu, k, muladd, pfA, i, 1, csA, csB, csC,
                     FFetch, ifetch, nfetch);
         if (mf > mf0)
         {
            mf0 = mf;
            kuB = k;
            latB = i;
         }
      }
   }
/*
 * If K is known, try only the most common unrollings
 */
   else
   {
      i = GetGoodLat(muladd, kb0, mu, nu, 1, lat);
      mf = mmcase(NULL, pre, "JIK", ta, tb, N, N, N, mb, nb, kb, 0, 0, 0,
                  mu, nu, 1, muladd, pfA, i, 1, csA, csB, csC,
                  FFetch, ifetch, nfetch);
      if (mf > mf0)
      {
         mf0 = mf;
         kuB = 1;
         latB = i;
      }
      i = GetGoodLat(muladd, kb0, mu, nu, 4, lat);
      mf = mmcase(NULL, pre, "JIK", ta, tb, N, N, N, mb, nb, kb, 0, 0, 0,
                  mu, nu, 4, muladd, pfA, i, 1, csA, csB, csC,
                  FFetch, ifetch, nfetch);
      if (mf > mf0)
      {
         mf0 = mf;
         kuB = 4;
         latB = i;
      }
      mf = mmcase(NULL, pre, "JIK", ta, tb, N, N, N, mb, nb, kb, 0, 0, 0,
                  mu, nu, kb, muladd, pfA, lat, 1, csA, csB, csC,
                  FFetch, ifetch, nfetch);
      if (mf > mf0)
      {
         mf0 = mf;
         kuB = kb;
         latB = lat;
      }
   }
/*
 * Try various latencies
 */
   if (kb) i = kuB;
   else i = 1;
   for (k=2; k < 9; k++)
   {
      if (((mu*nu*i)/k)*k == mu*nu*i)
      {
         mf = mmcase(NULL, pre, "JIK", ta, tb, N, N, N, mb, nb, kb, 0, 0, 0,
                     mu, nu, kuB, muladd, pfA, k, 1, csA, csB, csC,
                     FFetch, ifetch, nfetch);
         if (mf > mf0)
         {
            mf0 = mf;
            latB = k;
         }
      }
   }
   fprintf(stdout, "BEST for %c%c_%dx%dx%d: mflop=%.2f\n",
           ta, tb, mb, nb, kb, mf0);
   fprintf(stdout,
           "pre=%c ta=%c tb=%c nb=%d mu=%d nu=%d ku=%d muladd=%d lat=%d\n",
           pre, ta, tb, nb, mu, nu, kuB, muladd, latB);
   sprintf(fnam, "res/%cbest%c%c_%dx%dx%d", pre, ta, tb, mb, nb, kb);
   fp = fopen(fnam, "w");
   assert(fp);
   PutInstLogFile(fp, muladd, pfA, latB, N, mu, nu, kuB,
                  FFetch, ifetch, nfetch, mf0);
   fclose(fp);
}

void FindNC0(char ta, char tb, char pre, int nb, int mu, int nu, int ku,
             int muladd, int pfA, int lat, int FFetch, int ifetch, int nfetch)
{
   FindNC_0(ta, tb, pre, nb, nb, nb, nb, mu, nu, ku, muladd, pfA, lat, FFetch,
            ifetch, nfetch);
   FindNC_0(ta, tb, pre, nb, 0, 0, nb, mu, nu, ku, muladd, pfA, lat, FFetch,
            ifetch, nfetch);
   FindNC_0(ta, tb, pre, nb, 0, 0, 0, mu, nu, ku, muladd, pfA, lat, FFetch,
            ifetch, nfetch);
}

double NCcase(char pre, int nb, int mu, int nu, int ku, int ma, int pfA,
              int lat, int ffetch, int ifetch, int nfetch)
{
   double mf;
   int ld=Mmax(1000,nb), cs=1;
   char fnam[128];

   if (pre == 'c' || pre == 'z') cs = 2;
   do
   {
      sprintf(fnam, "res/%cNCNB%d_%d", pre, nb, ld);
      mf = mmcase(fnam, pre, "JIK", 'N', 'N', nb, nb, nb, nb, nb, nb,
                  -ld, nb, nb, mu, nu, ku, ma, pfA, lat, 1, cs, cs, cs,
                  ffetch, ifetch, nfetch);
      ld -= 10;
   }
   while (mf <= 0.0 && ld >= nb);
   assert(mf > 0.0);
   return(mf);
}

int FindNoCopyNB(char pre, int nb, int mu, int nu, int ku0, int muladd,
                 int *prefA, int lat, int FFetch, int ifetch, int nfetch)
/*
 * See if a smaller blocking factor is needed for no-copy
 */
{
   char fnam[128];
   int i, ku, nbB=nb, csA=2, csB=2, csC=2, kuIsNB=0, pfA=(*prefA);
   double mf, mfB, mf0;
   const double dmul = 1.02;
   FILE *fp;

   if (ku0 == nb)
      kuIsNB = 1;
   else if (!muladd)
      lat = GetGoodLat(muladd, nb, mu, nu, ku0, lat);
   sprintf(fnam, "res/%cNCNB", pre);
   if (!FileExists(fnam))
   {
/*
 *    Check both with and w/o prefetch, since no-copy prefetch different
 */
      mfB = NCcase(pre, nb, mu, nu, ku0, muladd, pfA, lat,
                   FFetch, ifetch, nfetch);
      mf0 = NCcase(pre, nb, mu, nu, ku0, muladd, !pfA, lat,
                   FFetch, ifetch, nfetch);
      if (mf0 > mfB)
      {
         mfB = mf0;
         pfA = !pfA;
      }
      mfB *= dmul;
      mf0 = mfB;
      i = (nb>>2)<<2;
      if (i == nb)
         i -= 4;
      for (; i >= 16; i -= 4)
      {
         if (kuIsNB) ku = i;
         else ku = Mmin(i, ku0);
         mf = NCcase(pre, i, mu, nu, ku, muladd, pfA, lat,
                     FFetch, ifetch, nfetch);
         if (1.2*mf < mfB) break; /* stop search after 20% slowdown */
         if (mf > mfB)
         {
            mfB = mf;
            nbB = i;
         }
      }
/*
 *    For safety, check opposite of prefetch result wt new NB
 */
      mf = NCcase(pre, nbB, mu, nu, ku, muladd, !pfA, lat,
                  FFetch, ifetch, nfetch);
      if (mf > mfB)
      {
         mfB = mf;
         pfA = !pfA;
      }

      fp = fopen(fnam, "w");
      assert(fp);
      fprintf(fp, "%d\n", nbB);
   }
   else   /* If we know the correct NB, just try prefetch or not */
   {
      fp = fopen(fnam, "r");
      *prefA = -1;
      fscanf(fp, "%d\n", &nbB);
      mf0 = mfB = -1.0;
      ku = kuIsNB ? nbB : (Mmin(ku0,nbB));
      mfB = NCcase(pre, nbB, mu, nu, ku, muladd, pfA, lat,
                   FFetch, ifetch, nfetch);
      mf  = NCcase(pre, nbB, mu, nu, ku, muladd, !pfA, lat,
                   FFetch, ifetch, nfetch);
      if (mf > mfB)
      {
        pfA = !pfA;
        mfB = mf;
      }
   }
   fclose(fp);
   fprintf(stdout, "\n%cNB = %d (%.2f), No copy %cNB = %d (%.2f)\n\n",
           pre, nb, mf0, pre, nbB, mfB);
   *prefA = pfA;
   return(nbB);
}

void FindNoCopy(char pre)
{
   char ln[128];
   int nb, mu, nu, ku, muladd, pfA, lat, FFetch, ifetch, nfetch, i;
   double mf;
   FILE *fp;
   ATL_mmnode_t *mmp;

   mmp = ReadMMFileWithPath(pre, "res", "gMMRES.sum");
   if (!mmp)
   {
      sprintf(ln, "make res/%cMMRES.sum\n", pre);
      assert(!system(ln));
      mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
      assert(mmp);
   }
   muladd = mmp->muladd;
   lat = mmp->lat;
   pfA = mmp->pref;
   nb = mmp->next ? mmp->next->nbB : mmp->nbB;
   mu = mmp->mu; nu = mmp->nu;  ku = mmp->ku;
   FFetch = mmp->fftch; ifetch = mmp->iftch; nfetch = mmp->nftch;
   KillAllMMNodes(mmp);
   nb = FindNoCopyNB(pre, nb, mu, nu, ku, muladd, &pfA, lat,
                     FFetch, ifetch, nfetch);
   ku = Mmin(ku, nb);
   if (!muladd && ku < nb)
      lat = GetGoodLat(muladd, nb, mu, nu, ku, lat);
   FindNC0('N', 'N', pre, nb, mu, nu, ku, muladd, pfA, lat,
           FFetch, ifetch, nfetch);
   FindNC0('N', 'T', pre, nb, mu, nu, ku, muladd, pfA, lat,
           FFetch, ifetch, nfetch);
   FindNC0('T', 'N', pre, nb, mu, nu, ku, muladd, pfA, lat,
           FFetch, ifetch, nfetch);
   FindNC0('T', 'T', pre, nb, mu, nu, ku, muladd, pfA, lat,
           FFetch, ifetch, nfetch);
}

void FindCleanupK(char pre, int nb, int mu, int nu, int ku0, int muladd,
                  int pfA, int lat0, int FFetch, int ifetch, int nfetch)
{
   char fnam[256];
   int genlat, genku, speclat, ku, kumax;
   int kb, beta, csC;
   double mf, genmf, specmf;
   int i, TimeIt=0;
   FILE *fp;

   i = 1024 / (mu*nu);
   for (kumax=4; kumax*kumax < i; kumax += 4);
   if (pre == 'd' || pre == 's') kumax *= 2;
   if (kumax >= nb) kumax = nb;
   else if (kumax > nb/2) kumax = nb/2;
   if (ifetch == -1 || nfetch == -1) { ifetch = mu+nu; nfetch = 1; }
   if (pre == 's' || pre == 'd')
   {
      csC = 1;
      beta = 1;
   }
   else
   {
      csC = 2;
      beta = 8;
   }
   sprintf(fnam, "res/%cCleanK", pre);
   if (FileExists(fnam)) /* file already there */
   {
      fp = fopen(fnam, "r");
      assert(fgets(fnam, 256, fp));
      assert(fscanf(fp, " %d", &kb) == 1);
      fclose(fp);
      if (kb > 0 && kb != nb) TimeIt = 1;
      sprintf(fnam, "res/%cCleanK", pre);
   }
   else TimeIt = 1;
   if (TimeIt)
   {
      fp = fopen(fnam, "w");
      assert(fp);
      fprintf(fp, " KB  MULADD  LAT  PREF NB  MU  NU  KU  FFTCH  IFTCH  NFTCH  GEN-MFLOP  SPC-MFLOP\n");

      for (kb = nb; kb; kb--)
      {
         ku = Mmin(ku0, kb);
         sprintf(fnam, "res/%cKB_%d", pre, kb);
         speclat = GetGoodLat(muladd, kb, mu, nu, ku, lat0);
         specmf = mmcase(fnam, pre, "JIK", 'T', 'N', nb, nb, kb, 0, 0,
                         kb, kb, kb, 0, mu, nu, ku, muladd, pfA, speclat, beta,
                         1, 1, csC, FFetch, ifetch, nfetch);

         sprintf(fnam, "res/%cKB_0_%d", pre, ku);
         genlat = GetGoodLat(muladd, 8000, mu, nu, 1, lat0);
         genku = Mmin(kumax, ku);
         genmf = mmcase(fnam,pre, "JIK", 'T', 'N', nb, nb, kb, 0, 0, 0, 0, 0, 0,
                        mu, nu, genku, muladd, pfA, genlat, beta, 1, 1, csC,
                        FFetch, ifetch, nfetch);
         if (ku != 1)  /* always try ku == 1 for general case */
         {
            sprintf(fnam, "res/%cKB_0_1", pre);
            mf = mmcase(fnam,pre, "JIK", 'T', 'N', nb, nb, kb, 0, 0, 0, 0, 0, 0,
                        mu, nu, 1, muladd, pfA, genlat, beta, 1, 1, csC,
                        FFetch, ifetch, nfetch);
            if (mf > genmf) { genku = 1; genmf = mf; }
         }
         if (1.01 * genmf > specmf) break;
         fprintf(fp,
          "%3d  %6d  %3d %4d %3d %3d %3d %3d  %5d  %5d  %5d  %9.2lf  %9.2lf\n",
                 kb, muladd, speclat, pfA, nb, mu, nu, ku, FFetch, ifetch,
                 nfetch, specmf, genmf);
         fflush(fp);
      }
      fprintf(fp,
         "%3d  %6d  %3d %4d %3d %3d %3d %3d  %5d  %5d  %5d  %9.2lf  %9.2lf\n",
              0, muladd, genlat, pfA, nb, mu, nu, genku, FFetch, ifetch,
              nfetch, specmf, genmf);
      fclose(fp);
   }
}

void FindCleanupMN(char pre, char cwh, int nb, int mu, int nu, int ku,
                   int muladd, int pfA, int lat,
                   int FFetch, int ifetch, int nfetch)
{
   char fnam[128];
   int nnb=nb, beta=1, csC=1, TimeIt=0;
   int Mb=nb, Nb=nb;
   int mu0, nu0, ku0, ma0, lat0, ff0, if0, nf0;
   double mf;
   FILE *fp;

   if (cwh == 'M') Mb = 0;
   else Nb = 0;
   if (ifetch == -1 || nfetch == -1) { ifetch = mu+nu; nfetch = 1; }
   if (pre == 'c' || pre == 'z')
   {
      beta = 8;
      csC = 2;
   }
   sprintf(fnam, "res/%cClean%c", pre, cwh);
   if (FileExists(fnam))
   {
      GetInstLogFile(fnam, pre, &ma0, &pfA, &lat0, &nnb, &mu0, &nu0, &ku0, &ff0,
                     &if0, &nf0, &mf);
      if (nnb != nb || mf <= 0.0) TimeIt = 1;
   }
   else TimeIt = 1;
   if (TimeIt)
   {
      mf = mmcase(NULL, pre, "JIK", 'T', 'N', nb, nb, nb, Mb, Nb, nb, nb, nb, 0,
                  mu, nu, ku, muladd, pfA, lat, beta, 1, 1, csC,
                  FFetch, ifetch, nfetch);
      fp = fopen(fnam, "w");
      assert(fp);
      PutInstLogFile(fp, muladd, pfA, lat, nb, mu, nu, ku,
                     FFetch, ifetch, nfetch, mf);
      fclose(fp);
   }
}

typedef struct CleanCase CLEANCASE;
struct CleanCase
{
   double mflop;
   CLEANCASE *next;
   int imult, icase, fixed, nb, nb0, nb1, nb2;
};

void PrintCleanCases(CLEANCASE *cp)
{
   for (; cp; cp = cp->next)
   {
      fprintf(stdout,
              "imult=%d, icase=%d, fixed=%d, nb=%d, %d,%d,%d, mflop=%.2f\n",
              cp->imult, cp->icase, cp->fixed, cp->nb, cp->nb0, cp->nb1,
              cp->nb2, cp->mflop);
   }
   fprintf(stdout, "\n");
}
CLEANCASE *GetUserCleanup(char pre, int nb, enum CW which)
/*
 * Read in user clean file
 */
{
   FILE *fp;
   CLEANCASE *cp, *cp0;
   int i, n;
   char cwh[3] = {'M', 'N', 'K'};
   char ln[128];

   sprintf(ln, "res/%cuClean%c", pre, cwh[which]);
   if (!FileExists(ln))
   {
      sprintf(ln, "make RunUMMClean pre=%c nb=%d which=%c\n",
              pre, nb, tolower(cwh[which]));
      assert(system(ln) == 0);
      sprintf(ln, "res/%cuClean%c", pre, cwh[which]);
   }
   fp = fopen(ln, "r");
   assert(fp);
   assert(fgets(ln, 128, fp));
   assert(fgets(ln, 128, fp));
   sscanf(ln, " %d", &n);
   if (n < 1) return(NULL);
   cp0 = cp = malloc(sizeof(CLEANCASE));
   assert(cp0);
   for (i=0; i < n; i++)
   {
      assert(fgets(ln, 128, fp));
      sscanf(ln, " %d %d %d %d %d %d %d %lf", &cp->imult, &cp->icase,&cp->fixed,
             &cp->nb, &cp->nb0, &cp->nb1, &cp->nb2, &cp->mflop);
      if (i != n-1)
      {
         cp->next = malloc(sizeof(CLEANCASE));
         assert(cp->next);
         cp = cp->next;
      }
      else cp->next = NULL;
   }
   fclose(fp);
   return(cp0);
}

int *GetKBs(char pre, int nb)
/*
 * returns nb+1 length vector, KB[i] is KB & lda of KB Cleanup; 0 means var
 */
{
   FILE *fp;
   int k, *KB;
   char ln[128];

   sprintf(ln, "res/%cCleanK", pre);
   fp = fopen(ln, "r");
   assert(fp);
   assert(fgets(ln, 128, fp));  /* skip titles */
   KB = malloc((nb+1)*sizeof(int));
   assert(KB);
   for (k=nb; k; k--)
   {
      if (fgets(ln, 128, fp)) { assert(sscanf(ln, " %d", KB+k)==1); }
      else break;
   }
   for(; k; k--) KB[k] = 0;
   return(KB);
}

double RebuttUserKCase(char pre, int nb, int mu, int nu, int ku, int ma,
                       int pfA, int lat, int FF, int iff, int nf, int *KBs,
                       int *NBs)
{
   double mf, mf0=0.0;
   int K, csC, i, ld, iku, ilat;

   if (pre == 'c' || pre == 'z') csC = 2;
   else csC = 1;

   for(i=0; i < 3 && NBs[i]; i++)
   {
      K = NBs[i];
      ld = KBs[K];
      iku = Mmin(ku, K);
      ilat = GetGoodLat(ma, K, mu, nu, 1, lat);
      mf = mmclean(pre, CleanK, "JIK", 'T', 'N', nb, nb, K,
                   nb, nb, ld, ld, ld, 0,
                   mu, nu, iku, ma, pfA, ilat, 1, 1, 1, csC, FF, iff, nf);
      fprintf(stdout, "   CleanK: %dx%dx%d : %.2f\n", nb, nb, K, mf);
      assert(mf > 0.0);
      mf0 += mf;
   }
   return(mf0 / i);
}

CLEANCASE *RebuttUserKClean(char pre, int nb, int mu, int nu, int ku,
                            int muladd, int pfA,  int lat,
                            int FF, int iff, int nf)
{
   double gmf, umf;
   int NB[3], *KBs;
   CLEANCASE *cp0, *cp;

   KBs = GetKBs(pre, nb);

   cp0 = GetUserCleanup(pre, nb, CleanK);
   for (cp=cp0; cp; cp = cp->next)
   {
      NB[0] = cp->nb0; NB[1] = cp->nb1; NB[2] = cp->nb2;
      gmf = RebuttUserKCase(pre, nb, mu, nu, ku, muladd, pfA, lat, FF, iff, nf,
                            KBs, NB);
      fprintf(stdout, "   pKBmm_%d: user=%.2f  generated=%.2f\n",
              cp->imult, cp->mflop, gmf);
      if (1.02*gmf > cp->mflop) cp->icase = -1;
   }
   free(KBs);
   return(cp0);
}

double RebuttUserCase(char pre, int nb, enum CW which, int mu, int nu, int ku,
                      int ma, int pfA, int lat, int FF, int iff, int nf,
                      int *NBs)
{
   double mf, mf0=0.0;
   int NB[3], M[3], NU[3], csC, i, j, NUmax, ilat;
   char cwh[3] = {'M', 'N', 'K'};

   if (pre == 'c' || pre == 'z') csC = 2;
   else csC = 1;
   NB[0] = NB[1] = NB[2] = M[0] = M[1] = M[2] = nb;
   NB[which] = 0;
   NU[0] = mu; NU[1] = nu; NU[2] = ku;

   NUmax = NU[which];
   for(i=0; i < 3 && NBs[i]; i++)
   {
      j = M[which] = NBs[i];
      NU[which] = Mmin(j, NUmax);
      ilat = GetGoodLat(ma, M[2], NU[0], NU[1], NU[2], lat);
      mf = mmclean(pre, which, "JIK", 'T', 'N', M[0], M[1], M[2],
                   NB[0], NB[1], NB[2], nb, nb, 0,
                   mu, nu, ku, ma, pfA, lat, 1, 1, 1, csC, FF, iff, nf);
      fprintf(stdout, "   Clean%c: %dx%dx%d : %.2f\n", cwh[which],
              M[0], M[1], M[2], mf);
      assert(mf > 0.0);
      mf0 += mf;
   }
   return(mf0 / i);
}

CLEANCASE *RebuttUserCases(char pre, int nb, enum CW which,
                           int mu, int nu, int ku, int muladd, int pfA, int lat,
                           int FF, int iff, int nf)
{
   double gmf, umf;
   int NB[3];
   CLEANCASE *cp0, *cp;
   char cwh[3] = {'M', 'N', 'K'};

   if (which == CleanK)
      return(RebuttUserKClean(pre, nb, mu, nu, ku, muladd, pfA, lat,
                              FF, iff, nf));
   cp0 = GetUserCleanup(pre, nb, which);
   for (cp=cp0; cp; cp = cp->next)
   {
      NB[0] = cp->nb0; NB[1] = cp->nb1; NB[2] = cp->nb2;
      gmf = RebuttUserCase(pre, nb, which, mu, nu, ku, muladd, pfA, lat,
                           FF, iff, nf, NB);
      fprintf(stdout, "   p%cBmm_%d: user=%.2f  generated=%.2f\n",
              cwh[which], cp->imult, cp->mflop, gmf);
      if (1.02*gmf > cp->mflop) cp->icase = -1;
   }
   return(cp0);
}

CLEANCASE *WeedOutLosers(CLEANCASE *cp0)
{
   CLEANCASE *cp, *cp1;

   while(cp0 && cp0->icase == -1)
   {
      cp = cp0->next;
      free(cp0);
      cp0 = cp;
   }
   if (cp0 && cp0->next)
   {
      for (cp=cp0; cp->next; cp = cp->next)
      {
         cp1 = cp->next;
         if (cp1->icase == -1)
         {
            cp->next = cp1->next;
            free(cp1);
            if (cp->next == NULL) break;
         }
      }
   }
   return(cp0);
}

void KillAllCleans(CLEANCASE *cp)
{
   CLEANCASE *cp1;
   while (cp)
   {
      cp1 = cp->next;
      free(cp);
      cp = cp1;
   }
}

int NumUserCleans(CLEANCASE *cp)
{
   int i;
   for (i=0; cp; cp = cp->next) if (cp->icase != -1) i++;
   return(i);
}

void FindUserCleanup(char pre, int nb, enum CW which, int mu, int nu, int ku,
                     int ma, int pfA, int lat, int FF, int iff, int nf)
{
   CLEANCASE *cp, *cp0;
   FILE *fp;
   char ln[128];
   char cwh[3] = {'M', 'N', 'K'};

   sprintf(ln, "res/%cuClean%cF", pre, cwh[which]);
   if (FileExists(ln)) return;/* already done */
   cp = RebuttUserCases(pre, nb, which, mu, nu, ku, ma, pfA, lat, FF, iff, nf);
   cp = WeedOutLosers(cp);
   fp = fopen (ln, "w");
   assert(fp);
   fprintf(fp, "MULT  ICASE  FIXED  NB\n");
   fprintf(fp, "%d\n", NumUserCleans(cp));
   for(cp0=cp; cp; cp = cp->next)
      fprintf(fp, "%4d  %5d  %5d %3d\n",
              cp->imult, cp->icase, cp->fixed, cp->nb);
   fclose(fp);
   KillAllCleans(cp0);
}

void FindAllUserClean(char pre, int nb, int mu, int nu, int ku,
                      int ma, int pfA, int lat, int FF, int iff, int nf)
{
   FindUserCleanup(pre, nb, CleanM, mu, nu, ku, ma, pfA, lat, FF, iff, nf);
   FindUserCleanup(pre, nb, CleanN, mu, nu, ku, ma, pfA, lat, FF, iff, nf);
   FindUserCleanup(pre, nb, CleanK, mu, nu, ku, ma, pfA, lat, FF, iff, nf);
}

void FindAllUserClean0(char pre)
{
   double mf;
   int nb, mu, nu, ku, muladd, lat, FF, iff, nf, pfA;
   char ln[64];
   ATL_mmnode_t *mmp;

   mmp = GetOrTransMMRES(pre);
   assert(mmp);
   FindAllUserClean(pre, mmp->next ? mmp->next->nbB : mmp->nbB,
                    mmp->mu, mmp->nu, mmp->ku, mmp->muladd, mmp->pref,
                    mmp->lat, mmp->fftch, mmp->iftch, mmp->nftch);
   KillAllMMNodes(mmp);
}

void FindCleanup(char pre, int nb, int mu, int nu, int ku, int muladd, int pfA,
                 int lat, int FFetch, int ifetch, int nfetch)
{
   int latS=lat;
   if (!muladd && ku < nb)
      latS = GetGoodLat(muladd, nb, mu, nu, ku, lat);
   FindCleanupMN(pre, 'M', nb, mu, nu, ku, muladd, pfA, latS,
                 FFetch, ifetch, nfetch);
   FindCleanupMN(pre, 'N', nb, mu, nu, ku, muladd, pfA, latS,
                 FFetch, ifetch, nfetch);
   FindCleanupK(pre, nb, mu, nu, ku, muladd, pfA, latS, FFetch, ifetch, nfetch);
   FindAllUserClean(pre, nb, mu, nu, ku, muladd, pfA, latS,
                    FFetch, ifetch, nfetch);
}
void FindAllClean(char pre)
{
   double mf;
   int nb, mu, nu, ku, muladd, lat, FF, iff, nf, pfA;
   char ln[64];
   ATL_mmnode_t *mmp;

   fprintf(stdout, "\n\nSTARTING CLEANUP SEARCH\n\n");
   mmp = GetOrTransMMRES(pre);
   assert(mmp);
   FindCleanup(pre, mmp->next ? mmp->next->nbB : mmp->nbB,
               mmp->mu, mmp->nu, mmp->ku, mmp->muladd, mmp->pref, mmp->lat,
               mmp->fftch, mmp->iftch, mmp->nftch);
   KillAllMMNodes(mmp);
   fprintf(stdout, "\n\nDONE CLEANUP SEARCH\n\n");
}

int GetNumRegs0(char pre, int muladd, int nb, int lat,
                int nr0, int nrN, int incN)
/*
 * RETURNS: nregisters before 10% drop-off in performance occured, or
 *          -1 if this drop-off did not occur
 */
{
   int n, nr, i, imax, nu, mu, pfA=0, iret=(-1), nrlast;
   double *rates, mf, mf1d, mmf=0.0;

   n = 0;
   nrlast = i = nr0;
   while (i <= nrN)
   {
      if (incN == -2) i <<= 1;
      else i += incN;
      n++;
   }
   nr = nr0;
   for (i=0; i < n; i++)
   {
      FindMUNU(muladd, lat, nr, 1, &mu, &nu);
      mf = mms_caseIC(pre, muladd, nb, mu, nu, nb, pfA, lat);
/*
 *    Try 1D case for small # regs in case we've got 2-op asg (eg, x86)
 */
      if (nr < 16)
      {
         mf1d = mms_caseIC(pre, muladd, nb, nr, 1, nb, pfA, lat);
         if (mf1d > mf)
            mf = mf1d;
      }
      if (mf > mmf)
      {
         mmf = mf;
         imax = i;
      }
/*
 *    Declare 10% drop in performance register exhaustion
 */
      else if (mf < 0.9*mmf)
      {
         iret = nrlast;
         break;
      }
      nrlast = nr;
      if (incN == -2) nr <<= 1;
      else nr += incN;
   }
   return(iret);
}

int RefineNumRegs(char pre, int muladd, int nb, int lat, int nr0, int nrN)
/*
 * recursively halves gap until true number is found
 */
{
   int i, nr;

   i = (nrN - nr0) / 2;
   if (i < 1) return(nr0);
   nr = GetNumRegs0(pre, muladd, nb, lat, nr0, nr0+i, i);
   if (nr != nr0) /* positive or no difference in two points, so go larger */
      nr0 += i;
   else          /* difference, point is between */
      nrN = nr0 + i;
   return(RefineNumRegs(pre, muladd, nb, lat, nr0, nrN));
}

int GetNumRegs00(char pre, int muladd, int nb, int lat, int maxnr)
{
   int nr, i;

   fprintf(stdout, "\n\nFINDING ROUGHLY HOW MANY REGISTERS TO USE:\n\n");

   nr = GetNumRegs0(pre, muladd, nb, lat, 4, maxnr, -2);
/*
 * Refine number of regs
 */
   if (nr != -1) i = RefineNumRegs(pre, muladd, nb, lat, nr, nr<<1);
   else i = nr;
   fprintf(stdout, "\n\nAPPROXIMATE NUMBER OF USABLE REGISTERS=%d\n\n", i);
   return(i);
}

int GetNumRegs(char pre, int MaxL1, int maxnreg)
{
   FILE *fp;
   int nreg, muladd, lat;
   char nam[32];
   void GetMulAdd(char pre, int *MULADD, int *lat);

   if (pre == 'z') pre = 'd';
   else if (pre == 'c') pre = 's';

   sprintf(nam, "res/%cnreg", pre);
   if (!FileExists(nam))
   {
      GetMulAdd(pre, &muladd, &lat);
      nreg = GetNumRegs00(pre, muladd, GuessSmallNB(pre, MaxL1*1024, 4, 3),
                          lat, maxnreg);
      fp = fopen(nam, "w");
      fprintf(fp, "%d\n", nreg);
   }
   else
   {
      fp = fopen(nam, "r");
      fscanf(fp, " %d", &nreg);
   }
   fclose(fp);
   fprintf(stdout, "mmnreg = %d\n", nreg);
   return(nreg);
}

void RunTimes(char pre)
{
   const char TR[2] = {'N', 'T'};
   char fnam[128], fnam2[128], ln[128];
   const int COMPLEX = (pre == 'c' || pre == 'z');
   int csC = (COMPLEX ? 2 : 1);
   int NB, muladd, lat, nb, mu, nu, ku, ffetch, ifetch, nfetch, ia, ib;
   int uma, ulat, unb=0, umu, unu, uku, uff, uif, unf, pfA;
   int maxreg;
   double mf, umf;
   FILE *fp;
   ATL_mmnode_t *mmp;

   fprintf(stdout, "\n\nStart RunTimes\n");
   sprintf(fnam, "res/%cgMMRES", pre);
   fp = fopen(fnam, "r");
   assert(fp);
   fgets(ln, 128, fp);
   GetInstLogLine(fp, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku, &ffetch,
                  &ifetch, &nfetch, &mf);
   fclose(fp);
   if (mf < 0.0) /* need to retime */
   {
      printf("\nRETIMING EXTERNAL KERNELS, PRE='%c':\n", pre);
      sprintf(ln, "make RunUMMSearch pre=%c nb=-1\n", pre);
      assert(!system(ln) == 0);
      mf = mmcase(NULL, pre, "JIK", 'T', 'N', nb, nb, nb, nb, nb, nb,
                  nb, nb, 0, mu, nu, ku, muladd, pfA, lat, 1, 1, 1, csC,
                  ffetch, ifetch, nfetch);
      PutInstLogFile1(fnam, pre, muladd, pfA, lat, nb, mu, nu, ku,
                      ffetch, ifetch, nfetch, mf);
      mmp = GetOrTransMMRES(pre);
      if (!mmp)
      {
         sprintf(ln, "make res/%cMMRES.sum pre=%c\n", pre, pre);
         assert(system(ln) == 0);
         mmp = GetOrTransMMRES(pre);
         assert(mmp);
      }
      if (mmp->mflop[0] <= 0.0)
      {
         TimeAllKernels(pre, 0, 0, mmp);
         CreateFinalSumFile(pre, mmp, mmp->next);
      }
   }
   sprintf(fnam, "res/%cNCNB", pre);
   if (!FileExists(fnam)) return;
   fp = fopen(fnam, "r");
   assert(fp);
   assert(fscanf(fp, " %d", &NB) == 1);
   fclose(fp);

   for (ia=0; ia < 2; ia++)
   {
      for (ib=0; ib < 2; ib++)
      {
         sprintf(fnam, "res/%cbest%c%c_%dx%dx%d", pre, TR[ia], TR[ib],
                 NB, NB, NB);
         if (FileExists(fnam))
         {
            GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                           &ffetch, &ifetch, &nfetch, &mf);
            if (mf < 0.0) /* need to retime */
            {
               mf = mmcase(NULL, pre, "JIK", TR[ia], TR[ib], nb, nb, nb,
                           nb, nb, nb, 0, 0, 0, mu, nu, ku, muladd, pfA, lat,
                           1, 1, 1, csC, ffetch, ifetch, nfetch);
               PutInstLogFile1(fnam, pre, muladd, pfA, lat, nb, mu, nu, ku,
                              ffetch, ifetch, nfetch, mf);
            }
         }
         sprintf(fnam, "res/%cbest%c%c_%dx%dx%d", pre, TR[ia], TR[ib],
                 0, 0, NB);
         if (FileExists(fnam))
         {
            GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                           &ffetch, &ifetch, &nfetch, &mf);
            if (mf < 0.0) /* need to retime */
            {
               mf = mmcase(NULL, pre, "JIK", TR[ia], TR[ib], nb, nb, nb,
                           0, 0, nb, 0, 0, 0, mu, nu, ku, muladd, pfA, lat, 1,
                           1, 1, csC, ffetch, ifetch, nfetch);
               PutInstLogFile1(fnam, pre, muladd, pfA, lat, nb, mu, nu, ku,
                              ffetch, ifetch, nfetch, mf);
            }
         }
         sprintf(fnam, "res/%cbest%c%c_%dx%dx%d", pre, TR[ia], TR[ib],
                 0, 0, 0);
         if (FileExists(fnam))
         {
            GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                           &ffetch, &ifetch, &nfetch, &mf);
            if (mf < 0.0) /* need to retime */
            {
               mf = mmcase(NULL, pre, "JIK", TR[ia], TR[ib], nb, nb, nb,
                           0, 0, 0, 0, 0, 0, mu, nu, ku, muladd, pfA, lat, 1,
                           1, 1, csC, ffetch, ifetch, nfetch);
               PutInstLogFile1(fnam, pre, muladd, pfA, lat, nb, mu, nu, ku,
                              ffetch, ifetch, nfetch, mf);
            }
         }
      }
   }
   fprintf(stdout, "\nDone  RunTimes\n\n");
}

void cmmsearch(char pre, int MULADD, int Fku, int nNBs, int *NBs, int nreg,
               int LAT, int Fnb)
/*
 * With all other parameters set by real search, find good complex NB
 */
{
   char *typ, ln[64], upre;
   int i, k, nbB, muladd, lat, nb, mu, nu, ku, ffetch, ifetch, nfetch, pfA;
   int KUisNB, NO1D;
   double mf, mfB;
   FILE *fp;
   ATL_mmnode_t *mmU, *mmG, *mmR;

   printf("LINE %d of %s\n", __LINE__, __FILE__);
   if (pre == 'c') upre = 's';
   else upre = 'd';
/*
 * See if search is available in new MMRES.sum or old-style MMRES
 */
   mmG = GetOrTransMMRES(pre);
   if (mmG)             /* already have result */
   {
      sprintf(ln, "make RunUMMSearch pre=%c nb=%d", pre, nb);
      assert(!system(ln));            /* update user & external searches */
      if (mmG->mflop[0] <= 0.0)   /* need to retime */
      {
         gmmsearch(upre, muladd, Fku, nNBs, NBs, nreg, lat, Fnb);
         TimeAllKernels(pre, 0, 0, mmG);
         CreateFinalSumFile(pre, mmG, mmG->next);
      }
      mmU = mmG->next;
      if (mmU && mmU->nbB != mmG->nbB)
         goto RECONCILE;
      nb = mmG->nbB;
      if (mmG->next && mmU->mflop[0] < 1.02*mmG->mflop[0])
      {
         mmU = mmG->next = KillMMNode(mmU);
         CreateFinalSumFile(pre, mmG, mmU);
      }
      nb = mmU ? mmU->nbB : mmG->nbB;
      sprintf(ln, "res/%cNB", pre);
      fp = fopen(ln, "w");
      fprintf(fp, "%d\n%d\n", 1, nb);
      fclose(fp);
      return;
   }
/*
 * If we don't have full search results, must run at least some searches
 */
   mmG = OldLogFile2MMNode(pre, "res", "gMMRES");
   if (!mmG)
   {
/*
 *    Complex search depends on the associated real search.  If the real search
 *    hasn't already been done, do it, and get the real values as starting
 *    points for complex search.  We don't care if real search timed or not.
 */
      mmR = OldLogFile2MMNode(upre, "res", "gMMRES");
      if (!mmR)
      {
         gmmsearch(upre, MULADD, Fku, nNBs, NBs, nreg, LAT, Fnb);
         mmR = OldLogFile2MMNode(upre, "res", "gMMRES");
      }
      assert(mmR);
/*
 *    Take real parameters as are starting point for restricted complex search
 */
      muladd = mmR->muladd;
      lat = mmR->lat;
      nb = mmR->nbB;
      mu = mmR->mu; nu = mmR->nu; ku = mmR->ku;
      mfB = 0.0;
      nbB = nb;
      KUisNB = (nb <= ku);
/*
 *    Find the best block factor using the real values for everything else
 */
      for (i=0; i < nNBs; i++)
      {
         if (KUisNB) k = NBs[i];
         else k = Mmin(ku, NBs[i]);
         mf = mms_case(pre, muladd, NBs[i], mu, nu, k, pfA, lat);
         if (mf > mfB)
         {
            mfB = mf;
            nbB = NBs[i];
         }
      }
      nb = nbB;
      fprintf(stdout, "\n\nBEST REAL NB=%d, BEST COMPLEX NB=%d (%.2f)\n\n",
              mmR->nbB, nbB, mfB);
/*
 *    See if not changing prefetch of A improves performance
 */
      mf = mms_case(pre, muladd, nb, mu, nu, KUisNB?nb:Mmin(ku,nb),
                    (pfA^1), lat);
      if (mf > mfB)
      {
         fprintf(stdout, "\n\nPREFETCH SWAPPED TO %d (%.2f)\n\n", pfA, mfB);
         pfA ^= 1;
         mfB = mf;
      }
/*
 *    If case is presently load-at-top, try load-at-bottom; give bottom an
 *    advantage, as it enormously better for error
 */
      if ((pfA | 512) != pfA)
      {
         mf = mms_case(pre, muladd, nb, mu, nu, KUisNB?nb:Mmin(ku,nb),
                       (pfA|512), lat);
         if (mf*1.01 > mfB)
         {
            pfA |= 512;
            mfB = mf;
         }
      }
/*
 *    Save best generated case in old-style gMMRES file
 */
      if (KUisNB) ku = nbB;
      else ku = Mmin(ku, nbB);
      sprintf(ln, "res/%cgMMRES", pre);
      PutInstLogFile1(ln, pre, muladd, pfA, lat, nbB, mu, nu, ku,
                      mmR->fftch, mmR->iftch, mmR->nftch, mfB);
      mmG = OldLogFile2MMNode(pre, "res", "gMMRES");
      assert(mmG);
   }
   else
   {
      if (mmG->mflop[0] <= 0.0) /* need to retime */
         TimeAllKernels(pre, 0, 0, mmG);
      nbB = mmG->nbB;
   }
/*
 * mmG has the best generated case found.  Figure out the best contributed
 * kernel (mmU)
 */
   mmU = ReadMMFileWithPath(pre, "res", "eMMRES.sum");
   if (!mmU)  /* need to run xummsearch */
   {
      sprintf(ln, "make RunUMMSearch pre=%c n=%d", pre, mmG->nbB);
      assert(!system(ln));
      mmU = ReadMMFileWithPath(pre, "res", "eMMRES.sum");
      assert(mmU);
   }
RECONCILE:
/*
 * If user-written kernel is noticably better than generated, use it;
 * if it changes NB, we will need to retune the generated case for new NB!
 */
   if (mmU->mflop[0] > 1.03*mfB)
   {
/*
 *    If user kernel has different NB, will need to retune most params
 *    (prefetch strategy, unrolling and register blocking possibly affected)
 */
      if (mmU->nbB != nbB)
      {
         sprintf(ln, "rm -f res/%cuClean* res/%cClean*", pre, pre);
         system(ln);  /* if NB changed, must redo cleanup searches */
         nbB = mmU->nbB;
         if (mmG->ku == mmG->nbB)  /* keep full unrolling wt new NB */
            ku = mmG->ku = mmU->nbB;
         else
            ku = mmG->ku;
         nb = mmG->nbB = mmG->mbB = mmG->kbB = nbB;
         mu = mmG->mu;  nu = mmG->nu;
         muladd = mmG->muladd;
         lat = mmG->lat;
         pfA = mmG->pref;
/*
 *       If present MU and NU don't evenly divide new NB, but do evenly divide
 *       the old NB, must re-search MU/NU
 */
         if (((mmG->nbB % mmG->mu == 0) && (mmU->nbB % mmG->mu != 0)) ||
             ((mmG->nbB % mmG->nu == 0) && (mmU->nbB % mmG->nu != 0)))
         {
            NO1D = GetNO1D(pre, nreg, nb, muladd, pfA, LAT);
            searchmu_nu(pre, nb, nreg, Fku, muladd, pfA, LAT, NO1D,
                        &mfB, &nbB, &mu, &nu, &ku, &lat);
            mmG->mu = mu;
            mmG->nu = nu;
            mmG->lat = lat;
         }
         FindKU(pre, muladd, pfA, LAT, nb, mu, nu, &mfB, &ku, &lat);
         mmG->ku = ku;
         FindLAT(pre, pfA, MAXLAT, nb, muladd, mu, nu, ku, &mfB, &lat);
         mmG->lat = lat;
         FindFetch('T', 'N', upre, nb, nb, nb, mu, nu, ku, muladd, pfA, lat,
                   &mmG->fftch, &mmG->iftch, &mmG->nftch);
         TimeAllKernels(pre, 0, 1, mmG);  /* force timing of resulting kernel */
         if (mmG->mflop[0]*1.03 > mmU->mflop[0])
         {
            KillAllMMNodes(mmU);
            mmU = NULL;
         }
      }
   }
/*
 * If user-supplied case slower than generated, kill mmU
 */
   else
   {
      KillAllMMNodes(mmU);
      mmU = NULL;
   }
/*
 * Save NB we've found
 */
   sprintf(ln, "res/%cNB", pre);
   fp = fopen(ln, "w");
   fprintf(fp, "%d\n%d\n", 1, mmG->nbB);
   fclose(fp);
   CreateFinalSumFile(pre, mmG, mmU);
   printf("LINE %d of %s\n", __LINE__, __FILE__);
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

int GetNumRegsMM(char pre, int MaxL1Size, int MAX_NREG)
{
   int nreg;
   nreg = GetNumRegs(pre, MaxL1Size, MAX_NREG);
   if (nreg == -1)
   {
      fprintf(stderr,
         "\nUNABLE TO FIND NUMBER OF REGISTERS, ASSUMMING 32.\n\n");
      nreg = 32;
   }
/*
   if (nreg > 128)
   {
      fprintf(stderr, "FOUND NUMBER OF REGISTERS TO BE %d; THIS WOULD TAKE TOO LONG TO SEARCH, SO SETTING TO 128.\n", nreg);
      nreg = 128;
   }
*/
   if (nreg < 8)
   {
      fprintf(stderr,
              "FOUND # OF REGISTERS TO BE %d; TRYING 8 FOR SAFETY.\n", nreg);
      nreg = 8;
   }
#if !defined (ATL_GAS_x8632) && !defined(ATL_GAS_x8664)
   else if (nreg < 16)
   {
      fprintf(stderr,
              "FOUND # OF REGISTERS TO BE %d; TRYING 16 FOR SAFETY.\n", nreg);
      nreg = 16;
   }
#endif
   return(nreg);
}

void GetMMRES(char pre, int nreg, int MaxL1Size, int ForceLat)
{
   char upre;
   char ln[128];
   int i, nNBs, muladd, lat, l1size;
   int *NBs;
   FILE *fp;

   if (pre == 'c') upre = 's';
   else if (pre == 'z') upre = 'd';
   else upre = pre;

   sprintf(ln, "res/%cNB", pre);
   if (!FileExists(ln)) findNBs(upre, ln, MaxL1Size);
   assert( (fp = fopen(ln, "r")) != NULL );
   fscanf(fp, "%d", &nNBs);
   fprintf(stdout, "\nNB's to try: ");
   NBs = malloc(nNBs*sizeof(int));
   for (i=0; i != nNBs; i++)
   {
      fscanf(fp, "%d", NBs+i);
      fprintf(stdout, "%d   ",NBs[i]);
   }
   fprintf(stdout, "\n\n");

   l1size = GetCacheSize(MaxL1Size) * 1024;
   ProbeFPU(pre, l1size, nreg, &muladd, &lat);
   if (ForceLat != -1) lat = ForceLat;


   if (pre == 'c' || pre == 'z')
      cmmsearch(pre, muladd, 0, nNBs, NBs, nreg, lat, 0);
   else mmsearch(pre, muladd, 0, nNBs, NBs, nreg, lat, 0);
   free(NBs);
}
#ifndef ATL_MAXNREG
   #define ATL_MAXNREG 64
#endif
int main(int nargs, char *args[])
{
   char prec, upre, lang;
   int MULADD, MaxL1Size, ForceLat, i, nreg, ROUT, FRC;
   int muladd, lat, nb, mu, nu, ku, ffetch, ifetch, nfetch, pfA;
   int unb, icase;
   double mf, umf;
   FILE *fp;
   char ln[128], auth[65];
   ATL_mmnode_t *mmp;

   GetSettings(nargs, args, &prec, &lang, &ku, &ForceLat, &FRC, &nreg,
               &MaxL1Size, &ROUT);
   assert(FRC == 0 && ku == 0); /* obsolete flags */
   LANG = lang;

   if (prec == 'z') upre = 'd';
   else if (prec == 'c') upre = 's';
   else upre = prec;
   if (ROUT == -3)
   {
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      FindAllClean(prec);
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      exit(0);
   }
   else if (ROUT == -4)
   {
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      FindAllUserClean0(prec);
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      exit(0);
   }
   else if (ROUT == -5) /* produce ATL_mmnreg for sysinfo */
   {
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      GetNumRegs(prec, Mmin(MaxL1Size,32), ATL_MAXNREG);
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      exit(0);
   }
   else if (ROUT == -6) /* Find the best no-copy code */
   {
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      FindNoCopy(prec);
      #if VERB > 0
         fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
      #endif
      exit(0);
   }
   fprintf(stderr, "Precision='%c', FORCE=%d, LAT=%d, nreg=%d, MaxL1=%d\n",
           prec, FRC, ForceLat, nreg, MaxL1Size);

   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   if (nreg == -1) nreg = GetNumRegsMM(upre, Mmin(MaxL1Size,32), ATL_MAXNREG);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   GetMMRES(prec, nreg, MaxL1Size, ForceLat);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   FindNoCopy(prec);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   FindAllClean(prec);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif

   mmp = GetOrTransMMRES(prec);
   assert(mmp);
   if (!mmp->next || mmp->mflop[0] >= mmp->next->mflop[0])
   {
      fprintf(stdout, "\n\nFor this run, the best parameters found were MULADD=%d, lat=%d, NB=%d, MU=%d, NU=%d, KU=%d\n",
              mmp->muladd, mmp->lat, mmp->nbB, mmp->mu, mmp->nu, mmp->ku);
      mf = mmp->mflop[0];
   }
   else
   {
       mf = umf;
       fprintf(stdout,
               "\n\nFor this run, the best case found was NB=%d user case %d\n",
               mmp->next->nbB, mmp->next->ID);
       fprintf(stdout, "written by %s.\n", mmp->next->auth);
      mf = mmp->next->mflop[0];
   }
   fprintf(stdout, "This gave a performance = %f MFLOP.\n", mf);
   fprintf(stdout,
"The necessary files have been created.  If you are happy with\n");
   fprintf(stdout,
"the above mflops for your system, type 'make %cinstall'.\n", prec);
   fprintf(stdout,
"Otherwise, try the xmmsearch with different parameters, or hand\n");
   fprintf(stdout, "tweak the code.\n");
   return(0);
}
