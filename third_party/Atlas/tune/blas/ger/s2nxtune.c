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
#include "atlas_tst.h"
#include "atlas_lvl2.h"
#include "atlas_level1.h"
#include "atlas_genparse.h"
#include "atlas_gentesttime.h"
int ATL_KERN_NX=16;

#if defined(__MINGW32__) || defined(__MINGW64__)

int slashdrivesub(char *ln)
/*
 * replaces \\c\ with c:\, returns change in string length
 * this version required for older cygwins
 */
{
   char *sp, *lp=ln, ctmp;
   int nrep=0;
   do
   {
      sp = strstr(lp, "\\\\");
      if (sp && strlen(sp) > 3)
      {
         if (sp[2] == 'a' || sp[2] == 'b' || sp[2] == 'c' || sp[2] == 'd' ||
             sp[2] == 'e' || sp[2] == 'f' || sp[2] == 'g' || sp[2] == 'h')
         {
            if (sp[3] == '\\')
            {
               ctmp = sp[2];
               sp[0] = sp[2];
               sp[1] = ':';
               sp[2] = '\\';
               for (lp=sp+3; *lp = lp[1]; lp++);
               lp = sp + 3;
               nrep++;
            }
            else lp = sp + 2;
         }
         else lp = sp + 2;
      }
      else lp = sp + 2;
   }
   while (sp);
   return(-nrep);
}

int cygdrivesub(char *ln)
/*
 * replaces \cygdrive\c\ with c:\, returns change in string length
 * this version works cygnus version 1.1.0
 */
{
   char *sp;
   int i=0;

   while(sp = strstr(ln, "\\cygdrive\\"))
   {
      i++;
      sp[0] = sp[10];
      sp[1] = ':';
      sp[2] = '\\';
      sp += 3;
      while (*sp = sp[9]) sp++;
   }
   return( slashdrivesub(ln) - (i*9) );
}

void slashsub(char *ln)
/*
 * changes forward slash of unix to backslash of windoze
 */
{
   int i;
   for (i=0; ln[i]; i++) if (ln[i] == '/') ln[i] = '\\';
}

#endif

static double GetTime
(
   enum ATLAS_UPLO Uplo,/* which triangle? */
   int verb,            /* verbosity */
   int nreps,           /* number of reps to do for one timing sample */
   size_t flushelts,    /* size of area to flush to avoid cache reuse */
   ATL_CINT N,          /* matrix size */
   int NX               /* what to set ref/kernel crossover to */
)
{
   size_t setsz, nsets, accsz, Nt;
   double t0, t1;
   void *vp;
   TYPE *tp, *x, *y, *a;
   #ifdef TCPLX
      const TYPE one[2] = {ATL_rone, ATL_rzero};
   #else
      const TYPE one = ATL_rone;
   #endif
   int i, j;

   ATL_KERN_NX = NX;
   accsz = 2*N + ((size_t)N)*(N>>1);
   setsz = 2*N + ((size_t)N)*N;
   nsets = (flushelts + accsz-1)/accsz;
   nsets = (nsets) ? nsets : 1;
   Nt = nsets * setsz;
   tp = vp = malloc(ATL_MulBySize(Nt));
   assert(vp);
   Mjoin(PATL,gegen)(Nt, 1, tp, Nt, N+127*37);
   t0 = time00();
   for (j=0, i=nreps; i; i--)
   {
      x = tp + j*setsz;
      y = x + N;
      a = y + N;
   #ifdef TCPLX
      Mjoin(PATL,her2)(Uplo, N, one, x, 1, y, 1, a, N);
   #else
      Mjoin(PATL,syr2)(Uplo, N, ATL_rone, x, 1, y, 1, a, N);
   #endif
      if (++j == nsets)
         j = 0;
   }
   t1 = time00();
   t1 = (t1 - t0) / nreps;
   free(vp);
   return(t1);
}
static double GetTimes
(
   enum ATLAS_UPLO Uplo,/* which triangle? */
   int verb,            /* verbosity */
   int nsample,         /* number of samples to take */
   int nreps,           /* number of reps to do for one timing sample */
   size_t flushelts,    /* size of area to flush to avoid cache reuse */
   ATL_CINT N,          /* matrix size */
   int NX               /* what to set ref/kernel crossover to */
)
{
   int i;
   double *times, t0;

   times = malloc(nsample*sizeof(double));
   assert(times);
   for (i=0; i < nsample; i++)
   {
      times[i] = GetTime(Uplo, verb, nreps, flushelts, N, NX);
      if (verb > 1)
         printf("      %d: %e\n", i, times[i]);
   }
   SortDoubles(nsample, times);
   #ifdef WALL
      t0 = times[0];
   #else
      i = (nsample > 1) ? (nsample>>1)+1 : 0;
      t0 = times[i];
   #endif
   free(times);
   if (verb > 1)
      printf("      RETURNING TIME: %e\n", t0);
   return(t0);
}

#define NX0 16
int RecDoubleNX
(
   enum ATLAS_UPLO Uplo,/* which triangle? */
   int verb,            /* verbosity */
   int nsample,         /* number of samples to take */
   int nreps,           /* number of reps to do for one timing sample */
   size_t flushelts,    /* size of area to flush to avoid cache reuse */
   ATL_CINT N           /* matrix size */
)
{
   double t0, tL, tN;  /* 0, Last, Next */
   double tB, tE, tM;  /* beginning, end, middle timings */
   ATL_INT n0, nL, nN;
   ATL_INT nB, nE, nM;

   t0 = GetTimes(Uplo, verb, nsample, nreps, flushelts, N, N);
   printf("\n   Time for N=NX=%d : %e\n", N, t0);
   printf("     N    NX    %% of N=%2d\n", NX0);
   printf("======  ====  ===========\n");
/*
 * Now halve NX until performance stops increasing
 */
   tL = t0;
   nL = N;
   do
   {
      nN = (nL>>4)<<3;
      tN = GetTimes(Uplo, verb, nsample, nreps, flushelts, N, nN);
      printf("%6d  %4d  %11.2f\n", N, nN, (tN/t0)*100.0);
      if (tN > tL) break;  /* stop if new time longer than last */
      nL = nN;
      tL = tN;
   }
   while (nN);  /* stop if NX = 0 */
   printf("\n");

   tE = tL;
   nE = nL;
   tB = tN;
   nB = nN;
   while (nE - nB > 8)
   {
      nM = nB + ((nE-nB)>>1);
      nM = (nM>>3) << 3;   /* keep mul of 8 for alignment, etc */
      tM = GetTimes(Uplo, verb, nsample, nreps, flushelts, N, nM);
      printf("%6d  %4d  %11.2f\n", N, nM, (tM/t0)*100.0);
      if (tE >= tB)
      {
         tE = tM;
         nE = nM;
      }
      else
      {
         tB = tM;
         nB = nM;
      }
   }
   if (tE < tB && tE < tM)
   {
      tM = tE;
      nM = nE;
   }
   else if (tB < tE && tB < tM)
   {
      tM = tB;
      nM = nB;
   }
   printf("NX selected as %d (%.2f%%)!\n", nM, (tM/t0)*100.0);
   return(nM);
}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);

   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -U <u/l>\n");
   fprintf(stderr, "   -n <N>\n");
   fprintf(stderr, "   -r <reps>\n");
   fprintf(stderr, "   -s <nsample>\n");
   fprintf(stderr, "   -v <verb>\n");
   fprintf(stderr, "   -C <flushKB>\n");
   fprintf(stderr, "   -o <outfile>\n");
   exit(ierr ? ierr : -1);
}

int GetFlags(int nargs, char **args, enum ATLAS_UPLO *Uplo, int *verb,
             int *nsample, int *nreps, size_t *flushelts, char **outfile)
{
   int N=2000, i;
   char ch;
   char *of;

   *flushelts = L2SIZE;
   *nsample = 10;
   *nreps = 1;
   *verb = 1;
   *Uplo = AtlasUpper;
   of = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, "No '-' preceeding flag!");
      switch(args[i][1])
      {
      case 's' :
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -s ");
         *nsample = atoi(args[i]);
         break;
      case 'v' :
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -v ");
         *verb = atoi(args[i]);
         break;
      case 'r' :
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -r ");
         *nreps = atoi(args[i]);
         break;
      case 'n' :
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -n ");
         N = atoi(args[i]);
         break;
      case 'U':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -C) ");
         ch = args[i][0];
         *Uplo = (ch == 'l' || ch == 'L') ? AtlasLower : AtlasUpper;
         break;
      case 'C' :
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -C) ");
         *flushelts = atoll(args[i])*1024;
         break;
      case 'o' :
         if (++i >= nargs)
            PrintUsage(args[0], i-1, "out of flags in -C) ");
         of = args[i];
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   if (of)
   {
      *outfile = DupString(of);
      #if defined(__MINGW32__) || defined(__MINGW64__)
         slashsub(*outfile);
         cygdrivesub(*outfile);
      #endif
   }
   else
   {
      of = malloc(sizeof(char)*32);
      assert(of);
      #if defined(__MINGW32__) || defined(__MINGW64__)
         sprintf(of, "res\atlas_%ssyrNX.h", Mstr(PRE));
      #else
         sprintf(of, "res/atlas_%ssyrNX.h", Mstr(PRE));
      #endif
      *outfile = of;
   }
   return(N);
}

void GenIncFile(char *outfile, int NX)
{
   FILE *fpout;
   fpout = fopen(outfile, "w");
   assert(fpout);
   fprintf(fpout, "#ifndef ATLAS_%sSYR2_H\n   #define ATLAS_%sSYR2_H\n",
           Mstr(PREU), Mstr(PREU));
   fprintf(fpout, "   #define ATL_S2NX %d\n#endif\n", NX);
   fclose(fpout);
}

int main(int nargs, char **args)
{
   size_t flushelts;
   char *outfile;
   int N, verb, nsample, nreps, NX;
   enum ATLAS_UPLO Uplo;

   N = GetFlags(nargs, args, &Uplo, &verb, &nsample, &nreps, &flushelts,
                &outfile);
   NX = RecDoubleNX(Uplo, verb, nsample, nreps, flushelts, N);
   GenIncFile(outfile, NX);
   free(outfile);
   printf("\nNX=%d!\n", NX);
   return(0);
}
