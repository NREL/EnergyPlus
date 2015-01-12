/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "atlas_type.h"
#include "atlas_fopen.h"
int GetL1CacheSize(int MaxL1Size)
{
   FILE *L1f;
   char ln[80];
   int L1Size;

   if (!FileExists("res/L1CacheSize"))
   {
      sprintf(ln, "make RunL1 MaxL1=%d\n",MaxL1Size);
      assert(system(ln) == 0);
   }
   L1f = fopen("res/L1CacheSize", "r");
   assert(L1f != NULL);
   fscanf(L1f, "%d", &L1Size);
   fclose(L1f);
   fprintf(stderr, "\n      Read in L1 Cache size as = %dKB.\n",L1Size);
   return(L1Size);
}

void getfpinfo0(char pre, int *muladd, int *lat, int *lbnreg, double *mf)
{
   char fnam[128];
   FILE *fp;

   if (pre == 'z') pre = 'd';
   else if (pre == 'c') pre = 's';

   sprintf(fnam, "res/%cMULADD", pre);
   if (!FileExists(fnam))
   {
      sprintf(fnam, "make res/%cMULADD pre=%c\n", pre, pre);
      assert(system(fnam) == 0);
      sprintf(fnam, "res/%cMULADD", pre);
   }
   fp = fopen(fnam, "r");
   assert(fp);
   assert(fscanf(fp, " %d", muladd) == 1);
   assert(fscanf(fp, " %d", lat) == 1);
   assert(fscanf(fp, " %lf", mf) == 1);
   assert(fscanf(fp, " %d", lbnreg) == 1);
   fclose(fp);
}

void getfpinfo(char pre, int *muladd, int *lat, int *lbnreg, int *nkflop)
{
   double mf;
   char ln[32];

   getfpinfo0(pre, muladd, lat, lbnreg, &mf);
   if (mf <= 0.0)
   {
      if (pre == 'c') pre = 's';
      else if (pre == 'z') pre = 'd';
      sprintf(ln, "make RunMADef pre=%c\n", pre);
      assert(system(ln) == 0);
      getfpinfo0(pre, muladd, lat, lbnreg, &mf);
      assert(mf >= 0.0);
   }
   mf = mf * 0.75 * 750;
   #ifdef ATL_ARCH_ATHLON  /* kludge for athlon defaults */
      mf *= 3.0;
   #endif
   if (mf > (double)(1<<30)) *nkflop = ~(1<<31);
   else *nkflop = mf;
}

void CreateHeader(char pre, char *fnam, int L1Size, int muladd, int lat,
                  int lbnreg, int nkflop, int mmnreg)
{
   FILE *fpout;

   fpout = fopen(fnam, "w");
   assert(fpout);
   fprintf(fpout, "#ifndef ATL_%cSYSINFO_H\n   #define ATL_%cSYSINFO_H\n\n",
           toupper(pre), toupper(pre));
   if (muladd) fprintf(fpout, "#define ATL_MULADD\n");
   else fprintf(fpout, "#define ATL_NOMULADD\n");
   fprintf(fpout, "#define ATL_L1elts %d\n", L1Size);
   fprintf(fpout, "#define ATL_fplat  %d\n", lat);
   fprintf(fpout, "#define ATL_lbnreg %d\n", lbnreg);
   fprintf(fpout, "#define ATL_mmnreg %d\n", mmnreg);
   fprintf(fpout, "#define ATL_nkflop %d\n", nkflop);
   fprintf(fpout, "\n#endif\n");
   fclose(fpout);
}

int getmmnreg(char pre)
{
   char fnam[128];
   int mmnregs;
   FILE *fp;

   if (pre == 'z') pre = 'd';
   else if (pre == 'c') pre = 's';
   sprintf(fnam, "res/%cnreg", pre);
   if (!FileExists(fnam))
   {
      sprintf(fnam, "make res/%cnreg\n", pre);
      assert(system(fnam) == 0);
      sprintf(fnam, "res/%cnreg", pre);
   }
   fp = fopen(fnam, "r");
   assert(fp);
   fscanf(fp, " %d", &mmnregs);
   fclose(fp);
   return(mmnregs);
}

int main(int nargs, char **args)
{
   int MaxL1Size;
   int muladd, lat, lbnreg, L1Size, mmnreg, nkflop;
   FILE *fpout;
   char pre;

   if (nargs != 3)
   {
      fprintf(stderr, "USAGE: %s <pre> <file>\n", args[0]);
      exit(-1);
   }
   pre = *args[1];

   L1Size = 1024 * GetL1CacheSize(64);
   if (pre == 'd') L1Size /= ATL_dsize;
   else if (pre == 's') L1Size /= ATL_ssize;
   else if (pre == 'z') L1Size /= ATL_csize;
   else if (pre == 'c') L1Size /= ATL_zsize;
   getfpinfo(pre, &muladd, &lat, &lbnreg, &nkflop);
   CreateHeader(pre, args[2], L1Size, muladd, lat, lbnreg, nkflop, 0);
   mmnreg = getmmnreg(pre);
   CreateHeader(pre, args[2], L1Size, muladd, lat, lbnreg, nkflop, mmnreg);
   return(0);
}
