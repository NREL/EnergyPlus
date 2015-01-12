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
#include <assert.h>
#include <string.h>
#include <ctype.h>

int GetPower2(int n)
{
   int pwr2, i;

   if (n == 1) return(0);
   for (pwr2=0, i=1; i < n; i <<= 1, pwr2++);
   if (i != n) pwr2 = 0;
   return(pwr2);
}

#define ShiftThresh 2
char *GetDiv(int N, char *inc)
{
   static char ln[256];
   int pwr2 = GetPower2(N);
   if (N == 1) sprintf(ln, "%s", inc);
   else if (pwr2) sprintf(ln, "((%s) >> %d)", inc, pwr2);
   else sprintf(ln, "((%s) / %d)", inc, N);
   return(ln);
}

char *GetInc(int N, char *inc)
{
   static char ln0[256];
   char ln[256];
   char *p=ln;
   int i, n=N, iPLUS=0;

   if (n == 0)
   {
      ln[0] = '0';
      ln[1] = '\0';
   }
   while(n > 1)
   {
      for (i=0; n >= (1<<i); i++);
      if ( (1 << i) > n) i--;
      if (iPLUS++) *p++ = '+';
      sprintf(p, "((%s) << %d)", inc, i);
      p += strlen(p);
      n -= (1 << i);
   }
   if (n == 1)
   {
      if (iPLUS++) *p++ = '+';
      sprintf(p, "%s", inc);
   }
   if (iPLUS > ShiftThresh) sprintf(ln0, "(%d*(%s))", N, inc);
   else if (iPLUS) sprintf(ln0, "(%s)", ln);
   else sprintf(ln0, "%s", ln);
   return(ln0);
}

void PrintTypeHead(FILE *fpout)
{
   int pwr2;

   fprintf(fpout, "#ifndef ATLAS_TYPE_H\n");
   fprintf(fpout, "#define ATLAS_TYPE_H\n\n");
   fprintf(fpout, "#define ATL_isize ((size_t)%d)\n", (int) sizeof(int));
   fprintf(fpout, "#define ATL_ssize ((size_t)%d)\n", (int) sizeof(float));
   fprintf(fpout, "#define ATL_dsize ((size_t)%d)\n", (int) sizeof(double));
   fprintf(fpout, "#define ATL_csize ((size_t)%d)\n", (int) (2*sizeof(float)));
   fprintf(fpout, "#define ATL_zsize ((size_t)%d)\n", (int) (2*sizeof(double)));
   fprintf(fpout, "#define ATL_%cMulBySize(N_) %s\n", 'i',
           GetInc(sizeof(int), "((size_t)(N_))"));
   fprintf(fpout, "#define ATL_%cMulBySize(N_) %s\n", 's',
           GetInc(sizeof(float), "((size_t)(N_))"));
   fprintf(fpout, "#define ATL_%cMulBySize(N_) %s\n", 'd',
           GetInc(sizeof(double), "((size_t)(N_))"));
   fprintf(fpout, "#define ATL_%cMulBySize(N_) %s\n", 'c',
           GetInc(2*sizeof(float), "((size_t)(N_))"));
   fprintf(fpout, "#define ATL_%cMulBySize(N_) %s\n", 'z',
           GetInc(2*sizeof(double), "((size_t)(N_))"));
   pwr2 = GetPower2(sizeof(int));
   if (pwr2)
   {
      fprintf(fpout, "#define ATL_ishift %d\n", pwr2);
      fprintf(fpout, "#define ATL_iDivBySize(N_) ((N_) >> %d)\n", pwr2);
   }
   else
      fprintf(fpout, "#define ATL_iDivBySize(N_) ((N_) / sizeof(int))\n");
   pwr2 = GetPower2(sizeof(float));
   if (pwr2)
   {
      fprintf(fpout, "#define ATL_sshift %d\n", pwr2);
      fprintf(fpout, "#define ATL_cshift %d\n", pwr2+1);
      fprintf(fpout, "#define ATL_%cDivBySize(N_) ((N_) >> %d)\n", 's', pwr2);
      fprintf(fpout, "#define ATL_%cDivBySize(N_) ((N_) >> %d)\n", 'c', pwr2+1);
   }
   else
   {
      fprintf(fpout, "#define ATL_sDivBySize(N_) ((N_) / sizeof(float))\n");
      fprintf(fpout, "#define ATL_cDivBySize(N_) ((N_) / %d)\n",
              (int)(2*sizeof(float)));
   }
   pwr2 = GetPower2(sizeof(double));
   if (pwr2)
   {
      fprintf(fpout, "#define ATL_dshift %d\n", pwr2);
      fprintf(fpout, "#define ATL_zshift %d\n", pwr2+1);
      fprintf(fpout, "#define ATL_%cDivBySize(N_) ((N_) >> %d)\n", 'd', pwr2);
      fprintf(fpout, "#define ATL_%cDivBySize(N_) ((N_) >> %d)\n", 'z', pwr2+1);
   }
   else
   {
      fprintf(fpout, "#define ATL_dDivBySize(N_) ((N_) / sizeof(double))\n");
      fprintf(fpout, "#define ATL_zDivBySize(N_) ((N_) / %d)\n",
              (int)(2*sizeof(double)));
   }
   fprintf(fpout, "\n#endif\n");
}
int main(int nargs, char *args[])
{
   FILE *fpout=NULL;
   if (nargs == 1) fpout = stdout;
   else if (nargs != 2)
   {
      fprintf(stderr, "usage: %s <file out>\n", args[0]);
      exit(-1);
   }
   if (fpout == NULL)
   {
      fpout = fopen(args[1], "w");
      assert(fpout != NULL);
   }
   PrintTypeHead(fpout);
   if (fpout != stdout) fclose(fpout);
   return(0);
}
