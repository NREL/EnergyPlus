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
#include "atlas_misc.h"
#include "atlas_fopen.h"
#include "atlas_prefetch.h"

#define Mmin(x, y) ( (x) > (y) ? (y) : (x) )

#define TOLERANCE 1.2
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

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -p <pre> -n <nb> -C <m/n/k>\n", nam);
   exit(-1);
}

int main(int nargs, char *args[])
{
   char ln[127], pre='d', ln2[127], ch;
   int i, nb=28;
   double mflops[3];
   double mf1, mf4;
   FILE *fpin, *fp;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'p':
         ch = args[++i][0];
         pre = Mlowcase(ch);
         break;
      case 'n':
         nb = atoi(args[++i]);
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   sprintf(ln, "res/M%cNB%d_4x1x1_0-1.mflop", pre, nb);
   if (!FileExists(ln))
   {
      sprintf(ln2, "make %chc_cases nb=%d\n", pre, nb);
      assert(system(ln2) == 0);
   }
   fpin = fopen(ln, "r");
   assert(fpin);
   for (i=0; i < 3; i++) assert( fscanf(fpin, "%lf", mflops+i) );
   mf1 = GetAvg(3, 1.2, mflops);
   fclose(fpin);

   sprintf(ln, "res/M%cNB%d_4x4x1_0-1.mflop", pre, nb);
   if (!FileExists(ln))
   {
      sprintf(ln2, "make %chc_cases nb=%d\n", pre, nb);
      if (system(ln2) != 0)
      {
         sprintf(ln, "rm -f res/M%cNB*\n", pre);
         system(ln);
         fprintf(stderr, "Error in command: %s", ln2);
         exit(-1);
      }
   }
   fpin = fopen(ln, "r");
   assert(fpin);
   for (i=0; i < 3; i++) assert( fscanf(fpin, "%lf", mflops+i) );
   mf4 = GetAvg(3, 1.2, mflops);
   fclose(fpin);

   sprintf(ln, "res/%cmmcase.h", pre);
   fp = fopen(ln, "w");
   assert(fp);
   fprintf(fp, "#ifndef %cMMCASE_H\n", Mupcase(pre));
   fprintf(fp, "   #define %cMMCASE_H\n", Mupcase(pre));
   if (mf1 >= mf4) fprintf(fp, "   #define Use4x1\n");
   else fprintf(fp, "   #define Use4x4\n");
   fprintf(fp, "#endif\n");
   fclose(fp);
   sprintf(ln, "res/%cHCRES", pre);
   fp = fopen(ln, "w");
   assert(fp);
   fprintf(fp, "%f\n", mf1);
   fprintf(fp, "%f\n", mf4);
   fclose(fp);

   return(0);
}
