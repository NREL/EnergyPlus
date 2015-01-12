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
#include <math.h>
#include "atlas_misc.h"
#include "atlas_lvl3.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))

#define dumb_seed(iseed_) srand(iseed_)
#ifndef RAND_MAX  /* rather dangerous non-ansi workaround */
   #define RAND_MAX ((unsigned long)(1<<30))
#endif
#define dumb_rand() ( 0.5 - ((double)rand())/((double)RAND_MAX) )

double time00();

#ifndef DENMAT
   #define DENMAT 200
#endif

#ifdef Right_
   char Side = 'R';
#else
   char Side = 'L';
#endif
#ifdef Upper_
   char Uplo = 'U';
#else
   char Uplo = 'L';
#endif
#ifdef Transpose_
   char Tran = 'T';
#else
   char Tran = 'N';
#endif
#ifdef UnitDiag_
   char Diag = 'U';
#else
   char Diag = 'N';
#endif


void PrintUsage(char *nam)
{
   fprintf(stderr,
           "usage: %s -m <M> -N <N0> <NN> <incN> -a <alpha> -f <filename>\n",
           nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *M, int *N0, int *NN, int *incN,
              TYPE *alpha,  char *file)
{
   int i;
   char *in, *out=file;

   file[0] = '\0';
   *M = NB;
   *N0 = 100;
   *NN = 2000;
   *incN = 100;
   *alpha = 1.0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'f':
         in = args[++i];
         while (*file++ = *in++);
         break;
      case 'm':
         *M = atoi(args[++i]);
         break;
      case 'a':
         *alpha = atof(args[++i]);
      case 'N':
         *N0 = atoi(args[++i]);
         *NN = atoi(args[++i]);
         *incN = atoi(args[++i]);
         break;
      default:
         PrintUsage(args[0]);
      }
   }
}

int main(int nargs, char *args[])
{
   char fnam[256];
   TYPE alpha;
   int M, N0, NN, incN, n, nn, k;
   FILE *fpout;

   GetFlags(nargs, args, &M, &N0, &NN, &incN, &alpha, fnam);
   if (fnam[0])
   {
      fpout = fopen(fnam, "a");
      assert(fpout);
      nn = 3*NB;
      fprintf(fpout, "#define TRSM_%c%c%c%c_Xover %d\n",
              Side, Uplo, Tran, Diag, nn);
   }
   fprintf(stdout, "\n\nXover point at NB=%d, N=%d\n\n", NB, nn);
   return(0);
}

