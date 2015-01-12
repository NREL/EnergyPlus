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

void PrintUsage(char *name, int ierr, char *flag)
{
   fprintf(stderr, "%s: GEMM search driver\n", name);
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
   fprintf(stderr, "   -o <outfile> : defaults to res/<pre>MMRES.sum\n");
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
assert(*nregs >= 0);
   if (*outfile == NULL)
   {
      *outfile = DupString("res/dMMRES.sum");
      (*outfile)[4] = pre;
   }
   return(pre);
}

void DoAllSearches(int verb, char pre, int nregs, int MACC, int lat, int nb,
                   int ku, char *outfile)
{
   char ln[256];
   ATL_mmnode_t *mmg, *mme, *mmp;
   double mf;

/*
 * First, find best generated (emit_mm) kernel
 */
   printf("\nINVOKING GMMSEARCH.C, PRE='%c'\n", pre);
   sprintf(ln, "make RunGMMSearch pre=%c\n", pre);
   assert(!system(ln));
   mmg = ReadMMFileWithPath(pre, "res", "guMMRES.sum");
   if (mmg)
   {
      if (mmg->mflop[0] <= 0)  /* need to retime */
      {
         mmg->mflop[0] = TimeGMMKernel(verb, 0, pre, mmg->muladd, mmg->lat,
                                       1, mmg->nbB, mmg->mu, mmg->nu, mmg->ku,
                                       mmg->fftch, mmg->iftch, mmg->nftch,
                                       FLAG_IS_SET(mmg->flag, MMF_LDCTOP),
                                       mmg->pref, -1, -1);
      }
      WriteMMFileWithPath(pre, "res", "guMMRES.sum", mmg);
   }
   else
   {
      mmg = ReadMMFileWithPath(pre, "res", "gMMRES.sum");
      assert(mmg);
   }
   printf("\nDONE GMMSEARCH.C, PRE='%c'\n\n", pre);
/*
 * Get results of all external searches
 */
   printf("\nRUNNING EXTERNAL SEARCHES, PRE='%c', NB=%d:\n", pre, mmg->nbB);
   sprintf(ln, "make RunUMMSearch pre=%c nb=%d", pre, mmg->nbB);
   assert(!system(ln));
   mme = ReadMMFileWithPath(pre, "res", "eMMRES.sum");
   assert(mme);
/*
 * If user-written kernel is noticably better than generated, use it;
 * If it changes NB< we will need to retune the generateed case to match new NB!
 */
   if (mme->mflop[0] > 1.03*mmg->mflop[0])
   {
      if (mme->nbB != mmg->nbB)
      {
         printf("\nFORCING NB=%d GMMSEARCH.C, PRE='%c'\n", mme->nbB, pre);
         sprintf(ln, "rm res/%cguMMRES.sum", pre);
         system(ln);
         sprintf(ln, "make res/%cguMMRES.sum pre=%c, nb=%d\n", pre, pre,
                 mme->nbB);
         assert(!system(ln));
         KillAllMMNodes(mmg);
         mmg = ReadMMFileWithPath(pre, "res", "guMMRES.sum");
         assert(mmg);
         assert(mmg->nbB == mme->nbB);
      }
      else
         WriteMMFileWithPath(pre, "res", "guMMRES.sum", mmg);
   }
   else  /* generated kernel is just as good */
   {
      WriteMMFileWithPath(pre, "res", "guMMRES.sum", mmg);
      KillAllMMNodes(mme);
      mme = NULL;
   }
   mmg->next = mme;
   WriteMMFileWithPath(pre, "res", "MMRES.sum", mmg);
/*
 * Find no-copy code
 */
   sprintf(ln, "./xmmcuncpsearch -p %c -R -6\n", pre);
   assert(!system(ln));
/*
 * Find cleanup code
 */
   sprintf(ln, "./xmmcuncpsearch -p %c -R -3\n", pre);
   assert(!system(ln));
   if (!mmg->next)
   {
      printf("\n\nFor this run, the best parameters found were MACC=%d, lat=%d, NB=%d, MU=%d, NU=%d, KU=%d\n",
             mmg->muladd, mmg->lat, mmg->nbB, mmg->mu, mmg->nu, mmg->ku);
      mf = mmg->mflop[0];
   }
   else
   {
      mf = mmg->next->mflop[0];
       printf("\n\nFor this run, the best case found was NB=%d user case %d\n",
              mmg->next->nbB, mmg->next->ID);
       if (mmg->next->auth)
          printf("written by %s", mmg->next->auth);
       if (mmg->next->ID > 0 && mmg->next->rout)
          printf(", filename='%s'.\n", mmg->next->rout);
       else if (mmg->next->auth)
          printf("\n");
   }
   printf("This gave a performance = %f MFLOP.\n", mf);
   printf("The necessary files have been created.  If you are happy with\n");
   printf("the above mflops for your system, type 'make %cinstall'.\n\n", pre);
   KillAllMMNodes(mmg);
}


int main(int nargs, char **args)
{
   char *outfile;
   int verb, nregs, nb, ku, MACC, lat;
   char pre;

   pre = GetFlags(nargs, args, &verb, &nregs, &nb, &ku, &MACC, &lat, &outfile);
   DoAllSearches(verb, pre, nregs, MACC, lat, nb, ku, outfile);
   exit(0);
}
