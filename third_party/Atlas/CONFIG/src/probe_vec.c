/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2006 R. Clint Whaley
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
#include "atlconf.h"

int RunISAProbe(char *isaxnam, int verb, char *targ, char *opt)
{
   char *cmnd, *res, *frm;
   int i=1;
   if (targ)
   {
      frm = "make IRun_%s atlrun=atlas_runX targ=%s MYFLAGS='%s' 2> /dev/null | fgrep SUCCESS";
      i += strlen(targ);
   }
   else
      frm = "make IRun_%s MYFLAGS='%s' 2> /dev/null | fgrep SUCCESS";
   i += strlen(frm) + strlen(isaxnam) + strlen(opt);
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   if (targ)
      sprintf(cmnd, frm, isaxnam, targ, opt);
   else
      sprintf(cmnd, frm, isaxnam, opt);
   res = atlsys_1L(targ, cmnd, verb, 0);
   if(res)
   {
      if (strstr(res, "SUCCESS"))
      {
         if (verb)
            fprintf(stdout, "   %s: DETECTED!\n", isaxnam);
         free(res);
         return(1);
      }
   }
   if (verb > 1)
      fprintf(stdout, "   cmnd='%s' out='%s'\n", cmnd, res);
   free(cmnd);
   free(res);
   if (verb)
      fprintf(stdout, "   %s: NO.\n", isaxnam);
   return(0);
}

int GetAllISAExt(int verb, char *targ, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret=0;
   char ln[256];

   if (verb)
      fprintf(stdout, "\nProbing for supported ISA extensions:\n");

/*
 * For OS X, throw try throwing their random-ass annoyance flag
 */
   if (OS == OSOSX)
   {
      if (RunISAProbe(ISAXNAM[ISA_AV], verb, targ, ln))
         iret |= (1<<ISA_AV);
   }
   sprintf(ln, "-DATL_OS_%s -DATL_%s", osnam[OS], ASMNAM[asmb]);
   for (i=1; i < NISA; i++)
      if (RunISAProbe(ISAXNAM[i], verb, targ, ln))
         iret |= (1<<i);
   return(iret);
}

void PrintUsage(char *name, int iarg, char *arg)
{
   fprintf(stderr, "\nERROR around arg %d (%s).\n", iarg,
           arg ? arg : "unknown");
   fprintf(stderr, "USAGE: %s [flags] where flags are:\n", name);
   fprintf(stderr, "   -v <verb> : verbosity level\n");
   fprintf(stderr, "   -O <enum OSTYPE #>  : set OS type\n");
   fprintf(stderr, "   -s <enum ASMDIA #>  : set assembly dialect\n");
   fprintf(stderr,
      "NOTE: enum #s can be found by : make xprint_enums ; ./xprint_enums\n");
   exit(iarg);
}

void GetFlags(int nargs,                /* nargs as passed into main */
              char **args,              /* args as passed into main */
              int *verb,                /* verbosity setting */
              enum OSTYPE *OS,          /* OS to assume */
              enum ASMDIA *asmb,        /* assembly dialect to assume */
              char **targ             /* mach to ssh to*/
             )
{
   int i, k, k0, kn, DoInt;
   char *sp, *sp0;

   *verb = 0;
   *targ = NULL;

   *asmb = 0;
   *OS = 0;
   *verb = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 's':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *asmb = atoi(args[i]);
         break;
      case 'O':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *OS = atoi(args[i]);
         break;
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *verb = atoi(args[i]);
         break;
      case 'T':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *targ = args[i];
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
}

int main(int nargs, char **args)
{
   int verb, iret;
   enum OSTYPE OS;
   enum ASMDIA asmb;
   char *targ;

   GetFlags(nargs, args, &verb, &OS, &asmb, &targ);
   iret = GetAllISAExt(verb, targ, OS, asmb);
   printf("VECFLAG=%d\n", iret);
   return(0);
}
