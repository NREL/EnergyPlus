#include "atlconf.h"

int RunASMProbe(char *targ, int verb, enum OSTYPE OS, char *asmnam)
{
   char ln[2048], ln2[2048];
   char *cmnd, *res, *frm;
   int i = 0;
   if (targ)
   {
      frm = "make IRun_%s atlrun=atlas_runX args=\"-v %d\" MYFLAGS=\"-DATL_OS_%s\" targ=%s 2> /dev/null | fgrep SUCCESS";
      i = strlen(targ);
   }
   else
      frm = "make IRun_%s args=\"-v %d\" MYFLAGS=\"-DATL_OS_%s\" 2> /dev/null | fgrep SUCCESS";

   i += strlen(frm) + strlen(asmnam) + 11 + strlen(osnam[OS]) + 1;
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   if (targ)
      sprintf(cmnd, frm, asmnam, verb, osnam[OS], targ);
   else
      sprintf(cmnd, frm, asmnam, verb, osnam[OS]);
   if (verb > 1)
      fprintf(stderr, "system(%s)\n", cmnd);
   res = atlsys_1L(NULL, cmnd, verb, 0);
   free(cmnd);
   if (res)
   {
      if (strstr(res, "SUCCESS"))
      {
         if (verb)
            fprintf(stdout, "   %s: DETECTED!\n", asmnam);
         free(res);
         return(1);
      }
      free(res);
   }
   if (verb)
      fprintf(stdout, "   %s: NO.\n", asmnam);
   return(0);
}

enum ASMDIA ProbeASM(char *targ, int verb, enum OSTYPE OS)
{
   int i;
   for (i=1; i < NASMD; i++)
      if (RunASMProbe(targ, verb, OS, ASMNAM[i]))
         return(i);
   return(ASM_None);
}


void PrintUsage(char *name, int iarg, char *arg)
{
   fprintf(stderr, "\nERROR around arg %d (%s).\n", iarg,
           arg ? arg : "unknown");
   fprintf(stderr, "USAGE: %s [flags] where flags are:\n", name);
   fprintf(stderr, "   -v <verb> : verbosity level\n");
   fprintf(stderr, "   -O <enum OSTYPE #>  : set OS type\n");
   fprintf(stderr,
      "NOTE: enum #s can be found by : make xprint_enums ; ./xprint_enums\n");
   exit(iarg);
}

void GetFlags(int nargs,                /* nargs as passed into main */
              char **args,              /* args as passed into main */
              int *verb,                /* verbosity setting */
              enum OSTYPE *OS,          /* OS to assume */
              char **targ             /* mach to ssh to*/
             )
{
   int i, k, k0, kn, DoInt;
   char *sp, *sp0;

   *verb = 0;
   *targ = NULL;

   *OS = 0;
   *verb = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
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
   char *targ;
   int verb;
   enum OSTYPE OS;
   enum ASMDIA asmd;
   GetFlags(nargs, args, &verb, &OS, &targ);
   asmd = ProbeASM(targ, verb, OS);
   printf("ASM=%d\n", asmd);
   return(asmd == ASM_None);
}
