#include "atlconf.h"

void BarfOutArchs(FILE *fpout, int ibeg, int iend)
{
   int i;

   assert(ibeg < NMACH && ibeg >= 0);
   assert(iend < NMACH && iend >= 0);
   if (ibeg == iend)
      fprintf(fpout, "\nMACHTYPE %d = '%s'\n", ibeg, machnam[ibeg]);
   else
   {
      fprintf(fpout, "Architectural enums (Config's enum MACHTYPE):\n");
      for (i=ibeg; i <= iend; i++)
         fprintf(fpout, "   %3d = '%s'\n", i, machnam[i]);
      fprintf(fpout, "\n");
   }
}

void BarfOutOSs(FILE *fpout, int ibeg, int iend)
{
   int i;

   assert(ibeg < NOS && ibeg >= 0);
   assert(iend < NOS && iend >= 0);
   if (ibeg == iend)
      fprintf(fpout, "\nOSTYPE %d = '%s'\n", ibeg, osnam[ibeg]);
   else
   {
      fprintf(fpout, "Operating System enums (Config's enum OSTYPE):\n");
      for (i=ibeg; i <= iend; i++)
         fprintf(fpout, "   %3d = '%s'\n", i, osnam[i]);
      fprintf(fpout, "\n");
   }
}

void BarfOutComps(FILE *fpout, int ibeg, int iend)
{
   int i;

   assert(ibeg < NCOMP && ibeg >= 0);
   assert(iend < NCOMP && iend >= 0);
   if (ibeg == iend)
      fprintf(fpout, "\nCOMPTYPE %d = '%s'\n", ibeg, COMPNAME[ibeg]);
   else
   {
      fprintf(fpout, "Compiler integer defines:\n");
      for (i=ibeg; i <= iend; i++)
         fprintf(fpout, "   %3d = '%s'\n", i, COMPNAME[i]);
      fprintf(fpout, "\n");
   }
}

void BarfOutISAs(FILE *fpout)
{
   int i;

   fprintf(fpout,
"\nISA extensions are combined by adding their values together (bitvector):\n");

   fprintf(fpout, "   %10s: %d\n", "none", 1);
   for (i=1; i < NISA; i++)
      fprintf(fpout, "   %10s: %d\n", ISAXNAM[i], 1<<i);
   fprintf(fpout, "\n");
}

void PrintUsage(char *name, int i)
{
   fprintf(stderr, "Error in arg %d, USAGE:\n", i);
   fprintf(stderr,
"   %s [-a (archs)] [-o (OSes)] [-A ibeg iend] [-O ibeg iend] [-i (ISAext)\n",
           name);
   exit(i);
}

void GetFlags(int nargs, char **args, int *DoArch, int *abeg, int *aend,
              int *DoOS, int *osbeg, int *osend,
              int *DoComps, int *cbeg, int *cend, int *DoISA)
{
   int i, usrargs=0;

   *DoISA = *DoArch = *DoOS = *DoComps = 0;
   *abeg = 0;
   *aend = NMACH-1;
   *osbeg = 0;
   *osend = NOS-1;
   *cbeg = 0;
   *cend = NCOMP-1;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i);
      switch(args[i][1])
      {
      case 'a':
         *DoArch = usrargs = 1;
         break;
      case 'o':
         *DoOS = usrargs = 1;
         break;
      case 'c':
         *DoComps = usrargs = 1;
         break;
      case 'i':
         *DoISA = usrargs = 1;
         break;
      case 'C':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *cbeg = atoi(args[i]);
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *cend = atoi(args[i]);
         usrargs = *DoComps = 1;
         break;
      case 'A':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *abeg = atoi(args[i]);
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *aend = atoi(args[i]);
         usrargs = *DoArch = 1;
         break;
      case 'O':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *osbeg = atoi(args[i]);
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *osend = atoi(args[i]);
         usrargs = *DoOS = 1;
         break;
      default:
         PrintUsage(args[0], i);
      }
   }
   if (!usrargs)
      *DoISA = *DoArch = *DoOS = *DoComps = 1;
}

int main(int nargs, char **args)
{
   int DoArch, abeg, aend, DoOS, osbeg, osend, DoComp, cbeg, cend, DoISA;

   GetFlags(nargs, args, &DoArch, &abeg, &aend, &DoOS, &osbeg, &osend,
            &DoComp, &cbeg, &cend, &DoISA);

   fprintf(stdout, "\n");
   if (DoArch)
      BarfOutArchs(stdout, abeg, aend);
   if (DoOS)
      BarfOutOSs(stdout, osbeg, osend);
   if (DoComp)
      BarfOutComps(stdout, cbeg, cend);
   if (DoISA)
      BarfOutISAs(stdout);
   return(0);
}
