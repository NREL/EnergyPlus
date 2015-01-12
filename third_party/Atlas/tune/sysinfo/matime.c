#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#ifdef QREAL
   #define TYPE long double
#elif defined(SREAL)
   #define TYPE float
#else
   #define TYPE double
#endif
#ifndef NTIM
   #define NTIM 3
#endif
#if defined(PentiumCPS) || defined(WALL)
   #define time00 ATL_walltime
#else
   #define time00 ATL_cputime
#endif
double time00();
double macase(int nrep, volatile TYPE *dum);

void PrintUsage(char *xnam)
{
   fprintf(stderr, "USAGE: %s -m <mflop>, -f <outfile> -t <0/1>\n", xnam);
   fprintf(stderr, "    -m x: do x MFLOPS of computation\n");
   fprintf(stderr, "   -f <outf>: write results to file outf\n");
   fprintf(stderr, "   -t 0: print mflop rates, else print times\n");
   exit(-1);
}

FILE *GetFlags(int nargs, char **args, double *MFLOPS, int *TIME)
{
   double mf=200;
   FILE *fpout = stdout;
   int i;

   *TIME = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 't':
         if (++i == nargs)
            PrintUsage(args[0]);
         *TIME = atoi(args[i]);
         break;
      case 'm':
         if (++i == nargs)
            PrintUsage(args[0]);
         mf = atof(args[i]);
         break;
      case 'f':
         if (++i == nargs)
            PrintUsage(args[0]);
         fpout = fopen(args[i], "w");
         assert(fpout);
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   *MFLOPS = mf;
   return(fpout);
}

int main(int nargs, char **args)
{
   FILE *fpout;
   TYPE dum[32];
   double lmf, mf, flops;
   char pre;
   int i, nrep, PTIME;

   fpout = GetFlags(nargs, args, &mf, &PTIME);
   for (i=0; i < 32; i++)
      dum[i] = 0.0;
   lmf = macase(1, dum);
   nrep = mf*1000000.0 / lmf;
   if (nrep < 0)
      nrep = 1;
   for (i=0; i < NTIM; i++)
   {
      double t0, flops;
      t0 = time00();
      flops = macase(nrep, dum);
      t0 = time00() - t0;
      mf = flops / (1000000.0*t0);
      if (PTIME)
         fprintf(fpout, "%.e\n", t0);
      else
         fprintf(fpout, "%.2f\n", mf);
   }
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
   return(0);
}
