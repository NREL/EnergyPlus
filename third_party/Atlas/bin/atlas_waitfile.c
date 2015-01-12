#include <stdio.h>
#include <stdlib.h>

void PrintUsage(char *nam)
{
   fprintf(stderr, "\n\nUSAGE: %s -f <filename> [-s <nsec>]\n\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, int *nsec, char **fnam)
{
   int i;

   *nsec = 5;
   *fnam = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'f':
         *fnam = args[++i];
         break;
      case 's':
         *nsec = atoi(args[++i]);
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   if (*fnam == NULL) PrintUsage(args[0]);
}

int WaitForIt(int nsec, char *fnam)
/*
 * only probe for file once every 1/5 of a second to avoid beating the crap
 * out of NFS server
 */
{
   FILE *fp;
   double t0, t1, dsec=(double)nsec, dwait;
   double ATL_walltime(void);

   fp = fopen(fnam, "r");
   if (!fp)
   {
      t0 = ATL_walltime();
      do
      {
         t1 = ATL_walltime();
         while(ATL_walltime()-t1 < 0.2);
         if (ATL_walltime()-t0 > dsec) return(1);
         fp = fopen(fnam, "r");
      }
      while(!fp);
   }
   fclose(fp);
   return(0);
}

int main(int nargs, char **args)
/*
 * This routine waits at last nsecs for a file to appear; used to get
 * around NFS probs, particularly when cross-compiling
 */
{
   int nsec, i;
   char *fnam;

   GetFlags(nargs, args, &nsec, &fnam);
   i = WaitForIt(nsec, fnam);
   if (i) fprintf(stderr, "\n\nTimeout waiting for file %s!!\n\n", fnam);
   return(0);
}
