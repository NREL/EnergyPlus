#include <stdio.h>
#include <stdlib.h>
#include "assert.h"
#include "atlas_r1parse.h"
void PrintUsage(char *name)
{
   fprintf(stderr,
           "USAGE: %s <files> : negate mflops in standard R1 files\n",
           name);
   exit(-1);
}
int main(int nargs, char **args)
{
   int i, k;
   double *mfs;
   ATL_r1node_t *p, *pb;
   if (nargs < 2)
      PrintUsage(args[0]);
   for (i=1; i < nargs; i++)
   {
      pb = ReadR1File(args[i]);
      assert(pb);
      for (p=pb; p; p = p->next)
      {
         mfs = p->mflop;
         for (k=0; k < 8; k++)
            mfs[k] = (mfs[k] > 0) ? -mfs[k] : mfs[k];
      }
      ResubGoodGccInR1Nodes(pb);
      WriteR1File(args[i], pb);
      KillAllR1Nodes(pb);
   }
   exit(0);
}
