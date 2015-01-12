#include <stdio.h>
#include <stdlib.h>
#include "assert.h"
#include "atlas_mvparse.h"
void PrintUsage(char *name)
{
   fprintf(stderr,
           "USAGE: %s <files> : negate mflops in standard MV files\n",
           name);
   exit(-1);
}
int main(int nargs, char **args)
{
   int i, k;
   double *mfs;
   ATL_mvnode_t *p, *pb;
   if (nargs < 2)
      PrintUsage(args[0]);
   for (i=1; i < nargs; i++)
   {
      pb = ReadMVFile(args[i]);
      assert(pb);
      for (p=pb; p; p = p->next)
      {
         mfs = p->mflop;
         for (k=0; k < 8; k++)
            mfs[k] = (mfs[k] > 0) ? -mfs[k] : mfs[k];
      }
      ResubGoodGccInMVNodes(pb);
      WriteMVFile(args[i], pb);
      KillAllMVNodes(pb);
   }
   exit(0);
}
