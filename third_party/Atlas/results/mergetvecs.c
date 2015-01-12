#include "atlas_tvec.h"

void PrintUsage(char *name, char *arg, int i)
{
   fprintf(stderr,
"This routine takes two vector files, and combines a list of named vectors\n");
   fprintf(stderr,
"so they form one contiguous range.  The range vector is specified by -r,\n");
   fprintf(stderr, "and these vectors should not overlap.\n\n");
   if (i > 0)
      fprintf(stderr, "BAD ARG '%s' ON %dth FLAG\n", arg, i);
   fprintf(stderr, "USAGE: %s <flags> ; flags include:\n", name);
   fprintf(stderr, "   -i1 <file> : (stdin) 1st file wt vecs to combine\n");
   fprintf(stderr, "   -i2 <file> : (stdin) 2nd file wt vecs to combine\n");
   fprintf(stderr, "   -o <file>  : (stdout) file for combined vecs\n");
   fprintf(stderr,
           "   -r <name>  : (\"N\"): range vector to sort combine on\n");
   fprintf(stderr, "   -C # <nam1> ... <nam#>: vectors to combine\n");
   exit (i ? i : -1);
}

char **GetFlags         /* RETURNS: array of names to combine */
(
   int nargs,
   char **args,
   FILE **fp1,          /* 1st input stream */
   FILE **fp2,          /* 2nd input stream */
   FILE **fpout,        /* output stream */
   char **rngv,         /* name of range/sort vector */
   int *Nc              /* # of vectors to combine using rngv */
)
{
   char **na=NULL, *sp;
   int i, j, n;
   FILE *fp;

   *fp1 = *fp2 = stdin;
   *fpout = stdout;
   *rngv = "N";
   *Nc = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], "no '-' preceeding flag!", i);
      switch(args[i][1])
      {
      case 'i':    /* -i[1,2] <file> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -i ", i-1);
         fp = fopen(args[i], "r");
         assert(fp);
         if (args[i-1][2] == '1')
            *fp1 = fp;
         else
            *fp2 = fp;
         break;
      case 'o':    /* -o <file> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -i ", i-1);
         fp = fopen(args[i], "w");
         assert(fp);
         *fpout = fp;
         break;
      case 'r':    /* -r <name> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -i ", i-1);
         *rngv = args[i];
         break;
      case 'C':    /* -C # <nam1> ... <nam#> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -C ", i-1);
         *Nc = n = atoi(args[i]);
         assert(n > 0 && n < 2048);
         na = malloc(sizeof(char*)*n);
         assert(na);
         for (j=0; j < n; j++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], "out of flags in -C ", i-1);
            na[j] = args[i];
         }
         break;
      default :
         PrintUsage(args[0], args[i], i);
      }                                         /* end switch over flags */
   }                                            /* end for over flags */
   if (*Nc == 0)
   {
      *Nc = 1;
      na = malloc(sizeof(char*));
      assert(na);
      na[0] = *rngv;
   }
//   assert(*fp1 != *fp2);
   return(na);
}

int main(int nargs, char **args)
{
   FILE *fp1, *fp2, *fpout;
   char *rngv, **combarr, *cmnt;
   int N, Nc, i, j, RNGINC=0, nrep;
   ATL_tvec_t *t1, *t2, *tp, *p1, *p2, *r1, *r2, *cb, *cp;

   combarr = GetFlags(nargs, args, &fp1, &fp2, &fpout, &rngv, &Nc);
/*
 * Grab only the vectors to be combined, in order user has specified from
 * both lists, and free all unused vectors
 */
   t1 = ATL_ReadTvecFile(fp1, &cmnt, &N, &nrep);
   if (fp1 != stdin)
      fclose(fp1);
   tp = ATL_PullNamedVecsFromList(Nc, combarr, &t1);
   if (t1)
      ATL_KillAllTvecs(t1);
   t1 = tp;

   t2 = ATL_ReadTvecFile(fp2, &cmnt, &i, &j);
   if (fp2 != stdin)
      fclose(fp2);
   tp = ATL_PullNamedVecsFromList(Nc, combarr, &t2);
   if (t2)
      ATL_KillAllTvecs(t2);
   t2 = tp;
/*
 * Find range vectors
 */
   r1 = ATL_FindTvecByName(t1, rngv);
   r2 = ATL_FindTvecByName(t2, rngv);
   assert(r1->pre == 'i' && r2->pre == 'i');  /* restriction to fix later */
/*
 * Combine all vectors and build new list cb
 */
   for (cb=NULL, p1=t1, p2=t2; p1 && p2; p1 = p1->next, p2 = p2->next)
   {
      ATL_tvec_t *p;

      assert(!strcmp(p1->name, p2->name));
      assert(p1->nrep == p2->nrep);
      p = ATL_CombineTheseVecsUsingInts(r1, r2, p1, p2);
      if (cb)
         cp->next = p;
      else
         cb = p;
      cp = p;
   }
   ATL_KillAllTvecs(t1);
   ATL_KillAllTvecs(t2);
   ATL_WriteTvecFile(fpout, cmnt, Nc, nrep, cb);
   ATL_KillAllTvecs(cb);
   free(cmnt);
#if 0
   printf("rngv = '%s'\n", rngv);
   for (i=0; i < Nc; i++)
      printf("nam[%d] = '%s'\n", i, combarr[i]);
#endif
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
   return(0);
}
