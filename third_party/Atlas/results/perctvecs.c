#include "atlas_tvec.h"

void PrintUsage(char *name, char *arg, int i)
{
   fprintf(stderr,
           "This routine computes tvecs as percentages of a base tvec\n");
   fprintf(stderr, "   -P vectors are replaced by precentages of the -b vec\n");
   if (i > 0)
      fprintf(stderr, "BAD ARG '%s' ON %dth FLAG\n", arg, i);
   fprintf(stderr, "USAGE: %s <flags> ; flags include:\n", name);
   fprintf(stderr, "   -i <file> : (stdin) input file\n");
   fprintf(stderr, "   -o <file>  : (stdout) output file\n");
   fprintf(stderr,
           "   -R # <nam1> ... <nam#>: vectors to replace with percentages\n");
   fprintf(stderr, "   -C # <nam1> ... <nam#>: vectors to keep unchanged\n");
   fprintf(stderr, "   -b <nam> : vector to use as percentage base\n");
   fprintf(stderr,
"   -m <mul> : multiplier for ratio (default 100.0 for %%; 1 for speedup)\n");
   exit (i ? i : -1);
}

char **GetFlags         /* RETURNS: array of names to combine/reduce */
(
   int nargs,
   char **args,
   int *nkeep,          /* # of vecs to keep unchanged stored at 1 of arr */
   int *nperc,          /* # of vecs to make %, stored at end of ret array */
   double *mul,         /* multiplier for ratio; set to 100.0 for % */
   FILE **fpin,         /* input stream */
   FILE **fpout         /* output stream */
)
{
   char **vc=NULL, **vr=NULL, **vv, *sp, *base="MFLOP_avg";
   int i, j, n, nk=0, nr=0;
   FILE *fp;

   *mul = 100.0;
   *fpin = stdin;
   *fpout = stdout;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], "no '-' preceeding flag!", i);
      switch(args[i][1])
      {
      case 'i':    /* -i <file> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -i ", i-1);
         *fpin = fopen(args[i], "r");
         assert(*fpin);
         break;
      case 'o':    /* -o <file> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -i ", i-1);
         fp = fopen(args[i], "w");
         assert(fp);
         *fpout = fp;
         break;
      case 'b':
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -R ", i-1);
         base = args[i];
         break;
      case 'm':
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -m ", i-1);
         *mul = atof(args[i]);
         break;
      case 'R':    /* -R # <nam1> ... <nam#> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -R ", i-1);
         nr = atoi(args[i]);
         assert(nr > 0 && nr < 2048);
         vr = malloc(sizeof(char*)*nr);
         assert(vr);
         for (j=0; j < nr; j++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], "out of flags in -R ", i-1);
            vr[j] = args[i];
         }
         break;
      case 'C':    /* -C # <nam1> ... <nam#> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -C ", i-1);
         nk = atoi(args[i]);
         assert(nk > 0 && nk < 2048);
         vc = malloc(sizeof(char*)*nk);
         assert(vc);
         for (j=0; j < nk; j++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], "out of flags in -C ", i-1);
            vc[j] = args[i];
         }
         break;
      default :
         PrintUsage(args[0], args[i], i);
      }                                         /* end switch over flags */
   }                                            /* end for over flags */
   if (nr < 1)
   {
      fprintf(stderr, "Must reduce at least one vector to percentages!\n");
      exit(-1);
   }
   n = nr + nk + 1;
   vv = malloc(n*sizeof(char*));
   assert(vv);
   vv[0] = base;
   for (i=0; i < nk; i++)
      vv[i+1] = vc[i];
   if (vc)
      free(vc);
   if (vr)
   {
      for (; i < n-1; i++)
         vv[i+1] = vr[i-nk];
      free(vr);
   }
   *nkeep = nk;
   *nperc = nr;
   return(vv);
}

int main(int nargs, char **args)
{
   FILE *fpin, *fpout;
   char **redarr, **keeparr, *cmnt, *basev;
   int N, Nk, Nr, nrep, i;
   double mul;
   ATL_tvec_t *tr, *tk, *tp, *tb, *np;

   keeparr = GetFlags(nargs, args, &Nk, &Nr, &mul, &fpin, &fpout);
   basev = keeparr[0];
   keeparr++;
   redarr = keeparr + Nk;

/*
 * Grab only the vectors to be combined and reduced (in the order the user
 * has specified) from list, and free all unused vectors
 */

   np = ATL_ReadTvecFile(fpin, &cmnt, &i, &nrep);
   if (fpin != stdin)
      fclose(fpin);
   tb = ATL_PullNamedVecsFromList(1, &basev, &np);
   tr = ATL_PullNamedVecsFromList(Nr, redarr, &np);
   tk = ATL_PullNamedVecsFromList(Nk, keeparr, &np);
   if (np)
      ATL_KillAllTvecs(np);
/*
 * Create all statistic vectors in queue
 */
   for (tp=tr; tp; tp = tp->next)
   {
      double *dp=tp->vp, *db=tb->vp;
      assert(tp->pre == 'd');
      for (N=tp->N,i=0; i < N; i++)
         dp[i] = (dp[i] / db[i])*mul;
   }
/*
 * Link lists back up, and write to file
 */
   ATL_FindLastVecInList(tr)->next = tb;
   ATL_FindLastVecInList(tk)->next = tr;
   ATL_WriteTvecFile(fpout, cmnt, ATL_CountTVecsInList(tk), 1, tk);
   ATL_KillAllTvecs(tk);
   free(cmnt);
   free(--keeparr);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
   return(0);
}
