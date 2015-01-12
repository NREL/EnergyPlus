#include "atlas_tvec.h"

void PrintUsage(char *name, char *arg, int i)
{
   fprintf(stderr, "This routine gets rid of the repititions within vectors\n");
   fprintf(stderr, "Vectors are *reduced* or *combined*:\n");
   fprintf(stderr, "   reduced vectors should be repeats (eg 100, 100)\n");
   fprintf(stderr,
      "   combined vectors are combined and produce several vectors:\n");
   fprintf(stderr, "      v_1...v_<nreps>, v_avg, v_min, v_max\n");
   fprintf(stderr,
   "   any vector not collapsed or combined does not appear in the output\n");
   if (i > 0)
      fprintf(stderr, "BAD ARG '%s' ON %dth FLAG\n", arg, i);
   fprintf(stderr, "USAGE: %s <flags> ; flags include:\n", name);
   fprintf(stderr, "   -i <file> : (stdin) file with vecs to reduce\n");
   fprintf(stderr, "   -o <file>  : (stdout) file for reduced vecs\n");
   fprintf(stderr, "   -R # <nam1> ... <nam#>: vectors to collapse\n");
   fprintf(stderr, "   -C # <nam1> ... <nam#>: vectors to combine\n");
   fprintf(stderr, "   -c [+,<,>,a] : specify how to combine repeated elts:\n");
   fprintf(stderr, "      +: repeated vector replaced by average of repeats\n");
   fprintf(stderr, "      <: repeated vector replaced by minimum of repeats\n");
   fprintf(stderr, "      >: repeated vector replaced by maximum of repeats\n");
   exit (i ? i : -1);
}

char **GetFlags         /* RETURNS: array of names to combine/reduce */
(
   int nargs,
   char **args,
   int *ncomb,          /* # of vecs to combine, stored in 1st ncomb elts */
   int *nred,           /* # of vecs to reduce, stored at end of ret array */
   FILE **fpin,         /* input stream */
   FILE **fpout         /* output stream */
)
{
   char **vc=NULL, **vr=NULL, **vv, *sp;
   int i, j, n, nc=0, nr=0;
   FILE *fp;

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
         nc = atoi(args[i]);
         assert(nc > 0 && nc < 2048);
         vc = malloc(sizeof(char*)*nc);
         assert(vc);
         for (j=0; j < nc; j++)
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
   if (!nr && !nc)
   {
      nr = nc = 1;
      vv = malloc(2*sizeof(char*));
      assert(vv);
      vv[0] = "MFLOP";
      vv[1] = "N";
   }
   else
   {
      n = nr + nc;
      vv = malloc(n*sizeof(char*));
      assert(vv);
      for (i=0; i < nc; i++)
         vv[i] = vc[i];
      if (vc)
         free(vc);
      if (vr)
      {
         for (; i < n; i++)
            vv[i] = vr[i-nc];
         free(vr);
      }
   }
   *ncomb = nc;
   *nred = nr;
   return(vv);
}

int main(int nargs, char **args)
{
   FILE *fpin, *fpout;
   char **redarr, **combarr, *cmnt;
   int N, Nc, Nr, i, j, RNGINC=0, nrep;
   ATL_tvec_t *tr, *tc, *tp, *np, *nb=NULL;

   combarr = GetFlags(nargs, args, &Nc, &Nr, &fpin, &fpout);
   redarr = combarr + Nc;

/*
 * Grab only the vectors to be combined and reduced (in the order the user
 * has specified) from list, and free all unused vectors
 */

   np = ATL_ReadTvecFile(fpin, &cmnt, &N, &nrep);
   if (fpin != stdin)
      fclose(fpin);
   tc = ATL_PullNamedVecsFromList(Nc, combarr, &np);
   tr = ATL_PullNamedVecsFromList(Nr, redarr, &np);
   if (np)
      ATL_KillAllTvecs(np);
/*
 * Create all individual run vectors, add to new list
 */
   for (tp=tc; tp; tp = tp->next)
   {
      ATL_tvec_t *p;
      p = ATL_SplitRepsVector(tp);
      ATL_FindLastVecInList(p)->next = nb;
      nb = p;
   }
/*
 * Create all statistic vectors in queue
 */
   for (tp=tc; tp; tp = tp->next)
   {
      ATL_tvec_t *p;
      assert(tp->pre == 'd');         /* relax this later if needed */
      p = ATL_GetStatVecsDOUBLE(tp);
      p->next->next->next = nb;
      nb = p;
   }
   ATL_KillAllTvecs(tc);
/*
 * Now reduce any repeated vectors
 */
   for (tp=tr; tp; tp = tp->next)
   {
      ATL_tvec_t *p;
      int i;
      char *sp;

      p = ATL_GetRep1Vector(tp, 0);
      p->next = nb;
      nb = p;

      for (sp=p->name,i=0; sp[i] != '_'; i++);   /* fix name back to orig */
      sp[i] = '\0';                              /* from name_0 */
   }
   ATL_KillAllTvecs(tr);

   ATL_WriteTvecFile(fpout, cmnt, ATL_CountTVecsInList(nb), 1, nb);
   ATL_KillAllTvecs(nb);
   free(cmnt);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
   return(0);
}
