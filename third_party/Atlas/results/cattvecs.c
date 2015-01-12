#include "atlas_tvec.h"

void PrintUsage(char *name, char *arg, int i)
{
   fprintf(stderr,
"This routine takes tvecs in multiple files and puts then into one\n");
   if (i > 0)
      fprintf(stderr, "BAD ARG '%s' ON %dth FLAG\n", arg, i);
   fprintf(stderr, "USAGE: %s <flags> ; flags include:\n", name);
   fprintf(stderr, "   -i <file> : (stdin) file with vecs to read\n");
   fprintf(stderr,
           "   -# <#> : (2) # of tvec files concatonated in input file\n");
   fprintf(stderr, "   -o <file>  : (stdout) output file for all tvecs\n");
   fprintf(stderr,
      "   -C # <nam1> ... <nam#>: vectors coming from all files\n");
   fprintf(stderr,
      "   -c # <nam1> ... <nam#>: vectors where we take first instance only\n");
   exit (i ? i : -1);
}

char **GetFlags         /* RETURNS: array of single/repeated names */
(
   int nargs,
   char **args,
   int *Nfiles,         /* # of tvec files in input stream */
   int *none,           /* # of vecs where we take only 1st definition */
   int *nmul,           /* # of vecs where we take all vectors of that name */
   FILE **fpin,         /* input stream */
   FILE **fpout         /* output stream */
)
{
   char **vc=NULL, **vr=NULL, **vv, *sp;
   int i, j, n, nc=0, nr=0;
   FILE *fp;

   *fpin = stdin;
   *fpout = stdout;
   *Nfiles = 2;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], "no '-' preceeding flag!", i);
      switch(args[i][1])
      {
      case '#':    /* -# <# files> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -i ", i-1);
         *Nfiles = atoi(args[i]);
         break;
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
      case 'c':    /* -R # <nam1> ... <nam#> */
         if (++i >= nargs)
            PrintUsage(args[0], "out of flags in -c ", i-1);
         nr = atoi(args[i]);
         assert(nr > 0 && nr < 2048);
         vr = malloc(sizeof(char*)*nr);
         assert(vr);
         for (j=0; j < nr; j++)
         {
            if (++i >= nargs)
               PrintUsage(args[0], "out of flags in -c ", i-1);
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
      vv[0] = "N";
      vv[1] = "MFLOP";
   }
   else
   {
      n = nr + nc;
      vv = malloc(n*sizeof(char*));
      assert(vv);
      for (i=0; i < nr; i++)
         vv[i] = vr[i];
      if (vr)
         free(vr);
      if (vc)
      {
         for (; i < n; i++)
            vv[i] = vc[i-nr];
         free(vc);
      }
   }
   *none = nr;
   *nmul = nc;
   return(vv);
}

int main(int nargs, char **args)
{
   FILE *fpin, *fpout;
   char **vnams1, **vnamsr, *cmnt;
   int Nf, N1, Nr, N, i, j, RNGINC=0, nrep;
   ATL_tvec_t *tp, *np, *nb=NULL, *rb=NULL;

   vnams1 = GetFlags(nargs, args, &Nf, &N1, &Nr, &fpin, &fpout);
   vnamsr = vnams1 + N1;

/*
 * Grab one copy of all vectors from output
 */
   tp = ATL_ReadTvecFile(fpin, &cmnt, &N, &nrep);
   N = N1 + Nr;
   nb = ATL_PullNamedVecsFromList(N, vnams1, &tp);
   assert(nb);
   if (tp)
      ATL_KillAllTvecs(tp);
/*
 * Get repeated vectors from all files
 */
   for (i=1; i < Nf; i++)
   {
      char suff[16];

      tp = ATL_ReadTvecFile(fpin, &cmnt, &N, &nrep);
      np = ATL_PullNamedVecsFromList(Nr, vnamsr, &tp);
      if (!np)
         break;
      sprintf(suff, "_%d", i);
      ATL_SuffixTvecNames(np, suff);
      ATL_FindLastVecInList(nb)->next = np;
      if (tp)
         ATL_KillAllTvecs(tp);
   }
   if (fpin != stdin)
      fclose(fpin);
/*
 * Write them out, and we are done
 */
   ATL_WriteTvecFile(fpout, cmnt, ATL_CountTVecsInList(nb), 1, nb);
   ATL_KillAllTvecs(nb);
   free(cmnt);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
   return(0);
}
