#include "atlas_misc.h"
#include "atlas_r1parse.h"


void Mflop2Perc(ATL_r1node_t *kp, int imf, double mf)
{
   for (; kp; kp = kp->next)
      kp->mflop[imf] /= mf;
}

void PrintMUxNU_CSV
(
   FILE *fpout,         /* file to print to */
   ATL_r1node_t *kp, /* pointer to kernel queue */
   int imf,             /* mflop value to print out */
   int PERC             /* nonzero: convert mflop to % of 1st case */
)
/*
 * Builds a 2-D output CSV, with MU along the rows,
 * and NU along the columns, and MFLOP inside the matrix.  Builds a second
 * matrix with routine name in the matrix as well.
 */
{
   int *mus, *nus;
   char **names;
   ATL_r1node_t *bp, *maxp, *minp, *p;
   int i, j, n, mmin, mmax, nmin, nmax, nmu, nnu;
   double mf1=0.0;

   bp = CloneR1Queue(kp);
   assert(bp);
/*
 * Discover max size of each dim of 2-D table, and allocate space to store
 * the MU/NU labels
 */
   n = ATL_CountNumberOfR1Nodes(bp);
   mmin = FindMinIntInR1Q(bp, &bp->XU)->XU;
   mmax = FindMaxIntInR1Q(bp, &bp->XU)->XU;
   nmin = FindMinIntInR1Q(bp, &bp->YU)->YU;
   nmax = FindMaxIntInR1Q(bp, &bp->YU)->YU;
   nmu = mmax - mmin;
   nnu = nmax - nmin;
   nmu = Mmin(nmu, n);
   nnu = Mmin(nnu, n);
   mus = malloc((nnu+nmu)*sizeof(int));
   assert(mus);
   nus = mus + nmu;

/*
 * Yank out all nodes that have minimum XU/MU, use this row to compute all the
 * legal YU/NUs, which are stored as our column headings (nus)
 */
   minp = YankR1NodesByIntVal(&bp, &bp->XU, mmin);  /* nodes wt min(XU) */
   assert(minp);
   minp = SortR1QByIntVal(minp, &minp->YU); /* sort least-to-great on YU */
   fprintf(fpout, "&&XU/YU");
   for (p=minp,nnu=0; p; p = p->next)            /* look thru all YU nodes */
   {
      nus[nnu++] = p->YU;                        /* and label Y axis */
      fprintf(fpout, "&%d", p->YU);              /* print label */
   }
   fprintf(fpout, "\n");
   names = malloc((nnu*nmu)*sizeof(char*));
   assert(names);
/*
 * Iteratively find the minimum XU, and then build a row of the table
 * by yanking out all entries with that XU value, and putting them in
 * a new queue
 */
   nmu = 0;
   do
   {
      fprintf(fpout, "&");
      fprintf(fpout, "&%d", minp->XU);
      for (j=0, p=minp; p; j++, p = p->next)
      {
         names[nmu*nnu+j] = DupString(p->rout);
         assert(p->YU == nus[j]);
         assert(p->XU == mmin);
         if (PERC)
         {
            if (mf1 == 0.0)
               mf1 = p->mflop[imf];
            fprintf(fpout, "&%.4f", p->mflop[imf]/mf1);

         }
         else
            fprintf(fpout, "&%.2f", p->mflop[imf]);
      }
      fprintf(fpout, "\n");
      mus[nmu++] = minp->XU;
      KillAllR1Nodes(minp);
      if (!bp)
         break;
      mmin = FindMinIntInR1Q(bp, &bp->XU)->XU;
      minp = YankR1NodesByIntVal(&bp, &bp->XU, mmin);
      assert(minp);
      minp = SortR1QByIntVal(minp, &minp->YU);
   }
   while (minp);
/*
 * If we printed percentages, give the base MFLOP rate
 */
   if (PERC)
      fprintf(fpout, "\n\nBASE MFLOP&%.2f\n", mf1);

/*
 * Print names of all routs as error check on grabbing the right kernels
 */
   fprintf(fpout, "\n\n\n\n\n\nNAME MATRIX:\n");
   fprintf(fpout, "&&XU/YU");
   for (j=0; j < nnu; j++)
      fprintf(fpout, "&%d", nus[j]);
   fprintf(fpout, "\n");
   for (i=0; i < nmu; i++)
   {
      fprintf(fpout, "&&%d", mus[i]);
      for (j=0; j < nnu; j++)
         fprintf(fpout, "&%s", names[i*nnu+j]);
      fprintf(fpout, "\n");
   }

   j = nmu * nnu;
   for (i=0; i < j; i++)
      free(names[j]);
   free(names);
   free(mus);
}

void Dump2CSV
(
   FILE *fpout,         /* file to print to */
   ATL_r1node_t *kb     /* pointer to kernel queue */
)
/*
 * Simply dumps all kp to CSV format, with all parameters present in
 * columns (col label is routine name)
 */
{
   ATL_r1node_t *kp;
   fprintf(fpout, "&&OOCL1&OOCL2&OOC&inL2&inL1\n");
   fprintf(fpout, "&NAME&MFLOP&MFLOP&MFLOP&MFLOP&MFLOP&MU&NU&CacheElts\n");
   for (kp=kb; kp; kp = kp->next)
   {
      fprintf(fpout, "&%s&%.2f&%.2f&%.2f&%.2f&%.2f&%.2f&%d&%d&%d\n",
              kp->rout, kp->mflop[0], kp->mflop[1], kp->mflop[2], kp->mflop[3],
              kp->mflop[4], kp->mflop[5], kp->XU, kp->YU, kp->CacheElts);
   }
}

void Dump2RowI
(
   FILE *fpout,                 /* file to print to */
   ATL_r1node_t *kb,         /* pointer to kernel queue */
   int ioff,                    /* offset of int param to put on rows */
   int imf,                     /* which mflop index to dump */
   int PERC                     /* dump percentages rather than mflop? */
)
/*
 * Simply dumps all kb to a single row with only the integer para ioff varying
 */
{
   double mfbase;
   ATL_r1node_t *kp;
   int *ip;
   char *cp;

   if (PERC)
   {
      mfbase = kb->mflop[imf];
      assert(mfbase > 0.0);
      fprintf(fpout, "BASE MFLOP=%e\n\n", mfbase);
   }
/*
 * Print headers of int value of ioff
 */
   fprintf(fpout, "IVAL (%d):&", ioff);
   for (kp=kb; kp; kp = kp->next)
   {
      ip = (int*)(((char*)kp)+ioff);
      fprintf(fpout, "&%d", *ip);
   }
/*
 * Print selected mflop values
 */
   fprintf(fpout, "\nmflop[%d]:&", imf);
   for (kp=kb; kp; kp = kp->next)
   {
      if (PERC)
         fprintf(fpout, "&%.4f", kp->mflop[imf]/mfbase);
      else
         fprintf(fpout, "&%.2f", kp->mflop[imf]);
   }
}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);
   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -m #: [0] mflop entry to use\n");
   fprintf(stderr,
           "   -p #: [0] nonzero -- convert mflops to %% of first case\n");
   fprintf(stderr,
      "   -D 0/1/2 [field [field]]: [0] control output CSV file:)\n");
   fprintf(stderr, "      0: straight dump along rows of all kernels\n");
   fprintf(stderr,
      "      1 [field]: [1 YU]: put all kerns with same YU along row\n");
   fprintf(stderr,
      "      1 [field [field]]: [2 YU XU]: 2-D matrix, YU along rows\n");

   fprintf(stderr, "      Right now, the only fields accepted or XU & YU\n");
   fprintf(stderr, "-F field val : delete all kernels where field != val\n");
   fprintf(stderr,
   "   -k # : [0] # of kernels to skip to get to first kernel of interest\n");
   fprintf(stderr, "   -s # : [1] stride between kernels of interest\n");
   fprintf(stderr, "   -n # : [0] # of kernels of interest\n");
   fprintf(stderr, "   -i <file> : [stdin] input file (output from timer)\n");
   fprintf(stderr, "   -o <file> : [tmp.csv] output file (CSV format)\n");
   exit(ierr ? ierr : -1);
}

int Name2Offset(char *name)
{
   ATL_r1node_t kp;
   int off = -1;
   if (!strcmp(name, "XU") || !strcmp(name, "MU"))
      off = (int)(((char*)(&kp.XU)) - ((char*)&kp));
   else if (!strcmp(name, "YU") || !strcmp(name, "NU"))
      off = (int)(((char*)(&kp.YU)) - ((char*)&kp));
   else if (!strcmp(name, "ID"))
      off = (int)(((char*)(&kp.ID)) - ((char*)&kp));
   return(off);
}

ATL_r1node_t *GetFlags
(
   int nargs, char **args,
   int *off1,   /* byte offset in structure to X axis dimension */
   int *off2,   /* byte offset in structure to Y axis dimension */
   int *imf,    /* mflop entry to use */
   int *perc,   /* non-zero : translate MFLOP to percentage of 1st case */
   FILE **FPOUT /* output file pointer */
)
/*
 * RETURNS: list of kernels read in from file, with any filtering applied
 */
{
   FILE *fpin=stdin;
   ATL_r1node_t *kb=NULL, *kp, *kn=NULL;
   ATL_sidnode_t *wp, *wb=NULL;
   int i, k, j, iskip=0, stride=1, off;

   *perc = 0;
   *FPOUT = stdout;
   *off1 = *off2 = -1;
//   *off1 = Name2Offset("YU");
//   *off2 = Name2Offset("XU");
   *imf = 0;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'D' : /* -D 1/2 field [field] */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         j = atoi(args[i]);
         if (j == 0)
         {
            *off1 = *off2 = -1;
            break;
         }
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *off1 = Name2Offset(args[i]);
         assert(*off1 >= 0);
         if (j > 1)
         {
            if (++i >= nargs)
               PrintUsage(args[0], i-1, NULL);
            *off2 = Name2Offset(args[i]);
            assert(*off2 >= 0);
         }
         break;
      case 'F' : /* -F field val */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         wp = ATL_NewSIDNode();
         wp->str = DupString(args[i]);
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         wp->i = atoi(args[i]);
         wp->next = wb;
         wb = wp;
         break;
      case 'p' : /* -p # */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *perc = atoi(args[i]);
         break;
      case 'm' : /* -n # */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *imf = atoi(args[i]);
         break;
      case 'n' : /* -n # */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         iskip = atoi(args[i]);
         assert(iskip >= 0);
         break;
      case 's' : /* -s # */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         stride = atoi(args[i]);
         assert(stride >= 0);
         break;
      case 'i' : /* -i <infile> */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         kb = ReadR1File(args[i]);
         assert(kb);
         break;
      case 'o' : /* -o <outfile> */
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *FPOUT = fopen(args[i], "w");
         assert(*FPOUT);
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   assert(kb);
/*
 * Weed out initial skip & strided kernels from kb
 */
   for (i=0; i < iskip && kb; i++)
      kb = KillR1Node(kb);
   if (stride > 1 && kb)
   {
      kp = CloneStridedR1Queue(kb, stride);
      KillAllR1Nodes(kb);
      kb = kp;
   }
/*
 * Weed out any kernels excluded by -F
 */
   if (wb)
   {
      for (wp=wb; wp && kb; wp = wp->next)
      {
         off = Name2Offset(wp->str);
         kp = FindIntValInR1Q(kb, ((char*)kb)+off, wp->i);
         if (kp)
         {
            kb = RemoveR1NodeFromQ(kb, kp);
            kp->next = kn;
            kn = kp;
         }
      }
      if (kb)
         KillAllR1Nodes(kb);
      kb = kn;
   }
   assert(kb);
   return(kb);
}

int main(int nargs, char **args)
{
   ATL_r1node_t *kb, *kp;
   FILE *fpout;
   int off1, off2, imf, PERC;

   kb = GetFlags(nargs, args, &off1, &off2, &imf, &PERC, &fpout);
   if (off1 < 0 && off2 < 0)
      Dump2CSV(fpout, kb);
   else if (off2 < 0) /* dump 1-D stuff to rows wt only off1 varying */
      Dump2RowI(fpout, kb, off1, imf, PERC);
   else /* if (off1 == Name2Offset("YU") && off2 == Name2Offset("XU")) */
      PrintMUxNU_CSV(fpout, kb, imf, PERC);

   KillAllR1Nodes(kb);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);

   return(0);
}
