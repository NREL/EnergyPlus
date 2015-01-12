#include "atlconf.h"

/*
 * The number of results, and their ordering (this must match the file).
 */
#define NBENCH 8
#define SELKMM 0
#define GENKMM 1
#define KMM_NT 2
#define KMM_TN 3
#define KMV_N  4
#define KMV_T  5
#define KGER   6
#define MM_BIG 7
#define CLKRATE 8

char *BNCHNAMES[NBENCH] =
   {"kSelMM", "kGenMM", "kMM_NT", "kMM_TN", "kMV_N", "kMV_T", "kGER", "BIG_MM"};

void PrintUsage(char *name, int iarg, char *flag)
{
   if (iarg)
      fprintf(stderr, "Error around argument %d (%s)!\n", iarg, flag);
   fprintf(stderr, "USAGE: %s [flags]\n", name);
   fprintf(stderr, "   -dp <prior benchmark directory>\n");
   fprintf(stderr, "   -dc <current benchmark directory>\n");
   fprintf(stderr, "   -f <filename w/o prefix>\n");
   fprintf(stderr, "   -o <outfile> : default=stdout\n");
   exit(iarg ? iarg : -1);
}

FILE *GetFlags(int nargs, char **args, char **fname, char **currd, char **oldd)
{
   char *sp;
   FILE *fpout;
   int i;

   *fname = "PerfSumm.txt";
   *currd = "bin/INSTALL_LOG";
   *oldd = NULL;
   fpout = stdout;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'd':
         if (++i >= nargs)
            PrintUsage(args[0], i, "Out of args");
         sp = args[i];
         if (args[i-1][2] == 'p') *oldd = sp;
         else if (args[i-1][2] == 'c') *currd = sp;
         else PrintUsage(args[0], i-1, args[i-1]);
         break;
      case 'f':
         if (++i >= nargs)
            PrintUsage(args[0], i, "Out of args");
         *fname = args[i];
         break;
      case 'o':
         if (++i >= nargs)
            PrintUsage(args[0], i, "Out of args");
         fpout = fopen(args[i], "w");
         assert(fpout);
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   if (*oldd == NULL)
      fprintf(stderr,
      "No prior benchmark directory given, no comparison will be made.\n");
   return(fpout);
}


double RunBigMM(char pre, double clkrate, int *N)
/*
 * Calls gemmtst to find asymptotic performance.
 * RETURNS: mflop of large matmul of precision pre.
 */
{
   char cmnd[2048], res[2048];
   int i, n=1600, offset;
   double mf0, mf1;
   FILE *fpin;

   offset = (pre == 's' || pre == 'd') ? 50 : 61;
   sprintf(cmnd, "cd bin ; make x%cmmtst_big", pre);
   syschk(cmnd);
/*
 * Don't run case taking longer than 1 minute, assuming 1flop/cycle
 */
   mf0 = clkrate*1000000.0 * 60.0;
   while (((pre=='c' || pre=='z') ? 8.0 : 2.0)*n*n*n > mf0)
      n -= 200;
   n = (n >= 1200) ? n :1200;
/*
 * May fail for lack of memory, so keep reducing N until we have success
 */
   do
   {
      assert(n > 200);
      remove("big.out");
      sprintf(cmnd, "./bin/x%cmmtst_big -n %d -Test 0 > big.out\n", pre, n);
      n -= 200;
/*      fprintf(stderr, "cmnd='%s'", cmnd); */
   }
   while(system(cmnd));
   *N = n + 200;
/*
 * This section parses xdmmtst output to get mflop; note that it is fragile,
 * so if we change the formatting of xdmmtst, we must change this!
 */
   fpin = fopen("big.out", "r");
   assert(fpin);
/*
 * Skip headers/blank lines, get 1st line of output
 */
   for (i=0; i < 5; i++)
      assert(fgets(res, 2048, fpin));
   mf0 = atof(res+offset);
   fprintf(stderr, "res+off=%s\n", res+offset);
   assert(fgets(res, 2048, fpin));
   mf1 = atof(res+offset);
   fclose(fpin);
   fprintf(stderr, "BIG_MM N=%d, mf=%.2f,%.2f!\n", *N, mf0, mf1);
   return((mf0 >= mf1) ? mf0 : mf1);
}

double **ReadBenchmarks(char *dir, char *basename)
/*
 * Allocates a benchmark array with 4 (one for each precision) vectors of
 * NBENCH+1 length.  The MM_BIG element may not be present, in which case it is
 * filled in as 0. The extra element is the clock rate.
 */
{
   char ln[2048];
   char pre[4] = {'s', 'c', 'd', 'z'};
   int i, j, RECOMPUTE=0, N;
   FILE *fpin;
   double *mf, **res;

   if (dir == NULL || basename == NULL)
      return(NULL);
/*
 * If no files exist, return NULL
 */
   for (fpin=NULL, i=0; i < 4 && !fpin; i++)
   {
      sprintf(ln, "%s/%c%s", dir, pre[i], basename);
      fpin = fopen(ln, "r");
   }
   if (!fpin)
      return(NULL);
   fclose(fpin);

   res = malloc(sizeof(double*)*4);
   assert(res);
   for (i=0; i < 4; i++)
   {
      res[i] = mf = malloc(sizeof(double)*(NBENCH+1));
      assert(mf);
      sprintf(ln, "%s/%c%s", dir, pre[i], basename);
      fpin = fopen(ln, "r");
      if (!fpin)
      {
         for (j=0; j <= NBENCH; j++)
            mf[j] = 0.0;
         continue;
      }
      assert(fgets(ln, 2048, fpin));
      assert(ln[10] == '=');
      mf[CLKRATE] = atof(ln+11);
      if (mf[CLKRATE] < 100)
         RECOMPUTE = 8;
      else RECOMPUTE = 0;
      if (i)
         mf[CLKRATE] = res[i-1][CLKRATE];
      else
      {
         while (mf[CLKRATE] < 100.0)
            mf[CLKRATE] = GetInt(stdin, 0, "", "Clock rate in Mhz");
      }
/*
 *    Skip table headers
 */
      assert(fgets(ln, 2048, fpin));
      assert(fgets(ln, 2048, fpin));
/*
 *    Read mandatory NBENCH-1 elements of table
 */
      for (j=0; j < NBENCH-1; j++)
      {
         assert(fgets(ln, 2048, fpin));
         mf[j] = atof(ln+RECOMPUTE);
         if (RECOMPUTE)
            mf[j] = (mf[j]/mf[CLKRATE])*100.0;
      }
/*
 *    If large-case MM in file, read as normal
 */
      if (fgets(ln, 2048, fpin) != NULL)
      {
         mf[j] = atof(ln+RECOMPUTE);
         if (RECOMPUTE)
            mf[j] = (mf[j]/mf[CLKRATE])*100.0;
      }
/*
 *    If large-case MM not in file, must run it, and then add to file
 */
      else
      {
         fclose(fpin);
         mf[j] = RunBigMM(pre[i], mf[CLKRATE], &N);
         sprintf(ln, "%s/%c%s", dir, pre[i], basename);
         fpin = fopen(ln, "a");
         assert(fpin);
         fprintf(fpin, "%7.1f %10.1f  N=%d GEMM\n",
                 (mf[j]/mf[CLKRATE])*100.0, mf[j], N);
         mf[j] = (mf[j]/mf[CLKRATE])*100.0;
      }
      fclose(fpin);
   }
   return(res);
}

void PrintNameDefs(FILE *fpout)
{
   fprintf(fpout,
"\nThe times labeled Reference are for ATLAS as installed by the authors.\n");
   fprintf(fpout, "NAMING ABBREVIATIONS:\n");
   fprintf(fpout, "   kSelMM : selected matmul kernel (may be hand-tuned)\n");
   fprintf(fpout, "   kGenMM : generated matmul kernel\n");
   fprintf(fpout, "   kMM_NT : worst no-copy kernel\n");
   fprintf(fpout, "   kMM_TN : best no-copy kernel\n");
   fprintf(fpout, "   BIG_MM : large GEMM timing (usually N=1600); estimate of asymptotic peak\n");
   fprintf(fpout, "   kMV_N  : NoTranspose matvec kernel\n");
   fprintf(fpout, "   kMV_T  : Transpose matvec kernel\n");
   fprintf(fpout, "   kGER   : GER (rank-1 update) kernel\n");
   fprintf(fpout,
           "Kernel routines are not called by the user directly, and their\n");
   fprintf(fpout, "performance is often somewhat different than the total\n");
   fprintf(fpout, "algorithm (eg, dGER perf may differ from dkGER)\n\n");
}

void PrintComparison(FILE *fpout, double **oldres, double **newres)
{
   int i, j, k;

   PrintNameDefs(fpout);
   fprintf(fpout, "\nReference clock rate=%dMhz, new rate=%dMhz\n",
           (int) oldres[0][NBENCH], (int) newres[0][NBENCH]);
   fprintf(fpout,
      "   Refrenc : %% of clock rate achieved by reference install\n");
   fprintf(fpout,
      "   Present : %% of clock rate achieved by present ATLAS install\n\n");

   fprintf(fpout, "                    single precision                  double precision\n");
   fprintf(fpout, "            ********************************   *******************************\n");
   fprintf(fpout, "                  real           complex           real           complex\n");
   fprintf(fpout, "            ---------------  ---------------  ---------------  ---------------\n");
   fprintf(fpout, "Benchmark   Refrenc Present  Refrenc Present  Refrenc Present  Refrenc Present\n");
   fprintf(fpout, "=========   ======= =======  ======= =======  ======= =======  ======= =======\n");
   for (i=0; i < NBENCH; i++)
   {
      if (i == KMV_N) j = MM_BIG;
      else if (i > KMV_N) j = i-1;
      else j = i;
      fprintf(fpout, "%8.8s  ", BNCHNAMES[j]);
      for (k=0; k < 4; k++)
         fprintf(fpout, " %8.1f %7.1f", oldres[k][j], newres[k][j]);
      fprintf(fpout, "\n");
   }
}

void PrintSum(FILE *fpout, double **res)
{
   int i, j, k;

   PrintNameDefs(fpout);
   fprintf(fpout, "\nClock rate=%dMhz\n", (int) res[0][NBENCH]);

   fprintf(fpout,
           "               single precision        double precision\n");
   fprintf(fpout,
           "            *********************    ********************\n");
   fprintf(fpout,
           "               real      complex       real      complex\n");
   fprintf(fpout,
           "Benchmark   %%   Clock   %%   Clock   %%   Clock   %%   Clock\n");
   fprintf(fpout,
           "=========   =========   =========   =========   =========\n");
   for (i=0; i < NBENCH; i++)
   {
      if (i == KMV_N) j = MM_BIG;
      else if (i > KMV_N) j = i-1;
      else j = i;
      fprintf(fpout, "%8.8s  ", BNCHNAMES[j]);
      for (k=0; k < 4; k++)
         fprintf(fpout, " %9.1f ", res[k][j]);
      fprintf(fpout, "\n");
   }
}

int main(int nargs, char **args)
/*
 * This program benchmarks an ATLAS install, and compares it against a prior
 * install, if such a prior install exists.  If necessary, it runs
 * gemmtst to figure out asymptotic performance of GEMM.
 */
{
   char *fname, *currd, *oldd;
   FILE *fpout;
   double **refres=NULL, **newres;

   fpout = GetFlags(nargs, args, &fname, &currd, &oldd);
   newres = ReadBenchmarks(currd, fname);
   assert(newres);
   refres = ReadBenchmarks(oldd, fname);
   if (refres)
      PrintComparison(fpout, refres, newres);
   else PrintSum(fpout, newres);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
   return(0);
}
