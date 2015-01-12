#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifndef NTIM
   #define NTIM 3
#endif

void magen
(
   char pre,    /* precision: s,d,q */
   FILE *fpout,
   int flops,   /* rough flops/iteration target */
   int MAC,     /* 0: separate multiply & add;  1: use MAC */
   int lat      /* latency to use */
)
{
   const int lfl = 2*lat, ur = (flops) ? (flops+lfl-1)/lfl:1;
   int i, j;
   char *typ="double";

   if (pre == 's')
      typ = "float";
   else if (pre == 'q')
      typ = "long double";
   fprintf(fpout, "double macase(int nrep, volatile %s *dum)\n", typ);
   fprintf(fpout,
      "/*\n *  Performs nrep loop its and returns mflops performed\n */\n");
   fprintf(fpout, "{\n");
   fprintf(fpout, "   register %s m0, a0=0.0", typ);
   for (i=1; i < lat; i++)
      fprintf(fpout, ", m%d, a%d=0.0", i, i);
   fprintf(fpout, ";\n");
   fprintf(fpout, "   register int i;\n");

   for (i=0; i < lat; i++)
      fprintf(fpout, "    m%d=dum[%d];\n", i, i);

   fprintf(fpout, "   for (i=0; i < nrep; i++)\n   {\n");
   for (j=0; j < ur; j++)
   {
      fprintf(fpout, "/*\n * Basic block %d\n */\n", j);
      for (i=0; i < lat; i++)
      {
         if (MAC)
            fprintf(fpout, "      a%d += m%d*m%d;  m%d=m%d;\n",
                    i, i, i, i, (i+1)%lat);
         else
            fprintf(fpout, "      a%d += m%d; m%d *= m%d;\n", i, i, i, i);
      }
   }
   fprintf(fpout, "   }\n\n");
   for (i=0; i < lat; i++)
      fprintf(fpout, "   dum[%d] = a%d;\n", i, i);
   fprintf(fpout, "   return(nrep*%d.0);\n", lfl*ur);
   fprintf(fpout, "}\n");
}

double matime
(
   char pre,   /* precision: s,d,q */
   int MAC,    /* 0: separate mul/add, 1: FMAC */
   int lat,    /* latency : number of multiply and add registers */
   int fmf,    /* how many mflops to force during timing */
   int TIME,   /* 0: return mflops, else return times in seconds */
   int FRC     /* 1: force retime if output file already there */
)
{
   char fnam[128];
   FILE *fp;
   double t0, mflop[NTIM];
   int i, j;

   if (FRC)
   {
      sprintf(fnam, "res/%ctmp", pre);
      fp = NULL;
   }
   else
   {
      sprintf(fnam, "res/%cmuladd%d_%d", pre, MAC, lat);
      fp = fopen(fnam, "r");
   }
   if (!fp)
   {
      FILE *fpout;
      char maf[16], ln[128], flgs[64];
      maf[0] = pre;
      maf[1] = 'm';
      maf[2] = 'u';
      maf[3] = 'l';
      maf[4] = 'a';
      maf[5] = 'd';
      maf[6] = 'd';
      maf[7] = '.';
      maf[8] = 'c';
      maf[9] = '\0';
      fpout = fopen(maf, "w");
      assert(fpout);
      magen(pre, fpout, 256, MAC, lat);
      fclose(fpout);
      i = sprintf(ln, "make x%cma outf='%s' flags=\"-m %d", pre, fnam, fmf);
      if (TIME)
         i += sprintf(ln+i, " -t 1");
      ln[i++] = '"';
      ln[i] = '\0';
      sprintf(ln+i, "2>&1 > /dev/null");
      assert(!system(ln));
      fp = fopen(fnam, "r");
      assert(fp);
   }
   t0 = 0.0;
   j = 0;
   for (i=0; i != NTIM; i++)
   {
      assert( fscanf(fp, "%lf", &mflop[i]) );
   }
   fclose(fp);
/*
 * Sort results, largest first
 */
   for (i=0; i != NTIM; i++)
   {
      for (j=i+1; j < NTIM; j++)
      {
         if (mflop[i] < mflop[j])
         {
            t0 = mflop[i];
            mflop[i] = mflop[j];
            mflop[j] = t0;
         }
      }
   }

/*
 * For walltime, return min time, else return median
 */
   #if defined(PentiumCPS) || defined(WALL)
      return(TIME ? mflop[NTIM-1] : mflop[0]);
   #else
      return(mflop[NTIM/2]);
   #endif
}

int FindMflop
(
   char pre,   /* precision: s,d,q */
   int MAC,    /* 0: separate mul/add, 1: FMAC */
   int lat     /* latency : number of multiply and add registers */
)
/*
 * Finds minimum mflop setting required to get a timing that takes roughly
 * 0.1 seconds
 */
{
   double tim;
   int mf, mfn = 1, ratio;
   printf("Finding how many mflops required to get .025 second timings:\n");

   do
   {
      mf = mfn;
      tim = matime(pre, MAC, lat, mf, 1, 1);
      printf("   %d: %e\n", mf, tim);
      mfn = mf<<1;
   }
   while(tim < 0.025);
   printf("FORCE MFLOP=%d, TIME=%e\n\n", mf, tim);
   return(mf);
}

FindNreg
(
   char pre,   /* precision: s,d,q */
   int fmf,    /* number of mflops to force timer on */
   int MAC,    /* 0: separate mul/add, 1: FMAC */
   int lat     /* latency : number of multiply and add registers */
)
/*
 * Tries to find total number of registers by noticing when performance
 * goes down with latency increase
 */
{
   double mfmax, mf;
   int nr, n, nreg=0;

   printf("FINDING USABLE NREG:\n");
   nr = 1;
   mfmax = matime(pre, MAC, nr, fmf, 0, 0);
   printf("   %3d: %.2f\n", nr+nr, mfmax);
   do
   {
      nr <<= 1;
      mf = matime(pre, MAC, nr, fmf, 0, 0);
      printf("   %3d: %.2f\n", nr+nr, mf);
      if (mf > mfmax)
         mfmax = mf;
      else if (1.08*mf < mfmax)
         break;
      nreg = nr+nr;
   }
   while (nr < 256);
   printf("\n");
/*
 * Since some compilers reserve some registers for themselves, don't believe
 * power-of-two results alone.  Refine estimate by scoping if intermediate
 * number of registers is still competitive
 */
   if (nreg == 16)
      n = 10;
   else if (nreg < 16)
      n = (nreg>>1)+2;
   else
      n = (nreg>>1) + (nreg>>2);
   mf = matime(pre, MAC, n, fmf, 0, 0);
   printf("   %3d: %.2f\n", n+n, mf);
   if (mf*1.02 >= mfmax)   /* worth refining further */
   {
      const int maxreg=nreg;
      if (mf*1.01 >= mfmax)
         nreg = n+n;
      if (mf > mfmax)
         mfmax = mf;

      for (nr = n+1; nr < maxreg; nr++)
      {
         mf = matime(pre, MAC, nr, fmf, 0, 0);
         printf("   %3d: %.2f\n", nr+nr, mf);
         if (mf*1.01 >= mfmax)
            nreg = nr+nr;
         if (mf > mfmax)
            mfmax = mf;
         else if (1.03*mf < mfmax)
            break;
      }
   }
   for (n=1; n < nreg; n <<= 1);
   if (n == nreg)
      printf("NREG=%d\n\n", nreg);
   else
   {
      printf("DETECTED nreg=%d; ASSIGNED nreg=%d\n", nreg, n);
      nreg = n;
   }
   return(nreg);
}

int FindLat
(
   char pre,   /* precision: s,d,q */
   int fmf,    /* number of mflops to force timer on */
   int MAC,    /* 0: separate mul/add, 1: FMAC */
   int nreg,   /* max number of registers to try */
   double *MFMAX
)
{
   double mf, mfmax;
   int i, latmax = 1;

   printf("FINDING BEST LATENCY, MAC=%d:\n", MAC);
   mfmax = matime(pre, MAC, 1, fmf, 0, 0);
   printf("   lat=%d, MFLOPS=%.2f\n", 1, mfmax);
   for (i=2, nreg >>= 1; i <= nreg; i++)
   {
      mf = matime(pre, MAC, i, fmf, 0, 0);
      printf("   lat=%d, MFLOPS=%.2f\n", i, mf);
      if (mf < mfmax*1.02)
         break;
      mfmax = mf;
      latmax = i;
   }
   printf("LAT=%d, MFLOPS=%.2f\n\n", latmax, mfmax);
   *MFMAX = mfmax;
   return(latmax);
}

void PrintUsage(char *xnam)
{
   fprintf(stderr,
   "USAGE: %s -p <pre> -o <outfile> -F <mflop> -n <nreg> -L <lat> -M <mac>\n",
           xnam);
   exit(-1);
}

char *GetFlags(int nargs, char **args, char *PRE, int *FMF, int *NREG,
               int *LAT, int *MAC)
{
   char *fnam=NULL;
   int i;
   *PRE = 'd';
   *LAT = *FMF = *NREG = 0;
   *MAC = -1;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'p':
         if (++i == nargs)
            PrintUsage(args[0]);
         *PRE = args[i][0];
         break;
      case 'L':
         if (++i == nargs)
            PrintUsage(args[0]);
         *LAT = atoi(args[i]);
         break;
      case 'n':
         if (++i == nargs)
            PrintUsage(args[0]);
         *NREG = atoi(args[i]);
         break;
      case 'F':
         if (++i == nargs)
            PrintUsage(args[0]);
         *FMF = atoi(args[i]);
         break;
      case 'M':
         if (++i == nargs)
            PrintUsage(args[0]);
         *MAC = atoi(args[i]);
         break;
      case 'o':
         if (++i == nargs)
            PrintUsage(args[0]);
         fnam = args[i];
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   assert(*PRE == 's' || *PRE == 'd' || *PRE == 'q');
   return(fnam);
}

int main(int nargs, char **args)
{
   char pre='d';
   int MAC=0, lat=1, fmf, nreg, latMAC, latMA, RUNSRCH=1;
   double mf, mfMAC, mfMA;
   char *fnam;
   FILE *fp;

   fnam = GetFlags(nargs, args, &pre, &fmf, &nreg, &lat, &MAC);
/*
 * If MAC is set, just time the indicated case and output to screen
 */
   if (MAC != -1)
   {
      mf = matime(pre, MAC, lat, fmf, 0, 1);
      printf("pre=%c, MAC=%d, lat=%d, mf=%.2f\n", pre, MAC, lat, mf);
      exit(0);
   }
/*
 * If user specified a file that already exists, just read in data and run
 * case
 */
   if (fnam)
   {
      fp = fopen(fnam, "r");
      if (fp)
      {
         assert(fscanf(fp, "%d", &MAC) == 1);
         assert(fscanf(fp, "%d", &lat) == 1);
         assert(fscanf(fp, "%lf", &mf) == 1);
         assert(fscanf(fp, "%d", &nreg) == 1);
         fclose(fp);
         fmf = FindMflop(pre, MAC, lat);
         mf = matime(pre, MAC, lat, fmf, 0, 1);
         RUNSRCH=0;
      }
   }
   if (RUNSRCH)
   {
      fmf = FindMflop(pre, 0, 4);
      nreg = FindNreg(pre, fmf, 0, 4);
      latMA = FindLat(pre, fmf, 0, nreg, &mfMA);
      latMAC = FindLat(pre, fmf, 1, nreg, &mfMAC);
      if (mfMA*1.02 > mfMAC)
      {
         MAC = 0;
         lat = latMA;
         mf = mfMA;
      }
      else
      {
         MAC = 1;
         lat = latMAC;
         mf = mfMAC;
      }
      printf("NREG=%d, FMAC=%d, LAT=%d, MFLOP=%.2f\n", nreg, MAC, lat, mf);
   }
   if (fnam)
   {
      fp = fopen(fnam, "w");
      assert(fp);
      fprintf(fp, "%d\n%d\n%.2f\n%d\n", MAC, lat, mf, nreg);
      fclose(fp);
   }
   else
      printf("NREG=%d, FMAC=%d, LAT=%d, MFLOP=%.2f\n", nreg, MAC, lat, mf);
   return(0);
}
