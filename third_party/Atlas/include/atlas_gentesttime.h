#ifndef ATLAS_GENTESTTIME_H
   #define ATLAS_GENTESTTIME_H

#include "atlas_fopen.h"

/* procedure 1 */
static int GetL1CacheSize()
{
   FILE *L1f;
   int L1Size;

   L1f = fopen("res/L1CacheSize", "r");
   if (!L1f)
   {
      assert(system("make res/L1CacheSize\n") == 0);
      L1f = fopen("res/L1CacheSize", "r");
   }
   assert(L1f != NULL);
   assert(fscanf(L1f, "%d", &L1Size) == 1);
   fclose(L1f);
   fprintf(stderr, "\n      Read in L1 Cache size as = %dKB.\n",L1Size);
   return(L1Size);
}

/* procedure 2 */
static int pre2size(char pre)
{
   int iret=8;
   if (pre == 's')
      iret = 4;
   else if (pre == 'z')
      iret = 16;
   return(iret);
}

/* procedure 3 */
static void SortDoubles(int N, double *d)
/*
 * Sorts N-length array d from least to greatest using N^2 selection sort
 */
{
   double min;
   int imin, i, j;

   for (i=0; i < N-1; i++)
   {
      imin = i;
      min = d[i];
      for (j=i+1; j < N; j++)
      {
         if (d[j] < min)
         {
            imin = j;
            min = d[j];
         }
      }
      if (imin != i)
      {
         d[imin] = d[i];
         d[i] = min;
      }
   }
}

/* procedure 4 */
static void *ReadResultsFile(int FULL, int nsample, char *resfile)
/*
 * Reads an ATLAS results file, which has the following form:
 *    <N> <walltime>   -> walltime of 0 means CPU timing was used
 *    sample1
 *    ....
 *    sampleN
 * RETURNS: NULL on error or if nsample > N, else if (FULL) it returns
 *          an array of form:  <N> <wall> <sample1> .... <sampleN>
 *          where samples have been sorted from least-to-greatest.
 *          where <N> is the number of samples.
 *          If (!FULL) then a pointer to the max value is returned for
 *          walltime, and a pointer to the median value is returned for
 *          for cputime.
 */
{
   static double dret;
   double *dres;
   FILE *fpin;
   char ln[1024];
   int i, n, wall, ierr;

   fpin = fopen(resfile, "r");
   if (!fpin)
      return(NULL);
   ierr = (fgets(ln, 1024, fpin) == NULL);
   if (ierr || sscanf(ln, " %d %d", &n, &wall) != 2)
   {
      fclose(fpin);
      return(NULL);
   }
   if (n < nsample)
   {
      fclose(fpin);
      return(NULL);
   }
   dres = malloc((n+2)*sizeof(double));
   assert(dres);
   dres[0] = n;
   dres[1] = wall;
   for (i=0; i < n; i++)
   {
      ierr = (fgets(ln, 1024, fpin) == NULL);
      if (ierr || sscanf(ln, " %le", dres+i+2) != 1)
      {
         fclose(fpin);
         return(NULL);
      }
   }
   fclose(fpin);
   SortDoubles(n, dres+2);
   if (FULL)
      return(dres);
   dret = (wall) ? dres[n+1] : dres[2+n/2];
   free(dres);
   return(&dret);
}

/* procedure 5 */
static double PrintResultsFromFile(FILE *fpout, double *darr)
{
   const int N=darr[0], wall=(darr[1] != 0.0);
   double *dres=darr+2;
   double dret;
   int i;

   assert(N > 0);
   for (i=0; i < N; i++)
      fprintf(fpout, "   %le\n", dres[i]);
   if (wall)
   {
      fprintf(fpout, "MAX = %.2f\n", dres[N-1]);
      return(dres[N-1]);
   }
   else
      fprintf(fpout, "MED = %.2f\n", dres[N/2]);
   return(dres[N/2]);
}

#endif  /* end guard around atlas_gentesttime.h */
