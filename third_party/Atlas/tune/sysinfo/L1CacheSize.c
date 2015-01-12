/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#define REPS 4096

#ifndef time00
   #define time00 ATL_cputime
#endif
double time00();

// #define USE_OLD
#ifdef USE_OLD
int L1CacheSize_read(int MAXL1CACHE, double tol)
{
   int ntests, h, i, j, k, reps=REPS*MAXL1CACHE, len, dlen, size=0;
   double *p, *d, *st;
   double t1, t2, *times;
   register double d0, d1, d2, d3;

   for (i=2, j=0; i <= MAXL1CACHE; i <<= 1, j++);
   times = malloc(j*sizeof(double));
   ntests = j;

   h=0;
   for (i=2; i <= MAXL1CACHE; i <<= 1)
   {
      dlen = (i*1024+7) / sizeof(double);
      dlen = (dlen >> 3)<<3;
      len = dlen * 8;
      d = p = malloc(len);
      st = p + dlen;

      for (k=0; k != dlen; k += 8)
      {
         d[k] = 5.0;
         d[k+1] = -5.0;
         d[k+2] = -5.0;
         d[k+3] = 5.0;
         d[k+4] = -5.0;
         d[k+5] = 5.0;
         d[k+6] = 5.0;
         d[k+7] = -5.0;
      }
      d0 = d1 = d2 = d3 = 0.0;
      do
      {
         t1 = time00();
         for (j=0; j != reps; j++)
         {
            do
            {
                d0 += *d;
                d1 += d[2];
                d2 += d[4];
                d3 += d[6];
                d += 8;
            }
            while (d != st);
            d = p;
         }
         t1 = time00() - t1;
         if (t1 < 0.6 && h == 0) reps += reps;
      }
      while (t1 < 0.6 && h == 0);
      times[h] = t1;
      d0 += d1 + d2 + d3;
      if (d0 > 100.0)
         exit(-1);

      fprintf(stderr, "      L1CS=%d, time=%f\n",i, times[h]);
      reps >>= 1;
      free(p);
      h++;
   }
#ifdef RawSize
   t2 = 0.0;  j = 0;
   for (i=1; i < ntests; i++)
   {
      t1 = times[i] - times[i-1];
      if (t1 < 0) t1 = 0.0;
      if (t1 > t2) { t2 = t1;  j=i; }
   }
#else
   t2 = 1.0;  j = 0;
   for (i=1; i < ntests; i++)  /* find biggest % increase */
   {
      t1 = times[i] / times[i-1];
      if (t1 > t2) { t2 = t1;  j=i; }
   }
#endif
   fprintf(stderr, "\n");
   if (tol != 0.0)
   {
      if (tol*times[j-1] > times[j])
      {
         free(times);
         return(0);
      }
   }
   free(times);
   return(1 << j);
}
#else
void initmem(int n, double *mem)
/*
 * Initialize mem to something that won't cause overlow, but confuse
 * things a little so compiler can't get rid of code
 */
{
   double d;
   int i;
   if (rand() > 27) d = ( 0.5 - ((double)rand())/((double)RAND_MAX) );
   else d = ( 1.0 - ((double)rand())/((double)RAND_MAX) );
   for (i=0; i < n; i++)
      mem[i] = ( 0.5 - ((double)rand())/((double)RAND_MAX) );
   for (i=0; i < n; i += 8)
   {
      mem[i] = d;
      mem[i+2] = -d;
      mem[i+4] = -d;
      mem[i+6] = d;
      d = -d;
   }
}

int L1CacheSize_read(int MAXL1CACHE, double tol)
{
   int ntests, h, i, j, k, reps=REPS*MAXL1CACHE, len, dlen, size=0;
   double *p, *d, *st;
   double t1, t2, *times;
   register double d0, d1, d2, d3;

   for (i=2, j=0; i <= MAXL1CACHE; i <<= 1, j++);
   times = malloc(j*sizeof(double));
   ntests = j;

   h=0;
   for (i=2; i <= MAXL1CACHE; i <<= 1)
   {
      dlen = (i*1024+7) / sizeof(double);
      dlen = (dlen >> 3)<<3;
      len = dlen * 8;
      d = p = malloc(len);
      st = p + dlen;

      initmem(dlen, d);
      d0 = d1 = d2 = d3 = 0.0;
      do
      {
         t1 = time00();
         for (j=0; j != reps; j++)
         {
            do
            {
                d0 += *d;
                d1 += d[2];
                d2 += d[4];
                d3 += d[6];
                d += 8;
            }
            while (d != st);
            d = p;
         }
         t1 = time00() - t1;
         if (t1 < 0.6 && h == 0) reps += reps;
      }
      while (t1 < 0.6 && h == 0);
      times[h] = t1;
      d0 += d1 + d2 + d3;

      fprintf(stderr, "      L1CS=%d, time=%f (ignore=%.1e)\n",i, times[h], d0);
      reps >>= 1;
      free(p);
      h++;
   }
#ifdef RawSize
   t2 = 0.0;  j = 0;
   for (i=1; i < ntests; i++)
   {
      t1 = times[i] - times[i-1];
      if (t1 < 0) t1 = 0.0;
      if (t1 > t2) { t2 = t1;  j=i; }
   }
#else
   t2 = 1.0;  j = 0;
   for (i=1; i < ntests; i++)  /* find biggest % increase */
   {
      t1 = times[i] / times[i-1];
      if (t1 > t2) { t2 = t1;  j=i; }
   }
#endif
   fprintf(stderr, "\n");
   if (tol != 0.0)
   {
      if (tol*times[j-1] > times[j])
      {
         free(times);
         return(0);
      }
   }
   free(times);
   return(1 << j);
}
#endif

#define BIGBOY 256
#define L1CacheSize L1CacheSize_read

int GetL1Size(int MaxSize, double tol)
{
   int L1Size, tmp, correct=1;

   fprintf(stderr, "\n   Calculating L1 cache size:\n");
   L1Size = L1CacheSize(MaxSize, tol);
   if (L1Size == 0) tmp = -1.0;
   else
   {
      fprintf(stderr, "      Confirming result of %dkb:\n", L1Size);
      tmp = L1CacheSize(MaxSize, 0.0);
      if (tmp == L1Size) tmp = L1CacheSize(MaxSize, 0.0);
   }
   if (tmp != L1Size && MaxSize < BIGBOY)
   {
      if (L1Size) fprintf(stderr,
                     "   L1 size not confirmed; trying large maximum size:\n");
      else fprintf(stderr, "      Timing differences below tolerance, trying larger maximum cache size:\n");
      L1Size = L1CacheSize(BIGBOY, 0.0);
      fprintf(stderr, "      Confirming result of %dkb:\n", L1Size);
      tmp = L1CacheSize(BIGBOY, 0.0);
      if (tmp == L1Size) tmp = L1CacheSize(BIGBOY, 0.0);
   }
   correct = (tmp == L1Size);
   fprintf(stderr, "Calculated L1 cache size = %dkb; Correct=%d\n",
           L1Size, correct);
   return (correct ? L1Size : 0);
}

int main(int nargs, char *args[])
{
   int L1Size, MaxSize=64, correct=1;
   FILE *L1f;

   if (nargs > 2)
   {
      fprintf(stderr, "USAGE: %s <max L1 cache size (in kilobytes)>\n",args[0]);
      exit(-1);
   }
   if (nargs > 1) MaxSize = atoi(args[1]);
   L1Size = GetL1Size(MaxSize, 1.08);
   if (!L1Size)
      L1Size = GetL1Size(MaxSize, 1.08);
   if (!L1Size)
   {
      fprintf(stderr, "\nCan't detect the L1 cache; setting to 32KB.\n");
      L1Size = 32;
      correct = 0;
   }
   fprintf(stderr, "Calculated L1 cache size = %dkb; Correct=%d\n",
           L1Size, correct);
   L1f = fopen("res/L1CacheSize", "w");
   if (L1f)
   {
      fprintf(L1f, "%d\n",L1Size);
      fclose(L1f);
      exit(0);
   }
   return(-1);
}
