#include "atlas_ptmisc.h"
#include <string.h>
#define dumb_rand() ( 0.5 - ((double)rand())/((double)RAND_MAX) )

#ifndef KMM
   #define KMM ATL_USERMM
#endif

#define CINT const int

void KMM(const int, const int, const int, const SCALAR, const TYPE*,
         const int, const TYPE*, const int, const SCALAR, TYPE*, const int);

struct kmm_struct{
   int mb, nb, kb;                      /* C: mbxnb, At: kbxmb, B: kbXnb */
   int movA, movB, movC;                /* which mat move in flush array? */
   int FLSIZE;                          /* min area to move in in bytes */
   int reps;                            /* # calls to kmm in one timing */
   int LDC;                             /* what should ldc be set to? */
   int iam;                             /* thread rank */
   int p;                               /* total number of threads */
   double mf;                           /* mflop returned by timing */
};

double GetKmmMflop
(
   CINT mb, CINT nb, CINT kb,           /* C: mbxnb, At: kbxmb, B: kbXnb */
   CINT movA, CINT movB, CINT movC,     /* which mat move in flush array? */
   int FLSIZE,                          /* min area to move in in bytes */
   CINT reps,                           /* # calls to kmm in one timing */
   CINT LDC                             /* what should ldc be set to? */
)
/*
 * Returns MFLOP rate of matmul kernel KMM
 * LDC: if (LDC == 0), then set ldc=MB for timings.
 *      if (LDC != 0 && movC != 0), then ldc= col length in move space
 *      else ldc = LDC;
 *
 */
{
   const int NOMOVE = !(movA|movB|movC);
   int ldc, setsz, nset, i, j, incA, incB, incC, n, extra;
   TYPE *C, *A, *B, *a, *b, *c;
   double t0, t1, mf;
   const TYPE alpha=1.0;
   TYPE beta=1.0;
   void *vp=NULL;

   if (NOMOVE)
   {
      ldc = (LDC) ? LDC : mb;
      setsz = (ldc * nb + kb*(mb+nb));
      vp = malloc(ATL_Cachelen + ATL_MulBySize(setsz));
      ATL_assert(vp);
      A =  ATL_AlignPtr(vp);
      B = A + mb*kb;
      C = B + kb*nb;
      for (i=0; i < setsz; i++) A[i] = dumb_rand();
      incA = incB = incC = 0;
   }
   else
   {
      if (movA && movB && movC)         /* no reuse at all */
      {
         setsz = ATL_MulBySize(mb*nb+kb*(mb+nb));
         nset = (FLSIZE+setsz-1)/setsz;
         FLSIZE = nset*setsz;
         setsz = mb*nb+kb*(mb+nb);
         vp = malloc(ATL_Cachelen + ATL_MulBySize(setsz));
         ATL_assert(vp);
         A = ATL_AlignPtr(vp);
         B = A + kb*mb*nset;
         C = B + kb*nb*nset;
         ldc = (LDC) ? mb*nset : mb;
         for (n=setsz*nset,i=0; i < n; i++) A[i] = dumb_rand();
         incA = mb*kb;
         incB = kb*nb;
         incC = mb*nb;
      }
      else if (movA && movB && !movC)   /* square-case ATLAS behavior */
      {
         setsz = kb*(mb+nb);
         ldc = (LDC) ? LDC : mb;
         ATL_assert(ldc >= mb);
         extra = ldc*nb;
         incA = mb*kb;
         incB = kb*nb;
         incC = 0;
      }
      else if (!movB && movA && movC)   /* rank-K behavior */
      {
         setsz = mb*(kb+nb);
         extra = kb*nb;
         incA = mb*kb;
         incB = 0;
         incC = mb*nb;
      }
      else
      {
         fprintf(stderr, "%s,%d: What case are you wanting?\n",
                 __FILE__, __LINE__);
         exit(-1);
      }
      if (!vp)
      {
         i = ATL_MulBySize(setsz);
         nset = (FLSIZE+i-1)/i;
         FLSIZE = nset * i;
         vp = malloc(ATL_Cachelen + ATL_MulBySize(FLSIZE+extra));
         ATL_assert(vp);
         A = ATL_AlignPtr(vp);
         if (movC)
         {
            C = A + mb*kb*nset;
            ldc = (LDC) ? mb*nset : mb;
            B = C + mb*nb*nset;
         }
         else
         {
            B = A + mb*kb*nset;
            C = B + kb*nb*nset;
         }
         for (n=setsz*nset+extra,i=0; i < n; i++) A[i] = dumb_rand();
      }
   }
   a = A; b = B; c = C;
   t0 = ATL_walltime();
   for (j=0,i=reps; i; i--)
   {
      KMM(mb, nb, kb, alpha, a, kb, b, kb, beta, c, ldc);
      if (++j != nset)
      {
         a += incA;
         b += incB;
         c += incC;
      }
      else
      {
         beta = (beta != 0.0) ? -beta : 0.0;
         j = 0;
         a = A; b = B; c = C;
      }
   }
   t1 = ATL_walltime() - t0;
   mf = (2.0*reps*mb*nb*kb) / (t1*1000000.0);
   free(vp);
   return(mf);
}

void *TimeOnCore(void *vp)
{
   struct kmm_struct *kp = vp;

   kp->mf = GetKmmMflop(kp->mb, kp->nb, kp->kb, kp->movA, kp->movB, kp->movC,
                        kp->FLSIZE, kp->reps, kp->LDC);
   return(NULL);
}

double *TimeOnCores(struct kmm_struct *kb)
{
   struct kmm_struct *kp;
   pthread_t *threads;
   pthread_attr_t *attr;
   unsigned long cpuset;
   double *mflops;
   int i, p;

   p = kb->p;
   kp = malloc(sizeof(struct kmm_struct)*p);
   threads = malloc(sizeof(pthread_t)*p);
   attr = malloc(sizeof(pthread_attr_t)*p);
   mflops = malloc(sizeof(double)*p);
   ATL_assert(kp && threads && attr && mflops);
   for (i=0; i < p; i++)
   {
      memcpy(kp+i, kb, sizeof(struct kmm_struct));
      kp[i].iam = i;
      cpuset = (1<<i);
      pthread_attr_setaffinity_np(attr+i, sizeof(cpuset), &cpuset);
      pthread_create(threads+i, attr+i, TimeOnCore, kp+i);
   }
   for (i=0; i < p; i++)
   {
      pthread_join(threads[i], NULL);
      mflops[i] = kp[i].mf;
   }
   free(kp);
   free(threads);
   free(attr);
   return(mflops);
}

void GetStat(int n, double *d, double *min, double *max, double *avg)
{
   int i;
   double dmin, dmax, dsum;

   dmin = dmax = dsum = d[0];
   for (i=1; i < n; i++)
   {
      dmax = (dmax >= d[i]) ? dmax : d[i];
      dmin = (dmin <= d[i]) ? dmin : d[i];
      dsum += d[i];
   }
   *min = dmin;
   *max = dmax;
   *avg = dsum / (double)n;
}

void PrintUsage(char *name, int iarg, char *arg)
{
   fprintf(stderr, "\nERROR around arg %d (%s).\n", iarg, arg ? arg:"unknown");
   fprintf(stderr, "USAGE: %s [flags], where flags are:\n", name);
   fprintf(stderr, "   -B <#> : mb = nb = kb = #\n");
   fprintf(stderr, "   -m <#> : mb = #\n");
   fprintf(stderr, "   -n <#> : nb = #\n");
   fprintf(stderr, "   -k <#> : kb = #\n");
   fprintf(stderr, "   -r <#> : set the # of times to call KMM\n");
   fprintf(stderr, "   -F <kb> : set flush size in kilobytes\n");
   fprintf(stderr, "   -C <#> : set ldc; 0 means mb\n");
   fprintf(stderr, "   -M[a,b,c] <#> : mov[A,B,C] = #\n");
   exit(iarg ? iarg : -1);
}

struct kmm_struct *GetFlags(int nargs, char **args)
{
   struct kmm_struct *kp;
   int i;

   kp = malloc(sizeof(struct kmm_struct));
   ATL_assert(kp);
   kp->p = 1;
   kp->mb = kp->nb = kp->kb = 40;
   kp->movA = kp->movB = kp->movC = 0;
   kp->FLSIZE = L2SIZE;
   kp->reps = 200;
   kp->LDC = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'F':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->FLSIZE = atoi(args[i]) * 1024;
         break;
      case 'C':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->LDC = atoi(args[i]);
         break;
      case 'r':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->reps = atoi(args[i]);
         break;
      case 'p':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->p = atoi(args[i]);
         break;
      case 'm':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->mb = atoi(args[i]);
         break;
      case 'n':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->nb = atoi(args[i]);
         break;
      case 'k':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->kb = atoi(args[i]);
         break;
      case 'B':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         kp->mb = kp->nb = kp->kb = atoi(args[i]);
         break;
      case 'M':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         switch(args[i-1][2])
         {
         case 'c':
         case 'C':
            kp->movC = atoi(args[i]);
            break;
         case 'b':
         case 'B':
            kp->movB = atoi(args[i]);
            break;
         case 'a':
         case 'A':
            kp->movA = atoi(args[i]);
            break;
         default:
            PrintUsage(args[0], i-1, "unknown mov matrix");
         }
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   return(kp);
}

int main(int nargs, char **args)
{
   struct kmm_struct *kp;
   int i, p;
   double *dp;
   double min, max, avg;
   FILE *fpout = stdout;

   kp = GetFlags(nargs, args);
   p = kp->p;
   dp = TimeOnCores(kp);
   free(kp);
   GetStat(p, dp, &min, &max, &avg);
   fprintf(fpout, "ALL CORES: min=%le, max=%le, avg=%le\n", min, max, avg);
   fprintf(fpout, "PER-CORE: %le", dp[0]);
   for (i=1; i < p; i++)
      fprintf(fpout, ", %le", dp[i]);
   fprintf(fpout, "\n\n%.2f\n", avg);
   free(dp);
   exit(0);
}
