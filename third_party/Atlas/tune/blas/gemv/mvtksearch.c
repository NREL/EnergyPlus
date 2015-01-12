#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "atlas_misc.h"
#include "atlas_mvparse.h"
#include "atlas_mvtesttime.h"
#include "atlas_ssysinfo.h"

/*
 * NREPCT & VERB currently unused.
 * I should also modify FORCETIME so that it will delete any result file if
 * FORCETIME is negative.
 */
static int FLUSHKB=(-1), L2KB=0, NREPCT=3, NREP=3, VERB=0;

void GetDimsByContext
(
   int L1CacheElts,     /* size of L1 cache in elements */
   int imf,             /* index into mflop & see below */
   char pre,            /* type/precision prefix */
   ATL_mvnode_t *kp, /* kernels to use */
   int *M,              /* # of rows to use */
   int *N,              /* # of cols to use */
   int *lda,            /* leading dim to use */
   int *percL1,         /* percL1 to block for */
   int *flushKB         /* flushing size to use */
)
{
   if (imf == 0 || imf == 1 || imf == 2)
   {
      *M = (pre == 's') ? 3000 : 2300;
      *N = (pre == 'z') ? 1000 : 2000;
      *flushKB = FLUSHKB;
      *percL1 = imf ? 400 : 85;
      *percL1 = (imf == 2) ? 0 : *percL1;
   }
   else   /* Time in-cache data with no blocking */
   {
      *flushKB = *percL1 = 0;
      if (imf == 3)  /* L2-contained data */
      {
         *N = ((16+kp->NU-1)/kp->NU)*kp->NU;
         *M = 128*1024/pre2size(pre);
         if (*M > 4*L1CacheElts)
            *M = 4*L1CacheElts;
         *M /= *N;
      }
      else if (imf == 4)  /* L1-contained data */
      {
         *N = ((8+kp->NU-1)/kp->NU)*kp->NU;
         *M = (85*L1CacheElts)/(*N*100);
      }
   }
   *N = (*N/kp->NU)*kp->NU;
   if (!(*N))
      *N = kp->NU;
   if (kp->ldamul == 0)
      *lda = ((*M + 9)/7)*7;  /* make lda prime for worst case */
   else
   {
      int i, sz = pre2size(pre);
      i = kp->ldamul / sz;
      assert(kp->ldamul == i*sz);
      *lda = ((*M + 8 + i-1)/i)*i;
   }
}

static double TimeMyKernel
(int verb,              /* 0: no output, 1 min ouput, 2: full output */
 int FTIME,             /* if nonzero, ignore existing timing file */
                        /* if negative, don't retain timing file */
 ATL_mvnode_t *r1p,     /* ptr to kernel structure */
 char pre,              /* precision prefix */
 ATL_INT M, ATL_INT N,  /* dimensions to time */
 ATL_INT lda,           /* stride between row elements */
 ATL_INT percL1,        /* if 0, time kernel directly wt no blocking */
                        /* if non-zero, block for that % of L1 cache size */
 int nrep,              /* if >=1, # of trials, else use default (3) */
 int imf,               /* which context you are using (diff than timer!) */
 int cflush             /* if >= 0, size of cache flush area, else ignored */
)
{
   double mf;
   #ifdef WALL
      static int fmflop[5] = {0,0,0,0,0};
   #else
      static int fmflop[5] =
         {ATL_nkflop/1000.0,ATL_nkflop/1000.0,ATL_nkflop/1000.0,
          ATL_nkflop/10.0,ATL_nkflop};
   #endif

   mf = TimeMVKernel(verb, FTIME, r1p, pre, M, N, lda, percL1, nrep,
                     fmflop[imf], cflush);
   while (mf == 0.0)
   {
      const int ft= (FTIME < 0) ? -1 : 1;
      fmflop[imf] = (fmflop[imf]) ? (fmflop[imf]<<1) : (ATL_nkflop+999)/1000;
      mf = TimeMVKernel(verb, ft, r1p, pre, M, N, lda, percL1, nrep,
                        fmflop[imf], cflush);
   }
   return(mf);
}

ATL_mvnode_t *TimeAllKernels
(
   int retclone,                /* 0: return w/o cloning, else clone best */
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to (see below) */
   int FTIME,                   /* 0: use prior output files */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *kq             /* queue of kernels */
)
/*
 * This routine times all kernels in kq, putting results in kq->mflop[imf]
 * Fflops: for very large problems, set to 0, else the # of flops to force
 * RETURNS: ptr to fastest kernel
 */
{
   ATL_mvnode_t *kp, *bestp=NULL, *besttp=NULL;
   int percL1, M, N, lda, i;
   double mf, mfmax=0.0, tmfmax=0.0;
   ATL_mvnode_t *TunePF(int L1CacheElts, int imf, char pre, int Fflops,
                           int flushKB, ATL_mvnode_t *kb);

   for (kp=kq; kp; kp = kp->next)
   {
      GetDimsByContext(L1CacheElts, imf, pre, kp, &M, &N, &lda, &percL1, &i);
      mf = ((double)kp->CacheElts) / ((double)L1CacheElts)*100.0;
      percL1 = mf;
      mf = TimeMVKernel(1, FTIME, kp, pre, M, N, lda, percL1, NREP,
                           Fflops, flushKB);
      kp->mflop[imf] = mf;
      if (mf > mfmax)    /* maximum for all kernels */
      {
         bestp = kp;
         mfmax = mf;
      }
      if (FLAG_IS_SET(kp->flag, MVF_PFTUNABLE) && mf > tmfmax)
      {
         besttp = kp;
         tmfmax = mf;
      }
   }
   if (retclone && besttp)  /* have a prefetch tunable kernel we should tune */
   {
      kp = CloneMVNode(besttp);
      kp = TunePF(L1CacheElts, imf, pre, Fflops, flushKB, kp);
      if (kp->mflop[imf] > mfmax)
         return(kp);
      KillMVNode(kp);
   }
   if (retclone)
      bestp = CloneMVNode(bestp);
   return(bestp);
}

ATL_mvnode_t *TimeAllKernelsForContext
(
   int L1CacheElts,     /* size of L1 cache in elements */
   int imf,             /* index into mflop & see below */
   char pre,            /* type/precision prefix */
   int nsamp,           /* # of samples for timing */
   ATL_mvnode_t *bp     /* queue of kernels */
)
/*
 * This routine naively assumes that 4*L1CacheSize is a decent marker for
 * the L2 size to use in initial L2-timings.
 *
 * imf: parameter describing type of timings to perform:
 *    0: large out-of-cache, blocked for 85% of L1CacheSize
 *    1: large out-of-cache, blocked for MIN(128K,4*L1CacheSize) (L2-blocked)
 *    2: large out-of-cache, no blocking
 *    3: in-L2 problem, no blocking
 *    4: in-L1 problem, no blocking
 * RETURNS: best-performing kernel in context
 */
{
   ATL_mvnode_t *r1p, *r1max=NULL;
   double mf, mfmax=0.0;
   int M, N, n, lda, percL1, cflush;

   for (r1p=bp; r1p; r1p = r1p->next)
   {
      GetDimsByContext(L1CacheElts, imf, pre, r1p, &M, &N, &lda, &percL1,
                       &cflush);
      mf = TimeMyKernel(1, 0, r1p, pre, M, N, lda, percL1, nsamp, 0, cflush);
      if (mf > mfmax)
      {
         mfmax = mf;
         r1max = r1p;
      }
      r1p->mflop[imf] = mf;
   }
   return(r1max);
}

static ATL_mvnode_t *DelBadTestKernels(char pre, ATL_mvnode_t *bp)
/*
 * Deletes all kernels that can't pass basic usage test
 */
{
   int die;
   ATL_mvnode_t *p, *prev;
   int m, n, lda, i, j;
   fprintf(stdout, "\nBEGIN BASIC KERNEL TESTS:\n");

   prev = p = bp;
   while(p)
   {
      m = n = lda = 1000;
      if (FLAG_IS_SET(p->flag, MVF_FNU))
      {
         i = p->NU;
         n = ((n+i-1)/i)*i;
      }
      if (p->ldamul)
      {
         j = pre2size(pre);
         i = p->ldamul / j;
         assert(p->ldamul == i*j);
         lda = ((lda+i-1)/i)*i;
      }
      if (MVKernelFailsTest(0, pre, m, n, lda, p))
      {
         fprintf(stdout, "   NUKING bad kernel %s(%d), MU=%d, NU=%d\n",
                 p->rout, p->ID, p->MU, p->NU);
         if (p->genstr)
            fprintf(stdout, "      genstr='%s'\n", p->genstr);
         if (p == bp)
            bp = p = KillMVNode(p);
         else
            prev->next = p = KillMVNode(p);
      }
      else
      {
         fprintf(stdout, "   Kernel %s(%d) passes basic test\n",
                 p->rout, p->ID);
         prev = p;
         p = p->next;
      }
   }
   fprintf(stdout, "DONE BASIC KERNEL TESTS:\n\n");
   return(bp);
}

ATL_mvnode_t *ChooseKernelBlocking
(ATL_mvnode_t *L1,      /* L1 blocked kernel, scope imf=0 */
 ATL_mvnode_t *L2,      /* L2 blocked kernel, scope imf=1 */
 ATL_mvnode_t *NOB      /* no-blocking kernel, scope imf=2 */
)
/*
 * This routine compares 3 different blocking strategy using 1-3 kernels
 * (i.e., they may all be the same kernel).
 * It is possible that the data we use may stay in a very large L3 cache,
 * so only accept no-blocking if it is significantly faster than doing
 * blocking.  L1-blocking will tend to minimize the number of kernels required,
 * so stress it very slightly more than L2 blocking.
 * RETURNS: cloned node of best blocking/kernel
 */
{
   ATL_mvnode_t *best;
   double mf1, mf2, mf3;

   mf1 = L1->mflop[0] * 1.05;  /* give small adv to safest option, L1 blk */
   mf2 = L2->mflop[1] * 1.03;  /* give small adv to blocking over not */
   mf3 = NOB->mflop[2];        /* no blocking loss may vary by size, penalize */
   if (mf1 > mf2 && mf1 > mf3)
      best = L1;
   else if (mf2 > mf1 && mf2 > mf3)
      best = L2;
   else
      best = NOB;
   return(best);
}

double ExhCESrch
/*
 * RETURNS: best mflop found
 */
(
   ATL_mvnode_t *r1p,           /* kernel to search with */
   char pre,                    /* type/precision prefix */
   int M, int N, int lda,
   int stride,                  /* stride to search with, real pL = pL*stride */
   int pLL,                     /* lower percL1 (mul by stride for real val) */
   int pLH,                     /* higher percL1 */
   double mfL,                  /* mflops achieved by lower */
   double mfH,                  /* mflops achieved by higher */
   int *pLB                     /* the best percL1 found */
)
{
   int plm, plb;
   double mf;

   plm = (pLH-pLL)>>1;
   if (plm < 1)
   {
      if (mfL < mfH)
      {
         *pLB = pLH;
         return(mfH);
      }
      *pLB = pLL;
      return(mfL);
   }
   plm += pLL;
   mf = TimeMyKernel(0, 0, r1p, pre, M, N, lda, plm*stride, NREP, 0, FLUSHKB);
   fprintf(stdout, "%6d  %6d  %6d  %6d  %9.2f\n", M, N, lda, plm*stride, mf);
   mfL = ExhCESrch(r1p, pre, M, N, lda, stride, pLL, plm, mfL, mf, pLB);
   mfH = ExhCESrch(r1p, pre, M, N, lda, stride, plm, pLH, mf, mfH, &plb);
   if (mfH > mfL)
   {
      mfL = mfH;
      *pLB = plb;
   }
   return(mfL);
}

void ExhaustiveCESrch
/*
 * Performs an exhaustive search on entire range using recursive halving
 * And modifies CE to be best % of L1 size found.
 */
(
   ATL_mvnode_t *r1p,           /* kernel to search with */
   int imf,                     /* set r1p->mflop[imf] to best perf */
   char pre,                    /* type/precision prefix */
   int M, int N, int lda,       /* prob size to tune with */
   int stride,                  /* stride to search with, real pL = pL*stride */
   int pLL,                     /* lower percL1 (mul by stride for real val) */
   int pLH                      /* higher percL1 */
)
{
   double mfH, mfL;
   int percL1;
   fprintf(stdout, "\nCACHE TUNING FOR %d:'%s', MU=%d NU=%d\n",
           r1p->ID, r1p->rout, r1p->MU, r1p->NU);
   fprintf(stdout, "------------------------------------------\n");
   fprintf(stdout, "     M       N     lda  percL1       mflop\n");
   fprintf(stdout, "======  ======  ======  ======  ==========\n");

   mfL = TimeMyKernel(0, 0, r1p, pre, M, N, lda, pLL*stride, NREP, 0, FLUSHKB);
   fprintf(stdout, "%6d  %6d  %6d  %6d  %9.2f\n", M, N, lda, pLL*stride, mfL);
   mfH = TimeMyKernel(0, 0, r1p, pre, M, N, lda, pLH*stride, NREP, 0, FLUSHKB);
   fprintf(stdout, "%6d  %6d  %6d  %6d  %9.2f\n", M, N, lda, pLH*stride, mfH);
   mfL = ExhCESrch(r1p, pre, M, N, lda, stride, pLL, pLH, mfL, mfH, &percL1);
   fprintf(stdout, "\nBEST CASE %d percent of L1, MFLOP=%.2f\n\n",
           percL1*stride, mfL);
   r1p->mflop[imf] = mfL;
   r1p->CacheElts = percL1*stride;
}

static int GetMaxID(ATL_mvnode_t *r1b)
{
   ATL_mvnode_t *r1p;
   int maxID=0;

   for (r1p=r1b; r1p; r1p = r1p->next)
      maxID = Mmax(maxID, r1p->ID);

   return(maxID);
}
#ifdef ATL_SSE3
void FillInGenNode
(
   ATL_mvnode_t *r1p,   /* data structure to fill in */
   int nmu,             /* unrolling on mu unrolled inner (X) loop */
   int mu,              /* register blk to apply to X */
                        /* total X unrolling is nmu*mu! */
   int nu,              /* unroll&jam on outer (Y) loop */
   int evenlda,         /* assume X&Y have same (mis)align, lda is even */
   int allalign16,      /* assume X,Y,A all aligned to 16 bytes */
   int aptrs            /* use ptrs rather than lda for column indexing */
)
{
   char ln[2048];
}
ATL_mvnode_t *SrchSSEGen
(
   char pre,    /* precision prefix indicating type */
   int maxID,   /* IDs > maxID are safe to use in generation */
   ATL_CINT M,  /* will use (M/MU)*MU as size of prob to time */
   ATL_CINT N,  /* will use (N/NU)*NU as size of prob to time */
   ATL_CINT lda /* leading dimension of array */
)
/*
 * Finds the best kernel the code generator can produce for given problem
 * size w/o blocking.
 * RETURNS: best kernels to be added to the multiple implementation srch
 */
{
   ATL_INT i, j;
/*
 * First, let's find decent NU=1 case to use
 */
   return(NULL);
}
#endif

ATL_mvnode_t *SortRestricted
/*
 * Sorts queue of kernels into a queue of unrestricted kernels (can be
 * always be used) and restricted (only used under certain conditions,
 * such as lda*size a multiple 16).  Destroys R1B in process.
 */
(
   ATL_mvnode_t *R1B,   /* original queue containing restricted & unrest */
   ATL_mvnode_t **R1R   /* queue of only restricted kernels */
)
{
   ATL_mvnode_t *r1B=NULL, *r1R=NULL, *r1b=NULL, *r1r=NULL, *r1p;

   for (r1p=R1B; r1p; r1p = r1p->next)
   {
      if (r1p->ldamul > MVflag2size(r1p->flag) ||
          r1p->alignA > MVflag2size(r1p->flag) ||
          r1p->minN >= 4 || r1p->minM >= 16
         )
      {
         if (r1R)
            r1r->next = r1p;
         else
            r1R = r1p;
         r1r = r1p;
      }
      else
      {
         if (r1B)
            r1b->next = r1p;
         else
            r1B = r1p;
         r1b = r1p;
      }
   }
   if (r1R)
      r1r->next = NULL;
   assert(r1B);
   r1b->next = NULL;
   *R1R = r1R;
   return(r1B);
}

ATL_mvnode_t *EliminateRepeatedRestrictions
(
   int imf,             /* entry in mflop array to read for performance */
   ATL_mvnode_t *R1R    /* queue of already-timed restricted kernels */
)
/*
 * This routine sorts R1R by performance, and then eliminates all slower
 * kernels that handle the same restricted cases.
 *
 * IMPORTANT: what constitutes valid restrictions will change with
 * architectures, so we need to keep this file up-to-date.
 *
 * Things that seem like restrictions but we don't care about:
 *    alignX & Y : handled by copy in ATL_ger
 *    R1F_ALIGNX2A: handled by copy in ATL-ger
 *    R1F_ALLALIGNXY : not presently supported, but would be handled by ATL_ger
 *    R1F_FNU : handled by main loop in ATL_ger
 *
 * Valid restrictions as of 02/14/10 are:
 *    alignA: asserts A must be aligned to alignA%ATL_cachelen boundary
 *            alignA in bytes
 *    ldamul: asserts lda*size must a multiple of ldamul (which is in bytes)
 *    minN: only restriction if >= 4 (else handled by ATL_ger)
 *    minM: only restriction if >= 16 (else handled by ATL_ger)
 */
{
   ATL_mvnode_t *p, *p2, *prev, *curr, *del;

   if (!R1R)
      return(NULL);
   if (!R1R->next)
      return(R1R);
   R1R = ATL_SortMVNodesByMflop(imf, R1R);  /* sort from fastest to slowest */
   for (p=R1R; p && p->next; p = p->next)
   {
      prev = p;
      p2 = p->next;
      while (p2)
      {
         int SAMEA, SAMEL;
         del = NULL;
         SAMEA = (p2->alignA == p->alignA);
         if (!SAMEA && p->alignA && p2->alignA)
            SAMEA = (p2->alignA%p->alignA) == 0;
         SAMEL = p->ldamul == p2->ldamul;
         if (!SAMEL && p->ldamul && p2->ldamul)
            SAMEL = (p2->ldamul%p->ldamul) == 0;
         if (SAMEA && SAMEL)
         {
            if (p->minN < 4 && p->minM < 16)  /* quit if that's all */
               del = p2;
            if (p2->minN >= p->minN && p2->minM >= p->minM)
               del = p2;
            if (p->minN != 0 && p2->minN%p->minN == 0)
               if (p->minM != 0 && p2->minM%p->minM == 0)
                  del = p2;
         }
         if (del)
            p2 = prev->next = KillMVNode(del);
         else
         {
            prev = p2;
            p2 = p2->next;
         }
      }
   }
   return(R1R);
}

ATL_mvnode_t *DecimateAndRankRestricted
(
   int imf,             /* entry in mflop array to read for performance */
   ATL_mvnode_t *urp,   /* unrestricted kernel to be used */
   ATL_mvnode_t *R1R    /* queue of already-timed restricted kernels */
)
/*
 * Takes a timed queue of restricted kernels (with relevant results in
 * (rp->mflop[imf]), and does the following ranking:
 * (1) Any kernel not noticably outperforming urp is removed,
 * (2) Any kernels with the same restrictions are compared, and only
 *     the fastest one is retained
 * (3) Surviving queue are ranked in performance between 1 and NR,
 *     with 1 being the slowest, and NR being the fastest.  This rank
 *     is stored in rp->rankR.  When code is generated, will select kernel
 *     to use by asking if restrictions are satisfied in high-to-low order.
 * (4) RETURNS: queue of surviving kernels, sorted from fastest to slowest
 */
{
   ATL_mvnode_t *rp, *prev;
/*
 * Give 4% advantage to unrestricted; not worth extra instruction load if
 * we can't get the improvement above 4% (often less than clock resolution)
 */
   const double mfur = urp->mflop[imf] * 1.04;
   int i;

/*
 * Get rid of any kernel not noticably faster than unrestricted kernel
 */
   while (R1R && R1R->mflop[imf] < mfur)
      R1R = KillMVNode(R1R);
   if (!R1R)
      return(NULL);
   for (prev=R1R,rp=R1R->next; rp; rp = rp->next)
   {
      if (rp->mflop[imf] < mfur)
      {
         prev->next = KillMVNode(rp);
         rp = prev;
      }
      else
         prev = rp;
   }
   R1R = EliminateRepeatedRestrictions(imf, R1R);
   for (i=ATL_CountNumberOfMVNodes(R1R), rp=R1R; i > 0; rp = rp->next, i--)
      rp->rankR = i;
   return(R1R);
}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);
   fprintf(stderr,"USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -p [s,d,c,z]: set precision prefix \n");
   fprintf(stderr,
           "   -e <fname> : external search filename (& make target)\n");
   fprintf(stderr,
           "   -o <fname> : output filename [<pre>MVTK.sum]\n");
   fprintf(stderr,
           "   -i <fnam> : kernel index file [MVTCASES/<pre>cases.idx]\n");
   fprintf(stderr,
           "   -C <kbytes> : default cache flush size to use\n");
   fprintf(stderr, "   -# <reps> : number of repetitions for each timing\n");
   fprintf(stderr,
      "   -#c <reps> : number of repetitions for each cache block timing\n");
   fprintf(stderr, "   -T <#> : Bitfield controlling detailed timings:\n");
   fprintf(stderr, "      0: if bitfield 0, do normal pruning search\n");
   fprintf(stderr, "      1: perform tests before timing kernels\n");
   fprintf(stderr, "      2: Time unblocked out-of-cache (OOC) performance\n");
   fprintf(stderr, "      4: Time L2-blocked OOC performance\n");
   fprintf(stderr, "      8: Time L1-blocked OOC performance\n");
   fprintf(stderr, "     16: Do Exhaustive search on OOC blocking factors\n");
   fprintf(stderr, "     32: Time in-L2 performance\n");
   fprintf(stderr, "     64: Time in-L1 performance\n");
   fprintf(stderr, "   -v <#> : verbosity level [0]\n");
   fprintf(stderr, "   -2 <kbytes> : size of L2 cache to assume\n");
   exit(ierr ? ierr : -1);
}

ATL_mvnode_t *GetFlags(int nargs, char **args, char *pre, int *ESRCH,
                       char **fin, char **fout)
{
   ATL_mvnode_t *eb=NULL, *ep, *en;
   int i;
   char ch;
   char ln[512];

   NREPCT = NREP = 3;
   FLUSHKB = 8192;
   VERB = 1;
   *ESRCH = 0;
   *fin = NULL;
   *fout = NULL;
   *pre = 'd';

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'o': /* -o <fname> : output filename [<pre>MVK.sum] */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *fout = args[i];
         break;
      case 'i': /* -i <fnam> : kernel index file [MVCASES/<pre>cases.idx] */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *fin = args[i];
         break;
      case 'v': /* -v <verbosity level> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         VERB = atoi(args[i]);
         break;
      case 'C': /* -C <kbytes> : default cache flush size to use */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         FLUSHKB = atoi(args[i]);
         break;
      case '2': /* -C <kbytes> : L2 cache size to assume */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         L2KB = atoi(args[i]);
         break;
      case 'T': /* -T # : do/don't do exhaustive timing */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         *ESRCH = atoi(args[i]);
         break;
      case '#': /* -# <reps>/ -#c : number of repetitions for timing */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         if (args[i-1][2] == 'c')
            NREPCT = atoi(args[i]);
         else
            NREP = atoi(args[i]);
         break;
      case 'e':  /* external file we should read in */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);
         sprintf(ln, "make %s", args[i]);
         if (system(ln) != 0)
            fprintf(stderr, "\nEXTERNAL SEARCH MAKE '%s' FAILED!\n\n", ln);
         else
         {
            en = ReadMVFile(ln+5);
            if (en)              /* We got new results */
            {
               if (!eb)         /* if this first, becomes base pointer */
                  eb = en;
               else             /* else, add it to queue after earlier */
               {
                  for (ep=eb; ep->next; ep = ep->next);
                  ep->next = en;
               }
            }
            else
               fprintf(stderr, "\nEMPTY EXTERNAL SEARCH '%s'!!\n\n", ln);
         }
         break;
      case 'p':  /* -p <pre> */
         if (++i >= nargs)
            PrintUsage(args[0], i, NULL);

         ch = tolower(args[i][0]);
         assert(ch == 's' || ch == 'd' || ch == 'c' || ch == 'z');
         *pre = ch;
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
/*
 * Get filenames in freeable string
 */
   if (!(*fout))
   {
      if (*ESRCH)
         sprintf(ln, "res/%cMVTKtimes.sum", *pre);
      else
         sprintf(ln, "res/%cMVTK.sum", *pre);
      *fout = DupString(ln);
   }
   else
      *fout = DupString(*fout);
   if (!(*fin))
   {
      if (*ESRCH)
         sprintf(ln, "MVTCASES/%cmvtime.idx", *pre);
      else
         sprintf(ln, "MVTCASES/%cmvtcases.idx", *pre);
      *fin = DupString(ln);
   }
   else
      *fin = DupString(*fin);
/*
 * Don't trust external search's own timings & settings: we will test ourselves
 */
   for (ep=eb; ep; ep = ep->next)
   {
      for (i=0; i < 8; i++)
         ep->mflop[0] = 0.0;
      if (ep->kname)
         free(ep->kname);
      ep->kname = NULL;
      ep->rankR = 0;
   }
   return(eb);
}

void WriteMflopExp(FILE *fpout)
{
   fprintf(fpout, "#\n#MFLOP array has following meaning by index:\n");
   fprintf(fpout, "#   0 : Out-of-cache, L1 blocked\n");
   fprintf(fpout, "#   1 : Out-of-cache, L2 blocked\n");
   fprintf(fpout, "#   2 : Out-of-cache, no blocking\n");
   fprintf(fpout, "#   3 : Problem preloaded to L2, no blocking\n");
   fprintf(fpout, "#   4 : Problem preloaded to L1, no blocking\n#\n");
}

void WriteMVSummFile(char *fnam, ATL_mvnode_t *r1b)
{
   char ln[256];
   FILE *fpout;
   ATL_mvnode_t *bases[4], *r1p;
   char *exp[4] = {"out-of-cache operands",
                   "operands preloaded to the L2 cache",
                   "operands preloaded to the L1 cache",
                   "operands out-of-cache, but blocked"
                  };
   int i;

   ATL_MVSplitContexts(r1b, bases, bases+1, bases+2, bases+3);
   fpout = fopen(fnam, "w");
   assert(fpout);
   WriteMflopExp(fpout);
   fprintf(fpout, "#\n#Each kernel context has multiple kernels:\n");
   fprintf(fpout,
           "#   All kernels with rankR > 0 have restrictions, and are only\n");
   fprintf(fpout,
           "#   used when certain conditions are met.  The last kernel in\n");
   fprintf(fpout,
"#   the context series has rankR=0, and can be called for any valid input\n");
   fprintf(fpout, "#\n");
   for (i=0; i < 4; i++)
   {
      fprintf(fpout,
"# ------------------------------------------------------------------------\n");

      fprintf(fpout,
         "# Following %d GEMV kernels are optimized for %s\n",
              bases[i]->rankR+1, exp[i]);
      if (i == 3)
         fprintf(fpout,
              "# for L1 reuse.  These are used in low-rank update\n");
      fprintf(fpout,
"# ------------------------------------------------------------------------\n");
      for (r1p = bases[i]; r1p; r1p = r1p->next)
         PrintMVLine(fpout, r1p);
   }
   fclose(fpout);
/*
 * Restore queue in case it used by caller
 */
   r1b = ATL_MVLinkContexts(bases[0],bases[1],bases[2],bases[3]);
}

void NameAllKernels(char pre, ATL_mvnode_t *r1b)
/*
 * Gives all kernels unique names.  For now, just name them gerk_<ID>
 */
{
   ATL_mvnode_t *p;
   char ln[64];

   for (p=r1b; p; p = p->next)
   {
      sprintf(ln, "ATL_%cmvtk__%d", pre, p->ID);
      p->kname = DupString(ln);
   }
}

static int Mylcm(const int M, const int N)
/*
 * Returns least common multiple (LCM) of two positive integers M & N by
 * computing greatest common divisor (GCD) and using the property that
 * M*N = GCD*LCM.
 */
{
   register int tmp, max, min, gcd=0;

   if (M != N)
   {
      if (M > N) { max = M; min = N; }
      else { max = N; min = M; }
      if (min > 0)  /* undefined for negative numbers */
      {
         do  /* while (min) */
         {
            if ( !(min & 1) ) /* min is even */
            {
               if ( !(max & 1) ) /* max is also even */
               {
                  do
                  {
                     min >>= 1;
                     max >>= 1;
                     gcd++;
                     if (min & 1) goto MinIsOdd;
                  }
                  while ( !(max & 1) );
               }
               do min >>=1 ; while ( !(min & 1) );
            }
/*
 *          Once min is odd, halve max until it too is odd.  Then, use
 *          property that gcd(max, min) = gcd(max, (max-min)/2)
 *          for odd max & min
 */
MinIsOdd:
            if (min != 1)
            {
               do  /* while (max >= min */
               {
                  max -= (max & 1) ? min : 0;
                  max >>= 1;
               }
               while (max >= min);
            }
            else return( (M*N) / (1<<gcd) );
            tmp = max;
            max = min;
            min = tmp;
         }
         while(tmp);
      }
      return( (M*N) / (max<<gcd) );
   }
   else return(M);
}

ATL_mvnode_t *FindSmallestNU(ATL_mvnode_t *r1b)
/*
 * RETURNS: ptr to node wt smallest NU setting
 */
{
   ATL_mvnode_t *r1min, *r1p;
   int numin;

   if (!r1b)
      return(NULL);
   numin = r1b->NU;
   r1min = r1b;
   for (r1p=r1b; r1p; r1p = r1p->next)
   {
      if (r1p->NU < numin)
      {
         numin = r1p->NU;
         r1min = r1p;
      }
   }
   return(r1min);
}

ATL_mvnode_t *SortQByNU(ATL_mvnode_t *r1b)
/*
 * Sorts queue on NU (greatest-to-least)
 * RETURNS: pointer to new Q sorted from greatest to least on NU (NU)
 */
{
   ATL_mvnode_t *r1s=NULL, *r1p, *r1m;
   while (r1b)
   {
      r1m = FindSmallestNU(r1b);
      if (r1m == r1b)
         r1b = r1b->next;
      else
      {
         for (r1p=r1b; r1p->next != r1m; r1p = r1p->next);
         r1p->next = r1p->next->next;
      }
      r1m->next = r1s;
      r1s = r1m;
   }
   return(r1s);
}

ATL_mvnode_t *FindPrefInst
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int  M, int N, int lda,      /* problem dimensions */
   int Fflops,                  /* what to set Force flops to (see below) */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *k0,         /* kernel to tune prefetch on */
   char arrc,                   /* A, X, or Y */
   int ninst,                   /* # of pref inst to try */
   char **pfinst                /* array of prefetch suffixes */
)
{
   ATL_mvnode_t *kb, *kp, *kB;
   double mf;
   int i, j, percL1;

   printf("\n   OP  PREF    MU    NU         MFLOP\n");
   printf(  "   ==  ====  ====  ====  ============\n");
   printf( "    %c   DEF %5d %5d  %12.2f\n", arrc, k0->MU, k0->NU, k0->mflop[imf]);
   kB = k0;
   kb = NULL;
/*
 * Retime default case before we believe its results (in case prior timing
 * was on different prob size or CacheElts or something)
 */
   mf = ((double)k0->CacheElts) / ((double)L1CacheElts)*100.0;
   percL1 = mf;
   mf = TimeMyKernel(0, 1, k0, pre, M, N, lda, percL1, Mmax(NREP,8),
                     imf, flushKB);
   k0->mflop[imf] = mf;
   for (i=0; i < ninst; i++)
   {
      #ifndef ATL_3DNow
         if (pfinst[i][0] == 'w' && pfinst[i][1] == '\0')
            continue;
      #endif
      kp = CloneMVNode(k0);
      if (kp->cflags)
      {
         j = strlen(kp->cflags) + 20;
         kp->cflags = GetLongerString(kp->cflags, j);
         sprintf(kp->cflags+j-20, " -DPFI%c=prefetch%s", arrc, pfinst[i]);
      }
      else
      {
         char ln[32];
         sprintf(ln, "-DPFI%c=prefetch%s", arrc, pfinst[i]);
         kp->cflags = DupString(ln);
         SET_FLAG(kp->flag, MVF_ADDCFLAGS, 1);
      }
      mf = ((double)kp->CacheElts) / ((double)L1CacheElts)*100.0;
      percL1 = mf;
      mf = TimeMyKernel(0, -1, kp, pre, M, N, lda, percL1, Mmax(NREP,8),
                        imf, flushKB);
      kp->mflop[imf] = mf;
      printf("    %c   %3.3s %5d %5d  %12.2f\n", arrc, pfinst[i], kp->MU, kp->NU, mf);
      if (mf > kB->mflop[imf])
         kB = kp;
      kp->next = kb;
      kb = kp;
   }
   kp = CloneMVNode(kB);
   KillAllMVNodes(kb);
   return(kp);
}

ATL_mvnode_t *TunePFINST
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to (see below) */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *kb          /* kernel to tune prefetch on */
)
/*
 * If kb is a kernel that you can tune the prefetch instruction type
 * using ATLAS's PFI[A,X,Y] macros, try all combinations to find the best
 * RETURNS: node ptr to best performing variant.
 * NOTE: kb is destroyed.
 */
{
   double mf;
   int M, N, lda, i, j;      /* problem dimensions */
#ifdef ATL_3DNow
   const int nA=5, nY=6;
   char *pfsA[5] = {"", "nta", "t0", "t1", "t2"};
   char *pfsY[6] = {"w", "", "nta", "t0", "t1", "t2"};
#else
   const int nA=4;
   char *pfsA[4] = {"nta", "t0", "t1", "t2"};
   #define nY nA
   #define pfsY pfsA
#endif
   #define nX nA
   #define pfsX pfsA
   if (!FLAG_IS_SET(kb->flag, MVF_PFTUNABLE))
      return(kb);
/*
 * First, find best case for prefetch of A, then for X, then for Y
 */
   mf = kb->mflop[imf];
   GetDimsByContext(L1CacheElts, imf, pre, kb, &M, &N, &lda, &i, &j);
   printf("\nTUNING PREFETCH INST, imf=%d, BASELINE MFLOP=%.2f\n", imf, mf);
   kb = FindPrefInst(L1CacheElts, imf, pre, M, N, lda, Fflops, flushKB,
                     kb, 'A', nA, pfsA);
   kb = FindPrefInst(L1CacheElts, imf, pre, M, N, lda, Fflops, flushKB,
                     kb, 'X', nX, pfsX);
   kb = FindPrefInst(L1CacheElts, imf, pre, M, N, lda, Fflops, flushKB,
                     kb, 'Y', nY, pfsY);
   printf("\nBEST MFLOP[%d]=%.2f (%.2f percent improvement)\n",
          imf, kb->mflop[imf], 100*(kb->mflop[imf]/mf - 1.0));
   printf("   cflags='%s'\n", kb->cflags);
   return(kb);
}

ATL_mvnode_t *FindPrefDist
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int  M, int N, int lda,      /* problem dimensions */
   int percL1,                  /* % of L1 to block for */
   int Fflops,                  /* what to set Force flops to (see below) */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *kb,         /* kernel to tune prefetch on */
   char arrc                    /* A, X, or Y */
)
{
   double mf, mfB;
   int maxd = 256, slen, lo, hi, dB, i;
   ATL_mvnode_t *kp;
   const int DEFFLAGS = (kb->cflags == NULL);

   if (DEFFLAGS)
      kb->cflags = GetKCFlags(pre);
   printf("\nTUNING PREFETCH DISTANCE FOR OPERAND '%c', imf=%d:\n", arrc, imf);
   if (pre == 'd' || pre == 'c')
      maxd *= 8;
   else if (pre == 's')
      maxd *= 4;
   else
      maxd *= 16;
/*
 * Retime default case to ensure perf is all up-to-date
 */
   mf = TimeMyKernel(0, 1, kb, pre, M, N, lda, percL1, Mmax(NREP,8),
                     imf, flushKB);
   kb->mflop[imf] = mf;
   printf("   OP  DIST    MU    NU         MFLOP\n");
   printf("   ==  ====  ====  ====  ============\n");
   printf("    %c   DEF %5d %5d  %12.2f\n", arrc, kb->MU, kb->NU, kb->mflop[imf]);
/*
 * First, starting with 64 bytes, double distance (hi starts at 256) until
 * performance drops
 * or we reach 256 elements (max distance ahead).
 */
   slen = strlen(kb->cflags);
   kp = CloneMVNode(kb);
   mfB = 0.0;
   i = 64;
   while (i < maxd)
   {
      if (kp->cflags)
         free(kp->cflags);
      kp->cflags = DupString(kb->cflags);
      kp->cflags = GetLongerString(kp->cflags, slen + 16);
      sprintf(kp->cflags+slen, " -DPF%cDIST=%d", arrc, i);
      mf = TimeMyKernel(0, -1, kp, pre, M, N, lda, percL1, Mmax(NREP,8),
                        imf, flushKB);
      printf("    %c %5d %5d %5d  %12.2f\n", arrc, i, kb->MU, kb->NU, mf);
      if (mf <= mfB)
         break;
      mfB = mf;
      i = (i == 64) ? 256 : (i<<1);
   }
/*
 * Now refine distance in steps of 64
 */
   dB = (i>>1);
   hi = i;
   for (i=dB+64; i < hi; i += 64)
   {
      if (kp->cflags)
         free(kp->cflags);
      kp->cflags = DupString(kb->cflags);
      kp->cflags = GetLongerString(kp->cflags, slen + 16);
      sprintf(kp->cflags+slen, " -DPF%cDIST=%d", arrc, i);
      mf = TimeMyKernel(0, -1, kp, pre, M, N, lda, percL1, Mmax(NREP,8),
                        imf, flushKB);
      printf("    %c %5d %5d %5d  %12.2f\n", arrc, i, kb->MU, kb->NU, mf);
      if (mf > mfB)
      {
         mfB = mf;
         dB = i;
      }
   }
/*
 * Try no prefetch at all (signaled by setting DIST=0)
 */
   if (kp->cflags)
      free(kp->cflags);
   kp->cflags = DupString(kb->cflags);
   kp->cflags = GetLongerString(kp->cflags, slen + 16);
   sprintf(kp->cflags+slen, " -DPF%cDIST=0", arrc);
   mf = TimeMyKernel(0, -1, kp, pre, M, N, lda, percL1, Mmax(NREP,8),
                     imf, flushKB);
   printf("    %c %5d %5d %5d  %12.2f\n", arrc, 0, kb->MU, kb->NU, mf);
   if (mf > mfB)
   {
      mfB = mf;
      dB = 0;
   }
   KillMVNode(kp);
   if (mfB > kb->mflop[imf])
   {
      kb->cflags = GetLongerString(kb->cflags, slen + 16);
      sprintf(kb->cflags+slen, " -DPF%cDIST=%d", arrc, dB);
      printf("BEST imf=%d PF%cDIST=%d (%.2f)\n", imf, arrc, dB, mfB);
      return(kb);
   }
   printf("DEFAULT imf=%d %cDISTANCE IS BEST (%.2f)\n",
          imf, arrc, kb->mflop[imf]);
   if (DEFFLAGS)
   {
      free(kb->cflags);
      kb->cflags = NULL;
   }
   return(kb);
}

ATL_mvnode_t *TunePFDIST
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to (see below) */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *kb          /* kernel to tune prefetch on */
)
{
   int i, j, M, N, lda, percL1;
   double mf;
   if (!FLAG_IS_SET(kb->flag, MVF_PFTUNABLE))
      return(kb);
/*
 * Find best prefetch distance for all operands
 */
   mf = ((double)kb->CacheElts) / ((double)L1CacheElts)*100.0;
   percL1 = mf;
   mf = kb->mflop[imf];
   GetDimsByContext(L1CacheElts, imf, pre, kb, &M, &N, &lda, &i, &j);
   kb = FindPrefDist(L1CacheElts, imf, pre, M, N, lda, percL1, Fflops,
                     flushKB, kb, 'A');
   kb = FindPrefDist(L1CacheElts, imf, pre, M, N, lda, percL1, Fflops,
                     flushKB, kb, 'X');
   kb = FindPrefDist(L1CacheElts, imf, pre, M, N, lda, percL1, Fflops,
                     flushKB, kb, 'Y');
/*
 * Retime kernel with improved prefetch
 */
   kb->mflop[imf] = TimeMyKernel(0, 1, kb, pre, M, N, lda, percL1,
                                 Mmax(NREP,8), imf, flushKB);
   printf(
      "\nPREFETCH DISTANCE TUNING imf=%d improved from %.2f to %.2f (%.2f percent)\n",
          imf, mf, kb->mflop[imf], (kb->mflop[imf]/mf)*100.0-100.0);
   return(kb);
}

ATL_mvnode_t *TunePF
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *kb          /* kernel to tune prefetch on */
)
/*
 * This routine tunes the prefetch parameters for kb
 */
{
/*
 * First tune the instruction types, then the prefetch distances
 */
   #if defined(ATL_SSE1)
      kb = TunePFINST(L1CacheElts, imf, pre, Fflops, flushKB, kb);
   #endif
   return(TunePFDIST(L1CacheElts, imf, pre, Fflops, flushKB, kb));
}

ATL_mvnode_t *TuneBestPF
(
   int replace,                 /* 0: return new node, else replace old */
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *kb          /* kernel to tune prefetch on */
)
/*
 * Assumes queue kb has been timed already for mflop[imf].  Finds the fastest
 * prefetch tunable kernel, and tunes it.
 * RETURNS: new node with best performing kernel.
 */
{
   double mfB, mftB;
   ATL_mvnode_t *kp, *tkpB, *kpB;

   mfB = mftB = 0;
   tkpB = kpB = NULL;
   for (kp=kb; kp; kp = kp->next)
   {
      if (FLAG_IS_SET(kp->flag, MVF_PFTUNABLE))
      {
         if (kp->mflop[imf] > mftB)
         {
            mftB = kp->mflop[imf];
            tkpB = kp;
         }
      }
      if (kp->mflop[imf] > mfB)
      {
         mfB = kp->mflop[imf];
         kpB = kp;
      }
   }
   assert(kpB);          /* ERROR: die if no kernel has valid timing */
   if (kpB == tkpB)
   {
      kpB = NULL;
      mfB = 0.0;
   }
/*
 * If replace, replace current best tunable kernel with tuned version and
 * return the possibly new queue head
 */
   if (replace)
   {
      if (tkpB)
      {
         kpB = CloneMVNode(tkpB);
         kpB = TunePF(L1CacheElts, imf, pre, Fflops, flushKB, kpB);
         if (tkpB != kb)
         {
            for (kp=kb; kp->next != tkpB; kp = kp->next);
            kpB->next = KillMVNode(tkpB);
            kp->next = kpB;
         }
         else
         {
            kpB->next = KillMVNode(kb);
            kb = kpB;
         }
      }
      return(kb);
   }
   if (tkpB)
   {
      tkpB = CloneMVNode(tkpB);
      tkpB = TunePF(L1CacheElts, imf, pre, Fflops, flushKB, tkpB);
   }
   else if (replace)
      return(NULL);
   if (!tkpB)
      return(CloneMVNode(kpB));
   else if (!kpB)
      return(tkpB);
   else if (tkpB->mflop[imf] > kpB->mflop[imf])
      return(tkpB);
   KillMVNode(tkpB);
   return(CloneMVNode(kpB));
}

ATL_mvnode_t *TuneExtractMU
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to (see below) */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   ATL_mvnode_t *klo         /* kernel wt good NU already selected */
)
{
   ATL_mvnode_t *khi, *kp;
   int i, maxU, minU, bestU, CL;
   double mfB;

   if (pre == 'd' || pre == 'c')
      CL = 8;
   else
      CL = (pre == 'z') ? 4 : 16;
   printf("\nBEGIN MU EXTRACT SEARCH, imf=%d\n", imf);
   printf("   NU=%d, MU=%d, mflop[%d]=%.2f\n", klo->NU, klo->MU,
          imf, klo->mflop[imf]);
   khi = CloneMVNode(klo);
   khi->minM = khi->MU = (klo->MU)<<1;
   FillInMVExtractGenStrings(pre, khi);
   TimeAllKernelsForContext(L1CacheElts, imf, pre, Mmax(NREP,8), khi);
   printf("   NU=%d, MU=%d, mflop[%d]=%.2f\n", khi->NU, khi->MU,
          imf, khi->mflop[imf]);
   while (khi->mflop[imf] > klo->mflop[imf] && klo->MU < 256)
   {
      kp = klo;
      klo = khi;
      khi = kp;
      khi->minM = khi->MU = (klo->MU)<<1;
      FillInMVExtractGenStrings(pre, khi);
      TimeAllKernelsForContext(L1CacheElts, imf, pre, Mmax(NREP,8), khi);
      printf("   NU=%d, MU=%d, mflop[%d]=%.2f\n", khi->NU, khi->MU,
             imf, khi->mflop[imf]);
   }
   minU = klo->MU;
   maxU = (khi->MU >= 256) ? 256 : khi->MU;
   if (khi->mflop[imf] > klo->mflop[imf])
   {
      bestU = khi->MU;
      mfB = khi->mflop[imf];
   }
   else
   {
      bestU = klo->MU;
      mfB = klo->mflop[imf];
   }
   KillAllMVNodes(khi);
   for (i=minU+CL; i < maxU; i += CL)
   {
      klo->minM = klo->MU = i;
      FillInMVExtractGenStrings(pre, klo);
      TimeAllKernelsForContext(L1CacheElts, imf, pre, Mmax(NREP,8), klo);
      if (klo->mflop[imf] > mfB)
      {
         mfB = klo->mflop[imf];
         bestU = i;
      }
      printf("   NU=%d, MU=%d, mflop[%d]=%.2f\n", klo->NU, klo->MU,
             imf, klo->mflop[imf]);
   }
/*
 * Try unrollings below CL for generators that support it
 */
   if (klo->asmbits != asmNames2bitfield("GAS_x8664"))
   {
      i = 1;
      while (i < CL)
      {
         klo->minM = klo->MU = i;
         FillInMVExtractGenStrings(pre, klo);
         TimeAllKernelsForContext(L1CacheElts, imf, pre, Mmax(NREP,8), klo);
         if (klo->mflop[imf] > mfB)
         {
            mfB = klo->mflop[imf];
            bestU = i;
         }
         printf("   NU=%d, MU=%d, mflop[%d]=%.2f\n", klo->NU, klo->MU,
                imf, klo->mflop[imf]);
         i = (i < 4) ? (i<<1) : i + 4;
      }
   }
   klo->mflop[imf] = mfB;
   klo->minM = klo->MU = bestU;
   FillInMVExtractGenStrings(pre, klo);
   printf("BEST MU-TUNED GENERATED KERNEL FOR imf=%d: NU=%d, MU=%d, MF=%.2f\n",
          imf, klo->NU, bestU, mfB);
   return(klo);
}

ATL_mvnode_t *TuneExtractMUNU
(
   int L1CacheElts,             /* size of L1 cache in elements */
   int imf,                     /* index into bp->mflop  */
   char pre,                    /* type/precision prefix */
   int Fflops,                  /* what to set Force flops to (see below) */
   int flushKB,                 /* 0: no flushing, else mem to flush */
   int isuff                    /* 0: C, 1:C/SSE, 2:AMD64/SSE */
)
/*
 * This routine uses extract and the basefile atlas-l2g.base to generate
 * MV kernels.  It exercises only M & N loop unrolling, leaving
 * all other parameters at their defaults (they will be searched later).
 */
{
   ATL_mvnode_t *kp, *kb, *kret, *b2, *k2;
/*
 * These variables document the number and values of valid N unrollings by
 * precision.  Will need to change when more support is added to l2g.base.
 */
   const int dnnu=8, dNUs[8] = {1, 2, 4, 6, 8, 10, 12, 14};
   const int snnu=6, sNUs[6] = {1, 2, 4, 8, 12, 14};
   const int znnu=6, zNUs[6] = {1, 2, 3, 4, 5, 6};
   const int cnnu=5, cNUs[5] = {1, 2, 3, 4, 6};
   const int cxnnu=6, xnnu=8, xNUs[8] = {1, 2, 4, 8, 12, 16, 20, 24};
   int i, j, nnu, mu, nu, CL;
   const int *NUs;
   char *suffs[3] = {"C", "Csse", "sse"};
   char ln[128];
   #if !defined(ATL_GAS_x8664) || !defined(ATL_SSE3)
      if (isuff == 2)
         return(NULL);
   #endif
   if (isuff != 0 && isuff != 2)
      return(NULL);

   fprintf(stdout, "BEGIN NU/MU EXTRACT SEARCH, imf=%d:\n", imf);
   switch(pre)
   {
   case 's' :
      if (isuff == 2)
      {
         nnu = snnu-1;
         NUs = sNUs;
      }
      else
      {
         nnu = xnnu;
         NUs = xNUs;
      }
      CL = 16;
      break;
   case 'z' :
      if (isuff == 2)
      {
         nnu = znnu;
         NUs = zNUs;
      }
      else
      {
         nnu = cxnnu;
         NUs = xNUs;
      }
      CL = 4;
      break;
   case 'c' :
      if (isuff == 2)
      {
         nnu = cnnu;
         NUs = cNUs;
      }
      else
      {
         nnu = cxnnu;
         NUs = xNUs;
      }
      CL = 8;
      break;
   default :
      if (isuff == 2)
      {
         nnu = dnnu-1;
         NUs = dNUs;
      }
      else
      {
         nnu = xnnu;
         NUs = xNUs;
      }
      CL = 8;
   }
/*
 * Create a queue of all legal NU with MU=1
 */
   kp = GetMVNode();
   kp->ID = 900000;        /* ID=900,000-999,999 reserved for genned codes */
   sprintf(ln, "%cmvt_%s.c", pre, suffs[isuff]);
   kp->rout = DupString(ln);
   if (isuff == 2)
   {
      kp->cflags = DupString("-x assembler-with-cpp");
      kp->asmbits = asmNames2bitfield("GAS_x8664");
   }
   else
      kp->cflags = GetKCFlags(pre);
   if (isuff)
   {
      kp->comp = DupString(GetGoodGcc());
      kp->alignX = kp->alignY = 16;
      kp->SSE = 3;
   }
   kp->auth = DupString("R. Clint Whaley");
   kp->exflags = DupString("MVTKdir=EXTDIR");
   kp->MU = kp->minM = CL;
   kp->NU = kp->minN = NUs[nnu-1];
   kp->TA = AtlasTrans;
   SET_FLAG(kp->flag, MVF_FNU, 1);
   SET_FLAG(kp->flag, MVF_INCYISONE, 1);
   SET_FLAG(kp->flag, MVF_PFTUNABLE, 1);
   kp->next = NULL;

   kb = kp;
   kp = CloneMVNode(kb);
   kp->NU = kp->minN = NUs[nnu-1];
   kp->MU = kp->minM = 2*CL;
   b2 = kp;
/*
 * Get all legal N unrollings added to queue, using MU=1, and roughly constant
 * number of inst, with MU=2 for largest NU;  The NU unrollings with constant
 * # of inst should minimize the choice of large NU based purely on loop
 * overhead reduction.  Smaller NU are better due to less conflict misses,
 * so only use them when real improvement due to vector reuse is seen.
 */
   j = NUs[nnu-1]*2*CL;  /* total number of inst */
   for (i=nnu-2; i >= 0; i--)
   {
      int big, small;
      if (isuff == 2 &&  NUs[i] == 3 && pre == 'c' && kp->TA == AtlasTrans)
         continue;  /* NU=3 not supported for scplx transpose */
/*
 *    Add MU=1 case
 */
      kp = CloneMVNode(kb);
      kp->NU = kp->minN = NUs[i];
      kp->next = kb;
      kb = kp;
/*
 *    Add MU >=2 case, where # of inst kept roughly constant
 */
      kp = CloneMVNode(b2);
      kp->NU = kp->minN = NUs[i];
      big = (kp->MU+CL) * NUs[i];
      big -= j;
      small = (kp->MU) * NUs[i];
      small -= j;
      big = Mabs(big);
      small = Mabs(small);
      kp->MU = kp->minM = (small < big) ? (kp->MU) : (kp->MU)+CL;
      kp->next = b2;
      b2 = kp;
   }
/*
 * Join queues together, create generation strings, and time.
 */
   for (kp=kb; kp->next; kp = kp->next);
   kp->next = b2;
   FillInMVExtractGenStrings(pre, kb);
   #ifndef DEBUG
      kb = DelBadTestKernels(pre, kb);
   #endif
   kp = TimeAllKernelsForContext(L1CacheElts, imf, pre, Mmax(NREP,8), kb);
   assert(kp);
/*
 * Print out results of NU search
 */
   for (k2=kb; k2; k2 = k2->next)
      printf("   mu=%d, nu=%d, mflop[%d]=%.2f\n", k2->MU, k2->NU, imf,
             k2->mflop[imf]);
/*
 * Choose best performing case for MU exploration, delete NU queue
 */
   kp = CloneMVNode(kp);
   KillAllMVNodes(kb);
   kp->next = NULL;
   printf("BEST GENERATED NU-TUNED KERNEL FOR imf=%d: MU=%d, NU=%d, MF=%.2f\n",
          imf, kp->MU, kp->NU, kp->mflop[imf]);
   kb = TuneExtractMU(L1CacheElts, imf, pre, Fflops, flushKB, kp);
   assert(DelBadTestKernels(pre,kb) == kb);
   return(kb);
}

void ExhaustiveTime(char pre, char *fin, char *fout, int lvl)
/*
 * This routine attempts to time every kernel for all contexts, with the
 * out-of-cache blocking being individually tuned for each routine.  It is
 * used to compare differing implementations head-to-head, and does no
 * kernel selection, just the timing.
 * All timings use all operands aligned to ATL_Cachelen, incX=incY=1.
 */
{
   ATL_mvnode_t *r1b, *r1p, *r1OOC, *r1L1, *r1L2, *r1s;
   ATL_mvnode_t *r1L1b=NULL, *r1L2b=NULL;
   double mfB;
   char *blkstr;
   FILE *fpout;
   size_t Nooc=8000/16, Mooc=8000*16;  /* rough dims of out-of-cache problem */
   int Nl2, Ml2, Nl1, Ml1, lcmM, lcmN, minNU, maxNU, n;
   int L1ce, L2ce, fflops, L2CacheElts, L1CacheElts, i;

   r1b = ReadMVFile(fin);  /* get kernels to be timed */
   assert(r1b);
   r1b = DelRepeatedMVKernels(r1b);
   r1b = DelBadArchMVKernels(r1b);
   if (lvl & 1)
      r1b = DelBadTestKernels(pre, r1b);
   assert(r1b);
   fprintf(stdout, "\nSurviving cases:\n");
   WriteMVFile("stdout", r1b);
/*
 * Find M/N unrollings that are multiples of all kernel's native blocking
 * Force lcmM to be at least 32 bytes long to avoid alignment issues
 */
   if (pre == 's')
      lcmM = 8;
   else if (pre == 'd' || pre == 'c')
      lcmM = 4;
   else
      lcmM = 2;
   lcmN = 1;
   maxNU = minNU = r1b->NU;
   for (r1p=r1b; r1p; r1p = r1p->next)
   {
      lcmM = Mylcm(lcmM, r1p->MU);
      lcmN = Mylcm(lcmN, r1p->NU);
      if (minNU > r1p->NU)
         minNU = r1p->NU;
      if (maxNU < r1p->NU)
         maxNU = r1p->NU;
   }
/*
 * Set out-of-cache dimensions; first compute minimum acceptable values
 */
   L2CacheElts = (L2KB) ? (L2KB*1024)/pre2size(pre) : 4*L1CacheElts;
   Mooc = (1.1*L2CacheElts)/(minNU+1);  /* OOC working set for everyone */
   if (Mooc >= lcmM)
      Mooc = ((Mooc+lcmM-1)/lcmM)*lcmM;
   Nooc = ((8*maxNU+lcmN-1)/lcmN)*lcmN;
   if (Nooc < 100)
      Nooc = ((100+lcmN-1)/lcmN)*lcmN;
/*
 * Adjust M upward until matrix at least as big as 8000x8000
 */
   if (Mooc * Nooc < 8000*8000)
   {
      Mooc = (8000*8000)/Nooc;
      if (Mooc >= lcmM)
         Mooc = ((Mooc+lcmM-1)/lcmM)*lcmM;
   }
/*
 * If matrix bigger than 488MB, may cause swapping to time it, so
 * reduce Nooc, at the cost of having to floor it with NU
 */
   if (Mooc*Nooc*pre2size(pre) > 8000*8000*sizeof(double))
   {
      Nooc = (8000*8000*sizeof(double))/pre2size(pre);
      Nooc = (Nooc)/Mooc;
      Nooc = ((Nooc+maxNU-1)/maxNU)*maxNU;
      if (Nooc < maxNU)
         Nooc = maxNU;
   }


/*
 * Force in-cache problems to do 3 mflops for timing
 */
   fflops = 3;
/*
 * Set in-L2 cache problem dimensions, estimate use of 80% of cache
 */
   Nl2 = lcmN;
   Ml2 = 0.8*L2CacheElts;
   Ml2 /= Nl2;
   if (Ml2 >= lcmM+lcmM)
      Ml2 = (Ml2/lcmM)*lcmM;
/*
 * If M dimension strongly constrained, rescale N dim and accept slightly
 * differing problem sizes.  Need Q sorted from greatest to least, so that
 * we ensure the common N can use the floor operation with NU in order
 * to vary the N dim to work with all NU
 */
   if (Ml2 < 480)
   {
      r1s = CloneMVQueue(r1b);  /* get dup Q */
      r1s = SortQByNU(r1s);     /* sort from greatest to least */
      Ml2 = 0.8*L2CacheElts;
      i = Nl2 = r1s->NU;
      for (r1p=r1s->next; r1p; r1p = r1p->next)
      {
         i = Mylcm(i, r1p->NU);
         if (i*480 > Ml2) break;
         Nl2 = i;
      }
      KillAllMVNodes(r1s);
      Ml2 /= Nl2;
      if (Ml2 >= lcmM+lcmM)
         Ml2 = (Ml2/lcmM)*lcmM;
   }
/*
 * Set in-L1 cache problem dimensions, estimate use of 90% of cache
 */
   L1CacheElts = (GetL1CacheSize()*1024) / pre2size(pre);
   Nl1 = lcmN;
   Ml1 = 0.9*L1CacheElts;
   Ml1 /= Nl1;
   Ml1 = (Ml1/lcmM)*lcmM;
   if (Ml1 >= lcmM+lcmM)
      Ml1 = (Ml1/lcmM)*lcmM;
   if (Ml1 < 120)
   {
      r1s = CloneMVQueue(r1b);  /* get dup Q */
      r1s = SortQByNU(r1s);     /* sort from greatest to least */
      Ml1 = 0.9*L2CacheElts;
      i = Nl1 = r1s->NU;
      for (r1p=r1s->next; r1p; r1p = r1p->next)
      {
         i = Mylcm(i, r1p->NU);
         if (i*120 > Ml1) break;
         Nl1 = i;
      }
      KillAllMVNodes(r1s);
      Ml1 /= Nl1;
      if (Ml1 >= lcmM+lcmM)
         Ml1 = (Ml1/lcmM)*lcmM;
   }
   printf("lcmM=%d, lcmN=%d\n", lcmM, lcmN);
   printf("Mooc=%d, Nooc=%d;  ML2=%d, NL2=%d;  ML1=%d NL1=%d\n",
          (int)Mooc, (int)Nooc, Ml2, Nl2, Ml1, Nl1);
   if (lvl & 2)
   {
/*
 *    Find kernels unblocked performance for out-of-cache timings
 */
      printf("\nTIMING OUT-OF-CACHE CASES WITH NO BLOCKING, M=%d, N=%d\n",
             (int)Mooc, (int)Nooc);
      for (r1p=r1b; r1p; r1p = r1p->next)
      {
         r1p->CacheElts = 0;
         r1p->mflop[2] = TimeMVKernel(VERB, 0, r1p, pre, (Mooc/r1p->MU)*r1p->MU,
                                      (Nooc/r1p->NU)*r1p->NU, Mooc, 0,
                                      NREP, fflops, 0);
      }
   }
/*
 * Handle blocking tuning differently if we need exhaustive search
 */
   if (lvl & 16)
   {
      if (lvl & 8) /* time L1-blocked OOC? */
      {
         r1L1b = CloneMVQueue(r1b);  /* get dup Q */
         for (r1p=r1L1b, r1s=r1b; r1p; r1p = r1p->next, r1s=r1s->next)
         {
            n = (Nooc/r1p->NU)*r1p->NU;
            ExhaustiveCESrch(r1p, 0, pre, (Mooc/r1p->MU)*r1p->MU, n, Mooc,
                             2, 25, 50);
            r1p->CacheElts = L1CacheElts*0.01*r1p->CacheElts;
            if (r1p->mflop[0] >= r1s->mflop[2])
               r1s->CacheElts = r1p->CacheElts;
            r1s->mflop[0] = r1p->mflop[0];
         }
      }
      if (lvl & 4)  /* time L2-blocked OOC? */
      {
         r1L2b = CloneMVQueue(r1b);  /* get dup Q */
         for (r1p=r1L2b, r1s=r1b; r1p; r1p = r1p->next, r1s = r1s->next)
         {
            n = (Nooc/r1p->NU)*r1p->NU;
            r1p->CacheElts = L2CacheElts;
            i = 2*L2CacheElts / (L1CacheElts);
            i = Mmax(i, 16);
            ExhaustiveCESrch(r1p, 1, pre, (Mooc/r1p->MU)*r1p->MU, n, Mooc,
                             50, 3, i);
            r1p->CacheElts = L1CacheElts*0.01*r1p->CacheElts;
            if (r1p->mflop[1] >= r1s->mflop[2] && r1p->mflop[1] > r1s->mflop[0])
               r1s->CacheElts = r1p->CacheElts;
            r1s->mflop[1] = r1p->mflop[1];
         }
      }
      if (lvl & 2)  /* Timed no-blocking results */
      {
         printf("\nNO BLOCKING RESULTS:\n");
         printf("--------------------\n");
         for (r1p=r1b; r1p; r1p = r1p->next)
            printf("   %d:%s, M=%d, N=%d, CacheElts=%d, gets =%.2f MFLOPS\n",
                   r1p->ID, r1p->rout, (int)(Mooc/r1p->MU)*r1p->MU,
                   (int)(Nooc/r1p->NU)*r1p->NU, r1p->CacheElts, r1p->mflop[2]);
      }
      if (lvl & 8) /* timed L1-blocked OOC? */
      {
         printf("\nL1 BLOCKING RESULTS:\n");
         printf("--------------------\n");
         for (r1p=r1L1b; r1p; r1p = r1p->next)
            printf("   %d:%s, M=%d, N=%d, CacheElts=%d, gets =%.2f MFLOPS\n",
                   r1p->ID, r1p->rout, (int)(Mooc/r1p->MU)*r1p->MU,
                   (int)(Nooc/r1p->NU)*r1p->NU, r1p->CacheElts, r1p->mflop[0]);
      }
      if (lvl & 4) /* timed L2-blocked OOC? */
      {
         printf("\nL2 BLOCKING RESULTS:\n");
         printf("--------------------\n");
         for (r1p=r1L2b; r1p; r1p = r1p->next)
            printf("   %d:%s, M=%d, N=%d, CacheElts=%d, gets =%.2f MFLOPS\n",
                   r1p->ID, r1p->rout, (int)(Mooc/r1p->MU)*r1p->MU,
                   (int)(Nooc/r1p->NU)*r1p->NU, r1p->CacheElts, r1p->mflop[1]);
      }
   }
   else  /* Just do normal 75/90% searches */
   {
      if (lvl & 8) /* time L1-blocked OOC? */
      {
/*
 *       Find kernels L1-blocked & L2-blocked performance using 85% of cache
 */
         printf("\nTIMING OUT-OF-CACHE CASES WITH L1 BLOCKING, M=%d, N=%d\n",
                (int)Mooc, (int)Nooc);
         for (r1p=r1b; r1p; r1p = r1p->next)
         {
            r1p->CacheElts = 0.92 * L1CacheElts;
            r1p->mflop[0] = TimeMyKernel(VERB, 0, r1p, pre,
                                         (Mooc/r1p->MU)*r1p->MU,
                                         (Nooc/r1p->NU)*r1p->NU, Mooc, 85,
                                         NREP, 0, 0);
         }
      }
      if (lvl & 4) /* time L2-blocked OOC? */
      {
         printf("\nTIMING OUT-OF-CACHE CASES WITH L2 BLOCKING, M=%d, N=%d\n",
                (int)Mooc, (int)Nooc);
         for (r1p=r1b; r1p; r1p = r1p->next)
         {
            r1p->CacheElts = 0.75 * L2CacheElts;
            i = (100.0*r1p->CacheElts)/(1.0*L1CacheElts);
            r1p->mflop[1] = TimeMyKernel(VERB, 0, r1p, pre,
                                         (Mooc/r1p->MU)*r1p->MU,
                                         (Nooc/r1p->NU)*r1p->NU, Mooc, i,
                                         NREP, 1, 0);
         }
      }
      if (lvl&12)  /* did both L1 & L2, choose best one */
      {
         for (r1p=r1b; r1p; r1p = r1p->next)
            r1p->CacheElts = (r1p->mflop[0] >= r1p->mflop[1]) ?
                             0.92*L1CacheElts : 0.75*L2CacheElts;
      }
   }  /* end if on exhaustive or estimate blocking search */

   if (lvl & 32) /* time in-L2 problems? */
   {
      printf("\nTIMING IN-L2 PROBLEMS, M=%d, N=%d\n", Ml2, Nl2);
      for (r1p=r1b; r1p; r1p = r1p->next)
         r1p->mflop[3] = TimeMyKernel(VERB, 0, r1p, pre, (Ml2/r1p->MU)*r1p->MU,
                                      (Nl2/r1p->NU)*r1p->NU, Ml2, 0,
                                      NREP, 3, 0);
   }
   if (lvl & 64) /* time in-L1 performance? */
   {
      printf("\nTIMING IN-L1 PROBLEMS, M=%d, N=%d\n", Ml1, Nl1);
      for (r1p=r1b; r1p; r1p = r1p->next)
         r1p->mflop[4] = TimeMyKernel(VERB, 0, r1p, pre, (Ml1/r1p->MU)*r1p->MU,
                                      (Nl1/r1p->NU)*r1p->NU, Ml1, 0,
                                      NREP, 4, 0);
   }
/*
 * Find best kernel for out-of-cache, and the blocking it used
 */
   r1OOC = NULL;
   mfB = 0.0;

   if (lvl & 2)  /* did we time unblocked OOC? */
   {
      r1OOC = FindFastestMVKernel(pre, r1b, 2, 1);
      r1OOC->CacheElts = 0;
      blkstr = "no blocking";
      mfB = r1OOC->mflop[2];
   }
   if (lvl&16)  /* did exhaustive cache blocking search */
   {
      if (lvl & 4) /* did we time L2-blocked OOC? */
      {
         r1p = FindFastestMVKernel(pre, r1L2b, 1, 1);
         if (!r1OOC || r1p->mflop[1] > mfB)
         {
            r1OOC = CloneMVNode(r1p);
            r1OOC->next = NULL;
            blkstr = "L2 blocking";
            mfB = r1p->mflop[1];
         }
         KillAllMVNodes(r1L2b);
      }

      if (lvl & 8) /* did we time L1-blocked OOC? */
      {
         r1p = FindFastestMVKernel(pre, r1L1b, 0, 1);
         if (!r1OOC || r1p->mflop[0] > mfB)
         {
            r1OOC = CloneMVNode(r1p);
            r1OOC->next = NULL;
            blkstr = "L1 blocking";
            mfB = r1p->mflop[0];
         }
         KillAllMVNodes(r1L1b);
      }
   }
   else  /* did simple % estimate cacheedge search */
   {
      if (lvl & 4) /* did L2-blocked OOC */
      {
         r1p = FindFastestMVKernel(pre, r1b, 1, 1);
         if (!r1OOC || r1p->mflop[1] > mfB)
         {
            mfB = r1p->mflop[1];
            r1OOC = r1p;
            blkstr = "L2 blocking";
         }
      }
      if (lvl & 8) /* did L1-blocked OOC */
      {
         r1p = FindFastestMVKernel(pre, r1b, 0, 1);
         if (!r1OOC || r1p->mflop[0] > mfB)
         {
            mfB = r1p->mflop[0];
            r1OOC = r1p;
            blkstr = "L1 blocking";
         }
      }
   }    /* end if on type of cacheedge search */
/*
 * Find best in-L2 and in-L1 kernels
 */
   r1L2 = FindFastestMVKernel(pre, r1b, 3, 1);
   r1L1 = FindFastestMVKernel(pre, r1b, 4, 1);

   fpout = fopen(fout, "w");
   assert(fpout);
   WriteMflopExp(fpout);
   fprintf(fpout,
           "#\n# Mooc=%d, Nooc=%d, Ml2=%d, Nl2=%d, Ml1=%d Nl1=%d\n#\n#\n",
           (int)Mooc, (int)Nooc, Ml2, Nl2, Ml1, Nl1);
   fprintf(fpout, "# ==========================\n");
   fprintf(fpout, "# Performance of all kernels\n");
   fprintf(fpout, "# ==========================\n");
   PrintMVNodes(fpout, r1b);
   fprintf(fpout, "################################### -- CUT HERE -- ############################\n");
   fprintf(fpout, "###############################################################################\n#\n");

   if (lvl & 64)
   {
      fprintf(fpout, "# ----------------------------------------\n");
      fprintf(fpout, "# Following kernel is best in-L1 performer\n");
      fprintf(fpout, "# ----------------------------------------\n");
      PrintMVLine(fpout, r1L1);
   }
   if (lvl & 32)
   {
      fprintf(fpout, "# ----------------------------------------\n");
      fprintf(fpout, "# Following kernel is best in-L2 performer\n");
      fprintf(fpout, "# ----------------------------------------\n");
      PrintMVLine(fpout, r1L2);
   }
   if (lvl & 2)
   {
      fprintf(fpout, "# ---------------------------------------------------\n");
      fprintf(fpout, "# Following kernel is best out-of-cache using %s\n",
              blkstr);
      fprintf(fpout, "# ---------------------------------------------------\n");
      PrintMVLine(fpout, r1OOC);
   }
   WriteMflopExp(fpout);
   fclose(fpout);
}

int main(int nargs, char **args)
{
   ATL_mvnode_t *ocb, *i2b, *i1b, *syb, *r1cA, *r1cB;
   ATL_mvnode_t *r1b, *r1p, *r1r, *r1bestL2b, *r1gen, *r1gA, *r1gB;
   ATL_mvnode_t *r1bestOC, *r1bestOCr, *r1bestL1b, *r1bestL1br;
   ATL_mvnode_t *r1bestL1, *r1bestL1r, *r1bestL2, *r1bestL2r;
   FILE *fpin, *fpout;
   double mf, percL1;
   int i, j, L1CacheElts, CE1, CE2, maxID, ETIME, imf, M, N, lda, genID;
   char *fin, *fout;
   char ln[128];
   char pre;

   r1r = GetFlags(nargs, args, &pre, &ETIME, &fin, &fout);
   if (ETIME)
   {
      ExhaustiveTime(pre, fin, fout, ETIME);
      exit(0);
   }
   L1CacheElts = (GetL1CacheSize()*1024) / pre2size(pre);
   r1b = ReadMVFile(fout);
   SetAllMVTypeFlags(pre, r1b);
   ATL_SubGoodGccInMVNodes(r1b);
   maxID = GetMaxID(r1b);
   if (r1b)
   {
      if (r1r)  /* don't care about external searches if we've got answer */
         KillAllMVNodes(r1r);
/*
 *    Create generator strings & say to look in EXTDIR for files that need it
 */
      FillInMVExtractGenStrings(pre, r1b);
      for (r1p=r1b; r1p; r1p = r1p->next)
      {
         if (r1p->ID >= 900000 && r1p->ID < 1000000)
         {
            if (r1p->exflags)
            {
               i = strlen(r1p->exflags);
               r1p->exflags = GetLongerString(r1p->exflags, i+16);
               strcpy(r1p->exflags+i, "MVTKdir=EXTDIR");
            }
            else
               r1p->exflags = DupString("MVTKdir=EXTDIR");
         }
      }
      if (r1b->mflop[3] <= 0.0)
      {
/*
 *       Split queue by context/routine
 */
         ATL_MVSplitContexts(r1b, &ocb, &i2b, &i1b, &syb);
/*
 *       Retime out-of-cache kernel
 */
         TimeAllKernels(0, L1CacheElts, 0, pre, 0, 0, -1, ocb);
/*
 *       Retime in-L2 timings
 */
         TimeAllKernelsForContext(L1CacheElts, 3, pre, NREP, i2b);
/*
 *       Retime in-L1 timings
 */
         TimeAllKernelsForContext(L1CacheElts, 4, pre, NREP, i1b);
/*
 *       Retime L1-blocked out-of-cache timings
 */
         TimeAllKernels(0, L1CacheElts, 0, pre, 0, 0, -1, syb);
         r1b = ATL_MVLinkContexts(ocb, i2b, i1b, syb);
         ResubGoodGccInMVNodes(r1b);
         WriteMVSummFile(fout, r1b);
      }
      PrintMVNodes(stdout, r1b);
      KillAllMVNodes(r1b);
      exit(0);
   }
   r1b = ReadMVFile(fin);
   if (r1r)  /* add external outputs to our queue */
   {
      if (!r1b)
         r1b = r1r;
      else
      {
         for (r1p=r1b; r1b->next; r1p = r1p->next);
         r1p->next = r1r;
      }
   }
   if (!r1b)
   {
      fprintf(stderr, "\n%s: NO INPUT KERNELS, CANNOT RUN!\n\n", args[0]);
      exit(-1);
   }
   fprintf(stdout, "\nCases read in:\n");
   WriteMVFile("stdout", r1b);
   r1b = DelRepeatedMVKernels(r1b);
   r1b = DelBadArchMVKernels(r1b);
   r1b = DelBadTestKernels(pre, r1b);
   fprintf(stdout, "\nSurviving cases:\n");
   WriteMVFile("stdout", r1b);

   r1b = SortRestricted(r1b, &r1r);     /* sort into general & rest kernels */
/*
 * Find best out-of-cache kernel using the ANSI C generator, and add it to
 * the unrestricted kernels.
 */
   genID = 0;
   r1gen = TuneExtractMUNU(L1CacheElts, 1, pre, 0, -1, 0);
   if (r1gen)
   {
      r1gen->next = r1b;
      r1gen->ID++;
      genID = r1gen->ID + 1;
      r1cA = r1b = r1gen;
   }
/*
 * Find best out-of-cache generated kernel using x8664 SSE3 assembly, and
 * add it to the unrestricted kernels; add an aligned-A version to the
 * restricted kernels
 */
   r1gen = TuneExtractMUNU(L1CacheElts, 1, pre, 0, -1, 2);
   if (r1gen)
   {
      r1gen->next = r1b;
      if (genID)
         r1gen->ID = genID;
      else
         r1gen->ID = genID = r1gen->ID + 1;
      genID++;
      r1gA = r1b = r1gen;
      r1gen = CloneMVNode(r1gen);
      r1gen->alignA = r1gen->ldamul = 16;
      r1gen->ID = genID++;
      FillInMVExtractGenStrings(pre, r1gen);
      r1gen->next = r1r;
      r1r = r1gen;
   }
   else r1gA = NULL;
/*
 * Find best general kernel for L1-blocked out-of-cache behavior; use
 * cache elements of 85% of L1 size
 */
   for (r1p=r1b; r1p; r1p = r1p->next)
      r1p->CacheElts = 0.85 * L1CacheElts;
   fprintf(stdout, "\nBEGIN L1-BLOCKED TUNING\n");
   TimeAllKernels(0, L1CacheElts, 0, pre, 0, 0, -1, r1b);
   r1bestL1b = TuneBestPF(0, L1CacheElts, 0, pre, 0, -1, r1b);
   fprintf(stdout, "DONE L1-BLOCKED TUNING, CHOSE '%s' (%.2f)\n\n",
           r1bestL1b->rout, r1bestL1b->mflop[0]);
/*
 * Find best L1 blocking % of cache, convert back to Elts
 */
   GetDimsByContext(L1CacheElts, 0, pre, r1bestL1b, &M, &N, &lda, &i, &j);
   ExhaustiveCESrch(r1bestL1b, 0, pre, M, N, lda, 2, 25, 50);
   r1bestL1b->CacheElts = L1CacheElts*0.01*r1bestL1b->CacheElts;
/*
 * Find best kernel for L2-blocked out-of-cache context; use min of 4*L1
 * and 128K as good effective L2 estimate, convert back to elts
 */
   CE1 = 4 * L1CacheElts;
   CE1 = Mmin(CE1, 128*1024/pre2size(pre));
   for (r1p=r1b; r1p; r1p = r1p->next)
      r1p->CacheElts = CE1;
   fprintf(stdout, "\nBEGIN L2-BLOCKED TUNING\n");
   TimeAllKernels(0, L1CacheElts, 1, pre, 0, 0, -1, r1b);
   r1bestL2b = TuneBestPF(0, L1CacheElts, 1, pre, 0, -1, r1b);
   fprintf(stdout, "DONE L2-BLOCKED TUNING, CHOSE '%s' (%.2f)\n\n",
           r1bestL2b->rout, r1bestL2b->mflop[1]);

   GetDimsByContext(L1CacheElts, 1, pre, r1bestL1b, &M, &N, &lda, &i, &j);
   ExhaustiveCESrch(r1bestL2b, 1, pre, M, N, lda, 50, 3, 16);
   r1bestL2b->CacheElts = L1CacheElts*0.01*r1bestL2b->CacheElts;

   printf("BEST L1-blocked kernel:\n");
   WriteMVFile("stdout", r1bestL1b);
   printf("BEST L2-blocked kernel:\n");
   WriteMVFile("stdout", r1bestL2b);
/*
 * Find best kernel for in-L2 and in-L1 usage; add best MU/NU generated cases
 */
   r1gen = TuneExtractMUNU(L1CacheElts, 4, pre, 0, -1, 0);
   if (r1gen && (r1gen->MU != r1cA->MU || r1gen->NU != r1cA->NU))
   {
      r1gen->ID = genID++;
      r1gen->next = r1b;
      r1cB = r1b = r1gen;
   }
   else
   {
      KillMVNode(r1gen);
      r1cB = r1cA;
   }
   r1gen = TuneExtractMUNU(L1CacheElts, 4, pre, 0, -1, 2);
   if (r1gen && (r1gen->MU != r1gA->MU || r1gen->NU != r1gA->NU))
   {
      r1gen->ID = genID++;
      r1gen->next = r1b;
      r1gB = r1b = r1gen;
      r1gen = CloneMVNode(r1gen);
      r1gen->alignA = r1gen->ldamul = 16;
      r1gen->ID = genID++;
      FillInMVExtractGenStrings(pre, r1gen);
      r1gen->next = r1r;
      r1r = r1gen;
   }
   else
   {
      KillMVNode(r1gen);
      r1gB = r1gA;
   }
   r1gen = TuneExtractMUNU(L1CacheElts, 3, pre, 0, -1, 0);
   if (r1gen && (r1gen->MU != r1cA->MU || r1gen->NU != r1cA->NU) &&
       (r1gen->MU != r1cB->MU || r1gen->NU != r1cB->NU))
   {
      r1gen->ID = genID++;
      r1gen->next = r1b;
      r1b = r1gen;
   }
   else if (r1gen)
      KillMVNode(r1gen);
   r1gen = TuneExtractMUNU(L1CacheElts, 3, pre, 0, -1, 2);
   if ( r1gen && (r1gen->MU != r1gA->MU || r1gen->NU != r1gA->NU) &&
        (r1gen->MU != r1gB->MU || r1gen->NU != r1gB->NU) )
   {
      r1gen->ID = genID++;
      r1gen->next = r1b;
      r1b = r1gen;
      r1gen = CloneMVNode(r1gen);
      r1gen->alignA = r1gen->ldamul = 16;
      r1gen->ID = genID++;
      FillInMVExtractGenStrings(pre, r1gen);
      r1gen->next = r1r;
      r1r = r1gen;
   }
   else
      KillMVNode(r1gen);
   TimeAllKernelsForContext(L1CacheElts, 4, pre, NREP, r1b);
   r1bestL1 = TuneBestPF(0, L1CacheElts, 4, pre, 0, 0, r1b);
   r1bestL2 = TimeAllKernelsForContext(L1CacheElts, 3, pre, NREP, r1b);
   r1bestL2 = TuneBestPF(0, L1CacheElts, 3, pre, 0, 0, r1b);
   r1bestOC = TimeAllKernelsForContext(L1CacheElts, 2, pre, NREP, r1b);
   r1bestOC = TuneBestPF(0, L1CacheElts, 2, pre, 0, -1, r1b);
   r1bestL2->CacheElts = r1bestL2b->CacheElts;
   r1bestOC->CacheElts = 0;
   r1bestL1->next = r1bestL2->next = r1bestOC->next = NULL;
   r1bestL1->CacheElts = r1bestL1b->CacheElts;
/*
 * Figure out what type of blocking to use for out-of-cache context
 */
   r1p = ChooseKernelBlocking(r1bestL1b, r1bestL2b, r1bestOC);
   if (r1p != r1bestOC)
   {
      KillMVNode(r1bestOC);
      r1bestOC = CloneMVNode(r1p);
   }
   printf("\nBest out-of-cache kernel:\n");
   WriteMVFile("stdout", r1bestOC);
/*
 * When timing restricted kernels, we will use best CE found by unresticted,
 * and we can tune only the contexts we use: r1bestOC, r1bestL1[b], r1bestL2.
 */
   if (r1r)
   {
/*
 *    Find best restricted out-of-cache, L1blocked, in-L1 & in-L2 kernels
 */
      fprintf(stdout, "\nBEGIN RESTRICTED OUT-OF-CACHE TUNING\n");
      for (r1p=r1r; r1p; r1p = r1p->next)
         r1p->CacheElts = r1bestOC->CacheElts;
      if (r1bestOC->CacheElts > L1CacheElts)
         i = 1;
      else if (r1bestOC->CacheElts)
         i = 0;
      else
         i = 2;
      TimeAllKernels(0, L1CacheElts, i, pre, 0, 0, -1, r1r);
      r1p = CloneMVQueue(r1r);  /* get private copy for destruction */
      r1p = TuneBestPF(1, L1CacheElts, i, pre, 0, -1, r1p);
      r1bestOCr = DecimateAndRankRestricted(i, r1bestOC, r1p);
      if (r1bestOCr)
      {
         fprintf(stdout, "KERNEL CHOSEN:\n");
         PrintMVNodes(stdout, r1bestOCr);
         fprintf(stdout, "DONE RESTRICTED OUT-OF-CACHE TUNING.\n");
      }
      else
         fprintf(stdout, "NO RESTRICTED OUT-OF-CACHE KERNEL USED.\n");

      fprintf(stdout, "\nBEGIN RESTRICTED L1-BLOCKED TUNING\n");
      for (r1p=r1r; r1p; r1p = r1p->next)
         r1p->CacheElts = r1bestL1b->CacheElts;
      TimeAllKernels(0, L1CacheElts, 0, pre, 0, 0, -1, r1r);
      r1p = CloneMVQueue(r1r);  /* get private copy for destruction */
      r1p = TuneBestPF(1, L1CacheElts, 0, pre, 0, -1, r1p);
      r1bestL1br = DecimateAndRankRestricted(0, r1bestL1b, r1p);
      if (r1bestL1br)
      {
         fprintf(stdout, "KERNEL CHOSEN:\n");
         PrintMVNodes(stdout, r1bestL1br);
         fprintf(stdout,"DONE RESTRICTED L1-BLOCKED TUNING.\n");
      }
      else
         fprintf(stdout, "NO RESTRICTED L1-BLOCKED KERNEL USED.\n");

      fprintf(stdout, "\nBEGIN RESTRICTED in-L1 TUNING\n");
      TimeAllKernelsForContext(L1CacheElts, 4, pre, NREP, r1r);
      r1bestL1r = CloneMVQueue(r1r);  /* get private copy for destruction */
      r1bestL1r = TuneBestPF(1, L1CacheElts, 4, pre, 0, 0, r1bestL1r);
      r1bestL1r = DecimateAndRankRestricted(4, r1bestL1, r1bestL1r);
      if (r1bestL1r)
      {
         fprintf(stdout, "KERNEL CHOSEN:\n");
         PrintMVNodes(stdout, r1bestL1r);
         fprintf(stdout, "DONE RESTRICTED in-L1 TUNING.\n");
      }
      else
         fprintf(stdout, "NO RESTRICTED L1-BLOCKED KERNEL USED.\n");

      fprintf(stdout, "\nBEGIN RESTRICTED in-L2 TUNING\n");
      for (r1p=r1r; r1p; r1p = r1p->next)
         r1p->CacheElts = r1bestL2b->CacheElts;
      TimeAllKernelsForContext(L1CacheElts, 3, pre, NREP, r1r);
      r1bestL2r = CloneMVQueue(r1r);  /* get private copy for destruction */
      r1bestL2r = TuneBestPF(1, L1CacheElts, 3, pre, 0, 0, r1bestL2r);
      r1bestL2r = DecimateAndRankRestricted(3, r1bestL2, r1bestL2r);
      if (r1bestL2r)
      {
         fprintf(stdout, "KERNEL CHOSEN:\n");
         PrintMVNodes(stdout, r1bestL2r);
         fprintf(stdout, "DONE RESTRICTED in-L2 TUNING.\n");
      }
      else
         fprintf(stdout, "NO RESTRICTED in-L2 KERNEL USED.\n");
   }
   else
      r1bestOCr = r1bestL1r = r1bestL2r = r1bestL1br = NULL;
/*
 * Indicate unrestricted kernel by setting rankR=0, and add it to end
 * of restricted kernels to make our final fastest-to-slowest call chain
 */
   KillAllMVNodes(r1b);
   KillAllMVNodes(r1r);
   r1bestOC->rankR = r1bestL1b->rankR = r1bestL1->rankR = r1bestL2->rankR = 0;

   if (r1bestOCr)
   {
      r1p = ATL_LastMVNode(r1bestOCr);
      r1p->next = r1bestOC;
   }
   else
      r1bestOCr = r1bestOC;

   if (r1bestL1br)
   {
      r1p = ATL_LastMVNode(r1bestL1br);
      r1p->next = r1bestL1b;
   }
   else
      r1bestL1br = r1bestL1b;

   if (r1bestL1r)
   {
      r1p = ATL_LastMVNode(r1bestL1r);
      r1p->next = r1bestL1;
   }
   else
      r1bestL1r = r1bestL1;

   if (r1bestL2r)
   {
      r1p = ATL_LastMVNode(r1bestL2r);
      r1p->next = r1bestL2;
   }
   else
      r1bestL2r = r1bestL2;
/*
 * Join all together in combined context queue, and write out answer
 */
   r1b = ATL_MVLinkContexts(r1bestOCr, r1bestL2r, r1bestL1r, r1bestL1b);
   NameAllKernels(pre, r1b);
   ResubGoodGccInMVNodes(r1b);
   WriteMVSummFile(fout, r1b);
   KillAllMVNodes(r1b);
   return(0);
}

