#ifndef ATLAS_MMTESTTIME_H
   #define ATLAS_MMTESTTIME_H

#include "atlas_mmparse.h"
#include "atlas_gentesttime.h"

/* procedure 1 */
int MMKernelFailsTest
(
   char pre,                    /* precision/type prefix */
   int mb, int nb, int kb,      /* dimensions to test */
   int beta,                    /* beta case to test */
   ATL_mmnode_t *umm            /* mmkern ptr */
)
/*
 * RETURNS: 0 on success, non-zero on failure
 */
{
   char ln[4096];
   int i, lda, ldb, ldc;
   char ch;

/*
 * If the file is generated, call generator to create it
 */
   if (umm->genstr)
   {
      if (system(umm->genstr))
      {
         fprintf(stderr, "ERROR, LINE %d of %s\n", __LINE__, __FILE__);
         fprintf(stderr, "UNABLE TO GENERATE WITH COMMAND: %s\n", umm->genstr);
         exit(-1);
      }
   }
   ldc = mb*2+4;
   if (FLAG_IS_SET(umm->flag, MMF_LDISKB))
      lda = ldb = kb;
   else if (FLAG_IS_SET(umm->flag, MMF_LDAB))
      lda = ldb = kb+8;
   else
   {
      lda = kb+8;
      ldb = kb+16;
   }
   if (pre == 'c' || pre == 'z')
      i = sprintf(ln, "make cmmutstcase mmrout=%s csC=2 ", umm->rout);
   else
      i = sprintf(ln, "make mmutstcase mmrout=%s ", umm->rout);
   if (umm->comp)
   {
      ch = (pre == 'c' || pre == 's') ? 'S' : 'D';
      i += sprintf(ln+i, "%cMC=\"%s\" %cMCFLAGS=\"%s\" ",
                   ch, umm->comp, ch, umm->cflags);
   }
   i += sprintf(ln+i, "pre=%c M=%d N=%d K=%d mb=%d nb=%d kb=%d ",
                pre, mb, nb, kb,
                FLAG_IS_SET(umm->flag, MMF_MRUNTIME) ? 0 : mb,
                FLAG_IS_SET(umm->flag, MMF_NRUNTIME) ? 0 : nb,
                FLAG_IS_SET(umm->flag, MMF_KRUNTIME) ? 0 : kb);
   i += sprintf(ln+i, "lda=%d ldb=%d ldc=%d", lda, ldb, ldc);
   i += sprintf(ln+i, " > /dev/null 2>&1\n");
   i = system(ln);
   if (i)
   {
      fprintf(stderr, "%d of %s: FAILED COMMAND : %s\n",__LINE__,__FILE__,ln);
      if (umm->genstr)
         fprintf(stderr, "   genstr was = '%s'\n", umm->genstr);
   }
   return(i);
}

/* procedure 2 */
static ATL_mmnode_t *DelBadMMKernels(char pre, int verb, ATL_mmnode_t *bp)
/*
 * Deletes all kernels that can't pass basic usage test
 * RETURNS: modifed bp queue wt failing kernels removed
 */
{
   ATL_mmnode_t *p, *prev;
   int die;

   if (verb > 0)
       printf("\nBEGIN BASIC MATMUL KERNEL TESTS:\n");

   prev = p = bp;
   while (p)
   {
      if (MMKernelFailsTest(pre, p->mbB, p->nbB, p->kbB, 0, p) ||
          MMKernelFailsTest(pre, p->mbB, p->nbB, p->kbB, 1, p) ||
          MMKernelFailsTest(pre, p->mbB, p->nbB, p->kbB, 2, p))
      {
         if (verb > 0)
            printf("   NUKING bad kernel %s(%d)\n", p->rout, p->ID);
         if (p == bp)
            bp = p = KillMMNode(p);
         else
            prev->next = p = KillMMNode(p);
      }
      else
      {
         if (verb > 0)
            printf("   Kernel %s(%d) passes basic tests\n", p->rout, p->ID);
         prev = p;
         p = p->next;
      }
   }
   printf("DONE BASIC KERNEL TESTS.\n\n");
   return(bp);
}

/* procedure 3 */
char *GetGmmGenString
(
   int verb,                    /* verbosity */
   char pre,                    /* precision */
   int MACC,                    /* 0 : separate mult&add, else MACC */
   int lat,                     /* multiply latency */
   int beta,                    /* 0,1 beta, else beta=X */
   int nb,                      /* blocking factor */
   int mu, int nu, int ku,      /* unrolling factors */
   int Fftch,                   /* do bogus fetch of C at top of loop? */
   int iftch,                   /* # of initial fetches to do */
   int nftch,                   /* # of fetches to do thereafter */
   int LDTOP,                   /* 1: load C at top, 0: at bottom */
   int pf                       /* prefetch strategy */
)
/*
 * returns a string that will result in generating a user-style kernel
 * specialized for non-cleanup cases by invoking a make target that
 * in turn invokes the scalar generator routine, emit_mm.c
 * Because it is specialized for kernel cases, we don't specify leading
 * dimensions, transpose cases, etc, but just take the defaults.
 */
{
   char ln[4096];
   int i;

   if (!LDTOP)
      pf |= 512;
   i = sprintf(ln, "make mmgencase pre=%c muladd=%d lat=%d beta=%d mb=%d nb=%d kb=%d mu=%d nu=%d ku=%d if=%d nf=%d ff=%d, pfA=%d, csC=%d",
               pre, MACC, lat, beta, nb, nb, nb, mu, nu, ku, iftch, nftch,
               Fftch, pf, (pre == 'c' || pre == 'z') ? 2 : 1);
   if (verb < 3)
      i += sprintf(ln+i, " > /dev/null 2>&1");
   else
      i += sprintf(ln+i, "\n");
   if (verb > 2)
      printf("genstr='%s'\n", ln);
   return(DupString(ln));
}

/* procedure 4 */
void FillInGMMNode(int verb, ATL_mmnode_t *mmp, char pre, int MACC, int lat,
                   int beta, int nb, int mu, int nu, int ku,
                   int fftch, int iftch, int nftch, int LDTOP, int pf)
/*
 * Take emit_mm's flags and fill in the standard ATL_mmnode_t struct mmp
 * making all the correct assumptions for the standard copy code
 */
{
   int i;

   if (ku > (nb>>1))
      ku = nb;
   mmp->mbB = mmp->nbB = mmp->kbB = nb;
   mmp->mu = mu;
   mmp->nu = nu;
   mmp->ku = ku;
   mmp->muladd = MACC;
   mmp->lat = lat;
   mmp->fftch = fftch;
   mmp->iftch = iftch;
   mmp->nftch = nftch;
   mmp->pref = pf;
   mmp->SSE = 0;
   mmp->TA = AtlasTrans;
   mmp->TB = AtlasNoTrans;
   mmp->asmbits = 0;
   if (mmp->auth)
      free(mmp->auth);
   if (mmp->rout)
      free(mmp->rout);
   if (mmp->comp)
      free(mmp->comp);
   if (mmp->cflags)
      free(mmp->cflags);
   if (mmp->str)
      free(mmp->str);
   if (mmp->genstr)
      free(mmp->genstr);
   mmp->comp = mmp->cflags = mmp->str = NULL;
   mmp->rout = DupString("dgmm.c");
   mmp->rout[0] = pre;
   mmp->genstr = GetGmmGenString(verb, pre, MACC, lat, beta, nb, mu, nu, ku,
                                 fftch, iftch, nftch, LDTOP, pf);
   mmp->auth = DupString("Whaley/emit_mm");
   i = 0;
   SET_FLAG(i, MMF_KUISKB, (ku == nb));
   SET_FLAG(i, MMF_LDISKB, 1);
   SET_FLAG(i, MMF_LDAB, 1);
   if (pre == 's' || pre == 'c')
      SET_FLAG(i, MMF_SINGLE, 1);
   if (pre == 'c' || pre == 'z')
      SET_FLAG(i, MMF_COMPLEX, 1);
   mmp->flag = i;
}

/* procedure 5 */
double TimeGMMKernel            /* times kernels generated by emit_mm */
(
   int verb,                    /* verbosity */
   int FORCETIME,               /* 1: ignore any prior output file */
   char pre,                    /* precision */
   int MACC,                    /* 0 : separate mult&add, else MACC */
   int lat,                     /* multiply latency */
   int beta,                    /* 0,1 beta, else beta=X */
   int nb,                      /* blocking factor */
   int mu, int nu, int ku,      /* unrolling factors */
   int Fftch,                   /* do bogus fetch of C at top of loop? */
   int iftch,                   /* # of initial fetches to do */
   int nftch,                   /* # of fetches to do thereafter */
   int LDTOP,                   /* 1: load C at top, 0: at bottom */
   int pf,                      /* prefetch strategy */
   int mflop,                   /* >0: force mflop MFLOPs in each time interv */
   int cflush                   /* >0: size of cache flush, else ignored */
)
{
   int i;
   char *bet;
   FILE *fp;
   double *dp;
   char fnam[256];
   char ln[4096];

   if (beta == 1)
      bet = "_b1";
   else if (beta == 0)
      bet = "_b0";
   else if (beta == -1)
      bet = "_bn1";
   else
      bet = "_bX";
   if (!LDTOP)
      pf |= 512;
   if (ku > nb/2)
      ku = nb;
   else if (ku == -1)
      ku = nb;
   sprintf(fnam,
           "res/%c%smm%c%c%d_%dx%dx%d_%dx%dx%d_%dx%dx%d%s%s_%dx%d_%d_pf%d_cf%d",
            pre, "JIK", 'T', 'N', nb, nb, nb, nb, nb, nb, 0, mu, nu, ku,
            "_a1", bet, MACC, lat, 1, pf, cflush);
   dp = (FORCETIME) ? NULL : ReadResultsFile(0, 0, fnam);
   if (!dp)
   {
      i = sprintf(ln, "make mmcase pre=%c loopO=JIK ta=T tb=N mb=%d nb=%d kb=%d lda=%d ldb=%d ldc=%d mu=%d nu=%d ku=%d alpha=1 beta=%d muladd=%d lat=%d cleanup=1 pfA=%d casnam=\"%s\"",
                  pre, nb, nb, nb, nb, nb, 0, mu, nu, ku, beta, MACC, lat, pf,
                  fnam);
      if (pre == 'c' || pre == 'z')
         i += sprintf(ln+i, " csA=1 csB=1 csC=2");

      if (verb < 3)
         i += sprintf(ln+i, " > /dev/null 2>&1\n");
      else
         i += sprintf(ln+i, "\n");
      if (verb > 1)
         fprintf(stdout, "SYSTEM: %s", ln);
      if (system(ln))
      {
         fprintf(stderr, "ERROR IN COMMAND: %s", ln);
         fprintf(stderr, "   PROPOSED FILENAME: %s\n", fnam);
         sprintf(ln, "rm -f %s\n", fnam);
         assert(!system(ln));
         exit(-1);
      }
      dp = ReadResultsFile(0, 0, fnam);
   }
   assert(dp);
   return(*dp);
}

/* procedure 6 */
double TimeMMKernel
(
   int verb,                    /* 0: no output, 1 min output, 2: full output */
   int FORCETIME,               /* 1: ignore any prior output file */
   ATL_mmnode_t *mmp,           /* ptr to mmkern struct */
   char pre,                    /* type/prec prefix: z,c,d,s */
   int mb, int nb, int kb,      /* dimensions to time */
   int lda,                     /* >0: lda to use, else kb */
   int ldb,                     /* >0: ldb to use, else kb */
   int ldc,                     /* >0: ldc to use, else mb+8 */
   int beta,                    /* beta to time */
   int mflop,                   /* >0: force mflop MFLOPs in each time interv */
   int cflush                   /* >0: size of cache flush, else ignored */
)
{
   char fnam[128], ln[2048];
   const char *LO = FLAG_IS_SET(mmp->flag, MMF_AOUTER) ? "IJK": "JIK";
   char *be;
   int i, j;
   char ch;
   double *dp;
/*
 * If it's a emit_mm generated file w/o the genstring, create the genstring
 * assuming it is a mmK
 */
   if (mmp->ID == 0 && !mmp->genstr && mmp->iftch > 0)
      mmp->genstr = GetGmmGenString(verb, pre, mmp->muladd, mmp->lat, beta, nb,
                                    mmp->mu, mmp->nu, mmp->ku,
                                    mmp->fftch, mmp->iftch, mmp->nftch,
                                    FLAG_IS_SET(mmp->flag, MMF_LDCTOP),
                                    mmp->pref);

   if (FLAG_IS_SET(mmp->flag, MMF_LDISKB))
      lda = ldb = kb;
   else
   {
      if (lda < 1)
         lda = kb;
      if (ldb < 1)
         ldb = kb;
      if (ldc < 1)
         ldc = mb + 8;
   }
   if (FLAG_IS_SET(mmp->flag, MMF_LDAB))
      ldb = lda;
/*
 * If the file is generated, call generator to create it
 */
   if (mmp->genstr)
   {
      i = sprintf(ln, "%s", mmp->genstr);
      if (verb < 3)
         i += sprintf(ln+i, " > /dev/null 2>&1\n");
      if (system(ln))
      {
         fprintf(stderr, "ERROR, LINE %d of %s\n", __LINE__, __FILE__);
         fprintf(stderr, "UNABLE TO GENERATE WITH COMMAND: %s\n", mmp->genstr);
         exit(-1);
      }
   }

   if (beta == 0)
      be = "b0";
   else if (beta == 1)
      be = "b1";
   else if (beta == -1)
      be = "bn1";
   else
      be = "bX";
/*   dmm%d_TNMBxNBxKB_muxnuxku_ldc_rtMxrtNxrtK_LDTOP_pf_a1_bX_flushKB */
   sprintf(fnam, "res/%cmm%s%d_%c%c%dx%dx%d_%dx%dx%d_%d_%dx%dx%d_%d_%d_a1_%s_%d",
           pre, FLAG_IS_SET(mmp->flag, MMF_AOUTER) ? "MNK" : "NMK",
           mmp->ID, 'T', 'N', mb, nb, kb, mmp->mu, mmp->nu, mmp->ku, ldc,
           FLAG_IS_SET(mmp->flag, MMF_MRUNTIME),
           FLAG_IS_SET(mmp->flag, MMF_NRUNTIME),
           FLAG_IS_SET(mmp->flag, MMF_KRUNTIME),
           FLAG_IS_SET(mmp->flag, MMF_LDCTOP), mmp->pref, be, cflush);

   if (FORCETIME || !FileExists(fnam))
   {
      if (pre == 'c' || pre == 'z')
         i = sprintf(ln, "make cmmucase mmrout=%s csC=2 ", mmp->rout);
      else i = sprintf(ln, "make mmucaseN mmrout=%s ", mmp->rout);
      if (mmp->cflags)
      {
         ch = (pre == 'c' || pre == 's') ? 'S' : 'D';
         i += sprintf(ln+i, "%cMCFLAGS=\"%s\" ", ch, mmp->cflags);
      }
      if (mmp->comp)
      {
         ch = (pre == 'c' || pre == 's') ? 'S' : 'D';
         i += sprintf(ln+i, "%cMC=\"%s\" ", ch, mmp->comp);
      }
      if (mmp->exflags)
         i += sprintf(ln+i, " %s ", mmp->exflags);

      if (!cflush)
         i += sprintf(ln+i, "moves=\"\" ");

      i += sprintf(ln+i, "casnam=%s ", fnam);
      i += sprintf(ln+i, "pre=%c M=%d N=%d K=%d mb=%d nb=%d kb=%d ",
                   pre, mb, nb, kb,
                   FLAG_IS_SET(mmp->flag, MMF_MRUNTIME) ? 0:mb,
                   FLAG_IS_SET(mmp->flag, MMF_NRUNTIME) ? 0:nb,
                   FLAG_IS_SET(mmp->flag, MMF_KRUNTIME) ? 0:kb);
      i += sprintf(ln+i, "mu=%d nu=%d ku=%d lda=%d ldb=%d ldc=%d beta=%d",
                   mmp->mu, mmp->nu, mmp->ku, lda, ldb, ldc, beta);
      if (verb < 3)
         i += sprintf(ln+i, " > /dev/null 2>&1\n");
      else
         i += sprintf(ln+i, "\n");
      if (verb > 1)
         fprintf(stdout, "SYSTEM: %s", ln);
      if (system(ln))
      {
         fprintf(stderr, "ERROR IN COMMAND: %s", ln);
         fprintf(stderr, "   PROPOSED FILENAME: %s\n", fnam);
         if (mmp->genstr)
            fprintf(stderr, "   GENSTR='%s'\n", mmp->genstr);
         sprintf(ln, "rm -f %s\n", fnam);
         assert(!system(ln));
         exit(-1);
      }
   }
   dp = ReadResultsFile(0, 0, fnam);
   if (!dp)
   {
      fprintf(stderr, "\nEmpty file '%s'!\n", fnam);
      fprintf(stderr, "From command: '%s'\n", ln);
      exit(-1);
   }
   return(*((double*)ReadResultsFile(0, 0, fnam)));
}

/* procedure 7 */
void TimeAllMMKernels
(
   int itime,                   /* index of mflop array to set */
   int verb,                    /* 0: no output, 1 min output, 2: full output */
   int FORCETIME,               /* 1: ignore any prior output file */
   ATL_mmnode_t *mmb,           /* ptr to mmkern struct queue */
   char pre,                    /* type/prec prefix: z,c,d,s */
   int lda,                     /* >0: lda to use, else kb */
   int ldb,                     /* >0: ldb to use, else kb */
   int ldc,                     /* >0: ldc to use, else mb+8 */
   int beta,                    /* beta to time */
   int mflop,                   /* >0: force mflop MFLOPs in each time interv */
   int cflush                   /* >0: size of cache flush, else ignored */
)
{
   ATL_mmnode_t *mmp;
   for (mmp=mmb; mmp; mmp = mmp->next)
      mmp->mflop[itime] = TimeMMKernel(verb, FORCETIME, mmp, pre,
                                       mmp->mbB, mmp->nbB, mmp->kbB,
                                       (lda >= 0) ? lda : mmp->kbB,
                                       (ldb >= 0) ? ldb : mmp->kbB,
                                       (ldc >= 0) ? ldc : mmp->mbB+8,
                                       beta, mflop, cflush);
}
#endif  /* end guard around atlas_mmtesttime.h */
