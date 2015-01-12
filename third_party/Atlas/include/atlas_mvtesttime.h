#ifndef ATLAS_MVTESTTIME_H
   #define ATLAS_MVTESTTIME_H

#include "atlas_mvparse.h"
#include "atlas_gentesttime.h"

static int SprintAlignStr
(
   char pre,            /* precision modifier */
   ATL_mvnode_t *kp, /* kernel pointer */
   char *str            /* (OUTPUT) string to print to */
)
/*
 * Prints alignment string to str, based on alignment setting.
 * This routine assumes to force the alignment to sizeof(TYPE) unless
 * restrictions are enabled.
 * RETURNS: number of chars added to str
 */
{
   const int size = (pre == 'd' || pre == 's') ? pre2size(pre)
                                                 : (pre2size(pre)>>1);
   int alignA, alignX, alignY, i;

   alignA = (kp->alignA) ? kp->alignA : size;
   alignX = (kp->alignX) ? kp->alignX : size;
   alignY = (kp->alignY) ? kp->alignY : size;
/*
 * If we are doing an AXPY-based No-Trans GEMV, it is Y, not X that must
 * be aligned to A.  Therefore, alignX/ALIGNX2A actually refer to Y, and
 * alignY refers to X.  Do this confusing transpose for the non-trans case.
 */
   if (kp->TA == AtlasNoTrans) /* ALIGNX* affects Y for No-Trans */
   {
      i = alignX;
      alignX = alignY;
      alignY = i;
      if (FLAG_IS_SET(kp->flag, MVF_ALIGNX2A))
         alignY = alignA; /* ALIGNX2A overrides alignY */
   }
   else if (FLAG_IS_SET(kp->flag, MVF_ALIGNX2A))
      alignX = alignA;
/*
 * If ALIGNX2A, we must force to vectors to have the same remainder when
 * divided by the vector length.  We do this by insisting they have the
 * the same modulo by ATL_Cachelen, which by definition is always a
 * multiple of the vector length (eg, veclen=16/32 (SSE/AVX), ATL_cl=32).
 */
   if (FLAG_IS_SET(kp->flag, MVF_ALIGNX2A))
   {
      int myalign = ATL_Cachelen - size;
      if (alignA)
      {
         myalign = ((ATL_Cachelen-size)/alignA)*alignA;
         if (!myalign)
            myalign = alignA;
      }
      if (kp->TA == AtlasNoTrans) /* ALIGNX* affects Y for No-Trans */
      {
         if (myalign < ATL_Cachelen)
            i = sprintf(str, " align=\"-Fa %d -Fa -%d -Fy %d -Fy -%d",
                        myalign, ATL_Cachelen, myalign, ATL_Cachelen);
         else
            i = sprintf(str, " align=\"-Fa %d -Fy %d", myalign, myalign);
         if (2*alignX <= ATL_Cachelen)
            i += sprintf(str+i, " -Fx %d -Fx -%d\"", alignX, 2*alignX);
         else
            i += sprintf(str+i, " -Fx %d\"", alignX);
      }
      else
      {
         if (myalign < ATL_Cachelen)
            i = sprintf(str, " align=\"-Fa %d -Fa -%d -Fx %d -Fx -%d",
                        myalign, ATL_Cachelen, myalign, ATL_Cachelen);
         else
            i = sprintf(str, " align=\"-Fa %d -Fx %d", myalign, myalign);

         if (2*alignY <= ATL_Cachelen)
            i += sprintf(str+i, " -Fy %d -Fy -%d\"", alignY, 2*alignY);
         else
            i += sprintf(str+i, " -Fy %d\"", alignY);
      }
   }
   else
   {
      if (2*alignA <= ATL_Cachelen)
         i = sprintf(str, " align=\"-Fa %d -Fa -%d", alignA, 2*alignA);
      else
         i = sprintf(str, " align=\"-Fa %d ", alignA);
      if (2*alignX <= ATL_Cachelen)
         i += sprintf(str+i, " -Fx %d -Fx -%d", alignX, 2*alignX);
      else
         i += sprintf(str+i, " -Fx %d", alignX);
      if (2*alignY <= ATL_Cachelen)
         i += sprintf(str+i, " -Fy %d -Fy -%d\"", alignY, 2*alignY);
      else
         i += sprintf(str+i, " -Fy %d\"", alignY);
   }
   return(i);
}

/* procedure 1 */
static int MVKernelFailsTest
   (int verb, char pre, ATL_INT M, ATL_INT N, ATL_INT lda, ATL_mvnode_t *kn)
{
   char ln[4096];
   char *sp;
   int i, lda0;
   static char outnam[L_tmpnam];
   static int FirstTime=1;

   if (FirstTime)
   {

      FirstTime = 0;
      assert(tmpnam(outnam));
   }
/*
 * If the file is generated, call generator to create it
 */
   if (kn->genstr)
   {
      i = sprintf(ln, "%s", kn->genstr);
      if (verb < 3)
         i += sprintf(ln+i, " > %s 2>&1\n", outnam);
      if (system(ln))
      {
         fprintf(stderr, "ERROR, LINE %d of %s\n", __LINE__, __FILE__);
         fprintf(stderr, "UNABLE TO GENERATE WITH COMMAND: %s\n", kn->genstr);
         if (verb < 3)
         {
            fprintf(stderr, "\nOUTPUT OF system():\n");
            sprintf(ln, "cat %s 1>&2\n", outnam);
            i = system(ln);
         }
         remove(outnam);
         exit(-1);
      }
   }
   assert(kn->rout);
   assert (M >= kn->minM);
   assert (N >= kn->minN);
   sp = (kn->TA == AtlasNoTrans) ? "mvn" : "mvt";
   if (kn->TA == AtlasNoTrans)
      i = sprintf(ln, "make %cmvnktest mvnrout=%s", pre, kn->rout);
   else
      i = sprintf(ln, "make %cmvtktest mvtrout=%s", pre, kn->rout);
   i += SprintAlignStr(pre, kn, ln+i);
   if (FLAG_IS_SET(kn->flag, MVF_FNU))
       i += sprintf(ln+i, " Nt=%d ", (1008/kn->NU)*kn->NU);
   if (kn->exflags)
      i += sprintf(ln+i, " %s", kn->exflags);
   if (1)   /* NOTE: replace with test on restrict or not! */
      i += sprintf(ln+i, " incy=1");
   if (kn->comp)
      i += sprintf(ln+i, " %cMVCC=\"%s\"", pre, kn->comp);
   if (kn->cflags)
      i += sprintf(ln+i, " %cMVFLAGS=\"%s\"", pre, kn->cflags);
   i += sprintf(ln+i, " Mt=%d Nt=%d ldat=%d", M, N, lda);
   if (verb < 3)
      i += sprintf(ln+i, " > %s 2>&1\n", outnam);
   else
      i += sprintf(ln+i, "\n");
   if (verb > 1)
      fprintf(stdout, "system call:%s\n", ln);
   i = system(ln);
   if (verb)
   {
      if (i)
      {
         fprintf(stderr, "\n%s(ID=%d) FAILS TESTER!!\n", kn->rout,kn->ID);
         fprintf(stderr, "FAILING CALL: '%s'\n", ln);
         if (verb < 3 && verb > 0)
         {
            int itmp;
            fprintf(stderr, "\nOUTPUT OF system():\n");
            sprintf(ln, "cat %s 1>&2\n", outnam);
            itmp = system(ln);
         }
      }
      else
         fprintf(stderr, "%s(ID=%d) *PASSES* TESTER!!\n", kn->rout,kn->ID);
   }
   if (verb < 3)
      remove(outnam);
   return(i);
}


/* procedure 2 */
static char *GetResIdStr(ATL_mvnode_t *r1p, ATL_INT M, ATL_INT N,
                         ATL_INT lda, ATL_INT percL1, int mflop)
{
/*
 * Return filename suffix that disambiguates most kernels:
 * <ID><TA>_<M>x<N>_<lda>-<ldamul>_<MU>x<NU>_<percL1>_a<alignA>x<aX>x<aY>_<flag>
 */
   static char ln[512];
   sprintf(ln, "%d%c_%dx%d_%d-%d_%dx%d_%d_a%dx%dx%d_%d", r1p->ID,
           (r1p->TA == AtlasNoTrans) ? 'N' : 'T', M, N, lda, r1p->ldamul,
            r1p->MU, r1p->NU, percL1, r1p->alignA, r1p->alignX, r1p->alignY,
            r1p->flag);
   return(ln);
}

/* procedure 3 */
static double TimeMVKernel
(int verb,              /* 0: no output, 1 min ouput, 2: full output */
 int FORCETIME,         /* if nonzero, ignore existing timing file */
                        /* if negative, don't retain timing file */
 ATL_mvnode_t *r1p,     /* ptr to kernel structure */
 char pre,              /* precision prefix */
 ATL_INT M, ATL_INT N,  /* dimensions to time */
 ATL_INT lda,           /* stride between row elements */
 ATL_INT percL1,        /* if 0, time kernel directly wt no blocking */
                        /* if non-zero, block for that % of L1 cache size */
 int nrep,              /* if >=1, # of trials, else use default (3) */
 int mflop,             /* force mflop flops in each timing interval */
 int cflush             /* if >= 0, size of cache flush area, else ignored */
)
{
   char ln[2048], resf[256], *sp;
   double *dp, mf;
   int i, align = pre2size(pre);
   static char outnam[L_tmpnam];
   static int FirstTime=1;

   if (FirstTime)
   {

      FirstTime = 0;
      assert(tmpnam(outnam));
   }
/*
 * If the file is generated, call generator to create it
 */
   if (r1p->genstr)
   {
      i = sprintf(ln, "%s", r1p->genstr);
      if (verb < 3)
         i += sprintf(ln+i, " > %s 2>&1\n", outnam);
      if (system(ln))
      {
         fprintf(stderr, "ERROR, LINE %d of %s\n", __LINE__, __FILE__);
         fprintf(stderr, "UNABLE TO GENERATE WITH COMMAND: %s\n", r1p->genstr);
         if (verb < 3)
         {
            int itmp;
            fprintf(stderr, "\nOUTPUT OF system():\n");
            sprintf(ln, "cat %s 1>&2\n", outnam);
            itmp = system(ln);
         }
         exit(-1);
      }
   }

   if (r1p->minN)
      N = Mmax(N, r1p->minN);
   if (r1p->minM)
   {
      M = Mmax(M, r1p->minM);
      if (lda < M)
         lda = M;
   }
   if (FLAG_IS_SET(r1p->flag, MVF_FNU))
      N = Mmax(r1p->NU, (N/r1p->NU)*r1p->NU);
   i = r1p->ldamul / pre2size(pre);
   lda = (i) ? ((lda+i-1)/i)*i : lda;

   if (FORCETIME < 0)
      sprintf(resf, "res/%cmvtmp", pre);
   else
      sprintf(resf, "res/%cmv%s", pre,
              GetResIdStr(r1p, M, N, lda, percL1, mflop));
   if (FORCETIME)
      remove(resf);
   dp = FORCETIME ? NULL : ReadResultsFile(0, nrep, resf);
   if (dp)
   {
      if (verb > 0)
         fprintf(stdout, "   %d:%s (M=%d, N=%d, lda=%d) gets %.2f MFLOPS\n",
                 r1p->ID, r1p->rout, M, N, lda, *dp);
      return(*dp);
   }

   sp = (r1p->TA == AtlasNoTrans || r1p->TA == AtlasConj) ? "mvn" : "mvt";
   if (percL1)
      i = sprintf(ln, "make %c%stime M=%d N=%d lda=%d l1mul=%d %srout=\"%s\"",
                  pre, sp, M, N, lda, percL1, sp, r1p->rout);
   else
      i = sprintf(ln, "make %c%sktime M=%d N=%d lda=%d %srout=\"%s\"",
                  pre, sp, M, N, lda, sp, r1p->rout);
   if (r1p->flag)
      i += sprintf(ln+i, " iflag=%d", r1p->flag);
   if (r1p->exflags)
      i += sprintf(ln+i, " %s", r1p->exflags);
   if (r1p->comp)
      i += sprintf(ln+i, " %cMVCC=\"%s\"", pre, r1p->comp);
   if (r1p->cflags)
      i += sprintf(ln+i, " %cMVFLAGS=\"%s\"", pre, r1p->cflags);
   i += SprintAlignStr(pre, r1p, ln+i);
   if (cflush >=0)
      i += sprintf(ln+i, " flushKB=%d", cflush);
   i += sprintf(ln+i, " tflags=\"-f %s", resf);
   if (nrep > 0)
      i += sprintf(ln+i, " -# %d", nrep);

   if (mflop >= 0)
      i += sprintf(ln+i, " -F %d", mflop);
   i += sprintf(ln+i, "\"");
   i += sprintf(ln+i, " mu=%d nu=%d", r1p->MU, r1p->NU);
   if (verb < 3)
      i += sprintf(ln+i, " > %s 2>&1\n", outnam);
   else
      i += sprintf(ln+i, "\n");
   i = system(ln);
   if (i)
   {
      fprintf(stderr, "\nERROR %d, LINE %d OF %s\n", i, __LINE__, __FILE__);
      fprintf(stderr, "SYSTEM CALL FAILED: %s\n", ln);
      if (verb < 3)
      {
         int itmp;
         fprintf(stderr, "\nOUTPUT OF system():\n");
         sprintf(ln, "cat %s 1>&2\n", outnam);
         itmp = system(ln);
         remove(outnam);
      }
      exit(-1);
   }
   if (verb < 3)
      remove(outnam);
   if (verb > 1)
   {
      dp = ReadResultsFile(1, nrep, resf);
      mf = PrintResultsFromFile(stdout, dp);
      free(dp);
      dp = &mf;
   }
   else
      dp = ReadResultsFile(0, nrep, resf);
   assert(dp);
   if (verb == 1)
      fprintf(stdout, "   %d:%s (M=%d, N=%d, lda=%d) gets %.2f MFLOPS\n",
              r1p->ID, r1p->rout, M, N, lda, *dp);
   return(*dp);
}

static void FillInMVExtractGenStrings(char pre, ATL_mvnode_t *kb)
/*
 * Creates generator strings to match kb settings
 */
{
   char ln[4096], *suff;
   int i, CL=8, mu;
   if (pre != 'd' && pre != 'c')
      CL = (pre == 'z') ? 4 : 16;
   while(kb)
   {
      if (kb->ID < 900000 || kb->ID >= 1000000)
      {
         kb = kb->next;
         continue;
      }
      if (kb->asmbits == asmNames2bitfield("GAS_x8664"))
      {
         assert(kb->MU%CL == 0);
         suff = "sse";
         mu = kb->MU/CL;
      }
      else
      {
         mu = kb->MU;
         if (kb->SSE)
            suff = "Csse";
         else
            suff = "C";
      }
      if (kb->TA == AtlasNoTrans)
         i = sprintf(ln, "make %cmvnext_%s order=clmajor mu=%d nu=%d", pre,
                     suff, mu, kb->NU);
      else
         i = sprintf(ln, "make %cmvtext_%s order=clmajor mu=%d nu=%d", pre,
                     suff, mu, kb->NU);
      if (kb->alignA && kb->alignA%16==0 && kb->ldamul && kb->ldamul%16==0)
         i += sprintf(ln+i, " genflags=\"-def ALIGNED 1\"");

      if (kb->genstr)
         free(kb->genstr);
      kb->genstr = DupString(ln);
      kb = kb->next;
   }
}
#endif  /* end guard around atlas_mvtesttime.h */
