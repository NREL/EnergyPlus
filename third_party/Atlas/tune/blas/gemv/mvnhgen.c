/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010, 2009 R. Clint Whaley
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
#include <string.h>
#include <assert.h>
#include "atlas_misc.h"
#include "atlas_mvparse.h"
#include "atlas_mvtesttime.h"

int GetPower2(int n)
{
   int pwr2, i;

   if (n == 1) return(0);
   for (pwr2=0, i=1; i < n; i <<= 1, pwr2++);
   if (i != n) pwr2 = 0;
   return(pwr2);
}

#define ShiftThresh 2
char *GetDiv(int N, char *inc)
/*
 * Given a runtime variable whose name is in the string inc that you want
 * to divide by the compile-time constant N, produces the appropriate shift
 * (N is power of 2) or division (N not a power of 2).
 */
{
   static char ln[256];
   int pwr2 = GetPower2(N);
   if (N == 1) sprintf(ln, "%s", inc);
   else if (pwr2) sprintf(ln, "((%s) >> %d)", inc, pwr2);
   else sprintf(ln, "((%s) / %d)", inc, N);
   return(ln);
}

char *GetMul(int N, char *inc)
/*
 * let inc be a runtime variable that you wish to multiply by the
 * compile-time constant N.  This routine attempts to use at most
 * ShiftThresh shifts and adds instead of using a multiply.  If more
 * than ShiftThresh adds are required, just uses multiply as normal.
 */
{
   static char ln0[256];
   char ln[256];
   char *p=ln;
   int i, n=N, iPLUS=0;

   if (n == 0)
   {
      ln[0] = '0';
      ln[1] = '\0';
   }
   while(n > 1)
   {
      for (i=0; n >= (1<<i); i++);
      if ( (1 << i) > n) i--;
      if (iPLUS++) *p++ = '+';
      sprintf(p, "((%s) << %d)", inc, i);
      p += strlen(p);
      n -= (1 << i);
   }
   if (n == 1)
   {
      if (iPLUS++) *p++ = '+';
      sprintf(p, "%s", inc);
   }
   if (iPLUS > ShiftThresh) sprintf(ln0, "(%d*(%s))", N, inc);
   else if (iPLUS) sprintf(ln0, "(%s)", ln);
   else sprintf(ln0, "%s", ln);
   return(ln0);
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

int FixMB(char pre, int mu, int mb)
/*
 * Makes sure MB is not a power of two, and that it won't mess up alignment
 * and that it is a multiple of mu
 */
{
   int MU;

/*
 * Find MU necessary to keep Cachelen-byte alignment & take LCM wt kernel's mu
 */
   if (pre == 's')
      MU = ATL_Cachelen/sizeof(float);
   else if (pre == 'c')
      MU = ATL_Cachelen/(2*sizeof(float));
   else if (pre == 'd')
      MU = ATL_Cachelen/sizeof(double);
   else  /* if (pre == 'z') */
      MU = ATL_Cachelen/(2*sizeof(double));
   MU = (MU) ? MU : 1;
   MU = Mylcm(mu, MU);
/*
 * If this is 0, then do not block!
 */
   return((mb/MU)*MU);
}

void PrintPrototype(FILE *fpout, char pre, char *rout, char *type, char *styp)
{
   fprintf(fpout,
      "void %s(ATL_CINT, ATL_CINT, const %s*, ATL_CINT, const %s*, %s*);\n",
           rout, type, type, type);
}

void GenKernFiles(char pre, char *path, ATL_mvnode_t *r1b)
/*
 * r1b is a list of rank-1 kernels that must be compiled (including those
 * needed to form SYR and SYR2).  This list should be unique (same kernel
 * not compiled twice).  r1b->kname will have the routine name to give the
 * kernel during compilation.
 */
{
   ATL_mvnode_t *r1p;
   char ln[2048];

   r1b = CloneMVQueue(r1b);
   r1b = DelRepeatedMVKernels(r1b);
   FillInMVExtractGenStrings(pre, r1b);
   for (r1p = r1b; r1p; r1p = r1p->next)
   {
      if (r1p->genstr)   /* generate kernel if necessary */
      {
         assert(!system(r1p->genstr));
         sprintf(ln, "cp EXTDIR/%s %s/%s.c\n", r1p->rout, path, r1p->kname);
      }
      else
         sprintf(ln, "cp MVNCASES/%s %s/%s.c\n", r1p->rout, path, r1p->kname);
      if (system(ln))
      {
         fprintf(stderr, "FAILED: %s\n", ln);
         exit(-1);
      }
   }
   KillAllMVNodes(r1b);
}

void EmitMakefile(char pre, char *path, ATL_mvnode_t *r1b)
/*
 * r1b is a list of kernels that must be compiled (including those
 * needed to form SYR and SYR2).  This list should be unique (same kernel
 * not compiled twice).  r1b->kname will have the routine name to give the
 * kernel during compilation.
 */
{
   ATL_mvnode_t *r1p, *r1k;
   char *kern, *outf, *typD;
   FILE *fpout;
   int i, ialias=0, USEGOODGCC=0;
   const char UPRE = (pre == 'z' || pre == 'd') ? 'D' : 'S';
   static char *aliased[16];

   r1b = CloneMVQueue(r1b);  /* get our own copy of kernel Q we can mess up */
   r1b = DelRepeatedMVKernels(r1b);  /* don't compile same kernel twice */
   assert(path);
   if (pre == 'd')
      typD = "DREAL";
   else if (pre == 's')
      typD = "SREAL";
   else if (pre == 'c')
      typD = "SCPLX";
   else if (pre == 'z')
      typD = "DCPLX";
   else
      assert(0);

   i = strlen(path);
   outf = malloc((i+16)*sizeof(char));
   assert(outf);
   strcpy(outf, path);
   strcpy(outf+i, "/Make_");   /* Make_<pre>mvn */
   outf[i+6] = pre;
   outf[i+7] = 'm';
   outf[i+8] = 'v';
   outf[i+9] = 'n';
   outf[i+10] = '\0';
   fpout = fopen(outf, "w");
   assert(fpout);

   fprintf(fpout, "#\n#  This file generated at line %d of %s\n#\n",
           __LINE__, __FILE__);
   fprintf(fpout, "include Make.inc\n\n");
   fprintf(fpout, "MVNCC = $(%cKC)\nMVNFLAGS = $(CDEFS) $(%cKCFLAGS)",
            UPRE, UPRE);
   fprintf(fpout, " -D%s\n\n", typD);
   fprintf(fpout, "obj =");
   for (r1p=r1b; r1p; r1p = r1p->next)
      fprintf(fpout, " %s.o %s_b0.o", r1p->kname, r1p->kname);
   fprintf(fpout, "\n");

   fprintf(fpout, "lib : %clib\n%clib : %cmvnk.grd\n", pre, pre, pre);
   fprintf(fpout, "%cmvnk.grd : $(obj)\n", pre);
   fprintf(fpout, "\t$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(obj)\n");
   fprintf(fpout, "\t$(RANLIB) $(ATLASlib)\n");
   fprintf(fpout, "\ttouch %cmvnk.grd\n", pre);

   fprintf(fpout, "%cclean : clean\n", pre);
   fprintf(fpout, "clean :\n\t- rm -f $(obj) %cmvnk.grd\n\n", pre);

   fprintf(fpout, "%ckilllib : killlib\n", pre);
   fprintf(fpout, "killlib : \n");
   fprintf(fpout, "\t- $(ARCHIVER) d $(ATLASlib) $(obj)\n");
   fprintf(fpout, "\t$(RANLIB) $(ATLASlib)\n");
   fprintf(fpout, "killall : killlib clean\n");
   fprintf(fpout, "\t rm -f");
   for (r1p=r1b; r1p; r1p = r1p->next)
      fprintf(fpout, " %s.c", r1p->kname);
   fprintf(fpout, "\n\n");
/*
 * Spit out build command for all surviving kernels
 */
   for (r1p=r1b; r1p; r1p = r1p->next)
   {
      fprintf(fpout, "%s.o : %s.c\n", r1p->kname, r1p->kname);
      if (r1p->comp)
      {
         if (r1p->comp[0] == 'g' && r1p->comp[1] == 'c' &&
             r1p->comp[2] == 'c' && r1p->comp[3] == '\0')
         {
            USEGOODGCC = 1;
            fprintf(fpout, "\t $(GOODGCC)");
         }
         else
            fprintf(fpout, "\t %s", r1p->comp);
      }
      else
         fprintf(fpout, "\t $(MVNCC)");
      fprintf(fpout, " -o %s.o -c -DATL_UGEMV=%s", r1p->kname, r1p->kname);
      if (r1p->cflags)
      {
         if (r1p->comp)
         {
            fprintf(fpout, " %s -D%s $(CDEFS)", r1p->cflags, typD);
            #ifdef ATL_DYLIBS
               if (strstr(r1p->comp, "gcc") && !strstr(r1p->cflags, "-fPIC"))
                  fprintf(fpout, " -fPIC");
            #endif
         }
         else /* using default compiler, start wt default flags */
            fprintf(fpout, " $(MVNFLAGS) -D%s %s", typD, r1p->cflags);
      }
      else
         fprintf(fpout, " $(MVNFLAGS)");
      fprintf(fpout, " %s.c\n", r1p->kname);
      USEGOODGCC = 0;
      fprintf(fpout, "%s_b0.o : %s.c\n", r1p->kname, r1p->kname);
      if (r1p->comp)
      {
         if (r1p->comp[0] == 'g' && r1p->comp[1] == 'c' &&
             r1p->comp[2] == 'c' && r1p->comp[3] == '\0')
         {
            USEGOODGCC = 1;
            fprintf(fpout, "\t $(GOODGCC)");
         }
         else
            fprintf(fpout, "\t %s", r1p->comp);
      }
      else
         fprintf(fpout, "\t $(MVNCC)");
      fprintf(fpout, " -o %s_b0.o -c -DATL_UGEMV=%s_b0 -DBETA0",
              r1p->kname, r1p->kname);
      if (r1p->cflags)
      {
         if (r1p->comp)
         {
            fprintf(fpout, " %s -D%s $(CDEFS)", r1p->cflags, typD);
            #ifdef ATL_DYLIBS
               if (strstr(r1p->comp, "gcc") && !strstr(r1p->cflags, "-fPIC"))
                  fprintf(fpout, " -fPIC");
            #endif
         }
         else /* using default compiler, start wt default flags */
            fprintf(fpout, " $(MVNFLAGS) -D%s %s", typD, r1p->cflags);
      }
      else
         fprintf(fpout, " $(MVNFLAGS)");
      fprintf(fpout, " %s.c\n", r1p->kname);
   }
   KillAllMVNodes(r1b);  /* done wt our copy of these queues */
   free(outf);
}

void mvnkhgen(char pre, char *path, ATL_mvnode_t *r1b)
{
   char *ln;
   int i;
   FILE *fpout;
   ATL_mvnode_t *r1p;
   char *styp, *type = (pre == 'd' || pre == 'z') ? "double" : "float";
   char PRE;
   void PrintPrototypes(char pre, FILE *fpout, ATL_mvnode_t *kb);

   PRE = toupper(pre);
   if (pre == 'd')
      styp = "double";
   else if (pre == 's')
      styp = "float";
   else if (pre == 'c')
      styp = "float*";
   else
      styp = "double*";

   i = strlen(path);
   ln = malloc(i+32*sizeof(char));
   sprintf(ln, "%s/atlas_%cmvnkernels.h", path, pre);

   fpout = fopen(ln, "w");
   assert(fpout);
   fprintf(fpout, "/*\n * This file generated on line %d of %s\n */\n",
           __LINE__, __FILE__);
   fprintf(fpout,
           "#ifndef ATLAS_%cMVNKERNELS_H\n   #define ATLAS_%cMVNKERNELS_H\n\n",
           PRE, PRE);

   PrintPrototypes(pre, fpout, r1b);
   fprintf(fpout, "\n#endif /* end guard around atlas_%cmvnkernels.h */\n", pre);

   fclose(fpout);
   free(ln);
}

char *Pre2Type(char pre)
{
   return((pre == 'c' || pre == 's') ? "float" : "double");
}

int Pre2SizeMin(char pre)
{
   return((pre == 'c' || pre == 's') ? 4 : 8);
}

int Pre2Size(char pre)
{
   if (pre == 'c' || pre == 'd')
      return(8);
   return(pre == 'z' ? 16 : 4);
}

char *Pre2ScalarType(char pre)
{
   char *sp;
   if (pre == 'c')
      sp = "float *";
   else if (pre == 'z')
      sp = "double *";
   else if (pre == 's')
      sp = "float";
   else
      sp = "double";
   return(sp);
}

void GenGetKern(char pre, ATL_mvnode_t *kb, FILE *fpout)
{
   char *type, *styp;
   ATL_mvnode_t *kp;
   int i, minsize;
   char spcs[128], *spc = spcs+127;

   type = Pre2Type(pre);
   styp = Pre2ScalarType(pre);
   minsize = Pre2SizeMin(pre);
   for (i=0; i < 127; i++)
      spcs[i] = ' ';
   spcs[127] = '\0';

   fprintf(fpout, "static ATL_mvkern_t ATL_GetMVNKern\n");
   fprintf(fpout, "   (ATL_CINT M, ATL_CINT N, const void *A, ATL_CINT lda,\n");
   fprintf(fpout, "    ATL_mvkern_t *mvk_b0, int *DOTBASED,\n");
   fprintf(fpout,
 "    int *mu, int *nu, int *minM, int *minN, int *alignX, int *ALIGNX2A,\n");
   fprintf(fpout,
   "    int *alignY, int *FNU, ATL_INT *CacheElts) \n{\n");

   spc -= 3;
   for (kp=kb; kp; kp = kp->next)
   {
      if (kp->rankR)  /* if its restricted, will need to see if we can use */
      {
         if (kp->alignA)
         {
            fprintf(fpout, "%sif (%s == (size_t)(A))\n%s{\n", spc,
                    GetMul(kp->alignA, GetDiv(kp->alignA, "((size_t)(A))")),
                    spc);
            spc -= 3;
         }
         if (kp->ldamul > 1)
         {
            fprintf(fpout, "%sif (%s == ATL_MulBySize(lda))\n%s{\n", spc,
                    GetMul(kp->ldamul,GetDiv(kp->ldamul, "ATL_MulBySize(lda)")),
                    spc);
            spc -= 3;
         }
         if (kp->minN > 1)
         {
            fprintf(fpout, "%sif (N >= %d)\n%s{\n", spc, kp->minN, spc);
            spc -= 3;
         }
         if (kp->minM > 1)
         {
            fprintf(fpout, "%sif (M >= %d)\n%s{\n", spc, kp->minM, spc);
            spc -= 3;
         }
      }
      fprintf(fpout, "%s*minM = %d;   *minN = %d;\n", spc, kp->minM, kp->minN);
      fprintf(fpout, "%s*mu = %d;     *nu = %d;\n", spc, kp->MU, kp->NU);
      fprintf(fpout, "%s*alignX = %d;  *alignY = %d;\n", spc,
              kp->alignX > 1 ? kp->alignX : minsize,
              kp->alignY > 1 ? kp->alignY : minsize);
      fprintf(fpout, "%s*ALIGNX2A = %d;\n", spc,
              FLAG_IS_SET(kp->flag, MVF_ALIGNX2A));
      fprintf(fpout, "%s*FNU = %d;\n", spc, FLAG_IS_SET(kp->flag, MVF_FNU));
      fprintf(fpout, "%s*CacheElts = %d;\n", spc, kp->CacheElts);
      fprintf(fpout, "%s*mvk_b0 = %s_b0;\n", spc, kp->kname);
      fprintf(fpout, "%s*DOTBASED = %d;\n", spc,
              FLAG_IS_SET(kp->flag, MVF_AXPYBASED) ? 0 : kp->ntlb);
      fprintf(fpout, "%sreturn(%s);\n", spc, kp->kname);
      if (kp->rankR)  /* if its restricted, end any ifs */
      {
         if (kp->minM > 1)
         {
           spc += 3;
           fprintf(fpout, "%s} /* end if on minimal N guard */\n", spc);
         }
         if (kp->minN > 1)
         {
           spc += 3;
           fprintf(fpout, "%s} /* end if on minimal M guard */\n", spc);
         }
         if (kp->ldamul > 1)
         {
           spc += 3;
           fprintf(fpout,"%s} /* end if on lda multiple restriction */\n", spc);
         }
         if (kp->alignA)
         {
           spc += 3;
           fprintf(fpout, "%s} /* end if on align of A */\n", spc);
         }
      }
   }
   fprintf(fpout, "}\n");
}

void PrintPrototypes(char pre, FILE *fpout, ATL_mvnode_t *kb)
{
   ATL_mvnode_t *kp;
   char *type;

   type = Pre2Type(pre);
   for (kp=kb; kp; kp = kp->next)
   {
      fprintf(fpout,
         "void %s(ATL_CINT, ATL_CINT, const %s*, ATL_CINT, const %s*, %s*);\n",
              kp->kname, type, type, type);
      fprintf(fpout,
      "void %s_b0(ATL_CINT, ATL_CINT, const %s*, ATL_CINT, const %s*, %s*);\n",
              kp->kname, type, type, type);
   }
   fprintf(fpout, "\n");
}

void mvnhgen(char pre, char *path, int LVL, ATL_mvnode_t *kb)
{
   ATL_mvnode_t *kp, *kur;
   FILE *fpout;
   char PRE = toupper(pre);
   char *styp, *sp, *type;
   char gerk[32];
   int mb, nb, TIMECASE=0;
   int irest=12, iconj=8;
   int alignX=0, alignY=0;

   assert(kb);
   if (LVL < 0)
   {
      TIMECASE = 1;
      LVL = 0;
   }
   for (kur=kb; kur->next; kur = kur->next);  /* unrestricted kernel */
/*
 * Name the kernel according to cache block level and data type
 */
   assert(LVL >= 0 && LVL <= 9);
   sp = malloc(strlen(path) + 18);
   assert(sp);
   if (!LVL)
      sprintf(sp, "%s/atlas_%cmvn.h", path, pre);
   else
      sprintf(sp, "%s/atlas_%cmvn_L%d.h", path, pre, LVL);
   fpout = fopen(sp, "w");
   free(sp);
   type = Pre2Type(pre);
   styp = Pre2ScalarType(pre);

   fprintf(fpout, "#ifndef ATLAS_%cMVN_L%d_H\n#define ATLAS_%cMVN_L%d_H\n\n",
           PRE, LVL, PRE, LVL);

   fprintf(fpout, "#include \"atlas_type.h\"\n\n");
   fprintf(fpout, "#ifndef ATL_MVKERN_DEF\n");
   fprintf(fpout, "   #define ATL_MVKERN_DEF\n");
   fprintf(fpout, "   typedef void (*ATL_mvkern_t)\n");
   fprintf(fpout,
      "      (ATL_CINT, ATL_CINT, const %s*, ATL_CINT, const %s*, %s*);\n\n",
           type, type, type);
   fprintf(fpout, "#endif\n");

   if (TIMECASE)
      kur->kname = DupString("ATL_UGEMVNK");
   PrintPrototypes(pre, fpout, kb);
   GenGetKern(pre, kb, fpout);
   if (!FLAG_IS_SET(kur->flag, MVF_AXPYBASED))
   {
      nb = (kur->ntlb >= 0) ? kur->ntlb : 0;
      nb = (nb > kur->MU) ?  nb = (nb/kur->MU)*kur->MU : kur->MU;
      mb = 0;
   }
   else
   if (kur->CacheElts)
   {
      mb = (kur->CacheElts - 2*kur->NU) / (2*(kur->NU+1));
      mb = (mb > kur->MU) ? (mb/kur->MU)*kur->MU : 0;
      nb = kur->NU;
   }
   else
      mb = nb = 0;
   fprintf(fpout,
"\n#define ATL_GetPartMVN(A_, lda_, mb_, nb_) { *(mb_) = %d; *(nb_) = %d; }\n",
           mb, nb);

   fprintf(fpout,
           "\n#endif  /* end protection around header file contents */\n");
   fclose(fpout);

}

void PrintUsage(char *name, int ierr, char *flag)
{
   if (ierr > 0)
      fprintf(stderr, "Bad argument #%d: '%s'\n",
              ierr, flag ? flag : "Not enough arguments");
   else if (ierr < 0)
      fprintf(stderr, "ERROR: %s\n", flag);
   fprintf(stderr, "USAGE: %s [flags]:\n", name);
   fprintf(stderr, "   -p [s,d,c,z]: set type/precision prefix \n");
   fprintf(stderr, "   -d <dir> : output files using path <dir>\n");
   fprintf(stderr, "   -F <file> : read kernel file & gen headers\n");
   fprintf(stderr, "    The following flags can be used if -F is not:\n");
   fprintf(stderr, "      -l <l1mul> : use l1mul*L1CacheSize for blocking\n");
   fprintf(stderr, "      -m <mu> : mu rows unrolled for matrix access\n");
   fprintf(stderr, "      -n <nu> : nu cols unrolled for matrix access\n");
   fprintf(stderr, "      -f <iflag> : set the flag bitfield to iflag\n");
   fprintf(stderr,
           "      -t <ntlb> : # of cols dot-based gemvN should traverse\n");
   exit(ierr ? ierr : -1);
}

void GetFlags(int nargs, char **args, char *PRE, char **FNAM, char **DIR,
              int *MU, int *NU, int *DOTBASED, int *L1MUL, int *IFLAG)
{
   int i, k;
   char pre='d';

   *DIR = "./";
   *IFLAG = *MU = *NU = *L1MUL = 0;
   *FNAM = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'm':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *MU = atoi(args[i]);
         break;
      case 'n':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *NU = atoi(args[i]);
         break;
      case 'l':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *L1MUL = atoi(args[i]);
         break;
      case 't':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *DOTBASED = atoi(args[i]);
         break;
      case 'f':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *IFLAG = atoi(args[i]);
         break;
      case 'd':
         if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
         *DIR = args[i];
         break;
      case 'p':
        if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
        pre = tolower(args[i][0]);
        assert(pre == 's' || pre == 'd' || pre == 'z' || pre == 'c');
        break;
      case 'F':
        if (++i >= nargs)
            PrintUsage(args[0], i-1, NULL);
        *FNAM = args[i];
        break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
   *PRE = pre;
   if (*FNAM == NULL && (*MU == 0 || *NU == 0))
   {
      *FNAM = malloc(16*sizeof(char));
      sprintf(*FNAM, "res/%cR1K.sum", pre);
   }
}

int main(int nargs, char **args)
{
   ATL_mvnode_t *bases[4];
   char *fnam, *path;
   ATL_mvnode_t *r1b, *r1p, *r1B;
   int i, mu, nu, l1mul, iflag, L1Elts, dotbased=0;
   char pre;

   GetFlags(nargs, args, &pre, &fnam, &path, &mu, &nu, &dotbased,
            &l1mul, &iflag);

/*
 * If we just want simple tuning header, no need to read file for details
 */
   if (!fnam)
   {
      r1b = GetMVNode();
      r1b->next = NULL;
      r1b->minM = r1b->MU = mu;
      r1b->minN = r1b->NU = nu;
      r1b->flag = iflag;

      if (dotbased)
      {
         r1b->ntlb = dotbased;
         r1b->flag &= ~(MVF_AXPYBASED);
      }
      else
         r1b->flag |= MVF_AXPYBASED;
      r1b->CacheElts = (l1mul/100.0) * GetL1CacheElts(pre);
      mvnhgen(pre, path, -1, r1b);
      exit(0);
   }
/*
 * Otherwise, we should be doing a full-blown install; read in summary file
 */
   r1b = ReadMVFile(fnam);
   SetAllMVTypeFlags(pre, r1b);
/*
 * Generate prototype file for all routines
 */
   mvnkhgen(pre, path, r1b);
/*
 * For each cache level, generate a header file which provides the function
 * returning the best kernel and its parameters
 */
   ATL_MVSplitContexts(r1b, bases, bases+1, bases+2, bases+3);
   mvnhgen(pre, path, 0, bases[0]);
   mvnhgen(pre, path, 2, bases[1]);
   mvnhgen(pre, path, 1, bases[2]);
   r1b = ATL_MVLinkContexts(bases[0], bases[1], bases[2], bases[3]);
/*
 * Generate Makefiles to compile all GER kernels (including those used by
 * SYR and SYR2).  These Makefiles & kernels will wind up in
 *   BLDdir/src/blas/ger/
 */
   EmitMakefile(pre, path, r1b);
/*
 * Get required .c kernel files
 */
   GenKernFiles(pre, path, r1b);
   KillAllMVNodes(r1b);
   return(0);
}
