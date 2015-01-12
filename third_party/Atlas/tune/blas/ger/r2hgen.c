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
#include "atlas_r1parse.h"
#include "atlas_r2testtime.h"

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

void UnrollSYR2
(
   FILE *fpout,         /* stream to print to */
   char *name,          /* name for macro */
   char pre,            /* precisition/type prefix */
   enum ATLAS_UPLO Uplo,
   int  nu              /* unroll factor */
)
/*
 * For SYR and SYR2, generate a macro which does a small NUxNU
 * triangular matrix so that GER kernel can be called
 * on rest of NU-wide panel.
 * Real precision unroll of SYR2
 */
{
   int i, j;

   fprintf(fpout, "#define %s(A_, lda_, x_, y_) \\\n{ \\\n", name);
   fprintf(fpout, "   TYPE *aa=(A_); \\\n");
   fprintf(fpout, "   ATL_CINT lda0_ = 0");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", lda%d_ = lda%d_+(lda_)", i, i-1);
   fprintf(fpout, "; \\\n   const TYPE x0_=*(x_)");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", x%d_=(x_)[%d]", i, i);
   fprintf(fpout, "; \\\n   const TYPE y0_=*(y_)");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", y%d_=(y_)[%d]", i, i);
   fprintf(fpout, "; \\\n");
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < nu; j++)
         for (i=0; i <= j; i++)
            fprintf(fpout, "   aa[lda%d_+%d] += x%d_*y%d_ + y%d_*x%d_; \\\n",
                    j, i, i, j, i, j);
   }
   else
   {
      for (j=0; j < nu; j++)
         for (i=j; i < nu; i++)
            fprintf(fpout, "   aa[lda%d_+%d] += x%d_*y%d_ + y%d_*x%d_; \\\n",
                    j, i, i, j, i, j);
   }
   fprintf(fpout, "}\n");
}

void UnrollHER2
(
   FILE *fpout,         /* stream to print to */
   char *name,          /* name for macro */
   char pre,            /* precisition/type prefix */
   enum ATLAS_UPLO Uplo,
   int  nu              /* unroll factor */
)
/*
 * Complex type unroll of HER2
 */
{
   int i, j;

   fprintf(fpout, "#define %s(A_, lda_, x_, y_, xt_, yt_) \\\n{ \\\n", name);
   fprintf(fpout, "   TYPE *aa=(A_); \\\n");
   fprintf(fpout, "   ATL_CINT lda0_ = 0");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", lda%d_ = lda%d_+(lda_)+(lda_)", i, i-1);
   fprintf(fpout, "; \\\n   const TYPE x0r=*(x_), x0i=(x_)[1]");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", x%dr=(x_)[%d], x%di=(x_)[%d]", i, 2*i, i, 2*i+1);
   fprintf(fpout, "; \\\n   const TYPE xt0r=*(xt_), xt0i=(xt_)[1]");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", xt%dr=(xt_)[%d], xt%di=(xt_)[%d]", i, 2*i, i, 2*i+1);
   fprintf(fpout, "; \\\n   const TYPE y0r=*(y_), y0i=(y_)[1]");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", y%dr=(y_)[%d], y%di=(y_)[%d]", i, 2*i, i, 2*i+1);
   fprintf(fpout, "; \\\n   const TYPE yt0r=*(yt_), yt0i=(yt_)[1]");
   for (i=1; i < nu; i++)
      fprintf(fpout, ", yt%dr=(yt_)[%d], yt%di=(yt_)[%d]", i, 2*i, i, 2*i+1);
   fprintf(fpout, "; \\\n");
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < nu; j++)
      {
         for (i=0; i < j; i++)
         {
            fprintf(fpout,
      "   aa[lda%d_+%d] += x%dr*yt%dr-x%di*yt%di + y%dr*xt%dr-y%di*xt%di; \\\n",
                    j, 2*i, i, j, i, j, i, j, i, j);
            fprintf(fpout,
      "   aa[lda%d_+%d] += x%dr*yt%di+x%di*yt%dr + y%dr*xt%di+y%di*xt%dr; \\\n",
                    j, 2*i+1, i, j, i, j, i, j, i, j);
         }
         fprintf(fpout,
      "   aa[lda%d_+%d] += x%dr*yt%dr-x%di*yt%di + y%dr*xt%dr-y%di*xt%di; \\\n",
                 j, 2*j, j, j, j, j, j, j, j, j);
         fprintf(fpout, "   aa[lda%d_+%d] = 0.0; \\\n", j, 2*j+1);
      }
   }
   else
   {
      for (j=0; j < nu; j++)
      {
         fprintf(fpout,
      "   aa[lda%d_+%d] += x%dr*yt%dr-x%di*yt%di + y%dr*xt%dr-y%di*xt%di; \\\n",
                 j, 2*j, j, j, j, j, j, j, j, j);
         fprintf(fpout, "   aa[lda%d_+%d] = 0.0; \\\n", j, 2*j+1);
         for (i=j+1; i < nu; i++)
         {
            fprintf(fpout,
      "   aa[lda%d_+%d] += x%dr*yt%dr-x%di*yt%di + y%dr*xt%dr-y%di*xt%di; \\\n",
                    j, 2*i, i, j, i, j, i, j, i, j);
            fprintf(fpout,
      "   aa[lda%d_+%d] += x%dr*yt%di+x%di*yt%dr + y%dr*xt%di+y%di*xt%dr; \\\n",
                    j, 2*i+1, i, j, i, j, i, j, i, j);
         }
      }
   }
   fprintf(fpout, "}\n");
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

void s2hgen
(
   ATL_r1node_t *r1B,   /* standard combined kernel list */
   int LVL,             /* 0:out-of-cache, 1: in-L1, 2: in-L2 */
   int L1Elts,          /* number of elements in L1 cache */
   char pre,
   char *path           /* path to generate header files in */
)
{
   int Pre2Size(char pre);
   ATL_r1node_t *r1q, *r1ur;  /* queue of kernels to use, unrestricted kern */
   ATL_r1node_t *ocb, *i2b, *i1b, *ocl1b, *r1p, *r1t, *minp;
   FILE *fpout;
   int maxNU, NUL, NU0L, NUU, NU0U, NU, NU0, NU0s[2], NUs[2], imf, i, j;
   const int tsize = Pre2Size(pre);
   char *ln;
   char PRE = toupper(pre);

   r1B = CloneR1Queue(r1B);   /* get private copy for destruction */
   ATL_R1SplitContexts(r1B, &ocb, &i2b, &i1b, &ocl1b);
/*
 * Select kernels based on LVL
 */
   if (!LVL)
   {
      imf = 0;
      r1q = ocb;
      maxNU = 16;
   }
   else if (LVL == 2)
   {
      imf = 3;
      r1q = i2b;
      maxNU = 16;
   }
   else if (LVL == 1)
   {
      imf = 4;
      r1q = i1b;
      maxNU = 8;
   }
   if (ocb != r1q)
      KillAllR1Nodes(ocb);
   if (i2b != r1q)
      KillAllR1Nodes(i2b);
   if (i1b != r1q)
      KillAllR1Nodes(i1b);
   if (ocl1b != r1q)
      KillAllR1Nodes(ocl1b);
   r1ur = ATL_LastR1Node(r1q);  /* find unrestricted kernel */
/*
 * Find NUs that would work for all kernels
 */
   if (r1ur->alignY && tsize%r1ur->alignY)
      NU0U = Mylcm(r1ur->NU, Mylcm(r1ur->alignY, tsize)/tsize);
   else
      NU0U = r1ur->NU;
   NU0L = (r1ur->alignX) ? Mylcm(NU0U, Mylcm(r1ur->alignY, tsize)/tsize) : NU0U;
   NUU = NU0U;
   NUL = NU0L;
   for (r1p=r1q; r1p->next; r1p = r1p->next)
   {
      NUU = Mylcm(NUU, r1p->NU);
      NUL = Mylcm(NUL, r1p->NU);
   }
   NU0s[0] = NU0U;
   NU0s[1] = NU0L;
   NUs[0] = NUU;
   NUs[1] = NUL;

/*
 * If NU has grown large enough to significantly impact our ability to
 * use it to tune the kernel, eliminate restricted kernel that causes us
 * to use the largest unrolling until NU is small enough
 */
   for (j=0; j < 2; j++)
   {
      NU0 = NU0s[j];
      NU = NUs[j];
      while (NU > maxNU && maxNU >= NU0)
      {
         NU = NU0;
         r1t = NULL;
         for (i=NU0, r1p=r1q; r1p->next; r1p = r1p->next)
         {
            i = Mylcm(i, r1p->NU);
            if (i > maxNU && r1p->next)  /* don't delete unrest kernel */
            {
               r1t = r1p;
               break;
            }
         }
         if (r1t)
            r1q = KillR1NodeFromQ(r1q, r1t);
         assert(r1q);
         NU = NU0;
         for (r1p=r1q; r1p->next; r1p = r1p->next)
            NU = Mylcm(NU, r1p->NU);
      }
/*
 *    See if we can allow NU to be a multiple of everybodies' alignment
 */
      if (NU < maxNU)
      {
         for (i=NU, r1p=r1q; r1p; r1p = r1p->next)
         {
            if (r1p->alignY && tsize%r1p->alignY)
               i = Mylcm(i, Mylcm(r1p->alignY, tsize)/tsize);
            if (j == 1)
            {
               if (r1p->alignX && tsize%r1p->alignX)
                  i = Mylcm(i, Mylcm(r1p->alignX, tsize)/tsize);
            }
            if (i > maxNU)
            {
               i = 0;
               break;
            }
         }
         NU = (i) ? i : NU;
      }
      NUs[j] = NU;
   }
   NUU = NUs[0];
   NUL = NUs[1];
   i = strlen(path) + 32;
   ln = malloc(i);
   assert(ln);

   if (LVL)
      sprintf(ln, "%s/atlas_%c%s_L%d.h", path, pre,
              (pre == 'c' || pre == 'z') ? "her2":"syr2", LVL);
   else
      sprintf(ln, "%s/atlas_%c%s.h", path, pre,
              (pre == 'c' || pre == 'z') ? "her2":"syr2");
   fpout = fopen(ln, "w");
   free(ln);
   fprintf(fpout, "/*\n * This file generated on line %d of %s\n */\n",
           __LINE__, __FILE__);
   if (LVL)
   {
      fprintf(fpout,
      "#ifndef ATLAS_%cSYR2_L%d_H\n   #define ATLAS_%cSYR2_L%d_H\n\n",
              PRE, LVL, PRE, LVL);
      fprintf(fpout, "#include \"atlas_%cr2_L%d.h\"\n", pre, LVL);
   }
   else
   {
      fprintf(fpout,
              "#ifndef ATLAS_%cSYR2_H\n   #define ATLAS_%cSYR2_H\n\n",
              PRE, PRE);
      fprintf(fpout, "#include \"atlas_%cr2.h\"\n", pre);
   }

//   NU = (NU < 4) ? NU+NU : NU;
   fprintf(fpout, "\n#define ATL_s2U_NU %d\n", NUU);
   fprintf(fpout, "\n#define ATL_s2L_NU %d\n", NUL);
/*
 * Only out-of-cache needs blocking stuff; others always unblocked
 */
   if (!LVL)
   {
      fprintf(fpout, "\n");
      if (!r1ur->CacheElts)
      {
         fprintf(fpout, "#define ATL_NOBLOCK_S2 1\n");
         fprintf(fpout,
  "#define ATL_GetPartS2(A_, lda_, mb_, nb_) { (mb_) = 0; (nb_) = 0; }\n");
      }
      else if (NU == r1ur->NU)
         fprintf(fpout, "#define ATL_GetPartS2 ATL_GetPartR2\n");
      else
      {
         i = r1ur->CacheElts;
         i = (i-4*NU)/(2*(NU+1));
         i = FixMB(pre, r1ur->MU, i);
         fprintf(fpout,
  "#define ATL_GetPartS2(A_, lda_, mb_, nb_) { (mb_) = %d; (nb_) = %d; }\n",
                 i, i ? NU : 0);
      }
   }
/*
 * Find the smallest M that can be used with all kernels
 */
   for (i=r1q->minM, minp=r1p=r1q; r1p->next; r1p = r1p->next)
   {
      i = Mmax(r1p->minM, i);
      if (r1p->minM < minp->minM)
         minp = r1p;
   }
   fprintf(fpout, "#define ATL_MIN_RESTRICTED_M %d\n", i);
/*
 * ATL_URGERK is always the unrestricted kernel
 */
   fprintf(fpout, "#define ATL_URGERK %s\n", r1ur->kname);

/*
 * ATL_GENGERK will be a kernel that can be called with any operands
 */
   if (r1ur->minM < 2 && r1ur->minN < 2)
      fprintf(fpout, "#define ATL_GENGERK %s\n", r1ur->kname);
   else
   {
      char *one, *pN;
      fprintf(fpout,
         "static void ATL_GENGERK(ATL_CINT M, ATL_CINT N, const TYPE *X,\n");
      fprintf(fpout,
      "                        const TYPE *Y, const TYPE *W, const TYPE *Z,\n");
      fprintf(fpout,
         "                        TYPE *A, ATL_CINT lda)\n");
      fprintf(fpout,
              "{\n   int nu, minM, minN, i, FNU, aX, aX2A, aY, aW, aZ;\n");
      fprintf(fpout, "   ATL_INT CEL;\n   ATL_r2kern_t gerk;\n");
      if (pre == 'd' || pre == 's')
      {
          one = "ATL_rone";
          pN = "n";
      }
      else
      {
          fprintf(fpout, "   const TYPE one[2] = {ATL_rone, ATL_rzero};\n");
          one = "one";
          pN = "(n+n)";
      }
      fprintf(fpout,
         "   gerk = ATL_GetR2Kern(M, N, A, lda, &i, &nu, &minM, &minN,\n");
      fprintf(fpout,
         "                        &aX, &aX2A, &aY, &FNU, &CEL);\n");
      fprintf(fpout, "   if (aX2A)\n   {\n");
      fprintf(fpout,
  "      aX = ((size_t)A) %% ATL_Cachelen == ((size_t)X) %% ATL_Cachelen;\n");
      fprintf(fpout,
  "      aW = ((size_t)A) %% ATL_Cachelen == ((size_t)W) %% ATL_Cachelen;\n");
      fprintf(fpout,
      "   }   else\n   {\n");
      fprintf(fpout,
              "      aW = (aX) ? (((size_t)W)/aX)*aX == (size_t)W : 1;\n");
      fprintf(fpout,
              "      aX = (aX) ? (((size_t)X)/aX)*aX == (size_t)X : 1;\n");
      fprintf(fpout, "   }\n");
      fprintf(fpout, "   aZ = (aY) ? (((size_t)Z)/aY)*aY == (size_t)Z : 1;\n");
      fprintf(fpout, "   aY = (aY) ? (((size_t)Y)/aY)*aY == (size_t)Y : 1;\n");
      fprintf(fpout,
              "   if (M >= minM && N >= minN && aX && aY && aW && aZ)\n   {\n");
      fprintf(fpout, "      if (FNU)\n      {\n");
      fprintf(fpout, "          ATL_CINT n = (N/nu)*nu, nr=N-n;\n");
      fprintf(fpout, "          gerk(M, n, X, Y, W, Z, A, lda);\n");
      fprintf(fpout, "          if (nr)\n");
      fprintf(fpout, "             ATL_%cger2k_Nlt8(M, nr, %s, X, 1, Y+%s, 1, %s, W, 1, Z+%s, 1, A+%s*lda, lda);\n",
              pre, one, pN, one, pN, pN);
      fprintf(fpout, "      } /* end if (FNU) */\n");
      fprintf(fpout, "      else\n");
      fprintf(fpout, "         gerk(M, N, X, Y, W, Z, A, lda);\n");
      fprintf(fpout, "   } /* end if can call optimized kernel */\n");
      fprintf(fpout, "   else\n      ATL_%cger2k_Mlt16(M, N, %s, X, 1, Y, 1, %s, W, 1, Z, 1, A, lda);\n",
              pre, one, one);
      fprintf(fpout, "}\n");
   }

   fprintf(fpout, "\n");
   if (pre == 's' || pre == 'd')
   {
      UnrollSYR2(fpout, "ATL_SYR2U_nu", pre, AtlasUpper, NUU);
      UnrollSYR2(fpout, "ATL_SYR2L_nu", pre, AtlasLower, NUL);
   }
   else
   {
      UnrollHER2(fpout, "ATL_HER2U_nu", pre, AtlasUpper, NUU);
      UnrollHER2(fpout, "ATL_HER2L_nu", pre, AtlasLower, NUL);
   }
   KillAllR1Nodes(r1q);

   fprintf(fpout, "\n#endif\n");
   fclose(fpout);
}


void GenKernFiles(char pre, char *path, ATL_r1node_t *r1b)
/*
 * r1b is a list of rank-1 kernels that must be compiled (including those
 * needed to form SYR and SYR2).  This list should be unique (same kernel
 * not compiled twice).  r1b->kname will have the routine name to give the
 * kernel during compilation.
 */
{
   ATL_r1node_t *r1p;
   char ln[2048];

   r1b = CloneR1Queue(r1b);
   r1b = DelRepeatedR1Kernels(r1b);
   FillInR2ExtractGenStrings(pre, r1b);
   for (r1p = r1b; r1p; r1p = r1p->next)
   {
      if (r1p->genstr)   /* generate kernel if necessary */
      {
         assert(!system(r1p->genstr));
         sprintf(ln, "cp EXTDIR/%s %s/%s.c\n", r1p->rout, path, r1p->kname);
      }
      else
         sprintf(ln, "cp R2CASES/%s %s/%s.c\n", r1p->rout, path, r1p->kname);
      if (system(ln))
      {
         fprintf(stderr, "FAILED: %s\n", ln);
         exit(-1);
      }
   }
   KillAllR1Nodes(r1b);
}

void EmitMakefile(char pre, char *path, ATL_r1node_t *r1b)
/*
 * r1b is a list of kernels that must be compiled (including those
 * needed to form SYR and SYR2).  This list should be unique (same kernel
 * not compiled twice).  r1b->kname will have the routine name to give the
 * kernel during compilation.
 */
{
   ATL_r1node_t *r1p, *r1k;
   char *kern, *outf, *typD;
   FILE *fpout;
   int i, ialias=0, USEGOODGCC=0;
   const char UPRE = (pre == 'z' || pre == 'd') ? 'D' : 'S';
   static char *aliased[16];

   r1b = CloneR1Queue(r1b);  /* get our own copy of kernel Q we can mess up */
   r1b = DelRepeatedR1Kernels(r1b);  /* don't compile same kernel twice */
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
   strcpy(outf+i, "/Make_");   /* Make_<pre>r2 */
   outf[i+6] = pre;
   outf[i+7] = 'r';
   outf[i+8] = '2';
   outf[i+9] = '\0';
   fpout = fopen(outf, "w");
   assert(fpout);

   fprintf(fpout, "#\n#  This file generated at line %d of %s\n#\n",
           __LINE__, __FILE__);
   fprintf(fpout, "include Make.inc\n\n");
   fprintf(fpout, "R2CC = $(%cKC)\nR2FLAGS = $(CDEFS) $(%cKCFLAGS)",
            UPRE, UPRE);
   fprintf(fpout, " -D%s\n\n", typD);
   fprintf(fpout, "obj =");
   for (r1p=r1b; r1p; r1p = r1p->next)
      fprintf(fpout, " %s.o", r1p->kname);
   fprintf(fpout, "\n");

   fprintf(fpout, "lib : %clib\n%clib : %cr2k.grd\n", pre, pre, pre);
   fprintf(fpout, "%cr2k.grd : $(obj)\n", pre);
   fprintf(fpout, "\t$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(obj)\n");
   fprintf(fpout, "\t$(RANLIB) $(ATLASlib)\n");
   fprintf(fpout, "\ttouch %cr2k.grd\n", pre);

   fprintf(fpout, "%cclean : clean\n", pre);
   fprintf(fpout, "clean :\n\t- rm -f $(obj) %cr2k.grd\n\n", pre);

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
         fprintf(fpout, "\t $(R2CC)");
      fprintf(fpout, " -o %s.o -c -DATL_UGER2K=%s", r1p->kname, r1p->kname);
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
            fprintf(fpout, " $(R2FLAGS) -D%s %s", typD, r1p->cflags);
      }
      else
         fprintf(fpout, " $(R2FLAGS)");
      fprintf(fpout, " %s.c\n", r1p->kname);
   }
   KillAllR1Nodes(r1b);  /* done wt our copy of these queues */
   free(outf);
}

void r2khgen(char pre, char *path, ATL_r1node_t *r1b)
{
   char *ln;
   int i;
   FILE *fpout;
   ATL_r1node_t *r1p;
   char *styp, *type = (pre == 'd' || pre == 'z') ? "double" : "float";
   char PRE;
   void PrintPrototypes(char pre, FILE *fpout, ATL_r1node_t *kb);

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
   sprintf(ln, "%s/atlas_%cr2kernels.h", path, pre);

   fpout = fopen(ln, "w");
   assert(fpout);
   fprintf(fpout, "/*\n * This file generated on line %d of %s\n */\n",
           __LINE__, __FILE__);
   fprintf(fpout,
           "#ifndef ATLAS_%cR2KERNELS_H\n   #define ATLAS_%cR2KERNELS_H\n\n",
           PRE, PRE);

   PrintPrototypes(pre, fpout, r1b);
   fprintf(fpout, "\n#endif /* end guard around atlas_%cr2kernels.h */\n", pre);

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

void GenGetKern(char pre, ATL_r1node_t *kb, FILE *fpout)
{
   char *type, *styp;
   ATL_r1node_t *kp;
   int i, minsize;
   char spcs[128], *spc = spcs+127;

   type = Pre2Type(pre);
   styp = Pre2ScalarType(pre);
   minsize = Pre2SizeMin(pre);
   for (i=0; i < 127; i++)
      spcs[i] = ' ';
   spcs[127] = '\0';

   fprintf(fpout, "static ATL_r2kern_t ATL_GetR2Kern\n");
   fprintf(fpout, "   (ATL_CINT M, ATL_CINT N, const void *A, ATL_CINT lda,\n");
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
              FLAG_IS_SET(kp->flag, R1F_ALIGNX2A));
      fprintf(fpout, "%s*FNU = %d;\n", spc, FLAG_IS_SET(kp->flag, R1F_FNU));
      fprintf(fpout, "%s*CacheElts = %d;\n", spc, kp->CacheElts);
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

void PrintPrototypes(char pre, FILE *fpout, ATL_r1node_t *kb)
{
   ATL_r1node_t *kp;
   char *type;

   type = Pre2Type(pre);
   for (kp=kb; kp; kp = kp->next)
   {
      fprintf(fpout, "void %s\n", kp->kname);
      fprintf(fpout,
         "   (ATL_CINT, ATL_CINT, const %s*, const %s*, const %s*,\n",
               type, type, type);
     fprintf(fpout, "    const %s*, %s*, ATL_CINT);\n", type, type);
   }
   fprintf(fpout, "\n");
}

void r2hgen(char pre, char *path, int LVL, ATL_r1node_t *kb)
{
   ATL_r1node_t *kp, *kur;
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
      sprintf(sp, "%s/atlas_%cr2.h", path, pre);
   else
      sprintf(sp, "%s/atlas_%cr2_L%d.h", path, pre, LVL);
   fpout = fopen(sp, "w");
   free(sp);
   type = Pre2Type(pre);
   styp = Pre2ScalarType(pre);

   fprintf(fpout, "#ifndef ATLAS_%cR2_L%d_H\n#define ATLAS_%cR2_L%d_H\n\n",
           PRE, LVL, PRE, LVL);

   fprintf(fpout, "#include \"atlas_type.h\"\n\n");
   fprintf(fpout, "typedef void (*ATL_r2kern_t)\n");
   fprintf(fpout,
      "   (ATL_CINT, ATL_CINT, const %s*, const %s*, const %s*,\n",
           type, type, type);
   fprintf(fpout, "    const %s*, %s*, ATL_CINT);\n", type, type);

   if (TIMECASE)
      kur->kname = DupString("ATL_UGER2K");
   PrintPrototypes(pre, fpout, kb);
   GenGetKern(pre, kb, fpout);
   if (kur->CacheElts)
   {
      mb = (kur->CacheElts - 4*kur->NU) / (2*(kur->NU+1));
      mb = (mb > kur->MU) ? (mb/kur->MU)*kur->MU : 0;
      nb = kur->NU;
   }
   else
      mb = nb = 0;
   fprintf(fpout,
"\n#define ATL_GetPartR2(A_, lda_, mb_, nb_) { (mb_) = %d; (nb_) = %d; }\n",
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
   exit(ierr ? ierr : -1);
}

void GetFlags(int nargs, char **args, char *PRE, char **FNAM, char **DIR,
              int *MU, int *NU, int *L1MUL, int *IFLAG)
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
      sprintf(*FNAM, "res/%cR2K.sum", pre);
   }
}

int main(int nargs, char **args)
{
   ATL_r1node_t *bases[4];
   char *fnam, *path;
   ATL_r1node_t *r1b, *r1p, *r1B;
   int i, mu, nu, l1mul, iflag, L1Elts, dotbased=0;
   char pre;

   GetFlags(nargs, args, &pre, &fnam, &path, &mu, &nu, &l1mul, &iflag);

/*
 * If we just want simple tuning header, no need to read file for details
 */
   if (!fnam)
   {
      r1b = GetR1Node();
      r1b->next = NULL;
      r1b->minM = r1b->MU = mu;
      r1b->minN = r1b->NU = nu;
      r1b->flag = iflag;

      r1b->CacheElts = (l1mul/100.0) * GetL1CacheElts(pre);
      r2hgen(pre, path, -1, r1b);
      exit(0);
   }
/*
 * Otherwise, we should be doing a full-blown install; read in summary file
 */
   r1b = ReadR1File(fnam);
   SetAllR1TypeFlags(pre, r1b);
/*
 * Generate prototype file for all routines
 */
   r2khgen(pre, path, r1b);
/*
 * For each cache level, generate a header file which provides the function
 * returning the best kernel and its parameters
 */
   ATL_R1SplitContexts(r1b, bases, bases+1, bases+2, bases+3);
   r2hgen(pre, path, 0, bases[0]);
   r2hgen(pre, path, 2, bases[1]);
   r2hgen(pre, path, 1, bases[2]);
   r1b = ATL_R1LinkContexts(bases[0], bases[1], bases[2], bases[3]);
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
/*
 * Generate header files for SYR2 and HER2
 */
   L1Elts = GetL1CacheElts(pre);
   for(i=0; i < 3; i++)
      s2hgen(r1b, i, L1Elts, pre, path);
   KillAllR1Nodes(r1b);
   return(0);
}
