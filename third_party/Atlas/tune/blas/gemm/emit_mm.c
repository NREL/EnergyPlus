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
#include <string.h>
#include <assert.h>
#include "atlas_prefetch.h"
#include "atlas_mmparse.h"

static int LD_AT_BOTTOM=0;  /* load $C$ after K-loop, rather than before? */
static int TEMP_TYPE=0; /* type of temp regs: 0-TYPE, 1-double, 2-long double */
#define VERB 1

typedef struct CleanNode CLEANNODE;
struct CleanNode
{                          /* let Nc by dim of cleanup */
   CLEANNODE *next;
   int *NBs;
   int imult;   /* kernel handles all Nc where Nc%imult = 0 */
   int icase;   /* ID of user kernel */
   int fixed;   /* how does kernel handle imult? 0: imult set at run-time, */
                /* 1: must recompile for all Nc, 2: only works for Nc=imult */
   int nb;
   int ncomps;
   char rout[256], CC[256], CCFLAGS[512];
};

#if defined(ATL_SSE1)
   #define ICC_IS_RETARDED
#endif
#ifndef MAX_CASG_KU
   #define MAX_CASG_KU 2
#endif
#define Mmin(x, y) ( (x) > (y) ? (y) : (x) )
#define Mmax(x, y) ( (x) > (y) ? (x) : (y) )
#define ATL_Mlcm(x_, y_, ans_) \
{ \
   if ((x_) >= (y_)) \
   { \
      (ans_) = (x_); \
      while( ((ans_)/(y_))*(y_) != (ans_) ) (ans_) += (x_); \
   } \
   else \
   { \
      (ans_) = (y_); \
      while( ((ans_)/(x_))*(x_) != (ans_) ) (ans_) += (y_); \
   } \
}
#define Mlowcase(C) ( ((C) > 64 && (C) < 91) ? (C) | 32 : (C) )

char PRE='d';
char *TYPE="double";
int MUL=1;

enum CW {CleanM=0, CleanN=1, CleanK=2, CleanNot=3};
enum ATLAS_LOOP_ORDER {AtlasIJK=0, AtlasJIK=1};
typedef void (*KLOOPFUNC)(FILE *, char*, enum ATLAS_LOOP_ORDER,
                          enum ATLAS_TRANS, enum ATLAS_TRANS, int, int, int,
                          int, int, int, int, char*, char*, char*, char*, char*,
                          int, int, int, int, int, int, int, int, int,
                          char*, char*);
#define SAFE_ALPHA -3
void PrintC99Defines(FILE *fpout, char *spc)
{
   fprintf(fpout, "%s#ifndef ATL_RESTRICT\n", spc);
   fprintf(fpout,
      "%s#if defined(__STDC_VERSION__) && (__STDC_VERSION__/100 >= 1999)\n",
           spc);
   fprintf(fpout, "%s   #define ATL_RESTRICT restrict\n", spc);
   fprintf(fpout, "%s#else\n%s   #define ATL_RESTRICT\n%s#endif\n",
           spc, spc, spc);
   fprintf(fpout, "%s#endif\n", spc);
}

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
{
   static char ln[256];
   int pwr2 = GetPower2(N);
   if (N == 1) sprintf(ln, "%s", inc);
   else if (pwr2) sprintf(ln, "((%s) >> %d)", inc, pwr2);
   else sprintf(ln, "((%s) / %d)", inc, N);
   return(ln);
}

char *GetInc(int N, char *inc)
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

void emit_uhead(FILE *fp, char pre, enum CW which, int mb, int nb, int kb,
                int lda, int ldb, int ldc, int beta)
/*
 * if which != CleanNot, ldc is not used, lda is imult ldb is fixed,
 * and ldc is NBs[j]
 */
{
   char *cbet;
   char cwh[3] = {'M', 'N', 'K'};
   int i;

   if (beta == 1) cbet = "1";
   else if (beta == 0) cbet = "0";
   else if (beta == -1) cbet = "n1";
   else cbet = "X";

   if (which == CleanNot)
   {
      fprintf(fp, "#define ATL_USERMM ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1_b%s\n",
              pre, mb, nb, kb, lda, ldb, ldc, cbet);
      fprintf(fp, "#define ATL_USERMM_b1 ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1_b1\n",
              pre, mb, nb, kb, lda, ldb, ldc);
      fprintf(fp, "#define ATL_USERMM_bn1 ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1_bn1\n",
              pre, mb, nb, kb, lda, ldb, ldc);
      fprintf(fp, "#define ATL_USERMM_b0 ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1_b0\n",
              pre, mb, nb, kb, lda, ldb, ldc);
      fprintf(fp, "#define ATL_USERMM_bX ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1_bX\n",
              pre, mb, nb, kb, lda, ldb, ldc);
   }
   else
   {
      fprintf(fp, "#define ATL_USERMM ATL_%cup%cBmm%d_%d_%d_b%s\n",
              pre, cwh[which], ldc, lda, ldb, cbet);
      fprintf(fp, "#define ATL_USERMM_b1 ATL_%cup%cBmm%d_%d_%d_b1\n",
              pre, cwh[which], ldc, lda, ldb);
      fprintf(fp, "#define ATL_USERMM_bn1 ATL_%cup%cBmm%d_%d_%d_bn1\n",
              pre, cwh[which], ldc, lda, ldb);
      fprintf(fp, "#define ATL_USERMM_b0 ATL_%cup%cBmm%d_%d_%d_b0\n",
              pre, cwh[which], ldc, lda, ldb);
      fprintf(fp, "#define ATL_USERMM_bX ATL_%cup%cBmm%d_%d_%d_bX\n",
              pre, cwh[which], ldc, lda, ldb);
   }
   if (beta == -1) fprintf(fp, "#define BETAN1\n");
   else fprintf(fp, "#define BETA%s\n", cbet);
   if (pre == 's') fprintf(fp, "#define SREAL\n");
   else if (pre == 'd') fprintf(fp, "#define DREAL\n");
   else if (pre == 'c') fprintf(fp, "#define SCPLX\n");
   else if (pre == 'z') fprintf(fp, "#define DCPLX\n");
   fprintf(fp, "\n#define MB %d\n#define NB %d\n#define KB %d\n", mb, nb, kb);
   fprintf(fp, "\n#define MBMB %d\n#define NBNB %d\n#define KBKB %d\n",
           mb*mb, nb*nb, kb*kb);
   for (i=2; i <= 8; i++)
      fprintf(fp, "\n#define MB%d %d\n#define NB%d %d\n#define KB%d %d\n\n",
              i, i*mb, i, i*nb, i, i*kb);
}

void emit_head(int NC, FILE *fpout, char pre, int nb, int muladd, int lat,
               int mu, int nu, int ku)
{
   int i, pow2nb;
   char nam[128];
   char upr;

   fprintf(fpout, "#ifndef %cMM_H\n", toupper(pre));
   fprintf(fpout, "   #define %cMM_H\n\n", toupper(pre));
   if (muladd) fprintf(fpout, "   #define ATL_mmMULADD\n");
   else fprintf(fpout, "   #define ATL_mmNOMULADD\n");
   fprintf(fpout, "   #define ATL_mmLAT %d\n", lat);
   fprintf(fpout, "   #define ATL_mmMU  %d\n", mu);
   fprintf(fpout, "   #define ATL_mmNU  %d\n", nu);
   fprintf(fpout, "   #define ATL_mmKU  %d\n", ku);
   fprintf(fpout, "   #define MB %d\n", nb);
   fprintf(fpout, "   #define NB %d\n", nb);
   fprintf(fpout, "   #define KB %d\n", nb);
   fprintf(fpout, "   #define NBNB %d\n", nb*nb);
   fprintf(fpout, "   #define MBNB %d\n", nb*nb);
   fprintf(fpout, "   #define MBKB %d\n", nb*nb);
   fprintf(fpout, "   #define NBKB %d\n", nb*nb);
   fprintf(fpout, "   #define NB2 %d\n", 2*nb);
   fprintf(fpout, "   #define NBNB2 %d\n\n", 2*nb*nb);

   for (i=1,pow2nb=0; i < nb; i <<= 1, pow2nb++);
   if (i == nb)
   {
      fprintf(fpout, "   #define ATL_MulByNB(N_) ((N_) << %d)\n", pow2nb);
      fprintf(fpout, "   #define ATL_DivByNB(N_) ((N_) >> %d)\n", pow2nb);
      fprintf(fpout, "   #define ATL_MulByNBNB(N_) ((N_) << %d)\n", 2*pow2nb);
   }
   else
   {
      fprintf(fpout, "   #define ATL_MulByNB(N_) ((N_) * %d)\n", nb);
      fprintf(fpout, "   #define ATL_DivByNB(N_) ((N_) / %d)\n", nb);
      fprintf(fpout, "   #define ATL_MulByNBNB(N_) ((N_) * %d)\n", nb*nb);
   }
   if (!NC)
   {
      sprintf(nam, "ATL_%cJIK%dx%dx%dTN%dx%dx%d", pre, nb, nb, nb, nb, nb, 0);
      if (pre == 'd' || pre == 's')
      {
         fprintf(fpout, "   #define NBmm %s_a1_b1\n", nam);
         fprintf(fpout, "   #define NBmm_b1 %s_a1_b1\n", nam);
         fprintf(fpout, "   #define NBmm_b0 %s_a1_b0\n", nam);
         fprintf(fpout, "   #define NBmm_bX %s_a1_bX\n", nam);
      }
      else
      {

         fprintf(fpout, "void %s_a1_b0(const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc);\n", nam);
         fprintf(fpout, "void %s_a1_b1(const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc);\n", nam);
         fprintf(fpout, "void %s_a1_bX(const int M, const int N, const int K, const TYPE alpha, const TYPE *A, const int lda, const TYPE *B, const int ldb, const TYPE beta, TYPE *C, const int ldc);\n\n", nam);

         fprintf(fpout, "   #define NBmm_b1(m_, n_, k_, al_, A_, lda_, B_, ldb_, be_, C_, ldc_) \\\n");
         fprintf(fpout, "{ \\\n");
            fprintf(fpout, "   %s_a1_bX(m_, n_, k_, al_, (A_), lda_, (B_), ldb_, ATL_rnone, C_, ldc_); \\\n", nam);
            fprintf(fpout, "   %s_a1_b1(m_, n_, k_, al_, (A_), lda_, (B_)+NBNB, ldb_, ATL_rone, (C_)+1, ldc_); \\\n", nam);
            fprintf(fpout, "   %s_a1_bX(m_, n_, k_, al_, (A_)+NBNB, lda_, (B_)+NBNB, ldb_, ATL_rnone, C_, ldc_); \\\n", nam);
            fprintf(fpout, "   %s_a1_b1(m_, n_, k_, al_, (A_)+NBNB, lda_, (B_), ldb_, ATL_rone, (C_)+1, ldc_); \\\n", nam);
         fprintf(fpout, "   }\n");

         fprintf(fpout, "   #define NBmm_b0(m_, n_, k_, al_, A_, lda_, B_, ldb_, be_, C_, ldc_) \\\n");
         fprintf(fpout, "{ \\\n");
         fprintf(fpout, "   %s_a1_b0(m_, n_, k_, al_, (A_), lda_, (B_), ldb_, ATL_rzero, C_, ldc_); \\\n", nam);
         fprintf(fpout, "   %s_a1_b0(m_, n_, k_, al_, (A_), lda_, (B_)+NBNB, ldb_, ATL_rzero, (C_)+1, ldc_); \\\n", nam);
         fprintf(fpout, "   %s_a1_bX(m_, n_, k_, al_, (A_)+NBNB, lda_, (B_)+NBNB, ldb_, ATL_rnone, C_, ldc_); \\\n", nam);
         fprintf(fpout, "   %s_a1_b1(m_, n_, k_, al_, (A_)+NBNB, lda_, (B_), ldb_, ATL_rone, (C_)+1, ldc_); \\\n", nam);
         fprintf(fpout, "   }\n");

         fprintf(fpout, "   #define NBmm_bX(m_, n_, k_, al_, A_, lda_, B_, ldb_, be_, C_, ldc_) \\\n");
         fprintf(fpout, "{ \\\n");
         fprintf(fpout, "   %s_a1_bX(m_, n_, k_, al_, (A_), lda_, (B_), ldb_, -(be_), C_, ldc_); \\\n", nam);
         fprintf(fpout, "   %s_a1_bX(m_, n_, k_, al_, (A_), lda_, (B_)+NBNB, ldb_, be_, (C_)+1, ldc_); \\\n", nam);
         fprintf(fpout, "   %s_a1_bX(m_, n_, k_, al_, (A_)+NBNB, lda_, (B_)+NBNB, ldb_, ATL_rnone, C_, ldc_); \\\n", nam);
         fprintf(fpout, "   %s_a1_b1(m_, n_, k_, al_, (A_)+NBNB, lda_, (B_), ldb_, ATL_rone, (C_)+1, ldc_); \\\n", nam);
         fprintf(fpout, "   }\n");

         if (pre == 'z') upr = 'd';
         else upr = 's';
         sprintf(nam, "ATL_%cJIK%dx%dx%dTN%dx%dx%d", upr, nb, nb, nb, nb, nb,0);
         fprintf(fpout, "   #define rNBmm_b1 %s_a1_b1\n", nam);
         fprintf(fpout, "   #define rNBmm_b0 %s_a1_b0\n", nam);
         fprintf(fpout, "   #define rNBmm_bX %s_a1_bX\n", nam);
      }
   }

   fprintf(fpout, "\n#endif\n");
}

int GetGoodLat(int muladd, int mu, int nu, int ku, int lat)
{
   int mul=mu*nu*ku, slat, blat;
   if (muladd) return(lat);
   for(slat=lat; mul % slat; slat--);
   for(blat=lat; mul % blat && blat < mul; blat++);
   if (blat-lat > lat-slat || mul%blat) return(slat);
   else return(blat);
}

void opfetch(FILE *fpout,          /* where to print */
             char *spc,            /* indentation string */
             int ifetch, /* number of elts to fetch from memory into regs */
             char *rA,  /* name for register holding elt of inner matrix */
             char *rB,  /* name for register holding elt of outer matrix */
             char *pA,  /* name for pointer to elt of inner matrix */
             char *pB,  /* name for pointer to elt of outer matrix */
             int mu,    /* register blocking for inner matrix */
             int nu,    /* register blocking for outer matrix */
             int offA,  /* offset to first elt of this block of inner matrix */
             int offB,  /* offset to first elt of this block of outer matrix */
             int lda,   /* row stride; if 0, row stride is arbitrary */
             int ldb,   /* row stride; if 0, row stride is arbitrary */
             int mulA,  /* col stride; 1: real, 2: cplx */
             int mulB,  /* col stride; 1: real, 2: cplx */
             int rowA,  /* if 0 : fetch within column, else fetch within row */
             int rowB,  /* if 0 : fetch within column, else fetch within row */
             int *ia0,  /* elt of inner matrix to be fetched */
             int *ib0)  /* elt of inner matrix to be fetched */
/*
 * This routine is used to generate memory-to-register fetches
 * A is inner matrix, fetched first;  B is outer matrix, fetched last
 * Assumes each matrix has either a pointer for each column of accessed, labeled
 * <pA><col#>, or the number of rows is a constant (ldx), and only one pointer,
 * <pA>0.
 */
{
   int ia = *ia0, ib = *ib0, nf = 0;

   if (ia >= mu && ib >= nu) return;

   if (ia == 0 && ib == 0) /* initial fetch, always get 2 */
   {
      assert(ifetch >= 2);
      if (offA) fprintf(fpout, "%s   %s0 = %s0[%d];\n", spc, rA, pA, offA);
      else fprintf(fpout, "%s   %s0 = *%s0;\n", spc, rA, pA);
      if (offB) fprintf(fpout, "%s   %s0 = %s0[%d];\n", spc, rB, pB, offB);
      else fprintf(fpout, "%s   %s0 = *%s0;\n", spc, rB, pB);
      nf = 2;
      ia = ib = 1;
   }
   while ( (nf < ifetch) && (ia < mu || ib < nu) )
   {
      if (ia < mu) /* remaining elts of inner matrix to be fetched */
      {
         if (rowA) /* fetching from row */
         {
            if (lda) fprintf(fpout, "%s   %s%d = %s0[%d];\n",
                              spc, rA, ia, pA, offA+(ia*lda)*mulA);
            else if (offA == 0)
               fprintf(fpout, "%s   %s%d = *%s%d;\n", spc, rA, ia, pA, ia);
            else fprintf(fpout, "%s   %s%d = %s%d[%d];\n", spc, rA, ia, pA,
                         ia, offA);
         }
         else fprintf(fpout, "%s   %s%d = %s0[%d];\n",
                      spc, rA, ia, pA, offA+ia*mulA);
         ia++;
      }
      else  /* after inner matrix fetched, fetch outer matrix */
      {
         if (rowB) /* fetching from row */
         {
            if (ldb) fprintf(fpout, "%s   %s%d = %s0[%d];\n",
                              spc, rB, ib, pB, offB+ib*ldb*mulB);
            else if (offB == 0)
               fprintf(fpout, "%s   %s%d = *%s%d;\n", spc, rB, ib, pB, ib);
            else fprintf(fpout, "%s   %s%d = %s%d[%d];\n", spc, rB, ib, pB,
                         ib, offB);
         }
         else fprintf(fpout, "%s   %s%d = %s0[%d];\n",
                      spc, rB, ib, pB, offB+ib*mulB);
         ib++;
      }
      nf++;
   }
   *ia0 = ia;
   *ib0 = ib;
}

/*
 * incABk : increment A & B pointers or offsets inside K-loop
 */
void incABk(FILE *fpout, char *spc,
            char *pA, char *pB,   /* varnams of pointers to matrices */
            int mu, int nu,       /* unrollings */
            int *offA, int *offB,  /* offsets from ptr to first part of block */
            int lda, int ldb,      /* leading dimensions */
            int mulA, int mulB,    /* col increment: 1 = real, 2 = cplx */
            int incpA, int incpB,  /* Increment ptrs at each K iteration? */
            char *incAk, char *incBk, /* if needed, K-loop inc constant */
            int rowA, int rowB)    /* 0: fetch from col, else fetch from row */
{
   int p;

   if (incpA)
   {
      if (rowA)
      {
         if (mulA == 1)
         {
            if (lda) fprintf(fpout, "%s   %s0++;\n", spc, pA);
            else for(p=0; p < mu; p++)
                    fprintf(fpout, "%s   %s%d++;\n",spc, pA, p);
         }
         else
         {
            if (lda) fprintf(fpout, "%s   %s0 += %d;\n", spc, pA, mulA);
            else for(p=0; p < mu; p++)
                    fprintf(fpout, "%s   %s%d += %d;\n",spc, pA, p, mulA);
         }
      }
      else  /* mu unroll is along a column */
      {
         if (lda) fprintf(fpout, "%s   %s0 += %d;\n", spc, pA, lda*mulA);
         else fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
      }
   }
   else
   {
      if (rowA) (*offA) += mulA;
      else
      {
         assert(lda);
         *offA += mulA*lda;
      }
   }

   if (incpB)
   {
      if (rowB)
      {
         if (mulB == 1)
         {
            if (ldb) fprintf(fpout, "%s   %s0++;\n", spc, pB);
            else for(p=0; p < nu; p++)
                    fprintf(fpout, "%s   %s%d++;\n",spc, pB, p);
         }
         else
         {
            if (ldb) fprintf(fpout, "%s   %s0 += %d;\n", spc, pB, mulB);
            else for(p=0; p < nu; p++)
                    fprintf(fpout, "%s   %s%d += %d;\n",spc, pB, p, mulB);
         }
      }
      else  /* nu unroll is along a row */
      {
         if (ldb) fprintf(fpout, "%s   %s0 += %d;\n", spc, pB, ldb*mulB);
         else fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
      }
   }
   else  /* incrementing offset, not pointers */
   {
      if (rowB) (*offB) += mulB;
      else
      {
         assert(ldb);
         *offB += mulB*ldb;
      }
   }
}

/*
 * regKunroll_ma: K-loop unrolling for combine multiply/add instruction.
 * Unrolls k loop by ku, with mu unroll along inner matrix & nu along outer.
 * If actual inner matrix is B instead of A, pass B's data in A and vice versa,
 * and assign pC[i][j] = rCji.
 */
void regKunroll_ma(FILE *fpout,
                   char *spc,  /* indentation string */
                   enum ATLAS_LOOP_ORDER LoopOrder,
                   int Asg1stC,/* 1: first C update gets =, instead of += */
                   int ifetch, /* number of initial fetches to perform */
                   int nfetch, /* # of fetches to perform for every flop */
                   char *rA,   /* varnam for registers of inner matrix */
                   char *rB,   /* varnam for registers of outer matrix */
                   char *rC,   /* varnam for registers holding C */
                   char *pA,   /* varnam for pointer(s) to inner matrix */
                   char *pB,   /* varnam for pointer(s) to outer matrix */
                   int mu,     /* unrolling along inner loop */
                   int nu,     /* unrolling along outer loop */
                   int ku,     /* unrolling along k-loop (innermost) */
                   int *offA0, /* offset to first elt of this block */
                   int *offB0, /* offset to first elt of this block */
                   int lda,    /* row stride; if 0, row stride is arbitrary */
                   int ldb,    /* row stride; if 0, row stride is arbitrary */
                   int mulA,   /* col stride; 1: real 2: cplx */
                   int mulB,   /* col stride; 1: real 2: cplx */
                   int incpA,  /* Increment A for every K iteration? */
                   int incpB,  /* Increment B for every K iteration? */
                   char *incAk,  /* if !rowA, k-loop increment for ptrs */
                   char *incBk,  /* if !rowB, k-loop increment for ptrs */
                   int rowA, /* if 0, fetch within col, else fetch within row */
                   int rowB) /* if 0, fetch within col, else fetch within row */
{
   int i, j, k, ia=0, ib=0, offA=(*offA0), offB=(*offB0);

   if (LoopOrder == AtlasIJK)
   {
      regKunroll_ma(fpout, spc, AtlasJIK, Asg1stC, ifetch, nfetch, rB, rA, rC,
                    pB, pA, nu, mu, ku, offB0, offA0, ldb, lda, mulB, mulA,
                    incpA, incpB, incBk, incAk, rowB, rowA);
      return;
   }
   for (k=0; k < ku; k++)
   {
      ia = ib = 0;
      opfetch(fpout, spc, ifetch, rA, rB, pA, pB, mu, nu, offA, offB,
              lda, ldb, mulA, mulB, rowA, rowB, &ia, &ib);
      for (j=0; j < nu; j++)
      {
         for (i=0; i < mu; i++)
         {
            if (Asg1stC && !k)
               fprintf(fpout, "%s   %s%d_%d = %s%d * %s%d;\n",
                       spc, rC, i, j, rA, i, rB, j);
            else
               fprintf(fpout, "%s   %s%d_%d += %s%d * %s%d;\n",
                       spc, rC, i, j, rA, i, rB, j);
            opfetch(fpout, spc, nfetch, rA, rB, pA, pB, mu, nu, offA, offB,
                    lda, ldb, mulA, mulB, rowA, rowB, &ia, &ib);
         }
      }
      incABk(fpout, spc, pA, pB, mu, nu, &offA, &offB, lda, ldb, mulA, mulB,
             incpA, incpB, incAk, incBk, rowA, rowB);
   }
}

/*
 * regKloop_ma : Creates a full K-loop, unrolled by ku, assuming M & N
 * unrollings of mu & nu, using combined muladd instruction
 */
void regKloop_ma(FILE *fpout,
                 char *spc,  /* indentation string */
                 enum ATLAS_LOOP_ORDER LoopOrder,
                 enum ATLAS_TRANS TA,
                 enum ATLAS_TRANS TB,
                 int AsgC1, /* if 1, 1st iter does c = instead of c += */
                 int M,     /* if 0, M is arbitrary, else M is len of M-loop */
                 int N,     /* if 0, N is arbitrary, else N is len of N-loop */
                 int K,      /* if 0, K is arbitrary, else K is len of K-loop */
                 int ifetch, /* number of initial fetches to perform */
                 int nfetch, /* # of fetches to perform for every flop */
                 int lat,    /* latency */
                 char *rA,   /* varnam for registers of inner matrix */
                 char *rB,   /* varnam for registers of outer matrix */
                 char *rC,   /* varnam for registers holding C */
                 char *pA,   /* varnam for pointer(s) to inner matrix */
                 char *pB,   /* varnam for pointer(s) to outer matrix */
                 int mu,     /* unrolling along inner loop */
                 int nu,     /* unrolling along outer loop */
                 int ku,     /* unrolling along k-loop (innermost) */
                 int lda,    /* row stride: 0 = unknown */
                 int ldb,    /* row stride: 0 = unknown */
                 int mulA,   /* column stride: 1 = real, 2 = cplx */
                 int mulB,   /* column stride: 1 = real, 2 = cplx */
                 int incpA,  /* Increment A for every K iteration? */
                 int incpB,  /* Increment B for every K iteration? */
                 char *incAk,  /* if !rowA, k-loop increment for ptrs */
                 char *incBk)  /* if !rowB, k-loop increment for ptrs */
{
   char kadj[8];
   int i;
   int rowA, rowB; /* mu/nu elts fetched from row? */
   int offA=0, offB=0;
   char *incAk0, *incBk0;
   if (K) assert (ku*2 <= K || K == ku);  /* need at least one iter. for loop */

   if (TA == AtlasNoTrans) rowA = 0;
   else rowA = 1;
   if (TB == AtlasNoTrans) rowB = 1;
   else rowB = 0;

   if (ku == K) /* loop fully unrolled */
   {
      regKunroll_ma(fpout, spc, LoopOrder, AsgC1, ifetch, nfetch, rA, rB,
                    rC, pA, pB, mu, nu, ku, &offA, &offB, lda, ldb, mulA, mulB,
                    incpA, incpB, incAk, incBk, rowA, rowB);
      if (!incpA)
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
         if (lda == 0 && TA != AtlasNoTrans)
            for (i=1; i < mu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk);
      }
      if (!incpB)
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
         if (ldb == 0 && TB == AtlasNoTrans)
            for (i=1; i < nu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk);
      }
      return;
   }

   if (K)
   {
/*
 *    If need to do C = on 1st iteration, peal 1st ku iterations, and then
 *    do loop with K-ku
 */
      if (AsgC1 && (K > ku) && ku <= MAX_CASG_KU)
      {
         fprintf(fpout,
         "/*\n *%s Peel 1st iter to assign C regs\n */\n", spc);
         regKunroll_ma(fpout, spc, LoopOrder, 1, ifetch, nfetch, rA, rB, rC,
                       pA, pB, mu, nu, ku, &offA, &offB, lda, ldb, mulA, mulB,
                       incpA, incpB, incAk, incBk, rowA, rowB);
         if (!incpA)
         {
            fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
            if (lda == 0 && TA != AtlasNoTrans)
               for (i=1; i < mu; i++)
                  fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk);
         }
         if (!incpB)
         {
            fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
            if (ldb == 0 && TB == AtlasNoTrans)
               for (i=1; i < nu; i++)
                  fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk);
         }
         fprintf(fpout, "/*\n *%s Unpeeled K iterations\n */\n", spc);
         K -= ku;
      }
#ifdef ICC_IS_RETARDED
      if (ku == 1)
         fprintf(fpout,
                 "%s   for (k=0; k < %d; k++) /* easy loop to unroll */\n",
                 spc, K);
      else fprintf(fpout,
                   "%s   for (k=0; k < %d; k++) /* easy loop to unroll */\n",
                   spc, K/ku);
#else
      if (ku == 1)
         fprintf(fpout, "%s   for (k=%d; k; k--) /* easy loop to unroll */\n",
                 spc, K);
      else fprintf(fpout,
                   "%s   for (k=%d; k; k -= %d) /* easy loop to unroll */\n",
                   spc, (K/ku)*ku, ku);
#endif
   }
   else
   {
#ifdef ICC_IS_RETARDED
      if (ku == 1)
         fprintf(fpout,
                 "%s   for (k=0; k < K; k++) /* easy loop to unroll */\n", spc);
      else fprintf(fpout,
                  "%s   for (k=0; k < Kb; k += %d) /* easy loop to unroll */\n",
                   spc, ku);
#else
      if (ku == 1)
         fprintf(fpout, "%s   for (k=K; k; k--) /* easy loop to unroll */\n",
                 spc);
      else fprintf(fpout,
                   "%s   for (k=Kb; k; k -= %d) /* easy loop to unroll */\n",
                   spc, ku);
#endif
   }
   fprintf(fpout, "%s   {\n", spc);
   spc -= 3;
   regKunroll_ma(fpout, spc, LoopOrder, 0, ifetch, nfetch, rA, rB, rC, pA, pB,
                 mu, nu, ku, &offA, &offB, lda, ldb, mulA, mulB, incpA, incpB,
                 incAk, incBk, rowA, rowB);
   if (!incpA) /* need to increment the A pointers ourselves */
   {
      fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
      if (rowA && !lda)
         for (i=1; i < mu; i++)
            fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk);
   }
   if (!incpB) /* need to increment the B pointers ourselves */
   {
      fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
      if (rowB && !ldb)
         for (i=1; i < nu; i++)
            fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk);
   }
   spc += 3;
   fprintf(fpout, "%s   }\n", spc);

/*
 * K-loop cleanup
 */
   if (K) /* known cleanup */
   {
      if (K%ku)
         regKunroll_ma(fpout, spc, LoopOrder, 0, ifetch, nfetch, rA, rB, rC,
                       pA, pB, mu, nu, K%ku, &offA, &offB, lda, ldb, mulA, mulB,
                       incpA, incpB, incAk, incBk, rowA, rowB);
      sprintf(kadj, "%d", K%ku);
   }
   else if (ku > 1) /* cleanup between 0-(ku-1) */
   {
      fprintf(fpout, "%s   switch(k = (K-Kb))\n%s   {\n", spc, spc);
      for (i=1; i < ku; i++)
      {
         fprintf(fpout, "%s   case %d:\n", spc, i);
         regKunroll_ma(fpout, spc-3, LoopOrder, 0, ifetch, nfetch, rA, rB, rC,
                       pA, pB, mu, nu, i, &offA, &offB, lda, ldb, mulA, mulB,
                       incpA, incpB, incAk, incBk, rowA, rowB);
         fprintf(fpout, "%s      break;\n", spc);
      }
      fprintf(fpout, "%s   case 0: ;\n%s   }\n", spc, spc);
      sprintf(kadj, "k");
   }
   if ( (K && K%ku) || ku > 1)
   {
      if (incpA)
      {
         fprintf(fpout, "%s   %s0 -= %s*%s;\n", spc, pA, kadj, incAk);
         if (lda == 0 && TA != AtlasNoTrans)
            for (i=1; i < mu; i++)
               fprintf(fpout, "%s   %s%d -= %s*%s;\n", spc, pA, i, kadj, incAk);
      }
      if (incpB)
      {
         fprintf(fpout, "%s   %s0 -= %s*%s;\n", spc, pB, kadj, incBk);
         if (rowB && !ldb)
            for (i=1; i < nu; i++)
               fprintf(fpout, "%s   %s%d -= %s*%s;\n", spc, pB, i, kadj, incBk);
      }
   }
}

/*
 * regKunroll: K-loop unrolling for separate multiply and add instructions.
 * Unrolls k loop by ku, with mu unroll along inner matrix & nu along outer.
 * If actual inner matrix is B instead of A, pass B's data in A and vice versa,
 * and assign pC[i][j] = rCji.
 * Assumes lat flops of this iteration done previously, lat of next done here,
 * Present flop to be done is (iop,jop) of the mu*nu to be done.
 * assumes ifetch has enough data for all lat operations !!!!
 */
int regKunroll(FILE *fpout,
               char *spc,  /* indentation string */
               enum ATLAS_LOOP_ORDER LoopOrder,
               int ifetch, /* number of initial fetches to perform */
               int nfetch, /* # of fetches to perform for every flop */
               int lat,    /* skew of K-loop */
               int STARTUP,/* 0: not starting pipeline, else doing so */
               int Asg1stC,/* 1: first C update gets =, instead of += */
               char *rA,   /* varnam for registers of inner matrix */
               char *rB,   /* varnam for registers of outer matrix */
               char *rC,   /* varnam for registers of C */
               char *pA,   /* varnam for pointer(s) to inner matrix */
               char *pB,   /* varnam for pointer(s) to outer matrix */
               int mu,     /* unrolling along inner loop */
               int nu,     /* unrolling along outer loop */
               int ku,     /* unrolling along k-loop (innermost) */
               int *offA,  /* offset to first elt of this block */
               int *offB,  /* offset to first elt of this block */
               int lda,    /* row stride; if 0, row stride is arbitrary */
               int ldb,    /* row stride; if 0, row stride is arbitrary */
               int mulA,   /* col stride; 1: real 2: cplx */
               int mulB,   /* col stride; 1: real 2: cplx */
               int incpA,  /* Increment A for every K iteration? */
               int incpB,  /* Increment B for every K iteration? */
               char *incAk,  /* if !rowA, k-loop increment for ptrs */
               char *incBk,  /* if !rowB, k-loop increment for ptrs */
               int rowA, /* if 0, fetch within col, else fetch within row */
               int rowB, /* if 0, fetch within col, else fetch within row */
               int *ia,    /* elt of inner matrix to be fetched */
               int *ib,    /* elt of outer matrix to be fetched */
               int *iop0,   /* 1st operand of inner matrix to use */
               int *jop0)   /* 1st operand of outer matrix to use */
{
   int i, j, k, h=0;
   int iop = *iop0, jop = *jop0;

   if (LoopOrder == AtlasIJK)
      return(regKunroll(fpout, spc, AtlasJIK, ifetch, nfetch, lat, STARTUP,
                        Asg1stC, rB, rA, rC, pB, pA, nu, mu, ku, offB, offA,
                        ldb, lda, mulB, mulA, incpB, incpA, incBk, incAk,
                        rowB, rowA, ib, ia, jop0, iop0));
   if (STARTUP) fprintf(fpout, "/*\n *%s Start pipeline\n */\n", spc);
/*
 * If we have not fetched any data for this iteration yet, do so
 */
   if (STARTUP && *ia == 0 && *ib == 0)
      opfetch(fpout, spc, ifetch, rA, rB, pA, pB, mu, nu, *offA, *offB,
              lda, ldb, mulA, mulB, rowA, rowB, ia, ib);
/*
 * One iteration of the ku-unrolled K loop
 */
   for (k=0; k < ku; k++)
   {
      for (j=0; j < nu; j++)
      {
         for (i=0; i < mu; i++)
         {
            if (!STARTUP)
            {
               if (Asg1stC && !k)
                  fprintf(fpout, "%s   %s%d_%d = m%d;\n", spc, rC, i, j, h);
               else
                  fprintf(fpout, "%s   %s%d_%d += m%d;\n", spc, rC, i, j, h);
            }
            fprintf(fpout, "%s   m%d = %s%d * %s%d;\n",
                    spc, h, rA, iop, rB, jop);
            if (++iop == mu)
            {
               iop = 0;
               if (++jop == nu) /* used all this iteration's data */
               {
                  jop = 0;
                  incABk(fpout, spc, pA, pB, mu, nu, offA, offB, lda, ldb,
                         mulA, mulB, incpA, incpB, incAk, incBk, rowA, rowB);
                  *ia = *ib = 0;
                  opfetch(fpout, spc, ifetch, rA, rB, pA, pB, mu, nu,
                          *offA, *offB, lda, ldb, mulA, mulB, rowA, rowB,
                          ia, ib);
               }
            }
            opfetch(fpout, spc, nfetch, rA, rB, pA, pB, mu, nu, *offA, *offB,
                    lda, ldb, mulA, mulB, rowA, rowB, ia, ib);
            if (++h == lat)
            {
               if (STARTUP)
               {
                  fprintf(fpout, "\n");
                  *iop0 = iop;
                  *jop0 = jop;
                  return(lat);
               }
               h = 0;
            }
         }
      }
   }
   *iop0 = iop;
   *jop0 = jop;
   return(h);
}

/*
 * regKdrain: drains the pipe by explicitly unrolling last K iteration
 */
void regKdrain(FILE *fpout,
               char *spc,  /* indentation string */
               enum ATLAS_LOOP_ORDER LoopOrder,
               int ifetch, /* number of initial fetches to perform */
               int nfetch, /* # of fetches to perform for every flop */
               int lat,    /* skew of K-loop */
               char *rA,   /* varnam for registers of inner matrix */
               char *rB,   /* varnam for registers of outer matrix */
               char *rC,   /* varnam for registers of C */
               char *pA,   /* varnam for pointer(s) to inner matrix */
               char *pB,   /* varnam for pointer(s) to outer matrix */
               int mu,     /* unrolling along inner loop */
               int nu,     /* unrolling along outer loop */
               int ku,     /* unrolling along k-loop (innermost) */
               int *offA,  /* offset to first elt of this block */
               int *offB,  /* offset to first elt of this block */
               int lda,    /* row stride; if 0, row stride is arbitrary */
               int ldb,    /* row stride; if 0, row stride is arbitrary */
               int mulA,   /* col stride; 1: real 2: cplx */
               int mulB,   /* col stride; 1: real 2: cplx */
               int incpA,  /* Increment A for every K iteration? */
               int incpB,  /* Increment B for every K iteration? */
               char *incAk,  /* if !rowA, k-loop increment for ptrs */
               char *incBk,  /* if !rowB, k-loop increment for ptrs */
               int rowA, /* if 0, fetch within col, else fetch within row */
               int rowB, /* if 0, fetch within col, else fetch within row */
               int *ia,    /* elt of inner matrix to be fetched */
               int *ib,    /* elt of outer matrix to be fetched */
               int iop,   /* 1st operand of inner matrix to use */
               int jop,   /* 1st operand of outer matrix to use */
               int h)     /* */
{
   int i, j, k=0;
   int REGFETCH=1;

   if (LoopOrder == AtlasIJK)
   {
      regKdrain(fpout, spc, AtlasJIK, ifetch, nfetch, lat, rB, rA, rC,
                pB, pA, nu, mu, ku, offB, offA, ldb, lda, mulB, mulA,
                incpB, incpA, incBk, incAk, rowB, rowA, ib, ia, jop, iop, h);
      return;
   }
/*
 * Drain part of pipe where we are still doing multiplies.  Once iop and jop
 * reach mu/nu, we stop doing fetches
 */
   fprintf(fpout, "/*\n *%s Drain pipe on last iteration of K-loop\n */\n",
           spc);
   for (j=0; j < nu; j++)
   {
      for (i=0; i < mu; i++)
      {
         fprintf(fpout, "%s   %s%d_%d += m%d;\n", spc, rC, i, j, h);
         if (iop < mu || jop < nu)
         {
            fprintf(fpout, "%s   m%d = %s%d * %s%d;\n",
                    spc, h, rA, iop, rB, jop);
            if (++iop == mu)
            {
               iop = 0;
               if (++jop == nu) /* used all this iteration's data */
               {
                  REGFETCH = 0;
                  iop = mu;
                  incABk(fpout, spc, pA, pB, mu, nu, offA, offB, lda, ldb,
                         mulA, mulB, incpA, incpB, incAk, incBk, rowA, rowB);
               }
            }
         }
         else k++;
         if (++h == lat) h = 0;
         if (REGFETCH)
            opfetch(fpout, spc, nfetch, rA, rB, pA, pB, mu, nu, *offA, *offB,
                    lda, ldb, mulA, mulB, rowA, rowB, ia, ib);
      }
   }
/*
 * Drain last of pipe, where all we do is adds
 */
   while (k < lat)
   {
      for (j=0; j < nu && k < lat; j++)
      {
         for (i=0; i < mu && k < lat; i++, k++)
         {
            fprintf(fpout, "%s   %s%d_%d += m%d;\n", spc, rC, i, j, h);
            if (++h == lat) h = 0;
         }
      }
   }
}
void fetchC(FILE*,char*, enum ATLAS_LOOP_ORDER, int, int, int, int, char*,
            char*, int, int, int, int, int, char*);
/*
 * regKloop : Creates a full K-loop, unrolled by ku, assuming M & N
 * unrollings of mu & nu, using separate muladd instruction
 */
void regKloop(FILE *fpout,
              char *spc,  /* indentation string */
              enum ATLAS_LOOP_ORDER LoopOrder,
              enum ATLAS_TRANS TA,
              enum ATLAS_TRANS TB,
              int AsgC1, /* if 0, 1st iter does cij = not cij += */
              int M,     /* if 0, M is arbitrary, else M is len of M-loop */
              int N,     /* if 0, N is arbitrary, else N is len of N-loop */
              int K,      /* if 0, K is arbitrary, else K is len of K-loop */
              int ifetch, /* number of initial fetches to perform */
              int nfetch, /* # of fetches to perform for every flop */
              int lat,    /* latency */
              char *rA,   /* varnam for registers of inner matrix */
              char *rB,   /* varnam for registers of outer matrix */
              char *rC,   /* varnam for registers holding C */
              char *pA,   /* varnam for pointer(s) to inner matrix */
              char *pB,   /* varnam for pointer(s) to outer matrix */
              int mu,     /* unrolling along inner loop */
              int nu,     /* unrolling along outer loop */
              int ku,     /* unrolling along k-loop (innermost) */
              int lda,    /* row stride: 0 = unknown */
              int ldb,    /* row stride: 0 = unknown */
              int mulA,   /* column stride: 1 = real, 2 = cplx */
              int mulB,   /* column stride: 1 = real, 2 = cplx */
              int incpA,  /* Increment A for every K iteration? */
              int incpB,  /* Increment B for every K iteration? */
              char *incAk,  /* if !rowA, k-loop increment for ptrs */
              char *incBk)  /* if !rowB, k-loop increment for ptrs */
{
   int k, Kb, Kpipe, Kloop, kr;
   int i, j, h;
   int rowA, rowB; /* mu/nu elts fetched from row? */
   int offA=0, offB=0, offA0, offB0;
   int ia=0, ib=0, iop=0, jop=0;
   char incAk0[64], incBk0[64];

   sprintf(incAk0, "%s0", incAk);
   sprintf(incBk0, "%s0", incBk);
   if (K)
   {
      Kb = K - K % ku;
      if (2*lat > K)
      {
         regKloop_ma(fpout, spc, LoopOrder, TA, TB, AsgC1, M, N, K,
                     ifetch, nfetch, lat, rA, rB, rC, pA, pB, mu, nu, ku,
                     lda, ldb, mulA, mulB, incpA, incpB, incAk, incBk);
         return;
      }
      assert (ku*2 <= K || K == ku);  /* need at least one iter. for loop */
   }
   if (K != ku)
   {
      if (K) i = mu*nu*ku;
      else i = mu*nu;
      if (i > lat) assert(i == ((i)/lat)*lat);
      else assert(lat == (lat/i)*i);
   }

   if (TA == AtlasNoTrans) rowA = 0;
   else rowA = 1;
   if (TB == AtlasNoTrans) rowB = 1;
   else rowB = 0;


   if (K == ku)  /* fully unrolled loop */
   {
      regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 1, AsgC1,
                 rA, rB, rC, pA, pB, mu, nu, ku, &offA, &offB, lda, ldb,
                 mulA, mulB, incpA, incpB, incAk, incBk, rowA, rowB,
                 &ia, &ib, &iop, &jop);
      fprintf(fpout, "/*\n *%s Completely unrolled K-loop\n */\n", spc);
      i = ku - (lat/(mu*nu)) - 1;
      h = regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 0, AsgC1,
                     rA, rB, rC, pA, pB, mu, nu, i, &offA, &offB, lda, ldb,
                     mulA, mulB, incpA, incpB, incAk, incBk, rowA, rowB,
                     &ia, &ib, &iop, &jop);
      regKdrain(fpout, spc, LoopOrder, ifetch, nfetch, lat, rA, rB, rC, pA, pB,
                mu, nu, ku, &offA, &offB, lda, ldb, mulA, mulB, incpA, incpB,
                incAk, incBk, rowA, rowB, &ia, &ib, iop, jop, h);
      if (!incpA)
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
         if (lda == 0 && TA != AtlasNoTrans)
            for (i=1; i < mu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk);
      }
      if (!incpB)
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
         if (ldb == 0 && TB == AtlasNoTrans)
            for (i=1; i < nu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk);
      }
   }
   else  /* unknown K != ku */
   {
      Kpipe = lat / (mu*nu) + 1;  /* its done in pipeline startup & drain */
      if (K)
      {
         Kloop = ((K-Kpipe)/ku)*ku;
         if (AsgC1 && ku <= MAX_CASG_KU && Kloop < ku)
         {
            regKloop_ma(fpout, spc, LoopOrder, TA, TB, AsgC1, M, N, K,
                        ifetch, nfetch, lat, rA, rB, rC, pA, pB, mu, nu, ku,
                        lda, ldb, mulA, mulB, incpA, incpB, incAk, incBk);
            return;
         }
         kr = K - Kpipe - Kloop;
      }
      else Kloop = kr = 0;
/*
 *    Issue pre-loop mulpipe fill, and increment ptrs if asked
 */
      offA0 = offA; offB0 = offB;
      regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 1, 0, rA, rB, rC,
                 pA, pB, mu, nu, ku, &offA, &offB, lda, ldb, mulA, mulB,
                 incpA, incpB, incAk0, incBk0, rowA, rowB,
                 &ia, &ib, &iop, &jop);
      k = Kpipe - 1;
      if (!incpA && k) /* need to increment the A pointers ourselves */
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, GetInc(k, incAk0));
         if (rowA && !lda)
            for (i=1; i < mu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i,
                       GetInc(k, incAk0));
      }
      if (!incpB && k) /* need to increment the B pointers ourselves */
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, GetInc(k, incBk0));
         if (rowB && !ldb)
            for (i=1; i < nu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i,
                       GetInc(k, incBk0));
      }
      offA = offA0; offB = offB0;  /* offsets stay as set by pipeline */

      if (AsgC1 && ku <= MAX_CASG_KU)
      {
         fprintf(fpout,
                 "/*\n *%s Peel first %d iterations for C assignment\n */\n",
                 spc, ku);
         if (!K)
         {
            fprintf(fpout, "%s   if (Kloop >= %d)\n%s   {\n", spc, ku, spc);
            spc -= 3;
         }
/*
 *       Peel first iteration of unrolled loop to use C = rather than C +=
 */
         h = regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 0, 1,
                        rA, rB, rC, pA, pB, mu, nu, ku, &offA, &offB, lda, ldb,
                        mulA, mulB, incpA, incpB, incAk, incBk, rowA, rowB,
                        &ia, &ib, &iop, &jop);
         if (!incpA) /* need to increment the A pointers ourselves */
         {
            fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
            if (rowA && !lda)
               for (i=1; i < mu; i++)
                  fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk);
         }
         if (!incpB) /* need to increment the B pointers ourselves */
         {
            fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
            if (rowB && !ldb)
               for (i=1; i < nu; i++)
                  fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk);
         }
         fprintf(fpout, "/*\n *%s Unpeeled K iterations\n */\n", spc);
         if (K)
         {
            if (ku == 1)
              fprintf(fpout,
                      "%s   for (k=0; k < %d; k++) /* easy loop to unroll */\n",
                       spc, Kloop-1);
            else fprintf(fpout,
                 "%s   for (k=0; k < %d; k += %d) /* easy loop to unroll */\n",
                         spc, Kloop-ku, ku);
         }
         else if (ku == 1)
            fprintf(fpout,
            "%s   for (k=1; k < Kloop; k++) /* easy loop to unroll */\n", spc);
         else fprintf(fpout,
              "%s   for (k=%d; k < Kloop; k += %d) /* easy loop to unroll */\n",
                      spc, ku, ku);
         offA = offA0;  offB = offB0;
      }
      else if (K)
      {
#ifdef ICC_IS_RETARDED
         if (ku == 1)
           fprintf(fpout,
                   "%s   for (k=0; k < %d; k++) /* easy loop to unroll */\n",
                    spc, Kloop);
         else fprintf(fpout,
                  "%s   for (k=0; k < %d; k += %d) /* easy loop to unroll */\n",
                      spc, Kloop, ku);
#else
         if (ku == 1)
           fprintf(fpout, "%s   for (k=%d; k; k--) /* easy loop to unroll */\n",
                    spc, Kloop);
         else fprintf(fpout,
                      "%s   for (k=%d; k; k -= %d) /* easy loop to unroll */\n",
                      spc, Kloop, ku);
#endif
      }
      else
      {
#ifdef ICC_IS_RETARDED
         if (ku == 1)
            fprintf(fpout,
            "%s   for (k=0; k < Kloop; k++) /* easy loop to unroll */\n", spc);
         else fprintf(fpout,
              "%s   for (k=0; k < Kloop; k += %d) /* easy loop to unroll */\n",
                      spc, ku);
#else
         if (ku == 1)
            fprintf(fpout,
                    "%s   for (k=Kloop; k; k--) /* easy loop to unroll */\n",
                    spc);
         else fprintf(fpout,
                 "%s   for (k=Kloop; k; k -= %d) /* easy loop to unroll */\n",
                      spc, ku);
#endif
      }
      fprintf(fpout, "%s   {\n", spc);
      spc -= 3;
      h = regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 0, 0,
                     rA, rB, rC, pA, pB, mu, nu, ku, &offA, &offB, lda, ldb,
                     mulA, mulB, incpA, incpB, incAk, incBk, rowA, rowB,
                     &ia, &ib, &iop, &jop);
      if (!incpA) /* need to increment the A pointers ourselves */
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk);
         if (rowA && !lda)
            for (i=1; i < mu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk);
      }
      if (!incpB) /* need to increment the B pointers ourselves */
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk);
         if (rowB && !ldb)
            for (i=1; i < nu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk);
      }
      spc += 3;
      fprintf(fpout, "%s   } /* end K-loop */\n", spc);
      if (!K && AsgC1 && ku <= MAX_CASG_KU)
      {
         spc += 3;
         fprintf(fpout, "%s   }\n", spc);
         fprintf(fpout, "%s   else\n%s   {\n", spc, spc);
         spc -= 3;
         fetchC(fpout, spc, LoopOrder, 0, mu, nu, 0, NULL, rC, 1, 0, 0, 0, 1,
                NULL);
         spc += 3;
         fprintf(fpout, "%s   }\n", spc);
      }
      if (kr)
      {
         offA = offA0;  offB = offB0;
         h = regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 0, 0,
                        rA, rB, rC, pA, pB, mu, nu, kr, &offA, &offB,
                        lda, ldb, mulA, mulB, incpA, incpB, incAk, incBk,
                        rowA, rowB, &ia, &ib, &iop, &jop);
         if (!incpA) /* need to increment the A pointers ourselves */
         {
            fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, GetInc(kr, incAk0));
            if (rowA && !lda)
               for (i=1; i < mu; i++)
                  fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i,
                          GetInc(kr, incAk0));
         }
         if (!incpB) /* need to increment the B pointers ourselves */
         {
            fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, GetInc(kr, incBk0));
            if (rowB && !ldb)
               for (i=1; i < nu; i++)
                  fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i,
                          GetInc(kr, incBk0));
         }
      }
      else if (K == 0 && ku != 1)
      {
         fprintf(fpout, "%s   switch(kr)\n%s   {\n", spc, spc);
         for (i=1; i < ku; i++)
         {
            fprintf(fpout, "%s   case %d:\n", spc, i);
            spc -= 3;
            offA = offA0;  offB = offB0;
            regKunroll(fpout, spc, LoopOrder, ifetch, nfetch, lat, 0, 0,
                       rA, rB, rC, pA, pB, mu, nu, i, &offA, &offB,
                       lda, ldb, mulA, mulB, incpA, incpB, incAk, incBk,
                       rowA, rowB, &ia, &ib, &iop, &jop);
            if (!incpA) /* need to increment the A pointers ourselves */
            {
               fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, GetInc(i, incAk0));
               if (rowA && !lda)
                  for (j=1; j < mu; j++)
                     fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, j,
                             GetInc(i, incAk0));
            }
            if (!incpB) /* need to increment the B pointers ourselves */
            {
               fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, GetInc(i, incBk0));
               if (rowB && !ldb)
                  for (j=1; j < nu; j++)
                     fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, j,
                             GetInc(i, incBk0));
            }
            spc += 3;
            fprintf(fpout, "%s      break;\n", spc);
         }
         fprintf(fpout, "%s   case 0: ;\n%s   }\n", spc, spc);

      }
      offA = offA0;  offB = offB0;
      regKdrain(fpout, spc, LoopOrder, ifetch, nfetch, lat, rA, rB, rC, pA, pB,
                mu, nu, ku, &offA, &offB, lda, ldb, mulA, mulB, incpA, incpB,
                incAk0, incBk0, rowA, rowB, &ia, &ib, iop, jop, h);
      if (!incpA)
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pA, incAk0);
         if (rowA && !lda)
            for (i=1; i < mu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pA, i, incAk0);

      }
      if (!incpB)
      {
         fprintf(fpout, "%s   %s0 += %s;\n", spc, pB, incBk0);
         if (rowB && !ldb)
            for (i=1; i < nu; i++)
               fprintf(fpout, "%s   %s%d += %s;\n", spc, pB, i, incBk0);
      }
   }
}

void DoPrefA_lda0(FILE *fpout, char *spc, char pre, int nprefA, int FIRST)
/*
 * DoPrefA_lda0: issues prefetch instructions to prefetch next block of A
 * when lda is unknown; we call this routine inside N loop, but split 1/2
 * pref inst before M loop, and other half at end of N loop
 */
{
   int i, k, size, npref, stpref;

   size = (pre == 's' || pre == 'c') ? 4 : 8;
/*
 * For small prefetch, do all prefetch at top of loop
 * NOTE: use this always, as splitting doesn't seem to help
 */
   if (nprefA <= 2 || 1)
   {
      if (FIRST)
      {
         for (i=0; i < nprefA; i++)
            fprintf(fpout, "%s   ATL_pfl1R(pfA+%d);\n", spc, i*(ATL_L1LS/size));
         fprintf(fpout, "%s   pfA += lda;\n", spc);
      }
   }
/*
 * If issuing initial prefetch at top of N-loop and we have a bunch
 * of prefetches to do (worth splitting between top and bottom of loop)
 */
   else if (FIRST)
   {
      nprefA = (nprefA+1)/2;
      for (i=0; i < nprefA; i++)
         fprintf(fpout, "%s   ATL_pfl1R(pfA+%d);\n", spc, i*(ATL_L1LS/size));
   }
/*
 * If issuing trailing prefetch at bottom of N-loop
 */
   else
   {
      k = ((nprefA+1)/2)*(ATL_L1LS/size);
      nprefA = nprefA/2;
      for (i=0; i < nprefA; i++)
         fprintf(fpout, "%s   ATL_pfl1R(pfA+%d);\n", spc, k+i*(ATL_L1LS/size));
      fprintf(fpout, "%s   pfA += lda;\n", spc);
   }
}

void DoPrefA(FILE *fpout, char *spc, int nprefA)
/*
 * DoPrefA: issues prefetch instructions to prefetch next block of A;
 * we call this routine inside M & N loop, but outside K-loop
 */
{
   int i;

   if (nprefA < 1) return;
   fprintf(fpout, "%s   ATL_pfl1R(pfA);\n", spc);
   if (nprefA > 1)
      fprintf(fpout, "%s   ATL_pfl1R(pfA+ATL_L1LS);\n", spc);
   for (i=3; i < nprefA; i++)
      fprintf(fpout, "%s   ATL_pfl1R(pfA+%d*ATL_L1LS);\n", spc, i);
   fprintf(fpout, "%s   pfA += incPFA;\n", spc);
}

/*
 * fetchC : fetches mu*nu elts of C, & applies beta
 * If LoopOrder is IJK, we will pass B in as A, so we need to transpose C
 */
void fetchC(FILE *fpout,
            char *spc,
            enum ATLAS_LOOP_ORDER LoopOrder,
            int ForceFetch,  /* fetch C even if beta==0? */
            int mu,          /* unroll of inner loop */
            int nu,          /* unroll of outer loop */
            int offC,    /* offset to start of C */
            char *pC,    /* varnam of pointer to C */
            char *rC,    /* register name for elts of C */
            int mul,     /* stride in elts of C in column (1=real, 2=cplx) */
            int ldc,     /* if 0: use ptrs for cols of C; else row stride */
            int alpha,
            int beta,
            int fetch,   /* 0: just do lame pref, don't actually fetch C */
            char *reg)   /* name of unrelated register for beta */
{
   int i, j;
/*
 * If we aren't fetching C, only thing to do is ForceFetch, and we might
 * as well act as if beta = 0
 */
   if (!fetch)
   {
      if (!ForceFetch) return;
      beta = 0;
   }
   if (ForceFetch && beta == 0) /* lame-ass prefetch on C */
   {
      fprintf(fpout, "/*\n *%s Feeble prefetch of C\n */\n", spc);
      for (j=0; j < nu; j++)
      {
         fprintf(fpout, "%s   %s%d_%d = ", spc, rC, 0, j);
         if (ldc)
         {
            if (offC+j) fprintf(fpout, "%s0[%d];\n", pC, offC+(j*ldc)*mul);
            else fprintf(fpout, "*%s0;\n", pC);
         }
         else
         {
            if (offC) fprintf(fpout, "%s%d[%d];\n", pC, j, offC);
            else fprintf(fpout, "*%s%d;\n", pC, j);
         }
      }
      if (!fetch) return;
   }
   else if ( (beta && alpha != SAFE_ALPHA) )
   {
      if (alpha != 1)
         fprintf(fpout, "%s   %s = BetaAlpha;\n", spc, reg);
      else if (beta != 1 && beta != -1 && beta != 0)
         fprintf(fpout, "%s   %s = beta;\n", spc, reg);
      for (j=0; j < nu; j++)
      {
         for (i=0; i < mu; i++)
         {
            if (LoopOrder == AtlasJIK)
               fprintf(fpout, "%s   %s%d_%d = ", spc, rC, i, j);
            else if (LoopOrder == AtlasIJK)
               fprintf(fpout, "%s   %s%d_%d = ", spc, rC, j, i);
            if (ldc)
            {
               if (offC+i+j)
                  fprintf(fpout, "%s0[%d];\n", pC, offC+(j*ldc+i)*mul);
               else fprintf(fpout, "*%s0;\n", pC);
            }
            else
            {
               if (offC+i) fprintf(fpout, "%s%d[%d];\n", pC, j, offC+i*mul);
               else fprintf(fpout, "*%s%d;\n", pC, j);
            }
            if (beta == -1) fprintf(fpout, "%s   %s%d_%d = -%s%d_%d;\n",
                                    spc, rC, i, j, rC, i, j);
            else if (beta != 1 && beta != 0 || alpha != 1)
               fprintf(fpout, "%s   %s%d_%d *= %s;\n", spc, rC, i, j, reg);
         }
      }
   }
   if (beta == 0 || alpha == SAFE_ALPHA)
   {
      fprintf(fpout, "%s   ", spc);
      for (j=0; j < nu; j++)
         for (i=0; i < mu; i++) fprintf(fpout, "%s%d_%d = ", rC, i, j);
      fprintf(fpout, "0.0;\n");
   }
}

void IncPtrs(FILE *fpout,
             char *spc,
             int np,    /* number of pointers to increment */
             char var,  /* variable: A, B, C */
             char loop) /* which loop: n, m, k */
{
   int i;

   for (i=0; i < np; i++)
      fprintf(fpout, "%s   p%c%d += inc%c%c;\n", spc, var, i, var, loop);
}

void Cass(FILE *fpout,
          char *spc,
          enum ATLAS_LOOP_ORDER LoopOrder,
          int LoadC,   /* 1 load C & apply beta before assignment */
          int mu,      /* unroll of inner loop */
          int nu,      /* unroll of outer loop */
          int alpha,
          int beta,
          int offC,    /* offset to start of C */
          char *rA,    /* register name for elts of A */
          char *rB,    /* register name for elts of B */
          char *pC,    /* varnam of pointer to C */
          char *rC,    /* register name for elts of C */
          int mulC,     /* stride in elts of C in column (1=real, 2=cplx) */
          int ldc,     /* if 0: use ptrs for cols of C; else row stride */
          char *incC)  /* increment for this loop; if NULL don't use */
{
   int i, j;
   char *cp, calpha[32], Cderef[32];
   int AlphaReg = (nu > 1);

   if (!beta) LoadC = 0;  /* never load C if beta == 0.0 */
   if (LoadC && alpha == SAFE_ALPHA) alpha = SAFE_ALPHA - 1;
   if (alpha == SAFE_ALPHA)
   {
      if (LoopOrder == AtlasIJK)
      {
         cp = rA;
         rA = rB;
         rB = cp;
         if (nu > 1) sprintf(calpha, "%s1", rB);
         else sprintf(calpha, "alpha");
      }
      else
      {
         if (nu > 1) sprintf(calpha, "%s1", rB);
         else sprintf(calpha, "alpha");
      }
      fprintf(fpout, "%s   %s0 = beta;\n", spc, rB);
      if (AlphaReg) fprintf(fpout, "%s   %s = alpha;\n", spc, calpha);
      for (j=0; j < nu; j++)
      {
         for (i=0; i < mu; i++)
         {
            fprintf(fpout, "%s   %s%d_%d *= %s;\n", spc, rC, i, j, calpha);
            if (ldc) fprintf(fpout, "%s   %s%d = %s0[%d];\n",
                             spc, rA, i, pC, mulC*(j*ldc+i));
            else fprintf(fpout, "%s   %s%d = %s%d[%d];\n",
                         spc, rA, i, pC, j, i*mulC);
            fprintf(fpout, "%s   %s%d_%d += %s0 * %s%d;\n",
                    spc, rC, i, j, rB, rA, i);
         }
      }
   }
   else if (alpha != 1)
   {
      fprintf(fpout, "%s   %s0 = alpha;\n", spc, rB);
      for (i=0; i < mu; i++)
      {
         for (j=0; j < nu; j++)
         {
            if (LoopOrder == AtlasJIK)
               fprintf(fpout, "%s   %s%d_%d *= %s0;\n", spc, rC, i, j, rB);
            else if (LoopOrder == AtlasIJK)
               fprintf(fpout, "%s   %s%d_%d *= %s0;\n", spc, rC, j, i, rB);
         }
      }
   }
   if (LoadC && beta != 0 && beta != 1)
         fprintf(fpout, "%s   %s0 = beta;\n", spc, rB);
   for (j=0; j < nu; j++)
   {
      for (i=0; i < mu; i++)
      {
         if (ldc)
         {
            if (offC+j+i) sprintf(Cderef, "%s0[%d]", pC, offC+(ldc*j+i)*mulC);
            else sprintf(Cderef, "*%s0", pC);
         }
         else
         {
            if (i) sprintf(Cderef, "%s%d[%d]", pC, j, i*mulC);
            else sprintf(Cderef, "*%s%d", pC, j);
         }
         fprintf(fpout, "%s   %s", spc, Cderef);
         if (LoadC)
         {
            if (beta == 1)
               fprintf(fpout, " += ");
            else if (beta == 0)
               fprintf(fpout, " = ");
            else
               fprintf(fpout, " = %s * %s0 + ", Cderef, rB);
            if (LoopOrder == AtlasJIK)
               fprintf(fpout, "%s%d_%d;\n", rC, i, j);
            else if (LoopOrder == AtlasIJK)
               fprintf(fpout, "%s%d_%d;\n", rC, j, i);
         }
         else
         {
            if (LoopOrder == AtlasJIK)
               fprintf(fpout, " = %s%d_%d;\n", rC, i, j);
            else if (LoopOrder == AtlasIJK)
               fprintf(fpout, " = %s%d_%d;\n", rC, j, i);
         }
      }
   }
}

#define RegCallSeq 1
void CallMM(FILE *fpout, char *spc, char pre, char *loopstr,
            int CleanUp, enum ATLAS_TRANS TA, enum ATLAS_TRANS TB,
            int M, int N, int K, int mu, int nu, int ku, int alpha, int beta,
            int lda, int ldb, int ldc, int Mb, int Nb, int Kb,
            char *cA, char *cB, char *cC, char *cM, char *cN, char *cK)
{
   char cTA='N', cTB='N';

   if (TA == AtlasTrans) cTA = 'T';
   else if (TA == AtlasConjTrans) cTA = 'C';
   if (TB == AtlasTrans) cTB = 'T';
   else if (TB == AtlasConjTrans) cTB = 'C';

   if (CleanUp)
      fprintf(fpout, "%s   ATL_%c%s%dx%dx%d%c%c%dx%dx%d", spc, pre, loopstr,
              M, N, K, cTA, cTB, mu, nu, ku);
   else
      fprintf(fpout, "%s   ATL_%c%s%dx%dx%d%c%c%dx%dx%d", spc, pre, loopstr,
              M, N, K, cTA, cTB, lda, ldb, ldc);

   if (alpha == 1) fprintf(fpout, "_a1");
   else if (alpha == -1) fprintf(fpout, "_an1");
   else if (alpha == SAFE_ALPHA) fprintf(fpout, "_aXX");
   else fprintf(fpout, "_aX");
   if (beta == 1 || beta == 0) fprintf(fpout, "_b%d(", beta);
   else if (beta == -1) fprintf(fpout, "_bn1(");
   else fprintf(fpout, "_bX(");
   if (M) fprintf(fpout, "%d, ", M);
   else fprintf(fpout, "%s, ", cM);
   if (N) fprintf(fpout, "%d, ", N);
   else fprintf(fpout, "%s, ", cN);
   if (K) fprintf(fpout, "%d, ", K);
   else fprintf(fpout, "%s, ", cK);
   if ((alpha != 1 && alpha != -1) || RegCallSeq) fprintf(fpout, "alpha, ");
   fprintf(fpout, "%s, ", cA);
   if (!lda || RegCallSeq) fprintf(fpout, "lda, ");
   fprintf(fpout, "%s, ", cB);
   if (!ldb || RegCallSeq) fprintf(fpout, "ldb, ");
   if ( (beta != 1 && beta != 0 && beta != -1) || RegCallSeq)
      fprintf(fpout, "beta, ");
   fprintf(fpout, "%s", cC);
   if (!ldc || RegCallSeq) fprintf(fpout, ", ldc");
   fprintf(fpout, ");\n");
}

void MMDeclare(FILE *fpout, char *spc, char pre, char *type, char *decmod,
               char *loopstr, enum ATLAS_TRANS TA, enum ATLAS_TRANS TB,
               int M, int N, int K, int mu, int nu, int ku,
               int alpha, int beta, int lda, int ldb, int ldc, int pfA)
{
   char cTA='N', cTB='N';

   if (TA == AtlasTrans) cTA = 'T';
   else if (TA == AtlasConjTrans) cTA = 'C';
   if (TB == AtlasTrans) cTB = 'T';
   else if (TB == AtlasConjTrans) cTB = 'C';
/*
 * For cleanup, put unrolling in name to distinguish from original function.
 * For regular function, encode leading dimensions instead
 */
   if (decmod[0] == '\0')
      fprintf(fpout, "%svoid ATL_%c%s%dx%dx%d%c%c%dx%dx%d",
              spc, pre, loopstr, M, N, K, cTA, cTB, lda, ldb, ldc);
   else
      fprintf(fpout, "%s%svoid ATL_%c%s%dx%dx%d%c%c%dx%dx%d",
              spc, decmod, pre, loopstr, M, N, K, cTA, cTB, mu, nu, ku);
   if (alpha == 1) fprintf(fpout, "_a1");
   else if (alpha == -1) fprintf(fpout, "_an1");
   else if (alpha == SAFE_ALPHA) fprintf(fpout, "_aXX");
   else fprintf(fpout, "_aX");
   if (beta == 1 || beta == 0) fprintf(fpout, "_b%d", beta);
   else if (beta == -1) fprintf(fpout, "_bn1");
   else fprintf(fpout, "_bX");
   fprintf(fpout, "\n   (");
   if (!M || RegCallSeq) fprintf(fpout, "const int M, ");
   if (!N || RegCallSeq) fprintf(fpout, "const int N, ");
   if (!K || RegCallSeq) fprintf(fpout, "const int K, ");
   if ( (alpha != 1 && alpha != -1) || RegCallSeq)
      fprintf(fpout, "const %s alpha, ", type);
   fprintf(fpout, "const %s * ATL_RESTRICT A, ", type);
   if (!lda || RegCallSeq) fprintf(fpout, "const int lda, ");
   fprintf(fpout, "const %s * ATL_RESTRICT B, ", type);
   if (!ldb || RegCallSeq) fprintf(fpout, "const int ldb, ");
   if (beta != 1 && beta != -1 && beta != 0 || RegCallSeq)
      fprintf(fpout, "const %s beta, ", type);
   fprintf(fpout, "%s * ATL_RESTRICT C", type);
   if (!ldc || RegCallSeq) fprintf(fpout, ", const int ldc");
   fprintf(fpout, ")\n");
   fprintf(fpout, "/*\n * matmul with TA=%c, TB=%c, MB=%d, NB=%d, KB=%d, \n",
           cTA, cTB, M, N, K);
   fprintf(fpout, " * lda=%d, ldb=%d, ldc=%d, mu=%d, nu=%d, ku=%d, pf=%d\n",
           lda, ldb, ldc, mu, nu, ku, pfA);
   fprintf(fpout, " * Generated by ATLAS/tune/blas/gemm/emit_mm.c (3.10.2)\n");
   fprintf(fpout, " */\n");
}

static int ncucases=0;
static int cucases[64][7];
/*
 * For every operand involved in a given loop, we pass:
 *  ldx : if 0, use pointer for each column, and moving between columns is
 *              accomplished by incrementing by constant passed in string incX
 *        else: use only 1 pointer, increment by ldx move between columns
 */
void emit_mm(FILE *fpout,
             char *spc,  /* indentation string */
             char pre,
             char *type,
             char *decmod,  /* routine declaration modifier (eg. static) */
             enum ATLAS_LOOP_ORDER LoopOrder,
             enum ATLAS_TRANS TA,
             enum ATLAS_TRANS TB,
             int CleanUp, /* 1 : issue cleanup code, 0: do not */
             int muladd, /* 0: separate mult & add, ELSE: combined muladd */
             int prefA,  /* 0: don't prefetch nxt blk of A */
             int lat,    /* pipeline length */
             int ForceFetch,
             int ifetch, /* number of initial fetches to perform */
             int nfetch, /* # of fetches to perform for every flop */
             int M,      /* if 0, M is arbitrary, else M is len of M-loop */
             int N,      /* if 0, N is arbitrary, else N is len of N-loop */
             int K,      /* if 0, K is arbitrary, else K is len of K-loop */
             int mu,     /* unrolling along inner loop */
             int nu,     /* unrolling along outer loop */
             int ku,     /* unrolling along k-loop (innermost) */
             int mulA,   /* column stride: 1 = real, 2 = cplx */
             int mulB,   /* column stride: 1 = real, 2 = cplx */
             int mulC,   /* column stride: 1 = real, 2 = cplx */
             int lda,    /* row stride: 0 = unknown */
             int ldb,    /* row stride: 0 = unknown */
             int ldc,    /* row stride: 0 = unknown */
             int alpha,
             int beta)

{
   char *lstr[2] = {"IJK", "JIK"};
   char *incAk="incAk", *incBk="incBk";
   char *rA = "rA";   /* varnam for registers holding A */
   char *rB = "rB";   /* varnam for registers of B */
   char *rC = "rC";
   char *pA = "pA";   /* varnam for pointer(s) to A */
   char *pB = "pB";   /* varnam for pointer(s) to B */
   char *pC = "pC";
   char *op, *ip;
   char outmat, innmat, OUTMAT, INNMAT;
   char cA[64], cB[64], cC[64], ctmp[64];
   char ln[64];
   int i, j, k, kuA, kuB, AsgC1, LdBott;
   int size = 8;
   int muC, nuC;
   int Mb, Nb, Kb, Kb0;
   int Aptrs, Bptrs, OuterLoop, OuterTest, InnerLoop, InnerTest, lat2;
   int incpA, incpB;
   int CleanN=0, Cleaning=0;
   int CleaningM=0, CleaningN=0;
   int nprefA = 0, prefA1=prefA, prefA2=0;
   KLOOPFUNC kloop;

   if (pre == 's' || pre == 'c')
     size = 4;
   if (CleanUp) CleaningM = CleaningN = 1;
   cucases[ncucases][0] = mu;
   cucases[ncucases][1] = nu;
   cucases[ncucases][2] = ku;
   cucases[ncucases][3] = LoopOrder;
   cucases[ncucases][4] = M;
   cucases[ncucases][5] = N;
   cucases[ncucases][6] = K;
   ncucases++;
/*
 * Error checks
 */
   assert(mulA == 1 || mulA == 2);
   assert(mulB == 1 || mulB == 2);
   assert(mulC == 1 || mulC == 2);
   assert(K >= 0 && M >= 0 && N >= 0);
   assert(mu > 0 && nu > 0 && ku > 0);
   if (M) if (mu > M) mu = M;
   if (N) if (nu > N) nu = N;
   if (K)
   {
      if (2*ku > K) ku = K;
      if (2*lat > K) muladd = lat = 1;  /* no use in pipelining short loop */
   }
   if (K != ku && !muladd)
   {
      if (K) i = mu*nu*ku;
      else i = mu*nu;
      assert(i == ((i)/lat)*lat);
   }
   if (ifetch == -1) ifetch = mu + nu;
   if (ifetch < mu+nu) assert(nfetch > 0);
/*
 * If there is a discrepency between the number of items fetched by entering
 * the loop, and not entering it (the K=0 case), fix it
 */
   if (K == 0 && muladd == 0 && lat < mu*nu)
      while(ifetch+nfetch*lat < mu+nu) ifetch++;
/*
 * If prefetch is selected, but lda != KB, do prefetch col-at-time
 */
   if (lda != K || K == 0)
   {
      prefA2 = prefA;
      prefA1 = 0;
   }

/*
 * If iterating in the K loop indexes A/B by non compile time constant, we
 * must increment that pointer at each step in the K loop, even if the K loop
 * is unrolled
 */
#if 0
   incpA = (lda == 0 && TA == AtlasNoTrans || ku == 1);
   incpB = (ldb == 0 && TB != AtlasNoTrans || ku == 1);
#else
   incpA = (lda == 0 && TA == AtlasNoTrans);
   incpB = (ldb == 0 && TB != AtlasNoTrans);
#endif
   if (incpA) kuA = 1;
   else kuA = ku;
   if (incpB) kuB = 1;
   else kuB = ku;

   if (muladd) kloop = regKloop_ma;
   else kloop = regKloop;

   PrintC99Defines(fpout, "");
   if (prefA1 || prefA2)
      fprintf(fpout, "#include \"atlas_prefetch.h\"\n\n");

   if (!muladd && !K)
   {
      for (k=0; k < ncucases; k++)
         if (1==cucases[k][0] && 1==cucases[k][1] && 1==cucases[k][2]
             && LoopOrder == cucases[k][3] &&
             cucases[k][4]==M && cucases[k][5]==N && cucases[k][6]==K) break;
      if ( (k == ncucases) ||
           (1==cucases[0][0] && 1==cucases[0][1] && 1==cucases[0][2] &&
            ncucases == 1 && mu*nu <= lat) )
         emit_mm(fpout, spc, pre, type, "static ", LoopOrder, TA, TB, 1, 1,
                 prefA, lat, ForceFetch, ifetch, nfetch, M, N, 0, 1, 1, 1,
                 mulA, mulB, mulC, lda, ldb, ldc, alpha, beta);
   }
   if (N)  /* generate N-cleanup, routine, if known */
   {
      if (N%nu)
      {
         if (K) lat2 = GetGoodLat(muladd, mu, N%nu, ku, lat);
         else lat2 = GetGoodLat(muladd, mu, N%nu, 1, lat);
         CleanN = Cleaning = 1;
         emit_mm(fpout, spc, pre, type, "static ", LoopOrder, TA, TB, 1, muladd,
                 prefA, lat2, ForceFetch, ifetch, nfetch, M, N%nu, K,
                 mu, N%nu, ku, mulA, mulB, mulC, lda, ldb, ldc, alpha, beta);
      }
   }
   else if (nu > 1 && CleaningN) /* unknown N */
   {
      if (K) lat2 = GetGoodLat(muladd, mu, 1, ku, lat);
      else lat2 = GetGoodLat(muladd, mu, 1, 1, lat);
      CleanN = Cleaning = 1;
      for (k=0; k < ncucases; k++)
         if (mu==cucases[k][0] && 1==cucases[k][1] && ku==cucases[k][2]
             && LoopOrder == cucases[k][3] &&
             cucases[k][4]==M && cucases[k][5]==N && cucases[k][6]==K) break;
      if (k == ncucases)
         emit_mm(fpout, spc, pre, type, "static ", LoopOrder, TA, TB, 1, muladd,
                 prefA, lat2, ForceFetch, ifetch, nfetch, M, 0, K, mu, 1, ku,
                 mulA, mulB, mulC, lda, ldb, ldc, alpha, beta);
   }
   if (CleanN) i = N - N % nu;
   else i = N;
   if (M)
   {
      if (M%mu)
      {
         if (K) lat2 = GetGoodLat(muladd, M%mu, nu, ku, lat);
         else lat2 = GetGoodLat(muladd, M%mu, nu, 1, lat);
         Cleaning = 1;
         emit_mm(fpout, spc, pre, type, "static ", LoopOrder, TA, TB, !CleanN,
                 muladd, prefA, lat2, ForceFetch, ifetch, nfetch, M%mu, i, K,
                 M%mu, nu, ku, mulA, mulB, mulC, lda, ldb, ldc, alpha, beta);
      }
   }
   else if (mu > 1 && CleaningM)
   {
      Cleaning = 1;
      if (K) lat2 = GetGoodLat(muladd, 1, nu, ku, lat);
      else lat2 = GetGoodLat(muladd, 1, nu, 1, lat);
      for (k=0; k < ncucases; k++)
         if (1==cucases[k][0] && nu==cucases[k][1] && ku==cucases[k][2]
             && LoopOrder == cucases[k][3] &&
             cucases[k][4]==M && cucases[k][5]==N && cucases[k][6]==K) break;
      if (k == ncucases)
         emit_mm(fpout, spc, pre, type, "static ", LoopOrder, TA, TB, 1,
                 muladd, prefA, lat2, ForceFetch, ifetch, nfetch, 0, i, K, 1,
                 nu, ku, mulA, mulB, mulC, lda, ldb, ldc, alpha, beta);
   }
/*
 * Name : ATL_<pre><IJK/JIK><MB>x<NB>x<KB>mm<mu>x<nu>x<ku>_aX_bX
 *        ATL_dJIK40x40x40mm4x4x1_a1_b1
 */
   MMDeclare(fpout, spc, pre, type, decmod, lstr[LoopOrder], TA, TB, M, N, K,
             mu, nu, ku, alpha, beta, lda, ldb, ldc, prefA);
   fprintf(fpout, "{\n");

   if (LoopOrder == AtlasJIK)
   {
      muC = mu;
      nuC = nu;
      outmat = 'n';
      innmat  = 'm';
   }
   else
   {
      muC = nu;
      nuC = mu;
      outmat = 'm';
      innmat  = 'n';
   }
/*
 * Figure dimension size, in multiples of blocking factor
 */
   if (!M)
   {
      if (mu == 1) fprintf(fpout, "   #define Mb M\n");
      else
      {
         i = GetPower2(mu);
         if (i) fprintf(fpout, "   const int Mb = (M>>%d)<<%d;\n", i, i);
         else fprintf(fpout, "   const int Mb = (M/%d)*%d;\n", mu, mu);
      }
      Mb = 0;
   }
   else Mb = M - M%mu;
   if (!N)
   {
      if (nu == 1) fprintf(fpout, "   #define Nb N\n");
      else
      {
         i = GetPower2(nu);
         if (i) fprintf(fpout, "   const int Nb = (N>>%d)<<%d;\n", i, i);
         else fprintf(fpout, "   const int Nb = (N/%d)*%d;\n", nu, nu);
      }
      Nb = 0;
   }
   else Nb = N - N%nu;
   if (!K)
   {
      Kb = 0;
      i = GetPower2(ku);
      if (!muladd)
      {
         fprintf(fpout, "   #define Kb K\n");
         j = lat/(mu*nu) + 1;
         if (ku == 1)
            fprintf(fpout, "   const int Kloop = K - %d;\n", j);
         else
         {
            if (ku == 1)
            {
               fprintf(fpout, "   const int Kloop = K-%d;\n", j);
               fprintf(fpout, "   #define kr 0\n");
            }
            else
            {
               if (i) fprintf(fpout,
                              "   const int Kloop = ((K-%d)>>%d)<<%d", j, i, i);
               else fprintf(fpout,
                              "   const int Kloop = ((K-%d)/%d)*%d", j, ku, ku);
               fprintf(fpout, ", kr = K - Kloop - %d;\n", j);
            }
         }
      }
      else
      {
         if (ku == 1) fprintf(fpout, "   #define Kb K\n");
         else
         {
            if (i) fprintf(fpout, "   const int Kb = (K>>%d)<<%d;\n", i, i);
            else fprintf(fpout, "   const int Kb = (K/%d)*%d;\n", ku, ku);
         }
      }
   }
   else Kb = K - K%ku;
   Kb0 = Kb;
   if (!muladd) Kb = K;

   if (Cleaning)
      fprintf(fpout, "   const %s *ca=A, *cb=B;\n   %s *cc=C;\n", type, type);
/*
 * Figure loop boundary tests, stM & stN
 */
   if (TA == AtlasNoTrans)
   {
      if (M) fprintf(fpout, "   const %s *stM = A + %d;\n",
                     type, mulA*Mb);
      else   fprintf(fpout, "   const %s *stM = A + %s;\n",
                     type, GetInc(mulA, "Mb"));
   }
   else
   {
      if (M)
      {
         if (lda) fprintf(fpout, "   const %s *stM = A + %d;\n",
                          type, Mb*lda*mulA);
         else fprintf(fpout, "   const %s *stM = A + %s;\n",
                      type, GetInc(Mb*mulA, "lda"));
      }
      else if (lda) fprintf(fpout, "   const %s *stM = A + %s;\n",
                            type, GetInc(mulA*lda, "Mb"));
      else fprintf(fpout, "   const %s *stM = A + %s;\n",
                   type, GetInc(mulA, "lda*Mb"));
   }
   if (TB == AtlasNoTrans)
   {
      if (N)
      {
         if (ldb) fprintf(fpout, "   const %s *stN = B + %d;\n",
                          type, Nb*ldb*mulB);
         else fprintf(fpout, "   const %s *stN = B + %s;\n",
                      type, GetInc(Nb*mulB, "ldb"));
      }
      else if (ldb) fprintf(fpout, "   const %s *stN = B + %s;\n",
                            type, GetInc(mulB*ldb, "Nb"));
      else fprintf(fpout, "   const %s *stN = B + %s;\n",
                   type, GetInc(mulB, "ldb*Nb"));
   }
   else
   {
      if (N) fprintf(fpout, "   const %s *stN = B + %d;\n",
                     type, mulB*Nb);
      else   fprintf(fpout, "   const %s *stN = B + %s;\n",
                     type, GetInc(mulB, "Nb"));
   }
   if (prefA1)
   {
      fprintf(fpout, "   const %s *pfA = stM;\n", type);
      if (N && M)
      {
         fprintf(fpout,
      "   const int incPFA0 = (((int)(stM - A))*%d*%d)/(%d*%d);\n",
                 mu, nu, M, N);
      }
      else
      {
         if (M)
            fprintf(fpout,
         "   const int incPFA0 = (((int)(stM - A))*%d*%d)/(%d*N*sizeof(%s));\n",
                    mu, nu, M, type);
         else
            fprintf(fpout,
         "   const int incPFA0 = (((int)(stM - A))*%d*%d)/(M*N*sizeof(%s));\n",
                    mu, nu, type);
      }
      fprintf(fpout, "   const int incPFA = (1 > incPFA0) ? 1 : incPFA0;\n");
/*
 *    Figure number of prefetch instructions that must be issued:
 *       (MB*KB*size)/ [ linesize*(MB*NB)/(mu*nu) ]
 *     = (KB*size*mu*nu) / (linesize*NB)
 *    ~= (size*mu*nu)/(linesize)
 */
      if (K && N) nprefA = (K*mu*nu*size)/(N*ATL_L1LS);
      else nprefA = (mu*nu*size)/ATL_L1LS;
      if (nprefA < 1) nprefA = 1;
   }
   else if (prefA2)
   {
      if (TA == AtlasTrans || TA == AtlasConjTrans)
         fprintf(fpout, "   const %s *pfA = A + M;\n", type);
      else
         fprintf(fpout, "   const %s *pfA = A + lda*M;\n", type);

/*
 *    True # of items to fetch is K; if it's not set, try M/N (usually square)
 *    If none set, only do 2 prefetch/col (hardware prefetch may get rest)
 */
      if (K)
         nprefA = (K*size) / ATL_L1LS;
      else if (M)
         nprefA = (M*size) / ATL_L1LS;
      else if (N)
         nprefA = (N*size) / ATL_L1LS;
      else
      {
         nprefA = (ku*size) / ATL_L1LS;
         if (nprefA < 2) nprefA = 2;
      }
   }
   if (alpha != SAFE_ALPHA && alpha != 1)
      fprintf(fpout, "   const %s BetaAlpha = beta / alpha;\n", type);

/*
 * Figure A & B's increments for all three loops
 */
   if (LoopOrder == AtlasJIK)
   {
      if (TA == AtlasNoTrans)
      {
         if (lda) fprintf(fpout, "   #define incAk %d\n", mulA*lda*kuA);
         else fprintf(fpout, "   const int incAk = %s;\n",
                      GetInc(kuA*mulA, "lda"));
         if (K)
         {
            if (lda) fprintf(fpout, "   const int incAm = %d - %d",
                             mulA*mu, Kb*mulA*lda);
            else fprintf(fpout, "   const int incAm = %d - %s", mulA*mu,
                         GetInc(Kb*mulA, "lda"));
         }
         else fprintf(fpout, "   const int incAm = %d - %s", mulA*mu,
                      GetInc(mulA, "Kb*lda"));
         if (M) fprintf(fpout, ", incAn = -%d;\n", mulA*Mb);
         else fprintf(fpout, ", incAn = -%s;\n", GetInc(mulA, "Mb"));
      }
      else
      {
         fprintf(fpout, "   #define incAk %d\n", kuA*mulA);
         if (K)
         {
            if (lda)
               fprintf(fpout, "   const int incAm = %d", mulA*(lda*mu - Kb));
            else
            {
               sprintf(ln, "%s - %d", GetInc(mu, "lda"), Kb);
               fprintf(fpout, "   const int incAm = %s", GetInc(mulA, ln));
            }
         }
         else
         {
            if (lda) fprintf(fpout, "   const int incAm = %d - %s", mulA*lda*mu,
                             GetInc(mulA,"Kb"));
            else
            {
               sprintf(ln, "%s - Kb", GetInc(mu, "lda"));
               fprintf(fpout, "   const int incAm = %s", GetInc(mulA, ln));
            }
         }
         if (M)
         {
            if (lda) fprintf(fpout, ", incAn = -%d;\n", mulA*lda*Mb);
            else fprintf(fpout, ", incAn = -%s;\n", GetInc(mulA*Mb, "lda"));
         }
         else if (lda)
            fprintf(fpout, ", incAn = -%s;\n", GetInc(mulA*lda,"Mb"));
         else fprintf(fpout, ", incAn = -%s;\n", GetInc(mulA, "Mb*lda"));
      }
      if (TB == AtlasNoTrans)
      {
         fprintf(fpout, "   #define incBk %d\n", mulB*kuB);
         if (K) fprintf(fpout, "   const int incBm = -%d", mulB*Kb);
         else fprintf(fpout, "   const int incBm = -%s", GetInc(mulB, "Kb"));
         if (ldb) fprintf(fpout, ", incBn = %d;\n", ldb*nu*mulB);
         else fprintf(fpout, ", incBn = %s;\n", GetInc(nu*mulB, "ldb"));
      }
      else
      {
         if (ldb)
         {
            fprintf(fpout, "   const int incBk = %d", mulB*kuB*ldb);
            if (K) fprintf(fpout, ", incBm = -%d", mulB*ldb*Kb);
            else fprintf(fpout, ", incBm = -%s", GetInc(mulB*ldb, "Kb"));
         }
         else
         {
            fprintf(fpout, "   const int incBk = %s", GetInc(mulB*kuB, "ldb"));
            if (K) fprintf(fpout, ", incBm = -%s", GetInc(mulB*Kb, "ldb"));
            else fprintf(fpout, ", incBm = -%s", GetInc(mulB, "Kb*ldb"));
         }
         fprintf(fpout, ";\n   #define incBn %d\n", nu*mulB);
      }
   }
   else if (LoopOrder == AtlasIJK)
   {
      if (TA == AtlasNoTrans)
      {
         if (lda)
         {
            fprintf(fpout, "   const int incAk = %d", lda*kuA*mulA);
            if (K) fprintf(fpout, ", incAn = -%d", mulA*lda*Kb);
            else fprintf(fpout, ", incAn = -%s", GetInc(mulA*lda, "Kb"));
         }
         else
         {
            fprintf(fpout, "   const int incAk = %s", GetInc(kuA*mulA, "lda"));
            if (K) fprintf(fpout, ", incAn = -%s", GetInc(mulA*Kb, "lda"));
            else fprintf(fpout, ", incAn = -%s", GetInc(mulA, "lda*Kb"));
         }
         fprintf(fpout, ";\n   #define incAm %d\n", mu*mulA);
      }
      else
      {
         fprintf(fpout, "   #define incAk %d\n", kuA*mulA);
         if (K) fprintf(fpout, "   const int incAn = -%d", Kb*mulA);
         else fprintf(fpout, "   const int incAn = -%s", GetInc(mulA, "Kb"));
         if (lda) fprintf(fpout, ";\n   #define incAm %d\n", mulA*lda*mu);
         else fprintf(fpout, ", incAm = %s;\n", GetInc(mulA*mu, "lda"));
      }
      if (TB == AtlasNoTrans)
      {
         fprintf(fpout, "   #define incBk %d\n", mulB*kuB);
         if (ldb)
         {
            if (K) fprintf(fpout, "   #define incBn %d;\n", mulB*(nu*ldb-Kb));
            else fprintf(fpout, "   const int incBn = %d - %s;\n",
                         mulB*nu*ldb, GetInc(mulB, "Kb"));
            if (N) fprintf(fpout, "   #define incBm -%d\n", mulB*ldb*Nb);
            else fprintf(fpout, "   const int incBm = -%s;\n",
                         GetInc(mulB*ldb, "Nb"));
         }
         else
         {
            if (K) fprintf(fpout, "   const int incBn = %s - %d",
                           GetInc(mulB*nu, "ldb"), mulB*Kb);
            else
            {
               fprintf(fpout, "   const int incBn = %s - ",
                       GetInc(mulB*nu, "ldb"));
               fprintf(fpout, "%s",  GetInc(mulB*nu, "Kb"));
            }
            if (N) fprintf(fpout, ", incBm = -%s;\n", GetInc(mulB*Nb, "ldb"));
            else fprintf(fpout, ", incBm = -%s;\n",
                         GetInc(mulB, "Nb*ldb"));
         }
      }
      else
      {
         if (ldb)
         {
            fprintf(fpout, "   #define incBk %d\n", mulB*kuB*ldb);
            if (K) fprintf(fpout, "   const int incBn = %d", mulB*(nu-ldb*Kb));
            else fprintf(fpout, "   const int incBn = %d - %s",
                         mulB*nu, GetInc(mulB*ldb, "Kb"));
         }
         else
         {
            fprintf(fpout, "   const int incBk = %s",
                      GetInc(mulB*kuB, "ldb"));
            if (K) fprintf(fpout, ", incBn = %d - %s",
                           mulB*nu, GetInc(mulB*Kb, "ldb"));
            else fprintf(fpout, ", incBn = %d - %s",
                         mulB*nu, GetInc(mulB, "Kb*ldb"));
         }
         if (N) fprintf(fpout, ";\n   #define incBm -%d\n", mulB*Nb);
         else fprintf(fpout, ", incBm = %s;\n", GetInc(mulB, "-Nb"));
      }
   }
   if (!muladd)
   {
      if (ku == 1)
         fprintf(fpout, "   #define incAk0 incAk\n   #define incBk0 incBk\n");
      else
      {
         if (!incpA)
         {
            fprintf(fpout, "   const int incAk0 = %s", GetDiv(ku, "incAk"));
            if (!incpB) fprintf(fpout, ", incBk0 = %s;\n", GetDiv(ku, "incBk"));
            else fprintf(fpout, ";\n   #define incBk0 incBk\n");
         }
         else
         {
            fprintf(fpout, "   #define incAk0 incAk\n");
            if (!incpB) fprintf(fpout, "   const int incBk0 = %s;\n",
                                GetDiv(ku, "incBk"));
            else fprintf(fpout, "   #define incBk0 incBk\n");
         }
      }
   }
   if (LoopOrder == AtlasJIK)
   {
      fprintf(fpout, "   #define incCm %d\n", mulC*mu);
      if (ldc)
      {
         if (M) fprintf(fpout, "   #define incCn %d\n", mulC*(ldc*nu-Mb));
         else fprintf(fpout, "   const int incCn = %d - %s;\n",
                      mulC*ldc*nu, GetInc(mulC,"Mb"));
      }
      else
      {
         fprintf(fpout, "   const int incCn = %s", GetInc(mulC*nu, "ldc"));
         if (M)
         {
            if (Mb) fprintf(fpout, " - %d;\n", mulC*Mb);
            else fprintf(fpout, ";\n");
         }
         else fprintf(fpout, " - %s;\n", GetInc(mulC, "Mb"));
      }
   }
   else if (LoopOrder == AtlasIJK)
   {
      if (ldc)
      {
         fprintf(fpout, "   #define incCn %d\n", mulC*nu*ldc);
         if (N) fprintf(fpout, "   #define incCm %d\n", mulC*(mu - Nb*ldc));
         else fprintf(fpout, "   const int incCm = %d - %s;\n",
                      mulC*mu, GetInc(mulC*ldc, "Nb"));
      }
      else
      {
         fprintf(fpout, "   const int incCn = %s", GetInc(mulC*nu, "ldc"));
         fprintf(fpout, ", incCm = %d", mulC*mu);
         if (N)
         {
            if (Nb) fprintf(fpout, " - %s;\n", GetInc(mulC*Nb, "ldc"));
            else fprintf(fpout, ";\n");
         }
         else fprintf(fpout, " - %s;\n", GetInc(mulC, "ldc*Nb"));
      }
   }
/*
 * Need to get mu[nu] ptrs when A [B] is accessed within a row inside the
 * K-loop, and the row-stride (ldx) is not known
 */
   Aptrs = ( (lda == 0) && (TA != AtlasNoTrans) );
   Bptrs = ( (ldb == 0) && (TB == AtlasNoTrans) );
/*
 * Setup necessary pointers
 */
   fprintf(fpout, "   %s *%s0=C", type, pC);
   if (!ldc)  /* need extra pointers if row stride is unknown */
      for (j=1; j < nu; j++)
         fprintf(fpout, ", *%s%d=%s%d+%s", pC, j, pC, j-1, GetInc(mulC,"ldc"));
   fprintf(fpout, ";\n");
   fprintf(fpout, "   const %s *%s0=A", type, pA);

   if (Aptrs)
      for (j=1; j < mu; j++)
         fprintf(fpout, ", *%s%d=%s%d+%s", pA, j, pA, j-1, GetInc(mulA,"lda"));
   fprintf(fpout, ";\n");
   fprintf(fpout, "   const %s *%s0=B", type, pB);
   if (Bptrs)
      for (j=1; j < nu; j++)
         fprintf(fpout, ", *%s%d=%s%d+%s", pB, j, pB, j-1, GetInc(mulB,"ldb"));
   fprintf(fpout, ";\n");

   fprintf(fpout, "   register int k;\n");

   fprintf(fpout, "   register %s %s0", type, rA);
   for (j=1; j < mu; j++) fprintf(fpout, ", %s%d", rA, j);
   fprintf(fpout, ";\n");

   fprintf(fpout, "   register %s %s0", type, rB);
   for (j=1; j < nu; j++) fprintf(fpout, ", %s%d", rB, j);
   fprintf(fpout, ";\n");

   if (!muladd && lat)
   {
      if (TEMP_TYPE == 1)
         fprintf(fpout, "   register double m0");
      else if (TEMP_TYPE == 2)
         fprintf(fpout, "   register long double m0");
      else
         fprintf(fpout, "   register %s m0", type);
      for (j=1; j < lat; j++) fprintf(fpout, ", m%d", j);
      fprintf(fpout, ";\n");
   }

   for (j=0; j < nuC; j++)
   {
      for (i=0; i < muC; i++)
      {
         if (i || j) fprintf(fpout, ", %s%d_%d", rC, i, j);
         else if (TEMP_TYPE == 1)
            fprintf(fpout, "   register double %s0_0", rC);
         else if (TEMP_TYPE == 2)
            fprintf(fpout, "   register long double %s0_0", rC);
         else
            fprintf(fpout, "   register %s %s0_0", type, rC);
      }
   }
   fprintf(fpout, ";\n");
/*
 * See which loops exist
 */
   if (LoopOrder == AtlasJIK)
   {
      OuterLoop = (nu != N);
      OuterTest = (N == 0) && (nu > 1) && CleaningN;
      InnerLoop = (mu != M);
      InnerTest = (M == 0) && (mu > 1) && CleaningM;
      outmat = 'n';
      OUTMAT = 'N';
      innmat  = 'm';
      INNMAT  = 'M';
      op = pB;
      ip = pA;
   }
   else if (LoopOrder == AtlasIJK)
   {
      OuterLoop = (mu != M);
      OuterTest = (M == 0) && (mu > 1) && CleaningM;
      InnerLoop = (nu != N);
      InnerTest = (N == 0) && (nu > 1) && CleaningN;
      outmat = 'm';
      OUTMAT = 'M';
      innmat  = 'n';
      INNMAT  = 'N';
      op = pA;
      ip = pB;
   }

   if (!muladd && !K && mu*nu <= lat)
   {
      k = 2 + lat/(mu*nu);
      fprintf(fpout, "%s   if (K < %d)\n%s   {\n", spc, k, spc);
      CallMM(fpout, spc-3, pre, lstr[LoopOrder], 1, TA, TB, M, N, K,
             1, 1, 1, alpha, beta, lda, ldb, ldc, Mb, Nb, Kb0,
             "A", "B", "C", "M", "N", "K");
      fprintf(fpout, "%s      return;\n%s   }\n", spc, spc);
   }
   if (InnerLoop && InnerTest)
   {
      fprintf(fpout, "%s   if (%s0 != st%c)\n%s   {\n",
              spc, ip, INNMAT, spc);
      spc -= 3;
   }
   if (OuterLoop)
   {
      if (OuterTest)
      {
         fprintf(fpout, "%s   if (%s0 != st%c)\n%s   {\n",
                 spc, op, OUTMAT, spc);
         spc -= 3;
      }
      fprintf(fpout, "%s   do /* %c-loop */\n%s   {\n", spc, OUTMAT, spc);
      spc -= 3;
      if (prefA2)
         DoPrefA_lda0(fpout, spc, pre, nprefA, 1);
   }
   if (InnerLoop)
   {
      fprintf(fpout, "%s   do /* %c-loop */\n%s   {\n", spc, INNMAT, spc);
      spc -= 3;
   }

/*
 * If beta = 0, or we are doing load-at-bottom gemm, we set AsgC1, indicating
 * first iteration of C assignment will be of form Cij = rather than Cij +=
 * NOTE: presently not supported for combined muladd when K is unknown;
 *       to avoid code bloat, we don't do it if ku > MAX_CASG_KU
 */
   LdBott = LD_AT_BOTTOM;
   AsgC1 = 0;
   if (!beta || LdBott)
      AsgC1 = (!muladd || K) && (ku == K || (ku <= MAX_CASG_KU));

   if (prefA1)
      DoPrefA(fpout, spc, nprefA);
   sprintf(ln, "%s0", rA);
   fetchC(fpout, spc, LoopOrder, ForceFetch, mu, nu, 0, pC, rC, mulC, ldc,
           alpha, (LdBott && !AsgC1) ? 0 : beta, !AsgC1, ln);
/*
 * If prefetch is on, prefetch cols of C if we don't access them at top-of-loop
 * NOTE: need to time and see if this slows more people down than it speeds
 *       up, maybe make tuning parameter.
 */
   if (prefA && AsgC1)
   {
      for (j=0; j < nu; j++)
      {
         if (ldc)
            fprintf(fpout, "%s   ATL_pfl1W(%s0+%d);\n", spc, pC, j*ldc*mulC);
         else
            fprintf(fpout, "%s   ATL_pfl1W(%s%d);\n", spc, pC, j);
      }
   }
   kloop(fpout, spc, LoopOrder, TA, TB, AsgC1, M, N, K, ifetch, nfetch, lat,
         rA, rB, rC, pA, pB, mu, nu, ku, lda, ldb, mulA, mulB, incpA, incpB,
         incAk, incBk);
   sprintf(ln, "incC%c", innmat);
   Cass(fpout, spc, LoopOrder, LdBott, mu, nu, alpha, beta, 0,
        rA, rB, pC, rC, mulC, ldc, ln);
/*
 * Must inc innerloop pointers if either loop exists, so indexing remains
 * as expected
 */
   if (InnerLoop || OuterLoop)
   {
      if (ldc != 0) IncPtrs(fpout, spc, 1, 'C', innmat);
      else IncPtrs(fpout, spc, nu, 'C', innmat);
      if (Aptrs) IncPtrs(fpout, spc, mu, 'A', innmat);
      else IncPtrs(fpout, spc, 1, 'A', innmat);
      if (Bptrs) IncPtrs(fpout, spc, nu, 'B', innmat);
      else IncPtrs(fpout, spc, 1, 'B', innmat);
   }
   if (InnerLoop)
   {
      spc += 3;
      fprintf(fpout, "%s   }\n%s   while(%s0 != st%c);\n",spc, spc, ip, INNMAT);
   }
   if (OuterLoop)
   {
      if (ldc != 0) IncPtrs(fpout, spc, 1, 'C', outmat);
      else IncPtrs(fpout, spc, nu, 'C', outmat);
      if (Aptrs) IncPtrs(fpout, spc, mu, 'A', outmat);
      else IncPtrs(fpout, spc, 1, 'A', outmat);
      if (Bptrs) IncPtrs(fpout, spc, nu, 'B', outmat);
      else IncPtrs(fpout, spc, 1, 'B', outmat);
      if (prefA2)
         DoPrefA_lda0(fpout, spc, pre, nprefA, 0);

      spc += 3;
      fprintf(fpout, "%s   }\n%s   while(%s0 != st%c);\n",spc, spc, op, OUTMAT);
      if (OuterTest)
      {
         spc += 3;
         fprintf(fpout, "%s   }\n", spc);
      }
   }
   if (InnerLoop && InnerTest)
   {
      spc += 3;
      fprintf(fpout, "%s   }\n", spc);
   }
/*
 * For loop cleanup, reuse matrix already in L1, if possible
 */
   k = 0;
   for (i=0; i < 2; i++)
   {
      if ( (LoopOrder == AtlasJIK && i == 0) ||
           (LoopOrder == AtlasIJK && i == 1 ) )
      {  /* A is in L1, clean N-loop first, else clean it second */
         if (N)
         {
            if (N%nu) /* there is cleanup to do */
            {
               if (ldc) sprintf(cC, "cc + %d", ldc*Nb*mulC);
               else sprintf(cC, "cc + %s", GetInc(Nb*mulC, "ldc"));
               if (TB == AtlasNoTrans)
               {
                  if (ldb) sprintf(cB, "cb + %d", ldb*Nb*mulB);
                  else sprintf(cB, "cb + %s", GetInc(Nb*mulB, "ldb"));
               }
               else sprintf(cB, "cb + %d", Nb*mulB);
               CallMM(fpout, spc, pre, lstr[LoopOrder], 1, TA, TB, M, N%nu, K,
                      mu, N%nu, ku, alpha, beta, lda, ldb, ldc, Mb, Nb, Kb0,
                      "ca", cB, cC, "M", "N", "K");
            }
         }
         else if (nu > 1 && CleaningN)
         {
            if (ldc) sprintf(cC, "cc + %s", GetInc(ldc*mulC, "Nb"));
            else sprintf(cC, "cc + %s", GetInc(mulC, "Nb*ldc"));
            if (TB == AtlasNoTrans)
            {
               if (ldb) sprintf(cB, "cb + %s", GetInc(ldb*mulB, "Nb"));
               else sprintf(cB, "cb + %s", GetInc(mulB, "Nb*ldb"));
            }
            else sprintf(cB, "cb + %s", GetInc(mulB, "Nb"));
            if (!k)
            {
               fprintf(fpout, "%s   if (k=N-Nb)\n", spc);
               CallMM(fpout, spc-3, pre, lstr[LoopOrder], 1, TA, TB, M, 0, K,
                      mu, 1, ku, alpha, beta, lda, ldb, ldc, M, Nb, Kb0,
                      "ca", cB, cC, "M", "k", "K");
            }
            else
            {
               fprintf(fpout, "%s   if ((k=N-Nb) && Nb)\n", spc);
               CallMM(fpout, spc-3, pre, lstr[LoopOrder], 1, TA, TB, M, 0, K,
                      mu, 1, ku, alpha, beta, lda, ldb, ldc, M, Nb, Kb0,
                      "ca", cB, cC, "Mb", "k", "K");
            }
            k = 1;
         }
      }
      else if ( (LoopOrder == AtlasIJK && i == 0) ||
                (LoopOrder == AtlasJIK && i == 1) )
      {  /* if B is in L1, clean M-loop first, else clean it second */
         if (CleanN)
         {
            j = N - N%nu;
            sprintf(ctmp, "Nb");
         }
         else
         {
            j = N;
            sprintf(ctmp, "N");
         }
         if (M)
         {
            sprintf(cC, "cc + %d", Mb*mulC);
            if (TA == AtlasNoTrans) sprintf(cA, "ca + %d", Mb*mulA);
            else
            {
               if (lda) sprintf(cA, "ca + %d", mulA*Mb*lda);
               else sprintf(cA, "ca + %s", GetInc(mulA*Mb, "lda"));
            }
            if (M%mu)
            {
               if (CleanN && !N)
               {
                  fprintf(fpout, "%s   if (Nb)\n\n", spc);
                  spc -= 3;
               }
               CallMM(fpout, spc, pre, lstr[LoopOrder], 1, TA, TB, M%mu, j, K,
                      M%mu, nu, ku, alpha, beta, lda, ldb, ldc, Mb, Nb, Kb0,
                      cA, "cb", cC, "M", ctmp, "K");
               if (CleanN && !N) spc += 3;
            }
         }
         else if (mu > 1 && CleaningM)
         {
            sprintf(cC, "cc + %s", GetInc(mulC, "Mb"));
            if (TA == AtlasNoTrans) sprintf(cA, "ca + %s", GetInc(mulA, "Mb"));
            else
            {
               if (lda) sprintf(cA, "ca + %s", GetInc(mulA*lda, "Mb"));
               else sprintf(cA, "ca + %s", GetInc(mulA, "Mb*lda"));
            }
            if (!k)
            {
               fprintf(fpout, "%s   if (k=M-Mb)\n", spc);
               CallMM(fpout, spc-3, pre, lstr[LoopOrder], 1, TA, TB, 0, j, K,
                      1, nu, ku, alpha, beta, lda, ldb, ldc, Mb, Nb, Kb0,
                      cA, "cb", cC, "k", "N", "K");
            }
            else
            {
               fprintf(fpout, "%s   if (Nb && (k=M-Mb))\n", spc);
               CallMM(fpout, spc-3, pre, lstr[LoopOrder], 1, TA, TB, 0, j, K,
                      1, nu, ku, alpha, beta, lda, ldb, ldc, Mb, Nb, Kb0,
                      cA, "cb", cC, "k", "Nb", "K");
            }
            k = 1;
         }
      }
   }
   fprintf(fpout, "}\n");
   fprintf(fpout, "#ifdef incAm\n   #undef incAm\n#endif\n");
   fprintf(fpout, "#ifdef incAn\n   #undef incAn\n#endif\n");
   fprintf(fpout, "#ifdef incAk\n   #undef incAk\n#endif\n");
   fprintf(fpout, "#ifdef incBm\n   #undef incBm\n#endif\n");
   fprintf(fpout, "#ifdef incBn\n   #undef incBn\n#endif\n");
   fprintf(fpout, "#ifdef incBk\n   #undef incBk\n#endif\n");
   fprintf(fpout, "#ifdef incCm\n   #undef incCm\n#endif\n");
   fprintf(fpout, "#ifdef incCn\n   #undef incCn\n#endif\n");
   fprintf(fpout, "#ifdef incCk\n   #undef incCk\n#endif\n");
   fprintf(fpout, "#ifdef Mb\n   #undef Mb\n#endif\n");
   fprintf(fpout, "#ifdef Nb\n   #undef Nb\n#endif\n");
   fprintf(fpout, "#ifdef Kb\n   #undef Kb\n#endif\n");
}

void GenMakefile(char pre, int nb, int nb0, int nbetas, char *betas,
                 CLEANNODE **cp)
{
   const int nalpha=2;
   char fnam[64], trans[2] = {'N', 'T'}, cal[2] = {'1', 'X'};
   char cwh[3] = {'M', 'N', 'K'};
   int i, j, k, p;
   char UPR;
   FILE *fp;
   CLEANNODE *wp;

   UPR = (pre == 's' || pre == 'c') ? 'S' : 'D';
   sprintf(fnam, "KERNEL/%cMakefile", pre);
   fp = fopen(fnam, "w");
   assert(fp);

   fprintf(fp, "include ../Make.inc\n\n");
   fprintf(fp, "CDEFS2 = $(CDEFS)");
   for (i=0; i < 3; i++)
      if (cp[i]) fprintf(fp, " -DATL_UCLEAN%c", cwh[i]);
   fprintf(fp, "\n");
   fprintf(fp, "obj = \\\n");
   for (i=0; i < nbetas; i++)
      fprintf(fp, "      ATL_%cNBmm_b%c.o \\\n", pre, betas[i]);
   for (j=0; j < 2; j++)
   {
      for (i=0; i < 2; i++)
      {
         fprintf(fp, "      ATL_%cNCCUmm%c%c.o \\\n", pre, trans[j], trans[i]);
         fprintf(fp, "      ATL_%cNCCUmm%c%c_K.o \\\n",pre, trans[j], trans[i]);
         for (k=0; k < nbetas; k++)
         {
            for (p=0; p < nalpha; p++)
            {
               fprintf(fp, "      ATL_%cNCmm%c%c_a%c_b%c.o \\\n",
                       pre, trans[j], trans[i], cal[p], betas[k]);

            }
         }
      }
   }
   for (i=0; i < 3; i++)
   {
      if (cp[i])
      {
         for (j=0; j < nbetas; j++)
         {
            fprintf(fp, "      ATL_%cup%cBmm_b%c.o \\\n", pre,cwh[i], betas[j]);
            for (wp=cp[i]; wp; wp = wp->next)
            {
               for(k=0; k < wp->ncomps; k++)
                  fprintf(fp, "      ATL_%cup%cBmm%d_%d_%d_b%c.o \\\n", pre,
                          cwh[i], wp->NBs[k], wp->imult, wp->fixed, betas[j]);

            }
         }
      }
   }
   for (i=0; i < nbetas; i++)
   {
      fprintf(fp, "      ATL_%cgpMBmm_b%c.o \\\n", pre, betas[i]);
      fprintf(fp, "      ATL_%cgpNBmm_b%c.o \\\n", pre, betas[i]);
   }
   if (nb0 != 0) fprintf(fp, "      ATL_%cgpKBmm0.o \\\n", pre);
   for (i=nb0+1; i <= nb; i++)
   {
      fprintf(fp, "      ATL_%cgpKBmm%d.o \\\n", pre, i);
   }
   fprintf(fp, "      ATL_%cgpKBmm.o \n\n", pre);

   fprintf(fp, "%cclean : clean\n", pre);
   fprintf(fp, "clean : \n\trm -f $(obj) %clib.grd\n\n", pre);

   fprintf(fp, "all : lib\n");
   fprintf(fp, "lib : %clib\n", pre);
   fprintf(fp, "%clib : %clib.grd\n", pre, pre);
   fprintf(fp, "%clib.grd : $(obj)\n", pre);
   fprintf(fp, "\t $(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(obj)\n");
   fprintf(fp, "\t $(RANLIB) $(ATLASlib)\n");
   fprintf(fp, "\t touch %clib.grd\n", pre);

   for (i=0; i < 4; i++)
   {
      for (wp=cp[i]; wp; wp = wp->next)
      {
         if (wp->CC[0])  /* need non-default compilation rule */
         {
            char *comp = (wp->CC[0] == 'g' && wp->CC[1] == 'c'
                          && wp->CC[2] == 'c' &&
                          (wp->CC[3] == '\0' || wp->CC[3] == ' '))
                         ? "$(GOODGCC)" : wp->CC;
            for (j=0; j < nbetas; j++)
            {
               for(k=0; k < wp->ncomps; k++)
               {
                  if (i < 3)
                     sprintf(fnam, "ATL_%cup%cBmm%d_%d_%d_b%c", pre, cwh[i],
                             wp->NBs[k], wp->imult, wp->fixed, betas[j]);
                  else sprintf(fnam, "ATL_%cNBmm_b%c", pre, betas[j]);
                     fprintf(fp, "%s.o : %s.c \n", fnam, fnam);
                  fprintf(fp, "\t%s $(CDEFS2) -DATL_BETA=%c -c %s %s.c\n",
                          comp, betas[j], wp->CCFLAGS, fnam);
               }
            }
         }
      }
   }

   fprintf(fp, "\n.c.o:\n\t$(%cMC) $(CDEFS2) $(%cMCFLAGS) -c $<\n", UPR, UPR);
   fclose(fp);
}

void ComplexWrap(FILE *fpout, char *type, char *caller, char *callee, int beta)
{
   char bnam0[8], bnam0N[8];

   if (beta == 1)
   {
      sprintf(bnam0N, "_bX");
      sprintf(bnam0, "_b1");
   }
   else if (beta == 0)
   {
      sprintf(bnam0N, "_b0");
      sprintf(bnam0, "_b0");
   }
   else
   {
      sprintf(bnam0N, "_bX");
      sprintf(bnam0, "_bX");
   }

   fprintf(fpout, "#ifdef ATL_UCLEAN%c\n", toupper(caller[6]));
   fprintf(fpout, "#define %s ATL_%cg%s\n", caller, caller[4], caller+5);
   fprintf(fpout, "#endif\n\n");

   fprintf(fpout, "void %s%s(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc);\n", callee, bnam0N, type, type, type, type, type);
   fprintf(fpout, "void %s%s(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc);\n", callee, "_bX", type, type, type, type, type);
   fprintf(fpout, "void %s%s(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc);\n", callee, "_b1", type, type, type, type, type);
   fprintf(fpout, "\n");

   fprintf(fpout, "void %s(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc)\n", caller, type, type, type, type, type);
   fprintf(fpout, "{\n");
   fprintf(fpout, "   %s%s(M, N, K, alpha, A, lda, B, ldb, -beta, C, ldc);\n", callee, bnam0N);
   fprintf(fpout, "   %s%s(M, N, K, alpha, A, lda, B+N*ldb, ldb, beta, C+1, ldc);\n", callee, bnam0N);
   fprintf(fpout, "   %s%s(M, N, K, alpha, A+M*lda, lda, B+N*ldb, ldb, -1.0, C, ldc);\n", callee, "_bX");
   fprintf(fpout, "   %s%s(M, N, K, alpha, A+M*lda, lda, B, ldb, 1.0, C+1, ldc);\n", callee, "_b1");
   fprintf(fpout, "}\n");

}

void GetInstLogFile(char *nam, int pre, int *muladd, int *pfA, int *lat,
                    int *nb, int *mu, int *nu, int *ku, int *ForceFetch,
                    int *ifetch, int *nfetch, double *mflop)
{
   char ln[128];
   FILE *fp;

   fp = fopen(nam, "r");
   if (fp == NULL) fprintf(stderr, "file %s not found!!\n\n", nam);
   assert(fp);
   fgets(ln, 128, fp);
   fscanf(fp, " %d %d %d %d %d %d %d %d %d %d %lf\n",
          muladd, lat, pfA, nb, mu, nu, ku, ForceFetch, ifetch, nfetch, mflop);
   fclose(fp);
}

void GenKBcases(char *spc, char pre, char *type, int *kb0, int *nb0)
{
   char fnam[128];
   char *bnams[3] = {"_b1", "_b0", "_bX"};
   int kb, muladd, lat, nb, mu, nu, ku, FFetch, ifetch, nfetch, i, pfA;
   int mulC=1;
   double specmf, genmf;
   FILE *fpin, *fpout;

   if (pre == 'c' || pre == 'z') mulC = 2;
   sprintf(fnam, "res/%cCleanK", pre);
   fpin = fopen(fnam, "r");
   assert(fpin);
   fgets(fnam, 128, fpin);
   i = 0;
   do
   {
      fscanf(fpin, " %d %d %d %d %d %d %d %d %d %d %d %lf %lf\n",
             &kb, &muladd, &lat, &pfA, &nb, &mu, &nu, &ku, &FFetch,
             &ifetch, &nfetch, &specmf, &genmf);
      ncucases = 0;
      sprintf(fnam, "KERNEL/ATL_%cgpKBmm%d.c", pre, kb);
      fpout = fopen(fnam, "w");
      assert(fpout);
      emit_mm(fpout, spc, pre, type, "", AtlasJIK, AtlasTrans, AtlasNoTrans,
              1, muladd, pfA, lat, FFetch, ifetch, nfetch, 0, 0, kb, mu, nu, ku,
              1, 1, mulC, kb, kb, 0, 1, 8);
      fclose(fpout);
      if (kb) i++;
   }
   while (kb && i < nb);
   fclose(fpin);
   *kb0 = nb - i;
   *nb0 = nb;
}

int GenKBmm(char *spc, char pre, char *type)
/*
 * Generate KBmm cases, and return kb0
 */
{
   char fnam[128];
   char *betnam = "_bX";
   char ba[3] = {'0', '1', 'X'};
   const int COMPLEX = (pre == 'c' || pre == 'z');
   int i, kb0, nb;
   FILE *fp;

   GenKBcases(spc, pre, type, &kb0, &nb);

   sprintf(fnam, "KERNEL/ATL_%cgpKBmm.c", pre);
   fp = fopen(fnam, "w");
   assert(fp);

   fprintf(fp, "#ifndef ATL_UCLEANK\n");
   fprintf(fp, "   #define ATL_%cgpKBmm ATL_%cpKBmm\n#endif\n\n", pre, pre);
   for (i=kb0+1; i <= nb; i++)
   {
      fprintf(fp, "void ATL_%cJIK0x0x%dTN%dx%dx0_a1%s(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc);\n", pre, i, i, i, betnam, type, type, type, type, type);

   }
   if (kb0 != 0)
      fprintf(fp, "void ATL_%cJIK0x0x0TN0x0x0_a1%s(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc);\n", pre, betnam, type, type, type, type, type);

   fprintf(fp, "typedef void (*MMfunc)(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc);\n\n", type, type, type, type, type);

   fprintf(fp, "void ATL_%cgpKBmm(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc)\n{\n", pre, type, type, type, type, type);

   if (kb0 >= nb)
   {
      if (COMPLEX)
      {
         fprintf(fp, "   ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A, lda, B, ldb, -beta, C, ldc);\n", pre);
         fprintf(fp, "   ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A, lda, B+N*ldb, ldb, beta, C+1, ldc);\n", pre);
         fprintf(fp, "   ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A+M*lda, lda, B+N*ldb, ldb, -1.0, C, ldc);\n", pre);
         fprintf(fp, "   ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A+M*lda, lda, B, ldb, 1.0, C+1, ldc);\n", pre);
      }
      else
      {
         fprintf(fp, "   ATL_%cJIK0x0x0TN0x0x0_a1%s(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);\n", pre, betnam);
      }
   }
   else
   {
      fprintf(fp, "   static MMfunc mmfunc[%3d] = {\n", nb-kb0);
      for (i=kb0+1; i < nb; i++)
         fprintf(fp,
                 "                         ATL_%cJIK0x0x%dTN%dx%dx0_a1%s,\n",
                 pre, i, i, i, betnam);
      fprintf(fp, "                         ATL_%cJIK0x0x%dTN%dx%dx0_a1%s,\n",
              pre, i, i, i, betnam);
      fprintf(fp, "                        };\n");
      if (COMPLEX) fprintf(fp, "   MMfunc mm;\n");
      fprintf(fp, "\n");

      if (kb0)
      {
         if (COMPLEX)
         {
            fprintf(fp, "   if (K <= %d)\n   {\n", kb0);
            fprintf(fp, "      ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A, lda, B, ldb, -beta, C, ldc);\n", pre);
            fprintf(fp, "      ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A, lda, B+N*ldb, ldb, beta, C+1, ldc);\n", pre);
            fprintf(fp, "      ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A+M*lda, lda, B+N*ldb, ldb, -1.0, C, ldc);\n", pre);
            fprintf(fp, "      ATL_%cJIK0x0x0TN0x0x0_a1_bX(M, N, K, alpha, A+M*lda, lda, B, ldb, 1.0, C+1, ldc);\n", pre);
            fprintf(fp, "   }\n   else\n   {\n");
            fprintf(fp, "      mm = mmfunc[K-%d];\n", kb0+1);
            fprintf(fp, "      mm(M, N, K, alpha, A, lda, B, ldb, -beta, C, ldc);\n");
            fprintf(fp, "      mm(M, N, K, alpha, A, lda, B+N*ldb, ldb, beta, C+1, ldc);\n");
            fprintf(fp, "      mm(M, N, K, alpha, A+M*lda, lda, B+N*ldb, ldb, -1.0, C, ldc);\n");
            fprintf(fp, "      mm(M, N, K, alpha, A+M*lda, lda, B, ldb, 1.0, C+1, ldc);\n");
            fprintf(fp, "   }\n");
         }
         else
         {
            fprintf(fp, "   if (K <= %d) ATL_%cJIK0x0x0TN0x0x0_a1%s(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);\n", kb0, pre, betnam);
            fprintf(fp, "   else mmfunc[K-%d](M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);\n", kb0+1);
         }
      }
      else if (COMPLEX)
      {
         fprintf(fp, "      mm = mmfunc[K-1];\n");
         fprintf(fp, "      mm(M, N, K, alpha, A, lda, B, ldb, -beta, C, ldc);\n");
         fprintf(fp, "      mm(M, N, K, alpha, A, lda, B+N*ldb, ldb, beta, C+1, ldc);\n");
         fprintf(fp, "      mm(M, N, K, alpha, A+M*lda, lda, B+N*ldb, ldb, -1.0, C, ldc);\n");
         fprintf(fp, "      mm(M, N, K, alpha, A+M*lda, lda, B, ldb, 1.0, C+1, ldc);\n");
      }
      else fprintf(fp, "   mmfunc[K-1](M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);\n");
   }
   fprintf(fp, "}\n");
   fprintf(fp, "#ifndef ATL_UCLEANK\n");
   for (i=0; i < 3; i++)
   {
      fprintf(fp, "void ATL_%cpKBmm_b%c(const int M, const int N, const int K, const %s alpha, const %s *A, const int lda, const %s *B, const int ldb, const %s beta, %s *C, const int ldc)\n", pre, ba[i], type, type, type, type, type);
      fprintf(fp,
      "{\n   ATL_%cgpKBmm(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);\n}\n",
              pre);
   }
   fprintf(fp, "#endif\n");
   fclose(fp);
   return(kb0);
}

void GenMNBcase(char *spc, char MN, char pre, char *type, int beta, char *bnam)
{
   const int COMPLEX = (pre == 'c' || pre == 'z');
   const int csA=1, csB=1, csC = ( COMPLEX ? 2 : 1 );
   char fnam[128], callee[128], caller[128];
   int muladd, lat, mb, nb, kb, mu, nu, ku, ffetch, ifetch, nfetch, pfA;
   double mflop;
   FILE *fpout;

   sprintf(fnam, "res/%cClean%c", pre, MN);
   GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                  &ffetch, &ifetch, &nfetch, &mflop);
   mb = kb = nb;
   if (MN == 'M') mb = 0;
   else nb = 0;

   sprintf(fnam, "KERNEL/ATL_%cgp%cBmm%s.c", pre, MN, bnam);
   fpout = fopen(fnam, "w");
   assert(fpout);
   if (!COMPLEX)
   {
      fprintf(fpout, "#ifdef ATL_UCLEAN%c\n", MN);
      fprintf(fpout,
              "#define ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1%s ATL_%cgp%cBmm%s\n",
              pre, mb, nb, kb, kb, kb, 0, bnam, pre, MN, bnam);
      fprintf(fpout, "#else\n");
      fprintf(fpout,"#define ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1%s ATL_%cp%cBmm%s\n",
              pre, mb, nb, kb, kb, kb, 0, bnam, pre, MN, bnam);
      fprintf(fpout, "#endif\n\n");
   }
   ncucases = 0;
   emit_mm(fpout, spc, pre, type, "", AtlasJIK, AtlasTrans, AtlasNoTrans,
           1, muladd, pfA, lat, ffetch, ifetch, nfetch, mb, nb, kb, mu, nu, ku,
           csA, csB, csC, kb, kb, 0, 1, beta);
   if (COMPLEX)
   {
      sprintf(caller, "ATL_%cp%cBmm%s", pre, MN, bnam);
      sprintf(callee, "ATL_%cJIK%dx%dx%dTN%dx%dx%d_a1",
              pre, mb, nb, kb, kb, kb, 0);
      ComplexWrap(fpout, type, caller, callee, beta);
   }
   fclose(fpout);
}

void GenMNClean(char *spc, char pre, char *type)
{
   char *bnams[3] = {"_b1", "_b0", "_bX"};
   int betas[3] = {1, 0, 8};
   int i;

   for (i=0; i < 3; i++)
   {
      GenMNBcase(spc, 'M', pre, type, betas[i], bnams[i]);
      GenMNBcase(spc, 'N', pre, type, betas[i], bnams[i]);
   }
}

int LineIsCont(char *ln)
{
   int i, iret=0;
   for(i=0; ln[i]; i++);
   if (i)
   {
      for(i--; isspace(ln[i]); i--);
      if (ln[i] == '\\') iret = 1;
   }
   return(iret);
}

int NumUserCases0(char *nam)
{
   int iret=0;
   char ln[512];
   FILE *fp;

   fp = fopen(nam, "r");
   if (fp)
   {
      fgets(ln, 512, fp);  /* skip comment line */
      assert(fscanf(fp, " %d", &iret) == 1);
      fclose(fp);
   }
   return(iret);
}

int NumUserCases(char pre)
{
   char ln[64];

   sprintf(ln, "%ccases.dsc", pre);
   return(NumUserCases0(ln));
}

void NoEndLineWhiteSpace(char *ln)
{
   int i;

   for (i=0; ln[i]; i++);
   if (i)
      for (i--; isspace(ln[i]); i--) ln[i] = '\0';
}

int GetUserCase(char pre, int icase, int *iflag, int *mb, int *nb, int *kb,
                int *ma, int *lat, int *mu, int *nu, int *ku,
                char *fnam, char *auth, char **MCC, char **MMFLAGS)
/*
 * if icase < 0, go to that line in file; if icase > 0 find that ID in file
 * return ID of selected line
 */
{
   int i, n, ID;
   char ln[512];
   static char sMCC[1024], sMMFLAGS[2048];
   FILE *fp;

   *MCC = *MMFLAGS = NULL;
   n = NumUserCases(pre);
   sprintf(ln, "%ccases.dsc", pre);
   fp = fopen(ln, "r");
   if (!fp) return(0);
   assert(fp);
   fgets(ln, 256, fp);  /* skip comment line */
   fgets(ln, 256, fp);  /* skip number of cases */
   for (i=0; i < n; i++)
   {
      if ( fgets(ln, 256, fp) == NULL )
      {
         fclose(fp);
         return(0);
      }
      assert(sscanf(ln, " %d %d %d %d %d %d %d %d %d %d %s \"%[^\"]", &ID,
                    iflag, mb, nb, kb, ma, lat, mu, nu, ku, fnam, auth) == 12);
      assert(ID > 0);
      if (i == -icase || ID == icase)
      {
         if (LineIsCont(ln))
         {
            assert( fgets(ln, 256, fp) != NULL );
            strcpy(sMCC, ln);
            NoEndLineWhiteSpace(sMCC);
            assert( fgets(ln, 512, fp) != NULL );
            strcpy(sMMFLAGS, ln);
            NoEndLineWhiteSpace(sMMFLAGS);
            *MCC = sMCC;
            *MMFLAGS = sMMFLAGS;
         }
         else *MCC = *MMFLAGS = NULL;
         fclose(fp);
         return(ID);
      }
      if (i != icase && LineIsCont(ln))
      {
         assert( fgets(ln, 256, fp) != NULL );
         assert( fgets(ln, 256, fp) != NULL );
      }
   }
   fclose(fp);
   return(0);
}

void PrintCleanNodes(CLEANNODE *cp)
{
   int NumUCleanRout(CLEANNODE *cp);
   fprintf(stdout, "\nncases=%d\n", NumUCleanRout(cp));
   for (; cp; cp = cp->next)
      fprintf(stdout,
      "icase=%d, imult=%d, nb=%d, fixed=%d, ncomps=%d, *NBs=%d, rout=%s\n",
              cp->icase, cp->nb, cp->imult, cp->fixed, cp->ncomps, *cp->NBs, cp->rout);
   fprintf(stdout, "\n");
}


void KillCleanNodes(CLEANNODE *cp0)
{
   CLEANNODE *cp;
   while (cp0)
   {
      if (cp0->NBs) free(cp0->NBs);
      cp = cp0->next;
      free(cp0);
      cp0 = cp;
   }
}

int CompMultHandled(CLEANNODE *cp, int nb)
/*
 * Returns 1 if nb is handled by succeeding case, 0 otherwise
 */
{
   for (; cp; cp = cp->next)
   {
      if (cp->fixed == 2)
      {
         if (cp->imult == nb)
            return(1);
      }
      else if (nb % cp->imult == 0)
         return(1);
   }
   return(0);
}

int *GetCompNBs(enum CW which, CLEANNODE *cp0, int *N)
/*
 * Get number NBs to compile cleanup cp0 with
 * NOTE: assumes CLEANNODEs are sorted in ascending imult order.
 */
{
   int *NBs, *ip;
   int i, n=1, istop;
   CLEANNODE *cp;

   if (!cp0)
   {
      *N = 0;
      return(NULL);
   }

   NBs = malloc(cp0->nb*sizeof(int));
   assert(NBs);
   if (cp0->fixed == 1)
   {
      NBs[0] = cp0->imult;
      istop = cp0->nb;
      while (istop % cp0->imult) istop--;
      if (istop == cp0->nb) istop -= cp0->imult;

      for (i=2*cp0->imult; i <= istop; i += cp0->imult)
      {
         if (!CompMultHandled(cp0->next, i)) NBs[n++] = i;
      }
   }
   else if (cp0->fixed) NBs[0] = cp0->imult;
   else NBs[0] = 0;

   ip = malloc(n * sizeof(int));
   assert(ip);
   for (i=0; i < n; i++) ip[i] = NBs[i];

   free(NBs);
   *N = n;

   return(ip);
}

CLEANNODE *GetUCleanInfo(char pre, enum CW which)
{
   char cwh[3] = {'M', 'N', 'K'};
   FILE *fp;
   CLEANNODE *cp, *cp0;
   char *MCC, *MMFLAGS;
   int i, j, n;
   char ln[256];

   sprintf(ln, "res/%cuClean%cF", pre, cwh[which]);
   fp = fopen(ln, "r");
   assert(fp);
   assert(fgets(ln, 256, fp));
   assert(fgets(ln, 256, fp));
   assert(sscanf(ln, " %d", &n) == 1);
   if (n < 1) return(NULL);
   cp = cp0 = malloc(sizeof(CLEANNODE));
   for (i=0; i < n; i++)
   {
      assert(fgets(ln, 256, fp));
      assert(sscanf(ln, " %d %d %d %d",
             &cp->imult, &cp->icase, &cp->fixed, &cp->nb) == 4);
      GetUserCase(pre, cp->icase, &j, &j, &j, &j, &j, &j, &j, &j, &j, cp->rout,
                  ln, &MCC, &MMFLAGS);

      if (MCC) strcpy(cp->CC, MCC);
      else cp->CC[0] = '\0';
      if (MMFLAGS) strcpy(cp->CCFLAGS, MMFLAGS);
      else cp->CCFLAGS[0] = '\0';

      if (i != n-1) cp->next = malloc(sizeof(CLEANNODE));
      else cp->next = NULL;
      cp = cp->next;
   }
   fclose(fp);
   for (cp=cp0; cp; cp=cp->next) cp->NBs = GetCompNBs(which, cp, &cp->ncomps);
   return(cp0);
}

int NumUCleanRout(CLEANNODE *cp)
{
   int i;
   for (i=0; cp; cp = cp->next) i++;
   return(i);
}

int NumUCleanRoutFixed(CLEANNODE *cp, int fix)
{
   int i;
   for (i=0; cp; cp = cp->next) if (cp->fixed == fix) i++;
   return(i);
}

CLEANNODE *GetCaseNode(CLEANNODE *cp, int icase)
{
   int i;
   for (i=0; cp && i+cp->ncomps < icase; cp = cp->next, i += cp->ncomps);
   return(cp);
}

CLEANNODE *GetFixedCaseNode(CLEANNODE *cp, int irout, int ifix)
{
   int i;

   for(i=0; cp; cp = cp->next);
   {
      if (cp->fixed == ifix)
      {
         i += cp->ncomps;
         if (i >= irout) return(cp);
      }
   }
   return(NULL);
}

char *GetZCleanNam(char *nam)
/*
 * For complex, upcases <pre> in name
 */
{
   static char znam[128];

   strcpy(znam, nam);
   if (znam[4] == 'c') znam[4] = 'C';
   else if (znam[4] == 'z') znam[4] = 'Z';
   return(znam);
}

char *GetNoBetaNam(char *nam)
/*
 * chops the beta suffix from name
 */
{
   static char bnam[128];
   char *sp;

   strcpy(bnam, nam);
   sp = strstr(bnam, "_b");
   *sp = '\0';
   return(bnam);
}

char *GetUCleanNam(char pre, CLEANNODE *cp, int inam, char cwh, char cbeta)
{
   static char fnam[128];
   int i;

   for (i=0; cp && i+cp->ncomps <= inam; cp = cp->next) i += cp->ncomps;
   assert(cp);
   inam -= i;
   sprintf(fnam, "ATL_%cup%cBmm%d_%d_%d_b%c",
           pre, cwh, cp->NBs[inam], cp->imult, cp->fixed, cbeta);
   return(fnam);
}

int NumUCompsFixed(CLEANNODE *cp, int fix)
{
   int i;

   for(i=0; cp; cp = cp->next)
      if (cp->fixed == fix) i += cp->ncomps;
   return(i);
}
int NumUComps(CLEANNODE *cp)
{
   int i;

   for(i=0; cp; cp = cp->next) i += cp->ncomps;
   return(i);
}

void PrintUHeaders(char pre, FILE *fp, CLEANNODE *cp, char cwh, char cbeta,
                   int FuncPtrs)
{
   int i, n, j, nbet=1;
   char cbe[3] = {'0', '1', 'X'}, cbeta0=cbeta;
   char *reg, *mac, *nob, *typ;
   const char *pargs = "M_, N_, K_, al_, A_, lda_, B_, ldb_";

   if (pre == 's') fprintf(fp, "#define SREAL\n");
   else if (pre == 'd') fprintf(fp, "#define DREAL\n");
   else if (pre == 'c') fprintf(fp, "#define SCPLX\n");
   else if (pre == 'z') fprintf(fp, "#define DCPLX\n");
   fprintf(fp, "#include \"atlas_misc.h\"\n");

   if (pre == 'c' || pre == 'z') nbet = 3;
   else cbe[0] = cbeta0;
   n = NumUComps(cp);

   if (pre == 'c' || pre == 's') typ = "float";
   else typ = "double";

   for (j=0; j < nbet; j++)
   {
      cbeta = cbe[j];
      for (i=0; i < n; i++)
      {
         fprintf(fp, "void %s\n", GetUCleanNam(pre, cp, i, cwh, cbeta));
         fprintf(fp,
      "   (const int M, const int N, const int K, const %s alpha,\n", typ);
         fprintf(fp,
      "    const %s *A, const int lda, const %s *B, const int ldb,\n",
                 typ, typ);
         fprintf(fp,
      "    const %s beta, %s *C, const int ldc);\n", typ, typ);
      }
      if (cwh == 'K') fprintf(fp, "void ATL_%cpKBmm_b%c\n", pre, cbeta);
      else fprintf(fp, "void ATL_%cgp%cBmm_b%c\n", pre, cwh, cbeta);
      fprintf(fp,
         "   (const int M, const int N, const int K, const %s alpha,\n", typ);
      fprintf(fp,
      "    const %s *A, const int lda, const %s *B, const int ldb,\n",
              typ, typ);
      fprintf(fp,
         "    const %s beta, %s *C, const int ldc);\n", typ, typ);
   }
   if (cwh == 'K')
   {
      fprintf(fp, "void ATL_%cgpKBmm\n", pre);
      fprintf(fp,
      "   (const int M, const int N, const int K, const %s alpha,\n", typ);
      fprintf(fp,
      "    const %s *A, const int lda, const %s *B, const int ldb,\n",
              typ, typ);
      fprintf(fp,
      "    const %s beta, %s *C, const int ldc);\n", typ, typ);
   }
/*
 * For complex, create macros for handling complex arithmetic
 */
   if (pre == 'c' || pre == 'z')
   {
      fprintf(fp, "\n");
      cbeta = cbeta0;
      for (i=0; i < n; i++)
      {
         reg = GetUCleanNam(pre, cp, i, cwh, cbeta);
         nob = GetNoBetaNam(reg);
         mac = GetZCleanNam(reg);
         if (FuncPtrs)
         {
            pargs = "M, N, K, alpha, A, lda, B, ldb";
            fprintf(fp, "static void %s\n", mac);
            fprintf(fp,
         "   (const int M, const int N, const int K, const %s alpha,\n", typ);
            fprintf(fp,
      "    const %s *A, const int lda, const %s *B, const int ldb,\n",
                    typ, typ);
            fprintf(fp,
         "    const %s beta, %s *C, const int ldc)\n{\n", typ, typ);
            fprintf(fp, "   const %s *rA = A + lda*M, *rB = B + ldb*N;\n", typ);
            if (cbeta == '0')
               fprintf(fp, "   %s_b0(%s, ATL_rzero, C, ldc); \n", nob, pargs);
            else if (cbeta == '1')
               fprintf(fp, "   %s_bX(%s, ATL_rnone, C, ldc); \n", nob, pargs);
            else fprintf(fp, "   %s_bX(%s, -beta, C, ldc); \n", nob, pargs);
            fprintf(fp, "   %s_b%c", nob, cbeta);
            fprintf(fp,
   "(M, N, K, alpha, A, lda, rB, ldb, beta, C+1, ldc); \n");
            fprintf(fp, "   %s_bX", nob, pargs);
            fprintf(fp,
   "(M, N, K, alpha, rA, lda, rB, ldb, ATL_rnone, C, ldc); \n");
            fprintf(fp, "   %s_b1", nob, pargs);
            fprintf(fp,
   "(M, N, K, alpha, rA, lda, B, ldb, ATL_rone, C+1, ldc); \n");
         }
         else
         {
            fprintf(fp,
         "#define %s(M_, N_, K_, al_, A_, lda_, B_, ldb_, be_, C_, ldc_) \\\n",
                    mac);
            fprintf(fp, "{ \\\n");
            if (cbeta == '0')
               fprintf(fp, "   %s_b0(%s, ATL_rzero, C_, ldc_); \\\n",
                       nob, pargs);
            else if (cbeta == '1')
               fprintf(fp, "   %s_bX(%s, ATL_rnone, C_, ldc_); \\\n",
                       nob, pargs);
            else
               fprintf(fp, "   %s_bX(%s, -(be_), C_, ldc_); \\\n",
                       nob, pargs);
            fprintf(fp, "   %s_b%c", nob, cbeta);
            fprintf(fp,
"(M_, N_, K_, al_, A_, lda_, (B_)+(ldb_)*(N_), ldb_, be_, (C_)+1, ldc_); \\\n");
            fprintf(fp, "   %s_bX", nob, pargs);
            fprintf(fp, "(M_, N_, K_, al_, (A_)+(lda_)*(M_), lda_, (B_)+(ldb_)*(N_), ldb_, ATL_rnone, C_, ldc); \\\n");
            fprintf(fp, "   %s_b1", nob, pargs);
            fprintf(fp, "(M_, N_, K_, al_, (A_)+(lda_)*(M_), lda_, B_, ldb_, ATL_rone, (C_)+1, ldc_); \\\n");
         }
         fprintf(fp, "}\n");
      }
   }
}

CLEANNODE *ReverseNodeList(CLEANNODE *cp0)
/*
 * reverses order of node list
 */
{
   CLEANNODE *cp, *cpR;
   cpR = cp0;
   if (cp0 && cp0->next)
   {
      cp0 = cp0->next;
      cpR->next = NULL;
      while(cp0)
      {
         cp = cp0->next;
         cp0->next = cpR;
         cpR = cp0;
         cp0 = cp;
      }
   }
   return(cpR);
}

const char *MMARGS="(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc)";

void GenUpNB_ap(char pre, enum CW which, CLEANNODE *cp, char cbeta)
/*
 * Generates top-level user cleanup code, using array of function ptrs
 */
{
   CLEANNODE *wp, *cp0;
   FILE *fp;
   int i, j, nb;
   char **funcp;
   const char cwh[3] = {'M', 'N', 'K'};
   char ln[128], st[2], cw;

   nb = cp->nb;
   cw = cwh[which];

   st[0] = cwh[which];
   st[1] = '\0';

   cp0 = ReverseNodeList(cp); /* Reverse order so it's greatest to least */

   if (pre == 'c') pre = 'C';
   else if (pre == 'z') pre = 'Z';
/*
 * Setup func ptr array
 */
   funcp = malloc(nb * sizeof(char *));
   assert(funcp);
   for (i=0; i < nb; i++)
   {
      funcp[i] = malloc(64*sizeof(char));
      assert(funcp[i]);
      funcp[i][0] = '\0';
   }

   for (cp=cp0; cp; cp = cp->next)  /* handle fixed = 2 cases */
   {
      if (cp->fixed == 2)
      {
         i = cp->imult;
         if (funcp[i][0] == '\0')
            sprintf(funcp[i], "ATL_%cup%cBmm%d_%d_%d_b%c",
                    pre, cw, cp->imult, cp->imult, cp->fixed, cbeta);
      }
   }

   for (cp=cp0; cp; cp = cp->next)  /* fixed = 1 cases */
   {
      if (cp->fixed == 1)
      {
         for (i=cp->ncomps-1; i >= 0; i--)
         {
            j = cp->NBs[i];
            if (funcp[j][0] == '\0')
               sprintf(funcp[j], "ATL_%cup%cBmm%d_%d_%d_b%c",
                    pre, cw, cp->NBs[i], cp->imult, cp->fixed, cbeta);
         }
      }
   }
   for (cp=cp0; cp; cp = cp->next)  /* fixed = 0 cases */
   {
      if (cp->fixed == 0)
      {
         for (i=cp->imult; i < nb; i += cp->imult)
         {
            if (funcp[i][0] == '\0')
               sprintf(funcp[i], "ATL_%cup%cBmm0_%d_%d_b%c",
                       pre, cw, cp->imult, cp->fixed, cbeta);
         }
      }
   }
   for (i=0; i < nb; i++) /* point remainder at generated code */
   {
      if (funcp[i][0] == '\0')
      {
         if (which != CleanK)
            sprintf(funcp[i], "ATL_%cgp%cBmm_b%c", tolower(pre), cw, cbeta);
         else sprintf(funcp[i], "ATL_%cgp%cBmm", tolower(pre), cw);
      }
   }

   sprintf(ln, "KERNEL/ATL_%cup%cBmm_b%c.c", tolower(pre), cw, cbeta);
   fp = fopen(ln, "w");
   assert(fp);
   PrintUHeaders(tolower(pre), fp, cp0, cw, cbeta, 1);
   if (which == CleanK && cbeta == '1')
      fprintf(fp, "#include \"%cmm.h\"\n", tolower(pre));
   fprintf(fp, "\n");

   fprintf(fp,
"typedef void (*MMfunc)(const int, const int, const int, const TYPE,\n");
   fprintf(fp, "                       const TYPE *, const int, const TYPE *, const int,\n");
   fprintf(fp,
           "                       const TYPE, TYPE *, const int);\n\n");

   fprintf(fp, "void ATL_%cp%cBmm_b%c\n", tolower(pre), cw, cbeta);
   fprintf(fp,
      "   (const int M, const int N, const int K, const TYPE alpha,\n");
   fprintf(fp,
      "    const TYPE *A, const int lda, const TYPE *B, const int ldb,\n");
   fprintf(fp,
      "    const TYPE beta, TYPE *C, const int ldc)\n{\n");
   fprintf(fp, "\n");

   fprintf(fp, "   static MMfunc mmfunc[%d] = \n", nb);
   fprintf(fp, "   {\n");
   fprintf(fp, "      NULL,\n");
   for (i=1; i < nb-1; i++)
   {
      fprintf(fp, "      %s,\n", funcp[i]);
      free(funcp[i]);
   }
   fprintf(fp, "      %s\n   };\n\n", funcp[nb-1]);
   free(funcp[nb-1]);
   free(funcp);

   fprintf(fp, "   mmfunc[%c]%s;\n", cw, MMARGS);

   fprintf(fp, "}\n");

   if (which == CleanK && cbeta == '1')
   {
      fprintf(fp, "void ATL_%cpKBmm_b0\n", tolower(pre));
      fprintf(fp,
         "   (const int M, const int N, const int K, const TYPE alpha,\n");
      fprintf(fp,
      "    const TYPE *A, const int lda, const TYPE *B, const int ldb,\n");
      fprintf(fp,
         "    const TYPE beta, TYPE *C, const int ldc);\n");
      fprintf(fp, "void ATL_%cpKBmm_bX\n", tolower(pre));
      fprintf(fp,
         "   (const int M, const int N, const int K, const TYPE alpha,\n");
      fprintf(fp,
      "    const TYPE *A, const int lda, const TYPE *B, const int ldb,\n");
      fprintf(fp,
         "    const TYPE beta, TYPE *C, const int ldc);\n");
      fprintf(fp, "\n");

      fprintf(fp, "void ATL_%cp%cBmm\n", tolower(pre), cw);
      fprintf(fp,
         "   (const int M, const int N, const int K, const TYPE alpha,\n");
      fprintf(fp,
      "    const TYPE *A, const int lda, const TYPE *B, const int ldb,\n");
      fprintf(fp,
         "    const TYPE beta, TYPE *C, const int ldc)\n{\n");
      fprintf(fp, "   if (M != NB || N != NB)\n");
      fprintf(fp, "      ATL_%cgpKBmm%s;\n", tolower(pre), MMARGS);
      fprintf(fp, "   else if (beta == ATL_rone)\n");
      fprintf(fp, "      ATL_%cpKBmm_b1%s;\n", tolower(pre), MMARGS);
      fprintf(fp, "   else if (beta == ATL_rzero)\n");
      fprintf(fp, "      ATL_%cpKBmm_b0%s;\n", tolower(pre), MMARGS);
      fprintf(fp, "   else\n");
      fprintf(fp, "      ATL_%cpKBmm_bX%s;\n", tolower(pre), MMARGS);

      fprintf(fp, "}\n");
   }
   fclose(fp);
   ReverseNodeList(cp0); /* put order back to ascending */
}


void GenUpNB_if(char pre, enum CW which, CLEANNODE *cp, char cbeta)
/*
 * Generates top-level user cleanup code, using ifs to determine who to call
 */
{
   CLEANNODE *wp, *cp0;
   FILE *fp;
   int i;
   const char *ifs = "else if", *ifp, *sp;
   const char cwh[3] = {'M', 'N', 'K'};
   char ln[128], st[2], *typ;

   if (pre == 's' || pre == 'c') typ = "float";
   else typ = "double";

   sprintf(ln, "KERNEL/ATL_%cup%cBmm_b%c.c", pre, cwh[which], cbeta);
   fp = fopen(ln, "w");
   assert(fp);
   st[0] = cwh[which];
   st[1] = '\0';
   PrintUHeaders(pre, fp, cp, cwh[which], cbeta, 0);
   if (which == CleanK && cbeta == '1')
      fprintf(fp, "#include \"%cmm.h\"\n", pre);
   fprintf(fp, "\n");
   cp0 = ReverseNodeList(cp); /* Reverse order so it's greatest to least */
   ifp = ifs + 5;

   fprintf(fp, "void ATL_%cp%cBmm_b%c\n", pre, cwh[which], cbeta);
   fprintf(fp,
      "   (const int M, const int N, const int K, const %s alpha,\n", typ);
   fprintf(fp,
      "    const %s *A, const int lda, const %s *B, const int ldb,\n",
           typ, typ);
   fprintf(fp,
      "    const %s beta, %s *C, const int ldc)\n{\n", typ, typ);
   fprintf(fp, "\n");

   if (pre == 'c') pre = 'C';
   else if (pre == 'z') pre = 'Z';
/*
 * fixed=2 means a kernel specialized for that exact N.  They will have not
 * survived pruning if they weren't better than all the general algorithms,
 * so we can test for them all up front
 */
   for (cp=cp0; cp; cp = cp->next)  /* handle fixed = 2 cases */
   {
      if (cp->fixed == 2)
      {
         fprintf(fp, "   %s (%c == %d)\n", ifp, cwh[which], cp->imult);
         fprintf(fp, "   {\n      ATL_%cup%cBmm%d_%d_%d_b%c%s;\n   }\n",
                 pre, cwh[which], cp->imult, cp->imult, cp->fixed,
                 cbeta, MMARGS);
         ifp = ifs;
      }
   }
/*
 * All remaining kernels are selected by being a multiple of imult;
 * fixed=0 will be called directly, while fixed=1 will have a nested
 * if to find the right compiled version to call
 */
   for (cp=cp0; cp; cp = cp->next)  /* fixed = 0/1 cases */
   {
      if (cp->fixed != 1 && cp->fixed != 0)
         continue;
      if (cp->imult > 1)
         fprintf(fp, "   %s (%c == %s)\n", ifp, cwh[which],
                 GetInc(cp->imult, GetDiv(cp->imult, st)));
      else if (ifp == ifs) fprintf(fp, "   else\n");
      if (cp->fixed == 0)
      {
         fprintf(fp, "   {\n      ATL_%cup%cBmm0_%d_%d_b%c%s;\n   }\n",
                 pre, cwh[which], cp->imult, cp->fixed, cbeta, MMARGS);
         ifp = ifs;
      }
      else if (cp->fixed == 1)
      {
         fprintf(fp, "   {\n");
         if (cp->ncomps == 1)
            fprintf(fp, "      ATL_%cup%cBmm%d_%d_%d_b%c%s;\n",
                    pre, cwh[which], cp->NBs[0], cp->imult, cp->fixed,
                    cbeta, MMARGS);
         else  /* must select kernel by NB[i] */
         {
            sp = "if";
            for (i=cp->ncomps-1; i >= 0; i--)
            {
               fprintf(fp, "      %s (%c == %d)\n", sp, cwh[which], cp->NBs[i]);
               fprintf(fp,
               "      {\n         ATL_%cup%cBmm%d_%d_%d_b%c%s;\n      }\n",
                       pre, cwh[which], cp->NBs[i], cp->imult, cp->fixed,
                       cbeta, MMARGS);
               sp = "else if";
            }
         }
         fprintf(fp, "   }\n");
      }
   }

   for (cp=cp0; cp && cp->imult != 1; cp = cp->next);
   if (!cp)
   {
      if (which == CleanK)
         fprintf(fp, "   else ATL_%cgp%cBmm%s;\n", tolower(pre), cwh[which],
                 MMARGS);
      else fprintf(fp, "   else ATL_%cgp%cBmm_b%c%s;\n",
                   tolower(pre), cwh[which], cbeta, MMARGS);
   }
   fprintf(fp, "}\n");
   if (which == CleanK && cbeta == '1')
   {
      fprintf(fp, "void ATL_%cpKBmm_b0\n", tolower(pre));
      fprintf(fp,
         "   (const int M, const int N, const int K, const TYPE alpha,\n");
      fprintf(fp,
      "    const TYPE *A, const int lda, const TYPE *B, const int ldb,\n");
      fprintf(fp,
         "    const TYPE beta, TYPE *C, const int ldc);\n");
      fprintf(fp, "void ATL_%cpKBmm_bX\n", tolower(pre));
      fprintf(fp,
         "   (const int M, const int N, const int K, const TYPE alpha,\n");
      fprintf(fp,
      "    const TYPE *A, const int lda, const TYPE *B, const int ldb,\n");
      fprintf(fp,
         "    const TYPE beta, TYPE *C, const int ldc);\n");
      fprintf(fp, "\n");

      fprintf(fp, "void ATL_%cp%cBmm\n", tolower(pre), cwh[which]);
      fprintf(fp,
         "   (const int M, const int N, const int K, const %s alpha,\n", typ);
      fprintf(fp,
      "    const %s *A, const int lda, const %s *B, const int ldb,\n",
              typ, typ);
      fprintf(fp,
         "    const %s beta, %s *C, const int ldc)\n{\n", typ, typ);
      fprintf(fp, "   if (M != NB || N != NB)\n");
      fprintf(fp, "      ATL_%cgpKBmm%s;\n", tolower(pre), MMARGS);
      fprintf(fp, "   else if (beta == ATL_rone)\n");
      fprintf(fp, "      ATL_%cpKBmm_b1%s;\n", tolower(pre), MMARGS);
      fprintf(fp, "   else if (beta == ATL_rzero)\n");
      fprintf(fp, "      ATL_%cpKBmm_b0%s;\n", tolower(pre), MMARGS);
      fprintf(fp, "   else\n");
      fprintf(fp, "      ATL_%cpKBmm_bX%s;\n", tolower(pre), MMARGS);

      fprintf(fp, "}\n");
   }
   fclose(fp);
   ReverseNodeList(cp0); /* put order back to ascending */
}

void GenAllUNBCases(char pre, enum CW which, CLEANNODE *cp)
{
   char cwh[3] = {'M', 'N', 'K'};
   char cbeta[3] = {'0', '1', 'X'};
   char ln[128];
   int i, j, n, *NBs, NB[3];
   FILE *fp;

   NB[0] = NB[1] = NB[2] = cp->nb;
   n = cp->ncomps;
   assert(n);
   NBs = cp->NBs;

   for (j=0; j < n; j++)
   {
      NB[which] = NBs[j];
      for (i=0; i < 3; i++)
      {
         sprintf(ln, "KERNEL/ATL_%cup%cBmm%d_%d_%d_b%c.c",
                 pre, cwh[which], NBs[j], cp->imult, cp->fixed, cbeta[i]);
         fp = fopen(ln, "w");
         assert(fp);
         emit_uhead(fp, pre, which, NB[0], NB[1], NB[2], cp->imult, cp->fixed,
                    cp->NBs[j], i);
         fclose(fp);
         sprintf(ln, "cat CASES/%s >> KERNEL/ATL_%cup%cBmm%d_%d_%d_b%c.c\n",
                 cp->rout, pre, cwh[which], NBs[j], cp->imult, cp->fixed,
                 cbeta[i]);
         if (system(ln))
         {
            fprintf(stderr, "\nFAILED: %s\n\n", ln);
            assert(0);
         }
      }
   }
}

CLEANNODE *GenUCleanX(char pre, enum CW which)
{
   CLEANNODE *cp, *cp0;

   for (cp0=cp=GetUCleanInfo(pre, which); cp; cp = cp->next)
      GenAllUNBCases(pre, which, cp);
   if (cp0)
   {
      if (NumUComps(cp0) > 3)
      {
         GenUpNB_ap(pre, which, cp0, '0');
         GenUpNB_ap(pre, which, cp0, '1');
         GenUpNB_ap(pre, which, cp0, 'X');
      }
      else
      {
         GenUpNB_if(pre, which, cp0, '0');
         GenUpNB_if(pre, which, cp0, '1');
         GenUpNB_if(pre, which, cp0, 'X');
      }
   }

   return(cp0);
}

int DoClean(char *spc, char pre, char *type, CLEANNODE **cp)
{
   int i, kb;
fprintf(stderr, "line %d of %s\n", __LINE__, __FILE__);
   kb = GenKBmm(spc, pre, type);
fprintf(stderr, "line %d of %s\n", __LINE__, __FILE__);
   GenMNClean(spc, pre, type);
fprintf(stderr, "line %d of %s\n", __LINE__, __FILE__);
   for (i=0; i < 3; i++) cp[i] = GenUCleanX(pre, i);
   return(kb);
}

void GetGenRes(char pre, int *muladd, int *pref, int *lat, int *nb,
               int *mu, int *nu, int *ku, int *ffetch, int *ifetch,
               int *nfetch, double *mf)
{
   ATL_mmnode_t *mmp;

   mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
   assert(mmp);
   *muladd = mmp->muladd;
   *pref = mmp->pref;
   *lat = mmp->lat;
   *nb = mmp->nbB;
   *mu = mmp->mu;
   *nu = mmp->nu;
   *ku = mmp->ku;
   *ffetch = mmp->fftch;
   *ifetch = mmp->iftch;
   *nfetch = mmp->nftch;
   *mf = mmp->mflop[0];
   KillAllMMNodes(mmp);
}
int DoNocopyTT(char *spc, char pre, char *type, char cTA, char cTB)
{
   char fnam[128];
   const int COMPLEX = (pre == 'c' || pre == 'z');
   char *bnams[3] = {"_b0", "_b1", "_bX"}, *anams[2] = {"_a1", "_aX"};
   int betas[3] = {0, 1, 8}, alphas[2] = {1, 8};
   int NB, muladd, lat, mb, nb, kb, mu, nu, ku, ffetch, ifetch, nfetch;
   int mul=1, i, j, pfA;
   enum ATLAS_TRANS TA, TB;
   double mflop;
   FILE *fp;

   if (cTA == 'N') TA = AtlasNoTrans;
   else TA = AtlasTrans;
   if (cTB == 'N') TB = AtlasNoTrans;
   else TB = AtlasTrans;

   if (COMPLEX) mul = 2;
   sprintf(fnam, "res/%cNCNB", pre);
   fp = fopen(fnam, "r");
   assert(fp);
   fscanf(fp, " %d", &NB);
   fclose(fp);

   sprintf(fnam, "res/%cbest%c%c_%dx%dx%d", pre, cTA, cTB, NB, NB, NB);
   GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                  &ffetch, &ifetch, &nfetch, &mflop);
   mb = kb = nb;
   for (j=0; j < 2; j++)
   {
      for (i=0; i < 3; i++)
      {
         sprintf(fnam, "KERNEL/ATL_%cNCmm%c%c%s%s.c", pre, cTA, cTB,
                  anams[j], bnams[i]);
         fp = fopen(fnam, "w");
         assert(fp);
         ncucases = 0;
         emit_mm(fp, spc, pre, type, "", AtlasJIK, TA, TB, 1, muladd, pfA, lat,
                 ffetch, ifetch, nfetch, mb, nb, kb, mu, nu, ku, mul, mul, mul,
                 0, 0, 0, alphas[j], betas[i]);
         fclose(fp);
      }
   }

   sprintf(fnam, "res/%cbest%c%c_0x0x%d", pre, cTA, cTB, NB);
   GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                  &ffetch, &ifetch, &nfetch, &mflop);
   kb = nb;
   mb = nb = 0;
   sprintf(fnam, "KERNEL/ATL_%cNCCUmm%c%c_K.c", pre, cTA, cTB);
   fp = fopen(fnam, "w");
   assert(fp);
   ncucases = 0;
   emit_mm(fp, spc, pre, type, "", AtlasJIK, TA, TB, 1, muladd, pfA, lat,
           ffetch, ifetch, nfetch, mb, nb, kb, mu, nu, ku, mul, mul, mul,
           0, 0, 0, 8, 8);
   fclose(fp);

   sprintf(fnam, "res/%cbest%c%c_0x0x0", pre, cTA, cTB);
   GetInstLogFile(fnam, pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
                  &ffetch, &ifetch, &nfetch, &mflop);
   kb = mb = nb = 0;
   sprintf(fnam, "KERNEL/ATL_%cNCCUmm%c%c.c", pre, cTA, cTB);
   fp = fopen(fnam, "w");
   assert(fp);
   ncucases = 0;
   emit_mm(fp, spc, pre, type, "", AtlasJIK, TA, TB, 1, muladd, pfA, lat,
           ffetch, ifetch, nfetch, mb, nb, kb, mu, nu, ku, mul, mul, mul,
           0, 0, 0, 8, 8);
   fclose(fp);
   return(NB);
}

void DoNocopy(char *spc, char pre, char *type)
{
   char fnam[128];
   int muladd, lat, NB, nb, mu, nu, ku, ffetch, ifetch, nfetch, pfA;
   double mflop;
   FILE *fp;

   DoNocopyTT(spc, pre, type, 'N', 'N');
   DoNocopyTT(spc, pre, type, 'N', 'T');
   DoNocopyTT(spc, pre, type, 'T', 'N');
   NB = DoNocopyTT(spc, pre, type, 'T', 'T');

   GetGenRes(pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
             &ffetch, &ifetch, &nfetch, &mflop);

   sprintf(fnam, "atlas_%cNCmm.h", pre);
   fp = fopen(fnam, "w");
   assert(fp);
   emit_head(1, fp, pre, NB, muladd, lat, mu, nu, ku);
   fclose(fp);
}

void GetMMRES(char pre, int *muladd, int *lat, int *nb,
              int *mu, int *nu, int *ku, int *ff, int *iff, int *nf,
              double *mf, int *icase, char *ufile, char *auth, double *umf)
{
   int h, i, j, k;
   char ln[256];
   FILE *fp;
   ATL_mmnode_t *mmp;

   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   GetGenRes(pre, muladd, &h, lat, nb, mu, nu, ku, ff, iff, nf, mf);
   mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
   assert(mmp);
   if (mmp->next)
   {
      strcpy(ufile, mmp->next->rout);
      strcpy(auth, mmp->next->auth ? mmp->next->auth : "R. Clint Whaley");
      *icase = mmp->next->ID;
      *umf = mmp->next->mflop[0];
   }
   else
   {
      ufile[0] = auth[0] = '\0';
      *umf = 0.0;
      *icase = -1;
   }
   KillAllMMNodes(mmp);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
}


ATL_mmnode_t *DoUNBmm(char pre, double gmf)
{
   char ln[256];
   ATL_mmnode_t *mmp, *umm;
   FILE *fp;
   int nb, i;
   char beta[3] = {'0', '1', 'X'};

   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
   assert(mmp);
   umm = KillMMNode(mmp);
   if (umm)  /* generate the files into the KERNEL/ if using user-contrib */
   {
      nb = umm->nbB;
      for (i=0; i < 3; i++)
      {
         sprintf(ln, "KERNEL/ATL_%cNBmm_b%c.c", pre, beta[i]);
         fp = fopen(ln, "w");
         assert(fp);
         emit_uhead(fp, pre, CleanNot, nb, nb, nb, nb, nb, 0, i);
         fclose(fp);
         sprintf(ln, "cat %s >> KERNEL/ATL_%cNBmm_b%c.c",
                 umm->rout, pre, beta[i]);
         assert(!system(ln));
      }
   }
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   return(umm);
}

CLEANNODE *DoNBmm(char *spc, char pre, char *type, int *nb0)
{
   char fnam[128], ln[256];
   char *bnams[3] = {"_b0", "_b1", "_bX"};
   int betas[3] = {0, 1, 8};
   int i, muladd, lat, mb, nb, kb, mu, nu, ku, ffetch, ifetch, nfetch;
   int nreg, maU, latU, muU, nuU, kuU, pfA;
   int mulC = 1;
   double mflop, mfU;
   FILE *fp;
   CLEANNODE *cp=NULL;
   ATL_mmnode_t *umm;

   if (pre == 'c' || pre == 'z') mulC = 2;

   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   GetGenRes(pre, &muladd, &pfA, &lat, &nb, &mu, &nu, &ku,
             &ffetch, &ifetch, &nfetch, &mflop);
   *nb0 = nb;

   umm = DoUNBmm(pre, mflop);

   if (!umm) /* using generated code */
   {
      kb = mb = nb;
      for (i=0; i < 3; i++)
      {
         sprintf(ln, "KERNEL/ATL_%cNBmm%s.c", pre, bnams[i]);
         fp = fopen(ln, "w");
         assert(fp);
         ncucases = 0;
         emit_mm(fp, spc, pre, type, "", AtlasJIK, AtlasTrans, AtlasNoTrans, 0,
                 muladd, pfA, lat, ffetch, ifetch, nfetch, mb, nb, kb,
                 mu, nu, ku, 1, 1, mulC, kb, kb, 0, 1, betas[i]);
         fclose(fp);
      }
      ncucases = 0;
   }
   else
   {
/*
 *    Translate external MM stored in ATL_mmnode_t to CLEANNODE
 */
      *nb0 = nb = umm->nbB;
      cp = malloc(sizeof(CLEANNODE));
      assert(cp);
      cp->nb = cp->imult = nb;
      cp->icase = umm->ID;
      cp->fixed = 2;
      cp->CC[0] = cp->CCFLAGS[0] = '\0';
      if (umm->comp)
         strcpy(cp->CC, umm->comp);
      if (umm->cflags)
         strcpy(cp->CCFLAGS, umm->cflags);
      cp->NBs = malloc(sizeof(int));
      assert(cp->NBs);
      cp->ncomps = 1;
      cp->next = NULL;
   }
   sprintf(fnam, "%cmm.h", pre);
   fp = fopen(fnam, "w");
   assert(fp);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
/*
 * If using the user kernel, make sure MU and NU match the user params,
 * not the generated.  Use generated lat, muladd, since this info may
 * be bogus from user, and can't affect user cases
 */
   if (umm)
   {
      emit_head(0, fp, pre, nb, muladd, lat, umm->mu, umm->nu, umm->ku);
      KillAllMMNodes(umm);
   }
   else
      emit_head(0, fp, pre, nb, muladd, lat, mu, nu, ku);
   fclose(fp);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   return(cp);
}

void DoInstall(char *spc, char pre, char *type)
{
   CLEANNODE *cp[4];
   char cbeta[3] = {'0', '1', 'X'};
   int i, nb, kb;

   assert(!system("make xummsearch"));  /* create .dsc files indexing */
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   cp[3] = DoNBmm(spc, pre, type, &nb);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   DoNocopy(spc, pre, type);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   kb = DoClean(spc, pre, type, cp);
   #if VERB > 0
      fprintf(stderr, "%d of %s\n", __LINE__, __FILE__);
   #endif
   GenMakefile(pre, nb, kb, 3, cbeta, cp);
   for (i=0; i < 3; i++) KillCleanNodes(cp[i]);
}
void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s <flags>; flags include:\n", nam);
   fprintf(stderr, "   -p <pre> : choices [s,c,d,z]\n");
   fprintf(stderr,
           "   -muladd 0/1 : 1 - combined MACC, 0 - separate mul & add\n");
   fprintf(stderr, "   -ta <n/t> : A's transpose setting\n");
   fprintf(stderr, "   -tb <n/t> : B's transpose setting\n");
   fprintf(stderr, "   -l <lat>  : latency between multiply and add\n");
   fprintf(stderr, "   -m <mu>   : M-loop unrolling\n");
   fprintf(stderr, "   -n <nu>   : N-loop unrolling\n");
   fprintf(stderr, "   -k <ku>   : K-loop unrolling\n");
   fprintf(stderr, "   -[M,N,K] <lim> : 0 - loop limit is runtime variable\n");
   fprintf(stderr,
           "                    else - int is compile-time loop limit\n");
   fprintf(stderr, "   -ld[a,b,c]: 0 - leading dim is runtime variable\n");
   fprintf(stderr, "               else - leading dim is provided constant\n");
   fprintf(stderr, "   -cs[A,B,C]: col stride of array\n");
   fprintf(stderr, "   -alpha <int>: alpha to generate (1,other)\n");
   fprintf(stderr, "   -beta  <int>: beta  to generate (1,-1,0,other)\n");
   fprintf(stderr, "   -L IJK/JIK  : loop order to use\n");
   fprintf(stderr, "   -f <file>   : file to output genned routine to\n");
   fprintf(stderr,
           "   -F0 <int>   : # of reg fetches to perf before 1st flop\n");
   fprintf(stderr,
           "   -FN <int>   : # of reg fetches to perf after each flop\n");
   fprintf(stderr, "   -FF 0/1 : don't/do useless load of C at top\n");
   fprintf(stderr, "   -Cleanup 0/1 : don't/do gen cleanup code\n");
   fprintf(stderr, "   -Z 0/1 : don't/do gen all complex kernel cases\n");
   fprintf(stderr, "   -R <int> : what do you want emitted:\n");
   fprintf(stderr, "     -2 : gen all files necessary for install\n");
   fprintf(stderr, "     -3 : emit header for use with user-supplied kernel\n");
   fprintf(stderr, "     else : emit usual generated gemm kernel\n");
   fprintf(stderr, "   -pfA <int> : type of prefetch to do, OR together:\n");
   fprintf(stderr, "      0: no prefetch performed\n");
   fprintf(stderr,"      1: pref next blk of A during this iter computation\n");
   fprintf(stderr, "      2: pref C at top of M-loop\n");
   fprintf(stderr, "    -Si <what> <int> : special instructions:\n");
   fprintf(stderr,
   "    -Si temp 0/1/2 : store all temp results as TYPE/double/long double\n");
   exit(-1);
}
void GetFlags(int nargs, char **args, FILE **fpout, char *pre, int *CleanUp,
              int *muladd, int *ForceFetch, int *ifetch, int *nfetch,
              enum ATLAS_LOOP_ORDER *LoopOrder,
              enum ATLAS_TRANS *TA, enum ATLAS_TRANS *TB, int *pfA, int *lat,
              int *mu, int *nu, int *ku, int *M, int *N, int *K,
              int *lda, int *ldb, int *ldc, int *csA, int *csB, int *csC,
              int *alpha, int *beta, int *CplxTst, int *rout)
{
   int i;

/*
 * Defaults
 */
   *muladd = 0;
   *ForceFetch = 0;
   *CleanUp = 0;
   *pre = 'd';
   *fpout = stdout;
   *LoopOrder = AtlasJIK;
   *ifetch = *nfetch = -1;
   *TA = AtlasTrans;
   *TB = AtlasNoTrans;
   *lat = 4;
   *mu = 4;
   *nu = 4;
   *ku = 1;
   *M = *N = *K = 0;
   *lda = *ldb = *ldc = 0;
   *csA = *csB = *csC = 1;
   *alpha = *beta = 1;
   *CplxTst = 0;
   *rout = -1;
   *pfA = 0;

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'R':
         *rout = atoi(args[++i]);
         break;
      case 'C':
         *CleanUp = atoi(args[++i]);
         break;
      case 'Z':
         *CplxTst = atoi(args[++i]);
         break;
      case 'p':
         if (args[i][2] == 'f' && args[i][3] == 'A')
         {
            *pfA = atoi(args[++i]);
            if (*pfA & 512)  /* pfA cheesily used as flag for ld@bottom */
            {
               *pfA ^= 512;
               LD_AT_BOTTOM = 1;
            }
         }
         else
         {
            *pre = tolower(args[++i][0]);
            if (*pre != 'd' && *pre != 's' && *pre != 'c' && *pre != 'z')
               PrintUsage(args[0]);
         }
         break;
      case 'S':
         if (!strcmp(args[++i], "temp"))
            TEMP_TYPE = atoi(args[++i]);
         else
            PrintUsage(args[0]);
      case 'L':
         i++;
         if (tolower(args[i][0]) == 'i') *LoopOrder = AtlasIJK;
         else if (tolower(args[i][0]) == 'j') *LoopOrder = AtlasJIK;
         else PrintUsage(args[0]);
         if ( (tolower(args[i][1]) != 'i' && tolower(args[i][1]) != 'j') ||
              (tolower(args[i][2]) != 'k') )
            PrintUsage(args[0]);
         break;
      case 'f':
         *fpout = fopen(args[++i], "w");
         assert(*fpout);
         break;
      case 'F':
         switch(tolower(args[i][2]))
         {
         case '0':
            *ifetch = atoi(args[++i]);
            break;
         case 'n':
            *nfetch = atoi(args[++i]);
            break;
         case 'f':
            *ForceFetch = atoi(args[++i]);
            break;
         default:
            fprintf(stderr, "What the heck: flag=%s %s???\n",
                    args[i], args[i+1]);
            PrintUsage(args[0]);
         }
         break;
      case 'a':
         *alpha = atoi(args[++i]);
         break;
      case 'b':
         *beta  = atoi(args[++i]);
         break;
      case 'm':
         if (args[i][2] == 'u') *muladd = atoi(args[++i]);
         else *mu = atoi(args[++i]);
         break;
      case 't':
         if (args[i][2] == 'a' || args[i][2] == 'A')
         {
            i++;
            if (args[i][0] == 'n' || args[i][0] == 'N') *TA = AtlasNoTrans;
            else if (args[i][0] == 't' || args[i][0] == 'T') *TA = AtlasTrans;
            else
            {
               fprintf(stderr, "What the hell: TA = ???%s???\n", args[i]);
               PrintUsage(args[0]);
            }
         }
         else if (args[i][2] == 'b' || args[i][2] == 'B')
         {
            i++;
            if (args[i][0] == 'n' || args[i][0] == 'N') *TB = AtlasNoTrans;
            else if (args[i][0] == 't' || args[i][0] == 'T') *TB = AtlasTrans;
            else
            {
               fprintf(stderr, "What the hell: TB = ???%s???\n", args[i]);
               PrintUsage(args[0]);
            }
         }
         else PrintUsage(args[0]);
         break;
      case 'l':
         if (args[i][2] == '\0') *lat = atoi(args[++i]);
         else if (tolower(args[i][2]) != 'd') PrintUsage(args[0]);
         else
         {
            switch(tolower(args[i][3]))
            {
            case 'a':
               *lda = atoi(args[++i]);
               break;
            case 'b':
               *ldb = atoi(args[++i]);
               break;
            case 'c':
               *ldc = atoi(args[++i]);
               break;
            default:
               fprintf(stderr, "What the heck: flag=%s %s???\n",
                       args[i], args[i+1]);
               PrintUsage(args[0]);
            }
         }
         break;
      case 'n':
         *nu = atoi(args[++i]);
         break;
      case 'k':
         *ku = atoi(args[++i]);
         break;
      case 'M':
         *M = atoi(args[++i]);
         break;
      case 'N':
         *N = atoi(args[++i]);
         break;
      case 'K':
         *K = atoi(args[++i]);
         break;
      case 'c':
         if (args[i][2] != 's') PrintUsage(args[0]);
         switch (tolower(args[i][3]))
         {
         case 'a':
            *csA = atoi(args[++i]);
            break;
         case 'b':
            *csB = atoi(args[++i]);
            break;
         case 'c':
            *csC = atoi(args[++i]);
            break;
         default:
            fprintf(stderr, "What the heck: flag=%s %s???\n",
                    args[i], args[i+1]);
            PrintUsage(args[0]);
         }
         break;
      }
   }
}

int main(int nargs, char **args)
{
   char *spcs = "                                                            ";
   char *spc = spcs+60;
   char pre, *type;
   FILE *fpout;
   enum ATLAS_TRANS TA, TB;
   enum ATLAS_LOOP_ORDER LoopOrder;
   int muladd, lat, mu, nu, ku, M, N, K, lda, ldb, ldc, csA, csB, csC, pfA;
   int alpha, beta, ifetch, nfetch, CleanUp, ForceFetch, CplxTst, rout;

   GetFlags(nargs, args, &fpout, &pre, &CleanUp, &muladd, &ForceFetch,
            &ifetch, &nfetch, &LoopOrder, &TA, &TB, &pfA, &lat, &mu, &nu, &ku,
            &M, &N, &K, &lda, &ldb, &ldc, &csA, &csB, &csC, &alpha, &beta,
            &CplxTst, &rout);
   if (mu == 1 && nu == 1 && ku == 1 && K == 0 && muladd == 0) muladd = 1;
   if (CleanUp && K == 0) /* don't let K-cleanup get too large */
   {
      while(ku > 1 && mu*nu*ku*ku > 4096) ku--;
      if (!muladd) while (lat > 1 && mu*nu*ku % lat) lat--;
   }
   fprintf(stderr, "pre=%c, CU=%d, ma=%d, ff=%d, if=%d, nf=%d, lo=%d, ta=%d, tb=%d, lat=%d, mu=%d, nu=%d, ku=%d, m=%d, n=%d, k=%d, lda=%d, ldb=%d, ldc=%d, csA=%d, csB=%d, csC=%d, alpha=%d, beta=%d\n\n",
           pre, CleanUp, muladd, ForceFetch, ifetch, nfetch, LoopOrder, TA, TB,
           lat, mu, nu, ku, M, N, K, lda, ldb, ldc, csA, csB, csC, alpha, beta);
   switch (pre)
   {
      case 'd':
      case 'z':
         type = "double";
         break;
      case 's':
      case 'c':
         type = "float";
         break;
   }
   if (ifetch == -1) ifetch = mu + nu;
   if (rout == -2) DoInstall(spc, pre, type);
   else if (rout == -3)
      emit_uhead(fpout, pre, CleanNot, M, N, K, lda, ldb, ldc, beta);
   else
   {
      emit_mm(fpout, spc, pre, type, "", LoopOrder, TA, TB, CleanUp, muladd,
              pfA, lat, ForceFetch, ifetch, nfetch, M, N, K, mu, nu, ku,
              csA, csB, csC, lda, ldb, ldc, alpha, beta);
/*      if (CplxTst && (pre == 'c' || pre == 'z') && M && N && K) */
      if (CplxTst && (pre == 'c' || pre == 'z'))
      {
         ncucases=0;
         if (beta == 1 || beta == 0)
            emit_mm(fpout, spc, pre, type, "", LoopOrder, TA, TB, CleanUp,
                    muladd, pfA, lat, ForceFetch, ifetch, nfetch, M, N, K,
                    mu, nu, ku, csA, csB, csC, lda, ldb, ldc, alpha, 8);
         ncucases=0;
         if (beta != 1)
            emit_mm(fpout, spc, pre, type, "", LoopOrder, TA, TB, CleanUp,
                    muladd, pfA, lat, ForceFetch, ifetch, nfetch, M, N, K,
                    mu, nu, ku, csA, csB, csC, lda, ldb, ldc, alpha, 1);
      }
   }
   return(0);
}
