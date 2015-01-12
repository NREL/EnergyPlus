/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
 *               R. Clint Whaley
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
#include <assert.h>
#include <string.h>
#include "atlas_misc.h"
#include "atlas_fopen.h"
#include "atlas_prefetch.h"
#include "atlas_gentesttime.h"
#include "atlas_mmtesttime.h"

#define Mmin(x, y) ( (x) > (y) ? (y) : (x) )

#define TOLERANCE 1.2
#define MAX_NB    80
int L1Elts(char pre, int MaxL1Size)
{
   FILE *L1f;
   int L1Size, tsize;
   char ln[128];

   if (!FileExists("res/L1CacheSize"))
   {
      sprintf(ln, "make RunL1 MaxL1=%d\n",MaxL1Size);
      if (system(ln) != 0)
      {
         remove("res/L1CacheSize");
         fprintf(stderr, "Error in command: %s", ln);
         exit(-1);
      }
   }
   L1f = fopen("res/L1CacheSize", "r");
   assert(L1f != NULL);
   fscanf(L1f, "%d", &L1Size);
   fclose(L1f);
   switch (pre)
   {
      case 's':
         tsize = sizeof(float);
         break;
      case 'd':
         tsize = sizeof(double);
         break;
      case 'q':
         tsize = sizeof(long double);
         break;
      case 'c':
         tsize = sizeof(float);
         break;
      case 'z':
         tsize = sizeof(double);
         break;
   }
   return( (L1Size*1024) / tsize);
}

int GetCacheSize(int MaxL1Size)
/*
 * Returns L1 size in kilobytes
 */
{
   FILE *L1f;
   int L1Size;
   char ln[32];

   if (!FileExists("res/L1CacheSize"))
   {
      sprintf(ln, "make RunL1 MaxL1=%d\n",MaxL1Size);
      if (system(ln) != 0)
      {
         remove("res/L1CacheSize");
         fprintf(stderr, "Error in command: %s", ln);
         exit(-1);
      }
   }
   L1f = fopen("res/L1CacheSize", "r");
   assert(L1f != NULL);
   fscanf(L1f, "%d", &L1Size);
   fclose(L1f);
   fprintf(stderr, "\n      Read in L1 Cache size as = %dKB.\n",L1Size);
   return(L1Size);
}

int GetTypeSize(char pre)
{
   int tsize;
   if (pre == 'c' || pre == 's') tsize = ATL_ssize;
   else tsize = ATL_dsize;
   return(tsize);
}
void findNBs(char prec, char *NBnam, int MaxL1Size)
{
   FILE *NBf;
   char ln[80];
   int i, L1Size, tmp, tsize, tL1Size, CL, nNB;
   int NB[100];

   fprintf(stderr, "NB setting not supplied; calculating:\n");

   L1Size = GetCacheSize(MaxL1Size);
   tsize = GetTypeSize(prec);

   tL1Size = L1Size * (1024 / tsize);
   tmp = CL = ATL_Cachelen / tsize;
   if (!tmp) tmp=1;
   nNB = 0;
   fprintf(stderr, "tmp=%d, tL1size=%d\n",tmp, tL1Size);
   while (tmp*tmp <= tL1Size)
   {
      if (tmp >= 16)        /* no block sizes smaller than 16 */
         NB[nNB++] = tmp;
      if (tmp >= 80) break;  /* no block sizes bigger than 80 */
      tmp += CL;
   }
   if (!nNB)  /* this should never happen */
   {
      nNB = 3;
      NB[0] = 8;
      NB[1] = 4;
      NB[2] = 16;
   }
   else if (nNB > 2)  /* put second biggest blocking factor first in list */
   {
      tmp = NB[nNB-2];
      NB[nNB-2] = NB[0];
      NB[0] = tmp;
   }

   NBf = fopen(NBnam, "w");
   fprintf(NBf, "%d\n", nNB);
   for (i=0; i != nNB; i++) fprintf(NBf, "%d\n", NB[i]);
   fclose(NBf);
}

int GetSafeNB(char pre, int MaxL1)
{
   int i, L1, tsize, inc;

   tsize = GetTypeSize(pre);
   inc = ATL_MinMMAlign / tsize;
   if (inc < 4) inc = 4;
   L1 = (GetCacheSize(MaxL1) * 1024) / tsize;
   for (i=inc; i*i < L1; i += inc);
   if (i*i > L1) i -= inc;
   if (pre == 'd' || pre == 's')
   {
      if (i*i == L1) i -= inc;
   }
   else
   {
      if (i*i == L1) i -= 2*inc;
      else i -= inc;
   }
   if (i < 16) i = 16;
   if (i > 80) i = 80;
   return(i);
}

double GetAvg(int n, double tolerance, double *mflop)
{
   int i, j;
   double t0, tavg;
/*
 * Sort results, largest first
 */
   for (i=0; i != n; i++)
   {
      for (j=i+1; j < n; j++)
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
 * Not doing tolerance anymore, just take largest mflop rate if doing wall
 * times, or median value if doing CPU
 */

#if 1
   #ifdef WALL
      tavg = mflop[0];
   #else
      tavg = mflop[n/2];
   #endif
#else
/*
 * Throw out result if it is outside tolerance; rerun if two mflop not within
 * tolerance;  this code assumes n == 3
 */
   if (tolerance*mflop[1] < mflop[0])  /* too big a range in results */
   {
      if (tolerance*mflop[2] < mflop[1]) return(-1.0);
      tavg = (mflop[1] + mflop[2]) / 2.0;
   }
   else if (tolerance*mflop[2] < mflop[0]) tavg = (mflop[0] + mflop[1]) / 2.0;
   else tavg = (mflop[0] + mflop[1] + mflop[2]) / 3.0;
#endif

   return(tavg);
}

void PutInstLogLine(FILE *fp, int muladd, int pfA, int lat, int nb,
                    int mu, int nu, int ku, int ForceFetch,
                    int ifetch, int nfetch, double mflop)
{
   fprintf(fp, "%6d  %3d %4d %3d %3d %3d %3d  %5d  %5d  %5d  %7.2lf\n",
           muladd, lat, pfA, nb, mu, nu, ku, ForceFetch, ifetch, nfetch, mflop);
}

void PutInstLogFile(FILE *fp, int muladd, int pfA, int lat, int nb,
                    int mu, int nu, int ku, int ForceFetch,
                    int ifetch, int nfetch, double mflop)
{
   fprintf(fp,
   "MULADD  LAT  PREF NB  MU  NU  KU  FFTCH  IFTCH  NFTCH    MFLOP\n");
   PutInstLogLine(fp, muladd, pfA, lat, nb, mu, nu, ku, ForceFetch,
                  ifetch, nfetch, mflop);
}

void PutInstLogFile1(char *fnam, char pre, int muladd, int pfA, int lat,
                     int nb, int mu, int nu, int ku,
                     int ForceFetch, int ifetch, int nfetch, double mflop)
{
   FILE *fp;

   fp = fopen(fnam, "w");
   assert(fp);
   PutInstLogFile(fp, muladd, pfA, lat, nb, mu, nu, ku, ForceFetch, ifetch,
                  nfetch, mflop);
   fclose(fp);
}

void GetInstLogLine(FILE *fp, int *muladd, int *pfA, int *lat, int *nb,
                    int *mu, int *nu, int *ku, int *ForceFetch,
                    int *ifetch, int *nfetch, double *mflop)
{
   assert(fscanf(fp, " %d %d %d %d %d %d %d %d %d %d %lf\n",
                 muladd, lat, pfA, nb, mu, nu, ku, ForceFetch,
                 ifetch, nfetch, mflop) == 11);
}

void GetInstLogFile(char *nam, char pre, int *muladd, int *pfA, int *lat,
                    int *nb, int *mu, int *nu, int *ku, int *ForceFetch,
                    int *ifetch, int *nfetch, double *mflop)
{
   char ln[128];
   FILE *fp;

   fp = fopen(nam, "r");
   if (fp == NULL) fprintf(stderr, "file %s not found!!\n\n", nam);
   assert(fp);
   fgets(ln, 128, fp);
   GetInstLogLine(fp, muladd, pfA, lat, nb, mu, nu, ku, ForceFetch,
                  ifetch, nfetch, mflop);
   fclose(fp);
}

#define AUTHLEN 256
#define ROUTLEN 256
#define NOTIMED -8.0
#define ATL_MMNoClean(iflag_) ( ((iflag_) | 8) == (iflag_) )
#define ATL_MMCleanOnly(iflag_) ( ((iflag_) | 16) == (iflag_) )
#define ATL_MMVarLda(iflag_) ( ((iflag_) | 32) == (iflag_) )
#define ATL_MMVarM(iflag_) ( ((iflag_) | 64) == (iflag_) )
#define ATL_MMVarN(iflag_) ( ((iflag_) | 128) == (iflag_) )
#define ATL_MMVarK(iflag_) ( ((iflag_) | 256) == (iflag_) )

enum CLEAN_WHICH{CleanM=0, CleanN=1, CleanK=2, CleanNot=3};

typedef struct RoutNode ROUTNODE;
struct RoutNode
{
   double mflop;
   char *rout;
   ROUTNODE *next;
   int icase;
   int fixed; /* 0: pNB input para; 1: pNB=NB; 2: pNB=imult */
};
typedef struct MultHead MULTHEAD;
struct MultHead
{
   int imult;
   MULTHEAD *next;
   ROUTNODE *rn;
} *imhead=NULL;

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

int IsCaseFixed(char pre, int icase, enum CLEAN_WHICH which)
{
   char stmp[ROUTLEN], *MCC, *MMFLAGS;
   int iflag, ma, lat, mu, nu, ku, NB[3];
   assert(GetUserCase(pre, icase, &iflag, NB, NB+1, NB+2, &ma, &lat,
                      &mu, &nu, &ku, stmp, stmp, &MCC, &MMFLAGS));
   if (NB[which] > 0)
   {
      if (which == CleanM && ATL_MMVarM(iflag)) ma = 0;
      else if (which == CleanN && ATL_MMVarN(iflag)) ma = 0;
      else if (which == CleanK && ATL_MMVarK(iflag) && ATL_MMVarLda(iflag))
         ma = 0;
      else ma = 1;
   }
   else if (NB[which] < 0) ma = 2;
   else
   {
      if (which == CleanK && !ATL_MMVarLda(iflag)) ma = 1;
      else ma = 0;
   }
   return(ma);
}

ROUTNODE *CreateRoutNode(char *rout, int icase, double mflop)
{
   ROUTNODE *rn;
   int i;

   rn = malloc(sizeof(ROUTNODE));
   assert(rn);
   i = strlen(rout)+1;
   rn->rout = malloc(i * sizeof(char));
   assert(rn->rout);
   strcpy(rn->rout, rout);
   rn->icase = icase;
   rn->mflop = mflop;
   rn->fixed = -1;
   rn->next = NULL;
   return(rn);
}

void KillRoutNode(ROUTNODE *rn)
{
   free(rn->rout);
   free(rn);
}

void RemoveRoutNode(MULTHEAD *mh, ROUTNODE *rn0)
{
   ROUTNODE *rn;

   if (rn0 == NULL) return;
   assert(mh->next);
   if (mh->rn != rn0)
   {
      for (rn=mh->rn; rn->next != rn0 && rn->next; rn = rn->next);
      assert(rn->next == rn0);
      rn->next = rn0->next;
   }
   else mh->rn = rn0->next;
   KillRoutNode(rn0);
}

void KillAllRoutNodes(ROUTNODE *rn0)
{
   ROUTNODE *rn;
   while(rn0)
   {
      rn = rn0->next;
      KillRoutNode(rn0);
      rn0 = rn;
   }
}

MULTHEAD *CreateMultNode(int imult)
{
   MULTHEAD *mh;

   mh = malloc(sizeof(MULTHEAD));
   assert(mh);
   mh->imult = imult;
   mh->rn = NULL;
   mh->next = NULL;
   return(mh);
}

MULTHEAD *GetMultNode(int imult)
/*
 * Finds, and if necessary, creates MULTHEAD with value imult, keeping it
 * in ascending order
 */
{
   MULTHEAD *mh, *mh0;
   if (imhead)
   {
      for (mh=imhead; mh; mh = mh->next) if (mh->imult >= imult) break;
      if (mh && imhead == mh && mh->imult != imult)
      {
         mh = CreateMultNode(imult);
         mh->next = imhead;
         imhead = mh;
      }
      else if (!mh || mh->imult != imult)
      {
         for (mh0=imhead; mh0->next != mh; mh0 = mh0->next);
         mh0->next = CreateMultNode(imult);
         mh0->next->next = mh;
         mh = mh0->next;
      }
   }
   else mh = imhead = CreateMultNode(imult);
   return(mh);
}

void KillMultNode(MULTHEAD *mh)
{
   KillAllRoutNodes(mh->rn);
   free(mh);
}

void KillThisMultNode(MULTHEAD *mhD)
{
   MULTHEAD *mh;

   if (mhD == NULL) return;
   if (mhD == imhead) imhead = imhead->next;
   else
   {
      for (mh=imhead; mh && mh->next != mhD; mh = mh->next);
      assert(mh);
      if (mh->next) mh->next = mh->next->next;
   }
   KillMultNode(mhD);
}

void KillAllMultNodes()
{
   MULTHEAD *mh;

   while(imhead)
   {
      mh = imhead->next;
      KillMultNode(imhead);
      imhead = mh;
   }
}

ROUTNODE *GetRoutNode(int imult, char *rout, int icase, double mflop)
/*
 * Finds, and if necessary creates, the desired RoutNode
 */
{
   MULTHEAD *mh;
   ROUTNODE *rn;

   mh = GetMultNode(imult);
   for (rn = mh->rn; rn; rn = rn->next) if (rn->icase == icase) break;
   if (!rn)
   {
      rn = CreateRoutNode(rout, icase, mflop);
      if (mh->rn)
      {
         rn->next = mh->rn;
         mh->rn = rn;
      }
      else mh->rn = rn;
   }
   return(rn);
}

void PrintTable(FILE *fpout)
{
   MULTHEAD *mh;
   ROUTNODE *rn;
   for (mh=imhead; mh; mh = mh->next)
   {
      fprintf(fpout, "%3d: ", mh->imult);
      for (rn=mh->rn; rn; rn = rn->next)
         fprintf(fpout, "%3d:%1d,%5.1f ", rn->icase, rn->fixed, rn->mflop);
      fprintf(fpout, "\n");
   }
   fprintf(fpout, "\n");
}

MULTHEAD *BuildTable(char pre, enum CLEAN_WHICH which, int nb)
/*
 * Builds table of possible cleanup codes, depending on which:
 * 0 : pMB
 * 1 : pNB
 * 2 : pKB
 */
{
   ROUTNODE *rn;
   int i, n, ID, NB[3];
   int iin, io1, io2, iflag, muladd, lat, mu, nu, ku;
   char *MCC, *MMFLAGS;
   char rout[ROUTLEN], auth[AUTHLEN];

   switch(which)
   {
   case CleanM:
      iin = 0;
      io1 = 1;
      io2 = 2;
      break;
   case CleanN:
      iin = 1;
      io1 = 0;
      io2 = 2;
      break;
   case CleanK:
      iin = 2;
      io1 = 0;
      io2 = 1;
      break;
   case CleanNot:
      exit(-1);
   }

   n = NumUserCases(pre);
   for (i=0; i < n; i++)
   {
      rn = NULL;
      ID = GetUserCase(pre, -i, &iflag, NB, NB+1, NB+2, &muladd, &lat,
                       &mu, &nu, &ku, rout, auth, &MCC, &MMFLAGS);
      if (ATL_MMNoClean(iflag)) continue;
      if (NB[io1] < 0 && NB[io1] != -nb) continue;
      if (NB[io2] < 0 && NB[io2] != -nb) continue;
      if (NB[io1] && (nb % NB[io1])) continue;
      if (NB[io2] && (nb % NB[io2])) continue;
      if (NB[iin] < 0)
      {
         if (-NB[iin] < nb) rn = GetRoutNode(-NB[iin], rout, ID, NOTIMED);
      }
      else if (NB[iin] == 0) rn = GetRoutNode(1, rout, ID, NOTIMED);
      else if (NB[iin] < nb) rn = GetRoutNode(NB[iin], rout, ID, NOTIMED);
      if (rn) rn->fixed = IsCaseFixed(pre, ID, which);
   }
   return(imhead);
}

int MakeMult(int nb, int mul)
/*
 * takes nb, makes it a multiple of mul by reducing
 */
{
   return( (nb / mul) * mul );
}

int GetPNB(char pre, enum CLEAN_WHICH which, int icase, int NB, int imul,
           int *pNB)
/*
 * Returns number of pNB that are multiple of imul (max of 3) to be timed;
 * pNB contains the values to try
 */
{
   int i=1, j;
   int iflag, NBs[3], muladd, lat, mu, nu, ku;
   char fnam[ROUTLEN], *MCC, *MMFLAGS;

   pNB[0] = pNB[1] = pNB[2] = 0;

   assert(GetUserCase(pre, icase, &iflag, NBs, NBs+1, NBs+2, &muladd, &lat,
                      &mu, &nu, &ku, fnam, fnam, &MCC, &MMFLAGS));
   if (NBs[which] < 0) pNB[0] = -NBs[which];
   else
   {
      j = pNB[0] = MakeMult(NB-NB/8, imul);
      if (!j) pNB[0] = imul;
      j = MakeMult(NB/2, imul);
      if (j && j != pNB[0])
      {
         pNB[1] = j;
         i = 2;
         j = NB/8;
         if (NB >= 32) j = Mmax(j, 16);
         j = pNB[2] = ((j+imul-1)/imul)*imul;
         if (j && j != pNB[1] && j != pNB[0]) i = 3;
         else pNB[2] = 0;
      }
   }
   return(i);
}

#define NO_RESULTS -88.7
double GetRes(char *fnam)
{
   double *dp;
   dp = ReadResultsFile(0, 3, fnam);
   if (dp)
      return(*dp);
   return(NO_RESULTS);
}
double ummcase0
(
   char pre,                  /* type prefix */
   int M, int N, int K,       /* problem sizes to time */
   int mb, int nb, int kb,    /* 0: variable NB, else fixed cpp macro of NB */
   int lda, int ldb, int ldc, /* leading dims */
   int muladd, int lat,       /* muladd and latency settings */
   int mu, int nu, int ku,    /* unrolling factors */
   char *fnam,                /* file name to compile */
   char *MCC, char *MMFLAGS,  /* NULL : use defaults, else comp to use */
   char *outnam               /* output name */
)
{
   char ln[4096];
   char ch;
   int i;
   double mf;

   if (!FileExists(outnam))
   {
      if (pre == 'c' || pre == 'z')
         i = sprintf(ln, "make cmmucase mmrout=CASES/%s csC=2 ", fnam);
      else i = sprintf(ln, "make mmucase mmrout=CASES/%s ", fnam);
      if (MCC)
      {
         ch = (pre == 'c' || pre == 's') ? 'S' : 'D';
         i += sprintf(ln+i, "%cMC=\"%s\" %cMCFLAGS=\"%s\" ",
                      ch, MCC, ch, MMFLAGS);
      }
      i += sprintf(ln+i, "casnam=%s ", outnam);
      i += sprintf(ln+i, "pre=%c muladd=%d lat=%d M=%d N=%d K=%d mb=%d nb=%d kb=%d mu=%d nu=%d ku=%d lda=%d ldb=%d ldc=%d ",
                   pre, muladd, lat, M, N, K, mb, nb, kb, mu, nu, ku,
                   lda, ldb, ldc);
      i += sprintf(ln+i, "> /dev/null 2>&1\n");
      if (system(ln) != 0) return(-1.0);
   }
   mf = GetRes(outnam);
   if (mf == NO_RESULTS) mf = -1.0;
   return(mf);
}

int GetIflag(char pre, int icase)
{
   int iflag, mb, nb, kb, muladd, lat, mu, nu, ku;
   char fnam[ROUTLEN], *MCC, *MMFLAGS;
   assert(GetUserCase(pre, icase, &iflag, &mb, &nb, &kb, &muladd, &lat,
                      &mu, &nu, &ku, fnam, fnam, &MCC, &MMFLAGS));
   return(iflag);
}

double GetCleanCase(char pre, enum CLEAN_WHICH which, int icase, int imul,
                    int mb, int nb, int kb)
{
   char cwh[3] = {'M', 'N', 'K'};
   char outf[ROUTLEN], fnam[ROUTLEN], *MCC, *MMFLAGS;
   int ld=kb, NB[3], NB1[3], NBs[3], nb0;
   int iflag, mb1, nb1, kb1, muladd, lat, mu, nu, ku;

   assert(GetUserCase(pre, icase, &iflag, NB1, NB1+1, NB1+2, &muladd, &lat,
                      &mu, &nu, &ku, fnam, outf, &MCC, &MMFLAGS));
   if (ATL_MMNoClean(iflag)) return(-1.0);
   NBs[0] = mb;
   NBs[1] = nb;
   NBs[2] = kb;
   nb0 = kb;
   if (which == CleanK)
   {
      nb0 = nb;
      if (ATL_MMVarLda(iflag)) ld = 0;
   }
   NB[0] = NB[1] = NB[2] = nb0;
   if (NB1[which]) NB[which] = NBs[which];
   else NB[which] = 0;
   sprintf(outf, "res/%cup%cB%d_%d_%dx%dx%d", pre, cwh[which], icase, imul,
           mb, nb, kb);
   return(ummcase0(pre, mb, nb, kb, NB[0], NB[1], NB[2], ld, ld, 0, muladd, lat,
                   mu, nu, ku, fnam, MCC, MMFLAGS, outf));
}

double GetCleanCases0(char pre, enum CLEAN_WHICH which, int nb, int imul,
                      int icase, int n, int *pNB)
{
   int i, NB[3];
   double mf0, mf=0.0;

   NB[0] = NB[1] = NB[2] = nb;
   for (i=0; i < n; i++)
   {
      NB[which] = pNB[i];
      mf0 = GetCleanCase(pre, which, icase, imul, NB[0], NB[1], NB[2]);
      if (mf0 <= 0.0) return(-1.0); /* reject if it fails to run */
      mf += mf0;
   }
   return(mf / n);
}

double GetCleanCases(char pre, enum CLEAN_WHICH which, int nb, int imul,
                     int icase)
{
   int n, pNB[3];

   n = GetPNB(pre, which, icase, nb, imul, pNB);
   return(GetCleanCases0(pre, which, nb, imul, icase, n, pNB));
}

void TimeRouts(char pre, enum CLEAN_WHICH which, int nb, int imul,
               ROUTNODE *rn0)
{
   ROUTNODE *rn;

   for (rn=rn0; rn; rn = rn->next)
      if (rn->mflop == NOTIMED)
         rn->mflop = GetCleanCases(pre, which, nb, imul, rn->icase);
}

void TimeTable(char pre, enum CLEAN_WHICH which, int nb)
/*
 * Times table of cleanup codes, depending on which
 */
{
   MULTHEAD *mh;

   for (mh=imhead; mh; mh = mh->next)
      TimeRouts(pre, which, nb, mh->imult, mh->rn);
}

ROUTNODE *FindBestRout(ROUTNODE *rn0, double adv)
/*
 * Excludes those of fixed size
 */
{
   ROUTNODE *rnB=NULL, *rn;
   double mfB=0.0, ad;
   for (rn=rn0; rn; rn = rn->next)
   {
      if (rn->fixed != 2)
      {
         ad = (rn->fixed == 0)*adv*rn->mflop;
         if (rn->mflop+ad > mfB)
         {
            rnB = rn;
            mfB = rn->mflop+ad;
         }
      }
   }
   return(rnB);
}

ROUTNODE *FindBestFixed2(ROUTNODE *rn0)
{
   ROUTNODE *rnB=NULL, *rn;
   double mfB=0.0;
   for (rn=rn0; rn; rn = rn->next)
   {
      if (rn->fixed == 2 && rn->mflop > mfB)
      {
         rnB = rn;
         mfB = rn->mflop;
      }
   }
   return(rnB);
}

void ReduceRouts(char pre, enum CLEAN_WHICH which, MULTHEAD *mh,
                 double adv, int nb)
/*
 * reduces routs to best, giving adv advantage to non-fixed routs
 */
{
   double mf;
   ROUTNODE *rn, *rnN, *rnF, *rnNF=NULL;
   int n, NB[3];

   rn = FindBestRout(mh->rn, adv);
   rnF = FindBestFixed2(mh->rn);
   if (rn || rnF) /* some case actually compiled and ran */
   {
      if (rnF && rn)
      {
         n = GetPNB(pre, which, rnF->icase, nb, mh->imult, NB);
         mf = GetCleanCases0(pre, which, nb, mh->imult, rn->icase, n, NB);
         mf += adv*mf;
         if (mf < rnF->mflop)
         {
            rnNF = CreateRoutNode(rnF->rout, rnF->icase, rnF->mflop);
            rnNF->fixed = 2;
         }
      }
      else if (rnF)
      {
         rnNF = CreateRoutNode(rnF->rout, rnF->icase, rnF->mflop);
         rnNF->fixed = 2;
         KillAllRoutNodes(mh->rn);
         mh->rn = rnNF;
      }
      if (rn)
      {
         rnN = CreateRoutNode(rn->rout, rn->icase, rn->mflop);
         rnN->fixed = rn->fixed;
         KillAllRoutNodes(mh->rn);
         mh->rn = rnN;
         if (rnNF) mh->rn->next = rnNF;
         else mh->rn->next = NULL;
      }
   }
   else KillThisMultNode(mh);
}

void ReduceMults(char pre, enum CLEAN_WHICH which, double adv, int nb)
/*
 * Finds mults that are multiples of each other, and takes best,
 * giving adv advantage to non-fixed routs, and .5 adv to lower imults
 */
{
   MULTHEAD *mh, *mh0, *mh1;
   double mf0, mf1;
   int imult;
   int NB[3];
   NB[0] = NB[1] = NB[2] = nb;
   for (mh0=imhead; mh0; mh0 = mh0->next)
   {
      if (mh0->rn->fixed == 2) continue;
      for (mh1=mh0, mh=mh0->next; mh; mh = mh->next)
      {
         if (mh->imult % mh0->imult == 0) /* higher mult is mult of lower */
         {
            imult = mh->imult;
            if (mh->rn->fixed == 2)
            {
               NB[which] = imult;
               mf1 = mh->rn->mflop;
            }
            else
            {
               NB[which] = imult <= 4 ? ((nb-imult)/imult)*imult :
                                         ((nb-1)/imult)*imult;
               mf1 = GetCleanCase(pre, which, mh->rn->icase, imult,
                                  NB[0], NB[1], NB[2]);
            }
            mf0 = GetCleanCase(pre, which, mh0->rn->icase, imult,
                               NB[0], NB[1], NB[2]);
            if (((mh0->rn->fixed == 0)+0.5)*adv*mf0+mf0 >
                (mh->rn->fixed == 0)*adv*mf1+mf1)
            {
               mh1->next = mh->next;
               KillMultNode(mh);
               mh = mh1;
            }
         }
         mh1 = mh;
      }
   }
}

void ReduceTable(char pre, enum CLEAN_WHICH which, int nb)
/*
 * Reduces table to best in each catagory
 */
{
   MULTHEAD *mh, *mhnext;
   const double advant=0.03;

   for (mh=imhead; mh; mh = mhnext)
   {
      mhnext = mh->next;
      ReduceRouts(pre, which, mh, advant, nb);
   }
   ReduceMults(pre, which, advant, nb);
}

int ummtstcase0
(
   char pre,                  /* type prefix */
   int M, int N, int K,       /* problem sizes to test */
   int mb, int nb, int kb,    /* 0: variable NB, else fixed cpp macro of NB */
   int lda, int ldb, int ldc, /* leading dims */
   int muladd, int lat,       /* muladd and latency settings */
   int mu, int nu, int ku,    /* unrolling factors */
   char *fnam,                /* file name to compile */
   char *MCC, char *MMFLAGS   /* NULL : use defaults, else comp to use */
)
{
   char ln[512];
   int i;
   char ch;

   if (pre == 'c' || pre == 'z')
      i = sprintf(ln, "make cmmutstcase mmrout=CASES/%s csC=2 ", fnam);
   else i = sprintf(ln, "make mmutstcase mmrout=CASES/%s ", fnam);
   if (MCC)
   {
      ch = (pre == 's' || pre == 'c') ? 'S' : 'D';
      i += sprintf(ln+i, "%cMC=\"%s\" %cMCFLAGS=\"%s\" ", ch, MCC, ch, MMFLAGS);
   }
   i += sprintf(ln+i, "pre=%c muladd=%d lat=%d M=%d N=%d K=%d mb=%d nb=%d kb=%d mu=%d nu=%d ku=%d lda=%d ldb=%d ldc=%d ",
                pre, muladd, lat, M, N, K, mb, nb, kb, mu, nu, ku,
                lda, ldb, ldc);
   i += sprintf(ln+i, "> /dev/null 2>&1\n");
   fprintf(stdout, "     TESTING PRE='%c' FILE='%s', NB=%d . . .",
           pre, fnam, nb);
   i = system(ln);
   if (!i) fprintf(stdout, " PASSED!\n");
   else fprintf(stdout, " FAILED!\n");
   return(i == 0);
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

int GetUserNB(char pre, int NB, int mb, int nb, int kb, int iflag)
/*
 * Given ATLAS's preference for NB, finds a good nb based on user's input
 */
{
   int i, mult;
   static int nelt=0;

   if (mb < 0)
   {
      if ( (nb < 0 && nb != mb) || (kb < 0 && kb != mb) ) return(0);
      else return(-mb);
   }
   else if (nb < 0)
   {
      if (kb < 0 && kb != nb) return(0);
      else return(-nb);
   }
   else if (kb < 0) return(-kb);
   if (!nelt)
      nelt = L1Elts(pre, 128*1024);
   if (!nb) nb = 1;
   if (!mb) mb = 1;
   if (!kb) kb = 1;
   mult = Mylcm(mb,nb);
   mult = Mylcm(kb,mult);
   if (iflag&512)  /* needs to keep 4 matrices in cache */
   {
      if ( (NB%mb == 0) && (NB%nb == 0) && (NB%kb == 0) && 3*NB*NB <= nelt)
         return(NB);
      i = (NB/mult)*mult;
      if (4*i*i <= nelt)
         return(i);
      for (; i >= 16; i -= mult)
         if (4*i*i <= nelt || i <= 16)
            return(i);
   }
   if ( (NB%mb == 0) && (NB%nb == 0) && (NB%kb == 0) ) return(NB);
   i = ((NB+mult-1)/mult)*mult;
   if (i*i < nelt) return(i);
   else return((NB/mult)*mult);
   return(0);
}


char *GetUserOutFile(char pre, int ifile, int mb, int nb, int kb)
{
   static char ln[32];
   sprintf(ln, "res/%cuser%03d_%dx%dx%d", pre, ifile+1, mb, nb, kb);
   return(ln);
}

double ummcase(char pre, int ifile, int NB)
{
   char outnam[256], fnam[256];
   char *MCC, *MMFLAGS;
   int iflag, mb, nb, kb, muladd, lat, mu, nu, ku;

   assert(GetUserCase(pre, ifile, &iflag, &mb, &nb, &kb, &muladd, &lat,
                      &mu, &nu, &ku, fnam, outnam, &MCC, &MMFLAGS));
   if (ATL_MMCleanOnly(iflag)) return(0.0); /* don't run if for cleanup only */
   return(ummcase0(pre, NB, NB, NB, NB, NB, NB, NB, NB, 0, muladd, lat,
                   mu, nu, ku, fnam, MCC, MMFLAGS,
                   GetUserOutFile(pre, ifile, NB, NB, NB)));
}

int utstmmcase(char pre, int ifile, int NB)
{
   char outnam[256], fnam[256];
   char *MCC, *MMFLAGS;
   int iflag, mb, nb, kb, muladd, lat, mu, nu, ku;

   assert(GetUserCase(pre, ifile, &iflag, &mb, &nb, &kb, &muladd, &lat,
                      &mu, &nu, &ku, fnam, outnam, &MCC, &MMFLAGS));
   return(ummtstcase0(pre, NB, NB, NB, NB, NB, NB, NB, NB, 0, muladd, lat,
                      mu, nu, ku, fnam, MCC, MMFLAGS));
}


int GetUserFNB(char pre, int NB, int mb, int nb, int kb, int iflag)
/*
 * RETURNS: 0 if described kernel cannot handle NB, else NB
 */
{
   int i, mult;
   static int nelt=0;

   if ((mb < 0 && mb != -NB) || (nb < 0 && nb != -NB) || (kb < 0 && kb != -NB))
      return(0);
   if (mb > 0 && NB%mb)
      return(0);
   if (nb > 0 && NB%nb)
      return(0);
   if (kb > 0 && NB%kb)
      return(0);
   return(NB);
}

int FindBestUserFNB(char pre, int nb0)
/*
 * returns index in <pre>cases.dsc of best user-supplied GEMM, using
 * a blocking factor of exactly nb
 */
{
   char *MCC, *MMFLAGS;
   char ln[256], fnam[256];
   double mf, mfbest=0.0, adv;
   int ibest=(-1), i, ncases;
   int ID, iflag, NB, mb, nb, kb, ma, lat, mu, nu, ku, nbB=0;
   FILE *fpsum;

   sprintf(ln, "res/%cucases.res", pre);
   fpsum = fopen(ln, "w");
   ncases = NumUserCases(pre);
   for (i=0; i < ncases; i++)
   {
      ID = GetUserCase(pre, -i, &iflag, &mb, &nb, &kb, &ma, &lat,
                       &mu, &nu, &ku, fnam, ln, &MCC, &MMFLAGS);
      assert(ID > 0);
      NB = GetUserFNB(pre, nb0, mb, nb, kb, iflag);
      if (!NB)
         printf("Rejected icase=%d, NB=%d, MB,NB,KB=%d,%d,%d\n",
                ID, nb0, mb, nb, kb);
      else
      {
         mf = ummcase(pre, ID, NB);
         adv = 1.0;

         if (mf > adv*mfbest)
         {
            if (utstmmcase(pre, ID, NB))
            { /* test kernel before accepting */
               ibest = ID;
               mfbest = mf;
               nbB = NB;
            }
         }
         fprintf(stdout, "%3d. NB=%3d, rout=%40s, MFLOP=%.2f\n",
                 i, NB, fnam, mf);
         if (fpsum)
            fprintf(fpsum, "%3d. ID=%8d, NB=%3d, rout=%40s, MFLOP=%.2f\n",
                    i, ID, NB, fnam, mf);
      }
   }
   if (fpsum)
      fclose(fpsum);
   printf("BEST CASE IS ID=%d, NB=%d, MFLOP=%.2f\n", ibest, nbB, mfbest);
   return(ibest);
}

int FindBestUser(char pre, int nb0)
/*
 * returns index in <pre>cases.dsc of best user-supplied GEMM, using
 * a blocking factor as close to nb0 as possible
 */
{
   char *MCC, *MMFLAGS;
   char ln[256], fnam[256];
   double mf, mfbest=0.0, adv;
   int ibest=(-1), i, ncases;
   int ID, iflag, NB, mb, nb, kb, ma, lat, mu, nu, ku, nbB=0;
   FILE *fpsum;

   sprintf(ln, "res/%cucases.res", pre);
   fpsum = fopen(ln, "w");
   ncases = NumUserCases(pre);
   for (i=0; i < ncases; i++)
   {
      ID = GetUserCase(pre, -i, &iflag, &mb, &nb, &kb, &ma, &lat,
                       &mu, &nu, &ku, fnam, ln, &MCC, &MMFLAGS);
      assert(ID > 0);
      NB = GetUserNB(pre, nb0, mb, nb, kb, iflag);
      if (NB)
      {
         mf = ummcase(pre, ID, NB);
         if (NB > 80 && nbB < NB-8)
            adv = 1.08;           /* penalize large NB due to cleanup probs */
         else if (NB > nbB)       /* slightly favor small NB */
            adv = 1.001;
         else if (NB < nbB)       /* slightly favor small NB */
            adv = 0.999;
         else
            adv = 1.0;

         if (mf > adv*mfbest)
         {
            if (utstmmcase(pre, ID, NB))
            { /* test kernel before accepting */
               ibest = ID;
               mfbest = mf;
               nbB = NB;
            }
         }
         fprintf(stdout, "%3d. NB=%3d, rout=%40s, MFLOP=%.2f\n",
                 i, NB, fnam, mf);
         if (fpsum)
            fprintf(fpsum, "%3d. ID=%8d, NB=%3d, rout=%40s, MFLOP=%.2f\n",
                    i, ID, NB, fnam, mf);
      }
   }
   if (fpsum)
      fclose(fpsum);
   printf("BEST CASE IS ID=%d, NB=%d, MFLOP=%.2f\n", ibest, nbB, mfbest);
   return(ibest);
}

void PrintUsage(char *fnam)
{
   fprintf(stderr,
           "\nUSAGE: %s -p <pre> -n <nb> -e <search summary file\n\n", fnam);
   exit(-1);
}

ROUTNODE *GetFlags(int nargs, char **args, char *pre, int *nb0, int *FNB,
                   enum CLEAN_WHICH *which)
/*
 * RETURNS: a queue of filenames, which are the output files from external
 *          searches (using atlas_mmtesttime.h format).
 */
{
   int i, ID=10000;
   char ch;
   ROUTNODE *efb=NULL, *ef, *ep;/* base &ptr to external search summary files */

   *FNB = *nb0 = 0;
   *pre = 'd';
   *which = CleanNot;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'p':
         i++;
         ch = tolower(args[i][0]);
         if (ch == 'd' || ch == 's' || ch == 'z' || ch == 'c') *pre = ch;
         else PrintUsage(args[0]);
         break;
      case 'n':
         *nb0 = atoi(args[++i]);
         break;
      case 'N':
         *FNB = atoi(args[++i]);
         break;
      case 'e':
         if (++i > nargs)
            PrintUsage(args[0]);
          ep = CreateRoutNode(args[i], ID, 0.0);
          if (efb)
             ef->next = ep;
          else
             efb = ep;
          ef = ep;
          ID += 5000;
          break;
      case 'C':
         switch(args[++i][0])
         {
         case 'm':
            *which = CleanM;
            break;
         case 'n':
            *which = CleanN;
            break;
         case 'k':
            *which = CleanK;
            break;
         default:
            *which = CleanNot;
         }
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   return(efb);
}

int FindBestNB(pre, icase)
{
   double mf, mfb;
   int iflag, mb, nb, kb, muladd, lat, mu, nu, ku;
   int i, j, mult, iret=0;
   char fnam[ROUTLEN], *MCC, *MMFLAGS;
   assert(GetUserCase(pre, icase, &iflag, &mb, &nb, &kb, &muladd, &lat,
                      &mu, &nu, &ku, fnam, fnam, &MCC, &MMFLAGS));

   if (mb < 0) iret = -mb;
   else if (nb < 0) iret = -nb;
   else if (kb < 0) iret = -kb;
   else
   {
/*
 *    Find mult necessary to satisfy user constraints on mb, nb, and kb
 */
      if (mb != 0) i = mb;
      else i = 1;
      if (nb != 0) j = nb;
      else j = 1;
      mult = Mylcm(i, j);
      if (kb != 0) i = kb;
      else i = 1;
      mult = Mylcm(i, mult);
/*
 *    Make sure mult is a multiple of Cachelen as well
 */
      if (pre == 's' || pre == 'c') i = ATL_Cachelen / ATL_ssize;
      else i = ATL_Cachelen / ATL_dsize;
      if (i > 1) mult = Mylcm(i, mult);
      fprintf(stdout, "\nFINDING BEST BLOCKING FACTOR FOR CASE %d, MUL=%d:\n",
              icase, mult);

      j = L1Elts(pre, 128);
      for (i=mult; i < 16; i += mult);
      mfb = 0.0;
      iret = i;
      for (; ((i*i <= j) && (i <= MAX_NB)); i += mult)
      {
         mf = ummcase(pre, icase, i);
         if (mf > mfb)
         {
            mfb = mf;
            iret = i;
         }
         fprintf(stdout, "   NB=%d: %.2f MFLOP\n", i, mf);
      }
   }
   fprintf(stdout, "BEST BLOCKING FACTOR FOR CASE %d:  %d\n", icase, iret);
   return(iret);
}

int NumMults(void)
{
   MULTHEAD *mh;
   int i;

   for(i=0, mh=imhead; mh; mh = mh->next, i++) if (mh->rn->next) i++;
   return(i);
}

void Printpline(char pre, enum CLEAN_WHICH which, int nb, int imult,
                ROUTNODE *rn, FILE *fp)
{
   int NB[3];
   GetPNB(pre, which, rn->icase, nb, imult, NB);
   fprintf(fp, "%4d  %5d  %5d %3d  %3d  %3d  %3d  %8.2f  %s\n",
           imult, rn->icase, rn->fixed, nb, NB[0], NB[1], NB[2],
           rn->mflop, rn->rout);
}
void CreatepUMMOut(char pre, enum CLEAN_WHICH which, int nb)
{
   MULTHEAD *mh;
   char cwh[3] = {'M', 'N', 'K'};
   char fnam[128];
   int NB[3];
   FILE *fp;

   sprintf(fnam, "res/%cuClean%c", pre, cwh[which]);
   fp = fopen(fnam, "w");
   assert(fp);
   fprintf(fp, "MULT  ICASE  FIXED  NB  NB0  NB1  NB2     MFLOP  ROUT\n");
   fprintf(fp, "%d\n", NumMults());
   for (mh=imhead; mh; mh = mh->next)
   {
      Printpline(pre, which, nb, mh->imult, mh->rn, fp);
      if (mh->rn->next) Printpline(pre, which, nb, mh->imult, mh->rn->next, fp);
   }
}

void FindUClean(char pre, int nb0, enum CLEAN_WHICH which)
{
   BuildTable(pre, which, nb0);
   PrintTable(stdout);
   TimeTable(pre, which, nb0);
   PrintTable(stdout);
   ReduceTable(pre, which, nb0);
   PrintTable(stdout);

   CreatepUMMOut(pre, which, nb0);
   KillAllMultNodes();
}

void CreateUMMOut(char pre, int icase, int NB, double mf)
{
   char fnam[ROUTLEN], auth[AUTHLEN], *MCC, *MMFLAGS;
   int iflag, mb, nb, kb, muladd, lat, mu, nu, ku;
   FILE *fp;

   sprintf(auth, "res/%cuMMRES", pre);
   fp = fopen(auth, "w");
   assert(fp);

   if (icase > 0)
   {
      assert(GetUserCase(pre, icase, &iflag, &mb, &nb, &kb, &muladd, &lat,
                         &mu, &nu, &ku, fnam, auth, &MCC, &MMFLAGS));
   }
   else
   {
      mf = -1.0;
      strcpy(auth, "Nobody");
      strcpy(fnam, "Nocomp");
   }
   fprintf(fp, "CASE  NB    MFLOP  ROUTINE\n");
   fprintf(fp, "%4d %3d %8.2f  \"%.64s\" \"%.64s\"\n",
           icase, NB, mf, fnam, auth);
   fclose(fp);
}

void FindUMM(char pre, int nb0)
{
   double mf, mf0;
   int nb, icase;

   icase = FindBestUser(pre, nb0);
   if (icase >= 0)
   {
      mf0 = GetRes(GetUserOutFile(pre, icase, nb0, nb0, nb0));
      nb = FindBestNB(pre, icase);
      mf = GetRes(GetUserOutFile(pre, icase, nb, nb, nb));
      if (mf <= mf0)
      {
         nb = nb0;
         mf = mf0;
      }
   }
   else
   {
      nb = nb0;
      mf = -1.0;
   }
   CreateUMMOut(pre, icase, nb, mf);
   fprintf(stdout, "\nBEST USER CASE %d, NB=%d: %.2f MFLOP\n\n", icase, nb, mf);
}

void RunTimes(char pre)
{
   double mf;
   FILE *fp;
   int j, icase, nb;
   char ln[128];
   ATL_mmnode_t *mmp;

   sprintf(ln, "res/%cuMMRES", pre);
   if (FileExists(ln))
   {
      fp = fopen(ln, "r");
      assert( fgets(ln, 128, fp) != NULL );
      assert( fscanf(fp, " %d %d", &icase, &nb) == 2);
      fclose(fp);
      if (icase >= 0)
      {
         mf = ummcase(pre, icase, nb);
         CreateUMMOut(pre, icase, nb, mf);
         fprintf(stdout, "\nBEST USER CASE %d, NB=%d: %.2f MFLOP\n\n",
                 icase, nb, mf);
      }
   }
   else
   {
      mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
      if (!mmp)
         mmp = ReadMMFileWithPath(pre, "res", "gMMRES.sum");
      assert(mmp);
      nb = (mmp->next) ? mmp->next->nbB : mmp->nbB;
      KillAllMMNodes(mmp);
      FindUMM(pre, nb);
   }
}

void GetuMMRES(char pre)
{
   double mf;
   FILE *fp;
   int j, icase, nb;
   char ln[128];
   ATL_mmnode_t *mmp;

   sprintf(ln, "res/%cuMMRES", pre);
   if (FileExists(ln))
   {
      fp = fopen(ln, "r");
      assert( fgets(ln, 128, fp) != NULL );
      assert( fscanf(fp, " %d %d %lf", &icase, &nb, &mf) == 3);
      fclose(fp);
      if (icase >= 0 && mf <= 0.0)
      {
         mf = ummcase(pre, icase, nb);
         CreateUMMOut(pre, icase, nb, mf);
         fprintf(stdout, "\nBEST USER CASE %d, NB=%d: %.2f MFLOP\n\n",
                 icase, nb, mf);
      }
   }
   else
   {
      mmp = ReadMMFileWithPath(pre, "res", "MMRES.sum");
      if (!mmp)
         mmp = ReadMMFileWithPath(pre, "res", "gMMRES.sum");
      assert(mmp);
      nb = (mmp->next) ? mmp->next->nbB : mmp->nbB;
      KillAllMMNodes(mmp);
      FindUMM(pre, nb);
   }
}

ATL_mmnode_t *ReadOldUserFile(char pre)
/*
 * Reads original res/<pre>uMMRES file, and returns filled-in ATL_mmnode_t
 */
{
   char ln[1024], ln2[1024];
   double mf;
   int id, nb, iflag, i;
   ATL_mmnode_t *mmp;
   FILE *fp;

   mmp = GetMMNode();
   sprintf(ln, "res/%cuMMRES", pre);
   fp = fopen(ln, "r");
   assert(fp);
   assert(fgets(ln, 1024, fp));
   assert(fgets(ln, 1024, fp));
   sscanf(ln, "%d %d %lf", &id, &nb, &mf);
   fclose(fp);
   mmp->ID = id;
   mmp->mflop[0] = mf;
   mmp->mbB = mmp->nbB = mmp->kbB = nb;
   assert(GetUserCase(pre, id, &iflag, &i, &i, &i, &mmp->muladd, &mmp->lat,
                      &mmp->mu, &mmp->nu, &mmp->ku, ln, ln2,
                      &mmp->comp, &mmp->cflags));
   mmp->comp = DupString(mmp->comp);
   mmp->cflags = DupString(mmp->cflags);
   i = strlen(ln) + 7;
   mmp->rout= malloc(i * sizeof(char));
   assert(mmp->rout);
   mmp->auth = DupString(ln2);
   sprintf(mmp->rout, "CASES/%s", ln);
   if (iflag & 32)
   {
      SET_FLAG(mmp->flag, MMF_LDISKB, 0);
      SET_FLAG(mmp->flag, MMF_LDAB, 0);
   }
   if (iflag & 64)
      SET_FLAG(mmp->flag, MMF_MRUNTIME, 1);
   if (iflag & 128)
      SET_FLAG(mmp->flag, MMF_NRUNTIME, 1);
   if (iflag & 256)
      SET_FLAG(mmp->flag, MMF_KRUNTIME, 1);
   return(mmp);
}

void getBestOfSearches
(
   char pre,            /* precision prefix */
   int verb,            /* verbosity */
   ROUTNODE *esumb      /* list of search output files */
)
/*
 * Scopes all external search summary files, output res/<pre>eMMRES.sum
 * file describing best user/external kernel available.
 */
{
   ATL_mmnode_t *mmb, *mmp, *mmB;
   double mfB;
   ROUTNODE *ep;
   char ln[2048];

   mmb = ReadMMFileWithPath(pre, "res", "eMMRES.sum");
   if (mmb)    /* already have output file from previous run */
   {
/*
 *    Rerun external searches to force generation of any needed files
 */
      for (ep=esumb; ep; ep = ep->next)
      {
         sprintf(ln, "make %s\n", ep->rout);
         if (system(ln))
            fprintf(stderr, "EXTERNAL SEARCH FAILED: %s", ln);
      }
/*
 *    Retime best kernel, and rewrite output file if necessary
 */
      if (mmb->mflop[0] <= 0.0)
      {
         TimeAllMMKernels(0, verb, 0, mmb, pre, 0, 0, 0, 1, 0, 0);
         WriteMMFileWithPath(pre, "res", "eMMRES.sum", mmb);
      }
/*
 *    Echo best case to screen and return
 */
      fprintf(stdout, "\nBEST OF USER CASES AND EXTERNAL SEARCHES:\n");
      PrintMMNodes(stdout, mmb);
      KillAllMMNodes(mmb);
      return;
   }
   mmb = mmp = ReadOldUserFile(pre);   /* get best user case */
   assert(mmb);
   for (ep=esumb; ep; ep = ep->next)
   {
      sprintf(ln, "make %s\n", ep->rout);
      if (system(ln))
      {
         fprintf(stderr, "EXTERNAL SEARCH FAILED: %s", ln);
         continue;
      }
      mmp->next = ReadMMFile(ep->rout);
      if (!mmp->next)
      {
         fprintf(stderr, "EXTERNAL SEARCH FAILED: %s", ep->rout);
         continue;
      }
/*
 *    If MFLOPs not set, time and the write back out
 */
      if (mmp->next->mflop[0] <= 0.0)
      {
         TimeAllMMKernels(0, verb, 1, mmp->next, pre, 0, 0, 0, 1, 0, 0);
         WriteMMFile(ep->rout, mmp->next);
      }
/*
 *    Right now, should always be one kernel, but if there are multiple,
 *    they all get considered, so find the last one as link in the queue
 *    to be compared
 */
      while (mmp->next)
         mmp = mmp->next;
   }
/*
 * Nuke kernels that don't pass basic tests, time survivors, in mflop[1]
 * NOTE: could time BETA cases separately and build best-of-breed, but
 *       this would require that kernels have same NB, and be kind of
 *       messy, so for now assume perf follows BETA=1 case
 */
   mmb = DelBadMMKernels(pre, verb, mmb);
   TimeAllMMKernels(1, verb, 1, mmb, pre, 0, 0, 0, 1, 0, 0);
/*
 * Pick fastest kernel whose NB <= 80
 */
   mfB = mmb->mflop[1];
   mmB = mmb;
   for (mmp=mmb->next; mmp; mmp = mmp->next)
   {
      if (mmp->nbB <= 80 && mfB < mmp->mflop[1])
      {
         mfB = mmp->mflop[1];
         mmB = mmp;
      }
   }
   mmB = CloneMMNode(mmB);
   KillAllMMNodes(mmb);
   WriteMMFileWithPath(pre, "res", "eMMRES.sum", mmB);
   fprintf(stdout, "\nBEST OF USER CASES AND EXTERNAL SEARCHES:\n");
   PrintMMNodes(stdout, mmB);
   KillAllMMNodes(mmB);
}

int main(int nargs, char **args)
{
   int i, nb0, verb=1, FNB;
   enum CLEAN_WHICH which;
   char pre, ln[16];
   ROUTNODE *esumb;

   esumb = GetFlags(nargs, args, &pre, &nb0, &FNB, &which);
   if (FNB)
   {
      i = FindBestUserFNB(pre, FNB);
      printf("nb=%d, i=%d\n", FNB, i);
   }
   else if (nb0 <= 0)
   {
      GetuMMRES(pre);
      getBestOfSearches(pre, verb, esumb);
   }
   else if (which == CleanNot)
   {
      sprintf(ln, "res/%cuMMRES", pre);
      if (FileExists(ln))
         GetuMMRES(pre);
      else
         FindUMM(pre, nb0);
      getBestOfSearches(pre, verb, esumb);
   }
   else
   {
      getBestOfSearches(pre, verb, esumb);
      FindUClean(pre, nb0, which);
   }
   KillAllRoutNodes(esumb);
   fprintf(stderr, "SUCCESSFUL FINISH FOR %s\n\n", args[0]);
   return(0);
}
