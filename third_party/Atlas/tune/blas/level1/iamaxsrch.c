#include "atlas_misc.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>

#define AlphaX 2
typedef struct FileNode FILENODE;
struct FileNode
{
   int ID, incX, incY, alpha, beta;
   char *rout, *auth, *cc, *ccflags;
   FILENODE *next;
};
typedef struct PerfNode PERFNODE;
struct PerfNode
{
   int incX, incY, alpha, beta;
   double mf;
   FILENODE *myroot;
   PERFNODE *next;
};

typedef struct QNode QNODE;
struct QNode
{
   FILENODE *fptr;
   PERFNODE *perf;
   QNODE *next;
};

void KillAllFN(FILENODE *fp)
{
   FILENODE *fn;
   while(fp)
   {
      fn = fp->next;
      if (fp->rout) free(fp->rout);
      if (fp->auth) free(fp->auth);
      if (fp->cc) free(fp->cc);
      if (fp->ccflags) free(fp->ccflags);
      free(fp);
      fp = fn;
   }
}

void PrintFN(FILENODE *fp)
{
   fprintf(stdout, "  ID  incX  incY  alpha  beta  ROUT\n");
   fprintf(stdout, "====  ====  ====  =====  ====  =============\n");
   while(fp)
   {
      fprintf(stdout, "%4d  %4d  %4d  %4d  %4d  %s\n",
              fp->ID, fp->incX, fp->incY, fp->alpha, fp->beta, fp->rout);
      fp = fp ->next;
   }
   fprintf(stdout, "\n");
}

FILENODE *AllocFN(int ID, int incX, int incY, int alpha, int beta,
                  char *rout, char *auth, char *cc, char *ccflags)
{
   FILENODE *fp;
   int i;

   fp = malloc(sizeof(FILENODE));
   assert(fp);

   fp->ID = ID;
   fp->incX = incX;
   fp->incY = incY;
   fp->alpha = alpha;
   fp->beta  = beta;
   if (rout)
   {
      i = strlen(rout)+1;
      fp->rout = malloc(i*sizeof(char));
      assert(fp->rout);
      strcpy(fp->rout, rout);
   }
   else fp->rout = NULL;

   if (auth)
   {
      i = strlen(auth)+1;
      fp->auth = malloc(i*sizeof(char));
      assert(fp->auth);
      strcpy(fp->auth, auth);
   }
   else fp->auth = NULL;

   if (cc)
   {
      i = strlen(cc)+1;
      fp->cc = malloc(i*sizeof(char));
      assert(fp->cc);
      strcpy(fp->cc, cc);
   }
   else fp->cc = NULL;
   if (ccflags)
   {
      i = strlen(ccflags)+1;
      fp->ccflags = malloc(i*sizeof(char));
      assert(fp->ccflags);
      strcpy(fp->ccflags, ccflags);
   }
   else fp->ccflags = NULL;

   fp->next = NULL;
   return(fp);
}

QNODE *AllocQN(FILENODE *fp)
{
   QNODE *qp;
   qp = malloc(sizeof(QNODE));
   assert(qp);
   qp->fptr = fp;
   qp->perf = NULL;
   qp->next = NULL;
   return(qp);
}

PERFNODE *AllocPN(int incX, int incY, int alpha, double mf, FILENODE *fp)
{
   PERFNODE *pp;
   pp = malloc(sizeof(PERFNODE));
   assert(pp);
   pp->incX = incX;
   pp->incY = incY;
   pp->mf = mf;
   pp->myroot = fp;
   pp->next = NULL;
   return(pp);
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

void FixFlags(char *ln)
{
   int i, iret=0;
   for(i=0; ln[i]; i++);
   if (i)
   {
      for(i--; isspace(ln[i]); i--) ln[i] = '\0';
   }
}

FILENODE *ReadFile(char pre)
{
   FILE *fpin;
   FILENODE *fbase, *fn, *fp=NULL;
   char ln[512], rout[256], auth[256], cc[256], ccflags[512];
   char *pcc, *pccflags;
   int i, n, ID, alpha, beta, incX, incY;

   sprintf(ln, "%ciamax.dsc", pre);
   fpin = fopen(ln, "r");
   assert(fpin);
   assert(fgets(ln, 512, fpin) != NULL);
   sscanf(ln, " %d", &n);
   for (i=0; i < n; i++)
   {
      assert(fgets(ln, 512, fpin) != NULL);
      assert(sscanf(ln, " %d %d %s \"%[^\"]", &ID, &incX, rout, auth) == 4);
      incY = 0;
      alpha = beta = AlphaX;
      if  (LineIsCont(ln))
      {
         assert(fgets(cc, 256, fpin) != NULL);
         assert(fgets(ccflags, 512, fpin) != NULL);
         FixFlags(cc);
         FixFlags(ccflags);
         pcc = cc;
         pccflags = ccflags;
      }
      else pcc = pccflags = NULL;
      fn = AllocFN(ID, incX, incY, alpha, beta, rout, auth, pcc, pccflags);
      if (fp) fp->next = fn;
      else fbase = fn;
      fp = fn;
   }
   PrintFN(fbase);
   return(fbase);
}

double GetMflop0(char pre, int ID, char *rout, int incX, int incY,
                 int alpha, int beta, char *cc, char *ccflags)
{
   double dalp=2.0;
   FILE *fpin;
   char ln[1024], fnam[128];
   double t0, t1=0.0;
   int i, N;

   N = 5000;
   sprintf(fnam, "res/%cIAMAX%d_a%db%dx%dy%d", pre, ID, alpha, beta,
           incX, incY);
   fpin = fopen(fnam, "r");
   if (fpin == NULL)
   {
      i = sprintf(ln, "make %ciamaxcase N=%d urout=%s fout=\"-f %s\" ",
                  pre, N, rout, fnam);
      if (cc) i += sprintf(ln+i, "%cUCC=\"%s\" ", pre, cc);
      if (ccflags) i += sprintf(ln+i, "%cUCCFLAGS=\"%s\" ", pre, ccflags);
      i += sprintf(ln+i, "opt=\"");
      if (incX != 1) i += sprintf(ln+i, "-X %d ", incX);
      i += sprintf(ln+i, "\"\n");
      fprintf(stdout, "TIM: %s", ln);
      if (system(ln) != 0) return(-1.0);
      fpin = fopen(fnam, "r");
      assert(fpin);
   }
   assert(fgets(ln, 512, fpin) != NULL);
   assert(sscanf(ln, " %lf", &t0) == 1);
   return(t0);
}

double GetMflop1(char pre, QNODE *qp, int incX, int incY, int alpha, int beta)
{
   PERFNODE *pp;
   double mf;

   if (qp->fptr->incX != incX && qp->fptr->incX != 0) return(-1.0);
   if (qp->fptr->incY != incY && qp->fptr->incY != 0) return(-1.0);
   if (qp->fptr->alpha != alpha && qp->fptr->alpha != AlphaX) return(-1.0);
   if (qp->fptr->beta != beta && qp->fptr->beta != AlphaX) return(-1.0);
   for(pp=qp->perf; pp; pp = pp->next)
      if (pp->incX == incX && pp->incY == incY && pp->alpha == alpha) break;
   if (pp) return(pp->mf);
   mf = GetMflop0(pre, qp->fptr->ID, qp->fptr->rout, incX, incY, alpha, beta,
                  qp->fptr->cc, qp->fptr->ccflags);
   if (mf < 0.0) return(-1.0);
   pp = AllocPN(incX, incY, alpha, mf, qp->fptr);
   pp->next = qp->perf;
   qp->perf = pp;
   return(pp->mf);
}

double GetMflop(char pre, QNODE *qp, int incX, int incY, int alpha, int beta)
/*
 * inc == 0, tests three possible values, and gives back combined score
 */
{
   int ix[3], iy[3], nx=1, ny=1, i, j, k;
   double t1, t0=0.0;

   if (incX == 0)
   {
      nx = 3;
      ix[0] = 1;
      ix[1] = 3;
      ix[2] = 2;
   }
   else ix[0] = incX;
   iy[0] = incY;
   for (k=i=0; i < nx; i++)
   {
      for (j=0; j < ny; j++, k++)
      {
         t1 = GetMflop1(pre, qp, ix[i], iy[j], alpha, beta);
         if (t1 < 0.0) return(-1.0);
         t0 += t1;
         if (i==0 && j==0) t0 *= 3.0; /* give inc = 1 case more weight */
      }
   }
   return(t0 / (nx*ny+2));
}

void PrintTable(int n, int *ix, int *iy, int *ia, int *ib, FILENODE **bp)
{
   int i;
   fprintf(stdout, "   incX  incY  alpha  beta    ID  ROUTINE\n");
   fprintf(stdout, "   ====  ====  =====  ====  ====  ==================\n");
   for (i=0; i < n; i++)
   {
      if (bp[i])
         fprintf(stdout, "   %4d  %4d  %5d  %4d  %4d  %s\n",
                 ix[i], iy[i], ia[i], ib[i], bp[i]->ID, bp[i]->rout);
      else
         fprintf(stdout, "   %4d  %4d  %5d  %4d  %4d  %s\n",
                 ix[i], iy[i], ia[i], ib[i], -1, "UNKNOWN");
   }
   fprintf(stdout, "\n");
}

int GetCombos(FILENODE *fptr, int *ix, int *iy, int *ia, int *ib, FILENODE **bp)
/*
 * finds all combos of incX, incY, alpha and beta returns # of unique combos
 */
{
   FILENODE *fp, *pp;
   int i=1, j;

   if (!fptr) return(0);
/*
 * Always include general case
 */
   *ix = 0;
   *iy = 0;
   *ia = AlphaX;
   *ib = AlphaX;
   *bp = NULL;
   for (fp=fptr; fp; fp = fp->next)
   {
      for (j=0; j < i; j++)
         if (fp->incX == ix[j] && fp->incY == iy[j] && fp->alpha == ia[j] &&
             fp->beta == ib[j]) break;
      if (j == i) /* not already there */
      {
         ix[i] = fp->incX;
         iy[i] = fp->incY;
         ia[i] = fp->alpha;
         ib[i] = fp->beta;
         bp[i++] = NULL;
      }
   }
   PrintFN(fptr);
   PrintTable(i, ix, iy, ia, ib, bp);
   return(i);
}

QNODE *MakeQ(FILENODE *fbase)
{
   QNODE *qbase, *qp;
   FILENODE *fp;

   if (!fbase) return(NULL);
   qp = qbase = AllocQN(fbase);
   for (fp=fbase->next; fp; fp = fp->next)
   {
      qp->next = AllocQN(fp);
      qp = qp->next;
   }
   return(qbase);
}

void BruteTime(char pre, int n, FILENODE *fbase, int *ix, int *iy,
               int *ia, int *ib, FILENODE **bp)
/*
 * finds best rout for each combo; gave up on elegance and just BFed it
 */
{
   int i;
   QNODE *qbase, *qp;
   double mf, mfmax=0.0;

   qbase = MakeQ(fbase);
   for (i=0; i < n; i++)
   {
      mfmax = 0.0;
      bp[i] = NULL;
      for (qp=qbase; qp; qp = qp->next)
      {
         mf = GetMflop(pre, qp, ix[i], iy[i], ia[i], ib[i]);
         if (mf > mfmax)
         {
            mfmax = mf;
            bp[i] = qp->fptr;
         }
      }
   }
}

FILENODE *FindGen(int n, int *ix, int *iy, int *ia, int *ib, FILENODE **bp)
/*
 * finds node with incX=0, incY=0, alpha=X, beta=X
 */
{
   int i;
   for (i=0; i < n; i++)
      if (ix[i] == 0 && iy[i] == 0 && ia[i] == AlphaX && ib[i] == AlphaX)
         return(bp[i]);
   return(NULL);
}

int KillSpecCases(int n, int *ix, int *iy, int *ia, int *ib, FILENODE **bp)
/*
 * Kills all special cases that are actually handled by general case
 */
{
   FILENODE *gp;
   int i, j;
   gp = FindGen(n, ix, iy, ia, ib, bp);
   if (gp == NULL)
   {
      fprintf(stderr, "NO GENERAL CASE SURVIVED!!  ABORTING!!\n");
      exit(-1);
   }
   for (i=0; i < n; i++)
   {
      if (bp[i] == gp)
      {
         if (ix[i] != 0) bp[i] = NULL;
      }
   }
/*
 * eliminate entries that are NULL
 */
   for (j=i=0; i < n; i++)
   {
      if (bp[i] != NULL)
      {
         ix[j] = ix[i];
         iy[j] = iy[i];
         ia[j] = ia[i];
         ib[j] = ib[i];
         bp[j] = bp[i];
         j++;
      }
   }
   return(j);
}

void SwapCases(int i, int j, int *ix, int *iy, int *ia, int *ib, FILENODE **bp)
{
   int itmp;
   FILENODE *ftmp;
   if (i != j)
   {
      ftmp = bp[i];
      bp[i] = bp[j];
      bp[j] = ftmp;
      itmp = ix[i];
      ix[i] = ix[j];
      ix[j] = itmp;
      itmp = iy[i];
      iy[i] = iy[j];
      iy[j] = itmp;
      itmp = ia[i];
      ia[i] = ia[j];
      ia[j] = itmp;
      itmp = ib[i];
      ib[i] = ib[j];
      ib[j] = itmp;
   }
}

void SortCases(int n, int *ix, int *iy, int *ia, int *ib, FILENODE **bp)
/*
 * sorts (BFI sort) cases so they are in this order:
 * incX=1, incY=1, alpha=X, beta=X case first
 * other specific incX & incY cases next
 * specific incY, general incX cases next
 * specific incX, general incY cases next
 * general case last
 */
{
/*
 * put specific incX and incY cases first
 */
   int i, j, itmp;
   FILENODE *ftmp;
   for (i=0; i < n; i++)
   {
      if (ix[i] != 0) continue;  /* don't stop until we find gen case */
      for (j=i+1; j < n; j++) /* search for other spec cases */
         if (ix[j] != 0) break;  /* found another case */
      if (j >= n) break; /* no more cases */
      SwapCases(i, j, ix, iy, ia, ib, bp);
   }
/*
 * General case will now have been forced to last by previous sorts
 */
}

FILENODE *KillFN(FILENODE *fbase, FILENODE *fk)
/*
 * Finds fk in Q, and removes it
 */
{
   FILENODE *fp, *pr=NULL;
   if (fk == fbase)
   {
      fp = fbase->next;
      fbase->next = NULL;
      KillAllFN(fbase);
      fbase = fp;
   }
   else
   {
      for (fp=fbase; fp != fk && fp; fp = fp->next) pr = fp;
      assert(fp && pr);
      pr->next = fp->next;
      fp->next = NULL;
      KillAllFN(fp);
   }
   return(fbase);
}

FILENODE *TstFile(char pre, FILENODE *fbase, int *N)
{
   FILENODE *fp, *fn;
   char ln[2048];
   int i, k=0;

   for (fp=fbase; fp; fp = fn)
   {
      fn = fp->next;
      i = sprintf(ln, "make %ciamaxtest urout=%s opt=\"", pre, fp->rout);
      if (fp->incX == 0) i += sprintf(ln+i, " -X 4 1 -1 2 -3");
      i += sprintf(ln+i, "\" ");
      if (fp->cc) i += sprintf(ln+i, "%cUCC=\"%s\" ", pre, fp->cc);
      if (fp->ccflags) i += sprintf(ln+i, "%cUCCFLAGS=\"%s\" ",pre,fp->ccflags);
      sprintf(ln+i, "\n");
      fprintf(stderr, "TST: %s", ln);
      #if 1
         if (system(ln)) fbase = KillFN(fbase, fp);
         else k++;
      #else
         k++;
      #endif
   }
   *N = k;
   return(fbase);
}

int NumOfAlpha(int alpha, int n, int *ia)
/*
 * returns number of occurences of alpha in ia
 */
{
   int i, j;
   for (i=j=0; i < n; i++) if (ia[i] == alpha) j++;
   return(j);
}

char *GetNam(char pre, int alpha, int beta, int incX, int incY)
{
   static char nam[128];
   char sx, sy, ca, cb;
   char *rout="iamax";

   if (alpha == 1) ca = '1';
   else if (alpha == -1) ca = 'n';
   else if (alpha == 0 && (pre=='c' || pre=='z')) ca = 'r';
   else ca = 'X';
   if (beta == 1) cb = '1';
   else if (beta == -1) cb = 'n';
   else if (beta == 0 && (pre=='c' || pre=='z')) cb = 'r';
   else cb = 'X';
   if (incX < 0) { sx = 'n'; incX = -incX; }
   else sx = 'p';
   if (incY < 0) { sy = 'n'; incY = -incY; }
   else sy = 'p';
   sprintf(nam, "ATL_%c%s_x%c%dy%c%da%cb%c",
           pre, rout, sx, incX, sy, incY, ca, cb);
   return(nam);
}

void MangleIncs(FILE *fpout, char *spc, char pre)
/*
 * Makes sure incX is positive
 */
{
   assert(spc[0] == ' ' && spc[1] == ' ' && spc[2] == ' ');
   fprintf(fpout, "%sif (incX > 0) incx = incX;\n", spc);
   fprintf(fpout, "%selse if (incX < 0)\n%s{\n", spc, spc);
   fprintf(fpout, "%s   X += ((N-1)SHIFT) * incX;\n", spc);
   fprintf(fpout, "%s   incx = -incX;\n", spc);
   fprintf(fpout, "%s}\n%selse return(0);\n", spc, spc);
}

void GenMakefile(char pre, int n, int *ix, int *iy, int *ia, int *ib,
                 FILENODE **bp)
{
   char *cc, *ccflags, *nam, *typ="";
   char ln[32];
   char *rout = "iamax";
   int i;
   FILE *fpout;

   sprintf(ln, "GEN/Make_%c%s", pre, rout);
   fpout = fopen(ln, "w");
   assert(fpout);
   if (pre == 's') typ="SREAL";
   else if (pre == 'd') typ="DREAL";
   else if (pre == 'c') typ="SCPLX";
   else if (pre == 'z') typ="DCPLX";

   fprintf(fpout, "include Make.inc\n\n");

   fprintf(fpout, "obj = ATL_%c%s.o", pre, rout);
   for (i=0; i < n; i++)
      fprintf(fpout, " %s.o", GetNam(pre, ia[i], ib[i], ix[i], iy[i]));
   fprintf(fpout, "\n\n");

   fprintf(fpout, "all : lib\n%clib : lib\nlib : %c%s.grd\n\n", pre, pre, rout);

   fprintf(fpout, "%c%s.grd : $(obj)\n", pre, rout);
   fprintf(fpout, "\t$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(obj)\n");
   fprintf(fpout, "\t$(RANLIB) $(ATLASlib)\n");
   fprintf(fpout, "\ttouch %c%s.grd\n", pre, rout);
   fprintf(fpout, "\n");

   sprintf(ln, "ATL_%c%s", pre, rout);
   fprintf(fpout, "%s.o : %s.c\n", ln, ln);
   fprintf(fpout, "\t$(ICC) $(ICCFLAGS) -D%s -o $@ -c %s.c\n", typ, ln);
   for (i=0; i < n; i++)
   {
      nam = GetNam(pre, ia[i], ib[i], ix[i], iy[i]);
      cc = bp[i]->cc;
      ccflags = bp[i]->ccflags;
      if (!cc) cc = (pre == 's' || pre == 'c') ? "$(SKC)" : "$(DKC)";
      if (!ccflags)
         ccflags = (pre == 's' || pre == 'c') ? "$(SKCFLAGS)":"$(DKCFLAGS)";
      fprintf(fpout, "%s.o : %s.c\n", nam, nam);
      fprintf(fpout, "\t%s %s $(CDEFS) -D%s -o $@ -c %s.c\n",
              cc, ccflags, typ, nam);
   }
   fclose(fpout);
}

void GenMainRout(char pre, int n, int *ix, int *iy, int *ia, int *ib,
                 FILENODE **bp)
{
   int i, j, NeedElse=0;
   FILENODE *gp;
   FILE *fpout;
   char *els="else if", *ifs ="if";
   char *spcs="                  ";
   char *spc = spcs+18;
   char ln[64];
   char *dargs="const int, const TYPE*, const int";
   char *args="N, X, incx";

   sprintf(ln, "GEN/ATL_%ciamax.c", pre);
   fpout = fopen(ln, "w");
   assert(fpout);
   fprintf(fpout, "#include \"atlas_misc.h\"\n\n");
   gp = FindGen(n, ix, iy, ia, ib, bp);
   assert(gp);
/*
 * prototype all routines
 */
   for (i=0; i < n; i++)
   {
      fprintf(fpout, "int %s(const int, const TYPE*, const int);\n",
              GetNam(pre, ia[i], ib[i], ix[i], iy[i]));
   }


   fprintf(fpout, "\nint ATL_i%camax(const int N, const TYPE *X, const int incX)\n{\n", pre);
   fprintf(fpout, "   int incx;\n\n");
   fprintf(fpout, "   if (N > 0)\n   {\n");
   spc -= 6;
   MangleIncs(fpout, spc, pre);
/*
 * Handle specific incX
 */
   for (i=0; i < n-1; i++)
   {
      fprintf(fpout, "%s%s (incx == %d)\n", spc, ifs, ix[i]);
      fprintf(fpout, "%s   return(%s(%s));\n", spc,
              GetNam(pre, ia[i], ib[i], ix[i], iy[i]), args);
      ifs = els;
      NeedElse = 1;
   }

   if (NeedElse)
   {
      fprintf(fpout, "%selse\n", spc);
      spc -= 3;
   }
   fprintf(fpout, "%sreturn(%s(N, X, incx));\n", spc,
           GetNam(pre, AlphaX, AlphaX, 0, 0));
   fprintf(fpout, "   }\n");
   fprintf(fpout, "   return(0);\n");
   fprintf(fpout, "}\n");
   fclose(fpout);
}

void DumpFile(char *fnam, FILE *fpout)
{
   FILE *fpin;
   char ln[512];
   sprintf(ln, "IAMAX/%s", fnam);
   fpin = fopen(ln, "r");
   assert(fpin);
   while(fgets(ln, 512, fpin)) fputs(ln, fpout);
   fclose(fpin);
}

void GenFiles(char pre, int n, int *ix, int *iy, int *ia, int *ib,
              FILENODE **bp)
/*
 * Generates all needed files
 */
{
   int i;
   char *nam, *typ;
   char ln[128];
   FILE *fpout;

   GenMainRout(pre, n, ix, iy, ia, ib, bp);
   GenMakefile(pre, n, ix, iy, ia, ib, bp);
   for (i=0; i < n; i++)
   {
      nam = GetNam(pre, ia[i], ib[i], ix[i], iy[i]);
      sprintf(ln, "GEN/%s.c", nam);
      fpout = fopen(ln, "w");
      assert(fpout);
      fprintf(fpout, "#define ATL_UIAMAX %s\n\n", nam);
      DumpFile(bp[i]->rout, fpout);
      fclose(fpout);
   }
}

void CreateSumm(char pre, int n, int *ix, int *iy, int *ia, int *ib,
                FILENODE **bp)
/*
 * Creates summary file of form:
<ncases>
<ID> <alpha> <beta> <incX> <incY> <rout> <auth>
 */
{
   int i;
   char ln[64];
   FILE *fpout;

   sprintf(ln, "res/%cIAMAX_SUMM", pre);
   fpout = fopen(ln, "w");
   assert(fpout);

   fprintf(fpout, "%d\n", n);
   for (i=0; i < n; i++)
   {
      fprintf(fpout, "%3d %3d %3d %3d %3d %s \"%s\"\n",
              bp[i]->ID, ia[i], ib[i], ix[i], iy[i], bp[i]->rout, bp[i]->auth);
   }

   fclose(fpout);
}

void AssBest(FILENODE *fbase, int n, int *id, FILENODE **bp)
/*
 * Associates best file pointers with filenodes based on ID
 */
{
   int i, ID;
   FILENODE *fp;

   for (i=0; i < n; i++)
   {
      ID = id[i];
      for (fp=fbase; fp; fp = fp->next) if (fp->ID == ID) break;
      assert(fp);
      bp[i] = fp;
   }
}

FILENODE *GetSumm(char pre, int *N, int **IX, int **IY, int **IA,  int **IB,
                  FILENODE ***BP)
{
   FILENODE *fbase, **bp;
   int i, n, *ix, *iy, *ia, *ib, *id;
   char ln[512];
   FILE *fpin;

   *N = 0;
   sprintf(ln, "res/%cIAMAX_SUMM", pre);
   fpin = fopen(ln, "r");
   if (fpin == NULL) return(NULL);
   fbase = ReadFile(pre);

   assert( fgets(ln, 512, fpin) != NULL );
   sscanf(ln, " %d", &n);
   assert(n > 0);

   ix = malloc(5*n*sizeof(int));
   assert(ix);
   iy = ix + n;
   ia = iy + n;
   ib = ia + n;
   id = ib + n;
   bp = malloc(n*sizeof(FILENODE*));
   assert(bp);

   for (i=0; i < n; i++)
   {
      assert( fgets(ln, 512, fpin) != NULL );
      assert(sscanf(ln, " %d %d %d %d %d", id+i, ia+i, ib+i, ix+i, iy+i) == 5);
      bp[i] = NULL;
   }
   fclose(fpin);
   AssBest(fbase, n, id, bp);

   *N = n;
   *IX = ix;
   *IY = iy;
   *IA = ia;
   *IB = ib;
   *BP = bp;
   return(fbase);
}

void DoIt(char pre)
{
   FILENODE *fbase, **bp;
   int n;
   int *ix, *iy, *ia, *ib;

   fbase = ReadFile(pre);
   fbase = TstFile(pre, fbase, &n);  /* get rid of nodes that don't work */
   ix = malloc(4*n*sizeof(int));
   assert(ix);
   bp = malloc(n*sizeof(FILENODE*));
   assert(bp);
   iy = ix + n;
   ia = iy + n;
   ib = ia + n;
   n = GetCombos(fbase, ix, iy, ia, ib, bp);
   BruteTime(pre, n, fbase, ix, iy, ia, ib, bp);
   n = KillSpecCases(n, ix, iy, ia, ib, bp);
   fprintf(stdout, "%d:\n", __LINE__);
   PrintTable(n, ix, iy, ia, ib, bp);
   SortCases(n, ix, iy, ia, ib, bp);
   fprintf(stdout, "%d:\n", __LINE__);
   PrintTable(n, ix, iy, ia, ib, bp);
   CreateSumm(pre, n, ix, iy, ia, ib, bp);
   KillAllFN(fbase);
   free(bp);
   free(ix);
}

void GoToTown(char pre)
{
   FILENODE *fptr, **bp;
   int n, *ix, *iy, *ia, *ib;

   fptr = GetSumm(pre, &n, &ix, &iy, &ia, &ib, &bp);
   if (!fptr)
   {
      DoIt(pre);
      fptr = GetSumm(pre, &n, &ix, &iy, &ia, &ib, &bp);
      assert(fptr);
   }
   fprintf(stdout, "%d:\n", __LINE__);
   PrintTable(n, ix, iy, ia, ib, bp);
   GenFiles(pre, n, ix, iy, ia, ib, bp);

   KillAllFN(fptr);
   free(bp);
   free(ix);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "\nUSAGE: %s -p <pre>\n\n", nam);
   exit(-1);
}

char GetFlags(int nargs, char **args)
{
   char ch;
   if (nargs < 2) return('d');
   if (args[1][0] == '-')
   {
      if (nargs < 3) PrintUsage(args[0]);
      ch = args[2][0];
   }
   else ch = args[1][0];

   switch(ch)
   {
   case 's':
   case 'd':
   case 'c':
   case 'z':
       break;
   case 'S':
      ch = 's';
      break;
   case 'D':
      ch = 'd';
      break;
   case 'C':
      ch = 'c';
      break;
   case 'Z':
      ch = 'z';
      break;
   default:
      PrintUsage(args[0]);
   }
   return(ch);
}
int main(int nargs, char **args)
{
   char pre;
   pre = GetFlags(nargs, args);
   GoToTown(pre);
   return(0);
}
