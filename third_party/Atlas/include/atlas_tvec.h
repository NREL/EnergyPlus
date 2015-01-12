#ifndef ATL_TVEC_H
   #define ATL_TVEC_H

#include "atlas_genparse.h"

typedef struct ATL_TVEC ATL_tvec_t;
struct ATL_TVEC
{
   void *vp;            /* pointer to vector, type in pre */
   char *name;          /* name of vector (eg, "MFLOP" or "N") */
   ATL_tvec_t *next;
   int  N;              /* # of elements in vector (inclucing reps) */
   int nrep;            /* # of repititions in timings */
   char pre;            /* double (d), string (s), char (c), integer (i) */
};

ATL_tvec_t *ATL_GetTvec(char *name, int N, int nrep, char pre)
{
   ATL_tvec_t *tp;
   int i;

   tp = malloc(sizeof(ATL_tvec_t));
   i = strlen(name);

   tp->name = malloc(sizeof(char)*(strlen(name)+1));
   strcpy(tp->name, name);
   tp->N = N;
   tp->nrep = nrep;
   tp->next = NULL;
   tp->pre = pre;
   i = N;
   if (pre == 's')
      i *= sizeof(char*);
   else if (pre == 'd')
      i *= sizeof(double);
   else
      i *= (pre == 'i') ? sizeof(int) : sizeof(char);
   tp->vp = malloc(N*sizeof(double));
   assert(tp->vp);
   return(tp);
}

ATL_tvec_t *ATL_KillThisTvec(ATL_tvec_t *tp)
{
   ATL_tvec_t *retp=NULL;

   if (tp)
   {
      if (tp->pre == 's')
      {
         char **sp = tp->vp;
         int i;

         for (i=0; i < tp->N; i++)
            free(sp[i]);
         free(sp);
      }
      else
         free(tp->vp);
      free(tp->name);
      retp = tp->next;
      free(tp);
   }
   return(retp);
}

void ATL_KillAllTvecs(ATL_tvec_t *tq)
{
   while (tq)
      tq = ATL_KillThisTvec(tq);
}

void ATL_ReadDoubleTvec(FILE *fpin, int N, double *dp)
{
   int i;

   for (i=0; i < N; i++)
      assert(fscanf(fpin, "%lf\n", dp+i) == 1);
}

void ATL_ReadIntTvec(FILE *fpin, int N, int *ip)
{
   int i;

   for (i=0; i < N; i++)
      assert(fscanf(fpin, "%d\n", ip+i) == 1);
}

void ATL_ReadCharTvec(FILE *fpin, int N, char *cp)
{
   int i;

   for (i=0; i < N; i++)
      assert(fscanf(fpin, "%c\n", cp+i) == 1);
}

void ATL_ReadStringTvec(FILE *fpin, int N, char **sa)
{
   int i;

   for (i=0; i < N; i++)
   {
      char *sp;
      int n, j;
      char ln[512];

      assert(fgets(ln, 512, fpin));
      n = strlen(ln) + 1;
      sa[i] = sp = malloc(sizeof(char)*n);
      assert(sp);
      for (j=0; j < n; j++)
         sp[j] = ln[j];
   }
}

ATL_tvec_t *ATL_ReadTvec(FILE *fpin, int nrep)
{
   int N;
   char nm[64], pre;
   ATL_tvec_t *tp;

   assert(fscanf(fpin, "%s\n", nm) == 1);
   assert(fscanf(fpin, "%d %c", &N, &pre) == 2);
   tp = ATL_GetTvec(nm, N, nrep, pre);
   if (pre == 'd')
     ATL_ReadDoubleTvec(fpin, N, tp->vp);
   else if (pre == 'i')
     ATL_ReadIntTvec(fpin, N, tp->vp);
   else if (pre == 'c')
     ATL_ReadCharTvec(fpin, N, tp->vp);
   else /* if (pre == 's') */
     ATL_ReadStringTvec(fpin, N, tp->vp);
   return(tp);
}

ATL_tvec_t *ATL_ReadTvecFile(FILE *fpin, char **cmnt, int *nvec, int *nrep)
/*
 * Reads an entire timing vector file.
 * RETURNS: linked list of timing vectors
 */
{
   int i, n, nr;
   char ln[512];
   ATL_tvec_t *tb, *tp;

   assert(fgets(ln, 512, fpin));
   n = strlen(ln);
   while (n > 0 && isspace(ln[n-1]))
      ln[--n] = '\0';;
   *cmnt = malloc(sizeof(char)*n);
   assert(*cmnt);
   strcpy(*cmnt, ln+1);
   assert(fscanf(fpin, " %d %d\n", nvec, nrep) == 2);

   n = *nvec;
   nr = *nrep;
   if (n < 1)
      return(NULL);

   tb = tp = ATL_ReadTvec(fpin, nr);
   for (i=1; i < n; i++)
   {
      tp->next = ATL_ReadTvec(fpin, nr);
      tp = tp->next;
   }
   return(tb);
}

void ATL_WriteTvec(FILE *fpout, ATL_tvec_t *tp)
/*
 * Write a single timing vector to the stream fpout
 */
{
   fprintf(fpout, "%s\n", tp->name);
   fprintf(fpout, "%d %c\n", tp->N, tp->pre);
   if (tp->pre == 'd')
   {
      double *dp = tp->vp;
      const int n = tp->N;
      int i;
      for (i=0; i < n; i++)
         fprintf(fpout, "%le\n", dp[i]);
   }
   else if (tp->pre == 'i')
   {
      int *ip = tp->vp;
      const int n = tp->N;
      int i;
      for (i=0; i < n; i++)
         fprintf(fpout, "%d\n", ip[i]);
   }
   else if (tp->pre == 'c')
   {
      char *cp = tp->vp;
      const int n = tp->N;
      int i;
      for (i=0; i < n; i++)
         fprintf(fpout, "%c\n", cp[i]);
   }
   else /* if (tp->pre == 's') */
   {
      char **sp = tp->vp;
      const int n = tp->N;
      int i;
      for (i=0; i < n; i++)
         fprintf(fpout, "%s\n", sp[i]);
   }
}

void ATL_WriteTvecs(FILE *fpout, ATL_tvec_t *tp)
/*
 * Writes out a queue of output vectors to a stream that has already had
 * the preample (name, nvec, nrep) written to it
 */
{
   while (tp)
   {
      ATL_WriteTvec(fpout, tp);
      tp = tp->next;
   }
}

void ATL_WriteTvecFile(FILE *fpout, char *cmnt, int nvec, int nrep,
                       ATL_tvec_t *tp)
/*
 * Writes the entire output file given a queue of timing vectors
 */
{
   int i;
   fprintf(fpout, "#%s\n", cmnt);
   fprintf(fpout, "%d %d\n", nvec, nrep);
   ATL_WriteTvecs(fpout, tp);
}

ATL_tvec_t *ATL_FindTvecByName(ATL_tvec_t *tb, char *name)
{
   ATL_tvec_t *tp;
   for (tp=tb; tp && strcmp(tp->name, name); tp = tp->next);
   return(tp);
}

void ATL_FillCombCHARVecUsingInts
(
   ATL_tvec_t *np,    /* combined vector */
   ATL_tvec_t *ip1,   /* 1st index array (we sort cp1 & cp2 on these ivecs) */
   ATL_tvec_t *ip2,   /* 2nd index array (we sort cp1 & cp2 on these ivecs) */
   ATL_tvec_t *cp1,   /* 1st array to be combined */
   ATL_tvec_t *cp2    /* 2st array to be combined */
)
{
   char *dn = np->vp;
   const char *d1 = cp1->vp, *d2 = cp2->vp;
   const int *s1 = ip1->vp, *s2 = ip2->vp;
   const int n = np->N, n1 = cp1->N, n2 = cp2->N;
   int ic, i1, i2;

   for (ic=i1=i2=0; ic < n; ic++)
   {
      if (i1 < n1)
      {
         if (i2 < n2)  /* both are available for comparison */
         {
            if (s1[i1] <= s2[i2])
               dn[ic] = d1[i1++];
            else
               dn[ic] = d2[i2++];
         }
         else
         {
            assert(i1 < n1);
            dn[ic] = d1[i1++];
         }
      }
      else
      {
         assert(i2 < n2);
         dn[ic] = d2[i2++];
      }
   }
}
void ATL_FillCombINTVecUsingInts
(
   ATL_tvec_t *np,    /* combined vector */
   ATL_tvec_t *ip1,   /* 1st index array (we sort cp1 & cp2 on these ivecs) */
   ATL_tvec_t *ip2,   /* 2nd index array (we sort cp1 & cp2 on these ivecs) */
   ATL_tvec_t *cp1,   /* 1st array to be combined */
   ATL_tvec_t *cp2    /* 2st array to be combined */
)
{
   int *dn = np->vp;
   const int *d1 = cp1->vp, *d2 = cp2->vp;
   const int *s1 = ip1->vp, *s2 = ip2->vp;
   const int n = np->N, n1 = cp1->N, n2 = cp2->N;
   int ic, i1, i2;

   for (ic=i1=i2=0; ic < n; ic++)
   {
      if (i1 < n1)
      {
         if (i2 < n2)  /* both are available for comparison */
         {
            if (s1[i1] <= s2[i2])
               dn[ic] = d1[i1++];
            else
               dn[ic] = d2[i2++];
         }
         else
         {
            assert(i1 < n1);
            dn[ic] = d1[i1++];
         }
      }
      else
      {
         assert(i2 < n2);
         dn[ic] = d2[i2++];
      }
   }
}
void ATL_FillCombDOUBLEVecUsingInts
(
   ATL_tvec_t *np,    /* combined vector */
   ATL_tvec_t *ip1,   /* 1st index array (we sort cp1 & cp2 on these ivecs) */
   ATL_tvec_t *ip2,   /* 2nd index array (we sort cp1 & cp2 on these ivecs) */
   ATL_tvec_t *cp1,   /* 1st array to be combined */
   ATL_tvec_t *cp2    /* 2st array to be combined */
)
{
   double *dn = np->vp;
   const double *d1 = cp1->vp, *d2 = cp2->vp;
   const int *s1 = ip1->vp, *s2 = ip2->vp;
   const int n = np->N, n1 = cp1->N, n2 = cp2->N;
   int ic, i1, i2;

   for (ic=i1=i2=0; ic < n; ic++)
   {
      if (i1 < n1)
      {
         if (i2 < n2)  /* both are available for comparison */
         {
            if (s1[i1] <= s2[i2])
               dn[ic] = d1[i1++];
            else
               dn[ic] = d2[i2++];
         }
         else
         {
            assert(i1 < n1);
            dn[ic] = d1[i1++];
         }
      }
      else
      {
         assert(i2 < n2);
         dn[ic] = d2[i2++];
      }
   }
}

ATL_tvec_t *ATL_CombineTheseVecsUsingInts
(
   ATL_tvec_t *sp1,      /* 1st vector's index array to sort on */
   ATL_tvec_t *sp2,      /* 2nd vector's index array to sort on */
   ATL_tvec_t *cp1,      /* vector to be combined */
   ATL_tvec_t *cp2       /* vector to be combined */
)
{
   ATL_tvec_t *np;
   const pre = cp1->pre;

   assert(sp1->N == cp1->N && sp2->N == cp2->N);
   assert(sp1->pre == sp2->pre && sp1->pre == 'i');
   assert(pre == cp2->pre);
   assert(sp1->nrep == sp2->nrep && sp1->nrep == cp1->nrep &&
          sp1->nrep == cp2->nrep);
   np = ATL_GetTvec(cp1->name, cp1->N+cp2->N, cp1->nrep, pre);
   if (pre == 'i')
      ATL_FillCombINTVecUsingInts(np, sp1, sp2, cp1, cp2);
   else if (pre == 'd')
      ATL_FillCombDOUBLEVecUsingInts(np, sp1, sp2, cp1, cp2);
   else if (pre == 'c')
      ATL_FillCombCHARVecUsingInts(np, sp1, sp2, cp1, cp2);
   return(np);
}

void ATL_SuffixTvecNames
(
   ATL_tvec_t *tb,   /* list whose names should be suffixed */
   char *suff
)
{
   ATL_tvec_t *tp;
   int isu;

   isu = strlen(suff) + 1;
   for (tp=tb; tp; tp = tp->next)
   {
      int i, inm, n;
      char *sp;

      inm = strlen(tp->name);
      sp = malloc((inm+isu)*sizeof(char));
      for (i=0; i < inm; i++)
         sp[i] = tp->name[i];
      for (n=inm+isu; i < n; i++)
         sp[i] = suff[i-inm];
      free(tp->name);
      tp->name = sp;
   }
}
ATL_tvec_t *ATL_PullNamedVecsFromList  /* returns list of pulled vecs */
(
   int N,               /* # of vectors to remove from list to */
   char **names,        /* names of vectors to grab */
   ATL_tvec_t **orig    /* original list has names removed */
)
/*
 * Note that the names will be returned in the provided order, and that
 * we assume a name only appears once in orig.
 */
{
   ATL_tvec_t *prev, *old=(*orig), *po, *pn, *nb=NULL;
   int i;
   if (!old || N < 1)
      return(NULL);
   for (i=0; i < N; i++)
   {
      prev = NULL;
      po = old;
      while (po)
      {
         if (!strcmp(names[i], po->name))
         {
            if (po == old)
               old = old->next;
            if (nb)
            {
               pn->next = po;
               pn = po;
            }
            else
               pn = nb = po;
            if (prev)
               prev->next = po->next;
            pn->next = NULL;
            break;
         }
         prev = po;
         po = po->next;
      }
   }
   *orig = old;
   return(nb);
}

int ATL_CountTVecsInList
(
   ATL_tvec_t *tb      /* list to count */
)
{
   int i;
   for (i=0; tb; i++, tb = tb->next);
   return(i);
}

ATL_tvec_t *ATL_FindLastVecInList  /* RETURNS: last tvec in list */
(
   ATL_tvec_t *tb      /* list to look through */
)
{
   if (tb)
   {
      while (tb->next)
         tb = tb->next;
   }
   return(tb);
}

ATL_tvec_t *ATL_AlphabetizeVecList  /* returns alphabatized list */
(
   int N,              /* # of vectors in list */
   ATL_tvec_t *tb      /* list to alphabetize */
)
/*
 * Alphabatizes N-len tb, and returns new ordered list (old is destroyed).
 */
{
   char **names;
   ATL_tvec_t *tp;
   int i, j;

   names = malloc(sizeof(char*)*N);
   assert(names);
   for (i=0; tp && i < N; i++, tp = tp->next)
      names[i] = tp->name;
   assert(i == N);
/*
 * Sort names using selection sort
 */
   for (j=0; j < N-1; j++)
   {
      for (i=j+1; i < N; i++)
      {
         if (strcmp(names[i], names[j]) < 0)
         {
            char *sp = names[j];
            names[j] = names[i];
            names[i] = sp;
         }
      }
   }
/*
 * Use sorted names to make new alphabetical list, free names and return list
 */
   tb = ATL_PullNamedVecsFromList(N, names, &tb);
   free(names);
   return(tb);
}

void ATL_CopyStridedVec(char pre, int n, int inc, void *vin, void *vout)
{
   if (pre == 'd')
   {
      int i, j;
      double *in = vin, *out = vout;
      for (j=i=0; i < n; i++, j += inc)
         out[i] = in[j];
   }
   else if (pre == 'i')
   {
      int i, j;
      int *in = vin, *out = vout;
      for (j=i=0; i < n; i++, j += inc)
         out[i] = in[j];
   }
   else if (pre == 'c')
   {
      int i, j;
      char *in = vin, *out = vout;
      for (j=i=0; i < n; i++, j += inc)
         out[i] = in[j];
   }
   else if (pre == 's')
   {
      int i, j;
      char **in = vin, **out = vout;
      for (j=i=0; i < n; i++, j += inc)
      {
         int n;
         n = strlen(in[i]) + 1;
         out[i] = malloc(n*sizeof(char));
         strcpy(out[i], in[j]);
      }
   }
}

void ATL_PrintTVecElt
(
   FILE *fpout,         /* stream to print to */
   ATL_tvec_t *tp,      /* vector to print from */
   int idx              /* index in vector to print */
)
{
   if (tp->pre == 'd')
   {
      double *p = tp->vp;
      fprintf(fpout, "%e", p[idx]);
   }
   else if (tp->pre == 'i')
   {
      int *p = tp->vp;
      fprintf(fpout, "%d", p[idx]);
   }
   else if (tp->pre == 'c')
   {
      char *p = tp->vp;
      fprintf(fpout, "%c", p[idx]);
   }
   else if (tp->pre == 's')
   {
      char **p = tp->vp;
      fprintf(fpout, "%s", p[idx]);
   }
}

void ATL_PrintTVecsInRow
(
   FILE *fpout,         /* stream to print to */
   ATL_tvec_t *tb,      /* list of vectors to print (rowwise) */
   char *sep            /* string to print between elements */
)
{
   ATL_tvec_t *tp;
   const int N = tb->N;
   int i;

   if (!tb)
      return;
   for (tp=tb->next; tp; tp = tp->next)
      assert(tp->N >= N);

   for (i=0; i < N; i++)                        /* loop over rows of vectors */
   {
      for (tp=tb; tp->next; tp = tp->next)      /* loop over columns  */
      {
         ATL_PrintTVecElt(fpout, tp, i);
         fprintf(fpout, "%s", sep);
      }
      ATL_PrintTVecElt(fpout, tp, i);
      fprintf(fpout, "%s\n", sep);
   }
}

ATL_tvec_t *ATL_GetRep1Vector
(
   ATL_tvec_t *tin,    /* vector to split */
   int istart          /* which repetition to grab */
)
{
   char *name;
   ATL_tvec_t *t1=NULL;
   int n;
   const int Nr = tin->N / tin->nrep;
   const char pre = tin->pre;

   assert(tin->nrep < 10000000);
   n = strlen(tin->name) + 9;
   name = malloc(n*sizeof(char));
   assert(name);
   sprintf(name, "%s_%d", tin->name, istart);
   t1 = ATL_GetTvec(name, Nr, 1, pre);
   free(name);
   if (pre == 'd')
      ATL_CopyStridedVec(pre, Nr, tin->nrep,((double*)(tin->vp))+istart,t1->vp);
   else if (pre == 'i')
      ATL_CopyStridedVec(pre, Nr, tin->nrep, ((int*)(tin->vp))+istart, t1->vp);
   else if (pre == 'c')
      ATL_CopyStridedVec(pre, Nr, tin->nrep, ((char*)(tin->vp))+istart, t1->vp);
   else if (pre == 's')
      ATL_CopyStridedVec(pre, Nr, tin->nrep, ((char**)(tin->vp))+istart,t1->vp);

   return(t1);
}

ATL_tvec_t *ATL_SplitRepsVector  /* returns Q of sep vecs for each rep */
(
   ATL_tvec_t *tin     /* vector to split */
)
{
   int i;
   const int nrep = tin->nrep;
   ATL_tvec_t *tb, *tp;

   tp = tb = ATL_GetRep1Vector(tin, 0);
   for (i=1; i < nrep; i++)
   {
      tp->next = ATL_GetRep1Vector(tin, i);
      tp = tp->next;
   }
   return(tb);
}

ATL_tvec_t *ATL_GetStatVecsDOUBLE  /* returns Q of stat vectors: <,+,> */
(
   ATL_tvec_t *tin     /* vector to get stats for */
)
{
   char *name;
   int i, j, n, N;
   ATL_tvec_t *tavg, *tmin, *tmax;
   double *dmin, *dmax, *davg;

   n = strlen(tin->name) + 5;
   name = malloc(sizeof(char)*n);
   N = tin->N / tin->nrep;
   assert(name);

   sprintf(name, "%s_min", tin->name);
   tmin = ATL_GetTvec(name, N, 1, tin->pre);
   sprintf(name, "%s_avg", tin->name);
   tavg = ATL_GetTvec(name, N, 1, tin->pre);
   sprintf(name, "%s_max", tin->name);
   tmax = ATL_GetTvec(name, N, 1, tin->pre);
   dmin = tmin->vp;
   dmax = tmax->vp;
   davg = tavg->vp;

   n = tin->nrep;
   for (j=0; j < N; j++)
   {
      double min, max, sum, *din;

      din = ((double*)(tin->vp)) + j*n;
      min = max = sum = *din;
      for (i=1; i < n; i++)
      {
         if (din[i] < min)
            min = din[i];
         if (din[i] > max)
            max = din[i];
         sum += din[i];
      }
      dmin[j] = min;
      dmax[j] = max;
      davg[j] = sum / n;
   }
   tmin->next = tavg;
   tavg->next = tmax;
   return(tmin);
}

#endif  /* end ifdef multi-inclusion guard */
