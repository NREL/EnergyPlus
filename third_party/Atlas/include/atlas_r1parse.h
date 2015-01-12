#ifndef ATLAS_R1PARSE_H
   #define ATLAS_R1PARSE_H

#include "atlas_genparse.h"


#define R1F_INCACHE     0  /* consider kernel for in-cache gemv */
#define R1F_OUTCACHE    1  /* consider kernel for out-of-cache gemv */
#define R1F_ALLALIGNXY  2  /* X&Y are copied into all legal alignments */
#define R1F_ALIGNX2A    3  /* X forced to same alignment as A */
#define R1F_SINGLE      4  /* single precision */
#define R1F_COMPLEX     5  /* complex arithmetic */
#define R1F_APTRS       6  /* use ptrs rather than lda for column indexing */
#define R1F_X87         7  /* requires the Intel x87 unit */
#define R1F_FNU         8  /* N must be a multiple of NU */
#define R1F_ADDCFLAGS   9  /* don't replace: append cflags to default flags */
#define R1F_INCYISONE  10
#define R1F_NFLAGS     11
#define R1F_PFTUNABLE  14  /* Can tune PFDIST & INST? */
char *R1F_exp[R1F_NFLAGS] =
{
"Consider kernel for in-cache use only",
"Consider kernel for out-of-cache use only",
"X&Y are copied into all legal alignments",
"X forced to have same alignment as A",
"Data uses single precision",
"Data is of complex type",
"use ptrs rather than lda for column indexing",
"Kernel requires the x87 unit for correct operation",
"N must be a multiple of NU"
};

#define R1F_DEFAULT ((1<<R1F_INCACHE) | (1<<R1F_OUTCACHE))
typedef struct R1NODE ATL_r1node_t;
struct R1NODE
{
   double mflop[8];
   ATL_r1node_t *next;
   char *rout;                  /* filename/path for kernel */
   char *auth;                  /* author of kernel */
   char *comp;                  /* particular compiler required for kernel */
   char *cflags;                /* compiler flags required for kernel */
   char *kname;                 /* The name kernel should be compiled to */
   char *str;                   /* tmp string used in generation */
   char *exflags;               /* extra flags to pass test/time call */
   char *genstr;                /* system(genstr) will generate gened kernel */
   int alignA, alignX, alignY;  /* required alignments */
   int ldamul;                  /* lda must be a multiple of ldamul */
   int ID, NU, MU;              /* unrolling for Y & X vectors */
   int NMU;                     /* # of repetitions of MU */
   int minN, minM;              /* min veclen to call the rout with */
   int SSE;                     /* 0: no SSE, 1: SSE1 req, 2: SSE2 req, etc */
   int asmbits;                 /* valid assemblies in this file */
   int CacheElts;               /* # of cache elts to assume for blocking */
   int rankR;                   /* restriction rank, higher faster kern */
   int flag;                    /* bit vector of R1F_* */
};

/* procedure 1 */
static ATL_r1node_t *GetR1Node(void)
{
   ATL_r1node_t *p;
   p = calloc(1, sizeof(ATL_r1node_t));
   assert(p);
   p->flag = R1F_DEFAULT;
   return(p);
}

/* procedure 2 */
static ATL_r1node_t *CloneR1Node(ATL_r1node_t *dup)
{
   ATL_r1node_t *p;
   p = malloc(sizeof(ATL_r1node_t));
   assert(p);
   memcpy(p, dup, sizeof(ATL_r1node_t));
   if (dup->rout)
      p->rout = DupString(dup->rout);
   if (dup->auth)
      p->auth = DupString(dup->auth);
   if (dup->comp)
      p->comp = DupString(dup->comp);
   if (dup->cflags)
      p->cflags = DupString(dup->cflags);
   if (dup->str)
      p->str = DupString(dup->str);
   if (dup->genstr)
      p->genstr = DupString(dup->genstr);
   if (dup->exflags)
      p->exflags = DupString(dup->exflags);
   if (dup->kname)
      p->kname = DupString(dup->kname);
   p->next = NULL;
   return(p);
}

/* procedure 3: clones a queue of R1 structs */
static ATL_r1node_t *CloneR1Queue(ATL_r1node_t *dupb)
{
   ATL_r1node_t *p, *pd, *nb;
   if (!dupb)
      return(NULL);
   p = nb = CloneR1Node(dupb);
   for (pd=dupb->next; pd; pd = pd->next)
   {
      p->next = CloneR1Node(pd);
      p = p->next;
   }
   return(nb);
}

/* procedure 4: clones a queue of strided R1 structs */
static ATL_r1node_t *CloneStridedR1Queue
(
   ATL_r1node_t *dupb,   /* queue of nodes to clone */
   int stride               /* increment between nodes to take */
)
/*
 * Creates a queue of cloned nodes from dupb; move stride each time
 * (stride must be >= 1); i.e. skip stride-1 structs in original queue
 */
{
   ATL_r1node_t *p, *pd, *nb;
   int i;

   if (!dupb)
      return(NULL);
   if (stride == 1)
      return(CloneR1Queue(dupb));
   assert(stride > 1);
   p = nb = CloneR1Node(dupb);
   pd = nb;
   while(pd)
   {
      for (i=0; i < stride && pd; i++, pd = pd->next);
      if (pd)
      {
         p->next = CloneR1Node(pd);
         p = p->next;
      }
      else
         p->next = NULL;
   }
   return(nb);
}

/* procedure 5 */
static ATL_r1node_t *KillR1Node(ATL_r1node_t *die)
{
   ATL_r1node_t *p=NULL;
   if (die)
   {
      p = die->next;
      if (die->rout)
         free(die->rout);
      if (die->auth)
         free(die->auth);
      if (die->comp)
         free(die->comp);
      if (die->cflags)
         free(die->cflags);
      if (die->str)
         free(die->str);
      if (die->genstr)
         free(die->genstr);
      if (die->exflags)
         free(die->exflags);
      if (die->kname)
         free(die->kname);
      free(die);
   }
   return(p);
}

/* procedure 6: safely remove nukeme from Q, reseting all links */
static ATL_r1node_t *RemoveR1NodeFromQ
(
   ATL_r1node_t *Q,     /* queue of nodes */
   ATL_r1node_t *nukeme /* node to remove from queue */
)
/*
 * Removes nukeme from Q, sets nukeme->next=NULL, and returns updated Q
 */
{
   ATL_r1node_t *p, *prev;

   if (!nukeme)
      return(Q);
   assert(Q);
   if (Q == nukeme)
   {
      Q = Q->next;
      nukeme->next = NULL;
      return(Q);
   }
   prev = Q;
   for (p=Q->next; p && p != nukeme; p = p->next)
      prev = p;
   assert(p);
   prev->next = nukeme->next;
   nukeme->next = NULL;
   return(Q);
}

/* procedure 7 */
static ATL_r1node_t *KillR1NodeFromQ
(
   ATL_r1node_t *Q,     /* queue of nodes */
   ATL_r1node_t *nukeme /* node to remove from queue */
)
{
   Q = RemoveR1NodeFromQ(Q, nukeme);
   KillR1Node(nukeme);
   return(Q);
}

/* procedure 8 */
static void KillAllR1Nodes(ATL_r1node_t *die)
{
   while (die)
      die = KillR1Node(die);
}

/* procedure 9 */
static void ATL_SubGoodGccInR1Nodes
(
   ATL_r1node_t *bp   /* queue to make sub in */
)
/*
 *  Gets GOODGCC (from Make.inc), and substitutes it for all comp == "gcc"
 *  in the queue.  This gets us mandatory flags like -pg,-m64,etc.
 */
{
   ATL_r1node_t *kp;  /* queue to make sub in */
   char *gcc;
   gcc = GetGoodGcc();
   for (kp=bp; kp; kp = kp->next)
   {
      if (kp->comp && !strcmp(kp->comp, "gcc"))
      {
         free(kp->comp);
	 kp->comp = DupString(gcc);
      }
   }
}

/* procedure 10 */
static void ATL_UnsubGoodGccInR1Nodes
(
   ATL_r1node_t *bp   /* queue to make reverse sub in */
)
/*
 *  Gets GOODGCC (from Make.inc); Any comp string matching that is switched
 *  back to "gcc".  This is usually necessary so that output files don't
 *  use an old GOODGCC that lacks something like -pg.
 */
{
   ATL_r1node_t *kp;  /* queue to make sub in */
   char *gcc;
   gcc = GetGoodGcc();
   for (kp=bp; kp; kp = kp->next)
   {
      if (kp->comp && !strcmp(kp->comp, gcc))
      {
         free(kp->comp);
	 kp->comp = DupString("gcc");
      }
   }
}

/* procedure 11 */
static void ResubGoodGccInR1Nodes
(
   ATL_r1node_t *bp   /* queue to make sub in */
)
/*
 * Takes gcc compiler that use GOODGCC, and replaces them with "gcc"
 * to help portability
 */
{
   ATL_r1node_t *kp;  /* queue to make sub in */
   char *gcc;
   gcc = GetGoodGcc();
   for (kp=bp; kp; kp = kp->next)
   {
      if (kp->comp && !strcmp(kp->comp, gcc))
      {
         free(kp->comp);
	 kp->comp = DupString("gcc");
      }
   }
}

/* procedure 12 */
static int ATL_CountNumberOfR1Nodes
(
    ATL_r1node_t *bp   /* queue to count */
)
{
   int i;
   for (i=0; bp; i++, bp = bp->next);
   return(i);
}

/* procedure 13 */
static ATL_r1node_t *ATL_LastR1Node(ATL_r1node_t *bp)
/*
 * RETURNS: pointer to last node in queue
 */
{
   ATL_r1node_t *p;
   if (!bp)
      return(NULL);
   for (p=bp; p->next; p = p->next);
   return(p);
}

/* procedure 14: finds max integer at ip0 in struct */
static ATL_r1node_t *FindMaxIntInR1Q
(
   ATL_r1node_t *bp,   /* queue to be searched */
   void *ip0           /* ptr to integer withinin node bp */
)
/*
 * RETURNS: ptr to structure containing max int value at byte offset
 *          offset in struct
 */
{
   ATL_r1node_t *mp=NULL, *p;
   int *ip;
   int val;
   const int offset = (int)((char*)((char*) ip0) - ((char*)bp));

   if (!bp)
      return(NULL);

   mp = bp;
   ip = (int*)(((char*)bp) + offset);
   val = *ip;
   for (p=bp->next; p; p = p->next)
   {
      ip = (int*)(((char*)p) + offset);
      if (*ip > val)
      {
         mp = p;
         val = *ip;
      }
   }
   return(mp);
}
/* procedure 15: finds min integer at ip0 in struct */
static ATL_r1node_t *FindMinIntInR1Q
(
   ATL_r1node_t *bp,   /* queue to be searched */
   void *ip0           /* ptr to integer withinin node bp */
)
/*
 * RETURNS: ptr to structure containing min int value at byte offset
 *          offset in struct
 */
{
   ATL_r1node_t *mp=NULL, *p;
   int *ip;
   int val;
   const int offset = (int)((char*)((char*) ip0) - ((char*)bp));

   if (!bp)
      return(NULL);

   mp = bp;
   ip = (int*)(((char*)bp) + offset);
   val = *ip;
   for (p=bp->next; p; p = p->next)
   {
      ip = (int*)(((char*)p) + offset);
      if (*ip < val)
      {
         mp = p;
         val = *ip;
      }
   }
   return(mp);
}

/* procedure 16: finds first integer equal to val at ip0 in struct */
static ATL_r1node_t *FindIntValInR1Q
(
   ATL_r1node_t *bp,   /* queue to be searched */
   void *ip0,          /* ptr to integer withinin node bp */
   int val             /* value being searched for */
)
/*
 * RETURNS: ptr to first structure containing value val at byte offset
 *          offset in struct, or NULL if no such value found
 */
{
   ATL_r1node_t *mp=NULL, *p;
   int *ip;
   const int offset = (int)((char*)((char*) ip0) - ((char*)bp));

   if (!bp)
      return(NULL);

   for (p=bp; p; p = p->next)
   {
      ip = (int*)(((char*)p) + offset);
      if (*ip == val)
         return(p);
   }
   return(NULL);
}

/* procedure 17: sorts Q from least-to-greatest on int val at ip0 in struc */
static ATL_r1node_t *SortR1QByIntVal
(
   ATL_r1node_t *bp,   /* queue to be sorted */
   void *ip0           /* ptr to integer withinin node bp to sort on*/
)
/*
 * RETURNS: possibly new queue base, sorted from least-to-greatest on int at ip0
 */
{
   ATL_r1node_t *sb=NULL, *p;
   int *ip;
   const int offset = (int)((char*)((char*) ip0) - ((char*)bp));

   if (!bp)
      return(NULL);

   while(bp)
   {
      ip = (int*)(((char*)bp) + offset);
      p = FindMaxIntInR1Q(bp, ip);
      bp = RemoveR1NodeFromQ(bp, p);
      p->next = sb;
      sb = p;
   }
   return(sb);
}

/* procedure 18: reverses order in Q */
static ATL_r1node_t *ReverseR1Q(ATL_r1node_t *bp)
/*
 * RETURNS: new base ptr of reversed queue
 */
{
   ATL_r1node_t *nb=NULL, *p;
   while(bp)
   {
      p = bp;
      bp = bp->next;
      p->next = nb;
      nb = p;
   }
   return(nb);
}

/* procedure 19: places all nodes wt int value val at ip0 in new queue */
static ATL_r1node_t *YankR1NodesByIntVal
(
   ATL_r1node_t **bp0,  /* queue to be searched */
   void *ip0,          /* ptr to integer withinin node *bp */
   int val             /* value to be yanked out of original Q */
)
/*
 * Finds all nodes that have the integeral value val stored in position
 * ip0-bp0 in nodes.  These nodes are removed from bp0, and placed in
 * their own queue, which is returned.  bp0 is modified in the process.
 * RETURNS: ptr to queue of nodes wt integer value val
 */
{
   ATL_r1node_t *bp=(*bp0), *p, *valb=NULL, *vp;
   int *ip;
   const int offset = (int)((char*)((char*) ip0) - ((char*)bp));

   while(bp)
   {
      p = FindIntValInR1Q(bp, (((char*)bp)+offset), val);  /* find node */
      if (!p)       /* if there are no more in bp, we are done */
         break;
      bp = RemoveR1NodeFromQ(bp, p);   /* remove it from original queue */
/*
 *    Add node at front of new value-only queue
 */
      if (valb)
      {
         vp->next = p;
         vp = p;
      }
      else
         vp = valb = p;
   }
   *bp0 = bp;
   return(valb);
}

/* procedure 20 */
static ATL_r1node_t *ATL_SortR1NodesByMflop
(
   int imf,            /* which mflop entry to sort on */
   ATL_r1node_t *bp    /* queue to be sorted */
)
/*
 * kills original queue, and returns a greatest-to-least sorted queue
 * on p->mflop[imf].  Does it with O(N^2) alg, but if this is a bottleneck,
 * we never get here because timing takes an eternity.
 */
{
   ATL_r1node_t *p, *prev, *sb=NULL;   /* ptr, prev, sorted base */
   ATL_r1node_t *minp;
   double mf;

/*
 * Sort from greatest-to-least by always adding smallest entry in old
 * list to head of greatest-to-least list
 */
   while (bp)
   {
/*
 *    Find slowest remaining kernel
 */
      mf = bp->mflop[imf];
      for (minp=prev=bp, p=bp->next; p; p = p->next)
      {
         if (p->mflop[imf] < mf)
         {
            minp = p;
            mf = p->mflop[imf];
         }
      }
/*
 *    Remove it from unsorted queue, and add as new head of sorted
 */
      if (minp == bp)
      {
         bp = bp->next;
         minp->next = sb;
      }
      else   /* in the middle of unsorted queue */
      {
         prev->next = minp->next;
         minp->next = sb;
      }
      sb = minp;
   }
   return(sb);
}

/* procedure 21 */
static ATL_r1node_t *ParseR1Line(char *ln)
/*
 * Given a line from a r1 index file (with multiple lines pasted together
 * into one line (ln), return a structure describing that line.
 */
{
   ATL_r1node_t *p;
   char *sp;
   int itmp;
   char ch;

   p = GetR1Node();

   sp = strstr(ln, "LDAMUL=");
   if (sp)
      p->ldamul = atoi(sp+6+1);
   else
      p->ldamul = 0;

   sp = strstr(ln, "rankR=");
   if (sp)
      p->rankR = atoi(sp+5+1);
   else
      p->rankR = 0;

   sp = strstr(ln, "CacheElts=");
   if (sp)
      p->CacheElts = atoi(sp+9+1);
   else
      p->CacheElts = 0;

   sp = strstr(ln, "SSE=");
   if (sp)
      p->SSE = atoi(sp+3+1);
   else
      p->SSE = 0;

   sp = strstr(ln, "alignA=");
   if (sp)
      p->alignA = atoi(sp+6+1);
   else
      p->alignA = 0;

   sp = strstr(ln, "alignY=");
   if (sp)
      p->alignY = atoi(sp+6+1);
   else
      p->alignY = 0;

   sp = strstr(ln, "alignX=");
   if (sp)
      p->alignX = atoi(sp+6+1);
   else
      p->alignX = 0;

   sp = strstr(ln, "minM=");
   if (sp)
      p->minM = atoi(sp+4+1);
   else
      p->minM = 0;

   sp = strstr(ln, "minN=");
   if (sp)
      p->minN = atoi(sp+4+1);
   else
      p->minN = 0;

   sp = strstr(ln, "NU=");
   if (sp)
      p->NU = atoi(sp+2+1);
   else
      p->NU = 0;

   sp = strstr(ln, "MU=");
   if (sp)
      p->MU = atoi(sp+2+1);
   else
      p->MU = 0;

   sp = strstr(ln, "ID=");
   if (sp)
      p->ID = atoi(sp+2+1);
   else
      p->ID = 0;

   sp = strstr(ln, "PFTUNABLE=");
   if (sp)
   {
      if (atoi(sp+9+1))
         p->flag |= (1<<R1F_PFTUNABLE);
      else
         p->flag &= ~(1<<R1F_PFTUNABLE);
   }
   sp = strstr(ln, "ADDCFLAGS=");
   if (sp)
   {
      if (atoi(sp+9+1))
         p->flag |= (1<<R1F_ADDCFLAGS);
      else
         p->flag &= ~(1<<R1F_ADDCFLAGS);
   }
   sp = strstr(ln, "ALIGNX2A=");
   if (sp)
   {
      if (atoi(sp+8+1))
         p->flag |= (1<<R1F_ALIGNX2A);
      else
         p->flag &= ~(1<<R1F_ALIGNX2A);
   }
   sp = strstr(ln, "INCYISONE=");
   if (sp)
   {
      if (atoi(sp+9+1))
         p->flag |= (1<<R1F_INCYISONE);
      else
         p->flag &= ~(1<<R1F_INCYISONE);
   }
   sp = strstr(ln, "FNU=");
   if (sp)
   {
      if (atoi(sp+3+1))
         p->flag |= (1<<R1F_FNU);
      else
         p->flag &= ~(1<<R1F_FNU);
   }
   sp = strstr(ln, "ALLALIGNXY=");
   if (sp)
   {
      if (atoi(sp+10+1))
         p->flag |= (1<<R1F_ALLALIGNXY);
      else
         p->flag &= ~(1<<R1F_ALLALIGNXY);
   }
   sp = strstr(ln, "X87=");
   if (sp)
   {
      if (atoi(sp+3+1))
         p->flag |= (1<<R1F_X87);
      else
         p->flag &= ~(1<<R1F_X87);
   }

   sp = strstr(ln, "MFLOP=");
   if (sp)
      GetDoubleArr(sp+6, 8, p->mflop);

   sp = strstr(ln, "ASM=");
   if (sp)
      p->asmbits = asmNames2bitfield(sp+4);



   sp = strstr(ln, "CFLAGS='");
   if (sp)
      p->cflags = GetSingleQuoteString(sp+6+1);
   else
      p->cflags = NULL;

   sp = strstr(ln, "COMP='");
   if (sp)
      p->comp = GetSingleQuoteString(sp+4+1);
   else
      p->comp = NULL;

   sp = strstr(ln, "AUTH='");
   if (sp)
      p->auth = GetSingleQuoteString(sp+4+1);
   else
      p->auth = NULL;

   sp = strstr(ln, "ROUT='");
   if (sp)
      p->rout = GetSingleQuoteString(sp+4+1);
   else
      p->rout = NULL;

   sp = strstr(ln, "KNAME='");
   if (sp)
      p->kname = GetSingleQuoteString(sp+5+1);
   else
      p->kname = NULL;

   return(p);
}

/* procedure 18 */
static void PrintR1Line(FILE *fpout, ATL_r1node_t *np)
{
   int i, j, k;
   char ta, tb;

   if (!np)
      return;
   if (!np->rout)
      np->ID = 0;
   fprintf(fpout, "ID=%d ROUT='%s' AUTH='%s'",
           np->ID, np->rout ? np->rout : "generated",
           np->auth ? np->auth : "R. Clint Whaley");
   if (np->kname)
      fprintf(fpout, " KNAME='%s' \\\n", np->kname);
   else
      fprintf(fpout, " \\\n");
   fprintf(fpout, "   ");
   i = 3;
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "rankR=%d ", np->rankR);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "CacheElts=%d ", np->CacheElts);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "SSE=%d ", np->SSE);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "alignA=%d ", np->alignA);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "alignY=%d ", np->alignY);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "alignX=%d ", np->alignX);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "minM=%d ", np->minM);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "minN=%d ", np->minN);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "NU=%d ", np->NU);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "MU=%d ", np->MU);
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "LDAMUL=%d ", np->ldamul);

   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "PFTUNABLE=%d ", FLAG_IS_SET(np->flag, R1F_PFTUNABLE));
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "ALIGNX2A=%d ", FLAG_IS_SET(np->flag, R1F_ALIGNX2A));
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "ADDCFLAGS=%d ", FLAG_IS_SET(np->flag, R1F_ADDCFLAGS));
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "FNU=%d ", FLAG_IS_SET(np->flag, R1F_FNU));
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "INCYISONE=%d ", FLAG_IS_SET(np->flag, R1F_INCYISONE));
   if (i > 70) { fprintf(fpout, " \\\n   "); i = 3; }
   i += fprintf(fpout, "X87=%d ", FLAG_IS_SET(np->flag, R1F_X87));

   if (np->mflop[0]+np->mflop[1]+np->mflop[2]+np->mflop[3]+np->mflop[4]+
       np->mflop[5]+np->mflop[6] != 0.0)
   {
      if (i > 3) { fprintf(fpout, " \\\n   "); i = 3; }
      i += fprintf(fpout, "MFLOP=%le", np->mflop[0]);
      for (j=7; j && np->mflop[j] == 0.0; j--);
      for (k=1; k <= j; k++)
         i += fprintf(fpout, ",%le", np->mflop[k]);
   }
   if (np->asmbits)
   {
      if (i > 40) { fprintf(fpout, " \\\n   "); i = 3; }
      for (j=0; !(np->asmbits & (1<<j)); j++);
      assert(j < NASMD);
      i += fprintf(fpout, "  ASM=%s", ASMNAM[j]);
      for (j++; j < NASMD; j++)
         if (np->asmbits & (1<<i))
            i += fprintf(fpout, ",%s", ASMNAM[j]);
   }
   if (np->cflags)
   {
      if (i+strlen(np->cflags) > 70) { fprintf(fpout, " \\\n   "); i = 3; }
      i += fprintf(fpout, "  CFLAGS='%s'", np->cflags);
   }
   if (np->comp)
   {
      if (i+strlen(np->comp) > 70) { fprintf(fpout, " \\\n   "); i = 3; }
      i += fprintf(fpout, "  COMP='%s'", np->comp);
   }
   if (i)
      fprintf(fpout, "\n");
}

/* procedure 22 */
static void PrintR1Nodes(FILE *fpout, ATL_r1node_t *bp)
{
   while (bp)
   {
      PrintR1Line(fpout, bp);
      bp = bp->next;
   }
}

/* procedure 23 */
static void WriteR1File(char *file, ATL_r1node_t *nq)
{
   FILE *fpout;

   if (!file || !strcmp(file, "stdout"))
      fpout = stdout;
   else if (!strcmp(file, "stderr"))
      fpout = stderr;
   else
   {
      fpout = fopen(file, "w");
      assert(fpout);
   }
   PrintR1Nodes(fpout, nq);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
}

/* procedure 24 */
static void WriteR1FileWithPath
   (char pre, char *path, char *file, ATL_r1node_t *nq)
{
   char ln[2048];
   sprintf(ln, "%s/%c%s", path, pre, file);
   WriteR1File(ln, nq);
}

/* procedure 25 */
static ATL_r1node_t *ReadR1File(char *file)
/*
 * Reads in a standard ATLAS parsable R1 index file, and returns a
 * list of all the kernels defined there.
 */
{
   ATL_r1node_t *nq=NULL, *p;
   FILE *fpin;
   char *ln, *sp;
   int i, j, KeepOn, len;

   if (!file || !strcmp(file, "stdin"))
      fpin = stdin;
   else
      fpin = fopen(file, "r");
   if (!fpin)
      return(NULL);
   nq = p = GetR1Node();
   while (ln = GetJoinedLines(fpin))
   {
      if (ln[0] != '#')
      {
         p->next = ParseR1Line(ln);
         p = p->next;
      }
   }
   fclose(fpin);
   return(KillR1Node(nq));
}

/* procedure 26 */
static ATL_r1node_t *ReadR1FileWithPath
   (char pre, char *path, char *file)
{
   char ln[2048];
   sprintf(ln, "%s/%c%s", path, pre, file);
   return(ReadR1File(ln));
}


/* procedure 27 */
static ATL_r1node_t *DelRepeatedR1Kernels(ATL_r1node_t *bp)
/*
 * Deletes any repeated IDs
 */
{
   ATL_r1node_t *prev, *p, *np;
   int ID;

   for (p=bp; p; p = p->next)
   {
      ID = p->ID;
      prev = p;
      do
      {
         for (np=p->next; np && np->ID != ID; np = np->next)
            prev = np;
         if (np)  /* found duplicate */
            prev->next = KillR1Node(np);
      }
      while (np);
   }
   return(bp);
}

/* procedure 28 */
static ATL_r1node_t *DelBadArchR1Kernels(ATL_r1node_t *bp)
/*
 * Weeds out kernels that require SSE/assembly that we haven't got
 */
{
   int asmb=0, die;
   ATL_r1node_t *p, *prev;
   #ifdef ATL_GAS_ARM
      asmb |= (1<<7);
   #endif
   #ifdef ATL_GAS_MIPS
      asmb |= (1<<6);
   #endif
   #ifdef ATL_GAS_PARISC
      asmb |= (1<<5);
   #endif
   #ifdef ATL_GAS_PPC
      asmb |= (1<<4);
   #endif
   #ifdef ATL_GAS_SPARC
      asmb |= (1<<3);
   #endif
   #ifdef ATL_GAS_x8664
      asmb |= (1<<2);
   #endif
   #ifdef ATL_GAS_x8632
      asmb |= (1<<1);
   #endif

   prev = p = bp;
   while (p)
   {
      die = (p->asmbits) ? !(asmb & p->asmbits) : 0;
      #ifndef ATL_SSE3
         if (p->SSE)
         {
            die |= (p->SSE >= 3);
            #ifndef ATL_SSE2
               die |= (p->SSE >= 2);
            #endif
            #ifndef ATL_SSE1
               die |= (p->SSE >= 1);
            #endif
         }
      #endif
      if (die)
      {
         if (p == bp)
            bp = p = KillR1Node(p);
         else
            prev->next = p = KillR1Node(p);
      }
      else
      {
         prev = p;
         p = p->next;
      }
   }
   return(bp);
}

#define MAXBASES 4
/* procedure 29 */
static int ATL_R1SplitContexts
(
   ATL_r1node_t *kb,   /* pointer to all read in kernels */
   ATL_r1node_t **ocb, /* set to all out-of-cache kernels */
   ATL_r1node_t **i2b, /* set to all in-L2 kernels */
   ATL_r1node_t **i1b, /* set to all in-L1 kernels */
   ATL_r1node_t **syb  /* NULL, or all SYR/SYR2 kernels (may not exist) */
)
/*
 *  Takes unified bp, and splits it into separate pieces.  bp is invalidated
 *  in the process (is split into child queues)
 *  RETURNS: number of children found
 */
{
   ATL_r1node_t *kp, *kn;
   ATL_r1node_t *bases[MAXBASES] = {NULL, NULL, NULL, NULL};
   int nbases;

/*
 * Kernels come as a series of kernels that are ranked by efficiency from
 * high to low based on the integer rankR.  All series must end with a
 * general kernel with no restrictions with a rankR of 0 (all non-zero
 * kernels have restrictions).  This loop splits these kernels into
 * their seperate series (series indicates calling context).
 */
   kn = kb;
   nbases = 0;
   while (kn)
   {
      bases[nbases++] = kn;
      for (kp=kn; kp && kp->rankR; kp = kp->next);  /* find end of series */
      if (!kp)
         break;
      kn = kp->next;
      kp->next = NULL;
   }
   if (ocb)
      *ocb = bases[0];
   else
      KillAllR1Nodes(bases[0]);
   if (i2b)
      *i2b = bases[1];
   else
      KillAllR1Nodes(bases[1]);
   if (i1b)
      *i1b = bases[2];
   else
      KillAllR1Nodes(bases[2]);
   if (syb)
      *syb = bases[3];
   else
      KillAllR1Nodes(bases[3]);
   return(nbases);
}

/* procedure 30 */
static ATL_r1node_t *ATL_R1LinkContexts
(
   ATL_r1node_t *kp1, /* all out-of-cache kernels */
   ATL_r1node_t *kp2, /* all in-L2 kernels */
   ATL_r1node_t *kp3, /* all in-L1 kernels */
   ATL_r1node_t *kp4  /* all SYR/SYR2 kernels */
)
/*
 *  Takes separate queue, and joins them into one long queue; if any
 *  is NULL, all remaining cases must also be NULL!
 *  Seperate queues are subsumed into returned queue
 */
{
   ATL_r1node_t *kps[MAXBASES] = {kp1, kp2, kp3, kp4};
   ATL_r1node_t *kp, *kprev;
   int i, j;

   for (i=0; i < MAXBASES-1; i++)
   {
      if (!kps[i])
      {
         for (j=i+1; j < MAXBASES; j++)
            assert(!kps[j]);
         return(kp1);
      }
      for (kp=kps[i]; kp->next; kp = kp->next);
      kp->next = kps[i+1];
   }
   return(kp1);
}
#undef MAXBASES

/* procedure 31 */
static ATL_r1node_t *FindFastestR1Kernel
(  char pre,             /* precision prefix */
   ATL_r1node_t *bp,  /* kernel queue */
   int imf,              /* which mflop entry to sort by */
   int RESTRICTOK        /* consider restricted kernel? */
)
/*
 * A RESTRICTed kernel is one that requires something that can't be fixed
 * by loop peeling or the like.  Examples include forcing lda to a given
 * multiple, or 16-byte alignment for double complex (can't peel 1/2 of
 * a complex number to make 8-byte aligned array 16).
 * RETURNS: pointer to node in bp that is fastest in context imf wt RESTRCT
 */
{
   double mf;
   ATL_r1node_t *kp, *kmax=bp;
   int size, usize, RKERN;

   if (bp)
   {
      usize = (pre == 'c' || pre == 's') ? 4 : 8;
      if (pre == 'c' || pre == 'd') size = 8;
      else if (pre == 's') size = 4;
      else size = 16;
      mf = bp->mflop[imf];
      for (kp=bp->next; kp; kp = kp->next)
      {
         if (kp->mflop[imf] > mf)
         {
            RKERN = (pre == 'z' || pre == 'c') ? (kp->alignA > usize) : 0;
            RKERN = RKERN | (kp->ldamul > size);
            if (RESTRICTOK | !RKERN)
            {
               mf = kp->mflop[imf];
               kmax = kp;
            }
         }
      }
   }
   return(kmax);
}

/* procedure 32 */
static int R1flag2size(int flag)
/*
 * RETURNS: size of type using precision/type bits in flag
 */
{
   int size;

   size = FLAG_IS_SET(flag, R1F_SINGLE) ? 4 : 8;
   size *= FLAG_IS_SET(flag, R1F_COMPLEX) ? 2 : 1;
   return(size);
}

/* procedure 33 */
static char R1flag2pre(int flag)
/*
 * RETURNS: correct precision/type prefix based on flag
 */
{
   char pre = 'd';
   if (FLAG_IS_SET(flag, R1F_SINGLE))
      return(FLAG_IS_SET(flag, R1F_COMPLEX) ? 'c' : 's');
   return(FLAG_IS_SET(flag, R1F_COMPLEX) ? 'z' : 'd');
}

/* procedure 34 */
static int pre2R1flag(char pre, int flag)
/*
 * RETURNS: flag modified to reflect type/precision indicated by pre
 */
{
   SET_FLAG(flag, R1F_COMPLEX, (pre == 'c' || pre == 'z'));
   SET_FLAG(flag, R1F_SINGLE, (pre == 'c' || pre == 's'));
   return(flag);
}

/* procedure 35 */
static void SetAllR1TypeFlags(char pre, ATL_r1node_t *bp)
{
   ATL_r1node_t *p;
   for (p=bp; p; p = p->next)
      p->flag = pre2R1flag(pre, p->flag);
}

/* procedure 36 */
void PutKernNameInStr(ATL_r1node_t *r1B)
/*
 * Fills in the proper name for all kernels in r1->str
 */
{
   char ln[32] = {"ATL_dgerk_L0_restrict"};
   char pre;
   const int ipre=4, iL=11, irest=12;

   pre = R1flag2pre(r1B->flag);
   ln[ipre] = pre;
   r1B->str = DupString(ln);
   ln[irest] = '\0';
   r1B->next->str = DupString(ln);
   ln[irest] = '_';
   r1B = r1B->next->next;

   ln[iL] = '2';
   r1B->str = DupString(ln);
   ln[irest] = '\0';
   r1B->next->str = DupString(ln);
   ln[irest] = '_';
   r1B = r1B->next->next;

   ln[iL] = '1';
   r1B->str = DupString(ln);
   ln[irest] = '\0';
   r1B->next->str = DupString(ln);
   ln[irest] = '_';
   r1B = r1B->next->next;

   sprintf(ln, "ATL_%cgerk_L1b_restrict", pre);
   r1B->str = DupString(ln);
   ln[irest+1] = '\0';
   r1B->next->str = DupString(ln);
}

#endif  /* end atlas_r1parse.h guard */
