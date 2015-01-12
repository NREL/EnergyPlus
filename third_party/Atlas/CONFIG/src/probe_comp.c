#include "atlconf.h"

COMPNODE *GetCompNode(void)
{
   COMPNODE *p;
   p = calloc(1, sizeof(COMPNODE));
   assert(p);
   return(p);
}
COMPNODE *KillCompNode(COMPNODE *die)
{
   COMPNODE *p=NULL;
   if (die)
   {
      p = die->next;
      if (die->comp)
         free(die->comp);
      if (die->flags)
         free(die->flags);
      free(die);
   }
   return(p);
}

void KillAllCompNodes(COMPNODE *kill)
{
   while(kill)
      kill = KillCompNode(kill);
}

COMPNODE *CloneCompNode(COMPNODE *orig)
{
   COMPNODE *new;

   new = GetCompNode();
/*
 * Copy everything but strings wt memcopy
 */
   memcpy(new, orig, sizeof(COMPNODE));
   if (orig->comp)
   {
      new->comp = malloc(sizeof(char)*(strlen(orig->comp)+1));
      assert(new->comp);
      strcpy(new->comp, orig->comp);
   }
   if (orig->flags)
   {
      new->flags = malloc(sizeof(char)*(strlen(orig->flags)+1));
      assert(new->flags);
      strcpy(new->flags, orig->flags);
   }
   return(new);
}

void PrintCompNodes(FILE *fpout, COMPNODE *q, char *id)
{
   int i;
   COMPNODE *p;

   fprintf(fpout, "\nCompiler nodes: %s\n", id);
   if (!q)
      fprintf(fpout, "***NONE***\n");
   for (i=0, p=q; p; p=p->next, i++)
   {
      fprintf(fpout,
         "%3d. priority=%d, comps,OS,arch[0]=(%d,%d,%d), comp='%s'\n",
              i, p->priority, p->comps[0], p->OS[0], p->arch[0], p->comp);
      fprintf(fpout, "     '%s'\n", p->flags);
   }
   fprintf(fpout, "\n");
}

COMPNODE *SortCompsByPriority(COMPNODE *q)
/*
 * Builds new queue out of q, by first adding largest priority to newq, etc.
 */
{
   COMPNODE *newq=NULL, *p, *prev, *maxprev;
   int maxpri;

   while(q)  /* BFI N^2 sort */
   {
/*
 *    Find remaining element wt largest priority
 */
      maxprev = NULL;
      prev = q;
      maxpri = q->priority;
      for (p=q->next; p; p = p->next)
      {
         if (p->priority > maxpri)
         {
            maxpri = p->priority;
            maxprev = prev;
         }
         prev = p;
      }
/*
 *    Take max node off of q, add to newq
 */
      if (maxprev)  /* max elt wasn't stop of stack (q) */
      {
         p = maxprev->next->next;
         maxprev->next->next = newq;
         newq = maxprev->next;
         maxprev->next = p;
      }
      else
      {
         p = q->next;
         q->next = newq;
         newq = q;
         q = p;
      }
   }
   return(newq);
}

void DivideCompsByComp(COMPNODE *q, COMPNODE **comps)
/*
 * Builds individual queues for each compiler camp, and then kills the original
 * queue.  Note that original q is sorted smallest-to-largest, and since we
 * add in order to a stack, we wind up with largest-to-smallest, as we want.
 * Note comps is really COMPNODE *comps[NCOMP], and can be indexed by ICC_, etc.
 */
{
   int i;
   COMPNODE *p, *new;

   for (i=0; i < NCOMP; i++)
      comps[i] = NULL;
   for (p=q; p; p = p->next)
   {
      for (i=0; i < NCOMP; i++)
      {
         if (IsBitSetInField(p->comps, i))
         {
            new = CloneCompNode(p);
            new->next = comps[i];
            comps[i] = new;
         }
      }
   }
   KillAllCompNodes(q);
}

static int OSMatches(enum OSTYPE OS, int *bits)
/*
 * RETURNS: 0 if no OS of arch in bitfield, nonzero otherwise
 */
{
   if (IsBitSetInField(bits, 0))   /* If 0 bit set, matches all OS */
      return(1);
   if (IsBitSetInField(bits, OS))  /* If OS bit set, matches this OS */
      return(2);
   return(0);
}

static int ArchMatches(enum MACHTYPE arch, int *bits)
/*
 * RETURNS: 0 if no match of arch in bitfield, nonzero otherwise
 */
{
   if (IsBitSetInField(bits, 0))     /* If 0 bit set, matches all archs */
      return(1);
   if (IsBitSetInField(bits, arch)) /* If arch bit set, matches this arch */
      return(2);
   return(0);
}

COMPNODE *KillBadArchOS(enum OSTYPE OS, enum MACHTYPE arch, COMPNODE *q)
/*
 * Deletes all non-matching OS/arch from queue; Note any node wt these
 * quantities set to 0 is a wildcard, and so stays
 */
{
   COMPNODE *prev, *next, *p;
   if (!OS && !arch)
      return(q);
/*
 * Delete all beginning nodes until we find one for this arch
 */
   while(q && (!ArchMatches(arch, q->arch) || !OSMatches(OS, q->OS)))
      q = KillCompNode(q);
/*
 * With good top of stack, delete trailing nodes that don't match
 */
   if (q)
   {
      prev = q;
      for (p=q->next; p; p = next)
      {
         next = p->next;
         if (!ArchMatches(arch, p->arch) || !OSMatches(OS, p->OS))
            prev->next = KillCompNode(p);
         else
            prev = p;
      }
   }
   return(q);
}

int OSNameToInt(char *name)
{
   int i;
   for (i=1; i < NOS; i++)
   {
      if (!strcmp(name, osnam[i]))
         return(i);
   }
   return(0);
}

int MachNameToInt(char *name)
{
   int i;
   for (i=1; i < NMACH; i++)
   {
      if (!strcmp(name, machnam[i]))
         return(i);
   }
   return(0);
}
void NamesToBitField(int MACH, char *str, int *bits)
/*
 * Takes a list of machine (MACH=1) or OS (MACH=0) names and translates them
 * to their enumerated type numbers, and sets the appropriate bit in the
 * bits field
 */
{
   char name[128];
   int i=0;
   while(*str)
   {
      if (*str == ',' || *str == ' ' || *str == '\t' || *str == '\n' ||
          *str == '\0')
      {  /* finished a name */
         name[i] = '\0';
         if (!strcmp(name, "all") || !strcmp(name, "ALL"))
            i = 0;
         else
         {
            if (MACH)
               i = MachNameToInt(name);
            else
               i = OSNameToInt(name);
            if (!i)
            {
               fprintf(stderr, "Nonsensical %s name in list: %s\n",
                       MACH ? "machine" : "OS", str);
               exit(1);
            }
         }
         SetBitInField(bits, i);
         if (*str != ',')  /* anything but ',' ends list */
            return;
         str++;
         i = 0;
      }
      else
         name[i++] = *str++;
   }
}

void NumListToBitfield(char *str, int *bits)
/*
 * Takes a list of number like : '5,3,2,8,0' and turns on those bits
 */
{
   char num[16], *sp;
   int i, j;

   while (*str)
   {
      for (sp=num,i=0; i < 15; i++)
      {
         if (!isdigit(str[i])) break;
         *sp++ = str[i];
      }
      assert(i != 15);
      *sp = '\0';
      j = atoi(num);
      SetBitInField(bits, j);
      if (str[i] != ',') break;
      str += i+1;
   }
}

COMPNODE *ParseNewCompLine(char *ln)
{
   COMPNODE *p;
   char *sp;
   p = GetCompNode();

   sp = strstr(ln, "MACH=");
   assert(sp);
   sp += 5;
/*   NumListToBitfield(sp, p->arch); */
   NamesToBitField(1, sp, p->arch);


   sp = strstr(ln, "OS=");
   assert(sp);
   sp += 3;
/*   NumListToBitfield(sp, p->OS); */
   NamesToBitField(0, sp, p->OS);

   sp = strstr(ln, "LVL=");
   assert(sp);
   sp += 4;
   p->priority = atoi(sp);
/*
 * Parse 'COMPS=[icc,smc,dmc,skc,dkc,xcc,gcc,f77]', at least one comp must exist
 */
   sp = strstr(ln, "COMPS=");
   assert(sp);
   sp += 6;
   while (*sp)
   {
      if (sp[0] == 'i' && sp[1] == 'c' && sp[2] == 'c')
         SetBitInField(p->comps, ICC_);
      else if (sp[0] == 's' && sp[1] == 'm' && sp[2] == 'c')
         SetBitInField(p->comps, SMC_);
      else if (sp[0] == 'd' && sp[1] == 'm' && sp[2] == 'c')
         SetBitInField(p->comps, DMC_);
      else if (sp[0] == 's' && sp[1] == 'k' && sp[2] == 'c')
         SetBitInField(p->comps, SKC_);
      else if (sp[0] == 'd' && sp[1] == 'k' && sp[2] == 'c')
         SetBitInField(p->comps, DKC_);
      else if (sp[0] == 'x' && sp[1] == 'c' && sp[2] == 'c')
         SetBitInField(p->comps, XCC_);
      else if (sp[0] == 'g' && sp[1] == 'c' && sp[2] == 'c')
         SetBitInField(p->comps, GCC_);
      else if (sp[0] == 'f' && sp[1] == '7' && sp[2] == '7')
         SetBitInField(p->comps, F77_);
      else
      {
         fprintf(stderr, "WTF(%d of %s): '%s'??\n", __LINE__, __FILE__, sp);
         exit(-1);
      }
      if (sp[3] != ',') break;
      sp += 4;
   }
   return(p);
}

char *CopySingleQuoteString(char *str, char *out)
/*
 * Finds the leading ' in str, and copies quoted text to out until closing '
 * is found or end of string.
 * RETURNS: pointer in str at closing ', NULL if closing ' not found
 */
{
/*
 * Skip text before staring quote, return if quote not found
 */
   *out = '\0';
   while (*str != '\'' && *str) str++;
   if (*str == '\0')
      return(NULL);
/*
 * Copy quoted text, return pointer to end quote if it exists
 */
   str++;
   while (*str != '\'' && *str) *out++ = *str++;
   *out++ = '\0';
   if (*str == '\'')
      return(str);
   return(NULL);
}

COMPNODE *ParseCompLine(char *ln)
{
   static int NewComp=1;
   static COMPNODE *p=NULL;
   int i;
   char *sp;
   char ln2[2048];

   if (NewComp)
      p = ParseNewCompLine(ln);
/*
 * Should be line of form "'compiler' 'flags'"
 */
   else
   {
      sp = CopySingleQuoteString(ln, ln2);
      assert(sp);
      i = strlen(ln2) + 1;
      p->comp = malloc(sizeof(char)*i);
      assert(p->comp);
      strcpy(p->comp, ln2);

      sp = CopySingleQuoteString(sp+1, ln2);
      i = strlen(ln2) + 1;
      p->flags = malloc(sizeof(char)*i);
      assert(p->flags);
      strcpy(p->flags, ln2);
   }
   NewComp = !NewComp;
   return(p);
}

COMPNODE *ReadComps(char *file)
/*
 * Reads in a file describing the compilers ATLAS knows about, and returns
 * a queue of them for later manipulation.
 */
{
   char ln[2048];
   FILE *fpin;
   COMPNODE *compq=NULL, *p;

   fpin = fopen(file, "r");
   while (fgets(ln, 2048, fpin))
   {
      if (ln[0] != '#')
      {
         KillUselessSpace(ln);
         if (ln[0] != '#' && ln[0] != '\0')
         {
            p = ParseCompLine(ln);
            if (p != compq)
            {
               p->next = compq;
               compq = p;
            }
         }
      }
   }
   fclose(fpin);
   return(compq);
}

COMPNODE **GetDefaultComps(enum OSTYPE OS, enum MACHTYPE arch, int verb,
                           int vecexts)
/*
 * This routine reads the file atlcomp.txt, and returns them sorted by
 * order of priority for each compiler ATLAS needs.  This list can then
 * be matched with the user's input to give final compiler and flags.
 */
{
   COMPNODE *q, *p, **comps;
   char *vp=NULL;
   int i;

   if ((vecexts & (1<<ISA_AVXFMA4)) && arch == AmdDozer)
      vp = "-msse4.2 -mfma4";
   else if ((vecexts & (1<<ISA_AVXMAC)) && arch == AmdDriver)
      vp = "-mavx -mno-sse2avx -mfma";
   else if ((vecexts & (1<<ISA_AVXMAC)))
      vp = "-mavx2 -mfma";
   else if (vecexts & (1<<ISA_VSX))
      vp = "-mvsx";
   else if (vecexts & (1<<ISA_AV))
      vp = "-maltivec";
   else if (vecexts & (1<<ISA_AVX))
      vp = "-mavx";
   else if (vecexts & (1<<ISA_SSE3))
      vp = "-msse3";
   else if (vecexts & (1<<ISA_SSE2))
      vp = "-msse2";
   else if (vecexts & (1<<ISA_SSE1))
      vp = "-msse";
   comps = malloc(sizeof(COMPNODE)*NCOMP);
   q = ReadComps("atlcomp.txt");    /* get all compiler lines */
   if (verb > 1)
      PrintCompNodes(stderr, q, "Fresh Read");
   q = KillBadArchOS(OS, arch, q);  /* discard comps for other platforms */
   if (verb > 1)
      PrintCompNodes(stderr, q, "Targeted");
   q = SortCompsByPriority(q);      /* q is smallest, bottom is largest */
   if (verb > 1)
      PrintCompNodes(stderr, q, "Sorted");
   DivideCompsByComp(q, comps);     /* split into individual queues */
/*
 * Add the ISA extension-using flags to any gcc compiler.  Since we have just
 * read in the compilers from compflags.txt, we can be sure that any gcc
 * compiler has gcc in the name
 * Don't add these flags to XCC, since it doesn't matter there
 */
   if (vp)
   {
      int i;
      for (i=0; i < NCOMP; i++)
      {
         if (i == XCC_)
	    continue;
         for (p=comps[i]; p; p = p->next)
         {
            #if defined(ATL_GCCCLANG) || defined(ATL_GCC3P)
               if (strstr(p->comp, "gfortran"))
               {
                  if (!strcmp(vp, "-mavx"))
                     p->flags = NewAppendedString(p->flags, "-msse3");
                  else
                     p->flags = NewAppendedString(p->flags, vp);
               }
               else if (strstr(p->comp, "gcc"))
                  p->flags = NewAppendedString(p->flags, vp);
            #else
               if (strstr(p->comp, "gcc") || strstr(p->comp, "gfortran"))
                  p->flags = NewAppendedString(p->flags, vp);
            #endif
         }
      }
   }
/*
 * Add appropriate ISA extension flags to all compilers except XCC
 */
   return(comps);
}

int CompTest(int verb, char *targ, int icomp, char *comp, char *flag)
/*
 * Tries to build simple program and run it.
 * RETURNS: 0: success: non-zero on error
 */
{
   char *cmnd, *res, *frm, *trg;
   int i, iret=1;

   if (targ)
   {
      i = 32 + strlen(targ);
      trg = malloc(i*sizeof(char));
      assert(trg);
      sprintf(trg, "atlrun=atlas_runX targ=%s ", targ);
   }
   else
   {
      trg = malloc(sizeof(char));
      assert(trg);
      trg[0] = '\0';
   }
   if (icomp == ICC_)
      targ = NULL;
   if (icomp == F77_)
      frm = "make IRunF77Comp F77='%s' F77FLAGS='%s' %s | fgrep SUCCESS";
   else
      frm = "make IRunCComp CC='%s' CCFLAGS='%s' %s | fgrep SUCCESS";
   i = 1 + strlen(frm) + strlen(comp) + strlen(flag) + strlen(trg);
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, frm, comp, flag, trg);
   free(trg);
   res = atlsys_1L(NULL, cmnd, verb, 0);
   if (res)
   {
      iret = !strstr(res, "SUCCESS");
      free(res);
   }
   if (verb > 1)
      fprintf(stderr, "cmnd=%s\n", cmnd);
   free(cmnd);
   if (verb)
      fprintf(stderr, "   %s %s : %s!\n", comp, flag,
              iret ? "FAILURE":"SUCCESS");
   return(iret);
}

void CompError(int icomp)
/*
 * Prints out informative error message when we die because a compiler doesn't
 * work
 */
{
   fprintf(stderr, "\n\nUnable to find usable compiler for %s; aborting",
           COMPNAME[icomp]);
   fprintf(stderr, "Make sure compilers are in your path, and specify good compilers to configure\n");
   fprintf(stderr,
           "(see INSTALL.txt or 'configure --help' for details)");
   exit(icomp+1);
}

char *GetPtrbitsFlag(enum OSTYPE OS, enum MACHTYPE arch, int ptrbits,
                     char *comp)
/*
 * RETURNS: string forcing setting of ptrbits for gcc
 */
{
   char *sp = "";
   int i, j, k;

   if (MachIsIA64(arch))
      return(sp);
   if (MachIsMIPS(arch))
      return((ptrbits == 64) ? "-mabi=64" : "-mabi=n32");
   if (MachIsS390(arch))
      return((ptrbits == 64) ? "-m64" : "-m31");
   if (!CompIsGcc(comp))
   {
/*
 *    Add correct 64/32 bit flags for Sun workshop compilers
 */
      if (MachIsUS(arch) && CompIsSunWorkshop(comp))
      {
         if (ptrbits == 64)
            sp = (arch == SunUSI || arch == SunUSII) ?
                 "-xarch=v9" : "-xarch=v9b";
         else
            sp = (arch == SunUSI || arch == SunUSII) ?
                 "-xarch=v8plusa" : "-xarch=v8plusb";
      }
      else if (CompIsIBMXL(comp))  /* IBM xl compilers */
         sp = (ptrbits == 64) ? "-q64" : "-q32";
      return(sp);
   }
   GetGccVers(comp, &k, &j, &k, &k);
   if ( !(j >= 3 && (OS != OSOSX || j > 3 || !CompIsAppleGcc(comp))) )
      return(sp);
   else if (OS == OSAIX)
      sp = (ptrbits == 64) ? "-maix64" : "-maix32";
   else if ((MachIsX86(arch) || MachIsPPC(arch) || MachIsUS(arch)) ||
            arch == IbmPwr7 || arch == IbmPwr6 || arch == Pwre6500)
   {
      if (ptrbits == 64)
         sp = "-m64";
      else if (ptrbits == 32)
         sp = "-m32";
   }
   return(sp);
}
char *GetStandardCompName(char *comp)
{
   int i, j, k;
   char *ucomp;
/*
 * Recognize gnu compiler regardless of name string (eg. ev6-gcc-3.2)
 */
   if (CompIsGcc(comp))
   {
      GetGccVers(comp, &k, &j, &k, &k);
      if (j < 4)
      {
         if (i == F77_)
            ucomp = "g77";
         else
            ucomp = "gcc";
      }
      else if (i == F77_)
         ucomp = "gfortran";
      else
         ucomp = "gcc";
   }
   else
      ucomp = NameWithoutPath(comp);
   return(ucomp);
}
char *GetWinComp(enum OSTYPE OS, char *comp, char *bindir)
{
   char *ln;
   char *ucomp;
   int i;
   if (!OSIsWin(OS))
      return(NULL);
   ucomp = GetStandardCompName(comp);
   if (!strcmp(ucomp, "icc") || !strcmp(ucomp, "icl"))
      ucomp = "ATLwin_icc";
   else if (!strcmp(ucomp, "ifort") || !strcmp(ucomp, "ivf"))
      ucomp = "ATLwin_ifort";
   else if (!strcmp(ucomp, "mvc") || !strcmp(ucomp, "cl"))
      ucomp = "ATLwin_cl";
   else /* not a recognized windows compiler that needs wrapping, done */
      return(NULL);
   i = strlen(bindir);
   ln = malloc(i+16+strlen(ucomp));
   assert(ln);
   sprintf(ln, "make %s/%s.exe", bindir, ucomp);
   if (system(ln))
   {
      fprintf(stderr, "Unable to to build %s, quitting\n", ucomp);
      fprintf(stderr, "cmnd='%s'\n", ln);
      free(ln);
      exit(-1);
   }
   sprintf(ln, "%s/%s", bindir, ucomp);
   return(ln);
}


int ReadMinGW(int ptrbits, int nof77,
              char **mgar, char **mgran, char **mgc, char **mgf)
/*
 * Reads MinGW library building tools from file MinGW[32,64].dat, of form:
 * <MinGW ar>
 * <MinGW ranlib>
 * <MinGW gcc>
 * <MinGW gfortran>
 * RETURNS: 1 if file exists, otherwise 0.
 */
{
   FILE *fpin;

   if (ptrbits == 64)
      fpin = fopen("MinGW64.dat", "r");
   else
      fpin = fopen("MinGW32.dat", "r");
   if (!fpin)
      return(0);
   *mgar = ATL_fgetln(fpin);
   *mgran = ATL_fgetln(fpin);
   *mgc = ATL_fgetln(fpin);
   *mgf = ATL_fgetln(fpin);
   if (!nof77)
      assert(*mgf);
   assert(*mgar && *mgran && *mgc);
   return(1);
}

void FindMinGW(int verb, int ptrbits, int nof77,
               char **mgar, char **mgran, char **mgc, char **mgf)
/*
 * Searches for MinGW compilers using cygwin's standard name/path on 06/11/2012
 */
{
   const char *pref = (ptrbits == 64) ? "/usr/bin/x86_64-w64-mingw32-" :
                                        "/usr/bin/i686-w64-mingw32-";
   char ln[64], *res, *cmnd;
   sprintf(ln, "ls -1 %sgcc-4*", pref);
   res = atlsys_1L(NULL, ln, verb, 1);
   if (!res)
   {
      sprintf(ln, "ls %sgcc.exe", pref);
      res = atlsys_1L(NULL, ln, verb, 1);
   }
   if (!res)
   {
      fprintf(stderr,
              "Cannot find MinGW gcc in /usr/bin, with names beginning with\n");
      fprintf(stderr,
              "%s* ; Make sure MinGW is installed, then try again.\n", pref);
      fprintf(stderr,
      "If your MinGW compiler not named like this, specify them using file\n");
      exit(-1);
   }
   *mgc = res;
   if (!nof77)
   {
      sprintf(ln, "ls -1 %sgfortran*", pref);
      res = atlsys_1L(NULL, ln, verb, 1);
      if (!res)
      {
         fprintf(stderr, "MinGW gfortran not found!\n");
         exit(-1);
      }
      *mgf = res;
   }
   else
      *mgf = NULL;
   sprintf(ln, "ls -1 %sar.exe", pref);
   res = atlsys_1L(NULL, ln, verb, 1);
   if (!res)
   {
      fprintf(stderr, "MinGW ar not found!\n");
      exit(-1);
   }
   *mgar = res;

   sprintf(ln, "ls -1 %sranlib.exe", pref);
   res = atlsys_1L(NULL, ln, verb, 1);
   if (!res)
   {
      fprintf(stderr, "MinGW ranlib not found!\n");
      exit(-1);
   }
   *mgran = res;
}

char *BuildMinGW(int verb, int ptrbits, int nof77,
                 char *mgar, char *mgran, char *mgc, char *mgf)
/*
 * Build the MinGW wrappers using the provided MinGW tool path/names
 * RETURNS: fully qualified path to BLDdir, where MinGW wrappers are built
 */
{
   char *cmnd, *mgwd;
   int mlen, i;
   mgwd = atlsys_1L(NULL, "pwd", verb, 1);
   assert(mgwd);
   mlen = strlen(mgar);
   i = strlen(mgran);
   mlen = (mlen >= i) ? mlen : i;
   i = strlen(mgc);
   mlen = (mlen >= i) ? mlen : i;
   if (mgf)
   {
      i = strlen(mgc);
      mlen = (mlen >= i) ? mlen : i;
   }
   cmnd = malloc((mlen+32)*sizeof(char));
   assert(cmnd);

   sprintf(cmnd, "make mgwar cmppath=%s", mgar);
   syschk(cmnd);

   sprintf(cmnd, "make mgwranlib cmppath=%s", mgran);
   syschk(cmnd);

   sprintf(cmnd, "make mgwgcc cmppath=%s", mgc);
   syschk(cmnd);

   if (mgf)
   {
      sprintf(cmnd, "make mgwgfortran cmppath=%s", mgf);
      syschk(cmnd);
   }
   return(mgwd);
}

char *SetupMinGW(int verb, int ptrbits, int nof77)
/*
 * RETURNS: BLDdir, where MinGW tools have been built
 */
{
   char *mgar=NULL, *mgran=NULL, *mgc=NULL, *mgf=NULL, *mgd;
   if (!ReadMinGW(ptrbits, nof77, &mgar, &mgran, &mgc, &mgf))
      FindMinGW(verb, ptrbits, nof77, &mgar, &mgran, &mgc, &mgf);
   mgd = BuildMinGW(verb, ptrbits, nof77, mgar, mgran, mgc, mgf);
   free(mgar);
   free(mgran);
   free(mgc);
   if (mgf)
      free(mgf);
   return(mgd);
}

void GetComps(enum OSTYPE OS, enum MACHTYPE arch, int verb, char *targ,
              int ptrbits, char **usrcomps, int nof77, int nocygwin, int vecext,
              char *goodgcc, char *bindir)
/*
 * This routine gives config a list of compilers to use.  The first NCOMP
 * entries in usrcomps indicate a user override of the default compiler,
 * and the next NCOMP entries indicate user override of flags.  The next
 * NCOMP entries indicate that those flags should be appended to prior flags.
 * A NULL in any entry says the user is happy to use the defaults (or no
 * appending).  Chosen compilers and flags are returned in usrcomps array.
 */
{
   COMPNODE **comps, *p;
   char *ucomp, *dcomp, *flg, *sp, *sp2, *mgwd=NULL, *mgwc=NULL, *mgwf=NULL;
   char *cmnd, *res;
   int i, j, k, h;
/*
 * Look through input compilers; any of them that is simply "gcc" gets replaced
 * with goodgcc;  If user has overridden with path or specific name (gcc-4)
 * leave it at his choice
 */
   for (i=0; i < NCOMP; i++)
   {
      if (usrcomps[i])
      {
         if (!strcmp(usrcomps[i], "gcc") && goodgcc)
	 {
	    free(usrcomps[i]);
	    usrcomps[i] = NewStringCopy(goodgcc);
	 }
      }
   }

/*
 * If the user requests MinGW install, or is on 64-bit platform, build
 * wrapper code
 */
   if (nocygwin || (OS == OSWin64 && ptrbits == 64))
   {
      int fndit;
      mgwd = SetupMinGW(verb, ptrbits, nof77);
      i = strlen(mgwd);
      mgwc = malloc(sizeof(char)*(i+7+1));
      assert(mgwc);
      sprintf(mgwc, "%s/mgwgcc", mgwd);
      mgwf = malloc(sizeof(char)*(i+12+1));
      assert(mgwf);
      sprintf(mgwf, "%s/mgwgfortran", mgwd);
   }
/*
 * Get the ATLAS-suggested compilers and flags
 */
   comps = GetDefaultComps(OS, arch, verb, vecext);
/*
 * Look through comps, and substitute any "gcc" with goodgcc
 */
   if (goodgcc)
   {
      for (i=0; i < NCOMP; i++)
      {
         for (p=comps[i]; p; p = p->next)
         {
            if (!strcmp(p->comp, "gcc"))
            {
               free(p->comp);
               p->comp = NewStringCopy(goodgcc);
            }
         }
      }
   }
   if (verb > 1)
      fprintf(stdout, "Finding good compilers:\n");
   for (i=0; i < NCOMP; i++)
   {
      if (nof77 && i == F77_) continue;
/*
 *    If the user has not specified the compiler, look through all available
 *    compilers with one that works (with user flags, if specified)
 */
      if (!usrcomps[i])
      {
         for (p=comps[i]; p; p = p->next)
         {
            char *freeme=NULL;
            flg = NewStringCopy(usrcomps[NCOMP+i]?usrcomps[NCOMP+i]:p->flags);
            if (usrcomps[NCOMP*2+i])
               flg = NewAppendedString(flg, usrcomps[NCOMP*2+i]);
            freeme = sp = GetWinComp(OS, p->comp, bindir);
            if (!sp)
            {
/*
 *             If mgwd is set, we must substitute MinGW wrap comps for gcc and
 *             gfortran
 */
               if (mgwd && i != XCC_)
               {
                  if (CompIsGcc(p->comp))
                  {
                     if (strstr(p->comp, "fortran") || strstr(p->comp, "f77"))
                        sp = mgwf;
                     else
                        sp = mgwc;
                  }
                  else
                     sp = p->comp;
               }
               else
                  sp = p->comp;
            }
            if (ptrbits)
            {
               if (OS == OSWin64 && ptrbits == 64 && i == XCC_)
                  flg = NewAppendedString(flg, "-m32");
               else
                  flg = NewAppendedString(flg,
                           GetPtrbitsFlag(OS, arch, ptrbits, sp));
            }
            if (sp == mgwf)
               flg = NewAppendedString(flg, "-static");
            if (!CompTest(verb, targ, i, sp, flg))
               break;
            free(flg);
            if (freeme)
               free(freeme);
         }                              /* end loop over compilers */
         if (!p)
            CompError(i);
         else
            free(flg);
	 if (mgwd)
	    usrcomps[i] = NewStringCopy(sp);
         else if (i == GCC_ || !strcmp(p->comp, "gcc"))
	    usrcomps[i] = goodgcc ? NewStringCopy(goodgcc) : "gcc";
	 else
	 {
	    usrcomps[i] = p->comp;
            p->comp = NULL;                /* so it isn't deleted by Kill */
	 }
         if (!usrcomps[NCOMP+i])
         {
            usrcomps[NCOMP+i] = p->flags;
            p->flags = NULL;            /* so it isn't deleted by Kill */
         }
      }
/*
 *    If user specified comp w/o flags, get default flags or error
 */
      else if (!usrcomps[NCOMP+i])
      {
         p = comps[i];
         ucomp = NameWithoutPath(usrcomps[i]);
/*
 *       Recognize gnu compiler regardless of name string (eg. ev6-gcc-3.2)
 */
         if (CompIsGcc(usrcomps[i]))
         {
            GetGccVers(usrcomps[i], &k, &j, &k, &k);
            if (j < 4)
            {
               if (i == F77_)
                  ucomp = "g77";
               else
                  ucomp = "gcc";
            }
            else if (i == F77_)
               ucomp = "gfortran";
            else
               ucomp = goodgcc;
         }
         for (p=comps[i]; p; p = p->next)
         {
            dcomp = NameWithoutPath(p->comp);
            if (!strcmp(p->comp, ucomp))
               break;
            free(dcomp);
         }
         if (!p)
         {
            fprintf(stderr,
               "UNKNOWN COMPILER '%s' for %s: you must also supply flags!\n",
                    usrcomps[i], COMPNAME[i]);
            exit(i+1);
         }
         usrcomps[NCOMP+i] = p->flags;
         p->flags = NULL;
      } /* If user specifed both flags and compiler, accept them */
/*
 *    On windows, build compiler wrapper for MSVC++ or Intel compilers
 */
      sp = GetWinComp(OS, usrcomps[i], bindir);
      if (sp)
      {
         free(usrcomps[i]);
         usrcomps[i] = sp;
      }
/*
 *    Test selected compiler and flags, and die if they don't work
 */
      flg = NewStringCopy(usrcomps[NCOMP+i]?usrcomps[NCOMP+i]:p->flags);
      if (usrcomps[NCOMP*2+i])
         flg = NewAppendedString(flg, usrcomps[NCOMP*2+i]);
      if (ptrbits)
      {
         if (OS == OSWin64 && ptrbits == 64 && i == XCC_)
            flg = NewAppendedString(flg, "-m32");
         else
            flg = NewAppendedString(flg,
                     GetPtrbitsFlag(OS, arch, ptrbits,usrcomps[i]));
      }
      if (strstr(usrcomps[i], "mgwgfortran"))
         flg = NewAppendedString(flg, "-static");
      if (CompTest(verb, targ, i, usrcomps[i], flg))
         CompError(i);
      free(flg);
   } /* end of loop over compilers */
   for (i=0; i < NCOMP; i++)
      KillAllCompNodes(comps[i]);
   free(comps);
/*
 * modify base flags by appending user flags
 */
   for (i=2*NCOMP; i < 3*NCOMP; i++)
   {
      if (usrcomps[i])  /* user has appended flags for compiler i-2*NCOMP */
         usrcomps[i-NCOMP] = NewAppendedString(usrcomps[i-NCOMP], usrcomps[i]);
   }
/*
 * If nof77, set fortran compiler & flags to ICC to avoid linking problems
 */
   if (nof77)
   {
      usrcomps[F77_] = NewStringCopy(usrcomps[ICC_]);
      usrcomps[F77_+NCOMP] = NewStringCopy(usrcomps[ICC_+NCOMP]);
   }
/*
 * If ptrbits is set to manual override, add -m32/64 to gnu compilers
 * but not on Itaniums or Apple's munged gcc 3 compiler!
 */
   if (ptrbits && arch != IA64Itan && arch != IA64Itan2)
   {
      for (i=0; i < NCOMP; i++)
      {
         sp = GetPtrbitsFlag(OS, arch, ptrbits, usrcomps[i]);
         if (OS == OSWin64 && ptrbits == 64 && i == XCC_)
            usrcomps[i+NCOMP] = NewAppendedString(usrcomps[i+NCOMP], "-m32");
         else
            usrcomps[i+NCOMP] = NewAppendedString(usrcomps[i+NCOMP], sp);
      }
   }
/*
 * Add -static to mgmfortran
 */
   if (OSIsWin(OS) && ptrbits == 64)
   {
      if (strstr(usrcomps[i], "mgwgfortran"))
         usrcomps[F77_+NCOMP] = NewAppendedString(usrcomps[F77_+NCOMP],
                                               " -static");
   }
/*
 * On windows, add required -mstackrealign to all gnu comps,
 * since gcc breaks the x86 ABI by default.
 */
   if (OSIsWin(OS) && ptrbits != 64)
   {
      for (i=0; i < NCOMP; i++)
      {
         if (i != F77_ && CompIsGcc(usrcomps[i]))
            usrcomps[i+NCOMP] = NewAppendedString(usrcomps[i+NCOMP],
                                                  " -mstackrealign");
      }
   }
/*
 * Need to add target & bitwidth args for MIPSpro compilers on IRIX
 */
   if (OS == OSIRIX)
   {
      char *fnd;
      fnd = FindUname(targ);
      i = strlen(fnd) + 4;
      cmnd = malloc(sizeof(char)*i);
      assert(cmnd);
      sprintf(cmnd, "%s -m", fnd);
      res = atlsys_1L(NULL, cmnd, verb, 1);
      free(cmnd);
      assert(res);
      sp = strstr(res, "IP");
      assert(sp);
      for (i=2; isdigit(sp[i]); i++);
      sp[i] = '\0';
      i = strlen(sp) + 20;
      cmnd = malloc(sizeof(char)*i);
      assert(cmnd);
      sprintf(cmnd, "-TARG:platform=%s", sp);
      if (ptrbits == 64 || !ptrbits)
         strcat(cmnd, " -64");
      else
         strcat(cmnd, " -32");
      for (i=0; i < NCOMP; i++)
      {
         if (CompIsMIPSpro(usrcomps[i]))
         {
            usrcomps[i+NCOMP] = NewAppendedString(usrcomps[i+NCOMP], cmnd);
         }
      }
      free(res);
      free(cmnd);
   }
}  /* end of routine GetComps */
int SelectBestGcc
(
   int verb,
   char *targ,
   int GMAJOR,
   int GMINOR,
   char **gccs   /* NULL-terminated list of possible gcc compilers */
)
/*
 * RETURNS: 0 if no gcc compiler,
 *          -i if ith compiler is best compiler we found
 *          i if ith compiler is gnu gcc with matching major & minor
 */
{
   int ibest=0, igood=0, ileastbad=0;
   int gmaj=0, gmin=0, lmaj=0, lmin=0, i;
   if (!gccs)
      return(0);
   for (i=0; gccs[i]; i++)
   {
      int icmp, major, minor, patch;
      GetGccVers(gccs[i], &icmp, &major, &minor, &patch);
      if (verb)
         printf("   icmp=%d, maj=%d, min=%d, pat=%d: %s\n",
                icmp, major, minor, patch, gccs[i]);
      if (major == GMAJOR && minor == GMINOR)
      {
         if (!CompTest(verb, targ, GCC_, gccs[i], "-O"))
         {
            if (CompIsMinGW(gccs[i]) || CompIsAppleGcc(gccs[i]))
               ibest = i+1;
            else if (!CompTest(verb, targ, GCC_, gccs[i], "-O"))
               return(i+1);
         }
      }
      else if (major == GMAJOR && minor >= GMINOR)
      {
         if (!CompTest(verb, targ, GCC_, gccs[i], "-O"))
            ibest = i+1;
      }
      else if (major == GMAJOR)
      {
         if (major > gmaj || (major == gmaj && minor > gmin))
	 {
            if (!CompTest(verb, targ, GCC_, gccs[i], "-O"))
	    {
               igood = i+1;
	       gmaj = major;
	       gmin = minor;
	    }
         }
      }
      else if (major > lmaj || (major == lmaj && minor > lmin))
      {
         if (!CompTest(verb, targ, GCC_, gccs[i], "-O"))
	 {
            ileastbad = i+1;
	    lmaj = major;
	    lmin = minor;
         }
      }
   }
   if (ibest)
      return(ibest);
   else if (igood)
      return(-igood);
   return(-ileastbad);
}


char *CheckStrLen(char *str, int *len, int reqlen)
{
   if (*len < reqlen)
   {
      free(str);
      str = malloc(reqlen*sizeof(char));
      *len = reqlen;
   }
   return(str);
}

void GetBestGccVers(enum OSTYPE OS, enum MACHTYPE arch,
                    int *GMAJOR, int *GMINOR, int *GPATCH)
{
   *GMAJOR = 4;
   switch(arch)
   {
   case PPCG4:          /* cannot install 4.7.0 */
   case PPCG5:          /* cannot install 4.7.0 */
   case SunUSII:        /* cannot install 4.7.0 */
   case IntPIII:
   case IntPPRO:        /* no longer have access */
   case IntAtom:
      *GMINOR = 6;
      *GPATCH = 2;
   case x86SSE1:
   case x86SSE2:
   case x86SSE3:
   case x86x87:
   case IbmPwr7:
   case IntCore2:
   case IntCorei1:
   case IntCorei2:
   case IntCorei3:
   case Amd64K10h:
   case ARMv7:
   default:
      *GMINOR = 7;
      *GPATCH = 0;
      break;
   }
}
char *FindGoodGcc(enum OSTYPE OS, enum MACHTYPE arch, int verb, char *targ)
{
   char *OSpaths=NULL, *sp, **gccs=NULL;
   char *ln;
   FILE *fp;
   int i, lnlen=1024;
   int GMAJOR, GMINOR, GPATCH;

   GetBestGccVers(OS, arch, &GMAJOR, &GMINOR, &GPATCH);
   ln = malloc(lnlen*sizeof(char));
   assert(ln);
/*
 * See if we have some OS-specific places to search for good gcc
 */
   if (OS == OSOSX)  /* /sw is for fink, /opt is for macports */
   {
      OSpaths = "/sw/bin /opt/local/bin /opt/local/sbin";
   }
/*
 * We first look in any high-priority OS-specific spots for the right files
 */
   if (OSpaths)
   {
      i = 64 + strlen(OSpaths);
      ln = CheckStrLen(ln, &lnlen, i);
      sprintf(ln, "find %s -maxdepth 1 -name '*gcc*' -exec ./xisgcc '{}' \\;",
              OSpaths);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      i = SelectBestGcc(verb, targ, GMAJOR, GMINOR, gccs);
      if (i > 1)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
         gccs = NewOneStringList(gccs, -(i+1));
      else
      {
         KillAllStringsInList(gccs);
	 gccs = NULL;
      }
   }
/*
 * If we are not satisfied with the places we've looked so far,
 * try the user's path environment variables, don't search recursively
 */
   sp = GetPathEnvVar();
   if (sp)
   {
      i = 64 + strlen(sp);
      ln = CheckStrLen(ln, &lnlen, i);
      sprintf(ln, "find %s -maxdepth 1 -name '*gcc*' -exec ./xisgcc '{}' \\;",
              sp);
      free(sp);
      fp = atlsys(NULL, ln, verb, 1);
      if (fp)
      {
         gccs = GetLinesFromFile(fp, gccs);
         i = SelectBestGcc(verb, targ, GMAJOR, GMINOR, gccs);
         if (i > 0)
         {
            free(ln);
            return(FreeListGetString(gccs, i-1));
         }
         if (i < 0)
            gccs = NewOneStringList(gccs, -(i+1));
         else
         {
            KillAllStringsInList(gccs);
	    gccs = NULL;
         }
      }
   }
/*
 * Try searching in $HOME/local, including all subdirs
 */
   ln = CheckStrLen(ln, &lnlen, 64);
   sprintf(ln, "find $HOME/local -name '*gcc*' -exec ./xisgcc '{}' \\;");
   fp = atlsys(NULL, ln, verb, 1);
   if (fp)
   {
      gccs = GetLinesFromFile(fp, gccs);
      i = SelectBestGcc(verb, targ, GMAJOR, GMINOR, gccs);
      if (i > 0)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
         gccs = NewOneStringList(gccs, -(i+1));
      else
      {
         KillAllStringsInList(gccs);
         gccs = NULL;
      }
   }
/*
 * If we still haven't found it, try standard unix places, recursively searched
 */
   {
      char *stdpaths =
         "/usr/local /bin /sbin /usr/bin /usr/sbin /opt/bin /opt/sbin";

      i = 64 + strlen(stdpaths);
      ln = CheckStrLen(ln, &lnlen, i);
      i = sprintf(ln, "find %s -name '*gcc*' -exec ./xisgcc '{}' \\;",
                  stdpaths);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      i = SelectBestGcc(verb, targ, GMAJOR, GMINOR, gccs);
      if (i > 1)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
      {
         free(ln);
         return(FreeListGetString(gccs, -(i+1)));
      }
      else
         KillAllStringsInList(gccs);
   }
   free(ln);
   return(NULL);
}

char *FindGoodGfortran(enum OSTYPE OS, enum MACHTYPE arch, int verb,
                       char *targ, char *gcc)
{
   char *OSpaths=NULL, *sp, **gccs=NULL;
   char *ln;
   FILE *fp;
   int i, lnlen=1024;
   int ccomp, cmaj, cmin, cpat;

   ln = malloc(lnlen*sizeof(char));
   assert(ln);
/*
 * Get gcc's version; we'll try to find a matching gfortran
 */
   GetGccVers(gcc, &ccomp, &cmaj, &cmin, &cpat);
/*
 * See if we can find gfortran in the same place we found gcc
 */
   sp = GetPathWithoutName(gcc);  /* get path to gcc */
   if (sp)
   {
      i = 64 + strlen(sp);
      ln = CheckStrLen(ln, &lnlen, i);
      sprintf(ln,
         "find %s -maxdepth 1 -name '*gfortran*' -exec ./xisgcc '{}' \\;", sp);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      i = SelectBestGcc(verb, targ, cmaj, cmin, gccs);
      if (i > 1)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
         gccs = NewOneStringList(gccs, -(i+1));
      else
      {
         KillAllStringsInList(gccs);
	 gccs = NULL;
      }
      free(sp);
   }
/*
 * See if we have some OS-specific places to search for good gcc
 */
   if (OS == OSOSX)  /* /sw is for fink, /opt is for macports */
   {
      OSpaths = "/sw/bin /opt/local/bin /opt/local/sbin";
   }
/*
 * We first look in any high-priority OS-specific spots for the right files
 */
   if (OSpaths)
   {
      i = 64 + strlen(OSpaths);
      ln = CheckStrLen(ln, &lnlen, i);
      sprintf(ln,
         "find %s -maxdepth 1 -name '*gfortran*' -exec ./xisgcc '{}' \\;",
         OSpaths);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      i = SelectBestGcc(verb, targ, cmaj, cmin, gccs);
      if (i > 1)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
         gccs = NewOneStringList(gccs, -(i+1));
      else
      {
         KillAllStringsInList(gccs);
	 gccs = NULL;
      }
   }
/*
 * If we are not satisfied with the places we've looked so far,
 * try the user's path environment variable and HOME/local
 */
   sp = GetPathEnvVar();
   if (sp)
   {
      i = 64 + strlen(sp);
      ln = CheckStrLen(ln, &lnlen, i);
      sprintf(ln,
         "find %s -maxdepth 1 -name '*gfortran*' -exec ./xisgcc '{}' \\;", sp);
      free(sp);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      i = SelectBestGcc(verb, targ, cmaj, cmin, gccs);
      if (i > 0)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
         gccs = NewOneStringList(gccs, -(i+1));
      else
      {
         KillAllStringsInList(gccs);
	 gccs = NULL;
      }
   }
/*
 * Try searching in $HOME/local, including all subdir
 */
   ln = CheckStrLen(ln, &lnlen, 64);
   sprintf(ln, "find $HOME/local -name '*gfortran*' -exec ./xisgcc '{}' \\;");
   fp = atlsys(NULL, ln, verb, 1);
   if (fp)
   {
      gccs = GetLinesFromFile(fp, gccs);
      i = SelectBestGcc(verb, targ, cmaj, cmin, gccs);
      if (i > 0)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
         gccs = NewOneStringList(gccs, -(i+1));
      else
      {
         KillAllStringsInList(gccs);
         gccs = NULL;
      }
   }
/*
 * If we still haven't found it, try standard unix places
 */
   {
      char *stdpaths =
         "/usr/local /bin /sbin /usr/bin /usr/sbin /opt/bin /opt/sbin";

      i = 64 + strlen(stdpaths);
      ln = CheckStrLen(ln, &lnlen, i);
      i = sprintf(ln, "find %s -name '*gcc*' -exec ./xisgcc '{}' \\;",
                  stdpaths);
      i = sprintf(ln, "find %s -name '*gfortran*' -exec ./xisgcc '{}' \\;",
                   stdpaths);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      i = SelectBestGcc(verb, targ, cmaj, cmin, gccs);
      if (i > 1)
      {
         free(ln);
         return(FreeListGetString(gccs, i-1));
      }
      if (i < 0)
      {
         free(ln);
         return(FreeListGetString(gccs, -(i+1)));
      }
      else
         KillAllStringsInList(gccs);
   }
   free(ln);
   return(NULL);
}

char *FindNamedComp(enum OSTYPE OS, enum MACHTYPE arch, int verb,
                    char *targ, char *comp)
/*
 * This routine searches for a named compiler only in the user's path.
 * RETURNS: fully qualified name
 */
{
   char *sp, *ln=NULL, **gccs=NULL;
   int i, lcmp, lnlen=0;

   if (!comp)
      return(NULL);
/*
 * Return copy of original name if it already contains a path
 */
   for (i=0; comp[i] != '\0'; i++)
      if (comp[i] == '/')
         return(NewStringCopy(comp));
   lcmp = i;
/*
 * Otherwise, search the user's path to find which compiler to use
 */
   sp = GetPathEnvVar();
   if (sp)
   {
      i = 64 + lcmp + strlen(sp);
      ln = CheckStrLen(ln, &lnlen, i);
      sprintf(ln, "find %s -maxdepth 1 -name '%s'", sp, comp);
      free(sp);
      gccs = GetLinesFromFile(atlsys(NULL, ln, verb, 1), gccs);
      free(ln);
      for (i=0; gccs[i]; i++)
         if (strstr(gccs[i], comp))
            break;
      assert(gccs[i]);
      return(FreeListGetString(gccs, i));
   }
   return(NewStringCopy(comp));
}

void TestComps(enum OSTYPE OS, enum MACHTYPE arch, int verb, char *targ,
               char *targarg, char **comps, enum F2CNAME *f2cnam,
               enum F2CINT *f2cint, enum F2CSTRING *f2cstr, int nof77)
/*
 * This file tests that all C compilers work and interact w/o any changes,
 * and figure out how to have the fortran compiler call the C compiler
 */
{
   char *sp;
   int i, ierr;
   if (verb)
      fprintf(stdout, "C compiler interoperation probe unimplemented!\n\n");
/*
 * C interoperation checks
 */
   if (verb > 1)
      fprintf(stderr, "ICC interoperation tests:\n");
   for (i=0; i < NCOMP; i++)
   {
      if (i != XCC_ && i != F77_ && i != ICC_)
      {
         if (strcmp(comps[i], comps[ICC_])) /* only check if different */
         {
            char *frm = "make IRunC2C CC='%s' CCFLAGS='%s' CC1='%s' CC1FLAGS='%s' | fgrep SUCCESS";
            char *cmnd, *res;
            int j;

            j = strlen(frm) + strlen(comps[ICC_]) + strlen(comps[NCOMP+ICC_])
                + strlen(comps[i]) + strlen(comps[i+NCOMP]) + 1;
            cmnd = malloc(j*sizeof(char));
            assert(cmnd);
            sprintf(cmnd, frm,
                    comps[ICC_], comps[NCOMP+ICC_], comps[i], comps[i+NCOMP]);
            if (verb > 1)
               fprintf(stderr, "cmnd='%s'\n", cmnd);
            res = atlsys_1L(NULL, cmnd, verb, 0);
            free(cmnd);
            ierr = (res) ? !strstr(res, "SUCCESS") : 1;
            if (ierr)
            {
               fprintf(stderr, "Compiler %d (%s) does not interoperate with interface compiler (%s), aborting!\n", i, comps[i], comps[ICC_]);
               fprintf(stderr, "ierr=%d, res='%s'\n", ierr, res);
               exit(ierr);
            }
            free(res);
            if (verb > 1)
               fprintf(stderr,
                       "   C2C %s/%s -- SUCCESS\n", comps[ICC_], comps[i]);
         }
      }
   }
/*
 * F2c tests
 */
   if (nof77)
   {
      *f2cnam = f2c_NamErr;
      *f2cint = f2c_IntErr;
      *f2cstr = f2c_StrErr;
   }
   else
   {
      char *frm, *cmnd, *res;
      int j;
      j = (strstr(comps[F77_], "mgwgfortran") != NULL);
      if (j)
         frm = "make IRun_f2c args=\"%s -C ic '%s' -F ic '%s' -C if '%s' -F if '%s -static'\" | fgrep 'F2C=('";
      else
         frm = "make IRun_f2c args=\"%s -C ic '%s' -F ic '%s' -C if '%s' -F if '%s'\" | fgrep 'F2C=('";

      j = strlen(frm) + strlen(targarg) + strlen(comps[ICC_]) +
          strlen(comps[ICC_+NCOMP]) + strlen(comps[F77_]) +
          strlen(comps[F77_+NCOMP]) + 1;
      cmnd = malloc(j*sizeof(char));
      assert(cmnd);
      sprintf(cmnd, frm, targarg, comps[ICC_], comps[ICC_+NCOMP],
              comps[F77_], comps[F77_+NCOMP]);
      *f2cnam = f2c_NamErr;
      *f2cint = f2c_IntErr;
      *f2cstr = f2c_StrErr;
      if (verb > 1)
         fprintf(stderr, "cmnd='%s'\n", cmnd);
      res = atlsys_1L(NULL, cmnd, verb, 0);
      free(cmnd);
      if (res)
      {
         if (verb > 1)
            fprintf(stderr, "res='%s'\n", res);
         i = sscanf(res, " F2C=(%d,%d,%d)", f2cnam, f2cint, f2cstr);
         if (verb > 1)
            fprintf(stderr, "nread=%d, f2cname=%d, f2cint=%d, f2cstr=%d\n",
                    i, *f2cnam, *f2cint, *f2cstr);
         if (i != 3)
           *f2cnam = *f2cint = *f2cstr = 0;
         free(res);
      }
      if (verb)
      {
         printf("F2C name = %s\n", f2c_namestr[*f2cnam]);
         printf("F2C int  = %s\n", f2c_intstr[*f2cint]);
         printf("F2C str  = %s\n", f2c_strstr[*f2cstr]);
      }
   }
}

void PrintCompResults(char *file, char **comps, enum F2CNAME f2cnam,
                      enum F2CINT f2cint, enum F2CSTRING f2cstr)
{
   FILE *fpout;
   int i;

   if (file)
      fpout = fopen(file, "w");
   else fpout = stdout;
   assert(fpout);

   for (i=0; i < NCOMP; i++)
   {
      if (comps[i])
         fprintf(fpout, "%d '%s' '%s'\n", i, comps[i], comps[i+NCOMP]);
   }
   if (comps[F77_])
      fprintf(fpout, "F2CNAME,F2CINT,F2CSTRING=(%d,%d,%d)\n",
              f2cnam, f2cint, f2cstr);
   if (fpout != stdout && fpout != stderr)
      fclose(fpout);
}


void PrintUsage(char *name, int iarg, char *arg)
{
   fprintf(stderr, "\nERROR around arg %d (%s).\n", iarg,
           arg ? arg : "unknown");
   fprintf(stderr, "USAGE: %s [flags] where flags are:\n", name);
   fprintf(stderr, "   -v <verb> : verbosity level\n");
   fprintf(stderr, "   -O <enum OSTYPE #>  : set OS type\n");
   fprintf(stderr, "   -A <enum MACHTYPE #> : set machine/architecture\n");
   fprintf(stderr,
   "   -V #    # = ((1<<vecISA1) | (1<<vecISA2) | ... | (1<<vecISAN))\n");
   fprintf(stderr, "   -b <32/64> : set pointer bitwidth\n");
   fprintf(stderr, "   -o <outfile>\n");
   fprintf(stderr, "   -C [xc,ic,if,sk,dk,sm,dm,al,ac] <compiler>\n");
   fprintf(stderr, "   -F [xc,ic,if,sk,dk,sm,dm,al,ac,gc] '<comp flags>'\n");
   fprintf(stderr,    /* HERE */
           "   -Fa [xc,ic,if,sk,dk,sm,dm,al,ac,gc] '<comp flags to append>'\n");
   fprintf(stderr, "        al: append flags to all compilers\n");
   fprintf(stderr, "        ac: append flags to all C compilers\n");
   fprintf(stderr,
      "   -T <targ> : ssh target for cross-compilation (probably broken)\n");
   fprintf(stderr, "   -d [s,b]  : set source/build directory\n");
   fprintf(stderr, "   -S[i/s] <handle> <val>  : special int/string arg\n");
      fprintf(stderr,
        "      -Si nof77 <0/1> : Have/don't have fortran compiler\n");
      fprintf(stderr,
        "      -Si nocygwin <0/1> : Do/don't depend on GPL cygwin library\n");
      fprintf(stderr,
        "                           (Windows compiler/cygwin install only)\n");
   fprintf(stderr,
      "NOTE: enum #s can be found by : make xprint_enums ; ./xprint_enums\n");
   exit(iarg);
}

void GetFlags(int nargs,                /* nargs as passed into main */
              char **args,              /* args as passed into main */
              int *verb,                /* verbosity setting */
              enum OSTYPE *OS,          /* OS to assume */
              int *vec,                 /* Vector ISA extension bitfield */
              enum MACHTYPE *mach,     /* machine/arch to assume */
              int *ptrbits             /* # of bits in ptr: 32/64 */,
              char **comps,
              char **outfile,
              char **srcdir,          /* path to top of source directory */
              char **bindir,          /* path to top of binary directory */
              int *NoF77,
              int *NoCygwin,
              char **targ             /* mach to ssh to*/
             )
{
   int i, k, k0, kn, DoInt;
   char *sp, *sp0;

   *verb = 0;
   *srcdir = *bindir = NULL;
   *outfile = NULL;
   *targ = NULL;
   for (k=0; k < NCOMP*3; k++)
      comps[k] = NULL;

   *ptrbits = 0;
   *mach = 0;
   *vec = 0;
   *OS = 0;
   *verb = 0;
   *NoCygwin = 0;
   *NoF77 = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'b':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *ptrbits = atoi(args[i]);
         break;
      case 'A':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *mach = atoi(args[i]);
         break;
      case 'V':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *vec = atoi(args[i]);
         break;
      case 'O':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *OS = atoi(args[i]);
         break;
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *verb = atoi(args[i]);
         break;
      case 'T':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *targ = args[i];
         break;
      case 'S':
         if (args[i][2] != 'i' && args[i][2] != 's')
            PrintUsage(args[0], i, "-S needs i or s suffix!");
         DoInt = args[i][2] == 'i';
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         sp0 = args[i];
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         if (DoInt)
            k = atoi(args[i]);
         else
            sp = NewStringCopy(args[i]);
          if (!strcmp(sp0, "nof77"))
            *NoF77 = k;
         else if (!strcmp(sp0, "nocygwin"))
            *NoCygwin = k;
         else
            PrintUsage(args[0], i-1, sp0);
         break;
      case 'o':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *outfile = args[i];
         break;
      case 'd':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         sp = args[i-1];
         if (*sp == 's')
            *srcdir = args[i];
         else if (*sp == 'b')
            *bindir = args[i];
         break;
      case 'C':
      case 'F':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         sp = args[i];
         k = -1;
         if (*sp == 'i' && sp[1] == 'c') k = ICC_;
         else if (*sp == 'g' && sp[1] == 'c') k = GCC_;
         else if (*sp == 'i' && sp[1] == 'f') k = F77_;
         else if (*sp == 's' && sp[1] == 'k') k = SKC_;
         else if (*sp == 'd' && sp[1] == 'k') k = DKC_;
         else if (*sp == 's' && sp[1] == 'm') k = SMC_;
         else if (*sp == 'd' && sp[1] == 'm') k = DMC_;
         else if (*sp == 'x' && sp[1] == 'c') k = XCC_;
         if (*sp == 'a' && (sp[1] == 'l' || sp[1] == 'c'))
         {  /* only appended flags can be applied to all compilers */
            const int SKIPGCC=(sp[2] != 'g'), SKIPF=(sp[1] == 'c');
            if (args[i-1][1] == 'F')
            {
               if (args[i-1][2] == 'a')
               {
                  k0 = NCOMP+NCOMP;
                  kn = k0 + NCOMP;
               }
               else
               {
                  k0 = NCOMP;
                  kn = NCOMP+NCOMP;
               }
            }
            else
            {
               k0 = 0;
               kn = NCOMP;
            }
            if (++i >= nargs)
               PrintUsage(args[0], i, "out of arguments");
            for (k=k0; k < kn; k++)
               if ((!SKIPF || k-k0 != F77_) && (!SKIPGCC || k-k0 != GCC_))
                  comps[k] = args[i];
         }
         else
         {
            if (k < 0) PrintUsage(args[0], i, args[i]);
            if (args[i-1][1] == 'F')
            {
               k += NCOMP;
               if (args[i-1][2] == 'a')
                  k += NCOMP;
            }
            if (++i >= nargs)
               PrintUsage(args[0], i, "out of arguments");
            comps[k] = args[i];
         }
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
/*
 * allocate these strings ourselves so we can free them later if necessary
 */
   for (i=0; i < 3*NCOMP; i++)
   {
      if (comps[i])
      {
         if (!strcmp(comps[i], "default"))
            comps[i] = NULL;
         else
         {
            sp = malloc(sizeof(char)*(strlen(comps[i])+1));
            strcpy(sp, comps[i]);
            comps[i] = sp;
         }
      }
   }
   if (*ptrbits != 32 && *ptrbits != 64)
      *ptrbits = 0;
}

int main(int nargs, char **args)
/*
 * probe_comp has the following responsibilities:
 * 1. Read in atlcomp.txt for recommended compiler and flags
 * 2. Find a GOODGCC to override generic "gcc" name and to assemble assembly codes
 * 3. If on Windows wt 64 bits, build MinGW compiler wrappers, for 64-bit comps
 * 4. Accept user override of compiler/flags
 * 5. Append any user appended flags
 * 6. Ensure all non-xcc C compilers interoperate by calling probe_ccomps
 * 7. Figure out F77/C interoperating rules by calling probe_f772c
 * 8. Printing out results of these probes for later use
 */
{
   enum OSTYPE OS;
   enum MACHTYPE mach;
   int ptrbits, verb, vecexts, i, nof77, nocygwin;
   char *usrcomps[3*NCOMP];
   char *outfile, *targ, *targarg, *goodgcc, *srcdir, *bindir;
   enum F2CNAME f2cnam;
   enum F2CINT f2cint;
   enum F2CSTRING f2cstr;

   GetFlags(nargs, args, &verb, &OS, &vecexts, &mach, &ptrbits, usrcomps,
            &outfile, &srcdir, &bindir, &nof77, &nocygwin, &targ);
/*
 * Fully qualify any usr-override compilers
 */
   for (i=0; i < NCOMP; i++)
   {
      if (usrcomps[i])
      {
         char *sp = usrcomps[i];
         usrcomps[i] = FindNamedComp(OS, mach, verb, targ, sp);
         free(sp);
      }
   }
   if (verb > 1)
   {
      fprintf(stdout, "User Override Compilers:\n");
      for (i=0; i < NCOMP; i++)
         fprintf(stdout, "   '%s' : '%s' '%s'\n",
            usrcomps[i] ? usrcomps[i]:"none",
            usrcomps[i+NCOMP] ? usrcomps[i+NCOMP]:"none",
            usrcomps[i+2*NCOMP] ? usrcomps[i+2*NCOMP]:"none");
      fprintf(stdout, "\n");
   }
   if (targ)
   {
      targarg = malloc(sizeof(char)*(strlen(targ)+24));
      assert(targarg);
      sprintf(targarg, "-T '%s'", targ);
   }
   else
      targarg = "";
   #if defined(ATL_GCCCLANG) || defined(ATL_GCC3P)
   {
      char *topd;
      int dlen;
      topd = atlsys_1L(NULL, "pwd", verb, 1);
      assert(topd);
      dlen = strlen(topd);
      goodgcc = malloc(dlen + 10);
      assert(goodgcc);
      #ifdef ATL_GCCCLANG
         sprintf(goodgcc, "%s/gccclang", topd);
      #else
         sprintf(goodgcc, "%s/gcc3p", topd);
      #endif
      free(topd);
   }
   #else
      if (usrcomps[GCC_])
         goodgcc = NewStringCopy(usrcomps[GCC_]);
      else
         goodgcc = FindGoodGcc(OS, mach,  verb, targ);
   #endif
   GetComps(OS, mach, verb, targ, ptrbits, usrcomps, nof77, nocygwin, vecexts,
            goodgcc, bindir);
/*
 * See if we need to search for gfortran
 */
   if (!nof77 && usrcomps[F77_] && !strcmp(usrcomps[F77_], "gfortran"))
   {
      free(usrcomps[F77_]);
      usrcomps[F77_] = FindGoodGfortran(OS, mach, verb, targ, goodgcc);
      if (!usrcomps[F77_])
         usrcomps[F77_] = NewStringCopy("gfortran");
   }
   free(goodgcc);
   TestComps(OS, mach, verb, targ, targarg, usrcomps,
             &f2cnam, &f2cint, &f2cstr, nof77);
   if (verb)
   {
      fprintf(stdout, "Compilers:\n");
      for (i=0; i < NCOMP; i++)
         fprintf(stdout, "   '%s' : '%s'\n", usrcomps[i], usrcomps[NCOMP+i]);
      fprintf(stdout, "\n");
   }
   PrintCompResults(outfile, usrcomps,  f2cnam, f2cint, f2cstr);
   for (i=0; i < 3*NCOMP; i++)
      if (usrcomps[i])
         free(usrcomps[i]);
   if (targ)
      free(targarg);
   return(0);
}
