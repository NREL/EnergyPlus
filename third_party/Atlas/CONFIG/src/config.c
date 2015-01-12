#include "atlconf.h"

char *GetStrProbe(int verb, char *targarg, char *prb, char *id)
/*
 * Performs probe where output is string delimited by '', returning string
 * RETURNS: NULL on error, else requested string.
 */
{
   char *sp, *sret=NULL;
   char *ln, *res;
   int i;

   i = strlen(targarg) + strlen(prb) + strlen(id) + 48;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "make IRun_%s args=\"-v %d %s\" | fgrep '%s='",
           prb, verb, targarg, id);

   if (verb > 1)
      printf("cmnd=%s\n", ln);
   res = atlsys_1L(NULL, ln, verb, 0);
   if (res)
   {
      sprintf(ln, "%s='", id);
      sp = strstr(res, ln);
      if (sp)
      {
         sp += strlen(ln);  /* sp pts to start of string */
         for (i=0; sp[i] != '\'' && sp[i] != '\0'; i++);
         if (sp[i] == '\'')
         {
            sp[i] = '\0';  /* get rid of trailing ' and everything behind it */
            sret = NewStringCopy(sp);
         }
      }
      free(res);
   }
   free(ln);
   return(sret);
}

int GetIntProbe(int verb, char *targarg, char *prb, char *id, int N)
{
   char *ln, *res;
   int iret=0, i;

   i = strlen(targarg) + strlen(prb) + strlen(id) + 48;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "make IRun_%s args=\"-v %d %s\" | fgrep '%s='",
           prb, verb, targarg, id);
   if (verb > 1)
      printf("cmnd=%s\n", ln);
   res = atlsys_1L(NULL, ln, verb, 0);
   free(ln);
   if (res)
   {
      iret = GetLastInt(res);
      free(res);
   }
   if (N)
   {
      if (iret > N || iret < 1)
      {
         printf("\nBad %s value=%d, res='%s'\n", id, iret, res ? res:"NULL");
         iret = 0;
      }
   }
   return(iret);
}

int GetIntProbeSure(int verb, char *targarg, char *prb, char *id,
                    int *sure)
{
   char *ln, *res;
   int iret=0, ierr, i;

   i = strlen(targarg) + strlen(prb) + strlen(id) + 48;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "make IRun_%s args=\"-v %d %s\" | fgrep '%s='",
           prb, verb, targarg, id);
   if (verb > 1)
      printf("cmnd=%s\n", ln);
   res = atlsys_1L(NULL, ln, verb, 0);
   free(ln);
   if (res)
   {
      iret = GetFirstInt(res);
      *sure = GetLastInt(res);
      free(res);
   }
   return(iret);
}

char *TransCompsToFlags(char **comps)
{
   int len, newlen;
   int i, j;
/*
 * WARNING: if you change the order of the ICC_, etc, must change
 *          compnames!
 */
   char *compnames[NCOMP] = {"ic", "sm", "dm", "sk",  "dk", "xc", "gc", "if"};
   char *flags=NULL, *sp;
   char flgtmp[8], aflg[9];
/*
 * Up-to-date check, failure will remind me to update this routine!
 */
   assert(NCOMP == 8);
   assert(ICC_ == 0);
   assert(SMC_ == 1);
   assert(DMC_ == 2);
   assert(SKC_ == 3);
   assert(DKC_ == 4);
   assert(XCC_ == 5);
   assert(GCC_ == 6);
   assert(F77_ == 7);
/*
 * Setup prefixes of form "-X xx '" and "-Fa xx '" for appending
 * X will be replaced by F or C, xx by compiler prefix of compnames
 */
   flgtmp[0] = '-';
   flgtmp[5] = flgtmp[2] = ' ';
   flgtmp[6] = '\'';
   flgtmp[7] = '\0';
   aflg[0] = '-'; aflg[1] = 'F'; aflg[2] = 'a'; aflg[3] = ' ';
   aflg[6] = ' '; aflg[7] = '\''; aflg[8] = '\0';
/*
 * Pass any override compilers/flags and appends to probe
 */
   for (i=0; i < NCOMP; i++)
   {
      flgtmp[3] = compnames[i][0];
      flgtmp[4] = compnames[i][1];
      if (comps[i])
      {
         flgtmp[1] = 'C';
         flags = NewAppendedString(flags, flgtmp);  /* append "-C cc '" */
         flags = NewAppendedString0(flags, comps[i]);
         flags = NewAppendedString0(flags, "'");
      }
      if (comps[NCOMP+i])
      {
         flgtmp[1] = 'F';
         flags = NewAppendedString(flags, flgtmp);  /* append "-F cc '" */
         flags = NewAppendedString0(flags, comps[NCOMP+i]);
         flags = NewAppendedString0(flags, "'");
      }
      if (comps[2*NCOMP+i])
      {
         aflg[4] = compnames[i][0];
         aflg[5] = compnames[i][1];
         flags = NewAppendedString(flags, aflg);  /* append "-Fa cc '" */
         flags = NewAppendedString0(flags, comps[2*NCOMP+i]);
         flags = NewAppendedString0(flags, "'");
      }
   }
   if (!flags)
   {
      flags = malloc(sizeof(char));
      flags[0] = '\0';
   }
   return(flags);
}

char *ProbeComp(int verb, char *targarg, enum OSTYPE OS, enum MACHTYPE arch,
                 char **comps, int nof77, int nocygwin, int ptrbits, int vecext)
/*
 * RETURNS: f2c define string
 */
{
   char *ln, *comp=NULL, *flag=NULL, *flags=NULL;
   char stmp[32];
   int f2cname, f2cint, f2cstr;
   char *f2cdefs;
   int len, i, cflen=0, if77=0;
   FILE *fpin;

   flags = TransCompsToFlags(comps);
   i = strlen(targarg) + strlen(flags) + 128;
   ln = malloc(i*sizeof(char));
   i = sprintf(ln, "make IRun_comp args=\"-v %d -o atlconf.txt -O %d -A %d -Si nof77 %d -V %d %s %s",
               verb, OS, arch, nof77, vecext, targarg, flags);
   free(flags);
   if (ptrbits == 64 || ptrbits == 32)
   {
      sprintf(stmp, "-b %d", ptrbits);
      ln = NewAppendedString(ln, stmp);
   }
   if (nocygwin)
      ln = NewAppendedString(ln, "-Si nocygwin 1");
   ln = NewAppendedString0(ln, "\"");
   len = strlen(ln);
   if (verb > 1)
      fprintf(stderr, "cmnd='%s'\n", ln);
   syschk(ln);
   fpin = fopen("atlconf.txt", "r");
   assert(fpin);
   while ((ln=ATL_fgets(ln, &len, fpin)))
   {
      if (cflen < len)
      {
         if (comp)
            free(comp);
         cflen = len;
         comp = malloc(2*len*sizeof(char));
         assert(comp);
         flag = comp+len;
      }
      if (ln[0] != '#')
      {
         if (isdigit(ln[0]))
         {
            assert(sscanf(ln, "%d '%[^']' '%[^']", &i, comp, flag) == 3);
            assert(i >= 0 && i < NCOMP);
            comps[i] = NewStringCopy(comp);
            comps[NCOMP+i] = NewStringCopy(flag);
         }
         else
         {
            for (i=0; ln[i] && ln[i] != '('; i++);
            assert(ln[i] = '(');
            assert(sscanf(ln+i+1, "%d,%d,%d", &f2cname, &f2cint, &f2cstr) == 3);
            if77 = 1;
         }
      }
   }
   fclose(fpin);
   if (comp)
      free(comp);
/*
 * Add the flag to build shared objects, if we know what it is
 */
#ifdef ATL_DYLIBS
   for (i=0; i < NCOMP; i++)
   {
      if (i != XCC_)
      {
         if (CompIsGcc(comps[i]) || CompIsAppleGcc(comps[i]) ||
             CompIsPathScale(comps[i]))
         {
            if (!OSIsWin(OS))  /* windows/gcc bitches about -fPIC flag */
               comps[i+NCOMP] = NewAppendedString(comps[i+NCOMP],"-fPIC");
         }
         else if (CompIsMIPSpro(comps[i]))
            comps[i+NCOMP] = NewAppendedString(comps[i+NCOMP],"-KPIC");
         else if (CompIsSunWorkshop(comps[i]))
            comps[i+NCOMP] = NewAppendedString(comps[i+NCOMP],"-KPIC");
#if 0 /* I don't think a special flag is needed for shared obj wt xlc */
         else if (CompIsIBMXL(comps[i]))
            comps[i+NCOMP] = NewAppendedString(comps[i+NCOMP],"????");
#endif
      }
   }
#endif
/*
 * Only f77 and xcc allowed to be NULL
 */
   for (i=0; i < NCOMP; i++)
   {
      if (comps[i] == NULL)
      {
         if (i == XCC_)
         {
            comps[XCC_] = NewStringCopy(comps[ICC_]);
            comps[NCOMP+XCC_] = NewStringCopy(comps[ICC_+NCOMP]);
         }
         else assert(i == F77_);
      }
      if (comps[i+NCOMP] == NULL)
      {
         if (i == XCC_)
            comps[XCC_+NCOMP] = NewStringCopy(comps[ICC_+NCOMP]);
         else assert(i == F77_);
      }
   }
/*
 * Echo compiler info to screen if user has asked for verbose output
 */
   if (verb)
   {
      fprintf(stdout, "Selected compilers:\n");
      for (i=0; i < NCOMP; i++)
      {
         fprintf(stdout, "%s = '%s' '%s'\n", COMPNAME[i],
                 comps[i] ? comps[i]:"none",
                 comps[NCOMP+i]?comps[NCOMP+i]:"none");
      }
      if (if77)
      {
         fprintf(stdout, "\nF77 calling C interface information:\n");
         fprintf(stdout, "   Name decoration = %s\n", f2c_namestr[f2cname]);
         fprintf(stdout, "   Integer style   = %s\n", f2c_intstr[f2cint]);
         fprintf(stdout, "   String style    = %s\n", f2c_strstr[f2cstr]);
      }
      else
         fprintf(stderr, "F77/C interface not defined!");
   }
   if (!if77)
      f2cdefs = NULL;
   else
   {
      i = strlen(f2c_namestr[f2cname]) + strlen(f2c_intstr[f2cint]) +
          strlen(f2c_strstr[f2cstr]) + 16;
      f2cdefs = malloc(i*sizeof(char));
      assert(f2cdefs);
      sprintf(f2cdefs, "-D%s -D%s -DString%s",
              f2c_namestr[f2cname], f2c_intstr[f2cint], f2c_strstr[f2cstr]);
   }
   return(f2cdefs);
}

int PathLength(char *str)
/*
 * Given string, finds end unix path, allowing \ to mean sticky space, stops at
 * first non-sticky space.  Skips any leading spaces.
 * RETURNS: index of first non-sticky space
 * NOTE: Assumes str[-1] valid
 */
{
   int i;
   for (i=0; isspace(str[i]); i++);  /* skip leading spaces */
   for (; str[i]; i++)
      if (str[i] == ' ' && i > 0 && str[i-1] != '\\')
         break;
   return(i);
}

int TestF77LIB(int verb, char *targarg, enum OSTYPE OS, enum MACHTYPE arch,
               char **comps, char *incd, char *f77lib)
{
   const char *frm ="make IRunTestCFLink F77='%s' F77FLAGS='%s' CC='%s' CCFLAGS='%s -L%s' F77LIB='%s' LIBS='-lm'";
   char *ln;
   int i;

   i = strlen(frm) + strlen(incd) + strlen(f77lib) +
       strlen(comps[F77_]) + strlen(comps[ICC_]) +
       strlen(comps[NCOMP+F77_]) + strlen(comps[NCOMP+ICC_]) + 1;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, frm, comps[F77_], comps[NCOMP+F77_],
           comps[ICC_], comps[NCOMP+ICC_], incd, f77lib);
   if (verb > 1)
      fprintf(stderr, "cmnd = %s\n", ln);
   if (verb)
      fprintf(stderr, "Trying F77 link path of %s ... ", incd);
   if (!system(ln))
   {
      if (verb) fprintf(stderr, "SUCCESS!\n");
      free(ln);
      return(1);
   }
   else if (verb)
      fprintf(stderr, "REJECTED!!!!\n");
   free(ln);
   return(0);
}

char *FindF77LIBInList(int verb, char *targarg, enum OSTYPE OS,
                       enum MACHTYPE arch, char **comps, char *f77lib,
                       char **paths)
{
   int i;
   if (!paths)
      return(NULL);
   for (i=0; paths[i]; i++)
   {
      char *path;
      path = GetPathWithoutName(paths[i]);
      if (path && TestF77LIB(verb, targarg, OS, arch, comps, path, f77lib))
      {
         free(path);
         return(FreeListGetString(paths, i));
      }
      free(path);
   }
   KillAllStringsInList(paths);
   return(NULL);
}

char *FindF77LIB0(int verb, char *targarg, enum OSTYPE OS, enum MACHTYPE arch,
                  char **comps, char *f77lib, char *f77libnam, char *path)
{
   char *frm="find %s -name '%s'", *cmnd;
   char **libs=NULL;
   int i;
   i = strlen(frm) + strlen(f77libnam) + strlen(path) + 1;
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd, frm, path, f77libnam);
fprintf(stderr, "cmnd='%s'\n", cmnd);
   libs = GetLinesFromFile(atlsys(NULL, cmnd, verb, 1), NULL);
PrintAllStringsInList("FOUND:", libs);
   free(cmnd);
   if (!libs)
      return(NULL);

   return(FindF77LIBInList(verb, targarg, OS, arch, comps, f77lib, libs));
}

char *FindF77LIB(int verb, char *targarg, enum OSTYPE OS, enum MACHTYPE arch,
                 char **comps, char *f77lib, char *f77libnam)
{
   char *path, *sp;
   int i;

/*
 * See if we can find by substituting "lib" for "bin" in compiler path
 */
   path = GetPathWithoutName(comps[F77_]);
   if (path)
   {
      sp = strstr(path, "/bin");
      if (sp)
      {
         sp[1] = 'l';
         sp[2] = 'i';
         sp[3] = 'b';
      }
      sp = FindF77LIB0(verb, targarg, OS, arch, comps, f77lib, f77libnam, path);
      free(path);
      if (sp)
         return(sp);
   }
/*
 * Scope some OS-specific places
 */
   if (OS == OSOSX)  /* /sw is for fink, /opt for macports */
   {
      sp = FindF77LIB0(verb, targarg, OS, arch, comps, f77lib, f77libnam,
                       "/sw/lib /opt/lib /opt/local/lib");
      if (sp)
         return(sp);
   }
/*
 * Scope standard unix places
 */
   sp = FindF77LIB0(verb, targarg, OS, arch, comps, f77lib, f77libnam,
           "/lib /usr/lib /usr/local/lib /lib64 /usr/lib64 /usr/local/lib64");
   if (verb)
   {
      if (sp)
         printf("F77libdir = %s\n", sp);
      else
         printf("F77LIB left blank\n");
   }
   return(sp);
}

char *BuildF77LinkLine(enum OSTYPE OS, char *path, char *libnam)
{
   char *sp, *OSextra="";
   int i;

   if (OSIsWin(OS))
      OSextra = "-lgcc";
   i = strlen(path) + strlen(libnam) + strlen(OSextra) + 2 + 2 + 1 + 1 + 1;
   sp = malloc(i*sizeof(char));
   assert(sp);
   sprintf(sp, "-L%s -l%s %s", path, libnam, OSextra);
   return(sp);
}

char *ProbeF77LIB(int verb, char *targarg, enum OSTYPE OS,
                  enum MACHTYPE arch, char **comps, char *f2cdefs, int nof77)
/*
 * Tries to find the directory that needs to be included to link in f77
 * routines
 */
{
   char *cmnd, *res;
   char *f77lib, *F77LIBdir, *sp, *suff, *f77libnam;
   int i;
   char ch;

   if (nof77 || !f2cdefs) return(NULL);
   if (CompIsGcc(comps[F77_]))
   {
      if  (strstr(comps[F77_], "g77")) f77lib = "g2c";
      else f77lib = "gfortran";
   }
   else
   {
      if (verb > 1)
         fprintf(stderr, "Unknown F77 compiler, leaving F77LIBS blank!\n");
      return(NULL);
   }
/*
 * Figure out full name of the libgfortran we are looking ofr
 */
   if (OS == OSOSX)
      suff = ".dylib";
   else if (OSIsWin(OS))
      suff = ".dll.a";
   else
      suff = ".so";
   i = 3 + strlen(f77lib) + strlen(suff) + 1;
   f77libnam = malloc(i*sizeof(char));
   assert(f77libnam);
   sprintf(f77libnam, "lib%s%s", f77lib, suff);
/*
 * First, see if we can use 'gfortran -print-file-name=<f77libnam>' to
 * get definitive answer.  If so, return it.
 */
   i = strlen(comps[F77_]) + strlen(comps[F77_+NCOMP]) + strlen(f77libnam) + 21;
   sp = malloc(i*sizeof(char));
   assert(sp);
   sprintf(sp, "%s %s -print-file-name=%s",
           comps[F77_], comps[F77_+NCOMP], f77libnam);
   res = atlsys_1L(NULL, sp, verb, 0);
   if (res)
   {
      free(f77libnam);
      free(sp);
      fprintf(stdout, "REPORTED: res=%s\n", res);
      sp = GetPathWithoutName(res);
      free(res);
      F77LIBdir = BuildF77LinkLine(OS, sp, f77lib);
      free(sp);
      if (verb)
         fprintf(stderr, "F77LIB = %s\n", F77LIBdir);
      return(F77LIBdir);
   }
   free(sp);
/*
 * If we are using a gfortran without the above option, try to parse
 * a verbose link line for the answer
 */
   i = strlen(comps[F77_]) + strlen(comps[NCOMP+F77_]) + 33;
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd,  "make IRunFlib F77='%s' F77FLAGS='%s'",
           comps[F77_], comps[NCOMP+F77_]);
   if (verb > 1)
      fprintf(stderr, "LIBDIR cmnd = %s\n", cmnd);
   res = atlsys_1L(NULL, cmnd, verb, 0);
   free(cmnd);
   if (verb > 1)
      fprintf(stderr, "LIBDIR res = %s\n", res);
/*
 * Find which -L leads us to f77lib
 */
   sp = res;
   while (sp = strstr(sp, "-L"))
   {
      sp += 2;
      i = PathLength(sp);
      if (i)
      {
         char *sp2;
         ch = sp[i];
         sp[i] = '\0';
         sp2 = NewStringCopy(sp);
         sp2 = NewAppendedString(sp2, f77libnam);
         if (FileIsThere(sp2) &&
             TestF77LIB(verb, targarg, OS, arch, comps, sp, f77lib))
         {
            F77LIBdir = NewStringCopy(sp-2);
            F77LIBdir = NewAppendedString(F77LIBdir, "-l");
            F77LIBdir = NewAppendedString0(F77LIBdir, f77lib);
            if (OSIsWin(OS))
               F77LIBdir = NewAppendedString(F77LIBdir, "-lgcc");
            if (verb)
               fprintf(stderr, "F77LIB = %s\n", F77LIBdir);
            free(sp2);
            free(f77libnam);
            free(res);
            return(F77LIBdir);
         }
         free(sp2);
         sp[i] = ch;
         sp += i;
      }
   }
   free(res);
   sp = FindF77LIB(verb, targarg, OS, arch, comps, f77lib, f77libnam);
   free(f77libnam);
   if (sp)
   {
      F77LIBdir = NewStringCopy("-L");
      suff = GetPathWithoutName(sp);
      F77LIBdir = NewAppendedString0(F77LIBdir, suff);
      free(suff);
      free(sp);
      F77LIBdir = NewAppendedString(F77LIBdir, "-l");
      F77LIBdir = NewAppendedString0(F77LIBdir, f77lib);
      if (OSIsWin(OS))
         F77LIBdir = NewAppendedString(F77LIBdir, "-lgcc");
      if (verb)
         fprintf(stderr, "F77LIB = %s\n", F77LIBdir);
   }
   return(NULL);
}

enum OSTYPE ProbeOS(int verb, char *targarg)
{
   enum OSTYPE iret;

   iret = GetIntProbe(verb, targarg, "OS", "OS", NOS);
   printf("\nOS configured as %s (%d)\n", osnam[iret], iret);
   assert(iret);
   return(iret);
}

enum ASMDIA ProbeAsm(int verb, char *targarg, enum OSTYPE OS)
{
   enum ASMDIA asmd=ASM_None;
   char *ln;
   int i;

   i = strlen(targarg) + 11 + 5;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "%s -O %d", targarg, OS);
   asmd = GetIntProbe(verb, ln, "asm", "ASM", NASMD);
   free(ln);
   printf("\nAssembly configured as %s (%d)\n", ASMNAM[asmd], asmd);
   return(asmd);
}

int ProbeVecs(int verb, char *targarg, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret;
   char *ln;

   i = strlen(targarg) + 22 + 9;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "%s -O %d -s %d", targarg, OS, asmb);
   iret = GetIntProbe(verb, ln, "vec", "VECFLAG", (1<<NISA));
   free(ln);
   for (i=0; i < NISA && (iret & (1<<i)) == 0; i++);
   if (i == NISA)
      i = 0;
   printf("\nVector ISA Extension configured as  %s (%d,%d)\n",
          ISAXNAM[i], i, iret);
   return(iret);
}

int ProbeArch(int verb, char *targarg, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret;
   char *ln;

   i = strlen(targarg) + 22 + 12;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "%s -O %d -s %d -a", targarg, OS, asmb);
   iret = GetIntProbe(verb, ln, "arch", "MACHTYPE", NMACH);
   free(ln);
   printf("\nArchitecture configured as  %s (%d)\n",
          machnam[iret], iret);
   return(iret);
}

int ProbeMhz(int verb, char *targarg, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret;
   char *ln;

   i = strlen(targarg) + 22 + 12;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "%s -O %d -s %d -m", targarg, OS, asmb);
   if (verb > 2)
      printf("Mhz Probe = '%s'\n", ln);
   iret = GetIntProbe(verb, ln, "arch", "CPU MHZ", 16384);
   free(ln);
   printf("\nClock rate configured as %dMhz\n", iret);
   return(iret);
}

int ProbeNcpu(int verb, char *targarg, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret;
   char *ln;

   i = strlen(targarg) + 22 + 12;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "%s -O %d -s %d -n", targarg, OS, asmb);
   iret = GetIntProbe(verb, ln, "arch", "NCPU", 2048);
   printf("\nMaximum number of threads configured as  %d\n", iret);
   free(ln);
   return(iret);
}

int ProbePtrbits(int verb, char *targarg, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret;
   char *ln;

   i = strlen(targarg) + 22 + 12;
   ln = malloc(sizeof(char)*i);
   assert(ln);
   sprintf(ln, "%s -O %d -s %d -b", targarg, OS, asmb);
   iret = GetIntProbeSure(verb, ln, "arch", "PTR BITS", &i);
   free(ln);
/*
 * If it's not 64-bit, make sure it's not just because of flag setting
 */
   if (iret != 64)
   {
   }
   if (iret != 64)
      iret = 32;
   printf("\nPointer width configured as %d\n", iret);
   return(iret);
}

int ProbeCPUThrottle(int verb, char *targarg, enum OSTYPE OS, enum ASMDIA asmb)
{
   int i, iret;
   char *ln;
   i = strlen(targarg) + 22 + 12;
   ln = malloc(i*sizeof(char));
   assert(ln);
   sprintf(ln, "%s -O %d -s %d -t", targarg, OS, asmb);
   iret = GetIntProbe(verb, ln, "arch", "CPU THROTTLE", 0);
   free(ln);
   if (iret) printf("CPU Throttling apparently enabled!\n");
   else printf("Cannot detect CPU throttling.\n");
   return(iret);
}

char *NewAppendedString_SFLAG(char *old, char *flag, char *str)
/*
 * RETURNS: string holding <old> + " <flag> '<str>'"
 * NOTE: frees old string after copy
 */
{
   char *new;
   int i;

   i = (old) ? strlen(old)+1 : 1;
   i += 1 + strlen(flag) + 1 + 1 + strlen(str) + 1 + 1;
   new = malloc(i*sizeof(char));
   assert(new);
   sprintf(new, "%s %s '%s'", (old)?old:"", flag, str);
   free(old);
   return(new);
}

char *NewAppendedString_IFLAG(char *old, char *flag, int iflag)
/*
 * RETURNS: string holding <old> + " <flag> <iflag>"
 * NOTE: frees old string after copy
 */
{
   char *new;
   int i;

   i = (old) ? strlen(old)+1 : 1;
   i += 1 + strlen(flag) + 1 + 11;
   new = malloc(i*sizeof(char));
   assert(new);
   sprintf(new, "%s %s %d", (old)?old:"", flag, iflag);
   free(old);
   return(new);
}

char *Comps2Flags(char **comps)
/*
 * Takes the comps array (1st NCOMP entries are compilers, next NCOMP entries
 * flags for those compilers) and translates them into the corresponding flags
 * for xspew (or indeed this config.c)
 * NOTE: assumes appended flags (2*NCOMP+i) already appended to flags (NCOMP+i)
 */
{
   char *cname[NCOMP] = {"ic", "sm", "dm", "sk", "dk", "xc", "gc", "if"};
   char *flags=NULL;
   int i, j=0;

   for (i=0; i < NCOMP; i++)
   {
      char stmp[6] = {'-', 'X', ' ', cname[i][0], cname[i][1], '\0'};
      if (comps[i])
      {
         stmp[1] = 'C';
         flags = NewAppendedString_SFLAG(flags, stmp, comps[i]);
      }
      if (comps[NCOMP+i])
      {
         stmp[1] = 'F';
         flags = NewAppendedString_SFLAG(flags, stmp, comps[NCOMP+i]);
      }
   }
   return(flags);
}

char *ProbePmake(int verb, enum OSTYPE OS, int ncpu)
/*
 * WARNING: if cross-comp really worked, this would be ncpu of front-end,
 *          not backend!
 */
{
   char args[32], *res;
   char *sp;
   int i;

   sprintf(args, "-O %d -t %d", OS, ncpu);

   res = GetStrProbe(verb, args, "pmake", "PMAKE");
   if (res) printf("Parallel make command configured as '%s'\n", res);
   else printf("Parallel make not configured.\n");
   return(res);
}


void SpewItForth(int verb, enum OSTYPE OS, enum MACHTYPE arch, int mhz,
                 enum ASMDIA asmb, int vecexts, int ptrbits,
                 int ncpu, int *tids, int omp, int AntThr,
                 int l2size, char *srcdir, char *bindir, int bozol1,
                 int archdef, int IEEE, int latune, int nof77, int lapackref,
                 char **comps, char *gccflags,
                 char *f2cdefs, char *cdefs, char *pmake, char *flapack,
                 char *smaflags, char *dmaflags, char *f77libs, char *ADd)
/*
 * Calls xspew with correct arguments to build required Make.inc
 */
{
   char *frm = "./xspew -v %d -O %d -A %d -m %d -s %d -V %d -b %d -f %d -d s '%s' -d b '%s' -D c '%s' -D f '%s' %s -Si archdef %d -Si ieee %d -Si bozol1 %d -Si latune %d -Si nof77 %d -o Make.inc";
   char *ln, *compsflags, archflags[1024];
   int i;

   assert(!system("make xspew"));
   compsflags = Comps2Flags(comps);  /* Xlate comp/flag array to xspew flags */
   i = strlen(frm) + 11*13 + strlen(srcdir) + strlen(bindir);
   if (cdefs)
      i += strlen(cdefs);
   if (f2cdefs)
      i += strlen(f2cdefs);
   i += strlen(compsflags);
   if (tids)
   {
      i += ncpu * 12 + 6;
   }
   else
      i += 16;
   ln = malloc(i*sizeof(char));
   assert(ln);
   i = sprintf(ln, frm, verb, OS, arch, mhz, asmb, vecexts, ptrbits, l2size,
               srcdir, bindir, cdefs ? cdefs:"", f2cdefs? f2cdefs : "",
               compsflags, archdef, IEEE, bozol1, latune, nof77);
   free(compsflags);
   if (tids)
   {
      int k;
      i += sprintf(ln+i, " -tl %d", ncpu);
      for (k=0; k < ncpu; k++)
         i += sprintf(ln+i, " %d", tids[k]);
   }
   else
      i += sprintf(ln+i, " -t %d", ncpu);
   if (pmake)
      ln = NewAppendedString_SFLAG(ln, "-Ss pmake", pmake);
   if (flapack)
      ln = NewAppendedString_SFLAG(ln, "-Ss flapack", flapack);
   if (smaflags)
      ln = NewAppendedString_SFLAG(ln, "-Ss smaflags", smaflags);
   if (dmaflags)
      ln = NewAppendedString_SFLAG(ln, "-Ss dmaflags", dmaflags);
   if (f77libs)
      ln = NewAppendedString_SFLAG(ln, "-Ss f77lib", f77libs);
   if (ADd)
      ln = NewAppendedString_SFLAG(ln, "-Ss ADdir", ADd);
   if (omp)
      ln = NewAppendedString_IFLAG(ln, "-Si omp", omp);
   if (AntThr)
      ln = NewAppendedString_IFLAG(ln, "-Si antthr", AntThr);
   if (lapackref)
      ln = NewAppendedString_IFLAG(ln, "-Si lapackref", lapackref);
   if (gccflags)
      ln = NewAppendedString_SFLAG(ln, "-Fa gc", gccflags);
   if (verb > 1)
      fprintf(stderr, "cmnd='%s'\n", ln);
   syschk(ln);
   free(ln);
}

void PrintUsage(char *name, int iarg, char *arg)
{
   fprintf(stderr, "\nERROR around arg %d (%s).\n", iarg,
           arg ? arg : "unknown");
   fprintf(stderr, "USAGE: %s [flags] where flags are:\n", name);
   fprintf(stderr, "   -v <verb> : verbosity level\n");
   fprintf(stderr, "   -O <enum OSTYPE #>  : set OS type\n");
   fprintf(stderr, "   -s <enum ASMDIA #>  : set assembly dialect\n");
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
   fprintf(stderr, "        gc: append flags to gcc compiler used in user-contributed index files.\n");
   fprintf(stderr, "        acg: append to all C compilers & the index gcc\n");
   fprintf(stderr, "        alg: append to all compilers & the index gcc\n");
   fprintf(stderr,
      "   -T <targ> : ssh target for cross-compilation (probably broken)\n");
   fprintf(stderr, "   -D [c,f] -D<mac>=<rep> : cpp #define to add to [CDEFS,F2CDEFS]\n");
   fprintf(stderr,
   "      eg. -D c -DL2SIZE=8388604 -D f -DADD__ -D f -DStringSunStyle\n");
   fprintf(stderr, "   -d [s,b]  : set source/build directory\n");
   fprintf(stderr, "   -f <#> : size (in KB) to flush before timing\n");
   fprintf(stderr,
           "   -t <#> : set # of threads (-1: autodect; 0: no threading)\n");
   fprintf(stderr, "   -m <mhz> : set clock rate\n");
   fprintf(stderr, "   -S[i/s] <handle> <val>  : special int/string arg\n");
   fprintf(stderr,
           "      -Si bozol1 <0/1> : supress/enable bozo L1 defaults\n");
   fprintf(stderr,
           "      -Si archdef <1/0> : enable/supress arch default use\n");
   fprintf(stderr,
"      -Si ieee <1/0> : dis/allow optimizations that break IEEE FP standard\n");
   fprintf(stderr,
           "          (eg., NEON, 3DNow!)\n");
   fprintf(stderr,
           "      -Si latune <1/0> : do/don't tune F77 LAPACK routines\n");
      fprintf(stderr,
        "      -Si nof77 <0/1> : Have/don't have fortran compiler\n");
      fprintf(stderr,
        "      -Si nocygwin <0/1> : Do/don't depend on GPL cygwin library\n");
      fprintf(stderr,
        "                           (Windows compiler/cygwin install only)\n");
/* Disabled due to abuse
      fprintf(stderr,
        "      -Si cputhrchk <0/1> : Ignore/heed CPU throttle probe\n");
 */
      fprintf(stderr,
           "   -tl <#> <list> : set # of threads, use list of affinity IDs\n");
      fprintf(stderr,
        "      -Si omp <0/1> : don'tuse/use OpenMP for threading\n");
      fprintf(stderr,
"      -Si antthr <0/1/2> : nobuild/build/use Antoine's code for threading\n");
      fprintf(stderr,
              "      -Si lapackref <0/1>: Netlib lapack is not/is unpacked\n");
      fprintf(stderr, "                           to $BLDdir/src/lapack/ref\n");
   fprintf(stderr,
        "      -Ss kern <path/to/comp> : use comp for all kernel compilers\n");
   fprintf(stderr,
      "      -Ss ADdir <path/to/archdefs> : Get archdefs frm custom path\n");
   fprintf(stderr,
        "      -Ss pmake <parallel make invocation (eg '$(MAKE) -j 4')>\n");
   fprintf(stderr,
"      -Ss f77lib <path to f77 lib needed by C compiler>\n");
   fprintf(stderr,
"      -Ss flapack <path to netlib lapack>: used to build full lapack lib\n");
   fprintf(stderr, "      -Ss [s,d]maflags 'flags'\n");
   fprintf(stderr,
      "NOTE: enum #s can be found by : make xprint_enums ; ./xprint_enums\n");
   exit(iarg);
}

void GetFlags(int nargs,                /* nargs as passed into main */
              char **args,              /* args as passed into main */
              int *verb,                /* verbosity setting */
              enum OSTYPE *OS,          /* OS to assume */
              enum ASMDIA *asmb,        /* assembly dialect to assume */
              int *vec,                 /* Vector ISA extension bitfield */
              enum MACHTYPE *mach,     /* machine/arch to assume */
              int *mhz,                /* Clock rate in Mhz */
              int *ptrbits             /* # of bits in ptr: 32/64 */,
              int *nthreads,           /* # of threads */
              int **tids,              /* thread affinity ID list */
              int *omp,                /* Build OpenMP version of threading? */
              int *AntThr,             /* Build Antoine's threads? */
              char **comps,
              char **gccflags,        /* append flags for user-contrib gcc */
              char **outfile,
              char **srcdir,          /* path to top of source directory */
              char **bindir,          /* path to top of binary directory */
              int *bozol1,            /* Use untuned L1 */
              int *UseArchDef,        /* Use arch defaults */
              int *IEEE,              /* enforce IEEE FP standard? */
              int *latune,            /* Tune LAPACK? */
              int *NoF77,
              int *NoCygwin,
              int *ThrChk,
              int *lapackref,
              char **f2cdefs,         /* F77-to-C interface defines */
              char **ecdefs,          /* extra cpp defines to add to CDEFS */
              char **pmake,           /* parallel make command */
              char **flapack,         /* netlib F77 LAPACK  */
              char **smaflags,       /* single prec muladd flags */
              char **dmaflags,       /* double prec muladd flags */
              char **f77lib,         /* netlib F77 LAPACK  */
              char **ADd,            /* ArchDef directory */
              int *flush,             /* size in KB to flush */
              char **targ             /* mach to ssh to*/
             )
{
   int i, k, k0, kn, DoInt;
   char *sp, *sp0;
   char *gcc3=NULL;
   char *cdefs=NULL, *fdefs=NULL;
   char ln[1024];

   *verb = 0;
   *ADd = NULL;
   *srcdir = *bindir = NULL;
    *bozol1 = 0;
    *IEEE = *latune = *UseArchDef = 1;
    *flapack = NULL;
    *f77lib = NULL;
    *smaflags = *dmaflags = NULL;
    *mhz = 0;
   *outfile = NULL;
   *targ = NULL;
   for (k=0; k < NCOMP*3; k++)
      comps[k] = NULL;
   *gccflags = NULL;

   *flush = 0;
   *ptrbits = 0;
   *mhz = 0;
   *mach = 0;
   *vec = 0;
   *asmb = 0;
   *OS = 0;
   *verb = 0;
   *NoCygwin = 0;
   *NoF77 = 0;
   *ThrChk = 1;
   *nthreads = -1;
   *tids = NULL;
   *omp = *AntThr = 0;
   *pmake = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 't':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *nthreads = atoi(args[i]);
         if (args[i-1][2] == 'l')
         {
            *tids = malloc(*nthreads * sizeof(int));
            assert(*tids);
            for (k=0; k < *nthreads; k++)
            {
               if (++i >= nargs)
                  PrintUsage(args[0], i, "out of arguments");
               (*tids)[k] = atoi(args[i]);
            }
         }
         break;
      case 'f':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *flush = atoi(args[i]);
         break;
      case 'b':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *ptrbits = atoi(args[i]);
         break;
      case 'm':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *mhz = atoi(args[i]);
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
      case 's':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *asmb = atoi(args[i]);
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
         if (!strcmp(sp0, "archdef"))
            *UseArchDef = k;
         else if (!strcmp(sp0, "ieee"))
            *IEEE = k;
         else if (!strcmp(sp0, "bozol1"))
            *bozol1 = k;
         else if (!strcmp(sp0, "latune"))
            *latune = k;
         else if (!strcmp(sp0, "omp"))
            *omp = k;
         else if (!strcmp(sp0, "antthr"))
            *AntThr = k;
         else if (!strcmp(sp0, "lapackref"))
            *lapackref = k;
         else if (!strcmp(sp0, "nof77"))
            *NoF77 = k;
         else if (!strcmp(sp0, "nocygwin"))
            *NoCygwin = k;
         else if (!strcmp(sp0, "kern"))
            gcc3 = sp;
         else if (!strcmp(sp0, "ADdir") || !strcmp(sp0, "addir"))
            *ADd = sp;
         else if (!strcmp(sp0, "pmake"))
            *pmake = sp;
        else if (!strcmp(sp0, "flapack"))
           *flapack = sp;
        else if (!strcmp(sp0, "f77lib"))
           *f77lib = sp;
        else if (!strcmp(sp0, "smaflags"))
           *smaflags = sp;
        else if (!strcmp(sp0, "dmaflags"))
           *dmaflags = sp;
         else
            PrintUsage(args[0], i-1, sp0);
         break;
      case 'o':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *outfile = args[i];
         break;
      case 'D':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         if (args[i-1][0] == 'f')
            fdefs = NewAppendedString(fdefs, args[i]);
         else
            cdefs = NewAppendedString(cdefs, args[i]);
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
            if (sp[2] == 'g' && args[i-2][1] == 'F')
               *gccflags = args[i];
         }
         else if (*sp == 'g' && sp[1] == 'c')
         {
            if (++i >= nargs)
               PrintUsage(args[0], i, "out of arguments");
            *gccflags = args[i];
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
/*
 * If the special flag -Ss gcc3 is thrown, force gcc3's use for all kernel
 * compilers (standard gcc assumed to be gcc4)
 */
   if (gcc3)
   {
      for (i=0; i < NCOMP; i++)
      {
         if (!comps[i] && (i == SMC_ || i == DMC_ || i == SKC_ || i == DKC_))
            comps[i] = NewStringCopy(gcc3);
      }
   }
   *f2cdefs = fdefs;
   *ecdefs = cdefs;
   if (*ptrbits != 32 && *ptrbits != 64)
      *ptrbits = 0;
}

int main(int nargs, char **args)
{
   enum OSTYPE OS;
   enum MACHTYPE mach;
   int i, verb, asmb, f2cname, f2cint, f2cstr, ncpu, nof77, nocygwin;
   int thrchk, mhz, omp, AntThr, lapackref;
   int j, k, h, vecexts;
   int ptrbits, l2size, bozol1, latune, archdef, ieee;
   int *tids;
   char *targ, *f2cdefs, *cdefs, *srcdir, *bindir, *outfile, *sp;
   char targarg[256];
   char *comps[3*NCOMP], *gccflags;
   char *pmake, *flapack, *smaflags, *dmaflags, *f77libs, *ADd;

   GetFlags(nargs, args, &verb, &OS, (enum ASMDIA*) &asmb, &vecexts, &mach,
            &mhz, &ptrbits, &ncpu, &tids, &omp, &AntThr, comps, &gccflags,
            &outfile, &srcdir, &bindir, &bozol1, &archdef, &ieee, &latune,
            &nof77, &nocygwin, &thrchk, &lapackref, &f2cdefs, &cdefs, &pmake,
            &flapack, &smaflags, &dmaflags, &f77libs, &ADd, &l2size, &targ);
   if (targ)
      sprintf(targarg, "-T %s", targ);
   else
      targarg[0] = '\0';
   if (OS == OSOther)
      OS = ProbeOS(verb, targarg);
   if (asmb == ASM_None)
      asmb = ProbeAsm(verb, targarg, OS);
   else if (asmb < 0)
      asmb = 0;
   if (!vecexts)
      vecexts = ProbeVecs(verb, targarg, OS, asmb);
   else if (vecexts < 0)
      vecexts = 0;
   if (mach == MACHOther)
      mach = ProbeArch(verb, targarg, OS, asmb);
   if (!mhz)
      mhz = ProbeMhz(verb, targarg, OS, asmb);
   if (ncpu < 0)
      ncpu = ProbeNcpu(verb, targarg, OS, asmb);
   if (!pmake && ncpu > 1)
      pmake = ProbePmake(verb, OS, ncpu);
   if (ptrbits == 0)
   {
      if (asmb == gas_x86_64)
         ptrbits = 64;
      else
         ptrbits = ProbePtrbits(verb, targarg, OS, asmb);
   }
   if (ProbeCPUThrottle(verb, targarg, OS, asmb))
   {
      fprintf(stderr,
         "It appears you have cpu throttling enabled, which makes timings\n");
      fprintf(stderr,
              "unreliable and an ATLAS install nonsensical.  Aborting.\n");
      fprintf(stderr,
              "See ATLAS/INSTALL.txt for further information\n");
      if (thrchk) exit(1);
      else fprintf(stderr, "Ignoring CPU throttling by user override!\n\n");
   }
/*
 * Override 32/64 bit assembler if asked
 */
   if (asmb == gas_x86_64 && ptrbits == 32)
      asmb = gas_x86_32;
   else if (asmb == gas_x86_32 && ptrbits == 64)
      asmb = gas_x86_64;
/*
 * Now that we've detected architecture stuff, kill assembly dialect for
 * 64-bit install of Win64, since we can't use our normal AMD64 assembly
 * there due to incompatable ABI
 */
   if (OS == OSWin64 && ptrbits == 64)
      asmb = ASM_None;

   sp = ProbeComp(verb, targarg, OS, mach, comps, nof77, nocygwin, ptrbits,
                  vecexts);
   if (nof77)
      f2cdefs = NewStringCopy("-DATL_NoF77");
   else if (!f2cdefs) f2cdefs = sp;
   if (!f77libs)
      f77libs = ProbeF77LIB(verb, targarg, OS, mach, comps, f2cdefs, nof77);
/*
 * If user has not specified muladd flags (which are suffixed to kernel flags),
 * add flags to keep gcc 4 from hanging, if necessary
 */
   SpewItForth(verb, OS, mach, mhz, asmb, vecexts, ptrbits, ncpu, tids, omp,
               AntThr, l2size, srcdir, bindir, bozol1, archdef, ieee, latune,
               nof77, lapackref, comps, gccflags, f2cdefs, cdefs, pmake,
               flapack, smaflags, dmaflags, f77libs, ADd);
   for (i=0; i < 3*NCOMP; i++)
      if (comps[i])
         free(comps[i]);
   free(f77libs);
   free(cdefs);
   free(f2cdefs);
   free(pmake);
/*
 * Cleanup directory, and exit
 */
   system("make confclean");
   return(0);
}
