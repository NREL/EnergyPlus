/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1998 R. Clint Whaley
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
#include "atlconf.h"
#include "atlconf_misc.h"

#include <time.h>
#include <ctype.h>
void GetDate(int *month, int *day, int *year, int *hour, int *min)
{
   time_t tv;
   struct tm *tp;

   tv = time(NULL);
   tp = localtime(&tv);
   *month = tp->tm_mon + 1;
   *day = tp->tm_mday;
   *year = tp->tm_year + 1900;
   *hour = tp->tm_hour;
   *min = tp->tm_min;
}

long GetInt(FILE *fpin, long Default, char *spc, char *expstr)
/*
 * Gets a signed integral type from fpin.  If nothing or garbage is entered,
 * Default is returned.
 */
{
   char str[64];
   long iin;
   if (expstr) fprintf(stdout, "%sEnter %s [%ld]: ", spc, expstr, Default);
   if (fgets(str, 64, fpin) == NULL) return(Default);
   if (sscanf(str, " %ld ", &iin) != 1) return(Default);
   return(iin);
}

long GetIntRange(long Default, long Min, long Max, char *spc, char *expstr)
{
   long i;
   int keepOn=0;
   do
   {
      i = GetInt(stdin, Default, spc, expstr);
      if (i > Max)
      {
         keepOn = 1;
         fprintf(stderr, "\n%ld larger than max value of %ld.  Try again.\n\n",
                 i, Max);
      }
      else if (i < Min)
      {
         keepOn = 1;
         fprintf(stderr, "\n%ld smaller than min value of %ld.  Try again.\n\n",
                 i, Min);
      }
      else keepOn = 0;
   }
   while (keepOn);
   return(i);
}

long GetIntVer(long Default, long Min, long Max, char *spc, char *expstr)
{
   long i, j;

   do
   {
      i = GetIntRange(Default, Min, Max, spc, expstr);
      fprintf(stdout, "%s   You entered: %ld\n", spc, i);
      j = GetIntRange(0, 0, 1, spc, "1 to reenter, 0 accepts");
   }
   while(j);
   return(i);
}


void GetString(FILE *fpin, char *Default, char *spc, char *expstr,
               int len, char *str0)
/*
 * Get a string of length len, not including NULL terminator; pads
 * any extra len with NULLs
 */
{
   char str[512], *sp;
   int i;

   assert(len+1 <= 512);
   if (expstr)
   {
      if (Default) fprintf(stdout, "%sEnter %s [%s]: ", spc, expstr, Default);
      else fprintf(stdout, "%sEnter %s:", spc, expstr);
   }
   sp = fgets(str, 512, fpin);
   if ( (sp == NULL) || (str[0] == '\0') || (str[0] == '\n') )
   {
      if (Default) strcpy(str0, Default);
      else str0[0] = '\0';
      return;
   }
   str[len] = '\0';
   for (i=0; str0[i] = str[i]; i++);
   if (i) i--;
   while (Mciswspace(str0[i])) i--;
   while (++i < len) str0[i] = '\0';
   str0[i] = '\0';
}

void GetStrVer(char *def, char *spc, char *expstr, int len, char *str)
{
   int i;

   do
   {
      GetString(stdin, def, spc, expstr, len, str);
      fprintf(stdout, "%sYou have entered '%s'\n", spc, str);
      i = GetIntRange(0, 0, 1, spc, "1 to reenter, 0 to accept");
   }
   while(i);
}

int IsYes(char def, char *spc, char *expstr)
{
   char ch, ln[256];
   fprintf(stdout, "%s%s [%c]: ", spc, expstr, def);
   if (fgets(ln, 256, stdin) == NULL) ch=def;
   else if (ln[0] == '\0' || ln[0] == '\n') ch=def;
   else ch = ln[0];
   return( ((ch == 'y') || (ch == 'Y')) );
}

char GetChar(char def, char *spc, char *expstr)
{
   char ch, ln[256];
   fprintf(stdout, "%s%s [%c]: ", spc, expstr, def);
   if (fgets(ln, 256, stdin) == NULL) ch=def;
   else if (ln[0] == '\0' || ln[0] == '\n') ch=def;
   else ch = ln[0];
   return(ch);
}

int FileIsThere(char *nam)
{
   FILE *fp;

   fp = fopen(nam, "r");
   if (fp == NULL) return(0);
   fclose(fp);
   return(1);
}

#include <stdarg.h>
#define ATL_UseStringVarArgs
void ATL_mprintf(int np, ...)
/*
 * Prints same message to np output file streams
 */
{
   va_list argptr;
   FILE *fp[8];
   char *form;
   int i;
   #ifdef ATL_UseStringVarArgs
      char ln[1024];
   #endif

   if (np > 0)
   {
      va_start(argptr, np);
      assert(np <= 8);
      for (i=0; i < np; i++) fp[i] = va_arg(argptr, FILE *);
      form = va_arg(argptr, char *);
      #ifdef ATL_UseStringVarArgs
         vsprintf(ln, form, argptr);
         assert(strlen(ln) < 1024);/* sanity test only, will not stop overrun */
         va_end(argptr);
         for (i=0; i < np; i++) if (fp[i]) fprintf(fp[i], "%s", ln);
      #else
         for (i=0; i < np; i++) if (fp[i]) vfprintf(fp[i], form, argptr);
         va_end(argptr);
      #endif
   }
}

int GetFirstInt(char *ln)
{
   int i, iret=0;
   for (i=0; ln[i]; i++)
   {
      if (isdigit(ln[i]))
      {
         sscanf(ln+i, "%d", &iret);
         break;
      }
   }
   return(iret);
}

int GetFirstHex(char *ln)
{
   int i, iret=0;
   for (i=0; ln[i]; i++)
   {
      if (isxdigit(ln[i]))
      {
         sscanf(ln+i, "%x", &iret);
         break;
      }
   }
   return(iret);
}

long long GetFirstLong(char *ln)
{
   int i;
   long long iret=0;
   for (i=0; ln[i]; i++)
   {
      if (isdigit(ln[i]))
      {
         sscanf(ln+i, "%Ld", &iret);
         break;
      }
   }
   return(iret);
}
long long GetFirstLongHex(char *ln)
{
   int i;
   long long iret=0;
   for (i=0; ln[i]; i++)
   {
      if (isxdigit(ln[i]))
      {
         sscanf(ln+i, "%lx", &iret);
         break;
      }
   }
   return(iret);
}

double GetFirstDouble(char *ln)
/*
 * Gets a double, which begins wt digit or "." (i.e., won't get e10)
 */
{
   int i;
   double dret=0;
   for (i=0; ln[i]; i++)
   {
      if (isdigit(ln[i]))
      {
         if (i > 0 && ln[i-1] == '.') i--;
         sscanf(ln+i, "%lf", &dret);
         break;
      }
   }
   return(dret);
}

int GetLastInt(char *ln)
{
   int i, iret=0;
   for (i=0; ln[i]; i++);
   if (i > 0) for (i--; i > 0 && !isdigit(ln[i]); i--);
   if (i > 0 || (i == 0 && isdigit(ln[0])))
   {
      while(isdigit(ln[i]) && i > 0) i--;
      if (!isdigit(ln[i])) i++;
      sscanf(ln+i, "%d", &iret);
   }
   return(iret);
}
int GetLastHex(char *ln)
{
   int i, iret=0;
   for (i=0; ln[i]; i++);
   if (i > 0) for (i--; i > 0 && !isxdigit(ln[i]); i--);
   if (i > 0 || (i == 0 && isxdigit(ln[0])))
   {
      while(isxdigit(ln[i]) && i > 0) i--;
      if (!isxdigit(ln[i])) i++;
      sscanf(ln+i, "%x", &iret);
   }
   return(iret);
}

long long GetLastLong(char *ln)
{
   int i;
   long iret=0;
   for (i=0; ln[i]; i++);
   if (i > 0) for (i--; i > 0 && !isdigit(ln[i]); i--);
   if (i > 0 || (i == 0 && isdigit(ln[0])))
   {
      while(isdigit(ln[i]) && i > 0) i--;
      if (!isdigit(ln[i])) i++;
      sscanf(ln+i, "%ld", &iret);
   }
   return(iret);
}
long long GetLastLongHex(char *ln)
{
   int i;
   long iret=0;
   for (i=0; ln[i]; i++);
   if (i > 0) for (i--; i > 0 && !isxdigit(ln[i]); i--);
   if (i > 0 || (i == 0 && isxdigit(ln[0])))
   {
      while(isxdigit(ln[i]) && i > 0) i--;
      if (!isxdigit(ln[i])) i++;
      sscanf(ln+i, "%lx", &iret);
   }
   return(iret);
}

long long GetLastLongWithRound(char *ln)
/*
 * Gets a long from end of line, assuming the line may be in decimal
 * format.  If given 1.6 it will return 2.  However, if given 1e256,
 * it will return 256!
 */
{
   int i;
   long iret=0;
   for (i=0; ln[i]; i++);
   if (i > 0)
   {
      for (i--; i > 0 && !isdigit(ln[i]); i--);
      if (i > 0 && ln[i] == '.')  /* allow skip of 1 decimal point */
         for (i--; i > 0 && !isdigit(ln[i]); i--);
   }
   if (i > 0 || (i == 0 && (isdigit(ln[0]) || ln[0] == '.')))
   {
      double d;
      while(isdigit(ln[i]) && i > 0) i--;
      if (!isdigit(ln[i])) i++;
      sscanf(ln+i, "%lf", &d);
      iret = (int)(d+0.5);
   }
   return(iret);
}

char *ATL_fgetln(FILE *fpin)
/*
 * RETURNS: dynamically allocates string containing a line from fpin, regardless
 *          of how long that line is; RETURNS NULL if EOF
 */
{
   char *ln, *sp;
   int len=128, i;
   ln = malloc(len*sizeof(char));
   assert(ln);
   if (!fgets(ln, len, fpin))
      return(NULL);
   i = strlen(ln);
/*
 * Keep extending lnlen and reading in more chars until \n is found
 */
   while (ln[i-1] != '\n')
   {
      char *ln2;
      len <<= 1;
      ln2 = malloc(len*sizeof(char));
      assert(ln2);
      strcpy(ln2, ln);
      free(ln);
      ln = ln2;
      if (!fgets(ln+i, len-i, fpin))
         return(ln);
      i += strlen(ln+i);
   }
   return(ln);
}

int fNumLines(char *fnam)
{
   FILE *fp;
   char ln[256];
   int i;

   fp = fopen(fnam, "r");
   assert(fp != NULL);
   for (i=0; fgets(ln, 256, fp); i++);
   return(i);
}


int GetIntBeforeWord(char *word, char *ln)
/*
 * Finds integer before word in sentence.
 * RETURNS: integer on success, BADINT on failure
 */
{
   char *sp;
   sp = strstr(ln, word);
   if (sp == ln) return(BADINT);
   sp--;
   while(isspace(*sp) && sp != ln) sp--;
   if (sp == ln) return(BADINT);
   while (isdigit(*sp) && sp != ln) sp--;
   if (sp == ln) return(BADINT);
   return(atoi(sp));
}

int GetScreenHeight()
/*
 * Returns the number of vertical lines window has
 */
{
   int i;
   for (i=160; i; i--) fprintf(stdout, "%03d\n", i);
   i = GetIntRange(0, 0, 160, "", "number at top left of screen");
   return(i);
}

void GetEnter(FILE *fpout)
{
   char ln[128];
   fprintf(fpout, "---------- PRESS ENTER TO CONTINUE ---------- ");
   fgets(ln, 128, stdin);
}

int DisplayFile(char *fnam, FILE *fpout, int nlines)
{
   FILE *fp;
   char ln[256];
   int i, GoOn=1;

   fp = fopen(fnam, "r");
   if (fp == NULL)
   {
      fprintf(stderr, "Unable to open file '%s', continuing without display.\n",
              fnam);
      return(-1);
   }
   if (nlines)
   {
      do
      {
         for (i=0; i < nlines; i++)
         {
            GoOn = (fgets(ln, 256, fp) != NULL);
            if (!GoOn) break;
            fprintf(fpout, "%s", ln);
         }
         if (GoOn) GetEnter(stdout);
         else break;
      }
      while(GoOn);
   }
   else while (fgets(ln, 256, fp)) fprintf(fpout, "%s", ln);
   i = ferror(fp);
   fclose(fp);
   return(i);
}

int DisplayFile0(char *fnam, FILE *fpout)
{
   FILE *fp;
   char ln[256];
   int i;

   fp = fopen(fnam, "r");
   if (fp == NULL)
   {
      fprintf(stderr, "Unable to open file '%s', continuing without display.\n",
              fnam);
      return(-1);
   }
   while (fgets(ln, 256, fp)) fprintf(fpout, "%s", ln);
   i = ferror(fp);
   fclose(fp);
   return(i);
}

int FoundInFile(char *fnam, char *str)
{
   FILE *fp;
   int found=0;
   char ln[256];

   fp = fopen(fnam, "r");
   assert(fp);
   while (fgets(ln, 256, fp))
   {
      if (strstr(ln, str))
      {
         found=1;
         break;
      }
   }
   fclose(fp);
   return(found);
}

char *FindUname(char *targ)
{
   static int FirstTime=1;
   static char unam[64];
   static char unamT[6];
   if (FirstTime)
   {
      if (FileIsThere("/bin/uname")) strcpy(unam, "/bin/uname");
      else if (FileIsThere("/usr/bin/uname")) strcpy(unam, "/usr/bin/uname");
      else strcpy(unam, "uname");
      strcpy(unamT, "uname");
      FirstTime = 0;
   }
   if (targ && targ[0] != '\0')
      return(unamT);
   return(unam);
}

enum ARCHFAM ProbeArchFam(char *targ)
/*
 * Tries to guess broad architectural family using uname
 */
{
   enum ARCHFAM fam=AFOther;
   char *cmnd, *res;
   char *uname;
   int i;

   uname = FindUname(targ);
   i = strlen(uname) + 4;

   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd, "%s -m", uname);
   res = atlsys_1L(targ, cmnd, 0, 0);
   if (res)
   {
      if (strstr(res, "ppc") || strstr(res, "Power Macintosh") ||
          strstr(res, "powerpc")) fam = AFPPC;
      else if (strstr(res, "sparc")) fam = AFSPARC;
      else if (strstr(res, "alpha")) fam = AFALPHA;
      else if (strstr(res, "ia64")) fam = AFIA64;
      else if (strstr(res, "mips")) fam = AFMIPS;
      else if (strstr(res, "arm")) fam = AFARM;
      else if (strstr(res, "s390")) fam = AFS390;
      else if ( strstr(res, "i686") || strstr(res, "i586") ||
                strstr(res, "i486") || strstr(res, "i386") ||
                strstr(res, "x86") || strstr(res, "x86_64") ) fam = AFX86;
      free(res);
   }
/*
 * Try uname -p if uname -m didn't work
 */
   if (fam == AFOther)
   {
      sprintf(cmnd, "%s -p", uname);
      res = atlsys_1L(targ, cmnd, 0, 0);
      if (res)
      {
         if (strstr(res, "ppc") || strstr(res, "Power Macintosh") ||
             strstr(res, "powerpc")) fam = AFPPC;
         else if (strstr(res, "sparc")) fam = AFSPARC;
         else if (strstr(res, "alpha")) fam = AFALPHA;
         else if (strstr(res, "ia64")) fam = AFIA64;
         else if ( strstr(res, "i686") || strstr(res, "i586") ||
                   strstr(res, "i486") || strstr(res, "i386") ||
                   strstr(res, "x86_64") ) fam = AFX86;
         else if (strstr(res, "mips")) fam = AFMIPS;
         else if (strstr(res, "arm")) fam = AFARM;
         else if (strstr(res, "s390")) fam = AFS390;
         free(res);
      }
   }
   free(cmnd);
   return(fam);
}

/*
 * ===========================================================================
 * These files handle setting/checking bits in (possibly) multi-word bitfields
 * ===========================================================================
 */
int IsBitSetInField(int *field, int bit)
/*
 * RETURNS: 1 if bit bit is 1, else 0
 */
{
   int word;
/*
 * Find which word the bit is in (assume 32-bit ints for safety), and what
 * bit in that word it is
 */
   word = bit >> 5;
   bit -= (word<<5);
   return(field[word] & (1<<bit));
}

void SetBitInField(int *field, int bit)
/*
 * Sets bit bit in multiword bitfield field
 */
{
   int word;
   word = bit >> 5;
   bit -= (word<<5);
   field[word] |= (1<<bit);
}

/*
 * ===================================================================
 * These files do some string processing for some crude pseudo-parsing
 * ===================================================================
 */

void KillUselessSpace(char *str)
/*
 * This routine removes all whitespace from beginning & end of str, and
 * collapses multiple intra-word whitespace to one space
 * NOTE: killing whitespace means '\n' are transformed to ' ' or '\0'!
 * NOTE: This implementation ignores ' and ", so will collapse substrings
 */
{
   int i;  /* index to uncopied portion */
   int j;  /* index to place to copy next character */

   if (str)
   {
      for (i=0; str[i] && isspace(str[i]); i++);
      if (str[i])
      {
         j = 0;
         while (str[i])
         {
            while (str[i] && !isspace(str[i]))
               str[j++] = str[i++];
            if (str[i])
            {
               str[j++] = ' ';
            }
            while (isspace(str[i])) i++;
         }
         if (isspace(str[j-1]))
            str[j-1] = '\0';
         else str[j] = '\0';
      }
      else str[0] = '\0';
   }
}

char *GetPathWithoutName(char *file)
/*
 *RETURNS: string containing path without last file/dir
 */
{
   char *sp;
   int i, lastslash;
   char ch;
   char *NewStringCopy(char *old);

   for (lastslash=i=0; file[i]; i++)
      if (file[i] == '/')
         lastslash = i;
   i = lastslash;
   ch = file[i];
   file[i] = '\0';
   sp = NewStringCopy(file);
   file[i] = ch;
   return(sp);
}
char *NameWithoutPath(char *file)
/*
 * Strips off path from file, assuming unix / for path
 * RETURNS: string containing file w/o path
 */
{
   int i, lastslash;
   char *cp;

   for (lastslash=i=0; file[i]; i++)
      if (file[i] == '/')
         lastslash = i;
   cp = malloc(sizeof(char)*(i-lastslash+1));
   assert(cp);
   strcpy(cp, file+((file[lastslash] == '/') ? lastslash+1 : 0));
   KillUselessSpace(cp);
   return(cp);
}

int GetIntVers(char *str, int *nskip)
{
   char ln[64];
   int i, j;

   *nskip = 0;
   for (i=0; str[i] && !isdigit(str[i]); i++);  /* skip non-digits */
   if (str[i])
   {
      for (j=0; j < 64 && str[j+i] && isdigit(str[j+i]); j++) ln[j] = str[j+i];
      ln[j] = '\0';
      if (j)
      {
         *nskip = i+j;
         return(atoi(ln));
      }
   }
   return(-1);
}

void GetGccVers(char *gcc, int *comp, int *major, int *minor, int *patch)
/*
 * comp: 0: gcc;  1: egcs;  2: pgcc, 3: apple's gcc
 */
{
   char *cmnd, *res;
   int i, j;

   *comp = *major = *minor = *patch = -1;
   i = strlen(gcc) + 12;
   cmnd = malloc(i * sizeof(char));
   assert(cmnd);
   sprintf(cmnd, "%s --version", gcc);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
      if (strstr(res, "Apple Computer") || strstr(res, "Apple Inc"))
         *comp = 3;
/*
 *    Skip compiler name, which may have digits in it
 */
      for (i=0; res[i] && !isspace(res[i]); i++);
      *major = GetIntVers(res+i, &j); j += i;
      if (*major != -1)
      {
         *minor = GetIntVers(res+j, &i); j += i;
         if (*minor != -1)
         {
            *patch = GetIntVers(res+j, &i); j += i;
            if (strstr(res, "egcs")) *comp = 1;
            else if (strstr(res, "pgcc")) *comp = 2;
            else if (*comp == -1) *comp = 0;
         }
      }
      free(res);
   }
}

char *GetPathEnvVar(void)
/*
 * returns users path with inter-name spaces replaced by "\ "
 * and dirs separated by spaces
 */
{
   char *path, *pp, *p;
   int i, n;

   path = getenv("PATH");
   if (!path)
      return(NULL);

   n = strlen(path);
   p = pp = malloc((2*n+1)*sizeof(char));
   assert(pp);
   for (i=0; i < n; i++)
   {
      if (path[i] == ':')
         *p++ = ' ';
      else if (path[i] == ' ')
      {
         *p = '\\';
	 p[1] = ' ';
	 p += 2;
      }
      else
         *p++ = path[i];
   }
   *p = '\0';
   return(pp);
}

int CompIsGcc(char *comp)
/*
 * Tries to detect if compiler is gcc w/o scoping name of compiler
 * However, rejects compilers with c89 and c90 in name, since these
 * guys turn off inline assembly, which hurts the ATLAS install on performance.
 */
{
   char *cmnd, *res;
   char *cmpname;
   int i;

   cmpname = NameWithoutPath(comp);
   if (strstr(cmpname, "c89") || strstr(cmpname, "c90"))
   {
      free(cmpname);
      return(0);
   }
   free(cmpname);
   i = strlen(comp) + 16;
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd, "%s --version", comp);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
/*
 *    The direct comps added because Ubuntu stopped printing (GCC) in the
 *    version line.  Don't want to search for just "gcc", since that will
 *    match with pgcc
 */
      if (strstr(res, "(GCC)")  || strstr(res, " GCC ") ||
          strstr(res, "gcc-mp") || strstr(res, "(GCC ") ||
          strstr(res, "GNU Fortran") ||
          (res[0] == 'g' && res[1] == 'c' && res[2] == 'c' &&
           (res[3] == ' ' || res[3] == '-'))
         )
      {
         free(res);
         return(1);
      }
      free(res);
   }
   return(0);
}

int CompIsMinGW(char *comp)
/*
 * Tries to detect if compiler is MinGW compiler
 */
{
   if (CompIsGcc(comp))
   {
      char *cmnd, *res;
      int i;
      i = strlen(comp) + 24;
      cmnd = malloc(sizeof(char)*i);
      assert(cmnd);
      sprintf(cmnd, "%s -v 2>&1 | fgrep mingw", comp);
      res = atlsys_1L(NULL, cmnd, 0, 0);
      free(cmnd);
      if (res)
      {
         if (strstr(res, "mingw"))
         {
            free(res);
            return(1);
         }
         free(res);
      }
   }
   return(0);
}

int CompIsAppleGcc(char *comp)
/*
 * Tries to detect if compiler is Apple's funked-up gcc
 */
{
   char *cmnd, *res;

   if (CompIsGcc(comp))
   {
      int i;
      i = strlen(comp) + 24;
      cmnd = malloc(sizeof(char)*i);
      assert(cmnd);
      sprintf(cmnd, "%s -v 2>&1 | fgrep Apple", comp);
      res = atlsys_1L(NULL, cmnd, 0, 0);
      free(cmnd);
      if (res)
      {
         if (strstr(res, "Apple"))
         {
            free(res);
            return(1);
         }
         free(res);
      }
   }
   return(0);
}

int CompIsMIPSpro(char *comp)
/*
 * RETURNS: 1 if compiler is MIPSpro compiler, 0 otherwise
 */
{
   char *cmnd, *res;
   int i;

   i = strlen(comp) + 4;
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, "%s -v", comp);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
      if (strstr(res, "MIPSpro Compiler"))
      {
         free(res);
         return(1);
      }
      free(res);
   }
   return(0);
}

int CompIsPathScale(char *comp)
{
   char *cmnd, *res;
   int i;

   i = strlen(comp) + 4;
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, "%s -v", comp);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
      if (strstr(res, "PathScale"))
      {
         free(res);
         return(1);
      }
      free(res);
   }
   return(0);
}

int CompIsSunWorkshop(char *comp)
/*
 * RETURNS: 1 if compiler is Sun WorkShop compiler, 0 otherwise
 */
{
   char *cmnd, *res;
   int i;

   i = strlen(comp) + 4;
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, "%s -V", comp);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
      if (strstr(res, "Sun WorkShop"))
      {
         free(res);
         return(1);
      }
      free(res);
   }
   return(0);
}

int CompIsIBMXL(char *comp)
/*
 * RETURNS: 1 if compiler is an IBM XL compiler, 0 otherwise
 */
{
   char *cmnd, *res;
   int i;

   i = strlen(comp) + 11;
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, "%s -qversion", comp);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
      if (strstr(res, "IBM XL"))
      {
         free(res);
         return(1);
      }
      free(res);
   }
   return(0);
}

char **GetLinesFromFile
(
   FILE *fpin,    /* stream to read from */
   char **curlns  /* NULL-termed list of lines to prepend to file list ret */
)
/*
 * Read all lines from file fnam, suffix them to lines in curlns, delete
 * curlns, and return them as NULL-terminated array of strings
 */
{
   char **lns=NULL;
   int i, n, nc=0, N;
   char ln[2048];

   assert(fpin);
   for (n=0; fgets(ln, 2048, fpin); n++);  /* count number of lines in file */
   rewind(fpin);
   if (curlns)
      for (nc=0; curlns[nc]; nc++);
   N = n+nc;
   if (N < 1)
      return(NULL);
   lns = malloc((N+1)*sizeof(char*));
   assert(lns);
   if (nc)
   {
      for (i=0; i < nc; i++)
         lns[i] = curlns[i];
      free(curlns);
   }
   for (i=0; i < n; i++)
   {
      int k;
      char *sp;
      assert(fgets(ln, 2048, fpin));
      k = strlen(ln)+1;
      lns[i+nc] = sp = malloc(k*sizeof(char));
      assert(sp);
      strcpy(sp, ln);
/*
 *    Get rid of whitespace on end of line
 */
      for (k -= 2; isspace(sp[k]); k--) sp[k] = '\0';
   }
   lns[N] = NULL;
   return(lns);
}

void PrintAllStringsInList
(
   char *exp,
   char **strs  /* NULL terminated arrayof strings to be printed */
)
{
   int i;
   if (!strs)
   {
      printf("%s: NULL\n", exp);
      return;
   }
   printf("%s:\n", exp);
   for (i=0; strs[i]; i++)
      printf("   '%s'\n", strs[i]);
}
void KillAllStringsInList
(
   char **strs  /* NULL terminated arrayof strings to be freed */
)
{
   int i;
   if (!strs)
      return;
   for (i=0; strs[i]; i++)
      free(strs[i]);
   free(strs);
}

char *FreeListGetString
(
   char **strs, /* NULL-terminated array of strings to be freed */
   int n        /* only string you want to retain from array (returned) */
)
{
   char *ret;
   int i;

   if (!strs)
      return(NULL);
   ret = strs[n];
   for (i=0; strs[i]; i++)
   {
      if (i != n)
         free(strs[i]);
   }
   free(strs);
   return(ret);
}

char **NewOneStringList
(
   char **strs, /* NULL-terminated array of strings to be freed */
   int n        /* only string you want to retain from array (returned) */
)
{
   char **nstrs;
   nstrs = malloc(2*sizeof(char*));
   assert(nstrs);
   nstrs[0] = FreeListGetString(strs, n);
   nstrs[1] = NULL;
   return(nstrs);
}
