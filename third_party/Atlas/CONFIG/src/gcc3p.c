#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#define DEBUG 1
#ifdef DEBUG
   FILE *fplog=NULL;
#endif
#ifndef MYGCC
   #define MYGCC "gcc"
#endif
#ifndef MYASM
   #define MYASM "as"
#endif
#ifndef MYLNK
   #define MYLNK "clang"
#endif
/*
 * This is a wrapper around a GNU gcc installation which is using the
 * native assembler and linker rather than the gnu alternatives.  This
 * scenario happens extremely often, for example on Mac OS 10.7 where
 * the native gnu binutils are broken old versions, or on Solaris with the same.
 * This wrapper parses the input and breaks the command into 1-3 separate
 * actions, depending on the flags and files given:
 * (1) Compile any .c or .S files to .s using the provided gcc
 * (2) Assemble these new .s files to .o using the provided assembler
 * (3) Link these files together using the provided linker
 *
 * Compiles with "-c" as a flag do only steps 1-2.
 */

char *CatStrs(char *s1, char *s2)
{
   int i1, i2, i;
   char *sp;
   i1 = strlen(s1);
   i2 = strlen(s2);
   sp = malloc(i1 + i2 + 1);
   assert(sp);
   for (i=0; i < i1; i++)
      sp[i] = s1[i];
   for (i=0; i < i2; i++)
      sp[i1+i] = s2[i];
   sp[i1+i2] = '\0';
   return(sp);
}

typedef struct ARGNODE ATL_arg_t;
struct ARGNODE
{
   ATL_arg_t *next;
   char *arg;
   int len, nespc;    /* len of arg, # of embedded spaces in arg */
};

ATL_arg_t *NewArg(char *arg)
{
   ATL_arg_t *ap;
   char *sp;
   int ib, ie, i, nes, len;

   ap = malloc(sizeof(ATL_arg_t));
   assert(ap);
   ap->next = NULL;

   if (!arg)
   {
      ap->arg = NULL;
      ap->len = ap->nespc = 0;
      return(ap);
   }
   for (ib=0; arg[ib] != '\0' && isspace(arg[ib]); ib++);
   for (ie=ib; arg[ie] != '\0'; ie++);
   for (ie--; isspace(arg[ie]); ie--);
   len = ie - ib + 1;
   ap->arg = sp = malloc(len+1);
   assert(sp);
   for (len=nes=0,i=ib; i <= ie; i++)
   {
      char ch;
      sp[len++] = ch = arg[i];
      if (isspace(ch))
         nes++;
   }
   sp[len] = '\0';
   ap->len = len;
   ap->nespc = nes;
   return(ap);
}

ATL_arg_t *DupArg(ATL_arg_t *ap)
{
   ATL_arg_t *np;
   np = malloc(sizeof(ATL_arg_t));
   assert(np);
   np->len = ap->len;
   np->nespc = ap->nespc;
   np->next = NULL;
   np->arg = malloc(ap->len+1);
   assert(np->arg);
   strcpy(np->arg, ap->arg);
   return(np);
}

ATL_arg_t *KillArg(ATL_arg_t *ap)
{
   ATL_arg_t *ret=NULL;
   if (ap)
   {
      ret = ap->next;
      if (ap->arg)
         free(ap->arg);
      free(ap);
   }
   return(ret);
}

void KillAllArgs(ATL_arg_t *ap)
{
   while(ap)
      ap = KillArg(ap);
}

ATL_arg_t *CombineArgs(ATL_arg_t *a1, ATL_arg_t *a2)
/*
 * RETURNS: one argument that cats the two args together, separated by a space
 */
{
   ATL_arg_t *np;
   int l1, l2, len;
   assert(a1 && a2);
   assert(a1->arg && a2->arg);
   l1 = a1->len;
   l2 = a2->len;
   len = l1 + l2 + 1;
   np = malloc(sizeof(ATL_arg_t));
   assert(np);
   np->arg = malloc(len);
   assert(np->arg);
   np->len = len;
   strcpy(np->arg, a1->arg);
   np->arg[l1] = ' ';
   strcpy(np->arg+l1+1, a2->arg);
   np->nespc = a1->nespc + a2->nespc + 1;
   np->next = NULL;
   return(np);
}

ATL_arg_t *CombAndKill(ATL_arg_t *ap, char *str)
/*
 * Cats string to argument in ap.
 */
{
   ATL_arg_t *np, *tp;

   tp = NewArg(str);
   np = CombineArgs(ap, tp);
   np->next = NULL;
   KillArg(ap);
   return(np);
}

int FindTotLen(ATL_arg_t *ap)
/*
 * Finds total length of all arguments, including enclosing "" for args with
 * embedded spaces and space added to end to seperate with next flag
 */
{
   int len = 0;
   while (ap)
   {
      len += ap->len + 1;
      if (ap->nespc > 0)
         len += 2;
      ap = ap->next;
   }
   return(len);
}

int FindMaxLen(ATL_arg_t *ap)
/*
 * Finds max length of any arg, including enclosing "" for args with
 * embedded spaces
 */
{
   int maxlen = 0;
   while (ap)
   {
      int len;
      len = ap->len;
      if (ap->nespc > 0)
         len += 2;
      maxlen = (maxlen >= len) ? maxlen : len;
      ap = ap->next;
   }
   return(maxlen);
}

int PrintArgToStr(ATL_arg_t *ap, char *str)
/*
 * Prints ap->arg to str, RETURNS: # of characters printed
 */
{
   int i;

   if (ap->nespc > 0)
      i = sprintf(str, "\"%s\" ", ap->arg);
   else
      i = sprintf(str, "%s ", ap->arg);
   return(i);
}

int PrintAllArgsToStr(ATL_arg_t *ab, char *str)
/*
 * Prints all arguments in ab to str, RETURNS: # of characters printed
 */
{
   int i=0;

   while (ab)
   {
      if (ab->nespc > 0)
         i += sprintf(str+i, "\"%s\" ", ab->arg);
      else
         i += sprintf(str+i, "%s ", ab->arg);
      ab = ab->next;
   }
   return(i);
}


char *ParseCompFlags(int nargs, char **args, int *BITS, ATL_arg_t **files,
                     ATL_arg_t **CMP, ATL_arg_t **ASM, ATL_arg_t **LNK,
                     int *CPPONLY)
{
   int i, nfiles=0, DOASM=1, DOLNK=1;
   ATL_arg_t *cmpb, *cp, *asmb, *ap, *lnkb, *lp, *filb, *fp, *an;
   char *outn=NULL;
   *CPPONLY = 0;
/*
 * Go through compiler flags and separate them into flags affecting
 * compilation, assembling, and linking
 */
   cmpb = cp = NewArg(MYGCC);
   asmb = ap = NewArg(MYASM);
   lnkb = lp = NewArg(MYLNK);
   filb = NULL;
   for (i=1; i < nargs; i++)
   {
      ATL_arg_t *at;
      at = NewArg(args[i]);
      if (at->arg[0] == '-')  /* flag */
      {
         switch (at->arg[1])
         {
         case 'o' :  /* -o : found name of output file */
/*
 *          If this is just -o, then actual name is given in the next arg
 */
            if (at->len == 2)
            {
               assert(++i < nargs);
               outn = args[i];
            }
/*
 *          is of form -o<name>
 */
            else
               outn = args[i]+2;
            break;
         case 'L' : /* -L/l args go only to linker  */
         case 'l' :
            if (at->len == 2)
            {
               assert(++i < nargs);
               lp->next = at;
               lp = at->next = NewArg(args[i]);
            }
            else
            {
               lp->next = at;
               lp = at;
            }
            break;
         case 'x' :  /* I only use -x asg-with-cpp, so pass to comp only */
            *CPPONLY = 1;
            cp->next = at;
            assert(++i < nargs);
            assert(!strcmp(args[i], "assembler-with-cpp"));
            cp = at->next = NewArg(args[i]);
            break;
         case 'c' :
            DOLNK = 0;
            break;
         case 'S' :
            DOASM = DOLNK = 0;
            break;
         case 'E':
         case 'X':
         case 'T':
         case 'u':
            fprintf(stderr, "FLAG '-%c' NOT HANDLED, DYING\n", at->arg[1]);
            assert(0); /* not implemented at moment */
         case 'W': /* possible linker/assembler pass-thru */
           if (at->len > 3)
           {
              if (at->arg[2] == 'a' && at->arg[3] == ',') /* asm pass-thru */
              {
                 strcpy(at->arg, at->arg+4);
                 at->len -= 4;
                 ap->next = at;
                 ap = at;
              }
              else if (at->arg[2] == 'l' && at->arg[3] == ',') /* lnk pass */
              {
                 lp->next = at;
                 lp = at;
              }
           }
           /* else fall-thru to default */
         default:  /* everything else assumed to be a compiler-only flag */
/*
 *          In default, compare against all known linker options
 */
            if ( (at->len == 13 && !strcmp(at->arg, "-nostartfiles")) ||
                 (at->len == 14 && !strcmp(at->arg, "-nodefaultlibs")) ||
                 (at->len == 9 && !strcmp(at->arg, "-nostdlib")) ||
                 (at->len == 4 && !strcmp(at->arg, "-pie")) ||
                 (at->len == 9 && !strcmp(at->arg, "-rdynamic")) ||
                 (at->len == 2 && at->arg[1] == 's') ||
                 (at->len == 7 && !strcmp(at->arg, "-static")) ||
                 (at->len == 14 && !strcmp(at->arg, "-static-libgcc")) ||
                 (at->len == 9 && !strcmp(at->arg, "-symbolic"))
               )
            {
               lp->next = at;
               lp = at;
            }
            else if (at->len == 8 && !strcmp(at->arg, "-Xlinker"))
            {
               assert(++i < nargs);
               lp->next = at;
               lp = at->next = NewArg(args[i]);
            }
            else if (at->len == 11 && !strcmp(at->arg, "-Xassembler"))
            {
               assert(++i < nargs);
              ap->next = NewArg(args[i]);
            }
/*
 *          -m64/32 args get passed to comp, asm & linker
 */
            else if (at->len == 4 &&
                     (!strcmp(at->arg, "-m64") || !strcmp(at->arg, "-m32")))
            {
               if (at->arg[2] == '6')
               {
                  ap->next = NewArg("--64");
                  *BITS = 64;
               }
               else
               {
                  ap->next = NewArg("--32");
                  *BITS = 32;
               }
               lp->next = DupArg(at);
               lp = lp->next;
               cp->next = at;
               cp = at;
            }
            else if (at->len == 9 && !strcmp(at->arg, "--version"))
            {
               char *sp;
               int i;
               i = strlen(MYGCC);
               sp = malloc(i + 12);
               strcpy(sp, MYGCC);
               strcpy(sp+i, " --version");
               system(sp);
               free(sp);
               #ifdef DEBUG
                  fclose(fplog);
               #endif
               exit(0);  /* ugly uncleaned-up exit; live with it */
            }
            else
            {
               cp->next = at;
               cp = at;
            }
         }
      }
      else  /* anything not beginning with - must be a filename */
      {
         char ch;
         int len = at->len;

         assert(len > 2);
         ch = at->arg[len-1];
         nfiles++;
         assert(at->arg[len-2] == '.' &&
            (ch == 'c' || ch == 'S' || ch == 's' || ch == 'o' || ch == 'a'));
/*
 *       If it isn't already an object file, add it to list of files to be
 *       renamed during compilation
 */
         if (ch != 'o' && ch != 'a')
         {
            if (!filb)
               filb = fp = at;
            else
            {
               fp->next = at;
               fp = at;
            }
/*
 *          Put empty node in linker options to indicate where to put in the
 *          generated file names
 */
            lp->next = NewArg(NULL);
            lp = lp->next;
         }
/*
 *       If it is a object/archive file, just add it to link line and forget it
 */
         else
         {
            lp->next = NewArg(at->arg);
            lp = lp->next;
         }
/*
 *       If the file is already an object file, add it to linker
 */
      }
   }
   if (outn)  /* must use particular output name */
   {
      ATL_arg_t *tp;
      tp = (DOLNK) ? lp : ap;
      tp->next = NewArg("-o");
      tp->next->next = NewArg(outn);
      tp = tp->next->next;
   }
   if (outn && !DOLNK)
      assert(nfiles == 1);
   else
      assert(nfiles > 0);
/*
 * If there are no files in filb, that means nothing need be compiled, so
 * the only thing we are doing is linking
 */
   if (!filb)
   {
      assert(DOLNK);
      if (cmpb)
         KillAllArgs(cmpb);
      if (asmb)
         KillAllArgs(asmb);
      *CMP = NULL;
      *ASM = NULL;
   }
   else
   {
      *CMP = cmpb;
      if (DOASM)
         *ASM = asmb;
      else
      {
         *ASM = NULL;
         KillAllArgs(asmb);
      }
   }
   if (DOLNK)
      *LNK = lnkb;
   else
   {
      *LNK = NULL;
      KillAllArgs(lnkb);
   }
   *files = filb;
   return(outn);
}

#ifdef DEBUG
void PrintFlags(int nargs, char **args)
{
   int i;

   fprintf(fplog, "\nINPUT:\n");
   for (i=0; i < nargs; i++)
      fprintf(fplog, "%s ", args[i]);
   fprintf(fplog, "\n");
   fflush(fplog);
}
#endif

char *GetDefObjNam(char *infile)
/*
 * When -c is specified, output file should be same as infile but w/o path
 */
{
   int ib, ie, len;
   char *sp;

   ie = strlen(infile);
   for (ib=ie-1; ib >=0 && infile[ib] != '/'; ib--);
   ib++;
   len = ie-ib;
   sp = malloc(len+1);
   assert(sp);
   strcpy(sp, infile+ib);
/*
 * Substitute .o for any 1 letter extension (eg: .c,.f,.s, etc)
 */
   if (sp[len-2] == '.')
      sp[len-1] = 'o';
   return(sp);
}

int main(int nargs, char **args)
{
   ATL_arg_t *cmpb, *asmb, *lnkb, *filb, *ap, *rmb=NULL, *rp;
   int bits, lcmp, lasm, llnk, lfil, i, lout=0, CPPONLY;
   char *cmps, *asms, *lnks, *outn;
   #ifdef DEBUG
      fplog = fopen("ATL_CCWRAP.log", "w");
      PrintFlags(nargs, args);
   #endif

   outn = ParseCompFlags(nargs, args, &bits, &filb, &cmpb, &asmb, &lnkb,
                         &CPPONLY);
   if (outn)
      lout = strlen(outn);
/*
 * Now, compile each file to the standard name ATL_tmp#.s using gcc -S
 */
   if (cmpb)
   {
      rp = rmb = NewArg("rm");
      lcmp = FindTotLen(cmpb);
      lfil = FindMaxLen(filb);
      cmps = malloc(lcmp + lfil + 32);
      assert(cmps);
      i = PrintAllArgsToStr(cmpb, cmps);
      if (i != lcmp)
      {
         fprintf(stderr, "ERROR: lcmp=%d, i=%d, strlen=%d, str='%s'\n",
                 lcmp, i, (int)strlen(cmps), cmps);
         #ifdef DEBUG
            fprintf(fplog, "ERROR: lcmp=%d, i=%d, strlen=%d, str='%s'\n",
                    lcmp, i, (int)strlen(cmps), cmps);
            fflush(fplog);
         #endif
         assert(i == lcmp);
      }
      for (i=0,ap=filb; ap; i++, ap = ap->next)
      {
         int k;
         char tmpnam[16];
         sprintf(tmpnam, "ATL_tmp%d.[s,o]", i);
         rp->next = NewArg(tmpnam);
         rp = rp->next;
         if (ap->arg[ap->len-1] == 'S' || CPPONLY)
            k = lcmp + sprintf(cmps+lcmp, "-E -o ATL_tmp%d.s ", i);
         else
            k = lcmp + sprintf(cmps+lcmp, "-S -o ATL_tmp%d.s ", i);
         PrintArgToStr(ap, cmps+k);
         #ifdef DEBUG
            fprintf(fplog, "COMP: %s\n", cmps);
            fflush(fplog);
         #endif
         assert(!system(cmps));
      }
      KillAllArgs(cmpb);
      free(cmps);
/*
 *    If we aren't linking and we have only one file with a specified name,
 *    assemble the .s file to the specified .o file
 */
      lasm = FindTotLen(asmb);
      cmps = malloc(lasm + lfil + 64);
      assert(cmps);
      i = PrintAllArgsToStr(asmb, cmps);
      assert(i == lasm);
      if (!lnkb && !filb->next)
      {
         int k;
         char *sp;
         if (outn)
            sp = outn;
         else
            sp = GetDefObjNam(filb->arg);
   /* still need to add arch+isaext flag */
         k = lasm + sprintf(cmps+lasm, "ATL_tmp0.s -o %s", sp);
         #ifdef DEBUG
            fprintf(fplog, "ASSEMBLE: %s\n", cmps);
         #endif
         k = system(cmps);
         KillAllArgs(filb);
         KillAllArgs(asmb);
         free(cmps);
         if (sp != outn)
            free(sp);
         return(k);
      }
/*
 *    Now assemble every .s into a .o
 */
      for (i=0,ap=filb; ap; i++, ap = ap->next)
      {
         int k;
         k = lasm + sprintf(cmps+lasm, "-c ATL_tmp%d.s -o ATL_tmp%d.o", i, i);
         #ifdef DEBUG
            fprintf(fplog, "ASSEMBLE: %s\n", cmps);
            fflush(fplog);
         #endif
         assert(!system(cmps));
      }
      KillAllArgs(asmb);
      free(cmps);
   }
/*
 * Now issue link line
 */
   if (lnkb)
   {
/*
 *    Put files back into link line in the order in which they were found
 *    NULL-arg nodes are placeholders for the names.
 */
      for (i=0,ap=filb; ap; i++,ap = ap->next)
      {
         ATL_arg_t *tp;
         for (tp=lnkb; tp && tp->arg; tp = tp->next);
         assert(tp);
         tp->arg = malloc(16);
         assert(tp->arg);
         tp->len = sprintf(tp->arg, "ATL_tmp%d.o", i);
      }
      llnk = FindTotLen(lnkb);
      cmps = malloc(llnk + 1);
      assert(cmps);
      i = PrintAllArgsToStr(lnkb, cmps);
      assert(i == llnk);
      #ifdef DEBUG
         fprintf(fplog, "LINK: '%s'\n", cmps);
         fflush(fplog);
      #endif
      assert(!system(cmps));
      KillAllArgs(lnkb);
      free(cmps);
   }
   KillAllArgs(filb);
/*
 * Now delete the temporary files that we have created
 */
   i = FindTotLen(rmb);
   cmps = malloc(i + 1);
   assert(cmps);
   assert(i == PrintAllArgsToStr(rmb, cmps));
   KillAllArgs(rmb);
   printf("%s\n", cmps);
   system(cmps);
   free(cmps);

   #ifdef DEBUG
      system("cat ATL_CCWRAP.log");
      fclose(fplog);
   #endif
   return(0);
}
