/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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
#define Mstr2(m) # m
#define Mstr(m) Mstr2(m)
#ifndef DEFDF
/*   #define DEFDF "c:/Program Files/Microsoft Visual Studio/VC/BIN/CL.EXE" */
   #define DEFDF CL.EXE
#endif

int slashdrivesub(char *ln)
/*
 * replaces \\c\ with c:\, returns change in string length
 * this version required for older cygwins
 */
{
   char *sp, *lp=ln, ctmp;
   int nrep=0;
   do
   {
      sp = strstr(lp, "\\\\");
      if (sp && strlen(sp) > 3)
      {
         if (sp[2] == 'a' || sp[2] == 'b' || sp[2] == 'c' || sp[2] == 'd' ||
             sp[2] == 'e' || sp[2] == 'f' || sp[2] == 'g' || sp[2] == 'h')
         {
            if (sp[3] == '\\')
            {
               ctmp = sp[2];
               sp[0] = sp[2];
               sp[1] = ':';
               sp[2] = '\\';
               for (lp=sp+3; *lp = lp[1]; lp++);
               lp = sp + 3;
               nrep++;
            }
            else lp = sp + 2;
         }
         else lp = sp + 2;
      }
      else lp = sp + 2;
   }
   while (sp);
   return(-nrep);
}

int cygdrivesub(char *ln)
/*
 * replaces \cygdrive\c\ with c:\, returns change in string length
 * this version works cygnus version 1.1.0
 */
{
   char *sp;
   int i=0;

   while(sp = strstr(ln, "\\cygdrive\\"))
   {
      i++;
      sp[0] = sp[10];
      sp[1] = ':';
      sp[2] = '\\';
      sp += 3;
      while (*sp = sp[9]) sp++;
   }
   return( slashdrivesub(ln) - (i*9) );
}

void slashsub(char *ln)
/*
 * changes forward slash of unix to backslash of windoze
 */
{
   int i;
   for (i=0; ln[i]; i++) if (ln[i] == '/') ln[i] = '\\';
}

void doto2dotobj(char *ln)
/*
 * changes all occurences of unix's .o extension to windoze-friendly obj
 */
{
   char ln2[4096];
   int i, j=0;
   for (i=0; ln[i]; i++)
   {
      ln2[j++] = ln[i];
      if (ln[i] == '.' && ln[i+1] == 'o' && isspace(ln[i+2]))
      {
         ln2[j] = 'o';
         ln2[j+1] = 'b';
         ln2[j+2] = 'j';
         ln2[j+3] = ln[i+2];
         j += 4;
         i += 2;
      }
   }
   ln2[j] = '\0';
   sprintf(ln, "%s", ln2);
}

#ifdef DEBUG

#define system SYSTEM
int system(char *ln)
{
   fprintf(stdout, "%s\n", ln);
   return(0);
}

#endif

typedef struct wOrDs WORDS;
struct wOrDs
{
   char *word;
   WORDS *next;
};

void KillWords(WORDS *wp)
{
   WORDS *wpn;

   while (wp)
   {
      free(wp->word);
      wpn = wp->next;
      free(wp);
      wp = wpn;
   }
}

WORDS *AddWord(WORDS *wbas, char *wrd, int wlen)
{
   WORDS *wp;
   int i;

   wp = malloc(sizeof(WORDS));
   assert(wp);
   wp->word = malloc( (wlen+1) * sizeof(char) );
   for (i=0; i != wlen; i++) wp->word[i] = wrd[i];
   wp->word[i] = '\0';
   wp->next = wbas;
   return(wp);
}

WORDS *AddFile(WORDS *wbase, char *fnam)
{
   int i, len;
   len = strlen(fnam);
   assert(len > 2);
   if (fnam[len-1] = 'c' && fnam[len-2] == '.');
   {
      for (i=len-3; i > 0; i--)
      {
         if (fnam[i] == ' ' || fnam[i] == '/' || fnam[i] == '\\')
         {
            i++;
            break;
         }
      }
      wbase = AddWord(wbase, fnam+i, len-i-2);
   }
   return(wbase);
}

int RenameFiles(WORDS *wbase)
{
   WORDS *wp;
   char ln[4096];

   for (wp=wbase; wp; wp = wp->next)
   {
      sprintf(ln, "MOVE %s.obj %s.o\n", wp->word, wp->word);
      fprintf(stdout, "%s", ln);
      if (system(ln)) return(1);
   }
   return(0);
}

int NeedsQuotes(char *str)
/*
 * This func added 07/20/07, as cygwin now requires us to put quotes
 * around any path containing windows \ path character or space.
 * Note that you need to compile prog without -mno-cygwin.  With
 * -mno-cygwin, seem to get same shell as when compiled with cl, which
 * seems to not work at all
 * RETURNS: 1 if need quotes, 0 else
 */
{
   int i, k;
   return(0);
   for (i=0; str[i]; i++)
   {
       if (str[i] == '\\') return(1);
       else if (str[i] == '/') return(2);
       else if (str[i] == ' ')
       {
          for (k=0; str[k]; k++)
             if (str[k] != ' ') return(4);
       }
   }
   return(0);
}

int main(int nargs, char **args)
{
   char ln[4096], *cptr=NULL;
   int i, j, k, ierr, ic, ii;
   int COMP=0, RENAME=0;
   WORDS *wbase=NULL;

   ic = sprintf(ln, "\"%s\" ", Mstr(DEFDF));
   slashsub(ln);

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] == '-')  /* compiler flag */
      {
         if (args[i][1] == 'o') /* handle renaming, guess exe not .obj */
         {
            RENAME = 1;
            k = strlen(args[++i]);
            if (NeedsQuotes(args[i]))
               j = sprintf(ln+ic, "'/Fe%s.exe' ", args[i]);
            else
               j = sprintf(ln+ic, "/Fe%s.exe ", args[i]);
            cptr = ln+ic;
            slashsub(ln+ic+3);
         }
         else if (args[i][1] == 'l') /* library to link against */
         {
            if (NeedsQuotes(args[i]))
               j = sprintf(ln+ic, "'/link:DEFAULTLIB:%s'", &args[i][2]);
            else
               j = sprintf(ln+ic, "/link:DEFAULTLIB:%s", &args[i][2]);
            slashsub(ln+ic+17);
         }
         else if (args[i][1] == 'm' && args[i][2] == '3'
                  && args[i][3] == '2' && args[i][4] == '\0')
            j=0;  /* do not pass on -m32/64 to windows compilers */
         else if (args[i][1] == 'm' && args[i][2] == '3'
                  && args[i][3] == '2' && args[i][4] == '\0')
            j=0;  /* do not pass on -m32/64 to windows compilers */
         else if (args[i][1]=='m' && args[i][2]=='s' && args[i][3]=='s' &&
                  args[i][4] == 'e')
         {
            if (args[i][5] == '\0')
               j = sprintf(ln+ic, "/arch:sse");
            else
               j = sprintf(ln+ic, "/arch:sse2");
         }
         else if (args[i][1]=='m' && args[i][2]=='a' && args[i][3]=='v' &&
                  args[i][4] == 'x')
            j = sprintf(ln+ic, "/arch:avx");
         else if (args[i][1] == 'm' && args[i][2] == '3'
                  && args[i][3] == '2' && args[i][4] == '\0')
              ;  /* do not pass on -m32/64 to windows compilers */
         else
         {
            if (args[i][1] == 'c') COMP=1;
            if (NeedsQuotes(args[i]))
            {
               j = sprintf(ln+ic, "'/%s' ", &args[i][1]);
               slashsub(ln+ic+2);
            }
            else
            {
               j = sprintf(ln+ic, "/%s ", &args[i][1]);
               slashsub(ln+ic+1);
            }
         }
      }
      else  /* must be files, not flags */
      {
         if (NeedsQuotes(args[i]))
         {
            ii = 1;
            j = sprintf(ln+ic, "'%s' ", args[i]);
         }
         else
         {
            ii = 0;
            j = sprintf(ln+ic, "%s ", args[i]);
         }
         if (j+ii > 3 && ln[ic+j-2-ii] == 'c' && ln[ic+j-3-ii] == '.')
         { /* add this file to list of files to be moved to .o */
            wbase = AddFile(wbase, args[i]);
         }
         slashsub(ln+ic);
      }
      ic += j;
   }
   sprintf(ln+ic, "\n");
   cygdrivesub(ln);
   if (RENAME && COMP) /* gotta use different rename command if not exe */
   {
      cptr = strstr(ln, "/Fe");
      assert(cptr);
      cptr[2] = 'o';
      cptr = strstr(cptr, ".exe");
      assert(cptr);
      cptr[0] = cptr[1] = cptr[2] = cptr[3] = ' ';
   }
   fprintf(stdout, "%s", ln);
   ierr = system(ln);
   if (!ierr && COMP && !RENAME) /* rename the .obj to .o */
      ierr = RenameFiles(wbase);
   KillWords(wbase);
   return(ierr);
}
