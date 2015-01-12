/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2011 R. Clint Whaley
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
/*
 * This is a BFI wrapper around MinGW compilers/ar for use in cygwin build
 * framework.  Its only job is to substutute 'c:' for 'cygdrive/c' (where
 * 'c' can be any single letter) in all paths (except that of the
 * compiler itself).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#define Mstr2(m) # m
#define Mstr(m) Mstr2(m)
#ifndef DEFDF
   #ifdef WRAP_FORTRAN
      #define DEFDF /usr/bin/x86_64-w64-mingw32-gfortran.exe
   #elif defined(WRAP_AR)
      #define DEFDF /usr/bin/x86_64-w64-mingw32-ar.exe
   #else
      #define DEFDF /usr/bin/x86_64-w64-mingw32-gcc-4.5.3.exe
   #endif
#endif

int cygdrivesub(char *ln)
/*
 * replaces /cygdrive/c/ with c://, returns change in string length
 */
{
   char *sp;
   int i=0;

/*
 * 01234567890
 * /cygdrive/L...
 */
   while(sp = strstr(ln, "/cygdrive/"))
   {
      i++;
      sp[0] = sp[10];
      sp[1] = ':';
      sp[2] = '/';
      sp += 3;
      while (*sp = sp[8]) sp++;
   }
   return(i*8);
}

#ifdef DEBUG
   #define system SYSTEM
   int system(char *ln)
   {
      fprintf(stdout, "%s\n", ln);
      return(0);
   }
#endif

char *GetLongerString(char *old, int len)
{
   char *sp;

   assert(len);
   sp = malloc(sizeof(char)*(len+1));
   assert(sp);
   if (old)
   {
      assert(len > strlen(old));
      strcpy(sp, old);
      free(old);
   }
   return(sp);
}

int main(int nargs, char **args)
{
   char *ln;
   int i, j, k, lnlen=0, ierr, ii;

/*
 * The path to the compiler does not substitute c:/, since it is cygwin
 * handling it, not MinGW
 */
   lnlen = (strlen(Mstr(DEFDF))+2)*2;
   ln = GetLongerString(NULL, lnlen);
   ii = sprintf(ln, "%s", Mstr(DEFDF));

/*
 * Just paste all commandline arguments together, getting rid of // from make
 * and then substituting c:// for any /cygdrive/c/
 */
   for (i=1; i < nargs; i++)
   {
      char *sp;
      j = strlen(args[i]) + 1;
      if (lnlen < ii+j)
      {
         lnlen = (lnlen+j)*2;
         ln = GetLongerString(ln, lnlen);
      }
      ln[ii++] = ' ';
      strcpy(ln+ii, args[i]);
/*
 *    ATLAS makefiles often put in // by accident, which is no problem under
 *    Unix, but means something special in Windows.  Therefore replace all
 *    // with / before doing cygdrivesub
 */
      while (sp = strstr(ln+ii, "//"))
      {
         strcpy(sp, sp+1);
         j--;
      }
      j -= cygdrivesub(ln+ii);   /* sub c:// for /cygdrive/c */
      ii += j - 1;
   }
//   fprintf(stdout, "%s", ln);
   ierr = system(ln);
   if (ierr)
      fprintf(stderr, "\nMINGW COMPILER WRAP '%s' GAVE ERROR FOR `%s'\n\n",
              args[0], ln);
/*
 * Error returns don't translate well between MinGW/cygwin; returning 0 or 1
 * just experimentally works.
 */
   return((!ierr)?0:1);
}

