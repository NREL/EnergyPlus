/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2000 R. Clint Whaley
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
void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE %s -p <pre>\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, char *pre)
{
   char ch;

   *pre = 'd';
   if (nargs > 1)
   {
      if (nargs != 3) PrintUsage(args[0]);
      if (args[1][0] != '-' || args[1][1] != 'p') PrintUsage(args[0]);
      ch = args[2][0];
      if (ch == 'Z') ch = 'z';
      else if (ch == 'C') ch = 'c';
      else if (ch == 'D') ch = 'd';
      else if (ch == 'S') ch = 's';
      else if (ch != 's' && ch != 'd' && ch != 'c' && ch != 'z')
         PrintUsage(args[0]);
      *pre = ch;
   }
}

#ifdef ATL_NOUSERMM
void CreateIndex(char pre)
{
   char fnam[16];
   FILE *fpout;
   sprintf(fnam, "%ccases.dsc", pre);
   fpout = fopen(fnam, "w")
   fprintf(fpout, "DUMMY INDEX FILE TO AVOID USER KERNELS\n0\n");
   fclose(fpout);
}
#else
void CreateIndex(char pre)
{
   char ln[512];
   char fnams[8][256];
   int n=1, i, j, itmp;

   sprintf(fnams[0], "CASES/%ccases.0", pre);
   #ifdef ATL_SSE1
      #ifdef ATL_SSE2
         if (pre == 'd' || pre == 'z')
            sprintf(fnams[n++], "%ccases.SSE", pre);
      #endif
      if (pre == 's' || pre == 'c')
         sprintf(fnams[n++], "%ccases.SSE", pre);
   #elif (defined(ATL_3DNow) && defined(ATL_3DNowFLOPS))
      if (pre == 's' || pre == 'c')
         sprintf(fnams[n++], "%ccases.3DN", pre);
   #endif
   sprintf(fnams[n++], "%ccases.flg", pre);
/*
 * substitute GOODGCC for gcc in compiler lines
 */
   for (i=1; i < n; i++)
   {
      sprintf(ln, "make %s flagfile=%s\n", fnams[i], fnams[i]);
      assert(system(ln) == 0);
   }
   itmp = n;

   j = sprintf(ln, "./xusercomb -o %ccases.dsc -i %d", pre, n);
   assert(n <= 8);
   for (i=0; i < n; i++) j += sprintf(ln+j, " %s", fnams[i]);
   sprintf(ln+j, "\n");
   fprintf(stdout, "%s", ln);
   assert(system(ln) == 0);
   remove(fnams[itmp]);
}
#endif

int main(int nargs, char **args)
/*
 * Creates system-dependent user matmul index file
 */
{
   char pre;
   GetFlags(nargs, args, &pre);
   CreateIndex(pre);
   return(0);
}

