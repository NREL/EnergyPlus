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
#include <stdio.h>
#include <assert.h>
#include "atlconf_misc.h"
void PrintUsage(char *name, int iarg, char *flag)
{
   fprintf(stderr, "Unknown flag '%s' in position %d!\n", flag, iarg);
   fprintf(stderr, "USAGE: [-l <lvl>] <gcc candidate>\n");
   exit(iarg);
}
int main(int nargs, char **args)
{
   int lvl=0;  /* 0: is gcc, 1: is gcc 4 but not apple gcc 2: gcc 4.x, with x >= 4 */
   int i;
   char *comp=NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] == '-')
      {
         if (args[i][1] == 'l')
	 {
	    if (++i >= nargs)
	       PrintUsage(args[0], i, "out of arguments");
	    lvl = atoi(args[i]);
	 }
	 else
	    PrintUsage(args[0], i, args[i]);
      }
      else
         comp = args[i];
   }
   assert(comp);
   if (!CompIsGcc(comp))
      return(1);
   if (lvl)
   {
      int icmp, major, minor, patch;

      GetGccVers(comp, &icmp, &major, &minor, &patch);
      #if 0
         fprintf(stderr, "comp='%s': cmp=%d, major=%d, minor=%d, patch=%d\n",
	         comp, icmp, major, minor, patch);
      #endif
      if (icmp || major < 4)
         return(2);
      if (lvl > 1)
         if (minor < 4)
	    return(3);
   }
   printf("%s\n", comp);
   return(0);
}
