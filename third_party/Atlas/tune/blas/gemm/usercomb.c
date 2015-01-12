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
#include <string.h>
#include <ctype.h>

int LineIsCont(char *ln)
{
   int i, iret=0;
   for(i=0; ln[i]; i++);
   if (i)
   {
      for(i--; isspace(ln[i]); i--);
      if (ln[i] == '\\') iret = 1;
   }
   return(iret);
}

int NumUserCases0(char *nam)
{
   int iret=0;
   char ln[512];
   FILE *fp;

   fp = fopen(nam, "r");
   if (fp)
   {
      fgets(ln, 512, fp);  /* skip comment line */
      assert(fscanf(fp, " %d", &iret) == 1);
      fclose(fp);
   }
   return(iret);
}

int NumUserCases(char pre)
{
   char ln[64];

   sprintf(ln, "%ccases.dsc", pre);
   return(NumUserCases0(ln));
}

void NoEndLineWhiteSpace(char *ln)
{
   int i;

   for (i=0; ln[i]; i++);
   if (i)
      for (i--; isspace(ln[i]); i--) ln[i] = '\0';
}

int GetUserCase(char pre, int icase, int *iflag, int *mb, int *nb, int *kb,
                int *ma, int *lat, int *mu, int *nu, int *ku,
                char *fnam, char *auth, char **MCC, char **MMFLAGS)
/*
 * if icase < 0, go to that line in file; if icase > 0 find that ID in file
 * return ID of selected line
 */
{
   int i, n, ID;
   char ln[512];
   static char sMCC[1024], sMMFLAGS[2048];
   FILE *fp;

   *MCC = *MMFLAGS = NULL;
   n = NumUserCases(pre);
   sprintf(ln, "%ccases.dsc", pre);
   fp = fopen(ln, "r");
   if (!fp) return(0);
   assert(fp);
   fgets(ln, 256, fp);  /* skip comment line */
   fgets(ln, 256, fp);  /* skip number of cases */
   for (i=0; i < n; i++)
   {
      if ( fgets(ln, 256, fp) == NULL )
      {
         fclose(fp);
         return(0);
      }
      assert(sscanf(ln, " %d %d %d %d %d %d %d %d %d %d %s \"%[^\"]", &ID,
                    iflag, mb, nb, kb, ma, lat, mu, nu, ku, fnam, auth) == 12);
      assert(ID > 0);
      if (i == -icase || ID == icase)
      {
         if (LineIsCont(ln))
         {
            assert( fgets(ln, 256, fp) != NULL );
            strcpy(sMCC, ln);
            NoEndLineWhiteSpace(sMCC);
            assert( fgets(ln, 512, fp) != NULL );
            strcpy(sMMFLAGS, ln);
            NoEndLineWhiteSpace(sMMFLAGS);
            *MCC = sMCC;
            *MMFLAGS = sMMFLAGS;
         }
         else *MCC = *MMFLAGS = NULL;
         fclose(fp);
         return(ID);
      }
      if (i != icase && LineIsCont(ln))
      {
         assert( fgets(ln, 256, fp) != NULL );
         assert( fgets(ln, 256, fp) != NULL );
      }
   }
   fclose(fp);
   return(0);
}

void CombineFiles(char *fout, int nfiles, char **fnams)
{
   char tnam[256], ln[512];
   int i, j, n, nn;
   FILE *fpout, *fpin;

   assert(tmpnam(tnam));
   for (n=i=0; i < nfiles; i++) n += NumUserCases0(fnams[i]);

   fpout = fopen(tnam, "w");
   assert(fpout);
   fprintf(fpout, "<ID> <flag> <mb> <nb> <kb> <muladd> <lat> <mu> <nu> <ku> <rout> \"<Contributer>\"\n");
   fprintf(fpout, "%d\n", n);
   for (i=0; i < nfiles; i++)
   {
      fpin = fopen(fnams[i], "r");
      if (fpin)
      {
         assert(fgets(ln, 512, fpin));
         assert(fgets(ln, 512, fpin));
         assert(sscanf(ln, " %d", &nn) == 1);
         for (j=0; j < nn; j++)
         {
            assert(fgets(ln, 512, fpin));
            fputs(ln, fpout);
            if (LineIsCont(ln))
            {
               assert(fgets(ln, 512, fpin));
               fputs(ln, fpout);
               assert(fgets(ln, 512, fpin));
               fputs(ln, fpout);
            }
         }
         fclose(fpin);
      }
      else
      {
         fprintf(stderr,
                 "COMBFILES WARNING: file %s not found / not readable!!\n",

                 fnams[i]);
      }
   }
   fclose(fpout);
   remove(fout);
   sprintf(ln, "cp %s %s\n", tnam, fout);
   assert(system(ln) == 0);
   remove(tnam);
}

void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -o <outfile> -i <Nin> <infile1> ... <infileN>\n",
           nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, char **fout, int *nfiles, char ***fnams0)
{
   int i, j, n;
   char **fnams;

   *nfiles = -1;
   *fout = NULL;
   *fnams0 = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'o': /* outfile */
         *fout = args[++i];
         break;
      case 'i': /* infiles */
         n = atoi(args[++i]);
         assert(n > 0);
         assert(nargs-i >= n);
         fnams = malloc(n * sizeof(char *));
         assert(fnams);
         for(j=0; j < n; j++) fnams[j] = args[++i];
         *nfiles = n;
         *fnams0 = fnams;
         break;
      default:
         PrintUsage(args[0]);
      }
   }
   assert(*fout);
   assert(*fnams0);
}

int main(int nargs, char **args)
{
   char *fnamout, **fnams;
   int nfiles;

   GetFlags(nargs, args, &fnamout, &nfiles, &fnams);
   CombineFiles(fnamout, nfiles, fnams);
   free(fnams);
   return(0);
}
