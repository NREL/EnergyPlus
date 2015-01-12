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
#include <stddef.h>

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

void GoGetThem(char *infile, char *outfile)
{
   char ln[512], ln2[512], tnam[256], MCC[256], MMFLAGS[256];
   char *chkfile = "FlagCheck.c", *sp, *sp2;
   FILE *fpin, *fpout;
   int i, j, n, nmin=0, good;
   int wass;

   fpout = fopen(chkfile, "w");
   assert(fpout);
   fprintf(fpout, "int main(){}\n");
   fclose(fpout);
   n = NumUserCases0(infile);
   fpin = fopen(infile, "r");
   assert(fpin);
   assert(tmpnam(tnam));
   fpout = fopen(tnam, "w");
   assert(fpout);
   assert(fgets(ln, 512, fpin));
   assert(fgets(ln, 512, fpin));
   for (i=0; i < n; i++)
   {
      wass = 0;
      assert(fgets(ln, 512, fpin));
      if (LineIsCont(ln))
      {
         assert(fgets(MCC, 256, fpin));
         assert(fgets(MMFLAGS, 256, fpin));
         sprintf(ln2, "%s %s -c -DATL_BETA=1 %s\n", MCC, MMFLAGS, chkfile);
/*
 *       Substitute -x c for -x assembler or -x assembler-with-cpp
 */
         if (sp = strstr(ln2, "assembler"))
         {
            sp2 = strstr(ln2, "-x");
            if (sp2 && ((ptrdiff_t) sp2) < ((ptrdiff_t) sp))
            {
               for (sp2 += 2; sp2 != sp; sp2++)
                  if (!isspace(*sp2)) break;
               if (sp2 == sp)
               {
                  wass = 1;
                  if (strstr(sp, "assembler-with-cpp"))
                  {
                     wass = 2;
                     for (j=1; j != 18; j++) sp[j] = ' ';
                  }
                  else for (j=1; j != 9; j++) sp[j] = ' ';
                  sp[0] = 'c';
               }
            }
         }
         for (j=0; ln2[j]; j++) if (ln2[j] == '\n') ln2[j] = ' ';
         ln2[j-1] = '\n';
         fprintf(stdout, "%s", ln2);
         if (!system(ln2))
         {
            if (wass)
            {
               sp[ 0] = 'a';
               sp[ 1] = 's';
               sp[ 2] = 's';
               sp[ 3] = 'e';
               sp[ 4] = 'm';
               sp[ 5] = 'b';
               sp[ 6] = 'l';
               sp[ 7] = 'e';
               sp[ 8] = 'r';
               if (wass > 1)
               {
                  sp[ 9] = '-';
                  sp[10] = 'w';
                  sp[11] = 'i';
                  sp[12] = 't';
                  sp[13] = 'h';
                  sp[14] = '-';
                  sp[15] = 'c';
                  sp[16] = 'p';
                  sp[17] = 'p';
               }
            }
            fputs(ln, fpout);
            fputs(MCC, fpout);
            fputs(MMFLAGS, fpout);
         }
         else nmin--;
      }
      else fputs(ln, fpout);
   }
   fclose(fpin);
   fclose(fpout);
/*
 * Now, create standard-style file with surviving routines
 */
   fpin = fopen(tnam, "r");
   assert(fpin);
   fpout = fopen(outfile, "w");
   assert(fpout);
   n += nmin;
   fprintf(fpout, "\n%d\n", n);
   for (i=0; i < n; i++)
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
   fclose(fpout);
   remove(tnam);
   sprintf(tnam, "%s", chkfile);
   for (i=0; tnam[i]; i++);
   for (i--; i > 0; i--)
   {
      if (tnam[i] == 'c')
      {
         tnam[i] = 'o';
         break;
      }
   }
   remove(tnam);
}
void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -o <outfile> -i <infile>\n", nam);
   exit(-1);
}

void GetFlags(int nargs, char **args, char **infile, char **outfile)
{
   int i;

   *infile = "CASES/dcases.flg";
   *outfile = "dcases.gfg";

   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'o':
         *outfile = args[++i];
         break;
      case 'i':
         *infile = args[++i];
         break;
      default:
         PrintUsage(args[0]);
      }
   }
}

int main(int nargs, char **args)
/*
 * Eliminates compiler/flag combos that don't work,
 * NOTE: infile & outfile can be same
 */
{
   char *infile, *outfile;
   GetFlags(nargs, args, &infile, &outfile);
   GoGetThem(infile, outfile);
   return(0);
}
