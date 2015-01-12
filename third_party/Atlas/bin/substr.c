#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

static char LINE0[2048], LINE1[2048];

void PrintUsage(char *nam)
{
   fprintf(stderr,
  "\nUSAGE: %s -i <infile> -o <outfile> -s \"<find str>\" \"<rep string>\"\n\n",
           nam);
   exit(-1);
}
void GetFlags(int nargs, char **args, FILE **fpin, FILE **fpout,
              int *nsub, char ***finds, char ***reps)
{
   int i, j, n;
   char **nfind, **nrep;
   *nsub = 0;
   *fpin = stdin;
   *fpout = stdout;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0]);
      switch(args[i][1])
      {
      case 'o':
         i++;
         if (!strcmp(args[i], "stderr")) *fpout = stderr;
         else if (!strcmp(args[i], "stdout")) *fpout = stdout;
         else *fpout = fopen(args[i], "w");
         assert(*fpout);
         break;
      case 'i':
         i++;
         if (!strcmp(args[i], "stdin")) *fpout = stdin;
         else *fpin = fopen(args[i], "r");
         assert(*fpin);
         break;
      case 's':
         n = *nsub + 1;
         nfind = malloc(sizeof(char *)*n);
         nrep = malloc(sizeof(char *)*n);
         assert(nfind && nrep);
         for (j=0; j < *nsub; j++)
         {
            nfind[j] = (*finds)[j];
            nrep[j] = (*reps)[j];
         }
         nfind[j] = args[++i];
         nrep[j] = args[++i];
         if (*nsub > 0)
         {
            free(*finds);
            free(*reps);
         }
         *finds = nfind;
         *reps = nrep;
         *nsub = n;
         break;
      default :
         PrintUsage(args[0]);
      }
   }
}

void MakeSub(char *ln, char *find, char *replace)
{
   char *ln2=LINE1;
   int i, j;
   char *sp;

   sp = strstr(ln, find);
   if (sp)
   {
      i = strlen(find);
      j = strlen(replace);
      strcpy(ln2, sp+i);
      strcpy(sp, replace);
      strcpy(sp+j, ln2);
      MakeSub(sp+j, find, replace);
   }
}

void FirstCharSub(char *ln, char *find, char *replace)
{
   char *ln2=LINE1;
   int i, j;

   i = strlen(find);
   if (!strncmp(ln, find, i))
   {
      j = strlen(replace);
      strcpy(ln2, ln+i);
      strcpy(ln, replace);
      strcpy(ln+j, ln2);
   }
}

void GoToTown(FILE *fpin, FILE *fpout, int nsub, char **finds, char **reps)
{
   char *ln=LINE0;
   int i;

   while (fgets(ln, 1024, fpin))
   {
      for (i=0; i < nsub; i++)
      {
         if (finds[i][0] == '^') FirstCharSub(ln, finds[i]+1, reps[i]);
         else MakeSub(ln, finds[i], reps[i]);
      }
      fputs(ln, fpout);
   }
}

int main(int nargs, char **args)
{
   FILE *fpin, *fpout;
   int nsub;
   char **finds, **reps;
   GetFlags(nargs, args, &fpin, &fpout, &nsub, &finds, &reps);
   GoToTown(fpin, fpout, nsub, finds, reps);
   if (nsub > 0)
   {
      free(finds);
      free(reps);
   }
   return(0);
}
