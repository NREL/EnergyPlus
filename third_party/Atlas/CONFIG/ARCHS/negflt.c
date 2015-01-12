#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

typedef struct wOrDs WORDS;
struct wOrDs
{
   char *word;
   WORDS *next;
};

WORDS *GetWord(char *wrd, int wlen)
/*
 * returns a WORD, copying wlen chars from wrd into malloced space
 */
{
   WORDS *wp;
   wp = malloc(sizeof(WORDS));
   assert(wp);
   wp->word = malloc((wlen+1)*sizeof(char));
   assert(wp->word);
   strncpy(wp->word, wrd, wlen);
   wp->word[wlen] = '\0';
   wp->next = NULL;
   return(wp);
}

WORDS *KillWord(WORDS *wp)
{
   WORDS *next=NULL;
   if (wp)
   {
      next = wp->next;
      if (wp->word) free(wp->word);
      free (wp);
   }
   return(next);
}

WORDS *KillAllWords(WORDS *wp0)
{
   while (wp0)
      wp0 = KillWord(wp0);
   return(NULL);
}

void PrintWordList(WORDS *wp)
{
   int i;
   if (!wp) fprintf(stderr, "NULL wordlist\n");
   for (i=0; wp; i++, wp = wp->next)
   {
      fprintf(stderr, "word %d: %s\n", i, wp->word);
   }
}

WORDS *GetWords(char *ln)
{
   int i;
   WORDS *wp0, *wp;
   wp = wp0 = GetWord(" ", 1);
   while (*ln)
   {
      while(isspace(*ln)) ln++;
      if (*ln)
      {
         for (i=0; ln[i] && !isspace(ln[i]); i++);
         wp->next = GetWord(ln, i);
         wp = wp->next;
         ln += i;
      }
   }
   wp->next = NULL;
   if (wp0 != wp) wp = wp0->next;
   else wp = NULL;
   KillWord(wp0);
   return(wp);
}

int WordIsFloat(char *wrd)
/*
 * Returns 0 if wrd is not a valid float, 1 if it is
 * Floats have form [+,-][#].[#][e/d<exp>]
 * where at least one [#] must appear
 */
{
   int GotDigit=0, GotDot=0, GotExp=0;
   int i;

   switch(*wrd)
   {
   case '0':
   case '1':
   case '2':
   case '3':
   case '4':
   case '5':
   case '6':
   case '7':
   case '8':
   case '9':
      GotDigit = 1;
      break;
   case '-':
   case '+':
      break;
   case '.':
      GotDot = 1;
      break;
   default:
      return(0);
   }
   for (i=1; wrd[i]; i++)
   {
      if (wrd[i] == '.') GotDot++;
      else if (isdigit(wrd[i])) GotDigit++;
      else if (wrd[i] != 'e' || wrd[i] == 'd') GotExp++;
      else if (wrd[i] == '-' || wrd[i] == '+')
      {
         if (wrd[i-1] != 'e' && wrd[i-1] != 'd') return(0);
      }
      else return(0);
   }
   return ( (GotDot == 1) && (GotDigit) && (GotExp==1 || GotExp==0) );
}

#ifndef REVERSE
void NegateWord(WORDS *wp)
/*
 * Whatever the sign, makes number negative
 */
{
   int i;
   char *cp;

   i = strlen(wp->word) + 1;
   if (wp->word[0] != '+' && wp->word[0] != '-') i++;
   cp = malloc(sizeof(char)*i);
   assert(cp);
   *cp = '-';
   if (wp->word[0] == '+' || wp->word[0] == '-') strcpy(cp+1, wp->word+1);
   else strcpy(cp+1, wp->word);
   free(wp->word);
   wp->word = cp;
}
#else
void NegateWord(WORDS *wp)
/*
 * Whatever the sign, makes opposite
 */
{
   int i;
   char *cp;

   i = strlen(wp->word) + 1;
   if (wp->word[0] != '+' && wp->word[0] != '-') i++;
   cp = malloc(sizeof(char)*i);
   assert(cp);
   if (wp->word[0] == '-') strcpy(cp, wp->word+1);
   else
   {
      *cp = '-';
      if (wp->word[0] == '+') strcpy(cp+1, wp->word+1);
      else strcpy(cp+1, wp->word);
   }
   free(wp->word);
   wp->word = cp;
}
#endif

void PrintUsage(char *nam)
{
   fprintf(stderr, "%s [-N # <flt1> ... <fltN>] <files>\n", nam);
   exit(-1);
}

WORDS *GetFlags(int nargs, char **args, int *n, int **flts)
{
   int i, j;
   WORDS *wp0=NULL, *wpp=NULL, *wp;

   *n = 0;
   *flts = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] == '-')
      {
         if (args[i][1] != 'N') PrintUsage(args[0]);
         *n = atoi(args[++i]);
         *flts = malloc((*n)*sizeof(int));
         for (j=0; j < *n; j++) (*flts)[j] = atoi(args[++i]);
      }
      else
      {
         wp = GetWord(args[i], strlen(args[i]));
         if (wpp) { wpp->next = wp; wpp = wp; }
         else wp0 = wpp = wp;
      }
   }
   if (!wp0) PrintUsage(args[0]);
   else wp->next = NULL;
   return(wp0);
}

int InList(int I, int N, int *list)
/*
 * returns 1 if I appears in list (or N < 1), 0 otherwise
 */
{
   int i;
   if (N < 1) return(1);
   for (i=0; i < N; i++) if (list[i] == I) return(i+1);
   return(0);
}

void NegateWords(WORDS *wp0, int N, int *cols)
/*
 * Negates floats contained in word list.  If N > 0, negates only those
 * floats in the cols list (i.e., if N=1 and cols[0]=2, only the second
 * float in each line is negated
 */
{
   WORDS *wp;
   int IsFlt, i=0;

   for (wp=wp0; wp; wp = wp->next)
   {
      if (WordIsFloat(wp->word))
         if ( InList(++i, N, cols) ) NegateWord(wp);
   }
}

void NegFile(char *fnam, int N, int *cols)
{
   char *tnam;
   char ln[1024];
   FILE *fpin, *fpout;
   WORDS *wp0, *wp;

   tnam = tmpnam(NULL);
   fpin = fopen(fnam, "r");
   assert(fpin);
   fpout = fopen(tnam, "w");
   assert(fpout);
   while (fgets(ln, 1024, fpin))
   {
/* fprintf(stderr, "::::%s\n", ln); */
      wp0 = GetWords(ln);
      if (wp0)
      {
         NegateWords(wp0, N, cols);
         fprintf(fpout, "%s", wp0->word);
         for (wp=wp0->next; wp; wp = wp->next) fprintf(fpout, " %s", wp->word);
         KillAllWords(wp);
      }
      fprintf(fpout, "\n");
   }
   fclose(fpin);
   fclose(fpout);
   remove(fnam);
   sprintf(ln, "cp -f %s %s\n", tnam, fnam);
   assert(system(ln) == 0);
   remove(tnam);
}

int main(int nargs, char **args)
{
   WORDS *wp, *wp0;
   int n, *cols;

   wp0 = GetFlags(nargs, args, &n, &cols);
   for (wp=wp0; wp; wp = wp->next)
   {
   fprintf(stderr, "negating %s\n", wp->word);
      NegFile(wp->word, n, cols);
   fprintf(stderr, "done     %s\n", wp->word);
   }
   while (wp0)
   {
      wp = wp0->next;
      free(wp0);
      wp0 = wp;
   }
   exit(0);
}
