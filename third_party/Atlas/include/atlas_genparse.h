#ifndef ATLAS_GENPARSE_H
   #define ATLAS_GENPARSE_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>
#define NASMD 9
enum ASMDIA
   {ASM_None=0, gas_x86_32, gas_x86_64, gas_sparc, gas_ppc, gas_parisc,
    gas_mips, gas_arm, gas_s390};
static char *ASMNAM[NASMD] =
   {"",     "GAS_x8632", "GAS_x8664", "GAS_SPARC", "GAS_PPC", "GAS_PARISC",
    "GAS_MIPS", "GAS_ARM", "GAS_S390"};
/*
 * Basic data structure for forming queues with some minimal info
 */
typedef struct SIDNode ATL_sidnode_t;
struct SIDNode  /* holds string, integer, and double */
{
   double d;
   char *str;
   int i;
   ATL_sidnode_t *next;
};

#define SET_FLAG(bits_, flg_, val_) \
{\
   if (val_) (bits_) |= (1<<(flg_)); \
   else (bits_) &= ~(1<<(flg_)); \
}
#define FLAG_IS_SET(field_, bit_) ( ((field_) & (1<<(bit_))) != 0 )

/* procedure 1 : allocates ATL_sidnode_t */
static ATL_sidnode_t *ATL_NewSIDNode(void)
{
   ATL_sidnode_t *sp;
   sp = calloc(1, sizeof(ATL_sidnode_t));
   assert(sp);
   return(sp);
}

/* procedure 2 : allocates ATL_sidnode_t */
static ATL_sidnode_t *ATL_FreeSIDNode(ATL_sidnode_t *die)
{
   ATL_sidnode_t *sp=NULL;
   if (die)
   {
      sp = die->next;
      if (die->str)
         free(die->str);
      free(die);
   }
   return(sp);
}

/* procedure 3 */
static int GetL1CacheElts(char pre)
{
   FILE *L1f;
   int L1Size, i;

   L1f = fopen("res/L1CacheSize", "r");
   if (!L1f)
   {
      assert(system("make res/L1CacheSize\n") == 0);
      L1f = fopen("res/L1CacheSize", "r");
   }
   assert(L1f);
   assert(fscanf(L1f, "%d", &L1Size) == 1);
   fclose(L1f);
   assert(L1Size > 0);
   if (pre == 'c' || pre == 'd')
      i = 1024/8;
   else if (pre == 's')
      i = 1024/4;
   else if (pre == 'z')
      i = 1024/16;
   else
      assert(0);
   return(i*L1Size);
}

/* procedure 4 */
static char *DupString(char *str)
{
   int i,n;
   char *s;

   if (!str)
      return(NULL);
   n = strlen(str)+1;
   s = malloc(sizeof(char)*n);
   assert(s);
   for (i=0; i < n; i++)
      s[i] = str[i];
   return(s);
}

/* procedure 5 */
static char *GetSingleQuoteString(char *str)
{
   char *sp;
   int i, n;

   assert(str[0] == '\'');
   for (i=1; str[i] && str[i] != '\''; i++);
   assert(str[i]);
   sp = malloc(i*sizeof(char));
   for (n=i,i=1; i < n; i++)
      sp[i-1] = str[i];
   sp[n-1] = '\0';
   return(sp);
}

/* procedure 6 */
static int asmNames2bitfield(char *str)
/*
 * Takes str containing an assembly name list.  The list is ended by the first
 * white space or end of string.  List items are seperated by ',', and there
 * can be no whitespace in list.
 * RETURNS: bitfield with bits set corresponding to assemblies, 0 on error.
 */
{
   char asmname[64];
   int i, KeepOn, bits=0;

   do
   {
      for (i=0; !isspace(str[i]) && str[i] != ',' && str[i] && i < 64; i++)
         asmname[i] = str[i];
      asmname[i] = '\0';
      KeepOn = str[i] == ',';
      str += i+1;
      if (i >= 64)
         return(0);  /* no asm name > 63 in length */
      for (i=0; i < NASMD; i++)
      {
         if (!strcmp(ASMNAM[i], asmname))
         {
            bits |= (1<<i);
            break;
         }
      }
   }
   while(KeepOn);
   return(bits);
}

/* procedure 7 */
static int GetDoubleArr(char *str, int N, double *d)
/*
 * Reads in a list with form "%le,%le...,%le"; N-length d recieves doubles.
 * RETURNS: the number of doubles found, or N, whichever is less
 */
{
   int i=1;
   assert(sscanf(str, "%le", d) == 1);
   while (i < N)
   {
      str = strstr(str, ",");
      if (!str)
         break;
      str++;
      assert(sscanf(str, "%le", d+i) == 1);
      i++;
   }
   return(i);
}

/* procedure 8 */
static char *GetLongerString(char *shrt, int newlen)
/*
 * Allocates new string of size newlen, copies shrt into it, and frees shrt.
 */
{
   char *sp;

   sp = malloc(sizeof(char)*newlen);
   assert(sp);
   if (shrt)
   {
      strcpy(sp, shrt);
      free(shrt);
   }
   else if (newlen >= 0)
      sp[0] = '\0';
   return(sp);
}

/* procedure 9 */
static char *GetOneLine(FILE *fpin)
/*
 * RETURNS: string of one line from stream fpin,  NULL if stream exhausted.
 */
{
   const int inc=256;
   static int len=0;
   static char *ln, *sp;
   int i, j, KeepOn;

   if (!len)
   {
      ln = malloc(inc*sizeof(char));
      assert(ln);
      len = inc;
   }
   if (!fgets(ln, len, fpin))
      return(NULL);

   for (i=0; ln[i]; i++);  /* find end of string */
   if (!i) return(ln);
   while (ln[i-1] != '\n')    /* if last char not \n, read rest of line */
   {
      len += inc;
      ln = GetLongerString(ln, len);
      if (!fgets(ln+i, inc, fpin))
         return(ln);
       for (; ln[i]; i++);  /* find end of string */
   }
   return(ln);
}

/* procedure 10 */
static char *GetJoinedLines(FILE *fpin)
/*
 * Gets lines from file fpin; if last non-whitespace char is '\', joins lines
 * RETURNS: line from file including joining, NULL if fpin exhausted
 */
{
   char *ln, *sp;
   static char *join=NULL;
   static int jlen=0;
   int i, j, k;

   sp = ln = GetOneLine(fpin);
   if (!sp)
      return(NULL);
   j = 0;   /* current length of join string */
   if (ln)
   {
      for (i=0; ln[i]; i++);  /* find end of string */
      if (!i) return(NULL);
      for (i--; isspace(ln[i]) && i > 0; i--);  /* find last non-wspace char */
      while (ln[i] == '\\')
      {
         if (jlen < j+i+3)
         {
            jlen = j+i+i+3;
            join = GetLongerString(join, jlen);
         }
         for (k=0; k < i; k++)
            join[j+k] = ln[k];
         j += k;
         join[j++] = ' ';
         join[j] = '\0';
         ln = GetOneLine(fpin);   /* get new line that should be joined */
         assert(ln);              /* can't end file with continue */
         for (i=0; ln[i]; i++);   /* find end of new line */
         for (i--; isspace(ln[i]) && i > 0; i--); /* find last non-wspc char */
         sp = join;
      }
      if (sp == join)
      {
         if (jlen < j+i+3)
         {
            jlen = j+i+i+3;
            join = GetLongerString(join, jlen);
         }
         for (k=0; k <= i; k++)
            join[j+k] = ln[k];
         j += k;
         join[j] = '\n';
         join[j+1] = '\0';
         sp = join;
      }
   }
   return(sp);
}

/* procedure 11 */
static char *GetGoodGcc()
/*
 * Gets gcc path and name along with mandatory flags (-g/-m64/-pg,etc) by
 * querying Make.inc setting
 */
{
   static char gcc[2048];
   static int INIT=0;
   if (!INIT)
   {
      FILE *fpin;
      assert(system("make res/goodgcc.txt > /dev/null 2>&1") == 0);
      fpin = fopen("res/goodgcc.txt", "r");
      assert(fpin);
      assert(fscanf(fpin, "'%[^\']", gcc) == 1);
      fclose(fpin);
   }
   return(gcc);
}

static char *GetKCFlags(char pre)
/*
 * Gets flags being used for <pre>KCFLAGS
 */
{
   char ln[4096];
   FILE *fpin;
   int i;

   if (pre == 'z')
      pre = 'd';
   else if (pre == 'c')
      pre = 's';
   i = system("rm -f res/kcflags.txt");
   sprintf(ln, "grep \"%cKCFLAGS = \" Make.inc | sed s/%cKCFLAGS\\ =\\ // > res/kcflags.txt", toupper(pre), toupper(pre));
   assert(system(ln) == 0);
   fpin = fopen("res/kcflags.txt", "r");
   assert(fpin);
   assert(fgets(ln, 4096, fpin) != NULL);
   fclose(fpin);
/*
 * Get rid of trailing and leading whitespaces
 */
   for (i=0; ln[i]; i++);
   for (i--; isspace(ln[i]); i--);
   ln[i+1] = '\0';
   for (i=0; isspace(ln[i]); i++);
   return(DupString(ln+i));
}
#endif /* end atlas_genparse.h guard */
