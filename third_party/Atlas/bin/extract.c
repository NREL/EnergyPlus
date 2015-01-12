/************************************************************************/
/*                        Extract v4.0.0                                */
/*  (C) Copyright 1994 R. Clint Whaley (rwhaley@cs.utk.edu).            */
/*  This program is distributed under the terms of the Gnu              */
/*  General Public License (GPL), with the following two exceptions:    */
/*  (1) Clause (9), dealing with updating the GPL automatically, is     */
/*      specifically disallowed by the author.  The author will         */
/*      determine if a newer GPL version is still appropriate.          */
/*  (2) The basefiles extract accepts as input, and the extracted       */
/*      files it produces as output, are specifically designated as     */
/*      as outside the scope if this license (i.e. they are *not*       */
/*      required by this license to be GPL).                            */
/*  The full, unaltered, text of the GPL is included at the end of      */
/*  the program source listing.                                         */
/*  ------------------------------------------------------------------  */
/*  Last modified by the author on  10/04/10.                           */
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>

#define NAMLEN 1024
#define LNLEN  2048
#define HANLEN 1024
#define SUBLEN 1024

#define F_nFlags     16
#define F_Case        0
#define F_RemBlank    1
#define F_Lang        2
#define F_Ugly        3
#define F_Verb        4
#define F_RepTab      5
#define F_LnLen       6
#define F_Append      7
#define F_TransNum    8
#define F_Query       9
#define F_AddKeys    10
#define F_Clint      11
#define F_FragMac    12
#define F_FragFlag   13
#define F_LLWarn     14
#define F_LocalProcs 15

#define EC_nExtCmnds 20
#define EC_Extract  0
#define EC_Skip     1
#define EC_Define   2
#define EC_Indent   3
#define EC_Key      4
#define EC_Abort    5
#define EC_MacSub   6
#define EC_EndExt   7
#define EC_Print    8
#define EC_Exp      9
#define EC_While   10 
#define EC_Ifdef   11 
#define EC_Dec     12 
#define EC_AddKey  13
#define EC_Echo    14
#define EC_Iwhile  15
#define EC_Proc    16
#define EC_Output  17
#define EC_Iif     18
#define EC_Mif     19

#define MODE_IIF  1  /* integer if mode */
#define MODE_MIF  2  /* macro if mode */
/*
 * Mode count numbers
 */
#define NMC   2
#define MCIIF 0
#define MCMIF 1

/*
 * KILLMODE must be INMODE or it sets instead
 */
#define INMODE(modenv_, which_) ( (modenv_) | (which_) == (modenv_) )
#define SETMODE(modenv_, which_) (modenv_) = (modenv_) | (which_)
#define KILLMODE(modenv_, which_) (modenv_) = (modenv_) ^ (which_)

enum LANG {LangC, LangF77, LangF90, LangMake};
typedef struct fIlE2 FILE2;
struct fIlE2
{
   char Fnam[NAMLEN];
   unsigned int LineNo;
   FILE *Fp;
   FILE2 *prev;
};

typedef struct aRgStAcK ARGSTACK;
struct aRgStAcK
{
   char KeyArgs[LNLEN];
   int Not;
   ARGSTACK *next;
};

typedef struct eXtMaC EXTMAC;
struct eXtMaC
{
   int HanLen, SubLen;
   char *Handle;
   char *Sub;
   EXTMAC *next;
};

typedef struct pUnYmAc PUNYMAC;
struct pUnYmAc
{
   EXTMAC *mp;
   PUNYMAC *next;
};

typedef struct kEyS KEYS;
struct kEyS
{
   char *KeyHandle, *Match;
   int HanLen;
   EXTMAC *KeyMac;
   ARGSTACK *MyArgs, *ArgStack; 
   KEYS *next;
};

typedef struct InDeNt INDENT;
struct InDeNt
{
   int start;
   int nspac;
   INDENT *next;
   INDENT *prev;
};

typedef struct wOrDs WORDS;
struct wOrDs
{
   char *word;
   WORDS *next;
};

typedef struct eXtPrOc EXTPROC;
struct eXtPrOc
{
   char *ProcNam, *FileNam;
   int nargs;
   WORDS *argnams;
   EXTPROC *next;
};

/*
 * If localprocs is set, I kill not only all my procedures, but all those 
 * defined by subservient extracts, and all proc macros become puny
 */
typedef struct eXtEnV EXTENV;
struct eXtEnV
{
   FILE2 *FpIn, *FpOut;
   char Flags[F_nFlags];/* logical flag array */
   char ExtCmndOn[EC_nExtCmnds];
   KEYS *KeyBase;       /* pointer to the list of keys */
   EXTPROC *MyProcs;    /* Pointer to first procedure I've defined */
   EXTPROC *MyFuncs;    /* Pointer to first function  I've defined */
   EXTMAC *MyMacBeg;
   PUNYMAC *MyPunyMacs; /* Puny macros */
   INDENT *clindent;    /* command-line indent */
   int Joining;         /* are we in a mode where joining lines is OK */
};

FILE *Warn;
EXTPROC *AllProcs=NULL, *AllFuncs=NULL;
EXTMAC *MacroBase=NULL;
INDENT *indbase=NULL;
int LnCount=0, nLnNums, *LnNums;
int ExtDone=0;
char *JoinLn=NULL;
unsigned int mode=0;
unsigned int modedepth[NMC] = {0,0};
char *version = "4.0.0";

/*
 * Prototypes of the functions used before their definition
 */
void HandleLine(EXTENV *EE, char *line);
void ApplyFlags(EXTENV *EE, char *line);

#define Mupcase(C) ( ((C) > 96 && (C) < 123) ? (C) & 0xDF : (C) )
#define Mlowcase(C) ( ((C) > 64 && (C) < 91) ? (C) | 32 : (C) )
#define Mciswspace(C) ( (((C) > 8) && ((C) < 14)) || ((C) == 32) )
#define Mcisnum(C) ( ((C) > 47) && ((C) < 58) )
#define Mabs(x) ( (x) < 0 ? (x) * -1 : (x) )

void *Wmalloc(size_t len)
{
   void *p;
   p = malloc(len);
   assert(p != NULL);
   return(p);
}

/*===========================================================================*/
/* The following routines are string handling routines of various flavors    */
/* Before the C purists attack: I am well aware some of these routines       */
/* duplicate functionality in the ANSI C standard libraries.  However, I     */
/* wrote the first version of extract a long time ago, and I ran into a      */
/* platform where the ANSI C string libraries did not work properly . . .    */
/*===========================================================================*/

int Wstrcmp(char *p1, char *p2)
/*****************************************************************************/
/*  PURPOSE: compares two strings pointed at by p1 & p2.                     */
/*  RETURNS: 0 if they are not the same, else non-zero.                      */
/*****************************************************************************/
{
   if ( (*p1) && (*p1 == *p2) )
   {
      do
      {
         p1++;
         p2++;
      }
      while ( (*p1 == *p2) && (*p1) );
   }
   return( !(*p1 - *p2) );
} /* end Wstrcmp */

int WstrcmpN(char *p1, char *p2, int N)
/*****************************************************************************/
/*  PURPOSE: Compares 1st N characters of strings pointed to by p1 & p2.     */
/*  RETURNS: nonzero if they are equal, else 0                               */
/*****************************************************************************/
{
   while( (*p1++ == *p2++) && (N) ) N--;
   return(!N);
}

int Wfnd_substr(char *str, char *sub)
/*****************************************************************************/
/*  PURPOSE: finds if a given string (sub) is contained in another (str).    */ 
/*  RETURNS: Position of start of string (starting from 1), 0 if string      */
/*           is not found.                                                   */
/*****************************************************************************/
{
   char ch=(*sub);
   char *str2, *sub2;
   int i=0;

   sub++;
   if (*str)
   {
      do
      {
         i++;
         if (*str++ == ch) /* 1st char matches */
         {
            str2 = str;
            sub2 = sub;
            while ( (*str2) && (*str2 == *sub2) ) {str2++; sub2++;}
            if ( !(*sub2) ) return(i);
         }
      }
      while (*str);
      return(0);
   }
   else if (ch) return(0);
   else return(1);
}
int Wstrlen(char *p1)
/*****************************************************************************/
/*  PURPOSE: finds the number of characters in a string, not including \0.   */
/*  RETURNS: Length of string, not including null terminator.                */
/*****************************************************************************/
{
   int length=0;
   if (*p1) do length++; while (*(++p1));
   return (length);
}

int Wstrcpy(char *p1, char *p2)
/*****************************************************************************/
/*  PURPOSE: Copies string pointed at by p2 into p1.                         */
/*  RETURNS: Length of strings, not including null terminator.               */
/*****************************************************************************/
{
   int i=0;

   if (*p1++ = *p2++) do i++; while(*p1++ = *p2++);
   return(i);
}

void WstrcpyN(char *p1, char *p2, int N)
/*****************************************************************************/
/*  PURPOSE: Copies N characters from p2 into p1.                            */
/*****************************************************************************/
{
   if (N) do *p1++ = *p2++; while(--N);
}

void WstrcpyN_LOW(char *p1, char *p2, int N)
/*****************************************************************************/
/*  PURPOSE: Copies N characters from p2 into p1 while lowcasing.            */
/*****************************************************************************/
{
   if (N)
   {
      do 
      {
         *p1++ = Mlowcase(*p2);
         p2++;
      }
      while(--N);
   }
}

void WstrcpyN_UP(char *p1, char *p2, int N)
/*****************************************************************************/
/*  PURPOSE: Copies N characters from p2 into p1 while upcasing.             */
/*****************************************************************************/
{
   if (N)
   {
      do 
      {
         *p1++ = Mupcase(*p2);
         p2++;
      }
      while(--N);
   }
}

int WstrcpyUP(char *p1, char *p2)
/*****************************************************************************/
/*  PURPOSE: Copies string pointed at by p2 into p1, and upcases.            */
/*  RETURNS: Length of strings, not including null terminator.               */
/*****************************************************************************/
{
   int i=0;
   while( *p1++ = Mupcase(*p2) )
   {
      i++;
      p2++;
   }
   return(i);
}

int WstrcpyLOW(char *p1, char *p2)
/*****************************************************************************/
/*  PURPOSE: Copies string pointed at by p2 into p1, and lowcases.           */
/*  RETURNS: Length of strings, not including null terminator.               */
/*****************************************************************************/
{
   int i=0;
   while( *p1++ = Mlowcase(*p2) )
   {
      i++;
      p2++;
   }
   return(i);
}

int Wsafe_slowcase(char line[], int maxlen)
/*****************************************************************************/
/*  PURPOSE: Changes string to lowercase, not touching quoted strings.       */
/*  RETURNS: -1 if there is an unmatched quote, else 0.                      */
/*  NOTE   : algorithm fails if have two strings: 'ST1' STUFF 'ST2'          */
/*           if you wanna fix, need to know the language                     */
/*****************************************************************************/
{
   int i, ERROR=0;
   char quote;

   for (i=0; (line[i] && (i < maxlen)); i++)
   {
      line[i] = Mlowcase(line[i]);
      if (line[i] == 34 || line[i] == 39 || line[i] == '`')  /* is a " or ' */
      {
         quote = line[i++];
         while(line[i]) i++;
         while(line[i] != quote) i--;
         if (i > maxlen) ERROR = -1;
      }
   }
   return(ERROR);
}

int Wsafe_supcase(char line[], int maxlen)
/*****************************************************************************/
/*  PURPOSE: Changes line to uppercase, not touching quoted strings.         */
/*  RETURNS: -1 if there is an unmatched quote, else 0.                      */
/*  NOTE   : algorithm fails if have two strings: 'ST1' STUFF 'ST2'          */
/*           if you wanna fix, need to know the language                     */
/*****************************************************************************/
{
   int i, ERROR=0;
   char quote;

   for (i=0; (line[i] && (i < maxlen)); i++)
   {
      line[i] = Mupcase(line[i]);
      if (line[i] == 34 || line[i] == 39 || line[i] == '`')  /* is a " or ' */
      {
         quote = line[i++];
         while ( (line[i] - quote) && (line[i]) && (i < maxlen) ) i++;
         if ( line[i] != quote ) ERROR = -1;  /* unmatched quote */
      }
   }
   return(ERROR);
}

void Wslowcase(char *p1)
/*****************************************************************************/
/*  PURPOSE: Changes to lowercase entire string pointed to by p1.            */
/*****************************************************************************/
{
   while(*p1 = Mlowcase(*p1)) p1++;
}

void Wsupcase(char *p1)
/*****************************************************************************/
/*  PURPOSE: Changes to uppercase entire string pointed to by p1.            */
/*****************************************************************************/
{
   while(*p1 = Mupcase(*p1)) p1++;
}

void Wsstrip(char *p1)
/*****************************************************************************/
/*  PURPOSE: strip all white spaces from string                              */
/*****************************************************************************/
{
   char *p2=p1;
   do
   {
      if (Mciswspace(*p2)) p2++;
      else *p1++ = *p2++;
   }
   while (*p1);
}

void Wtab2spcs(char *p, int N)
/*****************************************************************************/
/*  PURPOSE: replace all tabs in string p with N spaces                      */
/*****************************************************************************/
{
   char tline[256];
   int i;
   int Wstrcpy(char *p1, char *p2);

   while (*p)
   {
      if (*p == '\t')
      {
         Wstrcpy(tline, p+1);
         i = N;
         while (N--) *p++ = ' ';
         Wstrcpy(p, tline);
      }
      else p++;
   }
}

int Wstr2int(char *str, int *iptr)
/*****************************************************************************/
/*  PURPOSE: read an integer from string str into *iptr                      */
/*  RETURNS: number of characters read in                                    */
/*****************************************************************************/
{
   int i, j;

   sscanf(str, "%d", iptr);
   for (i=0, j=Mabs(*iptr); j > 0; i++) j /= 10;
   if (*iptr == 0) i=1;
   if (str[0] == '+' || str[0] == '-') i++;
   return(i);
}

void Wsafe_mws2spc(char *p1)
/*****************************************************************************/
/*  PURPOSE: replaces contiguous white spaces with 1 space, skip quotes.     */
/*****************************************************************************/
{
   char *p2;

   p2 = p1;

   while (*p2)
   {
      if (*p1 == '"')
      {
         do *p2++ = *p1++; while(*p1 != '"');
         *p2++ = *p1++;
         continue;
      }
      if (Mciswspace(*p1))
      {
         *p2++ = ' ';
         while (Mciswspace(*p1)) p1++;
         continue;
      }
      while ( (*p2) && !(Mciswspace(*p1)) ) *p2++ = *p1++;
   }
}

void Wremove_trailing_blanks(char *p)
/*****************************************************************************/
/*  PURPOSE: Remove trailing whitespace from string p.                       */
/*****************************************************************************/
{
   char *st=p;

   if ( !(*p) ) return;
   do p++; while (*p);
   p--;
   while ( (Mciswspace(*p)) && (p != st) ) p--;
   if ( Mciswspace(*p) )
   {
      *p = '\n';
      p[1] = '\0';
   }
   else
   {
      p[1] = '\n';
      p[2] = '\0';
   }
}

void CharSet(int N, char *x, char val)
/*****************************************************************************/
/* Set N elements starting at x to val                                       */
/*****************************************************************************/
{
   int i;
   for (i=0; i < N; i++) x[i] = val;
}

int commandcpy(char *out, char *in)
/*****************************************************************************/
/* PURPOSE: copy in to out, remove all leading spaces, put 1 space after     */
/*          first word, lowcase first word, then copy rest string unchanged. */
/* RETURNS: Length of first word (the possible command).                     */
/*****************************************************************************/
{
   int i=0;
   while( Mciswspace(*in) ) in++;   /* skip leading white spaces */

   while( (*in) && !Mciswspace(*in) )  /* copy first word */
   {
      *out++ = Mlowcase(*in);
      in++;
      i++;
   }
/*
 * Only 1 space follows word, and copy rest of line unaltered
 */
   *out++ = ' ';
   while( Mciswspace(*in) ) in++;
   while (*out++ = *in++);
   return(i);
}

void keycpy(char *out, char *in)
/*****************************************************************************/
/* PURPOSE: copy in to out, remove all leading spaces, lowcase and put 1     */
/*          space after each word, and leave stuff in ` ` alone.             */
/*****************************************************************************/
{
   char *p, *ch;

   p = out;
   while (*in)
   {
      while( Mciswspace(*in) ) in++;    /* skip leading spaces */

      if (*in == '`')                   /* don't mess with `  ` quotes */
      {
         ch = in;
         while (*ch) ch++;
         while (*ch != '`') ch--;
         while (in != ch) *out++ = *in++;
         *out++ = *in++;
      }

      while( (*in) && !Mciswspace(*in) )  /* copy word */
      {
         *out++ = Mlowcase(*in);
         in++;
      }
      *out++ = ' ';                 /* white spaces collapse to 1 space */
      if (*in) in++;
   }
   *out = '\0';
}

/*===========================================================================*/
/*  These functions print out error and warning messages from extract        */
/*===========================================================================*/

void ExtWarn(EXTENV *EE, char *form, ...)
{
   FILE2 *fp;
   va_list argptr;
   char cline[256];
   int i;

   va_start(argptr, form);
   vsprintf(cline, form, argptr);
   va_end(argptr);
/*
 * Remove newlines from cline
 */
   for (i=0; cline[i]; i++);
   i--;
   if (cline[i] == '\n') cline[i--] = '\0';

   if (EE)
   {
      fflush(EE->FpOut->Fp);
      fprintf(stderr, "\nExtract warning \'%s\'.\n", cline);
      fprintf(stderr, "Input file chain:\n");
      fprintf(stderr, "   Warning occured on line %d of file %s.\n",
              EE->FpIn->LineNo, EE->FpIn->Fnam); 
      for (fp=EE->FpIn->prev; fp; fp = fp->prev)
         fprintf(stderr, "      Which was extracted from line %d of file %s.\n",
                 fp->LineNo, fp->Fnam); 
      fprintf(stderr, "\nWarning occured after line %d of output file %s.\n",
              EE->FpOut->LineNo, EE->FpOut->Fnam); 

   }
   else fprintf(stderr, "\nExtract warning \'%s\'\n", cline);
}

void ExtErr(EXTENV *EE, char *form, ...)
{
   FILE2 *fp;
   va_list argptr;
   char cline[256];
   int i;

   va_start(argptr, form);
   vsprintf(cline, form, argptr);
   va_end(argptr);
/*
 * Remove newlines from cline
 */
   for (i=0; cline[i]; i++);
   i--;
   if (cline[i] == '\n') cline[i--] = '\0';

   if (EE)
   {
      if (EE->FpOut && EE->FpOut->Fp) fflush(EE->FpOut->Fp);
      fprintf(stderr, "\nExtract ERROR \'%s\'.\n", cline);
      if (EE->FpIn)
      {
         fprintf(stderr, "Input file chain:\n");
         fprintf(stderr, "   Error occured on line %d of file %s.\n",
                 EE->FpIn->LineNo, EE->FpIn->Fnam); 
         for (fp=EE->FpIn->prev; fp; fp = fp->prev)
            fprintf(stderr, 
                    "      Which was extracted from line %d of file %s.\n",
                    fp->LineNo, fp->Fnam); 
         fprintf(stderr, "\nError occured after line %d of output file %s.\n\n",
                 EE->FpOut->LineNo, EE->FpOut->Fnam); 

      }
   }
   else fprintf(stderr, "\nExtract ERROR \'%s\'\n\n", cline);

   exit(1);
}

void HandleAtEscape(char *ln)
{
   int i;

   while (i = Wfnd_substr(ln, "@(@)"))
   {
      Wstrcpy(ln+i, ln+i+3);
      ln += i;
   }
}
/*===========================================================================*/
/* These routines do basic file stuff                                        */
/*===========================================================================*/

void PutLn(EXTENV *EE, char *line)
{
   int ExtLn, h, i=0, k;
   static int j=0;
   char *tln;
/* 
 * Check if line needs to be joined with next line
 */
   if (EE->Joining)
   {
      while(line[i]) i++;
      if (i)
      {
         i--;
         while(Mciswspace(line[i]) && i) i--;
      }
      ExtLn = (line[i] == '\\') && (i);
      if (ExtLn) ExtLn = line[i-1] == '@';
/*
 *    If we are extending a line, or have an extended line in memory,
 *    save the present line, and merge it with any previously stored lines
 */
      if ( ExtLn || JoinLn )
      {
         tln = malloc(i + j);
         for (k=0; k != j; k++) tln[k] = JoinLn[k];
         if (ExtLn) h = i + j - 1;
         else h = i + j + 1;
         for (; k < h; k++) tln[k] = line[k-j];
         tln[k] = '\n';
         tln[k+1] = '\0';
         j = h;
         if (JoinLn) free(JoinLn);
         JoinLn = tln;
      }
      else tln = line;
   }
   else 
   {
      tln = line;
      ExtLn = 0;
   }
/*
 * If this line wasn't extended, can finally dump to file
 */
   if (!ExtLn)
   {
      EE->FpOut->LineNo++;
      if (!LnCount)
      {
         ApplyFlags(EE, tln);
         HandleAtEscape(tln);
         fputs(tln, EE->FpOut->Fp);
      }
      else if (EE->FpOut->LineNo == LnNums[nLnNums])
      {
         fprintf(Warn, 
                 "\nLine %d of extracted file corresponds to line %d of \'%s\'\n",
                 EE->FpOut->LineNo, EE->FpIn->LineNo, EE->FpIn->Fnam);
         nLnNums--;
         if (nLnNums == 0)
         {
            fprintf(Warn, "\n\nEXTRACT finished translating line numbers.\n\n");
            exit(0);
         }
      }
      if (JoinLn) free(JoinLn);
      JoinLn = NULL;
      j = 0;
   }
}

int GetLn(EXTENV *EE, char *line)
{
   if (ExtDone) return(0);
   EE->FpIn->LineNo++;
   return( (int) fgets(line, LNLEN, EE->FpIn->Fp) );
}

FILE2 *OpenFile(EXTENV *EE, char *Fnam, char *mode)
{
   char yorn;
   FILE2 *fp;

   fp = (FILE2 *) malloc(sizeof(*fp));
   Wstrcpy(fp->Fnam, Fnam);
   fp->LineNo = 0;

   if (mode[0] == 'w')
   {
      fp->prev = EE->FpOut;
      if ( Wstrcmp(Fnam, "stdout") )
      {
         fp->Fp = stdout;
         return(fp);
      }
      else if ( Wstrcmp(Fnam, "stderr") )
      {
         fp->Fp = stderr;
         return(fp);
      }

      if (EE->Flags[F_Verb] > 2) fprintf(Warn, "Creating file %s.\n", Fnam);

      if (EE->Flags[F_Query])
      {
         if ( !access(Fnam, 0) )
         {
            fprintf(Warn, "File %s already exists.  Overwrite?", Fnam);
            fflush(stdin);
            yorn = getchar();
            if ( (yorn != 'y') && (yorn != 'Y') ) exit(0);
         }
      }
   }
   else if ( (mode[0] == 'r') && (mode[1] == '\0') )
   {
      fp->prev = EE->FpIn;
      if ( Wstrcmp(Fnam, "stdin") )
      {
         fp->Fp = stdin;
         return(fp);
      }

      if (EE->Flags[F_Verb] > 2) 
         fprintf(Warn, "Opening file \'%s\' for input . . .\n", Fnam);
      
      if ( access(Fnam, 0) )
      {
         fprintf(Warn, "Cannot find input file \'%s\'\n", Fnam);
         exit(1);
      }
   }

   fp->Fp = fopen(Fnam, mode);
   if (fp->Fp == NULL) ExtErr(EE, "Error opening file %s",Fnam);
   return(fp);
}

void CloseFile(EXTENV *EE, FILE2 *fp)
{
   if (JoinLn) if (EE->FpOut == fp) PutLn(EE, "\0");
   if (EE->Flags[F_Verb] > 2) fprintf(Warn, "Closing file \'%s\'.\n", fp->Fnam);

   if ( (fp->Fp != stderr) && (fp->Fp != stdin) && (fp->Fp != stdout) )
   {
      if ( fclose(fp->Fp) == EOF )
         ExtErr(EE, "Error closing file %s", fp->Fnam);
   }
   free(fp);
}  /* end close_file */


/*===========================================================================*/
/*  These functions all deal with breaking lines into their respective words */
/*===========================================================================*/

static WORDS *GetWord(char *wrd)
/*****************************************************************************/
/* PURPOSE: Safely allocates word of correct length                          */
/* RETURNS: Allocated WORD.                                                  */
/*****************************************************************************/
{
   WORDS *wp;
   wp = malloc(sizeof(WORDS));
   wp->word = malloc( (Wstrlen(wrd)+1)*sizeof(char) );
   Wstrcpy(wp->word, wrd);
   wp->next = NULL;
   return(wp);
}

static WORDS *KillWord(WORDS *wbase, WORDS *wkill)
/*****************************************************************************/
/* PURPOSE: Kills single word wkill in list of words starting with wbase.    */
/* RETURNS: Possibly new wbase.                                              */
/*****************************************************************************/
{
   WORDS *wptr;

   if (wkill == wbase) wbase = wbase->next;
   else
   {
      for (wptr=wbase; wptr->next != wkill; wptr = wptr->next);
      wptr->next = wkill->next;
   }
   free(wkill->word);
   free(wkill);
   return(wbase);
}

static void KillWords(WORDS *wbase)
/*****************************************************************************/
/* PURPOSE: Frees space used by words, starting at wbase.                    */
/*****************************************************************************/
{
   while(wbase) wbase = KillWord(wbase, wbase);
}

void PrintWords(WORDS *wbase)
/*****************************************************************************/
/* PURPOSE: print words pointed to by wbase (debugging routine)              */
/*****************************************************************************/
{
   printf("Words: ");
   while(wbase)
   {
      printf("'%s' ",wbase->word);
      wbase = wbase->next;
   }
   printf("\n");
}

WORDS *GetVars(char *line, char Ab, char Ae)
/*****************************************************************************/
/* PURPOSE: splits a line into its seperate variables for @declare.  Ab and  */
/*          Ae are the array beginning and ending characters for your        */
/*          language ([] for C, and () for Fortran).  Variables are          */
/*          seperated by whitespace(s) and/or a comma.                       */
/*****************************************************************************/
{
   int i=0, j, k;
   WORDS *wbase=NULL, *wp;
   static WORDS *wptr=NULL;
   char wrd[LNLEN];

   if (line == NULL)
   {
      wptr = NULL;
      return(NULL);
   }
   while(Mciswspace(line[i])) i++;
   if (line[i] == '\0') return(NULL);
   wbase = GetWord(" ");
   if (wptr)
   {
      wp = wptr;
      wptr->next = wbase;
   }
   else wp = wbase;
   wptr = wbase;

   while(line[i])
   {
      j = 0;
      while(line[i] != ',')  /* get one word */
      {
         if (line[i] == '@')  /* check for sticky space */
         {
            if (line[i+1] == '^')
            {
               wrd[j++] = ' ';
               i += 2;
               continue;
            }
         }
         if (Mciswspace(line[i]))
         {
            while(Mciswspace(line[i])) i++;
            if (line[i] != Ab) break;
            else continue;
         }
         if (line[i] == Ab)  /* have an array declaration */
         {
            k = 0;
            do
            {
               wrd[j++] = line[i];
               if (line[i] == Ab) k++;
               else if (line[i] == Ae) k--;
               i++;
            }
            while(k);
            if (!line[i]) break;
         }
         else wrd[j++] = line[i++];
      }
      wrd[j] = '\0';
      wptr->next = GetWord(wrd);
      wptr = wptr->next;
      if (line[i] == ',') 
      {
         i++;
         while(Mciswspace(line[i])) i++;
      }
   }
   if (wptr == wbase) wptr = wbase = KillWord(wp, wbase);
   else wbase = KillWord(wp, wbase);
   return(wbase);
}

WORDS *GetArgs(EXTENV *EE, char *line)
/*****************************************************************************/
/* PURPOSE: splits a line into its seperate words by splitting on whitespcs, */
/*          accepts args in double quotes as one arg.                        */
/*****************************************************************************/
{
   int i=0, j;
   WORDS *wptr, *wbase=NULL;
   char word[LNLEN];

   while(Mciswspace(line[i])) i++;
   wptr = wbase = GetWord(" ");

   while(line[i])
   {
      if (line[i] == '\"')
      {
         i++;
         for (j=0; (line[i] != '\"') && line[i]; word[j++] = line[i++]);
         if (!line[i]) ExtErr(EE, "Unmatched double quote.");
         i++;
      }
      else
      {
         j = 0;
         while(!Mciswspace(line[i]) && line[i])
         {
            if (line[i] != '@' || line[i+1] != '^') 
               word[j++] = line[i++];
            else  /* sticky space */
            {
               word[j++] = ' ';
               i += 2;
            }
         }
      }
      word[j] = '\0';
      wptr->next = GetWord(word);
      wptr = wptr->next;
      while(Mciswspace(line[i])) i++;
   }
   return( KillWord(wbase, wbase) );
}

WORDS *GetWords(char *line)
/*****************************************************************************/
/* PURPOSE: splits a line into its seperate words by splitting on whitespcs  */
/*****************************************************************************/
{
   int i=0, j;
   WORDS *wptr, *wbase;
   char word[LNLEN];

   while(Mciswspace(line[i])) i++;
   wptr = wbase = GetWord(" ");

   while(line[i])
   {
      j = 0;
      while(!Mciswspace(line[i]) && line[i])
      {
         if (line[i] != '@' || line[i+1] != '^') 
            word[j++] = line[i++];
         else  /* sticky space */
         {
            word[j++] = ' ';
            i += 2;
         }
      }
      word[j] = '\0';
      wptr->next = GetWord(word);
      wptr = wptr->next;
      while(Mciswspace(line[i])) i++;
   }
   return( KillWord(wbase, wbase) );
}

FILE *CreateListFile(EXTENV *EE, char *EndStr, int EndLen, char *line,
                     char *tline)
{
   FILE2 fp, *FpHold;
   char fsave[6];
   int i, joining;

   joining = EE->Joining;
   EE->Joining = 0;
   sprintf(fp.Fnam, "Line%d_of_%s\n",EE->FpIn->LineNo, EE->FpIn->Fnam);
   fsave[0] = EE->Flags[F_Case];
   fsave[1] = EE->Flags[F_RemBlank];
   fsave[2] = EE->Flags[F_Ugly];
   fsave[3] = EE->Flags[F_RepTab];
   fsave[4] = EE->Flags[F_Clint];
   fsave[5] = EE->Flags[F_LLWarn];
   EE->Flags[F_Case] = EE->Flags[F_RemBlank] = EE->Flags[F_Ugly] =
                       EE->Flags[F_RepTab] = EE->Flags[F_Clint]  =
                       EE->Flags[F_LLWarn] = 0;
   fp.LineNo = 0;
   fp.Fp = tmpfile();
   FpHold = EE->FpOut;
   EE->FpOut = &fp;
   while(GetLn(EE, line))
   {
      i = commandcpy(tline, line) + 1;
      if (i == EndLen) if ( WstrcmpN(tline, EndStr, EndLen) ) break;
      HandleLine(EE, line);
   }
   EE->FpOut = FpHold;
   EE->Flags[F_Case]     = fsave[0];
   EE->Flags[F_RemBlank] = fsave[1];
   EE->Flags[F_Ugly]     = fsave[2];
   EE->Flags[F_RepTab]   = fsave[3];
   EE->Flags[F_Clint]    = fsave[4];
   EE->Flags[F_LLWarn]   = fsave[5];
   EE->Joining = joining;
   return(fp.Fp);
}

WORDS *fGetWords(EXTENV *EE, char *EndStr, int EndLen, char *line, char *tline)
/*****************************************************************************/
/* PURPOSE: Gets words from the input file until EndStr is found.            */
/*****************************************************************************/
{
   FILE *fp;
   WORDS *wptr, *wp;

   fp = CreateListFile(EE, EndStr, EndLen, line, tline);
   rewind(fp);
   if ( fgets(line, LNLEN, fp) )
   {
      do /* skip leading blank lines */
      {
         wp = wptr = GetWords(line);
         if (wp == NULL) if ( !fgets(line, LNLEN, fp) ) break;
      }
      while (wp == NULL);

      while( fgets(line, LNLEN, fp) ) 
      {
         while (wp->next) wp = wp->next;
         wp->next = GetWords(line);
      }
   }
   fclose(fp);
   return(wptr);
}

WORDS *fGetVars(EXTENV *EE, char *EndStr, int EndLen, char *line, char *tline,
                char Ab, char Ae)
/*****************************************************************************/
/* PURPOSE: Gets variables from the input file until EndStr is found.        */
/*****************************************************************************/
{
   FILE *fp;
   WORDS *wptr=NULL, *wp;

   fp = CreateListFile(EE, EndStr, EndLen, line, tline);
   rewind(fp);

   if ( fgets(line, LNLEN, fp) )
   {
      do
      {
         wptr = GetVars(line, Ab, Ae);
         if (!wptr) if ( !fgets(line, LNLEN, fp) ) break;
      }
      while (wptr == NULL); /* skip leading blank lines */
      while( fgets(line, LNLEN, fp) ) GetVars(line, Ab, Ae);
      GetVars(NULL, Ab, Ae);
   }
   fclose(fp);
   return(wptr);
}


WORDS *GetKeys(char *line)
/*****************************************************************************/
/* PURPOSE: Gets keyargs from keyline, puts a space at end.                  */
/*****************************************************************************/
{
   int i=0, j;
   WORDS *wptr, *wbase;
   char word[LNLEN];

   while(Mciswspace(line[i])) i++;
   wptr = wbase = GetWord(" ");

   while(line[i])
   {
      word[0] = ' ';
      for (j=1; !Mciswspace(line[i]); word[j++] = line[i++]);
      word[j++] = ' ';
      word[j] = '\0';
      wptr->next = GetWord(word);
      wptr = wptr->next;
      while(Mciswspace(line[i])) i++;
   }
   return( KillWord(wbase,wbase) );
}

WORDS *CreateKeyLine0(EXTENV *EE, WORDS *wbase, char *ln)
{
   WORDS *wp, *wp0;

   wp = GetKeys(ln);
   if (wp)
   {
      if (!wbase) wbase = wp;
      else
      {
         for (wp0=wbase; wp0->next; wp0 = wp0->next);
         wp0->next = wp;
      }
      while (wp->next) wp = wp->next;
      if (Wfnd_substr(wp->word, "@\\"))  /* ln extension */
      {
         wbase = KillWord(wbase, wp);
         assert(GetLn(EE, ln));
         CreateKeyLine0(EE, wbase, ln);
      }
   }
   return(wbase);
}

char *CreateKeyLine(EXTENV *EE, char *ln)
{
   char *tln;
   WORDS *wp, *wbase;
   int i;

   wbase = CreateKeyLine0(EE, NULL, ln);
   for (wp=wbase, i=0; wp; wp = wp->next) i += Wstrlen(wp->word)+1;
   if (i >= LNLEN) ExtErr(EE, "Lame-ass static ARGSTACK allocation exceeded");
   tln = malloc((i+1)*sizeof(char));
   assert(tln);
   for (wp=wbase, i=0; wp; wp = wp->next)
   {
      i += WstrcpyLOW(tln+i, wp->word);
      tln[i++] = ' ';
   }
   tln[i] = '\0';
   KillWords(wbase);

   return(tln);
}

void UpLowWords(char cas, WORDS *wbase)
/*****************************************************************************/
/* PURPOSE: if cas == 'u', upcases all words, otherwise, lowcases all words. */
/*****************************************************************************/
{
   void (*fcase)(char *);

   if (cas == 'u') fcase = Wsupcase;
   else fcase = Wslowcase;

   while(wbase)
   {
      fcase(wbase->word);
      wbase = wbase->next;
   }
}

void RemQuoteWords(WORDS *wp)
/*****************************************************************************/
/* PURPOSE: Removes quotes around words, if they exist, and stops when a     */
/*          word starts with -                                               */
/*****************************************************************************/
{
   int i;
   while (wp)
   {
      if (wp->word[0] == '-') break;
      if (wp->word[0] == '\"')
      {
         i = Wstrcpy(wp->word, wp->word+1) - 1;
	 assert(wp->word[i] == '\"');
	 wp->word[i] = '\0';
      }
      wp = wp->next;
   }
}

WORDS *SortWords(char *cas, WORDS *wbase)
/*****************************************************************************/
/* PURPOSE: sorts words in alphabetic order, and up or low cases them based  */
/*          on *cas.                                                         */
/*****************************************************************************/
{
   int i;
   WORDS *wbase2=NULL, *wptr, *wptr2, *prev;

   if (cas) UpLowWords(*cas, wbase);

   while(wbase)
   {
/*
 *    Find largest word left in original list
 */
      wptr2 = wbase;
      for (wptr=wbase->next; wptr; wptr=wptr->next)
      {
         for (i=0; !(wptr->word[i] - wptr2->word[i]) && (i < SUBLEN); i++);
         if (wptr->word[i] > wptr2->word[i]) wptr2 = wptr;
      }
/*
 *    Remove largest word from original list
 */
      for (wptr=wbase; (wptr != wptr2); wptr = wptr->next) prev = wptr;
      if (wptr == wbase) wbase = wbase->next;
      else prev->next = wptr->next;
/*
 *    Put largest word on second list
 */
      wptr2->next = wbase2;
      wbase2 = wptr2;
   }
   return(wbase2);
}

int CountWords(WORDS *wp)
/*****************************************************************************/
/* RETURNS: Number of words in list starting at wp                           */
/*****************************************************************************/
{
   int i;
   for (i=0; wp; wp = wp->next, i++);
   return(i);
}

/*===========================================================================*/
/* These routines all work on extract's macros.                              */
/*===========================================================================*/
EXTMAC *FindMac(char *handle)
{
   int i;
   EXTMAC *ptr;

   i = Wstrlen(handle);
   for(ptr=MacroBase; ptr; ptr=ptr->next)
   {
      if (ptr->HanLen == i + 3)
         if( WstrcmpN(&(ptr->Handle[2]), handle, i) ) 
            if( ptr->Handle[i+2] == ')' ) break;
   }
   return(ptr);
}

/************************************************************************/
/*   The routines push_macro & pop_macro are the heart of extract's     */
/*   macro substitution utility.  A macro is defined by:                */
/*   @define <handle> @<substitution>@                                  */
/*   The @define must followed by one and only one space.               */
/*   The handle is everything until                                     */
/*   the next whitespace.  The begin and end of the substitute string   */
/*   are demarked by the @ character.  Therefore, if we wanted to       */
/*   substitute "joe" for "bob" in the text, we could have following    */
/*   definition:                                                        */
/*   @define bob @joe@                                                  */
/*   the delimiter is @(handle)                                         */
/*   So, using the macro we've defined above,                           */
/*   we would have the following:                                       */
/*   "Well, @(bob) is a fairy."                                         */
/*   Once extracted, this would read, "Well, joe is a fairy.".          */
/*   Macros are based on a stack, so to use old @define's you must      */
/*   do an @undef.                                                      */
/************************************************************************/

EXTMAC *PushMacro2(EXTENV *EE, int PUNY, char *handle, char *sub)
{
   EXTMAC *mp;
   PUNYMAC *pp;

   mp = Wmalloc(sizeof(EXTMAC));
   mp->next = MacroBase;
   MacroBase = mp;
   mp->HanLen = Wstrlen(handle) + 3;
   mp->Handle = Wmalloc(mp->HanLen+1);
   mp->Handle[0] = '@';
   mp->Handle[1] = '(';
   Wstrcpy(mp->Handle+2, handle);
   mp->Handle[mp->HanLen-1] = ')';
   mp->Handle[mp->HanLen] = '\0';
   mp->SubLen = Wstrlen(sub);
   mp->Sub = Wmalloc(mp->SubLen+1);
   Wstrcpy(mp->Sub, sub);
   if ( Wfnd_substr(mp->Sub, mp->Handle) )
      ExtErr(EE, "No recursive macro definitions allowed.");
   if (PUNY)
   {
      pp = Wmalloc(sizeof(PUNYMAC));
      pp->next = EE->MyPunyMacs;
      EE->MyPunyMacs = pp;
      pp->mp = mp;
   }
   return(mp);
}

void PushMacro(EXTENV *EE, char *line)
{
   int i, j;
   char *h, *s, *t;

   h = line + 8;
   for (i=8; (!Mciswspace(line[i])); i++)
      if (line[i] == '@' || line[i] == '(' || line[i] == ')')
         ExtErr(EE, "Prohibited character in macro handle, line=\'%s\'",line);
   j = i;  /* where handle ends */
   i++;
   while( (Mciswspace(line[i])) && (line[i]) ) i++;
   if (line[i++] != '@') ExtErr(EE, "Bad macro definition: \'%s\'.", line);
   s = line + i;
   while (line[i]) i++;
   while (line[i] != '@') i--;
   if (s-1 == line + i) ExtErr(EE, "Bad macro definition: \'%s\'.", line);
   line[i] = '\0';
   line[j] = '\0';
   PushMacro2(EE, EE->Flags[F_FragMac], h, s);
}

int PopThisMacro(EXTENV *EE, EXTMAC *killme)
{
   int i;
   EXTMAC *ptr, *tptr;

   for(ptr=MacroBase; ptr && ptr != killme; ptr=ptr->next) tptr = ptr;
   if (ptr)
   {
      if (ptr == MacroBase) MacroBase = ptr->next;
      else tptr->next = ptr->next;
      free(ptr->Handle);
      free(ptr->Sub);
      free(ptr);
      return(1);
   }
   return(0);
}

int PopMacro2(EXTENV *EE, char *handle)
{
   int i;
   EXTMAC *ptr, *tptr;

   i = Wstrlen(handle);
   for(ptr=MacroBase; ptr; ptr=ptr->next)
   {
      if (ptr->HanLen == i + 3)
         if( WstrcmpN(&(ptr->Handle[2]), handle, i) ) 
            if( ptr->Handle[i+2] == ')' ) break;
      tptr = ptr;
   }
   if (ptr)
   {
      if (ptr == MacroBase) MacroBase = ptr->next;
      else tptr->next = ptr->next;
      free(ptr->Handle);
      free(ptr->Sub);
      free(ptr);
      return(1);
   }
   return(0);
}

int PopMacro(EXTENV *EE, char *line)
{
   char ch, *han;
   int i, j;

   han = line + 7;
   for (i=0; han[i] && !Mciswspace(han[i]); i++)
      if (han[i] == '(' || han[i] == ')' || han[i] == '@')
         ExtErr(EE, "Malformed @undef: \'%s\'", line);
   ch = han[i];
   han[i] = '\0';
   j = PopMacro2(EE, han);
   han[i] = ch;
   return(j);
}

void MakeMacSub(char *line, int I, EXTMAC *mp)
/*****************************************************************************/
/* PURPOSE: substitute mp->Sub at &line[I].                                  */
/*****************************************************************************/
{
   char ch1, ch='l';
   char *p, *st, *sub=mp->Sub;
   char lstr[32];
   int i=I, k, Spcs=0, len=mp->SubLen, sublen=mp->SubLen, hanlen=mp->HanLen;
   void (*cpyfuncN)(char *, char *, int);
   cpyfuncN = WstrcpyN;

   if (i > 2)  /* could have some formatting going on */
   {
/*
 *    If we have @up or @low, set cpyfuncN appropriatly
 */
      if (line[i-1] == 'p') /* maybe @up */
      {
         if ( (line[i-2] == 'u') && (line[i-3] == '@') )
         {
            i -= 3;
            cpyfuncN = WstrcpyN_UP;
         }
      }
      else if (i > 3)
      {
         if (line[i-4] == '@')
         {
            if (line[i-3] == 'l' && line[i-2] == 'o' && line[i-1] == 'w')
            {
               i -= 4;
               cpyfuncN = WstrcpyN_LOW;
            }
            else if (line[i-3] == 'l' && line[i-2] == 'e' && line[i-1] == 'n')
            {
               sprintf(lstr, "%d", mp->SubLen);
               len = sublen = Wstrlen(lstr);
               sub = lstr;
               i -= 4;
            }
         }
      }
/*
 *    See if we have formating of type @#[l,r,c] going on
 */
      if (i > 2)
      {
         ch1 = Mlowcase(line[i-1]);
         if ( (ch1 == 'l') || (ch1 == 'r') || (ch1 == 'c') )
         {
            if (Mcisnum(line[i-2]))
            {
               for (k=i-3; k && Mcisnum(line[k]); k--);
               if (line[k] == '@') /* it is one of those formats */
               {
                  line[i-1] = '\0'; /* gonna overwrite this later anyway */
                  sscanf(line+k+1, "%d", &len);
                  if (len < sublen) sublen = len;
                  i = k;
                  ch = ch1;
               }
            }
         }
      }
   }
   hanlen += I - i;
/*
 * Figure out how many spaces to put before mp->Sub due to formatting
 */
   if (len != sublen)
   {
      if (ch == 'r') Spcs = len - sublen;
      else if (ch == 'c') Spcs = (len - sublen) / 2;
   }
/*
 * If formatted Sub longer than handle, move last part of line to right
 */
   if (len > hanlen)
   {
      p = line + I + mp->HanLen;
      k = len - hanlen;  /* number of elements to move string */
      st = p - 1;
      while (*p) p++;
      do p[k] = *p; while (--p != st);
   }
/*
 * If Macro handle longer than formatted Sub, move last part of line to left
 */
   else if (len != hanlen) Wstrcpy(line+i+len, line+I+mp->HanLen);
   for (k=Spcs; k; k--) line[i++] = ' ';  /* handle preceding spaces */
   cpyfuncN(line+i, sub, sublen);     /* Copy Macro sub string */
   i += sublen;                           /* handle trailing spaces */
   for (k=len - Spcs - sublen; k; k--) line[i++] = ' ';
}

int ExpandMacro(EXTMAC *macp, char *line)
{
   int i, j, k, len, Expanded=0;
   char ch, tline[LNLEN];
   int (*cpyfunc)(char *, char *);

   while ( (i = Wfnd_substr(line, macp->Handle)) )
   {
      Expanded = 1;
      MakeMacSub(line, i-1, macp);
   }
   return(Expanded);
}

int ExpandThisMacro(EXTENV *EE, char *line, int i, int hlen)
{
   EXTMAC *mp;
   
   for (mp=MacroBase; mp; mp = mp->next)
      if (mp->HanLen == hlen) if ( WstrcmpN(line+i, mp->Handle, hlen) ) break;
   if (mp)
   {
      MakeMacSub(line, i, mp);
      return(1);
   }
   else return(0);
}

void MacroSub(EXTENV *EE, char *line)
{
   char *p;
   int i, hlen, il=0, DONE=1;
   EXTMAC *macp;

/*
 * Try substituting macros from left to right in line; if this fails, use
 * slower but more general method
 */
   while ( i = Wfnd_substr(line+il, "@(") )
   {
      il += i - 1;
      p = line + il + 2;
      for (hlen=2; (*p) && (*p != ')'); hlen++, p++);
      if (*p) hlen++;
      else return;  /* no macros without end paren */
      if (!ExpandThisMacro(EE, line, il, hlen))
      {
         DONE=0;
         break;
      }
   }
   if (!DONE)
   {
      macp=MacroBase; 
      while(macp)
      {
         i = Wfnd_substr(line, "@(");
         if (!i) break;
         if ( ExpandMacro(macp, line) ) macp = MacroBase;
         else macp = macp->next;
      }
   }
}  /* end of MacroSub */

void KillMyMacros(EXTENV *EE)
{
   EXTMAC *mp;

   while(MacroBase != EE->MyMacBeg)
   {
      mp = MacroBase;
      MacroBase = mp->next;
      free(mp->Handle);
      free(mp->Sub);
      free(mp);
   }
}


/*===========================================================================*/
/* These functions deal with key manipulations.                              */
/*===========================================================================*/
void KillThisKey(EXTENV *EE, KEYS *DeadKey)
{
   char line[LNLEN];
   KEYS *key, *prev=NULL;
   ARGSTACK *args;

   assert(EE->KeyBase != NULL);
   for (key=EE->KeyBase; key != DeadKey; key = key->next) prev = key;

   assert( PopThisMacro(EE, key->KeyMac) );  /* pop automatic macro */
/*
 * Remove key from list
 */
   if (key == EE->KeyBase) EE->KeyBase = key->next;
   else prev->next = key->next;

   while (key->ArgStack)
   {
      args = key->ArgStack->next;
      free(key->ArgStack);
      key->ArgStack = args;
   }
   if (key->MyArgs) free(key->MyArgs);
   free(key->KeyHandle);
   free(key->Match);
   free(key);
}

void KillKey(EXTENV *EE, char *s)
{
   int slen;
   KEYS *key, *prev=NULL;

   slen = Wstrlen(s);
   key = EE->KeyBase;
   while(key)
   {
      if (slen+2 == key->HanLen) /* possible match if they're same length */
      {
         if ( WstrcmpN(s, &key->KeyHandle[1], slen) )
         {
            KillThisKey(EE, key);
            if (prev) key = prev->next;
            else key = EE->KeyBase;
         }
         else {  prev = key;  key = key->next;  }
      }
      else {  prev = key;  key = key->next;  }
   }
}

void AddKey(EXTENV *EE, char *s)
{
   char *cp, *cp2;
   int i, j, BegAt, EndAt, len, err=0;
   KEYS *key;

   key = Wmalloc(sizeof(KEYS));
   key->next = EE->KeyBase;
   EE->KeyBase = key;
   for(i=0; s[i] != '=' && s[i]; i++);
   if (!(s[i])) ExtErr(NULL, "Bad key input \'%s\'.", s);
   if (!(s[i+1])) ExtErr(NULL, "Bad key input \'%s\'.", s);
   key->HanLen = i + 2;
   key->KeyHandle = Wmalloc(key->HanLen + 1);
   key->KeyHandle[0] = '@';
   WstrcpyN_LOW(key->KeyHandle+1, s, i);
   key->KeyHandle[key->HanLen-1] = ' ';
   key->KeyHandle[key->HanLen] = '\0';
   cp2 = s + i + 1;
   for (i=0; !Mciswspace(cp2[i]) && cp2[i]; i++);
   if (!i) ExtErr(NULL, "Bad key input \'%s\'.", s);
   len = i + 2;
   BegAt = (*cp2 == '@');
   EndAt = (cp2[i-1] == '@') && (i > 1);
   if (BegAt)
   {
      cp2++;
      j = 0;
      len -= 2;
      i--;
   }
   else j = 1;
   if (EndAt)
   {
      len -= 2;
      i--;
   }
   if ( i < 0 || len < 0) ExtErr(NULL, "Bad key input \'%s\'.", s);
   key->Match = Wmalloc(len+1);
   if (!BegAt) key->Match[0] = ' ';
   WstrcpyN_LOW(key->Match+j, cp2, i);
   if (!EndAt) key->Match[len-1] = ' ';
   key->Match[len] = '\0';

   key->MyArgs = (ARGSTACK *) Wmalloc(sizeof(ARGSTACK));
   key->MyArgs->Not = 1;
   key->MyArgs->KeyArgs[0] = '\0';
   key->ArgStack = NULL;
/*
 * Add automatic key macro
 */
   key->KeyHandle[key->HanLen-1] = '\0';
   cp = cp2 = Wmalloc(len+3);
   if (*key->Match != ' ') *cp++ = '@';
   cp += Wstrcpy(cp, key->Match) - 1;
   if (*cp != ' ') {  cp++;  *cp++ = '@';  }
   *cp = '\0';
   if (*cp2 == ' ') cp = cp2 + 1;
   else cp = cp2;
   key->KeyMac = PushMacro2(EE, 0, key->KeyHandle, cp);
   key->KeyHandle[key->HanLen-1] = ' ';
   free(cp2);
}

void PushKey(EXTENV *EE, KEYS *Key)
{
   int i;
   ARGSTACK *NewArgs;

   i = sizeof(ARGSTACK);
   NewArgs = (ARGSTACK *) Wmalloc(i);
   WstrcpyN((void *) NewArgs, (void *) Key->MyArgs, i);
   NewArgs->next = Key->ArgStack;
   Key->ArgStack = NewArgs;
}

void PopKey(EXTENV *EE, KEYS *Key)
{
   if (Key->ArgStack == NULL)
   {
      ExtWarn(EE, "Trying to pop from key ( %s) with nonexistant stack.\n",
              Key->KeyHandle);
      return;
   }
   if (Key->MyArgs) free(Key->MyArgs);
   Key->MyArgs = Key->ArgStack;
   Key->ArgStack = Key->ArgStack->next;
}

void PeekKey(EXTENV *EE, KEYS *Key)
{
   if (Key->ArgStack == NULL)
   {
      ExtWarn(EE, "Trying to peek from key(%s) with nonexistant stack.\n",
              Key->KeyHandle);
      return;
   }
   if (Key->MyArgs == NULL) Key->MyArgs = Wmalloc(sizeof(ARGSTACK));
   WstrcpyN((void *) Key->MyArgs, (void *) Key->ArgStack, sizeof(ARGSTACK));
}

void CopyKeys(EXTENV *EE1, EXTENV *EE2)
{
   KEYS *Key, *nKey, *tKey1, *tKey2;
   ARGSTACK *ap, *ap2;

   if (EE2)
   {
      if (EE2->KeyBase)
      {
         nKey = (KEYS *) malloc(sizeof(KEYS));
         if (EE1->KeyBase == NULL) EE1->KeyBase = nKey;
         else
         {
            for (Key=EE1->KeyBase; Key->next; Key = Key->next);
            Key->next = nKey;
         }
         nKey->next = NULL;
         tKey1 = nKey;
         for (Key=EE2->KeyBase; Key; Key = Key->next)  /* for all Key in OldEnv */
         {
/*
 *          Do not define keys that have already been defined on command line
 */
            for (tKey2=EE1->KeyBase; tKey2 != tKey1; tKey2 = tKey2->next)
               if (Wstrcmp(tKey2->KeyHandle, Key->KeyHandle)) break;
            if (tKey2 == tKey1)
            {
               nKey->HanLen = Key->HanLen;
               nKey->KeyHandle = Wmalloc(Key->HanLen+1);
               Wstrcpy(nKey->KeyHandle, Key->KeyHandle);
               nKey->Match = malloc( Wstrlen(Key->Match)+1 );
               Wstrcpy(nKey->Match, Key->Match);
               nKey->KeyMac = PushMacro2(EE1, 0, nKey->KeyHandle, nKey->Match);
               nKey->next = (KEYS *) malloc(sizeof(KEYS));
               assert(nKey->next);
               nKey->next->next = NULL;
/*
 *             copy MyArgs and ArgStack
 */
               nKey->MyArgs = (ARGSTACK *) malloc(sizeof(ARGSTACK));
               WstrcpyN((void *) nKey->MyArgs, (void *) Key->MyArgs,
                        sizeof(ARGSTACK));
               if (Key->ArgStack)
               {
                  ap2 = nKey->ArgStack = (ARGSTACK *) malloc(sizeof(ARGSTACK));
                  WstrcpyN((void *) nKey->ArgStack, (void *) Key->ArgStack,
                           sizeof(ARGSTACK));
                  for (ap=Key->ArgStack->next; ap; ap=ap->next)
                  {
                     ap2->next = (ARGSTACK *) malloc(sizeof(ARGSTACK));
                     ap2 = ap2->next;
                     WstrcpyN((void *) ap2, (void *) ap, sizeof(ARGSTACK));
                  }
                  ap2->next = NULL;
               }
               else nKey->ArgStack = NULL;

               nKey = nKey->next;
            }
         }
         if (nKey == EE1->KeyBase) EE1->KeyBase = nKey->next;
         else
         {
            for (Key=EE1->KeyBase; Key->next != nKey; Key = Key->next);
            Key->next = nKey->next;
         }
         if (nKey) free(nKey);
      }
   }
}

KEYS *IsKeyLn(EXTENV *EE, char *line)
{
   int i;
   KEYS *Key;

   for (i=0; !Mciswspace(line[i]) && line[i]; i++);
   i++;
   for(Key=EE->KeyBase; Key; Key = Key->next)
      if (Key->HanLen == i)
         if ( WstrcmpN(Key->KeyHandle, line, Key->HanLen) ) break;
   return(Key);
}

void HandleKeyLn(EXTENV *EE, char *line, KEYS *Key)
{
   char *cptr, *cp, tln[LNLEN];
   int i, j, k, argmatch, OneLn=0, CHSTAK=0;
   WORDS *wp, *wbase;
   void FindKeyMatch(EXTENV *EE, KEYS *Key);

   Wsafe_slowcase(line, LNLEN);
/*
 * If it's a 1-line keyline
 */
   for (i=0; (line[i]) && (line[i] != '`'); i++);
   if (line[i]) 
   {
      OneLn = i + 1;
      line[i] = '\0';
      PushKey(EE, Key);
   }

   i = Key->HanLen;
   cptr = Key->MyArgs->KeyArgs;
/*
 * Check if we are changing the argstack
 */
   if (line[i] == '@')
   {
      if ( WstrcmpN(&line[i], "@printargs", 10) )
      {
         i = 0;
         if (Key->MyArgs->Not) 
         {
            line[i++] = '!';
            line[i++] = ' ';
         }
         Wstrcpy(&line[i], cptr);
         fprintf(Warn, "Keyargs: \'%s\'\n", line);
         return;
      }
      CHSTAK = 1;
      if ( WstrcmpN(&line[i], "@push", 5) ) 
      {
         PushKey(EE, Key);
         PeekKey(EE, Key);
      }
      else if ( WstrcmpN(&line[i], "@pop", 4) ) PopKey(EE, Key);
      else if ( WstrcmpN(&line[i], "@peek", 5) ) PeekKey(EE, Key);
      else CHSTAK = 0;
   }
/*
 * See if we are adding/removing to/from MyArgs
 */
   if (!CHSTAK)
   {
      if ( (line[i] == '-') || (line[i] == '+') )
      {
         i += 2;
         Wstrcpy(tln, line+i);
         wbase = CreateKeyLine0(EE, NULL, tln);
         for(wp=wbase; wp; wp = wp->next)
         {
            k = Wfnd_substr(cptr, wp->word);
            j = Wstrlen(wp->word);

            if (line[i-2] == '+')
            {
               if (Key->MyArgs->Not) 
               {
                  if (k) Wstrcpy(&cptr[k-1], &cptr[k-1+j]);
               }
               else if (!k) 
               {
                  k = Wstrlen(cptr);
                  if (k == 0) cptr[k++] = ' ';
                  keycpy(&cptr[k], wp->word);
               }
            }
            else 
            {
               if (Key->MyArgs->Not) 
               {
                  if (!k)
                  {
                     k = Wstrlen(cptr);
                     if (k == 0) cptr[k++] = ' ';
                     keycpy(&cptr[k], wp->word);
                  }
               }
               else if (k) Wstrcpy(&cptr[k-1], &cptr[k-1+j]);
            }
         }
         KillWords(wbase);
      }
/*
 *    Otherwise, making a new MyArgs
 */
      else
      {
         if (line[i] ==  '!')
         {
            Key->MyArgs->Not = 1;
            i += 2;
         }
         else Key->MyArgs->Not = 0;
         cptr[0] = ' ';
         Wstrcpy(tln, line+i-1);
         cp = CreateKeyLine(EE, tln);
         if (cp)
         {
            Wstrcpy(cptr+1, cp);
            free(cp);
         }
         else cptr[1] = '\0';
      }
   }

   argmatch = Wfnd_substr(Key->MyArgs->KeyArgs, Key->Match);
   if (Key->MyArgs->Not) argmatch = !argmatch;

   if (OneLn)
   {
/*
 *    Get one line, and handle it
 */
      if (argmatch)
      {
         Wstrcpy(line, &line[OneLn]);
         for(i=0; line[i]; i++);
         while (line[i] != '`') i--;
         line[i++] = '\n';
         line[i] = '\0';
         HandleLine(EE, line);
      }
/*
 *    Restore key to previous state
 */
      PopKey(EE, Key);
      i = Wstrcpy(line, Key->KeyHandle);
      if (Key->MyArgs->Not)
      {
         line[i++] = '!';
         line[i++] = ' ';
      }
      Wstrcpy(&line[i], Key->MyArgs->KeyArgs);
      HandleLine(EE, line);
   }
   else if (!argmatch) FindKeyMatch(EE, Key);
}

void FindKeyMatch(EXTENV *EE, KEYS *Key)
{
   int j, k, argmatch=0;
   char line[LNLEN], tline[LNLEN];

   j = Key->HanLen + 1;
   k = EE->Joining;
   EE->Joining = 0;
   while(!argmatch)
   {
      argmatch = !GetLn(EE, line);
      if (EE->ExtCmndOn[EC_MacSub]) MacroSub(EE, line);
      keycpy(tline, line);
      if (!argmatch)
      {
         argmatch = WstrcmpN(tline, Key->KeyHandle, Key->HanLen);
         if (argmatch) HandleKeyLn(EE, tline, Key);
      }
   }
   EE->Joining = k;
}


/*===========================================================================*/
/* Rest of file is misc catagory :-)                                         */
/*===========================================================================*/

int icalc(EXTENV *EE, char line[])
{
   int i, k=0;
   int istack[100];

   if ( Mcisnum(line[0]) || (line[0] == '-' && Mcisnum(line[1])) )
      i = Wstr2int(line, &istack[k]);
   else ExtErr(EE, "Non-numeric string in @iexp: \'%s\'", line);
   while ( Mciswspace(line[i]) ) i++;

   while(line[i])
   {
      if ( Mcisnum(line[i]) || (line[i] == '-' && Mcisnum(line[i+1])) )
         i += Wstr2int(&line[i], &istack[++k]);
      else
      {
         switch(line[i++])
         {
         case '+':
            istack[k-1] += istack[k];
            k--;
            break;
         case '-':
            istack[k-1] = istack[k] - istack[k-1];
            k--;
            break;
         case '/':
            istack[k-1] = istack[k] / istack[k-1];
            k--;
            break;
         case '*':
            istack[k-1] = istack[k] * istack[k-1];
            k--;
            break;
         case '%':
            istack[k-1] = istack[k] % istack[k-1];
            k--;
            break;
         case 'a':  /* absolute value */
            if (istack[k] < 0) istack[k] = -istack[k];
            break;
#if 0
         case 'p':  /* print stack */
            for (j=0; j <= k; j++) printf(" %d ",istack[j]);
            printf("!\n");
            break;
#endif
         default:
            ExtErr(EE, 
                   "Unrecognized character/non-numeric string in @iexp: \'%s\'",
                   line);
         }
      }
      while ( Mciswspace(line[i]) ) i++;
   }
   return(istack[k]);
}

void MakeLnButtUgly(EXTENV *EE, char *line)
{
   char comm='*', ext='$';
   int i, k;
   static int LNSKIP=1;

   if (EE->Flags[F_Ugly] == 3)
   {
      comm = '!';
      ext = '&';
   }
/*
 *  All comment lines begin with '*'; all other lines must be upcase
 */
   if ( !Mciswspace(line[0]) && !Mcisnum(line[0]) ) line[0] = comm;
   else 
   {
/*      fprintf(stderr, "line0=%c, %d, %d\n",line[0], !Mciswspace(line[0]), !Mcisnum(line[0])); */
      for (i=0; Mciswspace(line[i]); i++);
/*
 *    if line is blank, make it a comment
 */
      if (line[i] == '\0')
      {
         if (EE->Flags[F_Ugly] != 3)
         {
            line[0] = comm;
            line[1] = '\n';
            line[2] = '\0';
            return;
         }
      }
      if (EE->Flags[F_Case] == 1) Wsafe_supcase(line, 72);
      else if (EE->Flags[F_Case] == 2) Wsafe_slowcase(line, 72);
/*
 *    Use only $ for continuation
 */
      if ( !Mciswspace(line[5]) ) 
      {
         if ( (line[5]) && (line[0] != comm) ) line[5] = ext;
      }
      else  /* Right justify labels to col 5 */
      {
         for (i=0; ( (i < 5) && (!Mcisnum(line[i])) ); i++);
         if (i < 5)
         {
            sscanf(&line[i], "%d", &k);
            for (i=0; i < 5; i++) line[i] = ' ';
            sprintf(line, "%5d", k);
            line[5] = ' ';
         }
      }
   }

/*
 * Change lines of ---- to blank lines 
 */
   if (EE->Flags[F_Ugly] == 2)
   {
      if (line[0] == comm)
      {
         for (i=1; ( (Mciswspace(line[i]) || line[i] == '-') ); i++);
         if (line[i] == '\0')
         {
            if (LNSKIP) line[0] = '\0';
            else
            {
               line[0] = comm;
               line[1] = '\n';
               line[2] = '\0';
            }
            LNSKIP = !LNSKIP;
         }
      }
   }
}

void HandleIfdef(EXTENV *EE, char line[])
{
   char tline[LNLEN];
   int i, j, k=7, defined, KeepOn;

   if (line[7] == '!')
   {
      k++;
      while(Mciswspace(line[k])) k++;
   }
   for(i=k; !Mciswspace(line[i]); i++) tline[i-k] = line[i];
   tline[i-k] = '\0';
   defined = (int) FindMac(tline);
   if (line[7] == '!') defined = !defined;
   if (GetLn(EE, line)) KeepOn = 1;
   else KeepOn = 0;
   if (KeepOn) j = commandcpy(tline, line);
   if (defined)  /* loop over calls to HandleLine */
   {
      while( KeepOn )
      {
         if (j == 9) KeepOn = !WstrcmpN(tline, "@endifdef ", 10);
         if (KeepOn)
         {
            HandleLine(EE, line);
            KeepOn = GetLn(EE, line);
            if (!KeepOn)
               fprintf(stderr, "File %s ended inside of an @ifdef\n",
                       EE->FpIn->Fnam);
            j = commandcpy(tline, line);
         }
      }
   }
   else  /* do not call HandleLine while skipping */
   {
      i = EE->Joining;
      EE->Joining = 0;
      while( KeepOn )
      {
         if (j == 9)
         {
            if (WstrcmpN(tline, "@endifdef ", 10)) KeepOn--;
         }
         else if (j == 6) if (WstrcmpN(tline, "@ifdef ", 7)) KeepOn++;
         if (KeepOn)
         {
            if (!GetLn(EE, line))
            {
               fprintf(stderr, "File %s ended inside of %d @ifdefs\n",
                       EE->FpIn->Fnam, KeepOn);
               KeepOn = 0;
            }
            else if (EE->ExtCmndOn[EC_MacSub]) MacroSub(EE, line);
            j = commandcpy(tline, line);
         }
      }
      EE->Joining = i;
   }
}

void HandleDec(EXTENV *EE, char *line)
{
   char tline[LNLEN];
   char *outln, *endstr=NULL;
   char *cas, ch, Ab, Ae;
   enum LANG WhatLang;
   int i, k, EndLen=0, istart, istart2=(-1), LnLen;
   int EXTENDLINE=1, SORT=1;
   WORDS *wptr, *wp, *wp0;

   LnLen = EE->Flags[F_LnLen];
   if (EE->Flags[F_Lang] == 'c')
   {
      WhatLang = LangC;
      if (LnLen == 0) LnLen = 79;
      Ab = '[';
      Ae = ']';
   }
   else if (EE->Flags[F_Lang] == 'm')
   {
      WhatLang = LangMake;
      if (LnLen == 0) LnLen = 79;
      Ae = Ab = '\"';
   }
   else
   {
      if (EE->Flags[F_Lang] == '9')
      {
         WhatLang = LangF90;
         if (LnLen == 0) LnLen = 77;
      }
      else
      {
         WhatLang = LangF77;
         if (LnLen == 0) LnLen = 71;
      }
      Ab = '(';
      Ae = ')';
   }
   switch(EE->Flags[F_Case])
   {
   case 0:
      cas = NULL;
      break;
   case 1:
      ch = 'u';
      cas = &ch;
      break;
   case 2:
      ch = 'l';
      cas = &ch;
      break;
   }

   outln = malloc(LnLen+10);
   for (i=10; line[i] != '\"'; i++)  outln[i-10] = line[i];
   Wsafe_mws2spc(&line[i+1]);
   if (line[i+1])
   {
      EXTENDLINE = (Mupcase(line[i+2]) != 'N');
      if ( (line[i+2]) && (line[i+3]) )
      {
         SORT = (Mupcase(line[i+4]) != 'N');
         if ( (line[i+4]) && (line[i+5]) )
         {
            if (line[i+6] == '\"')
            {
                for (k=i+7; line[k] != '\"'; k++) if (!line[k]) ExtErr(EE, "Unmatched quote on line %d of basefile %s", EE->FpIn->LineNo, EE->FpIn->Fnam);
                EndLen = k - (i+7);
                endstr = (char *) malloc(EndLen+1);
                for (k=0; k != EndLen; k++) endstr[k] = line[k+i+7];
                endstr[EndLen] = '\0';
                k += i + 8;
                if (line[k]) k++;
            }
            else k = i + 6;
            if ( Mcisnum(line[k]) )
            {
               Wstr2int(&line[k], &istart2);
               istart2--;
            }
         }
      }
   }
   istart = i - 10;
   if (istart2 == -1) istart2 = istart;
   wp0 = wptr = fGetVars(EE, "@enddeclare ", 12, line, tline, Ab, Ae);

   if (EXTENDLINE)
   {
      if (WhatLang == LangMake) LnLen -= 2;
   }
   else if (WhatLang == LangC) LnLen--;
   if (SORT) wp0 = wptr = SortWords(cas, wptr);
/*
 * After sorting, find last parameter and add end string to it
 */
   if (endstr)
   {
      for (wp=wptr; wp->next; wp = wp->next);
      sprintf(tline, "%s%s", wp->word, endstr);
      wp0 = wptr = KillWord(wp0, wp);
      if (wp0)
      {
         for (wp=wptr; wp->next; wp = wp->next);
         wp->next = GetWord(tline);
      }
      else
         wp0 = wptr = GetWord(tline);
   }

   while(wptr)
   {
      for(i=istart; i <= LnLen; i++)
      {
         outln[i] = wptr->word[i-istart];
         if (!outln[i]) break;
      }
      if (i > LnLen)
      {
         if (istart == istart2) /* parameter too long to fit on any line */
            ExtErr(EE, "Parameter \'%s\' too long to fit on @DECLARE line",
                   wptr->word);
         i = istart;
         if (!EXTENDLINE) 
         {
            i -= 2;
            if (WhatLang == LangC) outln[i++] = ';';
         }
         else
         {
            if (WhatLang == LangMake) outln[i++] = '\\';
            else i--;
            if (EE->Flags[F_Lang] == '9')
            {
               while(i <= LnLen) outln[i++] = ' ';
               i = LnLen + 1;
               outln[i++] = ' ';
               outln[i++] = '&';
            }
            else if (EE->Flags[F_Ugly] == 3)
            {
               while(i < 72) outln[i++] = ' ';
               i = 72;
               outln[i++] = '&';
            }
         }
         outln[i++] = '\n';
         outln[i] = '\0';
         HandleLine(EE, outln);
         istart = istart2;
         if (EXTENDLINE)
         {
            for(i=0; i < istart2; i++) outln[i] = ' ';
            if (EE->Flags[F_Lang] == 'f') outln[5] = '$';
         }
      }
      else 
      {
         wptr = wptr->next;
         if (WhatLang != LangMake) outln[i++] = ',';
         outln[i++] = ' ';
         istart = i;
      }
   }
   if (WhatLang == LangMake) i--;
   else i -= 2;
   if (endstr) free(endstr);
   outln[i++] = '\n';
   outln[i] = '\0';
   KillWords(wp0);
   HandleLine(EE, outln);
   free(outln);
}

void HandleMultidef(EXTENV *EE, char *line)
{
   WORDS *wp, *wp0;
   char tline[LNLEN], cline[LNLEN];
   char *han=line+10;
   int i, j, k;
   
   for (i=10; ((!Mciswspace(line[i])) && line[i]); i++);
   if (line[i]) line[i++] = '\0';
   for (j=i; ( (line[j]) && Mciswspace(line[j]) ); j++);
   if (line[j]) /* one-line multidef */
      wp0 = GetWords(&line[j]);
   else
   {
      han = Wmalloc(i-10);
      Wstrcpy(han, line+10);
      k = EE->Joining;
      EE->Joining = 0;
      wp0 = fGetWords(EE, "@endmultidef ", 13, line, tline);
      EE->Joining = k;
   }
   for (wp=wp0; wp; wp = wp->next) PushMacro2(EE, EE->Flags[F_FragMac], han, wp->word);
   KillWords(wp0);
   if (han != line+10) free(han);
}

void DumpSkip(EXTENV *EE, FILE *fpout, char *begstr, char *endstr)
/*
 * Dumps to file/skips lines between matching begstr and endstr
 */
{
   char ln[LNLEN], tln[LNLEN];
   int ibeg, iend, KeepOn=1, i, k;

   k = EE->Joining;
   EE->Joining = 0;
   ibeg = Wstrlen(begstr);
   iend = Wstrlen(endstr);
   do
   {
      if ( GetLn(EE, ln) )
      {
         i = commandcpy(tln, ln) + 1;
         if (EE->ExtCmndOn[EC_MacSub]) MacroSub(EE, tln);
         if (i == ibeg) if ( WstrcmpN(tln, begstr, ibeg) ) KeepOn++;
         if (i == iend) if ( WstrcmpN(tln, endstr, iend) ) KeepOn--;
         if (fpout && KeepOn) fputs(ln, fpout);
      }
      else break;
   }
   while (KeepOn);
   if (fpout) fflush(fpout);
   EE->Joining = k;
}

void HandleWhile(EXTENV *EE, char line[])
{
   FILE2 tfp, *OldFp;
   WORDS *wp, *wp2;
   char tline[LNLEN];
   char *han;
   int KeepOn, i;
   static int inest=0;

   inest++;
/*
 * Define while line definitions
 */
   wp2 = GetWords(&line[10]);
   if (wp2 == NULL) ExtErr(EE, "Malformed @whiledef=\'%s\'",line);
   i = Wstrlen(wp2->word) + 1;
   han = Wmalloc(i);
   Wstrcpy(han, wp2->word);

   for (wp=wp2->next; wp; wp = wp->next)
      PushMacro2(EE, EE->Flags[F_FragMac], han, wp->word);

   KillWords(wp2);

/*
 * Open temporary file, and dump while loop to it
 */
   sprintf(tfp.Fnam, "Whiledeftmpfile%d", inest);
   tfp.LineNo = 0;
   tfp.Fp = tmpfile(); /* fopen(tfp.Fnam, "w+"); */
   DumpSkip(EE, tfp.Fp, "@whiledef ", "@endwhile ");

   OldFp = EE->FpIn;
   tfp.prev = OldFp;
   EE->FpIn = &tfp;

   while ( FindMac(han) )
   {
      rewind(tfp.Fp);
      while( GetLn(EE, line) ) HandleLine(EE, line);
      PopMacro2(EE, han);
   }

   EE->FpIn = OldFp;
   fclose(tfp.Fp);
   free(han);
   inest--;
}

char GetIntComp(EXTENV *EE, char *ln, int *A1CONST, int *A2CONST,
                int *ia1, int *ia2, char *mac1, char *mac2)
/*
 * Expects comparison line of form arg1 [<,>,!,=] arg2;  
 * This routine finds arg1 and arg2.  Note that if
 * arg1 or arg2 is a macro, it cannot begin with a number or -
 * Returns type of comparison (<, =, !)
 */
{
   EXTMAC *mp;
   WORDS *wp, *wp0, *wp1;
   char comp = '<';

   *ia1 = *ia2 = *A1CONST = *A2CONST = 0;
   if (mac1) *mac1 = '\0';
   if (mac2) *mac2 = '\0';

   wp = GetWords(ln);
   if (wp == NULL || wp->next == NULL || wp->next->next == NULL)
      ExtErr(EE, "Invalid integer condition: '%s'\n", ln);

   { wp0 = wp; wp1 = wp->next->next; }
   if (wp->next->word[0] == '>') { wp1 = wp; wp0 = wp->next->next; }
   else if (wp->next->word[0] == '=') comp = '=';
   else if (wp->next->word[0] == '!') comp = '!';
   else if (wp->next->word[0] != '<')
      ExtErr(EE, "Invalid integer condition: '%s'\n", ln);

   if (Mcisnum(wp0->word[0]) || wp0->word[0] == '-')
   {
      *A1CONST = 1;
      if (sscanf(wp0->word, "%d", ia1) != 1)
         ExtErr(EE, "Invalid integer condition: '%s'\n", ln);
   }
   else if (mac1) sprintf(mac1, "@(%s)", wp0->word);
   else
   {
      *A1CONST = 1;
      mp = FindMac(wp0->word);
      if (!mp) ExtErr(EE, "Undefined macro in integer condition: '%s'\n", ln);
      if (sscanf(mp->Sub, "%d", ia1) != 1)
         ExtErr(EE, "Invalid macro in integer condition: '%s'\n", ln);
   }
   if (Mcisnum(wp1->word[0]) || wp1->word[0] == '-')
   {
      *A2CONST = 1;
      if (sscanf(wp1->word, "%d", ia2) != 1)
         ExtErr(EE, "Invalid integer condition: '%s'\n", ln);
   }
   else if (mac2) sprintf(mac2, "@(%s)", wp1->word);
   else
   {
      *A1CONST = 1;
      mp = FindMac(wp1->word);
      if (!mp) ExtErr(EE, "Undefined macro in integer condition: '%s'\n", ln);
      if (sscanf(mp->Sub, "%d", ia2) != 1)
         ExtErr(EE, "Invalid macro in integer condition: '%s'\n", ln);
   }
   KillWords(wp);
   return(comp);
}

int Getiarg(EXTENV *EE, int ARGCONST, int ival, char *mac)
{
   int i;
   char ln[LNLEN];

   if (ARGCONST) return(ival);
   Wstrcpy(ln, mac);
   MacroSub(EE, ln);
   if (sscanf(ln, "%d", &i) != 1)
      ExtErr(EE, "Invalid @iwhile macro: '%s'\n", mac);
   return(i);
}

int MIfCond(EXTENV *EE, char *ln)
/*
 * Expects ln of machandle1 [=,!,~] machandle2
 * where machandle is either or macro handle, or if it begins with ",
 * a string literal (white spaces have to come from @^, and there is no
 * matching closing ")
 * comparators:  = : equal;    ! : not equal;    ~ : is substring of
 */
{
   WORDS *wp;
   EXTMAC *mp1, *mp2;
   char *comp1, *comp2;
   int iret=0;

   wp = GetWords(ln);
   if (!wp || !wp->next || !wp->next->next)
      ExtErr(EE, "Invalid macro condition: '%s'\n", ln);

   if (wp->word[0] == '"') /* word is string literal, not macro */
      comp1 = wp->word + 1;
   else
   {
      mp1 = FindMac(wp->word);
      if (!mp1) ExtErr(EE, "Undefined macro in condition: '%s'\n", ln);
      comp1 = mp1->Sub;
   }
   if (wp->next->next->word[0] == '"') /* word is string literal, not macro */
      comp2 = wp->next->next->word + 1;
   else
   {
      mp2 = FindMac(wp->next->next->word);
      if (!mp2) ExtErr(EE, "Undefined macros in condition: '%s'\n", ln);
      comp2 = mp2->Sub;
   }

   if (wp->next->word[0] == '=') iret = Wstrcmp(comp2, comp1);
   else if (wp->next->word[0] == '!') iret = !Wstrcmp(comp2, comp1);
   else if (wp->next->word[0] == '~') iret = Wfnd_substr(comp2, comp1);
   else ExtErr(EE, "Invalid macro condition operator: '%s'\n", ln);

   KillWords(wp);
   return(iret);
}

void HandleMIf(EXTENV *EE, char *ln)
/*
 * Expects ln of @mif handle1 [<,>,=,!] handle2
 */
{
   if ( MIfCond(EE, ln+5) )
   {
      SETMODE(mode, MODE_MIF);
      modedepth[MCMIF]++;
   }
   else DumpSkip(EE, NULL, "@mif ", "@endmif ");
}

void HandleIIf(EXTENV *EE, char *ln)
/*
 * Expects ln of @iif int1 [<,>,=,!] int2
 */
{
   char ch;
   int i, j, ia1, ia2;
   ch = GetIntComp(EE, ln+5, &i, &j, &ia1, &ia2, NULL, NULL);
   if (ch == '=') i = (ia1 == ia2);
   else if (ch == '!') i = (ia1 != ia2);
   else if (ch == '<') i = (ia1 < ia2);
   if (!i) /* skip */
      DumpSkip(EE, NULL, "@iif ", "@endiif ");
   else
   {
      SETMODE(mode, MODE_IIF);
      modedepth[MCIIF]++;
   }
}

void HandleIwhile(EXTENV *EE, char *ln)
{
   int A1CONST, A2CONST, ia1, ia2, KeepOn;
   static int inest=0;
   FILE2 tfp, *OldFp;
   char mac1[HANLEN], mac2[HANLEN];
   char comp;

   inest++;
   comp = GetIntComp(EE, ln+8, &A1CONST, &A2CONST, &ia1, &ia2, mac1, mac2);

/*
 * Open temporary file, and dump while loop to it
 */
   sprintf(tfp.Fnam, "Iwhiletmpfile%d", inest);
   tfp.LineNo = 0;
   tfp.Fp = tmpfile(); /* fopen(tfp.Fnam, "w+"); */
   DumpSkip(EE, tfp.Fp, "@iwhile ", "@endiwhile ");

   OldFp = EE->FpIn;
   tfp.prev = OldFp;
   EE->FpIn = &tfp;

   ia1 = Getiarg(EE, A1CONST, ia1, mac1);
   ia2 = Getiarg(EE, A2CONST, ia2, mac2);
   if (comp == '<') KeepOn = (ia1 < ia2);
   else if (comp == '=') KeepOn = (ia1 == ia2);
   else if (comp == '!') KeepOn = (ia1 != ia2);
   while (KeepOn)
   {
      rewind(tfp.Fp);
      while( GetLn(EE, ln) ) HandleLine(EE, ln);
      ia1 = Getiarg(EE, A1CONST, ia1, mac1);
      ia2 = Getiarg(EE, A2CONST, ia2, mac2);
      if (comp == '<') KeepOn = (ia1 < ia2);
      else if (comp == '=') KeepOn = (ia1 == ia2);
      else if (comp == '!') KeepOn = (ia1 != ia2);
   }

   EE->FpIn = OldFp;
   fclose(tfp.Fp);
   inest--;
}



void ApplyFlags(EXTENV *EE, char *line)
{
   char tline[LNLEN], *flag;
   int i, j;
   INDENT *indptr;
   extern INDENT *indbase;

   flag = EE->Flags;

   if (flag[F_RemBlank]) Wremove_trailing_blanks(line);
   if (indbase)
   {
      Wtab2spcs(line, 8);
      indptr = indbase;
      do
      {
         j = indptr->start;
         if (indptr->nspac > 0)
         {
            Wstrcpy(tline, &line[j]);
            for (i=j; i < j + indptr->nspac; i++) line[i] = ' ';
            Wstrcpy(&line[i], tline);
         }
         else if (indptr->nspac < 0) Wstrcpy(&line[j+indptr->nspac], &line[j]);
         indptr = indptr->next;
      }
      while (indptr != indbase);
   }
   else if (flag[F_RepTab]) Wtab2spcs(line, flag[F_RepTab]);
   if (flag[F_Ugly]) MakeLnButtUgly(EE, line);
   else
   {
      if (flag[F_Case] == 1) Wsafe_supcase(line, LNLEN);
      else if (flag[F_Case] == 2) Wsafe_slowcase(line, LNLEN);
   }
   if (flag[F_Clint])
   {
      if (flag[F_Lang] == 'f')
         if (!( !Mciswspace(line[0]) && !Mcisnum(line[0]) ))
            Wsafe_slowcase(line, 72);
   }

   if (flag[F_LLWarn])
   {
      if ( (flag[F_LLWarn] > 1) || (flag[F_Lang] != 'f') ||
           Mciswspace(line[0]) || Mcisnum(line[0]) )
      {
         i = Wstrlen(line) - 1;
         while (Mciswspace(line[i])) i--;
         if (i > flag[F_LnLen])
            fprintf(Warn,
                    "\nWARNING: Line %d of file \'%s\' is %d characters long!\n",
                    EE->FpIn->LineNo, EE->FpIn->Fnam, i+1);
      }
   }
}

INDENT *AddIndent(EXTENV *EE, int start, int nspaces)
{
   INDENT *indptr;
   indptr = (INDENT *) malloc(sizeof(INDENT));
   if (indbase == NULL)
   {
      indbase = indptr;
      indbase->next = indbase;
      indbase->prev = indbase;
   }
   else
   {
      indptr->prev = indbase->prev;
      indptr->next = indbase;
      indbase->prev->next = indptr;
      indbase->prev = indptr;
   }
   indptr->start = start - 1;
   indptr->nspac = nspaces;
   return(indptr);
}

void KillIndent(EXTENV *EE, INDENT *indptr)
{
   INDENT *ip;

   if (indbase)
   {
      if (indptr == indbase) ip = indbase;
      else for(ip=indbase->next; ip != indptr && ip != indbase; ip = ip->next);
      if (ip != indptr) ExtErr(EE, "Error in indention freeing");

      ip->prev->next = ip->next;
      ip->next->prev = ip->prev;
      if (ip == indbase)
      {
         if (indbase->next == indbase) indbase = NULL;
         else indbase = indbase->next;
         free(ip);
      }
   }
   else ExtWarn(EE, "unmatched @ENDINDENT");
}

void PrintUsage()
{
   fprintf(Warn, "\nExtract v%s: Written by R. Clint Whaley.\n", version);
   fprintf(Warn, "Report bugs to: rwhaley@cs.utk.edu,\n");
   fprintf(Warn, "_after_ scoping the homepage: www.cs.utk.edu/~rwhaley/EXTRACT/Extract.html\n");
   fprintf(Warn, 
           "\nExtract3.0: Victor\'s Revenge -- bells, whistles, a gong, and\n");
   fprintf(Warn, "            a bag hanging off the side.\n");
   fprintf(Warn, 
           "\nExtract3.1: Andy Strikes Back -- bells, whistles, a gong, \n");
   fprintf(Warn, 
           "            a bag hanging off the side, and a toupee on top.\n");
   fprintf(Warn, 
           "\nExtract4.0: This Time, Its Personal -- bells, whistles, a gong, \n");
   fprintf(Warn, 
           "            a bag hanging off the side, a toupee on top, and\n");
   fprintf(Warn,
           "            chrome all around.\n");
   fprintf(Warn, "\nUser's guide and docs at:\n");
   fprintf(Warn, "\n   www.cs.utk.edu/~rwhaley/EXTRACT/Extract.html\n\n");
   fprintf(Warn, "\nUSAGE:");
   fprintf(Warn,
           "\n\nextract [-<flags>] [key1=match1, ...,  keyN=matchN]\n\n");
   fprintf(Warn,
           "Those items in [] are optional, while those in <> are to be\n");
   fprintf(Warn,"replaced by the appropriate string.\n");
   fprintf(Warn, 
"All flags have the option 0.  This means do not perform the\n");
   fprintf(Warn, 
"operation indicated by the flag.  For instance -case0 means do not change case.\n");
   fprintf(Warn, 
"This can be used to override the defaults setup by your ~/.extractrc file.\n");
   fprintf(Warn, "\nThe available flags are:\n");
   fprintf(Warn, 
      "  -o <outfile> : use file <outfile> for output rather than stdout.\n");
   fprintf(Warn,
      "  -b <basefile> : use file <basefile> for input rather than stdin.\n");
   fprintf(Warn, 
   "  -case[0,U,L]: change case of output file. U=upcase, L=lowcase.\n");
   fprintf(Warn, "  -RepTab[0][#]: replace tabs with # spaces.  # defaults to 8.\n");
   fprintf(Warn, 
           "  -Remtblank[0]: remove trailing blanks at the end of each line\n");
   fprintf(Warn, "  -LAPACK[0,1,2,3]: Make code look like LAPACK coding style.\n");
   fprintf(Warn, "  -verb[0,1,2,3]: Vary the verbosity of extract.\n");
   fprintf(Warn, "  -LnLen[0]#: set maximal number of columns for line length warning.\n");
   fprintf(Warn, "  -LLWarn[0,1,2]: Line length warnings: 0: none, 1: non-comment, 2: always\n");
   fprintf(Warn, 
      "  -fmode[0,Q,A]:  File mode.  0: default - overwrite if file exists;\n");
   fprintf(Warn, "                  Q: Query before overwriting, A: Append.\n");
   fprintf(Warn, "  -lang[0,f77,C,M] Language extracted file is.\n");
   fprintf(Warn, "  -addkeys[0]: Makes it so keys are always inherited.\n");
   fprintf(Warn, "  -punymac[0]: Makes it so macros in in-line extract files are automatically\n");
   fprintf(Warn, 
           "               popped before returning to the @extract line.\n");
   fprintf(Warn, "               NOTE: this flag is never inherited.\n");
   fprintf(Warn, 
   "  -punyflags[0]: Says that @extract being done should not inherit\n");
   fprintf(Warn, "                present flag settings.\n");
   fprintf(Warn, 
"   -localprocs[0]: If true, makes it so extract procedures and functions are\n");
   fprintf(Warn,
"                   undefined before returning to the calling @extract line\n");
   fprintf(Warn, "  -def <handle> \"<replacement>\".\n");
   fprintf(Warn, "  -indent <col> <nspaces>\n");
   fprintf(Warn, 
   "  -trans \"Ln#1, ..., Ln#N\": translate extracted line numbers to basefile\n");
   fprintf(Warn, "                            line numbers.\n");
   fprintf(Warn, 
     "  -no@<extract,skip,def,ifdef,whiledef,indent,key,abort,macsub,endext,print,exp,declare,addkey,echo,iwhile,proc,output,iif,mif,all>[0]:\n");
   fprintf(Warn, "     turn off basefile commands.\n");
   exit(1);
}

void ExtInit(EXTENV *EE, WORDS *wp)
{
   char *fin=NULL, *fout=NULL, *flags, *ec, *access;
   char line[LNLEN];
   int i, j, err;
   WORDS *p, *p2;

   EE->clindent = NULL;
   EE->MyPunyMacs = NULL;
   EE->MyProcs = NULL;
   EE->Joining = 1;
   p = wp;
   flags = EE->Flags;
   ec = EE->ExtCmndOn;
   access = "w";
   EE->KeyBase = NULL;

   while (p)
   {
      err = 0;
      Wsafe_slowcase(p->word, LNLEN);
/*
 *    parameter is flag
 */
      if (p->word[0] == '-')
      {
         if ( WstrcmpN(p->word+1, "multidef", 8) )
         {
            p2 = p = p->next;
            if (!p) ExtErr(NULL, "No handle for commandline macro definition");
            if (!p->next)
               ExtErr(NULL, "No substring for commandline macro definition");
            while (p->next)
            {
               if (p->next->word[0] == '-') break;
               sprintf(line, "@define %s @%s@\n",p2->word, p->next->word);
               PushMacro(EE, line);
               p = p->next;
            }
         }
         else if ( WstrcmpN(&p->word[1], "def", 3) )
         {
/*
 *          Push commandline macro onto stack
 */
            p = p->next;
            if (!p) ExtErr(NULL, "No handle for commandline macro definition");
            if (!p->next)
               ExtErr(NULL, "No substring for commandline macro definition");
            PushMacro2(EE, 1, p->word, p->next->word);
            p = p->next;
         }
         else if ( (p->word[1] == 'o') && (p->word[2] == '\0') )
         {
            p = p->next;
            if (p) fout = p->word;
         }
         else if ( (p->word[1] == 'b') && (p->word[2] == '\0') )
         {
            p = p->next;
            if (p) fin = p->word;
         }
         else if ( WstrcmpN(&p->word[1], "case", 4) )
         {
            if (p->word[5] == 'u') flags[F_Case] = 1;
            else if (p->word[5] == 'l') flags[F_Case] = 2;
            else if (p->word[5] == '0' || p->word[5] == '\0') flags[F_Case] = 0;
            else err = 1;
         }
         else if ( WstrcmpN(&p->word[1], "reptab", 6) )
         {
            if (p->word[7] == '\0') flags[F_RepTab] = 8;
            else 
            {
               sscanf(&p->word[7], "%d", &i);
               flags[F_RepTab] = i;
            }
         }
         else if ( WstrcmpN(&p->word[1], "remtblank", 9) )
         {
            flags[F_RemBlank] = (p->word[10] != '0');
         }
         else if ( WstrcmpN(&p->word[1], "lapack", 6) )
         {
            if (p->word[7] == '0') flags[F_Ugly] = 0;
            else if (p->word[7] == '2') flags[F_Ugly] = 2;
            else if (p->word[7] == '3') flags[F_Ugly] = 3;
            else 
            {
               flags[F_Ugly] = 1;
               flags[F_Case] = 1;
            }
         }
         else if ( WstrcmpN(&p->word[1], "verb", 4) )
         {
            if (p->word[5] == '1') flags[F_Verb] = 1;
            else if (p->word[5] == '2') flags[F_Verb] = 2;
            else if (p->word[5] == '3') flags[F_Verb] = 3;
            else flags[F_Verb] = 0;
         }
         else if ( WstrcmpN(&p->word[1], "lnlen", 5) )
         {
            sscanf(&p->word[6], "%d", &i);
            flags[F_LnLen] = i;
         }
         else if ( WstrcmpN(&p->word[1], "llwarn", 6) )
         {
            sscanf(&p->word[7], "%d", &i);
            flags[F_LLWarn] = i;
         }
         else if ( WstrcmpN(&p->word[1], "fmode", 5) )
         {
            if (p->word[6] == 'a') access = "a";
            else access = "w";
            flags[F_Query] = (p->word[6] == 'q');
         }
         else if ( WstrcmpN(&p->word[1], "lang", 4) )
         {
            flags[F_Lang] = 'f';
            if (p->word[5] == 'c') flags[F_Lang] = 'c';
            else if (p->word[5] == 'm') flags[F_Lang] = 'm';
            else if (p->word[5] == 'f')
               if (p->word[6] == '9') flags[F_Lang] = '9';
         }
         else if ( WstrcmpN(&p->word[1], "trans", 5) )
         {
            fprintf(Warn, "\nFlag \'-trans\' not yet implemented.\n");
         }
         else if ( WstrcmpN(&p->word[1], "addkeys", 7) )
         {
            if (p->word[8] == '0') flags[F_AddKeys] = 0;
            else flags[F_AddKeys] = 1;
         }
         else if ( WstrcmpN(&p->word[1], "localprocs", 10) )
         {
            if (p->word[11] == '0') flags[F_LocalProcs] = 0;
            else flags[F_LocalProcs] = 1;
         }
         else if ( WstrcmpN(&p->word[1], "punymac", 7) )
         {
            if (p->word[8] == '0') flags[F_FragMac] = 0;
            else flags[F_FragMac] = 1;
         }
         else if ( WstrcmpN(&p->word[1], "punyflags", 9) )
         {
            if (p->word[10] == '0') flags[F_FragFlag] = 0;
            else flags[F_FragFlag] = 1;
         }
         else if ( WstrcmpN(&p->word[1], "clint", 5) )
         {
            if (p->word[6] == '0') flags[F_Clint] = 0;
            else flags[F_Clint] = 1;
         }
         else if ( WstrcmpN(&p->word[1], "indent", 6) )
         {
            p = p->next;
            sscanf(p->word, "%d", &i);
            p = p->next;
            sscanf(p->word, "%d", &j);
            EE->clindent = AddIndent(EE, i, j);
         }
         else if ( WstrcmpN(&p->word[1], "help", 4) ) PrintUsage();
         else if ( WstrcmpN(&p->word[1], "no@", 3) )
         {
            if ( WstrcmpN(&p->word[4], "extract", 7) ) 
               ec[EC_Extract] = p->word[11] == '0';
            else if ( WstrcmpN(&p->word[4], "skip", 4) ) 
               ec[EC_Skip] = p->word[8] == '0';
            else if ( WstrcmpN(&p->word[4], "def", 3) ) 
               ec[EC_Define] = p->word[7] == '0';
            else if ( WstrcmpN(&p->word[4], "ifdef", 5) ) 
               ec[EC_Ifdef] = p->word[9] == '0';
            else if ( WstrcmpN(&p->word[4], "whiledef", 8) ) 
               ec[EC_While] = p->word[12] == '0';
            else if ( WstrcmpN(&p->word[4], "indent", 6) ) 
               ec[EC_Indent] = p->word[10] == '0';
            else if ( WstrcmpN(&p->word[4], "key", 3) ) 
               ec[EC_Key] = p->word[7] == '0';
            else if ( WstrcmpN(&p->word[4], "abort", 5) ) 
               ec[EC_Abort] = p->word[9] == '0';
            else if ( WstrcmpN(&p->word[4], "macsub", 6) ) 
               ec[EC_MacSub] = p->word[10] == '0';
            else if ( WstrcmpN(&p->word[4], "endext", 6) ) 
               ec[EC_EndExt] = p->word[10] == '0';
            else if ( WstrcmpN(&p->word[4], "print", 5) ) 
               ec[EC_EndExt] = p->word[9] == '0';
            else if ( WstrcmpN(&p->word[4], "exp", 3) ) 
               ec[EC_Exp] = p->word[7] == '0';
            else if ( WstrcmpN(&p->word[4], "declare", 7) ) 
               ec[EC_Dec] = p->word[7] == '0';
            else if ( WstrcmpN(&p->word[4], "addkey", 6) ) 
               ec[EC_AddKey] = p->word[6] == '0';
            else if ( WstrcmpN(&p->word[4], "echo", 4) ) 
               ec[EC_Echo] = p->word[4] == '0';
            else if ( WstrcmpN(&p->word[4], "iwhile", 6) ) 
               ec[EC_Iwhile] = p->word[6] == '0';
            else if ( WstrcmpN(&p->word[4], "output", 6) ) 
               ec[EC_Output] = p->word[6] == '0';
            else if ( WstrcmpN(&p->word[4], "iif", 3) ) 
               ec[EC_Iif] = p->word[6] == '0';
            else if ( WstrcmpN(&p->word[4], "mif", 3) ) 
               ec[EC_Mif] = p->word[6] == '0';
            else if ( WstrcmpN(&p->word[4], "all", 3) ) 
            {
               if (p->word[7] == '0') CharSet(EC_nExtCmnds, ec, 1);
               else
               {
                  CharSet(EC_nExtCmnds, ec, 0);
                  ec[EC_Output] = 1;
               }
            }
            else err = 1;
         }
         else err = 1;
         if (err) fprintf(Warn, "\n\nUnknown flag \'%s\' ignored.\n\n", p->word);
      }
/*
 *    Parameter is setting up keys
 */
      else AddKey(EE, p->word);
      p = p->next;
   }
   if (fin) EE->FpIn = OpenFile(EE, fin, "r");
   if (fout) EE->FpOut = OpenFile(EE, fout, access);
   KillWords(wp);
/*
 * Must set LnLen if we want warnings
 */
   if (flags[F_LLWarn])
   {
      if (!flags[F_LnLen])
      {
         if (flags[F_Lang] == 'f') flags[F_LnLen] = 71;
         else flags[F_LnLen] = 80;
      }
   }
/*
 * Never replace tabs in Makefiles
 */
   if (flags[F_Lang] == 'm') flags[F_RepTab] = 0;
}


void Extract(EXTENV *OldEnv, WORDS *wp)
{
   char line[LNLEN];
   EXTENV EE;
   PUNYMAC *pp;
   KEYS *kp;
   EXTPROC *fup;
   void KillProc(EXTENV *EE, EXTPROC *fpkill);

   EE.FpIn = NULL;
   EE.FpOut = NULL;
   if (OldEnv)
   {
      if (OldEnv->Flags[F_FragFlag]) CharSet(F_nFlags, EE.Flags, 0);
      else WstrcpyN((void *) EE.Flags, (void *) OldEnv->Flags, F_nFlags);
      EE.Flags[F_FragMac] = 0;
      WstrcpyN((void *) EE.ExtCmndOn, (void *) OldEnv->ExtCmndOn, EC_nExtCmnds);
      fflush(OldEnv->FpOut->Fp);  /* flush oldenv's outfile */
   }
   else
   {
      CharSet(F_nFlags, EE.Flags, 0);
      EE.Flags[F_Lang] = 'f';
      CharSet(EC_nExtCmnds, EE.ExtCmndOn, 1);
   }

/*
 * If no input/output or keys are specified, they are inherited
 */
   if (OldEnv) EE.FpIn = OldEnv->FpIn;
   if (OldEnv) EE.FpOut = OldEnv->FpOut;

   ExtInit(&EE, wp);

   if (EE.FpIn  == NULL) EE.FpIn  = OpenFile(&EE, "stdin", "r");
   if (EE.FpOut == NULL) EE.FpOut = OpenFile(&EE, "stdout", "w");
/*
 * Print 'extract starting' message, if desired
 */
   if (EE.Flags[F_Verb])
   {
      if (!OldEnv)
      {
         fprintf(Warn, "\nBegin commandline extract on files: in=\'%s\', out=\'%s\'.\n",
                 EE.FpIn->Fnam, EE.FpOut->Fnam);
      }
      else if (EE.Flags[F_Verb] > 1)
      {
         fprintf(Warn, "  Begin basefile extract on files: in=\'%s\', out=\'%s\'.\n",
                 EE.FpIn->Fnam, EE.FpOut->Fnam);
      }
   }

/*
 * If keys are inherited, they do not affect OldEnv's keys, so we must copy
 */
   if ( (!EE.KeyBase) || (EE.Flags[F_AddKeys]) ) CopyKeys(&EE, OldEnv);

/*
 * Store where my macros begin
 */
   sprintf(line, "@__MyMacBeg__%d", &EE);
   PushMacro2(&EE, 0, line, "");
   EE.MyMacBeg = MacroBase;

/*
 * Read in and handle the file
 */
   while( GetLn(&EE, line) )
   {
      HandleLine(&EE, line);
   }

   ExtDone = 0;
/*
 * Print 'extract done' message, if desired
 */
   if (EE.Flags[F_Verb])
   {
      if (!OldEnv)
      {
         fprintf(Warn, 
"Done  commandline extract. Files: in=\'%s\', out=\'%s\'; Lines: in=%d, out=%d.\n",
            EE.FpIn->Fnam, EE.FpOut->Fnam, EE.FpIn->LineNo, EE.FpOut->LineNo);
      }
      else if (EE.Flags[F_Verb] > 1)
      {
         fprintf(Warn, 
"  Done  basefile extract. Files: in=\'%s\', out=\'%s\'; Lines: in=%d, out=%d.\n",
            EE.FpIn->Fnam, EE.FpOut->Fnam, EE.FpIn->LineNo, EE.FpOut->LineNo);
      }
   }

/*
 * Close files
 */
   if (OldEnv == NULL) CloseFile(&EE, EE.FpIn);
   else if (EE.FpIn != OldEnv->FpIn) CloseFile(&EE, EE.FpIn);
   if (OldEnv == NULL) CloseFile(&EE, EE.FpOut);
   else if (EE.FpOut != OldEnv->FpOut) CloseFile(&EE, EE.FpOut);

/*
 * Undefine puny macros
 */
   while(pp = EE.MyPunyMacs)
   {
      EE.MyPunyMacs = pp->next;
      PopThisMacro(&EE, pp->mp);
      free(pp);
   }
/*
 * If Fragile macros are set, free all macros defined by this extract session
 */
   if (EE.Flags[F_FragMac]) KillMyMacros(&EE);
/*
 * Pop MyMacBeg
 */
   sprintf(line, "__MyMagBeg__%d", &EE);
   PopMacro2(&EE, line);

/*
 * Free my keys
 */
   while(kp = EE.KeyBase) KillThisKey(&EE, EE.KeyBase);
/*
 * Free my procedures
 */
   if (EE.MyProcs)
   {
      while (AllProcs != EE.MyProcs)
      {
         fup = AllProcs->next;
         KillProc(&EE, AllProcs);
         AllProcs = fup;
      }
      fup = AllProcs->next;
      KillProc(&EE, AllProcs);
      AllProcs = fup;
   }
   if (EE.clindent) KillIndent(&EE, EE.clindent);
}

/*
 *  ==================================================================
 *                     Extract procedures and functions
 *  ==================================================================
 */
void KillProc0(EXTENV *EE, EXTPROC **basep, EXTPROC **myprocs, EXTPROC *ppkill)
{
   EXTPROC *fp;
   if (ppkill)
   {
      free(ppkill->ProcNam);
      free(ppkill->FileNam);
      KillWords(ppkill->argnams);
      if (ppkill == *basep)
      {
         *basep = (*basep)->next;
         if (ppkill == *myprocs) *myprocs = NULL;
      }
      else
      {
         for (fp=(*basep); fp->next != ppkill ; fp = fp->next);
         if (ppkill == *myprocs) *myprocs = fp;
         fp->next = ppkill->next;
      }
      free(ppkill);
   }
}

void KillProc(EXTENV *EE, EXTPROC *fpkill)
{
   KillProc0(EE, &AllProcs, &(EE->MyProcs), fpkill);
}

void KillFunc(EXTENV *EE, EXTPROC *fpkill)
{
   KillProc0(EE, &AllFuncs, &(EE->MyFuncs), fpkill);
}

EXTPROC *FindProc(EXTENV *EE, EXTPROC *basep, char *proc)
{
   EXTPROC *pp;
   for (pp=basep; pp; pp = pp->next)
      if (Wstrcmp(proc, pp->ProcNam)) break;
   return(pp);
}

void PopProc(EXTENV *EE, char *proc)
{
   EXTPROC *pp;
   if (AllProcs)
   {
      pp = FindProc(EE, AllProcs, proc);
      if (pp) KillProc(EE, pp);
      else ExtWarn(EE, "Nonsensical @UNDEFPROC:  %s", proc);
   }
   else ExtWarn(EE, "Nonsensical @UNDEFPROC:  %s", proc);
}

void PopFunc(EXTENV *EE, char *func)
{
   EXTPROC *pp;
   if (AllFuncs)
   {
      pp = FindProc(EE, AllFuncs, func);
      if (pp) KillFunc(EE, pp);
      else ExtWarn(EE, "Nonsensical @UNDEFFUNC:  %s", func);
   }
   else ExtWarn(EE, "Nonsensical @UNDEFFUNC:  %s", func);
}

void AddProc0(EXTENV *EE, EXTPROC **basep, EXTPROC **myprocs, EXTPROC *fpadd)
{
   if (*myprocs == NULL) *myprocs = fpadd;
   fpadd->next = *basep;
   *basep = fpadd;
}

void PushProc0(EXTENV *EE, EXTPROC **basep, EXTPROC **myfuncs, char *ln)
/*
 * expects ln of form "<procnam> [arg1 ... argN]"
 */
{
   EXTPROC *pp;
   WORDS *wp, *wpbase;
   FILE *fp;
   char *cp;
   int i;

   pp = malloc(sizeof(EXTPROC));
   wpbase = wp = GetWords(ln);
   if (!wpbase)
   {
      if (*basep == AllProcs)
         ExtErr(EE, "@BEGINPROC must have a PROCNAME");
      else ExtErr(EE, "@BEGINFUNC must have a FUNCNAME");
   }

   i = Wstrlen(wp->word) + 1;
   pp->ProcNam = malloc(i);
   Wstrcpy(pp->ProcNam, wp->word);

   pp->argnams = KillWord(wp, wp);
   pp->nargs = CountWords(pp->argnams);

   cp = tmpnam(NULL);
   if (cp == NULL) ExtErr(EE, "Out of tmpnams!!!");
   i = Wstrlen(cp) + 1;
   pp->FileNam = malloc(i*sizeof(char));
   Wstrcpy(pp->FileNam, cp);
/*
 * Dump procedure to tmpfile
 */
   fp = fopen(pp->FileNam, "w");
   if (*basep == AllProcs) DumpSkip(EE, fp, "@beginproc ", "@endproc ");
   else DumpSkip(EE, fp, "@beginfunc ", "@endfunc ");
   fclose(fp);

   AddProc0(EE, basep, myfuncs, pp);
}

void PushProc(EXTENV *EE, char *ln)
/*
 * expects ln of form "@beginproc <procnam> [arg1 ... argN]"
 */
{
   PushProc0(EE, &AllProcs, &(EE->MyProcs), ln+11);
}

void PushFunc(EXTENV *EE, char *ln)
/*
 * expects ln of form "@beginfunc <procnam> [arg1 ... argN]"
 */
{
   PushProc0(EE, &AllFuncs, &(EE->MyFuncs), ln+11);
}

void HandleProcCall0(EXTENV *EE, EXTPROC *basep, char *ln)
/*
 * expects ln of form "<proc/func> [arg1 ... argN]"
 */
{
   WORDS *wbase, *wp, *ew, *ap, *dp;
   EXTPROC *pp;

   wbase = wp = GetWords(ln);
   pp = FindProc(EE, basep, wp->word);
   if (!pp) ExtErr(EE, "Call to undefined proc/func %s", wp->word);
   ew = GetWord("-b");
   ew->next = GetWord(pp->FileNam);
   wp = ew->next;
   if (EE->Flags[F_LocalProcs] || basep == AllFuncs)
   {
     wp->next = GetWord("-punymac");
     wp = wp->next;
     if (basep == AllFuncs)
     {
        wp->next = GetWord("-no@output");
        wp = wp->next;
     }
   }
   for (dp=wbase->next, ap = pp->argnams; ap; ap = ap->next, dp = dp->next)
   {
      wp->next = GetWord("-def");
      wp = wp->next;
      wp->next = GetWord(ap->word);
      wp = wp->next;
      wp->next = GetWord(dp->word);
      wp = wp->next;
   }
   KillWords(wbase);
   Extract(EE, ew);
}

void HandleProcCall(EXTENV *EE, char *ln)
/*
 * expects ln of form "@callproc <procnam> [arg1 ... argN]"
 */
{
   HandleProcCall0(EE, AllProcs, ln+10);
}

int LnIsExtCmnd(EXTENV *EE, char *line)
{
   int icalc(EXTENV *EE, char line[]);
   int i, j, k, KeepOn, DONE=1;
   char tline[LNLEN], *Use;
   INDENT *indptr;
   WORDS *wp, *wp0;
   KEYS *Key;

   Use = EE->ExtCmndOn;
/*
 * If first non-whitespace is not an @, line is not extract command
 */
   for (i=0; Mciswspace(line[i]); i++);
   if (line[i] != '@') return(0);

   i = commandcpy(tline, &line[i]) + 1;

   switch(i)
   {
   case 5:
      if (WstrcmpN(tline, "@iif ", 4))
      {
         if ( !Use[EC_Iif] ) return(0);
         HandleIIf(EE, tline);
      }
      else if (WstrcmpN(tline, "@mif ", 4))
      {
         if ( !Use[EC_Mif] ) return(0);
         HandleMIf(EE, tline);
      }
      else DONE = 0;
      break;
   case  6:
      if (WstrcmpN(tline, "@skip ", 6))
      {
         if ( !Use[EC_Skip] ) return(0);
      }
      else if (WstrcmpN(tline, "@echo ", 6))
      {
         if (Use[EC_Echo])
         {
            for (k=0; line[k] != '@'; k++);
            k += 6;
            PutLn(EE, line+k);
         }
         else return(0);
      }
      else if (WstrcmpN(tline, "@iexp ", 6))
      {
         if ( !Use[EC_Exp] ) return(0);
         for (i=6; Mciswspace(tline[i]); i++);
         j=0;
         while(!Mciswspace(tline[i])) line[j++] = tline[i++]; /* get mac handle */
         line[j] = '\0';
         keycpy(tline, &tline[i]);
         i = icalc(EE, tline);
         PopMacro2(EE, line); /* replace any previous definition */
         sprintf(tline, "%d", i);
         PushMacro2(EE, EE->Flags[F_FragMac], line, tline);
      }
      else DONE = 0;
      break;
   case  7:
      if (WstrcmpN(tline, "@undef ", 7))
      {
         if ( Use[EC_Define] ) 
         {
            if ( !PopMacro(EE, tline) )
               ExtWarn(EE, "Nonsensical @UNDEF:  %s", line);
         }
         else return(0);
      }
      else if (WstrcmpN(tline, "@ifdef ", 7))
      {
         if (!Use[EC_Ifdef]) return(0);
         HandleIfdef(EE, tline);
      }
      else if (WstrcmpN(tline, "@abort ", 7))
      {
         if (Use[EC_Abort])
         {
            CloseFile(EE, EE->FpIn);
            CloseFile(EE, EE->FpOut);
            ExtErr(EE, "Extract aborted with message `%s`", &tline[7]);
            exit(1);
         }
         else return(0);
      }
      else if (WstrcmpN(tline, "@print ", 7))
      {
         if (Use[EC_Print]) fprintf(Warn, "%s", &tline[7]);
         else return(0);
      }
      else DONE = 0;
      break;
   case  8:
      if (WstrcmpN(tline, "@define ", 8))
      {
         if ( Use[EC_Define] ) PushMacro(EE, tline);
         else return(0);
      }
      else if (WstrcmpN(tline, "@system ", 8))
      {
         i = Wstrcpy(line, &tline[9]);
         while (line[i] != '\"') i--;
         line[i++] = '\n';
         line[i++] = '\0';
         system(line);
      }
      else if (WstrcmpN(tline, "@iwhile ", 8))
      {
         if (!Use[EC_Iwhile]) return(0);
         HandleIwhile(EE, tline);
      }
      else if (WstrcmpN(tline, "@endmif ", 8))
      {
         if ( !Use[EC_Mif] ) return(0);
         if (INMODE(mode, MODE_MIF))
         {
            if (modedepth[MCMIF] > 0)
            {
               modedepth[MCMIF]--;
               if (!(modedepth[MCMIF])) KILLMODE(mode, MODE_MIF);
            }
            else ExtErr(EE, "Internal mode error:  `%s`", tline);
         }
         else ExtWarn(EE, "Nonsensical @ENDMIF");
      }
      else if (WstrcmpN(tline, "@endiif ", 8))
      {
         if ( !Use[EC_Iif] ) return(0);
         if (INMODE(mode, MODE_IIF))
         {
            if (modedepth[MCIIF] > 0)
            {
               modedepth[MCIIF]--;
               if (!(modedepth[MCIIF])) KILLMODE(mode, MODE_IIF);
            }
            else ExtErr(EE, "Internal mode error:  `%s`", tline);
         }
         else ExtWarn(EE, "Nonsensical @ENDIIF");
      }
      else DONE = 0;
      break;
   case  9:
      if (WstrcmpN(tline, "@extract ", 9))
      {
         if ( Use[EC_Extract] ) 
         {
            Wstrcpy(tline, &tline[9]);
            wp = GetArgs(EE, tline);
            Extract(EE, wp);
         }
         else return(0);
      }
      else if (WstrcmpN(tline, "@declare ", 9))
      {
         if (!Use[EC_Dec]) return(0);
         HandleDec(EE, tline);
      }
      else if (WstrcmpN(tline, "@addkeys ",9))
      {
         if ( Use[EC_AddKey] )
         {
            wp0 = wp = GetWords(&tline[9]);
            while (wp)
            {
               AddKey(EE, wp->word);
               wp = wp->next;
            }
            KillWords(wp0);
         }
         else return(0);
      }
      else if (WstrcmpN(tline, "@endskip ", 9))
      {
         if ( Use[EC_Skip] ) ExtWarn(EE, "unmatched @ENDSKIP");
         else return(0);
      }
      else if (WstrcmpN(tline, "@endproc ", 9))
      {
         if ( Use[EC_Proc] ) ExtWarn(EE, "unmatched @ENDPROC");
         else return(0);
      }
      else DONE = 0;
      break;
   case 10:
      if (WstrcmpN(tline, "@whiledef ", 10))
      {
         if (!Use[EC_While]) return(0);
         HandleWhile(EE, tline);
      }
      else if (WstrcmpN(tline, "@multidef ", 10))
      {
         if (!Use[EC_Define]) return(0);
         HandleMultidef(EE, tline);
      }
      else if (WstrcmpN(tline, "@callproc ", 10))
      {
         if (!Use[EC_Proc]) return(0);
         HandleProcCall(EE, tline);
      }
      else if (WstrcmpN(tline, "@killkeys ", 10))
      {
         if ( Use[EC_AddKey] )
         {
            wp0 = wp = GetWords(&tline[10]);
            while (wp)
            {
               KillKey(EE, wp->word);
               wp = wp->next;
            }
            KillWords(wp0);
         }
         else return(0);
      }
      else if (WstrcmpN(tline, "@endwhile ", 10))
      {
         if ( Use[EC_While] ) ExtWarn(EE, "unmatched @ENDWHILE");
         else return(0);
      }
      else if (WstrcmpN(tline, "@endifdef ", 10))
      {
         if ( Use[EC_Ifdef] ) ExtWarn(EE, "unmatched @ENDIFDEF");
         else return(0);
      }
      else if (WstrcmpN(tline, "@undefall ", 10))
      {
         if ( Use[EC_Define] )
         {
            keycpy(tline, &tline[10]);
/*
 *          If undefining all of one handle
 */
            if (tline[0])
            {
               Wstrcpy(line, "@undef ");
               i = 10;
               for (i=10; ( !Mciswspace(tline[i]) && tline[i] ); i++)
                  line[i-3] = tline[i];
               line[i-3] = '\0';
               while( PopMacro(EE, line) );
            }
/*
 *          If undefining all handles
 */
            else KillMyMacros(EE);
         }
         else return(0);
      }
      else DONE = 0;
      break;
   case 11:
      if (WstrcmpN(tline, "@beginskip ", 11))
      {
         if ( Use[EC_Skip] ) DumpSkip(EE, NULL, "@beginskip ", "@endskip ");
         else return(0);
      }
      else if (WstrcmpN(tline, "@endindent ", 11))
      {
         if ( Use[EC_Indent] ) KillIndent(EE, indbase->prev);
         else return(0);
      }
      else if (WstrcmpN(tline, "@beginproc ", 11))
      {
         if (Use[EC_Proc]) PushProc(EE, tline);
         else return(0);
      }
      else if (WstrcmpN(tline, "@endiwhile ", 11))
      {
         if ( Use[EC_Iwhile] ) ExtWarn(EE, "unmatched @ENDIWHILE");
         else return(0);
      }
      else DONE = 0;
      break;
   case 12:
      if (WstrcmpN(tline, "@enddeclare ", 12)) 
      {
         if ( Use[EC_Dec] ) ExtWarn(EE, "unmatched @enddeclare");
         else return(0);
      }
      else if (WstrcmpN(tline, "@endextract ", 12))
      {
         if ( Use[EC_EndExt] ) ExtDone = 1;
         else return(0);
      }
      else DONE = 0;
      break;
   case 13:
      if (WstrcmpN(tline, "@beginindent ", 13))
      {
         if ( Use[EC_Indent] )
         {
            keycpy(tline, line);
            sscanf(&tline[13], "%d", &j);
            for (i=13; Mcisnum(tline[i]); i++);
            sscanf(&tline[++i], "%d", &k);
            AddIndent(EE, j, k);
         }
         else return(0);
      }
      else DONE = 0;
      break;
   default:
      DONE = 0;
      break;
   }
/* 
 * Must be @<keyhandle>, a line continuation (@\) or an unassociated @ sign
 */
   if (!DONE)
   {
      if (Use[EC_Key])
      {
         keycpy(tline, line);
         Key = IsKeyLn(EE, tline);
         if (Key) HandleKeyLn(EE, tline, Key);
         else 
         {
            if (tline[1] != '\\') /* don't warn about line continuation */
            {
               if ( !(tline[1] == '(' && tline[2] == '@' && tline[3] == ')') )
                  ExtWarn(EE, "Non-associated @");
            }
            return(0);
         }
      }
      else return(0);
   }
   return(1);
}

void HandleLine(EXTENV *EE, char *line)
{
/*
 * If line is not an extract command, apply flags and write to file
 */
   if (EE->ExtCmndOn[EC_MacSub]) MacroSub(EE, line);
   if ( !LnIsExtCmnd(EE, line) )
   {
      if (EE->ExtCmndOn[EC_Output]) PutLn(EE, line);
      else ExtErr(EE, "Output in prohibited mode: %s", line);
   }
}


main(int nargs, char *args[])
{
   char line[LNLEN], *path;
   int i;
   FILE *fp;
   WORDS *wp, *wp2=NULL, *wp3;

   Warn = stderr;
   path = getenv("HOME");
   i = Wstrcpy(line, path);
   Wstrcpy(&line[i], "/.extractrc");
   fp = fopen(line, "r"); 
   if (fp)
   {
      if ( fgets(line, LNLEN, fp) ) wp2 = GetWords(line);
      fclose(fp);
   }
   if (nargs > 1)
   {
      wp3 = wp = GetWord(args[1]);
      for (i=2; i < nargs; i++)
      {
         wp->next = GetWord(args[i]);
         wp = wp->next;
      }
      wp = wp3;
      if (wp2)  /* add commandline words to end of  .extractrc words */
      {
         for (wp3=wp2; wp3->next; wp3 = wp3->next);
         wp3->next = wp;
         wp = wp2;
      }
   }
   else if (fp == NULL) wp = GetWord("-help");
   else wp = wp2;
   Extract(NULL, wp);
   exit(0);
}
/************************************************************************/
/*  This program is distributed under the terms of the Gnu              */
/*  General Public License (GPL), with the following two exceptions:    */
/*  (1) Clause (9), dealing with updating the GPL automatically, is     */
/*      specifically disallowed by the author.  The author will         */
/*      determine if a newer GPL version is still appropriate.          */
/*  (2) The basefiles extract accepts as input, and the extracted       */
/*      files it produces as output, are specifically designated as     */
/*      as outside the scope if this license (i.e. they are *not*       */
/*      required by this license to be GPL).                            */
/*  The full, unaltered, text of the GPL is included below.             */
/************************************************************************/

/*
                             GNU GENERAL PUBLIC LICENSE

                                Version 2, June 1991

        Copyright (C) 1989, 1991 Free Software Foundation, Inc. 675 Mass
        Ave, Cambridge, MA 02139, USA. Everyone is permitted to copy and
        distribute verbatim copies of this license document, but changing it
        is not allowed.

                                      Preamble

        The licenses for most software are designed to take away your
        freedom to share and change it. By contrast, the GNU General Public
        License is intended to guarantee your freedom to share and change
        free software--to make sure the software is free for all its users.
        This General Public License applies to most of the Free Software
        Foundation's software and to any other program whose authors commit
        to using it. (Some other Free Software Foundation software is
        covered by the GNU Library General Public License instead.) You can
        apply it to your programs, too.

        When we speak of free software, we are referring to freedom, not
        price. Our General Public Licenses are designed to make sure that
        you have the freedom to distribute copies of free software (and
        charge for this service if you wish), that you receive source code
        or can get it if you want it, that you can change the software or
        use pieces of it in new free programs; and that you know you can do
        these things.

        To protect your rights, we need to make restrictions that forbid
        anyone to deny you these rights or to ask you to surrender the
        rights. These restrictions translate to certain responsibilities for
        you if you distribute copies of the software, or if you modify it.

        For example, if you distribute copies of such a program, whether
        gratis or for a fee, you must give the recipients all the rights
        that you have. You must make sure that they, too, receive or can get
        the source code. And you must show them these terms so they know
        their rights.

        We protect your rights with two steps: (1) copyright the software,
        and (2) offer you this license which gives you legal permission to
        copy, distribute and/or modify the software.

        Also, for each author's protection and ours, we want to make certain
        that everyone understands that there is no warranty for this free
        software. If the software is modified by someone else and passed on,
        we want its recipients to know that what they have is not the
        original, so that any problems introduced by others will not reflect
        on the original authors' reputations.

        Finally, any free program is threatened constantly by software
        patents. We wish to avoid the danger that redistributors of a free
        program will individually obtain patent licenses, in effect making
        the program proprietary. To prevent this, we have made it clear that
        any patent must be licensed for everyone's free use or not licensed
        at all.

        The precise terms and conditions for copying, distribution and
        modification follow.

                             GNU GENERAL PUBLIC LICENSE
          TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

        0. This License applies to any program or other work which contains
        a notice placed by the copyright holder saying it may be distributed
        under the terms of this General Public License. The "Program",
        below, refers to any such program or work, and a "work based on the
        Program" means either the Program or any derivative work under
        copyright law: that is to say, a work containing the Program or a
        portion of it, either verbatim or with modifications and/or
        translated into another language. (Hereinafter, translation is
        included without limitation in the term "modification".) Each
        licensee is addressed as "you".

        Activities other than copying, distribution and modification are not
        covered by this License; they are outside its scope. The act of
        running the Program is not restricted, and the output from the
        Program is covered only if its contents constitute a work based on
        the Program (independent of having been made by running the
        Program). Whether that is true depends on what the Program does.

        1. You may copy and distribute verbatim copies of the Program's
        source code as you receive it, in any medium, provided that you
        conspicuously and appropriately publish on each copy an appropriate
        copyright notice and disclaimer of warranty; keep intact all the
        notices that refer to this License and to the absence of any
        warranty; and give any other recipients of the Program a copy of
        this License along with the Program.

        You may charge a fee for the physical act of transferring a copy,
        and you may at your option offer warranty protection in exchange for
        a fee.

        2. You may modify your copy or copies of the Program or any portion
        of it, thus forming a work based on the Program, and copy and
        distribute such modifications or work under the terms of Section 1
        above, provided that you also meet all of these conditions:

        a) You must cause the modified files to carry prominent notices
        stating that you changed the files and the date of any change.

        b) You must cause any work that you distribute or publish, that in
        whole or in part contains or is derived from the Program or any part
        thereof, to be licensed as a whole at no charge to all third parties
        under the terms of this License.

        c) If the modified program normally reads commands interactively
        when run, you must cause it, when started running for such
        interactive use in the most ordinary way, to print or display an
        announcement including an appropriate copyright notice and a notice
        that there is no warranty (or else, saying that you provide a
        warranty) and that users may redistribute the program under these
        conditions, and telling the user how to view a copy of this License.
        (Exception: if the Program itself is interactive but does not
        normally print such an announcement, your work based on the Program
        is not required to print an announcement.)

        These requirements apply to the modified work as a whole. If
        identifiable sections of that work are not derived from the Program,
        and can be reasonably considered independent and separate works in
        themselves, then this License, and its terms, do not apply to those
        sections when you distribute them as separate works. But when you
        distribute the same sections as part of a whole which is a work
        based on the Program, the distribution of the whole must be on the
        terms of this License, whose permissions for other licensees extend
        to the entire whole, and thus to each and every part regardless of
        who wrote it.

        Thus, it is not the intent of this section to claim rights or
        contest your rights to work written entirely by you; rather, the
        intent is to exercise the right to control the distribution of
        derivative or collective works based on the Program.

        In addition, mere aggregation of another work not based on the
        Program with the Program (or with a work based on the Program) on a
        volume of a storage or distribution medium does not bring the other
        work under the scope of this License.

        3. You may copy and distribute the Program (or a work based on it,
        under Section 2) in object code or executable form under the terms
        of Sections 1 and 2 above provided that you also do one of the
        following:

        a) Accompany it with the complete corresponding machine-readable
        source code, which must be distributed under the terms of Sections 1
        and 2 above on a medium customarily used for software interchange;
        or,

        b) Accompany it with a written offer, valid for at least three
        years, to give any third party, for a charge no more than your cost
        of physically performing source distribution, a complete
        machine-readable copy of the corresponding source code, to be
        distributed under the terms of Sections 1 and 2 above on a medium
        customarily used for software interchange; or,

        c) Accompany it with the information you received as to the offer to
        distribute corresponding source code. (This alternative is allowed
        only for noncommercial distribution and only if you received the
        program in object code or executable form with such an offer, in
        accord with Subsection b above.)

        The source code for a work means the preferred form of the work for
        making modifications to it. For an executable work, complete source
        code means all the source code for all modules it contains, plus any
        associated interface definition files, plus the scripts used to
        control compilation and installation of the executable. However, as
        a special exception, the source code distributed need not include
        anything that is normally distributed (in either source or binary
        form) with the major components (compiler, kernel, and so on) of the
        operating system on which the executable runs, unless that component
        itself accompanies the executable.

        If distribution of executable or object code is made by offering
        access to copy from a designated place, then offering equivalent
        access to copy the source code from the same place counts as
        distribution of the source code, even though third parties are not
        compelled to copy the source along with the object code.

        4. You may not copy, modify, sublicense, or distribute the Program
        except as expressly provided under this License. Any attempt
        otherwise to copy, modify, sublicense or distribute the Program is
        void, and will automatically terminate your rights under this
        License. However, parties who have received copies, or rights, from
        you under this License will not have their licenses terminated so
        long as such parties remain in full compliance.

        5. You are not required to accept this License, since you have not
        signed it. However, nothing else grants you permission to modify or
        distribute the Program or its derivative works. These actions are
        prohibited by law if you do not accept this License. Therefore, by
        modifying or distributing the Program (or any work based on the
        Program), you indicate your acceptance of this License to do so, and
        all its terms and conditions for copying, distributing or modifying
        the Program or works based on it.

        6. Each time you redistribute the Program (or any work based on the
        Program), the recipient automatically receives a license from the
        original licensor to copy, distribute or modify the Program subject
        to these terms and conditions. You may not impose any further
        restrictions on the recipients' exercise of the rights granted
        herein. You are not responsible for enforcing compliance by third
        parties to this License.

        7. If, as a consequence of a court judgment or allegation of patent
        infringement or for any other reason (not limited to patent issues),
        conditions are imposed on you (whether by court order, agreement or
        otherwise) that contradict the conditions of this License, they do
        not excuse you from the conditions of this License. If you cannot
        distribute so as to satisfy simultaneously your obligations under
        this License and any other pertinent obligations, then as a
        consequence you may not distribute the Program at all. For example,
        if a patent license would not permit royalty-free redistribution of
        the Program by all those who receive copies directly or indirectly
        through you, then the only way you could satisfy both it and this
        License would be to refrain entirely from distribution of the
        Program.

        If any portion of this section is held invalid or unenforceable
        under any particular circumstance, the balance of the section is
        intended to apply and the section as a whole is intended to apply in
        other circumstances.

        It is not the purpose of this section to induce you to infringe any
        patents or other property right claims or to contest validity of any
        such claims; this section has the sole purpose of protecting the
        integrity of the free software distribution system, which is
        implemented by public license practices. Many people have made
        generous contributions to the wide range of software distributed
        through that system in reliance on consistent application of that
        system; it is up to the author/donor to decide if he or she is
        willing to distribute software through any other system and a
        licensee cannot impose that choice.

        This section is intended to make thoroughly clear what is believed
        to be a consequence of the rest of this License.

        8. If the distribution and/or use of the Program is restricted in
        certain countries either by patents or by copyrighted interfaces,
        the original copyright holder who places the Program under this
        License may add an explicit geographical distribution limitation
        excluding those countries, so that distribution is permitted only in
        or among countries not thus excluded. In such case, this License
        incorporates the limitation as if written in the body of this
        License.

        9. The Free Software Foundation may publish revised and/or new
        versions of the General Public License from time to time. Such new
        versions will be similar in spirit to the present version, but may
        differ in detail to address new problems or concerns.

        Each version is given a distinguishing version number. If the
        Program specifies a version number of this License which applies to
        it and "any later version", you have the option of following the
        terms and conditions either of that version or of any later version
        published by the Free Software Foundation. If the Program does not
        specify a version number of this License, you may choose any version
        ever published by the Free Software Foundation.

        10. If you wish to incorporate parts of the Program into other free
        programs whose distribution conditions are different, write to the
        author to ask for permission. For software which is copyrighted by
        the Free Software Foundation, write to the Free Software Foundation;
        we sometimes make exceptions for this. Our decision will be guided
        by the two goals of preserving the free status of all derivatives of
        our free software and of promoting the sharing and reuse of software
        generally.

                                    NO WARRANTY

        11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO
        WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
        EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
        OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY
        KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
        THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
        PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
        PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
        DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
        CORRECTION.

        12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
        WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
        AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU
        FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
        CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
        PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
        RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
        FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF
        SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
        SUCH DAMAGES.

                            END OF TERMS AND CONDITIONS
*/
