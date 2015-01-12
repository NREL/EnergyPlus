#ifndef ATLCONF_MISC_H
   #define ATLCONF_MISC_H

#include "atlas_sys.h"
char *ATL_fgetln(FILE *fpin);
void GetDate(int *month, int *day, int *year, int *hour, int *min);
long GetInt(FILE *fpin, long Default, char *spc, char *expstr);
long GetIntRange(long Default, long Min, long Max, char *spc, char *expstr);
long GetIntVer(long Default, long Min, long Max, char *spc, char *expstr);
void GetString(FILE *fpin, char *Default, char *spc, char *expstr,
               int len, char *str0);
void GetStrVer(char *def, char *spc, char *expstr, int len, char *str);
int IsYes(char def, char *spc, char *expstr);
char GetChar(char def, char *spc, char *expstr);
int FileIsThere(char *nam);
void ATL_mprintf(int np, ...);
int GetFirstInt(char *ln);
int GetFirstHex(char *ln);
long long GetFirstLong(char *ln);
long long GetFirstLongHex(char *ln);
long long GetLastLongWithRound(char *ln);
double GetFirstDouble(char *ln);
int GetLastInt(char *ln);
int GetLastHex(char *ln);
long long GetLastLong(char *ln);
long long GetLastLongHex(char *ln);
int fNumLines(char *fnam);
char *GetPathEnvVar(void);
int GetIntBeforeWord(char *word, char *ln);
int GetScreenHeight();
void GetEnter(FILE *fpout);
int DisplayFile(char *fnam, FILE *fpout, int nlines);
int DisplayFile0(char *fnam, FILE *fpout);
int FoundInFile(char *fnam, char *str);
char *FindUname(char *targ);
enum ARCHFAM ProbeArchFam(char *targ);
void KillUselessSpace(char *str);
int IsBitSetInField(int *field, int bit);
void SetBitInField(int *field, int bit);
void KillUselessSpace(char *str);
char *NameWithoutPath(char *file);
char *GetPathWithoutName(char *file);
void GetGccVers(char *gcc, int *comp, int *major, int *minor, int *patch);
int CompIsGcc(char *comp);
int CompIsAppleGcc(char *comp);
int CompIsMIPSpro(char *comp);
int CompIsPathScale(char *comp);
int CompIsSunWorkshop(char *comp);
int CompIsIBMXL(char *comp);
char *NewStringCopy(char *old);
char *NewAppendedString0(char *old, char *app);
char *NewAppendedString(char *old, char *app);
char **GetLinesFromFile(FILE *fpin, char **curlns); /* append lines in fnam */
void KillAllStringsInList(char **strs);
void PrintAllStringsInList(char *exp, char **strs);
char *FreeListGetString(char **strs, int n);
char **NewOneStringList(char **strs, int n);

#define syschk(ln_) \
{ \
   int ierr; \
   ierr = system(ln_); \
   if (ierr) \
   { \
      fprintf(stderr, "ERROR %d IN SYSCMND: '%s'\n", ierr, (ln_)); \
      exit(ierr); \
   } \
}

#define Mciswspace(C) ( (((C) > 8) && ((C) < 14)) || ((C) == 32) )
#define Mlowcase(C) ( ((C) > 64 && (C) < 91) ? (C) | 32 : (C) )

#define BADINT -777938

#endif
