#include "atlconf.h"

enum OSTYPE ProbeOS(int verb, char *targ)
{
   int i, ierr=0;
   char ln[1024], ln2[1024];
   char *cmnd, *res;
   enum OSTYPE OS;
   char *unam;

   if (verb) printf("Probing to make operating system determination:\n");
   unam = FindUname(targ);

   i = strlen(unam) + 4;
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, "%s -s", unam);
   res = atlsys_1L(targ, cmnd, verb, 0);
   if (res)
   {
/*
 *    Accept GNU (HURD) as Linux, since they seem to use same stuff;
 *    This is patch from Sylvestre Ledru; I have no direct experience wt HURD
 */
      if(strstr(res, "Linux") || strstr(res, "GNU")) OS = OSLinux;
      else if(strstr(res, "FreeBSD")) OS = OSFreeBSD;
      else if (strstr(res, "Darwin")) OS = OSOSX;
      else if(strstr(res, "SunOS"))
      {
         sprintf(cmnd, "%s -r", unam);
         free(res);
         res = atlsys_1L(targ, cmnd, verb, 0);
         if (res[0] == '4') OS = OSSunOS4;
         else OS = OSSunOS;
      }
      else if(strstr(res, "OSF1")) OS = OSOSF1;
      else if(strstr(res, "IRIX")) OS = OSIRIX;
      else if(strstr(res, "AIX")) OS = OSAIX;
      else if(strstr(res, "WIN"))
      {
         if (strstr(res, "95") || strstr(res, "98") || strstr(res, "_ME"))
            OS = OSWin9x;
/*
 *       Need to confirm what is returned under cygwin for XP, etc.
 */
         else if (strstr(res, "WOW64")) OS =OSWin64;
         else if (strstr(res, "NT")) OS = OSWinNT;
         else ierr = 1;
      }
      else if (strstr(res, "HP-UX")) OS = OSHPUX;
      else ierr = 1;
      free(res);
   }
   free(cmnd);
   if (ierr)
   {
      printf("\n\nUnable to determine OS, quitting\n\n");
      exit(-1);
   }
   if (verb)
      printf("Operating system configured as %s\n\n", osnam[OS]);

   return(OS);
}

void PrintUsage(char *name, int i)
{
   fprintf(stderr, "Error around argument %d\n", i);
   fprintf(stderr, "USAGE: %s -v <verbose #> -T <targ machine>\n", name);
   exit(-1);
}

int GetFlags(int nargs, char **args, char *targ)
{
   int verb=1, i;
   *targ = '\0';
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i);
      switch(args[i][1])
      {
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         verb = atoi(args[i]);
         break;
      case 'T':                    /* target machine for spawn */
         if (++i >= nargs)
            PrintUsage(args[0], i);
         strcpy(targ, args[i]);
         break;
      default:
         PrintUsage(args[0], i);
      }
   }
   return(verb);
}

int main(int nargs, char **args)
{
   int verb;
   char targ[1024];
   enum OSTYPE OS;
   verb = GetFlags(nargs, args, targ);
   OS = ProbeOS(verb, *targ == '\0' ? NULL : targ);
   printf("OS=%d\n", OS);
   return(OS == OSOther);
}
