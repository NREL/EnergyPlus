#include "atlconf.h"

void PrintUsage(char *name, int iarg)
{
   fprintf(stderr, "error in arg %d USAGE: \n", iarg);
   fprintf(stderr, "   %s -O <os> -s <asm> -v <verb#> -c (cpu) -b (@ bits) -a (arch) -n (ncpu) -m (Mhz) -t (cpu throttling) -T <targ>\n", name);
   exit(iarg);
}

int GetFlags(int nargs, char **args, int *CacheLevel, enum OSTYPE *OS,
             enum ASMDIA *asmd, char **targ)
{
   int i, flag=0, k;
   *CacheLevel = 0;
   *targ = NULL;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0],i);
      switch(args[i][1])
      {
      case 'T':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *targ = args[i];
         break;
      case 's':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *asmd = atoi(args[i]);
         break;
      case 'O':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *OS = atoi(args[i]);
         break;
      case 'n':
         flag |= Pncpu;
         break;
      case 'c':
         flag |= Pncache;
         break;
      case 'C':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         *CacheLevel = atoi(args[i]);
         break;
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i);
         k = atoi(args[i]);
         if (k)
            flag |= Pverb;
         break;
      case 'm':
         flag |= PMhz;
         break;
      case 'a':
         flag |= Parch;
         break;
      case 'b':
         flag |= P64;
         break;
      case 't':
         flag |= Pthrottle;
         break;
      default:
         PrintUsage(args[0], i);
      }
   }
   if (!flag)
     flag = Parch;
   return(flag);
}

void strlowcpy(char *out, char *in)
{
   if (in && out)
   {
      do *out++ = tolower(*in);
      while (*in++);
   }
}

int ProbeOneInt(enum OSTYPE OS0, enum ASMDIA asmd0, char *targ0,
                char *flag, char *find, int *sure)
/*
 * Handles calls to all available backend probes that return one int,
 * keeps trying them until out of probes or one returns good (non-zero) value
 */
{
   char osname[128];
   char *cmnd, *res, *frm, *targ;
   enum OSTYPE OS;
   enum ASMDIA asmd;
   int i, iret=0;

   *sure = 0;
   asmd = (asmd0 > ASM_None && asmd0 < NASMD) ? asmd0 : gas_x86_32;
   OS = (OS0 > OSOther && OS0 < NOS) ? OS0 : OSLinux;
   if (OS == OSOSX)
      strlowcpy(osname, osnam[OSFreeBSD]);
   else if (OSIsWin(OS))
      strcpy(osname, "win");
   else
      strlowcpy(osname, osnam[OS]);
   if (targ0)
   {
      i = strlen(targ0) + 24;
      targ = malloc(sizeof(char)*i);
      assert(targ);
      sprintf(targ, "atlrun=atlas_runX targ=%s", targ0);
   }
   else
   {
      targ = malloc(sizeof(char));
      assert(targ);
      targ[0] = '\0';
   }

/*
 * If Assembler right or unspecified, try x86 probe
 */
   if (asmd == gas_x86_32 || asmd == gas_x86_64)
   {
      frm = "make IRunArchInfo_x86 MYFLAGS=\"-DATL_OS_%s -DATL_%s\" args=\"%s\" %s | fgrep '%s'";
      i = strlen(frm) + strlen(osnam[OS]) + strlen(ASMNAM[asmd]) + strlen(flag)
          + strlen(targ) + strlen(find) + 1;
      cmnd = malloc(sizeof(char)*i);
      assert(cmnd);
      sprintf(cmnd, frm, osnam[OS], ASMNAM[asmd], flag, targ, find);
      res = atlsys_1L(NULL, cmnd, 0, 0);
      if (res)
      {
         iret = GetFirstInt(res);
         *sure = GetLastInt(res);
         free(res);
      }
      free(cmnd);
   }
/*
 * If that didn't work, try OS-specific probe
 */
   if (!iret)
   {
      frm = "make IRunArchInfo_%s MYFLAGS=\"-DATL_OS_%s -DATL_%s\" args=\"%s\" %s | fgrep '%s'";
      i = 1 + strlen(frm) + strlen(osname) + 3+strlen(osnam[OS]) +
          strlen(ASMNAM[asmd]) + strlen(flag) + strlen(targ) + strlen(find);
      cmnd = malloc(sizeof(char)*i);
      assert(cmnd);
      sprintf(cmnd, frm, osname, osnam[OS], ASMNAM[asmd], flag, targ, find);
      res = atlsys_1L(NULL, cmnd, 0, 0);
      if (res)
      {
         iret = GetFirstInt(res);
         *sure = GetLastInt(res);
         free(res);
      }
      free(cmnd);
   }
   return(iret);
}

int ConfirmPtrbits(enum OSTYPE OS0, enum ASMDIA asmd0, char *targ0,
                   char *flag, char *find, int *sure)
/*
 * Retries pointer width probe using -m64; OK to fail: use prior val in that
 * case
 */
{
   char osname[128];
   char *cmnd, *res, *targ;
   char *frm="make IRunArchInfo_%s MYFLAGS=\"-m64 -DATL_OS_%s -DATL_%s\" args=\"%s\" %s | fgrep '%s'";
   enum OSTYPE OS;
   enum ASMDIA asmd;
   int i, iret=0;

   *sure = 0;
   asmd = (asmd0 > ASM_None && asmd0 < NASMD) ? asmd0 : gas_x86_32;
   OS = (OS0 > OSOther && OS0 < NOS) ? OS0 : OSLinux;
   i = (OS == OSOSX) ? OSFreeBSD : OS;
   assert(strlen(osnam[i]) < 128);
   strlowcpy(osname, osnam[i]);
   if (targ0)
   {
      i = strlen(targ0) + 24;
      targ = malloc(sizeof(char)*i);
      assert(targ);
      sprintf(targ, "atlrun=atlas_runX targ=%s", targ0);
   }
   else
   {
      targ = malloc(sizeof(char));
      assert(targ);
      targ[0] = '\0';
   }

/*
 * Try OS-specific probe, compiling with -m64 (assumes gcc workalike)
 */
   i = 1 + strlen(frm) + strlen(osnam) + strlen(osnam[OS]) +
       strlen(ASMNAM[asmd]) + strlen(flag) + strlen(targ) + strlen(find);
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd, frm, osname, osnam[OS], ASMNAM[asmd], flag, targ, find);
   free(targ);
   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
      iret = GetFirstInt(res);
      *sure = GetLastInt(res);
      free(res);
   }
   return(iret);
}

int main(int nargs, char **args)
{
   int flags, CacheLevel, osname[128], sure, bits, i, j;
   enum OSTYPE OS;
   enum ASMDIA asmd;
   enum MACHTYPE arch;
   char *targ;

   flags = GetFlags(nargs, args, &CacheLevel, &OS, &asmd, &targ);
   if (flags & Parch)
   {
      arch = ProbeOneInt(OS, asmd, targ, "-a", "MACHTYPE=", &sure);
      if (arch == MACHOther && (asmd == gas_x86_32 || asmd == gas_x86_64))
         arch = x86X;
      if (flags & Pverb)
         printf("Architecture detected as %s.\n", machnam[arch]);
      printf("MACHTYPE=%d\n", arch);
   }
   if (flags & Pncpu)
      printf("NCPU=%d\n", ProbeOneInt(OS, asmd, targ, "-n", "NCPU=", &sure));
   if (flags & PMhz)
      printf("CPU MHZ=%d\n",
             ProbeOneInt(OS, asmd, targ, "-m", "CPU MHZ=", &sure));
   if (flags & Pthrottle)
      printf("CPU THROTTLE=%d\n",
             ProbeOneInt(OS, asmd, targ, "-t", "CPU THROTTLE=", &sure));
   if (flags & P64)
   {
      if (asmd == gas_x86_64)
      {
         sure = 1;
         bits = 64;
      }
      else
      {
         bits = ProbeOneInt(OS, asmd, targ, "-b", "PTR BITS=", &sure);
         if (bits != 64)
         {
            i = ConfirmPtrbits(OS, asmd, targ, "-b", "PTR BITS=", &j);
            if (j)
            {
               bits = i;
               sure = j;
            }
         }
      }
      printf("PTR BITS=%d, SURE=%d\n", bits, sure);
   }

/*
 * Here for future, presently unsupported
 */
   if (flags & Pncache)
      printf("NCACHES=0\n");
   if (flags & PCacheSize)
      printf("%d Cache size (kb) = 0\n", CacheLevel);
   return(0);
}
