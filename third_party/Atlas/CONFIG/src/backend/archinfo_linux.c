#include "atlconf.h"

void PrintUsage(char *name, int i)
{
   fprintf(stderr, "USAGE: %s -v (verb) -b (@ bits) -a (arch) -n (ncpu) -c <ncache> -C <lvl> (cache size) -m (Mhz) -t (cpu throttling)\n", name);
   exit(i);
}

int GetFlags(int nargs, char **args, int *CacheLevel)
{
   int i, flag = 0;

   *CacheLevel = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-') PrintUsage(args[0], i);
      switch(args[i][1])
      {
      case 'n':
         flag |= Pncpu;
         break;
      case 'c':
         flag |= Pncache;
         break;
      case 'C':
         if (++i > nargs)
            PrintUsage(args[0], i);
         *CacheLevel = atoi(args[i]);
         break;
      case 'v':
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
     flag = Parch | P64;
   return(flag);
}

enum MACHTYPE ProbeArch()
{
   enum ARCHFAM fam;
   enum MACHTYPE mach=MACHOther;
   int ierr, i;
   char *res;

   fam = ProbeArchFam(NULL);
   switch(fam)
   {
   case AFPPC:
      res = atlsys_1L(NULL, "cat /proc/cpuinfo | fgrep cpu", 0, 0);
      if (res)
      {
#if 0
         if (strstr(res, "604e")) mach = PPC604e;
         else if (strstr(res, "604")) mach = PPC604;
         else
#endif
         if (strstr(res, "G4")) mach = PPCG4;
         else if (strstr(res, "7400")) mach = PPCG4;
         else if (strstr(res, "7410")) mach = PPCG4;
         else if (strstr(res, "7447")) mach = PPCG4;
         else if (strstr(res, "7455")) mach = PPCG4;
         else if (strstr(res, "PPC970FX")) mach = PPCG5;
         else if (strstr(res, "PPC970MP")) mach = PPCG5;
         else if (strstr(res, "POWER7")) mach = IbmPwr7;
         else if (strstr(res, "POWER6")) mach = IbmPwr6;
         else if (strstr(res, "POWER5")) mach = IbmPwr5;
         else if (strstr(res, "POWER4")) mach = IbmPwr4;
         else if (strstr(res, "e6500")) mach = Pwre6500;
         free(res);
      }
      break;
   case AFMIPS:
      res = atlsys_1L(NULL, "fgrep 'cpu model' /proc/cpuinfo", 0, 0);
      if (res)
      {
         if (res[0] != '\0')
         {
            if (strstr(res, "ICE9"))
               mach = MIPSICE9;
/*
 *          I have no access to what cpuinfo on Linux does for this procs,
 *          so this is a WAG as to what it would say
 */
            else if (strstr(res, "R10000") || strstr(res, "R12000") ||
                     strstr(res, "R12000") || strstr(res, "R14000"))
               mach = MIPSR1xK;
         }
         free(res);
      }
      break;
   case AFARM:
      res = atlsys_1L(NULL, "fgrep 'Processor' /proc/cpuinfo", 0, 0);
      if (res)
      {
         if (strstr(res, "ARMv7") || strstr(res,"v7l")) mach = ARMv7;
         free(res);
      }
      else if ( res=atlsys_1L(NULL, "fgrep cpu /proc/cpuinfo", 0, 0) )
      {
         if (strstr(res, "ARMv7") || strstr(res,"v7l")) mach = ARMv7;
         free(res);
      }
      break;
   case AFIA64:
      res = atlsys_1L(NULL, "fgrep 'Itanium' /proc/cpuinfo", 0, 0);
      if (res && res[0] == '\0')
      {
         free(res);
         res = NULL;
      }
      if (!res)
      {
         res = atlsys_1L(NULL, "fgrep 'IA-64' /proc/cpuinfo", 0, 0);
         if (res && res[0] == '\0')
         {
            free(res);
            res = NULL;
         }
      }
      if (!res)
         res = atlsys_1L(NULL, "fgrep \"model name\" /proc/cpuinfo", 0, 0);
      if (res)
      {
         if (res[0] != '\0')
         {
            if (strstr(res, "Itanium 2") || strstr(res, "McKinley") ||
                strstr(res, "IA-64"))
               mach = IA64Itan2;
            else if (strstr(res, "Itanium")) mach = IA64Itan;
         }
         free(res);
      }
      break;
   case AFX86:
      res = atlsys_1L(NULL, "fgrep 'model name' /proc/cpuinfo", 0, 0);
      if (res && res[0] == '\0')
      {
         free(res);
         res = NULL;
      }
      if (!res)
         res = atlsys_1L(NULL, "fgrep model /proc/cpuinfo", 0, 0);
      if (res && res[0] == '\0')
      {
         free(res);
         res = NULL;
      }
      if (res)
      {
         if (strstr(res, "Pentium"))
         { /* Pentium of some flavor */
            if (strstr(res, " III ")) mach = IntPIII;
            else if (strstr(res, " II ")) mach = IntPII;
            else if (strstr(res, "Pro")) mach = IntPPRO;
            else if (strstr(res, "MMX")) mach = IntP5MMX;
            else if (strstr(res, " 4 "))
            {
               char *rs2;
               rs2 = atlsys_1L(NULL,
                      "fgrep 'model' /proc/cpuinfo | fgrep -v 'name'", 0, 0);
               if (rs2)
               {
                  i = GetLastInt(res);
                  if (i < 3) mach = IntP4;
                  else if (i == 3) mach = IntP4E;
                  free(rs2);
               }
            }
         }
         else if (strstr(res, "Atom"))
            mach = IntAtom;
         else if (strstr(res, "Core"))
         {
            if (strstr(res, "i7"))
            {
               if (strstr(res, "4770") || strstr(res, "4765"))
                  mach = IntCorei3;
               else if (strstr(res, "2600") || strstr(res, "3770"))
                  mach = IntCorei2;
               else
                  mach = IntCorei1;
            }
            if (strstr(res, "i5"))
            {
               if (strstr(res, "4670") || strstr(res, "4570") ||
                   strstr(res, "4430"))
                  mach = IntCorei3;
               else if (strstr(res, "i5-2500") || strstr(res, "i5-2400") ||
	           strstr(res, "i5-2390") || strstr(res, "i5-2300"))
                  mach = IntCorei2;
               else
                  mach = IntCorei1;
            }
         }
         else if (strstr(res, "Xeon")) /* dreaded Xeon-is-anything */
         {
            if (strstr(res, "E5420")) mach = IntCore2;
            else if (strstr(res, "X7560")) mach = IntCorei1;
         }
         else if (strstr(res, "A8-3850"))
            mach = AmdLlano;
         else if (strstr(res, "Efficeon")) mach = TMEff;
         else if (strstr(res, "Athlon HX")) mach = AmdHammer;
         else if (strstr(res, "Opteron") || strstr(res, "Hammer") ||
                  strstr(res, "Athlon(tm) 64"))
            mach = AmdHammer;
         else if (strstr(res, "Athlon")) mach = AmdAthlon;
         else if (strstr(res, "AMD-K7")) mach = AmdAthlon;
         free(res);
      }
      break;
/*
 *    Add these back if we get machine access and can test
 */
   case AFSPARC:  /* don't know here anymore */
      res = atlsys_1L(NULL, "fgrep cpu /proc/cpuinfo", 0, 0);
      if (res)
      {
         if (strstr(res, "UltraSparc T2")) mach = SunUST2;
         else if (strstr(res, "UltraSparc T1")) mach = SunUST1;
         else if (strstr(res, "UltraSparc IV")) mach = SunUSIV;
         else if (strstr(res, "UltraSparc III")) mach = SunUSIII;
         else if (strstr(res, "UltraSparc II")) mach = SunUSII;
         else if (strstr(res, "UltraSparc I")) mach = SunUSI;
         else if (strstr(res, "UltraSparc")) mach = SunUSX;
         free(res);
      }
      break;
   case AFALPHA:
      #if 0
      res[0] = '\0';
      ierr = CmndOneLine(NULL, "fgrep 'model name' /proc/cpuinfo", res);
      if (ierr || res[0] == '\0')
         ierr = CmndOneLine(NULL, "fgrep model /proc/cpuinfo", res);
      if (!ierr && res[0] != '\0')
      {
         if (strstr(res, "EV5")) mach = Dec21164;
         else if (strstr(res, "EV4")) mach = Dec21064;
         else if (strstr(res, "EV6")) mach = Dec21264;
      }
      #endif
      break;
   case AFS390:
      res = atlsys_1L(NULL, "cat /proc/cpuinfo | fgrep \"processor \"", 0, 0);
      if (res)
      {
         if (strstr(res, "2094") || strstr(res, "2096")) mach = IbmZ9;
         else if (strstr(res, "2097") || strstr(res, "2098")) mach = IbmZ10;
         /* we consider anything else to be a z196 or later */
         else mach = IbmZ196;  /* looks risky to me, but IBM folks did it */
         free(res);
      }
      break;
   default:
#if 0
      if (!CmndOneLine(NULL, "fgrep 'cpu family' /proc/cpuinfo", res))
         if (strstr(res, "PA-RISC 2.0")) mach = HPPA20;
#else
     ;
#endif
   }
   return(mach);
}
int ProbeNCPU()
{
   int ncpu = 0;
   char *res;

   #if 0
   if (mach == Dec21264 || mach == Dec21164 || mach == Dec21064)
   {
      if ( !CmndOneLine(NULL, "fgrep 'cpus detected' /proc/cpuinfo", res) )
         ncpu = GetLastInt(res);
   }
   #endif
   if (!ncpu)
   {
      FILE *fpres;
      int len=0;
      fpres = atlsys(NULL, "grep '^processor' /proc/cpuinfo", 0, 0);
      if (fpres)
      {
         for (ncpu=0; res = ATL_fgets(res, &len, fpres); ncpu++);
         fclose(fpres);
      }
   }
   if (!ncpu)
   {
      res = atlsys_1L(NULL, "grep 'ncpus active' /proc/cpuinfo", 0, 0);
      if (res)
      {
         ncpu = GetFirstInt(res);
         free(res);
      }
   }
   return(ncpu);
}

int ProbePointerBits(int *sure)
{
   int i;
   char *uname;
   char *cmnd, *res;

   *sure = 0;
/*
 * This probe should be running on backend; if its ptr length is 8, we've
 * definitely got a 64 bit machine
 * NOTE: getting 4 could be a result of compiler flags on a 64-bit arch,
 * so reverse is not dispositive
 */
   if (sizeof(void*) == 8)
   {
      *sure = 1;
      return(64);
   }

/*
 * Note this is a weak probe, archinfo_x86 much better . . .
 */
   uname = FindUname(NULL);
   i = strlen(uname) + 4;
   cmnd = malloc(sizeof(char)*i);
   assert(cmnd);
   sprintf(cmnd, "%s -a", uname);

   res = atlsys_1L(NULL, cmnd, 0, 0);
   free(cmnd);
   if (res)
   {
/*
 *    If uname is a known 64-bit platform, we're sure we've got OS support
 *    for 64bits (may not have compiler support, but that's not our fault)
 */
      if (strstr(res, "x86_64") || strstr(res, "ppc64") || strstr(res, "ia64"))
      {
         *sure = 1;
         free(res);
         return(64);
      }
      free(res);
   }
   return(32);
}

int ProbeMhz()
{
   int mhz=0;
   char *res;
   res = atlsys_1L(NULL, "fgrep 'cpu MHz' /proc/cpuinfo", 0, 0);
   if (res)
   {
      mhz = GetFirstDouble(res) + 0.5;
      free(res);
   }
   if (!mhz)
   {
      res = atlsys_1L(NULL, "cat /proc/cpuinfo | fgrep clock | fgrep MHz",0,0);
      if (res)
      {
         mhz = GetLastLongWithRound(res);
         free(res);
      }
   }
/*
 * Try Linux/SPARC lookup
 */
   if (!mhz)
   {
      res = atlsys_1L(NULL, "fgrep 'ClkTck' /proc/cpuinfo", 0, 0);
      if (res)
      {
         mhz = GetLastHex(res) / 1000000;
         free(res);
      }
   }
   return(mhz);
}

int ProbeThrottle()
/*
 * RETURNS: 1 if cpu throttling is detected, 0 otherwise
 */
{
   int iret=0;
   int imax=0, imin=0, icur=0;
   char *res;

/*
 * If cpufreq directory doesn't exist, guess no throttling.  If
 * cpufreq exists, and cur Mhz < max, throttling is enabled,
 * throttling also enabled if governer is not "performance", and min freq
 * is less than max
 */
   res = atlsys_1L(NULL,
                   "cat /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq",
                   0, 0);
   if (res)
   {
      imax = GetFirstInt(res);
      free(res);
      res = atlsys_1L(NULL,
            "cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_min_freq", 0, 0);
      assert(res);
      imin = GetFirstInt(res);
      free(res);
      res = atlsys_1L(NULL,
            "cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq", 0, 0);
      assert(res);
      icur = GetFirstInt(res);
      free(res);
      res = atlsys_1L(NULL,
            "cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor", 0, 0);
      assert(res);
      if (icur < imax)
         iret = 1;
      else if (!strstr(res, "performance") && imin < imax)
         iret = 1;
      free(res);
   }
   return(iret);
}

main(int nargs, char **args)
{
   int flags, CacheLevel, ncpu, mhz, bits, sure;
   enum MACHTYPE arch=MACHOther;

   flags = GetFlags(nargs, args, &CacheLevel);
   if (flags & Parch)
   {
      arch = ProbeArch();
      if (flags & Pverb)
         printf("Architecture detected as %s.\n", machnam[arch]);
      printf("MACHTYPE=%d\n", arch);
   }
   if (flags & Pncpu)
      printf("NCPU=%d\n", ProbeNCPU());
   if (flags & PMhz)
      printf("CPU MHZ=%d\n", ProbeMhz());
   if (flags & Pthrottle)
      printf("CPU THROTTLE=%d\n", ProbeThrottle());
   if (flags & P64)
   {
      bits = ProbePointerBits(&sure);
      printf("PTR BITS=%d, SURE=%d\n", bits, sure);
   }

/*
 * Here for future, presently unsupported
 */
   if (flags & Pncache)
      printf("NCACHES=0\n");
   if (flags & PCacheSize)
      printf("%d Cache size (kb) = 0\n", CacheLevel);
   exit(0);
}
