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
   case AFSPARC:
      res = atlsys_1L(NULL, "/usr/sbin/psrinfo -pv | fgrep UltraSPARC", 0, 0);
      if (res)
      {
         if (strstr(res, "UltraSPARC-T2"))
            mach = SunUST2;
         else if (strstr(res, "UltraSPARC-T1"))
            mach = SunUST1;
         else if (strstr(res, "UltraSPARC-IV"))
            mach = SunUSIV;
         else if (strstr(res, "UltraSPARC-III"))
            mach = SunUSIII;
         else if (strstr(res, "UltraSPARC-II"))
            mach = SunUSII;
         else if (strstr(res, "UltraSPARC-I"))
            mach = SunUSI;
         free(res);
      }
/*
 *    sparcv9 could be UltraSPARC I,II, III or IV.  Only USIII/IV run faster
 *    than 650Mhz (AFAIK), and as far as ATLAS is concerned, USIII & IV are
 *    same processor; so declare anything with Mhz > 700 as an USIII.  Newer
 *    chips should have the newer psrinfo used above, which allows more
 *    precise determination anyway.  Actually, USIII redesign happened at
 *    1050Mhz, so I should probably call anything Mhz > 1040 an USIV, but
 *    I assume most USIV will have the newer SunOS/psrinfo above, so declare
 *    anything using this to be USIII, to minimize user confusion.
 */
      else if (res=atlsys_1L(NULL, "/usr/sbin/psrinfo -v | fgrep sparcv9", 0,0))
      {
         free(res);
         mach = SunUSX;
         res = atlsys_1L(NULL,  "/usr/sbin/psrinfo -v | fgrep MHz", 0, 0);
         if (res)
         {
            i = GetIntBeforeWord("MHz", res);
            if (i != BADINT && i > 700) mach = SunUSIII;
            free(res);
         }
         else if (res=atlsys_1L(NULL, "/usr/sbin/psrinfo -v | fgrep GHz", 0, 0))
         {
            mach = SunUSIII;
            free(res);
         }
      }
      break;
   }
   return(mach);
}
int ProbeNCPU()
{
   int ncpu = 0;
   char *res;

   res = atlsys_1L(NULL, "uname -X | fgrep NumCPU", 0, 0);
   if (res)
   {
      ncpu = GetFirstInt(res);
      free(res);
   }
   else if ((res=atlsys_1L(NULL, "/bin/uname -X | fgrep NumCPU", 0, 0)))
   {
      ncpu = GetFirstInt(res);
      free(res);
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
   res = atlsys_1L(NULL, "/usr/sbin/psrinfo -v | fgrep MHz", 0, 0);
   if (res)
   {
      mhz = GetIntBeforeWord("MHz", res);
      if (mhz == BADINT) mhz = 0;
      free(res);
   }
   if (!mhz)
   {
      res = atlsys_1L(NULL, "/usr/sbin/psrinfo -v | fgrep GHz", 0, 0);
      if (res)
      {
         mhz = GetIntBeforeWord("GHz", res);
         mhz = (mhz == BADINT) ? 0 : mhz*1000;
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
/*
 * I have no idea how to do this for SunOS/Irix/AIX/Windows/interix
 */
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
