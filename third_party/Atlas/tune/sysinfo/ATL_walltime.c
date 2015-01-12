/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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

#ifdef PentiumCPS  /* this code supplied by Phil Mucci */
   #include <stdlib.h>
   #include <stdio.h>

   static const double CPS = 1.0 / (PentiumCPS*1.0E6);
   static unsigned usec, sec;
   static unsigned tusec, tsec;
   static unsigned start=0, startu;
   static long long foo;

   static inline void microtime(unsigned *lo, unsigned *hi)
   {
     __asm __volatile (
           ".byte 0x0f; .byte 0x31; movl    %%edx,%0; movl    %%eax,%1"
                   : "=g" (*hi), "=g" (*lo) :: "eax", "edx");
   }

   double ATL_walltime(void)
   {
     if (!start)
     {
        microtime(&startu, &start);
        return(0.0);
     }
     microtime(&usec, &sec);

     foo = sec;
     foo -= start;
     foo = (foo << 32) + usec;
     foo -= startu;
     return(((double)foo)*CPS);
   }
#elif defined(ATL_OS_WinNT) /* special code for windows */
   #include <windows.h>
   double ATL_walltime(void)
   {
      static double freqRecip = 0.0;
      LARGE_INTEGER msout;
      unsigned long long myout;
      if (freqRecip == 0.0)
      {
         QueryPerformanceFrequency(&msout);
         myout = msout.HighPart;
         myout = (myout<<32) | msout.LowPart;
         freqRecip = 1.0/((double) myout);
      }
      QueryPerformanceCounter(&msout);
      myout = msout.HighPart;
      myout = (myout<<32) | msout.LowPart;
      return(myout*freqRecip);
   }
#elif defined(UseTimes)
   #include <stdlib.h>
   #include <sys/times.h>
   #include <unistd.h>
   double ATL_walltime(void)
   {
      struct tms ts;
      static double ClockTick=0.0;

      if (ClockTick == 0.0) ClockTick = 1.0 / ((double) sysconf(_SC_CLK_TCK));
      return( ((double) times(&ts)) * ClockTick);
   }
#elif defined(SUN_HR) /* use sun high resolution timers */
   #include <sys/time.h>
   double ATL_walltime(void)
   {
      return(gethrtime()*1.0e-9);
   }
#elif defined(POSIX_HR) /* use the POSIX HR timers */
   #include <time.h>
   double ATL_walltime(void)
   {
      struct timespec ts;
      static double t0;
      double res;
      static int INIT = 0;

      if (INIT)
      {
         clock_gettime(CLOCK_REALTIME, &ts);
         res = ts.tv_sec + 1.0e-9 * ts.tv_nsec;
         return(res - t0);
      }
      clock_gettime(CLOCK_REALTIME, &ts);
      t0 = ts.tv_sec + 1.0e-9 * ts.tv_nsec;
      INIT = 1;
      return(0.0);
   }
/*
 * Without gcc, I know no standard Windows wall-timer, so use cputime
 */
#elif (!defined(__GNUC__) && (defined(ATL_OS_Win9x) || defined(ATL_OS_WinNT))) \
      || defined(__MINGW32__)
   #include <time.h>
   double ATL_walltime(void)
   {
      clock_t t1;
      static int INIT=0;
      static clock_t t0;
      static const double CPS = 1.0 / (1.0*CLOCKS_PER_SEC);
      double d;

      if (INIT)
      {
         t1 = clock() - t0;
         d = t1 * CPS;
         return(d);
      }
      INIT = 1;
      t0 = clock();
      return(0.0);
   }
#else
   #include <stdlib.h>
   #include <sys/time.h>
   #include <sys/resource.h>
   double ATL_walltime(void)
   {
      struct timeval tp;
      static long start=0, startu;
      if (!start)
      {
         gettimeofday(&tp, NULL);
         start = tp.tv_sec;
         startu = tp.tv_usec;
         return(0.0);
      }
      gettimeofday(&tp, NULL);
      return( ((double) (tp.tv_sec - start)) + (tp.tv_usec-startu)/1000000.0 );
   }
#endif

