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

#if defined(UseClock)
   #include <time.h>
   double ATL_cputime(void)
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
#elif defined(POSIX_HR) /* use the POSIX HR timers */
   #include <time.h>
   double ATL_cputime(void)
   {
      struct timespec ts;
      static double t0;
      double res;
      static int INIT = 0;

      if (INIT)
      {
         clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
         res = ts.tv_sec + 1.0e-9 * ts.tv_nsec;
         return(res - t0);
      }
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID,&ts);
      t0 = ts.tv_sec + 1.0e-9 * ts.tv_nsec;
      INIT = 1;
      return(0.0);
   }
#elif defined(UseTimes)
   #include <stdlib.h>
   #include <sys/times.h>
   #include <unistd.h>
   double ATL_cputime(void)
   {
      struct tms ts;
      static double ClockTick=0.0;

      if (ClockTick == 0.0) ClockTick = 1.0 / ((double) sysconf(_SC_CLK_TCK));
      times(&ts);
      return( ((double) ts.tms_utime) * ClockTick );
   }
#elif defined(SUN_HR) /* use sun high resolution timers */
   #include <sys/time.h>
   double ATL_cputime(void)
   {
      return(gethrvtime()*1.0e-9);
   }
#else
   #include <stdlib.h>
   #include <sys/time.h>
   #include <sys/resource.h>
   double ATL_cputime(void)
   {
      struct rusage ruse;
      getrusage(RUSAGE_SELF, &ruse);
      return( (double)(ruse.ru_utime.tv_sec+ruse.ru_utime.tv_usec/1000000.0) );
   }
#endif

