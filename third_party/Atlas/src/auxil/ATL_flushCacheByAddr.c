/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2007 R. Clint Whaley
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
#include "atlas_misc.h"

#if ATL_LINEFLUSH    /* set in atlas-aux.h, if on one of the below archs */

#if defined(ATL_ARCH_PPCG5) || defined(ATL_ARCH_PPCG4) || defined(ATL_GAS_PPC)
   #define ATL_flushCacheLine(mem) __asm__ __volatile__ \
      ("dcbf 0, %0" :: "r"((void *)(mem)))
#elif defined(ATL_ARCH_IA64Itan) || defined(ATL_ARCH_IA64Itan2)
   #define ATL_flushCacheLine(mem) __asm__ __volatile__ \
      ("fc %0" :: "r"((void *)(mem)))
#elif defined(ATL_SSE2)
   #define ATL_flushCacheLine(mem) __asm__ __volatile__ \
      ("clflush %0" : : "m" (*((char *)(mem))))
#else
   #define ATL_flushCacheLine(mem) \
   { \
      fprintf(stderr, "Cannot do cache-line flushing, %d of %s!\n",  \
              __LINE__, __FILE__); \
      exit(-1); \
   }
#endif

void ATL_flushCacheByAddr(size_t N, void *vp)
{
   double *dp = vp;  /* assume cache line at least 8 bytes long */
   size_t i;
   for (i=0, N /= sizeof(double); i < N; i++)
      ATL_flushCacheLine(dp+i);
}


FLSTRUCT *ATL_GetFlushStruct(void *p, int length, FLSTRUCT *next)
{
   FLSTRUCT *fp;

   fp = malloc(sizeof(FLSTRUCT));
   ATL_assert(fp);
   fp->p = p;
   fp->length = length;
   fp->next = next;

   return(fp);
}

void ATL_KillAllFlushStructs(FLSTRUCT *p)
{
   FLSTRUCT *kill;
   while (p)
   {
      kill = p;
      p = p->next;
      free(kill);
   }
}

void ATL_FlushAreasByCL(FLSTRUCT *fp)
{
   int i, n;
   char *cp;
   while (fp)
   {
      ATL_flushCacheByAddr(fp->length, fp->p);
      fp = fp->next;
   }
}

#endif   /* end #if ATL_LINEFLUSH */
