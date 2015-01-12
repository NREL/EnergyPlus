/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2000 R. Clint Whaley
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

double ATL_flushcache(long long size)
/*
 * flush cache by reading enough mem; note that if the compiler gets
 * really smart, may be necessary to make vp a global variable so it
 * can't figure out it's not being modified other than during setup;
 * the fact that ATL_dzero is external will confuse most compilers
 */
{
  static void *vp=NULL;
  static long long N = 0;
  double *cache;
  double dret=0.0;
  size_t i;

  if (size < 0) /* flush cache */
  {
     ATL_assert(vp);
     cache = ATL_AlignPtr(vp);
     if (N > 0) for (i=0; i != N; i++) dret += cache[i];
  }
  else if (size > 0) /* initialize */
  {
     vp = malloc(ATL_Cachelen + size);
     ATL_assert(vp);
     N = size / sizeof(double);
     cache = ATL_AlignPtr(vp);
     ATL_dzero(N, cache, 1);
  }
  else if (size == 0) /* free cache */
  {
     if (vp) free(vp);
     vp = NULL;
     N = 0;
  }
  return(dret);
}
