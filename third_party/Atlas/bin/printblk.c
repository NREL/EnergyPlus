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
#include "atlas_lvl3.h"
#include "atlas_mv.h"
#include "atlas_r1.h"

int main(int nargs, char **args)
{
   int m, n, lda=4;
   TYPE a[4];

   printf("\nGEMM: NB=%d, lat=%d, mu=%d, nu=%d, ku=%d\n\n", NB, ATL_mmLAT,
          ATL_mmMU, ATL_mmNU, ATL_mmKU);

   ATL_GetPartMVN(a, lda, &m, &n);
   printf("mvN  block = %d x %d; mu=%d, nu=%d\n", m, n, ATL_mvNMU, ATL_mvNNU);
   ATL_GetPartMVT(a, lda, &m, &n);
   printf("mvT  block = %d x %d; mu=%d, nu=%d\n\n", m, n, ATL_mvTMU, ATL_mvTNU);

   ATL_GetPartSYMV(a, lda, &m, &n);
   printf("symv block = %d x %d\n\n", m, n);

   ATL_GetPartR1(a, lda, m, n);
   printf("ger  block = %d x %d; mu=%d, nu=%d\n\n", m, n, ATL_r1MU, ATL_r1NU);

   return(0);
}
