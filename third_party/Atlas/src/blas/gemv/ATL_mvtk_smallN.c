/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
#include "atlas_level1.h"
#include "atlas_lvl2.h"
#include "atlas_lvl3.h"

void Mjoin(PATL,mvtk_smallN)
   (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *X, ATL_CINT incX, const SCALAR beta, TYPE *Y, ATL_CINT incY)
/*
 * y = alpha*A*x + beta*y
 * A is MxN, so X is of length M and Y is of length M
 * NOTE: this routines is usually called for cleanup or when N is too small
 *       to allow for vector copies
 * This routine calls dot product, in the hope that the Level 1 has been
 * at least somewhat tuned to the architecture.
 */
{
#ifdef TREAL
   TYPE y0, dot;
   ATL_INT j;
   ATL_CINT BetaIsNonZero = (beta != ATL_rzero);

   for (j=0; j < N; j++, A += lda, Y += incY)
   {
      y0 = (BetaIsNonZero) ? *Y * beta : ATL_rzero;
      dot = alpha * Mjoin(PATL,dot)(M, A, 1, X, incX);
      *Y = y0 + dot;
   }
#else
   TYPE ry, iy, rd, id, tmp;
   const TYPE rbe = *beta, ibe = beta[1], ral = *alpha, ial = alpha[1];
   ATL_CINT lda2 = lda+lda, incY2 = incY+incY;
   ATL_INT j;

   if (ibe == ATL_rzero) /* real scalar */
   {
      if (rbe == ATL_rzero)     /* beta is zero */
      {
         for (j=0; j < N; j++, A += lda2, Y += incY2)
         {
            Mjoin(PATL,dotu_sub)(M, A, 1, X, incX, Y);
            rd = Y[0];
            id = Y[1];
            tmp = rd*ral - id*ial;
            id  = rd*ial + id*ral;
            *Y = tmp;
            Y[1] = id;
         }
      }
      else if (rbe == ATL_rone) /* beta is one */
      {
         for (j=0; j < N; j++, A += lda2, Y += incY2)
         {
            ry = *Y;
            iy = Y[1];
            Mjoin(PATL,dotu_sub)(M, A, 1, X, incX, Y);
            rd = Y[0];
            id = Y[1];
            tmp = rd*ral - id*ial;
            id  = rd*ial + id*ral;
            *Y = ry+tmp;
            Y[1] = iy+id;
         }
      }
      else                      /* beta is arbitrary real scalar */
      {
         for (j=0; j < N; j++, A += lda2, Y += incY2)
         {
            ry = *Y * rbe;
            iy = Y[1] * rbe;
            Mjoin(PATL,dotu_sub)(M, A, 1, X, incX, Y);
            rd = Y[0];
            id = Y[1];
            tmp = rd*ral - id*ial;
            id  = rd*ial + id*ral;
            *Y = ry+tmp;
            Y[1] = iy+id;
         }
      }
   }
   else /* beta is a complex scalar */
   {
      for (j=0; j < N; j++, A += lda2, Y += incY2)
      {
         ry = *Y;
         iy = Y[1];
         tmp = ry*rbe - iy*ibe;
         iy  = ry*ibe + iy*rbe;
         ry  = tmp;
         Mjoin(PATL,dotu_sub)(M, A, 1, X, incX, Y);
         rd = Y[0];
         id = Y[1];
         tmp = rd*ral - id*ial;
         id  = rd*ial + id*ral;
         *Y = ry+tmp;
         Y[1] = iy+id;
      }
   }
#endif
}
