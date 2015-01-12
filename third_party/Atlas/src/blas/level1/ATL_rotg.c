/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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
#include <math.h>

#ifdef TREAL
void Mjoin(PATL,rotg)
(
   TYPE *a,    /* INPUT : first rotational elimination parameter */
               /* OUTPUT: r (see below) */
   TYPE *b,    /* INPUT: second rotational elimination parameter */
               /* OUTPUT: z (see below) */
   TYPE *c,    /* OUTPUT: cosine */
   TYPE *s     /* OUTPUT: sine */
)
/*
 *   | c  s|*|a| = |r|
 *   |-s  c| |b|   |0|
 * This routine returns:
 *    r = sigma * sqrt(a^2 + b^2), where
 *      sigma = sign(a) if abs(a) > abs(b)
 *      sigma = sign(b) if abs(a) <= abs(b)
 *    r is returned in *a
 *
 *    z = s     if (abs(a) > abs(b))
 *    z = 1/c   if (abs(a) <= abs(b) && c != 0 && r != 0)
 *    z = 1     if (abs(a) <= abs(b) && c == 0 && r != 0)
 *    z = 0     if (r == 0)
 *    z is returned in *b
 *
 *    c : cosign of the angle of (Givens) rotation
 *    c = a/r   if (r != 0)
 *    c = 1     if (r == 0)
 *
 *    s : sine of the angle of (Givens) rotation
 *    s = b/r   if (r != 0)
 *    s = 0     if (r == 0)
 *    FURTHER DETAILS:
 *       http://publib.boulder.ibm.com/infocenter/clresctr/vxrx/index.jsp?topic=/com.ibm.cluster.essl43.guideref.doc/am501_hsrotg.html
 *
 */
{
   TYPE roe, scal, r, z, aa, ab, t0, t1;

   aa = Mabs(*a);
   ab = Mabs(*b);
   if (aa > ab) roe = *a;
   else roe = *b;
   scal = aa + ab;
   if (scal != ATL_rzero)
   {
      t0 = aa / scal; t1 = ab / scal;
      r = scal * sqrt(t0*t0 + t1*t1);
      if (roe < ATL_rzero) r = -r;
      *c = *a / r;
      *s = *b / r;
      if (aa > ab) z = *s;
      else if (*c != ATL_rzero) z = ATL_rone / *c;
      else z = ATL_rone;
      *a = r;
      *b = z;
   }
   else
   {
      *c = ATL_rone;
      *s = *a = *b = ATL_rzero;
   }
}
#else
#define Msafnrm2(x_, nrm2_) \
{ \
   register TYPE w_ = Mabs(*(x_)), z_=Mabs((x_)[1]); \
   if (w_ < z_) { (nrm2_) = w_; w_ = z_; z_ = (nrm2_); } \
   if (z_ != ATL_rzero) \
   { \
      z_ /= w_; \
      (nrm2_) = w_ * sqrt(ATL_rone + (z_*z_)); \
   } \
   else (nrm2_) = w_; \
}

void Mjoin(PATL,rotg)(TYPE *a, const TYPE *b, TYPE *c, TYPE *s)
{
   TYPE absA, absB, scal, norm, ra, ia, rb, ib;

   Msafnrm2(a, absA);
   if (absA != ATL_rzero)
   {
      Msafnrm2(b, absB);
      scal = absA + absB;
      ra = *a / scal; ia = a[1] / scal;
      rb = *b / scal; ib = b[1] / scal;
      norm = scal * sqrt( ra*ra+ia*ia + rb*rb+ib*ib );
      ra = *a / absA;
      ia = a[1] / absA;
      rb = *b; ib = b[1];

      *c = absA / norm;
      *s = (ra * rb + ia * ib) / norm;
      s[1] = (ia * rb - ra * ib) / norm;
      *a = ra * norm;
      a[1] = ia * norm;
   }
   else
   {
      *s = ATL_rone;
      *c = s[1] = ATL_rzero;
      *a = *b; a[1] = b[1];
   }
}
#endif
