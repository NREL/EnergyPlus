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

void Mjoin(PATL,rotmg)(TYPE *D1, TYPE *D2, TYPE *X1, const TYPE y1, TYPE *P)
{
   TYPE flag = *P;
   TYPE d1=(*D1), d2=(*D2), x1=(*X1);
   TYPE h11, h21, h12, h22, tmp, u, p1, p2, q1, q2;
   static const TYPE gam=ATL_typify(4096.0);
   static const TYPE gamsq = ATL_typify(4096.0) * ATL_typify(4096.0);
   static const TYPE rgam = ATL_rone / ATL_typify(4096.0);
   static const TYPE rgamsq = ATL_rone /
                              (ATL_typify(4096.0) * ATL_typify(4096.0));

   if (d1 < ATL_rzero)
   {
      *P = ATL_rnone;
      *D1 = *D2 = *X1 = P[1] = P[2] = P[3] = P[4] = ATL_rzero;
      return;
   }

   p2 = d2 * y1;
   if (p2 == ATL_rzero)
   {
      *P = -2.0;
      return;
   }

   p1 = d1 * x1;
   q2 = p2 * y1;
   q1 = p1 * x1;
   if (Mabs(q1) > Mabs(q2))
   {
      h21 = -y1 / x1;
      h12 = p2 / p1;
      u = ATL_rone - h12 * h21;
      if (u <= ATL_rzero)
      {
         *P = ATL_rnone;
         *D1 = *D2 = *X1 = P[1] = P[2] = P[3] = P[4] = ATL_rzero;
         return;
      }
      flag = ATL_rzero;
      d1 = d1 / u;
      d2 = d2 / u;
      x1 = x1 * u;
   }
   else
   {
      if (q2 < ATL_rzero)
      {
         *P = ATL_rnone;
         *D1 = *D2 = *X1 = P[1] = P[2] = P[3] = P[4] = ATL_rzero;
         return;
      }
      flag = ATL_rone;
      h11 = p1 / p2;
      h22 = x1 / y1;
      u = ATL_rone + h11 * h22;
      tmp = d2 / u;
      d2 = d1 / u;
      d1 = tmp;
      x1 = y1 * u;
   }

   if (d1 <= rgamsq)
   {
      if (d1 != ATL_rzero)
      {
         if (flag == ATL_rzero) { flag = ATL_rnone; h11 = h22 = ATL_rone; }
         else if (flag > ATL_rzero) { flag = h21 = ATL_rnone; h12 = ATL_rone; }
         do
         {
            d1 *= gamsq;
            x1 *= rgam;
            h11 *= rgam;
            h12 *= rgam;
         }
         while (d1 <= gamsq);
      }
   }
   else if (d1 >= gamsq)
   {
      if (flag == ATL_rzero) { flag = ATL_rnone; h11 = h22 = ATL_rone; }
      else if (flag > ATL_rzero) { flag = h21 = ATL_rnone; h12 = ATL_rone; }
      do
      {
         d1 *= rgamsq;
         x1 *= gam;
         h11 *= gam;
         h12 *= gam;
      }
      while (d1 >= gamsq);
   }

   tmp = Mabs(d2);
   if (tmp <= rgamsq)
   {
      if (d2 != ATL_rzero)
      {
         if (flag == ATL_rzero) { flag = ATL_rnone; h11 = h22 = ATL_rone; }
         else if (flag > ATL_rzero) { flag = h21 = ATL_rnone; h12 = ATL_rone; }
         if (d2 > ATL_rzero)
         {
            do
            {
               d2 *= gamsq;
               h21 *= rgam;
               h22 *= rgam;
            }
            while(d2 <= rgamsq);
         }
         else /* d2 < ATL_rzero */
         {
            tmp = -rgamsq;
            do
            {
               d2 *= gamsq;
               h21 *= rgam;
               h22 *= rgam;
            }
            while(d2 >= tmp);
         }
      }
   }
   else if (tmp >= gamsq)
   {
      if (flag == ATL_rzero) { flag = ATL_rnone; h11 = h22 = ATL_rone; }
      else if (flag > ATL_rzero) { flag = h21 = ATL_rnone; h12 = ATL_rone; }
      if (d2 > ATL_rzero)
      {
         do
         {
            d2 *= rgamsq;
            h21 *= gam;
            h22 *= gam;
         }
         while(d2 >= gamsq);
      }
      else /* d2 < ATL_rzero */
      {
         tmp = -gamsq;
         do
         {
            d2 *= rgamsq;
            h21 *= gam;
            h22 *= gam;
         }
         while(d2 <= tmp);
      }
   }
   *D1 = d1;
   *D2 = d2;
   *X1 = x1;
   *P = flag;
   if (flag == ATL_rnone) { P[1] = h11; P[2] = h21; P[3] = h12; P[4] = h22; }
   else if (flag == ATL_rzero) { P[2] = h21; P[3] = h12; }
   else if (flag == ATL_rone) { P[1] = h11; P[4] = h22; }
}
