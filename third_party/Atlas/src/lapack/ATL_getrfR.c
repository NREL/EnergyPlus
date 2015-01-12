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
#include "atlas_lvl3.h"
#include "atlas_level3.h"
#include "atlas_level1.h"
#include "atlas_lapack.h"
#include "atlas_lamch.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))
#ifdef ATL_USEPTHREADS
   #include "atlas_pthreads.h"
   #include "atlas_tcacheedge.h"
#else
   #include "atlas_cacheedge.h"
#endif


#ifdef TREAL
   #define ATL_luMmin 2
   #define ATL_PCAMin 1500
#else
   #define ATL_luMmin 1
   #ifdef SCPLX
      #define ATL_PCAMin 256
   #else
      #define ATL_PCAMin 512
   #endif
#endif

#ifdef TCPLX
   #define ATL_CplxInv(in, out) Mjoin(PATL,cplxinvert)(1, in, 1, out, 1);
#endif

int ATL_getrfR(const int M, const int N, TYPE *A, const int lda, int *ipiv)
/*
 * Row-major factorization of form
 *   A = L * U * P
 * where P is a column-permutation matrix, L is lower triangular (lower
 * trapazoidal if M > N), and U is upper triangular with unit diagonals (upper
 * trapazoidal if M < N).  This is the recursive Level 3 BLAS version.
 */
{
   const int MN = Mmin(M, N);
   int Nup, Ndown, i, k, ierr=0;
   #ifdef TCPLX
      const TYPE one[2] = {ATL_rone, ATL_rzero};
      const TYPE none[2] = {ATL_rnone, ATL_rzero};
      TYPE inv[2], tmp[2];
   #else
      #define one ATL_rone
      #define none ATL_rnone
      TYPE tmp;
   #endif
   TYPE *Ar, *Ac, *An;

   if (MN > 1)
   {
      Nup = MN >> 1;
      #ifdef NB
         if (Nup > NB) Nup = ATL_MulByNB(ATL_DivByNB(Nup));
      #endif
      Ndown = M - Nup;
      i = ATL_getrfR(Nup, N, A, lda, ipiv);
      if (i) if (!ierr) ierr = i;
      Ar = A + (Nup * lda SHIFT);
      Ac = A + (Nup SHIFT);
      An = Ar + (Nup SHIFT);

      ATL_laswp(Ndown, Ar, lda, 0, Nup, ipiv, 1);  /* apply pivots */
      cblas_trsm(CblasRowMajor, CblasRight, CblasUpper, CblasNoTrans,
                 CblasUnit, Ndown, Nup, one, A, lda, Ar, lda);
      cblas_gemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, Ndown, N-Nup, Nup,
                 none, Ar, lda, Ac, lda, one, An, lda);

      i = ATL_getrfR(Ndown, N-Nup, An, lda, ipiv+Nup);
      if (i) if (!ierr) ierr = Nup + i;
      for (i=Nup; i != MN; i++) ipiv[i] += Nup;
      ATL_laswp(Nup, A, lda, Nup, MN, ipiv, 1);  /* apply pivots */
   }
   else if (MN == 1)
   {
      *ipiv = i = cblas_iamax(N, A, 1);
      #ifdef TREAL
         tmp = A[i];
         if (tmp != ATL_rzero)
         {
            if (Mabs(tmp) >= ATL_laSAFMIN)
               cblas_scal(N, ATL_rone/tmp, A, 1);
            else
            {
               for (k=0; k < N; k++)
                  A[k] /= tmp;
            }
            A[i] = *A;
            *A = tmp;
         }
         else ierr = 1;
      #else
         i <<= 1;
         tmp[0] = A[i];
         tmp[1] = A[i+1];
         if (tmp[0] != ATL_rzero || tmp[1] != ATL_rzero)
         {
            if (ATL_lapy2(tmp[0], tmp[1]) >= ATL_laSAFMIN)
            {
               ATL_CplxInv(tmp, inv);
               cblas_scal(N, inv, A, 1);
            }
            else
               Mjoin(PATL,cplxdivide)(N, tmp, A, 1, A, 1);
            A[i] = *A;
            A[i+1] = A[1];
            *A = tmp[0];
            A[1] = tmp[1];
         }
         else ierr = 1;
      #endif
   }
   return(ierr);
}
