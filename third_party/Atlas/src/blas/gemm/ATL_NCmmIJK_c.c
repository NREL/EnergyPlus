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
#include Mstr(Mjoin(Mjoin(atlas_,PRE),NCmm.h))
#include "atlas_lvl3.h"

#ifndef MB
   #define MB NB
#endif
#ifndef KB
   #define KB NB
#endif
#define NBnam Mjoin(Mjoin(Mjoin(Mjoin(MB,x),NB),x),KB)
#define NCmm0 Mjoin(Mjoin(PATL,JIK),NBnam)
#define NCmm00 Mjoin(PATL,JIK)
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

#ifndef ATL_MaxMMalpha
   #define ATL_MaxMMalpha 3
#endif
#define mm_bX Mjoin(NCmm0,_a1_b0)
#define mm_b1 Mjoin(NCmm0,_a1_b1)
#ifndef MB
   #define MB NB
#endif
#ifndef KB
   #define KB NB
#endif

int Mjoin(PATL,NCmmIJK_c)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda0, const TYPE *B, const int ldb0,
    const SCALAR beta, TYPE *C, const int ldc0)
/*
 * IJK loop-ordered matmul with no matrix copy
 */
{
   size_t incAk, incAm, incAn, incBk, incBm, incBn;
   const size_t lda=lda0, ldb=ldb0, ldc=ldc0;
   const int Mb = M / MB, Nb = N / NB, Kb = K / KB;
   const int mr = M - Mb*MB, nr = N - Nb*NB, kr = K - Kb*KB;
   const size_t incCn = ldc*NB, incCm = MB - Nb * incCn;
   int i, j, k;
   const TYPE *a=A, *b=B;
   TYPE *c=C;
   TYPE btmp;
   void *vp;
   TYPE *cp;
   void (*geadd)(const int M, const int N, const SCALAR scalar, const TYPE *A,
                  const int lda, const SCALAR beta, TYPE *C, const int ldc);
   void (*mm_bX)(const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc);
   void (*mm_b1)(const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc);
   void (*mmcu) (const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc);
   void (*mm_fixedKcu)(const int M, const int N, const int K,
                       const SCALAR alpha, const TYPE *A, const int lda,
                       const TYPE *B, const int ldb, const
                       SCALAR beta, TYPE *C, const int ldc);

   if (TA == AtlasNoTrans)
   {
      if (TB == AtlasNoTrans)
      {
         mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0);
         mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX);
      }
      else
      {
         mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0);
         mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX);
      }
      incAk = lda * KB;
      incAn = -Kb * incAk;
      incAm = MB;
   }
   else
   {
      if (TB == AtlasNoTrans)
      {
         mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0);
         mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX);
      }
      else
      {
         mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
         mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
      incAk = KB;
      incAn = -Kb * incAk;
      incAm = MB * lda;
   }
   if (TB == AtlasNoTrans)
   {
      incBk = KB;
      incBn = ldb*NB - K + kr;
      incBm = -Nb * ldb * NB;
   }
   else
   {
      incBk = KB*ldb;
      incBn = NB - Kb*incBk;
      incBm = -Nb*NB;
   }

   if (alpha == ATL_rone)
   {
      if (beta == ATL_rzero) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_b0);
      else if (beta == ATL_rone)
         geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_b1);
      else geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_bX);
   }
   else if (beta == ATL_rzero)
      geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_b0);
   else if (beta == ATL_rone)
      geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_b1);
   else geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_bX);
   vp = malloc(ATL_Cachelen + ATL_MulBySize(MB * NB));
   ATL_assert(vp);
   cp = ATL_AlignPtr(vp);
   if (mr || nr || kr) for (j=MB*NB, i=0; i != j; i++) cp[i] = ATL_rzero;

   for (i=Mb; i; i--, a += incAm, b += incBm, c += incCm)
   {
      for (j=Nb; j; j--, a += incAn, b += incBn, c += incCn)
      {
         if (Kb)
         {
            mm_bX(MB, NB, KB, ATL_rone, a, lda, b, ldb, ATL_rzero, cp, MB);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
               mm_b1(MB, NB, KB, ATL_rone, a, lda, b, ldb, ATL_rone, cp, MB);
            if (kr)
               mmcu(MB, NB, kr, ATL_rone, a, lda, b, ldb, ATL_rone, cp, MB);
         }
         else if (kr)
         {
            Mjoin(PATL,zero)(MB*NB, cp, 1); /* kill NaN/INF from last time */
            mmcu(MB, NB, kr, ATL_rone, a, lda, b, ldb, ATL_rzero, cp, MB);
         }
         geadd(MB, NB, alpha, cp, MB, beta, c, ldc);
      }
   }
   if (mr)  /* M-loop remainder */
   {
      for (j=Nb; j; j--, a += incAn, b += incBn, c += incCn)
      {
         Mjoin(PATL,zero)(MB*NB, cp, 1); /* kill NaN and INF from last time */
         if (Kb)
         {
            mm_fixedKcu(mr, NB, KB, ATL_rone, a, lda, b, ldb, ATL_rzero,
                        cp, MB);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
               mm_fixedKcu(mr, NB, KB, ATL_rone, a, lda, b, ldb, ATL_rone,
                           cp, MB);
            if (kr)
               mmcu(mr, NB, kr, ATL_rone, a, lda, b, ldb, ATL_rone, cp, MB);
         }
         else if (kr)
            mmcu(mr, NB, kr, ATL_rone, a, lda, b, ldb, ATL_rzero, cp, MB);
         geadd(mr, NB, alpha, cp, MB, beta, c, ldc);
      }
   }
   if (nr)
      ATL_assert(Mjoin(PATL,NCmmJIK)(TA, TB, M, nr, K, alpha, A, lda,
                                     B+Nb*(incBn+Kb*incBk), ldb,
                                     beta, C+Nb*NB*ldc, ldc) == 0);
   free(vp);
   return(0);
}
