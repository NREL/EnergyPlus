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
#include "atlas_level3.h"
#ifdef SCPLX
   #include "smm.h"
#else
   #include "dmm.h"
#endif
#ifndef ATL_MaxMalloc   /* temp, defined in atlas_lvl3.h */
   #define ATL_MaxMalloc 16777216
#endif

typedef void (*MAT2BLK3)(const int, const int, const SCALAR, const TYPE*,
                         const int, TYPE*, const int, TYPE*, const int);

void Mjoin(PATLU,pNBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATLU,pMBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm_bX)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void NBmm_bX(const int M, const int N, const int K,
             const TYPE alpha, const TYPE *A, const int lda,
             const TYPE *B, const int ldb, const TYPE beta,
             TYPE *C, const int ldc);
void Mjoin(PATLU,pNBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATLU,pMBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm_b1)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void NBmm_b1(const int M, const int N, const int K,
             const TYPE alpha, const TYPE *A, const int lda,
             const TYPE *B, const int ldb, const TYPE beta,
             TYPE *C, const int ldc);
void Mjoin(PATLU,pNBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATLU,pMBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm_b0)(const int M, const int N, const int K,
                          const TYPE alpha, const TYPE *A, const int lda,
                          const TYPE *B, const int ldb, const TYPE beta,
                          TYPE *C, const int ldc);
void NBmm_b0(const int M, const int N, const int K,
             const TYPE alpha, const TYPE *A, const int lda,
             const TYPE *B, const int ldb, const TYPE beta,
             TYPE *C, const int ldc);
void Mjoin(PATLU,pKBmm)(const int M, const int N, const int K,
                        const TYPE alpha, const TYPE *A, const int lda,
                        const TYPE *B, const int ldb, const TYPE beta,
                        TYPE *C, const int ldc);
void Mjoin(PATL,row2blkT_a1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,col2blk_a1)(int, int, const TYPE*, int, TYPE*, const SCALAR);
void Mjoin(PATL,gereal2cplx)
   (const int M, const int N, const TYPE *alpha, const TYPE *R, const int ldr,
    const TYPE *I, const int ldi, const TYPE *beta, TYPE *C, const int ldc);


static void ATL_gecplx2real_a1
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *pR, const int ldr, TYPE *pI, const int ldi)
/*
 * Splits real & imag components of A into separate real arrays R/I.
 */
{
   const int incA = (lda-M)<<1;
   int i, j;

/*
 * Copy backwards so 1st part of matrix is LRU
 */
   A += ((N-1)*lda+M-1)<<1;
   pR += (N-1)*ldr;
   pI += (N-1)*ldi;

   for (j=N; j; j--, A -= incA, pR -= ldr, pI -= ldi)
   {
      for (i=M-1; i >= 0; i--, A -= 2)
      {
         pR[i] = *A;
         pI[i] = A[1];
      }
   }
}
static void ATL_gecplx2realT_a1
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *pR, const int ldr, TYPE *pI, const int ldi)
/*
 * Splits real & imag components of A' into separate real arrays R/I.
 * Output matrix is MxN, so A must be NxM
 */
{
   const int lda2 = (lda-N)<<1, incR = 1-N*ldr, incI = 1-N*ldi;
   int i, j;
/*
 * Loop over M cols of A
 */
   for (i=M; i; i--, A += lda2, pR += incR, pI += incI)
   {
      for (j=N; j; j--, A += 2, pR += ldr, pI += ldi)
      {
         *pR = *A;
         *pI = A[1];
      }
   }
}
static void ATL_gecplx2realConj_a1
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *pR, const int ldr, TYPE *pI, const int ldi)
/*
 * Splits real & imag components of A into separate real arrays R/I.
 */
{
   const int incA = (lda-M)<<1;
   int i, j;

/*
 * Copy backwards so 1st part of matrix is LRU
 */
   A += ((N-1)*lda+M-1)<<1;
   pR += (N-1)*ldr;
   pI += (N-1)*ldi;

   for (j=N; j; j--, A -= incA, pR -= ldr, pI -= ldi)
   {
      for (i=M-1; i >= 0; i--, A -= 2)
      {
         pR[i] = *A;
         pI[i] = -A[1];
      }
   }
}
static void ATL_gecplx2realC_a1
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    TYPE *pR, const int ldr, TYPE *pI, const int ldi)
/*
 * Splits real & imag components of A' into separate real arrays R/I.
 * Output matrix is MxN, so A must be NxM
 */
{
   const int lda2 = (lda-N)<<1, incR = 1-N*ldr, incI = 1-N*ldi;
   int i, j;
/*
 * Loop over M cols of A
 */
   for (i=M; i; i--, A += lda2, pR += incR, pI += incI)
   {
      for (j=N; j; j--, A += 2, pR += ldr, pI += ldi)
      {
         *pR = *A;
         *pI = -A[1];
      }
   }
}

static void Mjoin(PATL,mmK)
   (int M,  /* true # of rows in row-panel, M <= MB */
    int N,  /* true # of cols in col-panel, N < = NB */
    int nblk, /* # of blocks in K dimension */
    int KR,   /* KR = K - nKb*KB; */
    const TYPE *A, /* array to copy from, NULL if already cp */
    const int lda,  /* leading dimension of A */
    const int incA, /* inc to next blk in A */
    const TYPE *alpha,
    TYPE *pA,       /* wrkspace to copy A to */
    const int incAW, /* 0 : keep using same KBxMB space */
    const TYPE *B, /* array to copy from, NULL if already cp */
    const int ldb,  /* leading dimension of B */
    const int incB, /* inc to next blk in B */
    TYPE *pB,       /* wrkspace to copy B to */
    const int incBW, /* 0 : keep using same KBxNB space */
    const TYPE *beta,
    TYPE *C,         /* output matrix */
    const int ldc,
    TYPE *pC,       /* ldpc x NB workspace */
    const int ldpc,
    MAT2BLK3 A2blk, /* rout to copy A */
    MAT2BLK3 B2blk) /* rout to copy B */
/*
 * Performs a K-inner-loop matmul, while copying A & B if necessary.
 * If M > m, we are doing extra flops so we don't call cleanup (same for N)
 * This just-in-time copy is better alg when K dim dominates M & N.
 */
{
   int m, n, kr;  /* # of row/cols to operate on, m >= M, n >= N */
   int k, ZEROED=0;
   const TYPE one[2] = {ATL_rone, ATL_rzero}, zero[2] = {ATL_rzero, ATL_rzero};
/*
 * Indexes to next blk (i.e. real to imag) of matrices; always uses full
 * block stride, even when we have a partial block
 */
   int ipb = NB*KB, ipa = MB*KB, ipc = ldpc*NB;
   void (*NBmm0)(const int, const int, const int, const TYPE,
                 const TYPE*, const int, const TYPE*, const int,
                 const TYPE, TYPE*, const int);
   void (*NBmm1)(const int, const int, const int, const TYPE,
                 const TYPE*, const int, const TYPE*, const int,
                 const TYPE, TYPE*, const int);
   void (*NBmmX)(const int, const int, const int, const TYPE,
                 const TYPE*, const int, const TYPE*, const int,
                 const TYPE, TYPE*, const int);

   m = (M < MB && M+ATL_mmMU >= MB) ? MB : M;
   n = (N < NB && N+ATL_mmNU >= NB) ? NB : N;
   if (m == MB && n == NB)
   {
      NBmm0 = NBmm_b0;
      NBmm1 = NBmm_b1;
      NBmmX = NBmm_bX;
   }
   else if (m == MB)  /* N cleanup needed */
   {
      NBmm0 = Mjoin(PATLU,pNBmm_b0);
      NBmm1 = Mjoin(PATLU,pNBmm_b1);
      NBmmX = Mjoin(PATLU,pNBmm_bX);
   }
   else if (n == NB) /* M cleanup needed */
   {
      NBmm0 = Mjoin(PATLU,pMBmm_b0);
      NBmm1 = Mjoin(PATLU,pMBmm_b1);
      NBmmX = Mjoin(PATLU,pMBmm_bX);
   }
   else  /* two or more dim < NB, requires generated cleanup */
   {
      NBmm0 = NBmm1 = NBmmX = Mjoin(PATLU,pKBmm);
/*
 *    Must zero regardless of BETA, since we call wrkspc wt beta=0
 */
      Mjoin(PATLU,gezero)(M, N, pC+ipc, ldpc);
      Mjoin(PATLU,gezero)(M, N, pC, ldpc);
      ZEROED = 1;
   }
   if (nblk)
   {
      if (B)
      {
         if (n > N)
         {
            Mjoin(PATLU,gezero)(KB, n-N, pB+KB*N, KB);
            Mjoin(PATLU,gezero)(KB, n-N, pB+ipb+KB*N, KB);
         }
         B2blk(KB, N, one, B, ldb, pB+ipb, KB, pB, KB);
         B += incB;
      }
      if (A)
      {
         if (m > M)
         {
            Mjoin(PATLU,gezero)(KB, m-M, pA+KB*M, KB);
            Mjoin(PATLU,gezero)(KB, m-M, pA+ipa+KB*M, KB);
         }
         A2blk(KB, M, one, A, lda, pA+ipa, KB, pA, KB);
         A += incA;
      }
      NBmm0(m, n, KB, ATL_rone, pA, KB, pB, KB, ATL_rzero, pC, ldpc);
      NBmm0(m, n, KB, ATL_rone, pA, KB, pB+ipb, KB, ATL_rzero, pC+ipc, ldpc);
      NBmmX(m, n, KB, ATL_rone, pA+ipa, KB, pB+ipb, KB, ATL_rnone, pC, ldpc);
      NBmm1(m, n, KB, ATL_rone, pA+ipa, KB, pB, KB, ATL_rone, pC+ipc, ldpc);
      pA += incAW; pB += incBW;
      for (k=nblk-1; k; k--)
      {
         if (B)
         {
            if (n > N)
            {
               Mjoin(PATLU,gezero)(KB, n-N, pB+KB*N, KB);
               Mjoin(PATLU,gezero)(KB, n-N, pB+ipb+KB*N, KB);
            }
            B2blk(KB, N, one, B, ldb, pB+ipb, KB, pB, KB);
            B += incB;
         }
         if (A)
         {
            if (m > M)
            {
               Mjoin(PATLU,gezero)(KB, m-M, pA+KB*M, KB);
               Mjoin(PATLU,gezero)(KB, m-M, pA+ipa+KB*M, KB);
            }
            A2blk(KB, M, one, A, lda, pA+ipa, KB, pA, KB);
            A += incA;
         }
         NBmmX(m, n, KB, ATL_rone, pA, KB, pB, KB, ATL_rnone, pC, ldpc);
         NBmm1(m, n, KB, ATL_rone, pA, KB, pB+ipb, KB, ATL_rone, pC+ipc, ldpc);
         NBmmX(m, n, KB, ATL_rone, pA+ipa, KB, pB+ipb, KB, ATL_rnone, pC, ldpc);
         NBmm1(m, n, KB, ATL_rone, pA+ipa, KB, pB, KB, ATL_rone, pC+ipc, ldpc);
         pA += incAW; pB += incBW;
      }
   }
   if (KR)  /* need to cleanup K-loop */
   {
      if (KR+4 >= KB)  /* do extra flops to avoid cleanup loop */
         kr = KB;
      else  /* must use K cleanup */
      {
         kr = KR;
         if (m < MB || n < NB) /* use general K cleanup */
         {
            n = N; m = M;
            if (!nblk && !ZEROED)
            {
               Mjoin(PATLU,gezero)(M, N, pC, ldpc);
               Mjoin(PATLU,gezero)(M, N, pC+ipc, ldpc);
            }
            NBmm1 = NBmmX = NBmm0 = Mjoin(PATLU,pKBmm);
         }
         else /* use K-only cleanup */
         {
            NBmm0 = Mjoin(PATLU,pKBmm_b0);
            NBmm1 = Mjoin(PATLU,pKBmm_b1);
            NBmmX = Mjoin(PATLU,pKBmm_bX);
         }
      }
      if (B)
      {
         if (n > N)
         {
            Mjoin(PATLU,gezero)(kr, n-N, pB+kr*N, kr);
            Mjoin(PATLU,gezero)(kr, n-N, pB+ipb+kr*N, kr);
         }
         if (kr != KR)
         {
            Mjoin(PATLU,gezero)(kr-KR, n, pB+KR, kr);
            Mjoin(PATLU,gezero)(kr-KR, n, pB+ipb+KR, kr);
         }
         B2blk(KR, N, one, B, ldb, pB+ipb, kr, pB, kr);
      }
      if (A)
      {
         if (m > M)
         {
            Mjoin(PATLU,gezero)(kr, m-M, pA+kr*M, kr);
            Mjoin(PATLU,gezero)(kr, m-M, pA+ipa+kr*M, kr);
         }
         if (kr != KR)
         {
            Mjoin(PATLU,gezero)(kr-KR, n, pA+KR, kr);
            Mjoin(PATLU,gezero)(kr-KR, n, pA+ipa+KR, kr);
         }
         A2blk(KR, M, one, A, lda, pA+ipa, kr, pA, kr);
      }
      if (nblk)
      {
         NBmmX(m, n, kr, ATL_rone, pA, kr, pB, kr, ATL_rnone, pC, ldpc);
         NBmm1(m, n, kr, ATL_rone, pA, kr, pB+ipb, kr, ATL_rone, pC+ipc, ldpc);
      }
      else
      {
         NBmm0(m, n, kr, ATL_rone, pA, kr, pB, kr, ATL_rzero, pC, ldpc);
         NBmm0(m, n, kr, ATL_rone, pA, kr, pB+ipb, kr, ATL_rzero, pC+ipc, ldpc);
      }
      NBmmX(m, n, kr, ATL_rone, pA+ipa, kr, pB+ipb, kr, ATL_rnone, pC, ldpc);
      NBmm1(m, n, kr, ATL_rone, pA+ipa, kr, pB, kr, ATL_rone, pC+ipc, ldpc);
   }
   Mjoin(PATL,gereal2cplx)(M, N, alpha, pC, ldpc, pC+ipc, ldpc, beta, C, ldc);
}

static int mmNMK
   (const int M, const int N, const int K, const int cnmblks, const int cnnblks,
    const int cnkblks, const int nkblks, const int kr, const TYPE *alpha,
    const TYPE *A, const int lda, int incAk, int incAW,
    const TYPE *B, const int ldb, int incBk, int incBW,
    const TYPE *beta, TYPE *C, const int ldc, MAT2BLK3 A2blk, MAT2BLK3 B2blk)
{
   int incAm, incAn, incBn, incW, incCn, i, j, mb, nb;
   void *vp=NULL;
   const TYPE *b;
   TYPE *pA, *pB, *pC;

   incAm = (incAk == KB*2 ? MB*(lda+lda) : 2*MB);
   incBn = (incBk == KB*2 ? NB*(ldb+ldb) : 2*NB);
   incW = (incAW ? 2*MB*cnkblks*KB : 0);
   incCn = NB*(ldc+ldc) - cnmblks*(MB+MB);
   incAn = -cnmblks * (incAW ? incW : incAm);
   i = incAW ? 2*cnmblks*MB*cnkblks*KB : 2*MB*KB; /* wrk for A */
   i += incBW ? 2*cnkblks*KB*NB : 2*NB*KB;
   i += 2*MB*NB;
   i *= sizeof(TYPE);
   if (i <= ATL_MaxMalloc)
      vp = malloc(ATL_Cachelen+i);
   if (!vp) return(-1);
   pC = ATL_AlignPtr(vp);
   pB = pC + 2*MB*NB;
   pA = pB + (incBW ? 2*cnkblks*KB*NB : 2*NB*KB);
   for (j=0; j < N; j += NB, B += incBn)
   {
      b = B;
      nb = N - j;
      nb = Mmin(NB, nb);
      for (i=0; i < M; i += MB, A += incAm)
      {
         mb = M - i;
         mb = Mmin(MB, mb);
         Mjoin(PATL,mmK)(mb, nb, nkblks, kr, A, lda, incAk, alpha, pA, incAW,
                         b, ldb, incBk, pB, incBW, beta, C, ldc, pC, MB,
                         A2blk, B2blk);
         pA += incW;
         b = incBW ? NULL : b;  /* reuse col-panel of B if copied */
         C += MB+MB;
      }
      if (incAW)  /* we have copied all of A, just reuse now */
      {
         A = NULL;
         incAm = incAk = 0;
         pA += incAn;
      }
      else
         A += incAn;
      C += incCn;
   }
   free(vp);
   return(0);
}

static int mmMNK
   (const int M, const int N, const int K, const int cnmblks, const int cnnblks,
    const int cnkblks, const int nkblks, const int kr, const TYPE *alpha,
    const TYPE *A, const int lda, int incAk, int incAW,
    const TYPE *B, const int ldb, int incBk, int incBW,
    const TYPE *beta, TYPE *C, const int ldc, MAT2BLK3 A2blk, MAT2BLK3 B2blk)
{
   int incW, incAm, incBn, incBm, incCn, incCm;
   int i, j, mb, nb;
   void *vp=NULL;
   const TYPE *a;
   TYPE *pA, *pB, *pC;

   incW = (incBW ? 2*NB*cnkblks*KB : 0);
   incAm = (incAk == 2*KB ? 2*MB*lda : 2*MB);
   incCn = (ldc+ldc)*NB;
   incCm = MB+MB - cnnblks*incCn;
   incBn = (incBk == 2*KB ? 2*NB*ldb : 2*NB);
   incBm = -cnnblks * (incBW ? incW : incBn);

   i = incAW ? 2*cnkblks*KB*MB : 2*MB*KB;           /* wrk for A */
   i += incBW ? 2*cnnblks*cnkblks*KB*NB : 2*NB*KB;  /* wrk for B */
   i += 2*MB*NB;                                    /* wrk for C */
   i *= sizeof(TYPE);
   if (i <= ATL_MaxMalloc)
      vp = malloc(ATL_Cachelen+i);
   if (!vp) return(-1);

   pC = ATL_AlignPtr(vp);
   pA = pC + 2*MB*NB;
   pB = pA + (incAW ? 2*MB*cnkblks*KB : 2*MB*KB);

   for (i=0; i < M; i += MB, A += incAm)
   {
      a = A;
      mb = M - i;
      mb = Mmin(MB, mb);
      for (j=0; j < N; j += NB)
      {
         nb = N - j;
         nb = Mmin(NB, nb);
         Mjoin(PATL,mmK)(mb, nb, nkblks, kr, a, lda, incAk, alpha, pA, incAW,
                         B, ldb, incBk, pB, incBW, beta, C, ldc, pC, MB,
                         A2blk, B2blk);
         B += incBn;
         pB += incW;
         a = incAW ? NULL : a;  /* reuse row-panel of A if copied */
         C += incCn;
      }
      if (incBW)  /* we have copied all of B, just reuse now */
      {
         B = NULL;
         incBn = incBk = 0;
         pB += incBm;
      }
      else
         B += incBm;
      C += incCm;
   }
   free(vp);
   return(0);
}

int Mjoin(PATL,mmJITcp)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                        const int M0, const int N, const int K,
                        const SCALAR alpha, const TYPE *A, const int lda,
                        const TYPE *B, const int ldb, const SCALAR beta,
                        TYPE *C, const int ldc)
/*
 * This routine copies A & B just before using them, which gets better
 * cache reuse when the copy cost is dominant (K dominates M & N).  Normally,
 * (lots of reuse in algorithm) the extra cache noise makes this a bad idea.
 */
{
   size_t incAk, incBk, incAW, incBW, incAB, incC;
   int i, j, m, n;
   const int M = (M0 >= 0) ? M0 : -M0;
   const int nkblks = (K/KB), kr = K-nkblks*KB;
   const int cnkblks=(K+KB-1)/KB, cnmblks=(M+MB-1)/MB, cnnblks=(N+NB-1)/NB;
   MAT2BLK3 A2blk, B2blk;

   if (M0 > 0)  /* normally, only copy all of matrix if reuse is possible */
   {
       incAW = (N > NB) ? KB*MB*2 : 0;
       incBW = (M > NB) ? KB*NB*2 : 0;
   }
   else  /* M0 < 0 flag to use minimal workspace */
      incAW = incBW = 0;
   if (TA == AtlasNoTrans)
   {
      incAk = lda*2*KB;
      A2blk = ATL_gecplx2realT_a1;
   }
   else if (TA == AtlasConjTrans)
   {
      incAk = KB*2;
      A2blk = ATL_gecplx2realConj_a1;
   }
   else
   {
      incAk = KB*2;
      A2blk = ATL_gecplx2real_a1;
   }
   if (TB == AtlasNoTrans)
   {
      incBk = KB*2;
      B2blk = ATL_gecplx2real_a1;
   }
   else if (TB == AtlasConjTrans)
   {
      incBk = ldb*2*KB;
      B2blk = ATL_gecplx2realC_a1;
   }
   else
   {
      incBk = ldb*2*KB;
      B2blk = ATL_gecplx2realT_a1;
   }
/*
 * If A isn't copied, or is smaller than B, copy it as inner matrix
 */
   if (M <= N || incAW)
   {
      if (mmNMK(M, N, K, cnmblks, cnnblks, cnkblks, nkblks, kr, alpha,
                A, lda, incAk, incAW, B, ldb, incBk, incBW,
                beta, C, ldc, A2blk, B2blk))
      {
         if (!incAW)
            return(-1);
         m = cnmblks;
         i = cnmblks >> 1;
/*
 *       Keep trying half-size M until we can copy all of A subsection
 */
         j = 0;       /* # of M blocks to do undefined */
         while(i > 2)
         {
            i += m - i - i;  /* # of blks to try at a whack */
            if (!mmNMK(i*MB, N, K, i, cnnblks, cnkblks, nkblks, kr, alpha,
                       A, lda, incAk, incAW, B, ldb, incBk, incBW,
                       beta, C, ldc, A2blk, B2blk))
            {
               incAB = (TA == AtlasNoTrans) ? 2*MB*i : 2*MB*lda*i;
               incC = 2*MB*i;
               j = i;
               break;
            }
            m = i;
            i >>= 1;
         }
         if (j)  /* we've handled first J rows of A */
         {
            for (i=j; i < cnmblks; i += j)
            {
               A += incAB;
               C += incC;
               if (i+j < cnmblks)
                  m = j * MB;
               else
               {
                  m = M - i*MB;
                  j = cnmblks - i;
               }
               if (mmNMK(m, N, K, j, cnnblks, cnkblks, nkblks, kr, alpha,
                         A, lda, incAk, incAW, B, ldb, incBk, incBW,
                         beta, C, ldc, A2blk, B2blk))
                  if (mmNMK(m, N, K, j, cnnblks, cnkblks, nkblks, kr, alpha,
                            A, lda, incAk, 0, B, ldb, incBk, incBW,
                            beta, C, ldc, A2blk, B2blk))
                     ATL_assert(!mmNMK(m, N, K, j, cnnblks, cnkblks, nkblks, kr,
                                       alpha, A, lda, incAk, 0, B, ldb, incBk,
                                       0, beta, C, ldc, A2blk, B2blk));
            }
         }
         else  /* just try not copying A at all */
            return(mmNMK(M, N, K, cnmblks, cnnblks, cnkblks, nkblks, kr, alpha,
                         A, lda, incAk, 0, B, ldb, incBk, incBW,
                         beta, C, ldc, A2blk, B2blk));
      }
   }
/*
 * If A is copied and is larger than B, copy B as inner matrix
 */
   else if (mmMNK(M, N, K, cnmblks, cnnblks, cnkblks, nkblks, kr, alpha,
                  A, lda, incAk, incAW, B, ldb, incBk, incBW,
                  beta, C, ldc, A2blk, B2blk))
   {
      if (!incBW)
         return(-1);
      n = cnnblks;
      i = cnnblks >> 1;
/*
 *    Keep trying half-size M until we can copy all of A subsection
 */
      j = 0;       /* # of M blocks to do undefined */
      while(i > 2)
      {
         i += n - i - i;  /* # of blks to try at a whack */
         if (!mmMNK(M, i*NB, K, cnmblks, i, cnkblks, nkblks, kr, alpha,
                    A, lda, incAk, incAW, B, ldb, incBk, incBW,
                    beta, C, ldc, A2blk, B2blk))
         {
            incAB = (TB == AtlasNoTrans) ? 2*NB*ldb*i : 2*NB*i;
            incC = 2*NB*i*ldc;
            j = i;
            break;
         }
         n = i;
         i >>= 1;
      }
      if (j)  /* we've handled first J cols of B */
      {
         for (i=j; i < cnnblks; i += j)
         {
            B += incAB;
            C += incC;
            if (i+j < cnnblks)
               n = j * MB;
            else
            {
               n = N - i*NB;
               j = cnnblks - i;
            }
            if (mmMNK(M, n, K, cnmblks, j, cnkblks, nkblks, kr, alpha,
                      A, lda, incAk, incAW, B, ldb, incBk, incBW,
                      beta, C, ldc, A2blk, B2blk))
               if (mmMNK(M, n, K, cnmblks, j, cnkblks, nkblks, kr, alpha,
                         A, lda, incAk, incAW, B, ldb, incBk, 0,
                         beta, C, ldc, A2blk, B2blk))
                  ATL_assert(!mmMNK(M, n, K, cnmblks, j, cnkblks, nkblks, kr,
                                    alpha, A, lda, incAk, 0, B, ldb, incBk, 0,
                                    beta, C, ldc, A2blk, B2blk));
         }
      }
      else  /* just try not copying A at all */
         return(mmNMK(M, N, K, cnmblks, cnnblks, cnkblks, nkblks, kr, alpha,
                      A, lda, incAk, 0, B, ldb, incBk, incBW,
                      beta, C, ldc, A2blk, B2blk));
   }
   return(0);
}
