#include "atlas_tlapack.h"
#include "atlas_pca.h"
#include "atlas_level2.h"

void Mjoin(PATL,DoWorkGETF2_nowrk)(ATL_LAUNCHSTRUCT_t *lp, void *vp)
{
   ATL_thread_t *tp=vp;
   ATL_TGETF2_M_t *lup=((ATL_TGETF2_M_t*)lp->opstruct)+tp->rank;
   int *ipiv = lup->ipiv;
   ATL_CINT M=lup->M, N=lup->N, lda=lup->lda, MN = Mmin(M,N);
   const int p = lup->p, rank = lup->rank;
   ATL_CINT mp = M/p, mr = M - mp*p;
   ATL_INT m, locpiv, globpiv, k, j, i;
   #ifdef TCPLX
      ATL_CINT lda2 = lda+lda;
   #else
      #define lda2 lda
      #define none ATL_rnone
   #endif
   TYPE *A, *Ac, *a, *v;
   TYPE pivval, apv, apv2;
   #ifdef TCPLX
      const TYPE none[2] = {ATL_rnone, ATL_rzero};
   #endif
   volatile ATL_INT *maxindx=lup->maxindx, *stage=lup->stage;
   void (*my_ger)(const int M, const int N, const SCALAR alpha,
                  const TYPE *X, const int incX,
                  const TYPE *Y, const int incY, TYPE *A, const int lda);

   #ifdef TCPLX
      my_ger = Mjoin(PATL,geru);
   #else
      my_ger = Mjoin(PATL,ger);
   #endif
   m = (rank) ? mp : mp+mr;
   Ac = A = lup->A;
   a = (rank) ? A + ((m*rank + mr)SHIFT) : A;
   for (j=0; j < MN; j++, Ac += lda2, a += lda2)
   {
      locpiv = cblas_iamax(m, a, 1);
/*
 *    Combine local pivot into global
 */
      if (!rank)
      {
         globpiv = j+locpiv;
         #ifdef TCPLX
            apv = Mabs(Ac[globpiv+globpiv]) + Mabs(Ac[globpiv+globpiv+1]);
         #else
            apv = Mabs(Ac[globpiv]);
         #endif
         for (i=1; i < p; i++)
         {
            while(stage[i] < j);
            k = maxindx[i];
            apv2 = Mabs(Ac[k SHIFT]);
            #ifdef TCPLX
               apv2 += Mabs(Ac[k+k+1]);
            #endif
            if (apv < apv2)
            {
               apv = apv2;
               globpiv = k;
            }
            maxindx[i] = -1;
         }
         ipiv[j] = globpiv;
         if (globpiv != j)
            cblas_swap(N, A+(j SHIFT), lda, A+(globpiv SHIFT), lda);
         ATL_membarrier;
         stage[0] = j;
         m--;                                           /* just finished */
         #ifdef TCPLX
            a += 2;                                     /* one row */
         #else
            a++;                                        /* one row */
         #endif
      }
      else /* all threads except 0 write their results, and await 0 */
      {
         maxindx[rank] = locpiv+rank*mp+mr;
         stage[rank] = j;
         while (stage[0] < j);
      }
      #ifdef TCPLX
         if (Ac[j+j] != ATL_rzero || Ac[j+j+1] != ATL_rzero)
         {
            TYPE inv[2];
            Mjoin(PATL,cplxinvert)(1, Ac+j+j, 1, inv, 1);
            cblas_scal(m, inv, a, 1);
         }
      #else
         pivval = Ac[j];
         if (pivval != ATL_rzero)
            cblas_scal(m, ATL_rone/pivval, a, 1);
      #endif
      else /* pivot is zero, we have a singular matrix! */
         lup->info = j;   /* all threads have same info */

      #ifdef TCPLX
         my_ger(m, N-j-1, none, a, 1, Ac+((j+lda)<<1), lda, a+lda2, lda);
         my_ger = Mjoin(PATL,geru_L2);
      #else
         my_ger(m, N-j-1, ATL_rnone, a, 1, Ac+j+lda, lda, a+lda, lda);
         my_ger = Mjoin(PATL,ger_L2);
      #endif
   }
}

void Mjoin(PATL,DoWorkGETF2)(ATL_LAUNCHSTRUCT_t *lp, void *vp0)
{
   ATL_thread_t *tp=vp0;
   ATL_TGETF2_M_t *lup=((ATL_TGETF2_M_t*)lp->opstruct)+tp->rank;
   int *ipiv = lup->ipiv;
   ATL_CINT M=lup->M, N=lup->N, lda=lup->lda, MN = Mmin(M,N);
   const int p = lup->p, rank = lup->rank;
   int pivrank;
   ATL_CINT mp = M/p, mr = M - mp*p;
   ATL_INT m, locpiv, globpiv, k, j, i, ldw, ldw0, ldw1;
   void *vp;
   TYPE *a, *W, *Wc, *w, **WRKS=lup->works, *v;
   TYPE pivval, apv, apv2, pv2;
   volatile ATL_INT *maxindx=lup->maxindx, *stage=lup->stage;
   #ifdef TCPLX
      const TYPE none[2] = {ATL_rnone, ATL_rzero};
   #endif

   m = (rank) ? mp : mp+mr;
   a = (rank) ? (((TYPE*)lup->A)+((mp*rank + mr)SHIFT)) : lup->A;
/*
 * Make ldw's a multiple of 16 bytes that is not a power of 2; 0's ldw
 * is larger by mr than all other ldws (ldw1)
 */
#if defined(DREAL) || defined(SCPLX)
   ldw0 = ((mp+mr+1)>>1)<<1;
   ldw1 = ((mp+1)>>1)<<1;
   if (!(ldw0 & (ldw0-1)))
      ldw0 += 2;
   if (!(ldw1 & (ldw1-1)))
      ldw1 += 2;
#elif defined(SREAL)
   ldw0 = ((mp+mr+3)>>2)<<2;
   ldw1 = ((mp+3)>>2)<<2;
   if (!(ldw0 & (ldw0-1)))
      ldw0 += 4;
   if (!(ldw1 & (ldw1-1)))
      ldw1 += 4;
#else
   ldw0 = mp+mr;
   ldw1 = mp;
   if (!(ldw0 & (ldw0-1)))
      ldw0++;
   if (!(ldw1 & (ldw1-1)))
      ldw1++;
#endif
   ldw = (rank) ? ldw1 : ldw0;
   vp = malloc(ATL_MulBySize(ldw)*N+ATL_Cachelen);
/*
 * If anyone fails to allocate the space, free any allocated spaces and
 * call the no-copy version
 */
   j = (vp != NULL);
   if (!rank)
   {
      for (i=1; i < p; i++)
      {
         while (stage[i] != -2);
         j &= maxindx[i];
         maxindx[i] = -1;
      }
      *maxindx = j;
      stage[0] = -2;
   }
   else
   {
      maxindx[rank] = j;
      stage[rank] = -2;
      while (stage[0] != -2);
   }
   if (*maxindx == 0)
   {
      if (vp)
         free(vp);
      Mjoin(PATL,DoWorkGETF2_nowrk)(lp, vp0);
      return;
   }
   ATL_assert(vp);
   WRKS[rank] = w = W = ATL_AlignPtr(vp);
   Mjoin(PATL,gecopy)(m, N, a, lda, W, ldw);
   for (j=0; j < MN; j++, w += (ldw SHIFT))
   {
      locpiv = cblas_iamax(m, w, 1);
/*
 *    Combine local pivot into global
 */
      if (!rank)
      {
         globpiv = j+locpiv;
         pivrank = 0;
         apv = Mabs(w[locpiv SHIFT]);
         #ifdef TCPLX
            apv += Mabs(w[locpiv+locpiv+1]);
         #endif
         for (i=1; i < p; i++)
         {
            while(stage[i] < j);
            k = maxindx[i];
            apv2 = WRKS[i][(j*ldw1+k)SHIFT];
            apv2 = Mabs(apv2);
            #ifdef TCPLX
               apv2 += Mabs(WRKS[i][((j*ldw1+k)<<1)+1]);
            #endif
            if (apv < apv2)
            {
               apv = apv2;
               globpiv = k;
               pivrank = i;
            }
            maxindx[i] = -1;
         }
         if (pivrank)
         {
            ipiv[j] = mr+pivrank*mp+globpiv;
            cblas_swap(N, W+(j SHIFT), ldw,
                       WRKS[pivrank]+(globpiv SHIFT), ldw1);
         }
         else
         {
            ipiv[j] = globpiv;
            if (globpiv != j)
               cblas_swap(N, W+(j SHIFT), ldw, W+(globpiv SHIFT), ldw);
         }
         ATL_membarrier;
         stage[0] = j;
         m--;                                           /* just finished */
         #ifdef TCPLX
            w += 2;                                     /* one row */
         #else
            w++;                                        /* one row */
         #endif
      }
      else /* all threads except 0 write their results, and await 0 */
      {
         maxindx[rank] = locpiv;
         stage[rank] = j;
         while (stage[0] < j);
      }
      #ifdef TCPLX
         v = &WRKS[0][(j*ldw0+j)SHIFT];
         if (*v != ATL_rzero || v[1] != ATL_rzero)
         {
            TYPE inv[2];
            Mjoin(PATL,cplxinvert)(1, v, 1, inv, 1);
            cblas_scal(m, inv, w, 1);
         }
      #else
         pivval = WRKS[0][j*ldw0+j];
         if (pivval != ATL_rzero)
            cblas_scal(m, ATL_rone/pivval, w, 1);
      #endif
      else /* pivot is zero, we have a singular matrix! */
         lup->info = j;   /* all threads have same info */

      #ifdef TCPLX
         Mjoin(PATL,geru_L2)(m, N-j-1, none, w, 1,
                             WRKS[0]+((j*(ldw0+1)+ldw0)SHIFT), ldw0,
                             w+ldw+ldw, ldw);
      #else
         Mjoin(PATL,ger_L2)(m, N-j-1, ATL_rnone, w, 1, WRKS[0]+j*(ldw0+1)+ldw0,
                            ldw0, w+ldw, ldw);
      #endif
   }
   stage[rank] = MN;  /* let core 0 know we are done */
/*
 * Copy answer back out of workspace and then free workspace
 */
   Mjoin(PATL,gecopy)(rank?mp:mp+mr, N, W, ldw, a, lda);
/*
 * Core 0 waits for all other cores to finish before he frees his work:
 * all non-zero cores access 0's workspace, but 0 does not access others' work
 * after iamax barrier
 */
   if (!rank)
   {
      for (i=1; i < p; i++)
         while(stage[i] != MN);
   }
   free(vp);
}

int Mjoin(PATL,StructIsInitGETF2)(void *vp)
{
   return(((ATL_TGETF2_M_t*)vp)->M);
}

int Mjoin(PATL,tgetf2_nocp)(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT lda, int *ipiv)
{
   ATL_TGETF2_M_t lu2s[ATL_NTHREADS];
   ATL_INT maxindx[ATL_NTHREADS], stage[ATL_NTHREADS];
   TYPE *works[ATL_NTHREADS];

   ATL_CINT MN = Mmin(M,N);
   ATL_INT p = ATL_NTHREADS, m, mr, i, j;

   if (M < 1 || N < 1)
      return(0);
   m = M / ATL_NTHREADS;
   mr = M - m*ATL_NTHREADS;
/*
 * This logic is necessary since tgetf2 assumes only one processor owns entire
 * logical block.  Can remove if we rewrite tgetf2 to allow the diagonal to
 * span multiple processors
 */
   if (m+mr < N)
   {
      p = M / N;
      if (p)
         m = M / p;
   }
   if (p < 2)   /* not enough rows, call serial algorithm */
      return(Mjoin(PATL,getf2)(M, N, A, lda, ipiv));
   for (i=0; i < p; i++)
   {
      stage[i] = maxindx[i] = -1;
      lu2s[i].M = M;
      lu2s[i].N = N;
      lu2s[i].A = A;
      lu2s[i].lda = lda;
      lu2s[i].ipiv = ipiv;  /* only thread 0 will write ipiv */
      lu2s[i].info = 0;
      lu2s[i].maxindx = maxindx;
      lu2s[i].stage = stage;
      lu2s[i].p = p;
      lu2s[i].rank = i;
      lu2s[i].works = works;
   }
   for (; i < ATL_NTHREADS; i++)
      lu2s[i].M = 0;
   ATL_goparallel(p, Mjoin(PATL,DoWorkGETF2), lu2s, NULL);
   return(lu2s[0].info);
}
int Mjoin(PATL,tgetf2)(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT lda, int *ipiv)
{
   ATL_TGETF2_M_t lu2s[ATL_NTHREADS];
   ATL_INT maxindx[ATL_NTHREADS], stage[ATL_NTHREADS];
   TYPE *works[ATL_NTHREADS];

   ATL_CINT MN = Mmin(M,N);
   ATL_INT p = ATL_NTHREADS, m, mr, i, j;

   if (M < 1 || N < 1)
      return(0);
   m = M / ATL_NTHREADS;
   mr = M - m*ATL_NTHREADS;
/*
 * This logic is necessary since tgetf2 assumes only one processor owns entire
 * logical block.  Can remove if we rewrite tgetf2 to allow the diagonal to
 * span multiple processors
 */
   if (m+mr < N)
   {
      p = M / N;
      if (p)
         m = M / p;
   }
   if (p < 2)   /* not enough rows, call serial algorithm */
      return(Mjoin(PATL,getf2)(M, N, A, lda, ipiv));
   for (i=0; i < p; i++)
   {
      stage[i] = maxindx[i] = -1;
      lu2s[i].M = M;
      lu2s[i].N = N;
      lu2s[i].A = A;
      lu2s[i].lda = lda;
      lu2s[i].ipiv = ipiv;  /* only thread 0 will write ipiv */
      lu2s[i].info = 0;
      lu2s[i].maxindx = maxindx;
      lu2s[i].stage = stage;
      lu2s[i].p = p;
      lu2s[i].rank = i;
      lu2s[i].works = works;
   }
   for (; i < ATL_NTHREADS; i++)
      lu2s[i].M = 0;
   ATL_goparallel(p, Mjoin(PATL,DoWorkGETF2), lu2s, NULL);
   return(lu2s[0].info);
}
#ifndef TCPLX
   #undef lda2
#endif
