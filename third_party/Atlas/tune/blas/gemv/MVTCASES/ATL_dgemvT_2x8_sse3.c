#ifndef ATL_SSE3
   #error "This routine requires SSE3!"
#endif

#include <xmmintrin.h>
#include "atlas_misc.h"
#define _my_hadd_pd(dst, src) \
   __asm__ __volatile__ ("haddpd %2, %0" : "=x"(dst) : "0" (dst), "x"(src))

void ATL_UGEMV(ATL_CINT M, ATL_CINT N, const TYPE *A, ATL_CINT lda1,
               const TYPE *X, TYPE *Y)
{/* BEGIN GEMV: nMU=1, MU=2, NU=8 */
   ATL_INT i, j;
   ATL_CINT MAp = ((((size_t)A)&0xF) || M==1) ? 1 : 2;
   ATL_CINT MA = M - MAp;
   #define A0 A
   const TYPE *A1=A0+lda1, *A2=A1+lda1, *A3=A2+lda1, *A4=A3+lda1, *A5=A4+lda1, *A6=A5+lda1, *A7=A6+lda1;
   ATL_CINT M2=((((((MA) >> 1)) << 1)))+MAp, N8=(((((N) >> 3)) << 3)), lda8=(((lda1) << 3));
   __m128d x0, x1, y0, y1, y2, y3, y4, y5, y6, y7, a0_0, a0_1, a0_2, a0_3, a0_4, a0_5, a0_6, a0_7;
   if (!M || !N) return;

   for (j=0; j < N8; j += 8, A0 += lda8, A1 += lda8, A2 += lda8, A3 += lda8, A4 += lda8, A5 += lda8, A6 += lda8, A7 += lda8)
   {/* BEGIN N-LOOP UR=8 */
      if (MAp != 1)
      {/* peel to zero Y */
         i=0;
         x0 = _mm_load_pd(X+i+0);
         y0 = _mm_load_pd(A0+i);
         y0 = _mm_mul_pd(y0, x0);
         y1 = _mm_load_pd(A1+i);
         y1 = _mm_mul_pd(y1, x0);
         y2 = _mm_load_pd(A2+i);
         y2 = _mm_mul_pd(y2, x0);
         y3 = _mm_load_pd(A3+i);
         y3 = _mm_mul_pd(y3, x0);
         y4 = _mm_load_pd(A4+i);
         y4 = _mm_mul_pd(y4, x0);
         y5 = _mm_load_pd(A5+i);
         y5 = _mm_mul_pd(y5, x0);
         y6 = _mm_load_pd(A6+i);
         y6 = _mm_mul_pd(y6, x0);
         y7 = _mm_load_pd(A7+i);
         y7 = _mm_mul_pd(y7, x0);
      } /* end zero Y peel */
      else /* if (MAp == 1)*/
      {/* peel to force X/A alignment, zero Y */
         i=0;
         x0 = _mm_load_sd(X+i+0);
         y0 = _mm_load_sd(A0+i);
         y0 = _mm_mul_sd(y0, x0);
         y1 = _mm_load_sd(A1+i);
         y1 = _mm_mul_sd(y1, x0);
         y2 = _mm_load_sd(A2+i);
         y2 = _mm_mul_sd(y2, x0);
         y3 = _mm_load_sd(A3+i);
         y3 = _mm_mul_sd(y3, x0);
         y4 = _mm_load_sd(A4+i);
         y4 = _mm_mul_sd(y4, x0);
         y5 = _mm_load_sd(A5+i);
         y5 = _mm_mul_sd(y5, x0);
         y6 = _mm_load_sd(A6+i);
         y6 = _mm_mul_sd(y6, x0);
         y7 = _mm_load_sd(A7+i);
         y7 = _mm_mul_sd(y7, x0);
      } /* end force-align/zeroY peel */

      for (i=MAp; i < M2; i += 2)
      {/* ----- BEGIN M-LOOP BODY ----- */
         /* --- BEGIN MUxNU UNROLL 0 --- */
         x0 = _mm_load_pd(X+i+0);
         a0_0 = _mm_load_pd(A0+i);
         a0_0 = _mm_mul_pd(a0_0, x0);
         y0 = _mm_add_pd(y0, a0_0);

         a0_1 = _mm_load_pd(A1+i);
         a0_1 = _mm_mul_pd(a0_1, x0);
         y1 = _mm_add_pd(y1, a0_1);

         a0_2 = _mm_load_pd(A2+i);
         a0_2 = _mm_mul_pd(a0_2, x0);
         y2 = _mm_add_pd(y2, a0_2);

         a0_3 = _mm_load_pd(A3+i);
         a0_3 = _mm_mul_pd(a0_3, x0);
         y3 = _mm_add_pd(y3, a0_3);

         a0_4 = _mm_load_pd(A4+i);
         a0_4 = _mm_mul_pd(a0_4, x0);
         y4 = _mm_add_pd(y4, a0_4);

         a0_5 = _mm_load_pd(A5+i);
         a0_5 = _mm_mul_pd(a0_5, x0);
         y5 = _mm_add_pd(y5, a0_5);

         a0_6 = _mm_load_pd(A6+i);
         a0_6 = _mm_mul_pd(a0_6, x0);
         y6 = _mm_add_pd(y6, a0_6);

         a0_7 = _mm_load_pd(A7+i);
         a0_7 = _mm_mul_pd(a0_7, x0);
         y7 = _mm_add_pd(y7, a0_7);

         /* --- END MUxNU UNROLL 0 --- */
      }/* ----- END M-LOOP BODY ----- */
      if (M != M2)
      {/* ----- BEGIN SCALAR M CLEANUP ----- */
         x0 = _mm_load_sd(X+i+0);
         a0_0 = _mm_load_sd(A0+i);
         a0_0 = _mm_mul_sd(a0_0, x0);
         y0 = _mm_add_sd(y0, a0_0);

         a0_1 = _mm_load_sd(A1+i);
         a0_1 = _mm_mul_sd(a0_1, x0);
         y1 = _mm_add_sd(y1, a0_1);

         a0_2 = _mm_load_sd(A2+i);
         a0_2 = _mm_mul_sd(a0_2, x0);
         y2 = _mm_add_sd(y2, a0_2);

         a0_3 = _mm_load_sd(A3+i);
         a0_3 = _mm_mul_sd(a0_3, x0);
         y3 = _mm_add_sd(y3, a0_3);

         a0_4 = _mm_load_sd(A4+i);
         a0_4 = _mm_mul_sd(a0_4, x0);
         y4 = _mm_add_sd(y4, a0_4);

         a0_5 = _mm_load_sd(A5+i);
         a0_5 = _mm_mul_sd(a0_5, x0);
         y5 = _mm_add_sd(y5, a0_5);

         a0_6 = _mm_load_sd(A6+i);
         a0_6 = _mm_mul_sd(a0_6, x0);
         y6 = _mm_add_sd(y6, a0_6);

         a0_7 = _mm_load_sd(A7+i);
         a0_7 = _mm_mul_sd(a0_7, x0);
         y7 = _mm_add_sd(y7, a0_7);

      }/* ----- END SCALAR M CLEANUP ----- */
      _my_hadd_pd(y0, y1);
      #ifndef BETA0
         a0_0 = _mm_load_pd(Y+j+0);
         y0 = _mm_add_pd(y0, a0_0);
      #endif
      _mm_store_pd(Y+j+0, y0);
      _my_hadd_pd(y2, y3);
      #ifndef BETA0
         a0_1 = _mm_load_pd(Y+j+2);
         y2 = _mm_add_pd(y2, a0_1);
      #endif
      _mm_store_pd(Y+j+2, y2);
      _my_hadd_pd(y4, y5);
      #ifndef BETA0
         a0_2 = _mm_load_pd(Y+j+4);
         y4 = _mm_add_pd(y4, a0_2);
      #endif
      _mm_store_pd(Y+j+4, y4);
      _my_hadd_pd(y6, y7);
      #ifndef BETA0
         a0_3 = _mm_load_pd(Y+j+6);
         y6 = _mm_add_pd(y6, a0_3);
      #endif
      _mm_store_pd(Y+j+6, y6);
   }/* END N-LOOP UR=8 */

   for (j=N8; j < N; j++, A0 += lda1)
   {/* BEGIN N-LOOP UR=1 */
      if (MAp != 1)
      {/* peel to zero Y */
         i=0;
         x0 = _mm_load_pd(X+i+0);
         y0 = _mm_load_pd(A0+i);
         y0 = _mm_mul_pd(y0, x0);
      } /* end zero Y peel */
      else /* if (MAp == 1)*/
      {/* peel to force X/A alignment, zero Y */
         i=0;
         x0 = _mm_load_sd(X+i+0);
         y0 = _mm_load_sd(A0+i);
         y0 = _mm_mul_sd(y0, x0);
      } /* end force-align/zeroY peel */

      for (i=MAp; i < M2; i += 2)
      {/* ----- BEGIN M-LOOP BODY ----- */
         /* --- BEGIN MUxNU UNROLL 0 --- */
         x0 = _mm_load_pd(X+i+0);
         a0_0 = _mm_load_pd(A0+i);
         a0_0 = _mm_mul_pd(a0_0, x0);
         y0 = _mm_add_pd(y0, a0_0);

         /* --- END MUxNU UNROLL 0 --- */
      }/* ----- END M-LOOP BODY ----- */
      if (M != M2)
      {/* ----- BEGIN SCALAR M CLEANUP ----- */
         x0 = _mm_load_sd(X+i+0);
         a0_0 = _mm_load_sd(A0+i);
         a0_0 = _mm_mul_sd(a0_0, x0);
         y0 = _mm_add_sd(y0, a0_0);

      }/* ----- END SCALAR M CLEANUP ----- */
      _my_hadd_pd(y0, y0);
      #ifndef BETA0
         a0_0 = _mm_load_sd(Y+j+0);
         y0 = _mm_add_sd(y0, a0_0);
      #endif
      _mm_store_sd(Y+j+0, y0);
   }/* END N-LOOP UR=1 */
}/* END GEMV: nMU=1, MU=2, NU=8 */
#ifdef MA
   #undef MA
#endif
#ifdef MAp
   #undef MAp
#endif
#ifdef A0
   #undef A0
#endif
