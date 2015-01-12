/*
 * (C) Copyright IBM Corporation 2010
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

#ifndef ATL_VSX
   #error "This routine requires VSX!"
#endif
#include <altivec.h>
#include "atlas_misc.h"

#define VEC_STEP (vec_step(vector TYPE))

#ifdef DREAL
#ifdef __GNUC__
/* The AT 2.1 compiler does not implement vec_mergeh/vec_mergel for doubles */
#undef vec_mergeh
static inline vector double vec_mergeh(vector double a, vector double b)
{ return __builtin_vsx_xxpermdi_2df (a, b, 0); }
#undef vec_mergel
static inline vector double vec_mergel(vector double a, vector double b)
{ return __builtin_vsx_xxpermdi_2df (a, b, 3); }
#else
/* Assume no hack needed for vec_mergeh/vec_mergel */
#endif
#endif

#ifdef SREAL
#define ELEM0 (vector bool int) { 0, -1, -1, -1 }
#define ELEM1 (vector bool int) { -1, 0, -1, -1 }
#define ELEM2 (vector bool int) { -1, -1, 0, -1 }
#define ELEM3 (vector bool int) { -1, -1, -1, 0 }

static inline vector float vec_reduce( vector float v ) {
    v = vec_add( v, vec_sld( v, v, 8 ) );
    v = vec_add( v, vec_sld( v, v, 4 ) );
    return ( v );
}
#endif

/* GEMV with the following assumptions:
 * 1) alpha = 1
 * 2) beta = 0 or 1 (controlled by BETA0/BETA1)
 * 3) incX = 1 and incY = 1
 * 4) Column-major storage of A
 * 4) A is stored column major but is transposed.
 *
 * y = [0,1]*y + A*x, len(X) = M, len(Y) = N
 * At is MxN and is the transpose of A (NxM)
 */
void ATL_UGEMV(
    const int M, const int N,
    const TYPE *At, const int lda,
    const TYPE *X, TYPE *Y)
{
    int i, j;

    const long int mu = VEC_STEP*3;  /* Unrolling used for the X vector */
    const long int nu = 8;           /* Unrolling used for the Y vector */

    vector TYPE vzero = vec_splats( ((TYPE) 0.0) );

    long int M1 = (M/mu)*mu;
    long int N1 = (N/nu)*nu;

    TYPE *py = &Y[0];

    for (i=0; i < N1; i+=nu) {

        vector TYPE vy0, vy1, vy2, vy3;
        vector TYPE vy4, vy5, vy6, vy7;

        {
            #if defined BETA0
            vy0 = vzero;
            vy1 = vzero;
            vy2 = vzero;
            vy3 = vzero;
            vy4 = vzero;
            vy5 = vzero;
            vy6 = vzero;
            vy7 = vzero;
            #else /* BETA1 */
            vector TYPE vt0, vt1;

            vt0 = *((vector TYPE *)(py+0*VEC_STEP));
            vt1 = *((vector TYPE *)(py+1*VEC_STEP));

            #ifdef DREAL
            vector TYPE vt2, vt3;

            vt2 = *((vector TYPE *)(py+2*VEC_STEP));
            vt3 = *((vector TYPE *)(py+3*VEC_STEP));
            #endif        

            #ifdef DREAL
            vy0 = vec_mergeh(vt0, vzero);
            vy1 = vec_mergel(vt0, vzero);
            vy2 = vec_mergeh(vt1, vzero);
            vy3 = vec_mergel(vt1, vzero);
            vy4 = vec_mergeh(vt2, vzero);
            vy5 = vec_mergel(vt2, vzero);
            vy6 = vec_mergeh(vt3, vzero);
            vy7 = vec_mergel(vt3, vzero);
            #else  /* SREAL */
            vy0 = vec_sel(vt0, vzero, ELEM0);
            vy1 = vec_sel(vt0, vzero, ELEM1);
            vy2 = vec_sel(vt0, vzero, ELEM2);
            vy3 = vec_sel(vt0, vzero, ELEM3);
            vy4 = vec_sel(vt1, vzero, ELEM0);
            vy5 = vec_sel(vt1, vzero, ELEM1);
            vy6 = vec_sel(vt1, vzero, ELEM2);
            vy7 = vec_sel(vt1, vzero, ELEM3);
            #endif
            #endif
        }

        const TYPE *pa0 = &At[(i+0)*lda];
        const TYPE *pa1 = &At[(i+1)*lda];
        const TYPE *pa2 = &At[(i+2)*lda];
        const TYPE *pa3 = &At[(i+3)*lda];
        const TYPE *pa4 = &At[(i+4)*lda];
        const TYPE *pa5 = &At[(i+5)*lda];
        const TYPE *pa6 = &At[(i+6)*lda];
        const TYPE *pa7 = &At[(i+7)*lda];

        const TYPE *px = &X[0];

        for (j=0; j < M1; j+=mu) {
            vector TYPE va00, va01, va02, va03;
            vector TYPE va04, va05, va06, va07;
            vector TYPE va10, va11, va12, va13;
            vector TYPE va14, va15, va16, va17;
            vector TYPE va20, va21, va22, va23;
            vector TYPE va24, va25, va26, va27;

            vector TYPE vx0, vx1, vx2;

            vx0 = *((vector TYPE *)( px+0*VEC_STEP ));
            vx1 = *((vector TYPE *)( px+1*VEC_STEP ));
            vx2 = *((vector TYPE *)( px+2*VEC_STEP ));

            px += mu;

            va00 = *((vector TYPE *)( pa0+0*VEC_STEP ));
            va01 = *((vector TYPE *)( pa1+0*VEC_STEP ));
            va02 = *((vector TYPE *)( pa2+0*VEC_STEP ));
            va03 = *((vector TYPE *)( pa3+0*VEC_STEP ));
            va04 = *((vector TYPE *)( pa4+0*VEC_STEP ));
            va05 = *((vector TYPE *)( pa5+0*VEC_STEP ));
            va06 = *((vector TYPE *)( pa6+0*VEC_STEP ));
            va07 = *((vector TYPE *)( pa7+0*VEC_STEP ));

            vy0 = vec_madd(va00, vx0, vy0);
            vy1 = vec_madd(va01, vx0, vy1);
            vy2 = vec_madd(va02, vx0, vy2);
            vy3 = vec_madd(va03, vx0, vy3);
            vy4 = vec_madd(va04, vx0, vy4);
            vy5 = vec_madd(va05, vx0, vy5);
            vy6 = vec_madd(va06, vx0, vy6);
            vy7 = vec_madd(va07, vx0, vy7);

            va10 = *((vector TYPE *)( pa0+1*VEC_STEP ));
            va11 = *((vector TYPE *)( pa1+1*VEC_STEP ));
            va12 = *((vector TYPE *)( pa2+1*VEC_STEP ));
            va13 = *((vector TYPE *)( pa3+1*VEC_STEP ));
            va14 = *((vector TYPE *)( pa4+1*VEC_STEP ));
            va15 = *((vector TYPE *)( pa5+1*VEC_STEP ));
            va16 = *((vector TYPE *)( pa6+1*VEC_STEP ));
            va17 = *((vector TYPE *)( pa7+1*VEC_STEP ));

            vy0 = vec_madd(va10, vx1, vy0);
            vy1 = vec_madd(va11, vx1, vy1);
            vy2 = vec_madd(va12, vx1, vy2);
            vy3 = vec_madd(va13, vx1, vy3);
            vy4 = vec_madd(va14, vx1, vy4);
            vy5 = vec_madd(va15, vx1, vy5);
            vy6 = vec_madd(va16, vx1, vy6);
            vy7 = vec_madd(va17, vx1, vy7);

            va20 = *((vector TYPE *)( pa0+2*VEC_STEP ));
            va21 = *((vector TYPE *)( pa1+2*VEC_STEP ));
            va22 = *((vector TYPE *)( pa2+2*VEC_STEP ));
            va23 = *((vector TYPE *)( pa3+2*VEC_STEP ));
            va24 = *((vector TYPE *)( pa4+2*VEC_STEP ));
            va25 = *((vector TYPE *)( pa5+2*VEC_STEP ));
            va26 = *((vector TYPE *)( pa6+2*VEC_STEP ));
            va27 = *((vector TYPE *)( pa7+2*VEC_STEP ));

            vy0 = vec_madd(va20, vx2, vy0);
            vy1 = vec_madd(va21, vx2, vy1);
            vy2 = vec_madd(va22, vx2, vy2);
            vy3 = vec_madd(va23, vx2, vy3);
            vy4 = vec_madd(va24, vx2, vy4);
            vy5 = vec_madd(va25, vx2, vy5);
            vy6 = vec_madd(va26, vx2, vy6);
            vy7 = vec_madd(va27, vx2, vy7);

            pa0 += mu;
            pa1 += mu;
            pa2 += mu;
            pa3 += mu;
            pa4 += mu;
            pa5 += mu;
            pa6 += mu;
            pa7 += mu;

        }

        {
            #ifdef DREAL
            vector TYPE vt0, vt1, vt2, vt3;
            vector TYPE vt4, vt5, vt6, vt7;

            vt0 = vec_mergeh(vy0, vy1);
            vt1 = vec_mergel(vy0, vy1);
            vt2 = vec_mergeh(vy2, vy3);
            vt3 = vec_mergel(vy2, vy3);
            vt4 = vec_mergeh(vy4, vy5);
            vt5 = vec_mergel(vy4, vy5);
            vt6 = vec_mergeh(vy6, vy7);
            vt7 = vec_mergel(vy6, vy7);

            vy0 = vec_add(vt0, vt1);
            vy2 = vec_add(vt2, vt3);
            vy4 = vec_add(vt4, vt5);
            vy6 = vec_add(vt6, vt7);

            *((vector TYPE *)(py+0*VEC_STEP)) = vy0;
            *((vector TYPE *)(py+1*VEC_STEP)) = vy2;
            *((vector TYPE *)(py+2*VEC_STEP)) = vy4;
            *((vector TYPE *)(py+3*VEC_STEP)) = vy6;
            #else /* SREAL */
            vy0 = vec_reduce(vy0);
            vy1 = vec_reduce(vy1);
            vy2 = vec_reduce(vy2);
            vy3 = vec_reduce(vy3);
            vy4 = vec_reduce(vy4);
            vy5 = vec_reduce(vy5);
            vy6 = vec_reduce(vy6);
            vy7 = vec_reduce(vy7);

            vec_ste(vy0,0,py+0);
            vec_ste(vy1,0,py+1);
            vec_ste(vy2,0,py+2);
            vec_ste(vy3,0,py+3);
            vec_ste(vy4,0,py+4);
            vec_ste(vy5,0,py+5);
            vec_ste(vy6,0,py+6);
            vec_ste(vy7,0,py+7);
            #endif
        }

        {
            register TYPE y0, y1, y2, y3;
            register TYPE y4, y5, y6, y7;

            y0 = *(py+0);
            y1 = *(py+1);
            y2 = *(py+2);
            y3 = *(py+3);
            y4 = *(py+4);
            y5 = *(py+5);
            y6 = *(py+6);
            y7 = *(py+7);

            for (j=M1; j < M; j++) {
                y0 += *pa0 * X[j];
                y1 += *pa1 * X[j];
                y2 += *pa2 * X[j];
                y3 += *pa3 * X[j];
                y4 += *pa4 * X[j];
                y5 += *pa5 * X[j];
                y6 += *pa6 * X[j];
                y7 += *pa7 * X[j];
                pa0++;
                pa1++;
                pa2++;
                pa3++;
                pa4++;
                pa5++;
                pa6++;
                pa7++;
            }

            *(py+0) = y0;
            *(py+1) = y1;
            *(py+2) = y2;
            *(py+3) = y3;
            *(py+4) = y4;
            *(py+5) = y5;
            *(py+6) = y6;
            *(py+7) = y7;
        }


        py += nu;
    }

    for (i=N1; i < N; i++)
    {
        register TYPE y0;

        #if defined BETA0
        y0 = 0.0;
        #else /* BETA1 */
        y0 = Y[i];
        #endif

        for (j=0; j < M; j++)
            y0 += At[j+i*lda] * X[j];
        Y[i] = y0;
    }
}
