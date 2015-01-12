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

/* GEMV with the following assumptions:
 * 1) alpha = 1
 * 2) beta = 0 or 1 (controlled by BETA0/BETA1)
 * 3) incX = 1 and incY = 1
 * 4) Column-major storage of A
 *
 * y = [0,1]*y + A*x, A is MxN,  len(X) = N, len(Y) = M
 */
void ATL_UGEMV(
    const int M, const int N,
    const TYPE *A, const int lda,
    const TYPE *X, TYPE *Y)
{
    long int i, j;

    #if defined BETA0
    vector TYPE vzero = vec_splats( ((TYPE) 0.0) );
    #endif

    long int mu = VEC_STEP*6;
    long int nu = 8;

    long int M1 = (M/mu)*mu;
    long int M2 = (M/VEC_STEP)*VEC_STEP;
    long int N1 = (N/nu)*nu;

    vector TYPE vy0, vy1, vy2, vy3, vy4, vy5;

    {
        TYPE *py0 = &Y[0];

        for (i=0; i < M1; i+=mu) {

            #if defined BETA0
            vy0 = vzero;
            vy1 = vzero;
            vy2 = vzero;
            vy3 = vzero;
            vy4 = vzero;
            vy5 = vzero;
            #else /* BETA1 */
            vy0 = *((vector TYPE *)( py0+0*VEC_STEP ));
            vy1 = *((vector TYPE *)( py0+1*VEC_STEP ));
            vy2 = *((vector TYPE *)( py0+2*VEC_STEP ));
            vy3 = *((vector TYPE *)( py0+3*VEC_STEP ));
            vy4 = *((vector TYPE *)( py0+4*VEC_STEP ));
            vy5 = *((vector TYPE *)( py0+5*VEC_STEP ));
            #endif

            *((vector TYPE *)( py0+0*VEC_STEP )) = vy0;
            *((vector TYPE *)( py0+1*VEC_STEP )) = vy1;
            *((vector TYPE *)( py0+2*VEC_STEP )) = vy2;
            *((vector TYPE *)( py0+3*VEC_STEP )) = vy3;
            *((vector TYPE *)( py0+4*VEC_STEP )) = vy4;
            *((vector TYPE *)( py0+5*VEC_STEP )) = vy5;

            py0 += mu;
        }

        for (i=M1; i < M2; i+=VEC_STEP) {

            #if defined BETA0
            vy0 = vzero;
            #else /* BETA1 */
            vy0 = *((vector TYPE *)( py0 ));
            #endif

            *((vector TYPE *)( py0 )) = vy0;

            py0 += VEC_STEP;
        }

        for (i=M2; i < M; i++) {
            register TYPE y0;

            #if defined BETA0
            y0 = 0.0;
            #else /* BETA1 */
            y0 = *py0;
            #endif

            *py0 = y0;

            py0 += 1;
        }
    }

    TYPE *px = (TYPE *)X;

    for (j=0; j < N1; j+=nu) {

        TYPE *pa0 = (TYPE*)&A[(j+0)*lda];
        TYPE *pa1 = (TYPE*)&A[(j+1)*lda];
        TYPE *pa2 = (TYPE*)&A[(j+2)*lda];
        TYPE *pa3 = (TYPE*)&A[(j+3)*lda];
        TYPE *pa4 = (TYPE*)&A[(j+4)*lda];
        TYPE *pa5 = (TYPE*)&A[(j+5)*lda];
        TYPE *pa6 = (TYPE*)&A[(j+6)*lda];
        TYPE *pa7 = (TYPE*)&A[(j+7)*lda];

        vector TYPE vx0, vx1, vx2, vx3, vx4, vx5, vx6, vx7;

        vx0 = vec_splats( *((TYPE*)(px+0)) );
        vx1 = vec_splats( *((TYPE*)(px+1)) );
        vx2 = vec_splats( *((TYPE*)(px+2)) );
        vx3 = vec_splats( *((TYPE*)(px+3)) );
        vx4 = vec_splats( *((TYPE*)(px+4)) );
        vx5 = vec_splats( *((TYPE*)(px+5)) );
        vx6 = vec_splats( *((TYPE*)(px+6)) );
        vx7 = vec_splats( *((TYPE*)(px+7)) );
        px += nu;

        TYPE *py0 = &Y[0];

        vector TYPE va00, va01, va02, va03, va04, va05;
        vector TYPE va10, va11, va12, va13, va14, va15;
        vector TYPE va20, va21, va22, va23, va24, va25;
        vector TYPE va30, va31, va32, va33, va34, va35;
        vector TYPE va40, va41, va42, va43, va44, va45;
        vector TYPE va50, va51, va52, va53, va54, va55;
        vector TYPE va60, va61, va62, va63, va64, va65;
        vector TYPE va70, va71, va72, va73, va74, va75;

        for (i=0; i < M1; i+=mu) {

            vy0 = *((vector TYPE *)( py0+0*VEC_STEP ));
            vy1 = *((vector TYPE *)( py0+1*VEC_STEP ));
            vy2 = *((vector TYPE *)( py0+2*VEC_STEP ));
            vy3 = *((vector TYPE *)( py0+3*VEC_STEP ));
            vy4 = *((vector TYPE *)( py0+4*VEC_STEP ));
            vy5 = *((vector TYPE *)( py0+5*VEC_STEP ));
 
            va00 = *((vector TYPE *)( pa0+0*VEC_STEP ));
            va01 = *((vector TYPE *)( pa0+1*VEC_STEP ));
            va02 = *((vector TYPE *)( pa0+2*VEC_STEP ));
            va03 = *((vector TYPE *)( pa0+3*VEC_STEP ));
            va04 = *((vector TYPE *)( pa0+4*VEC_STEP ));
            va05 = *((vector TYPE *)( pa0+5*VEC_STEP ));
            pa0 += mu;

            vy0 = vec_madd(va00, vx0, vy0);
            vy1 = vec_madd(va01, vx0, vy1);
            vy2 = vec_madd(va02, vx0, vy2);
            vy3 = vec_madd(va03, vx0, vy3);
            vy4 = vec_madd(va04, vx0, vy4);
            vy5 = vec_madd(va05, vx0, vy5);

            va10 = *((vector TYPE *)( pa1+0*VEC_STEP ));
            va11 = *((vector TYPE *)( pa1+1*VEC_STEP ));
            va12 = *((vector TYPE *)( pa1+2*VEC_STEP ));
            va13 = *((vector TYPE *)( pa1+3*VEC_STEP ));
            va14 = *((vector TYPE *)( pa1+4*VEC_STEP ));
            va15 = *((vector TYPE *)( pa1+5*VEC_STEP ));
            pa1 += mu;

            vy0 = vec_madd(va10, vx1, vy0);
            vy1 = vec_madd(va11, vx1, vy1);
            vy2 = vec_madd(va12, vx1, vy2);
            vy3 = vec_madd(va13, vx1, vy3);
            vy4 = vec_madd(va14, vx1, vy4);
            vy5 = vec_madd(va15, vx1, vy5);

            va20 = *((vector TYPE *)( pa2+0*VEC_STEP ));
            va21 = *((vector TYPE *)( pa2+1*VEC_STEP ));
            va22 = *((vector TYPE *)( pa2+2*VEC_STEP ));
            va23 = *((vector TYPE *)( pa2+3*VEC_STEP ));
            va24 = *((vector TYPE *)( pa2+4*VEC_STEP ));
            va25 = *((vector TYPE *)( pa2+5*VEC_STEP ));
            pa2 += mu;

            vy0 = vec_madd(va20, vx2, vy0);
            vy1 = vec_madd(va21, vx2, vy1);
            vy2 = vec_madd(va22, vx2, vy2);
            vy3 = vec_madd(va23, vx2, vy3);
            vy4 = vec_madd(va24, vx2, vy4);
            vy5 = vec_madd(va25, vx2, vy5);

            va30 = *((vector TYPE *)( pa3+0*VEC_STEP ));
            va31 = *((vector TYPE *)( pa3+1*VEC_STEP ));
            va32 = *((vector TYPE *)( pa3+2*VEC_STEP ));
            va33 = *((vector TYPE *)( pa3+3*VEC_STEP ));
            va34 = *((vector TYPE *)( pa3+4*VEC_STEP ));
            va35 = *((vector TYPE *)( pa3+5*VEC_STEP ));
            pa3 += mu;

            vy0 = vec_madd(va30, vx3, vy0);
            vy1 = vec_madd(va31, vx3, vy1);
            vy2 = vec_madd(va32, vx3, vy2);
            vy3 = vec_madd(va33, vx3, vy3);
            vy4 = vec_madd(va34, vx3, vy4);
            vy5 = vec_madd(va35, vx3, vy5);

            va40 = *((vector TYPE *)( pa4+0*VEC_STEP ));
            va41 = *((vector TYPE *)( pa4+1*VEC_STEP ));
            va42 = *((vector TYPE *)( pa4+2*VEC_STEP ));
            va43 = *((vector TYPE *)( pa4+3*VEC_STEP ));
            va44 = *((vector TYPE *)( pa4+4*VEC_STEP ));
            va45 = *((vector TYPE *)( pa4+5*VEC_STEP ));
            pa4 += mu;

            vy0 = vec_madd(va40, vx4, vy0);
            vy1 = vec_madd(va41, vx4, vy1);
            vy2 = vec_madd(va42, vx4, vy2);
            vy3 = vec_madd(va43, vx4, vy3);
            vy4 = vec_madd(va44, vx4, vy4);
            vy5 = vec_madd(va45, vx4, vy5);

            va50 = *((vector TYPE *)( pa5+0*VEC_STEP ));
            va51 = *((vector TYPE *)( pa5+1*VEC_STEP ));
            va52 = *((vector TYPE *)( pa5+2*VEC_STEP ));
            va53 = *((vector TYPE *)( pa5+3*VEC_STEP ));
            va54 = *((vector TYPE *)( pa5+4*VEC_STEP ));
            va55 = *((vector TYPE *)( pa5+5*VEC_STEP ));
            pa5 += mu;

            vy0 = vec_madd(va50, vx5, vy0);
            vy1 = vec_madd(va51, vx5, vy1);
            vy2 = vec_madd(va52, vx5, vy2);
            vy3 = vec_madd(va53, vx5, vy3);
            vy4 = vec_madd(va54, vx5, vy4);
            vy5 = vec_madd(va55, vx5, vy5);

            va60 = *((vector TYPE *)( pa6+0*VEC_STEP ));
            va61 = *((vector TYPE *)( pa6+1*VEC_STEP ));
            va62 = *((vector TYPE *)( pa6+2*VEC_STEP ));
            va63 = *((vector TYPE *)( pa6+3*VEC_STEP ));
            va64 = *((vector TYPE *)( pa6+4*VEC_STEP ));
            va65 = *((vector TYPE *)( pa6+5*VEC_STEP ));
            pa6 += mu;

            vy0 = vec_madd(va60, vx6, vy0);
            vy1 = vec_madd(va61, vx6, vy1);
            vy2 = vec_madd(va62, vx6, vy2);
            vy3 = vec_madd(va63, vx6, vy3);
            vy4 = vec_madd(va64, vx6, vy4);
            vy5 = vec_madd(va65, vx6, vy5);

            va70 = *((vector TYPE *)( pa7+0*VEC_STEP ));
            va71 = *((vector TYPE *)( pa7+1*VEC_STEP ));
            va72 = *((vector TYPE *)( pa7+2*VEC_STEP ));
            va73 = *((vector TYPE *)( pa7+3*VEC_STEP ));
            va74 = *((vector TYPE *)( pa7+4*VEC_STEP ));
            va75 = *((vector TYPE *)( pa7+5*VEC_STEP ));
            pa7 += mu;

            vy0 = vec_madd(va70, vx7, vy0);
            vy1 = vec_madd(va71, vx7, vy1);
            vy2 = vec_madd(va72, vx7, vy2);
            vy3 = vec_madd(va73, vx7, vy3);
            vy4 = vec_madd(va74, vx7, vy4);
            vy5 = vec_madd(va75, vx7, vy5);

            *((vector TYPE *)( py0+0*VEC_STEP )) = vy0;
            *((vector TYPE *)( py0+1*VEC_STEP )) = vy1;
            *((vector TYPE *)( py0+2*VEC_STEP )) = vy2;
            *((vector TYPE *)( py0+3*VEC_STEP )) = vy3;
            *((vector TYPE *)( py0+4*VEC_STEP )) = vy4;
            *((vector TYPE *)( py0+5*VEC_STEP )) = vy5;

            py0 += mu;
        }

        for (i=M1; i < M2; i+=VEC_STEP) {
            vy0 = *((vector TYPE *)( py0 ));

            va00 = *((vector TYPE *)( pa0 ));
            va10 = *((vector TYPE *)( pa1 ));
            va20 = *((vector TYPE *)( pa2 ));
            va30 = *((vector TYPE *)( pa3 ));
            va40 = *((vector TYPE *)( pa4 ));
            va50 = *((vector TYPE *)( pa5 ));
            va60 = *((vector TYPE *)( pa6 ));
            va70 = *((vector TYPE *)( pa7 ));
            pa0 += VEC_STEP;
            pa1 += VEC_STEP;
            pa2 += VEC_STEP;
            pa3 += VEC_STEP;
            pa4 += VEC_STEP;
            pa5 += VEC_STEP;
            pa6 += VEC_STEP;
            pa7 += VEC_STEP;

            vy0 = vec_madd(va00, vx0, vy0);
            vy0 = vec_madd(va10, vx1, vy0);
            vy0 = vec_madd(va20, vx2, vy0);
            vy0 = vec_madd(va30, vx3, vy0);
            vy0 = vec_madd(va40, vx4, vy0);
            vy0 = vec_madd(va50, vx5, vy0);
            vy0 = vec_madd(va60, vx6, vy0);
            vy0 = vec_madd(va70, vx7, vy0);

            *((vector TYPE *)( py0 )) = vy0;
            py0+=VEC_STEP;
        }

        for (i=M2; i < M; i++) {
            register TYPE y0;

            y0 = Y[i];
            y0 += A[i+j*lda] * X[j];
            y0 += A[i+(j+1)*lda] * X[j+1];
            y0 += A[i+(j+2)*lda] * X[j+2];
            y0 += A[i+(j+3)*lda] * X[j+3];
            y0 += A[i+(j+4)*lda] * X[j+4];
            y0 += A[i+(j+5)*lda] * X[j+5];
            y0 += A[i+(j+6)*lda] * X[j+6];
            y0 += A[i+(j+7)*lda] * X[j+7];
            Y[i] = y0;
        }
    }

    for (j=N1; j < N; j++) {

        vector TYPE vx0;

        vx0 = vec_splats( *((TYPE*)(px)) );
        px += 1;

        TYPE *py0 = &Y[0];

        TYPE *pa0 = (TYPE*)&A[(j+0)*lda];

        vector TYPE va00, va01, va02, va03, va04, va05;

        for (i=0; i < M1; i+=mu) {

            vy0 = *((vector TYPE *)( py0+0*VEC_STEP ));
            vy1 = *((vector TYPE *)( py0+1*VEC_STEP ));
            vy2 = *((vector TYPE *)( py0+2*VEC_STEP ));
            vy3 = *((vector TYPE *)( py0+3*VEC_STEP ));
            vy4 = *((vector TYPE *)( py0+4*VEC_STEP ));
            vy5 = *((vector TYPE *)( py0+5*VEC_STEP ));

            va00 = *((vector TYPE *)( pa0+0*VEC_STEP ));
            va01 = *((vector TYPE *)( pa0+1*VEC_STEP ));
            va02 = *((vector TYPE *)( pa0+2*VEC_STEP ));
            va03 = *((vector TYPE *)( pa0+3*VEC_STEP ));
            va04 = *((vector TYPE *)( pa0+4*VEC_STEP ));
            va05 = *((vector TYPE *)( pa0+5*VEC_STEP ));
            pa0 += mu;

            vy0 = vec_madd(va00, vx0, vy0);
            vy1 = vec_madd(va01, vx0, vy1);
            vy2 = vec_madd(va02, vx0, vy2);
            vy3 = vec_madd(va03, vx0, vy3);
            vy4 = vec_madd(va04, vx0, vy4);
            vy5 = vec_madd(va05, vx0, vy5);

            *((vector TYPE *)( py0+0*VEC_STEP )) = vy0;
            *((vector TYPE *)( py0+1*VEC_STEP )) = vy1;
            *((vector TYPE *)( py0+2*VEC_STEP )) = vy2;
            *((vector TYPE *)( py0+3*VEC_STEP )) = vy3;
            *((vector TYPE *)( py0+4*VEC_STEP )) = vy4;
            *((vector TYPE *)( py0+5*VEC_STEP )) = vy5;
            py0 += mu;
        }

        for (i=M1; i < M; i++) {
            register TYPE y0;

            y0 = Y[i];
            y0 += A[i+j*lda] * X[j];
            Y[i] = y0;
        }
    }

}
