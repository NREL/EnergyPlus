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

/* GER with the following assumptions:
 * 1) alpha = 1
 * 2) incX = 1 and incY = 1
 * 3) Column-major storage of A
 *
 * A = A + x*y, A is MxN,  len(X) = M, len(Y) = N
 *
 * GER blocks X so that it is reused in L1. 
 * The dominant direction of the loop is expected to be down the columns of A.
 */
void ATL_UGERK(
    const int M, const int N,
    const TYPE *X, const TYPE *Y,
    TYPE *A, const int lda)
{
    long int i, j;

    long int mu = VEC_STEP;
    long int nu = 4;

    long int M1 = (M/mu)*mu;
    long int N1 = (N/nu)*nu;

    TYPE *py = (TYPE *)&Y[0];

    for (j=0; j < N1; j+=nu) {

        vector TYPE vy0 = vec_splats( *((TYPE*)(py+0)) );
        vector TYPE vy1 = vec_splats( *((TYPE*)(py+1)) );
        vector TYPE vy2 = vec_splats( *((TYPE*)(py+2)) );
        vector TYPE vy3 = vec_splats( *((TYPE*)(py+3)) );

        TYPE *pa0 = (TYPE*)&A[(j+0)*lda];
        TYPE *pa1 = (TYPE*)&A[(j+1)*lda];
        TYPE *pa2 = (TYPE*)&A[(j+2)*lda];
        TYPE *pa3 = (TYPE*)&A[(j+3)*lda];

        TYPE *px = (TYPE *)&X[0];

        for (i=0; i < M1; i+=mu) {

            vector TYPE vx0 = *((vector TYPE*)( px ));
            px += mu;

            vector TYPE va00, va01, va02, va03;

            va00 = *((vector TYPE *)( pa0+0*VEC_STEP ));
            va01 = *((vector TYPE *)( pa1+0*VEC_STEP ));
            va02 = *((vector TYPE *)( pa2+0*VEC_STEP ));
            va03 = *((vector TYPE *)( pa3+0*VEC_STEP ));

            *((vector TYPE *)( pa0+0*VEC_STEP )) = vec_madd(vx0, vy0, va00);
            *((vector TYPE *)( pa1+0*VEC_STEP )) = vec_madd(vx0, vy1, va01);
            *((vector TYPE *)( pa2+0*VEC_STEP )) = vec_madd(vx0, vy2, va02);
            *((vector TYPE *)( pa3+0*VEC_STEP )) = vec_madd(vx0, vy3, va03);

            pa0 += mu;
            pa1 += mu;
            pa2 += mu;
            pa3 += mu;
        }

        for (i=M1; i < M; i+=1) {

            *pa0 += *px * *(py+0);
            *pa1 += *px * *(py+1);
            *pa2 += *px * *(py+2);
            *pa3 += *px * *(py+3);

            pa0 += 1;
            pa1 += 1;
            pa2 += 1;
            pa3 += 1;

            px += 1;
        }

        py += nu;
    }

    for (j=N1; j < N; j++) {

        vector TYPE vy0 = vec_splats( *((TYPE*)(py+0)) );
        TYPE *pa0 = (TYPE*)&A[(j+0)*lda];
        TYPE *px = (TYPE *)&X[0];

        for (i=0; i < M1; i+=mu) {

            vector TYPE vx0 = *((vector TYPE*)( px ));
            vector TYPE va00 = *((vector TYPE *)( pa0 ));
            *((vector TYPE *)( pa0 )) = vec_madd(vx0, vy0, va00);

            pa0 += mu;
            px += mu;
        }

        for (i=M1; i < M; i+=1) {

            *pa0 += *px * *(py+0);
            pa0 += 1;
            px += 1;

        }

        py += 1;
    }
}
