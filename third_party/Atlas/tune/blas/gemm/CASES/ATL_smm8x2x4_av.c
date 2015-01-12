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

#include <altivec.h>
#include "atlas_misc.h"
#include "atlas_prefetch.h"               /* ATL_pfl1R, ATL_pfl1W */

#define VEC_SIZE 4

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *At, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc)
{
    long int i, j, k;

    for (i=0; i < M; i+=8) {
        
        #ifdef TREAL
        TYPE *pc0 = &C[i+(0*ldc)];
        TYPE *pc1 = &C[i+(1*ldc)];
        #else
        TYPE *pc0 = &C[2*(i+(0*ldc))];
        TYPE *pc1 = &C[2*(i+(1*ldc))];
        #endif

        for (j=0; j < N; j+=2) {

            vector TYPE vt00, vt10, vt20, vt30, vt40, vt50, vt60, vt70;
            vector TYPE vt01, vt11, vt21, vt31, vt41, vt51, vt61, vt71;

            vector TYPE vbeta = (vector TYPE){ beta, beta, beta, beta };
            vector TYPE vzero = (vector TYPE){ 0.0, 0.0, 0.0, 0.0 };

            {
                vt00 = vzero;
                vt10 = vzero;
                vt20 = vzero;
                vt30 = vzero;
                vt40 = vzero;
                vt50 = vzero;
                vt60 = vzero;
                vt70 = vzero;
            }

            {
                vt01 = vzero;
                vt11 = vzero;
                vt21 = vzero;
                vt31 = vzero;
                vt41 = vzero;
                vt51 = vzero;
                vt61 = vzero;
                vt71 = vzero;
            }

            const TYPE *pb0 = &B[(j+0)*ldb];
            const TYPE *pb1 = &B[(j+1)*ldb];

            const TYPE *pa0 = &At[(i+0)*lda];
            const TYPE *pa1 = &At[(i+1)*lda];
            const TYPE *pa2 = &At[(i+2)*lda];
            const TYPE *pa3 = &At[(i+3)*lda];
            const TYPE *pa4 = &At[(i+4)*lda];
            const TYPE *pa5 = &At[(i+5)*lda];
            const TYPE *pa6 = &At[(i+6)*lda];
            const TYPE *pa7 = &At[(i+7)*lda];

            vector TYPE vb0 = *((vector TYPE *)( pb0+0 ));
            vector TYPE vb1 = *((vector TYPE *)( pb1+0 ));

            vector TYPE va0 = *((vector TYPE *)( pa0+0 ));
            vector TYPE va1 = *((vector TYPE *)( pa1+0 ));
            vector TYPE va2 = *((vector TYPE *)( pa2+0 ));
            vector TYPE va3 = *((vector TYPE *)( pa3+0 ));
            vector TYPE va4 = *((vector TYPE *)( pa4+0 ));
            vector TYPE va5 = *((vector TYPE *)( pa5+0 ));
            vector TYPE va6 = *((vector TYPE *)( pa6+0 ));
            vector TYPE va7 = *((vector TYPE *)( pa7+0 ));

            for (k=VEC_SIZE; k < K; k+=VEC_SIZE) {

                vt00 = vec_madd(va0, vb0, vt00);
                vt01 = vec_madd(va0, vb1, vt01);
                va0 = *((vector TYPE *)( pa0+k ));

                vt10 = vec_madd(va1, vb0, vt10);
                vt11 = vec_madd(va1, vb1, vt11);
                va1 = *((vector TYPE *)( pa1+k ));

                vt20 = vec_madd(va2, vb0, vt20);
                vt21 = vec_madd(va2, vb1, vt21);
                va2 = *((vector TYPE *)( pa2+k ));

                vt30 = vec_madd(va3, vb0, vt30);
                vt31 = vec_madd(va3, vb1, vt31);
                va3 = *((vector TYPE *)( pa3+k ));

                vt40 = vec_madd(va4, vb0, vt40);
                vt41 = vec_madd(va4, vb1, vt41);
                va4 = *((vector TYPE *)( pa4+k ));

                vt50 = vec_madd(va5, vb0, vt50);
                vt51 = vec_madd(va5, vb1, vt51);
                va5 = *((vector TYPE *)( pa5+k ));

                vt60 = vec_madd(va6, vb0, vt60);
                vt61 = vec_madd(va6, vb1, vt61);
                va6 = *((vector TYPE *)( pa6+k ));

                vt70 = vec_madd(va7, vb0, vt70);
                vt71 = vec_madd(va7, vb1, vt71);
                va7 = *((vector TYPE *)( pa7+k ));
                vb0 = *((vector TYPE *)( pb0+k ));
                vb1 = *((vector TYPE *)( pb1+k ));

            }

            vt00 = vec_madd(va0, vb0, vt00);
            vt10 = vec_madd(va1, vb0, vt10);
            vt20 = vec_madd(va2, vb0, vt20);
            vt30 = vec_madd(va3, vb0, vt30);
            vt40 = vec_madd(va4, vb0, vt40);
            vt50 = vec_madd(va5, vb0, vt50);
            vt60 = vec_madd(va6, vb0, vt60);
            vt70 = vec_madd(va7, vb0, vt70);

            {
                {
                    vector TYPE vth, vtl, vt0, vt1, vt2, vt3;

                    vth = vec_mergeh(vt00, vt20);
                    vtl = vec_mergeh(vt10, vt30);
                    vt0 = vec_mergeh(vth, vtl);
                    vt1 = vec_mergel(vth, vtl);
                    vt0 = vec_add(vt0,vt1);

                    vth = vec_mergel(vt00, vt20);
                    vtl = vec_mergel(vt10, vt30);
                    vt2 = vec_mergeh(vth, vtl);
                    vt3 = vec_mergel(vth, vtl);
                    vt2 = vec_add(vt2,vt3);

                    vt00 = vec_add(vt0, vt2);
                }

                {
                    vector TYPE vth, vtl, vt0, vt1, vt2, vt3;

                    vth = vec_mergeh(vt40, vt60);
                    vtl = vec_mergeh(vt50, vt70);
                    vt0 = vec_mergeh(vth, vtl);
                    vt1 = vec_mergel(vth, vtl);
                    vt0 = vec_add(vt0,vt1);

                    vth = vec_mergel(vt40, vt60);
                    vtl = vec_mergel(vt50, vt70);
                    vt2 = vec_mergeh(vth, vtl);
                    vt3 = vec_mergel(vth, vtl);
                    vt2 = vec_add(vt2,vt3);

                    vt40 = vec_add(vt0, vt2);
                }

                #ifdef TREAL
                if ( (((unsigned long)pc0) & 0xF) == 0 ) {

                    #if defined BETA0
                    *((vector TYPE *)( pc0+0 )) = vt00;
                    *((vector TYPE *)( pc0+4 )) = vt40;
                    #else /* BETA1 or BETAX */

                    vector TYPE vc0, vc4;

                    vc0 = *((vector TYPE *)( pc0+0 ));
                    vc4 = *((vector TYPE *)( pc0+4 ));

                    vc0 = vec_madd(vc0, vbeta, vt00);
                    vc4 = vec_madd(vc4, vbeta, vt40);

                    *((vector TYPE *)( pc0+0 )) = vc0;
                    *((vector TYPE *)( pc0+4 )) = vc4;
                    #endif
                } else {
                    *(pc0+0) = *(pc0+0) * beta + vec_extract(vt00,0);
                    *(pc0+1) = *(pc0+1) * beta + vec_extract(vt00,1);
                    *(pc0+2) = *(pc0+2) * beta + vec_extract(vt00,2);
                    *(pc0+3) = *(pc0+3) * beta + vec_extract(vt00,3);
                    *(pc0+4) = *(pc0+4) * beta + vec_extract(vt40,0);
                    *(pc0+5) = *(pc0+5) * beta + vec_extract(vt40,1);
                    *(pc0+6) = *(pc0+6) * beta + vec_extract(vt40,2);
                    *(pc0+7) = *(pc0+7) * beta + vec_extract(vt40,3);
                }
                pc0 += 2*ldc;
                #else
                {
                    *(pc0+0)  = *(pc0+0)  * beta + vec_extract(vt00,0);
                    *(pc0+2)  = *(pc0+2)  * beta + vec_extract(vt00,1);
                    *(pc0+4)  = *(pc0+4)  * beta + vec_extract(vt00,2);
                    *(pc0+6)  = *(pc0+6)  * beta + vec_extract(vt00,3);
                    *(pc0+8)  = *(pc0+8)  * beta + vec_extract(vt40,0);
                    *(pc0+10) = *(pc0+10) * beta + vec_extract(vt40,1);
                    *(pc0+12) = *(pc0+12) * beta + vec_extract(vt40,2);
                    *(pc0+14) = *(pc0+14) * beta + vec_extract(vt40,3);
                }
                pc0 += 2*2*ldc;
                #endif
            }

            vt01 = vec_madd(va0, vb1, vt01);
            vt11 = vec_madd(va1, vb1, vt11);
            vt21 = vec_madd(va2, vb1, vt21);
            vt31 = vec_madd(va3, vb1, vt31);
            vt41 = vec_madd(va4, vb1, vt41);
            vt51 = vec_madd(va5, vb1, vt51);
            vt61 = vec_madd(va6, vb1, vt61);
            vt71 = vec_madd(va7, vb1, vt71);

            {
                {
                    vector TYPE vth, vtl, vt0, vt1, vt2, vt3;

                    vth = vec_mergeh(vt01, vt21);
                    vtl = vec_mergeh(vt11, vt31);
                    vt0 = vec_mergeh(vth, vtl);
                    vt1 = vec_mergel(vth, vtl);
                    vt0 = vec_add(vt0,vt1);

                    vth = vec_mergel(vt01, vt21);
                    vtl = vec_mergel(vt11, vt31);
                    vt2 = vec_mergeh(vth, vtl);
                    vt3 = vec_mergel(vth, vtl);
                    vt2 = vec_add(vt2,vt3);

                    vt01 = vec_add(vt0, vt2);
                }

                {
                    vector TYPE vth, vtl, vt0, vt1, vt2, vt3;

                    vth = vec_mergeh(vt41, vt61);
                    vtl = vec_mergeh(vt51, vt71);
                    vt0 = vec_mergeh(vth, vtl);
                    vt1 = vec_mergel(vth, vtl);
                    vt0 = vec_add(vt0,vt1);

                    vth = vec_mergel(vt41, vt61);
                    vtl = vec_mergel(vt51, vt71);
                    vt2 = vec_mergeh(vth, vtl);
                    vt3 = vec_mergel(vth, vtl);
                    vt2 = vec_add(vt2,vt3);

                    vt41 = vec_add(vt0, vt2);
                }

                #ifdef TREAL
                if ( (((unsigned long)pc1) & 0xF) == 0 ) {

                    #if defined BETA0
                    *((vector TYPE *)( pc1+0 )) = vt01;
                    *((vector TYPE *)( pc1+4 )) = vt41;
                    #else /* BETA1 or BETAX */

                    vector TYPE vc0, vc4;

                    vc0 = *((vector TYPE *)( pc1+0 ));
                    vc4 = *((vector TYPE *)( pc1+4 ));

                    vc0 = vec_madd(vc0, vbeta, vt01);
                    vc4 = vec_madd(vc4, vbeta, vt41);

                    *((vector TYPE *)( pc1+0 )) = vc0;
                    *((vector TYPE *)( pc1+4 )) = vc4;
                    #endif
                } else {
                    *(pc1+0) = *(pc1+0) * beta + vec_extract(vt01,0);
                    *(pc1+1) = *(pc1+1) * beta + vec_extract(vt01,1);
                    *(pc1+2) = *(pc1+2) * beta + vec_extract(vt01,2);
                    *(pc1+3) = *(pc1+3) * beta + vec_extract(vt01,3);
                    *(pc1+4) = *(pc1+4) * beta + vec_extract(vt41,0);
                    *(pc1+5) = *(pc1+5) * beta + vec_extract(vt41,1);
                    *(pc1+6) = *(pc1+6) * beta + vec_extract(vt41,2);
                    *(pc1+7) = *(pc1+7) * beta + vec_extract(vt41,3);
                }
                pc1 += 2*ldc;
                #else
                {
                    *(pc1+0)  = *(pc1+0)  * beta + vec_extract(vt01,0);
                    *(pc1+2)  = *(pc1+2)  * beta + vec_extract(vt01,1);
                    *(pc1+4)  = *(pc1+4)  * beta + vec_extract(vt01,2);
                    *(pc1+6)  = *(pc1+6)  * beta + vec_extract(vt01,3);
                    *(pc1+8)  = *(pc1+8)  * beta + vec_extract(vt41,0);
                    *(pc1+10) = *(pc1+10) * beta + vec_extract(vt41,1);
                    *(pc1+12) = *(pc1+12) * beta + vec_extract(vt41,2);
                    *(pc1+14) = *(pc1+14) * beta + vec_extract(vt41,3);
                }
                pc1 += 2*2*ldc;
                #endif
            }
        }
    }
}
