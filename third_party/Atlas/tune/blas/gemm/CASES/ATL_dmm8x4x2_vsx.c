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

#define VEC_SIZE 2

#ifdef __GNUC__
/* The AT 2.1 compiler does not implement vec_mergeh/vec_mergel for doubles */
#undef vec_mergeh
static inline vector TYPE vec_mergeh(vector TYPE a, vector TYPE b)
{ return __builtin_vsx_xxpermdi_2df (a, b, 0); }
#undef vec_mergel
static inline vector TYPE vec_mergel(vector TYPE a, vector TYPE b)
{ return __builtin_vsx_xxpermdi_2df (a, b, 3); }
#else
/* Assume no hack needed for vec_mergeh/vec_mergel */
#endif

#ifndef ATL_GOT_L1PREFETCH
#ifdef _ARCH_PPC
#undef ATL_pfl1R
#define ATL_pfl1R(mem)  { __asm__ volatile ("dcbt 0, %0, 0" : : "r" ((mem))); }
#endif
#endif

void ATL_USERMM
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *At, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc)
{
    long int i, j, k;

    for (i=0; i < M; i+=8) {

        #ifdef TREAL
        TYPE *pc0 = &C[i+0*ldc];
        TYPE *pc1 = &C[i+1*ldc];
        TYPE *pc2 = &C[i+2*ldc];
        TYPE *pc3 = &C[i+3*ldc];
        #else
        TYPE *pc0 = &C[2*(i+0*ldc)];
        TYPE *pc1 = &C[2*(i+1*ldc)];
        TYPE *pc2 = &C[2*(i+2*ldc)];
        TYPE *pc3 = &C[2*(i+3*ldc)];
        #endif

        ATL_pfl1R(pc0);
        ATL_pfl1R(pc1);
        ATL_pfl1R(pc2);
        ATL_pfl1R(pc3);

        for (j=0; j < N; j+=4) {

            vector TYPE vt00, vt10, vt20, vt30, vt40, vt50, vt60, vt70;
            vector TYPE vt01, vt11, vt21, vt31, vt41, vt51, vt61, vt71;
            vector TYPE vt02, vt12, vt22, vt32, vt42, vt52, vt62, vt72;
            vector TYPE vt03, vt13, vt23, vt33, vt43, vt53, vt63, vt73;

            vector TYPE vzero = (vector TYPE){ 0.0, 0.0 };
            vector TYPE vbeta = (vector TYPE){ beta, beta };

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

            {
                vt02 = vzero;
                vt12 = vzero;
                vt22 = vzero;
                vt32 = vzero;
                vt42 = vzero;
                vt52 = vzero;
                vt62 = vzero;
                vt72 = vzero;
            }

            {
                vt03 = vzero;
                vt13 = vzero;
                vt23 = vzero;
                vt33 = vzero;
                vt43 = vzero;
                vt53 = vzero;
                vt63 = vzero;
                vt73 = vzero;
            }

            const TYPE *pb0 = &B[(j+0)*ldb];
            const TYPE *pb1 = &B[(j+1)*ldb];
            const TYPE *pb2 = &B[(j+2)*ldb];
            const TYPE *pb3 = &B[(j+3)*ldb];

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
            vector TYPE vb2 = *((vector TYPE *)( pb2+0 ));
            vector TYPE vb3 = *((vector TYPE *)( pb3+0 ));

            vector TYPE va0 = *((vector TYPE *)( pa0+0 ));
            vector TYPE va1 = *((vector TYPE *)( pa1+0 ));
            vector TYPE va2 = *((vector TYPE *)( pa2+0 ));
            vector TYPE va3 = *((vector TYPE *)( pa3+0 ));
            vector TYPE va4 = *((vector TYPE *)( pa4+0 ));
            vector TYPE va5 = *((vector TYPE *)( pa5+0 ));
            vector TYPE va6 = *((vector TYPE *)( pa6+0 ));
            vector TYPE va7 = *((vector TYPE *)( pa7+0 ));

            for (k=2; k < K; k+=VEC_SIZE) {

                vt00 = vec_madd(va0, vb0, vt00);
                vt01 = vec_madd(va0, vb1, vt01);
                vt02 = vec_madd(va0, vb2, vt02);
                vt03 = vec_madd(va0, vb3, vt03);
                va0 = *((vector TYPE *)( pa0+k ));

                vt10 = vec_madd(va1, vb0, vt10);
                vt11 = vec_madd(va1, vb1, vt11);
                vt12 = vec_madd(va1, vb2, vt12);
                vt13 = vec_madd(va1, vb3, vt13);
                va1 = *((vector TYPE *)( pa1+k ));

                vt20 = vec_madd(va2, vb0, vt20);
                vt21 = vec_madd(va2, vb1, vt21);
                vt22 = vec_madd(va2, vb2, vt22);
                vt23 = vec_madd(va2, vb3, vt23);
                va2 = *((vector TYPE *)( pa2+k ));

                vt30 = vec_madd(va3, vb0, vt30);
                vt31 = vec_madd(va3, vb1, vt31);
                vt32 = vec_madd(va3, vb2, vt32);
                vt33 = vec_madd(va3, vb3, vt33);
                va3 = *((vector TYPE *)( pa3+k ));

                vt40 = vec_madd(va4, vb0, vt40);
                vt41 = vec_madd(va4, vb1, vt41);
                vt42 = vec_madd(va4, vb2, vt42);
                vt43 = vec_madd(va4, vb3, vt43);
                va4 = *((vector TYPE *)( pa4+k ));

                vt50 = vec_madd(va5, vb0, vt50);
                vt51 = vec_madd(va5, vb1, vt51);
                vt52 = vec_madd(va5, vb2, vt52);
                vt53 = vec_madd(va5, vb3, vt53);
                va5 = *((vector TYPE *)( pa5+k ));

                vt60 = vec_madd(va6, vb0, vt60);
                vt61 = vec_madd(va6, vb1, vt61);
                vt62 = vec_madd(va6, vb2, vt62);
                vt63 = vec_madd(va6, vb3, vt63);
                va6 = *((vector TYPE *)( pa6+k ));

                vt70 = vec_madd(va7, vb0, vt70);
                vb0 = *((vector TYPE *)( pb0+k ));
                vt71 = vec_madd(va7, vb1, vt71);
                vb1 = *((vector TYPE *)( pb1+k ));
                vt72 = vec_madd(va7, vb2, vt72);
                vb2 = *((vector TYPE *)( pb2+k ));
                vt73 = vec_madd(va7, vb3, vt73);
                va7 = *((vector TYPE *)( pa7+k ));
                vb3 = *((vector TYPE *)( pb3+k ));
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
                vector TYPE vt0h, vt0l, vt2h, vt2l, vt4h, vt4l, vt6h, vt6l;

                vt0h = vec_mergeh(vt00, vt10);
                vt0l = vec_mergel(vt00, vt10);
                vt2h = vec_mergeh(vt20, vt30);
                vt2l = vec_mergel(vt20, vt30);
                vt4h = vec_mergeh(vt40, vt50);
                vt4l = vec_mergel(vt40, vt50);
                vt6h = vec_mergeh(vt60, vt70);
                vt6l = vec_mergel(vt60, vt70);

                vt0h = vec_add(vt0h, vt0l);
                vt2h = vec_add(vt2h, vt2l);
                vt4h = vec_add(vt4h, vt4l);
                vt6h = vec_add(vt6h, vt6l);

                #ifdef TREAL
                #if defined BETA0
                *((vector TYPE *)(pc0+0)) = vt0h;
                *((vector TYPE *)(pc0+2)) = vt2h;
                *((vector TYPE *)(pc0+4)) = vt4h;
                *((vector TYPE *)(pc0+6)) = vt6h;
                #else /* BETA1 or BETAX */
                vector TYPE vc00, vc20, vc40, vc60;
    
                vc00 = *((vector TYPE *)(pc0+0));
                vc20 = *((vector TYPE *)(pc0+2));
                vc40 = *((vector TYPE *)(pc0+4));
                vc60 = *((vector TYPE *)(pc0+6));

                #if defined BETA1
                vc00 = vec_add(vc00, vt0h);
                vc20 = vec_add(vc20, vt2h);
                vc40 = vec_add(vc40, vt4h);
                vc60 = vec_add(vc60, vt6h);
                #else
                vc00 = vec_madd(vc00, vbeta, vt0h);
                vc20 = vec_madd(vc20, vbeta, vt2h);
                vc40 = vec_madd(vc40, vbeta, vt4h);
                vc60 = vec_madd(vc60, vbeta, vt6h);
                #endif

                *((vector TYPE *)(pc0+0)) = vc00;
                *((vector TYPE *)(pc0+2)) = vc20;
                *((vector TYPE *)(pc0+4)) = vc40;
                *((vector TYPE *)(pc0+6)) = vc60;
                #endif

                pc0 += 4*ldc;

                #else /* TCPLX */
                #if defined BETA0
                *(pc0+0)  = vec_extract(vt0h,0);
                *(pc0+2)  = vec_extract(vt0h,1);
                *(pc0+4)  = vec_extract(vt2h,0);
                *(pc0+6)  = vec_extract(vt2h,1);
                *(pc0+8)  = vec_extract(vt4h,0);
                *(pc0+10) = vec_extract(vt4h,1);
                *(pc0+12) = vec_extract(vt6h,0);
                *(pc0+14) = vec_extract(vt6h,1);
                #else /* BETA1 or BETAX */
                vector TYPE vc00, vc10, vc20, vc30, vc40, vc50, vc60, vc70;

                vc00 = *((vector TYPE *)(pc0+0));
                vc10 = *((vector TYPE *)(pc0+2));
                vc20 = *((vector TYPE *)(pc0+4));
                vc30 = *((vector TYPE *)(pc0+6));
                vc40 = *((vector TYPE *)(pc0+8));
                vc50 = *((vector TYPE *)(pc0+10));
                vc60 = *((vector TYPE *)(pc0+12));
                vc70 = *((vector TYPE *)(pc0+14));

                vc00 = vec_mergeh(vc00,vc10);
                vc20 = vec_mergeh(vc20,vc30);
                vc40 = vec_mergeh(vc40,vc50);
                vc60 = vec_mergeh(vc60,vc70);

                vc00 = vec_madd(vc00, vbeta, vt0h);
                vc20 = vec_madd(vc20, vbeta, vt2h);
                vc40 = vec_madd(vc40, vbeta, vt4h);
                vc60 = vec_madd(vc60, vbeta, vt6h);

                *(pc0+0)  = vec_extract(vc00,0);
                *(pc0+2)  = vec_extract(vc00,1);
                *(pc0+4)  = vec_extract(vc20,0);
                *(pc0+6)  = vec_extract(vc20,1);
                *(pc0+8)  = vec_extract(vc40,0);
                *(pc0+10) = vec_extract(vc40,1);
                *(pc0+12) = vec_extract(vc60,0);
                *(pc0+14) = vec_extract(vc60,1);
                #endif

                pc0 += 2*4*ldc;

                #endif

                ATL_pfl1R(pc0);
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
                vector TYPE vt0h, vt0l, vt2h, vt2l, vt4h, vt4l, vt6h, vt6l;

                vt0h = vec_mergeh(vt01, vt11);
                vt0l = vec_mergel(vt01, vt11);
                vt2h = vec_mergeh(vt21, vt31);
                vt2l = vec_mergel(vt21, vt31);
                vt4h = vec_mergeh(vt41, vt51);
                vt4l = vec_mergel(vt41, vt51);
                vt6h = vec_mergeh(vt61, vt71);
                vt6l = vec_mergel(vt61, vt71);

                vt0h = vec_add(vt0h, vt0l);
                vt2h = vec_add(vt2h, vt2l);
                vt4h = vec_add(vt4h, vt4l);
                vt6h = vec_add(vt6h, vt6l);

                #ifdef TREAL
                #if defined BETA0
                *((vector TYPE *)(pc1+0)) = vt0h;
                *((vector TYPE *)(pc1+2)) = vt2h;
                *((vector TYPE *)(pc1+4)) = vt4h;
                *((vector TYPE *)(pc1+6)) = vt6h;
                #else /* BETA1 or BETAX */
                vector TYPE vc01, vc21, vc41, vc61;
    
                vc01 = *((vector TYPE *)(pc1+0));
                vc21 = *((vector TYPE *)(pc1+2));
                vc41 = *((vector TYPE *)(pc1+4));
                vc61 = *((vector TYPE *)(pc1+6));

                #if defined BETA1
                vc01 = vec_add(vc01, vt0h);
                vc21 = vec_add(vc21, vt2h);
                vc41 = vec_add(vc41, vt4h);
                vc61 = vec_add(vc61, vt6h);
                #else
                vc01 = vec_madd(vc01, vbeta, vt0h);
                vc21 = vec_madd(vc21, vbeta, vt2h);
                vc41 = vec_madd(vc41, vbeta, vt4h);
                vc61 = vec_madd(vc61, vbeta, vt6h);
                #endif

                *((vector TYPE *)(pc1+0)) = vc01;
                *((vector TYPE *)(pc1+2)) = vc21;
                *((vector TYPE *)(pc1+4)) = vc41;
                *((vector TYPE *)(pc1+6)) = vc61;
                #endif

                pc1 += 4*ldc;

                #else /* TCPLX */
                #if defined BETA0
                *(pc1+0) = vec_extract(vt0h,0);
                *(pc1+2) = vec_extract(vt0h,1);
                *(pc1+4) = vec_extract(vt2h,0);
                *(pc1+6) = vec_extract(vt2h,1);
                *(pc1+8) = vec_extract(vt4h,0);
                *(pc1+10) = vec_extract(vt4h,1);
                *(pc1+12) = vec_extract(vt6h,0);
                *(pc1+14) = vec_extract(vt6h,1);
                #else /* BETA1 or BETAX */
                vector TYPE vc01, vc11, vc21, vc31, vc41, vc51, vc61, vc71;

                vc01 = *((vector TYPE *)(pc1+0));
                vc11 = *((vector TYPE *)(pc1+2));
                vc21 = *((vector TYPE *)(pc1+4));
                vc31 = *((vector TYPE *)(pc1+6));
                vc41 = *((vector TYPE *)(pc1+8));
                vc51 = *((vector TYPE *)(pc1+10));
                vc61 = *((vector TYPE *)(pc1+12));
                vc71 = *((vector TYPE *)(pc1+14));

                vc01 = vec_mergeh(vc01,vc11);
                vc21 = vec_mergeh(vc21,vc31);
                vc41 = vec_mergeh(vc41,vc51);
                vc61 = vec_mergeh(vc61,vc71);

                vc01 = vec_madd(vc01, vbeta, vt0h);
                vc21 = vec_madd(vc21, vbeta, vt2h);
                vc41 = vec_madd(vc41, vbeta, vt4h);
                vc61 = vec_madd(vc61, vbeta, vt6h);

                *(pc1+0)  = vec_extract(vc01,0);
                *(pc1+2)  = vec_extract(vc01,1);
                *(pc1+4)  = vec_extract(vc21,0);
                *(pc1+6)  = vec_extract(vc21,1);
                *(pc1+8)  = vec_extract(vc41,0);
                *(pc1+10) = vec_extract(vc41,1);
                *(pc1+12) = vec_extract(vc61,0);
                *(pc1+14) = vec_extract(vc61,1);
                #endif

                pc1 += 2*4*ldc;

                #endif

                ATL_pfl1R(pc1);
            }

            vt02 = vec_madd(va0, vb2 ,vt02);
            vt12 = vec_madd(va1, vb2 ,vt12);
            vt22 = vec_madd(va2, vb2 ,vt22);
            vt32 = vec_madd(va3, vb2 ,vt32);
            vt42 = vec_madd(va4, vb2 ,vt42);
            vt52 = vec_madd(va5, vb2 ,vt52);
            vt62 = vec_madd(va6, vb2 ,vt62);
            vt72 = vec_madd(va7, vb2 ,vt72);

            {
                vector TYPE vt0h, vt0l, vt2h, vt2l, vt4h, vt4l, vt6h, vt6l;

                vt0h = vec_mergeh(vt02, vt12);
                vt0l = vec_mergel(vt02, vt12);
                vt2h = vec_mergeh(vt22, vt32);
                vt2l = vec_mergel(vt22, vt32);
                vt4h = vec_mergeh(vt42, vt52);
                vt4l = vec_mergel(vt42, vt52);
                vt6h = vec_mergeh(vt62, vt72);
                vt6l = vec_mergel(vt62, vt72);

                vt0h = vec_add(vt0h, vt0l);
                vt2h = vec_add(vt2h, vt2l);
                vt4h = vec_add(vt4h, vt4l);
                vt6h = vec_add(vt6h, vt6l);

                #ifdef TREAL
                #if defined BETA0
                *((vector TYPE *)(pc2+0)) = vt0h;
                *((vector TYPE *)(pc2+2)) = vt2h;
                *((vector TYPE *)(pc2+4)) = vt4h;
                *((vector TYPE *)(pc2+6)) = vt6h;
                #else /* BETA1 or BETAX */
                vector TYPE vc02, vc22, vc42, vc62;
    
                vc02 = *((vector TYPE *)(pc2+0));
                vc22 = *((vector TYPE *)(pc2+2));
                vc42 = *((vector TYPE *)(pc2+4));
                vc62 = *((vector TYPE *)(pc2+6));

                #if defined BETA1
                vc02 = vec_add(vc02, vt0h);
                vc22 = vec_add(vc22, vt2h);
                vc42 = vec_add(vc42, vt4h);
                vc62 = vec_add(vc62, vt6h);
                #else
                vc02 = vec_madd(vc02, vbeta, vt0h);
                vc22 = vec_madd(vc22, vbeta, vt2h);
                vc42 = vec_madd(vc42, vbeta, vt4h);
                vc62 = vec_madd(vc62, vbeta, vt6h);
                #endif
                *((vector TYPE *)(pc2+0)) = vc02;
                *((vector TYPE *)(pc2+2)) = vc22;
                *((vector TYPE *)(pc2+4)) = vc42;
                *((vector TYPE *)(pc2+6)) = vc62;
                #endif

                pc2 += 4*ldc;

                #else /* TCPLX */
                #if defined BETA0
                *(pc2+0)  = vec_extract(vt0h,0);
                *(pc2+2)  = vec_extract(vt0h,1);
                *(pc2+4)  = vec_extract(vt2h,0);
                *(pc2+6)  = vec_extract(vt2h,1);
                *(pc2+8)  = vec_extract(vt4h,0);
                *(pc2+10) = vec_extract(vt4h,1);
                *(pc2+12) = vec_extract(vt6h,0);
                *(pc2+14) = vec_extract(vt6h,1);
                #else /* BETA1 or BETAX */
                vector TYPE vc02, vc12, vc22, vc32, vc42, vc52, vc62, vc72;

                vc02 = *((vector TYPE *)(pc2+0));
                vc12 = *((vector TYPE *)(pc2+2));
                vc22 = *((vector TYPE *)(pc2+4));
                vc32 = *((vector TYPE *)(pc2+6));
                vc42 = *((vector TYPE *)(pc2+8));
                vc52 = *((vector TYPE *)(pc2+10));
                vc62 = *((vector TYPE *)(pc2+12));
                vc72 = *((vector TYPE *)(pc2+14));

                vc02 = vec_mergeh(vc02,vc12);
                vc22 = vec_mergeh(vc22,vc32);
                vc42 = vec_mergeh(vc42,vc52);
                vc62 = vec_mergeh(vc62,vc72);

                vc02 = vec_madd(vc02, vbeta, vt0h);
                vc22 = vec_madd(vc22, vbeta, vt2h);
                vc42 = vec_madd(vc42, vbeta, vt4h);
                vc62 = vec_madd(vc62, vbeta, vt6h);

                *(pc2+0)  = vec_extract(vc02,0);
                *(pc2+2)  = vec_extract(vc02,1);
                *(pc2+4)  = vec_extract(vc22,0);
                *(pc2+6)  = vec_extract(vc22,1);
                *(pc2+8)  = vec_extract(vc42,0);
                *(pc2+10) = vec_extract(vc42,1);
                *(pc2+12) = vec_extract(vc62,0);
                *(pc2+14) = vec_extract(vc62,1);
                #endif

                pc2 += 2*4*ldc;

                #endif

                ATL_pfl1R(pc2);
            }

            vt03 = vec_madd(va0, vb3, vt03);
            vt13 = vec_madd(va1, vb3, vt13);
            vt23 = vec_madd(va2, vb3, vt23);
            vt33 = vec_madd(va3, vb3, vt33);
            vt43 = vec_madd(va4, vb3, vt43);
            vt53 = vec_madd(va5, vb3, vt53);
            vt63 = vec_madd(va6, vb3, vt63);
            vt73 = vec_madd(va7, vb3, vt73);

            {
                vector TYPE vt0h, vt0l, vt2h, vt2l, vt4h, vt4l, vt6h, vt6l;
  
                vt0h = vec_mergeh(vt03, vt13);
                vt0l = vec_mergel(vt03, vt13);
                vt2h = vec_mergeh(vt23, vt33);
                vt2l = vec_mergel(vt23, vt33);
                vt4h = vec_mergeh(vt43, vt53);
                vt4l = vec_mergel(vt43, vt53);
                vt6h = vec_mergeh(vt63, vt73);
                vt6l = vec_mergel(vt63, vt73);

                vt0h = vec_add(vt0h, vt0l);
                vt2h = vec_add(vt2h, vt2l);
                vt4h = vec_add(vt4h, vt4l);
                vt6h = vec_add(vt6h, vt6l);

                #ifdef TREAL
                #if defined BETA0
                *((vector TYPE *)(pc3+0)) = vt0h;
                *((vector TYPE *)(pc3+2)) = vt2h;
                *((vector TYPE *)(pc3+4)) = vt4h;
                *((vector TYPE *)(pc3+6)) = vt6h;
                #else /* BETA1 or BETAX */
                vector TYPE vc03, vc23, vc43, vc63;
    
                vc03 = *((vector TYPE *)(pc3+0));
                vc23 = *((vector TYPE *)(pc3+2));
                vc43 = *((vector TYPE *)(pc3+4));
                vc63 = *((vector TYPE *)(pc3+6));

                #if defined BETA1
                vc03 = vec_add(vc03, vt0h);
                vc23 = vec_add(vc23, vt2h);
                vc43 = vec_add(vc43, vt4h);
                vc63 = vec_add(vc63, vt6h);
                #else
                vc03 = vec_madd(vc03, vbeta, vt0h);
                vc23 = vec_madd(vc23, vbeta, vt2h);
                vc43 = vec_madd(vc43, vbeta, vt4h);
                vc63 = vec_madd(vc63, vbeta, vt6h);
                #endif
                *((vector TYPE *)(pc3+0)) = vc03;
                *((vector TYPE *)(pc3+2)) = vc23;
                *((vector TYPE *)(pc3+4)) = vc43;
                *((vector TYPE *)(pc3+6)) = vc63;
                #endif

                pc3 += 4*ldc;

                #else /* TCPLX */
                #if defined BETA0
                *(pc3+0)  = vec_extract(vt0h,0);
                *(pc3+2)  = vec_extract(vt0h,1);
                *(pc3+4)  = vec_extract(vt2h,0);
                *(pc3+6)  = vec_extract(vt2h,1);
                *(pc3+8)  = vec_extract(vt4h,0);
                *(pc3+10) = vec_extract(vt4h,1);
                *(pc3+12) = vec_extract(vt6h,0);
                *(pc3+14) = vec_extract(vt6h,1);
                #else /* BETA1 or BETAX */
                vector TYPE vc03, vc13, vc23, vc33, vc43, vc53, vc63, vc73;

                vc03 = *((vector TYPE *)(pc3+0));
                vc13 = *((vector TYPE *)(pc3+2));
                vc23 = *((vector TYPE *)(pc3+4));
                vc33 = *((vector TYPE *)(pc3+6));
                vc43 = *((vector TYPE *)(pc3+8));
                vc53 = *((vector TYPE *)(pc3+10));
                vc63 = *((vector TYPE *)(pc3+12));
                vc73 = *((vector TYPE *)(pc3+14));

                vc03 = vec_mergeh(vc03,vc13);
                vc23 = vec_mergeh(vc23,vc33);
                vc43 = vec_mergeh(vc43,vc53);
                vc63 = vec_mergeh(vc63,vc73);

                vc03 = vec_madd(vc03, vbeta, vt0h);
                vc23 = vec_madd(vc23, vbeta, vt2h);
                vc43 = vec_madd(vc43, vbeta, vt4h);
                vc63 = vec_madd(vc63, vbeta, vt6h);

                *(pc3+0)  = vec_extract(vc03,0);
                *(pc3+2)  = vec_extract(vc03,1);
                *(pc3+4)  = vec_extract(vc23,0);
                *(pc3+6)  = vec_extract(vc23,1);
                *(pc3+8)  = vec_extract(vc43,0);
                *(pc3+10) = vec_extract(vc43,1);
                *(pc3+12) = vec_extract(vc63,0);
                *(pc3+14) = vec_extract(vc63,1);
                #endif

                pc3 += 2*4*ldc;

                #endif

                ATL_pfl1R(pc3);
            }
        }
    }
}
