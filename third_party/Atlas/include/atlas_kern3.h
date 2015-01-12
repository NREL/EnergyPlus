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
#ifndef ATLAS_KERN3_H
#define ATLAS_KERN3_H

#include "atlas_misc.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),NCmm.h))
#include "atlas_lvl3.h"
#include "atlas_kernel3.h"
#include "atlas_reflevel3.h"
/*
 * Gemm entry points
 */
#define CgemmNN Mjoin(PATL,gemmNN)
#define CgemmNT Mjoin(PATL,gemmNT)
#define CgemmTN Mjoin(PATL,gemmTN)
#define CgemmNC Mjoin(PATL,gemmNC)
#define CgemmCN Mjoin(PATL,gemmCN)

#define CAgemmNN Mjoin(PATL,aliased_gemmNN)
#define CAgemmTN Mjoin(PATL,aliased_gemmTN)

#ifdef Left_
   #define Side_ AtlasLeft
   #define SideNM L
#elif defined(Right_)
   #define Side_ AtlasRight
   #define SideNM R
#endif

#ifdef Upper_
   #define Uplo_ AtlasUpper
   #define UploNM U
#elif defined(Lower_)
   #define Uplo_ AtlasLower
   #define UploNM L
#endif

#ifdef UnitDiag_
   #define Unit_ AtlasUnit
   #define UnitNM U
#elif defined(NonUnitDiag_)
   #define Unit_ AtlasNonUnit
   #define UnitNM N
#endif

#ifdef Transpose_
   #define Trans_ AtlasTrans
   #define TransNM T
#elif defined(Notranspose_)
   #define Trans_ AtlasNoTrans
   #define TransNM N
#elif defined(ConjTrans_)
   #define Trans_ AtlasConjTrans
   #define TransNM C
#endif

#ifndef TRSM_Xover
   #define TRSM_Xover NB
#endif
#ifndef TRMM_Xover
   #define TRMM_Xover NB
#endif
#ifndef HER2K_Xover
   #define HER2K_Xover NB
#endif
#ifndef SYR2K_Xover
   #define SYR2K_Xover NB
#endif
#ifndef HERK_Xover
   #define HERK_Xover NB
#endif
#ifndef SYRK_Xover
   #define SYRK_Xover NB
#endif
#ifndef HEMM_Xover
   #define HEMM_Xover NB
#endif
#ifndef SYMM_Xover
   #define SYMM_Xover NB
#endif

#endif
