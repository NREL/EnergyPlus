/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 1999 Antoine P. Petitet
 *
 * Code contributers : Antoine P. Petitet, R. Clint Whaley
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
#include "atlas_tst.h"
#include "atlas_f77blas.h"

void Mjoin( PATL, f77tpmv )
(
   const enum ATLAS_UPLO     UPLO,
   const enum ATLAS_TRANS    TRANS,
   const enum ATLAS_DIAG     DIAG,
   const int                 N,
   const TYPE                * A,
   TYPE                      * X,
   const int                 INCX
)
{
#if defined( StringSunStyle )
   #if defined( ATL_FunkyInts )
   F77_INTEGER               ONE = 1;
   #else
   int                       ONE = 1;
   #endif
#elif defined( StringStructVal ) || defined( StringStructPtr )
   F77_CHAR                  fuplo, ftran, fdiag;
#elif defined( StringCrayStyle )
   F77_CHAR                  fuplo, ftran, fdiag;
#endif

   char                      cuplo, ctran, cdiag;

#ifdef ATL_FunkyInts
   const F77_INTEGER         F77N = N,                         F77incx = INCX;
#else
   #define F77N              N
   #define F77incx           INCX
#endif

   if(      UPLO  == AtlasUpper   ) cuplo = 'U';
   else                             cuplo = 'L';

   if(      DIAG  == AtlasNonUnit ) cdiag = 'N';
   else                             cdiag = 'U';

   if(      TRANS == AtlasNoTrans ) ctran = 'N';
   else if( TRANS == AtlasTrans   ) ctran = 'T';
   else                             ctran = 'C';

   if( INCX < 0 ) X -= ( ( 1 - N ) ) * INCX SHIFT;

#if defined(StringSunStyle)
   F77tpmv( &cuplo, &ctran, &cdiag, &F77N,        A,          X, &F77incx,
            ONE, ONE, ONE );
#elif defined(StringCrayStyle)
   ftran = ATL_C2F_TransChar( ctran );
   fdiag = ATL_C2F_TransChar( cdiag );
   fuplo = ATL_C2F_TransChar( cuplo );
   F77tpmv( fuplo,  ftran,  fdiag,  &F77N,        A,          X, &F77incx );
#elif defined(StringStructVal)
   fuplo.len = 1; fuplo.cp = &cuplo;
   ftran.len = 1; ftran.cp = &ctran;
   fdiag.len = 1; fdiag.cp = &cdiag;
   F77tpmv( fuplo,  ftran,  fdiag,  &F77N,        A,          X, &F77incx );
#elif defined(StringStructPtr)
   fuplo.len = 1; fuplo.cp = &cuplo;
   F77tpmv( &fuplo, &ftran, &fdiag, &F77N,        A,          X, &F77incx );
   ftran.len = 1; ftran.cp = &ctran;
   fdiag.len = 1; fdiag.cp = &cdiag;
#else
   (void) fprintf( stderr, "\n\nF77/C interface not defined!!\n\n" );
   exit(-1);
#endif
}
