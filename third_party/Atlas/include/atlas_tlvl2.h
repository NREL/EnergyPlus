#ifndef ATLAS_TLVL2_H
   #define ATLAS_TLVL2_H

void Mjoin(PATL,tgemv)
   (const enum ATLAS_TRANS TA, ATL_CINT M, ATL_CINT N, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *X, ATL_CINT incX,
    const SCALAR beta, TYPE *Y, ATL_CINT incY);

#ifdef TREAL
   void Mjoin(PATL,tger)
         (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *X,
          ATL_CINT incX, const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda);
#else
   void Mjoin(PATL,tgeru)
         (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *X,
          ATL_CINT incX, const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda);
   void Mjoin(PATL,tgerc)
         (ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *X,
          ATL_CINT incX, const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda);
#endif

#endif   /* end repeat include guard */
