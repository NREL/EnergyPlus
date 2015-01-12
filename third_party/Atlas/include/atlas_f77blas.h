#ifndef ATLAS_F77_LVLS
#define ATLAS_F77_LVLS

#include "atlas_f77.h"

#if   defined( StringSunStyle  )
#define F77_CHAR_DECL          F77_CHAR          /* input character*1 */
#define F77_1_CHAR             , F77_INTEGER
#define F77_2_CHAR             F77_1_CHAR F77_1_CHAR
#define F77_3_CHAR             F77_2_CHAR F77_1_CHAR
#define F77_4_CHAR             F77_3_CHAR F77_1_CHAR
#elif defined( StringCrayStyle )
#define F77_CHAR_DECL          F77_CHAR          /* input character*1 */
#elif defined( StringStructVal )
#define F77_CHAR_DECL          F77_CHAR          /* input character*1 */
#elif defined( StringStructPtr )
#define F77_CHAR_DECL          F77_CHAR *        /* input character*1 */
#endif

#ifndef F77_1_CHAR
#define F77_1_CHAR
#define F77_2_CHAR
#define F77_3_CHAR
#define F77_4_CHAR
#endif

#ifndef F77_CHAR_DECL
   #define F77_CHAR_DECL          F77_CHAR *        /* input character*1 */
#endif

#define F77_INT_DECL           const F77_INTEGER *   /* input integer */

#ifdef TREAL
#define F77_SIN_DECL           const TYPE *           /* input scalar */
#define F77_SINOUT_DECL        TYPE *          /* input/output scalar */
#define F77_RIN_DECL           const TYPE *      /* input real scalar */
#define F77_RINOUT_DECL        TYPE *     /* input/output real scalar */
#else
#define F77_SIN_DECL           const TYPE *           /* input scalar */
#define F77_SINOUT_DECL        TYPE *          /* input/output scalar */
#define F77_RIN_DECL           const TYPE *      /* input real scalar */
#define F77_RINOUT_DECL        TYPE *     /* input/output real scalar */
#endif

#define F77_VIN_DECL           const TYPE *           /* input vector */
#define F77_VINOUT_DECL        TYPE *          /* input/output matrix */

#define F77_MIN_DECL           const TYPE *           /* input matrix */
#define F77_MINOUT_DECL        TYPE *          /* input/output matrix */

#if   defined( CRAY )
#define F77_VOID_FUN           extern fortran void      /* subroutine */
#define F77_INT_FUN            extern fortran int /* integer function */
#define F77_TYPE_FUN           extern fortran TYPE   /* real function */
#define F77_DBLE_FUN           extern fortran double /* dble function */
#else
#define F77_VOID_FUN           extern void              /* subroutine */
#define F77_INT_FUN            extern int         /* integer function */
#define F77_TYPE_FUN           extern TYPE           /* real function */
#define F77_DBLE_FUN           extern double         /* dble function */
#endif

#if   defined( NoChange )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine called by a C routine with the following  FORTRAN to C inter-
 * face:
 *
 *          FORTRAN DECLARATION            C CALL
 *          SUBROUTINE DGEMM(...)          dgemm(...)
 */
#if   defined( SREAL )

#define    F77rotg             srotg
#define    F77rotmg            srotmg
#define    F77nrm2             swrapnrm2
#define    F77asum             swrapasum
#define    F77amax             isamax
#define    F77scal             sscal
#define    F77axpy             saxpy
#define    F77axpby            fatlas_saxpby
#define    F77set              fatlas_sset
#define    F77copy             scopy
#define    F77swap             sswap
#define    F77rot              srot
#define    F77rotm             srotm
#define    F77dot              swrapdot
#define    F77dsdot            dswrapdot
#define    F77sdsdot           sdswrapdot

#define    F77gemv             sgemv
#define    F77gbmv             sgbmv
#define    F77sbmv             ssbmv
#define    F77spmv             sspmv
#define    F77symv             ssymv
#define    F77tbmv             stbmv
#define    F77tpmv             stpmv
#define    F77trmv             strmv
#define    F77tbsv             stbsv
#define    F77tpsv             stpsv
#define    F77trsv             strsv
#define    F77ger              sger
#define    F77spr              sspr
#define    F77syr              ssyr
#define    F77spr2             sspr2
#define    F77syr2             ssyr2

#define    F77gemm             sgemm
#define    F77symm             ssymm
#define    F77syrk             ssyrk
#define    F77syr2k            ssyr2k
#define    F77trmm             strmm
#define    F77trsm             strsm

#elif defined( DREAL )

#define    F77rotg             drotg
#define    F77rotmg            drotmg
#define    F77nrm2             dwrapnrm2
#define    F77asum             dwrapasum
#define    F77amax             idamax
#define    F77scal             dscal
#define    F77axpy             daxpy
#define    F77axpby            fatlas_daxpby
#define    F77set              fatlas_dset
#define    F77copy             dcopy
#define    F77swap             dswap
#define    F77rot              drot
#define    F77rotm             drotm
#define    F77dot              dwrapdot

#define    F77gemv             dgemv
#define    F77gbmv             dgbmv
#define    F77sbmv             dsbmv
#define    F77spmv             dspmv
#define    F77symv             dsymv
#define    F77tbmv             dtbmv
#define    F77tpmv             dtpmv
#define    F77trmv             dtrmv
#define    F77tbsv             dtbsv
#define    F77tpsv             dtpsv
#define    F77trsv             dtrsv
#define    F77ger              dger
#define    F77spr              dspr
#define    F77syr              dsyr
#define    F77spr2             dspr2
#define    F77syr2             dsyr2

#define    F77gemm             dgemm
#define    F77symm             dsymm
#define    F77syrk             dsyrk
#define    F77syr2k            dsyr2k
#define    F77trmm             dtrmm
#define    F77trsm             dtrsm

#elif defined( SCPLX )

#define    F77rotg             crotg
#define    F77nrm2             scwrapnrm2
#define    F77asum             scwrapasum
#define    F77amax             icamax
#define    F77scal             cscal
#define    F77rscal            csscal
#define    F77axpy             caxpy
#define    F77axpby            fatlas_caxpby
#define    F77set              fatlas_cset
#define    F77copy             ccopy
#define    F77swap             cswap
#define    F77rot              csrot
#define    F77dotc             cwrapdotc
#define    F77dotu             cwrapdotu

#define    F77gbmv             cgbmv
#define    F77gemv             cgemv
#define    F77hbmv             chbmv
#define    F77hpmv             chpmv
#define    F77hemv             chemv
#define    F77tbmv             ctbmv
#define    F77tpmv             ctpmv
#define    F77trmv             ctrmv
#define    F77tbsv             ctbsv
#define    F77tpsv             ctpsv
#define    F77trsv             ctrsv
#define    F77gerc             cgerc
#define    F77geru             cgeru
#define    F77hpr              chpr
#define    F77her              cher
#define    F77hpr2             chpr2
#define    F77her2             cher2

#define    F77gemm             cgemm
#define    F77hemm             chemm
#define    F77herk             cherk
#define    F77her2k            cher2k
#define    F77symm             csymm
#define    F77syrk             csyrk
#define    F77syr2k            csyr2k
#define    F77trmm             ctrmm
#define    F77trsm             ctrsm

#elif defined( DCPLX )

#define    F77rotg             zrotg
#define    F77nrm2             dzwrapnrm2
#define    F77asum             dzwrapasum
#define    F77amax             izamax
#define    F77scal             zscal
#define    F77rscal            zdscal
#define    F77axpy             zaxpy
#define    F77axpby            fatlas_zaxpby
#define    F77set              fatlas_zset
#define    F77copy             zcopy
#define    F77swap             zswap
#define    F77rot              zdrot
#define    F77dotc             zwrapdotc
#define    F77dotu             zwrapdotu

#define    F77gbmv             zgbmv
#define    F77gemv             zgemv
#define    F77hbmv             zhbmv
#define    F77hpmv             zhpmv
#define    F77hemv             zhemv
#define    F77tbmv             ztbmv
#define    F77tpmv             ztpmv
#define    F77trmv             ztrmv
#define    F77tbsv             ztbsv
#define    F77tpsv             ztpsv
#define    F77trsv             ztrsv
#define    F77gerc             zgerc
#define    F77geru             zgeru
#define    F77hpr              zhpr
#define    F77her              zher
#define    F77hpr2             zhpr2
#define    F77her2             zher2

#define    F77gemm             zgemm
#define    F77hemm             zhemm
#define    F77herk             zherk
#define    F77her2k            zher2k
#define    F77symm             zsymm
#define    F77syrk             zsyrk
#define    F77syr2k            zsyr2k
#define    F77trmm             ztrmm
#define    F77trsm             ztrsm

#endif

#elif defined( UpCase   )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine called by a C routine with the following  FORTRAN to C inter-
 * face:
 *
 *          FORTRAN DECLARATION            C CALL
 *          SUBROUTINE DGEMM(...)          DGEMM(...)
 */
#if   defined( SREAL )

#define    F77rotg             SROTG
#define    F77rotmg            SROTMG
#define    F77nrm2             SWRAPNRM2
#define    F77asum             SWRAPASUM
#define    F77amax             ISAMAX
#define    F77scal             SSCAL
#define    F77axpy             SAXPY
#define    F77axpby            FATLAS_SAXPBY
#define    F77set              FATLAS_SSET
#define    F77copy             SCOPY
#define    F77swap             SSWAP
#define    F77rot              SROT
#define    F77rotm             SROTM
#define    F77dot              SWRAPDOT
#define    F77dsdot            DSWRAPDOT
#define    F77sdsdot           SDSWRAPDOT

#define    F77gemv             SGEMV
#define    F77gbmv             SGBMV
#define    F77sbmv             SSBMV
#define    F77spmv             SSPMV
#define    F77symv             SSYMV
#define    F77tbmv             STBMV
#define    F77tpmv             STPMV
#define    F77trmv             STRMV
#define    F77tbsv             STBSV
#define    F77tpsv             STPSV
#define    F77trsv             STRSV
#define    F77ger              SGER
#define    F77spr              SSPR
#define    F77syr              SSYR
#define    F77spr2             SSPR2
#define    F77syr2             SSYR2

#define    F77gemm             SGEMM
#define    F77symm             SSYMM
#define    F77syrk             SSYRK
#define    F77syr2k            SSYR2K
#define    F77trmm             STRMM
#define    F77trsm             STRSM

#elif defined( DREAL )

#define    F77rotg             DROTG
#define    F77rotmg            DROTMG
#define    F77nrm2             DWRAPNRM2
#define    F77asum             DWRAPASUM
#define    F77amax             IDAMAX
#define    F77scal             DSCAL
#define    F77axpy             DAXPY
#define    F77axpby            FATLAS_DAXPBY
#define    F77set              FATLAS_DSET
#define    F77copy             DCOPY
#define    F77swap             DSWAP
#define    F77rot              DROT
#define    F77rotm             DROTM
#define    F77dot              DWRAPDOT

#define    F77gemv             DGEMV
#define    F77gbmv             DGBMV
#define    F77sbmv             DSBMV
#define    F77spmv             DSPMV
#define    F77symv             DSYMV
#define    F77tbmv             DTBMV
#define    F77tpmv             DTPMV
#define    F77trmv             DTRMV
#define    F77tbsv             DTBSV
#define    F77tpsv             DTPSV
#define    F77trsv             DTRSV
#define    F77ger              DGER
#define    F77spr              DSPR
#define    F77syr              DSYR
#define    F77spr2             DSPR2
#define    F77syr2             DSYR2

#define    F77gemm             DGEMM
#define    F77symm             DSYMM
#define    F77syrk             DSYRK
#define    F77syr2k            DSYR2K
#define    F77trmm             DTRMM
#define    F77trsm             DTRSM

#elif defined( SCPLX )

#define    F77rotg             CROTG
#define    F77nrm2             SCWRAPNRM2
#define    F77asum             SCWRAPASUM
#define    F77amax             ICAMAX
#define    F77scal             CSCAL
#define    F77rscal            CSSCAL
#define    F77axpy             CAXPY
#define    F77axpby            FATLAS_CAXPBY
#define    F77set              FATLAS_CSET
#define    F77copy             CCOPY
#define    F77swap             CSWAP
#define    F77rot              CSROT
#define    F77dotc             CWRAPDOTC
#define    F77dotu             CWRAPDOTU

#define    F77gbmv             CGBMV
#define    F77gemv             CGEMV
#define    F77hbmv             CHBMV
#define    F77hpmv             CHPMV
#define    F77hemv             CHEMV
#define    F77tbmv             CTBMV
#define    F77tpmv             CTPMV
#define    F77trmv             CTRMV
#define    F77tbsv             CTBSV
#define    F77tpsv             CTPSV
#define    F77trsv             CTRSV
#define    F77gerc             CGERC
#define    F77geru             CGERU
#define    F77hpr              CHPR
#define    F77her              CHER
#define    F77hpr2             CHPR2
#define    F77her2             CHER2

#define    F77gemm             CGEMM
#define    F77hemm             CHEMM
#define    F77herk             CHERK
#define    F77her2k            CHER2K
#define    F77symm             CSYMM
#define    F77syrk             CSYRK
#define    F77syr2k            CSYR2K
#define    F77trmm             CTRMM
#define    F77trsm             CTRSM

#elif defined( DCPLX )

#define    F77rotg             ZROTG
#define    F77nrm2             DZWRAPNRM2
#define    F77asum             DZWRAPASUM
#define    F77amax             IZAMAX
#define    F77scal             ZSCAL
#define    F77rscal            ZDSCAL
#define    F77axpy             ZAXPY
#define    F77axpby            FATLAS_ZAXPBY
#define    F77set              FATLAS_ZSET
#define    F77copy             ZCOPY
#define    F77swap             ZSWAP
#define    F77rot              ZDROT
#define    F77dotc             ZWRAPDOTC
#define    F77dotu             ZWRAPDOTU

#define    F77gbmv             ZGBMV
#define    F77gemv             ZGEMV
#define    F77hbmv             ZHBMV
#define    F77hpmv             ZHPMV
#define    F77hemv             ZHEMV
#define    F77tbmv             ZTBMV
#define    F77tpmv             ZTPMV
#define    F77trmv             ZTRMV
#define    F77tbsv             ZTBSV
#define    F77tpsv             ZTPSV
#define    F77trsv             ZTRSV
#define    F77gerc             ZGERC
#define    F77geru             ZGERU
#define    F77hpr              ZHPR
#define    F77her              ZHER
#define    F77hpr2             ZHPR2
#define    F77her2             ZHER2

#define    F77gemm             ZGEMM
#define    F77hemm             ZHEMM
#define    F77herk             ZHERK
#define    F77her2k            ZHER2K
#define    F77symm             ZSYMM
#define    F77syrk             ZSYRK
#define    F77syr2k            ZSYR2K
#define    F77trmm             ZTRMM
#define    F77trsm             ZTRSM

#endif

#elif defined( Add_     ) || defined( Add__    )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine called by a C routine  with the following  FORTRAN to C inter-
 * face:
 *
 *          FORTRAN DECLARATION            C CALL
 *          SUBROUTINE DGEMM(...)          dgemm_(...)
 */
#if   defined( SREAL )

#define    F77rotg             srotg_
#define    F77rotmg            srotmg_
#define    F77nrm2             swrapnrm2_
#define    F77asum             swrapasum_
#define    F77amax             isamax_
#define    F77scal             sscal_
#define    F77axpy             saxpy_
#ifdef Add_
   #define    F77axpby            fatlas_saxpby_
   #define    F77set              fatlas_sset_
#else
   #define    F77axpby            fatlas_saxpby__
   #define    F77set              fatlas_sset__
#endif
#define    F77copy             scopy_
#define    F77swap             sswap_
#define    F77rot              srot_
#define    F77rotm             srotm_
#define    F77dot              swrapdot_
#define    F77dsdot            dswrapdot_
#define    F77sdsdot           sdswrapdot_

#define    F77gemv             sgemv_
#define    F77gbmv             sgbmv_
#define    F77sbmv             ssbmv_
#define    F77spmv             sspmv_
#define    F77symv             ssymv_
#define    F77tbmv             stbmv_
#define    F77tpmv             stpmv_
#define    F77trmv             strmv_
#define    F77tbsv             stbsv_
#define    F77tpsv             stpsv_
#define    F77trsv             strsv_
#define    F77ger              sger_
#define    F77spr              sspr_
#define    F77syr              ssyr_
#define    F77spr2             sspr2_
#define    F77syr2             ssyr2_

#define    F77gemm             sgemm_
#define    F77symm             ssymm_
#define    F77syrk             ssyrk_
#define    F77syr2k            ssyr2k_
#define    F77trmm             strmm_
#define    F77trsm             strsm_

#elif defined( DREAL )

#define    F77rotg             drotg_
#define    F77rotmg            drotmg_
#define    F77nrm2             dwrapnrm2_
#define    F77asum             dwrapasum_
#define    F77amax             idamax_
#define    F77scal             dscal_
#define    F77axpy             daxpy_
#ifdef Add_
   #define    F77axpby            fatlas_daxpby_
   #define    F77set              fatlas_dset_
#else
   #define    F77axpby            fatlas_daxpby__
   #define    F77set              fatlas_dset__
#endif
#define    F77copy             dcopy_
#define    F77swap             dswap_
#define    F77rot              drot_
#define    F77rotm             drotm_
#define    F77dot              dwrapdot_

#define    F77gemv             dgemv_
#define    F77gbmv             dgbmv_
#define    F77sbmv             dsbmv_
#define    F77spmv             dspmv_
#define    F77symv             dsymv_
#define    F77tbmv             dtbmv_
#define    F77tpmv             dtpmv_
#define    F77trmv             dtrmv_
#define    F77tbsv             dtbsv_
#define    F77tpsv             dtpsv_
#define    F77trsv             dtrsv_
#define    F77ger              dger_
#define    F77spr              dspr_
#define    F77syr              dsyr_
#define    F77spr2             dspr2_
#define    F77syr2             dsyr2_

#define    F77gemm             dgemm_
#define    F77symm             dsymm_
#define    F77syrk             dsyrk_
#define    F77syr2k            dsyr2k_
#define    F77trmm             dtrmm_
#define    F77trsm             dtrsm_

#elif defined( SCPLX )

#define    F77rotg             crotg_
#define    F77nrm2             scwrapnrm2_
#define    F77asum             scwrapasum_
#define    F77amax             icamax_
#define    F77scal             cscal_
#define    F77rscal            csscal_
#define    F77axpy             caxpy_
#ifdef Add_
   #define    F77axpby            fatlas_caxpby_
   #define    F77set              fatlas_cset_
#else
   #define    F77axpby            fatlas_caxpby__
   #define    F77set              fatlas_cset__
#endif
#define    F77copy             ccopy_
#define    F77swap             cswap_
#define    F77rot              csrot_
#define    F77dotc             cwrapdotc_
#define    F77dotu             cwrapdotu_

#define    F77gbmv             cgbmv_
#define    F77gemv             cgemv_
#define    F77hbmv             chbmv_
#define    F77hpmv             chpmv_
#define    F77hemv             chemv_
#define    F77tbmv             ctbmv_
#define    F77tpmv             ctpmv_
#define    F77trmv             ctrmv_
#define    F77tbsv             ctbsv_
#define    F77tpsv             ctpsv_
#define    F77trsv             ctrsv_
#define    F77gerc             cgerc_
#define    F77geru             cgeru_
#define    F77hpr              chpr_
#define    F77her              cher_
#define    F77hpr2             chpr2_
#define    F77her2             cher2_

#define    F77gemm             cgemm_
#define    F77hemm             chemm_
#define    F77herk             cherk_
#define    F77her2k            cher2k_
#define    F77symm             csymm_
#define    F77syrk             csyrk_
#define    F77syr2k            csyr2k_
#define    F77trmm             ctrmm_
#define    F77trsm             ctrsm_

#elif defined( DCPLX )

#define    F77rotg             zrotg_
#define    F77nrm2             dzwrapnrm2_
#define    F77asum             dzwrapasum_
#define    F77amax             izamax_
#define    F77scal             zscal_
#define    F77rscal            zdscal_
#define    F77axpy             zaxpy_
#ifdef Add_
   #define    F77axpby            fatlas_zaxpby_
   #define    F77set              fatlas_zset_
#else
   #define    F77axpby            fatlas_zaxpby__
   #define    F77set              fatlas_zset__
#endif
#define    F77copy             zcopy_
#define    F77swap             zswap_
#define    F77rot              zdrot_
#define    F77dotc             zwrapdotc_
#define    F77dotu             zwrapdotu_

#define    F77gbmv             zgbmv_
#define    F77gemv             zgemv_
#define    F77hbmv             zhbmv_
#define    F77hpmv             zhpmv_
#define    F77hemv             zhemv_
#define    F77tbmv             ztbmv_
#define    F77tpmv             ztpmv_
#define    F77trmv             ztrmv_
#define    F77tbsv             ztbsv_
#define    F77tpsv             ztpsv_
#define    F77trsv             ztrsv_
#define    F77gerc             zgerc_
#define    F77geru             zgeru_
#define    F77hpr              zhpr_
#define    F77her              zher_
#define    F77hpr2             zhpr2_
#define    F77her2             zher2_

#define    F77gemm             zgemm_
#define    F77hemm             zhemm_
#define    F77herk             zherk_
#define    F77her2k            zher2k_
#define    F77symm             zsymm_
#define    F77syrk             zsyrk_
#define    F77syr2k            zsyr2k_
#define    F77trmm             ztrmm_
#define    F77trsm             ztrsm_

#endif

#endif

#ifdef TREAL
F77_VOID_FUN    F77rotg
( F77_SINOUT_DECL, F77_SINOUT_DECL, F77_SINOUT_DECL, F77_SINOUT_DECL );
F77_VOID_FUN    F77rotmg
( F77_SINOUT_DECL, F77_SINOUT_DECL, F77_SINOUT_DECL, F77_SIN_DECL,
  F77_VINOUT_DECL );
#else
F77_VOID_FUN    F77rotg
( F77_SINOUT_DECL, F77_SIN_DECL,    F77_SINOUT_DECL, F77_SINOUT_DECL );
#endif
F77_VOID_FUN    F77nrm2
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_RINOUT_DECL );
F77_VOID_FUN    F77asum
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_RINOUT_DECL );
F77_INT_FUN     F77amax
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL );
F77_VOID_FUN    F77scal
( F77_INT_DECL,    F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL );
#ifdef TCPLX
F77_VOID_FUN    F77rscal
( F77_INT_DECL,    F77_RIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL );
#endif
void F77set
( F77_INT_DECL,    F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL );
void F77axpby
( F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL, F77_VINOUT_DECL, F77_INT_DECL );
F77_VOID_FUN    F77axpy
( F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_VINOUT_DECL, F77_INT_DECL );
F77_VOID_FUN    F77copy
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL );
F77_VOID_FUN    F77swap
( F77_INT_DECL,    F77_VINOUT_DECL, F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL );
F77_VOID_FUN    F77rot
( F77_INT_DECL,    F77_VINOUT_DECL, F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL,    F77_SIN_DECL,    F77_SIN_DECL );
#ifdef TREAL
F77_VOID_FUN    F77rotm
( F77_INT_DECL,    F77_VINOUT_DECL, F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL,    F77_VIN_DECL );
#endif
#ifdef TREAL
F77_VOID_FUN    F77dot
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_SINOUT_DECL );
#ifdef SREAL
F77_VOID_FUN    F77dsdot
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    double * );
F77_VOID_FUN    F77sdsdot
( F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_VIN_DECL,    F77_INT_DECL,    F77_SINOUT_DECL );
#endif
#else
F77_VOID_FUN    F77dotc
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_SINOUT_DECL );
F77_VOID_FUN    F77dotu
( F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_SINOUT_DECL );
#endif

F77_VOID_FUN    F77gbmv
( F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,    F77_INT_DECL,
  F77_INT_DECL,    F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,
  F77_VIN_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77gemv
( F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
                   F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,
  F77_VIN_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL     F77_1_CHAR );
#ifdef TREAL
F77_VOID_FUN    F77ger
( F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL,
  F77_INT_DECL );
F77_VOID_FUN    F77sbmv
( F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,
  F77_MIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77spmv
( F77_CHAR_DECL,   F77_INT_DECL,                     F77_SIN_DECL,
  F77_MIN_DECL,                     F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77symv
( F77_CHAR_DECL,   F77_INT_DECL,                     F77_SIN_DECL,
  F77_MIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77spr
( F77_CHAR_DECL,   F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_MINOUT_DECL                   F77_1_CHAR );
F77_VOID_FUN    F77syr
( F77_CHAR_DECL,   F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_MINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77spr2
( F77_CHAR_DECL,   F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL
                   F77_1_CHAR );
F77_VOID_FUN    F77syr2
( F77_CHAR_DECL,   F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL,
  F77_INT_DECL     F77_1_CHAR );
#else
F77_VOID_FUN    F77gerc
( F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL,
  F77_INT_DECL );
F77_VOID_FUN    F77geru
( F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL,
  F77_INT_DECL );
F77_VOID_FUN    F77hbmv
( F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,
  F77_MIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77hpmv
( F77_CHAR_DECL,   F77_INT_DECL,                     F77_SIN_DECL,
  F77_MIN_DECL,                     F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77hemv
( F77_CHAR_DECL,   F77_INT_DECL,                     F77_SIN_DECL,
  F77_MIN_DECL,    F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_VINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77hpr
( F77_CHAR_DECL,   F77_INT_DECL,    F77_RIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_MINOUT_DECL                   F77_1_CHAR );
F77_VOID_FUN    F77her
( F77_CHAR_DECL,   F77_INT_DECL,    F77_RIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_MINOUT_DECL, F77_INT_DECL     F77_1_CHAR );
F77_VOID_FUN    F77hpr2
( F77_CHAR_DECL,   F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL
                   F77_1_CHAR );
F77_VOID_FUN    F77her2
( F77_CHAR_DECL,   F77_INT_DECL,    F77_SIN_DECL,    F77_VIN_DECL,
  F77_INT_DECL,    F77_VIN_DECL,    F77_INT_DECL,    F77_MINOUT_DECL,
  F77_INT_DECL     F77_1_CHAR );
#endif
F77_VOID_FUN    F77tbmv
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,
  F77_INT_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL     F77_3_CHAR );
F77_VOID_FUN    F77tpmv
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,
                   F77_MIN_DECL,                     F77_VINOUT_DECL,
  F77_INT_DECL     F77_3_CHAR );
F77_VOID_FUN    F77trmv
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,
                   F77_MIN_DECL,    F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL     F77_3_CHAR );
F77_VOID_FUN    F77tbsv
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,
  F77_INT_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL     F77_3_CHAR );
F77_VOID_FUN    F77tpsv
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,
                   F77_MIN_DECL,                     F77_VINOUT_DECL,
  F77_INT_DECL     F77_3_CHAR );
F77_VOID_FUN    F77trsv
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,
                   F77_MIN_DECL,    F77_INT_DECL,    F77_VINOUT_DECL,
  F77_INT_DECL     F77_3_CHAR );

F77_VOID_FUN    F77gemm
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_INT_DECL,    F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,
  F77_MIN_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_MINOUT_DECL,
  F77_INT_DECL     F77_2_CHAR );
F77_VOID_FUN    F77hemm
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_MIN_DECL,
  F77_INT_DECL,    F77_SIN_DECL,    F77_MINOUT_DECL, F77_INT_DECL
  F77_2_CHAR );
F77_VOID_FUN    F77her2k
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_MIN_DECL,
  F77_INT_DECL,    F77_RIN_DECL,    F77_MINOUT_DECL, F77_INT_DECL
  F77_2_CHAR );
F77_VOID_FUN    F77herk
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_RIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_RIN_DECL,
  F77_MINOUT_DECL, F77_INT_DECL     F77_2_CHAR );
F77_VOID_FUN    F77symm
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_MIN_DECL,
  F77_INT_DECL,    F77_SIN_DECL,    F77_MINOUT_DECL, F77_INT_DECL
  F77_2_CHAR );
F77_VOID_FUN    F77syr2k
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_MIN_DECL,
  F77_INT_DECL,    F77_SIN_DECL,    F77_MINOUT_DECL, F77_INT_DECL
  F77_2_CHAR );
F77_VOID_FUN    F77syrk
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_INT_DECL,    F77_INT_DECL,
  F77_SIN_DECL,    F77_MIN_DECL,    F77_INT_DECL,    F77_SIN_DECL,
  F77_MINOUT_DECL, F77_INT_DECL     F77_2_CHAR );
F77_VOID_FUN    F77trmm
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,
  F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_MIN_DECL,
  F77_INT_DECL,    F77_MINOUT_DECL, F77_INT_DECL     F77_4_CHAR );
F77_VOID_FUN    F77trsm
( F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,   F77_CHAR_DECL,
  F77_INT_DECL,    F77_INT_DECL,    F77_SIN_DECL,    F77_MIN_DECL,
  F77_INT_DECL,    F77_MINOUT_DECL, F77_INT_DECL     F77_4_CHAR );

#endif
