/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
 * Originally developed at the University of Tennessee,
 * Innovative Computing Laboratory, Knoxville TN, 37996-1301, USA.
 *
 * ---------------------------------------------------------------------
 *
 * -- Copyright notice and Licensing terms:
 *
 *  Redistribution  and  use in  source and binary forms, with or without
 *  modification, are  permitted provided  that the following  conditions
 *  are met:
 *
 * 1. Redistributions  of  source  code  must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce  the above copyright
 *    notice,  this list of conditions, and the  following disclaimer in
 *    the documentation and/or other materials provided with the distri-
 *    bution.
 * 3. The name of the University,  the ATLAS group,  or the names of its
 *    contributors  may not be used to endorse or promote products deri-
 *    ved from this software without specific written permission.
 *
 * -- Disclaimer:
 *
 * THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
 * RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
 * CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ---------------------------------------------------------------------
 */
#ifndef ATLAS_F77WRAP_H
#define ATLAS_F77WRAP_H
/*
 * =====================================================================
 * Include Files
 * =====================================================================
 */
#include "atlas_misc.h"
#include "atlas_f77.h"
/*
 * =====================================================================
 * Multi-threaded/reference implementation function names re-definition
 * =====================================================================
 *
 * Uncomment the following definition macros to call the  multi-threaded
 * implementation or define those macros at compile time.
 *
 * #define USE_L1_PTHREADS
 * #define USE_L2_PTHREADS
 * #define USE_L3_PTHREADS
 *
 * Uncomment  the following definition  macros to call the reference im-
 * plementation or define those macros at compile time.
 *
 * #define USE_L1_REFERENCE
 * #define USE_L2_REFERENCE
 * #define USE_L3_REFERENCE
 *
 * =====================================================================
 */

#ifdef ATL_USEPTHREADS
#define USE_L3_PTHREADS
#define USE_L2_PTHREADS
#endif

/*
 * =====================================================================
 * ATLAS Levels 1, 2 and 3 Prototypes
 * =====================================================================
 */
#if   defined( USE_L1_PTHREADS  )
   #include "atlas_ptalias1.h"
#elif defined( USE_L1_REFERENCE )
   #include "atlas_refalias1.h"
#endif
#include "atlas_level1.h"

#if   defined( USE_L2_PTHREADS  )
   #include "atlas_ptalias2.h"
#elif defined( USE_L2_REFERENCE )
   #include "atlas_refalias2.h"
#endif
#include "atlas_level2.h"

#if   defined( USE_L3_PTHREADS  )
   #include "atlas_ptalias3.h"
#elif defined( USE_L3_REFERENCE )
   #include "atlas_refalias3.h"
#endif
#include "atlas_level3.h"
/*
 * =====================================================================
 * #define macro constants
 * =====================================================================
 */
#define  PATLF77WRAP         Mjoin( ATL_F77wrap_, PRE )

#ifdef TREAL
#define  ATLPUF77WRAP        Mjoin( ATL_F77wrap_, PRE )
#define  ATLUPF77WRAP        Mjoin( ATL_F77wrap_, PRE )
#else
#define  ATLPUF77WRAP        Mjoin( Mjoin( ATL_F77wrap_, PRE ), UPR )
#define  ATLUPF77WRAP        Mjoin( Mjoin( ATL_F77wrap_, UPR ), PRE )
#endif

#define  F77_INOTRAN         111
#define  F77_ITRAN           112
#define  F77_ICOTRAN         113

#define  F77_IUPPER          121
#define  F77_ILOWER          122

#define  F77_INONUNIT        131
#define  F77_IUNIT           132

#define  F77_ILEFT           141
#define  F77_IRIGHT          142
/*
 * =====================================================================
 * #define macro functions
 * =====================================================================
 */
#ifdef TREAL
#define    V1N( n_, x_, incx_ )   \
                          ( (*n_) > 0 ? (x_)+(1-(*n_))*(*incx_) : (x_) )
#define    VN1( n_, x_, incx_ )   \
                          ( (*n_) > 0 ? (x_)+((*n_)-1)*(*incx_) : (x_) )
#define    W1N( n_, x_, incx_ )   \
                          ( (*incx_) < 0 ? V1N( n_, x_, incx_ ) : (x_) )
#else
#define    V1N( n_, x_, incx_ )   \
             ( (*n_) > 0 ? (x_)+( ( (1-(*n_))*(*incx_) ) << 1 ) : (x_) )
#define    VN1( n_, x_, incx_ )   \
             ( (*n_) > 0 ? (x_)+( ( ((*n_)-1)*(*incx_) ) << 1 ) : (x_) )
#define    W1N( n_, x_, incx_ )   \
                          ( (*incx_) < 0 ? V1N( n_, x_, incx_ ) : (x_) )
#endif
/*
 * =====================================================================
 * FORTRAN <-> C interface
 * =====================================================================
 *
 * These macros identifies how these wrappers will be called as follows:
 *
 * Add_:      the FORTRAN compiler expects the name of C functions to be
 * in all lower case and to have an underscore postfixed it (Suns, Intel
 * compilers expect this).
 *
 * NoChange:  the FORTRAN compiler expects the name of C functions to be
 * in all lower case (IBM RS6K compilers do this).
 *
 * UpCase:    the FORTRAN compiler expects the name of C functions to be
 * in all upcase. (Cray compilers expect this).
 *
 * Add__:     the FORTRAN compiler in use is f2c, a FORTRAN to C conver-
 * ter.
 */
#if defined( Add_ )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine calling a C routine.
 *
 *    FORTRAN CALL                          C declaration
 *    CALL ATL_F77WRAP_SGEMM(...)           void atl_f77wrap_sgemm_(...)
 *
 * This is the default.
 */
#if   defined( SREAL )

#define    ATL_F77wrap_srotg   atl_f77wrap_srotg_
#define    ATL_F77wrap_srotmg  atl_f77wrap_srotmg_
#define    ATL_F77wrap_snrm2   atl_f77wrap_snrm2_
#define    ATL_F77wrap_sasum   atl_f77wrap_sasum_
#define    ATL_F77wrap_sscal   atl_f77wrap_sscal_
#define    ATL_F77wrap_isamax  atl_f77wrap_isamax_
#define    ATL_F77wrap_saxpy   atl_f77wrap_saxpy_
#define    ATL_F77wrap_scopy   atl_f77wrap_scopy_
#define    ATL_F77wrap_sswap   atl_f77wrap_sswap_
#define    ATL_F77wrap_srot    atl_f77wrap_srot_
#define    ATL_F77wrap_srotm   atl_f77wrap_srotm_
#define    ATL_F77wrap_sdot    atl_f77wrap_sdot_
#define    ATL_F77wrap_dsdot   atl_f77wrap_dsdot_
#define    ATL_F77wrap_sdsdot  atl_f77wrap_sdsdot_

#define    ATL_F77wrap_sgbmv   atl_f77wrap_sgbmv_
#define    ATL_F77wrap_sgemv   atl_f77wrap_sgemv_
#define    ATL_F77wrap_sger    atl_f77wrap_sger_
#define    ATL_F77wrap_sger2   atl_f77wrap_sger2_
#define    ATL_F77wrap_ssbmv   atl_f77wrap_ssbmv_
#define    ATL_F77wrap_sspmv   atl_f77wrap_sspmv_
#define    ATL_F77wrap_ssymv   atl_f77wrap_ssymv_
#define    ATL_F77wrap_sspr    atl_f77wrap_sspr_
#define    ATL_F77wrap_ssyr    atl_f77wrap_ssyr_
#define    ATL_F77wrap_sspr2   atl_f77wrap_sspr2_
#define    ATL_F77wrap_ssyr2   atl_f77wrap_ssyr2_
#define    ATL_F77wrap_stbmv   atl_f77wrap_stbmv_
#define    ATL_F77wrap_stpmv   atl_f77wrap_stpmv_
#define    ATL_F77wrap_strmv   atl_f77wrap_strmv_
#define    ATL_F77wrap_stbsv   atl_f77wrap_stbsv_
#define    ATL_F77wrap_stpsv   atl_f77wrap_stpsv_
#define    ATL_F77wrap_strsv   atl_f77wrap_strsv_

#define    ATL_F77wrap_sgemm   atl_f77wrap_sgemm_
#define    ATL_F77wrap_ssymm   atl_f77wrap_ssymm_
#define    ATL_F77wrap_ssyrk   atl_f77wrap_ssyrk_
#define    ATL_F77wrap_ssyr2k  atl_f77wrap_ssyr2k_
#define    ATL_F77wrap_strmm   atl_f77wrap_strmm_
#define    ATL_F77wrap_strsm   atl_f77wrap_strsm_

#elif defined( DREAL )

#define    ATL_F77wrap_drotg   atl_f77wrap_drotg_
#define    ATL_F77wrap_drotmg  atl_f77wrap_drotmg_
#define    ATL_F77wrap_dnrm2   atl_f77wrap_dnrm2_
#define    ATL_F77wrap_dasum   atl_f77wrap_dasum_
#define    ATL_F77wrap_dscal   atl_f77wrap_dscal_
#define    ATL_F77wrap_idamax  atl_f77wrap_idamax_
#define    ATL_F77wrap_daxpy   atl_f77wrap_daxpy_
#define    ATL_F77wrap_dcopy   atl_f77wrap_dcopy_
#define    ATL_F77wrap_dswap   atl_f77wrap_dswap_
#define    ATL_F77wrap_drot    atl_f77wrap_drot_
#define    ATL_F77wrap_drotm   atl_f77wrap_drotm_
#define    ATL_F77wrap_ddot    atl_f77wrap_ddot_

#define    ATL_F77wrap_dgbmv   atl_f77wrap_dgbmv_
#define    ATL_F77wrap_dgemv   atl_f77wrap_dgemv_
#define    ATL_F77wrap_dger    atl_f77wrap_dger_
#define    ATL_F77wrap_dger2   atl_f77wrap_dger2_
#define    ATL_F77wrap_dsbmv   atl_f77wrap_dsbmv_
#define    ATL_F77wrap_dspmv   atl_f77wrap_dspmv_
#define    ATL_F77wrap_dsymv   atl_f77wrap_dsymv_
#define    ATL_F77wrap_dspr    atl_f77wrap_dspr_
#define    ATL_F77wrap_dsyr    atl_f77wrap_dsyr_
#define    ATL_F77wrap_dspr2   atl_f77wrap_dspr2_
#define    ATL_F77wrap_dsyr2   atl_f77wrap_dsyr2_
#define    ATL_F77wrap_dtbmv   atl_f77wrap_dtbmv_
#define    ATL_F77wrap_dtpmv   atl_f77wrap_dtpmv_
#define    ATL_F77wrap_dtrmv   atl_f77wrap_dtrmv_
#define    ATL_F77wrap_dtbsv   atl_f77wrap_dtbsv_
#define    ATL_F77wrap_dtpsv   atl_f77wrap_dtpsv_
#define    ATL_F77wrap_dtrsv   atl_f77wrap_dtrsv_

#define    ATL_F77wrap_dgemm   atl_f77wrap_dgemm_
#define    ATL_F77wrap_dsymm   atl_f77wrap_dsymm_
#define    ATL_F77wrap_dsyrk   atl_f77wrap_dsyrk_
#define    ATL_F77wrap_dsyr2k  atl_f77wrap_dsyr2k_
#define    ATL_F77wrap_dtrmm   atl_f77wrap_dtrmm_
#define    ATL_F77wrap_dtrsm   atl_f77wrap_dtrsm_

#elif defined( SCPLX )

#define    ATL_F77wrap_crotg   atl_f77wrap_crotg_
#define    ATL_F77wrap_scnrm2  atl_f77wrap_scnrm2_
#define    ATL_F77wrap_scasum  atl_f77wrap_scasum_
#define    ATL_F77wrap_cscal   atl_f77wrap_cscal_
#define    ATL_F77wrap_csscal  atl_f77wrap_csscal_
#define    ATL_F77wrap_icamax  atl_f77wrap_icamax_
#define    ATL_F77wrap_caxpy   atl_f77wrap_caxpy_
#define    ATL_F77wrap_ccopy   atl_f77wrap_ccopy_
#define    ATL_F77wrap_cswap   atl_f77wrap_cswap_
#define    ATL_F77wrap_csrot   atl_f77wrap_csrot_
#define    ATL_F77wrap_cdotc   atl_f77wrap_cdotc_
#define    ATL_F77wrap_cdotu   atl_f77wrap_cdotu_

#define    ATL_F77wrap_cgbmv   atl_f77wrap_cgbmv_
#define    ATL_F77wrap_cgemv   atl_f77wrap_cgemv_
#define    ATL_F77wrap_cgerc   atl_f77wrap_cgerc_
#define    ATL_F77wrap_cgeru   atl_f77wrap_cgeru_
#define    ATL_F77wrap_cger2c  atl_f77wrap_cger2c_
#define    ATL_F77wrap_cger2u  atl_f77wrap_cger2u_
#define    ATL_F77wrap_chbmv   atl_f77wrap_chbmv_
#define    ATL_F77wrap_chpmv   atl_f77wrap_chpmv_
#define    ATL_F77wrap_chemv   atl_f77wrap_chemv_
#define    ATL_F77wrap_chpr    atl_f77wrap_chpr_
#define    ATL_F77wrap_cher    atl_f77wrap_cher_
#define    ATL_F77wrap_chpr2   atl_f77wrap_chpr2_
#define    ATL_F77wrap_cher2   atl_f77wrap_cher2_
#define    ATL_F77wrap_ctbmv   atl_f77wrap_ctbmv_
#define    ATL_F77wrap_ctpmv   atl_f77wrap_ctpmv_
#define    ATL_F77wrap_ctrmv   atl_f77wrap_ctrmv_
#define    ATL_F77wrap_ctbsv   atl_f77wrap_ctbsv_
#define    ATL_F77wrap_ctpsv   atl_f77wrap_ctpsv_
#define    ATL_F77wrap_ctrsv   atl_f77wrap_ctrsv_

#define    ATL_F77wrap_cgemm   atl_f77wrap_cgemm_
#define    ATL_F77wrap_chemm   atl_f77wrap_chemm_
#define    ATL_F77wrap_cherk   atl_f77wrap_cherk_
#define    ATL_F77wrap_cher2k  atl_f77wrap_cher2k_
#define    ATL_F77wrap_csymm   atl_f77wrap_csymm_
#define    ATL_F77wrap_csyrk   atl_f77wrap_csyrk_
#define    ATL_F77wrap_csyr2k  atl_f77wrap_csyr2k_
#define    ATL_F77wrap_ctrmm   atl_f77wrap_ctrmm_
#define    ATL_F77wrap_ctrsm   atl_f77wrap_ctrsm_

#elif defined( DCPLX )

#define    ATL_F77wrap_zrotg   atl_f77wrap_zrotg_
#define    ATL_F77wrap_dznrm2  atl_f77wrap_dznrm2_
#define    ATL_F77wrap_dzasum  atl_f77wrap_dzasum_
#define    ATL_F77wrap_zscal   atl_f77wrap_zscal_
#define    ATL_F77wrap_zdscal  atl_f77wrap_zdscal_
#define    ATL_F77wrap_izamax  atl_f77wrap_izamax_
#define    ATL_F77wrap_zaxpy   atl_f77wrap_zaxpy_
#define    ATL_F77wrap_zcopy   atl_f77wrap_zcopy_
#define    ATL_F77wrap_zswap   atl_f77wrap_zswap_
#define    ATL_F77wrap_zdrot   atl_f77wrap_zdrot_
#define    ATL_F77wrap_zdotc   atl_f77wrap_zdotc_
#define    ATL_F77wrap_zdotu   atl_f77wrap_zdotu_

#define    ATL_F77wrap_zgbmv   atl_f77wrap_zgbmv_
#define    ATL_F77wrap_zgemv   atl_f77wrap_zgemv_
#define    ATL_F77wrap_zgerc   atl_f77wrap_zgerc_
#define    ATL_F77wrap_zgeru   atl_f77wrap_zgeru_
#define    ATL_F77wrap_zger2c  atl_f77wrap_zger2c_
#define    ATL_F77wrap_zger2u  atl_f77wrap_zger2u_
#define    ATL_F77wrap_zhbmv   atl_f77wrap_zhbmv_
#define    ATL_F77wrap_zhpmv   atl_f77wrap_zhpmv_
#define    ATL_F77wrap_zhemv   atl_f77wrap_zhemv_
#define    ATL_F77wrap_zhpr    atl_f77wrap_zhpr_
#define    ATL_F77wrap_zher    atl_f77wrap_zher_
#define    ATL_F77wrap_zhpr2   atl_f77wrap_zhpr2_
#define    ATL_F77wrap_zher2   atl_f77wrap_zher2_
#define    ATL_F77wrap_ztbmv   atl_f77wrap_ztbmv_
#define    ATL_F77wrap_ztpmv   atl_f77wrap_ztpmv_
#define    ATL_F77wrap_ztrmv   atl_f77wrap_ztrmv_
#define    ATL_F77wrap_ztbsv   atl_f77wrap_ztbsv_
#define    ATL_F77wrap_ztpsv   atl_f77wrap_ztpsv_
#define    ATL_F77wrap_ztrsv   atl_f77wrap_ztrsv_

#define    ATL_F77wrap_zgemm   atl_f77wrap_zgemm_
#define    ATL_F77wrap_zhemm   atl_f77wrap_zhemm_
#define    ATL_F77wrap_zherk   atl_f77wrap_zherk_
#define    ATL_F77wrap_zher2k  atl_f77wrap_zher2k_
#define    ATL_F77wrap_zsymm   atl_f77wrap_zsymm_
#define    ATL_F77wrap_zsyrk   atl_f77wrap_zsyrk_
#define    ATL_F77wrap_zsyr2k  atl_f77wrap_zsyr2k_
#define    ATL_F77wrap_ztrmm   atl_f77wrap_ztrmm_
#define    ATL_F77wrap_ztrsm   atl_f77wrap_ztrsm_

#endif

#elif defined( UpCase )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine calling a C routine with the following interface:
 *
 *    FORTRAN CALL                          C declaration
 *    CALL ATL_F77WRAP_SGEMM(...)           void ATL_F77WRAP_SGEMM(...)
 *
 */
#if   defined( SREAL )

#define    ATL_F77wrap_srotg   ATL_F77WRAP_SROTG
#define    ATL_F77wrap_srotmg  ATL_F77WRAP_SROTMG
#define    ATL_F77wrap_snrm2   ATL_F77WRAP_SNRM2
#define    ATL_F77wrap_sasum   ATL_F77WRAP_SASUM
#define    ATL_F77wrap_sscal   ATL_F77WRAP_SSCAL
#define    ATL_F77wrap_isamax  ATL_F77WRAP_ISAMAX
#define    ATL_F77wrap_saxpy   ATL_F77WRAP_SAXPY
#define    ATL_F77wrap_scopy   ATL_F77WRAP_SCOPY
#define    ATL_F77wrap_sswap   ATL_F77WRAP_SSWAP
#define    ATL_F77wrap_srot    ATL_F77WRAP_SROT
#define    ATL_F77wrap_srotm   ATL_F77WRAP_SROTM
#define    ATL_F77wrap_sdot    ATL_F77WRAP_SDOT
#define    ATL_F77wrap_dsdot   ATL_F77WRAP_DSDOT
#define    ATL_F77wrap_sdsdot  ATL_F77WRAP_SDSDOT

#define    ATL_F77wrap_sgbmv   ATL_F77WRAP_SGBMV
#define    ATL_F77wrap_sgemv   ATL_F77WRAP_SGEMV
#define    ATL_F77wrap_sger    ATL_F77WRAP_SGER
#define    ATL_F77wrap_sger2   ATL_F77WRAP_SGER2
#define    ATL_F77wrap_ssbmv   ATL_F77WRAP_SSBMV
#define    ATL_F77wrap_sspmv   ATL_F77WRAP_SSPMV
#define    ATL_F77wrap_ssymv   ATL_F77WRAP_SSYMV
#define    ATL_F77wrap_sspr    ATL_F77WRAP_SSPR
#define    ATL_F77wrap_ssyr    ATL_F77WRAP_SSYR
#define    ATL_F77wrap_sspr2   ATL_F77WRAP_SSPR2
#define    ATL_F77wrap_ssyr2   ATL_F77WRAP_SSYR2
#define    ATL_F77wrap_stbmv   ATL_F77WRAP_STBMV
#define    ATL_F77wrap_stpmv   ATL_F77WRAP_STPMV
#define    ATL_F77wrap_strmv   ATL_F77WRAP_STRMV
#define    ATL_F77wrap_stbsv   ATL_F77WRAP_STBSV
#define    ATL_F77wrap_stpsv   ATL_F77WRAP_STPSV
#define    ATL_F77wrap_strsv   ATL_F77WRAP_STRSV

#define    ATL_F77wrap_sgemm   ATL_F77WRAP_SGEMM
#define    ATL_F77wrap_ssymm   ATL_F77WRAP_SSYMM
#define    ATL_F77wrap_ssyrk   ATL_F77WRAP_SSYRK
#define    ATL_F77wrap_ssyr2k  ATL_F77WRAP_SSYR2K
#define    ATL_F77wrap_strmm   ATL_F77WRAP_STRMM
#define    ATL_F77wrap_strsm   ATL_F77WRAP_STRSM

#elif defined( DREAL )

#define    ATL_F77wrap_drotg   ATL_F77WRAP_DROTG
#define    ATL_F77wrap_drotmg  ATL_F77WRAP_DROTMG
#define    ATL_F77wrap_dnrm2   ATL_F77WRAP_DNRM2
#define    ATL_F77wrap_dasum   ATL_F77WRAP_DASUM
#define    ATL_F77wrap_dscal   ATL_F77WRAP_DSCAL
#define    ATL_F77wrap_idamax  ATL_F77WRAP_IDAMAX
#define    ATL_F77wrap_daxpy   ATL_F77WRAP_DAXPY
#define    ATL_F77wrap_dcopy   ATL_F77WRAP_DCOPY
#define    ATL_F77wrap_dswap   ATL_F77WRAP_DSWAP
#define    ATL_F77wrap_drot    ATL_F77WRAP_DROT
#define    ATL_F77wrap_drotm   ATL_F77WRAP_DROTM
#define    ATL_F77wrap_ddot    ATL_F77WRAP_DDOT

#define    ATL_F77wrap_dgbmv   ATL_F77WRAP_DGBMV
#define    ATL_F77wrap_dgemv   ATL_F77WRAP_DGEMV
#define    ATL_F77wrap_dger    ATL_F77WRAP_DGER
#define    ATL_F77wrap_dger2   ATL_F77WRAP_DGER2
#define    ATL_F77wrap_dsbmv   ATL_F77WRAP_DSBMV
#define    ATL_F77wrap_dspmv   ATL_F77WRAP_DSPMV
#define    ATL_F77wrap_dsymv   ATL_F77WRAP_DSYMV
#define    ATL_F77wrap_dspr    ATL_F77WRAP_DSPR
#define    ATL_F77wrap_dsyr    ATL_F77WRAP_DSYR
#define    ATL_F77wrap_dspr2   ATL_F77WRAP_DSPR2
#define    ATL_F77wrap_dsyr2   ATL_F77WRAP_DSYR2
#define    ATL_F77wrap_dtbmv   ATL_F77WRAP_DTBMV
#define    ATL_F77wrap_dtpmv   ATL_F77WRAP_DTPMV
#define    ATL_F77wrap_dtrmv   ATL_F77WRAP_DTRMV
#define    ATL_F77wrap_dtbsv   ATL_F77WRAP_DTBSV
#define    ATL_F77wrap_dtpsv   ATL_F77WRAP_DTPSV
#define    ATL_F77wrap_dtrsv   ATL_F77WRAP_DTRSV

#define    ATL_F77wrap_dgemm   ATL_F77WRAP_DGEMM
#define    ATL_F77wrap_dsymm   ATL_F77WRAP_DSYMM
#define    ATL_F77wrap_dsyrk   ATL_F77WRAP_DSYRK
#define    ATL_F77wrap_dsyr2k  ATL_F77WRAP_DSYR2K
#define    ATL_F77wrap_dtrmm   ATL_F77WRAP_DTRMM
#define    ATL_F77wrap_dtrsm   ATL_F77WRAP_DTRSM

#elif defined( SCPLX )

#define    ATL_F77wrap_crotg   ATL_F77WRAP_CROTG
#define    ATL_F77wrap_scnrm2  ATL_F77WRAP_SCNRM2
#define    ATL_F77wrap_scasum  ATL_F77WRAP_SCASUM
#define    ATL_F77wrap_cscal   ATL_F77WRAP_CSCAL
#define    ATL_F77wrap_csscal  ATL_F77WRAP_CSSCAL
#define    ATL_F77wrap_icamax  ATL_F77WRAP_ICAMAX
#define    ATL_F77wrap_caxpy   ATL_F77WRAP_CAXPY
#define    ATL_F77wrap_ccopy   ATL_F77WRAP_CCOPY
#define    ATL_F77wrap_cswap   ATL_F77WRAP_CSWAP
#define    ATL_F77wrap_csrot   ATL_F77WRAP_CSROT
#define    ATL_F77wrap_cdotc   ATL_F77WRAP_CDOTC
#define    ATL_F77wrap_cdotu   ATL_F77WRAP_CDOTU

#define    ATL_F77wrap_cgbmv   ATL_F77WRAP_CGBMV
#define    ATL_F77wrap_cgemv   ATL_F77WRAP_CGEMV
#define    ATL_F77wrap_cgerc   ATL_F77WRAP_CGERC
#define    ATL_F77wrap_cgeru   ATL_F77WRAP_CGERU
#define    ATL_F77wrap_cger2c  ATL_F77WRAP_CGER2C
#define    ATL_F77wrap_cger2u  ATL_F77WRAP_CGER2U
#define    ATL_F77wrap_chbmv   ATL_F77WRAP_CHBMV
#define    ATL_F77wrap_chpmv   ATL_F77WRAP_CHPMV
#define    ATL_F77wrap_chemv   ATL_F77WRAP_CHEMV
#define    ATL_F77wrap_chpr    ATL_F77WRAP_CHPR
#define    ATL_F77wrap_cher    ATL_F77WRAP_CHER
#define    ATL_F77wrap_chpr2   ATL_F77WRAP_CHPR2
#define    ATL_F77wrap_cher2   ATL_F77WRAP_CHER2
#define    ATL_F77wrap_ctbmv   ATL_F77WRAP_CTBMV
#define    ATL_F77wrap_ctpmv   ATL_F77WRAP_CTPMV
#define    ATL_F77wrap_ctrmv   ATL_F77WRAP_CTRMV
#define    ATL_F77wrap_ctbsv   ATL_F77WRAP_CTBSV
#define    ATL_F77wrap_ctpsv   ATL_F77WRAP_CTPSV
#define    ATL_F77wrap_ctrsv   ATL_F77WRAP_CTRSV

#define    ATL_F77wrap_cgemm   ATL_F77WRAP_CGEMM
#define    ATL_F77wrap_chemm   ATL_F77WRAP_CHEMM
#define    ATL_F77wrap_cherk   ATL_F77WRAP_CHERK
#define    ATL_F77wrap_cher2k  ATL_F77WRAP_CHER2K
#define    ATL_F77wrap_csymm   ATL_F77WRAP_CSYMM
#define    ATL_F77wrap_csyrk   ATL_F77WRAP_CSYRK
#define    ATL_F77wrap_csyr2k  ATL_F77WRAP_CSYR2K
#define    ATL_F77wrap_ctrmm   ATL_F77WRAP_CTRMM
#define    ATL_F77wrap_ctrsm   ATL_F77WRAP_CTRSM

#elif defined( DCPLX )

#define    ATL_F77wrap_zrotg   ATL_F77WRAP_ZROTG
#define    ATL_F77wrap_dznrm2  ATL_F77WRAP_DZNRM2
#define    ATL_F77wrap_dzasum  ATL_F77WRAP_DZASUM
#define    ATL_F77wrap_zscal   ATL_F77WRAP_ZSCAL
#define    ATL_F77wrap_zdscal  ATL_F77WRAP_ZDSCAL
#define    ATL_F77wrap_izamax  ATL_F77WRAP_IZAMAX
#define    ATL_F77wrap_zaxpy   ATL_F77WRAP_ZAXPY
#define    ATL_F77wrap_zcopy   ATL_F77WRAP_ZCOPY
#define    ATL_F77wrap_zswap   ATL_F77WRAP_ZSWAP
#define    ATL_F77wrap_zdrot   ATL_F77WRAP_ZDROT
#define    ATL_F77wrap_zdotc   ATL_F77WRAP_ZDOTC
#define    ATL_F77wrap_zdotu   ATL_F77WRAP_ZDOTU

#define    ATL_F77wrap_zgbmv   ATL_F77WRAP_ZGBMV
#define    ATL_F77wrap_zgemv   ATL_F77WRAP_ZGEMV
#define    ATL_F77wrap_zgerc   ATL_F77WRAP_ZGERC
#define    ATL_F77wrap_zgeru   ATL_F77WRAP_ZGERU
#define    ATL_F77wrap_zger2c  ATL_F77WRAP_ZGER2C
#define    ATL_F77wrap_zger2u  ATL_F77WRAP_ZGER2U
#define    ATL_F77wrap_zhbmv   ATL_F77WRAP_ZHBMV
#define    ATL_F77wrap_zhpmv   ATL_F77WRAP_ZHPMV
#define    ATL_F77wrap_zhemv   ATL_F77WRAP_ZHEMV
#define    ATL_F77wrap_zhpr    ATL_F77WRAP_ZHPR
#define    ATL_F77wrap_zher    ATL_F77WRAP_ZHER
#define    ATL_F77wrap_zhpr2   ATL_F77WRAP_ZHPR2
#define    ATL_F77wrap_zher2   ATL_F77WRAP_ZHER2
#define    ATL_F77wrap_ztbmv   ATL_F77WRAP_ZTBMV
#define    ATL_F77wrap_ztpmv   ATL_F77WRAP_ZTPMV
#define    ATL_F77wrap_ztrmv   ATL_F77WRAP_ZTRMV
#define    ATL_F77wrap_ztbsv   ATL_F77WRAP_ZTBSV
#define    ATL_F77wrap_ztpsv   ATL_F77WRAP_ZTPSV
#define    ATL_F77wrap_ztrsv   ATL_F77WRAP_ZTRSV

#define    ATL_F77wrap_zgemm   ATL_F77WRAP_ZGEMM
#define    ATL_F77wrap_zhemm   ATL_F77WRAP_ZHEMM
#define    ATL_F77wrap_zherk   ATL_F77WRAP_ZHERK
#define    ATL_F77wrap_zher2k  ATL_F77WRAP_ZHER2K
#define    ATL_F77wrap_zsymm   ATL_F77WRAP_ZSYMM
#define    ATL_F77wrap_zsyrk   ATL_F77WRAP_ZSYRK
#define    ATL_F77wrap_zsyr2k  ATL_F77WRAP_ZSYR2K
#define    ATL_F77wrap_ztrmm   ATL_F77WRAP_ZTRMM
#define    ATL_F77wrap_ztrsm   ATL_F77WRAP_ZTRSM

#endif

#elif defined( NoChange )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine calling a C routine with the following interface:
 *
 *    FORTRAN CALL                          C declaration
 *    CALL ATL_F77WRAP_SGEMM(...)           void atl_f77wrap_sgemm(...)
 */
#if   defined( SREAL )

#define    ATL_F77wrap_srotg   atl_f77wrap_srotg
#define    ATL_F77wrap_srotmg  atl_f77wrap_srotmg
#define    ATL_F77wrap_snrm2   atl_f77wrap_snrm2
#define    ATL_F77wrap_sasum   atl_f77wrap_sasum
#define    ATL_F77wrap_sscal   atl_f77wrap_sscal
#define    ATL_F77wrap_isamax  atl_f77wrap_isamax
#define    ATL_F77wrap_saxpy   atl_f77wrap_saxpy
#define    ATL_F77wrap_scopy   atl_f77wrap_scopy
#define    ATL_F77wrap_sswap   atl_f77wrap_sswap
#define    ATL_F77wrap_srot    atl_f77wrap_srot
#define    ATL_F77wrap_srotm   atl_f77wrap_srotm
#define    ATL_F77wrap_sdot    atl_f77wrap_sdot
#define    ATL_F77wrap_dsdot   atl_f77wrap_dsdot
#define    ATL_F77wrap_sdsdot  atl_f77wrap_sdsdot

#define    ATL_F77wrap_sgbmv   atl_f77wrap_sgbmv
#define    ATL_F77wrap_sgemv   atl_f77wrap_sgemv
#define    ATL_F77wrap_sger    atl_f77wrap_sger
#define    ATL_F77wrap_sger2   atl_f77wrap_sger2
#define    ATL_F77wrap_ssbmv   atl_f77wrap_ssbmv
#define    ATL_F77wrap_sspmv   atl_f77wrap_sspmv
#define    ATL_F77wrap_ssymv   atl_f77wrap_ssymv
#define    ATL_F77wrap_sspr    atl_f77wrap_sspr
#define    ATL_F77wrap_ssyr    atl_f77wrap_ssyr
#define    ATL_F77wrap_sspr2   atl_f77wrap_sspr2
#define    ATL_F77wrap_ssyr2   atl_f77wrap_ssyr2
#define    ATL_F77wrap_stbmv   atl_f77wrap_stbmv
#define    ATL_F77wrap_stpmv   atl_f77wrap_stpmv
#define    ATL_F77wrap_strmv   atl_f77wrap_strmv
#define    ATL_F77wrap_stbsv   atl_f77wrap_stbsv
#define    ATL_F77wrap_stpsv   atl_f77wrap_stpsv
#define    ATL_F77wrap_strsv   atl_f77wrap_strsv

#define    ATL_F77wrap_sgemm   atl_f77wrap_sgemm
#define    ATL_F77wrap_ssymm   atl_f77wrap_ssymm
#define    ATL_F77wrap_ssyrk   atl_f77wrap_ssyrk
#define    ATL_F77wrap_ssyr2k  atl_f77wrap_ssyr2k
#define    ATL_F77wrap_strmm   atl_f77wrap_strmm
#define    ATL_F77wrap_strsm   atl_f77wrap_strsm

#elif defined( DREAL )

#define    ATL_F77wrap_drotg   atl_f77wrap_drotg
#define    ATL_F77wrap_drotmg  atl_f77wrap_drotmg
#define    ATL_F77wrap_dnrm2   atl_f77wrap_dnrm2
#define    ATL_F77wrap_dasum   atl_f77wrap_dasum
#define    ATL_F77wrap_dscal   atl_f77wrap_dscal
#define    ATL_F77wrap_idamax  atl_f77wrap_idamax
#define    ATL_F77wrap_daxpy   atl_f77wrap_daxpy
#define    ATL_F77wrap_dcopy   atl_f77wrap_dcopy
#define    ATL_F77wrap_dswap   atl_f77wrap_dswap
#define    ATL_F77wrap_drot    atl_f77wrap_drot
#define    ATL_F77wrap_drotm   atl_f77wrap_drotm
#define    ATL_F77wrap_ddot    atl_f77wrap_ddot

#define    ATL_F77wrap_dgbmv   atl_f77wrap_dgbmv
#define    ATL_F77wrap_dgemv   atl_f77wrap_dgemv
#define    ATL_F77wrap_dger    atl_f77wrap_dger
#define    ATL_F77wrap_dger2   atl_f77wrap_dger2
#define    ATL_F77wrap_dsbmv   atl_f77wrap_dsbmv
#define    ATL_F77wrap_dspmv   atl_f77wrap_dspmv
#define    ATL_F77wrap_dsymv   atl_f77wrap_dsymv
#define    ATL_F77wrap_dspr    atl_f77wrap_dspr
#define    ATL_F77wrap_dsyr    atl_f77wrap_dsyr
#define    ATL_F77wrap_dspr2   atl_f77wrap_dspr2
#define    ATL_F77wrap_dsyr2   atl_f77wrap_dsyr2
#define    ATL_F77wrap_dtbmv   atl_f77wrap_dtbmv
#define    ATL_F77wrap_dtpmv   atl_f77wrap_dtpmv
#define    ATL_F77wrap_dtrmv   atl_f77wrap_dtrmv
#define    ATL_F77wrap_dtbsv   atl_f77wrap_dtbsv
#define    ATL_F77wrap_dtpsv   atl_f77wrap_dtpsv
#define    ATL_F77wrap_dtrsv   atl_f77wrap_dtrsv

#define    ATL_F77wrap_dgemm   atl_f77wrap_dgemm
#define    ATL_F77wrap_dsymm   atl_f77wrap_dsymm
#define    ATL_F77wrap_dsyrk   atl_f77wrap_dsyrk
#define    ATL_F77wrap_dsyr2k  atl_f77wrap_dsyr2k
#define    ATL_F77wrap_dtrmm   atl_f77wrap_dtrmm
#define    ATL_F77wrap_dtrsm   atl_f77wrap_dtrsm

#elif defined( SCPLX )

#define    ATL_F77wrap_crotg   atl_f77wrap_crotg
#define    ATL_F77wrap_scnrm2  atl_f77wrap_scnrm2
#define    ATL_F77wrap_scasum  atl_f77wrap_scasum
#define    ATL_F77wrap_cscal   atl_f77wrap_cscal
#define    ATL_F77wrap_csscal  atl_f77wrap_csscal
#define    ATL_F77wrap_icamax  atl_f77wrap_icamax
#define    ATL_F77wrap_caxpy   atl_f77wrap_caxpy
#define    ATL_F77wrap_ccopy   atl_f77wrap_ccopy
#define    ATL_F77wrap_cswap   atl_f77wrap_cswap
#define    ATL_F77wrap_csrot   atl_f77wrap_csrot
#define    ATL_F77wrap_cdotc   atl_f77wrap_cdotc
#define    ATL_F77wrap_cdotu   atl_f77wrap_cdotu

#define    ATL_F77wrap_cgbmv   atl_f77wrap_cgbmv
#define    ATL_F77wrap_cgemv   atl_f77wrap_cgemv
#define    ATL_F77wrap_cgerc   atl_f77wrap_cgerc
#define    ATL_F77wrap_cgeru   atl_f77wrap_cgeru
#define    ATL_F77wrap_cger2c  atl_f77wrap_cger2c
#define    ATL_F77wrap_cger2u  atl_f77wrap_cger2u
#define    ATL_F77wrap_chbmv   atl_f77wrap_chbmv
#define    ATL_F77wrap_chpmv   atl_f77wrap_chpmv
#define    ATL_F77wrap_chemv   atl_f77wrap_chemv
#define    ATL_F77wrap_chpr    atl_f77wrap_chpr
#define    ATL_F77wrap_cher    atl_f77wrap_cher
#define    ATL_F77wrap_chpr2   atl_f77wrap_chpr2
#define    ATL_F77wrap_cher2   atl_f77wrap_cher2
#define    ATL_F77wrap_ctbmv   atl_f77wrap_ctbmv
#define    ATL_F77wrap_ctpmv   atl_f77wrap_ctpmv
#define    ATL_F77wrap_ctrmv   atl_f77wrap_ctrmv
#define    ATL_F77wrap_ctbsv   atl_f77wrap_ctbsv
#define    ATL_F77wrap_ctpsv   atl_f77wrap_ctpsv
#define    ATL_F77wrap_ctrsv   atl_f77wrap_ctrsv

#define    ATL_F77wrap_cgemm   atl_f77wrap_cgemm
#define    ATL_F77wrap_chemm   atl_f77wrap_chemm
#define    ATL_F77wrap_cherk   atl_f77wrap_cherk
#define    ATL_F77wrap_cher2k  atl_f77wrap_cher2k
#define    ATL_F77wrap_csymm   atl_f77wrap_csymm
#define    ATL_F77wrap_csyrk   atl_f77wrap_csyrk
#define    ATL_F77wrap_csyr2k  atl_f77wrap_csyr2k
#define    ATL_F77wrap_ctrmm   atl_f77wrap_ctrmm
#define    ATL_F77wrap_ctrsm   atl_f77wrap_ctrsm

#elif defined( DCPLX )

#define    ATL_F77wrap_zrotg   atl_f77wrap_zrotg
#define    ATL_F77wrap_dznrm2  atl_f77wrap_dznrm2
#define    ATL_F77wrap_dzasum  atl_f77wrap_dzasum
#define    ATL_F77wrap_zscal   atl_f77wrap_zscal
#define    ATL_F77wrap_zdscal  atl_f77wrap_zdscal
#define    ATL_F77wrap_izamax  atl_f77wrap_izamax
#define    ATL_F77wrap_zaxpy   atl_f77wrap_zaxpy
#define    ATL_F77wrap_zcopy   atl_f77wrap_zcopy
#define    ATL_F77wrap_zswap   atl_f77wrap_zswap
#define    ATL_F77wrap_zdrot   atl_f77wrap_zdrot
#define    ATL_F77wrap_zdotc   atl_f77wrap_zdotc
#define    ATL_F77wrap_zdotu   atl_f77wrap_zdotu

#define    ATL_F77wrap_zgbmv   atl_f77wrap_zgbmv
#define    ATL_F77wrap_zgemv   atl_f77wrap_zgemv
#define    ATL_F77wrap_zgerc   atl_f77wrap_zgerc
#define    ATL_F77wrap_zgeru   atl_f77wrap_zgeru
#define    ATL_F77wrap_zger2c  atl_f77wrap_zger2c
#define    ATL_F77wrap_zger2u  atl_f77wrap_zger2u
#define    ATL_F77wrap_zhbmv   atl_f77wrap_zhbmv
#define    ATL_F77wrap_zhpmv   atl_f77wrap_zhpmv
#define    ATL_F77wrap_zhemv   atl_f77wrap_zhemv
#define    ATL_F77wrap_zhpr    atl_f77wrap_zhpr
#define    ATL_F77wrap_zher    atl_f77wrap_zher
#define    ATL_F77wrap_zhpr2   atl_f77wrap_zhpr2
#define    ATL_F77wrap_zher2   atl_f77wrap_zher2
#define    ATL_F77wrap_ztbmv   atl_f77wrap_ztbmv
#define    ATL_F77wrap_ztpmv   atl_f77wrap_ztpmv
#define    ATL_F77wrap_ztrmv   atl_f77wrap_ztrmv
#define    ATL_F77wrap_ztbsv   atl_f77wrap_ztbsv
#define    ATL_F77wrap_ztpsv   atl_f77wrap_ztpsv
#define    ATL_F77wrap_ztrsv   atl_f77wrap_ztrsv

#define    ATL_F77wrap_zgemm   atl_f77wrap_zgemm
#define    ATL_F77wrap_zhemm   atl_f77wrap_zhemm
#define    ATL_F77wrap_zherk   atl_f77wrap_zherk
#define    ATL_F77wrap_zher2k  atl_f77wrap_zher2k
#define    ATL_F77wrap_zsymm   atl_f77wrap_zsymm
#define    ATL_F77wrap_zsyrk   atl_f77wrap_zsyrk
#define    ATL_F77wrap_zsyr2k  atl_f77wrap_zsyr2k
#define    ATL_F77wrap_ztrmm   atl_f77wrap_ztrmm
#define    ATL_F77wrap_ztrsm   atl_f77wrap_ztrsm

#endif

#elif defined( Add__ )
/*
 * These defines  set  up  the  naming scheme required to have a FORTRAN
 * routine calling a C routine with the following interface:
 *
 *    FORTRAN CALL                          C declaration
 *    CALL ATL_F77WRAP_SGEMM(...)           void atl_f77wrap_sgemm__(...)
 */
#if   defined( SREAL )

#define    ATL_F77wrap_srotg   atl_f77wrap_srotg__
#define    ATL_F77wrap_srotmg  atl_f77wrap_srotmg__
#define    ATL_F77wrap_snrm2   atl_f77wrap_snrm2__
#define    ATL_F77wrap_sasum   atl_f77wrap_sasum__
#define    ATL_F77wrap_sscal   atl_f77wrap_sscal__
#define    ATL_F77wrap_isamax  atl_f77wrap_isamax__
#define    ATL_F77wrap_saxpy   atl_f77wrap_saxpy__
#define    ATL_F77wrap_scopy   atl_f77wrap_scopy__
#define    ATL_F77wrap_sswap   atl_f77wrap_sswap__
#define    ATL_F77wrap_srot    atl_f77wrap_srot__
#define    ATL_F77wrap_srotm   atl_f77wrap_srotm__
#define    ATL_F77wrap_sdot    atl_f77wrap_sdot__
#define    ATL_F77wrap_dsdot   atl_f77wrap_dsdot__
#define    ATL_F77wrap_sdsdot  atl_f77wrap_sdsdot__

#define    ATL_F77wrap_sgbmv   atl_f77wrap_sgbmv__
#define    ATL_F77wrap_sgemv   atl_f77wrap_sgemv__
#define    ATL_F77wrap_sger    atl_f77wrap_sger__
#define    ATL_F77wrap_sger2   atl_f77wrap_sger2__
#define    ATL_F77wrap_ssbmv   atl_f77wrap_ssbmv__
#define    ATL_F77wrap_sspmv   atl_f77wrap_sspmv__
#define    ATL_F77wrap_ssymv   atl_f77wrap_ssymv__
#define    ATL_F77wrap_sspr    atl_f77wrap_sspr__
#define    ATL_F77wrap_ssyr    atl_f77wrap_ssyr__
#define    ATL_F77wrap_sspr2   atl_f77wrap_sspr2__
#define    ATL_F77wrap_ssyr2   atl_f77wrap_ssyr2__
#define    ATL_F77wrap_stbmv   atl_f77wrap_stbmv__
#define    ATL_F77wrap_stpmv   atl_f77wrap_stpmv__
#define    ATL_F77wrap_strmv   atl_f77wrap_strmv__
#define    ATL_F77wrap_stbsv   atl_f77wrap_stbsv__
#define    ATL_F77wrap_stpsv   atl_f77wrap_stpsv__
#define    ATL_F77wrap_strsv   atl_f77wrap_strsv__

#define    ATL_F77wrap_sgemm   atl_f77wrap_sgemm__
#define    ATL_F77wrap_ssymm   atl_f77wrap_ssymm__
#define    ATL_F77wrap_ssyrk   atl_f77wrap_ssyrk__
#define    ATL_F77wrap_ssyr2k  atl_f77wrap_ssyr2k__
#define    ATL_F77wrap_strmm   atl_f77wrap_strmm__
#define    ATL_F77wrap_strsm   atl_f77wrap_strsm__

#elif defined( DREAL )

#define    ATL_F77wrap_drotg   atl_f77wrap_drotg__
#define    ATL_F77wrap_drotmg  atl_f77wrap_drotmg__
#define    ATL_F77wrap_dnrm2   atl_f77wrap_dnrm2__
#define    ATL_F77wrap_dasum   atl_f77wrap_dasum__
#define    ATL_F77wrap_dscal   atl_f77wrap_dscal__
#define    ATL_F77wrap_idamax  atl_f77wrap_idamax__
#define    ATL_F77wrap_daxpy   atl_f77wrap_daxpy__
#define    ATL_F77wrap_dcopy   atl_f77wrap_dcopy__
#define    ATL_F77wrap_dswap   atl_f77wrap_dswap__
#define    ATL_F77wrap_drot    atl_f77wrap_drot__
#define    ATL_F77wrap_drotm   atl_f77wrap_drotm__
#define    ATL_F77wrap_ddot    atl_f77wrap_ddot__

#define    ATL_F77wrap_dgbmv   atl_f77wrap_dgbmv__
#define    ATL_F77wrap_dgemv   atl_f77wrap_dgemv__
#define    ATL_F77wrap_dger    atl_f77wrap_dger__
#define    ATL_F77wrap_dger2   atl_f77wrap_dger2__
#define    ATL_F77wrap_dsbmv   atl_f77wrap_dsbmv__
#define    ATL_F77wrap_dspmv   atl_f77wrap_dspmv__
#define    ATL_F77wrap_dsymv   atl_f77wrap_dsymv__
#define    ATL_F77wrap_dspr    atl_f77wrap_dspr__
#define    ATL_F77wrap_dsyr    atl_f77wrap_dsyr__
#define    ATL_F77wrap_dspr2   atl_f77wrap_dspr2__
#define    ATL_F77wrap_dsyr2   atl_f77wrap_dsyr2__
#define    ATL_F77wrap_dtbmv   atl_f77wrap_dtbmv__
#define    ATL_F77wrap_dtpmv   atl_f77wrap_dtpmv__
#define    ATL_F77wrap_dtrmv   atl_f77wrap_dtrmv__
#define    ATL_F77wrap_dtbsv   atl_f77wrap_dtbsv__
#define    ATL_F77wrap_dtpsv   atl_f77wrap_dtpsv__
#define    ATL_F77wrap_dtrsv   atl_f77wrap_dtrsv__

#define    ATL_F77wrap_dgemm   atl_f77wrap_dgemm__
#define    ATL_F77wrap_dsymm   atl_f77wrap_dsymm__
#define    ATL_F77wrap_dsyrk   atl_f77wrap_dsyrk__
#define    ATL_F77wrap_dsyr2k  atl_f77wrap_dsyr2k__
#define    ATL_F77wrap_dtrmm   atl_f77wrap_dtrmm__
#define    ATL_F77wrap_dtrsm   atl_f77wrap_dtrsm__

#elif defined( SCPLX )

#define    ATL_F77wrap_crotg   atl_f77wrap_crotg__
#define    ATL_F77wrap_scnrm2  atl_f77wrap_scnrm2__
#define    ATL_F77wrap_scasum  atl_f77wrap_scasum__
#define    ATL_F77wrap_cscal   atl_f77wrap_cscal__
#define    ATL_F77wrap_csscal  atl_f77wrap_csscal__
#define    ATL_F77wrap_icamax  atl_f77wrap_icamax__
#define    ATL_F77wrap_caxpy   atl_f77wrap_caxpy__
#define    ATL_F77wrap_ccopy   atl_f77wrap_ccopy__
#define    ATL_F77wrap_cswap   atl_f77wrap_cswap__
#define    ATL_F77wrap_csrot   atl_f77wrap_csrot__
#define    ATL_F77wrap_cdotc   atl_f77wrap_cdotc__
#define    ATL_F77wrap_cdotu   atl_f77wrap_cdotu__

#define    ATL_F77wrap_cgbmv   atl_f77wrap_cgbmv__
#define    ATL_F77wrap_cgemv   atl_f77wrap_cgemv__
#define    ATL_F77wrap_cgerc   atl_f77wrap_cgerc__
#define    ATL_F77wrap_cgeru   atl_f77wrap_cgeru__
#define    ATL_F77wrap_cger2c  atl_f77wrap_cger2c__
#define    ATL_F77wrap_cger2u  atl_f77wrap_cger2u__
#define    ATL_F77wrap_chbmv   atl_f77wrap_chbmv__
#define    ATL_F77wrap_chpmv   atl_f77wrap_chpmv__
#define    ATL_F77wrap_chemv   atl_f77wrap_chemv__
#define    ATL_F77wrap_chpr    atl_f77wrap_chpr__
#define    ATL_F77wrap_cher    atl_f77wrap_cher__
#define    ATL_F77wrap_chpr2   atl_f77wrap_chpr2__
#define    ATL_F77wrap_cher2   atl_f77wrap_cher2__
#define    ATL_F77wrap_ctbmv   atl_f77wrap_ctbmv__
#define    ATL_F77wrap_ctpmv   atl_f77wrap_ctpmv__
#define    ATL_F77wrap_ctrmv   atl_f77wrap_ctrmv__
#define    ATL_F77wrap_ctbsv   atl_f77wrap_ctbsv__
#define    ATL_F77wrap_ctpsv   atl_f77wrap_ctpsv__
#define    ATL_F77wrap_ctrsv   atl_f77wrap_ctrsv__

#define    ATL_F77wrap_cgemm   atl_f77wrap_cgemm__
#define    ATL_F77wrap_chemm   atl_f77wrap_chemm__
#define    ATL_F77wrap_cherk   atl_f77wrap_cherk__
#define    ATL_F77wrap_cher2k  atl_f77wrap_cher2k__
#define    ATL_F77wrap_csymm   atl_f77wrap_csymm__
#define    ATL_F77wrap_csyrk   atl_f77wrap_csyrk__
#define    ATL_F77wrap_csyr2k  atl_f77wrap_csyr2k__
#define    ATL_F77wrap_ctrmm   atl_f77wrap_ctrmm__
#define    ATL_F77wrap_ctrsm   atl_f77wrap_ctrsm__

#elif defined( DCPLX )

#define    ATL_F77wrap_zrotg   atl_f77wrap_zrotg__
#define    ATL_F77wrap_dznrm2  atl_f77wrap_dznrm2__
#define    ATL_F77wrap_dzasum  atl_f77wrap_dzasum__
#define    ATL_F77wrap_zscal   atl_f77wrap_zscal__
#define    ATL_F77wrap_zdscal  atl_f77wrap_zdscal__
#define    ATL_F77wrap_izamax  atl_f77wrap_izamax__
#define    ATL_F77wrap_zaxpy   atl_f77wrap_zaxpy__
#define    ATL_F77wrap_zcopy   atl_f77wrap_zcopy__
#define    ATL_F77wrap_zswap   atl_f77wrap_zswap__
#define    ATL_F77wrap_zdrot   atl_f77wrap_zdrot__
#define    ATL_F77wrap_zdotc   atl_f77wrap_zdotc__
#define    ATL_F77wrap_zdotu   atl_f77wrap_zdotu__

#define    ATL_F77wrap_zgbmv   atl_f77wrap_zgbmv__
#define    ATL_F77wrap_zgemv   atl_f77wrap_zgemv__
#define    ATL_F77wrap_zgerc   atl_f77wrap_zgerc__
#define    ATL_F77wrap_zgeru   atl_f77wrap_zgeru__
#define    ATL_F77wrap_zger2c  atl_f77wrap_zger2c__
#define    ATL_F77wrap_zger2u  atl_f77wrap_zger2u__
#define    ATL_F77wrap_zhbmv   atl_f77wrap_zhbmv__
#define    ATL_F77wrap_zhpmv   atl_f77wrap_zhpmv__
#define    ATL_F77wrap_zhemv   atl_f77wrap_zhemv__
#define    ATL_F77wrap_zhpr    atl_f77wrap_zhpr__
#define    ATL_F77wrap_zher    atl_f77wrap_zher__
#define    ATL_F77wrap_zhpr2   atl_f77wrap_zhpr2__
#define    ATL_F77wrap_zher2   atl_f77wrap_zher2__
#define    ATL_F77wrap_ztbmv   atl_f77wrap_ztbmv__
#define    ATL_F77wrap_ztpmv   atl_f77wrap_ztpmv__
#define    ATL_F77wrap_ztrmv   atl_f77wrap_ztrmv__
#define    ATL_F77wrap_ztbsv   atl_f77wrap_ztbsv__
#define    ATL_F77wrap_ztpsv   atl_f77wrap_ztpsv__
#define    ATL_F77wrap_ztrsv   atl_f77wrap_ztrsv__

#define    ATL_F77wrap_zgemm   atl_f77wrap_zgemm__
#define    ATL_F77wrap_zhemm   atl_f77wrap_zhemm__
#define    ATL_F77wrap_zherk   atl_f77wrap_zherk__
#define    ATL_F77wrap_zher2k  atl_f77wrap_zher2k__
#define    ATL_F77wrap_zsymm   atl_f77wrap_zsymm__
#define    ATL_F77wrap_zsyrk   atl_f77wrap_zsyrk__
#define    ATL_F77wrap_zsyr2k  atl_f77wrap_zsyr2k__
#define    ATL_F77wrap_ztrmm   atl_f77wrap_ztrmm__
#define    ATL_F77wrap_ztrsm   atl_f77wrap_ztrsm__

#endif

#endif
/*
 * =====================================================================
 * Prototypes for F77 interface wrappers ATLAS BLAS routines
 * =====================================================================
 */
void       Mjoin( PATLF77WRAP,  rotg  )
( TYPE *,          TYPE *,          TYPE *,          TYPE * );
#ifdef TREAL
void       Mjoin( PATLF77WRAP,  rotmg )
( TYPE *,          TYPE *,          TYPE *,          TYPE *,
  TYPE * );
#endif
void       Mjoin( ATLUPF77WRAP, nrm2  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE * );
void       Mjoin( ATLUPF77WRAP, asum  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE * );
void       Mjoin( PATLF77WRAP,  scal  )
( F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER * );
#ifdef TCPLX
void       Mjoin( ATLPUF77WRAP, scal  )
( F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER * );
#endif
void       Mjoin( Mjoin( ATL_F77wrap_i, PRE ), amax )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  axpy  )
( F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER *,
  TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  copy  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  swap  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( ATLPUF77WRAP, rot   )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE *,          TYPE * );
#ifdef TREAL
void       Mjoin( PATLF77WRAP,  rotm  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE * );
#endif
#ifdef TREAL
void       Mjoin( PATLF77WRAP,  dot   )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE * );
#else
void       Mjoin( PATLF77WRAP,  dotc  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE * );
void       Mjoin( PATLF77WRAP,  dotu  )
( F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE * );
#endif
void       ATL_F77wrap_dsdot
( F77_INTEGER *,   float   *,       F77_INTEGER *,   float   *,
  F77_INTEGER *,   double  * );
void       ATL_F77wrap_sdsdot
( F77_INTEGER *,   float   *,       float   *,       F77_INTEGER *,
  float   *,       F77_INTEGER *,   float   * );

void       Mjoin( PATLF77WRAP,  gbmv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER *,
  TYPE *,          F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  gemv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER * );
#ifdef TREAL
void       Mjoin( PATLF77WRAP,  ger   )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  ger2  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *, F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  sbmv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  spmv  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  TYPE *,          F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  symv  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  spr   )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE * );
void       Mjoin( PATLF77WRAP,  syr   )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  spr2  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE * );
void       Mjoin( PATLF77WRAP,  syr2  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
#else
void       Mjoin( PATLF77WRAP,  gerc  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  geru  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  ger2c )
( F77_INTEGER *,   F77_INTEGER *,
  TYPE *, TYPE *, F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *, TYPE *, F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *, F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  ger2u )
( F77_INTEGER *,   F77_INTEGER *,
  TYPE *, TYPE *, F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *, TYPE *, F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *, F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  hbmv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER *,   TYPE *,          F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  hpmv  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  TYPE *,          F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  hemv  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  hpr   )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE * );
void       Mjoin( PATLF77WRAP,  her   )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  hpr2  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE * );
void       Mjoin( PATLF77WRAP,  her2  )
( F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
#endif
void       Mjoin( PATLF77WRAP,  tbmv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  tpmv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  trmv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          F77_INTEGER *,   TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  tbsv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  tpsv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  trsv  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          F77_INTEGER *,   TYPE *,          F77_INTEGER * );

void       Mjoin( PATLF77WRAP,  gemm  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER *,
  TYPE *,          F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER * );
#ifdef TCPLX
void       Mjoin( PATLF77WRAP,  hemm  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  herk  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  her2k )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER * );
#endif
void       Mjoin( PATLF77WRAP,  symm  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  syrk  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER *,   TYPE *,
  TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  syr2k )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  TYPE *,          TYPE *,          F77_INTEGER *,   TYPE *,
  F77_INTEGER *,   TYPE *,          TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  trmm  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER * );
void       Mjoin( PATLF77WRAP,  trsm  )
( F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,   F77_INTEGER *,
  F77_INTEGER *,   F77_INTEGER *,   TYPE *,          TYPE *,
  F77_INTEGER *,   TYPE *,          F77_INTEGER * );

#endif
/*
 * End of atlas_f77wrap.h
 */
