#ifndef ATLAS_CBLASZALIAS_H
   #define ATLAS_CBLASZALIAS_H

#define cblas_dotc_sub cblas_zdotc_sub
#define cblas_dotu_sub cblas_zdotu_sub
#define cblas_axpy     cblas_zaxpy
#define cblas_copy     cblas_zcopy
#define cblas_scal     cblas_zscal
#define cblas_swap     cblas_zswap
#define cblas_ger2c    cblas_zger2c
#define cblas_ger2u    cblas_zger2u
#define cblas_hpr2     cblas_zhpr2
#define cblas_her2     cblas_zher2
#define cblas_hpr      cblas_zhpr
#define cblas_her      cblas_zher
#define cblas_gerc     cblas_zgerc
#define cblas_geru     cblas_zgeru
#define cblas_tpsv     cblas_ztpsv
#define cblas_tbsv     cblas_ztbsv
#define cblas_trsv     cblas_ztrsv
#define cblas_tpmv     cblas_ztpmv
#define cblas_tbmv     cblas_ztbmv
#define cblas_trmv     cblas_ztrmv
#define cblas_hpmv     cblas_zhpmv
#define cblas_hbmv     cblas_zhbmv
#define cblas_hemv     cblas_zhemv
#define cblas_gbmv     cblas_zgbmv
#define cblas_gemv     cblas_zgemv
#define cblas_trsm     cblas_ztrsm
#define cblas_trmm     cblas_ztrmm
#define cblas_her2k    cblas_zher2k
#define cblas_syr2k    cblas_zsyr2k
#define cblas_herk     cblas_zherk
#define cblas_syrk     cblas_zsyrk
#define cblas_hemm     cblas_zhemm
#define cblas_symm     cblas_zsymm
#define cblas_gemm     cblas_zgemm
#define cblas_iamax    cblas_izamax
#define cblas_nrm2     cblas_dznrm2
#define cblas_asum     cblas_dzasum

#endif
