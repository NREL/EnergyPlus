#ifndef ATLAS_PTALIAS3_H
#define ATLAS_PTALIAS3_H
/*
 * Real BLAS
 */
   #define ATL_ssyr2k  ATL_stsyr2k
   #define ATL_ssyrk   ATL_stsyrk
   #define ATL_ssymm   ATL_stsymm
   #define ATL_strmm   ATL_sttrmm
   #define ATL_strsm   ATL_sttrsm
   #define ATL_sgemm   ATL_stgemm

   #define ATL_dsyr2k  ATL_dtsyr2k
   #define ATL_dsyrk   ATL_dtsyrk
   #define ATL_dsymm   ATL_dtsymm
   #define ATL_dtrmm   ATL_dttrmm
   #define ATL_dtrsm   ATL_dttrsm
   #define ATL_dgemm   ATL_dtgemm

/*
 * Complex BLAS
 */
   #define ATL_ctrmm     ATL_cttrmm
   #define ATL_cher2k    ATL_cther2k
   #define ATL_csyr2k    ATL_ctsyr2k
   #define ATL_cherk     ATL_ctherk
   #define ATL_csyrk     ATL_ctsyrk
   #define ATL_chemm     ATL_cthemm
   #define ATL_csymm     ATL_ctsymm
   #define ATL_cgemm     ATL_ctgemm
   #define ATL_ctrsm     ATL_cttrsm

   #define ATL_ztrmm     ATL_zttrmm
   #define ATL_zher2k    ATL_zther2k
   #define ATL_zsyr2k    ATL_ztsyr2k
   #define ATL_zherk     ATL_ztherk
   #define ATL_zsyrk     ATL_ztsyrk
   #define ATL_zhemm     ATL_zthemm
   #define ATL_zsymm     ATL_ztsymm
   #define ATL_zgemm     ATL_ztgemm
   #define ATL_ztrsm     ATL_zttrsm

#endif
