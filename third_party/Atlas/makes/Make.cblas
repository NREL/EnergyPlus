include Make.inc
mySRCdir = $(SRCdir)/interfaces/blas/C/src/

aobj = cblas_xerbla.o cblas_errprn.o
sptlvl3obj = cblas_sptgemm.o cblas_sptsymm.o cblas_sptsyr2k.o cblas_sptsyrk.o \
             cblas_spttrmm.o cblas_spttrsm.o
dptlvl3obj = cblas_dptgemm.o cblas_dptsymm.o cblas_dptsyr2k.o cblas_dptsyrk.o \
             cblas_dpttrmm.o cblas_dpttrsm.o

cptlvl3obj = cblas_cptgemm.o cblas_cpthemm.o cblas_cpther2k.o cblas_cptherk.o \
             cblas_cptsymm.o cblas_cptsyr2k.o cblas_cptsyrk.o cblas_cpttrmm.o \
             cblas_cpttrsm.o
zptlvl3obj = cblas_zptgemm.o cblas_zpthemm.o cblas_zpther2k.o cblas_zptherk.o \
             cblas_zptsymm.o cblas_zptsyr2k.o cblas_zptsyrk.o cblas_zpttrmm.o \
             cblas_zpttrsm.o

sptlvl2obj = cblas_sptgbmv.o cblas_sptgemv.o cblas_sptger.o cblas_sptger2.o \
             cblas_sptsbmv.o cblas_sptspmv.o cblas_sptspr.o cblas_sptspr2.o \
             cblas_sptsymv.o cblas_sptsyr.o cblas_sptsyr2.o cblas_spttbmv.o \
             cblas_spttbsv.o cblas_spttpmv.o cblas_spttpsv.o cblas_spttrmv.o \
             cblas_spttrsv.o
dptlvl2obj = cblas_dptgbmv.o cblas_dptgemv.o cblas_dptger.o cblas_dptger2.o \
             cblas_dptsbmv.o cblas_dptspmv.o cblas_dptspr.o cblas_dptspr2.o \
             cblas_dptsymv.o cblas_dptsyr.o cblas_dptsyr2.o cblas_dpttbmv.o \
             cblas_dpttbsv.o cblas_dpttpmv.o cblas_dpttpsv.o cblas_dpttrmv.o \
             cblas_dpttrsv.o
cptlvl2obj = cblas_cptgbmv.o cblas_cptgemv.o cblas_cptger2c.o \
             cblas_cptger2u.o cblas_cptgerc.o cblas_cptgeru.o cblas_cpthbmv.o \
             cblas_cpthemv.o cblas_cpther.o cblas_cpther2.o cblas_cpthpmv.o \
             cblas_cpthpr.o cblas_cpthpr2.o cblas_cpttbmv.o cblas_cpttbsv.o \
             cblas_cpttpmv.o cblas_cpttpsv.o cblas_cpttrmv.o cblas_cpttrsv.o
zptlvl2obj = cblas_zptgbmv.o cblas_zptgemv.o cblas_zptger2c.o \
             cblas_zptger2u.o cblas_zptgerc.o cblas_zptgeru.o cblas_zpthbmv.o \
             cblas_zpthemv.o cblas_zpther.o cblas_zpther2.o cblas_zpthpmv.o \
             cblas_zpthpr.o cblas_zpthpr2.o cblas_zpttbmv.o cblas_zpttbsv.o \
             cblas_zpttpmv.o cblas_zpttpsv.o cblas_zpttrmv.o cblas_zpttrsv.o

sptlvl1obj = catlas_sptaxpby.o catlas_sptset.o cblas_dsptdot.o \
             cblas_isptamax.o cblas_sdsptdot.o cblas_sptasum.o \
             cblas_sptaxpy.o cblas_sptcopy.o cblas_sptdot.o cblas_sptnrm2.o \
             cblas_sptrot.o cblas_sptrotg.o cblas_sptrotm.o cblas_sptrotmg.o \
             cblas_sptscal.o cblas_sptswap.o
dptlvl1obj = catlas_dptaxpby.o catlas_dptset.o cblas_dptasum.o \
             cblas_dptaxpy.o cblas_dptcopy.o cblas_dptdot.o cblas_dptnrm2.o \
             cblas_dptrot.o cblas_dptrotg.o cblas_dptrotm.o cblas_dptrotmg.o \
             cblas_dptscal.o cblas_dptswap.o cblas_idptamax.o

cptlvl1obj = catlas_cptaxpby.o catlas_cptset.o cblas_cptaxpy.o \
             cblas_cptcopy.o cblas_cptdotc.o cblas_cptdotu.o cblas_cptrotg.o \
             cblas_cptscal.o cblas_cptswap.o cblas_csptrot.o cblas_csptscal.o \
             cblas_icptamax.o cblas_scptasum.o cblas_scptnrm2.o
zptlvl1obj = catlas_zptaxpby.o catlas_zptset.o cblas_dzptasum.o \
             cblas_dzptnrm2.o cblas_izptamax.o cblas_zdptrot.o \
             cblas_zdptscal.o cblas_zptaxpy.o cblas_zptcopy.o cblas_zptdotc.o \
             cblas_zptdotu.o cblas_zptrotg.o cblas_zptscal.o cblas_zptswap.o
slvl3obj = cblas_sgemm.o cblas_ssymm.o cblas_ssyr2k.o cblas_ssyrk.o \
           cblas_strmm.o cblas_strsm.o
dlvl3obj = cblas_dgemm.o cblas_dsymm.o cblas_dsyr2k.o cblas_dsyrk.o \
           cblas_dtrmm.o cblas_dtrsm.o

clvl3obj = cblas_cgemm.o cblas_chemm.o cblas_cher2k.o cblas_cherk.o \
           cblas_csymm.o cblas_csyr2k.o cblas_csyrk.o cblas_ctrmm.o \
           cblas_ctrsm.o
zlvl3obj = cblas_zgemm.o cblas_zhemm.o cblas_zher2k.o cblas_zherk.o \
           cblas_zsymm.o cblas_zsyr2k.o cblas_zsyrk.o cblas_ztrmm.o \
           cblas_ztrsm.o

slvl2obj = cblas_sgbmv.o cblas_sgemv.o cblas_sger.o cblas_sger2.o \
           cblas_ssbmv.o cblas_sspmv.o cblas_sspr.o cblas_sspr2.o \
           cblas_ssymv.o cblas_ssyr.o cblas_ssyr2.o cblas_stbmv.o \
           cblas_stbsv.o cblas_stpmv.o cblas_stpsv.o cblas_strmv.o \
           cblas_strsv.o
dlvl2obj = cblas_dgbmv.o cblas_dgemv.o cblas_dger.o cblas_dger2.o \
           cblas_dsbmv.o cblas_dspmv.o cblas_dspr.o cblas_dspr2.o \
           cblas_dsymv.o cblas_dsyr.o cblas_dsyr2.o cblas_dtbmv.o \
           cblas_dtbsv.o cblas_dtpmv.o cblas_dtpsv.o cblas_dtrmv.o \
           cblas_dtrsv.o
clvl2obj = cblas_cgbmv.o cblas_cgemv.o cblas_cger2c.o cblas_cger2u.o \
           cblas_cgerc.o cblas_cgeru.o cblas_chbmv.o cblas_chemv.o \
           cblas_cher.o cblas_cher2.o cblas_chpmv.o cblas_chpr.o \
           cblas_chpr2.o cblas_ctbmv.o cblas_ctbsv.o cblas_ctpmv.o \
           cblas_ctpsv.o cblas_ctrmv.o cblas_ctrsv.o
zlvl2obj = cblas_zgbmv.o cblas_zgemv.o cblas_zger2c.o cblas_zger2u.o \
           cblas_zgerc.o cblas_zgeru.o cblas_zhbmv.o cblas_zhemv.o \
           cblas_zher.o cblas_zher2.o cblas_zhpmv.o cblas_zhpr.o \
           cblas_zhpr2.o cblas_ztbmv.o cblas_ztbsv.o cblas_ztpmv.o \
           cblas_ztpsv.o cblas_ztrmv.o cblas_ztrsv.o

slvl1obj = catlas_saxpby.o catlas_sset.o cblas_dsdot.o cblas_isamax.o \
           cblas_sasum.o cblas_saxpy.o cblas_scopy.o cblas_sdot.o \
           cblas_sdsdot.o cblas_snrm2.o cblas_srot.o cblas_srotg.o \
           cblas_srotm.o cblas_srotmg.o cblas_sscal.o cblas_sswap.o
dlvl1obj = catlas_daxpby.o catlas_dset.o cblas_dasum.o cblas_daxpy.o \
           cblas_dcopy.o cblas_ddot.o cblas_dnrm2.o cblas_drot.o \
           cblas_drotg.o cblas_drotm.o cblas_drotmg.o cblas_dscal.o \
           cblas_dswap.o cblas_idamax.o

clvl1obj = catlas_caxpby.o catlas_cset.o cblas_caxpy.o cblas_ccopy.o \
           cblas_cdotc.o cblas_cdotu.o cblas_crotg.o cblas_cscal.o \
           cblas_csrot.o cblas_csscal.o cblas_cswap.o cblas_icamax.o \
           cblas_scasum.o cblas_scnrm2.o
zlvl1obj = catlas_zaxpby.o catlas_zset.o cblas_dzasum.o cblas_dznrm2.o \
           cblas_izamax.o cblas_zaxpy.o cblas_zcopy.o cblas_zdotc.o \
           cblas_zdotu.o cblas_zdrot.o cblas_zdscal.o cblas_zrotg.o \
           cblas_zscal.o cblas_zswap.o

ptall : ptlib
ptlib : dptlib sptlib zptlib cptlib

ptlvl3 : dptlvl3 sptlvl3 cptlvl3 zptlvl3
ptlvl2 : dptlvl2 sptlvl2 cptlvl2 zptlvl2
ptlvl1 : dptlvl1 sptlvl1 cptlvl1 zptlvl1
dptlib : dptlvl3 dptlvl2 dptlvl1
sptlib : sptlvl3 sptlvl2 sptlvl1
cptlib : cptlvl3 cptlvl2 cptlvl1
zptlib : zptlvl3 zptlvl2 zptlvl1
ptclean : sptclean dptclean cptclean zptclean
	rm -f $(aobj)

sptlvl1 : 
	$(PMAKE) sptlvl1.grd
sptlvl1.grd : $(sptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(sptlvl1obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch sptlvl1.grd
sptlvl2 : 
	$(PMAKE) sptlvl2.grd
sptlvl2.grd : $(sptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(sptlvl2obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch sptlvl2.grd
sptlvl3 : 
	$(PMAKE) sptlvl3.grd
sptlvl3.grd : $(sptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(sptlvl3obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch sptlvl3.grd
sptclean :
	rm -f $(sptlvl1obj) $(sptlvl2obj) $(sptlvl3obj) s?ptlvl?.grd
dptlvl1 : 
	$(PMAKE) dptlvl1.grd
dptlvl1.grd : $(dptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(dptlvl1obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch dptlvl1.grd
dptlvl2 : 
	$(PMAKE) dptlvl2.grd
dptlvl2.grd : $(dptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(dptlvl2obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch dptlvl2.grd
dptlvl3 : 
	$(PMAKE) dptlvl3.grd
dptlvl3.grd : $(dptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(dptlvl3obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch dptlvl3.grd
dptclean :
	rm -f $(dptlvl1obj) $(dptlvl2obj) $(dptlvl3obj) d?ptlvl?.grd
cptlvl1 : 
	$(PMAKE) cptlvl1.grd
cptlvl1.grd : $(cptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(cptlvl1obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch cptlvl1.grd
cptlvl2 : 
	$(PMAKE) cptlvl2.grd
cptlvl2.grd : $(cptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(cptlvl2obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch cptlvl2.grd
cptlvl3 : 
	$(PMAKE) cptlvl3.grd
cptlvl3.grd : $(cptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(cptlvl3obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch cptlvl3.grd
cptclean :
	rm -f $(cptlvl1obj) $(cptlvl2obj) $(cptlvl3obj) c?ptlvl?.grd
zptlvl1 : 
	$(PMAKE) zptlvl1.grd
zptlvl1.grd : $(zptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(zptlvl1obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch zptlvl1.grd
zptlvl2 : 
	$(PMAKE) zptlvl2.grd
zptlvl2.grd : $(zptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(zptlvl2obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch zptlvl2.grd
zptlvl3 : 
	$(PMAKE) zptlvl3.grd
zptlvl3.grd : $(zptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTCBLASlib) $(zptlvl3obj) $(aobj) 
	$(RANLIB) $(PTCBLASlib)
	touch zptlvl3.grd
zptclean :
	rm -f $(zptlvl1obj) $(zptlvl2obj) $(zptlvl3obj) z?ptlvl?.grd
all : lib
lib : dlib slib zlib clib

lvl3 : dlvl3 slvl3 clvl3 zlvl3
lvl2 : dlvl2 slvl2 clvl2 zlvl2
lvl1 : dlvl1 slvl1 clvl1 zlvl1
dlib : dlvl3 dlvl2 dlvl1
slib : slvl3 slvl2 slvl1
clib : clvl3 clvl2 clvl1
zlib : zlvl3 zlvl2 zlvl1
clean : sclean dclean cclean zclean
	rm -f $(aobj)

slvl1 : 
	$(PMAKE) slvl1.grd
slvl1.grd : $(slvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(slvl1obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch slvl1.grd
slvl2 : 
	$(PMAKE) slvl2.grd
slvl2.grd : $(slvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(slvl2obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch slvl2.grd
slvl3 : 
	$(PMAKE) slvl3.grd
slvl3.grd : $(slvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(slvl3obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch slvl3.grd
sclean :
	rm -f $(slvl1obj) $(slvl2obj) $(slvl3obj) s?lvl?.grd
dlvl1 : 
	$(PMAKE) dlvl1.grd
dlvl1.grd : $(dlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(dlvl1obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch dlvl1.grd
dlvl2 : 
	$(PMAKE) dlvl2.grd
dlvl2.grd : $(dlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(dlvl2obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch dlvl2.grd
dlvl3 : 
	$(PMAKE) dlvl3.grd
dlvl3.grd : $(dlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(dlvl3obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch dlvl3.grd
dclean :
	rm -f $(dlvl1obj) $(dlvl2obj) $(dlvl3obj) d?lvl?.grd
clvl1 : 
	$(PMAKE) clvl1.grd
clvl1.grd : $(clvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(clvl1obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch clvl1.grd
clvl2 : 
	$(PMAKE) clvl2.grd
clvl2.grd : $(clvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(clvl2obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch clvl2.grd
clvl3 : 
	$(PMAKE) clvl3.grd
clvl3.grd : $(clvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(clvl3obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch clvl3.grd
cclean :
	rm -f $(clvl1obj) $(clvl2obj) $(clvl3obj) c?lvl?.grd
zlvl1 : 
	$(PMAKE) zlvl1.grd
zlvl1.grd : $(zlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(zlvl1obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch zlvl1.grd
zlvl2 : 
	$(PMAKE) zlvl2.grd
zlvl2.grd : $(zlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(zlvl2obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch zlvl2.grd
zlvl3 : 
	$(PMAKE) zlvl3.grd
zlvl3.grd : $(zlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(CBLASlib) $(zlvl3obj) $(aobj) 
	$(RANLIB) $(CBLASlib)
	touch zlvl3.grd
zclean :
	rm -f $(zlvl1obj) $(zlvl2obj) $(zlvl3obj) z?lvl?.grd

INCdep = $(INCSdir)/atlas_misc.h $(INCSdir)/atlas_lvl3.h \
         $(INCSdir)/cblas.h $(INCSdir)/atlas_level1.h 
INCPTdep = $(INCSdir)/atlas_misc.h $(INCSdir)/atlas_lvl3.h \
           $(INCSdir)/cblas.h $(INCSdir)/atlas_level1.h \
           $(INCSdir)/atlas_refalias1.h $(INCSdir)/atlas_refalias2.h \
           $(INCSdir)/atlas_refalias3.h $(INCSdir)/atlas_ptalias3.h

cblas_sptgemm.o : $(mySRCdir)/cblas_sgemm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sgemm.c
cblas_spttrsm.o : $(mySRCdir)/cblas_strsm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_strsm.c
cblas_spttrmm.o : $(mySRCdir)/cblas_strmm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_strmm.c
cblas_sptsyr2k.o : $(mySRCdir)/cblas_ssyr2k.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssyr2k.c
cblas_sptsyrk.o : $(mySRCdir)/cblas_ssyrk.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssyrk.c
cblas_sptsymm.o : $(mySRCdir)/cblas_ssymm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssymm.c
cblas_spttpsv.o : $(mySRCdir)/cblas_stpsv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_stpsv.c
cblas_spttbsv.o : $(mySRCdir)/cblas_stbsv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_stbsv.c
cblas_spttrsv.o : $(mySRCdir)/cblas_strsv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_strsv.c
cblas_spttpmv.o : $(mySRCdir)/cblas_stpmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_stpmv.c
cblas_spttbmv.o : $(mySRCdir)/cblas_stbmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_stbmv.c
cblas_spttrmv.o : $(mySRCdir)/cblas_strmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_strmv.c
cblas_sptspmv.o : $(mySRCdir)/cblas_sspmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sspmv.c
cblas_sptsbmv.o : $(mySRCdir)/cblas_ssbmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssbmv.c
cblas_sptsymv.o : $(mySRCdir)/cblas_ssymv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssymv.c
cblas_sptgbmv.o : $(mySRCdir)/cblas_sgbmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sgbmv.c
cblas_sptgemv.o : $(mySRCdir)/cblas_sgemv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sgemv.c
cblas_sptger2.o : $(mySRCdir)/cblas_sger2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sger2.c
cblas_sptspr2.o : $(mySRCdir)/cblas_sspr2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sspr2.c
cblas_sptsyr2.o : $(mySRCdir)/cblas_ssyr2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssyr2.c
cblas_sptspr.o : $(mySRCdir)/cblas_sspr.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sspr.c
cblas_sptsyr.o : $(mySRCdir)/cblas_ssyr.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ssyr.c
cblas_sptger.o : $(mySRCdir)/cblas_sger.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sger.c
cblas_sptasum.o : $(mySRCdir)/cblas_sasum.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sasum.c
cblas_sptnrm2.o : $(mySRCdir)/cblas_snrm2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_snrm2.c
cblas_sptdot.o : $(mySRCdir)/cblas_sdot.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sdot.c
cblas_sptaxpy.o : $(mySRCdir)/cblas_saxpy.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_saxpy.c
cblas_sptcopy.o : $(mySRCdir)/cblas_scopy.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_scopy.c
cblas_sptscal.o : $(mySRCdir)/cblas_sscal.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sscal.c
cblas_sptswap.o : $(mySRCdir)/cblas_sswap.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sswap.c
cblas_sptrotm.o : $(mySRCdir)/cblas_srotm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_srotm.c
cblas_sptrot.o : $(mySRCdir)/cblas_srot.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_srot.c
cblas_sptrotmg.o : $(mySRCdir)/cblas_srotmg.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_srotmg.c
cblas_sptrotg.o : $(mySRCdir)/cblas_srotg.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_srotg.c
catlas_sptset.o : $(mySRCdir)/catlas_sset.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_sset.c
catlas_sptaxpby.o : $(mySRCdir)/catlas_saxpby.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_saxpby.c
cblas_isptamax.o : $(mySRCdir)/cblas_isamax.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_isamax.c
cblas_dptgemm.o : $(mySRCdir)/cblas_dgemm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dgemm.c
cblas_dpttrsm.o : $(mySRCdir)/cblas_dtrsm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtrsm.c
cblas_dpttrmm.o : $(mySRCdir)/cblas_dtrmm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtrmm.c
cblas_dptsyr2k.o : $(mySRCdir)/cblas_dsyr2k.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsyr2k.c
cblas_dptsyrk.o : $(mySRCdir)/cblas_dsyrk.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsyrk.c
cblas_dptsymm.o : $(mySRCdir)/cblas_dsymm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsymm.c
cblas_dpttpsv.o : $(mySRCdir)/cblas_dtpsv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtpsv.c
cblas_dpttbsv.o : $(mySRCdir)/cblas_dtbsv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtbsv.c
cblas_dpttrsv.o : $(mySRCdir)/cblas_dtrsv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtrsv.c
cblas_dpttpmv.o : $(mySRCdir)/cblas_dtpmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtpmv.c
cblas_dpttbmv.o : $(mySRCdir)/cblas_dtbmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtbmv.c
cblas_dpttrmv.o : $(mySRCdir)/cblas_dtrmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dtrmv.c
cblas_dptspmv.o : $(mySRCdir)/cblas_dspmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dspmv.c
cblas_dptsbmv.o : $(mySRCdir)/cblas_dsbmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsbmv.c
cblas_dptsymv.o : $(mySRCdir)/cblas_dsymv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsymv.c
cblas_dptgbmv.o : $(mySRCdir)/cblas_dgbmv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dgbmv.c
cblas_dptgemv.o : $(mySRCdir)/cblas_dgemv.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dgemv.c
cblas_dptger2.o : $(mySRCdir)/cblas_dger2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dger2.c
cblas_dptspr2.o : $(mySRCdir)/cblas_dspr2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dspr2.c
cblas_dptsyr2.o : $(mySRCdir)/cblas_dsyr2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsyr2.c
cblas_dptspr.o : $(mySRCdir)/cblas_dspr.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dspr.c
cblas_dptsyr.o : $(mySRCdir)/cblas_dsyr.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsyr.c
cblas_dptger.o : $(mySRCdir)/cblas_dger.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dger.c
cblas_dptasum.o : $(mySRCdir)/cblas_dasum.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dasum.c
cblas_dptnrm2.o : $(mySRCdir)/cblas_dnrm2.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dnrm2.c
cblas_dptdot.o : $(mySRCdir)/cblas_ddot.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ddot.c
cblas_dptaxpy.o : $(mySRCdir)/cblas_daxpy.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_daxpy.c
cblas_dptcopy.o : $(mySRCdir)/cblas_dcopy.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dcopy.c
cblas_dptscal.o : $(mySRCdir)/cblas_dscal.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dscal.c
cblas_dptswap.o : $(mySRCdir)/cblas_dswap.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dswap.c
cblas_dptrotm.o : $(mySRCdir)/cblas_drotm.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_drotm.c
cblas_dptrot.o : $(mySRCdir)/cblas_drot.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_drot.c
cblas_dptrotmg.o : $(mySRCdir)/cblas_drotmg.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_drotmg.c
cblas_dptrotg.o : $(mySRCdir)/cblas_drotg.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_drotg.c
catlas_dptset.o : $(mySRCdir)/catlas_dset.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_dset.c
catlas_dptaxpby.o : $(mySRCdir)/catlas_daxpby.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_daxpby.c
cblas_idptamax.o : $(mySRCdir)/cblas_idamax.c $(INCPTdep)
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_idamax.c

cblas_cptgemm.o : $(mySRCdir)/cblas_cgemm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cgemm.c
cblas_cpttrsm.o : $(mySRCdir)/cblas_ctrsm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctrsm.c
cblas_cpttrmm.o : $(mySRCdir)/cblas_ctrmm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctrmm.c
cblas_cptsyr2k.o : $(mySRCdir)/cblas_csyr2k.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_csyr2k.c
cblas_cptsyrk.o : $(mySRCdir)/cblas_csyrk.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_csyrk.c
cblas_cptsymm.o : $(mySRCdir)/cblas_csymm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_csymm.c
cblas_cpther2k.o : $(mySRCdir)/cblas_cher2k.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cher2k.c
cblas_cptherk.o : $(mySRCdir)/cblas_cherk.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cherk.c
cblas_cpthemm.o : $(mySRCdir)/cblas_chemm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_chemm.c
cblas_cpttpsv.o : $(mySRCdir)/cblas_ctpsv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctpsv.c
cblas_cpttbsv.o : $(mySRCdir)/cblas_ctbsv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctbsv.c
cblas_cpttrsv.o : $(mySRCdir)/cblas_ctrsv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctrsv.c
cblas_cpttpmv.o : $(mySRCdir)/cblas_ctpmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctpmv.c
cblas_cpttbmv.o : $(mySRCdir)/cblas_ctbmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctbmv.c
cblas_cpttrmv.o : $(mySRCdir)/cblas_ctrmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ctrmv.c
cblas_cpthpmv.o : $(mySRCdir)/cblas_chpmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_chpmv.c
cblas_cpthbmv.o : $(mySRCdir)/cblas_chbmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_chbmv.c
cblas_cpthemv.o : $(mySRCdir)/cblas_chemv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_chemv.c
cblas_cptgbmv.o : $(mySRCdir)/cblas_cgbmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cgbmv.c
cblas_cptgemv.o : $(mySRCdir)/cblas_cgemv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cgemv.c
cblas_cptger2c.o : $(mySRCdir)/cblas_cger2c.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cger2c.c
cblas_cptger2u.o : $(mySRCdir)/cblas_cger2u.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cger2u.c
cblas_cpthpr2.o : $(mySRCdir)/cblas_chpr2.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_chpr2.c
cblas_cpther2.o : $(mySRCdir)/cblas_cher2.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cher2.c
cblas_cpthpr.o : $(mySRCdir)/cblas_chpr.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_chpr.c
cblas_cpther.o : $(mySRCdir)/cblas_cher.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cher.c
cblas_cptgerc.o : $(mySRCdir)/cblas_cgerc.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cgerc.c
cblas_cptgeru.o : $(mySRCdir)/cblas_cgeru.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cgeru.c
cblas_cptdotu.o : $(mySRCdir)/cblas_cdotu.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cdotu.c
cblas_cptdotc.o : $(mySRCdir)/cblas_cdotc.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cdotc.c
cblas_cptaxpy.o : $(mySRCdir)/cblas_caxpy.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_caxpy.c
cblas_cptcopy.o : $(mySRCdir)/cblas_ccopy.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ccopy.c
cblas_cptscal.o : $(mySRCdir)/cblas_cscal.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cscal.c
cblas_cptswap.o : $(mySRCdir)/cblas_cswap.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_cswap.c
cblas_cptrotg.o : $(mySRCdir)/cblas_crotg.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_crotg.c
catlas_cptset.o : $(mySRCdir)/catlas_cset.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_cset.c
catlas_cptaxpby.o : $(mySRCdir)/catlas_caxpby.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_caxpby.c
cblas_icptamax.o : $(mySRCdir)/cblas_icamax.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_icamax.c
cblas_scptasum.o : $(mySRCdir)/cblas_scasum.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_scasum.c
cblas_scptnrm2.o : $(mySRCdir)/cblas_scnrm2.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_scnrm2.c
cblas_csptrot.o : $(mySRCdir)/cblas_csrot.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_csrot.c
cblas_csptscal.o : $(mySRCdir)/cblas_csscal.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_csscal.c
cblas_zptgemm.o : $(mySRCdir)/cblas_zgemm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zgemm.c
cblas_zpttrsm.o : $(mySRCdir)/cblas_ztrsm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztrsm.c
cblas_zpttrmm.o : $(mySRCdir)/cblas_ztrmm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztrmm.c
cblas_zptsyr2k.o : $(mySRCdir)/cblas_zsyr2k.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zsyr2k.c
cblas_zptsyrk.o : $(mySRCdir)/cblas_zsyrk.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zsyrk.c
cblas_zptsymm.o : $(mySRCdir)/cblas_zsymm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zsymm.c
cblas_zpther2k.o : $(mySRCdir)/cblas_zher2k.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zher2k.c
cblas_zptherk.o : $(mySRCdir)/cblas_zherk.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zherk.c
cblas_zpthemm.o : $(mySRCdir)/cblas_zhemm.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zhemm.c
cblas_zpttpsv.o : $(mySRCdir)/cblas_ztpsv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztpsv.c
cblas_zpttbsv.o : $(mySRCdir)/cblas_ztbsv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztbsv.c
cblas_zpttrsv.o : $(mySRCdir)/cblas_ztrsv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztrsv.c
cblas_zpttpmv.o : $(mySRCdir)/cblas_ztpmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztpmv.c
cblas_zpttbmv.o : $(mySRCdir)/cblas_ztbmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztbmv.c
cblas_zpttrmv.o : $(mySRCdir)/cblas_ztrmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_ztrmv.c
cblas_zpthpmv.o : $(mySRCdir)/cblas_zhpmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zhpmv.c
cblas_zpthbmv.o : $(mySRCdir)/cblas_zhbmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zhbmv.c
cblas_zpthemv.o : $(mySRCdir)/cblas_zhemv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zhemv.c
cblas_zptgbmv.o : $(mySRCdir)/cblas_zgbmv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zgbmv.c
cblas_zptgemv.o : $(mySRCdir)/cblas_zgemv.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zgemv.c
cblas_zptger2c.o : $(mySRCdir)/cblas_zger2c.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zger2c.c
cblas_zptger2u.o : $(mySRCdir)/cblas_zger2u.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zger2u.c
cblas_zpthpr2.o : $(mySRCdir)/cblas_zhpr2.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zhpr2.c
cblas_zpther2.o : $(mySRCdir)/cblas_zher2.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zher2.c
cblas_zpthpr.o : $(mySRCdir)/cblas_zhpr.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zhpr.c
cblas_zpther.o : $(mySRCdir)/cblas_zher.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zher.c
cblas_zptgerc.o : $(mySRCdir)/cblas_zgerc.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zgerc.c
cblas_zptgeru.o : $(mySRCdir)/cblas_zgeru.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zgeru.c
cblas_zptdotu.o : $(mySRCdir)/cblas_zdotu.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zdotu.c
cblas_zptdotc.o : $(mySRCdir)/cblas_zdotc.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zdotc.c
cblas_zptaxpy.o : $(mySRCdir)/cblas_zaxpy.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zaxpy.c
cblas_zptcopy.o : $(mySRCdir)/cblas_zcopy.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zcopy.c
cblas_zptscal.o : $(mySRCdir)/cblas_zscal.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zscal.c
cblas_zptswap.o : $(mySRCdir)/cblas_zswap.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zswap.c
cblas_zptrotg.o : $(mySRCdir)/cblas_zrotg.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zrotg.c
catlas_zptset.o : $(mySRCdir)/catlas_zset.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_zset.c
catlas_zptaxpby.o : $(mySRCdir)/catlas_zaxpby.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/catlas_zaxpby.c
cblas_izptamax.o : $(mySRCdir)/cblas_izamax.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_izamax.c
cblas_dzptasum.o : $(mySRCdir)/cblas_dzasum.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dzasum.c
cblas_dzptnrm2.o : $(mySRCdir)/cblas_dznrm2.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dznrm2.c
cblas_zdptrot.o : $(mySRCdir)/cblas_zdrot.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zdrot.c
cblas_zdptscal.o : $(mySRCdir)/cblas_zdscal.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_zdscal.c

cblas_sdsptdot.o : $(mySRCdir)/cblas_sdsdot.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_sdsdot.c
cblas_dsptdot.o : $(mySRCdir)/cblas_dsdot.c $(INCPTdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS $(mySRCdir)/cblas_dsdot.c
cblas_sgemm.o : $(mySRCdir)/cblas_sgemm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sgemm.c
cblas_strsm.o : $(mySRCdir)/cblas_strsm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_strsm.c
cblas_strmm.o : $(mySRCdir)/cblas_strmm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_strmm.c
cblas_ssyr2k.o : $(mySRCdir)/cblas_ssyr2k.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssyr2k.c
cblas_ssyrk.o : $(mySRCdir)/cblas_ssyrk.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssyrk.c
cblas_ssymm.o : $(mySRCdir)/cblas_ssymm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssymm.c
cblas_stpsv.o : $(mySRCdir)/cblas_stpsv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_stpsv.c
cblas_stbsv.o : $(mySRCdir)/cblas_stbsv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_stbsv.c
cblas_strsv.o : $(mySRCdir)/cblas_strsv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_strsv.c
cblas_stpmv.o : $(mySRCdir)/cblas_stpmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_stpmv.c
cblas_stbmv.o : $(mySRCdir)/cblas_stbmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_stbmv.c
cblas_strmv.o : $(mySRCdir)/cblas_strmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_strmv.c
cblas_sspmv.o : $(mySRCdir)/cblas_sspmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sspmv.c
cblas_ssbmv.o : $(mySRCdir)/cblas_ssbmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssbmv.c
cblas_ssymv.o : $(mySRCdir)/cblas_ssymv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssymv.c
cblas_sgbmv.o : $(mySRCdir)/cblas_sgbmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sgbmv.c
cblas_sgemv.o : $(mySRCdir)/cblas_sgemv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sgemv.c
cblas_sger2.o : $(mySRCdir)/cblas_sger2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sger2.c
cblas_sspr2.o : $(mySRCdir)/cblas_sspr2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sspr2.c
cblas_ssyr2.o : $(mySRCdir)/cblas_ssyr2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssyr2.c
cblas_sspr.o : $(mySRCdir)/cblas_sspr.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sspr.c
cblas_ssyr.o : $(mySRCdir)/cblas_ssyr.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ssyr.c
cblas_sger.o : $(mySRCdir)/cblas_sger.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sger.c
cblas_sasum.o : $(mySRCdir)/cblas_sasum.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sasum.c
cblas_snrm2.o : $(mySRCdir)/cblas_snrm2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_snrm2.c
cblas_sdot.o : $(mySRCdir)/cblas_sdot.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sdot.c
cblas_saxpy.o : $(mySRCdir)/cblas_saxpy.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_saxpy.c
cblas_scopy.o : $(mySRCdir)/cblas_scopy.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_scopy.c
cblas_sscal.o : $(mySRCdir)/cblas_sscal.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sscal.c
cblas_sswap.o : $(mySRCdir)/cblas_sswap.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_sswap.c
cblas_srotm.o : $(mySRCdir)/cblas_srotm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_srotm.c
cblas_srot.o : $(mySRCdir)/cblas_srot.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_srot.c
cblas_srotmg.o : $(mySRCdir)/cblas_srotmg.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_srotmg.c
cblas_srotg.o : $(mySRCdir)/cblas_srotg.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_srotg.c
catlas_sset.o : $(mySRCdir)/catlas_sset.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/catlas_sset.c
catlas_saxpby.o : $(mySRCdir)/catlas_saxpby.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/catlas_saxpby.c
cblas_isamax.o : $(mySRCdir)/cblas_isamax.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_isamax.c
cblas_dgemm.o : $(mySRCdir)/cblas_dgemm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dgemm.c
cblas_dtrsm.o : $(mySRCdir)/cblas_dtrsm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtrsm.c
cblas_dtrmm.o : $(mySRCdir)/cblas_dtrmm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtrmm.c
cblas_dsyr2k.o : $(mySRCdir)/cblas_dsyr2k.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsyr2k.c
cblas_dsyrk.o : $(mySRCdir)/cblas_dsyrk.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsyrk.c
cblas_dsymm.o : $(mySRCdir)/cblas_dsymm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsymm.c
cblas_dtpsv.o : $(mySRCdir)/cblas_dtpsv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtpsv.c
cblas_dtbsv.o : $(mySRCdir)/cblas_dtbsv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtbsv.c
cblas_dtrsv.o : $(mySRCdir)/cblas_dtrsv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtrsv.c
cblas_dtpmv.o : $(mySRCdir)/cblas_dtpmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtpmv.c
cblas_dtbmv.o : $(mySRCdir)/cblas_dtbmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtbmv.c
cblas_dtrmv.o : $(mySRCdir)/cblas_dtrmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dtrmv.c
cblas_dspmv.o : $(mySRCdir)/cblas_dspmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dspmv.c
cblas_dsbmv.o : $(mySRCdir)/cblas_dsbmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsbmv.c
cblas_dsymv.o : $(mySRCdir)/cblas_dsymv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsymv.c
cblas_dgbmv.o : $(mySRCdir)/cblas_dgbmv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dgbmv.c
cblas_dgemv.o : $(mySRCdir)/cblas_dgemv.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dgemv.c
cblas_dger2.o : $(mySRCdir)/cblas_dger2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dger2.c
cblas_dspr2.o : $(mySRCdir)/cblas_dspr2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dspr2.c
cblas_dsyr2.o : $(mySRCdir)/cblas_dsyr2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsyr2.c
cblas_dspr.o : $(mySRCdir)/cblas_dspr.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dspr.c
cblas_dsyr.o : $(mySRCdir)/cblas_dsyr.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dsyr.c
cblas_dger.o : $(mySRCdir)/cblas_dger.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dger.c
cblas_dasum.o : $(mySRCdir)/cblas_dasum.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dasum.c
cblas_dnrm2.o : $(mySRCdir)/cblas_dnrm2.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dnrm2.c
cblas_ddot.o : $(mySRCdir)/cblas_ddot.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_ddot.c
cblas_daxpy.o : $(mySRCdir)/cblas_daxpy.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_daxpy.c
cblas_dcopy.o : $(mySRCdir)/cblas_dcopy.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dcopy.c
cblas_dscal.o : $(mySRCdir)/cblas_dscal.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dscal.c
cblas_dswap.o : $(mySRCdir)/cblas_dswap.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_dswap.c
cblas_drotm.o : $(mySRCdir)/cblas_drotm.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_drotm.c
cblas_drot.o : $(mySRCdir)/cblas_drot.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_drot.c
cblas_drotmg.o : $(mySRCdir)/cblas_drotmg.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_drotmg.c
cblas_drotg.o : $(mySRCdir)/cblas_drotg.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_drotg.c
catlas_dset.o : $(mySRCdir)/catlas_dset.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/catlas_dset.c
catlas_daxpby.o : $(mySRCdir)/catlas_daxpby.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/catlas_daxpby.c
cblas_idamax.o : $(mySRCdir)/cblas_idamax.c $(INCdep)
	$(KC) -o $@ -c $(KCFLAGS)  $(mySRCdir)/cblas_idamax.c

cblas_cgemm.o : $(mySRCdir)/cblas_cgemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cgemm.c
cblas_ctrsm.o : $(mySRCdir)/cblas_ctrsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctrsm.c
cblas_ctrmm.o : $(mySRCdir)/cblas_ctrmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctrmm.c
cblas_csyr2k.o : $(mySRCdir)/cblas_csyr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_csyr2k.c
cblas_csyrk.o : $(mySRCdir)/cblas_csyrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_csyrk.c
cblas_csymm.o : $(mySRCdir)/cblas_csymm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_csymm.c
cblas_cher2k.o : $(mySRCdir)/cblas_cher2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cher2k.c
cblas_cherk.o : $(mySRCdir)/cblas_cherk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cherk.c
cblas_chemm.o : $(mySRCdir)/cblas_chemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_chemm.c
cblas_ctpsv.o : $(mySRCdir)/cblas_ctpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctpsv.c
cblas_ctbsv.o : $(mySRCdir)/cblas_ctbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctbsv.c
cblas_ctrsv.o : $(mySRCdir)/cblas_ctrsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctrsv.c
cblas_ctpmv.o : $(mySRCdir)/cblas_ctpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctpmv.c
cblas_ctbmv.o : $(mySRCdir)/cblas_ctbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctbmv.c
cblas_ctrmv.o : $(mySRCdir)/cblas_ctrmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ctrmv.c
cblas_chpmv.o : $(mySRCdir)/cblas_chpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_chpmv.c
cblas_chbmv.o : $(mySRCdir)/cblas_chbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_chbmv.c
cblas_chemv.o : $(mySRCdir)/cblas_chemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_chemv.c
cblas_cgbmv.o : $(mySRCdir)/cblas_cgbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cgbmv.c
cblas_cgemv.o : $(mySRCdir)/cblas_cgemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cgemv.c
cblas_cger2c.o : $(mySRCdir)/cblas_cger2c.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cger2c.c
cblas_cger2u.o : $(mySRCdir)/cblas_cger2u.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cger2u.c
cblas_chpr2.o : $(mySRCdir)/cblas_chpr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_chpr2.c
cblas_cher2.o : $(mySRCdir)/cblas_cher2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cher2.c
cblas_chpr.o : $(mySRCdir)/cblas_chpr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_chpr.c
cblas_cher.o : $(mySRCdir)/cblas_cher.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cher.c
cblas_cgerc.o : $(mySRCdir)/cblas_cgerc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cgerc.c
cblas_cgeru.o : $(mySRCdir)/cblas_cgeru.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cgeru.c
cblas_cdotu.o : $(mySRCdir)/cblas_cdotu.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cdotu.c
cblas_cdotc.o : $(mySRCdir)/cblas_cdotc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cdotc.c
cblas_caxpy.o : $(mySRCdir)/cblas_caxpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_caxpy.c
cblas_ccopy.o : $(mySRCdir)/cblas_ccopy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ccopy.c
cblas_cscal.o : $(mySRCdir)/cblas_cscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cscal.c
cblas_cswap.o : $(mySRCdir)/cblas_cswap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_cswap.c
cblas_crotg.o : $(mySRCdir)/cblas_crotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_crotg.c
catlas_cset.o : $(mySRCdir)/catlas_cset.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/catlas_cset.c
catlas_caxpby.o : $(mySRCdir)/catlas_caxpby.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/catlas_caxpby.c
cblas_icamax.o : $(mySRCdir)/cblas_icamax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_icamax.c
cblas_scasum.o : $(mySRCdir)/cblas_scasum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_scasum.c
cblas_scnrm2.o : $(mySRCdir)/cblas_scnrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_scnrm2.c
cblas_csrot.o : $(mySRCdir)/cblas_csrot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_csrot.c
cblas_csscal.o : $(mySRCdir)/cblas_csscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_csscal.c
cblas_zgemm.o : $(mySRCdir)/cblas_zgemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zgemm.c
cblas_ztrsm.o : $(mySRCdir)/cblas_ztrsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztrsm.c
cblas_ztrmm.o : $(mySRCdir)/cblas_ztrmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztrmm.c
cblas_zsyr2k.o : $(mySRCdir)/cblas_zsyr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zsyr2k.c
cblas_zsyrk.o : $(mySRCdir)/cblas_zsyrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zsyrk.c
cblas_zsymm.o : $(mySRCdir)/cblas_zsymm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zsymm.c
cblas_zher2k.o : $(mySRCdir)/cblas_zher2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zher2k.c
cblas_zherk.o : $(mySRCdir)/cblas_zherk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zherk.c
cblas_zhemm.o : $(mySRCdir)/cblas_zhemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zhemm.c
cblas_ztpsv.o : $(mySRCdir)/cblas_ztpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztpsv.c
cblas_ztbsv.o : $(mySRCdir)/cblas_ztbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztbsv.c
cblas_ztrsv.o : $(mySRCdir)/cblas_ztrsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztrsv.c
cblas_ztpmv.o : $(mySRCdir)/cblas_ztpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztpmv.c
cblas_ztbmv.o : $(mySRCdir)/cblas_ztbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztbmv.c
cblas_ztrmv.o : $(mySRCdir)/cblas_ztrmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_ztrmv.c
cblas_zhpmv.o : $(mySRCdir)/cblas_zhpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zhpmv.c
cblas_zhbmv.o : $(mySRCdir)/cblas_zhbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zhbmv.c
cblas_zhemv.o : $(mySRCdir)/cblas_zhemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zhemv.c
cblas_zgbmv.o : $(mySRCdir)/cblas_zgbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zgbmv.c
cblas_zgemv.o : $(mySRCdir)/cblas_zgemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zgemv.c
cblas_zger2c.o : $(mySRCdir)/cblas_zger2c.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zger2c.c
cblas_zger2u.o : $(mySRCdir)/cblas_zger2u.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zger2u.c
cblas_zhpr2.o : $(mySRCdir)/cblas_zhpr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zhpr2.c
cblas_zher2.o : $(mySRCdir)/cblas_zher2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zher2.c
cblas_zhpr.o : $(mySRCdir)/cblas_zhpr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zhpr.c
cblas_zher.o : $(mySRCdir)/cblas_zher.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zher.c
cblas_zgerc.o : $(mySRCdir)/cblas_zgerc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zgerc.c
cblas_zgeru.o : $(mySRCdir)/cblas_zgeru.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zgeru.c
cblas_zdotu.o : $(mySRCdir)/cblas_zdotu.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zdotu.c
cblas_zdotc.o : $(mySRCdir)/cblas_zdotc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zdotc.c
cblas_zaxpy.o : $(mySRCdir)/cblas_zaxpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zaxpy.c
cblas_zcopy.o : $(mySRCdir)/cblas_zcopy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zcopy.c
cblas_zscal.o : $(mySRCdir)/cblas_zscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zscal.c
cblas_zswap.o : $(mySRCdir)/cblas_zswap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zswap.c
cblas_zrotg.o : $(mySRCdir)/cblas_zrotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zrotg.c
catlas_zset.o : $(mySRCdir)/catlas_zset.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/catlas_zset.c
catlas_zaxpby.o : $(mySRCdir)/catlas_zaxpby.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/catlas_zaxpby.c
cblas_izamax.o : $(mySRCdir)/cblas_izamax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_izamax.c
cblas_dzasum.o : $(mySRCdir)/cblas_dzasum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_dzasum.c
cblas_dznrm2.o : $(mySRCdir)/cblas_dznrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_dznrm2.c
cblas_zdrot.o : $(mySRCdir)/cblas_zdrot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zdrot.c
cblas_zdscal.o : $(mySRCdir)/cblas_zdscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_zdscal.c

cblas_sdsdot.o : $(mySRCdir)/cblas_sdsdot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_sdsdot.c
cblas_dsdot.o : $(mySRCdir)/cblas_dsdot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS)  $(mySRCdir)/cblas_dsdot.c

cblas_errprn.o : $(mySRCdir)/cblas_errprn.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/cblas_errprn.c
cblas_xerbla.o : $(mySRCdir)/cblas_xerbla.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/cblas_xerbla.c
