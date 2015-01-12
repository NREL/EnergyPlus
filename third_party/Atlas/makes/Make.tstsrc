include Make.inc
mySRCdir = $(SRCdir)/src/testing

INCdep = $(INCSdir)/atlas_misc.h $(INCSdir)/atlas_f77.h \
         $(INCSdir)/atlas_tst.h
INCdep1 = $(INCSdir)/atlas_misc.h $(INCSdir)/atlas_tst.h

obj = ATL_rand.o

   sf77wobjs = ATL_sf77wrap.o
   df77wobjs = ATL_df77wrap.o
   cf77wobjs = ATL_cf77wrap.o
   zf77wobjs = ATL_zf77wrap.o

sblas = ATL_dsf77dot.o ATL_sdsf77dot.o ATL_sf77amax.o ATL_sf77asum.o \
        ATL_sf77axpy.o ATL_sf77copy.o ATL_sf77dot.o ATL_sf77gbmv.o \
        ATL_sf77gelqf.o ATL_sf77gels.o ATL_sf77gemm.o ATL_sf77gemv.o \
        ATL_sf77geqlf.o ATL_sf77geqrf.o ATL_sf77ger.o ATL_sf77gerqf.o \
        ATL_sf77gesv.o ATL_sf77getrf.o ATL_sf77getri.o ATL_sf77lauum.o \
        ATL_sf77nrm2.o ATL_sf77posv.o ATL_sf77potrf.o ATL_sf77rot.o \
        ATL_sf77rotg.o ATL_sf77rotm.o ATL_sf77rotmg.o ATL_sf77sbmv.o \
        ATL_sf77scal.o ATL_sf77spmv.o ATL_sf77spr.o ATL_sf77spr2.o \
        ATL_sf77swap.o ATL_sf77symm.o ATL_sf77symv.o ATL_sf77syr.o \
        ATL_sf77syr2.o ATL_sf77syr2k.o ATL_sf77syrk.o ATL_sf77tbmv.o \
        ATL_sf77tbsv.o ATL_sf77tpmv.o ATL_sf77tpsv.o ATL_sf77trmm.o \
        ATL_sf77trmv.o ATL_sf77trsm.o ATL_sf77trsv.o ATL_sf77trtri.o

dblas = ATL_df77amax.o ATL_df77asum.o ATL_df77axpy.o ATL_df77copy.o \
        ATL_df77dot.o ATL_df77gbmv.o ATL_df77gelqf.o ATL_df77gels.o \
        ATL_df77gemm.o ATL_df77gemv.o ATL_df77geqlf.o ATL_df77geqrf.o \
        ATL_df77ger.o ATL_df77gerqf.o ATL_df77gesv.o ATL_df77getrf.o \
        ATL_df77getri.o ATL_df77lauum.o ATL_df77nrm2.o ATL_df77posv.o \
        ATL_df77potrf.o ATL_df77rot.o ATL_df77rotg.o ATL_df77rotm.o \
        ATL_df77rotmg.o ATL_df77sbmv.o ATL_df77scal.o ATL_df77spmv.o \
        ATL_df77spr.o ATL_df77spr2.o ATL_df77swap.o ATL_df77symm.o \
        ATL_df77symv.o ATL_df77syr.o ATL_df77syr2.o ATL_df77syr2k.o \
        ATL_df77syrk.o ATL_df77tbmv.o ATL_df77tbsv.o ATL_df77tpmv.o \
        ATL_df77tpsv.o ATL_df77trmm.o ATL_df77trmv.o ATL_df77trsm.o \
        ATL_df77trsv.o ATL_df77trtri.o


cblas = ATL_cf77amax.o ATL_cf77asum.o ATL_cf77axpy.o ATL_cf77copy.o \
        ATL_cf77dotc_sub.o ATL_cf77dotu_sub.o ATL_cf77gbmv.o ATL_cf77gelqf.o \
        ATL_cf77gels.o ATL_cf77gemm.o ATL_cf77gemv.o ATL_cf77geqlf.o \
        ATL_cf77geqrf.o ATL_cf77gerc.o ATL_cf77gerqf.o ATL_cf77geru.o \
        ATL_cf77gesv.o ATL_cf77getrf.o ATL_cf77getri.o ATL_cf77hbmv.o \
        ATL_cf77hemm.o ATL_cf77hemv.o ATL_cf77her.o ATL_cf77her2.o \
        ATL_cf77her2k.o ATL_cf77herk.o ATL_cf77hpmv.o ATL_cf77hpr.o \
        ATL_cf77hpr2.o ATL_cf77lauum.o ATL_cf77nrm2.o ATL_cf77posv.o \
        ATL_cf77potrf.o ATL_cf77rot.o ATL_cf77rotg.o ATL_cf77scal.o \
        ATL_cf77swap.o ATL_cf77symm.o ATL_cf77syr2k.o ATL_cf77syrk.o \
        ATL_cf77tbmv.o ATL_cf77tbsv.o ATL_cf77tpmv.o ATL_cf77tpsv.o \
        ATL_cf77trmm.o ATL_cf77trmv.o ATL_cf77trsm.o ATL_cf77trsv.o \
        ATL_cf77trtri.o ATL_csf77scal.o

zblas = ATL_zdf77scal.o ATL_zf77amax.o ATL_zf77asum.o ATL_zf77axpy.o \
        ATL_zf77copy.o ATL_zf77dotc_sub.o ATL_zf77dotu_sub.o ATL_zf77gbmv.o \
        ATL_zf77gelqf.o ATL_zf77gels.o ATL_zf77gemm.o ATL_zf77gemv.o \
        ATL_zf77geqlf.o ATL_zf77geqrf.o ATL_zf77gerc.o ATL_zf77gerqf.o \
        ATL_zf77geru.o ATL_zf77gesv.o ATL_zf77getrf.o ATL_zf77getri.o \
        ATL_zf77hbmv.o ATL_zf77hemm.o ATL_zf77hemv.o ATL_zf77her.o \
        ATL_zf77her2.o ATL_zf77her2k.o ATL_zf77herk.o ATL_zf77hpmv.o \
        ATL_zf77hpr.o ATL_zf77hpr2.o ATL_zf77lauum.o ATL_zf77nrm2.o \
        ATL_zf77posv.o ATL_zf77potrf.o ATL_zf77rot.o ATL_zf77rotg.o \
        ATL_zf77scal.o ATL_zf77swap.o ATL_zf77symm.o ATL_zf77syr2k.o \
        ATL_zf77syrk.o ATL_zf77tbmv.o ATL_zf77tbsv.o ATL_zf77tpmv.o \
        ATL_zf77tpsv.o ATL_zf77trmm.o ATL_zf77trmv.o ATL_zf77trsm.o \
        ATL_zf77trsv.o ATL_zf77trtri.o


all : lib
lib : dlib slib zlib clib
clean : dclean sclean zclean cclean
	rm -f $(obj)

sobj = $(sblas) ATL_sgegen.o ATL_sgeprint.o \
            ATL_sepsilon.o ATL_sgediff.o ATL_sgenrm1.o \
            ATL_sgediffnrm1.o ATL_sinfnrm.o ATL_svdiff.o \
            ATL_strgen.o ATL_strnrm1.o ATL_sgbnrm1.o \
            ATL_ssbnrm.o ATL_sspnrm.o ATL_ssynrm.o \
            ATL_stbnrm1.o ATL_stpnrm1.o ATL_ststsqtran.o \
            ATL_shediffnrm.o

slib : 
	$(PMAKE) slib.grd
slib.grd : $(sobj) $(obj) $(sf77wobjs)
	$(ARCHIVER) $(ARFLAGS) $(TESTlib) $(sobj) $(obj)
	- $(ARCHIVER) $(ARFLAGS) $(TESTlib) $(sf77wobjs)
	$(RANLIB) $(TESTlib)
	touch slib.grd

sclean : 
	rm -f $(sobj) $(sf77wobjs) s*.grd
dobj = $(dblas) ATL_dgegen.o ATL_dgeprint.o \
            ATL_depsilon.o ATL_dgediff.o ATL_dgenrm1.o \
            ATL_dgediffnrm1.o ATL_dinfnrm.o ATL_dvdiff.o \
            ATL_dtrgen.o ATL_dtrnrm1.o ATL_dgbnrm1.o \
            ATL_dsbnrm.o ATL_dspnrm.o ATL_dsynrm.o \
            ATL_dtbnrm1.o ATL_dtpnrm1.o ATL_dtstsqtran.o \
            ATL_dhediffnrm.o

dlib : 
	$(PMAKE) dlib.grd
dlib.grd : $(dobj) $(obj) $(df77wobjs)
	$(ARCHIVER) $(ARFLAGS) $(TESTlib) $(dobj) $(obj)
	- $(ARCHIVER) $(ARFLAGS) $(TESTlib) $(df77wobjs)
	$(RANLIB) $(TESTlib)
	touch dlib.grd

dclean : 
	rm -f $(dobj) $(df77wobjs) d*.grd
cobj = $(cblas) ATL_cgegen.o ATL_cgeprint.o \
            ATL_cepsilon.o ATL_cgediff.o ATL_cgenrm1.o \
            ATL_cgediffnrm1.o ATL_cinfnrm.o ATL_cvdiff.o \
            ATL_ctrgen.o ATL_ctrnrm1.o ATL_cgbnrm1.o \
            ATL_chbnrm.o ATL_chpnrm.o ATL_chenrm.o \
            ATL_csbnrm.o ATL_cspnrm.o ATL_csynrm.o \
            ATL_ctbnrm1.o ATL_ctpnrm1.o ATL_ctstsqtran.o \
            ATL_chediffnrm.o

clib : 
	$(PMAKE) clib.grd
clib.grd : $(cobj) $(obj) $(cf77wobjs)
	$(ARCHIVER) $(ARFLAGS) $(TESTlib) $(cobj) $(obj)
	- $(ARCHIVER) $(ARFLAGS) $(TESTlib) $(cf77wobjs)
	$(RANLIB) $(TESTlib)
	touch clib.grd

cclean : 
	rm -f $(cobj) $(cf77wobjs) c*.grd
zobj = $(zblas) ATL_zgegen.o ATL_zgeprint.o \
            ATL_zepsilon.o ATL_zgediff.o ATL_zgenrm1.o \
            ATL_zgediffnrm1.o ATL_zinfnrm.o ATL_zvdiff.o \
            ATL_ztrgen.o ATL_ztrnrm1.o ATL_zgbnrm1.o \
            ATL_zhbnrm.o ATL_zhpnrm.o ATL_zhenrm.o \
            ATL_zsbnrm.o ATL_zspnrm.o ATL_zsynrm.o \
            ATL_ztbnrm1.o ATL_ztpnrm1.o ATL_ztstsqtran.o \
            ATL_zhediffnrm.o

zlib : 
	$(PMAKE) zlib.grd
zlib.grd : $(zobj) $(obj) $(zf77wobjs)
	$(ARCHIVER) $(ARFLAGS) $(TESTlib) $(zobj) $(obj)
	- $(ARCHIVER) $(ARFLAGS) $(TESTlib) $(zf77wobjs)
	$(RANLIB) $(TESTlib)
	touch zlib.grd

zclean : 
	rm -f $(zobj) $(zf77wobjs) z*.grd

ATL_sf77wrap.o : $(mySRCdir)/ATL_sf77wrap.f
	- $(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ATL_sf77wrap.f
ATL_df77wrap.o : $(mySRCdir)/ATL_df77wrap.f
	- $(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ATL_df77wrap.f
ATL_cf77wrap.o : $(mySRCdir)/ATL_cf77wrap.f
	- $(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ATL_cf77wrap.f
ATL_zf77wrap.o : $(mySRCdir)/ATL_zf77wrap.f
	- $(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ATL_zf77wrap.f

ATL_sf77gemm.o : $(mySRCdir)/ATL_f77gemm.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gemm.c
ATL_sf77trsm.o : $(mySRCdir)/ATL_f77trsm.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77trsm.c
ATL_sf77trmm.o : $(mySRCdir)/ATL_f77trmm.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77trmm.c
ATL_sf77syr2k.o : $(mySRCdir)/ATL_f77syr2k.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77syr2k.c
ATL_sf77syrk.o : $(mySRCdir)/ATL_f77syrk.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77syrk.c
ATL_sf77symm.o : $(mySRCdir)/ATL_f77symm.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77symm.c
ATL_sf77ger.o : $(mySRCdir)/ATL_f77ger.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77ger.c
ATL_sf77gemv.o : $(mySRCdir)/ATL_f77gemv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gemv.c
ATL_sf77trmv.o : $(mySRCdir)/ATL_f77trmv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77trmv.c
ATL_sf77trsv.o : $(mySRCdir)/ATL_f77trsv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77trsv.c
ATL_sf77tpsv.o : $(mySRCdir)/ATL_f77tpsv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77tpsv.c
ATL_sf77tbsv.o : $(mySRCdir)/ATL_f77tbsv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77tbsv.c
ATL_sf77tpmv.o : $(mySRCdir)/ATL_f77tpmv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77tpmv.c
ATL_sf77tbmv.o : $(mySRCdir)/ATL_f77tbmv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77tbmv.c
ATL_sf77syr2.o : $(mySRCdir)/ATL_f77syr2.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77syr2.c
ATL_sf77syr.o : $(mySRCdir)/ATL_f77syr.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77syr.c
ATL_sf77symv.o : $(mySRCdir)/ATL_f77symv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77symv.c
ATL_sf77spr2.o : $(mySRCdir)/ATL_f77spr2.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77spr2.c
ATL_sf77spr.o : $(mySRCdir)/ATL_f77spr.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77spr.c
ATL_sf77spmv.o : $(mySRCdir)/ATL_f77spmv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77spmv.c
ATL_sf77sbmv.o : $(mySRCdir)/ATL_f77sbmv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77sbmv.c
ATL_sf77gbmv.o : $(mySRCdir)/ATL_f77gbmv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gbmv.c
ATL_sf77dot.o : $(mySRCdir)/ATL_f77dot.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77dot.c
ATL_sf77rotm.o : $(mySRCdir)/ATL_f77rotm.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77rotm.c
ATL_sf77rot.o : $(mySRCdir)/ATL_f77rot.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77rot.c
ATL_sf77swap.o : $(mySRCdir)/ATL_f77swap.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77swap.c
ATL_sf77copy.o : $(mySRCdir)/ATL_f77copy.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77copy.c
ATL_sf77axpy.o : $(mySRCdir)/ATL_f77axpy.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77axpy.c
ATL_sf77scal.o : $(mySRCdir)/ATL_f77scal.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77scal.c
ATL_sf77amax.o : $(mySRCdir)/ATL_f77amax.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77amax.c
ATL_sf77asum.o : $(mySRCdir)/ATL_f77asum.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77asum.c
ATL_sf77nrm2.o : $(mySRCdir)/ATL_f77nrm2.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77nrm2.c
ATL_sf77rotmg.o : $(mySRCdir)/ATL_f77rotmg.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77rotmg.c
ATL_sf77rotg.o : $(mySRCdir)/ATL_f77rotg.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77rotg.c
ATL_sf77gesv.o : $(mySRCdir)/ATL_f77gesv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gesv.c
ATL_sf77gels.o : $(mySRCdir)/ATL_f77gels.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gels.c
ATL_sf77posv.o : $(mySRCdir)/ATL_f77posv.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77posv.c
ATL_sf77lauum.o : $(mySRCdir)/ATL_f77lauum.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77lauum.c
ATL_sf77trtri.o : $(mySRCdir)/ATL_f77trtri.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77trtri.c
ATL_sf77potrf.o : $(mySRCdir)/ATL_f77potrf.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77potrf.c
ATL_sf77getrf.o : $(mySRCdir)/ATL_f77getrf.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77getrf.c
ATL_sf77getri.o : $(mySRCdir)/ATL_f77getri.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77getri.c
ATL_sf77geqlf.o : $(mySRCdir)/ATL_f77geqlf.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77geqlf.c
ATL_sf77gelqf.o : $(mySRCdir)/ATL_f77gelqf.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gelqf.c
ATL_sf77gerqf.o : $(mySRCdir)/ATL_f77gerqf.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77gerqf.c
ATL_sf77geqrf.o : $(mySRCdir)/ATL_f77geqrf.c $(INCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_f77geqrf.c
ATL_shediffnrm.o : $(mySRCdir)/ATL_hediffnrm.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_hediffnrm.c
ATL_svdiff.o : $(mySRCdir)/ATL_vdiff.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_vdiff.c
ATL_sinfnrm.o : $(mySRCdir)/ATL_infnrm.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_infnrm.c
ATL_sgediffnrm1.o : $(mySRCdir)/ATL_gediffnrm1.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gediffnrm1.c
ATL_sgenrm1.o : $(mySRCdir)/ATL_genrm1.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_genrm1.c
ATL_sepsilon.o : $(mySRCdir)/ATL_epsilon.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_epsilon.c
ATL_sgediff.o : $(mySRCdir)/ATL_gediff.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gediff.c
ATL_sgeprint.o : $(mySRCdir)/ATL_geprint.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_geprint.c
ATL_sgegen.o : $(mySRCdir)/ATL_gegen.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gegen.c
ATL_ststsqtran.o : $(mySRCdir)/ATL_tstsqtran.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_tstsqtran.c
ATL_ssynrm.o : $(mySRCdir)/ATL_synrm.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_synrm.c
ATL_sspnrm.o : $(mySRCdir)/ATL_spnrm.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_spnrm.c
ATL_ssbnrm.o : $(mySRCdir)/ATL_sbnrm.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_sbnrm.c
ATL_stpnrm1.o : $(mySRCdir)/ATL_tpnrm1.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_tpnrm1.c
ATL_stbnrm1.o : $(mySRCdir)/ATL_tbnrm1.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_tbnrm1.c
ATL_sgbnrm1.o : $(mySRCdir)/ATL_gbnrm1.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gbnrm1.c
ATL_strnrm1.o : $(mySRCdir)/ATL_trnrm1.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_trnrm1.c
ATL_strgen.o : $(mySRCdir)/ATL_trgen.c $(INCSdir)/atlas_misc.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_trgen.c
ATL_df77gemm.o : $(mySRCdir)/ATL_f77gemm.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gemm.c
ATL_df77trsm.o : $(mySRCdir)/ATL_f77trsm.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77trsm.c
ATL_df77trmm.o : $(mySRCdir)/ATL_f77trmm.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77trmm.c
ATL_df77syr2k.o : $(mySRCdir)/ATL_f77syr2k.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77syr2k.c
ATL_df77syrk.o : $(mySRCdir)/ATL_f77syrk.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77syrk.c
ATL_df77symm.o : $(mySRCdir)/ATL_f77symm.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77symm.c
ATL_df77ger.o : $(mySRCdir)/ATL_f77ger.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77ger.c
ATL_df77gemv.o : $(mySRCdir)/ATL_f77gemv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gemv.c
ATL_df77trmv.o : $(mySRCdir)/ATL_f77trmv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77trmv.c
ATL_df77trsv.o : $(mySRCdir)/ATL_f77trsv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77trsv.c
ATL_df77tpsv.o : $(mySRCdir)/ATL_f77tpsv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77tpsv.c
ATL_df77tbsv.o : $(mySRCdir)/ATL_f77tbsv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77tbsv.c
ATL_df77tpmv.o : $(mySRCdir)/ATL_f77tpmv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77tpmv.c
ATL_df77tbmv.o : $(mySRCdir)/ATL_f77tbmv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77tbmv.c
ATL_df77syr2.o : $(mySRCdir)/ATL_f77syr2.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77syr2.c
ATL_df77syr.o : $(mySRCdir)/ATL_f77syr.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77syr.c
ATL_df77symv.o : $(mySRCdir)/ATL_f77symv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77symv.c
ATL_df77spr2.o : $(mySRCdir)/ATL_f77spr2.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77spr2.c
ATL_df77spr.o : $(mySRCdir)/ATL_f77spr.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77spr.c
ATL_df77spmv.o : $(mySRCdir)/ATL_f77spmv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77spmv.c
ATL_df77sbmv.o : $(mySRCdir)/ATL_f77sbmv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77sbmv.c
ATL_df77gbmv.o : $(mySRCdir)/ATL_f77gbmv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gbmv.c
ATL_df77dot.o : $(mySRCdir)/ATL_f77dot.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77dot.c
ATL_df77rotm.o : $(mySRCdir)/ATL_f77rotm.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77rotm.c
ATL_df77rot.o : $(mySRCdir)/ATL_f77rot.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77rot.c
ATL_df77swap.o : $(mySRCdir)/ATL_f77swap.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77swap.c
ATL_df77copy.o : $(mySRCdir)/ATL_f77copy.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77copy.c
ATL_df77axpy.o : $(mySRCdir)/ATL_f77axpy.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77axpy.c
ATL_df77scal.o : $(mySRCdir)/ATL_f77scal.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77scal.c
ATL_df77amax.o : $(mySRCdir)/ATL_f77amax.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77amax.c
ATL_df77asum.o : $(mySRCdir)/ATL_f77asum.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77asum.c
ATL_df77nrm2.o : $(mySRCdir)/ATL_f77nrm2.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77nrm2.c
ATL_df77rotmg.o : $(mySRCdir)/ATL_f77rotmg.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77rotmg.c
ATL_df77rotg.o : $(mySRCdir)/ATL_f77rotg.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77rotg.c
ATL_df77gesv.o : $(mySRCdir)/ATL_f77gesv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gesv.c
ATL_df77gels.o : $(mySRCdir)/ATL_f77gels.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gels.c
ATL_df77posv.o : $(mySRCdir)/ATL_f77posv.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77posv.c
ATL_df77lauum.o : $(mySRCdir)/ATL_f77lauum.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77lauum.c
ATL_df77trtri.o : $(mySRCdir)/ATL_f77trtri.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77trtri.c
ATL_df77potrf.o : $(mySRCdir)/ATL_f77potrf.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77potrf.c
ATL_df77getrf.o : $(mySRCdir)/ATL_f77getrf.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77getrf.c
ATL_df77getri.o : $(mySRCdir)/ATL_f77getri.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77getri.c
ATL_df77geqlf.o : $(mySRCdir)/ATL_f77geqlf.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77geqlf.c
ATL_df77gelqf.o : $(mySRCdir)/ATL_f77gelqf.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gelqf.c
ATL_df77gerqf.o : $(mySRCdir)/ATL_f77gerqf.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77gerqf.c
ATL_df77geqrf.o : $(mySRCdir)/ATL_f77geqrf.c $(INCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_f77geqrf.c
ATL_dhediffnrm.o : $(mySRCdir)/ATL_hediffnrm.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_hediffnrm.c
ATL_dvdiff.o : $(mySRCdir)/ATL_vdiff.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_vdiff.c
ATL_dinfnrm.o : $(mySRCdir)/ATL_infnrm.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_infnrm.c
ATL_dgediffnrm1.o : $(mySRCdir)/ATL_gediffnrm1.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gediffnrm1.c
ATL_dgenrm1.o : $(mySRCdir)/ATL_genrm1.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_genrm1.c
ATL_depsilon.o : $(mySRCdir)/ATL_epsilon.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_epsilon.c
ATL_dgediff.o : $(mySRCdir)/ATL_gediff.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gediff.c
ATL_dgeprint.o : $(mySRCdir)/ATL_geprint.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_geprint.c
ATL_dgegen.o : $(mySRCdir)/ATL_gegen.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gegen.c
ATL_dtstsqtran.o : $(mySRCdir)/ATL_tstsqtran.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_tstsqtran.c
ATL_dsynrm.o : $(mySRCdir)/ATL_synrm.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_synrm.c
ATL_dspnrm.o : $(mySRCdir)/ATL_spnrm.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_spnrm.c
ATL_dsbnrm.o : $(mySRCdir)/ATL_sbnrm.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_sbnrm.c
ATL_dtpnrm1.o : $(mySRCdir)/ATL_tpnrm1.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_tpnrm1.c
ATL_dtbnrm1.o : $(mySRCdir)/ATL_tbnrm1.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_tbnrm1.c
ATL_dgbnrm1.o : $(mySRCdir)/ATL_gbnrm1.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gbnrm1.c
ATL_dtrnrm1.o : $(mySRCdir)/ATL_trnrm1.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_trnrm1.c
ATL_dtrgen.o : $(mySRCdir)/ATL_trgen.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_trgen.c

ATL_sdsf77dot.o : $(mySRCdir)/ATL_sdsf77dot.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DSREAL $(mySRCdir)/ATL_sdsf77dot.c
ATL_dsf77dot.o : $(mySRCdir)/ATL_dsf77dot.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DSREAL $(mySRCdir)/ATL_dsf77dot.c
ATL_rand.o : $(mySRCdir)/ATL_rand.c $(INCSdir)/atlas_misc.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DSREAL $(mySRCdir)/ATL_rand.c

ATL_cf77gemm.o : $(mySRCdir)/ATL_f77gemm.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gemm.c
ATL_cf77trsm.o : $(mySRCdir)/ATL_f77trsm.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77trsm.c
ATL_cf77trmm.o : $(mySRCdir)/ATL_f77trmm.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77trmm.c
ATL_cf77syr2k.o : $(mySRCdir)/ATL_f77syr2k.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77syr2k.c
ATL_cf77syrk.o : $(mySRCdir)/ATL_f77syrk.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77syrk.c
ATL_cf77symm.o : $(mySRCdir)/ATL_f77symm.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77symm.c
ATL_cf77her2k.o : $(mySRCdir)/ATL_f77her2k.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77her2k.c
ATL_cf77herk.o : $(mySRCdir)/ATL_f77herk.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77herk.c
ATL_cf77hemm.o : $(mySRCdir)/ATL_f77hemm.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77hemm.c
ATL_cf77geru.o : $(mySRCdir)/ATL_f77geru.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77geru.c
ATL_cf77gerc.o : $(mySRCdir)/ATL_f77gerc.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gerc.c
ATL_cf77gemv.o : $(mySRCdir)/ATL_f77gemv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gemv.c
ATL_cf77trmv.o : $(mySRCdir)/ATL_f77trmv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77trmv.c
ATL_cf77trsv.o : $(mySRCdir)/ATL_f77trsv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77trsv.c
ATL_cf77tpsv.o : $(mySRCdir)/ATL_f77tpsv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77tpsv.c
ATL_cf77tpmv.o : $(mySRCdir)/ATL_f77tpmv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77tpmv.c
ATL_cf77tbsv.o : $(mySRCdir)/ATL_f77tbsv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77tbsv.c
ATL_cf77tbmv.o : $(mySRCdir)/ATL_f77tbmv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77tbmv.c
ATL_cf77hpr2.o : $(mySRCdir)/ATL_f77hpr2.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77hpr2.c
ATL_cf77hpr.o : $(mySRCdir)/ATL_f77hpr.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77hpr.c
ATL_cf77hpmv.o : $(mySRCdir)/ATL_f77hpmv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77hpmv.c
ATL_cf77her2.o : $(mySRCdir)/ATL_f77her2.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77her2.c
ATL_cf77her.o : $(mySRCdir)/ATL_f77her.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77her.c
ATL_cf77hemv.o : $(mySRCdir)/ATL_f77hemv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77hemv.c
ATL_cf77hbmv.o : $(mySRCdir)/ATL_f77hbmv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77hbmv.c
ATL_cf77gbmv.o : $(mySRCdir)/ATL_f77gbmv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gbmv.c
ATL_cf77dotu_sub.o : $(mySRCdir)/ATL_f77dotu_sub.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77dotu_sub.c
ATL_cf77dotc_sub.o : $(mySRCdir)/ATL_f77dotc_sub.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77dotc_sub.c
ATL_cf77rot.o : $(mySRCdir)/ATL_f77rot.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77rot.c
ATL_cf77swap.o : $(mySRCdir)/ATL_f77swap.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77swap.c
ATL_cf77copy.o : $(mySRCdir)/ATL_f77copy.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77copy.c
ATL_cf77axpy.o : $(mySRCdir)/ATL_f77axpy.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77axpy.c
ATL_cf77scal.o : $(mySRCdir)/ATL_f77scal.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77scal.c
ATL_cf77amax.o : $(mySRCdir)/ATL_f77amax.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77amax.c
ATL_cf77asum.o : $(mySRCdir)/ATL_f77asum.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77asum.c
ATL_cf77nrm2.o : $(mySRCdir)/ATL_f77nrm2.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77nrm2.c
ATL_cf77rotg.o : $(mySRCdir)/ATL_f77rotg.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77rotg.c
ATL_cf77gesv.o : $(mySRCdir)/ATL_f77gesv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gesv.c
ATL_cf77gels.o : $(mySRCdir)/ATL_f77gels.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gels.c
ATL_cf77posv.o : $(mySRCdir)/ATL_f77posv.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77posv.c
ATL_cf77lauum.o : $(mySRCdir)/ATL_f77lauum.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77lauum.c
ATL_cf77trtri.o : $(mySRCdir)/ATL_f77trtri.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77trtri.c
ATL_cf77potrf.o : $(mySRCdir)/ATL_f77potrf.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77potrf.c
ATL_cf77getrf.o : $(mySRCdir)/ATL_f77getrf.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77getrf.c
ATL_cf77getri.o : $(mySRCdir)/ATL_f77getri.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77getri.c
ATL_cf77geqlf.o : $(mySRCdir)/ATL_f77geqlf.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77geqlf.c
ATL_cf77gelqf.o : $(mySRCdir)/ATL_f77gelqf.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gelqf.c
ATL_cf77gerqf.o : $(mySRCdir)/ATL_f77gerqf.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77gerqf.c
ATL_cf77geqrf.o : $(mySRCdir)/ATL_f77geqrf.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77geqrf.c
ATL_cvdiff.o : $(mySRCdir)/ATL_vdiff.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_vdiff.c
ATL_cinfnrm.o : $(mySRCdir)/ATL_infnrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_infnrm.c
ATL_cgediffnrm1.o : $(mySRCdir)/ATL_gediffnrm1.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gediffnrm1.c
ATL_cgenrm1.o : $(mySRCdir)/ATL_genrm1.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_genrm1.c
ATL_cepsilon.o : $(mySRCdir)/ATL_epsilon.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_epsilon.c
ATL_cgediff.o : $(mySRCdir)/ATL_gediff.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gediff.c
ATL_cgeprint.o : $(mySRCdir)/ATL_geprint.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_geprint.c
ATL_cgegen.o : $(mySRCdir)/ATL_gegen.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gegen.c
ATL_chediffnrm.o : $(mySRCdir)/ATL_hediffnrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_hediffnrm.c
ATL_ctrgen.o : $(mySRCdir)/ATL_trgen.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_trgen.c
ATL_ctpnrm1.o : $(mySRCdir)/ATL_tpnrm1.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_tpnrm1.c
ATL_ctbnrm1.o : $(mySRCdir)/ATL_tbnrm1.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_tbnrm1.c
ATL_cgbnrm1.o : $(mySRCdir)/ATL_gbnrm1.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gbnrm1.c
ATL_ctrnrm1.o : $(mySRCdir)/ATL_trnrm1.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_trnrm1.c
ATL_ctstsqtran.o : $(mySRCdir)/ATL_tstsqtran.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_tstsqtran.c
ATL_chenrm.o : $(mySRCdir)/ATL_henrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_henrm.c
ATL_chpnrm.o : $(mySRCdir)/ATL_hpnrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_hpnrm.c
ATL_chbnrm.o : $(mySRCdir)/ATL_hbnrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_hbnrm.c
ATL_csynrm.o : $(mySRCdir)/ATL_synrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_synrm.c
ATL_cspnrm.o : $(mySRCdir)/ATL_spnrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_spnrm.c
ATL_csbnrm.o : $(mySRCdir)/ATL_sbnrm.c $(INCdep1)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_sbnrm.c
ATL_csf77scal.o : $(mySRCdir)/ATL_f77rscal.c $(INCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_f77rscal.c
ATL_zf77gemm.o : $(mySRCdir)/ATL_f77gemm.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gemm.c
ATL_zf77trsm.o : $(mySRCdir)/ATL_f77trsm.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77trsm.c
ATL_zf77trmm.o : $(mySRCdir)/ATL_f77trmm.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77trmm.c
ATL_zf77syr2k.o : $(mySRCdir)/ATL_f77syr2k.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77syr2k.c
ATL_zf77syrk.o : $(mySRCdir)/ATL_f77syrk.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77syrk.c
ATL_zf77symm.o : $(mySRCdir)/ATL_f77symm.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77symm.c
ATL_zf77her2k.o : $(mySRCdir)/ATL_f77her2k.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77her2k.c
ATL_zf77herk.o : $(mySRCdir)/ATL_f77herk.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77herk.c
ATL_zf77hemm.o : $(mySRCdir)/ATL_f77hemm.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77hemm.c
ATL_zf77geru.o : $(mySRCdir)/ATL_f77geru.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77geru.c
ATL_zf77gerc.o : $(mySRCdir)/ATL_f77gerc.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gerc.c
ATL_zf77gemv.o : $(mySRCdir)/ATL_f77gemv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gemv.c
ATL_zf77trmv.o : $(mySRCdir)/ATL_f77trmv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77trmv.c
ATL_zf77trsv.o : $(mySRCdir)/ATL_f77trsv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77trsv.c
ATL_zf77tpsv.o : $(mySRCdir)/ATL_f77tpsv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77tpsv.c
ATL_zf77tpmv.o : $(mySRCdir)/ATL_f77tpmv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77tpmv.c
ATL_zf77tbsv.o : $(mySRCdir)/ATL_f77tbsv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77tbsv.c
ATL_zf77tbmv.o : $(mySRCdir)/ATL_f77tbmv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77tbmv.c
ATL_zf77hpr2.o : $(mySRCdir)/ATL_f77hpr2.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77hpr2.c
ATL_zf77hpr.o : $(mySRCdir)/ATL_f77hpr.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77hpr.c
ATL_zf77hpmv.o : $(mySRCdir)/ATL_f77hpmv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77hpmv.c
ATL_zf77her2.o : $(mySRCdir)/ATL_f77her2.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77her2.c
ATL_zf77her.o : $(mySRCdir)/ATL_f77her.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77her.c
ATL_zf77hemv.o : $(mySRCdir)/ATL_f77hemv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77hemv.c
ATL_zf77hbmv.o : $(mySRCdir)/ATL_f77hbmv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77hbmv.c
ATL_zf77gbmv.o : $(mySRCdir)/ATL_f77gbmv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gbmv.c
ATL_zf77dotu_sub.o : $(mySRCdir)/ATL_f77dotu_sub.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77dotu_sub.c
ATL_zf77dotc_sub.o : $(mySRCdir)/ATL_f77dotc_sub.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77dotc_sub.c
ATL_zf77rot.o : $(mySRCdir)/ATL_f77rot.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77rot.c
ATL_zf77swap.o : $(mySRCdir)/ATL_f77swap.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77swap.c
ATL_zf77copy.o : $(mySRCdir)/ATL_f77copy.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77copy.c
ATL_zf77axpy.o : $(mySRCdir)/ATL_f77axpy.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77axpy.c
ATL_zf77scal.o : $(mySRCdir)/ATL_f77scal.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77scal.c
ATL_zf77amax.o : $(mySRCdir)/ATL_f77amax.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77amax.c
ATL_zf77asum.o : $(mySRCdir)/ATL_f77asum.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77asum.c
ATL_zf77nrm2.o : $(mySRCdir)/ATL_f77nrm2.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77nrm2.c
ATL_zf77rotg.o : $(mySRCdir)/ATL_f77rotg.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77rotg.c
ATL_zf77gesv.o : $(mySRCdir)/ATL_f77gesv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gesv.c
ATL_zf77gels.o : $(mySRCdir)/ATL_f77gels.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gels.c
ATL_zf77posv.o : $(mySRCdir)/ATL_f77posv.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77posv.c
ATL_zf77lauum.o : $(mySRCdir)/ATL_f77lauum.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77lauum.c
ATL_zf77trtri.o : $(mySRCdir)/ATL_f77trtri.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77trtri.c
ATL_zf77potrf.o : $(mySRCdir)/ATL_f77potrf.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77potrf.c
ATL_zf77getrf.o : $(mySRCdir)/ATL_f77getrf.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77getrf.c
ATL_zf77getri.o : $(mySRCdir)/ATL_f77getri.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77getri.c
ATL_zf77geqlf.o : $(mySRCdir)/ATL_f77geqlf.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77geqlf.c
ATL_zf77gelqf.o : $(mySRCdir)/ATL_f77gelqf.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gelqf.c
ATL_zf77gerqf.o : $(mySRCdir)/ATL_f77gerqf.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77gerqf.c
ATL_zf77geqrf.o : $(mySRCdir)/ATL_f77geqrf.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77geqrf.c
ATL_zvdiff.o : $(mySRCdir)/ATL_vdiff.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_vdiff.c
ATL_zinfnrm.o : $(mySRCdir)/ATL_infnrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_infnrm.c
ATL_zgediffnrm1.o : $(mySRCdir)/ATL_gediffnrm1.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gediffnrm1.c
ATL_zgenrm1.o : $(mySRCdir)/ATL_genrm1.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_genrm1.c
ATL_zepsilon.o : $(mySRCdir)/ATL_epsilon.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_epsilon.c
ATL_zgediff.o : $(mySRCdir)/ATL_gediff.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gediff.c
ATL_zgeprint.o : $(mySRCdir)/ATL_geprint.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_geprint.c
ATL_zgegen.o : $(mySRCdir)/ATL_gegen.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gegen.c
ATL_zhediffnrm.o : $(mySRCdir)/ATL_hediffnrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_hediffnrm.c
ATL_ztrgen.o : $(mySRCdir)/ATL_trgen.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_trgen.c
ATL_ztpnrm1.o : $(mySRCdir)/ATL_tpnrm1.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_tpnrm1.c
ATL_ztbnrm1.o : $(mySRCdir)/ATL_tbnrm1.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_tbnrm1.c
ATL_zgbnrm1.o : $(mySRCdir)/ATL_gbnrm1.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gbnrm1.c
ATL_ztrnrm1.o : $(mySRCdir)/ATL_trnrm1.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_trnrm1.c
ATL_ztstsqtran.o : $(mySRCdir)/ATL_tstsqtran.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_tstsqtran.c
ATL_zhenrm.o : $(mySRCdir)/ATL_henrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_henrm.c
ATL_zhpnrm.o : $(mySRCdir)/ATL_hpnrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_hpnrm.c
ATL_zhbnrm.o : $(mySRCdir)/ATL_hbnrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_hbnrm.c
ATL_zsynrm.o : $(mySRCdir)/ATL_synrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_synrm.c
ATL_zspnrm.o : $(mySRCdir)/ATL_spnrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_spnrm.c
ATL_zsbnrm.o : $(mySRCdir)/ATL_sbnrm.c $(INCdep1)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_sbnrm.c
ATL_zdf77scal.o : $(mySRCdir)/ATL_f77rscal.c $(INCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_f77rscal.c
