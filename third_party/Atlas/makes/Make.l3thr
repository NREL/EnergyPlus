include Make.inc
mySRCdir = $(SRCdir)/src/threads/blas/level3

all : lib
lib : ilib dlib slib zlib clib
double    : dlib
single    : slib
complex   : clib
complex16 : zlib
clean : iclean dclean sclean cclean zclean

deps = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
       $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_tlvl3.h \
       $(INCAdir)/atlas_pthreads.h $(INCSdir)/atlas_threads.h \
       $(INCAdir)/atlas_tsumm.h

iobj = ATL_Xtsyrk.o ATL_Xtsyr2k.o ATL_Xtgemm.o

iclean:
	rm -f $(iobj) ilib.grd
ilib : ilib.grd
ilib.grd : $(iobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(iobj)
	$(RANLIB) $(ATLASlib)
	touch ilib.grd

ATL_Xtgemm.o : $(deps) $(mySRCdir)/ATL_Xtgemm.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/ATL_Xtgemm.c
ATL_Xtsyr2k.o : $(deps) $(mySRCdir)/ATL_Xtsyr2k.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/ATL_Xtsyr2k.c
ATL_Xtsyrk.o : $(deps) $(mySRCdir)/ATL_Xtsyrk.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/ATL_Xtsyrk.c

sINCdep = $(deps) $(INCAdir)/smm.h

sobj = ATL_sthreadMM.o ATL_stgemm_rec.o ATL_stgemm_K.o ATL_stgemm_N.o \
       ATL_stgemm_M.o ATL_stNumGemmThreads.o ATL_stsymm.o ATL_sttrmm.o \
       ATL_sttrsm.o ATL_stsyrk.o ATL_stsyr2k.o ATL_stgemm.o ATL_stgemm_rkK.o \
       ATL_stgemm_bigMN_Kp.o

slib : slib.grd ilib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
sclean:
	rm -f $(sobj) slib.grd

ATL_stgemm_p.o : $(deps) $(mySRCdir)/ATL_tgemm_p.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_p.c
ATL_sthreadMM.o : $(mySRCdir)/ATL_threadMM.c 
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_threadMM.c
ATL_stgemm.o : $(deps) $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm.c
ATL_stsyr2k.o : $(deps) $(mySRCdir)/ATL_tsyr2k.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tsyr2k.c
ATL_stsyrk.o : $(deps) $(mySRCdir)/ATL_tsyrk.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tsyrk.c
ATL_sttrsm.o : $(deps) $(mySRCdir)/ATL_ttrsm.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_ttrsm.c
ATL_sttrmm.o : $(deps) $(mySRCdir)/ATL_ttrmm.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_ttrmm.c
ATL_stsymm.o : $(deps) $(mySRCdir)/ATL_tsymm.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tsymm.c
ATL_stNumGemmThreads.o : $(deps) $(mySRCdir)/ATL_tNumGemmThreads.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tNumGemmThreads.c
ATL_stgemm_M.o : $(deps) $(mySRCdir)/ATL_tgemm_M.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_M.c
ATL_stgemm_N.o : $(deps) $(mySRCdir)/ATL_tgemm_N.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_N.c
ATL_stgemm_K.o : $(deps) $(mySRCdir)/ATL_tgemm_K.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_K.c
ATL_stgemm_rec.o : $(deps) $(mySRCdir)/ATL_tgemm_rec.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_rec.c
ATL_stgemm_rkK.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_rkK.c
ATL_stgemm_rkK_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK_Np.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_rkK_Np.c
ATL_stgemm_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_Kp.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_Kp.c
ATL_stgemm_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_Np.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_Np.c
ATL_stgemm_Mp.o : $(deps) $(mySRCdir)/ATL_tgemm_Mp.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_Mp.c
ATL_stgemm_MKp.o : $(deps) $(mySRCdir)/ATL_tgemm_MKp.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_MKp.c
ATL_stgemm_bigMN_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
ATL_stgemm_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DSREAL -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm.c
ATL_stgemm_bigMN_Kp_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DSREAL -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c


dINCdep = $(deps) $(INCAdir)/dmm.h

dobj = ATL_dthreadMM.o ATL_dtgemm_rec.o ATL_dtgemm_K.o ATL_dtgemm_N.o \
       ATL_dtgemm_M.o ATL_dtNumGemmThreads.o ATL_dtsymm.o ATL_dttrmm.o \
       ATL_dttrsm.o ATL_dtsyrk.o ATL_dtsyr2k.o ATL_dtgemm.o ATL_dtgemm_rkK.o \
       ATL_dtgemm_bigMN_Kp.o

dlib : dlib.grd ilib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
dclean:
	rm -f $(dobj) dlib.grd

ATL_dtgemm_p.o : $(deps) $(mySRCdir)/ATL_tgemm_p.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_p.c
ATL_dthreadMM.o : $(mySRCdir)/ATL_threadMM.c 
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_threadMM.c
ATL_dtgemm.o : $(deps) $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm.c
ATL_dtsyr2k.o : $(deps) $(mySRCdir)/ATL_tsyr2k.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tsyr2k.c
ATL_dtsyrk.o : $(deps) $(mySRCdir)/ATL_tsyrk.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tsyrk.c
ATL_dttrsm.o : $(deps) $(mySRCdir)/ATL_ttrsm.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_ttrsm.c
ATL_dttrmm.o : $(deps) $(mySRCdir)/ATL_ttrmm.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_ttrmm.c
ATL_dtsymm.o : $(deps) $(mySRCdir)/ATL_tsymm.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tsymm.c
ATL_dtNumGemmThreads.o : $(deps) $(mySRCdir)/ATL_tNumGemmThreads.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tNumGemmThreads.c
ATL_dtgemm_M.o : $(deps) $(mySRCdir)/ATL_tgemm_M.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_M.c
ATL_dtgemm_N.o : $(deps) $(mySRCdir)/ATL_tgemm_N.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_N.c
ATL_dtgemm_K.o : $(deps) $(mySRCdir)/ATL_tgemm_K.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_K.c
ATL_dtgemm_rec.o : $(deps) $(mySRCdir)/ATL_tgemm_rec.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_rec.c
ATL_dtgemm_rkK.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_rkK.c
ATL_dtgemm_rkK_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK_Np.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_rkK_Np.c
ATL_dtgemm_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_Kp.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_Kp.c
ATL_dtgemm_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_Np.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_Np.c
ATL_dtgemm_Mp.o : $(deps) $(mySRCdir)/ATL_tgemm_Mp.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_Mp.c
ATL_dtgemm_MKp.o : $(deps) $(mySRCdir)/ATL_tgemm_MKp.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_MKp.c
ATL_dtgemm_bigMN_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
ATL_dtgemm_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DDREAL -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm.c
ATL_dtgemm_bigMN_Kp_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DDREAL -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c


cINCdep = $(deps) $(INCAdir)/cmm.h

cobj = ATL_cthreadMM.o ATL_ctgemm_rec.o ATL_ctgemm_K.o ATL_ctgemm_N.o \
       ATL_ctgemm_M.o ATL_ctNumGemmThreads.o ATL_ctsymm.o ATL_cttrmm.o \
       ATL_cttrsm.o ATL_ctsyrk.o ATL_ctsyr2k.o ATL_ctgemm.o ATL_ctherk.o \
       ATL_cthemm.o ATL_cther2k.o ATL_ctgemm_rkK.o ATL_ctgemm_bigMN_Kp.o

clib : clib.grd ilib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
cclean:
	rm -f $(cobj) clib.grd

ATL_cther2k.o : $(deps) $(mySRCdir)/ATL_ther2k.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_ther2k.c
ATL_cthemm.o : $(deps) $(mySRCdir)/ATL_themm.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_themm.c
ATL_ctherk.o : $(deps) $(mySRCdir)/ATL_therk.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_therk.c
ATL_ctgemm_p.o : $(deps) $(mySRCdir)/ATL_tgemm_p.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_p.c
ATL_cthreadMM.o : $(mySRCdir)/ATL_threadMM.c 
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_threadMM.c
ATL_ctgemm.o : $(deps) $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm.c
ATL_ctsyr2k.o : $(deps) $(mySRCdir)/ATL_tsyr2k.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tsyr2k.c
ATL_ctsyrk.o : $(deps) $(mySRCdir)/ATL_tsyrk.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tsyrk.c
ATL_cttrsm.o : $(deps) $(mySRCdir)/ATL_ttrsm.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_ttrsm.c
ATL_cttrmm.o : $(deps) $(mySRCdir)/ATL_ttrmm.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_ttrmm.c
ATL_ctsymm.o : $(deps) $(mySRCdir)/ATL_tsymm.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tsymm.c
ATL_ctNumGemmThreads.o : $(deps) $(mySRCdir)/ATL_tNumGemmThreads.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tNumGemmThreads.c
ATL_ctgemm_M.o : $(deps) $(mySRCdir)/ATL_tgemm_M.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_M.c
ATL_ctgemm_N.o : $(deps) $(mySRCdir)/ATL_tgemm_N.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_N.c
ATL_ctgemm_K.o : $(deps) $(mySRCdir)/ATL_tgemm_K.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_K.c
ATL_ctgemm_rec.o : $(deps) $(mySRCdir)/ATL_tgemm_rec.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_rec.c
ATL_ctgemm_rkK.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_rkK.c
ATL_ctgemm_rkK_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK_Np.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_rkK_Np.c
ATL_ctgemm_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_Kp.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_Kp.c
ATL_ctgemm_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_Np.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_Np.c
ATL_ctgemm_Mp.o : $(deps) $(mySRCdir)/ATL_tgemm_Mp.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_Mp.c
ATL_ctgemm_MKp.o : $(deps) $(mySRCdir)/ATL_tgemm_MKp.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_MKp.c
ATL_ctgemm_bigMN_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
ATL_ctgemm_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DSCPLX -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm.c
ATL_ctgemm_bigMN_Kp_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DSCPLX -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c


zINCdep = $(deps) $(INCAdir)/zmm.h

zobj = ATL_zthreadMM.o ATL_ztgemm_rec.o ATL_ztgemm_K.o ATL_ztgemm_N.o \
       ATL_ztgemm_M.o ATL_ztNumGemmThreads.o ATL_ztsymm.o ATL_zttrmm.o \
       ATL_zttrsm.o ATL_ztsyrk.o ATL_ztsyr2k.o ATL_ztgemm.o ATL_ztherk.o \
       ATL_zthemm.o ATL_zther2k.o ATL_ztgemm_rkK.o ATL_ztgemm_bigMN_Kp.o

zlib : zlib.grd ilib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
zclean:
	rm -f $(zobj) zlib.grd

ATL_zther2k.o : $(deps) $(mySRCdir)/ATL_ther2k.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_ther2k.c
ATL_zthemm.o : $(deps) $(mySRCdir)/ATL_themm.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_themm.c
ATL_ztherk.o : $(deps) $(mySRCdir)/ATL_therk.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_therk.c
ATL_ztgemm_p.o : $(deps) $(mySRCdir)/ATL_tgemm_p.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_p.c
ATL_zthreadMM.o : $(mySRCdir)/ATL_threadMM.c 
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_threadMM.c
ATL_ztgemm.o : $(deps) $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm.c
ATL_ztsyr2k.o : $(deps) $(mySRCdir)/ATL_tsyr2k.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tsyr2k.c
ATL_ztsyrk.o : $(deps) $(mySRCdir)/ATL_tsyrk.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tsyrk.c
ATL_zttrsm.o : $(deps) $(mySRCdir)/ATL_ttrsm.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_ttrsm.c
ATL_zttrmm.o : $(deps) $(mySRCdir)/ATL_ttrmm.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_ttrmm.c
ATL_ztsymm.o : $(deps) $(mySRCdir)/ATL_tsymm.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tsymm.c
ATL_ztNumGemmThreads.o : $(deps) $(mySRCdir)/ATL_tNumGemmThreads.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tNumGemmThreads.c
ATL_ztgemm_M.o : $(deps) $(mySRCdir)/ATL_tgemm_M.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_M.c
ATL_ztgemm_N.o : $(deps) $(mySRCdir)/ATL_tgemm_N.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_N.c
ATL_ztgemm_K.o : $(deps) $(mySRCdir)/ATL_tgemm_K.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_K.c
ATL_ztgemm_rec.o : $(deps) $(mySRCdir)/ATL_tgemm_rec.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_rec.c
ATL_ztgemm_rkK.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_rkK.c
ATL_ztgemm_rkK_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_rkK_Np.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_rkK_Np.c
ATL_ztgemm_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_Kp.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_Kp.c
ATL_ztgemm_Np.o : $(deps) $(mySRCdir)/ATL_tgemm_Np.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_Np.c
ATL_ztgemm_Mp.o : $(deps) $(mySRCdir)/ATL_tgemm_Mp.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_Mp.c
ATL_ztgemm_MKp.o : $(deps) $(mySRCdir)/ATL_tgemm_MKp.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_MKp.c
ATL_ztgemm_bigMN_Kp.o : $(deps) $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
ATL_ztgemm_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm.c
	$(KC) $(KCFLAGS) -DDCPLX -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm.c
ATL_ztgemm_bigMN_Kp_CE.o : $(deps) slib $(mySRCdir)/ATL_tgemm_bigMN_Kp.c
	$(KC) $(KCFLAGS) -DDCPLX -DFindingCE -o $@ \
               -c $(mySRCdir)/ATL_tgemm_bigMN_Kp.c



