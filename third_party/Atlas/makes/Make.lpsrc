include Make.inc
mySRCdir = $(SRCdir)/src/lapack

all : lib ptlib
lib : dlib slib zlib clib
clean : sclean dclean cclean zclean

tlib : ptlib
ptlib : dtlib stlib ztlib ctlib
slib : 
	$(PMAKE) slib.grd
stlib : 
	$(PMAKE) stlib.grd
	cd $(BLDdir)/src/threads/lapack ; $(PMAKE) stlib
dlib : 
	$(PMAKE) dlib.grd
dtlib : 
	$(PMAKE) dtlib.grd
	cd $(BLDdir)/src/threads/lapack ; $(PMAKE) dtlib
clib : 
	$(PMAKE) clib.grd
ctlib : 
	$(PMAKE) ctlib.grd
	cd $(BLDdir)/src/threads/lapack ; $(PMAKE) ctlib
zlib : 
	$(PMAKE) zlib.grd
ztlib : 
	$(PMAKE) ztlib.grd
	cd $(BLDdir)/src/threads/lapack ; $(PMAKE) ztlib
	
stobj = ATL_itlaenv.o ATL_sgelq2.o ATL_sgeql2.o ATL_sgeqr2.o ATL_sgerq2.o \
        ATL_sgetf2.o ATL_sgetrfR.o ATL_sgetri.o ATL_sgetriC.o ATL_sgetriR.o \
        ATL_sgetrs.o ATL_slamch.o ATL_slapy2.o ATL_slarf.o ATL_slarfb.o \
        ATL_slarfg.o ATL_slarft.o ATL_slascl.o ATL_slaswp.o ATL_slauum.o \
        ATL_slauumCL.o ATL_slauumCU.o ATL_slauumRL.o ATL_slauumRU.o \
        ATL_sormlq.o ATL_sormql.o ATL_sormqr.o ATL_sormrq.o ATL_spotrf.o \
        ATL_spotrfL.o ATL_spotrfU.o ATL_spotrs.o ATL_stgelqf.o ATL_stgelqr.o \
        ATL_stgels.o ATL_stgeqlf.o ATL_stgeqlr.o ATL_stgeqrf.o ATL_stgeqrr.o \
        ATL_stgerqf.o ATL_stgerqr.o ATL_stgetrf.o ATL_stgetrfC.o ATL_strtri.o \
        ATL_strtriCL.o ATL_strtriCU.o ATL_strtriRL.o ATL_strtriRU.o \
        ATL_strtrs.o

incdep = $(INCSdir)/atlas_lapack.h
stlib.grd : $(stobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(stobj)
	$(RANLIB) $(PTLAPACKlib)
	touch stlib.grd

   
ATL_stlamch.o : $(mySRCdir)/ATL_lamch.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlamch.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lamch.c
ATL_sttrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sttrtrs.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtrs.c
ATL_stgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgels.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gels.c
ATL_stlascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlascl.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lascl.c
ATL_stlapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlapy2.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lapy2.c
ATL_stlarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlarfgp.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfgp.c
ATL_stlarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlarft.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larft.c
ATL_stlarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlarfg.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfg.c
ATL_stlarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlarf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larf.c
ATL_stlarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlarfb.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfb.c
ATL_stormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stormlq.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormlq.c
ATL_stormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stormql.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormql.c
ATL_stormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stormrq.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormrq.c
ATL_stormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stormqr.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormqr.c
ATL_stgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgelqf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqf.c
ATL_stgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgelq2.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelq2.c
ATL_stgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgeqlf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlf.c
ATL_stgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgeql2.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geql2.c
ATL_stgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgerqf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqf.c
ATL_stgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgerq2.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerq2.c
ATL_stgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgeqrf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrf.c
ATL_stgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgeqr2.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqr2.c
ATL_stpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stpotrfL.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfL.c
ATL_stpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stpotrfU.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfU.c
ATL_stpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stpotrs.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrs.c
ATL_stpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stpotrf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrf.c
ATL_stgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetf2.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getf2.c
ATL_stgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetrfR.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfR.c
ATL_stgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetrfC.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfC.c
ATL_stgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetrs.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrs.c
ATL_stgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetrf.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf.c
ATL_stlaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlaswp.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_laswp.c
ATL_stgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetriC.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriC.c
ATL_stgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetriR.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriR.c
ATL_stgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetri.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getri.c
ATL_stlauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlauum.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauum.c
ATL_sttrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sttrtriRL.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRL.c
ATL_sttrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sttrtriRU.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRU.c
ATL_sttrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sttrtriCL.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCL.c
ATL_sttrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sttrtriCU.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCU.c
ATL_sttrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sttrtri.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtri.c
ATL_stgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_stgeqrr.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrr.c
ATL_stgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_stgerqr.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqr.c
ATL_stgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_stgeqlr.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlr.c
ATL_stgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_stgelqr.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqr.c
ATL_stlauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlauumCL.o -DSREAL -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_stlauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlauumRL.o -DSREAL -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_stlauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlauumCU.o -DSREAL -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c
ATL_stlauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stlauumRU.o -DSREAL -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c

ATL_stgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_stgetrf4.o -DSREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) ATL_stgetrf4.o
	$(RANLIB) $(PTLAPACKlib)
	

stclean :
	rm -f $(stobj)
dtobj = ATL_dgelq2.o ATL_dgeql2.o ATL_dgeqr2.o ATL_dgerq2.o ATL_dgetf2.o \
        ATL_dgetrfR.o ATL_dgetri.o ATL_dgetriC.o ATL_dgetriR.o ATL_dgetrs.o \
        ATL_dlamch.o ATL_dlapy2.o ATL_dlarf.o ATL_dlarfb.o ATL_dlarfg.o \
        ATL_dlarft.o ATL_dlascl.o ATL_dlaswp.o ATL_dlauum.o ATL_dlauumCL.o \
        ATL_dlauumCU.o ATL_dlauumRL.o ATL_dlauumRU.o ATL_dormlq.o \
        ATL_dormql.o ATL_dormqr.o ATL_dormrq.o ATL_dpotrf.o ATL_dpotrfL.o \
        ATL_dpotrfU.o ATL_dpotrs.o ATL_dtgelqf.o ATL_dtgelqr.o ATL_dtgels.o \
        ATL_dtgeqlf.o ATL_dtgeqlr.o ATL_dtgeqrf.o ATL_dtgeqrr.o ATL_dtgerqf.o \
        ATL_dtgerqr.o ATL_dtgetrf.o ATL_dtgetrfC.o ATL_dtrtri.o \
        ATL_dtrtriCL.o ATL_dtrtriCU.o ATL_dtrtriRL.o ATL_dtrtriRU.o \
        ATL_dtrtrs.o ATL_itlaenv.o

incdep = $(INCSdir)/atlas_lapack.h
dtlib.grd : $(dtobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(dtobj)
	$(RANLIB) $(PTLAPACKlib)
	touch dtlib.grd

   
ATL_dtlamch.o : $(mySRCdir)/ATL_lamch.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlamch.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lamch.c
ATL_dttrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dttrtrs.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtrs.c
ATL_dtgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgels.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gels.c
ATL_dtlascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlascl.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lascl.c
ATL_dtlapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlapy2.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lapy2.c
ATL_dtlarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlarfgp.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfgp.c
ATL_dtlarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlarft.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larft.c
ATL_dtlarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlarfg.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfg.c
ATL_dtlarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlarf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larf.c
ATL_dtlarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlarfb.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfb.c
ATL_dtormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtormlq.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormlq.c
ATL_dtormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtormql.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormql.c
ATL_dtormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtormrq.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormrq.c
ATL_dtormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtormqr.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormqr.c
ATL_dtgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgelqf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqf.c
ATL_dtgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgelq2.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelq2.c
ATL_dtgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgeqlf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlf.c
ATL_dtgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgeql2.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geql2.c
ATL_dtgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgerqf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqf.c
ATL_dtgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgerq2.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerq2.c
ATL_dtgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgeqrf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrf.c
ATL_dtgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgeqr2.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqr2.c
ATL_dtpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtpotrfL.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfL.c
ATL_dtpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtpotrfU.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfU.c
ATL_dtpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtpotrs.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrs.c
ATL_dtpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtpotrf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrf.c
ATL_dtgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetf2.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getf2.c
ATL_dtgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetrfR.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfR.c
ATL_dtgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetrfC.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfC.c
ATL_dtgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetrs.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrs.c
ATL_dtgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetrf.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf.c
ATL_dtlaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlaswp.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_laswp.c
ATL_dtgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetriC.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriC.c
ATL_dtgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetriR.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriR.c
ATL_dtgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetri.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getri.c
ATL_dtlauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlauum.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauum.c
ATL_dttrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dttrtriRL.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRL.c
ATL_dttrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dttrtriRU.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRU.c
ATL_dttrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dttrtriCL.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCL.c
ATL_dttrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dttrtriCU.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCU.c
ATL_dttrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dttrtri.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtri.c
ATL_dtgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgeqrr.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrr.c
ATL_dtgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgerqr.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqr.c
ATL_dtgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgeqlr.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlr.c
ATL_dtgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgelqr.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqr.c
ATL_dtlauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlauumCL.o -DDREAL -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_dtlauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlauumRL.o -DDREAL -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_dtlauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlauumCU.o -DDREAL -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c
ATL_dtlauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtlauumRU.o -DDREAL -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c

ATL_dtgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtgetrf4.o -DDREAL \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) ATL_dtgetrf4.o
	$(RANLIB) $(PTLAPACKlib)
	

dtclean :
	rm -f $(dtobj)
ctobj = ATL_cgelq2.o ATL_cgeql2.o ATL_cgeqr2.o ATL_cgerq2.o ATL_cgetf2.o \
        ATL_cgetrfR.o ATL_cgetri.o ATL_cgetriC.o ATL_cgetriR.o ATL_cgetrs.o \
        ATL_clacgv.o ATL_cladiv.o ATL_clapy2.o ATL_clapy3.o ATL_clarf.o \
        ATL_clarfb.o ATL_clarfg.o ATL_clarft.o ATL_clascl.o ATL_claswp.o \
        ATL_clauum.o ATL_clauumCL.o ATL_clauumCU.o ATL_clauumRL.o \
        ATL_clauumRU.o ATL_cormlq.o ATL_cormql.o ATL_cormqr.o ATL_cormrq.o \
        ATL_cpotrf.o ATL_cpotrfL.o ATL_cpotrfRL.o ATL_cpotrfRU.o \
        ATL_cpotrfU.o ATL_cpotrs.o ATL_ctgelqf.o ATL_ctgelqr.o ATL_ctgels.o \
        ATL_ctgeqlf.o ATL_ctgeqlr.o ATL_ctgeqrf.o ATL_ctgeqrr.o ATL_ctgerqf.o \
        ATL_ctgerqr.o ATL_ctgetrf.o ATL_ctgetrfC.o ATL_ctrtri.o \
        ATL_ctrtriCL.o ATL_ctrtriCU.o ATL_ctrtriRL.o ATL_ctrtriRU.o \
        ATL_ctrtrs.o ATL_itlaenv.o

incdep = $(INCSdir)/atlas_lapack.h
ctlib.grd : $(ctobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(ctobj)
	$(RANLIB) $(PTLAPACKlib)
	touch ctlib.grd

   
ATL_ctlacgv.o : $(mySRCdir)/ATL_lacgv.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlacgv.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lacgv.c
ATL_ctlapy3.o : $(mySRCdir)/ATL_lapy3.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlapy3.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lapy3.c
ATL_ctladiv.o : $(mySRCdir)/ATL_ladiv.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctladiv.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ladiv.c
ATL_ctpotrfRU.o : $(mySRCdir)/ATL_potrfRU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctpotrfRU.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfRU.c
ATL_ctpotrfRL.o : $(mySRCdir)/ATL_potrfRL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctpotrfRL.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfRL.c
ATL_cttrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cttrtrs.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtrs.c
ATL_ctgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgels.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gels.c
ATL_ctlascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlascl.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lascl.c
ATL_ctlapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlapy2.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lapy2.c
ATL_ctlarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlarfgp.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfgp.c
ATL_ctlarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlarft.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larft.c
ATL_ctlarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlarfg.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfg.c
ATL_ctlarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlarf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larf.c
ATL_ctlarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlarfb.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfb.c
ATL_ctormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctormlq.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormlq.c
ATL_ctormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctormql.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormql.c
ATL_ctormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctormrq.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormrq.c
ATL_ctormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctormqr.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormqr.c
ATL_ctgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgelqf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqf.c
ATL_ctgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgelq2.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelq2.c
ATL_ctgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgeqlf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlf.c
ATL_ctgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgeql2.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geql2.c
ATL_ctgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgerqf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqf.c
ATL_ctgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgerq2.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerq2.c
ATL_ctgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgeqrf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrf.c
ATL_ctgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgeqr2.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqr2.c
ATL_ctpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctpotrfL.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfL.c
ATL_ctpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctpotrfU.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfU.c
ATL_ctpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctpotrs.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrs.c
ATL_ctpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctpotrf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrf.c
ATL_ctgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetf2.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getf2.c
ATL_ctgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetrfR.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfR.c
ATL_ctgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetrfC.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfC.c
ATL_ctgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetrs.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrs.c
ATL_ctgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetrf.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf.c
ATL_ctlaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlaswp.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_laswp.c
ATL_ctgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetriC.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriC.c
ATL_ctgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetriR.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriR.c
ATL_ctgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetri.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getri.c
ATL_ctlauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlauum.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauum.c
ATL_cttrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cttrtriRL.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRL.c
ATL_cttrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cttrtriRU.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRU.c
ATL_cttrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cttrtriCL.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCL.c
ATL_cttrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cttrtriCU.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCU.c
ATL_cttrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cttrtri.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtri.c
ATL_ctgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgeqrr.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrr.c
ATL_ctgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgerqr.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqr.c
ATL_ctgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgeqlr.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlr.c
ATL_ctgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgelqr.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqr.c
ATL_ctlauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlauumCL.o -DSCPLX -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_ctlauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlauumRL.o -DSCPLX -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_ctlauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlauumCU.o -DSCPLX -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c
ATL_ctlauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctlauumRU.o -DSCPLX -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c

ATL_ctgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctgetrf4.o -DSCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) ATL_ctgetrf4.o
	$(RANLIB) $(PTLAPACKlib)
	

ctclean :
	rm -f $(ctobj)
ztobj = ATL_itlaenv.o ATL_zgelq2.o ATL_zgeql2.o ATL_zgeqr2.o ATL_zgerq2.o \
        ATL_zgetf2.o ATL_zgetrfR.o ATL_zgetri.o ATL_zgetriC.o ATL_zgetriR.o \
        ATL_zgetrs.o ATL_zlacgv.o ATL_zladiv.o ATL_zlapy2.o ATL_zlapy3.o \
        ATL_zlarf.o ATL_zlarfb.o ATL_zlarfg.o ATL_zlarft.o ATL_zlascl.o \
        ATL_zlaswp.o ATL_zlauum.o ATL_zlauumCL.o ATL_zlauumCU.o \
        ATL_zlauumRL.o ATL_zlauumRU.o ATL_zormlq.o ATL_zormql.o ATL_zormqr.o \
        ATL_zormrq.o ATL_zpotrf.o ATL_zpotrfL.o ATL_zpotrfRL.o ATL_zpotrfRU.o \
        ATL_zpotrfU.o ATL_zpotrs.o ATL_ztgelqf.o ATL_ztgelqr.o ATL_ztgels.o \
        ATL_ztgeqlf.o ATL_ztgeqlr.o ATL_ztgeqrf.o ATL_ztgeqrr.o ATL_ztgerqf.o \
        ATL_ztgerqr.o ATL_ztgetrf.o ATL_ztgetrfC.o ATL_ztrtri.o \
        ATL_ztrtriCL.o ATL_ztrtriCU.o ATL_ztrtriRL.o ATL_ztrtriRU.o \
        ATL_ztrtrs.o

incdep = $(INCSdir)/atlas_lapack.h
ztlib.grd : $(ztobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(ztobj)
	$(RANLIB) $(PTLAPACKlib)
	touch ztlib.grd

   
ATL_ztlacgv.o : $(mySRCdir)/ATL_lacgv.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlacgv.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lacgv.c
ATL_ztlapy3.o : $(mySRCdir)/ATL_lapy3.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlapy3.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lapy3.c
ATL_ztladiv.o : $(mySRCdir)/ATL_ladiv.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztladiv.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ladiv.c
ATL_ztpotrfRU.o : $(mySRCdir)/ATL_potrfRU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztpotrfRU.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfRU.c
ATL_ztpotrfRL.o : $(mySRCdir)/ATL_potrfRL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztpotrfRL.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfRL.c
ATL_zttrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zttrtrs.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtrs.c
ATL_ztgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgels.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gels.c
ATL_ztlascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlascl.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lascl.c
ATL_ztlapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlapy2.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lapy2.c
ATL_ztlarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlarfgp.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfgp.c
ATL_ztlarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlarft.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larft.c
ATL_ztlarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlarfg.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfg.c
ATL_ztlarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlarf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larf.c
ATL_ztlarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlarfb.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_larfb.c
ATL_ztormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztormlq.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormlq.c
ATL_ztormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztormql.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormql.c
ATL_ztormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztormrq.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormrq.c
ATL_ztormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztormqr.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_ormqr.c
ATL_ztgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgelqf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqf.c
ATL_ztgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgelq2.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelq2.c
ATL_ztgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgeqlf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlf.c
ATL_ztgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgeql2.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geql2.c
ATL_ztgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgerqf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqf.c
ATL_ztgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgerq2.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerq2.c
ATL_ztgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgeqrf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrf.c
ATL_ztgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgeqr2.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqr2.c
ATL_ztpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztpotrfL.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfL.c
ATL_ztpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztpotrfU.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrfU.c
ATL_ztpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztpotrs.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrs.c
ATL_ztpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztpotrf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_potrf.c
ATL_ztgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetf2.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getf2.c
ATL_ztgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetrfR.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfR.c
ATL_ztgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetrfC.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrfC.c
ATL_ztgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetrs.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrs.c
ATL_ztgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetrf.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf.c
ATL_ztlaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlaswp.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_laswp.c
ATL_ztgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetriC.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriC.c
ATL_ztgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetriR.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getriR.c
ATL_ztgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetri.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getri.c
ATL_ztlauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlauum.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauum.c
ATL_zttrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zttrtriRL.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRL.c
ATL_zttrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zttrtriRU.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriRU.c
ATL_zttrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zttrtriCL.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCL.c
ATL_zttrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zttrtriCU.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtriCU.c
ATL_zttrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zttrtri.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_trtri.c
ATL_ztgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgeqrr.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqrr.c
ATL_ztgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgerqr.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gerqr.c
ATL_ztgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgeqlr.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_geqlr.c
ATL_ztgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgelqr.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_gelqr.c
ATL_ztlauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlauumCL.o -DDCPLX -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_ztlauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlauumRL.o -DDCPLX -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumL.c
ATL_ztlauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlauumCU.o -DDCPLX -DColMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c
ATL_ztlauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztlauumRU.o -DDCPLX -DRowMajor_ \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_lauumU.c

ATL_ztgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztgetrf4.o -DDCPLX \
              -DATL_USEPTHREADS $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) ATL_ztgetrf4.o
	$(RANLIB) $(PTLAPACKlib)
	

ztclean :
	rm -f $(ztobj)
	
sobj = ATL_ilaenv.o ATL_sgelq2.o ATL_sgelqf.o ATL_sgelqr.o ATL_sgels.o \
       ATL_sgeql2.o ATL_sgeqlf.o ATL_sgeqlr.o ATL_sgeqr2.o ATL_sgeqrf.o \
       ATL_sgeqrr.o ATL_sgerq2.o ATL_sgerqf.o ATL_sgerqr.o ATL_sgetf2.o \
       ATL_sgetrf.o ATL_sgetrfC.o ATL_sgetrfR.o ATL_sgetri.o ATL_sgetriC.o \
       ATL_sgetriR.o ATL_sgetrs.o ATL_slamch.o ATL_slapy2.o ATL_slarf.o \
       ATL_slarfb.o ATL_slarfg.o ATL_slarft.o ATL_slascl.o ATL_slaswp.o \
       ATL_slauum.o ATL_slauumCL.o ATL_slauumCU.o ATL_slauumRL.o \
       ATL_slauumRU.o ATL_sormlq.o ATL_sormql.o ATL_sormqr.o ATL_sormrq.o \
       ATL_spotrf.o ATL_spotrfL.o ATL_spotrfU.o ATL_spotrs.o ATL_strtri.o \
       ATL_strtriCL.o ATL_strtriCU.o ATL_strtriRL.o ATL_strtriRU.o \
       ATL_strtrs.o

incdep = $(INCSdir)/atlas_lapack.h
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(sobj)
	$(RANLIB) $(LAPACKlib)
	touch slib.grd

   
ATL_slamch.o : $(mySRCdir)/ATL_lamch.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slamch.o -DSREAL \
               $(mySRCdir)/ATL_lamch.c
ATL_strtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_strtrs.o -DSREAL \
               $(mySRCdir)/ATL_trtrs.c
ATL_sgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgels.o -DSREAL \
               $(mySRCdir)/ATL_gels.c
ATL_slascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slascl.o -DSREAL \
               $(mySRCdir)/ATL_lascl.c
ATL_slapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slapy2.o -DSREAL \
               $(mySRCdir)/ATL_lapy2.c
ATL_slarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slarfgp.o -DSREAL \
               $(mySRCdir)/ATL_larfgp.c
ATL_slarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slarft.o -DSREAL \
               $(mySRCdir)/ATL_larft.c
ATL_slarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slarfg.o -DSREAL \
               $(mySRCdir)/ATL_larfg.c
ATL_slarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slarf.o -DSREAL \
               $(mySRCdir)/ATL_larf.c
ATL_slarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slarfb.o -DSREAL \
               $(mySRCdir)/ATL_larfb.c
ATL_sormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sormlq.o -DSREAL \
               $(mySRCdir)/ATL_ormlq.c
ATL_sormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sormql.o -DSREAL \
               $(mySRCdir)/ATL_ormql.c
ATL_sormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sormrq.o -DSREAL \
               $(mySRCdir)/ATL_ormrq.c
ATL_sormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sormqr.o -DSREAL \
               $(mySRCdir)/ATL_ormqr.c
ATL_sgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgelqf.o -DSREAL \
               $(mySRCdir)/ATL_gelqf.c
ATL_sgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgelq2.o -DSREAL \
               $(mySRCdir)/ATL_gelq2.c
ATL_sgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgeqlf.o -DSREAL \
               $(mySRCdir)/ATL_geqlf.c
ATL_sgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgeql2.o -DSREAL \
               $(mySRCdir)/ATL_geql2.c
ATL_sgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgerqf.o -DSREAL \
               $(mySRCdir)/ATL_gerqf.c
ATL_sgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgerq2.o -DSREAL \
               $(mySRCdir)/ATL_gerq2.c
ATL_sgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgeqrf.o -DSREAL \
               $(mySRCdir)/ATL_geqrf.c
ATL_sgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgeqr2.o -DSREAL \
               $(mySRCdir)/ATL_geqr2.c
ATL_spotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spotrfL.o -DSREAL \
               $(mySRCdir)/ATL_potrfL.c
ATL_spotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spotrfU.o -DSREAL \
               $(mySRCdir)/ATL_potrfU.c
ATL_spotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spotrs.o -DSREAL \
               $(mySRCdir)/ATL_potrs.c
ATL_spotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spotrf.o -DSREAL \
               $(mySRCdir)/ATL_potrf.c
ATL_sgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetf2.o -DSREAL \
               $(mySRCdir)/ATL_getf2.c
ATL_sgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetrfR.o -DSREAL \
               $(mySRCdir)/ATL_getrfR.c
ATL_sgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetrfC.o -DSREAL \
               $(mySRCdir)/ATL_getrfC.c
ATL_sgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetrs.o -DSREAL \
               $(mySRCdir)/ATL_getrs.c
ATL_sgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetrf.o -DSREAL \
               $(mySRCdir)/ATL_getrf.c
ATL_slaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slaswp.o -DSREAL \
               $(mySRCdir)/ATL_laswp.c
ATL_sgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetriC.o -DSREAL \
               $(mySRCdir)/ATL_getriC.c
ATL_sgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetriR.o -DSREAL \
               $(mySRCdir)/ATL_getriR.c
ATL_sgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetri.o -DSREAL \
               $(mySRCdir)/ATL_getri.c
ATL_slauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slauum.o -DSREAL \
               $(mySRCdir)/ATL_lauum.c
ATL_strtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_strtriRL.o -DSREAL \
               $(mySRCdir)/ATL_trtriRL.c
ATL_strtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_strtriRU.o -DSREAL \
               $(mySRCdir)/ATL_trtriRU.c
ATL_strtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_strtriCL.o -DSREAL \
               $(mySRCdir)/ATL_trtriCL.c
ATL_strtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_strtriCU.o -DSREAL \
               $(mySRCdir)/ATL_trtriCU.c
ATL_strtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_strtri.o -DSREAL \
               $(mySRCdir)/ATL_trtri.c
ATL_sgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_sgeqrr.o -DSREAL \
               $(mySRCdir)/ATL_geqrr.c
ATL_sgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_sgerqr.o -DSREAL \
               $(mySRCdir)/ATL_gerqr.c
ATL_sgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_sgeqlr.o -DSREAL \
               $(mySRCdir)/ATL_geqlr.c
ATL_sgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(SKC) $(sKCFLAGS) -c -o ATL_sgelqr.o -DSREAL \
               $(mySRCdir)/ATL_gelqr.c
ATL_slauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slauumCL.o -DSREAL -DColMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_slauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slauumRL.o -DSREAL -DRowMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_slauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slauumCU.o -DSREAL -DColMajor_ \
               $(mySRCdir)/ATL_lauumU.c
ATL_slauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_slauumRU.o -DSREAL -DRowMajor_ \
               $(mySRCdir)/ATL_lauumU.c

ATL_sgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgetrf4.o -DSREAL \
               $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) ATL_sgetrf4.o
	$(RANLIB) $(LAPACKlib)
	

sclean :
	rm -f $(sobj)
dobj = ATL_dgelq2.o ATL_dgelqf.o ATL_dgelqr.o ATL_dgels.o ATL_dgeql2.o \
       ATL_dgeqlf.o ATL_dgeqlr.o ATL_dgeqr2.o ATL_dgeqrf.o ATL_dgeqrr.o \
       ATL_dgerq2.o ATL_dgerqf.o ATL_dgerqr.o ATL_dgetf2.o ATL_dgetrf.o \
       ATL_dgetrfC.o ATL_dgetrfR.o ATL_dgetri.o ATL_dgetriC.o ATL_dgetriR.o \
       ATL_dgetrs.o ATL_dlamch.o ATL_dlapy2.o ATL_dlarf.o ATL_dlarfb.o \
       ATL_dlarfg.o ATL_dlarft.o ATL_dlascl.o ATL_dlaswp.o ATL_dlauum.o \
       ATL_dlauumCL.o ATL_dlauumCU.o ATL_dlauumRL.o ATL_dlauumRU.o \
       ATL_dormlq.o ATL_dormql.o ATL_dormqr.o ATL_dormrq.o ATL_dpotrf.o \
       ATL_dpotrfL.o ATL_dpotrfU.o ATL_dpotrs.o ATL_dtrtri.o ATL_dtrtriCL.o \
       ATL_dtrtriCU.o ATL_dtrtriRL.o ATL_dtrtriRU.o ATL_dtrtrs.o ATL_ilaenv.o

incdep = $(INCSdir)/atlas_lapack.h
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(dobj)
	$(RANLIB) $(LAPACKlib)
	touch dlib.grd

   
ATL_dlamch.o : $(mySRCdir)/ATL_lamch.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlamch.o -DDREAL \
               $(mySRCdir)/ATL_lamch.c
ATL_dtrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtrtrs.o -DDREAL \
               $(mySRCdir)/ATL_trtrs.c
ATL_dgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgels.o -DDREAL \
               $(mySRCdir)/ATL_gels.c
ATL_dlascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlascl.o -DDREAL \
               $(mySRCdir)/ATL_lascl.c
ATL_dlapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlapy2.o -DDREAL \
               $(mySRCdir)/ATL_lapy2.c
ATL_dlarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlarfgp.o -DDREAL \
               $(mySRCdir)/ATL_larfgp.c
ATL_dlarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlarft.o -DDREAL \
               $(mySRCdir)/ATL_larft.c
ATL_dlarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlarfg.o -DDREAL \
               $(mySRCdir)/ATL_larfg.c
ATL_dlarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlarf.o -DDREAL \
               $(mySRCdir)/ATL_larf.c
ATL_dlarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlarfb.o -DDREAL \
               $(mySRCdir)/ATL_larfb.c
ATL_dormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dormlq.o -DDREAL \
               $(mySRCdir)/ATL_ormlq.c
ATL_dormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dormql.o -DDREAL \
               $(mySRCdir)/ATL_ormql.c
ATL_dormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dormrq.o -DDREAL \
               $(mySRCdir)/ATL_ormrq.c
ATL_dormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dormqr.o -DDREAL \
               $(mySRCdir)/ATL_ormqr.c
ATL_dgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgelqf.o -DDREAL \
               $(mySRCdir)/ATL_gelqf.c
ATL_dgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgelq2.o -DDREAL \
               $(mySRCdir)/ATL_gelq2.c
ATL_dgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgeqlf.o -DDREAL \
               $(mySRCdir)/ATL_geqlf.c
ATL_dgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgeql2.o -DDREAL \
               $(mySRCdir)/ATL_geql2.c
ATL_dgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgerqf.o -DDREAL \
               $(mySRCdir)/ATL_gerqf.c
ATL_dgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgerq2.o -DDREAL \
               $(mySRCdir)/ATL_gerq2.c
ATL_dgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgeqrf.o -DDREAL \
               $(mySRCdir)/ATL_geqrf.c
ATL_dgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgeqr2.o -DDREAL \
               $(mySRCdir)/ATL_geqr2.c
ATL_dpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpotrfL.o -DDREAL \
               $(mySRCdir)/ATL_potrfL.c
ATL_dpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpotrfU.o -DDREAL \
               $(mySRCdir)/ATL_potrfU.c
ATL_dpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpotrs.o -DDREAL \
               $(mySRCdir)/ATL_potrs.c
ATL_dpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpotrf.o -DDREAL \
               $(mySRCdir)/ATL_potrf.c
ATL_dgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetf2.o -DDREAL \
               $(mySRCdir)/ATL_getf2.c
ATL_dgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetrfR.o -DDREAL \
               $(mySRCdir)/ATL_getrfR.c
ATL_dgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetrfC.o -DDREAL \
               $(mySRCdir)/ATL_getrfC.c
ATL_dgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetrs.o -DDREAL \
               $(mySRCdir)/ATL_getrs.c
ATL_dgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetrf.o -DDREAL \
               $(mySRCdir)/ATL_getrf.c
ATL_dlaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlaswp.o -DDREAL \
               $(mySRCdir)/ATL_laswp.c
ATL_dgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetriC.o -DDREAL \
               $(mySRCdir)/ATL_getriC.c
ATL_dgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetriR.o -DDREAL \
               $(mySRCdir)/ATL_getriR.c
ATL_dgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetri.o -DDREAL \
               $(mySRCdir)/ATL_getri.c
ATL_dlauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlauum.o -DDREAL \
               $(mySRCdir)/ATL_lauum.c
ATL_dtrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtrtriRL.o -DDREAL \
               $(mySRCdir)/ATL_trtriRL.c
ATL_dtrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtrtriRU.o -DDREAL \
               $(mySRCdir)/ATL_trtriRU.c
ATL_dtrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtrtriCL.o -DDREAL \
               $(mySRCdir)/ATL_trtriCL.c
ATL_dtrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtrtriCU.o -DDREAL \
               $(mySRCdir)/ATL_trtriCU.c
ATL_dtrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dtrtri.o -DDREAL \
               $(mySRCdir)/ATL_trtri.c
ATL_dgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dgeqrr.o -DDREAL \
               $(mySRCdir)/ATL_geqrr.c
ATL_dgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dgerqr.o -DDREAL \
               $(mySRCdir)/ATL_gerqr.c
ATL_dgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dgeqlr.o -DDREAL \
               $(mySRCdir)/ATL_geqlr.c
ATL_dgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(DKC) $(dKCFLAGS) -c -o ATL_dgelqr.o -DDREAL \
               $(mySRCdir)/ATL_gelqr.c
ATL_dlauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlauumCL.o -DDREAL -DColMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_dlauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlauumRL.o -DDREAL -DRowMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_dlauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlauumCU.o -DDREAL -DColMajor_ \
               $(mySRCdir)/ATL_lauumU.c
ATL_dlauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dlauumRU.o -DDREAL -DRowMajor_ \
               $(mySRCdir)/ATL_lauumU.c

ATL_dgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgetrf4.o -DDREAL \
               $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) ATL_dgetrf4.o
	$(RANLIB) $(LAPACKlib)
	

dclean :
	rm -f $(dobj)
cobj = ATL_cgelq2.o ATL_cgelqf.o ATL_cgelqr.o ATL_cgels.o ATL_cgeql2.o \
       ATL_cgeqlf.o ATL_cgeqlr.o ATL_cgeqr2.o ATL_cgeqrf.o ATL_cgeqrr.o \
       ATL_cgerq2.o ATL_cgerqf.o ATL_cgerqr.o ATL_cgetf2.o ATL_cgetrf.o \
       ATL_cgetrfC.o ATL_cgetrfR.o ATL_cgetri.o ATL_cgetriC.o ATL_cgetriR.o \
       ATL_cgetrs.o ATL_clacgv.o ATL_cladiv.o ATL_clapy2.o ATL_clapy3.o \
       ATL_clarf.o ATL_clarfb.o ATL_clarfg.o ATL_clarft.o ATL_clascl.o \
       ATL_claswp.o ATL_clauum.o ATL_clauumCL.o ATL_clauumCU.o ATL_clauumRL.o \
       ATL_clauumRU.o ATL_cormlq.o ATL_cormql.o ATL_cormqr.o ATL_cormrq.o \
       ATL_cpotrf.o ATL_cpotrfL.o ATL_cpotrfRL.o ATL_cpotrfRU.o ATL_cpotrfU.o \
       ATL_cpotrs.o ATL_ctrtri.o ATL_ctrtriCL.o ATL_ctrtriCU.o ATL_ctrtriRL.o \
       ATL_ctrtriRU.o ATL_ctrtrs.o ATL_ilaenv.o

incdep = $(INCSdir)/atlas_lapack.h
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(cobj)
	$(RANLIB) $(LAPACKlib)
	touch clib.grd

   
ATL_clacgv.o : $(mySRCdir)/ATL_lacgv.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clacgv.o -DSCPLX \
               $(mySRCdir)/ATL_lacgv.c
ATL_clapy3.o : $(mySRCdir)/ATL_lapy3.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clapy3.o -DSCPLX \
               $(mySRCdir)/ATL_lapy3.c
ATL_cladiv.o : $(mySRCdir)/ATL_ladiv.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cladiv.o -DSCPLX \
               $(mySRCdir)/ATL_ladiv.c
ATL_cpotrfRU.o : $(mySRCdir)/ATL_potrfRU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpotrfRU.o -DSCPLX \
               $(mySRCdir)/ATL_potrfRU.c
ATL_cpotrfRL.o : $(mySRCdir)/ATL_potrfRL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpotrfRL.o -DSCPLX \
               $(mySRCdir)/ATL_potrfRL.c
ATL_ctrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctrtrs.o -DSCPLX \
               $(mySRCdir)/ATL_trtrs.c
ATL_cgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgels.o -DSCPLX \
               $(mySRCdir)/ATL_gels.c
ATL_clascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clascl.o -DSCPLX \
               $(mySRCdir)/ATL_lascl.c
ATL_clapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clapy2.o -DSCPLX \
               $(mySRCdir)/ATL_lapy2.c
ATL_clarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clarfgp.o -DSCPLX \
               $(mySRCdir)/ATL_larfgp.c
ATL_clarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clarft.o -DSCPLX \
               $(mySRCdir)/ATL_larft.c
ATL_clarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clarfg.o -DSCPLX \
               $(mySRCdir)/ATL_larfg.c
ATL_clarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clarf.o -DSCPLX \
               $(mySRCdir)/ATL_larf.c
ATL_clarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clarfb.o -DSCPLX \
               $(mySRCdir)/ATL_larfb.c
ATL_cormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cormlq.o -DSCPLX \
               $(mySRCdir)/ATL_ormlq.c
ATL_cormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cormql.o -DSCPLX \
               $(mySRCdir)/ATL_ormql.c
ATL_cormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cormrq.o -DSCPLX \
               $(mySRCdir)/ATL_ormrq.c
ATL_cormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cormqr.o -DSCPLX \
               $(mySRCdir)/ATL_ormqr.c
ATL_cgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgelqf.o -DSCPLX \
               $(mySRCdir)/ATL_gelqf.c
ATL_cgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgelq2.o -DSCPLX \
               $(mySRCdir)/ATL_gelq2.c
ATL_cgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgeqlf.o -DSCPLX \
               $(mySRCdir)/ATL_geqlf.c
ATL_cgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgeql2.o -DSCPLX \
               $(mySRCdir)/ATL_geql2.c
ATL_cgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgerqf.o -DSCPLX \
               $(mySRCdir)/ATL_gerqf.c
ATL_cgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgerq2.o -DSCPLX \
               $(mySRCdir)/ATL_gerq2.c
ATL_cgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgeqrf.o -DSCPLX \
               $(mySRCdir)/ATL_geqrf.c
ATL_cgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgeqr2.o -DSCPLX \
               $(mySRCdir)/ATL_geqr2.c
ATL_cpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpotrfL.o -DSCPLX \
               $(mySRCdir)/ATL_potrfL.c
ATL_cpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpotrfU.o -DSCPLX \
               $(mySRCdir)/ATL_potrfU.c
ATL_cpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpotrs.o -DSCPLX \
               $(mySRCdir)/ATL_potrs.c
ATL_cpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpotrf.o -DSCPLX \
               $(mySRCdir)/ATL_potrf.c
ATL_cgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetf2.o -DSCPLX \
               $(mySRCdir)/ATL_getf2.c
ATL_cgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetrfR.o -DSCPLX \
               $(mySRCdir)/ATL_getrfR.c
ATL_cgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetrfC.o -DSCPLX \
               $(mySRCdir)/ATL_getrfC.c
ATL_cgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetrs.o -DSCPLX \
               $(mySRCdir)/ATL_getrs.c
ATL_cgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetrf.o -DSCPLX \
               $(mySRCdir)/ATL_getrf.c
ATL_claswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_claswp.o -DSCPLX \
               $(mySRCdir)/ATL_laswp.c
ATL_cgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetriC.o -DSCPLX \
               $(mySRCdir)/ATL_getriC.c
ATL_cgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetriR.o -DSCPLX \
               $(mySRCdir)/ATL_getriR.c
ATL_cgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetri.o -DSCPLX \
               $(mySRCdir)/ATL_getri.c
ATL_clauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clauum.o -DSCPLX \
               $(mySRCdir)/ATL_lauum.c
ATL_ctrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctrtriRL.o -DSCPLX \
               $(mySRCdir)/ATL_trtriRL.c
ATL_ctrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctrtriRU.o -DSCPLX \
               $(mySRCdir)/ATL_trtriRU.c
ATL_ctrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctrtriCL.o -DSCPLX \
               $(mySRCdir)/ATL_trtriCL.c
ATL_ctrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctrtriCU.o -DSCPLX \
               $(mySRCdir)/ATL_trtriCU.c
ATL_ctrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_ctrtri.o -DSCPLX \
               $(mySRCdir)/ATL_trtri.c
ATL_cgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_cgeqrr.o -DSCPLX \
               $(mySRCdir)/ATL_geqrr.c
ATL_cgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_cgerqr.o -DSCPLX \
               $(mySRCdir)/ATL_gerqr.c
ATL_cgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_cgeqlr.o -DSCPLX \
               $(mySRCdir)/ATL_geqlr.c
ATL_cgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(CKC) $(cKCFLAGS) -c -o ATL_cgelqr.o -DSCPLX \
               $(mySRCdir)/ATL_gelqr.c
ATL_clauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clauumCL.o -DSCPLX -DColMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_clauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clauumRL.o -DSCPLX -DRowMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_clauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clauumCU.o -DSCPLX -DColMajor_ \
               $(mySRCdir)/ATL_lauumU.c
ATL_clauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_clauumRU.o -DSCPLX -DRowMajor_ \
               $(mySRCdir)/ATL_lauumU.c

ATL_cgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgetrf4.o -DSCPLX \
               $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) ATL_cgetrf4.o
	$(RANLIB) $(LAPACKlib)
	

cclean :
	rm -f $(cobj)
zobj = ATL_ilaenv.o ATL_zgelq2.o ATL_zgelqf.o ATL_zgelqr.o ATL_zgels.o \
       ATL_zgeql2.o ATL_zgeqlf.o ATL_zgeqlr.o ATL_zgeqr2.o ATL_zgeqrf.o \
       ATL_zgeqrr.o ATL_zgerq2.o ATL_zgerqf.o ATL_zgerqr.o ATL_zgetf2.o \
       ATL_zgetrf.o ATL_zgetrfC.o ATL_zgetrfR.o ATL_zgetri.o ATL_zgetriC.o \
       ATL_zgetriR.o ATL_zgetrs.o ATL_zlacgv.o ATL_zladiv.o ATL_zlapy2.o \
       ATL_zlapy3.o ATL_zlarf.o ATL_zlarfb.o ATL_zlarfg.o ATL_zlarft.o \
       ATL_zlascl.o ATL_zlaswp.o ATL_zlauum.o ATL_zlauumCL.o ATL_zlauumCU.o \
       ATL_zlauumRL.o ATL_zlauumRU.o ATL_zormlq.o ATL_zormql.o ATL_zormqr.o \
       ATL_zormrq.o ATL_zpotrf.o ATL_zpotrfL.o ATL_zpotrfRL.o ATL_zpotrfRU.o \
       ATL_zpotrfU.o ATL_zpotrs.o ATL_ztrtri.o ATL_ztrtriCL.o ATL_ztrtriCU.o \
       ATL_ztrtriRL.o ATL_ztrtriRU.o ATL_ztrtrs.o

incdep = $(INCSdir)/atlas_lapack.h
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(zobj)
	$(RANLIB) $(LAPACKlib)
	touch zlib.grd

   
ATL_zlacgv.o : $(mySRCdir)/ATL_lacgv.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlacgv.o -DDCPLX \
               $(mySRCdir)/ATL_lacgv.c
ATL_zlapy3.o : $(mySRCdir)/ATL_lapy3.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlapy3.o -DDCPLX \
               $(mySRCdir)/ATL_lapy3.c
ATL_zladiv.o : $(mySRCdir)/ATL_ladiv.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zladiv.o -DDCPLX \
               $(mySRCdir)/ATL_ladiv.c
ATL_zpotrfRU.o : $(mySRCdir)/ATL_potrfRU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpotrfRU.o -DDCPLX \
               $(mySRCdir)/ATL_potrfRU.c
ATL_zpotrfRL.o : $(mySRCdir)/ATL_potrfRL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpotrfRL.o -DDCPLX \
               $(mySRCdir)/ATL_potrfRL.c
ATL_ztrtrs.o : $(mySRCdir)/ATL_trtrs.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztrtrs.o -DDCPLX \
               $(mySRCdir)/ATL_trtrs.c
ATL_zgels.o : $(mySRCdir)/ATL_gels.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgels.o -DDCPLX \
               $(mySRCdir)/ATL_gels.c
ATL_zlascl.o : $(mySRCdir)/ATL_lascl.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlascl.o -DDCPLX \
               $(mySRCdir)/ATL_lascl.c
ATL_zlapy2.o : $(mySRCdir)/ATL_lapy2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlapy2.o -DDCPLX \
               $(mySRCdir)/ATL_lapy2.c
ATL_zlarfgp.o : $(mySRCdir)/ATL_larfgp.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlarfgp.o -DDCPLX \
               $(mySRCdir)/ATL_larfgp.c
ATL_zlarft.o : $(mySRCdir)/ATL_larft.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlarft.o -DDCPLX \
               $(mySRCdir)/ATL_larft.c
ATL_zlarfg.o : $(mySRCdir)/ATL_larfg.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlarfg.o -DDCPLX \
               $(mySRCdir)/ATL_larfg.c
ATL_zlarf.o : $(mySRCdir)/ATL_larf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlarf.o -DDCPLX \
               $(mySRCdir)/ATL_larf.c
ATL_zlarfb.o : $(mySRCdir)/ATL_larfb.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlarfb.o -DDCPLX \
               $(mySRCdir)/ATL_larfb.c
ATL_zormlq.o : $(mySRCdir)/ATL_ormlq.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zormlq.o -DDCPLX \
               $(mySRCdir)/ATL_ormlq.c
ATL_zormql.o : $(mySRCdir)/ATL_ormql.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zormql.o -DDCPLX \
               $(mySRCdir)/ATL_ormql.c
ATL_zormrq.o : $(mySRCdir)/ATL_ormrq.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zormrq.o -DDCPLX \
               $(mySRCdir)/ATL_ormrq.c
ATL_zormqr.o : $(mySRCdir)/ATL_ormqr.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zormqr.o -DDCPLX \
               $(mySRCdir)/ATL_ormqr.c
ATL_zgelqf.o : $(mySRCdir)/ATL_gelqf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgelqf.o -DDCPLX \
               $(mySRCdir)/ATL_gelqf.c
ATL_zgelq2.o : $(mySRCdir)/ATL_gelq2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgelq2.o -DDCPLX \
               $(mySRCdir)/ATL_gelq2.c
ATL_zgeqlf.o : $(mySRCdir)/ATL_geqlf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgeqlf.o -DDCPLX \
               $(mySRCdir)/ATL_geqlf.c
ATL_zgeql2.o : $(mySRCdir)/ATL_geql2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgeql2.o -DDCPLX \
               $(mySRCdir)/ATL_geql2.c
ATL_zgerqf.o : $(mySRCdir)/ATL_gerqf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgerqf.o -DDCPLX \
               $(mySRCdir)/ATL_gerqf.c
ATL_zgerq2.o : $(mySRCdir)/ATL_gerq2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgerq2.o -DDCPLX \
               $(mySRCdir)/ATL_gerq2.c
ATL_zgeqrf.o : $(mySRCdir)/ATL_geqrf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgeqrf.o -DDCPLX \
               $(mySRCdir)/ATL_geqrf.c
ATL_zgeqr2.o : $(mySRCdir)/ATL_geqr2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgeqr2.o -DDCPLX \
               $(mySRCdir)/ATL_geqr2.c
ATL_zpotrfL.o : $(mySRCdir)/ATL_potrfL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpotrfL.o -DDCPLX \
               $(mySRCdir)/ATL_potrfL.c
ATL_zpotrfU.o : $(mySRCdir)/ATL_potrfU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpotrfU.o -DDCPLX \
               $(mySRCdir)/ATL_potrfU.c
ATL_zpotrs.o : $(mySRCdir)/ATL_potrs.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpotrs.o -DDCPLX \
               $(mySRCdir)/ATL_potrs.c
ATL_zpotrf.o : $(mySRCdir)/ATL_potrf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpotrf.o -DDCPLX \
               $(mySRCdir)/ATL_potrf.c
ATL_zgetf2.o : $(mySRCdir)/ATL_getf2.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetf2.o -DDCPLX \
               $(mySRCdir)/ATL_getf2.c
ATL_zgetrfR.o : $(mySRCdir)/ATL_getrfR.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetrfR.o -DDCPLX \
               $(mySRCdir)/ATL_getrfR.c
ATL_zgetrfC.o : $(mySRCdir)/ATL_getrfC.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetrfC.o -DDCPLX \
               $(mySRCdir)/ATL_getrfC.c
ATL_zgetrs.o : $(mySRCdir)/ATL_getrs.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetrs.o -DDCPLX \
               $(mySRCdir)/ATL_getrs.c
ATL_zgetrf.o : $(mySRCdir)/ATL_getrf.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetrf.o -DDCPLX \
               $(mySRCdir)/ATL_getrf.c
ATL_zlaswp.o : $(mySRCdir)/ATL_laswp.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlaswp.o -DDCPLX \
               $(mySRCdir)/ATL_laswp.c
ATL_zgetriC.o : $(mySRCdir)/ATL_getriC.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetriC.o -DDCPLX \
               $(mySRCdir)/ATL_getriC.c
ATL_zgetriR.o : $(mySRCdir)/ATL_getriR.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetriR.o -DDCPLX \
               $(mySRCdir)/ATL_getriR.c
ATL_zgetri.o : $(mySRCdir)/ATL_getri.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetri.o -DDCPLX \
               $(mySRCdir)/ATL_getri.c
ATL_zlauum.o : $(mySRCdir)/ATL_lauum.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlauum.o -DDCPLX \
               $(mySRCdir)/ATL_lauum.c
ATL_ztrtriRL.o : $(mySRCdir)/ATL_trtriRL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztrtriRL.o -DDCPLX \
               $(mySRCdir)/ATL_trtriRL.c
ATL_ztrtriRU.o : $(mySRCdir)/ATL_trtriRU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztrtriRU.o -DDCPLX \
               $(mySRCdir)/ATL_trtriRU.c
ATL_ztrtriCL.o : $(mySRCdir)/ATL_trtriCL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztrtriCL.o -DDCPLX \
               $(mySRCdir)/ATL_trtriCL.c
ATL_ztrtriCU.o : $(mySRCdir)/ATL_trtriCU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztrtriCU.o -DDCPLX \
               $(mySRCdir)/ATL_trtriCU.c
ATL_ztrtri.o : $(mySRCdir)/ATL_trtri.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_ztrtri.o -DDCPLX \
               $(mySRCdir)/ATL_trtri.c
ATL_zgeqrr.o : $(mySRCdir)/ATL_geqrr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgeqrr.o -DDCPLX \
               $(mySRCdir)/ATL_geqrr.c
ATL_zgerqr.o : $(mySRCdir)/ATL_gerqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgerqr.o -DDCPLX \
               $(mySRCdir)/ATL_gerqr.c
ATL_zgeqlr.o : $(mySRCdir)/ATL_geqlr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgeqlr.o -DDCPLX \
               $(mySRCdir)/ATL_geqlr.c
ATL_zgelqr.o : $(mySRCdir)/ATL_gelqr.c $(incdep) \
                           $(INCSdir)/atlas_qrrmeth.h
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgelqr.o -DDCPLX \
               $(mySRCdir)/ATL_gelqr.c
ATL_zlauumCL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlauumCL.o -DDCPLX -DColMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_zlauumRL.o : $(mySRCdir)/ATL_lauumL.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlauumRL.o -DDCPLX -DRowMajor_ \
               $(mySRCdir)/ATL_lauumL.c
ATL_zlauumCU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlauumCU.o -DDCPLX -DColMajor_ \
               $(mySRCdir)/ATL_lauumU.c
ATL_zlauumRU.o : $(mySRCdir)/ATL_lauumU.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zlauumRU.o -DDCPLX -DRowMajor_ \
               $(mySRCdir)/ATL_lauumU.c

ATL_zgetrf4.o : $(mySRCdir)/ATL_getrf4.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgetrf4.o -DDCPLX \
               $(mySRCdir)/ATL_getrf4.c
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) ATL_zgetrf4.o
	$(RANLIB) $(LAPACKlib)
	

zclean :
	rm -f $(zobj)

tenvdep = $(INCSdir)/atlas_lapack.h $(INCAdir)/atlas_stGetNB_gerqf.h \
          $(INCAdir)/atlas_stGetNB_geqrf.h $(INCAdir)/atlas_stGetNB_geqlf.h \
          $(INCAdir)/atlas_stGetNB_gelqf.h $(INCAdir)/atlas_dtGetNB_gerqf.h \
          $(INCAdir)/atlas_dtGetNB_geqrf.h $(INCAdir)/atlas_dtGetNB_geqlf.h \
          $(INCAdir)/atlas_dtGetNB_gelqf.h $(INCAdir)/atlas_ctGetNB_gerqf.h \
          $(INCAdir)/atlas_ctGetNB_geqrf.h $(INCAdir)/atlas_ctGetNB_geqlf.h \
          $(INCAdir)/atlas_ctGetNB_gelqf.h $(INCAdir)/atlas_ztGetNB_gerqf.h \
          $(INCAdir)/atlas_ztGetNB_geqrf.h $(INCAdir)/atlas_ztGetNB_geqlf.h \
          $(INCAdir)/atlas_ztGetNB_gelqf.h
ATL_itlaenv.o : $(tenvdep) $(mySRCdir)/ATL_ilaenv.c
	$(DKC) $(DKCFLAGS) $(CDEFS) -c -o ATL_itlaenv.o -DATL_USEPTHREADS \
               $(mySRCdir)/ATL_ilaenv.c
envdep = $(INCSdir)/atlas_lapack.h $(INCAdir)/atlas_sGetNB_gerqf.h \
         $(INCAdir)/atlas_sGetNB_geqrf.h $(INCAdir)/atlas_sGetNB_geqlf.h \
         $(INCAdir)/atlas_sGetNB_gelqf.h $(INCAdir)/atlas_dGetNB_gerqf.h \
         $(INCAdir)/atlas_dGetNB_geqrf.h $(INCAdir)/atlas_dGetNB_geqlf.h \
         $(INCAdir)/atlas_dGetNB_gelqf.h $(INCAdir)/atlas_cGetNB_gerqf.h \
         $(INCAdir)/atlas_cGetNB_geqrf.h $(INCAdir)/atlas_cGetNB_geqlf.h \
         $(INCAdir)/atlas_cGetNB_gelqf.h $(INCAdir)/atlas_zGetNB_gerqf.h \
         $(INCAdir)/atlas_zGetNB_geqrf.h $(INCAdir)/atlas_zGetNB_geqlf.h \
         $(INCAdir)/atlas_zGetNB_gelqf.h
ATL_ilaenv.o : $(envdep) $(mySRCdir)/ATL_ilaenv.c
	$(DKC) $(DKCFLAGS) $(CDEFS) -c -o ATL_ilaenv.o  \
               $(mySRCdir)/ATL_ilaenv.c


