include Make.inc
mySRCdir = $(SRCdir)/interfaces/lapack/F77/src

all :
	$(MAKE) lib
	- $(MAKE) tlib
clean : dclean sclean zclean cclean iclean \
        dtclean stclean ztclean ctclean itclean
ptlib : tlib
tlib : treflapack.grd dtlib stlib ztlib ctlib


stobj = ATL_f77wrap_itlaenv.o ATL_f77wrap_sgetnb.o ATL_f77wrap_sgetri.o \
        ATL_f77wrap_sgetrs.o ATL_f77wrap_slarfb.o ATL_f77wrap_slarft.o \
        ATL_f77wrap_slauum.o ATL_f77wrap_sposv.o ATL_f77wrap_spotrf.o \
        ATL_f77wrap_spotrs.o ATL_f77wrap_stgelqf.o ATL_f77wrap_stgels.o \
        ATL_f77wrap_stgeqlf.o ATL_f77wrap_stgeqrf.o ATL_f77wrap_stgerqf.o \
        ATL_f77wrap_stgesv.o ATL_f77wrap_stgetrf.o ATL_f77wrap_strtri.o \
        ieeeck.o ilaenv.o sgetri.o sgetrs.o slamc3.o slamch.o slarfb.o \
        slarft.o slauum.o sposv.o spotrf.o spotri.o spotrs.o stgelqf.o \
        stgels.o stgeqlf.o stgeqrf.o stgerqf.o stgesv.o stgetrf.o strtri.o
dtobj = ATL_f77wrap_dgetnb.o ATL_f77wrap_dgetri.o ATL_f77wrap_dgetrs.o \
        ATL_f77wrap_dlarfb.o ATL_f77wrap_dlarft.o ATL_f77wrap_dlauum.o \
        ATL_f77wrap_dposv.o ATL_f77wrap_dpotrf.o ATL_f77wrap_dpotrs.o \
        ATL_f77wrap_dtgelqf.o ATL_f77wrap_dtgels.o ATL_f77wrap_dtgeqlf.o \
        ATL_f77wrap_dtgeqrf.o ATL_f77wrap_dtgerqf.o ATL_f77wrap_dtgesv.o \
        ATL_f77wrap_dtgetrf.o ATL_f77wrap_dtrtri.o ATL_f77wrap_itlaenv.o \
        dgetri.o dgetrs.o dlamc3.o dlamch.o dlarfb.o dlarft.o dlauum.o \
        dposv.o dpotrf.o dpotri.o dpotrs.o dtgelqf.o dtgels.o dtgeqlf.o \
        dtgeqrf.o dtgerqf.o dtgesv.o dtgetrf.o dtrtri.o ieeeck.o ilaenv.o
ctobj = ATL_f77wrap_cgetnb.o ATL_f77wrap_cgetri.o ATL_f77wrap_cgetrs.o \
        ATL_f77wrap_clarfb.o ATL_f77wrap_clarft.o ATL_f77wrap_clauum.o \
        ATL_f77wrap_cposv.o ATL_f77wrap_cpotrf.o ATL_f77wrap_cpotrs.o \
        ATL_f77wrap_ctgelqf.o ATL_f77wrap_ctgels.o ATL_f77wrap_ctgeqlf.o \
        ATL_f77wrap_ctgeqrf.o ATL_f77wrap_ctgerqf.o ATL_f77wrap_ctgesv.o \
        ATL_f77wrap_ctgetrf.o ATL_f77wrap_ctrtri.o ATL_f77wrap_itlaenv.o \
        cgetri.o cgetrs.o clarfb.o clarft.o clauum.o cposv.o cpotrf.o \
        cpotri.o cpotrs.o ctgelqf.o ctgels.o ctgeqlf.o ctgeqrf.o ctgerqf.o \
        ctgesv.o ctgetrf.o ctrtri.o ieeeck.o ilaenv.o
ztobj = ATL_f77wrap_itlaenv.o ATL_f77wrap_zgetnb.o ATL_f77wrap_zgetri.o \
        ATL_f77wrap_zgetrs.o ATL_f77wrap_zlarfb.o ATL_f77wrap_zlarft.o \
        ATL_f77wrap_zlauum.o ATL_f77wrap_zposv.o ATL_f77wrap_zpotrf.o \
        ATL_f77wrap_zpotrs.o ATL_f77wrap_ztgelqf.o ATL_f77wrap_ztgels.o \
        ATL_f77wrap_ztgeqlf.o ATL_f77wrap_ztgeqrf.o ATL_f77wrap_ztgerqf.o \
        ATL_f77wrap_ztgesv.o ATL_f77wrap_ztgetrf.o ATL_f77wrap_ztrtri.o \
        ieeeck.o ilaenv.o zgetri.o zgetrs.o zlarfb.o zlarft.o zlauum.o \
        zposv.o zpotrf.o zpotri.o zpotrs.o ztgelqf.o ztgels.o ztgeqlf.o \
        ztgeqrf.o ztgerqf.o ztgesv.o ztgetrf.o ztrtri.o

itclean :
	rm -f $(itlaobj) ieeeck.o
stclean :
	rm -f $(stobj) stlib.grd reflapack.grd
stlib : stlib.grd 
stlib.grd : $(stobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(stobj)
	$(RANLIB) $(PTLAPACKlib)
	touch stlib.grd
dtclean :
	rm -f $(dtobj) dtlib.grd reflapack.grd
dtlib : dtlib.grd 
dtlib.grd : $(dtobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(dtobj)
	$(RANLIB) $(PTLAPACKlib)
	touch dtlib.grd
ctclean :
	rm -f $(ctobj) ctlib.grd reflapack.grd
ctlib : ctlib.grd 
ctlib.grd : $(ctobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(ctobj)
	$(RANLIB) $(PTLAPACKlib)
	touch ctlib.grd
ztclean :
	rm -f $(ztobj) ztlib.grd reflapack.grd
ztlib : ztlib.grd 
ztlib.grd : $(ztobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(ztobj)
	$(RANLIB) $(PTLAPACKlib)
	touch ztlib.grd

stpotri.o : $(mySRCdir)/spotri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/spotri.f
stgetri.o : $(mySRCdir)/sgetri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgetri.f
ATL_f77wrap_stgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
stlauum.o : $(mySRCdir)/slauum.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/slauum.f
ATL_f77wrap_stlauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
sttrtri.o : $(mySRCdir)/strtri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/strtri.f
ATL_f77wrap_sttrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
stgetrs.o : $(mySRCdir)/sgetrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgetrs.f
ATL_f77wrap_stgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
stgetrf.o : $(mySRCdir)/sgetrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgetrf.f
ATL_f77wrap_stgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
stgesv.o : $(mySRCdir)/sgesv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgesv.f
ATL_f77wrap_stgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
stpotrs.o : $(mySRCdir)/spotrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/spotrs.f
ATL_f77wrap_stpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
stpotrf.o : $(mySRCdir)/spotrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/spotrf.f
ATL_f77wrap_stpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
stposv.o : $(mySRCdir)/sposv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sposv.f
ATL_f77wrap_stposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
stgels.o : $(mySRCdir)/sgels.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgels.f
ATL_f77wrap_stgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
stlarft.o : $(mySRCdir)/slarft.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/slarft.f
ATL_f77wrap_stlarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
stlarfb.o : $(mySRCdir)/slarfb.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/slarfb.f
ATL_f77wrap_stlarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
stgelqf.o : $(mySRCdir)/sgelqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgelqf.f
ATL_f77wrap_stgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
stgerqf.o : $(mySRCdir)/sgerqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgerqf.f
ATL_f77wrap_stgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
stgeqlf.o : $(mySRCdir)/sgeqlf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgeqlf.f
ATL_f77wrap_stgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
stgeqrf.o : $(mySRCdir)/sgeqrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/sgeqrf.f
ATL_f77wrap_stgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_stgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

dtpotri.o : $(mySRCdir)/dpotri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dpotri.f
dtgetri.o : $(mySRCdir)/dgetri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgetri.f
ATL_f77wrap_dtgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
dtlauum.o : $(mySRCdir)/dlauum.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dlauum.f
ATL_f77wrap_dtlauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
dttrtri.o : $(mySRCdir)/dtrtri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dtrtri.f
ATL_f77wrap_dttrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
dtgetrs.o : $(mySRCdir)/dgetrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgetrs.f
ATL_f77wrap_dtgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
dtgetrf.o : $(mySRCdir)/dgetrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgetrf.f
ATL_f77wrap_dtgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
dtgesv.o : $(mySRCdir)/dgesv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgesv.f
ATL_f77wrap_dtgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
dtpotrs.o : $(mySRCdir)/dpotrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dpotrs.f
ATL_f77wrap_dtpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
dtpotrf.o : $(mySRCdir)/dpotrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dpotrf.f
ATL_f77wrap_dtpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
dtposv.o : $(mySRCdir)/dposv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dposv.f
ATL_f77wrap_dtposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
dtgels.o : $(mySRCdir)/dgels.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgels.f
ATL_f77wrap_dtgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
dtlarft.o : $(mySRCdir)/dlarft.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dlarft.f
ATL_f77wrap_dtlarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
dtlarfb.o : $(mySRCdir)/dlarfb.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dlarfb.f
ATL_f77wrap_dtlarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
dtgelqf.o : $(mySRCdir)/dgelqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgelqf.f
ATL_f77wrap_dtgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
dtgerqf.o : $(mySRCdir)/dgerqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgerqf.f
ATL_f77wrap_dtgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
dtgeqlf.o : $(mySRCdir)/dgeqlf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgeqlf.f
ATL_f77wrap_dtgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
dtgeqrf.o : $(mySRCdir)/dgeqrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/dgeqrf.f
ATL_f77wrap_dtgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_dtgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

ctpotri.o : $(mySRCdir)/cpotri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cpotri.f
ctgetri.o : $(mySRCdir)/cgetri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgetri.f
ATL_f77wrap_ctgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
ctlauum.o : $(mySRCdir)/clauum.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/clauum.f
ATL_f77wrap_ctlauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
cttrtri.o : $(mySRCdir)/ctrtri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/ctrtri.f
ATL_f77wrap_cttrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
ctgetrs.o : $(mySRCdir)/cgetrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgetrs.f
ATL_f77wrap_ctgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
ctgetrf.o : $(mySRCdir)/cgetrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgetrf.f
ATL_f77wrap_ctgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
ctgesv.o : $(mySRCdir)/cgesv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgesv.f
ATL_f77wrap_ctgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
ctpotrs.o : $(mySRCdir)/cpotrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cpotrs.f
ATL_f77wrap_ctpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
ctpotrf.o : $(mySRCdir)/cpotrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cpotrf.f
ATL_f77wrap_ctpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
ctposv.o : $(mySRCdir)/cposv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cposv.f
ATL_f77wrap_ctposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
ctgels.o : $(mySRCdir)/cgels.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgels.f
ATL_f77wrap_ctgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
ctlarft.o : $(mySRCdir)/clarft.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/clarft.f
ATL_f77wrap_ctlarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
ctlarfb.o : $(mySRCdir)/clarfb.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/clarfb.f
ATL_f77wrap_ctlarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
ctgelqf.o : $(mySRCdir)/cgelqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgelqf.f
ATL_f77wrap_ctgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
ctgerqf.o : $(mySRCdir)/cgerqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgerqf.f
ATL_f77wrap_ctgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
ctgeqlf.o : $(mySRCdir)/cgeqlf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgeqlf.f
ATL_f77wrap_ctgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
ctgeqrf.o : $(mySRCdir)/cgeqrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/cgeqrf.f
ATL_f77wrap_ctgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_ctgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

ztpotri.o : $(mySRCdir)/zpotri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zpotri.f
ztgetri.o : $(mySRCdir)/zgetri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgetri.f
ATL_f77wrap_ztgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
ztlauum.o : $(mySRCdir)/zlauum.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zlauum.f
ATL_f77wrap_ztlauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
zttrtri.o : $(mySRCdir)/ztrtri.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/ztrtri.f
ATL_f77wrap_zttrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
ztgetrs.o : $(mySRCdir)/zgetrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgetrs.f
ATL_f77wrap_ztgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
ztgetrf.o : $(mySRCdir)/zgetrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgetrf.f
ATL_f77wrap_ztgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
ztgesv.o : $(mySRCdir)/zgesv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgesv.f
ATL_f77wrap_ztgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
ztpotrs.o : $(mySRCdir)/zpotrs.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zpotrs.f
ATL_f77wrap_ztpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
ztpotrf.o : $(mySRCdir)/zpotrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zpotrf.f
ATL_f77wrap_ztpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
ztposv.o : $(mySRCdir)/zposv.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zposv.f
ATL_f77wrap_ztposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
ztgels.o : $(mySRCdir)/zgels.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgels.f
ATL_f77wrap_ztgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
ztlarft.o : $(mySRCdir)/zlarft.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zlarft.f
ATL_f77wrap_ztlarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
ztlarfb.o : $(mySRCdir)/zlarfb.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zlarfb.f
ATL_f77wrap_ztlarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
ztgelqf.o : $(mySRCdir)/zgelqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgelqf.f
ATL_f77wrap_ztgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
ztgerqf.o : $(mySRCdir)/zgerqf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgerqf.f
ATL_f77wrap_ztgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
ztgeqlf.o : $(mySRCdir)/zgeqlf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgeqlf.f
ATL_f77wrap_ztgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
ztgeqrf.o : $(mySRCdir)/zgeqrf.f
	$(F77) -o $@ -DATL_USEPTHREADS -c $(F77FLAGS) $(mySRCdir)/zgeqrf.f
ATL_f77wrap_ztgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_ztgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

ATL_f77wrap_itlaenv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_ilaenv.c
	$(KC) $(KCFLAGS) -o $@ -DATL_USEPTHREADS -c $(mySRCdir)/f77wrap/ATL_f77wrap_ilaenv.c
treflarebuild :    # force rebuild of netlib lapack
	rm -f $(FLAdir)/lapack_$(ARCH).a
	$(MAKE) treflabuild
treflapack.grd : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) treflabuild ; \
        fi
	touch reflapack.grd
treflabuild : $(FLAdir)/lapack_$(ARCH).a
	mkdir flapack 
	cd flapack ; $(ARCHIVER) x $(FLAdir)/lapack_$(ARCH).a
	cd flapack ; rm -f ?posv.o ?potrf.o ?potrs.o ?gesv.o ?getrf.o ?getrs.o \
           ?potri.o ?lauum.o ?getri.o ?trtri.o ieeeck.o ilaenv.o [s,d]lamch.o \
           ?geqrf.o ?gerqf.o ?geqlf.o ?gelqf.o ?larfb.o ?larft.o ?gels.o \
           xerbla.o lsame.o
	cd flapack ; $(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) *.o
	rm -rf flapack
	$(RANLIB) $(PTLAPACKlib)
lib : reflapack.grd dlib slib zlib clib


sobj = ATL_f77wrap_ilaenv.o ATL_f77wrap_sgelqf.o ATL_f77wrap_sgels.o \
       ATL_f77wrap_sgeqlf.o ATL_f77wrap_sgeqrf.o ATL_f77wrap_sgerqf.o \
       ATL_f77wrap_sgesv.o ATL_f77wrap_sgetnb.o ATL_f77wrap_sgetrf.o \
       ATL_f77wrap_sgetri.o ATL_f77wrap_sgetrs.o ATL_f77wrap_slarfb.o \
       ATL_f77wrap_slarft.o ATL_f77wrap_slauum.o ATL_f77wrap_sposv.o \
       ATL_f77wrap_spotrf.o ATL_f77wrap_spotrs.o ATL_f77wrap_strtri.o \
       ieeeck.o ilaenv.o sgelqf.o sgels.o sgeqlf.o sgeqrf.o sgerqf.o sgesv.o \
       sgetrf.o sgetri.o sgetrs.o slamc3.o slamch.o slarfb.o slarft.o \
       slauum.o sposv.o spotrf.o spotri.o spotrs.o strtri.o
dobj = ATL_f77wrap_dgelqf.o ATL_f77wrap_dgels.o ATL_f77wrap_dgeqlf.o \
       ATL_f77wrap_dgeqrf.o ATL_f77wrap_dgerqf.o ATL_f77wrap_dgesv.o \
       ATL_f77wrap_dgetnb.o ATL_f77wrap_dgetrf.o ATL_f77wrap_dgetri.o \
       ATL_f77wrap_dgetrs.o ATL_f77wrap_dlarfb.o ATL_f77wrap_dlarft.o \
       ATL_f77wrap_dlauum.o ATL_f77wrap_dposv.o ATL_f77wrap_dpotrf.o \
       ATL_f77wrap_dpotrs.o ATL_f77wrap_dtrtri.o ATL_f77wrap_ilaenv.o \
       dgelqf.o dgels.o dgeqlf.o dgeqrf.o dgerqf.o dgesv.o dgetrf.o dgetri.o \
       dgetrs.o dlamc3.o dlamch.o dlarfb.o dlarft.o dlauum.o dposv.o dpotrf.o \
       dpotri.o dpotrs.o dtrtri.o ieeeck.o ilaenv.o
cobj = ATL_f77wrap_cgelqf.o ATL_f77wrap_cgels.o ATL_f77wrap_cgeqlf.o \
       ATL_f77wrap_cgeqrf.o ATL_f77wrap_cgerqf.o ATL_f77wrap_cgesv.o \
       ATL_f77wrap_cgetnb.o ATL_f77wrap_cgetrf.o ATL_f77wrap_cgetri.o \
       ATL_f77wrap_cgetrs.o ATL_f77wrap_clarfb.o ATL_f77wrap_clarft.o \
       ATL_f77wrap_clauum.o ATL_f77wrap_cposv.o ATL_f77wrap_cpotrf.o \
       ATL_f77wrap_cpotrs.o ATL_f77wrap_ctrtri.o ATL_f77wrap_ilaenv.o \
       cgelqf.o cgels.o cgeqlf.o cgeqrf.o cgerqf.o cgesv.o cgetrf.o cgetri.o \
       cgetrs.o clarfb.o clarft.o clauum.o cposv.o cpotrf.o cpotri.o cpotrs.o \
       ctrtri.o ieeeck.o ilaenv.o
zobj = ATL_f77wrap_ilaenv.o ATL_f77wrap_zgelqf.o ATL_f77wrap_zgels.o \
       ATL_f77wrap_zgeqlf.o ATL_f77wrap_zgeqrf.o ATL_f77wrap_zgerqf.o \
       ATL_f77wrap_zgesv.o ATL_f77wrap_zgetnb.o ATL_f77wrap_zgetrf.o \
       ATL_f77wrap_zgetri.o ATL_f77wrap_zgetrs.o ATL_f77wrap_zlarfb.o \
       ATL_f77wrap_zlarft.o ATL_f77wrap_zlauum.o ATL_f77wrap_zposv.o \
       ATL_f77wrap_zpotrf.o ATL_f77wrap_zpotrs.o ATL_f77wrap_ztrtri.o \
       ieeeck.o ilaenv.o zgelqf.o zgels.o zgeqlf.o zgeqrf.o zgerqf.o zgesv.o \
       zgetrf.o zgetri.o zgetrs.o zlarfb.o zlarft.o zlauum.o zposv.o zpotrf.o \
       zpotri.o zpotrs.o ztrtri.o

iclean :
	rm -f $(ilaobj) ieeeck.o
sclean :
	rm -f $(sobj) slib.grd reflapack.grd
slib : slib.grd 
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(sobj)
	$(RANLIB) $(LAPACKlib)
	touch slib.grd
dclean :
	rm -f $(dobj) dlib.grd reflapack.grd
dlib : dlib.grd 
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(dobj)
	$(RANLIB) $(LAPACKlib)
	touch dlib.grd
cclean :
	rm -f $(cobj) clib.grd reflapack.grd
clib : clib.grd 
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(cobj)
	$(RANLIB) $(LAPACKlib)
	touch clib.grd
zclean :
	rm -f $(zobj) zlib.grd reflapack.grd
zlib : zlib.grd 
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(zobj)
	$(RANLIB) $(LAPACKlib)
	touch zlib.grd

spotri.o : $(mySRCdir)/spotri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/spotri.f
sgetri.o : $(mySRCdir)/sgetri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgetri.f
ATL_f77wrap_sgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
slauum.o : $(mySRCdir)/slauum.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/slauum.f
ATL_f77wrap_slauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
strtri.o : $(mySRCdir)/strtri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/strtri.f
ATL_f77wrap_strtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
sgetrs.o : $(mySRCdir)/sgetrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgetrs.f
ATL_f77wrap_sgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
sgetrf.o : $(mySRCdir)/sgetrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgetrf.f
ATL_f77wrap_sgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
sgesv.o : $(mySRCdir)/sgesv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgesv.f
ATL_f77wrap_sgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
spotrs.o : $(mySRCdir)/spotrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/spotrs.f
ATL_f77wrap_spotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
spotrf.o : $(mySRCdir)/spotrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/spotrf.f
ATL_f77wrap_spotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
sposv.o : $(mySRCdir)/sposv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sposv.f
ATL_f77wrap_sposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
sgels.o : $(mySRCdir)/sgels.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgels.f
ATL_f77wrap_sgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
slarft.o : $(mySRCdir)/slarft.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/slarft.f
ATL_f77wrap_slarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
slarfb.o : $(mySRCdir)/slarfb.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/slarfb.f
ATL_f77wrap_slarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
sgelqf.o : $(mySRCdir)/sgelqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgelqf.f
ATL_f77wrap_sgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
sgerqf.o : $(mySRCdir)/sgerqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgerqf.f
ATL_f77wrap_sgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
sgeqlf.o : $(mySRCdir)/sgeqlf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgeqlf.f
ATL_f77wrap_sgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
sgeqrf.o : $(mySRCdir)/sgeqrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/sgeqrf.f
ATL_f77wrap_sgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_sgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@  -c -DSREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

dpotri.o : $(mySRCdir)/dpotri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dpotri.f
dgetri.o : $(mySRCdir)/dgetri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgetri.f
ATL_f77wrap_dgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
dlauum.o : $(mySRCdir)/dlauum.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dlauum.f
ATL_f77wrap_dlauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
dtrtri.o : $(mySRCdir)/dtrtri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dtrtri.f
ATL_f77wrap_dtrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
dgetrs.o : $(mySRCdir)/dgetrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgetrs.f
ATL_f77wrap_dgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
dgetrf.o : $(mySRCdir)/dgetrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgetrf.f
ATL_f77wrap_dgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
dgesv.o : $(mySRCdir)/dgesv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgesv.f
ATL_f77wrap_dgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
dpotrs.o : $(mySRCdir)/dpotrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dpotrs.f
ATL_f77wrap_dpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
dpotrf.o : $(mySRCdir)/dpotrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dpotrf.f
ATL_f77wrap_dpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
dposv.o : $(mySRCdir)/dposv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dposv.f
ATL_f77wrap_dposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
dgels.o : $(mySRCdir)/dgels.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgels.f
ATL_f77wrap_dgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
dlarft.o : $(mySRCdir)/dlarft.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dlarft.f
ATL_f77wrap_dlarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
dlarfb.o : $(mySRCdir)/dlarfb.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dlarfb.f
ATL_f77wrap_dlarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
dgelqf.o : $(mySRCdir)/dgelqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgelqf.f
ATL_f77wrap_dgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
dgerqf.o : $(mySRCdir)/dgerqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgerqf.f
ATL_f77wrap_dgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
dgeqlf.o : $(mySRCdir)/dgeqlf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgeqlf.f
ATL_f77wrap_dgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
dgeqrf.o : $(mySRCdir)/dgeqrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/dgeqrf.f
ATL_f77wrap_dgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_dgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@  -c -DDREAL $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

cpotri.o : $(mySRCdir)/cpotri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cpotri.f
cgetri.o : $(mySRCdir)/cgetri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgetri.f
ATL_f77wrap_cgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
clauum.o : $(mySRCdir)/clauum.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/clauum.f
ATL_f77wrap_clauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
ctrtri.o : $(mySRCdir)/ctrtri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/ctrtri.f
ATL_f77wrap_ctrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
cgetrs.o : $(mySRCdir)/cgetrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgetrs.f
ATL_f77wrap_cgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
cgetrf.o : $(mySRCdir)/cgetrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgetrf.f
ATL_f77wrap_cgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
cgesv.o : $(mySRCdir)/cgesv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgesv.f
ATL_f77wrap_cgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
cpotrs.o : $(mySRCdir)/cpotrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cpotrs.f
ATL_f77wrap_cpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
cpotrf.o : $(mySRCdir)/cpotrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cpotrf.f
ATL_f77wrap_cpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
cposv.o : $(mySRCdir)/cposv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cposv.f
ATL_f77wrap_cposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
cgels.o : $(mySRCdir)/cgels.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgels.f
ATL_f77wrap_cgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
clarft.o : $(mySRCdir)/clarft.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/clarft.f
ATL_f77wrap_clarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
clarfb.o : $(mySRCdir)/clarfb.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/clarfb.f
ATL_f77wrap_clarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
cgelqf.o : $(mySRCdir)/cgelqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgelqf.f
ATL_f77wrap_cgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
cgerqf.o : $(mySRCdir)/cgerqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgerqf.f
ATL_f77wrap_cgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
cgeqlf.o : $(mySRCdir)/cgeqlf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgeqlf.f
ATL_f77wrap_cgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
cgeqrf.o : $(mySRCdir)/cgeqrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/cgeqrf.f
ATL_f77wrap_cgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_cgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@  -c -DSCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

zpotri.o : $(mySRCdir)/zpotri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zpotri.f
zgetri.o : $(mySRCdir)/zgetri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgetri.f
ATL_f77wrap_zgetri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getri.c
zlauum.o : $(mySRCdir)/zlauum.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zlauum.f
ATL_f77wrap_zlauum.o : $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_lauum.c
ztrtri.o : $(mySRCdir)/ztrtri.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/ztrtri.f
ATL_f77wrap_ztrtri.o : $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_trtri.c
zgetrs.o : $(mySRCdir)/zgetrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgetrs.f
ATL_f77wrap_zgetrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrs.c
zgetrf.o : $(mySRCdir)/zgetrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgetrf.f
ATL_f77wrap_zgetrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getrf.c
zgesv.o : $(mySRCdir)/zgesv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgesv.f
ATL_f77wrap_zgesv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gesv.c
zpotrs.o : $(mySRCdir)/zpotrs.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zpotrs.f
ATL_f77wrap_zpotrs.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrs.c
zpotrf.o : $(mySRCdir)/zpotrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zpotrf.f
ATL_f77wrap_zpotrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_potrf.c
zposv.o : $(mySRCdir)/zposv.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zposv.f
ATL_f77wrap_zposv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_posv.c
zgels.o : $(mySRCdir)/zgels.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgels.f
ATL_f77wrap_zgels.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gels.c
zlarft.o : $(mySRCdir)/zlarft.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zlarft.f
ATL_f77wrap_zlarft.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larft.c
zlarfb.o : $(mySRCdir)/zlarfb.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zlarfb.f
ATL_f77wrap_zlarfb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_larfb.c
zgelqf.o : $(mySRCdir)/zgelqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgelqf.f
ATL_f77wrap_zgelqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gelqf.c
zgerqf.o : $(mySRCdir)/zgerqf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgerqf.f
ATL_f77wrap_zgerqf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_gerqf.c
zgeqlf.o : $(mySRCdir)/zgeqlf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgeqlf.f
ATL_f77wrap_zgeqlf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqlf.c
zgeqrf.o : $(mySRCdir)/zgeqrf.f
	$(F77) -o $@  -c $(F77FLAGS) $(mySRCdir)/zgeqrf.f
ATL_f77wrap_zgeqrf.o : $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_geqrf.c
ATL_f77wrap_zgetnb.o : $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c
	$(KC) $(KCFLAGS) -o $@  -c -DDCPLX $(mySRCdir)/f77wrap/ATL_f77wrap_getnb.c

ATL_f77wrap_ilaenv.o : $(mySRCdir)/f77wrap/ATL_f77wrap_ilaenv.c
	$(KC) $(KCFLAGS) -o $@  -c $(mySRCdir)/f77wrap/ATL_f77wrap_ilaenv.c
reflarebuild :    # force rebuild of netlib lapack
	rm -f $(FLAdir)/lapack_$(ARCH).a
	$(MAKE) reflabuild
reflapack.grd : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) reflabuild ; \
        fi
	touch reflapack.grd
reflabuild : $(FLAdir)/lapack_$(ARCH).a
	mkdir flapack 
	cd flapack ; $(ARCHIVER) x $(FLAdir)/lapack_$(ARCH).a
	cd flapack ; rm -f ?posv.o ?potrf.o ?potrs.o ?gesv.o ?getrf.o ?getrs.o \
           ?potri.o ?lauum.o ?getri.o ?trtri.o ieeeck.o ilaenv.o [s,d]lamch.o \
           ?geqrf.o ?gerqf.o ?geqlf.o ?gelqf.o ?larfb.o ?larft.o ?gels.o \
           xerbla.o lsame.o
	cd flapack ; $(ARCHIVER) $(ARFLAGS) $(LAPACKlib) *.o
	rm -rf flapack
	$(RANLIB) $(LAPACKlib)

makeincex = $(FLAdir)/make.inc.example
$(FLAdir)/make.inc : $(makeincex)
	rm -f $(FLAdir)/make.inc
	cat $(makeincex) | sed -e 's@PLAT *=.*@PLAT = _$(ARCH)@' \
         -e 's@^FORTRAN *=.*@FORTRAN = $(F77)@' \
         -e 's@^OPTS *=.*@OPTS = $(F77FLAGS)@' \
         -e 's@^NOOPT *=.*@NOOPT = $(F77FLAGS) $(F77NOOPT)@' \
         -e 's@^LOADER *=.*@LOADER = $(F77)@' \
         -e 's@^LOADOPTS *=.*@LOADOPTS = $(F77FLAGS)@' \
         -e 's@^TIMER *=.*@TIMER = NONE@' \
         > $(FLAdir)/make.inc

$(FLAdir)/lapack_$(ARCH).a : $(FLAdir)/make.inc
	cd $(FLAdir) ; $(PMAKE) lib
	if [ -f $(FLAdir)/liblapack.a ]; then \
           ln -s $(FLAdir)/liblapack.a $(FLAdir)/lapack_$(ARCH).a ; \
        fi

slamch.o : $(mySRCdir)/lamch.c
	$(KC) $(KCFLAGS) -o $@ -c -DSREAL $(mySRCdir)/lamch.c
slamc3.o : $(mySRCdir)/slamc3.f
	$(F77) $(F77FLAGS) $(F77NOOPT) -o $@ -c $(mySRCdir)/slamc3.f
dlamch.o : $(mySRCdir)/lamch.c
	$(KC) $(KCFLAGS) -o $@ -c -DDREAL $(mySRCdir)/lamch.c
dlamc3.o : $(mySRCdir)/dlamc3.f
	$(F77) $(F77FLAGS) $(F77NOOPT) -o $@ -c $(mySRCdir)/dlamc3.f

ieeeck.o : $(mySRCdir)/ieeeck.f
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ieeeck.f
ilaenv.o : $(mySRCdir)/ilaenv.f
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ilaenv.f

