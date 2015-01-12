include Make.inc
mySRCdir = $(SRCdir)/interfaces/lapack/C/src

iobj = clapack_ilaenv.o
itobj = clapack_itlaenv.o
stobj = clapack_sgetri.o clapack_sgetrs.o clapack_slamch.o clapack_slauum.o \
        clapack_sposv.o clapack_spotrf.o clapack_spotri.o clapack_spotrs.o \
        clapack_stgelqf.o clapack_stgels.o clapack_stgeqlf.o \
        clapack_stgeqrf.o clapack_stgerqf.o clapack_stgesv.o \
        clapack_stgetrf.o clapack_strtri.o

dtobj = clapack_dgetri.o clapack_dgetrs.o clapack_dlamch.o clapack_dlauum.o \
        clapack_dposv.o clapack_dpotrf.o clapack_dpotri.o clapack_dpotrs.o \
        clapack_dtgelqf.o clapack_dtgels.o clapack_dtgeqlf.o \
        clapack_dtgeqrf.o clapack_dtgerqf.o clapack_dtgesv.o \
        clapack_dtgetrf.o clapack_dtrtri.o

ctobj = clapack_cgetri.o clapack_cgetrs.o clapack_clauum.o clapack_cposv.o \
        clapack_cpotrf.o clapack_cpotri.o clapack_cpotrs.o clapack_ctgelqf.o \
        clapack_ctgels.o clapack_ctgeqlf.o clapack_ctgeqrf.o \
        clapack_ctgerqf.o clapack_ctgesv.o clapack_ctgetrf.o clapack_ctrtri.o

ztobj = clapack_zgetri.o clapack_zgetrs.o clapack_zlauum.o clapack_zposv.o \
        clapack_zpotrf.o clapack_zpotri.o clapack_zpotrs.o clapack_ztgelqf.o \
        clapack_ztgels.o clapack_ztgeqlf.o clapack_ztgeqrf.o \
        clapack_ztgerqf.o clapack_ztgesv.o clapack_ztgetrf.o clapack_ztrtri.o

sobj = clapack_sgelqf.o clapack_sgels.o clapack_sgeqlf.o clapack_sgeqrf.o \
       clapack_sgerqf.o clapack_sgesv.o clapack_sgetrf.o clapack_sgetri.o \
       clapack_sgetrs.o clapack_slamch.o clapack_slauum.o clapack_sposv.o \
       clapack_spotrf.o clapack_spotri.o clapack_spotrs.o clapack_strtri.o

dobj = clapack_dgelqf.o clapack_dgels.o clapack_dgeqlf.o clapack_dgeqrf.o \
       clapack_dgerqf.o clapack_dgesv.o clapack_dgetrf.o clapack_dgetri.o \
       clapack_dgetrs.o clapack_dlamch.o clapack_dlauum.o clapack_dposv.o \
       clapack_dpotrf.o clapack_dpotri.o clapack_dpotrs.o clapack_dtrtri.o

cobj = clapack_cgelqf.o clapack_cgels.o clapack_cgeqlf.o clapack_cgeqrf.o \
       clapack_cgerqf.o clapack_cgesv.o clapack_cgetrf.o clapack_cgetri.o \
       clapack_cgetrs.o clapack_clauum.o clapack_cposv.o clapack_cpotrf.o \
       clapack_cpotri.o clapack_cpotrs.o clapack_ctrtri.o

zobj = clapack_zgelqf.o clapack_zgels.o clapack_zgeqlf.o clapack_zgeqrf.o \
       clapack_zgerqf.o clapack_zgesv.o clapack_zgetrf.o clapack_zgetri.o \
       clapack_zgetrs.o clapack_zlauum.o clapack_zposv.o clapack_zpotrf.o \
       clapack_zpotri.o clapack_zpotrs.o clapack_ztrtri.o

all : lib ptlib
lib : dlib slib zlib clib ilib
ptlib : tlib
tlib : dtlib stlib ztlib ctlib itlib
tclean :  dtclean stclean ztclean ctclean itclean
clean : dclean sclean zclean cclean iclean

itclean :
	rm -f $(itobj) itlib.grd
itlib :
	$(PMAKE) itlib.grd
itlib.grd : $(itobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(itobj)
	$(RANLIB) $(PTLAPACKlib)
	touch itlib.grd

stclean :
	rm -f $(stobj) stlib.grd
stlib :
	$(PMAKE) stlib.grd
stlib.grd : $(stobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(stobj)
	$(RANLIB) $(PTLAPACKlib)
	touch stlib.grd

dtclean :
	rm -f $(dtobj) dtlib.grd
dtlib :
	$(PMAKE) dtlib.grd
dtlib.grd : $(dtobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(dtobj)
	$(RANLIB) $(PTLAPACKlib)
	touch dtlib.grd

ctclean :
	rm -f $(ctobj) ctlib.grd
ctlib :
	$(PMAKE) ctlib.grd
ctlib.grd : $(ctobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(ctobj)
	$(RANLIB) $(PTLAPACKlib)
	touch ctlib.grd

ztclean :
	rm -f $(ztobj) ztlib.grd
ztlib :
	$(PMAKE) ztlib.grd
ztlib.grd : $(ztobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(ztobj)
	$(RANLIB) $(PTLAPACKlib)
	touch ztlib.grd


clapack_stgetri.o : $(mySRCdir)/clapack_sgetri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgetri.c
clapack_stpotri.o : $(mySRCdir)/clapack_spotri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_spotri.c
clapack_stlauum.o : $(mySRCdir)/clapack_slauum.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_slauum.c
clapack_sttrtri.o : $(mySRCdir)/clapack_strtri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_strtri.c
clapack_stgetrs.o : $(mySRCdir)/clapack_sgetrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgetrs.c
clapack_stgetrf.o : $(mySRCdir)/clapack_sgetrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgetrf.c
clapack_stgesv.o : $(mySRCdir)/clapack_sgesv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgesv.c
clapack_stpotrs.o : $(mySRCdir)/clapack_spotrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_spotrs.c
clapack_stpotrf.o : $(mySRCdir)/clapack_spotrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_spotrf.c
clapack_stposv.o : $(mySRCdir)/clapack_sposv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sposv.c
clapack_stgels.o : $(mySRCdir)/clapack_sgels.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgels.c
clapack_stgelqf.o : $(mySRCdir)/clapack_sgelqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgelqf.c
clapack_stgerqf.o : $(mySRCdir)/clapack_sgerqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgerqf.c
clapack_stgeqlf.o : $(mySRCdir)/clapack_sgeqlf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgeqlf.c
clapack_stgeqrf.o : $(mySRCdir)/clapack_sgeqrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_sgeqrf.c
clapack_stlamch.o : $(mySRCdir)/clapack_slamch.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_slamch.c
clapack_dtgetri.o : $(mySRCdir)/clapack_dgetri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgetri.c
clapack_dtpotri.o : $(mySRCdir)/clapack_dpotri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dpotri.c
clapack_dtlauum.o : $(mySRCdir)/clapack_dlauum.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dlauum.c
clapack_dttrtri.o : $(mySRCdir)/clapack_dtrtri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dtrtri.c
clapack_dtgetrs.o : $(mySRCdir)/clapack_dgetrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgetrs.c
clapack_dtgetrf.o : $(mySRCdir)/clapack_dgetrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgetrf.c
clapack_dtgesv.o : $(mySRCdir)/clapack_dgesv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgesv.c
clapack_dtpotrs.o : $(mySRCdir)/clapack_dpotrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dpotrs.c
clapack_dtpotrf.o : $(mySRCdir)/clapack_dpotrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dpotrf.c
clapack_dtposv.o : $(mySRCdir)/clapack_dposv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dposv.c
clapack_dtgels.o : $(mySRCdir)/clapack_dgels.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgels.c
clapack_dtgelqf.o : $(mySRCdir)/clapack_dgelqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgelqf.c
clapack_dtgerqf.o : $(mySRCdir)/clapack_dgerqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgerqf.c
clapack_dtgeqlf.o : $(mySRCdir)/clapack_dgeqlf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgeqlf.c
clapack_dtgeqrf.o : $(mySRCdir)/clapack_dgeqrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dgeqrf.c
clapack_dtlamch.o : $(mySRCdir)/clapack_dlamch.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_dlamch.c
clapack_ctgetri.o : $(mySRCdir)/clapack_cgetri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgetri.c
clapack_ctpotri.o : $(mySRCdir)/clapack_cpotri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cpotri.c
clapack_ctlauum.o : $(mySRCdir)/clapack_clauum.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_clauum.c
clapack_cttrtri.o : $(mySRCdir)/clapack_ctrtri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_ctrtri.c
clapack_ctgetrs.o : $(mySRCdir)/clapack_cgetrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgetrs.c
clapack_ctgetrf.o : $(mySRCdir)/clapack_cgetrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgetrf.c
clapack_ctgesv.o : $(mySRCdir)/clapack_cgesv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgesv.c
clapack_ctpotrs.o : $(mySRCdir)/clapack_cpotrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cpotrs.c
clapack_ctpotrf.o : $(mySRCdir)/clapack_cpotrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cpotrf.c
clapack_ctposv.o : $(mySRCdir)/clapack_cposv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cposv.c
clapack_ctgels.o : $(mySRCdir)/clapack_cgels.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgels.c
clapack_ctgelqf.o : $(mySRCdir)/clapack_cgelqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgelqf.c
clapack_ctgerqf.o : $(mySRCdir)/clapack_cgerqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgerqf.c
clapack_ctgeqlf.o : $(mySRCdir)/clapack_cgeqlf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgeqlf.c
clapack_ctgeqrf.o : $(mySRCdir)/clapack_cgeqrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_cgeqrf.c
clapack_ctlamch.o : $(mySRCdir)/clapack_clamch.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_clamch.c
clapack_ztgetri.o : $(mySRCdir)/clapack_zgetri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgetri.c
clapack_ztpotri.o : $(mySRCdir)/clapack_zpotri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zpotri.c
clapack_ztlauum.o : $(mySRCdir)/clapack_zlauum.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zlauum.c
clapack_zttrtri.o : $(mySRCdir)/clapack_ztrtri.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_ztrtri.c
clapack_ztgetrs.o : $(mySRCdir)/clapack_zgetrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgetrs.c
clapack_ztgetrf.o : $(mySRCdir)/clapack_zgetrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgetrf.c
clapack_ztgesv.o : $(mySRCdir)/clapack_zgesv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgesv.c
clapack_ztpotrs.o : $(mySRCdir)/clapack_zpotrs.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zpotrs.c
clapack_ztpotrf.o : $(mySRCdir)/clapack_zpotrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zpotrf.c
clapack_ztposv.o : $(mySRCdir)/clapack_zposv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zposv.c
clapack_ztgels.o : $(mySRCdir)/clapack_zgels.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgels.c
clapack_ztgelqf.o : $(mySRCdir)/clapack_zgelqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgelqf.c
clapack_ztgerqf.o : $(mySRCdir)/clapack_zgerqf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgerqf.c
clapack_ztgeqlf.o : $(mySRCdir)/clapack_zgeqlf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgeqlf.c
clapack_ztgeqrf.o : $(mySRCdir)/clapack_zgeqrf.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zgeqrf.c
clapack_ztlamch.o : $(mySRCdir)/clapack_zlamch.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_zlamch.c
clapack_itlaenv.o : $(mySRCdir)/clapack_ilaenv.c
	$(ICC) $(ICCFLAGS) -DATL_USEPTHREADS -o $@ -c $(mySRCdir)/clapack_ilaenv.c
iclean :
	rm -f $(iobj) ilib.grd
ilib :
	$(PMAKE) ilib.grd
ilib.grd : $(iobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(iobj)
	$(RANLIB) $(LAPACKlib)
	touch ilib.grd

sclean :
	rm -f $(sobj) slib.grd
slib :
	$(PMAKE) slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(sobj)
	$(RANLIB) $(LAPACKlib)
	touch slib.grd

dclean :
	rm -f $(dobj) dlib.grd
dlib :
	$(PMAKE) dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(dobj)
	$(RANLIB) $(LAPACKlib)
	touch dlib.grd

cclean :
	rm -f $(cobj) clib.grd
clib :
	$(PMAKE) clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(cobj)
	$(RANLIB) $(LAPACKlib)
	touch clib.grd

zclean :
	rm -f $(zobj) zlib.grd
zlib :
	$(PMAKE) zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(zobj)
	$(RANLIB) $(LAPACKlib)
	touch zlib.grd


clapack_sgetri.o : $(mySRCdir)/clapack_sgetri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgetri.c
clapack_spotri.o : $(mySRCdir)/clapack_spotri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_spotri.c
clapack_slauum.o : $(mySRCdir)/clapack_slauum.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_slauum.c
clapack_strtri.o : $(mySRCdir)/clapack_strtri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_strtri.c
clapack_sgetrs.o : $(mySRCdir)/clapack_sgetrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgetrs.c
clapack_sgetrf.o : $(mySRCdir)/clapack_sgetrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgetrf.c
clapack_sgesv.o : $(mySRCdir)/clapack_sgesv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgesv.c
clapack_spotrs.o : $(mySRCdir)/clapack_spotrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_spotrs.c
clapack_spotrf.o : $(mySRCdir)/clapack_spotrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_spotrf.c
clapack_sposv.o : $(mySRCdir)/clapack_sposv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sposv.c
clapack_sgels.o : $(mySRCdir)/clapack_sgels.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgels.c
clapack_sgelqf.o : $(mySRCdir)/clapack_sgelqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgelqf.c
clapack_sgerqf.o : $(mySRCdir)/clapack_sgerqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgerqf.c
clapack_sgeqlf.o : $(mySRCdir)/clapack_sgeqlf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgeqlf.c
clapack_sgeqrf.o : $(mySRCdir)/clapack_sgeqrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_sgeqrf.c
clapack_slamch.o : $(mySRCdir)/clapack_slamch.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_slamch.c
clapack_dgetri.o : $(mySRCdir)/clapack_dgetri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgetri.c
clapack_dpotri.o : $(mySRCdir)/clapack_dpotri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dpotri.c
clapack_dlauum.o : $(mySRCdir)/clapack_dlauum.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dlauum.c
clapack_dtrtri.o : $(mySRCdir)/clapack_dtrtri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dtrtri.c
clapack_dgetrs.o : $(mySRCdir)/clapack_dgetrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgetrs.c
clapack_dgetrf.o : $(mySRCdir)/clapack_dgetrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgetrf.c
clapack_dgesv.o : $(mySRCdir)/clapack_dgesv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgesv.c
clapack_dpotrs.o : $(mySRCdir)/clapack_dpotrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dpotrs.c
clapack_dpotrf.o : $(mySRCdir)/clapack_dpotrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dpotrf.c
clapack_dposv.o : $(mySRCdir)/clapack_dposv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dposv.c
clapack_dgels.o : $(mySRCdir)/clapack_dgels.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgels.c
clapack_dgelqf.o : $(mySRCdir)/clapack_dgelqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgelqf.c
clapack_dgerqf.o : $(mySRCdir)/clapack_dgerqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgerqf.c
clapack_dgeqlf.o : $(mySRCdir)/clapack_dgeqlf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgeqlf.c
clapack_dgeqrf.o : $(mySRCdir)/clapack_dgeqrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dgeqrf.c
clapack_dlamch.o : $(mySRCdir)/clapack_dlamch.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_dlamch.c
clapack_cgetri.o : $(mySRCdir)/clapack_cgetri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgetri.c
clapack_cpotri.o : $(mySRCdir)/clapack_cpotri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cpotri.c
clapack_clauum.o : $(mySRCdir)/clapack_clauum.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_clauum.c
clapack_ctrtri.o : $(mySRCdir)/clapack_ctrtri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_ctrtri.c
clapack_cgetrs.o : $(mySRCdir)/clapack_cgetrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgetrs.c
clapack_cgetrf.o : $(mySRCdir)/clapack_cgetrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgetrf.c
clapack_cgesv.o : $(mySRCdir)/clapack_cgesv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgesv.c
clapack_cpotrs.o : $(mySRCdir)/clapack_cpotrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cpotrs.c
clapack_cpotrf.o : $(mySRCdir)/clapack_cpotrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cpotrf.c
clapack_cposv.o : $(mySRCdir)/clapack_cposv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cposv.c
clapack_cgels.o : $(mySRCdir)/clapack_cgels.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgels.c
clapack_cgelqf.o : $(mySRCdir)/clapack_cgelqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgelqf.c
clapack_cgerqf.o : $(mySRCdir)/clapack_cgerqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgerqf.c
clapack_cgeqlf.o : $(mySRCdir)/clapack_cgeqlf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgeqlf.c
clapack_cgeqrf.o : $(mySRCdir)/clapack_cgeqrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_cgeqrf.c
clapack_clamch.o : $(mySRCdir)/clapack_clamch.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_clamch.c
clapack_zgetri.o : $(mySRCdir)/clapack_zgetri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgetri.c
clapack_zpotri.o : $(mySRCdir)/clapack_zpotri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zpotri.c
clapack_zlauum.o : $(mySRCdir)/clapack_zlauum.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zlauum.c
clapack_ztrtri.o : $(mySRCdir)/clapack_ztrtri.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_ztrtri.c
clapack_zgetrs.o : $(mySRCdir)/clapack_zgetrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgetrs.c
clapack_zgetrf.o : $(mySRCdir)/clapack_zgetrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgetrf.c
clapack_zgesv.o : $(mySRCdir)/clapack_zgesv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgesv.c
clapack_zpotrs.o : $(mySRCdir)/clapack_zpotrs.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zpotrs.c
clapack_zpotrf.o : $(mySRCdir)/clapack_zpotrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zpotrf.c
clapack_zposv.o : $(mySRCdir)/clapack_zposv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zposv.c
clapack_zgels.o : $(mySRCdir)/clapack_zgels.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgels.c
clapack_zgelqf.o : $(mySRCdir)/clapack_zgelqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgelqf.c
clapack_zgerqf.o : $(mySRCdir)/clapack_zgerqf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgerqf.c
clapack_zgeqlf.o : $(mySRCdir)/clapack_zgeqlf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgeqlf.c
clapack_zgeqrf.o : $(mySRCdir)/clapack_zgeqrf.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zgeqrf.c
clapack_zlamch.o : $(mySRCdir)/clapack_zlamch.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_zlamch.c
clapack_ilaenv.o : $(mySRCdir)/clapack_ilaenv.c
	$(ICC) $(ICCFLAGS)  -o $@ -c $(mySRCdir)/clapack_ilaenv.c
