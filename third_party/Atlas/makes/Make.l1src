include Make.inc
mySRCdir = $(SRCdir)/src/blas/level1

sobj = ATL_dsdot.o ATL_sdsdot.o ATL_srotg.o ATL_srotm.o ATL_srotmg.o

dobj = ATL_drotg.o ATL_drotm.o ATL_drotmg.o

cobj = ATL_crotg.o

zobj = ATL_zrotg.o


all : lvl1
lib : lvl1
lvl1 : dlvl1 slvl1 zlvl1 clvl1
clean : sclean dclean cclean zclean

sgen :
	$(MAKE) Make_srot
	$(MAKE) -f Make_srot lib
	$(MAKE) Make_saxpby
	$(MAKE) -f Make_saxpby lib
	$(MAKE) Make_scpsc
	$(MAKE) -f Make_scpsc lib
	$(MAKE) Make_sset
	$(MAKE) -f Make_sset lib
	$(MAKE) Make_snrm2
	$(MAKE) -f Make_snrm2 lib
	$(MAKE) Make_sasum
	$(MAKE) -f Make_sasum lib
	$(MAKE) Make_sdot
	$(MAKE) -f Make_sdot lib
	$(MAKE) Make_sswap
	$(MAKE) -f Make_sswap lib
	$(MAKE) Make_sscal
	$(MAKE) -f Make_sscal lib
	$(MAKE) Make_scopy
	$(MAKE) -f Make_scopy lib
	$(MAKE) Make_saxpy
	$(MAKE) -f Make_saxpy lib
	$(MAKE) Make_siamax
	$(MAKE) -f Make_siamax lib
slvl1 : slib
slib :   slib.grd
	$(MAKE) sgen
	$(PMAKE) slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
sclean :
	rm -f $(sobj) s*.grd
dgen :
	$(MAKE) Make_drot
	$(MAKE) -f Make_drot lib
	$(MAKE) Make_daxpby
	$(MAKE) -f Make_daxpby lib
	$(MAKE) Make_dcpsc
	$(MAKE) -f Make_dcpsc lib
	$(MAKE) Make_dset
	$(MAKE) -f Make_dset lib
	$(MAKE) Make_dnrm2
	$(MAKE) -f Make_dnrm2 lib
	$(MAKE) Make_dasum
	$(MAKE) -f Make_dasum lib
	$(MAKE) Make_ddot
	$(MAKE) -f Make_ddot lib
	$(MAKE) Make_dswap
	$(MAKE) -f Make_dswap lib
	$(MAKE) Make_dscal
	$(MAKE) -f Make_dscal lib
	$(MAKE) Make_dcopy
	$(MAKE) -f Make_dcopy lib
	$(MAKE) Make_daxpy
	$(MAKE) -f Make_daxpy lib
	$(MAKE) Make_diamax
	$(MAKE) -f Make_diamax lib
dlvl1 : dlib
dlib :   dlib.grd
	$(MAKE) dgen
	$(PMAKE) dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
dclean :
	rm -f $(dobj) d*.grd
cgen :
	$(MAKE) Make_crot
	$(MAKE) -f Make_crot lib
	$(MAKE) Make_caxpby
	$(MAKE) -f Make_caxpby lib
	$(MAKE) Make_ccpsc
	$(MAKE) -f Make_ccpsc lib
	$(MAKE) Make_cset
	$(MAKE) -f Make_cset lib
	$(MAKE) Make_cnrm2
	$(MAKE) -f Make_cnrm2 lib
	$(MAKE) Make_casum
	$(MAKE) -f Make_casum lib
	$(MAKE) Make_cdot
	$(MAKE) -f Make_cdot lib
	$(MAKE) Make_cswap
	$(MAKE) -f Make_cswap lib
	$(MAKE) Make_cscal
	$(MAKE) -f Make_cscal lib
	$(MAKE) Make_ccopy
	$(MAKE) -f Make_ccopy lib
	$(MAKE) Make_caxpy
	$(MAKE) -f Make_caxpy lib
	$(MAKE) Make_ciamax
	$(MAKE) -f Make_ciamax lib
	$(MAKE) Make_cdotc
	$(MAKE) -f Make_cdotc lib
clvl1 : clib
clib : slib clib.grd
	$(MAKE) cgen
	$(PMAKE) clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
cclean :
	rm -f $(cobj) c*.grd
zgen :
	$(MAKE) Make_zrot
	$(MAKE) -f Make_zrot lib
	$(MAKE) Make_zaxpby
	$(MAKE) -f Make_zaxpby lib
	$(MAKE) Make_zcpsc
	$(MAKE) -f Make_zcpsc lib
	$(MAKE) Make_zset
	$(MAKE) -f Make_zset lib
	$(MAKE) Make_znrm2
	$(MAKE) -f Make_znrm2 lib
	$(MAKE) Make_zasum
	$(MAKE) -f Make_zasum lib
	$(MAKE) Make_zdot
	$(MAKE) -f Make_zdot lib
	$(MAKE) Make_zswap
	$(MAKE) -f Make_zswap lib
	$(MAKE) Make_zscal
	$(MAKE) -f Make_zscal lib
	$(MAKE) Make_zcopy
	$(MAKE) -f Make_zcopy lib
	$(MAKE) Make_zaxpy
	$(MAKE) -f Make_zaxpy lib
	$(MAKE) Make_ziamax
	$(MAKE) -f Make_ziamax lib
	$(MAKE) Make_zdotc
	$(MAKE) -f Make_zdotc lib
zlvl1 : zlib
zlib : dlib zlib.grd
	$(MAKE) zgen
	$(PMAKE) zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
zclean :
	rm -f $(zobj) z*.grd
#
# Routines with regular real & complex
#
Make_srot :
	cd $(L1Tdir) ; $(MAKE) sinstall_rot
Make_saxpby :
	cd $(L1Tdir) ; $(MAKE) sinstall_axpby
Make_scpsc :
	cd $(L1Tdir) ; $(MAKE) sinstall_cpsc
Make_sset :
	cd $(L1Tdir) ; $(MAKE) sinstall_set
Make_snrm2 :
	cd $(L1Tdir) ; $(MAKE) sinstall_nrm2
Make_sasum :
	cd $(L1Tdir) ; $(MAKE) sinstall_asum
Make_sswap :
	cd $(L1Tdir) ; $(MAKE) sinstall_swap
Make_sdot :
	cd $(L1Tdir) ; $(MAKE) sinstall_dot
Make_sscal :
	cd $(L1Tdir) ; $(MAKE) sinstall_scal
Make_scopy :
	cd $(L1Tdir) ; $(MAKE) sinstall_copy
Make_siamax :
	cd $(L1Tdir) ; $(MAKE) sinstall_iamax
Make_saxpy :
	cd $(L1Tdir) ; $(MAKE) sinstall_axpy
ATL_srotg.o : $(mySRCdir)/ATL_rotg.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_rotg.c
ATL_sswap.o : $(mySRCdir)/ATL_swap.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_swap.c

Make_drot :
	cd $(L1Tdir) ; $(MAKE) dinstall_rot
Make_daxpby :
	cd $(L1Tdir) ; $(MAKE) dinstall_axpby
Make_dcpsc :
	cd $(L1Tdir) ; $(MAKE) dinstall_cpsc
Make_dset :
	cd $(L1Tdir) ; $(MAKE) dinstall_set
Make_dnrm2 :
	cd $(L1Tdir) ; $(MAKE) dinstall_nrm2
Make_dasum :
	cd $(L1Tdir) ; $(MAKE) dinstall_asum
Make_dswap :
	cd $(L1Tdir) ; $(MAKE) dinstall_swap
Make_ddot :
	cd $(L1Tdir) ; $(MAKE) dinstall_dot
Make_dscal :
	cd $(L1Tdir) ; $(MAKE) dinstall_scal
Make_dcopy :
	cd $(L1Tdir) ; $(MAKE) dinstall_copy
Make_diamax :
	cd $(L1Tdir) ; $(MAKE) dinstall_iamax
Make_daxpy :
	cd $(L1Tdir) ; $(MAKE) dinstall_axpy
ATL_drotg.o : $(mySRCdir)/ATL_rotg.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_rotg.c
ATL_dswap.o : $(mySRCdir)/ATL_swap.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_swap.c

Make_crot :
	cd $(L1Tdir) ; $(MAKE) cinstall_rot
Make_caxpby :
	cd $(L1Tdir) ; $(MAKE) cinstall_axpby
Make_ccpsc :
	cd $(L1Tdir) ; $(MAKE) cinstall_cpsc
Make_cset :
	cd $(L1Tdir) ; $(MAKE) cinstall_set
Make_cnrm2 :
	cd $(L1Tdir) ; $(MAKE) cinstall_nrm2
Make_casum :
	cd $(L1Tdir) ; $(MAKE) cinstall_asum
Make_cswap :
	cd $(L1Tdir) ; $(MAKE) cinstall_swap
Make_cdot :
	cd $(L1Tdir) ; $(MAKE) cinstall_dot
Make_cscal :
	cd $(L1Tdir) ; $(MAKE) cinstall_scal
Make_ccopy :
	cd $(L1Tdir) ; $(MAKE) cinstall_copy
Make_ciamax :
	cd $(L1Tdir) ; $(MAKE) cinstall_iamax
Make_caxpy :
	cd $(L1Tdir) ; $(MAKE) cinstall_axpy
ATL_crotg.o : $(mySRCdir)/ATL_rotg.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_rotg.c
ATL_cswap.o : $(mySRCdir)/ATL_swap.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_swap.c

Make_zrot :
	cd $(L1Tdir) ; $(MAKE) zinstall_rot
Make_zaxpby :
	cd $(L1Tdir) ; $(MAKE) zinstall_axpby
Make_zcpsc :
	cd $(L1Tdir) ; $(MAKE) zinstall_cpsc
Make_zset :
	cd $(L1Tdir) ; $(MAKE) zinstall_set
Make_znrm2 :
	cd $(L1Tdir) ; $(MAKE) zinstall_nrm2
Make_zasum :
	cd $(L1Tdir) ; $(MAKE) zinstall_asum
Make_zswap :
	cd $(L1Tdir) ; $(MAKE) zinstall_swap
Make_zdot :
	cd $(L1Tdir) ; $(MAKE) zinstall_dot
Make_zscal :
	cd $(L1Tdir) ; $(MAKE) zinstall_scal
Make_zcopy :
	cd $(L1Tdir) ; $(MAKE) zinstall_copy
Make_ziamax :
	cd $(L1Tdir) ; $(MAKE) zinstall_iamax
Make_zaxpy :
	cd $(L1Tdir) ; $(MAKE) zinstall_axpy
ATL_zrotg.o : $(mySRCdir)/ATL_rotg.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_rotg.c
ATL_zswap.o : $(mySRCdir)/ATL_swap.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_swap.c

#
# Real routines
#
ATL_dsdot.o : $(mySRCdir)/ATL_dsdot.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DSREAL $(mySRCdir)/ATL_dsdot.c
ATL_sdsdot.o : $(mySRCdir)/ATL_sdsdot.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DSREAL $(mySRCdir)/ATL_sdsdot.c

ATL_sasum.o : $(mySRCdir)/ATL_asum.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_asum.c
ATL_snrm2.o : $(mySRCdir)/ATL_nrm2.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_nrm2.c
ATL_sdot.o : $(mySRCdir)/ATL_dot.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_dot.c
ATL_srotm.o : $(mySRCdir)/ATL_rotm.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_rotm.c
ATL_srot.o : $(mySRCdir)/ATL_rot.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_rot.c
ATL_srotmg.o : $(mySRCdir)/ATL_rotmg.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_rotmg.c

ATL_dasum.o : $(mySRCdir)/ATL_asum.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_asum.c
ATL_dnrm2.o : $(mySRCdir)/ATL_nrm2.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_nrm2.c
ATL_ddot.o : $(mySRCdir)/ATL_dot.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_dot.c
ATL_drotm.o : $(mySRCdir)/ATL_rotm.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_rotm.c
ATL_drot.o : $(mySRCdir)/ATL_rot.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_rot.c
ATL_drotmg.o : $(mySRCdir)/ATL_rotmg.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_rotmg.c

#
# Complex routines
#
Make_cdotc :
	cd $(L1Tdir) ; $(MAKE) cinstall_dotc
ATL_scrot.o : $(mySRCdir)/ATL_rot.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_rot.c
ATL_scasum.o : $(mySRCdir)/ATL_asum.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_asum.c
ATL_scnrm2.o : $(mySRCdir)/ATL_nrm2.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_nrm2.c
ATL_cdotc.o : $(mySRCdir)/ATL_dotc.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_dotc.c
ATL_cdotu.o : $(mySRCdir)/ATL_dotu.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_dotu.c

Make_zdotc :
	cd $(L1Tdir) ; $(MAKE) zinstall_dotc
ATL_dzrot.o : $(mySRCdir)/ATL_rot.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_rot.c
ATL_dzasum.o : $(mySRCdir)/ATL_asum.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_asum.c
ATL_dznrm2.o : $(mySRCdir)/ATL_nrm2.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_nrm2.c
ATL_zdotc.o : $(mySRCdir)/ATL_dotc.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_dotc.c
ATL_zdotu.o : $(mySRCdir)/ATL_dotu.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_dotu.c

