# =========================================================================
# Note that this Makefile compiles only the GER driver routines.  The 
# empirically tuned kernel files are compiled by the generated Make_<pre>r1
# helper makefiles
# =========================================================================
include Make.inc
mySRCdir = $(SRCdir)/src/blas/ger

sobj = ATL_sger.o ATL_sger_L1.o ATL_sger_L2.o \
       ATL_sger_OOC.o ATL_sgerk_Mlt16.o ATL_sgerk_axpy.o \
       ATL_sger2.o ATL_sger2_L1.o ATL_sger2_L2.o \
       ATL_sger2_OOC.o ATL_sger2k_Mlt16.o ATL_sger2k_Nlt8.o 
dobj = ATL_dger.o ATL_dger_L1.o ATL_dger_L2.o \
       ATL_dger_OOC.o ATL_dgerk_Mlt16.o ATL_dgerk_axpy.o \
       ATL_dger2.o ATL_dger2_L1.o ATL_dger2_L2.o \
       ATL_dger2_OOC.o ATL_dger2k_Mlt16.o ATL_dger2k_Nlt8.o 
cobj = ATL_cgeru.o ATL_cgerc.o \
       ATL_cgeru_L1.o ATL_cgerc_L1.o \
       ATL_cgeru_L2.o ATL_cgerc_L2.o \
       ATL_cgeru_OOC.o ATL_cgerc_OOC.o \
       ATL_cgerk_Mlt16.o ATL_cgerk_axpy.o \
       ATL_cger2u.o ATL_cger2c.o \
       ATL_cger2u_OOC.o ATL_cger2c_OOC.o \
       ATL_cger2u_L1.o ATL_cger2c_L1.o \
       ATL_cger2u_L2.o ATL_cger2c_L2.o \
       ATL_cgerck_Mlt16.o ATL_cgerck_axpy.o \
       ATL_cger2k_Mlt16.o ATL_cger2k_Nlt8.o \
       ATL_cger2ck_Mlt16.o ATL_cger2ck_Nlt8.o 
zobj = ATL_zgeru.o ATL_zgerc.o \
       ATL_zgeru_L1.o ATL_zgerc_L1.o \
       ATL_zgeru_L2.o ATL_zgerc_L2.o \
       ATL_zgeru_OOC.o ATL_zgerc_OOC.o \
       ATL_zgerk_Mlt16.o ATL_zgerk_axpy.o \
       ATL_zger2u.o ATL_zger2c.o \
       ATL_zger2u_OOC.o ATL_zger2c_OOC.o \
       ATL_zger2u_L1.o ATL_zger2c_L1.o \
       ATL_zger2u_L2.o ATL_zger2c_L2.o \
       ATL_zgerck_Mlt16.o ATL_zgerck_axpy.o \
       ATL_zger2k_Mlt16.o ATL_zger2k_Nlt8.o \
       ATL_zger2ck_Mlt16.o ATL_zger2ck_Nlt8.o 

lib : all
all : slib dlib clib zlib
lvl2 : all
dlvl2 : dlib
slvl2 : slib
clvl2 : clib
zlvl2 : zlib
clean : sclean dclean cclean zclean

dlib :
	$(PMAKE) dlib.grd
	$(PMAKE) -f Make_dr1 lib
	$(PMAKE) -f Make_dr2 lib
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
dclean : 
	rm -f $(dobj) d*.grd
slib :
	$(PMAKE) slib.grd
	$(PMAKE) -f Make_sr1 lib
	$(PMAKE) -f Make_sr2 lib
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
sclean : 
	rm -f $(sobj) s*.grd
clib :
	$(PMAKE) clib.grd
	$(PMAKE) -f Make_cr1 lib
	$(PMAKE) -f Make_cr2 lib
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
cclean : 
	rm -f $(cobj) c*.grd
zlib :
	$(PMAKE) zlib.grd
	$(PMAKE) -f Make_zr1 lib
	$(PMAKE) -f Make_zr2 lib
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
zclean : 
	rm -f $(zobj) z*.grd

ATL_sger2_OOC.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_sr2.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INOOC $(mySRCdir)/ATL_ger2.c
ATL_sger2_L2.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_sr2.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL2 $(mySRCdir)/ATL_ger2.c
ATL_sger2_L1.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_sr2.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL1 $(mySRCdir)/ATL_ger2.c
ATL_sger2.o : $(mySRCdir)/ATL_ger2.c $(INCSdir)/atlas_r2.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_ger2.c
ATL_dger2_OOC.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_dr2.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INOOC $(mySRCdir)/ATL_ger2.c
ATL_dger2_L2.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_dr2.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL2 $(mySRCdir)/ATL_ger2.c
ATL_dger2_L1.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_dr2.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL1 $(mySRCdir)/ATL_ger2.c
ATL_dger2.o : $(mySRCdir)/ATL_ger2.c $(INCSdir)/atlas_r2.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_ger2.c

ATL_cger2u_OOC.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_cr2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INOOC $(mySRCdir)/ATL_ger2.c
ATL_cger2c_OOC.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_cr2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INOOC \
        -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_cger2u_L2.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_cr2_L2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 $(mySRCdir)/ATL_ger2.c
ATL_cger2c_L2.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_cr2_L2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 \
        -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_cger2u_L1.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_cr2_L1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 $(mySRCdir)/ATL_ger2.c
ATL_cger2c_L1.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_cr2_L1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 \
        -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_cger2u.o : $(mySRCdir)/ATL_ger2.c $(INCSdir)/atlas_r2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_ger2.c
ATL_cger2c.o : $(mySRCdir)/ATL_ger2.c $(INCSdir)/atlas_r2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_zger2u_OOC.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_zr2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INOOC $(mySRCdir)/ATL_ger2.c
ATL_zger2c_OOC.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_zr2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INOOC \
        -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_zger2u_L2.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_zr2_L2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 $(mySRCdir)/ATL_ger2.c
ATL_zger2c_L2.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_zr2_L2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 \
        -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_zger2u_L1.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_zr2_L1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 $(mySRCdir)/ATL_ger2.c
ATL_zger2c_L1.o : $(mySRCdir)/ATL_ger2.c $(INCAdir)/atlas_zr2_L1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 \
        -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_zger2u.o : $(mySRCdir)/ATL_ger2.c $(INCSdir)/atlas_r2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_ger2.c
ATL_zger2c.o : $(mySRCdir)/ATL_ger2.c $(INCSdir)/atlas_r2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConj_ $(mySRCdir)/ATL_ger2.c
ATL_sger_OOC.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_sr1.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INOOC $(mySRCdir)/ATL_ger.c
ATL_sger_L2.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_sr1.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL2 $(mySRCdir)/ATL_ger.c
ATL_sger_L1.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_sr1.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL1 $(mySRCdir)/ATL_ger.c
ATL_sger.o : $(mySRCdir)/ATL_ger.c $(INCSdir)/atlas_r1.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_ger.c
ATL_dger_OOC.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_dr1.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INOOC $(mySRCdir)/ATL_ger.c
ATL_dger_L2.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_dr1.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL2 $(mySRCdir)/ATL_ger.c
ATL_dger_L1.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_dr1.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL1 $(mySRCdir)/ATL_ger.c
ATL_dger.o : $(mySRCdir)/ATL_ger.c $(INCSdir)/atlas_r1.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_ger.c

ATL_cgeru_OOC.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_cr1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INOOC $(mySRCdir)/ATL_ger.c
ATL_cgerc_OOC.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_cr1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INOOC \
        -DConj_ $(mySRCdir)/ATL_ger.c
ATL_cgeru_L2.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_cr1_L2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 $(mySRCdir)/ATL_ger.c
ATL_cgerc_L2.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_cr1_L2.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 \
        -DConj_ $(mySRCdir)/ATL_ger.c
ATL_cgeru_L1.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_cr1_L1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 $(mySRCdir)/ATL_ger.c
ATL_cgerc_L1.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_cr1_L1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 \
        -DConj_ $(mySRCdir)/ATL_ger.c
ATL_cgeru.o : $(mySRCdir)/ATL_ger.c $(INCSdir)/atlas_r1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_ger.c
ATL_cgerc.o : $(mySRCdir)/ATL_ger.c $(INCSdir)/atlas_r1.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConj_ $(mySRCdir)/ATL_ger.c
ATL_zgeru_OOC.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_zr1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INOOC $(mySRCdir)/ATL_ger.c
ATL_zgerc_OOC.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_zr1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INOOC \
        -DConj_ $(mySRCdir)/ATL_ger.c
ATL_zgeru_L2.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_zr1_L2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 $(mySRCdir)/ATL_ger.c
ATL_zgerc_L2.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_zr1_L2.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 \
        -DConj_ $(mySRCdir)/ATL_ger.c
ATL_zgeru_L1.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_zr1_L1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 $(mySRCdir)/ATL_ger.c
ATL_zgerc_L1.o : $(mySRCdir)/ATL_ger.c $(INCAdir)/atlas_zr1_L1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 \
        -DConj_ $(mySRCdir)/ATL_ger.c
ATL_zgeru.o : $(mySRCdir)/ATL_ger.c $(INCSdir)/atlas_r1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_ger.c
ATL_zgerc.o : $(mySRCdir)/ATL_ger.c $(INCSdir)/atlas_r1.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConj_ $(mySRCdir)/ATL_ger.c

ATL_zgerk_axpy.o : $(mySRCdir)/ATL_gerk_axpy.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gerk_axpy.c
ATL_zgerk_Mlt16.o : $(mySRCdir)/ATL_gerk_Mlt16.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gerk_Mlt16.c
ATL_zger2k_Nlt8.o : $(mySRCdir)/ATL_ger2k_Nlt8.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_ger2k_Nlt8.c
ATL_zger2k_Mlt16.o : $(mySRCdir)/ATL_ger2k_Mlt16.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_ger2k_Mlt16.c
ATL_cgerk_axpy.o : $(mySRCdir)/ATL_gerk_axpy.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gerk_axpy.c
ATL_cgerk_Mlt16.o : $(mySRCdir)/ATL_gerk_Mlt16.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gerk_Mlt16.c
ATL_cger2k_Nlt8.o : $(mySRCdir)/ATL_ger2k_Nlt8.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_ger2k_Nlt8.c
ATL_cger2k_Mlt16.o : $(mySRCdir)/ATL_ger2k_Mlt16.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_ger2k_Mlt16.c
ATL_dgerk_axpy.o : $(mySRCdir)/ATL_gerk_axpy.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gerk_axpy.c
ATL_dgerk_Mlt16.o : $(mySRCdir)/ATL_gerk_Mlt16.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gerk_Mlt16.c
ATL_dger2k_Nlt8.o : $(mySRCdir)/ATL_ger2k_Nlt8.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_ger2k_Nlt8.c
ATL_dger2k_Mlt16.o : $(mySRCdir)/ATL_ger2k_Mlt16.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_ger2k_Mlt16.c
ATL_sgerk_axpy.o : $(mySRCdir)/ATL_gerk_axpy.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gerk_axpy.c
ATL_sgerk_Mlt16.o : $(mySRCdir)/ATL_gerk_Mlt16.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gerk_Mlt16.c
ATL_sger2k_Nlt8.o : $(mySRCdir)/ATL_ger2k_Nlt8.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_ger2k_Nlt8.c
ATL_sger2k_Mlt16.o : $(mySRCdir)/ATL_ger2k_Mlt16.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_ger2k_Mlt16.c
ATL_zgerck_axpy.o : $(mySRCdir)/ATL_gerk_axpy.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConj_ $(mySRCdir)/ATL_gerk_axpy.c
ATL_zgerck_Mlt16.o : $(mySRCdir)/ATL_gerk_Mlt16.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConj_ $(mySRCdir)/ATL_gerk_Mlt16.c
ATL_zger2ck_Nlt8.o : $(mySRCdir)/ATL_ger2k_Nlt8.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConj_ $(mySRCdir)/ATL_ger2k_Nlt8.c
ATL_zger2ck_Mlt16.o : $(mySRCdir)/ATL_ger2k_Mlt16.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConj_ $(mySRCdir)/ATL_ger2k_Mlt16.c
ATL_cgerck_axpy.o : $(mySRCdir)/ATL_gerk_axpy.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConj_ $(mySRCdir)/ATL_gerk_axpy.c
ATL_cgerck_Mlt16.o : $(mySRCdir)/ATL_gerk_Mlt16.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConj_ $(mySRCdir)/ATL_gerk_Mlt16.c
ATL_cger2ck_Nlt8.o : $(mySRCdir)/ATL_ger2k_Nlt8.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConj_ $(mySRCdir)/ATL_ger2k_Nlt8.c
ATL_cger2ck_Mlt16.o : $(mySRCdir)/ATL_ger2k_Mlt16.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConj_ $(mySRCdir)/ATL_ger2k_Mlt16.c
