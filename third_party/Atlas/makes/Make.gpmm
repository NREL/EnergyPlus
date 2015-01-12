include Make.inc
mySRCdir = $(SRCdir)/src/blas/pklevel3/gpmm

all : lib
lib : dlib slib zlib clib
clean : sclean dclean cclean zclean

incdep : $(INCSdir)/atlas_pkblas.h $(INCSdir)/atlas_misc.h

sobj = ATL_sgpmm.o ATL_spcol2blk_a1.o ATL_spcol2blk_aX.o ATL_spmmJIK.o \
       ATL_spputblk.o ATL_sprankK.o ATL_sprow2blkT_a1.o ATL_sprow2blkT_aX.o

slib : 
	$(PMAKE) slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
ATL_sprankK.o : $(mySRCdir)/ATL_prankK.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sprankK.o -DSREAL \
              $(mySRCdir)/ATL_prankK.c
ATL_sgpmm.o : $(mySRCdir)/ATL_gpmm.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sgpmm.o -DSREAL \
              $(mySRCdir)/ATL_gpmm.c
ATL_spputblk.o : $(mySRCdir)/ATL_pputblk.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spputblk.o -DSREAL \
              $(mySRCdir)/ATL_pputblk.c
ATL_spmmJIK.o : $(mySRCdir)/ATL_pmmJIK.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spmmJIK.o -DSREAL \
              $(mySRCdir)/ATL_pmmJIK.c
ATL_sprow2blkT_aX.o : $(mySRCdir)/ATL_prow2blkT.c $(incdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX $(mySRCdir)/ATL_prow2blkT.c
ATL_spcol2blk_aX.o : $(mySRCdir)/ATL_pcol2blk.c $(incdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX $(mySRCdir)/ATL_pcol2blk.c
ATL_sprow2blkT_a1.o : $(mySRCdir)/ATL_prow2blkT.c $(incdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 $(mySRCdir)/ATL_prow2blkT.c
ATL_spcol2blk_a1.o : $(mySRCdir)/ATL_pcol2blk.c $(incdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 $(mySRCdir)/ATL_pcol2blk.c

dobj = ATL_dgpmm.o ATL_dpcol2blk_a1.o ATL_dpcol2blk_aX.o ATL_dpmmJIK.o \
       ATL_dpputblk.o ATL_dprankK.o ATL_dprow2blkT_a1.o ATL_dprow2blkT_aX.o

dlib : 
	$(PMAKE) dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
ATL_dprankK.o : $(mySRCdir)/ATL_prankK.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dprankK.o -DDREAL \
              $(mySRCdir)/ATL_prankK.c
ATL_dgpmm.o : $(mySRCdir)/ATL_gpmm.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dgpmm.o -DDREAL \
              $(mySRCdir)/ATL_gpmm.c
ATL_dpputblk.o : $(mySRCdir)/ATL_pputblk.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpputblk.o -DDREAL \
              $(mySRCdir)/ATL_pputblk.c
ATL_dpmmJIK.o : $(mySRCdir)/ATL_pmmJIK.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpmmJIK.o -DDREAL \
              $(mySRCdir)/ATL_pmmJIK.c
ATL_dprow2blkT_aX.o : $(mySRCdir)/ATL_prow2blkT.c $(incdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX $(mySRCdir)/ATL_prow2blkT.c
ATL_dpcol2blk_aX.o : $(mySRCdir)/ATL_pcol2blk.c $(incdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX $(mySRCdir)/ATL_pcol2blk.c
ATL_dprow2blkT_a1.o : $(mySRCdir)/ATL_prow2blkT.c $(incdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 $(mySRCdir)/ATL_prow2blkT.c
ATL_dpcol2blk_a1.o : $(mySRCdir)/ATL_pcol2blk.c $(incdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 $(mySRCdir)/ATL_pcol2blk.c

cobj = ATL_cgpmm.o ATL_cpcol2blk_a1.o ATL_cpcol2blk_a1C.o ATL_cpcol2blk_aX.o \
       ATL_cpcol2blk_aXC.o ATL_cpcol2blk_aXi0.o ATL_cpcol2blk_aXi0C.o \
       ATL_cpmmJIK.o ATL_cpputblk.o ATL_cprankK.o ATL_cprow2blkT_a1.o \
       ATL_cprow2blkT_a1C.o ATL_cprow2blkT_aX.o ATL_cprow2blkT_aXC.o \
       ATL_cprow2blkT_aXi0.o ATL_cprow2blkT_aXi0C.o

clib : 
	$(PMAKE) clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
ATL_cprankK.o : $(mySRCdir)/ATL_prankK.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cprankK.o -DSCPLX \
              $(mySRCdir)/ATL_prankK.c
ATL_cgpmm.o : $(mySRCdir)/ATL_gpmm.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cgpmm.o -DSCPLX \
              $(mySRCdir)/ATL_gpmm.c
ATL_cpputblk.o : $(mySRCdir)/ATL_cpputblk.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpputblk.o -DSCPLX \
              $(mySRCdir)/ATL_cpputblk.c
ATL_cpmmJIK.o : $(mySRCdir)/ATL_cpmmJIK.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpmmJIK.o -DSCPLX \
              $(mySRCdir)/ATL_cpmmJIK.c
ATL_cprow2blkT_aX.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAX $(mySRCdir)/ATL_cprow2blkT.c
ATL_cpcol2blk_aX.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAX $(mySRCdir)/ATL_cpcol2blk.c
ATL_cprow2blkT_a1.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHA1 $(mySRCdir)/ATL_cprow2blkT.c
ATL_cpcol2blk_a1.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHA1 $(mySRCdir)/ATL_cpcol2blk.c
ATL_cprow2blkT_aXi0.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAXI0 $(mySRCdir)/ATL_cprow2blkT.c
ATL_cpcol2blk_aXi0.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAXI0 $(mySRCdir)/ATL_cpcol2blk.c
ATL_cprow2blkT_aXi0C.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DConj_ -DSCPLX -DALPHAXI0 $(mySRCdir)/ATL_cprow2blkT.c
ATL_cpcol2blk_aXi0C.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DConj_ -DSCPLX -DALPHAXI0 $(mySRCdir)/ATL_cpcol2blk.c
ATL_cprow2blkT_aXC.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DConj_ -DSCPLX -DALPHAX $(mySRCdir)/ATL_cprow2blkT.c
ATL_cpcol2blk_aXC.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DConj_ -DSCPLX -DALPHAX $(mySRCdir)/ATL_cpcol2blk.c
ATL_cprow2blkT_a1C.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DConj_ -DSCPLX -DALPHA1 $(mySRCdir)/ATL_cprow2blkT.c
ATL_cpcol2blk_a1C.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DConj_ -DSCPLX -DALPHA1 $(mySRCdir)/ATL_cpcol2blk.c

zobj = ATL_zgpmm.o ATL_zpcol2blk_a1.o ATL_zpcol2blk_a1C.o ATL_zpcol2blk_aX.o \
       ATL_zpcol2blk_aXC.o ATL_zpcol2blk_aXi0.o ATL_zpcol2blk_aXi0C.o \
       ATL_zpmmJIK.o ATL_zpputblk.o ATL_zprankK.o ATL_zprow2blkT_a1.o \
       ATL_zprow2blkT_a1C.o ATL_zprow2blkT_aX.o ATL_zprow2blkT_aXC.o \
       ATL_zprow2blkT_aXi0.o ATL_zprow2blkT_aXi0C.o

zlib : 
	$(PMAKE) zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
ATL_zprankK.o : $(mySRCdir)/ATL_prankK.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zprankK.o -DDCPLX \
              $(mySRCdir)/ATL_prankK.c
ATL_zgpmm.o : $(mySRCdir)/ATL_gpmm.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zgpmm.o -DDCPLX \
              $(mySRCdir)/ATL_gpmm.c
ATL_zpputblk.o : $(mySRCdir)/ATL_cpputblk.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpputblk.o -DDCPLX \
              $(mySRCdir)/ATL_cpputblk.c
ATL_zpmmJIK.o : $(mySRCdir)/ATL_cpmmJIK.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpmmJIK.o -DDCPLX \
              $(mySRCdir)/ATL_cpmmJIK.c
ATL_zprow2blkT_aX.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAX $(mySRCdir)/ATL_cprow2blkT.c
ATL_zpcol2blk_aX.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAX $(mySRCdir)/ATL_cpcol2blk.c
ATL_zprow2blkT_a1.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHA1 $(mySRCdir)/ATL_cprow2blkT.c
ATL_zpcol2blk_a1.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHA1 $(mySRCdir)/ATL_cpcol2blk.c
ATL_zprow2blkT_aXi0.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAXI0 $(mySRCdir)/ATL_cprow2blkT.c
ATL_zpcol2blk_aXi0.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAXI0 $(mySRCdir)/ATL_cpcol2blk.c
ATL_zprow2blkT_aXi0C.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DConj_ -DDCPLX -DALPHAXI0 $(mySRCdir)/ATL_cprow2blkT.c
ATL_zpcol2blk_aXi0C.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DConj_ -DDCPLX -DALPHAXI0 $(mySRCdir)/ATL_cpcol2blk.c
ATL_zprow2blkT_aXC.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DConj_ -DDCPLX -DALPHAX $(mySRCdir)/ATL_cprow2blkT.c
ATL_zpcol2blk_aXC.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DConj_ -DDCPLX -DALPHAX $(mySRCdir)/ATL_cpcol2blk.c
ATL_zprow2blkT_a1C.o : $(mySRCdir)/ATL_cprow2blkT.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DConj_ -DDCPLX -DALPHA1 $(mySRCdir)/ATL_cprow2blkT.c
ATL_zpcol2blk_a1C.o : $(mySRCdir)/ATL_cpcol2blk.c $(incdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DConj_ -DDCPLX -DALPHA1 $(mySRCdir)/ATL_cpcol2blk.c
