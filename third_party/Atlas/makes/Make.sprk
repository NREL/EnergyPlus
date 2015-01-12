include Make.inc
mySRCdir = $(SRCdir)/src/blas/pklevel3/sprk

all : lib
lib : dlib slib zlib clib
clean : sclean dclean cclean zclean

incdep : $(INCSdir)/atlas_pkblas.h $(INCSdir)/atlas_misc.h

sobj = ATL_spputblk_aX.o ATL_sprk_kmm.o ATL_ssprk.o ATL_ssprk_rK.o
slib : 
	$(PMAKE) slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
ATL_sphk_kmm.o : $(mySRCdir)/ATL_phk_kmm.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sphk_kmm.o -DSREAL \
              $(mySRCdir)/ATL_phk_kmm.c
ATL_shprk_rK.o : $(mySRCdir)/ATL_hprk_rK.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_shprk_rK.o -DSREAL \
              $(mySRCdir)/ATL_hprk_rK.c
ATL_shprk.o : $(mySRCdir)/ATL_hprk.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_shprk.o -DSREAL \
              $(mySRCdir)/ATL_hprk.c
ATL_ssprk_rK.o : $(mySRCdir)/ATL_sprk_rK.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_ssprk_rK.o -DSREAL \
              $(mySRCdir)/ATL_sprk_rK.c
ATL_ssprk.o : $(mySRCdir)/ATL_sprk.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_ssprk.o -DSREAL \
              $(mySRCdir)/ATL_sprk.c
ATL_spputblk_aX.o : $(mySRCdir)/ATL_pputblk_aX.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_spputblk_aX.o -DSREAL \
              $(mySRCdir)/ATL_pputblk_aX.c
ATL_sprk_kmm.o : $(mySRCdir)/ATL_prk_kmm.c $(incdep)
	$(SKC) $(sKCFLAGS) -c -o ATL_sprk_kmm.o -DSREAL \
              $(mySRCdir)/ATL_prk_kmm.c

dobj = ATL_dpputblk_aX.o ATL_dprk_kmm.o ATL_dsprk.o ATL_dsprk_rK.o
dlib : 
	$(PMAKE) dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
ATL_dphk_kmm.o : $(mySRCdir)/ATL_phk_kmm.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dphk_kmm.o -DDREAL \
              $(mySRCdir)/ATL_phk_kmm.c
ATL_dhprk_rK.o : $(mySRCdir)/ATL_hprk_rK.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dhprk_rK.o -DDREAL \
              $(mySRCdir)/ATL_hprk_rK.c
ATL_dhprk.o : $(mySRCdir)/ATL_hprk.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dhprk.o -DDREAL \
              $(mySRCdir)/ATL_hprk.c
ATL_dsprk_rK.o : $(mySRCdir)/ATL_sprk_rK.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dsprk_rK.o -DDREAL \
              $(mySRCdir)/ATL_sprk_rK.c
ATL_dsprk.o : $(mySRCdir)/ATL_sprk.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dsprk.o -DDREAL \
              $(mySRCdir)/ATL_sprk.c
ATL_dpputblk_aX.o : $(mySRCdir)/ATL_pputblk_aX.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dpputblk_aX.o -DDREAL \
              $(mySRCdir)/ATL_pputblk_aX.c
ATL_dprk_kmm.o : $(mySRCdir)/ATL_prk_kmm.c $(incdep)
	$(DKC) $(dKCFLAGS) -c -o ATL_dprk_kmm.o -DDREAL \
              $(mySRCdir)/ATL_prk_kmm.c

cobj = ATL_chprk.o ATL_chprk_rK.o ATL_cphk_kmm.o ATL_cpputblk_aX.o \
       ATL_cprk_kmm.o ATL_csprk.o ATL_csprk_rK.o
clib : 
	$(PMAKE) clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
ATL_cphk_kmm.o : $(mySRCdir)/ATL_phk_kmm.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cphk_kmm.o -DSCPLX \
              $(mySRCdir)/ATL_phk_kmm.c
ATL_chprk_rK.o : $(mySRCdir)/ATL_hprk_rK.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_chprk_rK.o -DSCPLX \
              $(mySRCdir)/ATL_hprk_rK.c
ATL_chprk.o : $(mySRCdir)/ATL_hprk.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_chprk.o -DSCPLX \
              $(mySRCdir)/ATL_hprk.c
ATL_csprk_rK.o : $(mySRCdir)/ATL_sprk_rK.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_csprk_rK.o -DSCPLX \
              $(mySRCdir)/ATL_sprk_rK.c
ATL_csprk.o : $(mySRCdir)/ATL_sprk.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_csprk.o -DSCPLX \
              $(mySRCdir)/ATL_sprk.c
ATL_cpputblk_aX.o : $(mySRCdir)/ATL_cpputblk_aX.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cpputblk_aX.o -DSCPLX \
              $(mySRCdir)/ATL_cpputblk_aX.c
ATL_cprk_kmm.o : $(mySRCdir)/ATL_cprk_kmm.c $(incdep)
	$(CKC) $(cKCFLAGS) -c -o ATL_cprk_kmm.o -DSCPLX \
              $(mySRCdir)/ATL_cprk_kmm.c

zobj = ATL_zhprk.o ATL_zhprk_rK.o ATL_zphk_kmm.o ATL_zpputblk_aX.o \
       ATL_zprk_kmm.o ATL_zsprk.o ATL_zsprk_rK.o
zlib : 
	$(PMAKE) zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
ATL_zphk_kmm.o : $(mySRCdir)/ATL_phk_kmm.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zphk_kmm.o -DDCPLX \
              $(mySRCdir)/ATL_phk_kmm.c
ATL_zhprk_rK.o : $(mySRCdir)/ATL_hprk_rK.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zhprk_rK.o -DDCPLX \
              $(mySRCdir)/ATL_hprk_rK.c
ATL_zhprk.o : $(mySRCdir)/ATL_hprk.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zhprk.o -DDCPLX \
              $(mySRCdir)/ATL_hprk.c
ATL_zsprk_rK.o : $(mySRCdir)/ATL_sprk_rK.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zsprk_rK.o -DDCPLX \
              $(mySRCdir)/ATL_sprk_rK.c
ATL_zsprk.o : $(mySRCdir)/ATL_sprk.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zsprk.o -DDCPLX \
              $(mySRCdir)/ATL_sprk.c
ATL_zpputblk_aX.o : $(mySRCdir)/ATL_cpputblk_aX.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zpputblk_aX.o -DDCPLX \
              $(mySRCdir)/ATL_cpputblk_aX.c
ATL_zprk_kmm.o : $(mySRCdir)/ATL_cprk_kmm.c $(incdep)
	$(ZKC) $(zKCFLAGS) -c -o ATL_zprk_kmm.o -DDCPLX \
              $(mySRCdir)/ATL_cprk_kmm.c
