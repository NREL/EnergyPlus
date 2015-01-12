include Make.inc
mySRCdir = $(SRCdir)/src/blas/gemv

sobj = ATL_sgemv.o ATL_sgemvN.o ATL_sgemvN_L1.o ATL_sgemvN_L2.o ATL_sgemvT.o \
       ATL_sgemvT_L1.o ATL_sgemvT_L2.o ATL_smvnk_Mlt16.o ATL_smvnk_smallN.o \
       ATL_smvtk_Mlt16.o ATL_smvtk_smallN.o

dobj = ATL_dgemv.o ATL_dgemvN.o ATL_dgemvN_L1.o ATL_dgemvN_L2.o ATL_dgemvT.o \
       ATL_dgemvT_L1.o ATL_dgemvT_L2.o ATL_dmvnk_Mlt16.o ATL_dmvnk_smallN.o \
       ATL_dmvtk_Mlt16.o ATL_dmvtk_smallN.o

cobj = ATL_cgemv.o ATL_cgemvCN.o ATL_cgemvCN_L1.o ATL_cgemvCN_L2.o \
       ATL_cgemvCT.o ATL_cgemvCT_L1.o ATL_cgemvCT_L2.o ATL_cgemvN.o \
       ATL_cgemvN_L1.o ATL_cgemvN_L2.o ATL_cgemvT.o ATL_cgemvT_L1.o \
       ATL_cgemvT_L2.o ATL_cmvnk_Mlt16.o ATL_cmvnk_smallN.o ATL_cmvtk_Mlt16.o \
       ATL_cmvtk_smallN.o

zobj = ATL_zgemv.o ATL_zgemvCN.o ATL_zgemvCN_L1.o ATL_zgemvCN_L2.o \
       ATL_zgemvCT.o ATL_zgemvCT_L1.o ATL_zgemvCT_L2.o ATL_zgemvN.o \
       ATL_zgemvN_L1.o ATL_zgemvN_L2.o ATL_zgemvT.o ATL_zgemvT_L1.o \
       ATL_zgemvT_L2.o ATL_zmvnk_Mlt16.o ATL_zmvnk_smallN.o ATL_zmvtk_Mlt16.o \
       ATL_zmvtk_smallN.o

lib : all
all : slib dlib clib zlib
lvl2 : all
dlvl2 : dlib
slvl2 : slib
clvl2 : clib
zlvl2 : zlib
clean : sclean dclean cclean zclean

dlib : 
	$(PMAKE) -f Make_dmvt dlib
	$(PMAKE) -f Make_dmvn dlib
	$(PMAKE) dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
dclean : 
	rm -f $(dobj) d*.grd
slib : 
	$(PMAKE) -f Make_smvt slib
	$(PMAKE) -f Make_smvn slib
	$(PMAKE) slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
sclean : 
	rm -f $(sobj) s*.grd
clib : 
	$(PMAKE) -f Make_cmvt clib
	$(PMAKE) -f Make_cmvn clib
	$(PMAKE) clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
cclean : 
	rm -f $(cobj) c*.grd
zlib : 
	$(PMAKE) -f Make_zmvt zlib
	$(PMAKE) -f Make_zmvn zlib
	$(PMAKE) zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
zclean : 
	rm -f $(zobj) z*.grd

ATL_smvtk_smallN.o : $(mySRCdir)/ATL_mvtk_smallN.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mvtk_smallN.c
ATL_smvtk_Mlt16.o : $(mySRCdir)/ATL_mvtk_Mlt16.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mvtk_Mlt16.c
ATL_smvnk_smallN.o : $(mySRCdir)/ATL_mvnk_smallN.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mvnk_smallN.c
ATL_smvnk_Mlt16.o : $(mySRCdir)/ATL_mvnk_Mlt16.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mvnk_Mlt16.c
ATL_sgemvT.o : $(mySRCdir)/ATL_gemvT.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gemvT.c
ATL_sgemvN.o : $(mySRCdir)/ATL_gemvN.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gemvN.c
ATL_sgemv.o : $(mySRCdir)/ATL_gemv.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gemv.c
ATL_sgemvCT.o : $(mySRCdir)/ATL_gemvCT.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gemvCT.c
ATL_sgemvCN.o : $(mySRCdir)/ATL_gemvCN.c \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_gemvCN.c
ATL_sgemvCN_L2.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL2 $(mySRCdir)/ATL_gemvCN.c
ATL_sgemvCN_L1.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL1 $(mySRCdir)/ATL_gemvCN.c
ATL_sgemvCT_L2.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL2 $(mySRCdir)/ATL_gemvCT.c
ATL_sgemvCT_L1.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL1 $(mySRCdir)/ATL_gemvCT.c
ATL_sgemvN_L2.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL2 $(mySRCdir)/ATL_gemvN.c
ATL_sgemvN_L1.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL1 $(mySRCdir)/ATL_gemvN.c
ATL_sgemvT_L2.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL2 $(mySRCdir)/ATL_gemvT.c
ATL_sgemvT_L1.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_smvn.h $(INCAdir)/atlas_smvt.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATL_INL1 $(mySRCdir)/ATL_gemvT.c
ATL_dmvtk_smallN.o : $(mySRCdir)/ATL_mvtk_smallN.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mvtk_smallN.c
ATL_dmvtk_Mlt16.o : $(mySRCdir)/ATL_mvtk_Mlt16.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mvtk_Mlt16.c
ATL_dmvnk_smallN.o : $(mySRCdir)/ATL_mvnk_smallN.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mvnk_smallN.c
ATL_dmvnk_Mlt16.o : $(mySRCdir)/ATL_mvnk_Mlt16.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mvnk_Mlt16.c
ATL_dgemvT.o : $(mySRCdir)/ATL_gemvT.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gemvT.c
ATL_dgemvN.o : $(mySRCdir)/ATL_gemvN.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gemvN.c
ATL_dgemv.o : $(mySRCdir)/ATL_gemv.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gemv.c
ATL_dgemvCT.o : $(mySRCdir)/ATL_gemvCT.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gemvCT.c
ATL_dgemvCN.o : $(mySRCdir)/ATL_gemvCN.c \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_gemvCN.c
ATL_dgemvCN_L2.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL2 $(mySRCdir)/ATL_gemvCN.c
ATL_dgemvCN_L1.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL1 $(mySRCdir)/ATL_gemvCN.c
ATL_dgemvCT_L2.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL2 $(mySRCdir)/ATL_gemvCT.c
ATL_dgemvCT_L1.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL1 $(mySRCdir)/ATL_gemvCT.c
ATL_dgemvN_L2.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL2 $(mySRCdir)/ATL_gemvN.c
ATL_dgemvN_L1.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL1 $(mySRCdir)/ATL_gemvN.c
ATL_dgemvT_L2.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL2 $(mySRCdir)/ATL_gemvT.c
ATL_dgemvT_L1.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_dmvn.h $(INCAdir)/atlas_dmvt.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATL_INL1 $(mySRCdir)/ATL_gemvT.c
ATL_cmvtk_smallN.o : $(mySRCdir)/ATL_mvtk_smallN.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_mvtk_smallN.c
ATL_cmvtk_Mlt16.o : $(mySRCdir)/ATL_mvtk_Mlt16.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_mvtk_Mlt16.c
ATL_cmvnk_smallN.o : $(mySRCdir)/ATL_mvnk_smallN.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_mvnk_smallN.c
ATL_cmvnk_Mlt16.o : $(mySRCdir)/ATL_mvnk_Mlt16.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_mvnk_Mlt16.c
ATL_cgemvT.o : $(mySRCdir)/ATL_gemvT.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gemvT.c
ATL_cgemvN.o : $(mySRCdir)/ATL_gemvN.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gemvN.c
ATL_cgemv.o : $(mySRCdir)/ATL_gemv.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gemv.c
ATL_cgemvCT.o : $(mySRCdir)/ATL_gemvCT.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gemvCT.c
ATL_cgemvCN.o : $(mySRCdir)/ATL_gemvCN.c \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gemvCN.c
ATL_cgemvCN_L2.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvCN.c
ATL_cgemvCN_L1.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvCN.c
ATL_cgemvCT_L2.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvCT.c
ATL_cgemvCT_L1.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvCT.c
ATL_cgemvN_L2.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvN.c
ATL_cgemvN_L1.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvN.c
ATL_cgemvT_L2.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvT.c
ATL_cgemvT_L1.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_cmvn.h $(INCAdir)/atlas_cmvt.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvT.c
ATL_zmvtk_smallN.o : $(mySRCdir)/ATL_mvtk_smallN.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_mvtk_smallN.c
ATL_zmvtk_Mlt16.o : $(mySRCdir)/ATL_mvtk_Mlt16.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_mvtk_Mlt16.c
ATL_zmvnk_smallN.o : $(mySRCdir)/ATL_mvnk_smallN.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_mvnk_smallN.c
ATL_zmvnk_Mlt16.o : $(mySRCdir)/ATL_mvnk_Mlt16.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_mvnk_Mlt16.c
ATL_zgemvT.o : $(mySRCdir)/ATL_gemvT.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gemvT.c
ATL_zgemvN.o : $(mySRCdir)/ATL_gemvN.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gemvN.c
ATL_zgemv.o : $(mySRCdir)/ATL_gemv.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gemv.c
ATL_zgemvCT.o : $(mySRCdir)/ATL_gemvCT.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gemvCT.c
ATL_zgemvCN.o : $(mySRCdir)/ATL_gemvCN.c \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gemvCN.c
ATL_zgemvCN_L2.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvCN.c
ATL_zgemvCN_L1.o : $(mySRCdir)/ATL_gemvCN.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvCN.c
ATL_zgemvCT_L2.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvCT.c
ATL_zgemvCT_L1.o : $(mySRCdir)/ATL_gemvCT.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvCT.c
ATL_zgemvN_L2.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvN.c
ATL_zgemvN_L1.o : $(mySRCdir)/ATL_gemvN.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvN.c
ATL_zgemvT_L2.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL2 $(mySRCdir)/ATL_gemvT.c
ATL_zgemvT_L1.o : $(mySRCdir)/ATL_gemvT.c  \
                   $(INCAdir)/atlas_zmvn.h $(INCAdir)/atlas_zmvt.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATL_INL1 $(mySRCdir)/ATL_gemvT.c
