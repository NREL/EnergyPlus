include Make.inc
mySRCdir = $(SRCdir)/EXtest
SBINdir = $(SRCdir)/bin

scripts : RUN_ormtst.sh RUN_qrtest_pt.sh
RUN_qrtest_pt.sh : $(mySRCdir)/RUN_qrtest_pt.sh
	cp $(mySRCdir)/RUN_qrtest_pt.sh .
RUN_ormtst.sh : $(mySRCdir)/RUN_ormtst.sh
	cp $(mySRCdir)/RUN_ormtst.sh .
xstmmnantest : stmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  stmmnantest.o \
           $(GMMdir)/ATL_ssmall_mm.o $(GMMdir)/ATL_sbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
xssmmnantest : ssmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  ssmmnantest.o \
           $(GMMdir)/ATL_ssmall_mm.o $(GMMdir)/ATL_sbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
ssmmnantest.o : $(SBINdir)/gemmtst.c ssmmlib
	$(KC) $(KCFLAGS) -DSREAL=1 -DATL_NANC -DTEST_BIG -DTRUST_SMALL \
               -o $@ -c $(SBINdir)/gemmtst.c
stmmnantest.o : $(SBINdir)/gemmtst.c stmmlib
	$(KC) $(KCFLAGS) -DSREAL=1 -DATL_NANC -DATL_USEPTHREADS \
               -o $@ -c $(SBINdir)/gemmtst.c
xsmvttest : smvttest.o ssl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  smvttest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
smvttest.o : $(mySRCdir)/mvttest.c
	$(KC) $(KCFLAGS) -DSREAL=1 -o $@ -c $(mySRCdir)/mvttest.c
xsmvntest : smvntest.o ssl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  smvntest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
smvntest.o : $(mySRCdir)/mvntest.c
	$(KC) $(KCFLAGS) -DSREAL=1 -o $@ -c $(mySRCdir)/mvntest.c
xsr2test : sr2test.o ssl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  sr2test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
sr2test.o : $(mySRCdir)/r2test.c
	$(KC) $(KCFLAGS) -DSREAL=1 -o $@ -c $(mySRCdir)/r2test.c
xsr1test : sr1test.o ssl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  sr1test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
sr1test.o : $(mySRCdir)/r1test.c
	$(KC) $(KCFLAGS) -DSREAL=1 -o $@ -c $(mySRCdir)/r1test.c
stl3lib : 
	cd $(BINdir) ; $(MAKE) sl3lib_pt
ssl3lib : 
	cd $(BINdir) ; $(MAKE) sl3lib
stmmlib : 
	cd $(BINdir) ; $(MAKE) smmlib_pt
ssmmlib : 
	cd $(BINdir) ; $(MAKE) smmlib
stl2lib : 
	cd $(BINdir) ; $(MAKE) sl2lib_pt
ssl2lib : 
	cd $(BINdir) ; $(MAKE) sl2lib
xdtmmnantest : dtmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  dtmmnantest.o \
           $(GMMdir)/ATL_dsmall_mm.o $(GMMdir)/ATL_dbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
xdsmmnantest : dsmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  dsmmnantest.o \
           $(GMMdir)/ATL_dsmall_mm.o $(GMMdir)/ATL_dbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
dsmmnantest.o : $(SBINdir)/gemmtst.c dsmmlib
	$(KC) $(KCFLAGS) -DDREAL=1 -DATL_NANC -DTEST_BIG -DTRUST_SMALL \
               -o $@ -c $(SBINdir)/gemmtst.c
dtmmnantest.o : $(SBINdir)/gemmtst.c dtmmlib
	$(KC) $(KCFLAGS) -DDREAL=1 -DATL_NANC -DATL_USEPTHREADS \
               -o $@ -c $(SBINdir)/gemmtst.c
xdmvttest : dmvttest.o dsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  dmvttest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
dmvttest.o : $(mySRCdir)/mvttest.c
	$(KC) $(KCFLAGS) -DDREAL=1 -o $@ -c $(mySRCdir)/mvttest.c
xdmvntest : dmvntest.o dsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  dmvntest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
dmvntest.o : $(mySRCdir)/mvntest.c
	$(KC) $(KCFLAGS) -DDREAL=1 -o $@ -c $(mySRCdir)/mvntest.c
xdr2test : dr2test.o dsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  dr2test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
dr2test.o : $(mySRCdir)/r2test.c
	$(KC) $(KCFLAGS) -DDREAL=1 -o $@ -c $(mySRCdir)/r2test.c
xdr1test : dr1test.o dsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  dr1test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
dr1test.o : $(mySRCdir)/r1test.c
	$(KC) $(KCFLAGS) -DDREAL=1 -o $@ -c $(mySRCdir)/r1test.c
dtl3lib : 
	cd $(BINdir) ; $(MAKE) dl3lib_pt
dsl3lib : 
	cd $(BINdir) ; $(MAKE) dl3lib
dtmmlib : 
	cd $(BINdir) ; $(MAKE) dmmlib_pt
dsmmlib : 
	cd $(BINdir) ; $(MAKE) dmmlib
dtl2lib : 
	cd $(BINdir) ; $(MAKE) dl2lib_pt
dsl2lib : 
	cd $(BINdir) ; $(MAKE) dl2lib
xctmmnantest : ctmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  ctmmnantest.o \
           $(GMMdir)/ATL_csmall_mm.o $(GMMdir)/ATL_cbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
xcsmmnantest : csmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  csmmnantest.o \
           $(GMMdir)/ATL_csmall_mm.o $(GMMdir)/ATL_cbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
csmmnantest.o : $(SBINdir)/gemmtst.c csmmlib
	$(KC) $(KCFLAGS) -DSCPLX=1 -DATL_NANC -DTEST_BIG -DTRUST_SMALL \
               -o $@ -c $(SBINdir)/gemmtst.c
ctmmnantest.o : $(SBINdir)/gemmtst.c ctmmlib
	$(KC) $(KCFLAGS) -DSCPLX=1 -DATL_NANC -DATL_USEPTHREADS \
               -o $@ -c $(SBINdir)/gemmtst.c
xcmvttest : cmvttest.o csl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  cmvttest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
cmvttest.o : $(mySRCdir)/mvttest.c
	$(KC) $(KCFLAGS) -DSCPLX=1 -o $@ -c $(mySRCdir)/mvttest.c
xcmvntest : cmvntest.o csl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  cmvntest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
cmvntest.o : $(mySRCdir)/mvntest.c
	$(KC) $(KCFLAGS) -DSCPLX=1 -o $@ -c $(mySRCdir)/mvntest.c
xcr2test : cr2test.o csl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  cr2test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
cr2test.o : $(mySRCdir)/r2test.c
	$(KC) $(KCFLAGS) -DSCPLX=1 -o $@ -c $(mySRCdir)/r2test.c
xcr1test : cr1test.o csl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  cr1test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
cr1test.o : $(mySRCdir)/r1test.c
	$(KC) $(KCFLAGS) -DSCPLX=1 -o $@ -c $(mySRCdir)/r1test.c
ctl3lib : 
	cd $(BINdir) ; $(MAKE) cl3lib_pt
csl3lib : 
	cd $(BINdir) ; $(MAKE) cl3lib
ctmmlib : 
	cd $(BINdir) ; $(MAKE) cmmlib_pt
csmmlib : 
	cd $(BINdir) ; $(MAKE) cmmlib
ctl2lib : 
	cd $(BINdir) ; $(MAKE) cl2lib_pt
csl2lib : 
	cd $(BINdir) ; $(MAKE) cl2lib
xztmmnantest : ztmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  ztmmnantest.o \
           $(GMMdir)/ATL_zsmall_mm.o $(GMMdir)/ATL_zbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
xzsmmnantest : zsmmnantest.o 
	$(FLINKER) $(FCLINKFLAGS) -o $@  zsmmnantest.o \
           $(GMMdir)/ATL_zsmall_mm.o $(GMMdir)/ATL_zbig_mm.o \
           $(TESTlib) $(ATLASlib) $(LIBS)
zsmmnantest.o : $(SBINdir)/gemmtst.c zsmmlib
	$(KC) $(KCFLAGS) -DDCPLX=1 -DATL_NANC -DTEST_BIG -DTRUST_SMALL \
               -o $@ -c $(SBINdir)/gemmtst.c
ztmmnantest.o : $(SBINdir)/gemmtst.c ztmmlib
	$(KC) $(KCFLAGS) -DDCPLX=1 -DATL_NANC -DATL_USEPTHREADS \
               -o $@ -c $(SBINdir)/gemmtst.c
xzmvttest : zmvttest.o zsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  zmvttest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
zmvttest.o : $(mySRCdir)/mvttest.c
	$(KC) $(KCFLAGS) -DDCPLX=1 -o $@ -c $(mySRCdir)/mvttest.c
xzmvntest : zmvntest.o zsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  zmvntest.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
zmvntest.o : $(mySRCdir)/mvntest.c
	$(KC) $(KCFLAGS) -DDCPLX=1 -o $@ -c $(mySRCdir)/mvntest.c
xzr2test : zr2test.o zsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  zr2test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
zr2test.o : $(mySRCdir)/r2test.c
	$(KC) $(KCFLAGS) -DDCPLX=1 -o $@ -c $(mySRCdir)/r2test.c
xzr1test : zr1test.o zsl2lib
	$(FLINKER) $(FCLINKFLAGS) -o $@  zr1test.o \
                   $(TESTlib) $(ATLASlib) $(LIBS)
zr1test.o : $(mySRCdir)/r1test.c
	$(KC) $(KCFLAGS) -DDCPLX=1 -o $@ -c $(mySRCdir)/r1test.c
ztl3lib : 
	cd $(BINdir) ; $(MAKE) zl3lib_pt
zsl3lib : 
	cd $(BINdir) ; $(MAKE) zl3lib
ztmmlib : 
	cd $(BINdir) ; $(MAKE) zmmlib_pt
zsmmlib : 
	cd $(BINdir) ; $(MAKE) zmmlib
ztl2lib : 
	cd $(BINdir) ; $(MAKE) zl2lib_pt
zsl2lib : 
	cd $(BINdir) ; $(MAKE) zl2lib

