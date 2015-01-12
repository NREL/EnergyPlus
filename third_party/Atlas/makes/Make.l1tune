include Make.inc
mySRCdir = $(SRCdir)/tune/blas/level1

$(SYSdir)/time.o:
	cd $(SYSdir) ; $(MAKE) time.o

$(INCAdir)/atlas_type.h : 
	cd $(SYSdir) ; $(MAKE) RunTyp

$(SYSdir)/res/L1CacheSize :
	rm -f res/L1CacheSize
	cd $(SYSdir) ; $(MAKE) res/L1CacheSize
	
RunL1 : force_build
	$(MAKE) $(SYSdir)/res/L1CacheSize 
	ln -s $(SYSdir)/res/L1CacheSize res/L1CacheSize

RunTLB : 
	cd $(SYSdir) ; $(MAKE) RunTLB

$(SYSdir)/res/$(pre)MULADD : 
	rm -f res/$(pre)MULADD
	cd $(SYSdir) ; $(MAKE) res/$(pre)MULADD pre=$(pre) 
RunMulAdd: $(SYSdir)/res/$(pre)MULADD
	ln -s $(SYSdir)/res/$(pre)MULADD res/$(pre)MULADD

RunTyp: force_build
	cd $(SYSdir) ; $(MAKE) RunTyp



sUCC = $(SKC)
sUCCFLAGS = $(SKCFLAGS)
cUCC = $(SKC)
cUCCFLAGS = $(SKCFLAGS)
dUCC = $(DKC)
dUCCFLAGS = $(DKCFLAGS)
zUCC = $(DKC)
zUCCFLAGS = $(DKCFLAGS)
incX = 1
N = 777
IDNUM=599932
opt=
fout=

tstlib.grd :
	cd $(TSTdir) ; $(MAKE) lib
	touch tstlib.grd

force_build :
# 
# Go to src/auxil directory, and compile just those routines, to avoid circular
# dependency in full auxil install (which dep on L1BLAS/this directory!)
#
auxlib :
	cd $(AUXdir) ; make lib.grd0
$(BINdir)/xsubstr:
	cd $(BINdir) ; $(MAKE) xsubstr
cdotc.dsc : $(mySRCdir)/DOT/ccasesc.dsc $(BINdir)/xsubstr
	rm -f cdotc.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/DOT/ccasesc.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zdotc.dsc : $(mySRCdir)/DOT/zcasesc.dsc $(BINdir)/xsubstr
	rm -f zdotc.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/DOT/zcasesc.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
sdot.dsc : $(mySRCdir)/DOT/scases.dsc $(BINdir)/xsubstr
	rm -f sdot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/DOT/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
ddot.dsc : $(mySRCdir)/DOT/dcases.dsc $(BINdir)/xsubstr
	rm -f ddot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/DOT/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
cdot.dsc : $(mySRCdir)/DOT/ccases.dsc $(BINdir)/xsubstr
	rm -f cdot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/DOT/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zdot.dsc : $(mySRCdir)/DOT/zcases.dsc $(BINdir)/xsubstr
	rm -f zdot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/DOT/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dotsrch.o : $(mySRCdir)/dotsrch.c sdot.dsc ddot.dsc cdot.dsc zdot.dsc \
              cdotc.dsc zdotc.dsc
	$(XCC) $(XCCFLAGS) -o dotsrch.o -c $(mySRCdir)/dotsrch.c
xdotsrch : dotsrch.o
	$(XCC) $(XCCFLAGS) -o xdotsrch dotsrch.o
srot.dsc : $(mySRCdir)/ROT/scases.dsc $(BINdir)/xsubstr
	rm -f srot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ROT/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
drot.dsc : $(mySRCdir)/ROT/dcases.dsc $(BINdir)/xsubstr
	rm -f drot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ROT/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
crot.dsc : $(mySRCdir)/ROT/ccases.dsc $(BINdir)/xsubstr
	rm -f crot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ROT/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zrot.dsc : $(mySRCdir)/ROT/zcases.dsc $(BINdir)/xsubstr
	rm -f zrot.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ROT/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
rotsrch.o : $(mySRCdir)/rotsrch.c srot.dsc drot.dsc crot.dsc zrot.dsc
	$(XCC) $(XCCFLAGS) -o rotsrch.o -c $(mySRCdir)/rotsrch.c
xrotsrch : rotsrch.o
	$(XCC) $(XCCFLAGS) -o xrotsrch rotsrch.o
saxpby.dsc : $(mySRCdir)/AXPBY/scases.dsc $(BINdir)/xsubstr
	rm -f saxpby.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPBY/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
daxpby.dsc : $(mySRCdir)/AXPBY/dcases.dsc $(BINdir)/xsubstr
	rm -f daxpby.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPBY/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
caxpby.dsc : $(mySRCdir)/AXPBY/ccases.dsc $(BINdir)/xsubstr
	rm -f caxpby.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPBY/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zaxpby.dsc : $(mySRCdir)/AXPBY/zcases.dsc $(BINdir)/xsubstr
	rm -f zaxpby.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPBY/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
axpbysrch.o : $(mySRCdir)/axpbysrch.c saxpby.dsc daxpby.dsc caxpby.dsc zaxpby.dsc
	$(XCC) $(XCCFLAGS) -o axpbysrch.o -c $(mySRCdir)/axpbysrch.c
xaxpbysrch : axpbysrch.o
	$(XCC) $(XCCFLAGS) -o xaxpbysrch axpbysrch.o
sset.dsc : $(mySRCdir)/SET/scases.dsc $(BINdir)/xsubstr
	rm -f sset.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SET/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dset.dsc : $(mySRCdir)/SET/dcases.dsc $(BINdir)/xsubstr
	rm -f dset.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SET/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
cset.dsc : $(mySRCdir)/SET/ccases.dsc $(BINdir)/xsubstr
	rm -f cset.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SET/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zset.dsc : $(mySRCdir)/SET/zcases.dsc $(BINdir)/xsubstr
	rm -f zset.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SET/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
setsrch.o : $(mySRCdir)/setsrch.c sset.dsc dset.dsc cset.dsc zset.dsc
	$(XCC) $(XCCFLAGS) -o setsrch.o -c $(mySRCdir)/setsrch.c
xsetsrch : setsrch.o
	$(XCC) $(XCCFLAGS) -o xsetsrch setsrch.o
scpsc.dsc : $(mySRCdir)/CPSC/scases.dsc $(BINdir)/xsubstr
	rm -f scpsc.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/CPSC/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dcpsc.dsc : $(mySRCdir)/CPSC/dcases.dsc $(BINdir)/xsubstr
	rm -f dcpsc.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/CPSC/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
ccpsc.dsc : $(mySRCdir)/CPSC/ccases.dsc $(BINdir)/xsubstr
	rm -f ccpsc.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/CPSC/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zcpsc.dsc : $(mySRCdir)/CPSC/zcases.dsc $(BINdir)/xsubstr
	rm -f zcpsc.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/CPSC/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
cpscsrch.o : $(mySRCdir)/cpscsrch.c scpsc.dsc dcpsc.dsc ccpsc.dsc zcpsc.dsc
	$(XCC) $(XCCFLAGS) -o cpscsrch.o -c $(mySRCdir)/cpscsrch.c
xcpscsrch : cpscsrch.o
	$(XCC) $(XCCFLAGS) -o xcpscsrch cpscsrch.o
sasum.dsc : $(mySRCdir)/ASUM/scases.dsc $(BINdir)/xsubstr
	rm -f sasum.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ASUM/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dasum.dsc : $(mySRCdir)/ASUM/dcases.dsc $(BINdir)/xsubstr
	rm -f dasum.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ASUM/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
casum.dsc : $(mySRCdir)/ASUM/ccases.dsc $(BINdir)/xsubstr
	rm -f casum.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ASUM/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zasum.dsc : $(mySRCdir)/ASUM/zcases.dsc $(BINdir)/xsubstr
	rm -f zasum.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/ASUM/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
asumsrch.o : $(mySRCdir)/asumsrch.c sasum.dsc dasum.dsc casum.dsc zasum.dsc
	$(XCC) $(XCCFLAGS) -o asumsrch.o -c $(mySRCdir)/asumsrch.c
xasumsrch : asumsrch.o
	$(XCC) $(XCCFLAGS) -o xasumsrch asumsrch.o
snrm2.dsc : $(mySRCdir)/NRM2/scases.dsc $(BINdir)/xsubstr
	rm -f snrm2.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/NRM2/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dnrm2.dsc : $(mySRCdir)/NRM2/dcases.dsc $(BINdir)/xsubstr
	rm -f dnrm2.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/NRM2/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
cnrm2.dsc : $(mySRCdir)/NRM2/ccases.dsc $(BINdir)/xsubstr
	rm -f cnrm2.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/NRM2/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
znrm2.dsc : $(mySRCdir)/NRM2/zcases.dsc $(BINdir)/xsubstr
	rm -f znrm2.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/NRM2/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
nrm2srch.o : $(mySRCdir)/nrm2srch.c snrm2.dsc dnrm2.dsc cnrm2.dsc znrm2.dsc
	$(XCC) $(XCCFLAGS) -o nrm2srch.o -c $(mySRCdir)/nrm2srch.c
xnrm2srch : nrm2srch.o
	$(XCC) $(XCCFLAGS) -o xnrm2srch nrm2srch.o
sscal.dsc : $(mySRCdir)/SCAL/scases.dsc $(BINdir)/xsubstr
	rm -f sscal.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SCAL/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dscal.dsc : $(mySRCdir)/SCAL/dcases.dsc $(BINdir)/xsubstr
	rm -f dscal.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SCAL/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
cscal.dsc : $(mySRCdir)/SCAL/ccases.dsc $(BINdir)/xsubstr
	rm -f cscal.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SCAL/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zscal.dsc : $(mySRCdir)/SCAL/zcases.dsc $(BINdir)/xsubstr
	rm -f zscal.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SCAL/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
scalsrch.o : $(mySRCdir)/scalsrch.c sscal.dsc dscal.dsc cscal.dsc zscal.dsc
	$(XCC) $(XCCFLAGS) -o scalsrch.o -c $(mySRCdir)/scalsrch.c
xscalsrch : scalsrch.o
	$(XCC) $(XCCFLAGS) -o xscalsrch scalsrch.o
sswap.dsc : $(mySRCdir)/SWAP/scases.dsc $(BINdir)/xsubstr
	rm -f sswap.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SWAP/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dswap.dsc : $(mySRCdir)/SWAP/dcases.dsc $(BINdir)/xsubstr
	rm -f dswap.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SWAP/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
cswap.dsc : $(mySRCdir)/SWAP/ccases.dsc $(BINdir)/xsubstr
	rm -f cswap.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SWAP/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zswap.dsc : $(mySRCdir)/SWAP/zcases.dsc $(BINdir)/xsubstr
	rm -f zswap.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/SWAP/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
swapsrch.o : $(mySRCdir)/swapsrch.c sswap.dsc dswap.dsc cswap.dsc zswap.dsc
	$(XCC) $(XCCFLAGS) -o swapsrch.o -c $(mySRCdir)/swapsrch.c
xswapsrch : swapsrch.o
	$(XCC) $(XCCFLAGS) -o xswapsrch swapsrch.o
scopy.dsc : $(mySRCdir)/COPY/scases.dsc $(BINdir)/xsubstr
	rm -f scopy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/COPY/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
dcopy.dsc : $(mySRCdir)/COPY/dcases.dsc $(BINdir)/xsubstr
	rm -f dcopy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/COPY/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
ccopy.dsc : $(mySRCdir)/COPY/ccases.dsc $(BINdir)/xsubstr
	rm -f ccopy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/COPY/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zcopy.dsc : $(mySRCdir)/COPY/zcases.dsc $(BINdir)/xsubstr
	rm -f zcopy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/COPY/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
copysrch.o : $(mySRCdir)/copysrch.c scopy.dsc dcopy.dsc ccopy.dsc zcopy.dsc
	$(XCC) $(XCCFLAGS) -o copysrch.o -c $(mySRCdir)/copysrch.c
xcopysrch : copysrch.o
	$(XCC) $(XCCFLAGS) -o xcopysrch copysrch.o
saxpy.dsc : $(mySRCdir)/AXPY/scases.dsc $(BINdir)/xsubstr
	rm -f saxpy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPY/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
daxpy.dsc : $(mySRCdir)/AXPY/dcases.dsc $(BINdir)/xsubstr
	rm -f daxpy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPY/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
caxpy.dsc : $(mySRCdir)/AXPY/ccases.dsc $(BINdir)/xsubstr
	rm -f caxpy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPY/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
zaxpy.dsc : $(mySRCdir)/AXPY/zcases.dsc $(BINdir)/xsubstr
	rm -f zaxpy.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/AXPY/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
axpysrch.o : $(mySRCdir)/axpysrch.c saxpy.dsc daxpy.dsc caxpy.dsc zaxpy.dsc
	$(XCC) $(XCCFLAGS) -o axpysrch.o -c $(mySRCdir)/axpysrch.c
xaxpysrch : axpysrch.o
	$(XCC) $(XCCFLAGS) -o xaxpysrch axpysrch.o
siamax.dsc : $(mySRCdir)/IAMAX/scases.dsc $(BINdir)/xsubstr
	rm -f siamax.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/IAMAX/scases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
diamax.dsc : $(mySRCdir)/IAMAX/dcases.dsc $(BINdir)/xsubstr
	rm -f diamax.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/IAMAX/dcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
ciamax.dsc : $(mySRCdir)/IAMAX/ccases.dsc $(BINdir)/xsubstr
	rm -f ciamax.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/IAMAX/ccases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
ziamax.dsc : $(mySRCdir)/IAMAX/zcases.dsc $(BINdir)/xsubstr
	rm -f ziamax.dsc
	$(BINdir)/xsubstr -i $(mySRCdir)/IAMAX/zcases.dsc -o $@ \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
iamaxsrch.o : $(mySRCdir)/iamaxsrch.c siamax.dsc diamax.dsc ciamax.dsc ziamax.dsc
	$(XCC) $(XCCFLAGS) -o iamaxsrch.o -c $(mySRCdir)/iamaxsrch.c
xiamaxsrch : iamaxsrch.o
	$(XCC) $(XCCFLAGS) -o xiamaxsrch iamaxsrch.o
sinstall_rot : xrotsrch
	rm -f GEN/*
	./xrotsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_srot_stub.o : $(mySRCdir)/ROT/rot_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/ROT/rot_stub.c
srottime.o : $(mySRCdir)/rottime.c
	$(KC) $(KCFLAGS) -o srottime.o -c -DSREAL $(mySRCdir)/rottime.c
srottest.o : $(mySRCdir)/rottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o srottest.o -c -DSREAL $(mySRCdir)/rottest.c
srottest : srottest.o ATL_srot_stub.o
	rm -f srot.c srot.o xsrottst
	cp $(mySRCdir)/ROT/$(urout) srot.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c srot.c
	$(CLINKER) $(CLINKFLAGS) -o xsrottst srottest.o \
          ATL_srot_stub.o srot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsrottst -n $(N) $(opt)
srottime : srotcase
srotcase : srottime.o ATL_srot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f srot.c srot.o xsrot
	cp $(mySRCdir)/ROT/$(urout) srot.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c srot.c
	$(CLINKER) $(CLINKFLAGS) -o xsrot srottime.o \
	           ATL_srot_stub.o srot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsrot -n $(N) $(fout) $(opt)
sinstall_axpby : xaxpbysrch
	rm -f GEN/*
	./xaxpbysrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_saxpby_stub.o : $(mySRCdir)/AXPBY/axpby_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/AXPBY/axpby_stub.c
saxpbytime.o : $(mySRCdir)/axpbytime.c
	$(KC) $(KCFLAGS) -o saxpbytime.o -c -DSREAL $(mySRCdir)/axpbytime.c
saxpbytest.o : $(mySRCdir)/axpbytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o saxpbytest.o -c -DSREAL $(mySRCdir)/axpbytest.c
saxpbytest : saxpbytest.o ATL_saxpby_stub.o
	rm -f saxpby.c saxpby.o xsaxpbytst
	cp $(mySRCdir)/AXPBY/$(urout) saxpby.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c saxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xsaxpbytst saxpbytest.o \
          ATL_saxpby_stub.o saxpby.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsaxpbytst -n $(N) $(opt)
saxpbytime : saxpbycase
saxpbycase : saxpbytime.o ATL_saxpby_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f saxpby.c saxpby.o xsaxpby
	cp $(mySRCdir)/AXPBY/$(urout) saxpby.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c saxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xsaxpby saxpbytime.o \
	           ATL_saxpby_stub.o saxpby.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsaxpby -n $(N) $(fout) $(opt)
sinstall_set : xsetsrch
	rm -f GEN/*
	./xsetsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_sset_stub.o : $(mySRCdir)/SET/set_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/SET/set_stub.c
ssettime.o : $(mySRCdir)/settime.c
	$(KC) $(KCFLAGS) -o ssettime.o -c -DSREAL $(mySRCdir)/settime.c
ssettest.o : $(mySRCdir)/settest.c tstlib.grd
	$(KC) $(KCFLAGS) -o ssettest.o -c -DSREAL $(mySRCdir)/settest.c
ssettest : ssettest.o ATL_sset_stub.o
	rm -f sset.c sset.o xssettst
	cp $(mySRCdir)/SET/$(urout) sset.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sset.c
	$(CLINKER) $(CLINKFLAGS) -o xssettst ssettest.o \
          ATL_sset_stub.o sset.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xssettst -n $(N) $(opt)
ssettime : ssetcase
ssetcase : ssettime.o ATL_sset_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f sset.c sset.o xsset
	cp $(mySRCdir)/SET/$(urout) sset.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sset.c
	$(CLINKER) $(CLINKFLAGS) -o xsset ssettime.o \
	           ATL_sset_stub.o sset.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsset -n $(N) $(fout) $(opt)
sinstall_cpsc : xcpscsrch
	rm -f GEN/*
	./xcpscsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_scpsc_stub.o : $(mySRCdir)/CPSC/cpsc_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/CPSC/cpsc_stub.c
scpsctime.o : $(mySRCdir)/cpsctime.c
	$(KC) $(KCFLAGS) -o scpsctime.o -c -DSREAL $(mySRCdir)/cpsctime.c
scpsctest.o : $(mySRCdir)/cpsctest.c tstlib.grd
	$(KC) $(KCFLAGS) -o scpsctest.o -c -DSREAL $(mySRCdir)/cpsctest.c
scpsctest : scpsctest.o ATL_scpsc_stub.o
	rm -f scpsc.c scpsc.o xscpsctst
	cp $(mySRCdir)/CPSC/$(urout) scpsc.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c scpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xscpsctst scpsctest.o \
          ATL_scpsc_stub.o scpsc.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xscpsctst -n $(N) $(opt)
scpsctime : scpsccase
scpsccase : scpsctime.o ATL_scpsc_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f scpsc.c scpsc.o xscpsc
	cp $(mySRCdir)/CPSC/$(urout) scpsc.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c scpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xscpsc scpsctime.o \
	           ATL_scpsc_stub.o scpsc.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xscpsc -n $(N) $(fout) $(opt)
sinstall_asum : xasumsrch
	rm -f GEN/*
	./xasumsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_sasum_stub.o : $(mySRCdir)/ASUM/asum_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/ASUM/asum_stub.c
sasumtime.o : $(mySRCdir)/asumtime.c
	$(KC) $(KCFLAGS) -o sasumtime.o -c -DSREAL $(mySRCdir)/asumtime.c
sasumtest.o : $(mySRCdir)/asumtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o sasumtest.o -c -DSREAL $(mySRCdir)/asumtest.c
sasumtest : sasumtest.o ATL_sasum_stub.o
	rm -f sasum.c sasum.o xsasumtst
	cp $(mySRCdir)/ASUM/$(urout) sasum.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sasum.c
	$(CLINKER) $(CLINKFLAGS) -o xsasumtst sasumtest.o \
          ATL_sasum_stub.o sasum.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsasumtst -n $(N) $(opt)
sasumtime : sasumcase
sasumcase : sasumtime.o ATL_sasum_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f sasum.c sasum.o xsasum
	cp $(mySRCdir)/ASUM/$(urout) sasum.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sasum.c
	$(CLINKER) $(CLINKFLAGS) -o xsasum sasumtime.o \
	           ATL_sasum_stub.o sasum.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsasum -n $(N) $(fout) $(opt)
sinstall_nrm2 : xnrm2srch
	rm -f GEN/*
	./xnrm2srch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_snrm2_stub.o : $(mySRCdir)/NRM2/nrm2_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/NRM2/nrm2_stub.c
snrm2time.o : $(mySRCdir)/nrm2time.c
	$(KC) $(KCFLAGS) -o snrm2time.o -c -DSREAL $(mySRCdir)/nrm2time.c
snrm2test.o : $(mySRCdir)/nrm2test.c tstlib.grd
	$(KC) $(KCFLAGS) -o snrm2test.o -c -DSREAL $(mySRCdir)/nrm2test.c
snrm2test : snrm2test.o ATL_snrm2_stub.o
	rm -f snrm2.c snrm2.o xsnrm2tst
	cp $(mySRCdir)/NRM2/$(urout) snrm2.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c snrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xsnrm2tst snrm2test.o \
          ATL_snrm2_stub.o snrm2.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsnrm2tst -n $(N) $(opt)
snrm2time : snrm2case
snrm2case : snrm2time.o ATL_snrm2_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f snrm2.c snrm2.o xsnrm2
	cp $(mySRCdir)/NRM2/$(urout) snrm2.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c snrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xsnrm2 snrm2time.o \
	           ATL_snrm2_stub.o snrm2.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsnrm2 -n $(N) $(fout) $(opt)
sinstall_dot : xdotsrch
	rm -f GEN/*
	./xdotsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_sdot_stub.o : $(mySRCdir)/DOT/dot_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/DOT/dot_stub.c
sdottime.o : $(mySRCdir)/dottime.c
	$(KC) $(KCFLAGS) -o sdottime.o -c -DSREAL $(mySRCdir)/dottime.c
sdottest.o : $(mySRCdir)/dottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o sdottest.o -c -DSREAL $(mySRCdir)/dottest.c
sdottest : sdottest.o ATL_sdot_stub.o
	rm -f sdot.c sdot.o xsdottst
	cp $(mySRCdir)/DOT/$(urout) sdot.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sdot.c
	$(CLINKER) $(CLINKFLAGS) -o xsdottst sdottest.o \
          ATL_sdot_stub.o sdot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsdottst -n $(N) $(opt)
sdottime : sdotcase
sdotcase : sdottime.o ATL_sdot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f sdot.c sdot.o xsdot
	cp $(mySRCdir)/DOT/$(urout) sdot.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sdot.c
	$(CLINKER) $(CLINKFLAGS) -o xsdot sdottime.o \
	           ATL_sdot_stub.o sdot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsdot -n $(N) $(fout) $(opt)
sinstall_scal : xscalsrch
	rm -f GEN/*
	./xscalsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_sscal_stub.o : $(mySRCdir)/SCAL/scal_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/SCAL/scal_stub.c
sscaltime.o : $(mySRCdir)/scaltime.c
	$(KC) $(KCFLAGS) -o sscaltime.o -c -DSREAL $(mySRCdir)/scaltime.c
sscaltest.o : $(mySRCdir)/scaltest.c tstlib.grd
	$(KC) $(KCFLAGS) -o sscaltest.o -c -DSREAL $(mySRCdir)/scaltest.c
sscaltest : sscaltest.o ATL_sscal_stub.o
	rm -f sscal.c sscal.o xsscaltst
	cp $(mySRCdir)/SCAL/$(urout) sscal.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sscal.c
	$(CLINKER) $(CLINKFLAGS) -o xsscaltst sscaltest.o \
          ATL_sscal_stub.o sscal.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsscaltst -n $(N) $(opt)
sscaltime : sscalcase
sscalcase : sscaltime.o ATL_sscal_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f sscal.c sscal.o xsscal
	cp $(mySRCdir)/SCAL/$(urout) sscal.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sscal.c
	$(CLINKER) $(CLINKFLAGS) -o xsscal sscaltime.o \
	           ATL_sscal_stub.o sscal.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsscal -n $(N) $(fout) $(opt)
sinstall_swap : xswapsrch
	rm -f GEN/*
	./xswapsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_sswap_stub.o : $(mySRCdir)/SWAP/swap_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/SWAP/swap_stub.c
sswaptime.o : $(mySRCdir)/swaptime.c
	$(KC) $(KCFLAGS) -o sswaptime.o -c -DSREAL $(mySRCdir)/swaptime.c
sswaptest.o : $(mySRCdir)/swaptest.c tstlib.grd
	$(KC) $(KCFLAGS) -o sswaptest.o -c -DSREAL $(mySRCdir)/swaptest.c
sswaptest : sswaptest.o ATL_sswap_stub.o
	rm -f sswap.c sswap.o xsswaptst
	cp $(mySRCdir)/SWAP/$(urout) sswap.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sswap.c
	$(CLINKER) $(CLINKFLAGS) -o xsswaptst sswaptest.o \
          ATL_sswap_stub.o sswap.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsswaptst -n $(N) $(opt)
sswaptime : sswapcase
sswapcase : sswaptime.o ATL_sswap_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f sswap.c sswap.o xsswap
	cp $(mySRCdir)/SWAP/$(urout) sswap.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c sswap.c
	$(CLINKER) $(CLINKFLAGS) -o xsswap sswaptime.o \
	           ATL_sswap_stub.o sswap.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsswap -n $(N) $(fout) $(opt)
sinstall_copy : xcopysrch
	rm -f GEN/*
	./xcopysrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_scopy_stub.o : $(mySRCdir)/COPY/copy_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/COPY/copy_stub.c
scopytime.o : $(mySRCdir)/copytime.c
	$(KC) $(KCFLAGS) -o scopytime.o -c -DSREAL $(mySRCdir)/copytime.c
scopytest.o : $(mySRCdir)/copytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o scopytest.o -c -DSREAL $(mySRCdir)/copytest.c
scopytest : scopytest.o ATL_scopy_stub.o
	rm -f scopy.c scopy.o xscopytst
	cp $(mySRCdir)/COPY/$(urout) scopy.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c scopy.c
	$(CLINKER) $(CLINKFLAGS) -o xscopytst scopytest.o \
          ATL_scopy_stub.o scopy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xscopytst -n $(N) $(opt)
scopytime : scopycase
scopycase : scopytime.o ATL_scopy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f scopy.c scopy.o xscopy
	cp $(mySRCdir)/COPY/$(urout) scopy.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c scopy.c
	$(CLINKER) $(CLINKFLAGS) -o xscopy scopytime.o \
	           ATL_scopy_stub.o scopy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xscopy -n $(N) $(fout) $(opt)
sinstall_axpy : xaxpysrch
	rm -f GEN/*
	./xaxpysrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_saxpy_stub.o : $(mySRCdir)/AXPY/axpy_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/AXPY/axpy_stub.c
saxpytime.o : $(mySRCdir)/axpytime.c
	$(KC) $(KCFLAGS) -o saxpytime.o -c -DSREAL $(mySRCdir)/axpytime.c
saxpytest.o : $(mySRCdir)/axpytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o saxpytest.o -c -DSREAL $(mySRCdir)/axpytest.c
saxpytest : saxpytest.o ATL_saxpy_stub.o
	rm -f saxpy.c saxpy.o xsaxpytst
	cp $(mySRCdir)/AXPY/$(urout) saxpy.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c saxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xsaxpytst saxpytest.o \
          ATL_saxpy_stub.o saxpy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsaxpytst -n $(N) $(opt)
saxpytime : saxpycase
saxpycase : saxpytime.o ATL_saxpy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f saxpy.c saxpy.o xsaxpy
	cp $(mySRCdir)/AXPY/$(urout) saxpy.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c saxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xsaxpy saxpytime.o \
	           ATL_saxpy_stub.o saxpy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsaxpy -n $(N) $(fout) $(opt)
sinstall_iamax : xiamaxsrch
	rm -f GEN/*
	./xiamaxsrch -p s
	cp -f GEN/* $(L1Bdir)/.
ATL_siamax_stub.o : $(mySRCdir)/IAMAX/iamax_stub.c
	$(KC) $(KCFLAGS) -DSREAL -c -o $@ $(mySRCdir)/IAMAX/iamax_stub.c
siamaxtime.o : $(mySRCdir)/iamaxtime.c
	$(KC) $(KCFLAGS) -o siamaxtime.o -c -DSREAL $(mySRCdir)/iamaxtime.c
siamaxtest.o : $(mySRCdir)/iamaxtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o siamaxtest.o -c -DSREAL $(mySRCdir)/iamaxtest.c
siamaxtest : siamaxtest.o ATL_siamax_stub.o
	rm -f siamax.c siamax.o xsiamaxtst
	cp $(mySRCdir)/IAMAX/$(urout) siamax.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c siamax.c
	$(CLINKER) $(CLINKFLAGS) -o xsiamaxtst siamaxtest.o \
          ATL_siamax_stub.o siamax.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsiamaxtst -n $(N) $(opt)
siamaxtime : siamaxcase
siamaxcase : siamaxtime.o ATL_siamax_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f siamax.c siamax.o xsiamax
	cp $(mySRCdir)/IAMAX/$(urout) siamax.c
	$(sUCC) $(CDEFS) $(sUCCFLAGS) -DSREAL -c siamax.c
	$(CLINKER) $(CLINKFLAGS) -o xsiamax siamaxtime.o \
	           ATL_siamax_stub.o siamax.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xsiamax -n $(N) $(fout) $(opt)

dinstall_rot : xrotsrch
	rm -f GEN/*
	./xrotsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_drot_stub.o : $(mySRCdir)/ROT/rot_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/ROT/rot_stub.c
drottime.o : $(mySRCdir)/rottime.c
	$(KC) $(KCFLAGS) -o drottime.o -c -DDREAL $(mySRCdir)/rottime.c
drottest.o : $(mySRCdir)/rottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o drottest.o -c -DDREAL $(mySRCdir)/rottest.c
drottest : drottest.o ATL_drot_stub.o
	rm -f drot.c drot.o xdrottst
	cp $(mySRCdir)/ROT/$(urout) drot.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c drot.c
	$(CLINKER) $(CLINKFLAGS) -o xdrottst drottest.o \
          ATL_drot_stub.o drot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdrottst -n $(N) $(opt)
drottime : drotcase
drotcase : drottime.o ATL_drot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f drot.c drot.o xdrot
	cp $(mySRCdir)/ROT/$(urout) drot.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c drot.c
	$(CLINKER) $(CLINKFLAGS) -o xdrot drottime.o \
	           ATL_drot_stub.o drot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdrot -n $(N) $(fout) $(opt)
dinstall_axpby : xaxpbysrch
	rm -f GEN/*
	./xaxpbysrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_daxpby_stub.o : $(mySRCdir)/AXPBY/axpby_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/AXPBY/axpby_stub.c
daxpbytime.o : $(mySRCdir)/axpbytime.c
	$(KC) $(KCFLAGS) -o daxpbytime.o -c -DDREAL $(mySRCdir)/axpbytime.c
daxpbytest.o : $(mySRCdir)/axpbytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o daxpbytest.o -c -DDREAL $(mySRCdir)/axpbytest.c
daxpbytest : daxpbytest.o ATL_daxpby_stub.o
	rm -f daxpby.c daxpby.o xdaxpbytst
	cp $(mySRCdir)/AXPBY/$(urout) daxpby.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c daxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xdaxpbytst daxpbytest.o \
          ATL_daxpby_stub.o daxpby.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdaxpbytst -n $(N) $(opt)
daxpbytime : daxpbycase
daxpbycase : daxpbytime.o ATL_daxpby_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f daxpby.c daxpby.o xdaxpby
	cp $(mySRCdir)/AXPBY/$(urout) daxpby.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c daxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xdaxpby daxpbytime.o \
	           ATL_daxpby_stub.o daxpby.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdaxpby -n $(N) $(fout) $(opt)
dinstall_set : xsetsrch
	rm -f GEN/*
	./xsetsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dset_stub.o : $(mySRCdir)/SET/set_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/SET/set_stub.c
dsettime.o : $(mySRCdir)/settime.c
	$(KC) $(KCFLAGS) -o dsettime.o -c -DDREAL $(mySRCdir)/settime.c
dsettest.o : $(mySRCdir)/settest.c tstlib.grd
	$(KC) $(KCFLAGS) -o dsettest.o -c -DDREAL $(mySRCdir)/settest.c
dsettest : dsettest.o ATL_dset_stub.o
	rm -f dset.c dset.o xdsettst
	cp $(mySRCdir)/SET/$(urout) dset.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dset.c
	$(CLINKER) $(CLINKFLAGS) -o xdsettst dsettest.o \
          ATL_dset_stub.o dset.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdsettst -n $(N) $(opt)
dsettime : dsetcase
dsetcase : dsettime.o ATL_dset_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dset.c dset.o xdset
	cp $(mySRCdir)/SET/$(urout) dset.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dset.c
	$(CLINKER) $(CLINKFLAGS) -o xdset dsettime.o \
	           ATL_dset_stub.o dset.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdset -n $(N) $(fout) $(opt)
dinstall_cpsc : xcpscsrch
	rm -f GEN/*
	./xcpscsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dcpsc_stub.o : $(mySRCdir)/CPSC/cpsc_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/CPSC/cpsc_stub.c
dcpsctime.o : $(mySRCdir)/cpsctime.c
	$(KC) $(KCFLAGS) -o dcpsctime.o -c -DDREAL $(mySRCdir)/cpsctime.c
dcpsctest.o : $(mySRCdir)/cpsctest.c tstlib.grd
	$(KC) $(KCFLAGS) -o dcpsctest.o -c -DDREAL $(mySRCdir)/cpsctest.c
dcpsctest : dcpsctest.o ATL_dcpsc_stub.o
	rm -f dcpsc.c dcpsc.o xdcpsctst
	cp $(mySRCdir)/CPSC/$(urout) dcpsc.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dcpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xdcpsctst dcpsctest.o \
          ATL_dcpsc_stub.o dcpsc.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdcpsctst -n $(N) $(opt)
dcpsctime : dcpsccase
dcpsccase : dcpsctime.o ATL_dcpsc_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dcpsc.c dcpsc.o xdcpsc
	cp $(mySRCdir)/CPSC/$(urout) dcpsc.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dcpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xdcpsc dcpsctime.o \
	           ATL_dcpsc_stub.o dcpsc.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdcpsc -n $(N) $(fout) $(opt)
dinstall_asum : xasumsrch
	rm -f GEN/*
	./xasumsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dasum_stub.o : $(mySRCdir)/ASUM/asum_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/ASUM/asum_stub.c
dasumtime.o : $(mySRCdir)/asumtime.c
	$(KC) $(KCFLAGS) -o dasumtime.o -c -DDREAL $(mySRCdir)/asumtime.c
dasumtest.o : $(mySRCdir)/asumtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o dasumtest.o -c -DDREAL $(mySRCdir)/asumtest.c
dasumtest : dasumtest.o ATL_dasum_stub.o
	rm -f dasum.c dasum.o xdasumtst
	cp $(mySRCdir)/ASUM/$(urout) dasum.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dasum.c
	$(CLINKER) $(CLINKFLAGS) -o xdasumtst dasumtest.o \
          ATL_dasum_stub.o dasum.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdasumtst -n $(N) $(opt)
dasumtime : dasumcase
dasumcase : dasumtime.o ATL_dasum_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dasum.c dasum.o xdasum
	cp $(mySRCdir)/ASUM/$(urout) dasum.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dasum.c
	$(CLINKER) $(CLINKFLAGS) -o xdasum dasumtime.o \
	           ATL_dasum_stub.o dasum.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdasum -n $(N) $(fout) $(opt)
dinstall_nrm2 : xnrm2srch
	rm -f GEN/*
	./xnrm2srch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dnrm2_stub.o : $(mySRCdir)/NRM2/nrm2_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/NRM2/nrm2_stub.c
dnrm2time.o : $(mySRCdir)/nrm2time.c
	$(KC) $(KCFLAGS) -o dnrm2time.o -c -DDREAL $(mySRCdir)/nrm2time.c
dnrm2test.o : $(mySRCdir)/nrm2test.c tstlib.grd
	$(KC) $(KCFLAGS) -o dnrm2test.o -c -DDREAL $(mySRCdir)/nrm2test.c
dnrm2test : dnrm2test.o ATL_dnrm2_stub.o
	rm -f dnrm2.c dnrm2.o xdnrm2tst
	cp $(mySRCdir)/NRM2/$(urout) dnrm2.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dnrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xdnrm2tst dnrm2test.o \
          ATL_dnrm2_stub.o dnrm2.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdnrm2tst -n $(N) $(opt)
dnrm2time : dnrm2case
dnrm2case : dnrm2time.o ATL_dnrm2_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dnrm2.c dnrm2.o xdnrm2
	cp $(mySRCdir)/NRM2/$(urout) dnrm2.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dnrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xdnrm2 dnrm2time.o \
	           ATL_dnrm2_stub.o dnrm2.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdnrm2 -n $(N) $(fout) $(opt)
dinstall_dot : xdotsrch
	rm -f GEN/*
	./xdotsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_ddot_stub.o : $(mySRCdir)/DOT/dot_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/DOT/dot_stub.c
ddottime.o : $(mySRCdir)/dottime.c
	$(KC) $(KCFLAGS) -o ddottime.o -c -DDREAL $(mySRCdir)/dottime.c
ddottest.o : $(mySRCdir)/dottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o ddottest.o -c -DDREAL $(mySRCdir)/dottest.c
ddottest : ddottest.o ATL_ddot_stub.o
	rm -f ddot.c ddot.o xddottst
	cp $(mySRCdir)/DOT/$(urout) ddot.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c ddot.c
	$(CLINKER) $(CLINKFLAGS) -o xddottst ddottest.o \
          ATL_ddot_stub.o ddot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xddottst -n $(N) $(opt)
ddottime : ddotcase
ddotcase : ddottime.o ATL_ddot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f ddot.c ddot.o xddot
	cp $(mySRCdir)/DOT/$(urout) ddot.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c ddot.c
	$(CLINKER) $(CLINKFLAGS) -o xddot ddottime.o \
	           ATL_ddot_stub.o ddot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xddot -n $(N) $(fout) $(opt)
dinstall_scal : xscalsrch
	rm -f GEN/*
	./xscalsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dscal_stub.o : $(mySRCdir)/SCAL/scal_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/SCAL/scal_stub.c
dscaltime.o : $(mySRCdir)/scaltime.c
	$(KC) $(KCFLAGS) -o dscaltime.o -c -DDREAL $(mySRCdir)/scaltime.c
dscaltest.o : $(mySRCdir)/scaltest.c tstlib.grd
	$(KC) $(KCFLAGS) -o dscaltest.o -c -DDREAL $(mySRCdir)/scaltest.c
dscaltest : dscaltest.o ATL_dscal_stub.o
	rm -f dscal.c dscal.o xdscaltst
	cp $(mySRCdir)/SCAL/$(urout) dscal.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dscal.c
	$(CLINKER) $(CLINKFLAGS) -o xdscaltst dscaltest.o \
          ATL_dscal_stub.o dscal.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdscaltst -n $(N) $(opt)
dscaltime : dscalcase
dscalcase : dscaltime.o ATL_dscal_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dscal.c dscal.o xdscal
	cp $(mySRCdir)/SCAL/$(urout) dscal.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dscal.c
	$(CLINKER) $(CLINKFLAGS) -o xdscal dscaltime.o \
	           ATL_dscal_stub.o dscal.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdscal -n $(N) $(fout) $(opt)
dinstall_swap : xswapsrch
	rm -f GEN/*
	./xswapsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dswap_stub.o : $(mySRCdir)/SWAP/swap_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/SWAP/swap_stub.c
dswaptime.o : $(mySRCdir)/swaptime.c
	$(KC) $(KCFLAGS) -o dswaptime.o -c -DDREAL $(mySRCdir)/swaptime.c
dswaptest.o : $(mySRCdir)/swaptest.c tstlib.grd
	$(KC) $(KCFLAGS) -o dswaptest.o -c -DDREAL $(mySRCdir)/swaptest.c
dswaptest : dswaptest.o ATL_dswap_stub.o
	rm -f dswap.c dswap.o xdswaptst
	cp $(mySRCdir)/SWAP/$(urout) dswap.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dswap.c
	$(CLINKER) $(CLINKFLAGS) -o xdswaptst dswaptest.o \
          ATL_dswap_stub.o dswap.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdswaptst -n $(N) $(opt)
dswaptime : dswapcase
dswapcase : dswaptime.o ATL_dswap_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dswap.c dswap.o xdswap
	cp $(mySRCdir)/SWAP/$(urout) dswap.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dswap.c
	$(CLINKER) $(CLINKFLAGS) -o xdswap dswaptime.o \
	           ATL_dswap_stub.o dswap.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdswap -n $(N) $(fout) $(opt)
dinstall_copy : xcopysrch
	rm -f GEN/*
	./xcopysrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_dcopy_stub.o : $(mySRCdir)/COPY/copy_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/COPY/copy_stub.c
dcopytime.o : $(mySRCdir)/copytime.c
	$(KC) $(KCFLAGS) -o dcopytime.o -c -DDREAL $(mySRCdir)/copytime.c
dcopytest.o : $(mySRCdir)/copytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o dcopytest.o -c -DDREAL $(mySRCdir)/copytest.c
dcopytest : dcopytest.o ATL_dcopy_stub.o
	rm -f dcopy.c dcopy.o xdcopytst
	cp $(mySRCdir)/COPY/$(urout) dcopy.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dcopy.c
	$(CLINKER) $(CLINKFLAGS) -o xdcopytst dcopytest.o \
          ATL_dcopy_stub.o dcopy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdcopytst -n $(N) $(opt)
dcopytime : dcopycase
dcopycase : dcopytime.o ATL_dcopy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f dcopy.c dcopy.o xdcopy
	cp $(mySRCdir)/COPY/$(urout) dcopy.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c dcopy.c
	$(CLINKER) $(CLINKFLAGS) -o xdcopy dcopytime.o \
	           ATL_dcopy_stub.o dcopy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdcopy -n $(N) $(fout) $(opt)
dinstall_axpy : xaxpysrch
	rm -f GEN/*
	./xaxpysrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_daxpy_stub.o : $(mySRCdir)/AXPY/axpy_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/AXPY/axpy_stub.c
daxpytime.o : $(mySRCdir)/axpytime.c
	$(KC) $(KCFLAGS) -o daxpytime.o -c -DDREAL $(mySRCdir)/axpytime.c
daxpytest.o : $(mySRCdir)/axpytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o daxpytest.o -c -DDREAL $(mySRCdir)/axpytest.c
daxpytest : daxpytest.o ATL_daxpy_stub.o
	rm -f daxpy.c daxpy.o xdaxpytst
	cp $(mySRCdir)/AXPY/$(urout) daxpy.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c daxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xdaxpytst daxpytest.o \
          ATL_daxpy_stub.o daxpy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdaxpytst -n $(N) $(opt)
daxpytime : daxpycase
daxpycase : daxpytime.o ATL_daxpy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f daxpy.c daxpy.o xdaxpy
	cp $(mySRCdir)/AXPY/$(urout) daxpy.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c daxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xdaxpy daxpytime.o \
	           ATL_daxpy_stub.o daxpy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdaxpy -n $(N) $(fout) $(opt)
dinstall_iamax : xiamaxsrch
	rm -f GEN/*
	./xiamaxsrch -p d
	cp -f GEN/* $(L1Bdir)/.
ATL_diamax_stub.o : $(mySRCdir)/IAMAX/iamax_stub.c
	$(KC) $(KCFLAGS) -DDREAL -c -o $@ $(mySRCdir)/IAMAX/iamax_stub.c
diamaxtime.o : $(mySRCdir)/iamaxtime.c
	$(KC) $(KCFLAGS) -o diamaxtime.o -c -DDREAL $(mySRCdir)/iamaxtime.c
diamaxtest.o : $(mySRCdir)/iamaxtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o diamaxtest.o -c -DDREAL $(mySRCdir)/iamaxtest.c
diamaxtest : diamaxtest.o ATL_diamax_stub.o
	rm -f diamax.c diamax.o xdiamaxtst
	cp $(mySRCdir)/IAMAX/$(urout) diamax.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c diamax.c
	$(CLINKER) $(CLINKFLAGS) -o xdiamaxtst diamaxtest.o \
          ATL_diamax_stub.o diamax.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdiamaxtst -n $(N) $(opt)
diamaxtime : diamaxcase
diamaxcase : diamaxtime.o ATL_diamax_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f diamax.c diamax.o xdiamax
	cp $(mySRCdir)/IAMAX/$(urout) diamax.c
	$(dUCC) $(CDEFS) $(dUCCFLAGS) -DDREAL -c diamax.c
	$(CLINKER) $(CLINKFLAGS) -o xdiamax diamaxtime.o \
	           ATL_diamax_stub.o diamax.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xdiamax -n $(N) $(fout) $(opt)

cinstall_rot : xrotsrch
	rm -f GEN/*
	./xrotsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_crot_stub.o : $(mySRCdir)/ROT/rot_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/ROT/rot_stub.c
crottime.o : $(mySRCdir)/rottime.c
	$(KC) $(KCFLAGS) -o crottime.o -c -DSCPLX $(mySRCdir)/rottime.c
crottest.o : $(mySRCdir)/rottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o crottest.o -c -DSCPLX $(mySRCdir)/rottest.c
crottest : crottest.o ATL_crot_stub.o
	rm -f crot.c crot.o xcrottst
	cp $(mySRCdir)/ROT/$(urout) crot.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c crot.c
	$(CLINKER) $(CLINKFLAGS) -o xcrottst crottest.o \
          ATL_crot_stub.o crot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcrottst -n $(N) $(opt)
crottime : crotcase
crotcase : crottime.o ATL_crot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f crot.c crot.o xcrot
	cp $(mySRCdir)/ROT/$(urout) crot.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c crot.c
	$(CLINKER) $(CLINKFLAGS) -o xcrot crottime.o \
	           ATL_crot_stub.o crot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcrot -n $(N) $(fout) $(opt)
cinstall_axpby : xaxpbysrch
	rm -f GEN/*
	./xaxpbysrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_caxpby_stub.o : $(mySRCdir)/AXPBY/axpby_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/AXPBY/axpby_stub.c
caxpbytime.o : $(mySRCdir)/axpbytime.c
	$(KC) $(KCFLAGS) -o caxpbytime.o -c -DSCPLX $(mySRCdir)/axpbytime.c
caxpbytest.o : $(mySRCdir)/axpbytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o caxpbytest.o -c -DSCPLX $(mySRCdir)/axpbytest.c
caxpbytest : caxpbytest.o ATL_caxpby_stub.o
	rm -f caxpby.c caxpby.o xcaxpbytst
	cp $(mySRCdir)/AXPBY/$(urout) caxpby.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c caxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xcaxpbytst caxpbytest.o \
          ATL_caxpby_stub.o caxpby.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcaxpbytst -n $(N) $(opt)
caxpbytime : caxpbycase
caxpbycase : caxpbytime.o ATL_caxpby_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f caxpby.c caxpby.o xcaxpby
	cp $(mySRCdir)/AXPBY/$(urout) caxpby.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c caxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xcaxpby caxpbytime.o \
	           ATL_caxpby_stub.o caxpby.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcaxpby -n $(N) $(fout) $(opt)
cinstall_set : xsetsrch
	rm -f GEN/*
	./xsetsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_cset_stub.o : $(mySRCdir)/SET/set_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/SET/set_stub.c
csettime.o : $(mySRCdir)/settime.c
	$(KC) $(KCFLAGS) -o csettime.o -c -DSCPLX $(mySRCdir)/settime.c
csettest.o : $(mySRCdir)/settest.c tstlib.grd
	$(KC) $(KCFLAGS) -o csettest.o -c -DSCPLX $(mySRCdir)/settest.c
csettest : csettest.o ATL_cset_stub.o
	rm -f cset.c cset.o xcsettst
	cp $(mySRCdir)/SET/$(urout) cset.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cset.c
	$(CLINKER) $(CLINKFLAGS) -o xcsettst csettest.o \
          ATL_cset_stub.o cset.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcsettst -n $(N) $(opt)
csettime : csetcase
csetcase : csettime.o ATL_cset_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f cset.c cset.o xcset
	cp $(mySRCdir)/SET/$(urout) cset.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cset.c
	$(CLINKER) $(CLINKFLAGS) -o xcset csettime.o \
	           ATL_cset_stub.o cset.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcset -n $(N) $(fout) $(opt)
cinstall_cpsc : xcpscsrch
	rm -f GEN/*
	./xcpscsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_ccpsc_stub.o : $(mySRCdir)/CPSC/cpsc_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/CPSC/cpsc_stub.c
ccpsctime.o : $(mySRCdir)/cpsctime.c
	$(KC) $(KCFLAGS) -o ccpsctime.o -c -DSCPLX $(mySRCdir)/cpsctime.c
ccpsctest.o : $(mySRCdir)/cpsctest.c tstlib.grd
	$(KC) $(KCFLAGS) -o ccpsctest.o -c -DSCPLX $(mySRCdir)/cpsctest.c
ccpsctest : ccpsctest.o ATL_ccpsc_stub.o
	rm -f ccpsc.c ccpsc.o xccpsctst
	cp $(mySRCdir)/CPSC/$(urout) ccpsc.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c ccpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xccpsctst ccpsctest.o \
          ATL_ccpsc_stub.o ccpsc.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xccpsctst -n $(N) $(opt)
ccpsctime : ccpsccase
ccpsccase : ccpsctime.o ATL_ccpsc_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f ccpsc.c ccpsc.o xccpsc
	cp $(mySRCdir)/CPSC/$(urout) ccpsc.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c ccpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xccpsc ccpsctime.o \
	           ATL_ccpsc_stub.o ccpsc.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xccpsc -n $(N) $(fout) $(opt)
cinstall_asum : xasumsrch
	rm -f GEN/*
	./xasumsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_casum_stub.o : $(mySRCdir)/ASUM/asum_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/ASUM/asum_stub.c
casumtime.o : $(mySRCdir)/asumtime.c
	$(KC) $(KCFLAGS) -o casumtime.o -c -DSCPLX $(mySRCdir)/asumtime.c
casumtest.o : $(mySRCdir)/asumtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o casumtest.o -c -DSCPLX $(mySRCdir)/asumtest.c
casumtest : casumtest.o ATL_casum_stub.o
	rm -f casum.c casum.o xcasumtst
	cp $(mySRCdir)/ASUM/$(urout) casum.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c casum.c
	$(CLINKER) $(CLINKFLAGS) -o xcasumtst casumtest.o \
          ATL_casum_stub.o casum.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcasumtst -n $(N) $(opt)
casumtime : casumcase
casumcase : casumtime.o ATL_casum_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f casum.c casum.o xcasum
	cp $(mySRCdir)/ASUM/$(urout) casum.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c casum.c
	$(CLINKER) $(CLINKFLAGS) -o xcasum casumtime.o \
	           ATL_casum_stub.o casum.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcasum -n $(N) $(fout) $(opt)
cinstall_nrm2 : xnrm2srch
	rm -f GEN/*
	./xnrm2srch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_cnrm2_stub.o : $(mySRCdir)/NRM2/nrm2_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/NRM2/nrm2_stub.c
cnrm2time.o : $(mySRCdir)/nrm2time.c
	$(KC) $(KCFLAGS) -o cnrm2time.o -c -DSCPLX $(mySRCdir)/nrm2time.c
cnrm2test.o : $(mySRCdir)/nrm2test.c tstlib.grd
	$(KC) $(KCFLAGS) -o cnrm2test.o -c -DSCPLX $(mySRCdir)/nrm2test.c
cnrm2test : cnrm2test.o ATL_cnrm2_stub.o
	rm -f cnrm2.c cnrm2.o xcnrm2tst
	cp $(mySRCdir)/NRM2/$(urout) cnrm2.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cnrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xcnrm2tst cnrm2test.o \
          ATL_cnrm2_stub.o cnrm2.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcnrm2tst -n $(N) $(opt)
cnrm2time : cnrm2case
cnrm2case : cnrm2time.o ATL_cnrm2_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f cnrm2.c cnrm2.o xcnrm2
	cp $(mySRCdir)/NRM2/$(urout) cnrm2.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cnrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xcnrm2 cnrm2time.o \
	           ATL_cnrm2_stub.o cnrm2.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcnrm2 -n $(N) $(fout) $(opt)
cinstall_dot : xdotsrch
	rm -f GEN/*
	./xdotsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_cdot_stub.o : $(mySRCdir)/DOT/dot_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/DOT/dot_stub.c
cdottime.o : $(mySRCdir)/dottime.c
	$(KC) $(KCFLAGS) -o cdottime.o -c -DSCPLX $(mySRCdir)/dottime.c
cdottest.o : $(mySRCdir)/dottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o cdottest.o -c -DSCPLX $(mySRCdir)/dottest.c
cdottest : cdottest.o ATL_cdot_stub.o
	rm -f cdot.c cdot.o xcdottst
	cp $(mySRCdir)/DOT/$(urout) cdot.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cdot.c
	$(CLINKER) $(CLINKFLAGS) -o xcdottst cdottest.o \
          ATL_cdot_stub.o cdot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcdottst -n $(N) $(opt)
cdottime : cdotcase
cdotcase : cdottime.o ATL_cdot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f cdot.c cdot.o xcdot
	cp $(mySRCdir)/DOT/$(urout) cdot.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cdot.c
	$(CLINKER) $(CLINKFLAGS) -o xcdot cdottime.o \
	           ATL_cdot_stub.o cdot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcdot -n $(N) $(fout) $(opt)
cinstall_scal : xscalsrch
	rm -f GEN/*
	./xscalsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_cscal_stub.o : $(mySRCdir)/SCAL/scal_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/SCAL/scal_stub.c
cscaltime.o : $(mySRCdir)/scaltime.c
	$(KC) $(KCFLAGS) -o cscaltime.o -c -DSCPLX $(mySRCdir)/scaltime.c
cscaltest.o : $(mySRCdir)/scaltest.c tstlib.grd
	$(KC) $(KCFLAGS) -o cscaltest.o -c -DSCPLX $(mySRCdir)/scaltest.c
cscaltest : cscaltest.o ATL_cscal_stub.o
	rm -f cscal.c cscal.o xcscaltst
	cp $(mySRCdir)/SCAL/$(urout) cscal.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cscal.c
	$(CLINKER) $(CLINKFLAGS) -o xcscaltst cscaltest.o \
          ATL_cscal_stub.o cscal.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcscaltst -n $(N) $(opt)
cscaltime : cscalcase
cscalcase : cscaltime.o ATL_cscal_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f cscal.c cscal.o xcscal
	cp $(mySRCdir)/SCAL/$(urout) cscal.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cscal.c
	$(CLINKER) $(CLINKFLAGS) -o xcscal cscaltime.o \
	           ATL_cscal_stub.o cscal.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcscal -n $(N) $(fout) $(opt)
cinstall_swap : xswapsrch
	rm -f GEN/*
	./xswapsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_cswap_stub.o : $(mySRCdir)/SWAP/swap_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/SWAP/swap_stub.c
cswaptime.o : $(mySRCdir)/swaptime.c
	$(KC) $(KCFLAGS) -o cswaptime.o -c -DSCPLX $(mySRCdir)/swaptime.c
cswaptest.o : $(mySRCdir)/swaptest.c tstlib.grd
	$(KC) $(KCFLAGS) -o cswaptest.o -c -DSCPLX $(mySRCdir)/swaptest.c
cswaptest : cswaptest.o ATL_cswap_stub.o
	rm -f cswap.c cswap.o xcswaptst
	cp $(mySRCdir)/SWAP/$(urout) cswap.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cswap.c
	$(CLINKER) $(CLINKFLAGS) -o xcswaptst cswaptest.o \
          ATL_cswap_stub.o cswap.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcswaptst -n $(N) $(opt)
cswaptime : cswapcase
cswapcase : cswaptime.o ATL_cswap_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f cswap.c cswap.o xcswap
	cp $(mySRCdir)/SWAP/$(urout) cswap.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c cswap.c
	$(CLINKER) $(CLINKFLAGS) -o xcswap cswaptime.o \
	           ATL_cswap_stub.o cswap.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcswap -n $(N) $(fout) $(opt)
cinstall_copy : xcopysrch
	rm -f GEN/*
	./xcopysrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_ccopy_stub.o : $(mySRCdir)/COPY/copy_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/COPY/copy_stub.c
ccopytime.o : $(mySRCdir)/copytime.c
	$(KC) $(KCFLAGS) -o ccopytime.o -c -DSCPLX $(mySRCdir)/copytime.c
ccopytest.o : $(mySRCdir)/copytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o ccopytest.o -c -DSCPLX $(mySRCdir)/copytest.c
ccopytest : ccopytest.o ATL_ccopy_stub.o
	rm -f ccopy.c ccopy.o xccopytst
	cp $(mySRCdir)/COPY/$(urout) ccopy.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c ccopy.c
	$(CLINKER) $(CLINKFLAGS) -o xccopytst ccopytest.o \
          ATL_ccopy_stub.o ccopy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xccopytst -n $(N) $(opt)
ccopytime : ccopycase
ccopycase : ccopytime.o ATL_ccopy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f ccopy.c ccopy.o xccopy
	cp $(mySRCdir)/COPY/$(urout) ccopy.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c ccopy.c
	$(CLINKER) $(CLINKFLAGS) -o xccopy ccopytime.o \
	           ATL_ccopy_stub.o ccopy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xccopy -n $(N) $(fout) $(opt)
cinstall_axpy : xaxpysrch
	rm -f GEN/*
	./xaxpysrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_caxpy_stub.o : $(mySRCdir)/AXPY/axpy_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/AXPY/axpy_stub.c
caxpytime.o : $(mySRCdir)/axpytime.c
	$(KC) $(KCFLAGS) -o caxpytime.o -c -DSCPLX $(mySRCdir)/axpytime.c
caxpytest.o : $(mySRCdir)/axpytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o caxpytest.o -c -DSCPLX $(mySRCdir)/axpytest.c
caxpytest : caxpytest.o ATL_caxpy_stub.o
	rm -f caxpy.c caxpy.o xcaxpytst
	cp $(mySRCdir)/AXPY/$(urout) caxpy.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c caxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xcaxpytst caxpytest.o \
          ATL_caxpy_stub.o caxpy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcaxpytst -n $(N) $(opt)
caxpytime : caxpycase
caxpycase : caxpytime.o ATL_caxpy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f caxpy.c caxpy.o xcaxpy
	cp $(mySRCdir)/AXPY/$(urout) caxpy.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c caxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xcaxpy caxpytime.o \
	           ATL_caxpy_stub.o caxpy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcaxpy -n $(N) $(fout) $(opt)
cinstall_iamax : xiamaxsrch
	rm -f GEN/*
	./xiamaxsrch -p c
	cp -f GEN/* $(L1Bdir)/.
ATL_ciamax_stub.o : $(mySRCdir)/IAMAX/iamax_stub.c
	$(KC) $(KCFLAGS) -DSCPLX -c -o $@ $(mySRCdir)/IAMAX/iamax_stub.c
ciamaxtime.o : $(mySRCdir)/iamaxtime.c
	$(KC) $(KCFLAGS) -o ciamaxtime.o -c -DSCPLX $(mySRCdir)/iamaxtime.c
ciamaxtest.o : $(mySRCdir)/iamaxtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o ciamaxtest.o -c -DSCPLX $(mySRCdir)/iamaxtest.c
ciamaxtest : ciamaxtest.o ATL_ciamax_stub.o
	rm -f ciamax.c ciamax.o xciamaxtst
	cp $(mySRCdir)/IAMAX/$(urout) ciamax.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c ciamax.c
	$(CLINKER) $(CLINKFLAGS) -o xciamaxtst ciamaxtest.o \
          ATL_ciamax_stub.o ciamax.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xciamaxtst -n $(N) $(opt)
ciamaxtime : ciamaxcase
ciamaxcase : ciamaxtime.o ATL_ciamax_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f ciamax.c ciamax.o xciamax
	cp $(mySRCdir)/IAMAX/$(urout) ciamax.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -c ciamax.c
	$(CLINKER) $(CLINKFLAGS) -o xciamax ciamaxtime.o \
	           ATL_ciamax_stub.o ciamax.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xciamax -n $(N) $(fout) $(opt)

zinstall_rot : xrotsrch
	rm -f GEN/*
	./xrotsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zrot_stub.o : $(mySRCdir)/ROT/rot_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/ROT/rot_stub.c
zrottime.o : $(mySRCdir)/rottime.c
	$(KC) $(KCFLAGS) -o zrottime.o -c -DDCPLX $(mySRCdir)/rottime.c
zrottest.o : $(mySRCdir)/rottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zrottest.o -c -DDCPLX $(mySRCdir)/rottest.c
zrottest : zrottest.o ATL_zrot_stub.o
	rm -f zrot.c zrot.o xzrottst
	cp $(mySRCdir)/ROT/$(urout) zrot.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zrot.c
	$(CLINKER) $(CLINKFLAGS) -o xzrottst zrottest.o \
          ATL_zrot_stub.o zrot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzrottst -n $(N) $(opt)
zrottime : zrotcase
zrotcase : zrottime.o ATL_zrot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zrot.c zrot.o xzrot
	cp $(mySRCdir)/ROT/$(urout) zrot.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zrot.c
	$(CLINKER) $(CLINKFLAGS) -o xzrot zrottime.o \
	           ATL_zrot_stub.o zrot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzrot -n $(N) $(fout) $(opt)
zinstall_axpby : xaxpbysrch
	rm -f GEN/*
	./xaxpbysrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zaxpby_stub.o : $(mySRCdir)/AXPBY/axpby_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/AXPBY/axpby_stub.c
zaxpbytime.o : $(mySRCdir)/axpbytime.c
	$(KC) $(KCFLAGS) -o zaxpbytime.o -c -DDCPLX $(mySRCdir)/axpbytime.c
zaxpbytest.o : $(mySRCdir)/axpbytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zaxpbytest.o -c -DDCPLX $(mySRCdir)/axpbytest.c
zaxpbytest : zaxpbytest.o ATL_zaxpby_stub.o
	rm -f zaxpby.c zaxpby.o xzaxpbytst
	cp $(mySRCdir)/AXPBY/$(urout) zaxpby.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zaxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xzaxpbytst zaxpbytest.o \
          ATL_zaxpby_stub.o zaxpby.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzaxpbytst -n $(N) $(opt)
zaxpbytime : zaxpbycase
zaxpbycase : zaxpbytime.o ATL_zaxpby_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zaxpby.c zaxpby.o xzaxpby
	cp $(mySRCdir)/AXPBY/$(urout) zaxpby.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zaxpby.c
	$(CLINKER) $(CLINKFLAGS) -o xzaxpby zaxpbytime.o \
	           ATL_zaxpby_stub.o zaxpby.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzaxpby -n $(N) $(fout) $(opt)
zinstall_set : xsetsrch
	rm -f GEN/*
	./xsetsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zset_stub.o : $(mySRCdir)/SET/set_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/SET/set_stub.c
zsettime.o : $(mySRCdir)/settime.c
	$(KC) $(KCFLAGS) -o zsettime.o -c -DDCPLX $(mySRCdir)/settime.c
zsettest.o : $(mySRCdir)/settest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zsettest.o -c -DDCPLX $(mySRCdir)/settest.c
zsettest : zsettest.o ATL_zset_stub.o
	rm -f zset.c zset.o xzsettst
	cp $(mySRCdir)/SET/$(urout) zset.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zset.c
	$(CLINKER) $(CLINKFLAGS) -o xzsettst zsettest.o \
          ATL_zset_stub.o zset.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzsettst -n $(N) $(opt)
zsettime : zsetcase
zsetcase : zsettime.o ATL_zset_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zset.c zset.o xzset
	cp $(mySRCdir)/SET/$(urout) zset.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zset.c
	$(CLINKER) $(CLINKFLAGS) -o xzset zsettime.o \
	           ATL_zset_stub.o zset.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzset -n $(N) $(fout) $(opt)
zinstall_cpsc : xcpscsrch
	rm -f GEN/*
	./xcpscsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zcpsc_stub.o : $(mySRCdir)/CPSC/cpsc_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/CPSC/cpsc_stub.c
zcpsctime.o : $(mySRCdir)/cpsctime.c
	$(KC) $(KCFLAGS) -o zcpsctime.o -c -DDCPLX $(mySRCdir)/cpsctime.c
zcpsctest.o : $(mySRCdir)/cpsctest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zcpsctest.o -c -DDCPLX $(mySRCdir)/cpsctest.c
zcpsctest : zcpsctest.o ATL_zcpsc_stub.o
	rm -f zcpsc.c zcpsc.o xzcpsctst
	cp $(mySRCdir)/CPSC/$(urout) zcpsc.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zcpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xzcpsctst zcpsctest.o \
          ATL_zcpsc_stub.o zcpsc.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzcpsctst -n $(N) $(opt)
zcpsctime : zcpsccase
zcpsccase : zcpsctime.o ATL_zcpsc_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zcpsc.c zcpsc.o xzcpsc
	cp $(mySRCdir)/CPSC/$(urout) zcpsc.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zcpsc.c
	$(CLINKER) $(CLINKFLAGS) -o xzcpsc zcpsctime.o \
	           ATL_zcpsc_stub.o zcpsc.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzcpsc -n $(N) $(fout) $(opt)
zinstall_asum : xasumsrch
	rm -f GEN/*
	./xasumsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zasum_stub.o : $(mySRCdir)/ASUM/asum_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/ASUM/asum_stub.c
zasumtime.o : $(mySRCdir)/asumtime.c
	$(KC) $(KCFLAGS) -o zasumtime.o -c -DDCPLX $(mySRCdir)/asumtime.c
zasumtest.o : $(mySRCdir)/asumtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zasumtest.o -c -DDCPLX $(mySRCdir)/asumtest.c
zasumtest : zasumtest.o ATL_zasum_stub.o
	rm -f zasum.c zasum.o xzasumtst
	cp $(mySRCdir)/ASUM/$(urout) zasum.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zasum.c
	$(CLINKER) $(CLINKFLAGS) -o xzasumtst zasumtest.o \
          ATL_zasum_stub.o zasum.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzasumtst -n $(N) $(opt)
zasumtime : zasumcase
zasumcase : zasumtime.o ATL_zasum_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zasum.c zasum.o xzasum
	cp $(mySRCdir)/ASUM/$(urout) zasum.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zasum.c
	$(CLINKER) $(CLINKFLAGS) -o xzasum zasumtime.o \
	           ATL_zasum_stub.o zasum.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzasum -n $(N) $(fout) $(opt)
zinstall_nrm2 : xnrm2srch
	rm -f GEN/*
	./xnrm2srch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_znrm2_stub.o : $(mySRCdir)/NRM2/nrm2_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/NRM2/nrm2_stub.c
znrm2time.o : $(mySRCdir)/nrm2time.c
	$(KC) $(KCFLAGS) -o znrm2time.o -c -DDCPLX $(mySRCdir)/nrm2time.c
znrm2test.o : $(mySRCdir)/nrm2test.c tstlib.grd
	$(KC) $(KCFLAGS) -o znrm2test.o -c -DDCPLX $(mySRCdir)/nrm2test.c
znrm2test : znrm2test.o ATL_znrm2_stub.o
	rm -f znrm2.c znrm2.o xznrm2tst
	cp $(mySRCdir)/NRM2/$(urout) znrm2.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c znrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xznrm2tst znrm2test.o \
          ATL_znrm2_stub.o znrm2.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xznrm2tst -n $(N) $(opt)
znrm2time : znrm2case
znrm2case : znrm2time.o ATL_znrm2_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f znrm2.c znrm2.o xznrm2
	cp $(mySRCdir)/NRM2/$(urout) znrm2.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c znrm2.c
	$(CLINKER) $(CLINKFLAGS) -o xznrm2 znrm2time.o \
	           ATL_znrm2_stub.o znrm2.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xznrm2 -n $(N) $(fout) $(opt)
zinstall_dot : xdotsrch
	rm -f GEN/*
	./xdotsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zdot_stub.o : $(mySRCdir)/DOT/dot_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/DOT/dot_stub.c
zdottime.o : $(mySRCdir)/dottime.c
	$(KC) $(KCFLAGS) -o zdottime.o -c -DDCPLX $(mySRCdir)/dottime.c
zdottest.o : $(mySRCdir)/dottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zdottest.o -c -DDCPLX $(mySRCdir)/dottest.c
zdottest : zdottest.o ATL_zdot_stub.o
	rm -f zdot.c zdot.o xzdottst
	cp $(mySRCdir)/DOT/$(urout) zdot.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zdot.c
	$(CLINKER) $(CLINKFLAGS) -o xzdottst zdottest.o \
          ATL_zdot_stub.o zdot.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzdottst -n $(N) $(opt)
zdottime : zdotcase
zdotcase : zdottime.o ATL_zdot_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zdot.c zdot.o xzdot
	cp $(mySRCdir)/DOT/$(urout) zdot.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zdot.c
	$(CLINKER) $(CLINKFLAGS) -o xzdot zdottime.o \
	           ATL_zdot_stub.o zdot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzdot -n $(N) $(fout) $(opt)
zinstall_scal : xscalsrch
	rm -f GEN/*
	./xscalsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zscal_stub.o : $(mySRCdir)/SCAL/scal_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/SCAL/scal_stub.c
zscaltime.o : $(mySRCdir)/scaltime.c
	$(KC) $(KCFLAGS) -o zscaltime.o -c -DDCPLX $(mySRCdir)/scaltime.c
zscaltest.o : $(mySRCdir)/scaltest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zscaltest.o -c -DDCPLX $(mySRCdir)/scaltest.c
zscaltest : zscaltest.o ATL_zscal_stub.o
	rm -f zscal.c zscal.o xzscaltst
	cp $(mySRCdir)/SCAL/$(urout) zscal.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zscal.c
	$(CLINKER) $(CLINKFLAGS) -o xzscaltst zscaltest.o \
          ATL_zscal_stub.o zscal.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzscaltst -n $(N) $(opt)
zscaltime : zscalcase
zscalcase : zscaltime.o ATL_zscal_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zscal.c zscal.o xzscal
	cp $(mySRCdir)/SCAL/$(urout) zscal.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zscal.c
	$(CLINKER) $(CLINKFLAGS) -o xzscal zscaltime.o \
	           ATL_zscal_stub.o zscal.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzscal -n $(N) $(fout) $(opt)
zinstall_swap : xswapsrch
	rm -f GEN/*
	./xswapsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zswap_stub.o : $(mySRCdir)/SWAP/swap_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/SWAP/swap_stub.c
zswaptime.o : $(mySRCdir)/swaptime.c
	$(KC) $(KCFLAGS) -o zswaptime.o -c -DDCPLX $(mySRCdir)/swaptime.c
zswaptest.o : $(mySRCdir)/swaptest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zswaptest.o -c -DDCPLX $(mySRCdir)/swaptest.c
zswaptest : zswaptest.o ATL_zswap_stub.o
	rm -f zswap.c zswap.o xzswaptst
	cp $(mySRCdir)/SWAP/$(urout) zswap.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zswap.c
	$(CLINKER) $(CLINKFLAGS) -o xzswaptst zswaptest.o \
          ATL_zswap_stub.o zswap.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzswaptst -n $(N) $(opt)
zswaptime : zswapcase
zswapcase : zswaptime.o ATL_zswap_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zswap.c zswap.o xzswap
	cp $(mySRCdir)/SWAP/$(urout) zswap.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zswap.c
	$(CLINKER) $(CLINKFLAGS) -o xzswap zswaptime.o \
	           ATL_zswap_stub.o zswap.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzswap -n $(N) $(fout) $(opt)
zinstall_copy : xcopysrch
	rm -f GEN/*
	./xcopysrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zcopy_stub.o : $(mySRCdir)/COPY/copy_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/COPY/copy_stub.c
zcopytime.o : $(mySRCdir)/copytime.c
	$(KC) $(KCFLAGS) -o zcopytime.o -c -DDCPLX $(mySRCdir)/copytime.c
zcopytest.o : $(mySRCdir)/copytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zcopytest.o -c -DDCPLX $(mySRCdir)/copytest.c
zcopytest : zcopytest.o ATL_zcopy_stub.o
	rm -f zcopy.c zcopy.o xzcopytst
	cp $(mySRCdir)/COPY/$(urout) zcopy.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zcopy.c
	$(CLINKER) $(CLINKFLAGS) -o xzcopytst zcopytest.o \
          ATL_zcopy_stub.o zcopy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzcopytst -n $(N) $(opt)
zcopytime : zcopycase
zcopycase : zcopytime.o ATL_zcopy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zcopy.c zcopy.o xzcopy
	cp $(mySRCdir)/COPY/$(urout) zcopy.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zcopy.c
	$(CLINKER) $(CLINKFLAGS) -o xzcopy zcopytime.o \
	           ATL_zcopy_stub.o zcopy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzcopy -n $(N) $(fout) $(opt)
zinstall_axpy : xaxpysrch
	rm -f GEN/*
	./xaxpysrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_zaxpy_stub.o : $(mySRCdir)/AXPY/axpy_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/AXPY/axpy_stub.c
zaxpytime.o : $(mySRCdir)/axpytime.c
	$(KC) $(KCFLAGS) -o zaxpytime.o -c -DDCPLX $(mySRCdir)/axpytime.c
zaxpytest.o : $(mySRCdir)/axpytest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zaxpytest.o -c -DDCPLX $(mySRCdir)/axpytest.c
zaxpytest : zaxpytest.o ATL_zaxpy_stub.o
	rm -f zaxpy.c zaxpy.o xzaxpytst
	cp $(mySRCdir)/AXPY/$(urout) zaxpy.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zaxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xzaxpytst zaxpytest.o \
          ATL_zaxpy_stub.o zaxpy.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzaxpytst -n $(N) $(opt)
zaxpytime : zaxpycase
zaxpycase : zaxpytime.o ATL_zaxpy_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f zaxpy.c zaxpy.o xzaxpy
	cp $(mySRCdir)/AXPY/$(urout) zaxpy.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c zaxpy.c
	$(CLINKER) $(CLINKFLAGS) -o xzaxpy zaxpytime.o \
	           ATL_zaxpy_stub.o zaxpy.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzaxpy -n $(N) $(fout) $(opt)
zinstall_iamax : xiamaxsrch
	rm -f GEN/*
	./xiamaxsrch -p z
	cp -f GEN/* $(L1Bdir)/.
ATL_ziamax_stub.o : $(mySRCdir)/IAMAX/iamax_stub.c
	$(KC) $(KCFLAGS) -DDCPLX -c -o $@ $(mySRCdir)/IAMAX/iamax_stub.c
ziamaxtime.o : $(mySRCdir)/iamaxtime.c
	$(KC) $(KCFLAGS) -o ziamaxtime.o -c -DDCPLX $(mySRCdir)/iamaxtime.c
ziamaxtest.o : $(mySRCdir)/iamaxtest.c tstlib.grd
	$(KC) $(KCFLAGS) -o ziamaxtest.o -c -DDCPLX $(mySRCdir)/iamaxtest.c
ziamaxtest : ziamaxtest.o ATL_ziamax_stub.o
	rm -f ziamax.c ziamax.o xziamaxtst
	cp $(mySRCdir)/IAMAX/$(urout) ziamax.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c ziamax.c
	$(CLINKER) $(CLINKFLAGS) -o xziamaxtst ziamaxtest.o \
          ATL_ziamax_stub.o ziamax.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xziamaxtst -n $(N) $(opt)
ziamaxtime : ziamaxcase
ziamaxcase : ziamaxtime.o ATL_ziamax_stub.o \
                  $(SYSdir)/time.o auxlib
	rm -f ziamax.c ziamax.o xziamax
	cp $(mySRCdir)/IAMAX/$(urout) ziamax.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -c ziamax.c
	$(CLINKER) $(CLINKFLAGS) -o xziamax ziamaxtime.o \
	           ATL_ziamax_stub.o ziamax.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xziamax -n $(N) $(fout) $(opt)

cinstall_dotc : xdotsrch
	rm -f GEN/*
	./xdotsrch -p C
	cp -f GEN/* $(L1Bdir)/.
cdotctest.o : $(mySRCdir)/dottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o cdotctest.o -c -DSCPLX -DConj_ $(mySRCdir)/dottest.c
cdotctest : cdotctest.o ATL_cdot_stub.o
	rm -f cdotc.c cdotc.o
	cp $(mySRCdir)/DOT/$(urout) cdotc.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -DConj_ -c cdotc.c
	$(CLINKER) $(CLINKFLAGS) -o xcdotctst cdotctest.o \
          ATL_cdot_stub.o cdotc.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcdotctst -n $(N) $(opt)
cdotccase : cdottime.o $(SYSdir)/time.o \
                   ATL_cdot_stub.o auxlib
	rm -f cdot.c cdot.o
	cp $(mySRCdir)/DOT/$(urout) cdot.c
	$(cUCC) $(CDEFS) $(cUCCFLAGS) -DSCPLX -DConj_ -c cdot.c
	$(CLINKER) $(CLINKFLAGS) -o xcdot cdottime.o \
	           ATL_cdot_stub.o cdot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xcdot -n $(N) $(fout) $(opt)
zinstall_dotc : xdotsrch
	rm -f GEN/*
	./xdotsrch -p Z
	cp -f GEN/* $(L1Bdir)/.
zdotctest.o : $(mySRCdir)/dottest.c tstlib.grd
	$(KC) $(KCFLAGS) -o zdotctest.o -c -DDCPLX -DConj_ $(mySRCdir)/dottest.c
zdotctest : zdotctest.o ATL_zdot_stub.o
	rm -f zdotc.c zdotc.o
	cp $(mySRCdir)/DOT/$(urout) zdotc.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -DConj_ -c zdotc.c
	$(CLINKER) $(CLINKFLAGS) -o xzdotctst zdotctest.o \
          ATL_zdot_stub.o zdotc.o $(TESTlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzdotctst -n $(N) $(opt)
zdotccase : zdottime.o $(SYSdir)/time.o \
                   ATL_zdot_stub.o auxlib
	rm -f zdot.c zdot.o
	cp $(mySRCdir)/DOT/$(urout) zdot.c
	$(zUCC) $(CDEFS) $(zUCCFLAGS) -DDCPLX -DConj_ -c zdot.c
	$(CLINKER) $(CLINKFLAGS) -o xzdot zdottime.o \
	           ATL_zdot_stub.o zdot.o \
              $(SYSdir)/time.o $(ATLASlib) $(LIBS)
	$(ATLRUN) $(L1Tdir) xzdot -n $(N) $(fout) $(opt)
