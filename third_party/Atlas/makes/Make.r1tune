include Make.inc
mySRCdir = $(SRCdir)/tune/blas/ger
r1SRCdir = $(SRCdir)/src/blas/ger
R1Kdir = $(mySRCdir)/R1CASES/
R2Kdir = $(mySRCdir)/R2CASES/

sR1CC = $(SKC)
sR1CFLAGS = $(SKCFLAGS)
cR1CC = $(SKC)
cR1CFLAGS = $(SKCFLAGS)
dR1CC = $(DKC)
dR1CFLAGS = $(DKCFLAGS)
zR1CC = $(DKC)
zR1CFLAGS = $(DKCFLAGS)
sR2CC = $(sR1CC)
sR2CFLAGS = $(sR1CFLAGS)
dR2CC = $(dR1CC)
dR2CFLAGS = $(dR1CFLAGS)
cR2CC = $(cR1CC)
cR2CFLAGS = $(cR1CFLAGS)
zR2CC = $(zR1CC)
zR2CFLAGS = $(zR1CFLAGS)
cas=0
mu=16
nu=2
l1mul=75
iflag=0
Mt=297
Nt=177
ldat=$(Mt)
M=1000
N=1000
lda=$(M)
incy=1
flushKB = 4192
align = -Fx 16 -Fy 16 -Fa 16
tflags =
ssegenargs=
#
# Macros for generator
#
mu=1
nu=1
genflags=
order=clmajor

xr1sum2csv : r1sum2csv.o
	$(XCC) $(XCCFLAGS) -o $@ r1sum2csv.o
r1sum2csv.o : $(INCAdir)/atlas_type.h $(mySRCdir)/r1sum2csv.c \
              $(INCSdir)/atlas_r1parse.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/r1sum2csv.c
xr2hgen : r2hgen.o
	$(XCC) $(XCCFLAGS) -o $@ r2hgen.o
r2hgen.o : $(INCAdir)/atlas_type.h $(mySRCdir)/r2hgen.c \
              $(INCSdir)/atlas_r1parse.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/r2hgen.c
xr2ksearch : r2ksearch.o
	$(XCC) $(XCCFLAGS) -o $@ r2ksearch.o
r2ksearch.o : $(INCAdir)/atlas_type.h $(mySRCdir)/r2ksearch.c \
              $(INCSdir)/atlas_r1parse.h $(INCSdir)/atlas_r2testtime.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/r2ksearch.c
xr1hgen : r1hgen.o
	$(XCC) $(XCCFLAGS) -o $@ r1hgen.o
r1hgen.o : $(INCAdir)/atlas_type.h $(mySRCdir)/r1hgen.c \
              $(INCSdir)/atlas_r1parse.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/r1hgen.c
xr1ksearch : r1ksearch.o
	$(XCC) $(XCCFLAGS) -o $@ r1ksearch.o
r1ksearch.o : $(INCAdir)/atlas_type.h $(mySRCdir)/r1ksearch.c \
              $(INCSdir)/atlas_r1parse.h $(INCSdir)/atlas_r1testtime.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/r1ksearch.c
$(INCAdir)/atlas_type.h :
	cd $(SYSdir) ; $(MAKE) RunTyp
l1lib : sl1lib dl1lib cl1lib zl1lib 
tstlib : ststlib dtstlib ctstlib ztstlib

force_build :

res/L1CacheSize : $(SYSdir)/res/L1CacheSize
	cd $(SYSdir) ; $(MAKE) res/L1CacheSize
	ln -s $(SYSdir)/res/L1CacheSize res/L1CacheSize
res/goodgcc.txt :
	echo "'$(GOODGCC)'" > res/goodgcc.txt

ATL_ger.c : $(r1SRCdir)/ATL_ger.c
	rm -f ATL_ger.c
	echo "#define ATL_TUNING" > ATL_ger.c
	cat $(r1SRCdir)/ATL_ger.c >> ATL_ger.c
ATL_ger2.c : $(r1SRCdir)/ATL_ger2.c
	rm -f ATL_ger2.c
	echo "#define ATL_TUNING" > ATL_ger2.c
	cat $(r1SRCdir)/ATL_ger2.c >> ATL_ger2.c

sr1tstobj = ATL_sger1k.o ATL_sgerk_Mlt16.o ATL_sgerk_axpy.o
sr2tstobj = ATL_sger2k.o ATL_sger2k_Mlt16.o ATL_sger2k_Nlt8.o
sr1obj = ATL_sger.o ATL_sger1k.o ATL_sgerk_Mlt16.o ATL_sgerk_axpy.o
sr2obj = ATL_sger2.o ATL_sger2k.o ATL_sger2k_Mlt16.o ATL_sger2k_Nlt8.o

snxtune :
	cd $(BLDdir)/src/blas/reference/level2 ; make slib
	cd $(GR1dir) ; make slib
	$(MAKE) xss1nxtune
	$(ATLRUN) $(R1Tdir) xss1nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_ssyrNX.h
	$(MAKE) xss2nxtune
	$(ATLRUN) $(R1Tdir) xss2nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_ssyr2NX.h
	cd $(L2Bdir) ; make slib
#
# This target is for directly timing a call to the kernel, w/o cache flushing
#
sr2ktime : sr2ktime.o ATL_sger2k_Mlt16.o ATL_sger2k_Nlt8.o
	rm -f ATL_sger2.c
	cp -f $(R2Kdir)/$(r2rout) ATL_sger2.c
	$(sR2CC) -c $(CDEFS) $(sR2CFLAGS) -I$(R1Tdir) -o ATL_sger2.o \
           -DSREAL -DATL_sger2_a1_x1_yX=ATL_UGERK ATL_sger2.c
	$(CLINKER) $(CLINKFLAGS) -o xsr2ktime sr2ktime.o \
                   ATL_sger2.o \
                   ATL_sger2k_Mlt16.o ATL_sger2k_Nlt8.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xsr2ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r2 tuning of ref/r2 crossover point (NX)
#
xss2nxtune : ss2nxtune.o ATL_ssyr2_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xss2nxtune ss2nxtune.o \
                   ATL_ssyr2_NX.o $(TESTlib) $(ATLASlib)
ss2nxtune.o : $(mySRCdir)/s2nxtune.c $(INCAdir)/atlas_sr2.h \
        $(INCAdir)/atlas_sr2_L1.h $(INCAdir)/atlas_sr2_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/s2nxtune.c
#
# These targets use extract to generate a GER2 kernel
#
sr2ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r2_C nu=$(nu) type=SREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/sr2_C.c
sr2ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r2_sse nu=$(nu) type=SREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/sr2_sse.c
#
#
# This target calls the GER2 driver (which blocks), with cache flushing
#
sr2time : sr2time.o xr2hgen
	rm -f $(sr2obj) ATL_sger2.c atlas_sr2.h \
              r2h.bak xsr2time
	- mv $(INCAdir)/atlas_sr2.h rr2.bak
	./xr2hgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_sger2k.c
	$(MAKE) $(sr2obj)
	$(CLINKER) $(CLINKFLAGS) -o xsr2time sr2time.o \
                   $(sr2obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xsr2time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r2h.bak $(INCAdir)/atlas_sr2.h

sr2ktest : sr2ktest.o xr2hgen
	rm -f $(sr2tstobj) ATL_sger2k.c xsr2ktest
	- mv $(INCAdir)/atlas_sr2.h r2h.bak
	./xr2hgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_sger2k.c
	$(MAKE) $(sr2tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xsr2ktest sr2ktest.o \
                   $(sr2tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xsr2ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r2h.bak $(INCAdir)/atlas_sr2.h

sr2time.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/r2ktime.c
sr2ktime.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DTIME_KERNEL $(mySRCdir)/r2ktime.c
sr2ktest.o : $(mySRCdir)/r2ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/r2ktest.c
sr2test.o : $(mySRCdir)/r2test.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/r2test.c
sger2tune.o : $(mySRCdir)/ger2tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/ger2tune.c
sr1ktime : sr1ktime.o ATL_sgerk_Mlt16.o ATL_sgerk_axpy.o
	rm -f ATL_sger1.c
	cp -f $(R1Kdir)/$(r1rout) ATL_sger1.c
	$(sR1CC) -c $(CDEFS) $(sR1CFLAGS) -I$(R1Tdir) -o ATL_sger1.o \
           -DSREAL -DATL_sger1_a1_x1_yX=ATL_UGERK ATL_sger1.c
	$(CLINKER) $(CLINKFLAGS) -o xsr1ktime sr1ktime.o \
                   ATL_sger1.o \
                   ATL_sgerk_Mlt16.o ATL_sgerk_axpy.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xsr1ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r1 tuning of ref/r1 crossover point (NX)
#
xss1nxtune : ss1nxtune.o ATL_ssyr_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xss1nxtune ss1nxtune.o \
                   ATL_ssyr_NX.o $(TESTlib) $(ATLASlib)
ss1nxtune.o : $(mySRCdir)/s1nxtune.c $(INCAdir)/atlas_sr1.h \
        $(INCAdir)/atlas_sr1_L1.h $(INCAdir)/atlas_sr1_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/s1nxtune.c
#
# These targets use extract to generate a GER1 kernel
#
sr1ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r1_C nu=$(nu) type=SREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/sr1_C.c
sr1ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r1_sse nu=$(nu) type=SREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/sr1_sse.c
#
#
# This target calls the GER1 driver (which blocks), with cache flushing
#
sr1time : sr1time.o xr1hgen
	rm -f $(sr1obj) ATL_sger1.c atlas_sr1.h \
              r1h.bak xsr1time
	- mv $(INCAdir)/atlas_sr1.h rr1.bak
	./xr1hgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_sger1k.c
	$(MAKE) $(sr1obj)
	$(CLINKER) $(CLINKFLAGS) -o xsr1time sr1time.o \
                   $(sr1obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xsr1time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r1h.bak $(INCAdir)/atlas_sr1.h

sr1ktest : sr1ktest.o xr1hgen
	rm -f $(sr1tstobj) ATL_sger1k.c xsr1ktest
	- mv $(INCAdir)/atlas_sr1.h r1h.bak
	./xr1hgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_sger1k.c
	$(MAKE) $(sr1tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xsr1ktest sr1ktest.o \
                   $(sr1tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xsr1ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r1h.bak $(INCAdir)/atlas_sr1.h

sr1time.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/r1ktime.c
sr1ktime.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DTIME_KERNEL $(mySRCdir)/r1ktime.c
sr1ktest.o : $(mySRCdir)/r1ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/r1ktest.c
sr1test.o : $(mySRCdir)/r1test.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/r1test.c
sger1tune.o : $(mySRCdir)/ger1tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/ger1tune.c
ATL_sger.o : ATL_ger.c atlas_sr1.h
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DTUNING -I$(R1Tdir) ATL_ger.c
ATL_sger2.o : ATL_ger2.c atlas_sr2.h
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DTUNING -I$(R1Tdir) ATL_ger2.c
ATL_sger2k_Nlt8.o : $(r1SRCdir)/ATL_ger2k_Nlt8.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSREAL $(r1SRCdir)/ATL_ger2k_Nlt8.c
ATL_sgerk_Nlt8.o : $(r1SRCdir)/ATL_gerk_Nlt8.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSREAL $(r1SRCdir)/ATL_gerk_Nlt8.c
ATL_sger2k_axpy.o : $(r1SRCdir)/ATL_ger2k_axpy.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSREAL $(r1SRCdir)/ATL_ger2k_axpy.c
ATL_sgerk_axpy.o : $(r1SRCdir)/ATL_gerk_axpy.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSREAL $(r1SRCdir)/ATL_gerk_axpy.c
ATL_sger2k_Mlt16.o : $(r1SRCdir)/ATL_ger2k_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSREAL $(r1SRCdir)/ATL_ger2k_Mlt16.c
ATL_sgerk_Mlt16.o : $(r1SRCdir)/ATL_gerk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSREAL $(r1SRCdir)/ATL_gerk_Mlt16.c
ATL_sger2k.o : ATL_sger2k.c
	$(sR2CC) -c $(CDEFS) $(sR2CFLAGS) -I$(R1Tdir) -o $@ -DSREAL ATL_sger2k.c
ATL_sger2c.o : ATL_sger2.c
	$(sR2CC) -c $(CDEFS) $(sR2CFLAGS) -I$(R1Tdir) -o $@ -DSREAL -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_sger2.c
res/sR2K.sum : xr2ksearch force_build res/sR1K.sum sr1install
	./xr2ksearch -p s
	$(MAKE) sr2install
sr2outd:
	mkdir sr2outd
sr2install : xccobj sr2outd xr2hgen force_build
	rm -f sr2outd/*
	if [ -s "$(GR1dir)/Make_sr2" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_sr2 killall ; \
           rm -f $(GR1dir)/Make_sr2 ; \
        fi
	./xr2hgen -p s -F res/sR2K.sum -d sr2outd
	cp -f sr2outd/Make_sr2 $(GR1dir)/.
	cp sr2outd/*.c $(GR1dir)/.
	cp -f sr2outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r2.h
	rm -f sr2outd/* ; rmdir sr2outd
ATL_sger1k.o : ATL_sger1k.c
	$(sR1CC) -c $(CDEFS) $(sR1CFLAGS) -I$(R1Tdir) -o $@ -DSREAL ATL_sger1k.c
ATL_sger1c.o : ATL_sger1.c
	$(sR1CC) -c $(CDEFS) $(sR1CFLAGS) -I$(R1Tdir) -o $@ -DSREAL -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_sger1.c
res/sR1K.sum : xr1ksearch force_build  
	./xr1ksearch -p s
	$(MAKE) sr1install
sr1outd:
	mkdir sr1outd
sr1install : xccobj sr1outd xr1hgen force_build
	rm -f sr1outd/*
	if [ -s "$(GR1dir)/Make_sr1" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_sr1 killall ; \
           rm -f $(GR1dir)/Make_sr1 ; \
        fi
	./xr1hgen -p s -F res/sR1K.sum -d sr1outd
	cp -f sr1outd/Make_sr1 $(GR1dir)/.
	cp sr1outd/*.c $(GR1dir)/.
	cp -f sr1outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r1.h
	rm -f sr1outd/* ; rmdir sr1outd

ATL_sher2_NX.o : $(SRCdir)/src/blas/level2/ATL_her2.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her2.c
ATL_sher_NX.o : $(SRCdir)/src/blas/level2/ATL_her.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her.c
ATL_ssyr2_NX.o : $(SRCdir)/src/blas/level2/ATL_syr2.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr2.c
ATL_ssyr_NX.o : $(SRCdir)/src/blas/level2/ATL_syr.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr.c
sl1lib :
	cd $(BINdir) ; $(MAKE) sl1lib
ststlib :
	cd $(BINdir) ; $(MAKE) ststlib
dr1tstobj = ATL_dger1k.o ATL_dgerk_Mlt16.o ATL_dgerk_axpy.o
dr2tstobj = ATL_dger2k.o ATL_dger2k_Mlt16.o ATL_dger2k_Nlt8.o
dr1obj = ATL_dger.o ATL_dger1k.o ATL_dgerk_Mlt16.o ATL_dgerk_axpy.o
dr2obj = ATL_dger2.o ATL_dger2k.o ATL_dger2k_Mlt16.o ATL_dger2k_Nlt8.o

dnxtune :
	cd $(BLDdir)/src/blas/reference/level2 ; make dlib
	cd $(GR1dir) ; make dlib
	$(MAKE) xds1nxtune
	$(ATLRUN) $(R1Tdir) xds1nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_dsyrNX.h
	$(MAKE) xds2nxtune
	$(ATLRUN) $(R1Tdir) xds2nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_dsyr2NX.h
	cd $(L2Bdir) ; make dlib
#
# This target is for directly timing a call to the kernel, w/o cache flushing
#
dr2ktime : dr2ktime.o ATL_dger2k_Mlt16.o ATL_dger2k_Nlt8.o
	rm -f ATL_dger2.c
	cp -f $(R2Kdir)/$(r2rout) ATL_dger2.c
	$(dR2CC) -c $(CDEFS) $(dR2CFLAGS) -I$(R1Tdir) -o ATL_dger2.o \
           -DDREAL -DATL_dger2_a1_x1_yX=ATL_UGERK ATL_dger2.c
	$(CLINKER) $(CLINKFLAGS) -o xdr2ktime dr2ktime.o \
                   ATL_dger2.o \
                   ATL_dger2k_Mlt16.o ATL_dger2k_Nlt8.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xdr2ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r2 tuning of ref/r2 crossover point (NX)
#
xds2nxtune : ds2nxtune.o ATL_dsyr2_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xds2nxtune ds2nxtune.o \
                   ATL_dsyr2_NX.o $(TESTlib) $(ATLASlib)
ds2nxtune.o : $(mySRCdir)/s2nxtune.c $(INCAdir)/atlas_dr2.h \
        $(INCAdir)/atlas_dr2_L1.h $(INCAdir)/atlas_dr2_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/s2nxtune.c
#
# These targets use extract to generate a GER2 kernel
#
dr2ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r2_C nu=$(nu) type=DREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/dr2_C.c
dr2ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r2_sse nu=$(nu) type=DREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/dr2_sse.c
#
#
# This target calls the GER2 driver (which blocks), with cache flushing
#
dr2time : dr2time.o xr2hgen
	rm -f $(dr2obj) ATL_dger2.c atlas_dr2.h \
              r2h.bak xdr2time
	- mv $(INCAdir)/atlas_dr2.h rr2.bak
	./xr2hgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_dger2k.c
	$(MAKE) $(dr2obj)
	$(CLINKER) $(CLINKFLAGS) -o xdr2time dr2time.o \
                   $(dr2obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xdr2time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r2h.bak $(INCAdir)/atlas_dr2.h

dr2ktest : dr2ktest.o xr2hgen
	rm -f $(dr2tstobj) ATL_dger2k.c xdr2ktest
	- mv $(INCAdir)/atlas_dr2.h r2h.bak
	./xr2hgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_dger2k.c
	$(MAKE) $(dr2tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xdr2ktest dr2ktest.o \
                   $(dr2tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xdr2ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r2h.bak $(INCAdir)/atlas_dr2.h

dr2time.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/r2ktime.c
dr2ktime.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DTIME_KERNEL $(mySRCdir)/r2ktime.c
dr2ktest.o : $(mySRCdir)/r2ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/r2ktest.c
dr2test.o : $(mySRCdir)/r2test.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/r2test.c
dger2tune.o : $(mySRCdir)/ger2tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/ger2tune.c
dr1ktime : dr1ktime.o ATL_dgerk_Mlt16.o ATL_dgerk_axpy.o
	rm -f ATL_dger1.c
	cp -f $(R1Kdir)/$(r1rout) ATL_dger1.c
	$(dR1CC) -c $(CDEFS) $(dR1CFLAGS) -I$(R1Tdir) -o ATL_dger1.o \
           -DDREAL -DATL_dger1_a1_x1_yX=ATL_UGERK ATL_dger1.c
	$(CLINKER) $(CLINKFLAGS) -o xdr1ktime dr1ktime.o \
                   ATL_dger1.o \
                   ATL_dgerk_Mlt16.o ATL_dgerk_axpy.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xdr1ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r1 tuning of ref/r1 crossover point (NX)
#
xds1nxtune : ds1nxtune.o ATL_dsyr_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xds1nxtune ds1nxtune.o \
                   ATL_dsyr_NX.o $(TESTlib) $(ATLASlib)
ds1nxtune.o : $(mySRCdir)/s1nxtune.c $(INCAdir)/atlas_dr1.h \
        $(INCAdir)/atlas_dr1_L1.h $(INCAdir)/atlas_dr1_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/s1nxtune.c
#
# These targets use extract to generate a GER1 kernel
#
dr1ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r1_C nu=$(nu) type=DREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/dr1_C.c
dr1ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r1_sse nu=$(nu) type=DREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/dr1_sse.c
#
#
# This target calls the GER1 driver (which blocks), with cache flushing
#
dr1time : dr1time.o xr1hgen
	rm -f $(dr1obj) ATL_dger1.c atlas_dr1.h \
              r1h.bak xdr1time
	- mv $(INCAdir)/atlas_dr1.h rr1.bak
	./xr1hgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_dger1k.c
	$(MAKE) $(dr1obj)
	$(CLINKER) $(CLINKFLAGS) -o xdr1time dr1time.o \
                   $(dr1obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xdr1time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r1h.bak $(INCAdir)/atlas_dr1.h

dr1ktest : dr1ktest.o xr1hgen
	rm -f $(dr1tstobj) ATL_dger1k.c xdr1ktest
	- mv $(INCAdir)/atlas_dr1.h r1h.bak
	./xr1hgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_dger1k.c
	$(MAKE) $(dr1tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xdr1ktest dr1ktest.o \
                   $(dr1tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xdr1ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r1h.bak $(INCAdir)/atlas_dr1.h

dr1time.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/r1ktime.c
dr1ktime.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DTIME_KERNEL $(mySRCdir)/r1ktime.c
dr1ktest.o : $(mySRCdir)/r1ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/r1ktest.c
dr1test.o : $(mySRCdir)/r1test.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/r1test.c
dger1tune.o : $(mySRCdir)/ger1tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/ger1tune.c
ATL_dger.o : ATL_ger.c atlas_dr1.h
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DTUNING -I$(R1Tdir) ATL_ger.c
ATL_dger2.o : ATL_ger2.c atlas_dr2.h
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DTUNING -I$(R1Tdir) ATL_ger2.c
ATL_dger2k_Nlt8.o : $(r1SRCdir)/ATL_ger2k_Nlt8.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDREAL $(r1SRCdir)/ATL_ger2k_Nlt8.c
ATL_dgerk_Nlt8.o : $(r1SRCdir)/ATL_gerk_Nlt8.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDREAL $(r1SRCdir)/ATL_gerk_Nlt8.c
ATL_dger2k_axpy.o : $(r1SRCdir)/ATL_ger2k_axpy.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDREAL $(r1SRCdir)/ATL_ger2k_axpy.c
ATL_dgerk_axpy.o : $(r1SRCdir)/ATL_gerk_axpy.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDREAL $(r1SRCdir)/ATL_gerk_axpy.c
ATL_dger2k_Mlt16.o : $(r1SRCdir)/ATL_ger2k_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDREAL $(r1SRCdir)/ATL_ger2k_Mlt16.c
ATL_dgerk_Mlt16.o : $(r1SRCdir)/ATL_gerk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDREAL $(r1SRCdir)/ATL_gerk_Mlt16.c
ATL_dger2k.o : ATL_dger2k.c
	$(dR2CC) -c $(CDEFS) $(dR2CFLAGS) -I$(R1Tdir) -o $@ -DDREAL ATL_dger2k.c
ATL_dger2c.o : ATL_dger2.c
	$(dR2CC) -c $(CDEFS) $(dR2CFLAGS) -I$(R1Tdir) -o $@ -DDREAL -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_dger2.c
res/dR2K.sum : xr2ksearch force_build res/dR1K.sum dr1install
	./xr2ksearch -p d
	$(MAKE) dr2install
dr2outd:
	mkdir dr2outd
dr2install : xccobj dr2outd xr2hgen force_build
	rm -f dr2outd/*
	if [ -s "$(GR1dir)/Make_dr2" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_dr2 killall ; \
           rm -f $(GR1dir)/Make_dr2 ; \
        fi
	./xr2hgen -p d -F res/dR2K.sum -d dr2outd
	cp -f dr2outd/Make_dr2 $(GR1dir)/.
	cp dr2outd/*.c $(GR1dir)/.
	cp -f dr2outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r2.h
	rm -f dr2outd/* ; rmdir dr2outd
ATL_dger1k.o : ATL_dger1k.c
	$(dR1CC) -c $(CDEFS) $(dR1CFLAGS) -I$(R1Tdir) -o $@ -DDREAL ATL_dger1k.c
ATL_dger1c.o : ATL_dger1.c
	$(dR1CC) -c $(CDEFS) $(dR1CFLAGS) -I$(R1Tdir) -o $@ -DDREAL -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_dger1.c
res/dR1K.sum : xr1ksearch force_build  
	./xr1ksearch -p d
	$(MAKE) dr1install
dr1outd:
	mkdir dr1outd
dr1install : xccobj dr1outd xr1hgen force_build
	rm -f dr1outd/*
	if [ -s "$(GR1dir)/Make_dr1" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_dr1 killall ; \
           rm -f $(GR1dir)/Make_dr1 ; \
        fi
	./xr1hgen -p d -F res/dR1K.sum -d dr1outd
	cp -f dr1outd/Make_dr1 $(GR1dir)/.
	cp dr1outd/*.c $(GR1dir)/.
	cp -f dr1outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r1.h
	rm -f dr1outd/* ; rmdir dr1outd

ATL_dher2_NX.o : $(SRCdir)/src/blas/level2/ATL_her2.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her2.c
ATL_dher_NX.o : $(SRCdir)/src/blas/level2/ATL_her.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her.c
ATL_dsyr2_NX.o : $(SRCdir)/src/blas/level2/ATL_syr2.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr2.c
ATL_dsyr_NX.o : $(SRCdir)/src/blas/level2/ATL_syr.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr.c
dl1lib :
	cd $(BINdir) ; $(MAKE) dl1lib
dtstlib :
	cd $(BINdir) ; $(MAKE) dtstlib
cr1tstobj = ATL_cger1k.o  \
            ATL_cgerk_Mlt16.o ATL_cgerk_axpy.o \
            ATL_cgerck_Mlt16.o ATL_cgerck_axpy.o 
cr2tstobj = ATL_cger2k.o  \
            ATL_cger2k_Mlt16.o ATL_cger2k_Nlt8.o \
            ATL_cger2ck_Mlt16.o ATL_cger2ck_Nlt8.o 
cr1obj = ATL_cger.o ATL_cger1k.o \
         ATL_cgerk_Mlt16.o ATL_cgerk_axpy.o \
         ATL_cgerck_Mlt16.o ATL_cgerck_axpy.o 
cr2obj = ATL_cger2.o ATL_cger2k.o \
         ATL_cger2k_Mlt16.o ATL_cger2k_Nlt8.o \
         ATL_cger2ck_Mlt16.o ATL_cger2ck_Nlt8.o 

cnxtune :
	cd $(BLDdir)/src/blas/reference/level2 ; make clib
	cd $(GR1dir) ; make clib
	$(MAKE) xcs1nxtune
	$(ATLRUN) $(R1Tdir) xcs1nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_csyrNX.h
	$(MAKE) xcs2nxtune
	$(ATLRUN) $(R1Tdir) xcs2nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_csyr2NX.h
	cd $(L2Bdir) ; make clib
#
# This target is for directly timing a call to the kernel, w/o cache flushing
#
cr2ktime : cr2ktime.o ATL_cger2k_Mlt16.o ATL_cger2k_Nlt8.o
	rm -f ATL_cger2.c
	cp -f $(R2Kdir)/$(r2rout) ATL_cger2.c
	$(cR2CC) -c $(CDEFS) $(cR2CFLAGS) -I$(R1Tdir) -o ATL_cger2.o \
           -DSCPLX -DATL_cger2_a1_x1_yX=ATL_UGERK ATL_cger2.c
	$(CLINKER) $(CLINKFLAGS) -o xcr2ktime cr2ktime.o \
                   ATL_cger2.o \
                   ATL_cger2k_Mlt16.o ATL_cger2k_Nlt8.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xcr2ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r2 tuning of ref/r2 crossover point (NX)
#
xcs2nxtune : cs2nxtune.o ATL_cher2_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xcs2nxtune cs2nxtune.o \
                   ATL_cher2_NX.o $(TESTlib) $(ATLASlib)
cs2nxtune.o : $(mySRCdir)/s2nxtune.c $(INCAdir)/atlas_cr2.h \
        $(INCAdir)/atlas_cr2_L1.h $(INCAdir)/atlas_cr2_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/s2nxtune.c
#
# These targets use extract to generate a GER2 kernel
#
cr2ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r2_C nu=$(nu) type=SCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/cr2_C.c
cr2ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r2_sse nu=$(nu) type=SCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/cr2_sse.c
#
#
# This target calls the GER2 driver (which blocks), with cache flushing
#
cr2time : cr2time.o xr2hgen
	rm -f $(cr2obj) ATL_cger2.c atlas_cr2.h \
              r2h.bak xcr2time
	- mv $(INCAdir)/atlas_cr2.h rr2.bak
	./xr2hgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_cger2k.c
	$(MAKE) $(cr2obj)
	$(CLINKER) $(CLINKFLAGS) -o xcr2time cr2time.o \
                   $(cr2obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xcr2time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r2h.bak $(INCAdir)/atlas_cr2.h

cr2ktest : cr2ktest.o xr2hgen
	rm -f $(cr2tstobj) ATL_cger2k.c xcr2ktest
	- mv $(INCAdir)/atlas_cr2.h r2h.bak
	./xr2hgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_cger2k.c
	$(MAKE) $(cr2tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xcr2ktest cr2ktest.o \
                   $(cr2tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xcr2ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r2h.bak $(INCAdir)/atlas_cr2.h

cr2time.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/r2ktime.c
cr2ktime.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DTIME_KERNEL $(mySRCdir)/r2ktime.c
cr2ktest.o : $(mySRCdir)/r2ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/r2ktest.c
cr2test.o : $(mySRCdir)/r2test.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/r2test.c
cger2tune.o : $(mySRCdir)/ger2tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/ger2tune.c
cr1ktime : cr1ktime.o ATL_cgerk_Mlt16.o ATL_cgerk_axpy.o
	rm -f ATL_cger1.c
	cp -f $(R1Kdir)/$(r1rout) ATL_cger1.c
	$(cR1CC) -c $(CDEFS) $(cR1CFLAGS) -I$(R1Tdir) -o ATL_cger1.o \
           -DSCPLX -DATL_cger1_a1_x1_yX=ATL_UGERK ATL_cger1.c
	$(CLINKER) $(CLINKFLAGS) -o xcr1ktime cr1ktime.o \
                   ATL_cger1.o \
                   ATL_cgerk_Mlt16.o ATL_cgerk_axpy.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xcr1ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r1 tuning of ref/r1 crossover point (NX)
#
xcs1nxtune : cs1nxtune.o ATL_cher_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xcs1nxtune cs1nxtune.o \
                   ATL_cher_NX.o $(TESTlib) $(ATLASlib)
cs1nxtune.o : $(mySRCdir)/s1nxtune.c $(INCAdir)/atlas_cr1.h \
        $(INCAdir)/atlas_cr1_L1.h $(INCAdir)/atlas_cr1_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/s1nxtune.c
#
# These targets use extract to generate a GER1 kernel
#
cr1ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r1_C nu=$(nu) type=SCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/cr1_C.c
cr1ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r1_sse nu=$(nu) type=SCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/cr1_sse.c
#
#
# This target calls the GER1 driver (which blocks), with cache flushing
#
cr1time : cr1time.o xr1hgen
	rm -f $(cr1obj) ATL_cger1.c atlas_cr1.h \
              r1h.bak xcr1time
	- mv $(INCAdir)/atlas_cr1.h rr1.bak
	./xr1hgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_cger1k.c
	$(MAKE) $(cr1obj)
	$(CLINKER) $(CLINKFLAGS) -o xcr1time cr1time.o \
                   $(cr1obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xcr1time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r1h.bak $(INCAdir)/atlas_cr1.h

cr1ktest : cr1ktest.o xr1hgen
	rm -f $(cr1tstobj) ATL_cger1k.c xcr1ktest
	- mv $(INCAdir)/atlas_cr1.h r1h.bak
	./xr1hgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_cger1k.c
	$(MAKE) $(cr1tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xcr1ktest cr1ktest.o \
                   $(cr1tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xcr1ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r1h.bak $(INCAdir)/atlas_cr1.h

cr1time.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/r1ktime.c
cr1ktime.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DTIME_KERNEL $(mySRCdir)/r1ktime.c
cr1ktest.o : $(mySRCdir)/r1ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/r1ktest.c
cr1test.o : $(mySRCdir)/r1test.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/r1test.c
cger1tune.o : $(mySRCdir)/ger1tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/ger1tune.c
ATL_cger.o : ATL_ger.c atlas_cr1.h
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DTUNING -I$(R1Tdir) ATL_ger.c
ATL_cger2.o : ATL_ger2.c atlas_cr2.h
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DTUNING -I$(R1Tdir) ATL_ger2.c
ATL_cger2k_Nlt8.o : $(r1SRCdir)/ATL_ger2k_Nlt8.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(r1SRCdir)/ATL_ger2k_Nlt8.c
ATL_cger2ck_Nlt8.o : $(r1SRCdir)/ATL_ger2k_Nlt8.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DSCPLX $(r1SRCdir)/ATL_ger2k_Nlt8.c
ATL_cgerk_Nlt8.o : $(r1SRCdir)/ATL_gerk_Nlt8.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(r1SRCdir)/ATL_gerk_Nlt8.c
ATL_cgerck_Nlt8.o : $(r1SRCdir)/ATL_gerk_Nlt8.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DSCPLX $(r1SRCdir)/ATL_gerk_Nlt8.c
ATL_cger2k_axpy.o : $(r1SRCdir)/ATL_ger2k_axpy.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(r1SRCdir)/ATL_ger2k_axpy.c
ATL_cger2ck_axpy.o : $(r1SRCdir)/ATL_ger2k_axpy.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DSCPLX $(r1SRCdir)/ATL_ger2k_axpy.c
ATL_cgerk_axpy.o : $(r1SRCdir)/ATL_gerk_axpy.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(r1SRCdir)/ATL_gerk_axpy.c
ATL_cgerck_axpy.o : $(r1SRCdir)/ATL_gerk_axpy.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DSCPLX $(r1SRCdir)/ATL_gerk_axpy.c
ATL_cger2k_Mlt16.o : $(r1SRCdir)/ATL_ger2k_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(r1SRCdir)/ATL_ger2k_Mlt16.c
ATL_cger2ck_Mlt16.o : $(r1SRCdir)/ATL_ger2k_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DSCPLX $(r1SRCdir)/ATL_ger2k_Mlt16.c
ATL_cgerk_Mlt16.o : $(r1SRCdir)/ATL_gerk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(r1SRCdir)/ATL_gerk_Mlt16.c
ATL_cgerck_Mlt16.o : $(r1SRCdir)/ATL_gerk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DSCPLX $(r1SRCdir)/ATL_gerk_Mlt16.c
ATL_cger2k.o : ATL_cger2k.c
	$(cR2CC) -c $(CDEFS) $(cR2CFLAGS) -I$(R1Tdir) -o $@ -DSCPLX ATL_cger2k.c
ATL_cger2c.o : ATL_cger2.c
	$(cR2CC) -c $(CDEFS) $(cR2CFLAGS) -I$(R1Tdir) -o $@ -DSCPLX -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_cger2.c
res/cR2K.sum : xr2ksearch force_build res/cR1K.sum cr1install
	./xr2ksearch -p c
	$(MAKE) cr2install
cr2outd:
	mkdir cr2outd
cr2install : xccobj cr2outd xr2hgen force_build
	rm -f cr2outd/*
	if [ -s "$(GR1dir)/Make_cr2" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_cr2 killall ; \
           rm -f $(GR1dir)/Make_cr2 ; \
        fi
	./xr2hgen -p c -F res/cR2K.sum -d cr2outd
	cp -f cr2outd/Make_cr2 $(GR1dir)/.
	cp cr2outd/*.c $(GR1dir)/.
	cp -f cr2outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r2.h
	rm -f cr2outd/* ; rmdir cr2outd
ATL_cger1k.o : ATL_cger1k.c
	$(cR1CC) -c $(CDEFS) $(cR1CFLAGS) -I$(R1Tdir) -o $@ -DSCPLX ATL_cger1k.c
ATL_cger1c.o : ATL_cger1.c
	$(cR1CC) -c $(CDEFS) $(cR1CFLAGS) -I$(R1Tdir) -o $@ -DSCPLX -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_cger1.c
res/cR1K.sum : xr1ksearch force_build  
	./xr1ksearch -p c
	$(MAKE) cr1install
cr1outd:
	mkdir cr1outd
cr1install : xccobj cr1outd xr1hgen force_build
	rm -f cr1outd/*
	if [ -s "$(GR1dir)/Make_cr1" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_cr1 killall ; \
           rm -f $(GR1dir)/Make_cr1 ; \
        fi
	./xr1hgen -p c -F res/cR1K.sum -d cr1outd
	cp -f cr1outd/Make_cr1 $(GR1dir)/.
	cp cr1outd/*.c $(GR1dir)/.
	cp -f cr1outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r1.h
	rm -f cr1outd/* ; rmdir cr1outd

ATL_cher2_NX.o : $(SRCdir)/src/blas/level2/ATL_her2.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her2.c
ATL_cher_NX.o : $(SRCdir)/src/blas/level2/ATL_her.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her.c
ATL_csyr2_NX.o : $(SRCdir)/src/blas/level2/ATL_syr2.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr2.c
ATL_csyr_NX.o : $(SRCdir)/src/blas/level2/ATL_syr.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr.c
cl1lib :
	cd $(BINdir) ; $(MAKE) cl1lib
ctstlib :
	cd $(BINdir) ; $(MAKE) ctstlib
zr1tstobj = ATL_zger1k.o  \
            ATL_zgerk_Mlt16.o ATL_zgerk_axpy.o \
            ATL_zgerck_Mlt16.o ATL_zgerck_axpy.o 
zr2tstobj = ATL_zger2k.o  \
            ATL_zger2k_Mlt16.o ATL_zger2k_Nlt8.o \
            ATL_zger2ck_Mlt16.o ATL_zger2ck_Nlt8.o 
zr1obj = ATL_zger.o ATL_zger1k.o \
         ATL_zgerk_Mlt16.o ATL_zgerk_axpy.o \
         ATL_zgerck_Mlt16.o ATL_zgerck_axpy.o 
zr2obj = ATL_zger2.o ATL_zger2k.o \
         ATL_zger2k_Mlt16.o ATL_zger2k_Nlt8.o \
         ATL_zger2ck_Mlt16.o ATL_zger2ck_Nlt8.o 

znxtune :
	cd $(BLDdir)/src/blas/reference/level2 ; make zlib
	cd $(GR1dir) ; make zlib
	$(MAKE) xzs1nxtune
	$(ATLRUN) $(R1Tdir) xzs1nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_zsyrNX.h
	$(MAKE) xzs2nxtune
	$(ATLRUN) $(R1Tdir) xzs2nxtune -U u -n 2000 -r 1 -s 10 -C 0 \
                  -o $(INCAdir)/atlas_zsyr2NX.h
	cd $(L2Bdir) ; make zlib
#
# This target is for directly timing a call to the kernel, w/o cache flushing
#
zr2ktime : zr2ktime.o ATL_zger2k_Mlt16.o ATL_zger2k_Nlt8.o
	rm -f ATL_zger2.c
	cp -f $(R2Kdir)/$(r2rout) ATL_zger2.c
	$(zR2CC) -c $(CDEFS) $(zR2CFLAGS) -I$(R1Tdir) -o ATL_zger2.o \
           -DDCPLX -DATL_zger2_a1_x1_yX=ATL_UGERK ATL_zger2.c
	$(CLINKER) $(CLINKFLAGS) -o xzr2ktime zr2ktime.o \
                   ATL_zger2.o \
                   ATL_zger2k_Mlt16.o ATL_zger2k_Nlt8.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xzr2ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r2 tuning of ref/r2 crossover point (NX)
#
xzs2nxtune : zs2nxtune.o ATL_zher2_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xzs2nxtune zs2nxtune.o \
                   ATL_zher2_NX.o $(TESTlib) $(ATLASlib)
zs2nxtune.o : $(mySRCdir)/s2nxtune.c $(INCAdir)/atlas_zr2.h \
        $(INCAdir)/atlas_zr2_L1.h $(INCAdir)/atlas_zr2_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/s2nxtune.c
#
# These targets use extract to generate a GER2 kernel
#
zr2ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r2_C nu=$(nu) type=DCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/zr2_C.c
zr2ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r2_sse nu=$(nu) type=DCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/zr2_sse.c
#
#
# This target calls the GER2 driver (which blocks), with cache flushing
#
zr2time : zr2time.o xr2hgen
	rm -f $(zr2obj) ATL_zger2.c atlas_zr2.h \
              r2h.bak xzr2time
	- mv $(INCAdir)/atlas_zr2.h rr2.bak
	./xr2hgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_zger2k.c
	$(MAKE) $(zr2obj)
	$(CLINKER) $(CLINKFLAGS) -o xzr2time zr2time.o \
                   $(zr2obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xzr2time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r2h.bak $(INCAdir)/atlas_zr2.h

zr2ktest : zr2ktest.o xr2hgen
	rm -f $(zr2tstobj) ATL_zger2k.c xzr2ktest
	- mv $(INCAdir)/atlas_zr2.h r2h.bak
	./xr2hgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R2Kdir)/$(r2rout) ATL_zger2k.c
	$(MAKE) $(zr2tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xzr2ktest zr2ktest.o \
                   $(zr2tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xzr2ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r2h.bak $(INCAdir)/atlas_zr2.h

zr2time.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/r2ktime.c
zr2ktime.o : $(mySRCdir)/r2ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DTIME_KERNEL $(mySRCdir)/r2ktime.c
zr2ktest.o : $(mySRCdir)/r2ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/r2ktest.c
zr2test.o : $(mySRCdir)/r2test.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/r2test.c
zger2tune.o : $(mySRCdir)/ger2tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/ger2tune.c
zr1ktime : zr1ktime.o ATL_zgerk_Mlt16.o ATL_zgerk_axpy.o
	rm -f ATL_zger1.c
	cp -f $(R1Kdir)/$(r1rout) ATL_zger1.c
	$(zR1CC) -c $(CDEFS) $(zR1CFLAGS) -I$(R1Tdir) -o ATL_zger1.o \
           -DDCPLX -DATL_zger1_a1_x1_yX=ATL_UGERK ATL_zger1.c
	$(CLINKER) $(CLINKFLAGS) -o xzr1ktime zr1ktime.o \
                   ATL_zger1.o \
                   ATL_zgerk_Mlt16.o ATL_zgerk_axpy.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xzr1ktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# These targets are for post-r1 tuning of ref/r1 crossover point (NX)
#
xzs1nxtune : zs1nxtune.o ATL_zher_NX.o
	$(CLINKER) $(CLINKFLAGS) -o xzs1nxtune zs1nxtune.o \
                   ATL_zher_NX.o $(TESTlib) $(ATLASlib)
zs1nxtune.o : $(mySRCdir)/s1nxtune.c $(INCAdir)/atlas_zr1.h \
        $(INCAdir)/atlas_zr1_L1.h $(INCAdir)/atlas_zr1_L2.h 
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/s1nxtune.c
#
# These targets use extract to generate a GER1 kernel
#
zr1ext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base -langC \
                           order=$(order) rout=r1_C nu=$(nu) type=DCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/zr1_C.c
zr1ext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(SRCdir)/tune/blas/gemv/atlas-l2g.base \
                           order=$(order) rout=r1_sse nu=$(nu) type=DCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/zr1_sse.c
#
#
# This target calls the GER1 driver (which blocks), with cache flushing
#
zr1time : zr1time.o xr1hgen
	rm -f $(zr1obj) ATL_zger1.c atlas_zr1.h \
              r1h.bak xzr1time
	- mv $(INCAdir)/atlas_zr1.h rr1.bak
	./xr1hgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_zger1k.c
	$(MAKE) $(zr1obj)
	$(CLINKER) $(CLINKFLAGS) -o xzr1time zr1time.o \
                   $(zr1obj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xzr1time -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv r1h.bak $(INCAdir)/atlas_zr1.h

zr1ktest : zr1ktest.o xr1hgen
	rm -f $(zr1tstobj) ATL_zger1k.c xzr1ktest
	- mv $(INCAdir)/atlas_zr1.h r1h.bak
	./xr1hgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag)
	cp -f $(R1Kdir)/$(r1rout) ATL_zger1k.c
	$(MAKE) $(zr1tstobj)
	$(CLINKER) $(CLINKFLAGS) -o xzr1ktest zr1ktest.o \
                   $(zr1tstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(R1Tdir) xzr1ktest -m 1 $(Mt) -n 1 $(Nt) -y 1 $(incy) \
                  -l 1 $(ldat) $(align)
	- mv r1h.bak $(INCAdir)/atlas_zr1.h

zr1time.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/r1ktime.c
zr1ktime.o : $(mySRCdir)/r1ktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DTIME_KERNEL $(mySRCdir)/r1ktime.c
zr1ktest.o : $(mySRCdir)/r1ktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/r1ktest.c
zr1test.o : $(mySRCdir)/r1test.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/r1test.c
zger1tune.o : $(mySRCdir)/ger1tune.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/ger1tune.c
ATL_zger.o : ATL_ger.c atlas_zr1.h
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DTUNING -I$(R1Tdir) ATL_ger.c
ATL_zger2.o : ATL_ger2.c atlas_zr2.h
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DTUNING -I$(R1Tdir) ATL_ger2.c
ATL_zger2k_Nlt8.o : $(r1SRCdir)/ATL_ger2k_Nlt8.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(r1SRCdir)/ATL_ger2k_Nlt8.c
ATL_zger2ck_Nlt8.o : $(r1SRCdir)/ATL_ger2k_Nlt8.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DDCPLX $(r1SRCdir)/ATL_ger2k_Nlt8.c
ATL_zgerk_Nlt8.o : $(r1SRCdir)/ATL_gerk_Nlt8.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(r1SRCdir)/ATL_gerk_Nlt8.c
ATL_zgerck_Nlt8.o : $(r1SRCdir)/ATL_gerk_Nlt8.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DDCPLX $(r1SRCdir)/ATL_gerk_Nlt8.c
ATL_zger2k_axpy.o : $(r1SRCdir)/ATL_ger2k_axpy.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(r1SRCdir)/ATL_ger2k_axpy.c
ATL_zger2ck_axpy.o : $(r1SRCdir)/ATL_ger2k_axpy.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DDCPLX $(r1SRCdir)/ATL_ger2k_axpy.c
ATL_zgerk_axpy.o : $(r1SRCdir)/ATL_gerk_axpy.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(r1SRCdir)/ATL_gerk_axpy.c
ATL_zgerck_axpy.o : $(r1SRCdir)/ATL_gerk_axpy.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DDCPLX $(r1SRCdir)/ATL_gerk_axpy.c
ATL_zger2k_Mlt16.o : $(r1SRCdir)/ATL_ger2k_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(r1SRCdir)/ATL_ger2k_Mlt16.c
ATL_zger2ck_Mlt16.o : $(r1SRCdir)/ATL_ger2k_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DDCPLX $(r1SRCdir)/ATL_ger2k_Mlt16.c
ATL_zgerk_Mlt16.o : $(r1SRCdir)/ATL_gerk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(R1Tdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(r1SRCdir)/ATL_gerk_Mlt16.c
ATL_zgerck_Mlt16.o : $(r1SRCdir)/ATL_gerk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -DConj_ -I$(R1Tdir) \
                       -DTUNING -o $@ -DDCPLX $(r1SRCdir)/ATL_gerk_Mlt16.c
ATL_zger2k.o : ATL_zger2k.c
	$(zR2CC) -c $(CDEFS) $(zR2CFLAGS) -I$(R1Tdir) -o $@ -DDCPLX ATL_zger2k.c
ATL_zger2c.o : ATL_zger2.c
	$(zR2CC) -c $(CDEFS) $(zR2CFLAGS) -I$(R1Tdir) -o $@ -DDCPLX -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_zger2.c
res/zR2K.sum : xr2ksearch force_build res/zR1K.sum zr1install
	./xr2ksearch -p z
	$(MAKE) zr2install
zr2outd:
	mkdir zr2outd
zr2install : xccobj zr2outd xr2hgen force_build
	rm -f zr2outd/*
	if [ -s "$(GR1dir)/Make_zr2" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_zr2 killall ; \
           rm -f $(GR1dir)/Make_zr2 ; \
        fi
	./xr2hgen -p z -F res/zR2K.sum -d zr2outd
	cp -f zr2outd/Make_zr2 $(GR1dir)/.
	cp zr2outd/*.c $(GR1dir)/.
	cp -f zr2outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r2.h
	rm -f zr2outd/* ; rmdir zr2outd
ATL_zger1k.o : ATL_zger1k.c
	$(zR1CC) -c $(CDEFS) $(zR1CFLAGS) -I$(R1Tdir) -o $@ -DDCPLX ATL_zger1k.c
ATL_zger1c.o : ATL_zger1.c
	$(zR1CC) -c $(CDEFS) $(zR1CFLAGS) -I$(R1Tdir) -o $@ -DDCPLX -DConj_ \
              -DATL_UGERK=ATL_GERK_Conj ATL_zger1.c
res/zR1K.sum : xr1ksearch force_build  
	./xr1ksearch -p z
	$(MAKE) zr1install
zr1outd:
	mkdir zr1outd
zr1install : xccobj zr1outd xr1hgen force_build
	rm -f zr1outd/*
	if [ -s "$(GR1dir)/Make_zr1" ]; then \
           cd  $(GR1dir) ; $(MAKE) -f Make_zr1 killall ; \
           rm -f $(GR1dir)/Make_zr1 ; \
        fi
	./xr1hgen -p z -F res/zR1K.sum -d zr1outd
	cp -f zr1outd/Make_zr1 $(GR1dir)/.
	cp zr1outd/*.c $(GR1dir)/.
	cp -f zr1outd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GR1dir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GR1dir)/. ; \
        fi
	touch $(INCSdir)/atlas_r1.h
	rm -f zr1outd/* ; rmdir zr1outd

ATL_zher2_NX.o : $(SRCdir)/src/blas/level2/ATL_her2.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her2.c
ATL_zher_NX.o : $(SRCdir)/src/blas/level2/ATL_her.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_her.c
ATL_zsyr2_NX.o : $(SRCdir)/src/blas/level2/ATL_syr2.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr2.c
ATL_zsyr_NX.o : $(SRCdir)/src/blas/level2/ATL_syr.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_NXTUNE \
                $(SRCdir)/src/blas/level2/ATL_syr.c
zl1lib :
	cd $(BINdir) ; $(MAKE) zl1lib
ztstlib :
	cd $(BINdir) ; $(MAKE) ztstlib

$(BINdir)/xsubstr :
	cd $(BINdir) ; $(MAKE) xsubstr
$(BINdir)/xccobj :
	cd $(BINdir) ; $(MAKE) xccobj
xccobj : $(BINdir)/xccobj
	-@ cp -f $(BINdir)/xccobj .
	-@ cp -f $(BINdir)/xccobj.exe .
EXTDIR :
	mkdir EXTDIR
$(BINdir)/xextract :
	cd $(BINdir) ; make xextract
