include Make.inc
mySRCdir = $(SRCdir)/tune/blas/gemv
mvSRCdir = $(SRCdir)/src/blas/gemv
MVNKdir = $(mySRCdir)/MVNCASES/
MVTKdir = $(mySRCdir)/MVTCASES/

cas=0
lat=0
nY=1
ngap=1
agap=1
gapmul=1
xregs=1
aregs=1
yregs=$(nY)
Mt=997
Nt=177
ldat=1111
M=1000
N=1000
lda=M
mvrout=bogus
ta=N
nta=T
iflag=0
l1mul=80
outf=
align = -Fx 16 -Fy 16 -Fa 16
sMVCC = $(SKC)
sMVFLAGS = $(SKCFLAGS)
cMVCC = $(SKC)
cMVFLAGS = $(SKCFLAGS)
dMVCC = $(DKC)
dMVFLAGS = $(DKCFLAGS)
zMVCC = $(DKC)
zMVFLAGS = $(DKCFLAGS)
EXTFLAGS =
sSVCC = $(SKC)
sSVFLAGS = $(SKCFLAGS)
cSVCC = $(SKC)
cSVFLAGS = $(SKCFLAGS)
dSVCC = $(DKC)
dSVFLAGS = $(DKCFLAGS)
zSVCC = $(DKC)
zSVFLAGS = $(DKCFLAGS)
SEXTFLAGS = 
opt=
BETA=1
beta=1
flushKB=-1
salphas = 3 0.0 1.0 0.5
sbetas = 3 0.0 1.0 0.2
calphas = 4 0.0 0.0 1.0 0.0 2.3 0.0 0.5 0.25
cbetas = 4 0.0 0.0 1.0 0.0 0.8 0.0 0.2 0.6
incs = -y 4 -1 1 2 -3 -x 4 -1 1 -5 4
dalphas = $(salphas)
dbetas = $(sbetas)
zalphas = $(calphas)
zbetas = $(cbetas)
ntlb=
#
# Macros for generator
#
mu=1
nu=1
minM=$(mu)
minN=$(nu)
genflags=
order=clmajor

all:
$(BINdir)/xextract :
	cd $(BINdir) ; make xextract
xmvnhgen : mvnhgen.o
	$(XCC) $(XCCFLAGS) -o $@ mvnhgen.o
mvnhgen.o : $(mySRCdir)/mvnhgen.c $(INCSdir)/atlas_mvparse.h \
          $(INCSdir)/atlas_mvtesttime.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/mvnhgen.c
xmvthgen : mvthgen.o
	$(XCC) $(XCCFLAGS) -o $@ mvthgen.o
mvthgen.o : $(mySRCdir)/mvthgen.c $(INCSdir)/atlas_mvparse.h \
          $(INCSdir)/atlas_mvtesttime.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/mvthgen.c
xmvtksearch : mvtksearch.o
	$(XCC) $(XCCFLAGS) -o $@ mvtksearch.o
mvtksearch.o : $(mySRCdir)/mvtksearch.c $(INCSdir)/atlas_mvparse.h \
          $(INCSdir)/atlas_mvtesttime.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/mvtksearch.c
xmvnksearch : mvnksearch.o
	$(XCC) $(XCCFLAGS) -o $@ mvnksearch.o
mvnksearch.o : $(mySRCdir)/mvnksearch.c $(INCSdir)/atlas_mvparse.h \
          $(INCSdir)/atlas_mvtesttime.h
	$(XCC) -c $(XCCFLAGS) -o $@ $(mySRCdir)/mvnksearch.c
$(INCAdir)/atlas_type.h :
	cd $(SYSdir) ; $(MAKE) RunTyp
res/goodgcc.txt :
	echo "'$(GOODGCC)'" > res/goodgcc.txt

auxlib :
	cd $(AUXdir) ; $(MAKE) lib
l1lib : sl1lib dl1lib cl1lib zl1lib 
tstlib : ststlib dtstlib ctstlib ztstlib

force_build :

res/L1CacheSize : $(SYSdir)/res/L1CacheSize
	cd $(SYSdir) ; $(MAKE) res/L1CacheSize
	ln -s $(SYSdir)/res/L1CacheSize res/L1CacheSize

ATL_gemv.c :
	cp $(mySRCdir)/ATL_gemv.c .
ststNobj = ATL_sgemvN_b0.o ATL_sgemvN_b1.o ATL_sgemvN_bX.o
ststTobj = ATL_sgemvT_b0.o ATL_sgemvT_b1.o ATL_sgemvT_bX.o
sobj = ATL_sgemv.o $(ststNobj) $(ststTobj)
ststobj = $(ststNobj) $(ststTobj)
smvttstobj = ATL_smvtk.o ATL_smvtk_smallN.o ATL_smvtk_Mlt16.o
smvttobj = ATL_smvtk_b0.o ATL_smvtk_b1.o \
           ATL_smvtk_smallN.o ATL_smvtk_Mlt16.o
smvntstobj = ATL_smvnk.o ATL_smvnk_smallN.o ATL_smvnk_Mlt16.o
smvntobj = ATL_smvnk_b0.o ATL_smvnk_b1.o \
           ATL_smvnk_smallN.o ATL_smvnk_Mlt16.o

#
# This target searches for best kernels and installs them
#
res/sMVNK.sum : xmvnksearch force_build
	./xmvnksearch -p s
	$(MAKE) smvninstall
#
# This target uses extract to generate a MVN kernel
#
smvnext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvn_C nu=$(nu) type=SREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/smvn_C.c
smvnext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvn_sse nu=$(nu) type=SREAL -def MU $(mu) \
                           $(genflags) -o EXTDIR/smvn_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
smvnktime : smvktimeN.o 
	rm -f ATL_smvnk.c ATL_sgemvN.o xsmvnktime
	cp -f $(MVNKdir)/$(mvnrout) ATL_smvnk.c
	$(sMVCC) -c $(CDEFS) $(sMVFLAGS) -I$(MVTdir) -o ATL_smvnk.o \
           -DSREAL ATL_smvnk.c
	$(CLINKER) $(CLINKFLAGS) -o xsmvnktime smvktimeN.o \
                   ATL_smvnk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvnktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
smvntime : smvntime.o xmvnhgen
	- rm -f $(smvntobj) ATL_smvnk.c atlas_smvn.h \
                mvnh.bak ATL_sgemvN.o xsmvntime
	- mv $(INCAdir)/atlas_smvn.h mvnh.bak
	./xmvnhgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_smvnk.c
	$(MAKE) $(smvntobj) ATL_sgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xsmvntime smvntime.o \
                   ATL_sgemvN.o $(smvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvntime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvnh.bak $(INCAdir)/atlas_smvn.h
#
# This tester calls the driver routine that blocks
#
smvntest : xmvnhgen smvntest.o
	rm -f $(smvntobj) ATL_smvnk.c  ATL_sgemvN.o \
              xsmvntest
	- mv $(INCAdir)/atlas_smvn.h mvnh.bak
	./xmvnhgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_smvnk.c
	$(MAKE) $(smvntobj) ATL_sgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xsmvntest smvntest.o \
                   ATL_sgemvN.o $(smvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvntest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(salphas) -b $(sbetas) 
	- mv mvnh.bak $(INCAdir)/atlas_smvn.h

smvnktest : xmvnhgen
	rm -f $(smvntstobj) ATL_smvnk.c smvnktest.o \
              ATL_sgemvN.o xsmvnktest
	- mv $(INCAdir)/atlas_smvn.h mvnh.bak
	./xmvnhgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_smvnk.c
	$(MAKE) smvnktest.o $(smvntstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xsmvnktest smvnktest.o \
                   $(smvntstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvnktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvnh.bak $(INCAdir)/atlas_smvn.h

smvntime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL   $(mySRCdir)/mvktime.c
smvtimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL   $(mySRCdir)/mvktime.c

smvktimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL   -DTIME_KERNEL $(mySRCdir)/mvktime.c
smvnktest.o : $(mySRCdir)/mvnktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DBETA$(beta)   $(mySRCdir)/mvnktest.c
smvntest.o : $(mySRCdir)/mvntest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/mvntest.c

ATL_sgemvN.o : $(mvSRCdir)/ATL_gemvN.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DSREAL $(mvSRCdir)/ATL_gemvN.c
#
# This target searches for best kernels and installs them
#
res/sMVTK.sum : xmvtksearch force_build
	./xmvtksearch -p s
	$(MAKE) smvtinstall
#
# This target uses extract to generate a MVT kernel
#
smvtext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvt_C nu=$(nu) type=SREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/smvt_C.c
smvtext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvt_sse nu=$(nu) type=SREAL -def MU $(mu) \
                           $(genflags) -o EXTDIR/smvt_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
smvtktime : smvktimeT.o 
	rm -f ATL_smvtk.c ATL_sgemvT.o xsmvtktime
	cp -f $(MVTKdir)/$(mvtrout) ATL_smvtk.c
	$(sMVCC) -c $(CDEFS) $(sMVFLAGS) -I$(MVTdir) -o ATL_smvtk.o \
           -DSREAL ATL_smvtk.c
	$(CLINKER) $(CLINKFLAGS) -o xsmvtktime smvktimeT.o \
                   ATL_smvtk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvtktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
smvttime : smvttime.o xmvthgen
	- rm -f $(smvttobj) ATL_smvtk.c atlas_smvt.h \
                mvth.bak ATL_sgemvT.o xsmvttime
	- mv $(INCAdir)/atlas_smvt.h mvth.bak
	./xmvthgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_smvtk.c
	$(MAKE) $(smvttobj) ATL_sgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xsmvttime smvttime.o \
                   ATL_sgemvT.o $(smvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvttime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvth.bak $(INCAdir)/atlas_smvt.h
#
# This tester calls the driver routine that blocks
#
smvttest : xmvthgen smvttest.o
	rm -f $(smvttobj) ATL_smvtk.c  ATL_sgemvT.o \
              xsmvttest
	- mv $(INCAdir)/atlas_smvt.h mvth.bak
	./xmvthgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_smvtk.c
	$(MAKE) $(smvttobj) ATL_sgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xsmvttest smvttest.o \
                   ATL_sgemvT.o $(smvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvttest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(salphas) -b $(sbetas) 
	- mv mvth.bak $(INCAdir)/atlas_smvt.h

smvtktest : xmvthgen
	rm -f $(smvttstobj) ATL_smvtk.c smvtktest.o \
              ATL_sgemvT.o xsmvtktest
	- mv $(INCAdir)/atlas_smvt.h mvth.bak
	./xmvthgen -p s -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_smvtk.c
	$(MAKE) smvtktest.o $(smvttstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xsmvtktest smvtktest.o \
                   $(smvttstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xsmvtktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvth.bak $(INCAdir)/atlas_smvt.h

smvttime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_TRANS_ $(mySRCdir)/mvktime.c
smvtimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_TRANS_ $(mySRCdir)/mvktime.c

smvktimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DATL_TRANS_ -DTIME_KERNEL $(mySRCdir)/mvktime.c
smvtktest.o : $(mySRCdir)/mvtktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL -DBETA$(beta) -DATL_TRANS_ $(mySRCdir)/mvtktest.c
smvttest.o : $(mySRCdir)/mvttest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSREAL $(mySRCdir)/mvttest.c

ATL_sgemvT.o : $(mvSRCdir)/ATL_gemvT.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DSREAL $(mvSRCdir)/ATL_gemvT.c

ATL_smvtk_smallN.o : $(mvSRCdir)/ATL_mvtk_smallN.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSREAL $(mvSRCdir)/ATL_mvtk_smallN.c
ATL_smvtk_Mlt16.o : $(mvSRCdir)/ATL_mvtk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSREAL $(mvSRCdir)/ATL_mvtk_Mlt16.c
ATL_smvtk_b0.o : ATL_smvtk.c
	$(sMVCC) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSREAL \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVTK_b0 -o ATL_smvtk_b0.o \
                 -c ATL_smvtk.c
ATL_smvtk_b1.o : ATL_smvtk.c
	$(sMVCC) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSREAL \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVTK -o ATL_smvtk_b1.o \
                 -c ATL_smvtk.c
ATL_smvtk.o : ATL_smvtk.c
	$(sMVCC) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSREAL \
                 -DBETA$(beta) -o ATL_smvtk.o -c ATL_smvtk.c

ATL_smvnk_smallN.o : $(mvSRCdir)/ATL_mvnk_smallN.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSREAL $(mvSRCdir)/ATL_mvnk_smallN.c
ATL_smvnk_Mlt16.o : $(mvSRCdir)/ATL_mvnk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSREAL $(mvSRCdir)/ATL_mvnk_Mlt16.c
ATL_smvnk_b0.o : ATL_smvnk.c
	$(sMVCC) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSREAL \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVNK_b0 -o ATL_smvnk_b0.o \
                 -c ATL_smvnk.c
ATL_smvnk_b1.o : ATL_smvnk.c
	$(sMVCC) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSREAL \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVNK -o ATL_smvnk_b1.o \
                 -c ATL_smvnk.c
ATL_smvnk.o : ATL_smvnk.c
	$(sMVCC) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSREAL \
                 -DBETA$(beta) -o ATL_smvnk.o -c ATL_smvnk.c

ATL_sgemv.o : ATL_gemv.c atlas_smvn.h atlas_smvt.h
	$(SKC) -c -I$(MVTdir) $(sKCFLAGS) -o $@ -DSREAL ATL_gemv.c
ATL_sgemvN_b0.o : ATL_sgemvN.c
	$(sMVCC) -c -I$(MVTdir) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSREAL -DBETA0 -DALPHA1 ATL_sgemvN.c
ATL_sgemvN_b1.o : ATL_sgemvN.c
	$(sMVCC) -c -I$(MVTdir) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSREAL -DBETA1 -DALPHA1 ATL_sgemvN.c
ATL_sgemvN_bX.o : ATL_sgemvN.c
	$(sMVCC) -c -I$(MVTdir) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSREAL -DBETAX -DALPHA1 ATL_sgemvN.c
ATL_sgemvT_b0.o : ATL_sgemvT.c
	$(sMVCC) -c -I$(MVTdir) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSREAL -DBETA0 -DALPHA1 ATL_sgemvT.c
ATL_sgemvT_b1.o : ATL_sgemvT.c
	$(sMVCC) -c -I$(MVTdir) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSREAL -DBETA1 -DALPHA1 ATL_sgemvT.c
ATL_sgemvT_bX.o : ATL_sgemvT.c
	$(sMVCC) -c -I$(MVTdir) $(sMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSREAL -DBETAX -DALPHA1 ATL_sgemvT.c

smvtoutd :
	mkdir smvtoutd
smvtinstall : xccobj smvtoutd xmvthgen force_build
	rm -f smvtoutd/*
	if [ -s "$(GMVdir)/Make_smvt" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_smvt killall ; \
           rm -f $(GMVdir)/Make_smvt ; \
        fi
	./xmvthgen -p s -F res/sMVTK.sum -d smvtoutd
	cp -f smvtoutd/Make_smvt $(GMVdir)/.
	cp smvtoutd/*.c $(GMVdir)/.
	cp -f smvtoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f smvtoutd/* ; rmdir smvtoutd
smvnoutd :
	mkdir smvnoutd
smvninstall : xccobj smvnoutd xmvnhgen force_build
	rm -f smvnoutd/*
	if [ -s "$(GMVdir)/Make_smvn" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_smvn killall ; \
           rm -f $(GMVdir)/Make_smvn ; \
        fi
	./xmvnhgen -p s -F res/sMVNK.sum -d smvnoutd
	cp -f smvnoutd/Make_smvn $(GMVdir)/.
	cp smvnoutd/*.c $(GMVdir)/.
	cp -f smvnoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f smvnoutd/* ; rmdir smvnoutd
sl1lib :
	cd $(BINdir) ; $(MAKE) sl1lib
ststlib :
	cd $(BINdir) ; $(MAKE) ststlib
dtstNobj = ATL_dgemvN_b0.o ATL_dgemvN_b1.o ATL_dgemvN_bX.o
dtstTobj = ATL_dgemvT_b0.o ATL_dgemvT_b1.o ATL_dgemvT_bX.o
dobj = ATL_dgemv.o $(dtstNobj) $(dtstTobj)
dtstobj = $(dtstNobj) $(dtstTobj)
dmvttstobj = ATL_dmvtk.o ATL_dmvtk_smallN.o ATL_dmvtk_Mlt16.o
dmvttobj = ATL_dmvtk_b0.o ATL_dmvtk_b1.o \
           ATL_dmvtk_smallN.o ATL_dmvtk_Mlt16.o
dmvntstobj = ATL_dmvnk.o ATL_dmvnk_smallN.o ATL_dmvnk_Mlt16.o
dmvntobj = ATL_dmvnk_b0.o ATL_dmvnk_b1.o \
           ATL_dmvnk_smallN.o ATL_dmvnk_Mlt16.o

#
# This target searches for best kernels and installs them
#
res/dMVNK.sum : xmvnksearch force_build
	./xmvnksearch -p d
	$(MAKE) dmvninstall
#
# This target uses extract to generate a MVN kernel
#
dmvnext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvn_C nu=$(nu) type=DREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/dmvn_C.c
dmvnext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvn_sse nu=$(nu) type=DREAL -def MU $(mu) \
                           $(genflags) -o EXTDIR/dmvn_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
dmvnktime : dmvktimeN.o 
	rm -f ATL_dmvnk.c ATL_dgemvN.o xdmvnktime
	cp -f $(MVNKdir)/$(mvnrout) ATL_dmvnk.c
	$(dMVCC) -c $(CDEFS) $(dMVFLAGS) -I$(MVTdir) -o ATL_dmvnk.o \
           -DDREAL ATL_dmvnk.c
	$(CLINKER) $(CLINKFLAGS) -o xdmvnktime dmvktimeN.o \
                   ATL_dmvnk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvnktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
dmvntime : dmvntime.o xmvnhgen
	- rm -f $(dmvntobj) ATL_dmvnk.c atlas_dmvn.h \
                mvnh.bak ATL_dgemvN.o xdmvntime
	- mv $(INCAdir)/atlas_dmvn.h mvnh.bak
	./xmvnhgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_dmvnk.c
	$(MAKE) $(dmvntobj) ATL_dgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xdmvntime dmvntime.o \
                   ATL_dgemvN.o $(dmvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvntime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvnh.bak $(INCAdir)/atlas_dmvn.h
#
# This tester calls the driver routine that blocks
#
dmvntest : xmvnhgen dmvntest.o
	rm -f $(dmvntobj) ATL_dmvnk.c  ATL_dgemvN.o \
              xdmvntest
	- mv $(INCAdir)/atlas_dmvn.h mvnh.bak
	./xmvnhgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_dmvnk.c
	$(MAKE) $(dmvntobj) ATL_dgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xdmvntest dmvntest.o \
                   ATL_dgemvN.o $(dmvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvntest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(dalphas) -b $(dbetas) 
	- mv mvnh.bak $(INCAdir)/atlas_dmvn.h

dmvnktest : xmvnhgen
	rm -f $(dmvntstobj) ATL_dmvnk.c dmvnktest.o \
              ATL_dgemvN.o xdmvnktest
	- mv $(INCAdir)/atlas_dmvn.h mvnh.bak
	./xmvnhgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_dmvnk.c
	$(MAKE) dmvnktest.o $(dmvntstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xdmvnktest dmvnktest.o \
                   $(dmvntstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvnktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvnh.bak $(INCAdir)/atlas_dmvn.h

dmvntime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL   $(mySRCdir)/mvktime.c
dmvtimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL   $(mySRCdir)/mvktime.c

dmvktimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL   -DTIME_KERNEL $(mySRCdir)/mvktime.c
dmvnktest.o : $(mySRCdir)/mvnktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DBETA$(beta)   $(mySRCdir)/mvnktest.c
dmvntest.o : $(mySRCdir)/mvntest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/mvntest.c

ATL_dgemvN.o : $(mvSRCdir)/ATL_gemvN.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DDREAL $(mvSRCdir)/ATL_gemvN.c
#
# This target searches for best kernels and installs them
#
res/dMVTK.sum : xmvtksearch force_build
	./xmvtksearch -p d
	$(MAKE) dmvtinstall
#
# This target uses extract to generate a MVT kernel
#
dmvtext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvt_C nu=$(nu) type=DREAL \
                           -def MU $(mu) $(genflags) -o EXTDIR/dmvt_C.c
dmvtext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvt_sse nu=$(nu) type=DREAL -def MU $(mu) \
                           $(genflags) -o EXTDIR/dmvt_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
dmvtktime : dmvktimeT.o 
	rm -f ATL_dmvtk.c ATL_dgemvT.o xdmvtktime
	cp -f $(MVTKdir)/$(mvtrout) ATL_dmvtk.c
	$(dMVCC) -c $(CDEFS) $(dMVFLAGS) -I$(MVTdir) -o ATL_dmvtk.o \
           -DDREAL ATL_dmvtk.c
	$(CLINKER) $(CLINKFLAGS) -o xdmvtktime dmvktimeT.o \
                   ATL_dmvtk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvtktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
dmvttime : dmvttime.o xmvthgen
	- rm -f $(dmvttobj) ATL_dmvtk.c atlas_dmvt.h \
                mvth.bak ATL_dgemvT.o xdmvttime
	- mv $(INCAdir)/atlas_dmvt.h mvth.bak
	./xmvthgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_dmvtk.c
	$(MAKE) $(dmvttobj) ATL_dgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xdmvttime dmvttime.o \
                   ATL_dgemvT.o $(dmvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvttime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvth.bak $(INCAdir)/atlas_dmvt.h
#
# This tester calls the driver routine that blocks
#
dmvttest : xmvthgen dmvttest.o
	rm -f $(dmvttobj) ATL_dmvtk.c  ATL_dgemvT.o \
              xdmvttest
	- mv $(INCAdir)/atlas_dmvt.h mvth.bak
	./xmvthgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_dmvtk.c
	$(MAKE) $(dmvttobj) ATL_dgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xdmvttest dmvttest.o \
                   ATL_dgemvT.o $(dmvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvttest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(dalphas) -b $(dbetas) 
	- mv mvth.bak $(INCAdir)/atlas_dmvt.h

dmvtktest : xmvthgen
	rm -f $(dmvttstobj) ATL_dmvtk.c dmvtktest.o \
              ATL_dgemvT.o xdmvtktest
	- mv $(INCAdir)/atlas_dmvt.h mvth.bak
	./xmvthgen -p d -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_dmvtk.c
	$(MAKE) dmvtktest.o $(dmvttstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xdmvtktest dmvtktest.o \
                   $(dmvttstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xdmvtktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvth.bak $(INCAdir)/atlas_dmvt.h

dmvttime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_TRANS_ $(mySRCdir)/mvktime.c
dmvtimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_TRANS_ $(mySRCdir)/mvktime.c

dmvktimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DATL_TRANS_ -DTIME_KERNEL $(mySRCdir)/mvktime.c
dmvtktest.o : $(mySRCdir)/mvtktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL -DBETA$(beta) -DATL_TRANS_ $(mySRCdir)/mvtktest.c
dmvttest.o : $(mySRCdir)/mvttest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDREAL $(mySRCdir)/mvttest.c

ATL_dgemvT.o : $(mvSRCdir)/ATL_gemvT.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DDREAL $(mvSRCdir)/ATL_gemvT.c

ATL_dmvtk_smallN.o : $(mvSRCdir)/ATL_mvtk_smallN.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDREAL $(mvSRCdir)/ATL_mvtk_smallN.c
ATL_dmvtk_Mlt16.o : $(mvSRCdir)/ATL_mvtk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDREAL $(mvSRCdir)/ATL_mvtk_Mlt16.c
ATL_dmvtk_b0.o : ATL_dmvtk.c
	$(dMVCC) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDREAL \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVTK_b0 -o ATL_dmvtk_b0.o \
                 -c ATL_dmvtk.c
ATL_dmvtk_b1.o : ATL_dmvtk.c
	$(dMVCC) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDREAL \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVTK -o ATL_dmvtk_b1.o \
                 -c ATL_dmvtk.c
ATL_dmvtk.o : ATL_dmvtk.c
	$(dMVCC) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDREAL \
                 -DBETA$(beta) -o ATL_dmvtk.o -c ATL_dmvtk.c

ATL_dmvnk_smallN.o : $(mvSRCdir)/ATL_mvnk_smallN.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDREAL $(mvSRCdir)/ATL_mvnk_smallN.c
ATL_dmvnk_Mlt16.o : $(mvSRCdir)/ATL_mvnk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDREAL $(mvSRCdir)/ATL_mvnk_Mlt16.c
ATL_dmvnk_b0.o : ATL_dmvnk.c
	$(dMVCC) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDREAL \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVNK_b0 -o ATL_dmvnk_b0.o \
                 -c ATL_dmvnk.c
ATL_dmvnk_b1.o : ATL_dmvnk.c
	$(dMVCC) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDREAL \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVNK -o ATL_dmvnk_b1.o \
                 -c ATL_dmvnk.c
ATL_dmvnk.o : ATL_dmvnk.c
	$(dMVCC) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDREAL \
                 -DBETA$(beta) -o ATL_dmvnk.o -c ATL_dmvnk.c

ATL_dgemv.o : ATL_gemv.c atlas_dmvn.h atlas_dmvt.h
	$(DKC) -c -I$(MVTdir) $(dKCFLAGS) -o $@ -DDREAL ATL_gemv.c
ATL_dgemvN_b0.o : ATL_dgemvN.c
	$(dMVCC) -c -I$(MVTdir) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDREAL -DBETA0 -DALPHA1 ATL_dgemvN.c
ATL_dgemvN_b1.o : ATL_dgemvN.c
	$(dMVCC) -c -I$(MVTdir) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDREAL -DBETA1 -DALPHA1 ATL_dgemvN.c
ATL_dgemvN_bX.o : ATL_dgemvN.c
	$(dMVCC) -c -I$(MVTdir) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDREAL -DBETAX -DALPHA1 ATL_dgemvN.c
ATL_dgemvT_b0.o : ATL_dgemvT.c
	$(dMVCC) -c -I$(MVTdir) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDREAL -DBETA0 -DALPHA1 ATL_dgemvT.c
ATL_dgemvT_b1.o : ATL_dgemvT.c
	$(dMVCC) -c -I$(MVTdir) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDREAL -DBETA1 -DALPHA1 ATL_dgemvT.c
ATL_dgemvT_bX.o : ATL_dgemvT.c
	$(dMVCC) -c -I$(MVTdir) $(dMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDREAL -DBETAX -DALPHA1 ATL_dgemvT.c

dmvtoutd :
	mkdir dmvtoutd
dmvtinstall : xccobj dmvtoutd xmvthgen force_build
	rm -f dmvtoutd/*
	if [ -s "$(GMVdir)/Make_dmvt" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_dmvt killall ; \
           rm -f $(GMVdir)/Make_dmvt ; \
        fi
	./xmvthgen -p d -F res/dMVTK.sum -d dmvtoutd
	cp -f dmvtoutd/Make_dmvt $(GMVdir)/.
	cp dmvtoutd/*.c $(GMVdir)/.
	cp -f dmvtoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f dmvtoutd/* ; rmdir dmvtoutd
dmvnoutd :
	mkdir dmvnoutd
dmvninstall : xccobj dmvnoutd xmvnhgen force_build
	rm -f dmvnoutd/*
	if [ -s "$(GMVdir)/Make_dmvn" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_dmvn killall ; \
           rm -f $(GMVdir)/Make_dmvn ; \
        fi
	./xmvnhgen -p d -F res/dMVNK.sum -d dmvnoutd
	cp -f dmvnoutd/Make_dmvn $(GMVdir)/.
	cp dmvnoutd/*.c $(GMVdir)/.
	cp -f dmvnoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f dmvnoutd/* ; rmdir dmvnoutd
dl1lib :
	cd $(BINdir) ; $(MAKE) dl1lib
dtstlib :
	cd $(BINdir) ; $(MAKE) dtstlib
cobj = ATL_cgemv.o \
       ATL_cgemvN_b0.o ATL_cgemvT_b0.o \
       ATL_cgemvN_b1.o ATL_cgemvT_b1.o \
       ATL_cgemvN_bXi0.o ATL_cgemvT_bXi0.o \
       ATL_cgemvN_bX.o ATL_cgemvT_bX.o

ctstNobj = ATL_cgemvN_b0.o ATL_cgemvNc_b0.o \
           ATL_cgemvN_b1.o ATL_cgemvNc_b1.o \
           ATL_cgemvN_bXi0.o ATL_cgemvNc_bXi0.o \
           ATL_cgemvN_bX.o ATL_cgemvNc_bX.o

ctstTobj = ATL_cgemvT_b0.o ATL_cgemvC_b0.o \
           ATL_cgemvT_b1.o ATL_cgemvC_b1.o \
           ATL_cgemvT_bXi0.o ATL_cgemvC_bXi0.o \
           ATL_cgemvT_bX.o ATL_cgemvC_bX.o
ctstobj = $(ctstNobj) $(ctstTobj)
cmvttstobj = ATL_cmvtk.o ATL_cmvtk_smallN.o ATL_cmvtk_Mlt16.o
cmvttobj = ATL_cmvtk_b0.o ATL_cmvtk_b1.o \
           ATL_cmvtk_smallN.o ATL_cmvtk_Mlt16.o
cmvntstobj = ATL_cmvnk.o ATL_cmvnk_smallN.o ATL_cmvnk_Mlt16.o
cmvntobj = ATL_cmvnk_b0.o ATL_cmvnk_b1.o \
           ATL_cmvnk_smallN.o ATL_cmvnk_Mlt16.o

#
# This target searches for best kernels and installs them
#
res/cMVNK.sum : xmvnksearch force_build
	./xmvnksearch -p c
	$(MAKE) cmvninstall
#
# This target uses extract to generate a MVN kernel
#
cmvnext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvn_C nu=$(nu) type=SCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/cmvn_C.c
cmvnext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvn_sse nu=$(nu) type=SCPLX -def MU $(mu) \
                           $(genflags) -o EXTDIR/cmvn_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
cmvnktime : cmvktimeN.o 
	rm -f ATL_cmvnk.c ATL_cgemvN.o xcmvnktime
	cp -f $(MVNKdir)/$(mvnrout) ATL_cmvnk.c
	$(cMVCC) -c $(CDEFS) $(cMVFLAGS) -I$(MVTdir) -o ATL_cmvnk.o \
           -DSCPLX ATL_cmvnk.c
	$(CLINKER) $(CLINKFLAGS) -o xcmvnktime cmvktimeN.o \
                   ATL_cmvnk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvnktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
cmvntime : cmvntime.o xmvnhgen
	- rm -f $(cmvntobj) ATL_cmvnk.c atlas_cmvn.h \
                mvnh.bak ATL_cgemvN.o xcmvntime
	- mv $(INCAdir)/atlas_cmvn.h mvnh.bak
	./xmvnhgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_cmvnk.c
	$(MAKE) $(cmvntobj) ATL_cgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xcmvntime cmvntime.o \
                   ATL_cgemvN.o $(cmvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvntime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvnh.bak $(INCAdir)/atlas_cmvn.h
#
# This tester calls the driver routine that blocks
#
cmvntest : xmvnhgen cmvntest.o
	rm -f $(cmvntobj) ATL_cmvnk.c  ATL_cgemvN.o \
              xcmvntest
	- mv $(INCAdir)/atlas_cmvn.h mvnh.bak
	./xmvnhgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_cmvnk.c
	$(MAKE) $(cmvntobj) ATL_cgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xcmvntest cmvntest.o \
                   ATL_cgemvN.o $(cmvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvntest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(calphas) -b $(cbetas) 
	- mv mvnh.bak $(INCAdir)/atlas_cmvn.h

cmvnktest : xmvnhgen
	rm -f $(cmvntstobj) ATL_cmvnk.c cmvnktest.o \
              ATL_cgemvN.o xcmvnktest
	- mv $(INCAdir)/atlas_cmvn.h mvnh.bak
	./xmvnhgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_cmvnk.c
	$(MAKE) cmvnktest.o $(cmvntstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xcmvnktest cmvnktest.o \
                   $(cmvntstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvnktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvnh.bak $(INCAdir)/atlas_cmvn.h

cmvntime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX   $(mySRCdir)/mvktime.c
cmvtimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX   $(mySRCdir)/mvktime.c

cmvktimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX   -DTIME_KERNEL $(mySRCdir)/mvktime.c
cmvnktest.o : $(mySRCdir)/mvnktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DBETA$(beta)   $(mySRCdir)/mvnktest.c
cmvntest.o : $(mySRCdir)/mvntest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/mvntest.c

ATL_cgemvN.o : $(mvSRCdir)/ATL_gemvN.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DSCPLX $(mvSRCdir)/ATL_gemvN.c
#
# This target searches for best kernels and installs them
#
res/cMVTK.sum : xmvtksearch force_build
	./xmvtksearch -p c
	$(MAKE) cmvtinstall
#
# This target uses extract to generate a MVT kernel
#
cmvtext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvt_C nu=$(nu) type=SCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/cmvt_C.c
cmvtext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvt_sse nu=$(nu) type=SCPLX -def MU $(mu) \
                           $(genflags) -o EXTDIR/cmvt_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
cmvtktime : cmvktimeT.o 
	rm -f ATL_cmvtk.c ATL_cgemvT.o xcmvtktime
	cp -f $(MVTKdir)/$(mvtrout) ATL_cmvtk.c
	$(cMVCC) -c $(CDEFS) $(cMVFLAGS) -I$(MVTdir) -o ATL_cmvtk.o \
           -DSCPLX ATL_cmvtk.c
	$(CLINKER) $(CLINKFLAGS) -o xcmvtktime cmvktimeT.o \
                   ATL_cmvtk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvtktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
cmvttime : cmvttime.o xmvthgen
	- rm -f $(cmvttobj) ATL_cmvtk.c atlas_cmvt.h \
                mvth.bak ATL_cgemvT.o xcmvttime
	- mv $(INCAdir)/atlas_cmvt.h mvth.bak
	./xmvthgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_cmvtk.c
	$(MAKE) $(cmvttobj) ATL_cgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xcmvttime cmvttime.o \
                   ATL_cgemvT.o $(cmvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvttime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvth.bak $(INCAdir)/atlas_cmvt.h
#
# This tester calls the driver routine that blocks
#
cmvttest : xmvthgen cmvttest.o
	rm -f $(cmvttobj) ATL_cmvtk.c  ATL_cgemvT.o \
              xcmvttest
	- mv $(INCAdir)/atlas_cmvt.h mvth.bak
	./xmvthgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_cmvtk.c
	$(MAKE) $(cmvttobj) ATL_cgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xcmvttest cmvttest.o \
                   ATL_cgemvT.o $(cmvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvttest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(calphas) -b $(cbetas) 
	- mv mvth.bak $(INCAdir)/atlas_cmvt.h

cmvtktest : xmvthgen
	rm -f $(cmvttstobj) ATL_cmvtk.c cmvtktest.o \
              ATL_cgemvT.o xcmvtktest
	- mv $(INCAdir)/atlas_cmvt.h mvth.bak
	./xmvthgen -p c -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_cmvtk.c
	$(MAKE) cmvtktest.o $(cmvttstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xcmvtktest cmvtktest.o \
                   $(cmvttstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xcmvtktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvth.bak $(INCAdir)/atlas_cmvt.h

cmvttime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_TRANS_ $(mySRCdir)/mvktime.c
cmvtimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_TRANS_ $(mySRCdir)/mvktime.c

cmvktimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DATL_TRANS_ -DTIME_KERNEL $(mySRCdir)/mvktime.c
cmvtktest.o : $(mySRCdir)/mvtktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX -DBETA$(beta) -DATL_TRANS_ $(mySRCdir)/mvtktest.c
cmvttest.o : $(mySRCdir)/mvttest.c
	$(KC) -c $(KCFLAGS) -o $@ -DSCPLX $(mySRCdir)/mvttest.c

ATL_cgemvT.o : $(mvSRCdir)/ATL_gemvT.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DSCPLX $(mvSRCdir)/ATL_gemvT.c

ATL_cmvtk_smallN.o : $(mvSRCdir)/ATL_mvtk_smallN.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(mvSRCdir)/ATL_mvtk_smallN.c
ATL_cmvtk_Mlt16.o : $(mvSRCdir)/ATL_mvtk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(mvSRCdir)/ATL_mvtk_Mlt16.c
ATL_cmvtk_b0.o : ATL_cmvtk.c
	$(cMVCC) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSCPLX \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVTK_b0 -o ATL_cmvtk_b0.o \
                 -c ATL_cmvtk.c
ATL_cmvtk_b1.o : ATL_cmvtk.c
	$(cMVCC) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSCPLX \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVTK -o ATL_cmvtk_b1.o \
                 -c ATL_cmvtk.c
ATL_cmvtk.o : ATL_cmvtk.c
	$(cMVCC) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSCPLX \
                 -DBETA$(beta) -o ATL_cmvtk.o -c ATL_cmvtk.c

ATL_cmvnk_smallN.o : $(mvSRCdir)/ATL_mvnk_smallN.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(mvSRCdir)/ATL_mvnk_smallN.c
ATL_cmvnk_Mlt16.o : $(mvSRCdir)/ATL_mvnk_Mlt16.c
	$(SKC) -c $(SKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DSCPLX $(mvSRCdir)/ATL_mvnk_Mlt16.c
ATL_cmvnk_b0.o : ATL_cmvnk.c
	$(cMVCC) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSCPLX \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVNK_b0 -o ATL_cmvnk_b0.o \
                 -c ATL_cmvnk.c
ATL_cmvnk_b1.o : ATL_cmvnk.c
	$(cMVCC) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSCPLX \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVNK -o ATL_cmvnk_b1.o \
                 -c ATL_cmvnk.c
ATL_cmvnk.o : ATL_cmvnk.c
	$(cMVCC) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -DSCPLX \
                 -DBETA$(beta) -o ATL_cmvnk.o -c ATL_cmvnk.c

ATL_cgemv.o : ATL_gemv.c atlas_cmvn.h atlas_cmvt.h
	$(CKC) -c -I$(MVTdir) $(cKCFLAGS) -o $@ -DSCPLX ATL_gemv.c
ATL_cgemvN_b0.o : ATL_cgemvN.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETA0 -DALPHA1 ATL_cgemvN.c
ATL_cgemvN_b1.o : ATL_cgemvN.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETA1 -DALPHA1 ATL_cgemvN.c
ATL_cgemvN_bX.o : ATL_cgemvN.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETAX -DALPHA1 ATL_cgemvN.c
ATL_cgemvN_bXi0.o : ATL_cgemvN.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETAXI0 -DALPHA1 ATL_cgemvN.c
ATL_cgemvT_b0.o : ATL_cgemvT.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETA0 -DALPHA1 ATL_cgemvT.c
ATL_cgemvT_b1.o : ATL_cgemvT.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETA1 -DALPHA1 ATL_cgemvT.c
ATL_cgemvT_bX.o : ATL_cgemvT.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETAX -DALPHA1 ATL_cgemvT.c
ATL_cgemvT_bXi0.o : ATL_cgemvT.c
	$(cMVCC) -c -I$(MVTdir) $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DSCPLX -DBETAXI0 -DALPHA1 ATL_cgemvT.c
ATL_cgemvC_b0.o : ATL_cgemvT.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETA0 -DALPHA1 -DConj_ ATL_cgemvT.c
ATL_cgemvC_b1.o : ATL_cgemvT.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETA1 -DALPHA1 -DConj_ ATL_cgemvT.c
ATL_cgemvC_bX.o : ATL_cgemvT.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETAX -DALPHA1 -DConj_ ATL_cgemvT.c
ATL_cgemvC_bXi0.o : ATL_cgemvT.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETAXI0 -DALPHA1 -DConj_ ATL_cgemvT.c
ATL_cgemvNc_b0.o : ATL_cgemvN.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETA0 -DALPHA1 -DConj_ ATL_cgemvN.c
ATL_cgemvNc_b1.o : ATL_cgemvN.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETA1 -DALPHA1 -DConj_ ATL_cgemvN.c
ATL_cgemvNc_bX.o : ATL_cgemvN.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETAX -DALPHA1 -DConj_ ATL_cgemvN.c
ATL_cgemvNc_bXi0.o : ATL_cgemvN.c
	$(cMVCC) -c $(cMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DSCPLX -DBETAXI0 -DALPHA1 -DConj_ ATL_cgemvN.c

cmvtoutd :
	mkdir cmvtoutd
cmvtinstall : xccobj cmvtoutd xmvthgen force_build
	rm -f cmvtoutd/*
	if [ -s "$(GMVdir)/Make_cmvt" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_cmvt killall ; \
           rm -f $(GMVdir)/Make_cmvt ; \
        fi
	./xmvthgen -p c -F res/cMVTK.sum -d cmvtoutd
	cp -f cmvtoutd/Make_cmvt $(GMVdir)/.
	cp cmvtoutd/*.c $(GMVdir)/.
	cp -f cmvtoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f cmvtoutd/* ; rmdir cmvtoutd
cmvnoutd :
	mkdir cmvnoutd
cmvninstall : xccobj cmvnoutd xmvnhgen force_build
	rm -f cmvnoutd/*
	if [ -s "$(GMVdir)/Make_cmvn" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_cmvn killall ; \
           rm -f $(GMVdir)/Make_cmvn ; \
        fi
	./xmvnhgen -p c -F res/cMVNK.sum -d cmvnoutd
	cp -f cmvnoutd/Make_cmvn $(GMVdir)/.
	cp cmvnoutd/*.c $(GMVdir)/.
	cp -f cmvnoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f cmvnoutd/* ; rmdir cmvnoutd
cl1lib :
	cd $(BINdir) ; $(MAKE) cl1lib
ctstlib :
	cd $(BINdir) ; $(MAKE) ctstlib
zobj = ATL_zgemv.o \
       ATL_zgemvN_b0.o ATL_zgemvT_b0.o \
       ATL_zgemvN_b1.o ATL_zgemvT_b1.o \
       ATL_zgemvN_bXi0.o ATL_zgemvT_bXi0.o \
       ATL_zgemvN_bX.o ATL_zgemvT_bX.o

ztstNobj = ATL_zgemvN_b0.o ATL_zgemvNc_b0.o \
           ATL_zgemvN_b1.o ATL_zgemvNc_b1.o \
           ATL_zgemvN_bXi0.o ATL_zgemvNc_bXi0.o \
           ATL_zgemvN_bX.o ATL_zgemvNc_bX.o

ztstTobj = ATL_zgemvT_b0.o ATL_zgemvC_b0.o \
           ATL_zgemvT_b1.o ATL_zgemvC_b1.o \
           ATL_zgemvT_bXi0.o ATL_zgemvC_bXi0.o \
           ATL_zgemvT_bX.o ATL_zgemvC_bX.o
ztstobj = $(ztstNobj) $(ztstTobj)
zmvttstobj = ATL_zmvtk.o ATL_zmvtk_smallN.o ATL_zmvtk_Mlt16.o
zmvttobj = ATL_zmvtk_b0.o ATL_zmvtk_b1.o \
           ATL_zmvtk_smallN.o ATL_zmvtk_Mlt16.o
zmvntstobj = ATL_zmvnk.o ATL_zmvnk_smallN.o ATL_zmvnk_Mlt16.o
zmvntobj = ATL_zmvnk_b0.o ATL_zmvnk_b1.o \
           ATL_zmvnk_smallN.o ATL_zmvnk_Mlt16.o

#
# This target searches for best kernels and installs them
#
res/zMVNK.sum : xmvnksearch force_build
	./xmvnksearch -p z
	$(MAKE) zmvninstall
#
# This target uses extract to generate a MVN kernel
#
zmvnext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvn_C nu=$(nu) type=DCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/zmvn_C.c
zmvnext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvn_sse nu=$(nu) type=DCPLX -def MU $(mu) \
                           $(genflags) -o EXTDIR/zmvn_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
zmvnktime : zmvktimeN.o 
	rm -f ATL_zmvnk.c ATL_zgemvN.o xzmvnktime
	cp -f $(MVNKdir)/$(mvnrout) ATL_zmvnk.c
	$(zMVCC) -c $(CDEFS) $(zMVFLAGS) -I$(MVTdir) -o ATL_zmvnk.o \
           -DDCPLX ATL_zmvnk.c
	$(CLINKER) $(CLINKFLAGS) -o xzmvnktime zmvktimeN.o \
                   ATL_zmvnk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvnktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
zmvntime : zmvntime.o xmvnhgen
	- rm -f $(zmvntobj) ATL_zmvnk.c atlas_zmvn.h \
                mvnh.bak ATL_zgemvN.o xzmvntime
	- mv $(INCAdir)/atlas_zmvn.h mvnh.bak
	./xmvnhgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_zmvnk.c
	$(MAKE) $(zmvntobj) ATL_zgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xzmvntime zmvntime.o \
                   ATL_zgemvN.o $(zmvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvntime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvnh.bak $(INCAdir)/atlas_zmvn.h
#
# This tester calls the driver routine that blocks
#
zmvntest : xmvnhgen zmvntest.o
	rm -f $(zmvntobj) ATL_zmvnk.c  ATL_zgemvN.o \
              xzmvntest
	- mv $(INCAdir)/atlas_zmvn.h mvnh.bak
	./xmvnhgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_zmvnk.c
	$(MAKE) $(zmvntobj) ATL_zgemvN.o
	$(CLINKER) $(CLINKFLAGS) -o xzmvntest zmvntest.o \
                   ATL_zgemvN.o $(zmvntobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvntest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(zalphas) -b $(zbetas) 
	- mv mvnh.bak $(INCAdir)/atlas_zmvn.h

zmvnktest : xmvnhgen
	rm -f $(zmvntstobj) ATL_zmvnk.c zmvnktest.o \
              ATL_zgemvN.o xzmvnktest
	- mv $(INCAdir)/atlas_zmvn.h mvnh.bak
	./xmvnhgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVNKdir)/$(mvnrout) ATL_zmvnk.c
	$(MAKE) zmvnktest.o $(zmvntstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xzmvnktest zmvnktest.o \
                   $(zmvntstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvnktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvnh.bak $(INCAdir)/atlas_zmvn.h

zmvntime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX   $(mySRCdir)/mvktime.c
zmvtimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX   $(mySRCdir)/mvktime.c

zmvktimeN.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX   -DTIME_KERNEL $(mySRCdir)/mvktime.c
zmvnktest.o : $(mySRCdir)/mvnktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DBETA$(beta)   $(mySRCdir)/mvnktest.c
zmvntest.o : $(mySRCdir)/mvntest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/mvntest.c

ATL_zgemvN.o : $(mvSRCdir)/ATL_gemvN.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DDCPLX $(mvSRCdir)/ATL_gemvN.c
#
# This target searches for best kernels and installs them
#
res/zMVTK.sum : xmvtksearch force_build
	./xmvtksearch -p z
	$(MAKE) zmvtinstall
#
# This target uses extract to generate a MVT kernel
#
zmvtext_C : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base -langC \
                           order=$(order) rout=mvt_C nu=$(nu) type=DCPLX \
                           -def MU $(mu) $(genflags) -o EXTDIR/zmvt_C.c
zmvtext_sse : $(BINdir)/xextract EXTDIR
	$(BINdir)/xextract -b $(mySRCdir)/atlas-l2g.base order=$(order) \
                           rout=mvt_sse nu=$(nu) type=DCPLX -def MU $(mu) \
                           $(genflags) -o EXTDIR/zmvt_sse.c
#
# This target calls the kernel directly, with the only cache flush coming 
# from the size of the operands themselves
#
zmvtktime : zmvktimeT.o 
	rm -f ATL_zmvtk.c ATL_zgemvT.o xzmvtktime
	cp -f $(MVTKdir)/$(mvtrout) ATL_zmvtk.c
	$(zMVCC) -c $(CDEFS) $(zMVFLAGS) -I$(MVTdir) -o ATL_zmvtk.o \
           -DDCPLX ATL_zmvtk.c
	$(CLINKER) $(CLINKFLAGS) -o xzmvtktime zmvktimeT.o \
                   ATL_zmvtk.o \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvtktime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
#
# This target calls the driver (which blocks), with cache flushing
#
zmvttime : zmvttime.o xmvthgen
	- rm -f $(zmvttobj) ATL_zmvtk.c atlas_zmvt.h \
                mvth.bak ATL_zgemvT.o xzmvttime
	- mv $(INCAdir)/atlas_zmvt.h mvth.bak
	./xmvthgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_zmvtk.c
	$(MAKE) $(zmvttobj) ATL_zgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xzmvttime zmvttime.o \
                   ATL_zgemvT.o $(zmvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvttime -m $(M) -n $(N) -l $(lda) \
                  -C $(flushKB) $(align) $(tflags)
	- mv mvth.bak $(INCAdir)/atlas_zmvt.h
#
# This tester calls the driver routine that blocks
#
zmvttest : xmvthgen zmvttest.o
	rm -f $(zmvttobj) ATL_zmvtk.c  ATL_zgemvT.o \
              xzmvttest
	- mv $(INCAdir)/atlas_zmvt.h mvth.bak
	./xmvthgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_zmvtk.c
	$(MAKE) $(zmvttobj) ATL_zgemvT.o
	$(CLINKER) $(CLINKFLAGS) -o xzmvttest zmvttest.o \
                   ATL_zgemvT.o $(zmvttobj) \
                   $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvttest -m 1 $(Mt) -n 1 $(Nt) $(incs) \
                  -l 1 $(ldat) $(align) -a $(zalphas) -b $(zbetas) 
	- mv mvth.bak $(INCAdir)/atlas_zmvt.h

zmvtktest : xmvthgen
	rm -f $(zmvttstobj) ATL_zmvtk.c zmvtktest.o \
              ATL_zgemvT.o xzmvtktest
	- mv $(INCAdir)/atlas_zmvt.h mvth.bak
	./xmvthgen -p z -l $(l1mul) -m $(mu) -n $(nu) -f $(iflag) $(ntlb)
	cp -f $(MVTKdir)/$(mvtrout) ATL_zmvtk.c
	$(MAKE) zmvtktest.o $(zmvttstobj) beta=$(beta)
	$(CLINKER) $(CLINKFLAGS) -o xzmvtktest zmvtktest.o \
                   $(zmvttstobj) $(TESTlib) $(ATLASlib)
	$(ATLRUN) $(MVTdir) xzmvtktest -m 1 $(Mt) -n 1 $(Nt) \
                  -l 1 $(ldat) $(align)
	- mv mvth.bak $(INCAdir)/atlas_zmvt.h

zmvttime.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_TRANS_ $(mySRCdir)/mvktime.c
zmvtimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_TRANS_ $(mySRCdir)/mvktime.c

zmvktimeT.o : $(mySRCdir)/mvktime.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DATL_TRANS_ -DTIME_KERNEL $(mySRCdir)/mvktime.c
zmvtktest.o : $(mySRCdir)/mvtktest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX -DBETA$(beta) -DATL_TRANS_ $(mySRCdir)/mvtktest.c
zmvttest.o : $(mySRCdir)/mvttest.c
	$(KC) -c $(KCFLAGS) -o $@ -DDCPLX $(mySRCdir)/mvttest.c

ATL_zgemvT.o : $(mvSRCdir)/ATL_gemvT.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) \
                       -o $@ -DDCPLX $(mvSRCdir)/ATL_gemvT.c

ATL_zmvtk_smallN.o : $(mvSRCdir)/ATL_mvtk_smallN.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(mvSRCdir)/ATL_mvtk_smallN.c
ATL_zmvtk_Mlt16.o : $(mvSRCdir)/ATL_mvtk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(mvSRCdir)/ATL_mvtk_Mlt16.c
ATL_zmvtk_b0.o : ATL_zmvtk.c
	$(zMVCC) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDCPLX \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVTK_b0 -o ATL_zmvtk_b0.o \
                 -c ATL_zmvtk.c
ATL_zmvtk_b1.o : ATL_zmvtk.c
	$(zMVCC) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDCPLX \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVTK -o ATL_zmvtk_b1.o \
                 -c ATL_zmvtk.c
ATL_zmvtk.o : ATL_zmvtk.c
	$(zMVCC) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDCPLX \
                 -DBETA$(beta) -o ATL_zmvtk.o -c ATL_zmvtk.c

ATL_zmvnk_smallN.o : $(mvSRCdir)/ATL_mvnk_smallN.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(mvSRCdir)/ATL_mvnk_smallN.c
ATL_zmvnk_Mlt16.o : $(mvSRCdir)/ATL_mvnk_Mlt16.c
	$(DKC) -c $(DKCFLAGS) $(CDEFS) -I$(MVTdir) -DTUNING=1 \
                       -o $@ -DDCPLX $(mvSRCdir)/ATL_mvnk_Mlt16.c
ATL_zmvnk_b0.o : ATL_zmvnk.c
	$(zMVCC) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDCPLX \
                 -DBETA0 -DATL_UGEMV=ATL_UGEMVNK_b0 -o ATL_zmvnk_b0.o \
                 -c ATL_zmvnk.c
ATL_zmvnk_b1.o : ATL_zmvnk.c
	$(zMVCC) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDCPLX \
                 -DBETA1 -DATL_UGEMV=ATL_UGEMVNK -o ATL_zmvnk_b1.o \
                 -c ATL_zmvnk.c
ATL_zmvnk.o : ATL_zmvnk.c
	$(zMVCC) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -DDCPLX \
                 -DBETA$(beta) -o ATL_zmvnk.o -c ATL_zmvnk.c

ATL_zgemv.o : ATL_gemv.c atlas_zmvn.h atlas_zmvt.h
	$(ZKC) -c -I$(MVTdir) $(zKCFLAGS) -o $@ -DDCPLX ATL_gemv.c
ATL_zgemvN_b0.o : ATL_zgemvN.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETA0 -DALPHA1 ATL_zgemvN.c
ATL_zgemvN_b1.o : ATL_zgemvN.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETA1 -DALPHA1 ATL_zgemvN.c
ATL_zgemvN_bX.o : ATL_zgemvN.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETAX -DALPHA1 ATL_zgemvN.c
ATL_zgemvN_bXi0.o : ATL_zgemvN.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETAXI0 -DALPHA1 ATL_zgemvN.c
ATL_zgemvT_b0.o : ATL_zgemvT.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETA0 -DALPHA1 ATL_zgemvT.c
ATL_zgemvT_b1.o : ATL_zgemvT.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETA1 -DALPHA1 ATL_zgemvT.c
ATL_zgemvT_bX.o : ATL_zgemvT.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETAX -DALPHA1 ATL_zgemvT.c
ATL_zgemvT_bXi0.o : ATL_zgemvT.c
	$(zMVCC) -c -I$(MVTdir) $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -o $@ \
              -DDCPLX -DBETAXI0 -DALPHA1 ATL_zgemvT.c
ATL_zgemvC_b0.o : ATL_zgemvT.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETA0 -DALPHA1 -DConj_ ATL_zgemvT.c
ATL_zgemvC_b1.o : ATL_zgemvT.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETA1 -DALPHA1 -DConj_ ATL_zgemvT.c
ATL_zgemvC_bX.o : ATL_zgemvT.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETAX -DALPHA1 -DConj_ ATL_zgemvT.c
ATL_zgemvC_bXi0.o : ATL_zgemvT.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETAXI0 -DALPHA1 -DConj_ ATL_zgemvT.c
ATL_zgemvNc_b0.o : ATL_zgemvN.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETA0 -DALPHA1 -DConj_ ATL_zgemvN.c
ATL_zgemvNc_b1.o : ATL_zgemvN.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETA1 -DALPHA1 -DConj_ ATL_zgemvN.c
ATL_zgemvNc_bX.o : ATL_zgemvN.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETAX -DALPHA1 -DConj_ ATL_zgemvN.c
ATL_zgemvNc_bXi0.o : ATL_zgemvN.c
	$(zMVCC) -c $(zMVFLAGS) $(CDEFS) $(EXTFLAGS) -I$(MVTdir) -o $@ \
              -DDCPLX -DBETAXI0 -DALPHA1 -DConj_ ATL_zgemvN.c

zmvtoutd :
	mkdir zmvtoutd
zmvtinstall : xccobj zmvtoutd xmvthgen force_build
	rm -f zmvtoutd/*
	if [ -s "$(GMVdir)/Make_zmvt" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_zmvt killall ; \
           rm -f $(GMVdir)/Make_zmvt ; \
        fi
	./xmvthgen -p z -F res/zMVTK.sum -d zmvtoutd
	cp -f zmvtoutd/Make_zmvt $(GMVdir)/.
	cp zmvtoutd/*.c $(GMVdir)/.
	cp -f zmvtoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f zmvtoutd/* ; rmdir zmvtoutd
zmvnoutd :
	mkdir zmvnoutd
zmvninstall : xccobj zmvnoutd xmvnhgen force_build
	rm -f zmvnoutd/*
	if [ -s "$(GMVdir)/Make_zmvn" ]; then \
           cd  $(GMVdir) ; $(MAKE) -f Make_zmvn killall ; \
           rm -f $(GMVdir)/Make_zmvn ; \
        fi
	./xmvnhgen -p z -F res/zMVNK.sum -d zmvnoutd
	cp -f zmvnoutd/Make_zmvn $(GMVdir)/.
	cp zmvnoutd/*.c $(GMVdir)/.
	cp -f zmvnoutd/*.h $(INCAdir)/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMVdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMVdir)/. ; \
        fi
	touch $(INCSdir)/atlas_mv.h
	rm -f zmvnoutd/* ; rmdir zmvnoutd
zl1lib :
	cd $(BINdir) ; $(MAKE) zl1lib
ztstlib :
	cd $(BINdir) ; $(MAKE) ztstlib
$(BINdir)/xccobj :
	cd $(BINdir) ; $(MAKE) xccobj
xccobj : $(BINdir)/xccobj
	-@ cp -f $(BINdir)/xccobj .
	-@ cp -f $(BINdir)/xccobj.exe .
$(BINdir)/xsubstr:
	cd $(BINdir) ; $(MAKE) xsubstr
EXTDIR :
	mkdir EXTDIR
