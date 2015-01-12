include Make.inc
mySRCdir = $(SRCdir)/interfaces/blas/C/testing

MYDIR = $(BLDdir)/interfaces/blas/C/testing/
sanity_test :
	rm -f sanity.out
	$(MAKE) ssanity_test dsanity_test csanity_test zsanity_test
ptsanity_test :
	rm -f ptsanity.out
	$(MAKE) sptsanity_test dptsanity_test cptsanity_test zptsanity_test

all : lvl1 lvl2 lvl3
lvl3 : dlvl3 slvl3 zlvl3 clvl3
lvl2 : dlvl2 slvl2 zlvl2 clvl2
lvl1 : dlvl1 slvl1 zlvl1 clvl1


ssanity_test : xscblat1 xscblat2 xscblat3
	$(ATLRUN) $(MYDIR) xscblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xscblat2 < $(mySRCdir)/c_sblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xscblat3 < $(mySRCdir)/c_sblat3.dat \
                  >> $(MYDIR)/sanity.out
slvl1 : xscblat1 xsaxpbytst xssettst
slvl2 : xscblat2
slvl3 : xscblat3
dsanity_test : xdcblat1 xdcblat2 xdcblat3
	$(ATLRUN) $(MYDIR) xdcblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xdcblat2 < $(mySRCdir)/c_dblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xdcblat3 < $(mySRCdir)/c_dblat3.dat \
                  >> $(MYDIR)/sanity.out
dlvl1 : xdcblat1 xdaxpbytst xdsettst
dlvl2 : xdcblat2
dlvl3 : xdcblat3
csanity_test : xccblat1 xccblat2 xccblat3
	$(ATLRUN) $(MYDIR) xccblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xccblat2 < $(mySRCdir)/c_cblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xccblat3 < $(mySRCdir)/c_cblat3.dat \
                  >> $(MYDIR)/sanity.out
clvl1 : xccblat1 xcaxpbytst xcsettst
clvl2 : xccblat2
clvl3 : xccblat3
zsanity_test : xzcblat1 xzcblat2 xzcblat3
	$(ATLRUN) $(MYDIR) xzcblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xzcblat2 < $(mySRCdir)/c_zblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xzcblat3 < $(mySRCdir)/c_zblat3.dat \
                  >> $(MYDIR)/sanity.out
zlvl1 : xzcblat1 xzaxpbytst xzsettst
zlvl2 : xzcblat2
zlvl3 : xzcblat3
xscblat1 : c_sblat1.o c_sblas1.o silvl1 \
           auxiliary.o sl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_sblat1.o c_sblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xscblat2 : c_sblat2.o c_sblas2.o silvl2 \
           auxiliary.o sl2lib c_s2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_sblat2.o c_sblas2.o \
                   c_s2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xscblat3 : c_sblat3.o c_sblas3.o silvl3 \
           auxiliary.o sl3lib c_s3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_sblat3.o c_sblas3.o \
                   c_s3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xssettst : ssettst.o silvl1 sl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ ssettst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xsaxpbytst : saxpbytst.o silvl1 sl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ saxpbytst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xdcblat1 : c_dblat1.o c_dblas1.o dilvl1 \
           auxiliary.o dl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_dblat1.o c_dblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdcblat2 : c_dblat2.o c_dblas2.o dilvl2 \
           auxiliary.o dl2lib c_d2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_dblat2.o c_dblas2.o \
                   c_d2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdcblat3 : c_dblat3.o c_dblas3.o dilvl3 \
           auxiliary.o dl3lib c_d3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_dblat3.o c_dblas3.o \
                   c_d3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdsettst : dsettst.o dilvl1 dl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ dsettst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xdaxpbytst : daxpbytst.o dilvl1 dl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ daxpbytst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xccblat1 : c_cblat1.o c_cblas1.o cilvl1 \
           auxiliary.o cl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_cblat1.o c_cblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xccblat2 : c_cblat2.o c_cblas2.o cilvl2 \
           auxiliary.o cl2lib c_c2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_cblat2.o c_cblas2.o \
                   c_c2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xccblat3 : c_cblat3.o c_cblas3.o cilvl3 \
           auxiliary.o cl3lib c_c3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_cblat3.o c_cblas3.o \
                   c_c3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcsettst : csettst.o cilvl1 cl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ csettst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xcaxpbytst : caxpbytst.o cilvl1 cl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ caxpbytst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xzcblat1 : c_zblat1.o c_zblas1.o zilvl1 \
           auxiliary.o zl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_zblat1.o c_zblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzcblat2 : c_zblat2.o c_zblas2.o zilvl2 \
           auxiliary.o zl2lib c_z2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_zblat2.o c_zblas2.o \
                   c_z2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzcblat3 : c_zblat3.o c_zblas3.o zilvl3 \
           auxiliary.o zl3lib c_z3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_zblat3.o c_zblas3.o \
                   c_z3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzsettst : zsettst.o zilvl1 zl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ zsettst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)
xzaxpbytst : zaxpbytst.o zilvl1 zl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ zaxpbytst.o \
           $(TESTlib) $(CBLASlib) $(ATLASlib) $(LIBS)

ptall : ptlvl1 ptlvl2 ptlvl3
ptlvl3 : dptlvl3 sptlvl3 zptlvl3 cptlvl3
ptlvl2 : dptlvl2 sptlvl2 zptlvl2 cptlvl2
ptlvl1 : dptlvl1 sptlvl1 zptlvl1 cptlvl1


sptsanity_test : xsptcblat1 xsptcblat2 xsptcblat3
	$(ATLRUN) $(MYDIR) xsptcblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xsptcblat2 < $(mySRCdir)/c_sblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xsptcblat3 < $(mySRCdir)/c_sblat3.dat \
                  >> $(MYDIR)/ptsanity.out
sptlvl1 : xsptcblat1 xsptaxpbytst xsptsettst
sptlvl2 : xsptcblat2
sptlvl3 : xsptcblat3
dptsanity_test : xdptcblat1 xdptcblat2 xdptcblat3
	$(ATLRUN) $(MYDIR) xdptcblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xdptcblat2 < $(mySRCdir)/c_dblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xdptcblat3 < $(mySRCdir)/c_dblat3.dat \
                  >> $(MYDIR)/ptsanity.out
dptlvl1 : xdptcblat1 xdptaxpbytst xdptsettst
dptlvl2 : xdptcblat2
dptlvl3 : xdptcblat3
cptsanity_test : xcptcblat1 xcptcblat2 xcptcblat3
	$(ATLRUN) $(MYDIR) xcptcblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xcptcblat2 < $(mySRCdir)/c_cblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xcptcblat3 < $(mySRCdir)/c_cblat3.dat \
                  >> $(MYDIR)/ptsanity.out
cptlvl1 : xcptcblat1 xcptaxpbytst xcptsettst
cptlvl2 : xcptcblat2
cptlvl3 : xcptcblat3
zptsanity_test : xzptcblat1 xzptcblat2 xzptcblat3
	$(ATLRUN) $(MYDIR) xzptcblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xzptcblat2 < $(mySRCdir)/c_zblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xzptcblat3 < $(mySRCdir)/c_zblat3.dat \
                  >> $(MYDIR)/ptsanity.out
zptlvl1 : xzptcblat1 xzptaxpbytst xzptsettst
zptlvl2 : xzptcblat2
zptlvl3 : xzptcblat3
xsptcblat1 : c_sblat1.o c_sblas1.o sptilvl1 \
           auxiliary.o sl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_sblat1.o c_sblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsptcblat2 : c_sblat2.o c_sblas2.o sptilvl2 \
           auxiliary.o sl2lib c_s2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_sblat2.o c_sblas2.o \
                   c_s2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsptcblat3 : c_sblat3.o c_sblas3.o sptilvl3 \
           auxiliary.o sl3lib c_s3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_sblat3.o c_sblas3.o \
                   c_s3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsptsettst : ssettst.o sptilvl1 sl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ ssettst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xsptaxpbytst : saxpbytst.o sptilvl1 sl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ saxpbytst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xdptcblat1 : c_dblat1.o c_dblas1.o dptilvl1 \
           auxiliary.o dl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_dblat1.o c_dblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdptcblat2 : c_dblat2.o c_dblas2.o dptilvl2 \
           auxiliary.o dl2lib c_d2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_dblat2.o c_dblas2.o \
                   c_d2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdptcblat3 : c_dblat3.o c_dblas3.o dptilvl3 \
           auxiliary.o dl3lib c_d3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_dblat3.o c_dblas3.o \
                   c_d3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdptsettst : dsettst.o dptilvl1 dl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ dsettst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xdptaxpbytst : daxpbytst.o dptilvl1 dl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ daxpbytst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xcptcblat1 : c_cblat1.o c_cblas1.o cptilvl1 \
           auxiliary.o cl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_cblat1.o c_cblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcptcblat2 : c_cblat2.o c_cblas2.o cptilvl2 \
           auxiliary.o cl2lib c_c2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_cblat2.o c_cblas2.o \
                   c_c2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcptcblat3 : c_cblat3.o c_cblas3.o cptilvl3 \
           auxiliary.o cl3lib c_c3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_cblat3.o c_cblas3.o \
                   c_c3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcptsettst : csettst.o cptilvl1 cl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ csettst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xcptaxpbytst : caxpbytst.o cptilvl1 cl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ caxpbytst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xzptcblat1 : c_zblat1.o c_zblas1.o zptilvl1 \
           auxiliary.o zl1lib  
	$(FLINKER) $(FLINKFLAGS) -o $@ c_zblat1.o c_zblas1.o \
                     auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzptcblat2 : c_zblat2.o c_zblas2.o zptilvl2 \
           auxiliary.o zl2lib c_z2chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_zblat2.o c_zblas2.o \
                   c_z2chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzptcblat3 : c_zblat3.o c_zblas3.o zptilvl3 \
           auxiliary.o zl3lib c_z3chke.o c_xerbla.o
	$(FLINKER) $(FLINKFLAGS) -o $@ c_zblat3.o c_zblas3.o \
                   c_z3chke.o c_xerbla.o auxiliary.o $(LAPACKlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzptsettst : zsettst.o zptilvl1 zl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ zsettst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)
xzptaxpbytst : zaxpbytst.o zptilvl1 zl1lib
	$(CLINKER) $(CLINKFLAGS) -o $@ zaxpbytst.o \
           $(TESTlib) $(PTCBLASlib) $(ATLASlib) $(LIBS)

auxlib :
	cd $(AUXdir) ; $(MAKE) lib
sptilvl3 :
	cd $(CBLdir) ; $(MAKE) sptlvl3
sptilvl2 :
	cd $(CBLdir) ; $(MAKE) sptlvl2
sptilvl1 :
	cd $(CBLdir) ; $(MAKE) sptlvl1
silvl3 :
	cd $(CBLdir) ; $(MAKE) slvl3
silvl2 :
	cd $(CBLdir) ; $(MAKE) slvl2
silvl1 :
	cd $(CBLdir) ; $(MAKE) slvl1
sl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) slvl1
sl2lib : sl1lib auxlib
	cd $(GMVdir) ; $(MAKE) slib
	cd $(GR1dir) ; $(MAKE) slib
	cd $(L2Bdir) ; $(MAKE) slib
sl3lib :
	cd $(GMMdir) ; $(MAKE) slib
	cd $(L3Bdir) ; $(MAKE) slib
dptilvl3 :
	cd $(CBLdir) ; $(MAKE) dptlvl3
dptilvl2 :
	cd $(CBLdir) ; $(MAKE) dptlvl2
dptilvl1 :
	cd $(CBLdir) ; $(MAKE) dptlvl1
dilvl3 :
	cd $(CBLdir) ; $(MAKE) dlvl3
dilvl2 :
	cd $(CBLdir) ; $(MAKE) dlvl2
dilvl1 :
	cd $(CBLdir) ; $(MAKE) dlvl1
dl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) dlvl1
dl2lib : dl1lib auxlib
	cd $(GMVdir) ; $(MAKE) dlib
	cd $(GR1dir) ; $(MAKE) dlib
	cd $(L2Bdir) ; $(MAKE) dlib
dl3lib :
	cd $(GMMdir) ; $(MAKE) dlib
	cd $(L3Bdir) ; $(MAKE) dlib
cptilvl3 :
	cd $(CBLdir) ; $(MAKE) cptlvl3
cptilvl2 :
	cd $(CBLdir) ; $(MAKE) cptlvl2
cptilvl1 :
	cd $(CBLdir) ; $(MAKE) cptlvl1
cilvl3 :
	cd $(CBLdir) ; $(MAKE) clvl3
cilvl2 :
	cd $(CBLdir) ; $(MAKE) clvl2
cilvl1 :
	cd $(CBLdir) ; $(MAKE) clvl1
cl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) clvl1
cl2lib : cl1lib auxlib
	cd $(GMVdir) ; $(MAKE) clib
	cd $(GR1dir) ; $(MAKE) clib
	cd $(L2Bdir) ; $(MAKE) clib
cl3lib :
	cd $(GMMdir) ; $(MAKE) clib
	cd $(L3Bdir) ; $(MAKE) clib
zptilvl3 :
	cd $(CBLdir) ; $(MAKE) zptlvl3
zptilvl2 :
	cd $(CBLdir) ; $(MAKE) zptlvl2
zptilvl1 :
	cd $(CBLdir) ; $(MAKE) zptlvl1
zilvl3 :
	cd $(CBLdir) ; $(MAKE) zlvl3
zilvl2 :
	cd $(CBLdir) ; $(MAKE) zlvl2
zilvl1 :
	cd $(CBLdir) ; $(MAKE) zlvl1
zl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) zlvl1
zl2lib : zl1lib auxlib
	cd $(GMVdir) ; $(MAKE) zlib
	cd $(GR1dir) ; $(MAKE) zlib
	cd $(L2Bdir) ; $(MAKE) zlib
zl3lib :
	cd $(GMMdir) ; $(MAKE) zlib
	cd $(L3Bdir) ; $(MAKE) zlib


ssettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/settst.c
saxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DSREAL -o $@ -c $(mySRCdir)/axpbytst.c
c_sblat3.o : $(mySRCdir)/c_sblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_sblat3.f
c_sblat2.o : $(mySRCdir)/c_sblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_sblat2.f
c_sblat1.o : $(mySRCdir)/c_sblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_sblat1.f
dsettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/settst.c
daxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DDREAL -o $@ -c $(mySRCdir)/axpbytst.c
c_dblat3.o : $(mySRCdir)/c_dblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_dblat3.f
c_dblat2.o : $(mySRCdir)/c_dblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_dblat2.f
c_dblat1.o : $(mySRCdir)/c_dblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_dblat1.f
csettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/settst.c
caxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DSCPLX -o $@ -c $(mySRCdir)/axpbytst.c
c_cblat3.o : $(mySRCdir)/c_cblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_cblat3.f
c_cblat2.o : $(mySRCdir)/c_cblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_cblat2.f
c_cblat1.o : $(mySRCdir)/c_cblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_cblat1.f
zsettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/settst.c
zaxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DDCPLX -o $@ -c $(mySRCdir)/axpbytst.c
c_zblat3.o : $(mySRCdir)/c_zblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_zblat3.f
c_zblat2.o : $(mySRCdir)/c_zblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_zblat2.f
c_zblat1.o : $(mySRCdir)/c_zblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/c_zblat1.f

c_sblas3.o : $(mySRCdir)/c_sblas3.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_sblas3.c
c_s3chke.o : $(mySRCdir)/c_s3chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_s3chke.c
c_sblas2.o : $(mySRCdir)/c_sblas2.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_sblas2.c
c_s2chke.o : $(mySRCdir)/c_s2chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_s2chke.c
c_sblas1.o : $(mySRCdir)/c_sblas1.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_sblas1.c
c_s1chke.o : $(mySRCdir)/c_s1chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_s1chke.c
c_dblas3.o : $(mySRCdir)/c_dblas3.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_dblas3.c
c_d3chke.o : $(mySRCdir)/c_d3chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_d3chke.c
c_dblas2.o : $(mySRCdir)/c_dblas2.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_dblas2.c
c_d2chke.o : $(mySRCdir)/c_d2chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_d2chke.c
c_dblas1.o : $(mySRCdir)/c_dblas1.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_dblas1.c
c_d1chke.o : $(mySRCdir)/c_d1chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_d1chke.c
c_cblas3.o : $(mySRCdir)/c_cblas3.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_cblas3.c
c_c3chke.o : $(mySRCdir)/c_c3chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_c3chke.c
c_cblas2.o : $(mySRCdir)/c_cblas2.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_cblas2.c
c_c2chke.o : $(mySRCdir)/c_c2chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_c2chke.c
c_cblas1.o : $(mySRCdir)/c_cblas1.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_cblas1.c
c_c1chke.o : $(mySRCdir)/c_c1chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_c1chke.c
c_zblas3.o : $(mySRCdir)/c_zblas3.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_zblas3.c
c_z3chke.o : $(mySRCdir)/c_z3chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_z3chke.c
c_zblas2.o : $(mySRCdir)/c_zblas2.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_zblas2.c
c_z2chke.o : $(mySRCdir)/c_z2chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_z2chke.c
c_zblas1.o : $(mySRCdir)/c_zblas1.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_zblas1.c
c_z1chke.o : $(mySRCdir)/c_z1chke.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_z1chke.c
auxiliary.o : $(mySRCdir)/auxiliary.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/auxiliary.c
c_xerbla.o : $(mySRCdir)/c_xerbla.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/c_xerbla.c

clean :
	rm -f *.o *.grd
