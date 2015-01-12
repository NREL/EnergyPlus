include Make.inc
mySRCdir = $(SRCdir)/interfaces/blas/F77/testing

MYDIR = $(BLDdir)/interfaces/blas/F77/testing/

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

slvl1 : xsblat1 xsaxpbytst xssettst
slvl2 : xsblat2
slvl3 : xsblat3
dlvl1 : xdblat1 xdaxpbytst xdsettst
dlvl2 : xdblat2
dlvl3 : xdblat3
clvl1 : xcblat1 xcaxpbytst xcsettst
clvl2 : xcblat2
clvl3 : xcblat3
zlvl1 : xzblat1 xzaxpbytst xzsettst
zlvl2 : xzblat2
zlvl3 : xzblat3
ssanity_test : xsblat1 xsblat2 xsblat3
	$(ATLRUN) $(MYDIR) xsblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xsblat2 < $(mySRCdir)/sblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xsblat3 < $(mySRCdir)/sblat3.dat \
                  >> $(MYDIR)/sanity.out
xsblat1 : sblat1.o sl1lib silvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ sblat1.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xsblat2 : sblat2.o sl2lib silvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ sblat2.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xsblat3 : sblat3.o sl3lib silvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ sblat3.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xssettst : ssettst.o sl1lib silvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ ssettst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
xsaxpbytst : saxpbytst.o sl1lib silvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ saxpbytst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
dsanity_test : xdblat1 xdblat2 xdblat3
	$(ATLRUN) $(MYDIR) xdblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xdblat2 < $(mySRCdir)/dblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xdblat3 < $(mySRCdir)/dblat3.dat \
                  >> $(MYDIR)/sanity.out
xdblat1 : dblat1.o dl1lib dilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ dblat1.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xdblat2 : dblat2.o dl2lib dilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ dblat2.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xdblat3 : dblat3.o dl3lib dilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ dblat3.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xdsettst : dsettst.o dl1lib dilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ dsettst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
xdaxpbytst : daxpbytst.o dl1lib dilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ daxpbytst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
csanity_test : xcblat1 xcblat2 xcblat3
	$(ATLRUN) $(MYDIR) xcblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xcblat2 < $(mySRCdir)/cblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xcblat3 < $(mySRCdir)/cblat3.dat \
                  >> $(MYDIR)/sanity.out
xcblat1 : cblat1.o cl1lib cilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ cblat1.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xcblat2 : cblat2.o cl2lib cilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ cblat2.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xcblat3 : cblat3.o cl3lib cilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ cblat3.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xcsettst : csettst.o cl1lib cilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ csettst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
xcaxpbytst : caxpbytst.o cl1lib cilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ caxpbytst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
zsanity_test : xzblat1 xzblat2 xzblat3
	$(ATLRUN) $(MYDIR) xzblat1 >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xzblat2 < $(mySRCdir)/zblat2.dat \
                  >> $(MYDIR)/sanity.out
	$(ATLRUN) $(MYDIR) xzblat3 < $(mySRCdir)/zblat3.dat \
                  >> $(MYDIR)/sanity.out
xzblat1 : zblat1.o zl1lib zilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ zblat1.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xzblat2 : zblat2.o zl2lib zilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ zblat2.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xzblat3 : zblat3.o zl3lib zilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ zblat3.o $(LAPACKlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xzsettst : zsettst.o zl1lib zilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ zsettst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
xzaxpbytst : zaxpbytst.o zl1lib zilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ zaxpbytst.o \
           $(TESTlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
ptall : ptlvl1 ptlvl2 ptlvl3
ptlvl3 : dptlvl3 sptlvl3 zptlvl3 cptlvl3
ptlvl2 : dptlvl2 sptlvl2 zptlvl2 cptlvl2
ptlvl1 : dptlvl1 sptlvl1 zptlvl1 cptlvl1

sptlvl1 : xsptblat1 xsptaxpbytst xsptsettst
sptlvl2 : xsptblat2
sptlvl3 : xsptblat3
dptlvl1 : xdptblat1 xdptaxpbytst xdptsettst
dptlvl2 : xdptblat2
dptlvl3 : xdptblat3
cptlvl1 : xcptblat1 xcptaxpbytst xcptsettst
cptlvl2 : xcptblat2
cptlvl3 : xcptblat3
zptlvl1 : xzptblat1 xzptaxpbytst xzptsettst
zptlvl2 : xzptblat2
zptlvl3 : xzptblat3
sptsanity_test : xsptblat1 xsptblat2 xsptblat3
	$(ATLRUN) $(MYDIR) xsptblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xsptblat2 < $(mySRCdir)/sblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xsptblat3 < $(mySRCdir)/sblat3.dat \
                  >> $(MYDIR)/ptsanity.out
xsptblat1 : sblat1.o sl1lib sptilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ sblat1.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xsptblat2 : sblat2.o sl2lib sptilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ sblat2.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xsptblat3 : sblat3.o sl3lib sptilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ sblat3.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xsptsettst : ssettst.o sl1lib sptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ ssettst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xsptaxpbytst : saxpbytst.o sl1lib sptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ saxpbytst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
dptsanity_test : xdptblat1 xdptblat2 xdptblat3
	$(ATLRUN) $(MYDIR) xdptblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xdptblat2 < $(mySRCdir)/dblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xdptblat3 < $(mySRCdir)/dblat3.dat \
                  >> $(MYDIR)/ptsanity.out
xdptblat1 : dblat1.o dl1lib dptilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ dblat1.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xdptblat2 : dblat2.o dl2lib dptilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ dblat2.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xdptblat3 : dblat3.o dl3lib dptilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ dblat3.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xdptsettst : dsettst.o dl1lib dptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ dsettst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xdptaxpbytst : daxpbytst.o dl1lib dptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ daxpbytst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
cptsanity_test : xcptblat1 xcptblat2 xcptblat3
	$(ATLRUN) $(MYDIR) xcptblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xcptblat2 < $(mySRCdir)/cblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xcptblat3 < $(mySRCdir)/cblat3.dat \
                  >> $(MYDIR)/ptsanity.out
xcptblat1 : cblat1.o cl1lib cptilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ cblat1.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xcptblat2 : cblat2.o cl2lib cptilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ cblat2.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xcptblat3 : cblat3.o cl3lib cptilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ cblat3.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xcptsettst : csettst.o cl1lib cptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ csettst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xcptaxpbytst : caxpbytst.o cl1lib cptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ caxpbytst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
zptsanity_test : xzptblat1 xzptblat2 xzptblat3
	$(ATLRUN) $(MYDIR) xzptblat1 >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xzptblat2 < $(mySRCdir)/zblat2.dat \
                  >> $(MYDIR)/ptsanity.out
	$(ATLRUN) $(MYDIR) xzptblat3 < $(mySRCdir)/zblat3.dat \
                  >> $(MYDIR)/ptsanity.out
xzptblat1 : zblat1.o zl1lib zptilvl1
	$(FLINKER) $(FLINKFLAGS) -o $@ zblat1.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xzptblat2 : zblat2.o zl2lib zptilvl2
	$(FLINKER) $(FLINKFLAGS) -o $@ zblat2.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xzptblat3 : zblat3.o zl3lib zptilvl3
	$(FLINKER) $(FLINKFLAGS) -o $@ zblat3.o $(LAPACKlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xzptsettst : zsettst.o zl1lib zptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ zsettst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xzptaxpbytst : zaxpbytst.o zl1lib zptilvl1
	$(FLINKER) $(FCLINKFLAGS) -o $@ zaxpbytst.o \
           $(TESTlib) $(PTF77BLASlib) $(ATLASlib) $(LIBS)

force_build:
auxlib : force_build
	cd $(AUXdir) ; $(MAKE) lib

sptilvl3 :
	cd $(FBLdir) ; $(MAKE) sptlvl3
sptilvl2 :
	cd $(FBLdir) ; $(MAKE) sptlvl2
sptilvl1 :
	cd $(FBLdir) ; $(MAKE) sptlvl1
silvl3 :
	cd $(FBLdir) ; $(MAKE) slvl3
silvl2 :
	cd $(FBLdir) ; $(MAKE) slvl2
silvl1 :
	cd $(FBLdir) ; $(MAKE) slvl1
sl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) slib
sl2lib : sl1lib auxlib
	cd $(GMVdir) ; $(MAKE) slib
	cd $(GR1dir) ; $(MAKE) slib
	cd $(L2Bdir) ; $(MAKE) slib
sl3lib :
	cd $(GMMdir) ; $(MAKE) slib
	cd $(L3Bdir) ; $(MAKE) slib
dptilvl3 :
	cd $(FBLdir) ; $(MAKE) dptlvl3
dptilvl2 :
	cd $(FBLdir) ; $(MAKE) dptlvl2
dptilvl1 :
	cd $(FBLdir) ; $(MAKE) dptlvl1
dilvl3 :
	cd $(FBLdir) ; $(MAKE) dlvl3
dilvl2 :
	cd $(FBLdir) ; $(MAKE) dlvl2
dilvl1 :
	cd $(FBLdir) ; $(MAKE) dlvl1
dl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) dlib
dl2lib : dl1lib auxlib
	cd $(GMVdir) ; $(MAKE) dlib
	cd $(GR1dir) ; $(MAKE) dlib
	cd $(L2Bdir) ; $(MAKE) dlib
dl3lib :
	cd $(GMMdir) ; $(MAKE) dlib
	cd $(L3Bdir) ; $(MAKE) dlib
cptilvl3 :
	cd $(FBLdir) ; $(MAKE) cptlvl3
cptilvl2 :
	cd $(FBLdir) ; $(MAKE) cptlvl2
cptilvl1 :
	cd $(FBLdir) ; $(MAKE) cptlvl1
cilvl3 :
	cd $(FBLdir) ; $(MAKE) clvl3
cilvl2 :
	cd $(FBLdir) ; $(MAKE) clvl2
cilvl1 :
	cd $(FBLdir) ; $(MAKE) clvl1
cl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) clib
cl2lib : cl1lib auxlib
	cd $(GMVdir) ; $(MAKE) clib
	cd $(GR1dir) ; $(MAKE) clib
	cd $(L2Bdir) ; $(MAKE) clib
cl3lib :
	cd $(GMMdir) ; $(MAKE) clib
	cd $(L3Bdir) ; $(MAKE) clib
zptilvl3 :
	cd $(FBLdir) ; $(MAKE) zptlvl3
zptilvl2 :
	cd $(FBLdir) ; $(MAKE) zptlvl2
zptilvl1 :
	cd $(FBLdir) ; $(MAKE) zptlvl1
zilvl3 :
	cd $(FBLdir) ; $(MAKE) zlvl3
zilvl2 :
	cd $(FBLdir) ; $(MAKE) zlvl2
zilvl1 :
	cd $(FBLdir) ; $(MAKE) zlvl1
zl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) zlib
zl2lib : zl1lib auxlib
	cd $(GMVdir) ; $(MAKE) zlib
	cd $(GR1dir) ; $(MAKE) zlib
	cd $(L2Bdir) ; $(MAKE) zlib
zl3lib :
	cd $(GMMdir) ; $(MAKE) zlib
	cd $(L3Bdir) ; $(MAKE) zlib


ssettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DSREAL -DTEST_F77 -o $@ -c $(mySRCdir)/settst.c
saxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DSREAL -DTEST_F77 -o $@ -c $(mySRCdir)/axpbytst.c
sblat3.o : $(mySRCdir)/sblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/sblat3.f
sblat2.o : $(mySRCdir)/sblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/sblat2.f
sblat1.o : $(mySRCdir)/sblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/sblat1.f
dsettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DDREAL -DTEST_F77 -o $@ -c $(mySRCdir)/settst.c
daxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DDREAL -DTEST_F77 -o $@ -c $(mySRCdir)/axpbytst.c
dblat3.o : $(mySRCdir)/dblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/dblat3.f
dblat2.o : $(mySRCdir)/dblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/dblat2.f
dblat1.o : $(mySRCdir)/dblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/dblat1.f
csettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DSCPLX -DTEST_F77 -o $@ -c $(mySRCdir)/settst.c
caxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DSCPLX -DTEST_F77 -o $@ -c $(mySRCdir)/axpbytst.c
cblat3.o : $(mySRCdir)/cblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/cblat3.f
cblat2.o : $(mySRCdir)/cblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/cblat2.f
cblat1.o : $(mySRCdir)/cblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/cblat1.f
zsettst.o : $(mySRCdir)/settst.c
	$(KC) $(KCFLAGS) -DDCPLX -DTEST_F77 -o $@ -c $(mySRCdir)/settst.c
zaxpbytst.o : $(mySRCdir)/axpbytst.c
	$(KC) $(KCFLAGS) -DDCPLX -DTEST_F77 -o $@ -c $(mySRCdir)/axpbytst.c
zblat3.o : $(mySRCdir)/zblat3.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/zblat3.f
zblat2.o : $(mySRCdir)/zblat2.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/zblat2.f
zblat1.o : $(mySRCdir)/zblat1.f
	$(F77) $(F77NOOPT) -o $@ -c $(mySRCdir)/zblat1.f

sclean : clean
dclean : clean
cclean : clean
zclean : clean
clean :
	rm -rf *.o *.grd
