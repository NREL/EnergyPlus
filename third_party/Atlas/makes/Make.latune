include Make.inc
mySRCdir = $(SRCdir)/tune/lapack

res/ATL_sGetNB_geqrf.h : xslanbsrch
	./xslanbsrch -oc res/ATL_sGetNB_geqrf
res/ATL_stGetNB_geqrf.h : xslanbsrch_pt
	./xslanbsrch_pt -oc res/ATL_stGetNB_geqrf
xslanbsrch_pt : slanbsrch_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt \
                  $(MMTdir)/res/sMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbsrch_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)

slanbsrch_pt.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DSREAL $(mySRCdir)/lanbsrch.c
sl3lib_pt : 
	cd $(BINdir) ; $(MAKE) sl3lib_pt
ststlib_pt : 
	cd $(BINdir) ; $(MAKE) ststlib_pt
scblas_pt : 
	cd $(BINdir) ; $(MAKE) scblas_pt
sl1lib_pt : 
	cd $(BINdir) ; $(MAKE) sl1lib_pt
sl2lib_pt : 
	cd $(BINdir) ; $(MAKE) sl2lib_pt
xslanbsrch : slanbsrch.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas \
                  $(MMTdir)/res/sMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbsrch.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)

slanbsrch.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DSREAL $(mySRCdir)/lanbsrch.c
sl3lib : 
	cd $(BINdir) ; $(MAKE) sl3lib
ststlib : 
	cd $(BINdir) ; $(MAKE) ststlib
scblas : 
	cd $(BINdir) ; $(MAKE) scblas
sl1lib : 
	cd $(BINdir) ; $(MAKE) sl1lib
sl2lib : 
	cd $(BINdir) ; $(MAKE) sl2lib
$(MMTdir)/res/sMMRES.sum :
	cd $(MMTdir) ; $(MAKE) res/sMMRES.sum
slapack :
	cd $(BINdir) ; $(MAKE) slapack
res/ATL_dGetNB_geqrf.h : xdlanbsrch
	./xdlanbsrch -oc res/ATL_dGetNB_geqrf
res/ATL_dtGetNB_geqrf.h : xdlanbsrch_pt
	./xdlanbsrch_pt -oc res/ATL_dtGetNB_geqrf
xdlanbsrch_pt : dlanbsrch_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt \
                  $(MMTdir)/res/dMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbsrch_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)

dlanbsrch_pt.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DDREAL $(mySRCdir)/lanbsrch.c
dl3lib_pt : 
	cd $(BINdir) ; $(MAKE) dl3lib_pt
dtstlib_pt : 
	cd $(BINdir) ; $(MAKE) dtstlib_pt
dcblas_pt : 
	cd $(BINdir) ; $(MAKE) dcblas_pt
dl1lib_pt : 
	cd $(BINdir) ; $(MAKE) dl1lib_pt
dl2lib_pt : 
	cd $(BINdir) ; $(MAKE) dl2lib_pt
xdlanbsrch : dlanbsrch.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas \
                  $(MMTdir)/res/dMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbsrch.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)

dlanbsrch.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DDREAL $(mySRCdir)/lanbsrch.c
dl3lib : 
	cd $(BINdir) ; $(MAKE) dl3lib
dtstlib : 
	cd $(BINdir) ; $(MAKE) dtstlib
dcblas : 
	cd $(BINdir) ; $(MAKE) dcblas
dl1lib : 
	cd $(BINdir) ; $(MAKE) dl1lib
dl2lib : 
	cd $(BINdir) ; $(MAKE) dl2lib
$(MMTdir)/res/dMMRES.sum :
	cd $(MMTdir) ; $(MAKE) res/dMMRES.sum
dlapack :
	cd $(BINdir) ; $(MAKE) dlapack
res/ATL_cGetNB_geqrf.h : xclanbsrch
	./xclanbsrch -oc res/ATL_cGetNB_geqrf
res/ATL_ctGetNB_geqrf.h : xclanbsrch_pt
	./xclanbsrch_pt -oc res/ATL_ctGetNB_geqrf
xclanbsrch_pt : clanbsrch_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt \
                  $(MMTdir)/res/cMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbsrch_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)

clanbsrch_pt.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/lanbsrch.c
cl3lib_pt : 
	cd $(BINdir) ; $(MAKE) cl3lib_pt
ctstlib_pt : 
	cd $(BINdir) ; $(MAKE) ctstlib_pt
ccblas_pt : 
	cd $(BINdir) ; $(MAKE) ccblas_pt
cl1lib_pt : 
	cd $(BINdir) ; $(MAKE) cl1lib_pt
cl2lib_pt : 
	cd $(BINdir) ; $(MAKE) cl2lib_pt
xclanbsrch : clanbsrch.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas \
                  $(MMTdir)/res/cMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbsrch.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)

clanbsrch.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DSCPLX $(mySRCdir)/lanbsrch.c
cl3lib : 
	cd $(BINdir) ; $(MAKE) cl3lib
ctstlib : 
	cd $(BINdir) ; $(MAKE) ctstlib
ccblas : 
	cd $(BINdir) ; $(MAKE) ccblas
cl1lib : 
	cd $(BINdir) ; $(MAKE) cl1lib
cl2lib : 
	cd $(BINdir) ; $(MAKE) cl2lib
$(MMTdir)/res/cMMRES.sum :
	cd $(MMTdir) ; $(MAKE) res/cMMRES.sum
clapack :
	cd $(BINdir) ; $(MAKE) clapack
res/ATL_zGetNB_geqrf.h : xzlanbsrch
	./xzlanbsrch -oc res/ATL_zGetNB_geqrf
res/ATL_ztGetNB_geqrf.h : xzlanbsrch_pt
	./xzlanbsrch_pt -oc res/ATL_ztGetNB_geqrf
xzlanbsrch_pt : zlanbsrch_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt \
                  $(MMTdir)/res/zMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbsrch_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)

zlanbsrch_pt.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/lanbsrch.c
zl3lib_pt : 
	cd $(BINdir) ; $(MAKE) zl3lib_pt
ztstlib_pt : 
	cd $(BINdir) ; $(MAKE) ztstlib_pt
zcblas_pt : 
	cd $(BINdir) ; $(MAKE) zcblas_pt
zl1lib_pt : 
	cd $(BINdir) ; $(MAKE) zl1lib_pt
zl2lib_pt : 
	cd $(BINdir) ; $(MAKE) zl2lib_pt
xzlanbsrch : zlanbsrch.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas \
                  $(MMTdir)/res/zMMRES.sum
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbsrch.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)

zlanbsrch.o : $(mySRCdir)/lanbsrch.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DDCPLX $(mySRCdir)/lanbsrch.c
zl3lib : 
	cd $(BINdir) ; $(MAKE) zl3lib
ztstlib : 
	cd $(BINdir) ; $(MAKE) ztstlib
zcblas : 
	cd $(BINdir) ; $(MAKE) zcblas
zl1lib : 
	cd $(BINdir) ; $(MAKE) zl1lib
zl2lib : 
	cd $(BINdir) ; $(MAKE) zl2lib
$(MMTdir)/res/zMMRES.sum :
	cd $(MMTdir) ; $(MAKE) res/zMMRES.sum
zlapack :
	cd $(BINdir) ; $(MAKE) zlapack
