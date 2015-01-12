include Make.inc
mySRCdir = $(SRCdir)/src/threads/blas/level2
deps = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
       $(INCAdir)/atlas_pthreads.h $(INCSdir)/atlas_threads.h

all : lib
lib : dlib
double : dlib
sobj = ATL_stgemv.o ATL_stger.o
slib : slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd

ATL_stgemv.o : $(deps) $(mySRCdir)/ATL_tgemv.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -o $@ -c $(mySRCdir)/ATL_tgemv.c
ATL_stger.o : $(deps) $(mySRCdir)/ATL_tger.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -o $@ -c $(mySRCdir)/ATL_tger.c


dobj = ATL_dtgemv.o ATL_dtger.o
dlib : dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd

ATL_dtgemv.o : $(deps) $(mySRCdir)/ATL_tgemv.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -o $@ -c $(mySRCdir)/ATL_tgemv.c
ATL_dtger.o : $(deps) $(mySRCdir)/ATL_tger.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -o $@ -c $(mySRCdir)/ATL_tger.c


cobj = ATL_ctgemv.o ATL_ctgerc.o ATL_ctgeru.o
clib : clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd

ATL_ctgemv.o : $(deps) $(mySRCdir)/ATL_tgemv.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgemv.c
ATL_ctgerc.o : $(deps) $(mySRCdir)/ATL_tger.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -DConj_ -o $@ -c $(mySRCdir)/ATL_tger.c
ATL_ctgeru.o : $(deps) $(mySRCdir)/ATL_tger.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX   -o $@ -c $(mySRCdir)/ATL_tger.c


zobj = ATL_ztgemv.o ATL_ztgerc.o ATL_ztgeru.o
zlib : zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd

ATL_ztgemv.o : $(deps) $(mySRCdir)/ATL_tgemv.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgemv.c
ATL_ztgerc.o : $(deps) $(mySRCdir)/ATL_tger.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -DConj_ -o $@ -c $(mySRCdir)/ATL_tger.c
ATL_ztgeru.o : $(deps) $(mySRCdir)/ATL_tger.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX   -o $@ -c $(mySRCdir)/ATL_tger.c



