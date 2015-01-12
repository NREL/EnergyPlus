include Make.inc
mySRCdir = $(SRCdir)/src/threads/lapack

all : lib
ptlib : lib
tlib : lib
lib : dlib slib zlib clib
clean : sclean dclean cclean zclean

deps = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
       $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_tlvl3.h \
       $(INCAdir)/atlas_pthreads.h $(INCSdir)/atlas_threads.h

sobj = ATL_stgetf2.o ATL_stgeqr2.o ATL_stgeql2.o \
       ATL_stgerq2.o ATL_stgelq2.o
stlib : slib
slib : slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(sobj)
	$(RANLIB) $(PTLAPACKlib)
	touch slib.grd
sclean:
	rm -f $(sobj) slib.grd
ATL_stgeqr2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -o $@ -c $(mySRCdir)/ATL_tgeqr2.c
ATL_stgeql2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -o $@ -c $(mySRCdir)/ATL_tgeql2.c
ATL_stgetf2.o : $(deps) $(mySRCdir)/ATL_tgetf2.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -o $@ -c $(mySRCdir)/ATL_tgetf2.c
ATL_stgelq2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -DBUILD_LQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeqr2.c
ATL_stgerq2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DSREAL -DBUILD_RQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeql2.c

dobj = ATL_dtgetf2.o ATL_dtgeqr2.o ATL_dtgeql2.o \
       ATL_dtgerq2.o ATL_dtgelq2.o
dtlib : dlib
dlib : dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(dobj)
	$(RANLIB) $(PTLAPACKlib)
	touch dlib.grd
dclean:
	rm -f $(dobj) dlib.grd
ATL_dtgeqr2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -o $@ -c $(mySRCdir)/ATL_tgeqr2.c
ATL_dtgeql2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -o $@ -c $(mySRCdir)/ATL_tgeql2.c
ATL_dtgetf2.o : $(deps) $(mySRCdir)/ATL_tgetf2.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -o $@ -c $(mySRCdir)/ATL_tgetf2.c
ATL_dtgelq2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -DBUILD_LQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeqr2.c
ATL_dtgerq2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DDREAL -DBUILD_RQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeql2.c

cobj = ATL_ctgetf2.o ATL_ctgeqr2.o ATL_ctgeql2.o \
       ATL_ctgerq2.o ATL_ctgelq2.o
ctlib : clib
clib : clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(cobj)
	$(RANLIB) $(PTLAPACKlib)
	touch clib.grd
cclean:
	rm -f $(cobj) clib.grd
ATL_ctgeqr2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgeqr2.c
ATL_ctgeql2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgeql2.c
ATL_ctgetf2.o : $(deps) $(mySRCdir)/ATL_tgetf2.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -o $@ -c $(mySRCdir)/ATL_tgetf2.c
ATL_ctgelq2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -DBUILD_LQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeqr2.c
ATL_ctgerq2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DSCPLX -DBUILD_RQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeql2.c

zobj = ATL_ztgetf2.o ATL_ztgeqr2.o ATL_ztgeql2.o \
       ATL_ztgerq2.o ATL_ztgelq2.o
ztlib : zlib
zlib : zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(zobj)
	$(RANLIB) $(PTLAPACKlib)
	touch zlib.grd
zclean:
	rm -f $(zobj) zlib.grd
ATL_ztgeqr2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgeqr2.c
ATL_ztgeql2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgeql2.c
ATL_ztgetf2.o : $(deps) $(mySRCdir)/ATL_tgetf2.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -o $@ -c $(mySRCdir)/ATL_tgetf2.c
ATL_ztgelq2.o : $(deps) $(mySRCdir)/ATL_tgeqr2.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -DBUILD_LQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeqr2.c
ATL_ztgerq2.o : $(deps) $(mySRCdir)/ATL_tgeql2.c
	$(KC) $(KCFLAGS) -I./ -DDCPLX -DBUILD_RQ2 -o $@ -c \
               $(mySRCdir)/ATL_tgeql2.c

