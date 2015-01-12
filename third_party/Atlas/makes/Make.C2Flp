include Make.inc
mySRCdir = $(SRCdir)/interfaces/lapack/C2F/src

sobj = ATL_C2Fsgels.o ATL_C2Fsormlq.o ATL_C2Fsormql.o ATL_C2Fsormqr.o \
       ATL_C2Fsormrq.o
dobj = ATL_C2Fdgels.o ATL_C2Fdormlq.o ATL_C2Fdormql.o ATL_C2Fdormqr.o \
       ATL_C2Fdormrq.o
cobj = ATL_C2Fcgels.o ATL_C2Fcunmlq.o ATL_C2Fcunmql.o ATL_C2Fcunmqr.o \
       ATL_C2Fcunmrq.o
zobj = ATL_C2Fzgels.o ATL_C2Fzunmlq.o ATL_C2Fzunmql.o ATL_C2Fzunmqr.o \
       ATL_C2Fzunmrq.o

ptlib : tlib
all :
	$(MAKE) lib
	- $(MAKE) tlib

clean : dclean sclean zclean cclean dtclean stclean ztclean ctclean
#
# NOTE: only build C2F interface if F77 LAPACK exists; otherwise, we will
#       not be able to build dynamic libs due to unsatisfied calls to F77LAPACK
#
stclean :
	rm -f $(stobj) stlib.grd
tlib : dtlib stlib ztlib ctlib
stlib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) stlib.grd ; \
        fi
stlib.grd : $(sobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(sobj) ; \
           $(RANLIB) $(PTLAPACKlib) ; \
        fi
	touch stlib.grd
sclean :
	rm -f $(sobj) slib.grd
lib : dlib slib zlib clib
slib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) slib.grd ; \
        fi
slib.grd : $(sobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(sobj) ; \
           $(RANLIB) $(LAPACKlib) ; \
        fi
	touch slib.grd
dtclean :
	rm -f $(dtobj) dtlib.grd
tlib : dtlib stlib ztlib ctlib
dtlib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) dtlib.grd ; \
        fi
dtlib.grd : $(dobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(dobj) ; \
           $(RANLIB) $(PTLAPACKlib) ; \
        fi
	touch dtlib.grd
dclean :
	rm -f $(dobj) dlib.grd
lib : dlib slib zlib clib
dlib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) dlib.grd ; \
        fi
dlib.grd : $(dobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(dobj) ; \
           $(RANLIB) $(LAPACKlib) ; \
        fi
	touch dlib.grd
ctclean :
	rm -f $(ctobj) ctlib.grd
tlib : dtlib stlib ztlib ctlib
ctlib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) ctlib.grd ; \
        fi
ctlib.grd : $(cobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(cobj) ; \
           $(RANLIB) $(PTLAPACKlib) ; \
        fi
	touch ctlib.grd
cclean :
	rm -f $(cobj) clib.grd
lib : dlib slib zlib clib
clib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) clib.grd ; \
        fi
clib.grd : $(cobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(cobj) ; \
           $(RANLIB) $(LAPACKlib) ; \
        fi
	touch clib.grd
ztclean :
	rm -f $(ztobj) ztlib.grd
tlib : dtlib stlib ztlib ctlib
ztlib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) ztlib.grd ; \
        fi
ztlib.grd : $(zobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(PTLAPACKlib) $(zobj) ; \
           $(RANLIB) $(PTLAPACKlib) ; \
        fi
	touch ztlib.grd
zclean :
	rm -f $(zobj) zlib.grd
lib : dlib slib zlib clib
zlib : 
	if [ -d "$(FLAdir)" ]; then \
           $(MAKE) zlib.grd ; \
        fi
zlib.grd : $(zobj)
	if [ -d "$(FLAdir)" ]; then \
           $(ARCHIVER) $(ARFLAGS) $(LAPACKlib) $(zobj) ; \
           $(RANLIB) $(LAPACKlib) ; \
        fi
	touch zlib.grd

ATL_C2Fcunmlq.o : $(mySRCdir)/ATL_C2Funmlq.c
	$(KC) $(KCFLAGS) -o $@ -c -DSCPLX $(mySRCdir)/ATL_C2Funmlq.c
ATL_C2Fcunmrq.o : $(mySRCdir)/ATL_C2Funmrq.c
	$(KC) $(KCFLAGS) -o $@ -c -DSCPLX $(mySRCdir)/ATL_C2Funmrq.c
ATL_C2Fcunmql.o : $(mySRCdir)/ATL_C2Funmql.c
	$(KC) $(KCFLAGS) -o $@ -c -DSCPLX $(mySRCdir)/ATL_C2Funmql.c
ATL_C2Fcunmqr.o : $(mySRCdir)/ATL_C2Funmqr.c
	$(KC) $(KCFLAGS) -o $@ -c -DSCPLX $(mySRCdir)/ATL_C2Funmqr.c
ATL_C2Fzunmlq.o : $(mySRCdir)/ATL_C2Funmlq.c
	$(KC) $(KCFLAGS) -o $@ -c -DDCPLX $(mySRCdir)/ATL_C2Funmlq.c
ATL_C2Fzunmrq.o : $(mySRCdir)/ATL_C2Funmrq.c
	$(KC) $(KCFLAGS) -o $@ -c -DDCPLX $(mySRCdir)/ATL_C2Funmrq.c
ATL_C2Fzunmql.o : $(mySRCdir)/ATL_C2Funmql.c
	$(KC) $(KCFLAGS) -o $@ -c -DDCPLX $(mySRCdir)/ATL_C2Funmql.c
ATL_C2Fzunmqr.o : $(mySRCdir)/ATL_C2Funmqr.c
	$(KC) $(KCFLAGS) -o $@ -c -DDCPLX $(mySRCdir)/ATL_C2Funmqr.c
ATL_C2Fsormlq.o : $(mySRCdir)/ATL_C2Formlq.c
	$(KC) $(KCFLAGS) -o $@ -c -DSREAL $(mySRCdir)/ATL_C2Formlq.c
ATL_C2Fsormrq.o : $(mySRCdir)/ATL_C2Formrq.c
	$(KC) $(KCFLAGS) -o $@ -c -DSREAL $(mySRCdir)/ATL_C2Formrq.c
ATL_C2Fsormql.o : $(mySRCdir)/ATL_C2Formql.c
	$(KC) $(KCFLAGS) -o $@ -c -DSREAL $(mySRCdir)/ATL_C2Formql.c
ATL_C2Fsormqr.o : $(mySRCdir)/ATL_C2Formqr.c
	$(KC) $(KCFLAGS) -o $@ -c -DSREAL $(mySRCdir)/ATL_C2Formqr.c
ATL_C2Fdormlq.o : $(mySRCdir)/ATL_C2Formlq.c
	$(KC) $(KCFLAGS) -o $@ -c -DDREAL $(mySRCdir)/ATL_C2Formlq.c
ATL_C2Fdormrq.o : $(mySRCdir)/ATL_C2Formrq.c
	$(KC) $(KCFLAGS) -o $@ -c -DDREAL $(mySRCdir)/ATL_C2Formrq.c
ATL_C2Fdormql.o : $(mySRCdir)/ATL_C2Formql.c
	$(KC) $(KCFLAGS) -o $@ -c -DDREAL $(mySRCdir)/ATL_C2Formql.c
ATL_C2Fdormqr.o : $(mySRCdir)/ATL_C2Formqr.c
	$(KC) $(KCFLAGS) -o $@ -c -DDREAL $(mySRCdir)/ATL_C2Formqr.c

ATL_C2Fsgels.o : $(mySRCdir)/ATL_C2Fgels.c
	$(KC) $(KCFLAGS) -o $@ -c -DSREAL $(mySRCdir)/ATL_C2Fgels.c
ATL_C2Fdgels.o : $(mySRCdir)/ATL_C2Fgels.c
	$(KC) $(KCFLAGS) -o $@ -c -DDREAL $(mySRCdir)/ATL_C2Fgels.c
ATL_C2Fcgels.o : $(mySRCdir)/ATL_C2Fgels.c
	$(KC) $(KCFLAGS) -o $@ -c -DSCPLX $(mySRCdir)/ATL_C2Fgels.c
ATL_C2Fzgels.o : $(mySRCdir)/ATL_C2Fgels.c
	$(KC) $(KCFLAGS) -o $@ -c -DDCPLX $(mySRCdir)/ATL_C2Fgels.c

