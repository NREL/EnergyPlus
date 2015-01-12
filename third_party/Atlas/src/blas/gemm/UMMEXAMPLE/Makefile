include Make.inc

sobj = ATLU_susergemm.o
dobj = ATLU_dusergemm.o
cobj = ATLU_cusergemm.o
zobj = ATLU_zusergemm.o

susermm : slib.grd
slib.grd : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
sclean :
	rm -f $(sobj) slib.grd

dusermm : dlib.grd
dlib.grd : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
dclean :
	rm -f $(dobj) dlib.grd

cusermm : clib.grd
clib.grd : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
cclean :
	rm -f $(cobj) clib.grd

zusermm : zlib.grd
zlib.grd : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
zclean :
	rm -f $(zobj) zlib.grd


ATLU_susergemm.o : ../ATLU_usergemm.c
	$(KC) $(KCFLAGS) $(UMMDEFS) $(OJ) $(NM) $@ -DSREAL ../ATLU_usergemm.c
ATLU_dusergemm.o : ../ATLU_usergemm.c
	$(KC) $(KCFLAGS) $(UMMDEFS) $(OJ) $(NM) $@ -DDREAL ../ATLU_usergemm.c
ATLU_cusergemm.o : ../ATLU_usergemm.c
	$(KC) $(KCFLAGS) $(UMMDEFS) $(OJ) $(NM) $@ -DSCPLX ../ATLU_usergemm.c
ATLU_zusergemm.o : ../ATLU_usergemm.c
	$(KC) $(KCFLAGS) $(UMMDEFS) $(OJ) $(NM) $@ -DDCPLX ../ATLU_usergemm.c
