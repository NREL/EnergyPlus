include Make.inc
mySRCdir = $(SRCdir)/src/blas/pklevel3

all : lib
lib : dlib slib zlib clib
clean : sclean dclean cclean zclean
sclean :
	cd gpmm ; $(MAKE) sclean
	cd sprk ; $(MAKE) sclean
slib :
	cd gpmm ; $(MAKE) slib
	cd sprk ; $(MAKE) slib
dclean :
	cd gpmm ; $(MAKE) dclean
	cd sprk ; $(MAKE) dclean
dlib :
	cd gpmm ; $(MAKE) dlib
	cd sprk ; $(MAKE) dlib
zclean :
	cd gpmm ; $(MAKE) zclean
	cd sprk ; $(MAKE) zclean
zlib :
	cd gpmm ; $(MAKE) zlib
	cd sprk ; $(MAKE) zlib
cclean :
	cd gpmm ; $(MAKE) cclean
	cd sprk ; $(MAKE) cclean
clib :
	cd gpmm ; $(MAKE) clib
	cd sprk ; $(MAKE) clib
