include Make.inc
mySRCdir = $(SRCdir)/src/blas/f77reference

sobj = isamax.o sasum.o saxpy.o scopy.o sdot.o sdsdot.o sgbmv.o sgemm.o \
       sgemv.o sger.o snrm2.o srot.o srotg.o srotm.o srotmg.o ssbmv.o sscal.o \
       sspmv.o sspr.o sspr2.o sswap.o ssymm.o ssymv.o ssyr.o ssyr2.o ssyr2k.o \
       ssyrk.o stbmv.o stbsv.o stpmv.o stpsv.o strmm.o strmv.o strsm.o \
       strsv.o
dobj = dasum.o daxpy.o dcopy.o ddot.o dgbmv.o dgemm.o dgemv.o dger.o dnrm2.o \
       drot.o drotg.o drotm.o drotmg.o dsbmv.o dscal.o dsdot.o dspmv.o dspr.o \
       dspr2.o dswap.o dsymm.o dsymv.o dsyr.o dsyr2.o dsyr2k.o dsyrk.o \
       dtbmv.o dtbsv.o dtpmv.o dtpsv.o dtrmm.o dtrmv.o dtrsm.o dtrsv.o \
       idamax.o
cobj = caxpy.o ccopy.o cdotc.o cdotu.o cgbmv.o cgemm.o cgemv.o cgerc.o \
       cgeru.o chbmv.o chemm.o chemv.o cher.o cher2.o cher2k.o cherk.o \
       chpmv.o chpr.o chpr2.o crotg.o cscal.o csrot.o csscal.o cswap.o \
       csymm.o csyr2k.o csyrk.o ctbmv.o ctbsv.o ctpmv.o ctpsv.o ctrmm.o \
       ctrmv.o ctrsm.o ctrsv.o icamax.o scabs1.o scasum.o scnrm2.o
zobj = dcabs1.o dzasum.o dznrm2.o izamax.o zaxpy.o zcopy.o zdotc.o zdotu.o \
       zdrot.o zdscal.o zgbmv.o zgemm.o zgemv.o zgerc.o zgeru.o zhbmv.o \
       zhemm.o zhemv.o zher.o zher2.o zher2k.o zherk.o zhpmv.o zhpr.o zhpr2.o \
       zrotg.o zscal.o zswap.o zsymm.o zsyr2k.o zsyrk.o ztbmv.o ztbsv.o \
       ztpmv.o ztpsv.o ztrmm.o ztrmv.o ztrsm.o ztrsv.o

all : lib

lib : zlib clib dlib slib lsame.o xerbla.o
	$(ARCHIVER) $(ARFLAGS) $(FBLASlib) lsame.o xerbla.o
	$(RANLIB) $(FBLASlib)
slib : $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(FBLASlib) $(sobj)
	$(RANLIB) $(FBLASlib)
dlib : $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(FBLASlib) $(dobj)
	$(RANLIB) $(FBLASlib)
clib : $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(FBLASlib) $(cobj)
	$(RANLIB) $(FBLASlib)
zlib : $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(FBLASlib) $(zobj)
	$(RANLIB) $(FBLASlib)

xerbla.o : $(mySRCdir)/xerbla.f
	$(F77) $(F77FLAGS) -o xerbla.o -c $(mySRCdir)/xerbla.f
lsame.o : $(mySRCdir)/lsame.f
	$(F77) $(F77FLAGS) -o lsame.o -c $(mySRCdir)/lsame.f

saxpy.o : $(mySRCdir)/saxpy.f
	$(F77) $(F77FLAGS) -o saxpy.o -c $(mySRCdir)/saxpy.f
scopy.o : $(mySRCdir)/scopy.f
	$(F77) $(F77FLAGS) -o scopy.o -c $(mySRCdir)/scopy.f
sscal.o : $(mySRCdir)/sscal.f
	$(F77) $(F77FLAGS) -o sscal.o -c $(mySRCdir)/sscal.f
sswap.o : $(mySRCdir)/sswap.f
	$(F77) $(F77FLAGS) -o sswap.o -c $(mySRCdir)/sswap.f
srotg.o : $(mySRCdir)/srotg.f
	$(F77) $(F77FLAGS) -o srotg.o -c $(mySRCdir)/srotg.f
isamax.o : $(mySRCdir)/isamax.f
	$(F77) $(F77FLAGS) -o isamax.o -c $(mySRCdir)/isamax.f
sgbmv.o : $(mySRCdir)/sgbmv.f
	$(F77) $(F77FLAGS) -o sgbmv.o -c $(mySRCdir)/sgbmv.f
stbmv.o : $(mySRCdir)/stbmv.f
	$(F77) $(F77FLAGS) -o stbmv.o -c $(mySRCdir)/stbmv.f
stbsv.o : $(mySRCdir)/stbsv.f
	$(F77) $(F77FLAGS) -o stbsv.o -c $(mySRCdir)/stbsv.f
stpmv.o : $(mySRCdir)/stpmv.f
	$(F77) $(F77FLAGS) -o stpmv.o -c $(mySRCdir)/stpmv.f
stpsv.o : $(mySRCdir)/stpsv.f
	$(F77) $(F77FLAGS) -o stpsv.o -c $(mySRCdir)/stpsv.f
sgemv.o : $(mySRCdir)/sgemv.f
	$(F77) $(F77FLAGS) -o sgemv.o -c $(mySRCdir)/sgemv.f
ssyr2k.o : $(mySRCdir)/ssyr2k.f
	$(F77) $(F77FLAGS) -o ssyr2k.o -c $(mySRCdir)/ssyr2k.f
ssyrk.o : $(mySRCdir)/ssyrk.f
	$(F77) $(F77FLAGS) -o ssyrk.o -c $(mySRCdir)/ssyrk.f
strmm.o : $(mySRCdir)/strmm.f
	$(F77) $(F77FLAGS) -o strmm.o -c $(mySRCdir)/strmm.f
strmv.o : $(mySRCdir)/strmv.f
	$(F77) $(F77FLAGS) -o strmv.o -c $(mySRCdir)/strmv.f
strsm.o : $(mySRCdir)/strsm.f
	$(F77) $(F77FLAGS) -o strsm.o -c $(mySRCdir)/strsm.f
strsv.o : $(mySRCdir)/strsv.f
	$(F77) $(F77FLAGS) -o strsv.o -c $(mySRCdir)/strsv.f
sgemm.o : $(mySRCdir)/sgemm.f
	$(F77) $(F77FLAGS) -o sgemm.o -c $(mySRCdir)/sgemm.f
ssymm.o : $(mySRCdir)/ssymm.f
	$(F77) $(F77FLAGS) -o ssymm.o -c $(mySRCdir)/ssymm.f
snrm2.o : $(mySRCdir)/snrm2.f
	$(F77) $(F77FLAGS) -o snrm2.o -c $(mySRCdir)/snrm2.f
sasum.o : $(mySRCdir)/sasum.f
	$(F77) $(F77FLAGS) -o sasum.o -c $(mySRCdir)/sasum.f
sdot.o : $(mySRCdir)/sdot.f
	$(F77) $(F77FLAGS) -o sdot.o -c $(mySRCdir)/sdot.f
srot.o : $(mySRCdir)/srot.f
	$(F77) $(F77FLAGS) -o srot.o -c $(mySRCdir)/srot.f
srotm.o : $(mySRCdir)/srotm.f
	$(F77) $(F77FLAGS) -o srotm.o -c $(mySRCdir)/srotm.f
srotmg.o : $(mySRCdir)/srotmg.f
	$(F77) $(F77FLAGS) -o srotmg.o -c $(mySRCdir)/srotmg.f
sger.o : $(mySRCdir)/sger.f
	$(F77) $(F77FLAGS) -o sger.o -c $(mySRCdir)/sger.f
ssbmv.o : $(mySRCdir)/ssbmv.f
	$(F77) $(F77FLAGS) -o ssbmv.o -c $(mySRCdir)/ssbmv.f
sspmv.o : $(mySRCdir)/sspmv.f
	$(F77) $(F77FLAGS) -o sspmv.o -c $(mySRCdir)/sspmv.f
sspr2.o : $(mySRCdir)/sspr2.f
	$(F77) $(F77FLAGS) -o sspr2.o -c $(mySRCdir)/sspr2.f
sspr.o : $(mySRCdir)/sspr.f
	$(F77) $(F77FLAGS) -o sspr.o -c $(mySRCdir)/sspr.f
ssymv.o : $(mySRCdir)/ssymv.f
	$(F77) $(F77FLAGS) -o ssymv.o -c $(mySRCdir)/ssymv.f
ssyr.o : $(mySRCdir)/ssyr.f
	$(F77) $(F77FLAGS) -o ssyr.o -c $(mySRCdir)/ssyr.f
ssyr2.o : $(mySRCdir)/ssyr2.f
	$(F77) $(F77FLAGS) -o ssyr2.o -c $(mySRCdir)/ssyr2.f
sdsdot.o : $(mySRCdir)/sdsdot.f
	$(F77) $(F77FLAGS) -o sdsdot.o -c $(mySRCdir)/sdsdot.f
daxpy.o : $(mySRCdir)/daxpy.f
	$(F77) $(F77FLAGS) -o daxpy.o -c $(mySRCdir)/daxpy.f
dcopy.o : $(mySRCdir)/dcopy.f
	$(F77) $(F77FLAGS) -o dcopy.o -c $(mySRCdir)/dcopy.f
dscal.o : $(mySRCdir)/dscal.f
	$(F77) $(F77FLAGS) -o dscal.o -c $(mySRCdir)/dscal.f
dswap.o : $(mySRCdir)/dswap.f
	$(F77) $(F77FLAGS) -o dswap.o -c $(mySRCdir)/dswap.f
drotg.o : $(mySRCdir)/drotg.f
	$(F77) $(F77FLAGS) -o drotg.o -c $(mySRCdir)/drotg.f
idamax.o : $(mySRCdir)/idamax.f
	$(F77) $(F77FLAGS) -o idamax.o -c $(mySRCdir)/idamax.f
dgbmv.o : $(mySRCdir)/dgbmv.f
	$(F77) $(F77FLAGS) -o dgbmv.o -c $(mySRCdir)/dgbmv.f
dtbmv.o : $(mySRCdir)/dtbmv.f
	$(F77) $(F77FLAGS) -o dtbmv.o -c $(mySRCdir)/dtbmv.f
dtbsv.o : $(mySRCdir)/dtbsv.f
	$(F77) $(F77FLAGS) -o dtbsv.o -c $(mySRCdir)/dtbsv.f
dtpmv.o : $(mySRCdir)/dtpmv.f
	$(F77) $(F77FLAGS) -o dtpmv.o -c $(mySRCdir)/dtpmv.f
dtpsv.o : $(mySRCdir)/dtpsv.f
	$(F77) $(F77FLAGS) -o dtpsv.o -c $(mySRCdir)/dtpsv.f
dgemv.o : $(mySRCdir)/dgemv.f
	$(F77) $(F77FLAGS) -o dgemv.o -c $(mySRCdir)/dgemv.f
dsyr2k.o : $(mySRCdir)/dsyr2k.f
	$(F77) $(F77FLAGS) -o dsyr2k.o -c $(mySRCdir)/dsyr2k.f
dsyrk.o : $(mySRCdir)/dsyrk.f
	$(F77) $(F77FLAGS) -o dsyrk.o -c $(mySRCdir)/dsyrk.f
dtrmm.o : $(mySRCdir)/dtrmm.f
	$(F77) $(F77FLAGS) -o dtrmm.o -c $(mySRCdir)/dtrmm.f
dtrmv.o : $(mySRCdir)/dtrmv.f
	$(F77) $(F77FLAGS) -o dtrmv.o -c $(mySRCdir)/dtrmv.f
dtrsm.o : $(mySRCdir)/dtrsm.f
	$(F77) $(F77FLAGS) -o dtrsm.o -c $(mySRCdir)/dtrsm.f
dtrsv.o : $(mySRCdir)/dtrsv.f
	$(F77) $(F77FLAGS) -o dtrsv.o -c $(mySRCdir)/dtrsv.f
dgemm.o : $(mySRCdir)/dgemm.f
	$(F77) $(F77FLAGS) -o dgemm.o -c $(mySRCdir)/dgemm.f
dsymm.o : $(mySRCdir)/dsymm.f
	$(F77) $(F77FLAGS) -o dsymm.o -c $(mySRCdir)/dsymm.f
dnrm2.o : $(mySRCdir)/dnrm2.f
	$(F77) $(F77FLAGS) -o dnrm2.o -c $(mySRCdir)/dnrm2.f
dasum.o : $(mySRCdir)/dasum.f
	$(F77) $(F77FLAGS) -o dasum.o -c $(mySRCdir)/dasum.f
ddot.o : $(mySRCdir)/ddot.f
	$(F77) $(F77FLAGS) -o ddot.o -c $(mySRCdir)/ddot.f
drot.o : $(mySRCdir)/drot.f
	$(F77) $(F77FLAGS) -o drot.o -c $(mySRCdir)/drot.f
drotm.o : $(mySRCdir)/drotm.f
	$(F77) $(F77FLAGS) -o drotm.o -c $(mySRCdir)/drotm.f
drotmg.o : $(mySRCdir)/drotmg.f
	$(F77) $(F77FLAGS) -o drotmg.o -c $(mySRCdir)/drotmg.f
dger.o : $(mySRCdir)/dger.f
	$(F77) $(F77FLAGS) -o dger.o -c $(mySRCdir)/dger.f
dsbmv.o : $(mySRCdir)/dsbmv.f
	$(F77) $(F77FLAGS) -o dsbmv.o -c $(mySRCdir)/dsbmv.f
dspmv.o : $(mySRCdir)/dspmv.f
	$(F77) $(F77FLAGS) -o dspmv.o -c $(mySRCdir)/dspmv.f
dspr2.o : $(mySRCdir)/dspr2.f
	$(F77) $(F77FLAGS) -o dspr2.o -c $(mySRCdir)/dspr2.f
dspr.o : $(mySRCdir)/dspr.f
	$(F77) $(F77FLAGS) -o dspr.o -c $(mySRCdir)/dspr.f
dsymv.o : $(mySRCdir)/dsymv.f
	$(F77) $(F77FLAGS) -o dsymv.o -c $(mySRCdir)/dsymv.f
dsyr.o : $(mySRCdir)/dsyr.f
	$(F77) $(F77FLAGS) -o dsyr.o -c $(mySRCdir)/dsyr.f
dsyr2.o : $(mySRCdir)/dsyr2.f
	$(F77) $(F77FLAGS) -o dsyr2.o -c $(mySRCdir)/dsyr2.f
dsdot.o : $(mySRCdir)/dsdot.f
	$(F77) $(F77FLAGS) -o dsdot.o -c $(mySRCdir)/dsdot.f
caxpy.o : $(mySRCdir)/caxpy.f
	$(F77) $(F77FLAGS) -o caxpy.o -c $(mySRCdir)/caxpy.f
ccopy.o : $(mySRCdir)/ccopy.f
	$(F77) $(F77FLAGS) -o ccopy.o -c $(mySRCdir)/ccopy.f
cscal.o : $(mySRCdir)/cscal.f
	$(F77) $(F77FLAGS) -o cscal.o -c $(mySRCdir)/cscal.f
cswap.o : $(mySRCdir)/cswap.f
	$(F77) $(F77FLAGS) -o cswap.o -c $(mySRCdir)/cswap.f
crotg.o : $(mySRCdir)/crotg.f
	$(F77) $(F77FLAGS) -o crotg.o -c $(mySRCdir)/crotg.f
icamax.o : $(mySRCdir)/icamax.f
	$(F77) $(F77FLAGS) -o icamax.o -c $(mySRCdir)/icamax.f
cgbmv.o : $(mySRCdir)/cgbmv.f
	$(F77) $(F77FLAGS) -o cgbmv.o -c $(mySRCdir)/cgbmv.f
ctbmv.o : $(mySRCdir)/ctbmv.f
	$(F77) $(F77FLAGS) -o ctbmv.o -c $(mySRCdir)/ctbmv.f
ctbsv.o : $(mySRCdir)/ctbsv.f
	$(F77) $(F77FLAGS) -o ctbsv.o -c $(mySRCdir)/ctbsv.f
ctpmv.o : $(mySRCdir)/ctpmv.f
	$(F77) $(F77FLAGS) -o ctpmv.o -c $(mySRCdir)/ctpmv.f
ctpsv.o : $(mySRCdir)/ctpsv.f
	$(F77) $(F77FLAGS) -o ctpsv.o -c $(mySRCdir)/ctpsv.f
cgemv.o : $(mySRCdir)/cgemv.f
	$(F77) $(F77FLAGS) -o cgemv.o -c $(mySRCdir)/cgemv.f
csyr2k.o : $(mySRCdir)/csyr2k.f
	$(F77) $(F77FLAGS) -o csyr2k.o -c $(mySRCdir)/csyr2k.f
csyrk.o : $(mySRCdir)/csyrk.f
	$(F77) $(F77FLAGS) -o csyrk.o -c $(mySRCdir)/csyrk.f
ctrmm.o : $(mySRCdir)/ctrmm.f
	$(F77) $(F77FLAGS) -o ctrmm.o -c $(mySRCdir)/ctrmm.f
ctrmv.o : $(mySRCdir)/ctrmv.f
	$(F77) $(F77FLAGS) -o ctrmv.o -c $(mySRCdir)/ctrmv.f
ctrsm.o : $(mySRCdir)/ctrsm.f
	$(F77) $(F77FLAGS) -o ctrsm.o -c $(mySRCdir)/ctrsm.f
ctrsv.o : $(mySRCdir)/ctrsv.f
	$(F77) $(F77FLAGS) -o ctrsv.o -c $(mySRCdir)/ctrsv.f
cgemm.o : $(mySRCdir)/cgemm.f
	$(F77) $(F77FLAGS) -o cgemm.o -c $(mySRCdir)/cgemm.f
csymm.o : $(mySRCdir)/csymm.f
	$(F77) $(F77FLAGS) -o csymm.o -c $(mySRCdir)/csymm.f
scasum.o : $(mySRCdir)/scasum.f
	$(F77) $(F77FLAGS) -o scasum.o -c $(mySRCdir)/scasum.f
scnrm2.o : $(mySRCdir)/scnrm2.f
	$(F77) $(F77FLAGS) -o scnrm2.o -c $(mySRCdir)/scnrm2.f
cdotu.o : $(mySRCdir)/cdotu.f
	$(F77) $(F77FLAGS) -o cdotu.o -c $(mySRCdir)/cdotu.f
cdotc.o : $(mySRCdir)/cdotc.f
	$(F77) $(F77FLAGS) -o cdotc.o -c $(mySRCdir)/cdotc.f
csscal.o : $(mySRCdir)/csscal.f
	$(F77) $(F77FLAGS) -o csscal.o -c $(mySRCdir)/csscal.f
csrot.o : $(mySRCdir)/csrot.f
	$(F77) $(F77FLAGS) -o csrot.o -c $(mySRCdir)/csrot.f
scabs1.o : $(mySRCdir)/scabs1.f
	$(F77) $(F77FLAGS) -o scabs1.o -c $(mySRCdir)/scabs1.f
cgerc.o : $(mySRCdir)/cgerc.f
	$(F77) $(F77FLAGS) -o cgerc.o -c $(mySRCdir)/cgerc.f
cgeru.o : $(mySRCdir)/cgeru.f
	$(F77) $(F77FLAGS) -o cgeru.o -c $(mySRCdir)/cgeru.f
chbmv.o : $(mySRCdir)/chbmv.f
	$(F77) $(F77FLAGS) -o chbmv.o -c $(mySRCdir)/chbmv.f
chpmv.o : $(mySRCdir)/chpmv.f
	$(F77) $(F77FLAGS) -o chpmv.o -c $(mySRCdir)/chpmv.f
chpr.o : $(mySRCdir)/chpr.f
	$(F77) $(F77FLAGS) -o chpr.o -c $(mySRCdir)/chpr.f
chpr2.o : $(mySRCdir)/chpr2.f
	$(F77) $(F77FLAGS) -o chpr2.o -c $(mySRCdir)/chpr2.f
chemv.o : $(mySRCdir)/chemv.f
	$(F77) $(F77FLAGS) -o chemv.o -c $(mySRCdir)/chemv.f
cher2.o : $(mySRCdir)/cher2.f
	$(F77) $(F77FLAGS) -o cher2.o -c $(mySRCdir)/cher2.f
cher2k.o : $(mySRCdir)/cher2k.f
	$(F77) $(F77FLAGS) -o cher2k.o -c $(mySRCdir)/cher2k.f
cher.o : $(mySRCdir)/cher.f
	$(F77) $(F77FLAGS) -o cher.o -c $(mySRCdir)/cher.f
cherk.o : $(mySRCdir)/cherk.f
	$(F77) $(F77FLAGS) -o cherk.o -c $(mySRCdir)/cherk.f
chemm.o : $(mySRCdir)/chemm.f
	$(F77) $(F77FLAGS) -o chemm.o -c $(mySRCdir)/chemm.f
zaxpy.o : $(mySRCdir)/zaxpy.f
	$(F77) $(F77FLAGS) -o zaxpy.o -c $(mySRCdir)/zaxpy.f
zcopy.o : $(mySRCdir)/zcopy.f
	$(F77) $(F77FLAGS) -o zcopy.o -c $(mySRCdir)/zcopy.f
zscal.o : $(mySRCdir)/zscal.f
	$(F77) $(F77FLAGS) -o zscal.o -c $(mySRCdir)/zscal.f
zswap.o : $(mySRCdir)/zswap.f
	$(F77) $(F77FLAGS) -o zswap.o -c $(mySRCdir)/zswap.f
zrotg.o : $(mySRCdir)/zrotg.f
	$(F77) $(F77FLAGS) -o zrotg.o -c $(mySRCdir)/zrotg.f
izamax.o : $(mySRCdir)/izamax.f
	$(F77) $(F77FLAGS) -o izamax.o -c $(mySRCdir)/izamax.f
zgbmv.o : $(mySRCdir)/zgbmv.f
	$(F77) $(F77FLAGS) -o zgbmv.o -c $(mySRCdir)/zgbmv.f
ztbmv.o : $(mySRCdir)/ztbmv.f
	$(F77) $(F77FLAGS) -o ztbmv.o -c $(mySRCdir)/ztbmv.f
ztbsv.o : $(mySRCdir)/ztbsv.f
	$(F77) $(F77FLAGS) -o ztbsv.o -c $(mySRCdir)/ztbsv.f
ztpmv.o : $(mySRCdir)/ztpmv.f
	$(F77) $(F77FLAGS) -o ztpmv.o -c $(mySRCdir)/ztpmv.f
ztpsv.o : $(mySRCdir)/ztpsv.f
	$(F77) $(F77FLAGS) -o ztpsv.o -c $(mySRCdir)/ztpsv.f
zgemv.o : $(mySRCdir)/zgemv.f
	$(F77) $(F77FLAGS) -o zgemv.o -c $(mySRCdir)/zgemv.f
zsyr2k.o : $(mySRCdir)/zsyr2k.f
	$(F77) $(F77FLAGS) -o zsyr2k.o -c $(mySRCdir)/zsyr2k.f
zsyrk.o : $(mySRCdir)/zsyrk.f
	$(F77) $(F77FLAGS) -o zsyrk.o -c $(mySRCdir)/zsyrk.f
ztrmm.o : $(mySRCdir)/ztrmm.f
	$(F77) $(F77FLAGS) -o ztrmm.o -c $(mySRCdir)/ztrmm.f
ztrmv.o : $(mySRCdir)/ztrmv.f
	$(F77) $(F77FLAGS) -o ztrmv.o -c $(mySRCdir)/ztrmv.f
ztrsm.o : $(mySRCdir)/ztrsm.f
	$(F77) $(F77FLAGS) -o ztrsm.o -c $(mySRCdir)/ztrsm.f
ztrsv.o : $(mySRCdir)/ztrsv.f
	$(F77) $(F77FLAGS) -o ztrsv.o -c $(mySRCdir)/ztrsv.f
zgemm.o : $(mySRCdir)/zgemm.f
	$(F77) $(F77FLAGS) -o zgemm.o -c $(mySRCdir)/zgemm.f
zsymm.o : $(mySRCdir)/zsymm.f
	$(F77) $(F77FLAGS) -o zsymm.o -c $(mySRCdir)/zsymm.f
dzasum.o : $(mySRCdir)/dzasum.f
	$(F77) $(F77FLAGS) -o dzasum.o -c $(mySRCdir)/dzasum.f
dznrm2.o : $(mySRCdir)/dznrm2.f
	$(F77) $(F77FLAGS) -o dznrm2.o -c $(mySRCdir)/dznrm2.f
zdotu.o : $(mySRCdir)/zdotu.f
	$(F77) $(F77FLAGS) -o zdotu.o -c $(mySRCdir)/zdotu.f
zdotc.o : $(mySRCdir)/zdotc.f
	$(F77) $(F77FLAGS) -o zdotc.o -c $(mySRCdir)/zdotc.f
zdscal.o : $(mySRCdir)/zdscal.f
	$(F77) $(F77FLAGS) -o zdscal.o -c $(mySRCdir)/zdscal.f
zdrot.o : $(mySRCdir)/zdrot.f
	$(F77) $(F77FLAGS) -o zdrot.o -c $(mySRCdir)/zdrot.f
dcabs1.o : $(mySRCdir)/dcabs1.f
	$(F77) $(F77FLAGS) -o dcabs1.o -c $(mySRCdir)/dcabs1.f
zgerc.o : $(mySRCdir)/zgerc.f
	$(F77) $(F77FLAGS) -o zgerc.o -c $(mySRCdir)/zgerc.f
zgeru.o : $(mySRCdir)/zgeru.f
	$(F77) $(F77FLAGS) -o zgeru.o -c $(mySRCdir)/zgeru.f
zhbmv.o : $(mySRCdir)/zhbmv.f
	$(F77) $(F77FLAGS) -o zhbmv.o -c $(mySRCdir)/zhbmv.f
zhpmv.o : $(mySRCdir)/zhpmv.f
	$(F77) $(F77FLAGS) -o zhpmv.o -c $(mySRCdir)/zhpmv.f
zhpr.o : $(mySRCdir)/zhpr.f
	$(F77) $(F77FLAGS) -o zhpr.o -c $(mySRCdir)/zhpr.f
zhpr2.o : $(mySRCdir)/zhpr2.f
	$(F77) $(F77FLAGS) -o zhpr2.o -c $(mySRCdir)/zhpr2.f
zhemv.o : $(mySRCdir)/zhemv.f
	$(F77) $(F77FLAGS) -o zhemv.o -c $(mySRCdir)/zhemv.f
zher2.o : $(mySRCdir)/zher2.f
	$(F77) $(F77FLAGS) -o zher2.o -c $(mySRCdir)/zher2.f
zher2k.o : $(mySRCdir)/zher2k.f
	$(F77) $(F77FLAGS) -o zher2k.o -c $(mySRCdir)/zher2k.f
zher.o : $(mySRCdir)/zher.f
	$(F77) $(F77FLAGS) -o zher.o -c $(mySRCdir)/zher.f
zherk.o : $(mySRCdir)/zherk.f
	$(F77) $(F77FLAGS) -o zherk.o -c $(mySRCdir)/zherk.f
zhemm.o : $(mySRCdir)/zhemm.f
	$(F77) $(F77FLAGS) -o zhemm.o -c $(mySRCdir)/zhemm.f
