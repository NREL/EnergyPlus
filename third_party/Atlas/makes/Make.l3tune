include Make.inc
mySRCdir = $(SRCdir)/tune/blas/level3

$(SYSdir)/time.o:
	cd $(SYSdir) ; $(MAKE) time.o

$(INCAdir)/atlas_type.h : 
	cd $(SYSdir) ; $(MAKE) RunTyp

$(SYSdir)/res/L1CacheSize :
	rm -f res/L1CacheSize
	cd $(SYSdir) ; $(MAKE) res/L1CacheSize
	
RunL1 : force_build
	$(MAKE) $(SYSdir)/res/L1CacheSize 
	ln -s $(SYSdir)/res/L1CacheSize res/L1CacheSize

RunTLB : 
	cd $(SYSdir) ; $(MAKE) RunTLB

$(SYSdir)/res/$(pre)MULADD : 
	rm -f res/$(pre)MULADD
	cd $(SYSdir) ; $(MAKE) res/$(pre)MULADD pre=$(pre) 
RunMulAdd: $(SYSdir)/res/$(pre)MULADD
	ln -s $(SYSdir)/res/$(pre)MULADD res/$(pre)MULADD

RunTyp: force_build
	cd $(SYSdir) ; $(MAKE) RunTyp



side=Left_
uplo=Upper_
trans=NoTranspose_
diag=NonUnitDiag_

force_build :

res/atlas_trsmNB.h : xgen_trsmnb \
                     res/sTRSM_NB res/dTRSM_NB res/cTRSM_NB res/zTRSM_NB
	./xgen_trsmnb
	rm -f $(INCAdir)/atlas_trsmNB.h
	cp res/atlas_trsmNB.h $(INCAdir)/.
xgen_trsmnb : $(mySRCdir)/gen_trsmnb.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/gen_trsmnb.c

res/sTRSM_NB :
	$(MAKE) xsinvtrsm
	$(ATLRUN) $(L3Tdir) xsinvtrsm
xsinvtrsm : sinvtrsm.o ATL_strsm.o 
	$(MAKE) sbindep
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtrsm.o ATL_strsm.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(ATLASlib) $(LIBS)
sbindep :
	cd $(BINdir) ; $(MAKE) slapack ststlib sl3lib sl2lib \
                            sl1lib scblas
sinvtrsm.o : $(mySRCdir)/invtrsm.c $(INCSdir)/atlas_misc.h $(INCAdir)/smm.h \
                  $(INCSdir)/atlas_lvl3.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DATLCINT -DTimeC -DSREAL $(mySRCdir)/invtrsm.c
ATL_strsm.o : $(mySRCdir)/ATL_trsm.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_trsm.c

res/dTRSM_NB :
	$(MAKE) xdinvtrsm
	$(ATLRUN) $(L3Tdir) xdinvtrsm
xdinvtrsm : dinvtrsm.o ATL_dtrsm.o 
	$(MAKE) dbindep
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtrsm.o ATL_dtrsm.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(ATLASlib) $(LIBS)
dbindep :
	cd $(BINdir) ; $(MAKE) dlapack dtstlib dl3lib dl2lib \
                            dl1lib dcblas
dinvtrsm.o : $(mySRCdir)/invtrsm.c $(INCSdir)/atlas_misc.h $(INCAdir)/dmm.h \
                  $(INCSdir)/atlas_lvl3.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DATLCINT -DTimeC -DDREAL $(mySRCdir)/invtrsm.c
ATL_dtrsm.o : $(mySRCdir)/ATL_trsm.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_trsm.c

res/cTRSM_NB :
	$(MAKE) xcinvtrsm
	$(ATLRUN) $(L3Tdir) xcinvtrsm
xcinvtrsm : cinvtrsm.o ATL_ctrsm.o 
	$(MAKE) cbindep
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtrsm.o ATL_ctrsm.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(ATLASlib) $(LIBS)
cbindep :
	cd $(BINdir) ; $(MAKE) clapack ctstlib cl3lib cl2lib \
                            cl1lib ccblas
cinvtrsm.o : $(mySRCdir)/invtrsm.c $(INCSdir)/atlas_misc.h $(INCAdir)/cmm.h \
                  $(INCSdir)/atlas_lvl3.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DATLCINT -DTimeC -DSCPLX $(mySRCdir)/invtrsm.c
ATL_ctrsm.o : $(mySRCdir)/ATL_trsm.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_trsm.c

res/zTRSM_NB :
	$(MAKE) xzinvtrsm
	$(ATLRUN) $(L3Tdir) xzinvtrsm
xzinvtrsm : zinvtrsm.o ATL_ztrsm.o 
	$(MAKE) zbindep
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtrsm.o ATL_ztrsm.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(ATLASlib) $(LIBS)
zbindep :
	cd $(BINdir) ; $(MAKE) zlapack ztstlib zl3lib zl2lib \
                            zl1lib zcblas
zinvtrsm.o : $(mySRCdir)/invtrsm.c $(INCSdir)/atlas_misc.h $(INCAdir)/zmm.h \
                  $(INCSdir)/atlas_lvl3.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DATLCINT -DTimeC -DDCPLX $(mySRCdir)/invtrsm.c
ATL_ztrsm.o : $(mySRCdir)/ATL_trsm.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_trsm.c


strsmXover = $(INCAdir)/atlas_strsmXover.h

$(strsmXover) : res/atlas_strsmXover.h

res/atlas_strsmXover.h :
	$(MAKE) stsmfc
	cp $(strsmXover) res/.

stsmfc :
	rm -f $(strsmXover)
	cd $(L3Bdir) ; $(MAKE) slib
	$(MAKE) xstsmfc2 pre=s typ=SREAL side=$(side) uplo=Upper_ \
                tran=NoTranspose_ diag=$(diag)
	$(MAKE) xstsmfc2 pre=s typ=SREAL side=$(side) uplo=Upper_ \
                tran=Transpose_ diag=$(diag)
	$(MAKE) xstsmfc2 pre=s typ=SREAL side=$(side) uplo=Lower_ \
                tran=NoTranspose_ diag=$(diag)
	$(MAKE) xstsmfc2 pre=s typ=SREAL side=$(side) uplo=Lower_ \
                tran=Transpose_ diag=$(diag)
	cd $(L3Bdir) ; $(MAKE) slib
dtrsmXover = $(INCAdir)/atlas_dtrsmXover.h

$(dtrsmXover) : res/atlas_dtrsmXover.h

res/atlas_dtrsmXover.h :
	$(MAKE) dtsmfc
	cp $(dtrsmXover) res/.

dtsmfc :
	rm -f $(dtrsmXover)
	cd $(L3Bdir) ; $(MAKE) dlib
	$(MAKE) xdtsmfc2 pre=d typ=DREAL side=$(side) uplo=Upper_ \
                tran=NoTranspose_ diag=$(diag)
	$(MAKE) xdtsmfc2 pre=d typ=DREAL side=$(side) uplo=Upper_ \
                tran=Transpose_ diag=$(diag)
	$(MAKE) xdtsmfc2 pre=d typ=DREAL side=$(side) uplo=Lower_ \
                tran=NoTranspose_ diag=$(diag)
	$(MAKE) xdtsmfc2 pre=d typ=DREAL side=$(side) uplo=Lower_ \
                tran=Transpose_ diag=$(diag)
	cd $(L3Bdir) ; $(MAKE) dlib
qtrsmXover = $(INCAdir)/atlas_qtrsmXover.h

$(qtrsmXover) : res/atlas_qtrsmXover.h

res/atlas_qtrsmXover.h :
	$(MAKE) qtsmfc
	cp $(qtrsmXover) res/.

qtsmfc :
	rm -f $(qtrsmXover)
	cd $(L3Bdir) ; $(MAKE) qlib
	$(MAKE) xqtsmfc2 pre=q typ=QREAL side=$(side) uplo=Upper_ \
                tran=NoTranspose_ diag=$(diag)
	$(MAKE) xqtsmfc2 pre=q typ=QREAL side=$(side) uplo=Upper_ \
                tran=Transpose_ diag=$(diag)
	$(MAKE) xqtsmfc2 pre=q typ=QREAL side=$(side) uplo=Lower_ \
                tran=NoTranspose_ diag=$(diag)
	$(MAKE) xqtsmfc2 pre=q typ=QREAL side=$(side) uplo=Lower_ \
                tran=Transpose_ diag=$(diag)
	cd $(L3Bdir) ; $(MAKE) qlib

$(pre)tsmfc.o : force_build
	$(KC) -o $(pre)tsmfc.o -c $(KCFLAGS) -D$(typ) $(mySRCdir)/tsmfc.c \
                 -D$(side) -D$(uplo) -D$(tran) -D$(diag)

x$(pre)tsmfc2 : force_build
	rm -f x$(pre)tsmfc
	$(MAKE) x$(pre)tsmfc pre=$(pre) typ=$(typ) side=$(side) uplo=$(uplo) \
                             tran=$(tran) diag=$(diag)
	$(ATLRUN) $(L3Tdir) x$(pre)tsmfc -f $(INCAdir)/atlas_$(pre)trsmXover.h

x$(pre)tsmfc : force_build $(INCAdir)/atlas_type.h $(SYSdir)/time.o \
               $(pre)tsmfc.o $(INCAdir)/atlas_$(pre)sysinfo.h
	$(KC) -o big_trsm.o -c $(KCFLAGS) -D$(typ) \
                 -D$(side) -D$(uplo) -D$(tran) -D$(diag) -DBIG_TRSM \
                 $(SRCdir)/src/blas/level3/kernel/ATL_trsmL.c
	$(KC) -o small_trsm.o -c $(KCFLAGS) -D$(typ) \
                 -D$(side) -D$(uplo) -D$(tran) -D$(diag) -DSMALL_TRSM \
                  $(SRCdir)/src/blas/level3/kernel/ATL_trsmL.c
	$(CLINKER) $(CLINKFLAGS) -o $@ $(pre)tsmfc.o $(SYSdir)/time.o \
                                      big_trsm.o small_trsm.o $(ATLASlib) \
                                      $(ATLASlib) $(LIBS)

clean : 
	rm -f *.o x?tsmfc

