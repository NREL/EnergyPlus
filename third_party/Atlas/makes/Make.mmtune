include Make.inc
mySRCdir = $(SRCdir)/tune/blas/gemm

default : xmmsearch
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




L3INCdep = $(INCSdir)/atlas_misc.h $(INCSdir)/atlas_lvl3.h \
           $(INCSdir)/atlas_f77.h $(INCAdir)/atlas_type.h
parsedeps = $(INCSdir)/atlas_mmparse.h $(INCSdir)/atlas_mmtesttime.h \
            $(INCSdir)/atlas_genparse.h $(INCSdir)/atlas_gentesttime.h
basf = /home/whaley/git/math-atlas/AtlasBase/Clint/atlas-mmg.base
extC = $(BINdir)/xextract -b $(basf) -langC
if = -1
nf = 0
ff = 0
beta = 1
ldc = $(nb)
regs = 32
nb = 28
mmrout=JIKNBmm

pre = d
muladd = 1
ta = t
tb = n
lat = 4
mu = 4
nu = 4
ku = 1
nb = 4
mb = $(nb)
kb = $(nb)
M = $(mb)
N = $(nb)
K = $(kb)
lda = $(kb)
ldb = $(kb)
ldc = 0
lda2 = $(lda)
ldb2 = $(ldb)
ldc2 = $(ldc)
csA = 1
csB = 1
csC = 1
alpha = 1
beta = 1
loopO = JIK
F0 = 16
FN = 0
moves = -DMoveA -DMoveB
cleanup = 0
casnam =
bnb = 28
mmrout = $(pre)mm0.c
mmlib =
pfA = 0
flagfile=ignore
srchflags=
casarg = pre=$(pre) ta=$(ta) tb=$(tb) muladd=$(muladd) lat=$(lat) \
         loopO=$(loopO) M=$(M) N=$(N) K=$(K) mb=$(mb) nb=$(nb) kb=$(kb) \
         mu=$(mu) nu=$(nu) ku=$(ku) lda=$(lda) ldb=$(ldb) ldc=$(ldc) \
         lda2=$(lda2) ldb2=$(ldb2) ldc2=$(ldc2) pfA=$(pfA) \
         csA=$(csA) csB=$(csB) csC=$(csC) alpha=$(alpha) beta=$(beta) \
         moves="$(moves)" cleanup=$(cleanup) mmrout="$(mmrout)" \
         SMC="$(SMC)" SMCFLAGS="$(SMCFLAGS)" DMC="$(DMC)" DMCFLAGS="$(DMCFLAGS)"
EXTSEARCHES = -e res/$(pre)MMKSSE.sum

RunMMDef : xmmsearch
	./xmmsearch -p $(pre)
RunGMMSearch : xgmmsearch
	./xgmmsearch -p $(pre)
RunUMMSearch : xummsearch
	./xummsearch -p $(pre) -n $(nb) $(EXTSEARCHES)
RunUMMClean : xummsearch
	./xummsearch -p $(pre) -n $(nb) -C $(which)
$(BINdir)/xextract :
	cd $(BINdir) ; $(MAKE) xextract
$(BINdir)/xsubstr:
	cd $(BINdir) ; $(MAKE) xsubstr
res/goodgcc.txt :
	echo "'$(GOODGCC)'" > res/goodgcc.txt
$(flagfile) : xuserflag $(BINdir)/xsubstr $(mySRCdir)/CASES/$(flagfile)
	- rm -f ATL_tMpFiLe
	$(BINdir)/xsubstr -o ATL_tMpFiLe -i $(mySRCdir)/CASES/$(flagfile) \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
	./xuserflag -i ATL_tMpFiLe -o $(flagfile)
	- rm -f ATL_tMpFiLe
FindFlagCases : xuserflag $(BINdir)/xsubstr
	- rm -f ATL_tMpFiLe
	$(BINdir)/xsubstr -o ATL_tMpFiLe -i $(mySRCdir)/CASES/$(pre)cases.flg \
           -s '^gcc' '$(GOODGCC)' -s ' gcc' ' $(GOODGCC)'
	./xuserflag -i ATL_tMpFiLe -o $(outfile)
	- rm -f ATL_tMpFiLe

xuserindex : $(INCAdir)/atlas_type.h userindex.o xusercomb $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ userindex.o
userindex.o : $(mySRCdir)/userindex.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/userindex.c
xuserflag : $(INCAdir)/atlas_type.h userflag.o   $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ userflag.o
userflag.o : $(mySRCdir)/userflag.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/userflag.c
xusercomb : $(INCAdir)/atlas_type.h usercomb.o   $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ usercomb.o
usercomb.o : $(mySRCdir)/usercomb.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/usercomb.c
xummsearch : $(INCAdir)/atlas_type.h ummsearch.o scases.dsc dcases.dsc ccases.dsc zcases.dsc $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ ummsearch.o
ummsearch.o : $(mySRCdir)/ummsearch.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/ummsearch.c
xgmmsearch : $(INCAdir)/atlas_type.h gmmsearch.o   $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ gmmsearch.o
gmmsearch.o : $(mySRCdir)/gmmsearch.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/gmmsearch.c
xmmcuncpsearch : $(INCAdir)/atlas_type.h mmcuncpsearch.o   $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ mmcuncpsearch.o
mmcuncpsearch.o : $(mySRCdir)/mmcuncpsearch.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/mmcuncpsearch.c
xmmflagsearch : $(INCAdir)/atlas_type.h mmflagsearch.o   $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ mmflagsearch.o
mmflagsearch.o : $(mySRCdir)/mmflagsearch.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/mmflagsearch.c
xmmsearch : $(INCAdir)/atlas_type.h mmsearch.o xmmcuncpsearch $(parsedeps)
	$(XCC) $(XCCFLAGS) -o $@ mmsearch.o
mmsearch.o : $(mySRCdir)/mmsearch.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/mmsearch.c

SSEGENOUTDIR :
	mkdir SSEGENOUTDIR
xmmgen_sse : $(mySRCdir)/mmgen_sse.c
	$(XCC) -o $@ $(mySRCdir)/mmgen_sse.c -lm
xmmksearch_sse : xmmgen_sse mmksearch_sse.o SSEGENOUTDIR
	$(XCC) $(XCCFLAGS) -o $@ mmksearch_sse.o
mmksearch_sse.o : $(mySRCdir)/mmksearch_sse.c $(parsedeps)
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/mmksearch_sse.c
xmmsearch_sse : $(INCAdir)/atlas_type.h $(INCSdir)/atlas_mmtesttime.h \
                $(INCSdir)/atlas_mmparse.h $(INCSdir)/atlas_genpars.h \
                mmsearch_sse.c
	$(XCC) $(XCCFLAGS) -o $@ mmsearch_sse.c

res/L1CacheSize :
	$(MAKE) RunL1

res/$(pre)MULADD : $(SYSdir)/res/$(pre)MULADD 

res/$(pre)bestTT_$(bnb)x$(bnb)x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestTT_0x0x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestTT_0x0x0 : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestTN_$(bnb)x$(bnb)x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestTN_0x0x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestTN_0x0x0 : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestNT_$(bnb)x$(bnb)x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestNT_0x0x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestNT_0x0x0 : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestNN_$(bnb)x$(bnb)x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestNN_0x0x$(bnb) : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)bestNN_0x0x0 : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
	./xemit_mm  -p $(pre) -R -2
	$(MAKE) install pre=$(pre)
res/$(pre)BEST : xmmsearch
	./xmmsearch -p $(pre)

res/$(pre)NCNB  : xmmsearch xemit_mm 
	./xmmsearch -p $(pre)
res/$(pre)Xover.h :
	$(MAKE) $(pre)Run_tfc pre=$(pre)

RunFindCE :
	$(MAKE) dRunFindCE

res/atlas_cacheedge.h :
	$(MAKE) $(pre)RunFindCE

	
res/atlas_zdNKB.h :
	$(MAKE) zdRunFindCE
res/atlas_csNKB.h :
	$(MAKE) csRunFindCE

xemit_vkb  : emit_vKB.o
	$(XCC) $(XCCFLAGS) -o xemit_vkb emit_vKB.o

xemit_mm  : emit_mm.o
	$(XCC) $(XCCFLAGS) -o xemit_mm emit_mm.o

xemit_reg : emit_reg.o
	$(XCC) $(XCCFLAGS) -o xemit_reg emit_reg.o

xemit_cmm : emit_cmm.o
	$(XCC) $(XCCFLAGS) -o xemit_cmm emit_cmm.o

xemit_fmm : emit_fmm.o
	$(XCC) $(XCCFLAGS) -o xemit_fmm emit_fmm.o
xhcsearch : hcsearch.o
	$(XCC) $(XCCFLAGS) -o xhcsearch hcsearch.o

RunHCtest : xhcsearch
	rm -f $(INCAdir)/$(pre)mmcase.h
	touch $(INCAdir)/$(pre)mmcase.h
	./xhcsearch -p $(pre) -n $(nb)
	mv res/$(pre)mmcase.h $(INCAdir)/.

$(pre)killclean :
	rm -f res/$(pre)Clean?
$(pre)killNC :
	rm -f res/$(pre)best*
$(pre)killMM : $(pre)killclean $(pre)killNC
	rm -f res/$(pre)MMRES res/$(pre)*MMRES.sum res/$(pre)*NB
xvmmf_gccsub : $(mySRCdir)/vmmf_gccsub.c
	$(XCC) $(XCCFLAGS) -o xvmmf_gccsub $(mySRCdir)/vmmf_gccsub.c

res/sMMKSSE.sum : xmmksearch_sse force_build
	./xmmksearch_sse -p s
res/svMMRES : res/svgMMRES xvnbmmsearch scases.vnb
	./xvnbmmsearch -p s
res/svgMMRES : res/sgMMRES
	$(MAKE) xmm_old2new
	./xmm_old2new -i res/sgMMRES -o res/svgMMRES

scases.vnb : $(mySRCdir)/CASES/scases.vnb xvmmf_gccsub
	cp $(mySRCdir)/CASES/scases.vnb scases.vnb
	./xvmmf_gccsub 'gcc' '$(GOODGCC)' scases.vnb
scases.dsc: xuserindex $(mySRCdir)/CASES/scases.0 $(mySRCdir)/CASES/scases.SSE \
                 $(mySRCdir)/CASES/scases.3DN $(mySRCdir)/CASES/scases.flg 
	./xuserindex -p s
res/snreg : xgmmsearch
	./xgmmsearch -p s -r -1
sRun_tfc : stfc
	$(ATLRUN) $(MMTdir) xstfc
	cp -f $(INCAdir)/sXover.h res/.

stfc :
	$(MAKE) xstfc pre=s typ=SREAL


sinstall : xemit_mm res/sMMRES.sum $(BINdir)/xccobj \
                res/sCleanM res/sCleanN res/sCleanK \
                res/suCleanMF res/suCleanNF res/suCleanKF 
	./xemit_mm  -p s -R -2
	mv -f smm.h $(INCAdir)/.
	mv -f atlas_sNCmm.h $(INCAdir)/.
	cd $(GMMdir)/KERNEL ; rm -f ATL_s*.o
	cd $(GMMdir)/KERNEL ; rm -f ATL_s*
	cd $(GMMdir)/KERNEL ; rm -f sMakefile xccobj
	cd KERNEL ; mv -f ATL_s*.c $(GMMdir)/KERNEL/.
	mv -f KERNEL/sMakefile $(GMMdir)/KERNEL/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMMdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMMdir)/. ; \
        fi
	$(MAKE) smmlib
	touch sinstall

res/sMMRES.sum :
	$(MAKE) xmmsearch
	./xmmsearch -p s
res/suCleanKF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3
res/suCleanNF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3
res/suCleanMF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3
res/sCleanK :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3
res/sCleanN :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3
res/sCleanM :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3
res/smmksearch_sse :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p s -R -3

sRunFindCE : xsfindCE
	$(ATLRUN) $(MMTdir) xsfindCE -f res/atlas_cacheedge.h
	cp -f res/atlas_cacheedge.h $(INCAdir)/atlas_cacheedge.h
sssegenf = CASES/sgenmm_sse.c
sgensseK : xmmgen_sse
	rm -f ssegen_b0.c ssegen_b1.c ssegen_bX.c $(sssegenf)
	./xmmgen_sse -p s -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                      -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                      $(ssegenflags) -beta 0 -f ssegen_b0.c
	./xmmgen_sse -p s -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 1 -f ssegen_b1.c
	./xmmgen_sse -p s -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 8 -f ssegen_bX.c
	echo "#ifdef BETA0" > $(sssegenf)
	cat ssegen_b0.c >> $(sssegenf)
	echo "#elif defined(BETAX)" >> $(sssegenf)
	cat ssegen_bX.c >> $(sssegenf)
	echo "#else" >> $(sssegenf)
	cat ssegen_b1.c >> $(sssegenf)
	echo "#endif" >> $(sssegenf)
res/dMMKSSE.sum : xmmksearch_sse force_build
	./xmmksearch_sse -p d
res/dvMMRES : res/dvgMMRES xvnbmmsearch dcases.vnb
	./xvnbmmsearch -p d
res/dvgMMRES : res/dgMMRES
	$(MAKE) xmm_old2new
	./xmm_old2new -i res/dgMMRES -o res/dvgMMRES

dcases.vnb : $(mySRCdir)/CASES/dcases.vnb xvmmf_gccsub
	cp $(mySRCdir)/CASES/dcases.vnb dcases.vnb
	./xvmmf_gccsub 'gcc' '$(GOODGCC)' dcases.vnb
dcases.dsc: xuserindex $(mySRCdir)/CASES/dcases.0 $(mySRCdir)/CASES/dcases.SSE \
                 $(mySRCdir)/CASES/dcases.3DN $(mySRCdir)/CASES/dcases.flg 
	./xuserindex -p d
res/dnreg : xgmmsearch
	./xgmmsearch -p d -r -1
dRun_tfc : dtfc
	$(ATLRUN) $(MMTdir) xdtfc
	cp -f $(INCAdir)/dXover.h res/.

dtfc :
	$(MAKE) xdtfc pre=d typ=DREAL


dinstall : xemit_mm res/dMMRES.sum $(BINdir)/xccobj \
                res/dCleanM res/dCleanN res/dCleanK \
                res/duCleanMF res/duCleanNF res/duCleanKF 
	./xemit_mm  -p d -R -2
	mv -f dmm.h $(INCAdir)/.
	mv -f atlas_dNCmm.h $(INCAdir)/.
	cd $(GMMdir)/KERNEL ; rm -f ATL_d*.o
	cd $(GMMdir)/KERNEL ; rm -f ATL_d*
	cd $(GMMdir)/KERNEL ; rm -f dMakefile xccobj
	cd KERNEL ; mv -f ATL_d*.c $(GMMdir)/KERNEL/.
	mv -f KERNEL/dMakefile $(GMMdir)/KERNEL/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMMdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMMdir)/. ; \
        fi
	$(MAKE) dmmlib
	touch dinstall

res/dMMRES.sum :
	$(MAKE) xmmsearch
	./xmmsearch -p d
res/duCleanKF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p d -R -3
res/duCleanNF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p d -R -3
res/duCleanMF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p d -R -3
res/dCleanK :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p d -R -3
res/dCleanN :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p d -R -3
res/dCleanM :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p d -R -3

dRunFindCE : xdfindCE
	$(ATLRUN) $(MMTdir) xdfindCE -f res/atlas_cacheedge.h
	cp -f res/atlas_cacheedge.h $(INCAdir)/atlas_cacheedge.h
dssegenf = CASES/dgenmm_sse.c
dgensseK : xmmgen_sse
	rm -f ssegen_b0.c ssegen_b1.c ssegen_bX.c $(dssegenf)
	./xmmgen_sse -p d -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                      -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                      $(ssegenflags) -beta 0 -f ssegen_b0.c
	./xmmgen_sse -p d -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 1 -f ssegen_b1.c
	./xmmgen_sse -p d -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 8 -f ssegen_bX.c
	echo "#ifdef BETA0" > $(dssegenf)
	cat ssegen_b0.c >> $(dssegenf)
	echo "#elif defined(BETAX)" >> $(dssegenf)
	cat ssegen_bX.c >> $(dssegenf)
	echo "#else" >> $(dssegenf)
	cat ssegen_b1.c >> $(dssegenf)
	echo "#endif" >> $(dssegenf)
res/qMMKSSE.sum : xmmksearch_sse force_build
	./xmmksearch_sse -p q
res/qvMMRES : res/qvgMMRES xvnbmmsearch qcases.vnb
	./xvnbmmsearch -p q
res/qvgMMRES : res/qgMMRES
	$(MAKE) xmm_old2new
	./xmm_old2new -i res/qgMMRES -o res/qvgMMRES

qcases.vnb : $(mySRCdir)/CASES/qcases.vnb xvmmf_gccsub
	cp $(mySRCdir)/CASES/qcases.vnb qcases.vnb
	./xvmmf_gccsub 'gcc' '$(GOODGCC)' qcases.vnb
qcases.dsc: xuserindex $(mySRCdir)/CASES/qcases.0 $(mySRCdir)/CASES/qcases.SSE \
                 $(mySRCdir)/CASES/qcases.3DN $(mySRCdir)/CASES/qcases.flg 
	./xuserindex -p q
res/qnreg : xgmmsearch
	./xgmmsearch -p q -r -1
qRun_tfc : qtfc
	$(ATLRUN) $(MMTdir) xqtfc
	cp -f $(INCAdir)/qXover.h res/.

qtfc :
	$(MAKE) xqtfc pre=q typ=QREAL


qinstall : xemit_mm res/qMMRES.sum $(BINdir)/xccobj \
                res/qCleanM res/qCleanN res/qCleanK \
                res/quCleanMF res/quCleanNF res/quCleanKF 
	./xemit_mm  -p q -R -2
	mv -f qmm.h $(INCAdir)/.
	mv -f atlas_qNCmm.h $(INCAdir)/.
	cd $(GMMdir)/KERNEL ; rm -f ATL_q*.o
	cd $(GMMdir)/KERNEL ; rm -f ATL_q*
	cd $(GMMdir)/KERNEL ; rm -f qMakefile xccobj
	cd KERNEL ; mv -f ATL_q*.c $(GMMdir)/KERNEL/.
	mv -f KERNEL/qMakefile $(GMMdir)/KERNEL/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMMdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMMdir)/. ; \
        fi
	$(MAKE) qmmlib
	touch qinstall

res/qMMRES.sum :
	$(MAKE) xmmsearch
	./xmmsearch -p q
res/quCleanKF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p q -R -3
res/quCleanNF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p q -R -3
res/quCleanMF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p q -R -3
res/qCleanK :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p q -R -3
res/qCleanN :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p q -R -3
res/qCleanM :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p q -R -3

qRunFindCE : xqfindCE
	$(ATLRUN) $(MMTdir) xqfindCE -f res/atlas_cacheedge.h
	cp -f res/atlas_cacheedge.h $(INCAdir)/atlas_cacheedge.h
qssegenf = CASES/qgenmm_sse.c
qgensseK : xmmgen_sse
	rm -f ssegen_b0.c ssegen_b1.c ssegen_bX.c $(qssegenf)
	./xmmgen_sse -p q -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                      -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                      $(ssegenflags) -beta 0 -f ssegen_b0.c
	./xmmgen_sse -p q -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 1 -f ssegen_b1.c
	./xmmgen_sse -p q -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 8 -f ssegen_bX.c
	echo "#ifdef BETA0" > $(qssegenf)
	cat ssegen_b0.c >> $(qssegenf)
	echo "#elif defined(BETAX)" >> $(qssegenf)
	cat ssegen_bX.c >> $(qssegenf)
	echo "#else" >> $(qssegenf)
	cat ssegen_b1.c >> $(qssegenf)
	echo "#endif" >> $(qssegenf)
res/cMMKSSE.sum : xmmksearch_sse force_build
	./xmmksearch_sse -p c
res/cvMMRES : res/cvgMMRES xvnbmmsearch ccases.vnb
	./xvnbmmsearch -p c
res/cvgMMRES : res/cgMMRES
	$(MAKE) xmm_old2new
	./xmm_old2new -i res/cgMMRES -o res/cvgMMRES

ccases.vnb : $(mySRCdir)/CASES/ccases.vnb xvmmf_gccsub
	cp $(mySRCdir)/CASES/ccases.vnb ccases.vnb
	./xvmmf_gccsub 'gcc' '$(GOODGCC)' ccases.vnb
ccases.dsc: xuserindex $(mySRCdir)/CASES/ccases.0 $(mySRCdir)/CASES/ccases.SSE \
                 $(mySRCdir)/CASES/ccases.3DN $(mySRCdir)/CASES/ccases.flg 
	./xuserindex -p c
res/cnreg : xgmmsearch
	./xgmmsearch -p c -r -1
cRun_tfc : ctfc
	$(ATLRUN) $(MMTdir) xctfc
	cp -f $(INCAdir)/cXover.h res/.

ctfc :
	$(MAKE) xctfc pre=c typ=SCPLX


cinstall : xemit_mm res/cMMRES.sum $(BINdir)/xccobj \
                res/cCleanM res/cCleanN res/cCleanK \
                res/cuCleanMF res/cuCleanNF res/cuCleanKF 
	./xemit_mm  -p c -R -2
	mv -f cmm.h $(INCAdir)/.
	mv -f atlas_cNCmm.h $(INCAdir)/.
	cd $(GMMdir)/KERNEL ; rm -f ATL_c*.o
	cd $(GMMdir)/KERNEL ; rm -f ATL_c*
	cd $(GMMdir)/KERNEL ; rm -f cMakefile xccobj
	cd KERNEL ; mv -f ATL_c*.c $(GMMdir)/KERNEL/.
	mv -f KERNEL/cMakefile $(GMMdir)/KERNEL/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMMdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMMdir)/. ; \
        fi
	$(MAKE) cmmlib
	touch cinstall

res/cMMRES.sum :
	$(MAKE) xmmsearch
	./xmmsearch -p c
res/cuCleanKF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p c -R -3
res/cuCleanNF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p c -R -3
res/cuCleanMF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p c -R -3
res/cCleanK :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p c -R -3
res/cCleanN :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p c -R -3
res/cCleanM :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p c -R -3

cRunFindCE : xcfindCE
	$(ATLRUN) $(MMTdir) xcfindCE -f res/atlas_cacheedge.h
	cp -f res/atlas_cacheedge.h $(INCAdir)/atlas_cacheedge.h
cssegenf = CASES/cgenmm_sse.c
cgensseK : xmmgen_sse
	rm -f ssegen_b0.c ssegen_b1.c ssegen_bX.c $(cssegenf)
	./xmmgen_sse -p c -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                      -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                      $(ssegenflags) -beta 0 -f ssegen_b0.c
	./xmmgen_sse -p c -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 1 -f ssegen_b1.c
	./xmmgen_sse -p c -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 8 -f ssegen_bX.c
	echo "#ifdef BETA0" > $(cssegenf)
	cat ssegen_b0.c >> $(cssegenf)
	echo "#elif defined(BETAX)" >> $(cssegenf)
	cat ssegen_bX.c >> $(cssegenf)
	echo "#else" >> $(cssegenf)
	cat ssegen_b1.c >> $(cssegenf)
	echo "#endif" >> $(cssegenf)
res/zMMKSSE.sum : xmmksearch_sse force_build
	./xmmksearch_sse -p z
res/zvMMRES : res/zvgMMRES xvnbmmsearch zcases.vnb
	./xvnbmmsearch -p z
res/zvgMMRES : res/zgMMRES
	$(MAKE) xmm_old2new
	./xmm_old2new -i res/zgMMRES -o res/zvgMMRES

zcases.vnb : $(mySRCdir)/CASES/zcases.vnb xvmmf_gccsub
	cp $(mySRCdir)/CASES/zcases.vnb zcases.vnb
	./xvmmf_gccsub 'gcc' '$(GOODGCC)' zcases.vnb
zcases.dsc: xuserindex $(mySRCdir)/CASES/zcases.0 $(mySRCdir)/CASES/zcases.SSE \
                 $(mySRCdir)/CASES/zcases.3DN $(mySRCdir)/CASES/zcases.flg 
	./xuserindex -p z
res/znreg : xgmmsearch
	./xgmmsearch -p z -r -1
zRun_tfc : ztfc
	$(ATLRUN) $(MMTdir) xztfc
	cp -f $(INCAdir)/zXover.h res/.

ztfc :
	$(MAKE) xztfc pre=z typ=DCPLX


zinstall : xemit_mm res/zMMRES.sum $(BINdir)/xccobj \
                res/zCleanM res/zCleanN res/zCleanK \
                res/zuCleanMF res/zuCleanNF res/zuCleanKF 
	./xemit_mm  -p z -R -2
	mv -f zmm.h $(INCAdir)/.
	mv -f atlas_zNCmm.h $(INCAdir)/.
	cd $(GMMdir)/KERNEL ; rm -f ATL_z*.o
	cd $(GMMdir)/KERNEL ; rm -f ATL_z*
	cd $(GMMdir)/KERNEL ; rm -f zMakefile xccobj
	cd KERNEL ; mv -f ATL_z*.c $(GMMdir)/KERNEL/.
	mv -f KERNEL/zMakefile $(GMMdir)/KERNEL/.
	if [ -s "xccobj.exe" ] ; then \
	   cp -f xccobj.exe $(GMMdir)/. ; \
	elif [ -s "xccobj" ]; then \
           cp -f xccobj $(GMMdir)/. ; \
        fi
	$(MAKE) zmmlib
	touch zinstall

res/zMMRES.sum :
	$(MAKE) xmmsearch
	./xmmsearch -p z
res/zuCleanKF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p z -R -3
res/zuCleanNF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p z -R -3
res/zuCleanMF :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p z -R -3
res/zCleanK :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p z -R -3
res/zCleanN :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p z -R -3
res/zCleanM :
	$(MAKE) xmmcuncpsearch
	./xmmcuncpsearch -p z -R -3

zRunFindCE : xzfindCE
	$(ATLRUN) $(MMTdir) xzfindCE -f res/atlas_cacheedge.h
	cp -f res/atlas_cacheedge.h $(INCAdir)/atlas_cacheedge.h
zssegenf = CASES/zgenmm_sse.c
zgensseK : xmmgen_sse
	rm -f ssegen_b0.c ssegen_b1.c ssegen_bX.c $(zssegenf)
	./xmmgen_sse -p z -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                      -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                      $(ssegenflags) -beta 0 -f ssegen_b0.c
	./xmmgen_sse -p z -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 1 -f ssegen_b1.c
	./xmmgen_sse -p z -m $(mu) -n $(nu) -k $(ku) -M $(MB) -N $(NB) \
                   -K $(KB) -lda $(lda) -ldb $(ldb) -ldc $(ldc) \
                   $(ssegenflags) -beta 8 -f ssegen_bX.c
	echo "#ifdef BETA0" > $(zssegenf)
	cat ssegen_b0.c >> $(zssegenf)
	echo "#elif defined(BETAX)" >> $(zssegenf)
	cat ssegen_bX.c >> $(zssegenf)
	echo "#else" >> $(zssegenf)
	cat ssegen_b1.c >> $(zssegenf)
	echo "#endif" >> $(zssegenf)

csRunFindCE : xcsfindCE
	$(ATLRUN) $(MMTdir) xcsfindCE -f res/atlas_csNKB.h
	cp -f res/atlas_csNKB.h $(INCAdir)/.
csfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DSCPLX -DATL_JITcp -o $@ $(mySRCdir)/findCE.c

xcsfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) cmmlib smmlib \
                csfindCE.o
	cd $(GMMdir) ; $(MAKE) ATL_csFindCE_mm.o typ=SCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ csfindCE.o \
                   $(GMMdir)/ATL_csFindCE_mm.o $(ATLASlib) $(LIBS)
zdRunFindCE : xzdfindCE
	$(ATLRUN) $(MMTdir) xzdfindCE -f res/atlas_zdNKB.h
	cp -f res/atlas_zdNKB.h $(INCAdir)/.
zdfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DDCPLX -DATL_JITcp -o $@ $(mySRCdir)/findCE.c

xzdfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) zmmlib dmmlib \
                zdfindCE.o
	cd $(GMMdir) ; $(MAKE) ATL_zdFindCE_mm.o typ=DCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ zdfindCE.o \
                   $(GMMdir)/ATL_zdFindCE_mm.o $(ATLASlib) $(LIBS)

res/atlas_tcacheedge.h : 
	$(MAKE) xdtfindCE
	$(ATLRUN) $(MMTdir) xdtfindCE -f res/atlas_tcacheedge.h
	cp -f res/atlas_tcacheedge.h $(INCAdir)/.
smmlib:
	cd $(GMMdir) ; $(MAKE) slib

sstandard : force_build
	$(MAKE) sready $(sATLres)
	$(MAKE) sinstall

res/sgMMRES.sum : xgmmsearch
	./xgmmsearch -p s $(srchflags)
res/sguMMRES.sum : xgmmsearch
	./xgmmsearch -p s -b $(nb) -o res/sguMMRES.sum

sfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DSREAL -o $@ $(mySRCdir)/findCE.c
stfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DATL_THREADED_CE -DSREAL -o $@ \
               $(mySRCdir)/findCE.c

sthrCE :
	cd $(BLDdir)/src/threads ; $(MAKE) lib.grd
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) slib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) ATL_stgemm_CE.o \
           ATL_stgemm_bigMN_Kp_CE.o
xstfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) smmlib \
                 stfindCE.o sthrCE
	cd $(GMMdir) ; $(MAKE) ATL_sFindCE_mm.o pre=s typ=SREAL
	$(CLINKER) $(CLINKFLAGS) -o $@ stfindCE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_stgemm_CE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_stgemm_bigMN_Kp_CE.o \
           $(GMMdir)/ATL_sFindCE_mm.o $(ATLASlib) $(LIBS)

sRunFindCE_thr : xstfindCE
	$(ATLRUN) $(MMTdir) xstfindCE -f res/atlas_tscacheedge.h
	cp -f res/atlas_tscacheedge.h $(INCAdir)/.

xsfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) smmlib \
                sfindCE.o
	cd $(GMMdir) ; $(MAKE) ATL_sFindCE_mm.o pre=s typ=SREAL
	$(CLINKER) $(CLINKFLAGS) -o $@ sfindCE.o \
                   $(GMMdir)/ATL_sFindCE_mm.o $(ATLASlib) $(LIBS)

xstfc : $(INCAdir)/atlas_type.h $(L3INCdep) \
             $(pre)tfc.o smmlib
	cd $(GMMdir) ; $(MAKE) ATL_sbig_mm.o ATL_ssmall_mm.o \
           pre=s typ=SREAL
	$(CLINKER) $(CLINKFLAGS) -o $@ stfc.o \
              $(GMMdir)/ATL_$(pre)big_mm.o $(GMMdir)/ATL_ssmall_mm.o \
              $(ATLASlib) $(LIBS)

srbob : $(SYSdir)/time.o xemit_reg $(INCAdir)/atlas_type.h \
       $(INCAdir)/atlas_ssysinfo.h
	rm -f fc.o smm.o smm.c
	./xemit_reg x s $(muladd) $(mmrout) $(mb) $(nb) $(kb) $(if) $(nf) \
                    $(mu) $(nu) $(ku) $(lat) $(beta) > smm.c
	$(KC) $(KCFLAGS) -DsREAL -DMB=$(mb) -DNB=$(nb) -DKB=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) -DLAT=$(lat) -DMULADD=$(muladd) \
              -DNBmm=ATL_s$(mmrout) -DLDC=$(ldc) -DBETA=$(beta) $(moves) \
              -o sfc.o -c $(mySRCdir)/fc.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -c smm.c
	$(CLINKER) $(CLINKFLAGS) -o xsfc sfc.o smm.o \
                                    $(SYSdir)/time.o
	$(ATLRUN) $(MMTdir) xsfc

smmtime_pt.o : $(mySRCdir)/mmtime_pt.c
	$(KC) $(KCFLAGS) -c -DSREAL -o smmtime_pt.o \
               $(mySRCdir)/mmtime_pt.c
xsmmtime_pt : force_build
	rm -f $(pre)mm.c
	$(MAKE) smmtime_pt.o xemit_mm
	./xemit_mm -p s -ta t -tb n -muladd $(muladd) -l $(lat) \
           -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
           -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) \
           -csC $(csC) -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) \
           -Z 1 -C $(cleanup) -R -3 | fgrep -v "define ATL_USERMM" > smm.c
	cat $(mmrout) >> smm.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c smm.c 
	$(CLINKER) $(CLINKFLAGS) -o xsmmtime_pt smmtime_pt.o \
           smm.o $(SYSdir)/time.o $(ATLASlib) $(LIBS)

	
smmcaseN0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_ssysinfo.h
	rm -f sfc.o smm.o
	$(KC) $(KCFLAGS) -DsREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup) -DATL_NEWTIME=1 \
              -o sfc.o -c $(mySRCdir)/fc.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c smm.c 
	$(CLINKER) $(CLINKFLAGS) -o xsfc sfc.o smm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xsfc $(casnam)
smmcase0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_ssysinfo.h
	rm -f sfc.o smm.o
	$(KC) $(KCFLAGS) -DsREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup)   \
              -o sfc.o -c $(mySRCdir)/fc.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c smm.c 
	$(CLINKER) $(CLINKFLAGS) -o xsfc sfc.o smm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xsfc $(casnam)

dmmlib:
	cd $(GMMdir) ; $(MAKE) dlib

dstandard : force_build
	$(MAKE) dready $(dATLres)
	$(MAKE) dinstall

res/dgMMRES.sum : xgmmsearch
	./xgmmsearch -p d $(srchflags)
res/dguMMRES.sum : xgmmsearch
	./xgmmsearch -p d -b $(nb) -o res/dguMMRES.sum

dfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DDREAL -o $@ $(mySRCdir)/findCE.c
dtfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DATL_THREADED_CE -DDREAL -o $@ \
               $(mySRCdir)/findCE.c

dthrCE :
	cd $(BLDdir)/src/threads ; $(MAKE) lib.grd
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) dlib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) ATL_dtgemm_CE.o \
           ATL_dtgemm_bigMN_Kp_CE.o
xdtfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) dmmlib \
                 dtfindCE.o dthrCE
	cd $(GMMdir) ; $(MAKE) ATL_dFindCE_mm.o pre=d typ=DREAL
	$(CLINKER) $(CLINKFLAGS) -o $@ dtfindCE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_dtgemm_CE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_dtgemm_bigMN_Kp_CE.o \
           $(GMMdir)/ATL_dFindCE_mm.o $(ATLASlib) $(LIBS)

dRunFindCE_thr : xdtfindCE
	$(ATLRUN) $(MMTdir) xdtfindCE -f res/atlas_tdcacheedge.h
	cp -f res/atlas_tdcacheedge.h $(INCAdir)/.

xdfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) dmmlib \
                dfindCE.o
	cd $(GMMdir) ; $(MAKE) ATL_dFindCE_mm.o pre=d typ=DREAL
	$(CLINKER) $(CLINKFLAGS) -o $@ dfindCE.o \
                   $(GMMdir)/ATL_dFindCE_mm.o $(ATLASlib) $(LIBS)

xdtfc : $(INCAdir)/atlas_type.h $(L3INCdep) \
             $(pre)tfc.o dmmlib
	cd $(GMMdir) ; $(MAKE) ATL_dbig_mm.o ATL_dsmall_mm.o \
           pre=d typ=DREAL
	$(CLINKER) $(CLINKFLAGS) -o $@ dtfc.o \
              $(GMMdir)/ATL_$(pre)big_mm.o $(GMMdir)/ATL_dsmall_mm.o \
              $(ATLASlib) $(LIBS)

drbob : $(SYSdir)/time.o xemit_reg $(INCAdir)/atlas_type.h \
       $(INCAdir)/atlas_dsysinfo.h
	rm -f fc.o dmm.o dmm.c
	./xemit_reg x d $(muladd) $(mmrout) $(mb) $(nb) $(kb) $(if) $(nf) \
                    $(mu) $(nu) $(ku) $(lat) $(beta) > dmm.c
	$(KC) $(KCFLAGS) -DdREAL -DMB=$(mb) -DNB=$(nb) -DKB=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) -DLAT=$(lat) -DMULADD=$(muladd) \
              -DNBmm=ATL_d$(mmrout) -DLDC=$(ldc) -DBETA=$(beta) $(moves) \
              -o dfc.o -c $(mySRCdir)/fc.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -c dmm.c
	$(CLINKER) $(CLINKFLAGS) -o xdfc dfc.o dmm.o \
                                    $(SYSdir)/time.o
	$(ATLRUN) $(MMTdir) xdfc

dmmtime_pt.o : $(mySRCdir)/mmtime_pt.c
	$(KC) $(KCFLAGS) -c -DDREAL -o dmmtime_pt.o \
               $(mySRCdir)/mmtime_pt.c
xdmmtime_pt : force_build
	rm -f $(pre)mm.c
	$(MAKE) dmmtime_pt.o xemit_mm
	./xemit_mm -p d -ta t -tb n -muladd $(muladd) -l $(lat) \
           -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
           -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) \
           -csC $(csC) -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) \
           -Z 1 -C $(cleanup) -R -3 | fgrep -v "define ATL_USERMM" > dmm.c
	cat $(mmrout) >> dmm.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c dmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xdmmtime_pt dmmtime_pt.o \
           dmm.o $(SYSdir)/time.o $(ATLASlib) $(LIBS)

	
dmmcaseN0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_dsysinfo.h
	rm -f dfc.o dmm.o
	$(KC) $(KCFLAGS) -DdREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup) -DATL_NEWTIME=1 \
              -o dfc.o -c $(mySRCdir)/fc.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c dmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xdfc dfc.o dmm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xdfc $(casnam)
dmmcase0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_dsysinfo.h
	rm -f dfc.o dmm.o
	$(KC) $(KCFLAGS) -DdREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup)   \
              -o dfc.o -c $(mySRCdir)/fc.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c dmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xdfc dfc.o dmm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xdfc $(casnam)

cmmlib:
	cd $(GMMdir) ; $(MAKE) clib

cstandard : force_build
	$(MAKE) cready $(cATLres)
	$(MAKE) cinstall

res/cgMMRES.sum : xgmmsearch
	./xgmmsearch -p c $(srchflags)
res/cguMMRES.sum : xgmmsearch
	./xgmmsearch -p c -b $(nb) -o res/cguMMRES.sum

cfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DSCPLX -o $@ $(mySRCdir)/findCE.c
ctfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DATL_THREADED_CE -DSCPLX -o $@ \
               $(mySRCdir)/findCE.c

cthrCE :
	cd $(BLDdir)/src/threads ; $(MAKE) lib.grd
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) clib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) ATL_ctgemm_CE.o \
           ATL_ctgemm_bigMN_Kp_CE.o
xctfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) cmmlib \
                 ctfindCE.o cthrCE
	cd $(GMMdir) ; $(MAKE) ATL_cFindCE_mm.o pre=c typ=SCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ ctfindCE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_ctgemm_CE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_ctgemm_bigMN_Kp_CE.o \
           $(GMMdir)/ATL_cFindCE_mm.o $(ATLASlib) $(LIBS)

cRunFindCE_thr : xctfindCE
	$(ATLRUN) $(MMTdir) xctfindCE -f res/atlas_tccacheedge.h
	cp -f res/atlas_tccacheedge.h $(INCAdir)/.

xcfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) cmmlib \
                cfindCE.o
	cd $(GMMdir) ; $(MAKE) ATL_cFindCE_mm.o pre=c typ=SCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ cfindCE.o \
                   $(GMMdir)/ATL_cFindCE_mm.o $(ATLASlib) $(LIBS)

xctfc : $(INCAdir)/atlas_type.h $(L3INCdep) \
             $(pre)tfc.o cmmlib
	cd $(GMMdir) ; $(MAKE) ATL_cbig_mm.o ATL_csmall_mm.o \
           ATL_csbig_mm.o pre=c typ=SCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ ctfc.o \
              $(GMMdir)/ATL_$(pre)big_mm.o $(GMMdir)/ATL_csmall_mm.o \
              $(GMMdir)/ATL_csbig_mm.o \
              $(ATLASlib) $(LIBS)

crbob : $(SYSdir)/time.o xemit_reg $(INCAdir)/atlas_type.h \
       $(INCAdir)/atlas_csysinfo.h
	rm -f fc.o cmm.o cmm.c
	./xemit_reg x c $(muladd) $(mmrout) $(mb) $(nb) $(kb) $(if) $(nf) \
                    $(mu) $(nu) $(ku) $(lat) $(beta) > cmm.c
	$(KC) $(KCFLAGS) -DcREAL -DMB=$(mb) -DNB=$(nb) -DKB=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) -DLAT=$(lat) -DMULADD=$(muladd) \
              -DNBmm=ATL_c$(mmrout) -DLDC=$(ldc) -DBETA=$(beta) $(moves) \
              -o cfc.o -c $(mySRCdir)/fc.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -c cmm.c
	$(CLINKER) $(CLINKFLAGS) -o xcfc cfc.o cmm.o \
                                    $(SYSdir)/time.o
	$(ATLRUN) $(MMTdir) xcfc

cmmtime_pt.o : $(mySRCdir)/mmtime_pt.c
	$(KC) $(KCFLAGS) -c -DSCPLX -o cmmtime_pt.o \
               $(mySRCdir)/mmtime_pt.c
xcmmtime_pt : force_build
	rm -f $(pre)mm.c
	$(MAKE) cmmtime_pt.o xemit_mm
	./xemit_mm -p c -ta t -tb n -muladd $(muladd) -l $(lat) \
           -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
           -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) \
           -csC $(csC) -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) \
           -Z 1 -C $(cleanup) -R -3 | fgrep -v "define ATL_USERMM" > cmm.c
	cat $(mmrout) >> cmm.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c cmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xcmmtime_pt cmmtime_pt.o \
           cmm.o $(SYSdir)/time.o $(ATLASlib) $(LIBS)

	
cmmcaseN0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_csysinfo.h
	rm -f cfc.o cmm.o
	$(KC) $(KCFLAGS) -DcREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup) -DATL_NEWTIME=1 \
              -o cfc.o -c $(mySRCdir)/fc.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c cmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xcfc cfc.o cmm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xcfc $(casnam)
cmmcase0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_csysinfo.h
	rm -f cfc.o cmm.o
	$(KC) $(KCFLAGS) -DcREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup)   \
              -o cfc.o -c $(mySRCdir)/fc.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c cmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xcfc cfc.o cmm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xcfc $(casnam)

zmmlib:
	cd $(GMMdir) ; $(MAKE) zlib

zstandard : force_build
	$(MAKE) zready $(zATLres)
	$(MAKE) zinstall

res/zgMMRES.sum : xgmmsearch
	./xgmmsearch -p z $(srchflags)
res/zguMMRES.sum : xgmmsearch
	./xgmmsearch -p z -b $(nb) -o res/zguMMRES.sum

zfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DDCPLX -o $@ $(mySRCdir)/findCE.c
ztfindCE.o : $(mySRCdir)/findCE.c
	$(KC) $(KCFLAGS) -c -DATL_THREADED_CE -DDCPLX -o $@ \
               $(mySRCdir)/findCE.c

zthrCE :
	cd $(BLDdir)/src/threads ; $(MAKE) lib.grd
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) zlib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) ATL_ztgemm_CE.o \
           ATL_ztgemm_bigMN_Kp_CE.o
xztfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) zmmlib \
                 ztfindCE.o zthrCE
	cd $(GMMdir) ; $(MAKE) ATL_zFindCE_mm.o pre=z typ=DCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ ztfindCE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_ztgemm_CE.o \
           $(BLDdir)/src/threads/blas/level3/ATL_ztgemm_bigMN_Kp_CE.o \
           $(GMMdir)/ATL_zFindCE_mm.o $(ATLASlib) $(LIBS)

zRunFindCE_thr : xztfindCE
	$(ATLRUN) $(MMTdir) xztfindCE -f res/atlas_tzcacheedge.h
	cp -f res/atlas_tzcacheedge.h $(INCAdir)/.

xzfindCE : $(INCAdir)/atlas_type.h $(L3INCdep) zmmlib \
                zfindCE.o
	cd $(GMMdir) ; $(MAKE) ATL_zFindCE_mm.o pre=z typ=DCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ zfindCE.o \
                   $(GMMdir)/ATL_zFindCE_mm.o $(ATLASlib) $(LIBS)

xztfc : $(INCAdir)/atlas_type.h $(L3INCdep) \
             $(pre)tfc.o zmmlib
	cd $(GMMdir) ; $(MAKE) ATL_zbig_mm.o ATL_zsmall_mm.o \
           ATL_zdbig_mm.o pre=z typ=DCPLX
	$(CLINKER) $(CLINKFLAGS) -o $@ ztfc.o \
              $(GMMdir)/ATL_$(pre)big_mm.o $(GMMdir)/ATL_zsmall_mm.o \
              $(GMMdir)/ATL_zdbig_mm.o \
              $(ATLASlib) $(LIBS)

zrbob : $(SYSdir)/time.o xemit_reg $(INCAdir)/atlas_type.h \
       $(INCAdir)/atlas_zsysinfo.h
	rm -f fc.o zmm.o zmm.c
	./xemit_reg x z $(muladd) $(mmrout) $(mb) $(nb) $(kb) $(if) $(nf) \
                    $(mu) $(nu) $(ku) $(lat) $(beta) > zmm.c
	$(KC) $(KCFLAGS) -DzREAL -DMB=$(mb) -DNB=$(nb) -DKB=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) -DLAT=$(lat) -DMULADD=$(muladd) \
              -DNBmm=ATL_z$(mmrout) -DLDC=$(ldc) -DBETA=$(beta) $(moves) \
              -o zfc.o -c $(mySRCdir)/fc.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -c zmm.c
	$(CLINKER) $(CLINKFLAGS) -o xzfc zfc.o zmm.o \
                                    $(SYSdir)/time.o
	$(ATLRUN) $(MMTdir) xzfc

zmmtime_pt.o : $(mySRCdir)/mmtime_pt.c
	$(KC) $(KCFLAGS) -c -DDCPLX -o zmmtime_pt.o \
               $(mySRCdir)/mmtime_pt.c
xzmmtime_pt : force_build
	rm -f $(pre)mm.c
	$(MAKE) zmmtime_pt.o xemit_mm
	./xemit_mm -p z -ta t -tb n -muladd $(muladd) -l $(lat) \
           -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
           -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) \
           -csC $(csC) -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) \
           -Z 1 -C $(cleanup) -R -3 | fgrep -v "define ATL_USERMM" > zmm.c
	cat $(mmrout) >> zmm.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c zmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xzmmtime_pt zmmtime_pt.o \
           zmm.o $(SYSdir)/time.o $(ATLASlib) $(LIBS)

	
zmmcaseN0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_zsysinfo.h
	rm -f zfc.o zmm.o
	$(KC) $(KCFLAGS) -DzREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup) -DATL_NEWTIME=1 \
              -o zfc.o -c $(mySRCdir)/fc.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c zmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xzfc zfc.o zmm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xzfc $(casnam)
zmmcase0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h xccobj \
       $(INCAdir)/atlas_zsysinfo.h
	rm -f zfc.o zmm.o
	$(KC) $(KCFLAGS) -DzREAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DLDA2=$(lda2) -DLDB2=$(ldb2) -DLDC2=$(ldc2) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup)   \
              -o zfc.o -c $(mySRCdir)/fc.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c zmm.c 
	$(CLINKER) $(CLINKFLAGS) -o xzfc zfc.o zmm.o \
                                    $(SYSdir)/time.o $(mmlib)
	$(ATLRUN) $(MMTdir) xzfc $(casnam)



mmcugen  : xemit_mm
	rm -f KERNEL/$(pre)Makefile KERNEL/ATL_$(pre)pKB*.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku0) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -b $(beta) -a $(alpha) -Z 1 -C $(cleanup) -R -3

mmucaseN : xemit_mm auxlib xccobj
	rm -f $(pre)mm.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) -Z 1 -C $(cleanup)\
        -R -3            > $(pre)mm.c
	cat $(mmrout) >> $(pre)mm.c
	$(MAKE) $(pre)mmcaseN0 $(casarg) casnam=$(casnam) mmlib="$(ATLASlib) \
                $(LIBS)"
mmucase : xemit_mm auxlib xccobj
	rm -f $(pre)mm.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) -Z 1 -C $(cleanup)\
        -R -3            > $(pre)mm.c
	cat $(mmrout) >> $(pre)mm.c
	$(MAKE) $(pre)mmcase0 $(casarg) casnam=$(casnam) mmlib="$(ATLASlib) \
                $(LIBS)"

cBuildCobjs: xemit_mm
	rm -f cmm_b[1,0,X].[o,c]
	./xemit_mm -p c -b 0 -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > cmm_b0.c
	cat $(mmrout) >> cmm_b0.c
	./xemit_mm -p c -b 1 -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > cmm_b1.c
	cat $(mmrout) >> cmm_b1.c
	./xemit_mm -p c -b 8 -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > cmm_bX.c
	cat $(mmrout) >> cmm_bX.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=1 -c cmm_b1.c 
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=0 -c cmm_b0.c 
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=8 -c cmm_bX.c 
smmutstcase1: auxlib xccobj
	rm -f smm.[o,c]
	./xemit_mm -p s -b $(beta) -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > smm.c
	cat $(mmrout) >> smm.c
	$(SMC) $(CDEFS) $(SMCFLAGS) -DATL_BETA=$(beta) -c smm.c
	$(MAKE) mmtstcase0 $(casarg) mmobjs=smm.o \
                mmlib="$(ATLASlib) $(LIBS)"
smmtstcase : xemit_mm $(INCAdir)/atlas_type.h xccobj
	rm -f smm.c
	./xemit_mm -p s -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) -Z 1 \
        -C $(cleanup) -pfA $(pfA) > smm.c
	$(SMC) -DATL_BETA=$(beta) $(CDEFS) $(SMCFLAGS) -c smm.c
	$(MAKE) mmtstcase0 $(casarg) mmobjs=smm.o
zBuildCobjs: xemit_mm
	rm -f zmm_b[1,0,X].[o,c]
	./xemit_mm -p z -b 0 -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > zmm_b0.c
	cat $(mmrout) >> zmm_b0.c
	./xemit_mm -p z -b 1 -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > zmm_b1.c
	cat $(mmrout) >> zmm_b1.c
	./xemit_mm -p z -b 8 -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > zmm_bX.c
	cat $(mmrout) >> zmm_bX.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=1 -c zmm_b1.c 
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=0 -c zmm_b0.c 
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=8 -c zmm_bX.c 
dmmutstcase1: auxlib xccobj
	rm -f dmm.[o,c]
	./xemit_mm -p d -b $(beta) -M $(mb) -N $(nb) -K $(kb) -R -3 \
                   -lda $(lda) -ldb $(ldb) -ldc $(ldc) > dmm.c
	cat $(mmrout) >> dmm.c
	$(DMC) $(CDEFS) $(DMCFLAGS) -DATL_BETA=$(beta) -c dmm.c
	$(MAKE) mmtstcase0 $(casarg) mmobjs=dmm.o \
                mmlib="$(ATLASlib) $(LIBS)"
dmmtstcase : xemit_mm $(INCAdir)/atlas_type.h xccobj
	rm -f dmm.c
	./xemit_mm -p d -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) -Z 1 \
        -C $(cleanup) -pfA $(pfA) > dmm.c
	$(DMC) -DATL_BETA=$(beta) $(CDEFS) $(DMCFLAGS) -c dmm.c
	$(MAKE) mmtstcase0 $(casarg) mmobjs=dmm.o
BuildCobjs : $(pre)BuildCobjs

sammmtst : $(INCAdir)/atlas_type.h $(BINdir)/xextract
	$(extC) -def mu "${mu}" -def nu "${nu}" -def be "${beta}" \
                 -def M "${M}" -def N "${N}" -def K "${K}" \
                -o ammmtst.c rout=ammmtst
	$(SMC) $(CDEFS) $(SMCFLAGS) -DSREAL=1 -DBETA${beta} \
           -o sammm.o -c $(mmrout)
	$(KC) $(KCFLAGS) -DSREAL=1 -o xsammmtst ammmtst.c sammm.o
	$(ATLRUN) $(MMTdir) xsammmtst 
dammmtst : $(INCAdir)/atlas_type.h $(BINdir)/xextract
	$(extC) -def mu "${mu}" -def nu "${nu}" -def be "${beta}" \
                 -def M "${M}" -def N "${N}" -def K "${K}" \
                -o ammmtst.c rout=ammmtst
	$(DMC) $(CDEFS) $(DMCFLAGS) -DDREAL=1 -DBETA${beta} \
           -o dammm.o -c $(mmrout)
	$(KC) $(KCFLAGS) -DDREAL=1 -o xdammmtst ammmtst.c dammm.o
	$(ATLRUN) $(MMTdir) xdammmtst 
cammmtst : $(INCAdir)/atlas_type.h $(BINdir)/xextract
	$(extC) -def mu "${mu}" -def nu "${nu}" -def be "${beta}" \
                 -def M "${M}" -def N "${N}" -def K "${K}" \
                -o ammmtst.c rout=ammmtst
	$(SMC) $(CDEFS) $(SMCFLAGS) -DSCPLX=1 -DBETA${beta} \
           -o cammm.o -c $(mmrout)
	$(KC) $(KCFLAGS) -DSCPLX=1 -o xcammmtst ammmtst.c cammm.o
	$(ATLRUN) $(MMTdir) xcammmtst 
zammmtst : $(INCAdir)/atlas_type.h $(BINdir)/xextract
	$(extC) -def mu "${mu}" -def nu "${nu}" -def be "${beta}" \
                 -def M "${M}" -def N "${N}" -def K "${K}" \
                -o ammmtst.c rout=ammmtst
	$(DMC) $(CDEFS) $(DMCFLAGS) -DDCPLX=1 -DBETA${beta} \
           -o zammm.o -c $(mmrout)
	$(KC) $(KCFLAGS) -DDCPLX=1 -o xzammmtst ammmtst.c zammm.o
	$(ATLRUN) $(MMTdir) xzammmtst 
cmmucase : xemit_mm $(SYSdir)/time.o $(INCAdir)/atlas_type.h \
           $(INCAdir)/atlas_$(pre)sysinfo.h auxlib xccobj
	rm -f $(pre)fc.o 
	$(MAKE) $(pre)BuildCobjs pre=$(pre) mb=$(mb) nb=$(nb) kb=$(kb) \
                lda=$(lda) ldb=$(ldb) ldc=$(ldc) $(casarg)
	$(KC) $(KCFLAGS) -D$(pre)REAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DPREFA=$(pfA) -DCLEANUP=$(cleanup) \
              -o $(pre)fc.o -c $(mySRCdir)/fc.c
	$(CLINKER) $(CLINKFLAGS) -o x$(pre)fc $(pre)fc.o $(pre)mm_b0.o \
                   $(pre)mm_b1.o $(pre)mm_bX.o $(SYSdir)/time.o \
                   $(ATLASlib) $(LIBS)
	$(ATLRUN) $(MMTdir) x$(pre)fc $(casnam)

ummcase: xccobj
	$(MAKE) mmucase $(casarg) casnam=res/ignore

mmutstcase : $(pre)mmutstcase1

cmmutstcase: auxlib xccobj
	rm -f $(pre)mm.c
	$(MAKE) BuildCobjs pre=$(pre) mb=$(mb) nb=$(nb) kb=$(kb) \
                lda=$(lda) ldb=$(ldb) ldc=$(ldc) mmrout=$(mmrout) \
                SMC="$(SMC)" SMCFLAGS="$(SMCFLAGS)" \
                DMC="$(DMC)" DMCFLAGS="$(DMCFLAGS)"
	$(MAKE) mmtstcase0 $(casarg) csC=2 \
                mmobjs="$(pre)mm_b0.o $(pre)mm_b1.o $(pre)mm_bX.o" \
                mmlib="$(ATLASlib) $(LIBS)"

#
# This target allows the code generator to fake being a user-contributed kernel
# by packing all BETA cases in one file
#
mmgencase : xemit_mm xccobj
	rm -f $(pre)gmm.c
	echo "#ifdef BETA0" >> $(pre)gmm.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b 0 -a $(alpha) -Z 0 \
        -C $(cleanup) -pfA $(pfA) >> $(pre)gmm.c
	echo "#elif defined(BETAX)" >> $(pre)gmm.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b 8 -a $(alpha) -Z 0 \
        -C $(cleanup) -pfA $(pfA) >> $(pre)gmm.c
	echo "#else" >> $(pre)gmm.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b 1 -a $(alpha) -Z 0 \
        -C $(cleanup) -pfA $(pfA) >> $(pre)gmm.c
	echo "#endif" >> $(pre)gmm.c
mmcase : xemit_mm xccobj
	rm -f $(pre)mm.c
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -FF $(ff) -b $(beta) -a $(alpha) -Z 1 \
        -C $(cleanup) -pfA $(pfA) > $(pre)mm.c
	$(MAKE) $(pre)mmcase0 $(casarg) casnam=$(casnam)

mmtstcase : $(pre)mmtstcase

mmtstcase0: $(SYSdir)/time.o $(INCAdir)/atlas_type.h auxlib xccobj
	rm -f $(pre)mmtst.o
	$(KC) $(KCFLAGS) -D$(pre)REAL -DtranA$(ta) -DtranB$(tb) \
              -DMULADD=$(muladd) -DLAT=$(lat) -D$(loopO) \
              -DMB0=$(mb) -DNB0=$(nb) -DKB0=$(kb) \
              -DMB=$(M) -DNB=$(N) -DKB=$(K) \
              -DKU=$(ku) -DNU=$(nu) -DMU=$(mu) \
              -DLDA=$(lda) -DLDB=$(ldb) -DLDC=$(ldc) \
              -DcsA=$(csA) -DcsB=$(csB) -DcsC=$(csC) \
              -DALPHA=$(alpha) -DBETA=$(beta) $(moves) \
              -DCLEANUP=$(cleanup) \
              -o $(pre)mmtst.o -c $(mySRCdir)/mmtst.c
	$(CLINKER) $(CLINKFLAGS) -o x$(pre)mmtst $(pre)mmtst.o $(mmobjs) \
                   $(mmlib)
	$(ATLRUN) $(MMTdir) x$(pre)mmtst

NCemit : xemit_mm
	rm -f $(mmnam)
	./xemit_mm -p $(pre) -ta $(ta) -tb $(tb) -muladd $(muladd) -l $(lat) \
        -L $(loopO) -M $(mb) -N $(nb) -K $(kb) -m $(mu) -n $(nu) -k $(ku) \
        -lda $(lda) -ldb $(ldb) -ldc $(ldc) -csA $(csA) -csB $(csB) -csC $(csC)\
        -F0 $(if) -FN $(nf) -b $(beta) -a $(alpha) -Z 0 -C $(cleanup) \
                    > $(mmnam)
NCready :  xemit_mm $(INCAdir)/atlas_type.h
	rm -f atlas_$(pre)NCmm.h
	./xemit_mm -p $(pre) -N $(nb) -R -2 > atlas_$(pre)NCmm.h
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=0 \
                       mmnam=ATL_$(pre)NCmmNN_a1_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=1 \
                       mmnam=ATL_$(pre)NCmmNN_a1_b1.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=12 \
                       mmnam=ATL_$(pre)NCmmNN_a1_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=0 \
                       mmnam=ATL_$(pre)NCmmNN_aX_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=12 \
                       mmnam=ATL_$(pre)NCmmNN_aX_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_0x0x$(nb)` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmNN_K.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNN_0x0x0` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmNN.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=0 \
                       mmnam=ATL_$(pre)NCmmNT_a1_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=1 \
                       mmnam=ATL_$(pre)NCmmNT_a1_b1.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=12 \
                       mmnam=ATL_$(pre)NCmmNT_a1_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=0 \
                       mmnam=ATL_$(pre)NCmmNT_aX_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=12 \
                       mmnam=ATL_$(pre)NCmmNT_aX_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_0x0x$(nb)` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmNT_K.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestNT_0x0x0` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmNT.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=0 \
                       mmnam=ATL_$(pre)NCmmTN_a1_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=1 \
                       mmnam=ATL_$(pre)NCmmTN_a1_b1.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=12 \
                       mmnam=ATL_$(pre)NCmmTN_a1_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=0 \
                       mmnam=ATL_$(pre)NCmmTN_aX_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=12 \
                       mmnam=ATL_$(pre)NCmmTN_aX_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_0x0x$(nb)` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmTN_K.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTN_0x0x0` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmTN.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=0 \
                       mmnam=ATL_$(pre)NCmmTT_a1_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=1 \
                       mmnam=ATL_$(pre)NCmmTT_a1_b1.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_$(nb)x$(nb)x$(nb)` \
                       alpha=1 beta=12 \
                       mmnam=ATL_$(pre)NCmmTT_a1_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=0 \
                       mmnam=ATL_$(pre)NCmmTT_aX_b0.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_$(nb)x$(nb)x$(nb)` \
                       alpha=3 beta=12 \
                       mmnam=ATL_$(pre)NCmmTT_aX_bX.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_0x0x$(nb)` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmTT_K.c
	$(MAKE) NCemit `head -n 1 res/$(pre)bestTT_0x0x0` \
                       alpha=12 beta=12 mmnam=ATL_$(pre)NCCUmmTT.c

$(BINdir)/xccobj :
	cd $(BINdir) ; $(MAKE) xccobj
xccobj : $(BINdir)/xccobj
	-@ cp -f $(BINdir)/xccobj .
	-@ cp -f $(BINdir)/xccobj.exe .

auxlib :
	cd $(AUXdir) ; $(MAKE) lib
	touch auxlib
force_build :

install :
	$(MAKE) $(pre)install

GetSysSum.o : $(mySRCdir)/GetSysSum.c
	$(KC) -c $(KCFLAGS) $(mySRCdir)/GetSysSum.c

emit_vKB.o : $(mySRCdir)/emit_vKB.c $(INCSdir)/atlas_vnbcase.h
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/emit_vKB.c

emit_mm.o : $(mySRCdir)/emit_mm.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/emit_mm.c

emit_reg.o : $(mySRCdir)/emit_reg.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/emit_reg.c

emit_cmm.o : $(mySRCdir)/emit_cmm.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/emit_cmm.c

emit_fmm.o : $(mySRCdir)/emit_fmm.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/emit_fmm.c


hcsearch.o : $(L3INCdep) $(mySRCdir)/hcsearch.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/hcsearch.c

gemmtst.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
            $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_tst.h $(INCSdir)/cblas.h
	$(KC) -c $(KCFLAGS) $(mySRCdir)/gemmtst.c

clean :
	rm -f *.o *.grd

$(pre)tfc.o : $(mySRCdir)/tfc.c $(INCSdir)/atlas_misc.h $(INCAdir)/$(pre)mm.h \
              $(INCAdir)/atlas_$(pre)sysinfo.h
	$(KC) -o $@ -c $(KCFLAGS) -D$(typ) $(mySRCdir)/tfc.c \
                 -DSM_FOUT=$(INCAdir)/$(pre)Xover.h

