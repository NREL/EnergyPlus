include ../Make.inc
arch=$(ARCH)
mach=$(ARCH)
dcc=default
cc=gcc
skc=$(skcD)
dkc=$(dkcD)
smc=$(smcD)
dmc=$(dmcD)
comps = skc=$(skc) dkc=$(dkc) smc=$(smc) dmc=$(dmc)

adefd = $(BLDdir)/ARCHS/$(mach)

tarfile : $(mach)
	- rm $(mach).tgz $(mach).tar
	tar cvf $(mach).tar $(mach)
	$(BZIP) $(mach).tar

$(mach)/kern/$(dcc) :
	mkdir $(mach)/kern/$(dcc)
$(mach)/gemm/$(dcc) :
	mkdir $(mach)/gemm/$(dcc)
$(mach)/lapack/$(dcc) :
	mkdir $(mach)/lapack/$(dcc)

res:
	mkdir res
res/goodgcc.txt : res
	echo "'$(GOODGCC)'" > res/goodgcc.txt
$(mach) :
	mkdir $(mach)
	mkdir $(mach)/kern $(mach)/gemm $(mach)/lapack
	$(MAKE) $(mach)/kern/$(skc) dcc=$(skc)
	$(MAKE) $(mach)/kern/$(dkc) dcc=$(dkc)
	$(MAKE) $(mach)/gemm/$(smc) dcc=$(smc)
	$(MAKE) $(mach)/gemm/$(dmc) dcc=$(dmc)
	$(MAKE) $(mach)/lapack/$(dmc) dcc=$(dmc)
	$(MAKE) $(mach)/lapack/$(smc) dcc=$(smc)

xnegr1 : $(SRCdir)/CONFIG/ARCHS/negr1file.c
	$(XCC) $(XCCFLAGS) -o $@ $(SRCdir)/CONFIG/ARCHS/negr1file.c
xnegmm : $(SRCdir)/CONFIG/ARCHS/negmmfile.c
	$(XCC) $(XCCFLAGS) -o $@ $(SRCdir)/CONFIG/ARCHS/negmmfile.c
xnegmv : $(SRCdir)/CONFIG/ARCHS/negmvfile.c
	$(XCC) $(XCCFLAGS) -o $@ $(SRCdir)/CONFIG/ARCHS/negmvfile.c
xnegflt : $(SRCdir)/CONFIG/ARCHS/negflt.c
	$(XCC) -O -o xnegflt $(SRCdir)/CONFIG/ARCHS/negflt.c

ssys_put:
	- cp $(adefd)/kern/sMULADD $(SYSdir)/res/.
	- cp $(adefd)/kern/L1CacheSize $(SYSdir)/res/.
ssys :
	- cp $(SYSdir)/res/sMULADD $(adefd)/kern/.
	./xnegflt $(adefd)/kern/sMULADD
	- cp $(SYSdir)/res/L1CacheSize $(adefd)/kern/.
skern_put:
	- cp $(adefd)/kern/$(skc)/[s,c]TRSM_NB $(BLDdir)/tune/blas/level3/res/.
	- cp $(adefd)/kern/$(skc)/[s,c]MV[N,T]K.sum $(BLDdir)/tune/blas/gemv/res/.
	- cp  $(adefd)/kern/$(skc)/[s,c]R?K.sum $(BLDdir)/tune/blas/ger/res/.
	- cp $(adefd)/kern/$(skc)/[s,c]*_SUMM $(L1Tdir)/res/.
skern: xnegflt xnegr1 xnegmv
	rm -f $(adefd)/kern/$(skc)/[s,c]*
	- cp $(BLDdir)/tune/blas/level3/res/[s,c]TRSM_NB $(adefd)/kern/$(skc)/.
	- cp $(BLDdir)/tune/blas/gemv/res/[s,c]MV[N,T]K.sum $(adefd)/kern/$(skc)/.
	- cp $(BLDdir)/tune/blas/ger/res/[s,c]R?K.sum $(adefd)/kern/$(skc)/.
	./xnegmv  $(adefd)/kern/$(skc)/[s,c]MVNK.sum
	./xnegmv  $(adefd)/kern/$(skc)/[s,c]MVTK.sum
	./xnegr1 $(adefd)/kern/$(skc)/[s,c]R?K.sum
	- cp $(L1Tdir)/res/[s,c]*_SUMM $(adefd)/kern/$(skc)/.
sgemm_put:
	- cp $(adefd)/gemm/$(smc)/[s,c]MMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]gMMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]guMMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]eMMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]MMKSSE.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]Clean[M,N,K] $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]best[N,T][N,T]_* $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]uMMRES $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]uClean[M,N,K] $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]uClean[M,N,K]F $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]nreg $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]fpuMM $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]NCNB $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(smc)/[s,c]Xover.h $(INCAdir)/.
	- cp $(adefd)/gemm/$(smc)/[s,c]Xover.h $(MMTdir)/res/.
sgemm: xnegflt xnegmm
	rm -f $(adefd)/gemm/$(smc)/[s,c]*
	- cp $(MMTdir)/res/[s,c]Clean[M,N,K] $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]best[N,T][N,T]_* $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]uMMRES $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]uClean[M,N,K] $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]uClean[M,N,K]F $(adefd)/gemm/$(smc)/.
	./xnegflt $(adefd)/gemm/$(smc)/*
	- cp $(MMTdir)/res/[s,c]MMRES.sum $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]gMMRES.sum $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]guMMRES.sum $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]eMMRES.sum $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]MMKSSE.sum $(adefd)/gemm/$(smc)/.
	- ./xnegmm $(adefd)/gemm/$(smc)/*MM*.sum
	- cp $(MMTdir)/res/[s,c]nreg $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]fpuMM $(adefd)/gemm/$(smc)/.
	- cp $(MMTdir)/res/[s,c]NCNB $(adefd)/gemm/$(smc)/.
	- cp $(INCAdir)/[s,c]Xover.h $(adefd)/gemm/$(smc)/.
sla_get:
	rm -f $(adefd)/lapack/$(smc)/atlas_[s,c]GetNB_*.h
	for file in $(INCAdir)/atlas_[s,c]GetNB_*.h ; do \
           if [ -s "$${file}" ]; then \
	      cp $${file} $(adefd)/lapack/$(smc)/. ; \
           fi \
        done
	rm -f $(adefd)/lapack/$(smc)/atlas_[s,c]tGetNB_*.h
	for file in $(INCAdir)/atlas_[s,c]tGetNB_*.h ; do \
           if [ -s "$${file}" ]; then \
	      cp $${file} $(adefd)/lapack/$(smc)/. ; \
           fi \
        done
sla_put :
	- cp $(adefd)/lapack/$(smc)/atlas_[s,c]GetNB_*.h $(INCAdir)/.
	- cp $(adefd)/lapack/$(smc)/atlas_[s,c]tGetNB_*.h $(INCAdir)/.
dsys_put:
	- cp $(adefd)/kern/dMULADD $(SYSdir)/res/.
	- cp $(adefd)/kern/L1CacheSize $(SYSdir)/res/.
dsys :
	- cp $(SYSdir)/res/dMULADD $(adefd)/kern/.
	./xnegflt $(adefd)/kern/dMULADD
	- cp $(SYSdir)/res/L1CacheSize $(adefd)/kern/.
dkern_put:
	- cp $(adefd)/kern/$(dkc)/[d,z]TRSM_NB $(BLDdir)/tune/blas/level3/res/.
	- cp $(adefd)/kern/$(dkc)/[d,z]MV[N,T]K.sum $(BLDdir)/tune/blas/gemv/res/.
	- cp  $(adefd)/kern/$(dkc)/[d,z]R?K.sum $(BLDdir)/tune/blas/ger/res/.
	- cp $(adefd)/kern/$(dkc)/[d,z]*_SUMM $(L1Tdir)/res/.
dkern: xnegflt xnegr1 xnegmv
	rm -f $(adefd)/kern/$(dkc)/[d,z]*
	- cp $(BLDdir)/tune/blas/level3/res/[d,z]TRSM_NB $(adefd)/kern/$(dkc)/.
	- cp $(BLDdir)/tune/blas/gemv/res/[d,z]MV[N,T]K.sum $(adefd)/kern/$(dkc)/.
	- cp $(BLDdir)/tune/blas/ger/res/[d,z]R?K.sum $(adefd)/kern/$(dkc)/.
	./xnegmv  $(adefd)/kern/$(dkc)/[d,z]MVNK.sum
	./xnegmv  $(adefd)/kern/$(dkc)/[d,z]MVTK.sum
	./xnegr1 $(adefd)/kern/$(dkc)/[d,z]R?K.sum
	- cp $(L1Tdir)/res/[d,z]*_SUMM $(adefd)/kern/$(dkc)/.
dgemm_put:
	- cp $(adefd)/gemm/$(dmc)/[d,z]MMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]gMMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]guMMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]eMMRES.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]MMKSSE.sum $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]Clean[M,N,K] $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]best[N,T][N,T]_* $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]uMMRES $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]uClean[M,N,K] $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]uClean[M,N,K]F $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]nreg $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]fpuMM $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]NCNB $(MMTdir)/res/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]Xover.h $(INCAdir)/.
	- cp $(adefd)/gemm/$(dmc)/[d,z]Xover.h $(MMTdir)/res/.
dgemm: xnegflt xnegmm
	rm -f $(adefd)/gemm/$(dmc)/[d,z]*
	- cp $(MMTdir)/res/[d,z]Clean[M,N,K] $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]best[N,T][N,T]_* $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]uMMRES $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]uClean[M,N,K] $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]uClean[M,N,K]F $(adefd)/gemm/$(dmc)/.
	./xnegflt $(adefd)/gemm/$(dmc)/*
	- cp $(MMTdir)/res/[d,z]MMRES.sum $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]gMMRES.sum $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]guMMRES.sum $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]eMMRES.sum $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]MMKSSE.sum $(adefd)/gemm/$(dmc)/.
	- ./xnegmm $(adefd)/gemm/$(dmc)/*MM*.sum
	- cp $(MMTdir)/res/[d,z]nreg $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]fpuMM $(adefd)/gemm/$(dmc)/.
	- cp $(MMTdir)/res/[d,z]NCNB $(adefd)/gemm/$(dmc)/.
	- cp $(INCAdir)/[d,z]Xover.h $(adefd)/gemm/$(dmc)/.
dla_get:
	rm -f $(adefd)/lapack/$(dmc)/atlas_[d,z]GetNB_*.h
	for file in $(INCAdir)/atlas_[d,z]GetNB_*.h ; do \
           if [ -s "$${file}" ]; then \
	      cp $${file} $(adefd)/lapack/$(dmc)/. ; \
           fi \
        done
	rm -f $(adefd)/lapack/$(dmc)/atlas_[d,z]tGetNB_*.h
	for file in $(INCAdir)/atlas_[d,z]tGetNB_*.h ; do \
           if [ -s "$${file}" ]; then \
	      cp $${file} $(adefd)/lapack/$(dmc)/. ; \
           fi \
        done
dla_put :
	- cp $(adefd)/lapack/$(dmc)/atlas_[d,z]GetNB_*.h $(INCAdir)/.
	- cp $(adefd)/lapack/$(dmc)/atlas_[d,z]tGetNB_*.h $(INCAdir)/.

la_get : sla_get dla_get
la_put : sla_put dla_put
sys_put : ssys_put dsys_put
kern_put : skern_put dkern_put
gemm_put : sgemm_put dgemm_put
	- cp $(adefd)/gemm/atlas_cacheedge.h $(INCAdir)/.
	- cp $(adefd)/gemm/atlas_cacheedge.h $(MMTdir)/res/.
	- cp $(adefd)/gemm/atlas_tcacheedge.h $(INCAdir)/.
	- cp $(adefd)/gemm/atlas_tcacheedge.h $(MMTdir)/res/.
	- cp $(adefd)/gemm/atlas_??NKB.h $(INCAdir)/.
	- cp $(adefd)/gemm/atlas_??NKB.h $(MMTdir)/res/.
sys : ssys dsys
kern : skern dkern
gemm : sgemm dgemm
	- cp $(INCAdir)/atlas_cacheedge.h $(adefd)/gemm/
	- cp $(INCAdir)/atlas_tcacheedge.h $(adefd)/gemm/
	- cp $(INCAdir)/atlas_??NKB.h $(adefd)/gemm/
ArchNew : $(mach) xnegflt
	$(MAKE) sys kern gemm la_get
	- cp $(BLDdir)/bin/INSTALL_LOG/?PerfSumm.txt $(adefd)/.
	rm -f xnegflt
archput : sys_put kern_put gemm_put la_put
