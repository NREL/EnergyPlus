include Make.inc
mySRCdir = $(SRCdir)/bin

scalf = $(mflop_l1)

pre=j
nb = 88
NBs = 31 1 4 8 12 16 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 96 \
      112 128 144 160 176 192 224 256

dmmobj = $(GMMdir)/ATL_dbig_mm.o $(GMMdir)/ATL_dbignork_mm.o \
         $(GMMdir)/ATL_dsmall_mm.o $(GMMdir)/ATL_dsmallK_mm.o
smmobj = $(GMMdir)/ATL_sbig_mm.o $(GMMdir)/ATL_sbignork_mm.o \
         $(GMMdir)/ATL_ssmall_mm.o $(GMMdir)/ATL_ssmallK_mm.o
cmmobj = $(GMMdir)/ATL_cbig_mm.o $(GMMdir)/ATL_csmall_mm.o \
         $(GMMdir)/ATL_csmallK_mm.o
zmmobj = $(GMMdir)/ATL_zbig_mm.o $(GMMdir)/ATL_zsmall_mm.o \
         $(GMMdir)/ATL_zsmallK_mm.o

$(SYSdir)/ATL_Xwalltime.o :
	cd $(SYSdir) ; $(MAKE) ATL_Xwalltime.o
xextract : $(mySRCdir)/extract.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/extract.c
xatlas_waitfile : atlas_waitfile.o $(SYSdir)/ATL_Xwalltime.o
	$(XCC) $(XCCFLAGS) -o $@ atlas_waitfile.o $(SYSdir)/ATL_Xwalltime.o
atlas_waitfile.o : $(mySRCdir)/atlas_waitfile.c
	$(XCC) $(XCCFLAGS) -c $(mySRCdir)/atlas_waitfile.c
xatlas_build : xatlas_tee atlas_install.o 
	$(XCC) $(XCCFLAGS) -o $@ atlas_install.o
atlas_install.o : $(mySRCdir)/atlas_install.c
	$(XCC) $(XCCFLAGS) -c $(mySRCdir)/atlas_install.c

xatlas_tee     : atlas_tee.o 
	$(XCC) $(XCCFLAGS) -o $@ atlas_tee.o
atlas_tee.o : $(mySRCdir)/atlas_tee.c
	$(XCC) $(XCCFLAGS) -c $(mySRCdir)/atlas_tee.c
xccobj  : $(mySRCdir)/ccobj.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/ccobj.c -DTOPDIR=$(BLDdir)
xsubstr : $(mySRCdir)/substr.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/substr.c
xatl2vec : $(mySRCdir)/atl2vec.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/atl2vec.c -lm
xstattime : $(mySRCdir)/stattime.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/stattime.c -lm


IBuildRef:
	cd $(BLDdir)/src/blas/reference/level3 ; $(MAKE) $(pre)lib
	cd $(BLDdir)/src/blas/reference/level2 ; $(MAKE) $(pre)lib


IBuildDyLibs:
	cd $(BLDdir) ; $(MAKE) dylibs
$(MMTdir)/res/atlas_tcachedge.h :
	cd $(MMTdir) ; $(MAKE) res/atlas_tcacheedge.h
INSTALL_LOG/atlas_tcacheedge.h : $(MMTdir)/res/atlas_tcachedge.h
	cp $(MMTdir)/res/atlas_tcacheedge.h $(INCAdir)/.
	cp $(MMTdir)/res/atlas_tcacheedge.h INSTALL_LOG/.
IBuildPtlibs0:
	cd $(BLDdir)/src/auxil/ ; $(MAKE) ptlib
	cd $(BLDdir)/src/blas/gemm ; $(MAKE) lib
	cd $(BLDdir)/src/threads/ ; $(MAKE) lib
	cd $(BLDdir)/interfaces/blas/C/src/ ; $(MAKE) ptlib
	- cd $(BLDdir)/interfaces/blas/F77/src/ ; $(MAKE) ptlib
	cd $(BLDdir)/src/lapack ; $(MAKE) ptlib

IPTtune:
	cd $(BLDdir)/tune/threads ; $(MAKE) IThreadTune
	cd $(BLDdir)/src/auxil/ ; $(MAKE) ptlib
	$(MAKE) INSTALL_LOG/atlas_tcacheedge.h
	cd $(BLDdir)/src/blas/gemm ; $(MAKE) lib
	cd $(BLDdir)/src/threads/ ; $(MAKE) lib

IBuildPtlibs:
	cd $(BLDdir)/src/blas/gemm ; $(MAKE) lib
	cd $(BLDdir)/interfaces/blas/C/src/ ; $(MAKE) ptlib
	- cd $(BLDdir)/interfaces/blas/F77/src/ ; $(MAKE) ptlib
	cd $(BLDdir)/src/lapack ; $(MAKE) ptlib
	cd $(BLDdir)/interfaces/lapack/C/src/ ; $(MAKE) ptlib
	- cd $(BLDdir)/interfaces/lapack/F77/src/ ; $(MAKE) ptlib
	if [ -d "$(FLAdir)" ]; then \
	   cd $(BLDdir)/interfaces/lapack/C2F/src ; $(MAKE) ptlib ; \
        fi

IBuildLibs:
	cd $(BLDdir)/src/auxil ; $(MAKE) lib
	cd $(BLDdir)/src/blas/gemm ; $(MAKE) lib
	cd $(BLDdir)/src/blas/gemv ; $(MAKE) lib
	cd $(BLDdir)/src/blas/ger ; $(MAKE) lib
	cd $(BLDdir)/src/blas/level1 ; $(MAKE) lib
	cd $(BLDdir)/src/blas/level2 ; $(MAKE) lib
	cd $(BLDdir)/src/blas/level3 ; $(MAKE) lib
	cd $(BLDdir)/src/blas/pklevel3 ; $(MAKE) lib
	- cd $(BLDdir)/src/blas/f77reference ; $(MAKE) lib
	cd $(BLDdir)/src/blas/reference/level3 ; $(MAKE) lib
	cd $(BLDdir)/src/lapack ; $(MAKE) lib
	cd $(BLDdir)/interfaces/blas/C/src ; $(MAKE) all
	- cd $(BLDdir)/interfaces/blas/F77/src ; $(MAKE) lib
	cd $(BLDdir)/interfaces/lapack/C/src ; $(MAKE) lib
	- cd $(BLDdir)/interfaces/lapack/F77/src ; $(MAKE) lib
	if [ -d "$(FLAdir)" ]; then \
	   cd $(BLDdir)/interfaces/lapack/C2F/src ; $(MAKE) lib ; \
        fi
latune_all :
	$(MAKE) ILATune pre=d
	$(MAKE) ILATune pre=s
	$(MAKE) ILATune pre=z
	$(MAKE) ILATune pre=c
ILATune:
	if [ -s "$(INCAdir)/atlas_$(pre)GetNB_geqrf.h" ]; then \
           echo "Arch Defaults allow us to skip tuning" ; \
        else \
	    cd $(BLDdir)/tune/lapack ; $(MAKE) res/ATL_$(pre)GetNB_geqrf.h ; \
	    cp $(BLDdir)/tune/lapack/res/ATL_$(pre)GetNB_geqrf.h \
               $(INCAdir)/atlas_$(pre)GetNB_geqrf.h ; \
        fi
	if [ -s "$(INCAdir)/atlas_$(pre)tGetNB_geqrf.h" ]; then \
           echo "Arch Defaults allow us to skip threaded tuning" ; \
        elif [ -s "$(INCAdir)/atlas_pthreads.h" ]; then \
	    cd $(BLDdir)/tune/lapack ; $(MAKE) res/ATL_$(pre)tGetNB_geqrf.h ; \
	    cp $(BLDdir)/tune/lapack/res/ATL_$(pre)tGetNB_geqrf.h \
               $(INCAdir)/atlas_$(pre)tGetNB_geqrf.h ; \
        fi

IPostTune:
	cd $(L3Tdir) ; $(MAKE) res/atlas_trsmNB.h
	cd $(BLDdir)/src/blas/level3 ; $(MAKE) lib
error_report :
	cd $(BLDdir) ; $(MAKE) error_report

IBozoL1: IBozoL1.grd
$(BLDdir)/ARCHS/BOZOL1 : $(SRCdir)/CONFIG/ARCHS/BOZOL1.tar.bz2
	cd $(BLDdir)/ARCHS ; \
           $(BUNZIP) -c $(SRCdir)/CONFIG/ARCHS/BOZOL1.tar.bz2 | tar xvf -
	
IBozoL1.grd : $(BLDdir)/ARCHS/BOZOL1
	$(MAKE) IForceL1 pre=$(pre) defdir=$(BLDdir)/BOZOL1
	touch IBozoL1.grd
IForceL1:
	- cp -f $(defdir)/*_SUMM $(L1Tdir)/res/.

UntarArch: $(BLDdir)/ARCHS/$(ARCH)
$(BLDdir)/ARCHS/$(ARCH): $(ADdir)/$(ARCH).tar.bz2
	cd $(BLDdir)/ARCHS ; \
           $(BUNZIP) -c $(ADdir)/$(ARCH).tar.bz2 | tar xvf -
IArchDef.grd : $(BLDdir)/ARCHS/$(ARCH)
	cd $(BLDdir)/ARCHS ; \
           $(MAKE) archput
	touch IArchDef.grd
	


IStage1:
	- cd $(AUXdir) ; $(MAKE) -s ATL_buildinfo.o
	cd $(AUXdir) ; $(MAKE) lib
	cd $(TSTdir) ; $(MAKE) lib
	$(MAKE) INSTALL_LOG/L1CacheSize
	$(MAKE) INSTALL_LOG/sMULADD pre=s
	$(MAKE) INSTALL_LOG/dMULADD pre=d
	cd $(SYSdir) ; $(MAKE) RunLamch

IRunMADef :
	cd $(SYSdir) ; $(MAKE) RunMADef pre=$(pre)

IRunMMDef :
	cd $(MMTdir) ; $(MAKE) RunMMDef pre=$(pre)
	cd $(MMTdir) ; ./xemit_mm  -p $(pre) -R -2
	cd $(MMTdir) ; $(MAKE) install pre=$(pre)
IKillL1 : force_build
	rm -f $(SYSdir)/res/L1CacheSize INSTALL_LOG/L1CacheSize

$(SYSdir)/res/L1CacheSize:
	cd $(SYSdir) ; $(MAKE) res/L1CacheSize

INSTALL_LOG/L1CacheSize : $(SYSdir)/res/L1CacheSize
	cp $(SYSdir)/res/L1CacheSize INSTALL_LOG/.

IL1Override : force_build
	rm -f $(SYSdir)/res/L1CacheSize
	cp $(tnam) $(SYSdir)/res/L1CacheSize
	rm -f $(tnam)

IKillMuladd : force_build
	rm -f $(SYSdir)/res/$(pre)MULADD $(SYSdir)/res/$(pre)muladd* \
              INSTALL_LOG/$(pre)MULADD

$(SYSdir)/res/$(pre)MULADD :
	cd $(SYSdir) ; $(MAKE) res/$(pre)MULADD pre=$(pre)

INSTALL_LOG/$(pre)MULADD :  $(SYSdir)/res/$(pre)MULADD
	cp $(SYSdir)/res/$(pre)MULADD INSTALL_LOG/.

$(INCAdir)atlas_$(pre)sysinfo.h :
	cd $(SYSdir) ; $(MAKE) $(INCAdir)/atlas_$(pre)sysinfo.h

INSTALL_LOG/atlas_$(pre)sysinfo.h : $(INCAdir)/res/atlas_$(pre)sysinfo.h
	cp $(INCAdir)/atlas_$(pre)sysinfo.h INSTALL_LOG/.

IKillMMres: force_build
	rm -f $(MMTdir)/res/* INSTALL_LOG/$(pre)MMRES.sum


Irbob :
	rm -f $(tnam)
	cd $(MMTdir) ; $(MAKE) $(pre)rbob pre=$(pre) muladd=$(muladd) \
           ldc=$(nb) nb=$(nb) mu=$(mu) nu=$(nu) lat=$(lat) ku=$(ku)
	cp $(MMTdir)/res/C$(pre)NB$(nb)_$(mu)x$(nu)x$(ku)_$(muladd)-$(lat).mflop $(tnam)
	echo $(muladd) $(lat) $(nb) $(mu) $(nu) $(ku) >> $(tnam)
Icbob :
	rm -f $(tnam)
	cd $(MMTdir) ; $(MAKE) cbob pre=$(pre) muladd=$(muladd) ldc=$(nb) \
                                 nb=$(nb) mu=$(mu) nu=$(nu) lat=$(lat) ku=$(ku)
	cp $(MMTdir)/res/C$(pre)NB$(nb)_$(mu)x$(nu)x$(ku)_$(muladd)-$(lat).mflop $(tnam)
	echo $(muladd) $(lat) $(nb) $(mu) $(nu) $(ku) >> $(tnam)

Imakefch:
	rm -f $(INCAdir)/atlas_$(pre)sysinfo.h
	cp $(tnam) $(INCAdir)/atlas_$(pre)sysinfo.h
	rm -f $(tnam)

INSTALL_LOG/smmstandard : 
	cd $(MMTdir) ; $(MAKE) rready $(sATLres)
	cd $(MMTdir) ; $(MAKE) install pre=s typ=SREAL
	$(MAKE) Irbob tnam=INSTALL_LOG/smmstandard $(sATLres)
INSTALL_LOG/dmmstandard : 
	cd $(MMTdir) ; $(MAKE) rready $(dATLres)
	cd $(MMTdir) ; $(MAKE) install pre=d typ=DREAL
	$(MAKE) Irbob tnam=INSTALL_LOG/dmmstandard $(dATLres)
INSTALL_LOG/cmmstandard : 
	cd $(MMTdir) ; $(MAKE) cready $(cATLres)
	cd $(MMTdir) ; $(MAKE) install pre=c typ=SCPLX
	$(MAKE) Icbob tnam=INSTALL_LOG/cmmstandard $(cATLres)
INSTALL_LOG/zmmstandard : 
	cd $(MMTdir) ; $(MAKE) cready $(zATLres)
	cd $(MMTdir) ; $(MAKE) install pre=z typ=DCPLX
	$(MAKE) Icbob tnam=INSTALL_LOG/zmmstandard $(zATLres)

$(L3Tdir)/res/atlas_$(pre)trsmXover.h:
	cd $(L3Tdir) ; $(MAKE) res/atlas_$(pre)trsmXover.h pre=$(pre)

INSTALL_LOG/atlas_$(pre)trsmXover.h : $(L3Tdir)/res/atlas_$(pre)trsmXover.h
	cp $(INCAdir)/atlas_$(pre)trsmXover.h INSTALL_LOG/.

$(MMTdir)/res/$(pre)bestNN_0x0x0 :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestNN_0x0x0 pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestNN_0x0x0 : \
   $(MMTdir)/res/$(pre)bestNN_0x0x0
	cp $(MMTdir)/res/$(pre)bestNN_0x0x0 INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestNN_0x0x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestNN_0x0x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestNN_0x0x$(nb) : \
   $(MMTdir)/res/$(pre)bestNN_0x0x$(nb)
	cp $(MMTdir)/res/$(pre)bestNN_0x0x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestNN_$(nb)x$(nb)x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestNN_$(nb)x$(nb)x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestNN_$(nb)x$(nb)x$(nb) : \
   $(MMTdir)/res/$(pre)bestNN_$(nb)x$(nb)x$(nb)
	cp $(MMTdir)/res/$(pre)bestNN_$(nb)x$(nb)x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestNT_0x0x0 :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestNT_0x0x0 pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestNT_0x0x0 : \
   $(MMTdir)/res/$(pre)bestNT_0x0x0
	cp $(MMTdir)/res/$(pre)bestNT_0x0x0 INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestNT_0x0x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestNT_0x0x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestNT_0x0x$(nb) : \
   $(MMTdir)/res/$(pre)bestNT_0x0x$(nb)
	cp $(MMTdir)/res/$(pre)bestNT_0x0x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestNT_$(nb)x$(nb)x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestNT_$(nb)x$(nb)x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestNT_$(nb)x$(nb)x$(nb) : \
   $(MMTdir)/res/$(pre)bestNT_$(nb)x$(nb)x$(nb)
	cp $(MMTdir)/res/$(pre)bestNT_$(nb)x$(nb)x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestTN_0x0x0 :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestTN_0x0x0 pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestTN_0x0x0 : \
   $(MMTdir)/res/$(pre)bestTN_0x0x0
	cp $(MMTdir)/res/$(pre)bestTN_0x0x0 INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestTN_0x0x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestTN_0x0x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestTN_0x0x$(nb) : \
   $(MMTdir)/res/$(pre)bestTN_0x0x$(nb)
	cp $(MMTdir)/res/$(pre)bestTN_0x0x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestTN_$(nb)x$(nb)x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestTN_$(nb)x$(nb)x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestTN_$(nb)x$(nb)x$(nb) : \
   $(MMTdir)/res/$(pre)bestTN_$(nb)x$(nb)x$(nb)
	cp $(MMTdir)/res/$(pre)bestTN_$(nb)x$(nb)x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestTT_0x0x0 :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestTT_0x0x0 pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestTT_0x0x0 : \
   $(MMTdir)/res/$(pre)bestTT_0x0x0
	cp $(MMTdir)/res/$(pre)bestTT_0x0x0 INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestTT_0x0x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestTT_0x0x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestTT_0x0x$(nb) : \
   $(MMTdir)/res/$(pre)bestTT_0x0x$(nb)
	cp $(MMTdir)/res/$(pre)bestTT_0x0x$(nb) INSTALL_LOG/.
$(MMTdir)/res/$(pre)bestTT_$(nb)x$(nb)x$(nb) :
	cd $(MMTdir) ; $(MAKE) res/$(pre)bestTT_$(nb)x$(nb)x$(nb) pre=$(pre) bnb=$(nb)
INSTALL_LOG/$(pre)bestTT_$(nb)x$(nb)x$(nb) : \
   $(MMTdir)/res/$(pre)bestTT_$(nb)x$(nb)x$(nb)
	cp $(MMTdir)/res/$(pre)bestTT_$(nb)x$(nb)x$(nb) INSTALL_LOG/.

$(R1Tdir)/res/$(pre)R2K.sum : $(R1Tdir)/res/$(pre)R1K.sum force_build
	cd $(R1Tdir) ; $(MAKE) res/$(pre)R2K.sum pre=$(pre)
$(R1Tdir)/res/$(pre)R1K.sum : force_build
	cd $(R1Tdir) ; $(MAKE) res/$(pre)R1K.sum pre=$(pre)
INSTALL_LOG/$(pre)R1K.sum : $(R1Tdir)/res/$(pre)R1K.sum
	cp $(R1Tdir)/res/$(pre)R1K.sum INSTALL_LOG/.
INSTALL_LOG/$(pre)R2K.sum : INSTALL_LOG/$(pre)R1K.sum \
   $(R1Tdir)/res/$(pre)R2K.sum
	cp $(R1Tdir)/res/$(pre)R2K.sum INSTALL_LOG/.
	cd $(R1Tdir) ; $(MAKE) $(pre)nxtune

$(MVTdir)/res/$(pre)MVNK.sum : force_build
	cd $(MVTdir) ; $(MAKE) res/$(pre)MVNK.sum pre=$(pre)
INSTALL_LOG/$(pre)MVNK.sum : $(MVTdir)/res/$(pre)MVNK.sum
	cp $(MVTdir)/res/$(pre)MVNK.sum INSTALL_LOG/.
$(MVTdir)/res/$(pre)MVTK.sum : force_build
	cd $(MVTdir) ; $(MAKE) res/$(pre)MVTK.sum pre=$(pre)
INSTALL_LOG/$(pre)MVTK.sum : $(MVTdir)/res/$(pre)MVTK.sum
	cp $(MVTdir)/res/$(pre)MVTK.sum INSTALL_LOG/.


$(MMTdir)/res/$(pre)Xover.h :
	cd $(MMTdir) ; $(MAKE) res/$(pre)Xover.h pre=$(pre) nb=$(nb)
INSTALL_LOG/$(pre)Xover.h : $(MMTdir)/res/$(pre)Xover.h
	cp $(MMTdir)/res/$(pre)Xover.h INSTALL_LOG/.
$(MMTdir)/res/$(pre)NCNB :
	cd $(MMTdir) ; $(MAKE) res/$(pre)NCNB pre=$(pre) nb=$(nb)
INSTALL_LOG/$(pre)NCNB : $(MMTdir)/res/$(pre)NCNB
	cp $(MMTdir)/res/$(pre)NCNB INSTALL_LOG/.
$(MMTdir)/res/$(pre)MMRES.sum :
	cd $(MMTdir) ; $(MAKE) res/$(pre)MMRES.sum pre=$(pre) nb=$(nb)
INSTALL_LOG/$(pre)MMRES.sum : $(MMTdir)/res/$(pre)MMRES.sum
	cp $(MMTdir)/res/$(pre)MMRES.sum INSTALL_LOG/.
MMinstall :
	cd $(MMTdir) ; $(MAKE) $(pre)install
$(MMTdir)/res/atlas_cachedge.h :
	cd $(MMTdir) ; $(MAKE) res/atlas_cacheedge.h pre=$(pre)
INSTALL_LOG/atlas_cacheedge.h : $(MMTdir)/res/atlas_cachedge.h
	cp $(MMTdir)/res/atlas_cacheedge.h $(INCAdir)/.
	cp $(MMTdir)/res/atlas_cacheedge.h INSTALL_LOG/.
$(MMTdir)/res/atlas_zdNKB.h :
	cd $(MMTdir) ; $(MAKE) res/atlas_zdNKB.h
$(MMTdir)/res/atlas_csNKB.h :
	cd $(MMTdir) ; $(MAKE) res/atlas_csNKB.h
INSTALL_LOG/atlas_zdNKB.h : $(MMTdir)/res/atlas_zdNKB.h
	cp $(MMTdir)/res/atlas_zdNKB.h $(INCAdir)/.
	cp $(MMTdir)/res/atlas_zdNKB.h INSTALL_LOG/.
INSTALL_LOG/atlas_csNKB.h : $(MMTdir)/res/atlas_csNKB.h
	cp $(MMTdir)/res/atlas_csNKB.h $(INCAdir)/.
	cp $(MMTdir)/res/atlas_csNKB.h INSTALL_LOG/.

Il3lib : force_build
	cd $(L3Bdir) ; $(MAKE) $(pre)lib

auxlib : force_build
	cd $(AUXdir) ; $(MAKE) lib
auxlib_pt : force_build
	cd $(AUXdir) ; $(MAKE) ptlib

xprint_buildinfo : $(mySRCdir)/print_buildinfo.c
	$(KC) $(KCFLAGS) -o $@ $(mySRCdir)/print_buildinfo.c $(ATLASlib)
xdlp1000: ftime00.o dlp1000.o dlapack dl3lib dl2lib dl1lib dcblas dfblas
	$(FLINKER) $(FLINKFLAGS) -o $@ dlp1000.o ftime00.o \
	$(LAPACKlib) $(CBLASlib) $(F77BLASlib) $(ATLASlib) $(LIBS)
ftime00.o : $(mySRCdir)/ftime00.c
	$(KC) -o $@ -c $(KCFLAGS) $(mySRCdir)/ftime00.c
dlp1000.o : $(mySRCdir)/dlp1000.f
	$(F77) -o $@ -c $(FFLAGS) $(mySRCdir)/dlp1000.f

sanity_test: 
	rm -f sanity.out
	$(MAKE) ssanity_test dsanity_test csanity_test zsanity_test
ptsanity_test:
	rm -f ptsanity.out
	$(MAKE) ssanity_test_pt dsanity_test_pt csanity_test_pt zsanity_test_pt

shared: shared.grd
ptshared: ptshared.grd
ptshared.grd :
	cd $(LIBdir) : make ptshared
shared.grd :
	cd $(LIBdir) ; make shared
killshared:
	rm -f $(LIBdir)/libatlas.so $(LIBdir)/libcatlas.so \
              shared.grd ptshared.grd

xslamchtest : $(mySRCdir)/slamchtest.f $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) $(F77NOOPT) -o $@ $(mySRCdir)/slamchtest.f \
               $(FLAPACKlib) $(ATLASlib)
xdlamchtest : $(mySRCdir)/dlamchtest.f $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) $(F77NOOPT) -o $@ $(mySRCdir)/dlamchtest.f \
               $(FLAPACKlib) $(ATLASlib)
xsgelstest : $(mySRCdir)/sgelstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/sgelstest.f90 \
               $(FLAPACKlib) $(LAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xsscltest : $(mySRCdir)/sscltest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/sscltest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xsger2test : $(mySRCdir)/sger2test.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/sger2test.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xstranstest : $(mySRCdir)/stranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/stranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xsqrtranstest : $(mySRCdir)/sqrtranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/sqrtranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xsl1blastst_pt_dyn : sl1blastst_pt.o sl1lib_pt ststlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl1blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsl2blastst_pt_dyn : sl2blastst_pt.o sl2lib_pt ststlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl2blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsl3blastst_pt_dyn : $(INCAdir)/atlas_type.h sl3blastst_pt.o \
                   ststlib_pt sl3lib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsorm_all_pt_dyn : xsormtst_pt xsormtstF_pt \
                   xsormtst_sys_pt xsormtstF_sys_pt PTshared

xsormtst_pt_dyn : sormtst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f sormtst_pt.o
	$(MAKE) sormtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsormtstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f sormtst_pt.o
	$(MAKE) sormtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsqr_all_pt_dyn : xsqrtst_pt xsqrtstF_pt \
                   xsqrtst_sys_pt xsqrtstF_sys_pt PTshared

xsqrtst_pt_dyn : sqrtst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f sqrtst_pt.o
	$(MAKE) sqrtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsqrtstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f sqrtst_pt.o
	$(MAKE) sqrtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsinv_all_pt_dyn : xsinvtst_pt xsinvtstF_pt \
                   xsinvtst_sys_pt xsinvtstF_sys_pt PTshared

xsinvtst_pt_dyn : sinvtst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f sinvtst_pt.o
	$(MAKE) sinvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsinvtstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f sinvtst_pt.o
	$(MAKE) sinvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xstrtri_all_pt_dyn : xstrtritst_pt xstrtritstF_pt \
                   xstrtritst_sys_pt xstrtritstF_sys_pt PTshared

xstrtritst_pt_dyn : strtritst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f strtritst_pt.o
	$(MAKE) strtritst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xstrtritstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f strtritst_pt.o
	$(MAKE) strtritst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsuum_all_pt_dyn : xsuumtst_pt xsuumtstF_pt \
                   xsuumtst_sys_pt xsuumtstF_sys_pt PTshared

xsuumtst_pt_dyn : suumtst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f suumtst_pt.o
	$(MAKE) suumtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsuumtstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f suumtst_pt.o
	$(MAKE) suumtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsslv_all_pt_dyn : xsslvtst_pt xsslvtstF_pt \
                   xsslvtst_sys_pt xsslvtstF_sys_pt PTshared

xsslvtst_pt_dyn : sslvtst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f sslvtst_pt.o
	$(MAKE) sslvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsslvtstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f sslvtst_pt.o
	$(MAKE) sslvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsllt_all_pt_dyn : xsllttst_pt xsllttstF_pt \
                   xsllttst_sys_pt xsllttstF_sys_pt PTshared

xsllttst_pt_dyn : sllttst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f sllttst_pt.o
	$(MAKE) sllttst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsllttstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f sllttst_pt.o
	$(MAKE) sllttst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xslu_all_pt_dyn : xslutst_pt xslutstF_pt \
                   xslutst_sys_pt xslutstF_sys_pt PTshared

xslutst_pt_dyn : slutst_pt.o slapack ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt PTshared
	rm -f slutst_pt.o
	$(MAKE) slutst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xslutstF_pt_dyn : sPTlapack ststlib_pt sl3lib_pt sl2lib_pt \
                   sl1lib_pt scblas_pt sfblas_pt PTshared
	rm -f slutst_pt.o
	$(MAKE) slutst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsmmtst_pt_dyn : $(INCAdir)/atlas_type.h sgemmtst_pt.o smmlib_pt ststlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_pt.o \
              $(TESTlib) $(smmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)

xsl1blastst_dyn : sl1blastst.o sl1lib ststlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl1blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsl2blastst_dyn : sl2blastst.o sl2lib ststlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl2blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsl3blastst_dyn : $(INCAdir)/atlas_type.h sl3blastst.o \
                   ststlib sl3lib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsorm_all_dyn : xsormtst xsormtstF \
                   xsormtst_sys xsormtstF_sys shared

xsormtst_dyn : sormtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f sormtst.o
	$(MAKE) sormtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsormtstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f sormtst.o
	$(MAKE) sormtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsqr_all_dyn : xsqrtst xsqrtstF \
                   xsqrtst_sys xsqrtstF_sys shared

xsqrtst_dyn : sqrtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f sqrtst.o
	$(MAKE) sqrtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsqrtstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f sqrtst.o
	$(MAKE) sqrtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsinv_all_dyn : xsinvtst xsinvtstF \
                   xsinvtst_sys xsinvtstF_sys shared

xsinvtst_dyn : sinvtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f sinvtst.o
	$(MAKE) sinvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsinvtstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f sinvtst.o
	$(MAKE) sinvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xstrtri_all_dyn : xstrtritst xstrtritstF \
                   xstrtritst_sys xstrtritstF_sys shared

xstrtritst_dyn : strtritst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f strtritst.o
	$(MAKE) strtritst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xstrtritstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f strtritst.o
	$(MAKE) strtritst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsuum_all_dyn : xsuumtst xsuumtstF \
                   xsuumtst_sys xsuumtstF_sys shared

xsuumtst_dyn : suumtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f suumtst.o
	$(MAKE) suumtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsuumtstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f suumtst.o
	$(MAKE) suumtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsslv_all_dyn : xsslvtst xsslvtstF \
                   xsslvtst_sys xsslvtstF_sys shared

xsslvtst_dyn : sslvtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f sslvtst.o
	$(MAKE) sslvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsslvtstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f sslvtst.o
	$(MAKE) sslvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsllt_all_dyn : xsllttst xsllttstF \
                   xsllttst_sys xsllttstF_sys shared

xsllttst_dyn : sllttst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f sllttst.o
	$(MAKE) sllttst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsllttstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f sllttst.o
	$(MAKE) sllttst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xslu_all_dyn : xslutst xslutstF \
                   xslutst_sys xslutstF_sys shared

xslutst_dyn : slutst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas shared
	rm -f slutst.o
	$(MAKE) slutst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xslutstF_dyn : slapack ststlib sl3lib sl2lib \
                   sl1lib scblas sfblas shared
	rm -f slutst.o
	$(MAKE) slutst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xsmmtst_dyn : $(INCAdir)/atlas_type.h sgemmtst.o smmlib ststlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst.o \
              $(TESTlib) $(smmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)


xsl1blastst_mike : sl1blastst_mike.o sl1lib_pt ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl1blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xsl2blastst_mike : sl2blastst_pt.o sl2lib_pt ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl2blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xsl3blastst_mike : $(INCAdir)/atlas_type.h sl3blastst_pt.o \
                   ststlib_pt sl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)

ststlib_tb : ststlib_pt
ststlib_sb : ststlib
sl1lib_sb : sl1lib
sl1lib_pb : sl1lib_pt
sl2lib_sb : sl2lib
sl2lib_pb : sl2lib_pt
sl3lib_sb : sl3lib
sl3lib_pb : sl3lib_pt
#
# Ugly renaming of old system to new; new is <pre>[t,s]name, where
# 't' is threaded and 's' is serial
#
sslapack : slapack
stlapack : slapack_pt
ssl3lib : sl3lib
stl3lib : sl3lib_pt
ssl2lib : sl2lib
stl2lib : sl2lib_pt
ssl1lib : sl1lib
stl1lib : sl1lib_pt
sststlib : ststlib
sttstlib : ststlib_pt
ssfblas : sfblas
stfblas : sfblas_pt
sscblas : scblas
stcblas : scblas_pt
#
# Default lanbtime, calls ATLAS's internal iface
#
xsslatime : slatime.o sslapack sststlib \
      ssl3lib ssl2lib ssl1lib ssfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slatime.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xstlatime : stlatime.o stlapack sttstlib \
      stl3lib stl2lib stl1lib stfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ stlatime.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
#
# Targets for building latime with the F77 iface, using ATLAS, system,
# or F77 BLAS and LAPACK in serial and parallel
#
# ATLAS-ATLAS
xsslatime_al_ab : slatimeF.o sslapack sststlib \
      ssl3lib ssl2lib ssl1lib ssfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slatimeF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xstlatime_al_ab : stlatimeF.o stlapack sttstlib \
      stl3lib stl2lib stl1lib stfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ stlatimeF.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# ATLAS2-ATLAS2 (to compare against competing ATLAS install)
xsslatime_2l_2b : slatimeF.o sststlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ slatimeF.o \
                   $(TESTlib) $(AT2dir)/liblapack.a $(AT2dir)/libcblas.a \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xstlatime_2l_2b : stlatimeF.o sttstlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ stlatimeF.o \
                   $(TESTlib) $(AT2dir)/libptlapack.a $(AT2dir)/libptcblas.a \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
# F77-ATLAS
xsslatime_fl_ab : slatimeF.o sslapack sststlib \
      ssl3lib ssl2lib ssl1lib ssfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xstlatime_fl_ab : stlatimeF.o stlapack sttstlib \
      stl3lib stl2lib stl1lib stfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ stlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# System-system; CBLASlib in there since tester can call that iface
xsslatime_sl_sb : slatimeF.o sslapack sststlib \
      ssl3lib ssl2lib ssl1lib ssfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slatimeF.o \
                   $(TESTlib) $(SSLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xstlatime_sl_sb : stlatimeF.o stlapack sttstlib \
      stl3lib stl2lib stl1lib stfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ stlatimeF.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
# F77-system
xsslatime_fl_sb : slatimeF.o sslapack sststlib \
      ssl3lib ssl2lib ssl1lib ssfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xstlatime_fl_sb : stlatimeF.o stlapack sttstlib \
      stl3lib stl2lib stl1lib stfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ stlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)

slatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeF77  -DSREAL $(mySRCdir)/latime.c
slatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DSREAL $(mySRCdir)/latime.c
slatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL $(mySRCdir)/latime.c
scblas_sb : scblas
stlatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DTimeF77 \
                 -DSREAL $(mySRCdir)/latime.c
stlatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DATL_USEPTHREADS=1 -DSREAL \
                 $(mySRCdir)/latime.c
stlatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DSREAL \
                 $(mySRCdir)/latime.c
scblas_sb : scblas
scblas_tb : scblas_pt
slapack_sl : slapack
slapack_al : slapack_pt
# All ATLAS libs, C interface to lapack
xsormtst_tl_tb : sormtstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsqrtst_tl_tb : sqrtstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsinvtst_tl_tb : sinvtstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xstrtritst_tl_tb : strtritstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsuumtst_tl_tb : suumtstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsslvtst_tl_tb : sslvtstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsllttst_tl_tb : sllttstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslutst_tl_tb : slutstC_pt.o slapack_tl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsormtst_sl_tb : sormtstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsqrtst_sl_tb : sqrtstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsinvtst_sl_tb : sinvtstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xstrtritst_sl_tb : strtritstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsuumtst_sl_tb : suumtstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsslvtst_sl_tb : sslvtstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsllttst_sl_tb : sllttstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslutst_sl_tb : slutstC.o slapack_sl ststlib_tb sl3lib_tb \
                  sl2lib_tb sl1lib_tb scblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsormtst_tl_sb : sormtstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsqrtst_tl_sb : sqrtstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsinvtst_tl_sb : sinvtstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xstrtritst_tl_sb : strtritstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsuumtst_tl_sb : suumtstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsslvtst_tl_sb : sslvtstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsllttst_tl_sb : sllttstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslutst_tl_sb : slutstC_pt.o slapack_tl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsormtst_sl_sb : sormtstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsqrtst_sl_sb : sqrtstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsinvtst_sl_sb : sinvtstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xstrtritst_sl_sb : strtritstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsuumtst_sl_sb : suumtstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsslvtst_sl_sb : sslvtstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsllttst_sl_sb : sllttstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslutst_sl_sb : slutstC.o slapack_sl ststlib_sb sl3lib_sb \
                  sl2lib_sb sl1lib_sb scblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
xsl1blastst_pt : sl1blastst_pt.o sl1lib_pt ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl1blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xsl2blastst_pt : sl2blastst_pt.o sl2lib_pt ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl2blastst_pt.o \
                   $(TESTlib) $(BLASlib)  $(ATLASlib) $(LIBS)
xsl3blastst_pt : $(INCAdir)/atlas_type.h sl3blastst_pt.o \
                   ststlib_pt sl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
ssanity_test_pt: xsinvtst_pt xsqrtst_pt xsslvtst_pt xstrtritst_pt xsuumtst_pt
	$(ATLRUN) $(BINdir) xsslvtst_pt -n 477 -r 517 -O 2 c r \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xsslvtst_pt -n 477 -r 517 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xsqrtst_pt -n 1 477 -m 1 517 -U 2 u l \
                  -S 2 r l >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xstrtritst_pt -n 477  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xsuumtst_pt -n 477 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsormtst_pt : sormtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsormtstC_pt : sormtstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsormtstF_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsormtst_sb_pt : sormtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsormtstF_sb_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsormtstF_sl_sb_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsormtstF_sl_ab_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsormtstF_fl_ab_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsormtstF_fl_sb_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsormtstF_fl_fb_pt : sormtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsorm_all_pt : xsormtst_pt xsormtstF_pt \
                   xsormtst_sb_pt xsormtstF_sys_pt \
                   xsormtst_sb_pt xsormtstF_sl_sb_pt \
                   xsormtstF_fl_ab_pt xsormtstF_sl_ab_pt \
                   xsormtstF_fl_fb_pt xsormtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsqrtst_pt : sqrtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsqrtstC_pt : sqrtstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsqrtstF_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsqrtst_sb_pt : sqrtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsqrtstF_sb_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsqrtstF_sl_sb_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsqrtstF_sl_ab_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsqrtstF_fl_ab_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsqrtstF_fl_sb_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsqrtstF_fl_fb_pt : sqrtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsqr_all_pt : xsqrtst_pt xsqrtstF_pt \
                   xsqrtst_sb_pt xsqrtstF_sys_pt \
                   xsqrtst_sb_pt xsqrtstF_sl_sb_pt \
                   xsqrtstF_fl_ab_pt xsqrtstF_sl_ab_pt \
                   xsqrtstF_fl_fb_pt xsqrtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xslanbtst_pt : slanbtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslanbtstC_pt : slanbtstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xslanbtstF_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xslanbtst_sb_pt : slanbtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xslanbtstF_sb_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xslanbtstF_sl_sb_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xslanbtstF_sl_ab_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xslanbtstF_fl_ab_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xslanbtstF_fl_sb_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xslanbtstF_fl_fb_pt : slanbtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xslanb_all_pt : xslanbtst_pt xslanbtstF_pt \
                   xslanbtst_sb_pt xslanbtstF_sys_pt \
                   xslanbtst_sb_pt xslanbtstF_sl_sb_pt \
                   xslanbtstF_fl_ab_pt xslanbtstF_sl_ab_pt \
                   xslanbtstF_fl_fb_pt xslanbtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsinvtst_pt : sinvtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsinvtstC_pt : sinvtstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsinvtstF_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsinvtst_sb_pt : sinvtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsinvtstF_sb_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsinvtstF_sl_sb_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsinvtstF_sl_ab_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsinvtstF_fl_ab_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsinvtstF_fl_sb_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsinvtstF_fl_fb_pt : sinvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsinv_all_pt : xsinvtst_pt xsinvtstF_pt \
                   xsinvtst_sb_pt xsinvtstF_sys_pt \
                   xsinvtst_sb_pt xsinvtstF_sl_sb_pt \
                   xsinvtstF_fl_ab_pt xsinvtstF_sl_ab_pt \
                   xsinvtstF_fl_fb_pt xsinvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xstrtritst_pt : strtritst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xstrtritstC_pt : strtritstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xstrtritstF_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xstrtritst_sb_pt : strtritst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xstrtritstF_sb_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xstrtritstF_sl_sb_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xstrtritstF_sl_ab_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xstrtritstF_fl_ab_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xstrtritstF_fl_sb_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xstrtritstF_fl_fb_pt : strtritstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xstrtri_all_pt : xstrtritst_pt xstrtritstF_pt \
                   xstrtritst_sb_pt xstrtritstF_sys_pt \
                   xstrtritst_sb_pt xstrtritstF_sl_sb_pt \
                   xstrtritstF_fl_ab_pt xstrtritstF_sl_ab_pt \
                   xstrtritstF_fl_fb_pt xstrtritstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsuumtst_pt : suumtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsuumtstC_pt : suumtstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsuumtstF_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsuumtst_sb_pt : suumtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsuumtstF_sb_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsuumtstF_sl_sb_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsuumtstF_sl_ab_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsuumtstF_fl_ab_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsuumtstF_fl_sb_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsuumtstF_fl_fb_pt : suumtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsuum_all_pt : xsuumtst_pt xsuumtstF_pt \
                   xsuumtst_sb_pt xsuumtstF_sys_pt \
                   xsuumtst_sb_pt xsuumtstF_sl_sb_pt \
                   xsuumtstF_fl_ab_pt xsuumtstF_sl_ab_pt \
                   xsuumtstF_fl_fb_pt xsuumtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsslvtst_pt : sslvtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsslvtstC_pt : sslvtstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsslvtstF_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsslvtst_sb_pt : sslvtst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsslvtstF_sb_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsslvtstF_sl_sb_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsslvtstF_sl_ab_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsslvtstF_fl_ab_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsslvtstF_fl_sb_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsslvtstF_fl_fb_pt : sslvtstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsslv_all_pt : xsslvtst_pt xsslvtstF_pt \
                   xsslvtst_sb_pt xsslvtstF_sys_pt \
                   xsslvtst_sb_pt xsslvtstF_sl_sb_pt \
                   xsslvtstF_fl_ab_pt xsslvtstF_sl_ab_pt \
                   xsslvtstF_fl_fb_pt xsslvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsllttst_pt : sllttst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsllttstC_pt : sllttstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsllttstF_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsllttst_sb_pt : sllttst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsllttstF_sb_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsllttstF_sl_sb_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsllttstF_sl_ab_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsllttstF_fl_ab_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsllttstF_fl_sb_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsllttstF_fl_fb_pt : sllttstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xsllt_all_pt : xsllttst_pt xsllttstF_pt \
                   xsllttst_sb_pt xsllttstF_sys_pt \
                   xsllttst_sb_pt xsllttstF_sl_sb_pt \
                   xsllttstF_fl_ab_pt xsllttstF_sl_ab_pt \
                   xsllttstF_fl_fb_pt xsllttstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xslutst_pt : slutst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslutstC_pt : slutstC_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xslutstF_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xslutst_sb_pt : slutst_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xslutstF_sb_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xslutstF_sl_sb_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xslutstF_sl_ab_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xslutstF_fl_ab_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xslutstF_fl_sb_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xslutstF_fl_fb_pt : slutstF_pt.o slapack_pt ststlib_pt sl3lib_pt \
                  sl2lib_pt sl1lib_pt scblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xslu_all_pt : xslutst_pt xslutstF_pt \
                   xslutst_sb_pt xslutstF_sys_pt \
                   xslutst_sb_pt xslutstF_sl_sb_pt \
                   xslutstF_fl_ab_pt xslutstF_sl_ab_pt \
                   xslutstF_fl_fb_pt xslutstF_fl_sb_pt
xsmmtst_pt : $(INCAdir)/atlas_type.h sgemmtst_pt.o smmlib_pt ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_pt.o \
              $(TESTlib) $(BLASlib) $(smmobj) $(ATLASlib) $(LIBS)

xsl1blastst : sl1blastst.o sl1lib ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl1blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
xsl2blastst : sl2blastst.o sl2lib ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl2blastst.o \
                   $(TESTlib) $(SBLASlib)  $(ATLASlib) $(LIBS)
xsl3blastst : $(INCAdir)/atlas_type.h sl3blastst.o \
                   ststlib sl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
ssanity_test: xsinvtst xsqrtst xsslvtst xstrtritst xsuumtst
	$(ATLRUN) $(BINdir) xsslvtst -n 167 -r 83 -O 2 c r \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xsslvtst -n 167 -r 83 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xsqrtst -n 1 167 -m 1 83 -U 2 u l \
                  -S 2 r l >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xstrtritst -n 167  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xsuumtst -n 167 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsormtst : sormtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsormtstC : sormtstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsormtstF : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsormtst_sb : sormtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsormtstF_sb : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsormtstF_sl_sb : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsormtstF_sl_ab : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsormtstF_fl_ab : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsormtstF_fl_sb : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsormtstF_fl_fb : sormtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xsorm_all : xsormtst xsormtstF \
                   xsormtst_sb xsormtstF_sys \
                   xsormtst_sb xsormtstF_sl_sb \
                   xsormtstF_fl_ab xsormtstF_sl_ab \
                   xsormtstF_fl_fb xsormtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsqrtst : sqrtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsqrtstC : sqrtstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsqrtstF : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsqrtst_sb : sqrtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsqrtstF_sb : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsqrtstF_sl_sb : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsqrtstF_sl_ab : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsqrtstF_fl_ab : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsqrtstF_fl_sb : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsqrtstF_fl_fb : sqrtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xsqr_all : xsqrtst xsqrtstF \
                   xsqrtst_sb xsqrtstF_sys \
                   xsqrtst_sb xsqrtstF_sl_sb \
                   xsqrtstF_fl_ab xsqrtstF_sl_ab \
                   xsqrtstF_fl_fb xsqrtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xslanbtst : slanbtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslanbtstC : slanbtstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xslanbtstF : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xslanbtst_sb : slanbtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xslanbtstF_sb : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xslanbtstF_sl_sb : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xslanbtstF_sl_ab : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xslanbtstF_fl_ab : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xslanbtstF_fl_sb : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xslanbtstF_fl_fb : slanbtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xslanb_all : xslanbtst xslanbtstF \
                   xslanbtst_sb xslanbtstF_sys \
                   xslanbtst_sb xslanbtstF_sl_sb \
                   xslanbtstF_fl_ab xslanbtstF_sl_ab \
                   xslanbtstF_fl_fb xslanbtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsinvtst : sinvtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsinvtstC : sinvtstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsinvtstF : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsinvtst_sb : sinvtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsinvtstF_sb : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsinvtstF_sl_sb : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsinvtstF_sl_ab : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsinvtstF_fl_ab : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsinvtstF_fl_sb : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsinvtstF_fl_fb : sinvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xsinv_all : xsinvtst xsinvtstF \
                   xsinvtst_sb xsinvtstF_sys \
                   xsinvtst_sb xsinvtstF_sl_sb \
                   xsinvtstF_fl_ab xsinvtstF_sl_ab \
                   xsinvtstF_fl_fb xsinvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xstrtritst : strtritst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xstrtritstC : strtritstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xstrtritstF : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xstrtritst_sb : strtritst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xstrtritstF_sb : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xstrtritstF_sl_sb : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xstrtritstF_sl_ab : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xstrtritstF_fl_ab : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xstrtritstF_fl_sb : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xstrtritstF_fl_fb : strtritstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ strtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xstrtri_all : xstrtritst xstrtritstF \
                   xstrtritst_sb xstrtritstF_sys \
                   xstrtritst_sb xstrtritstF_sl_sb \
                   xstrtritstF_fl_ab xstrtritstF_sl_ab \
                   xstrtritstF_fl_fb xstrtritstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsuumtst : suumtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsuumtstC : suumtstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsuumtstF : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsuumtst_sb : suumtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsuumtstF_sb : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsuumtstF_sl_sb : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsuumtstF_sl_ab : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsuumtstF_fl_ab : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsuumtstF_fl_sb : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsuumtstF_fl_fb : suumtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ suumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xsuum_all : xsuumtst xsuumtstF \
                   xsuumtst_sb xsuumtstF_sys \
                   xsuumtst_sb xsuumtstF_sl_sb \
                   xsuumtstF_fl_ab xsuumtstF_sl_ab \
                   xsuumtstF_fl_fb xsuumtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsslvtst : sslvtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsslvtstC : sslvtstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsslvtstF : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsslvtst_sb : sslvtst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsslvtstF_sb : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsslvtstF_sl_sb : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsslvtstF_sl_ab : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsslvtstF_fl_ab : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsslvtstF_fl_sb : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsslvtstF_fl_fb : sslvtstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xsslv_all : xsslvtst xsslvtstF \
                   xsslvtst_sb xsslvtstF_sys \
                   xsslvtst_sb xsslvtstF_sl_sb \
                   xsslvtstF_fl_ab xsslvtstF_sl_ab \
                   xsslvtstF_fl_fb xsslvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xsllttst : sllttst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xsllttstC : sllttstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xsllttstF : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xsllttst_sb : sllttst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xsllttstF_sb : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xsllttstF_sl_sb : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xsllttstF_sl_ab : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xsllttstF_fl_ab : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xsllttstF_fl_sb : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xsllttstF_fl_fb : sllttstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ sllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xsllt_all : xsllttst xsllttstF \
                   xsllttst_sb xsllttstF_sys \
                   xsllttst_sb xsllttstF_sl_sb \
                   xsllttstF_fl_ab xsllttstF_sl_ab \
                   xsllttstF_fl_fb xsllttstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xslutst : slutst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xslutstC : slutstC.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xslutstF : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xslutst_sb : slutst.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xslutstF_sb : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xslutstF_sl_sb : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xslutstF_sl_ab : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xslutstF_fl_ab : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xslutstF_fl_sb : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xslutstF_fl_fb : slutstF.o slapack ststlib sl3lib \
                  sl2lib sl1lib scblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ slutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xslu_all : xslutst xslutstF \
                   xslutst_sb xslutstF_sys \
                   xslutst_sb xslutstF_sl_sb \
                   xslutstF_fl_ab xslutstF_sl_ab \
                   xslutstF_fl_fb xslutstF_fl_sb
xsmmtst : $(INCAdir)/atlas_type.h sgemmtst.o smmlib ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst.o \
              $(TESTlib) $(SBLASlib) $(smmobj) $(ATLASlib) $(LIBS)

#
#  These symbols are for timing, and time the same routine twice: atlas/sys
#
xsl3blastst_sys : $(INCAdir)/atlas_type.h sl3blastst_f77.o ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xsl3blastst_sys_pt : $(INCAdir)/atlas_type.h sl3blastst_f77_pt.o \
   ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_f77.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)
xsl3blastst_atl : $(INCAdir)/atlas_type.h sl3blastst_f77.o \
   ststlib sl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xsl3blastst_atl_pt : $(INCAdir)/atlas_type.h sl3blastst_f77_pt.o \
   ststlib_pt sl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_f77.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xsl3blastst_at2 : $(INCAdir)/atlas_type.h sl3blastst_f77.o \
   ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xsl3blastst_at2_pt : $(INCAdir)/atlas_type.h sl3blastst_f77_pt.o \
   ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xsmmtst_sys : $(INCAdir)/atlas_type.h sgemmtst_f77.o ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xsmmtst_atl : $(INCAdir)/atlas_type.h sgemmtst_f77.o smmlib ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xsmmtst_atl_pt : $(INCAdir)/atlas_type.h sgemmtst_f77_pt.o smmlib_pt ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_f77_pt.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xsmmtst_at2 : sgemmtst_f77_pt.o ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xsmmtst_at2_pt : sgemmtst_f77_pt.o ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xsmmtst_sys_pt : $(INCAdir)/atlas_type.h sgemmtst_f77_pt.o ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_f77_pt.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)

sl3blastst_f77.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DSREAL $(mySRCdir)/l3blastst.c
sl3blastst_f77_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DSREAL $(mySRCdir)/l3blastst.c
sgemmtst_f77.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DSREAL $(mySRCdir)/gemmtst.c
sgemmtst_f77_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DSREAL $(mySRCdir)/gemmtst.c

noaffobj = ATL_goparallel_noaf.o ATL_dyntlaunch_noaf.o \
           ATL_log2tlaunch_noaf.o ATL_thread_start_noaf.o
noaffobjd = $(BLDdir)/tune/threads/ATL_goparallel_noaf.o \
            $(BLDdir)/tune/threads/ATL_dyntlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_log2tlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_thread_start_noaf.o 

xsmmtst_noaff : $(INCAdir)/atlas_type.h sgemmtst_noaff.o \
   smmlib_pt ststlib_pt noaff_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_noaff.o $(noaffobjd) \
              $(TESTlib) $(BLASlib) $(smmobj) $(ATLASlib) $(LIBS)
sgemmtst_noaff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_NOAFFINITY=1 \
               -DSREAL $(mySRCdir)/gemmtst.c

xsmmtst_aff : $(INCAdir)/atlas_type.h sgemmtst_aff.o smmlib_pt \
              ststlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_aff.o \
                   $(TESTlib) $(BLASlib) $(smmobj) $(ATLASlib) $(LIBS)
xsmmtst_big : $(INCAdir)/atlas_type.h sgemmtst_big.o smmlib \
              ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgemmtst_big.o \
                   $(TESTlib) $(BLASlib) $(smmobj) $(ATLASlib) $(LIBS)
$(MMTdir)/res/svMMRES :
	cd $(MMTdir) ; make res/svMMRES
	cd $(MMTdir) ; make vinstall pre=s
xsgpmmtst : $(INCAdir)/atlas_type.h sgpmmtst.o smmlib ststlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ sgpmmtst.o \
              $(TESTlib) $(BLASlib) $(smmobj) $(ATLASlib) $(LIBS)
sgpmmtst.o : $(mySRCdir)/gpmmtst.c $(INCSdir)/atlas_misc.h $(INCAdir)/smm.h \
                  $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_pkblas.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL $(mySRCdir)/gpmmtst.c
	
ststlib_pt : ststlib
ststlib :
	cd $(TSTdir) ; $(MAKE) slib

sfblas_pt : sfblaslib_pt
sfblaslib_pt :
	cd $(FBLdir) ; $(MAKE) sptlib
sfblas : sfblaslib
sfblaslib :
	cd $(FBLdir) ; $(MAKE) slib
scblas_pt : scblaslib_pt
scblaslib_pt :
	cd $(CBLdir) ; $(MAKE) sptlib
scblas : scblaslib
scblaslib :
	cd $(CBLdir) ; $(MAKE) slib

smmlib_pt : auxlib_pt
	cd $(GMMdir) ; $(MAKE) slib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) slib
smmlib : auxlib
	cd $(GMMdir) ; $(MAKE) slib
spklib : auxlib
	cd $(BLDdir)/src/blas/pklevel3 ; $(MAKE) slib

sl2lib_pt : sl2lib
	cd $(BLDdir)/src/threads/blas/level2 ; $(MAKE) slib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
sl2lib : sl1lib auxlib
	cd $(GMVdir) ; $(MAKE) slib
	cd $(GR1dir) ; $(MAKE) slib
	cd $(L2Bdir) ; $(MAKE) slib

sl3lib_pt : sl3lib smmlib_pt
	cd $(BLDdir)/src/threads ; $(MAKE) slib3
sl3lib : smmlib
	cd $(L3Bdir) ; $(MAKE) slib

sl1lib_pt : auxlib_pt sl1lib
sl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) slvl1

slapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) slib
slapack_pt : stlapack
stlapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) stlib
	cd $(BLDdir)/src/threads/lapack ; $(MAKE) stlib

xsprintblk : sprintblk.o
	$(CLINKER) -o $@ sprintblk.o $(ATLASlib)
sprintblk.o : $(INCSdir)/atlas_mv.h $(INCSdir)/atlas_r1.h \
                   $(INCAdir)/smm.h $(mySRCdir)/printblk.c
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL $(mySRCdir)/printblk.c

sl1blastst_mike.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l1blastst.c
sl2blastst_mike.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l2blastst.c

sl3blastst_mike.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/l3blastst.c
slanbtime_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
slanbtimeC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtimeF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lanbtst.c
sormtst_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/ormtst.c
sormtstC_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/ormtst.c
sormtstF_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/ormtst.c
sqrtst_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/qrtst.c
sqrtstC_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/qrtst.c
sqrtstF_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/qrtst.c
slanbtst_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtstC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtstF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lanbtst.c
sinvtst_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/invtst.c
sinvtstC_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/invtst.c
sinvtstF_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/invtst.c
strtritst_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/trtritst.c
strtritstC_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/trtritst.c
strtritstF_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/trtritst.c
sslvtst_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/slvtst.c
sslvtstC_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/slvtst.c
sslvtstF_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/slvtst.c
slutst_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lutst.c
slutstC_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lutst.c
slutstF_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/lutst.c
sllttst_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/llttst.c
sllttstC_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/llttst.c
sllttstF_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/llttst.c
suumtst_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/uumtst.c
suumtstC_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/uumtst.c
suumtstF_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSREAL \
               $(mySRCdir)/uumtst.c

sscaltst_mike.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DSREAL $(mySRCdir)/scaltst.c

sgemmtst_mike.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DSREAL $(mySRCdir)/gemmtst.c

sl1blastst_pt.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DATL_USEPTHREADS $(mySRCdir)/l1blastst.c
sl2blastst_pt.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DATL_USEPTHREADS $(mySRCdir)/l2blastst.c

sl3blastst_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/l3blastst.c
slanbtime_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
slanbtimeC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtimeF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lanbtst.c
sormtst_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/ormtst.c
sormtstC_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/ormtst.c
sormtstF_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/ormtst.c
sqrtst_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/qrtst.c
sqrtstC_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/qrtst.c
sqrtstF_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/qrtst.c
slanbtst_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtstC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtstF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lanbtst.c
sinvtst_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/invtst.c
sinvtstC_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/invtst.c
sinvtstF_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/invtst.c
strtritst_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/trtritst.c
strtritstC_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/trtritst.c
strtritstF_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/trtritst.c
sslvtst_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/slvtst.c
sslvtstC_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/slvtst.c
sslvtstF_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/slvtst.c
slutst_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lutst.c
slutstC_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lutst.c
slutstF_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/lutst.c
sllttst_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/llttst.c
sllttstC_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/llttst.c
sllttstF_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/llttst.c
suumtst_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/uumtst.c
suumtstC_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/uumtst.c
suumtstF_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSREAL \
               $(mySRCdir)/uumtst.c

sscaltst_pt.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/scaltst.c

sgemmtst_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/gemmtst.c

sl1blastst.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL  $(mySRCdir)/l1blastst.c
sl2blastst.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL  $(mySRCdir)/l2blastst.c

sl3blastst.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DSREAL \
               $(mySRCdir)/l3blastst.c
slanbtime.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
slanbtimeC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtimeF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/lanbtst.c
sormtst.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/ormtst.c
sormtstC.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/ormtst.c
sormtstF.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/ormtst.c
sqrtst.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/qrtst.c
sqrtstC.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/qrtst.c
sqrtstF.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/qrtst.c
slanbtst.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtstC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/lanbtst.c
slanbtstF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/lanbtst.c
sinvtst.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/invtst.c
sinvtstC.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/invtst.c
sinvtstF.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/invtst.c
strtritst.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/trtritst.c
strtritstC.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/trtritst.c
strtritstF.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/trtritst.c
sslvtst.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/slvtst.c
sslvtstC.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/slvtst.c
sslvtstF.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/slvtst.c
slutst.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/lutst.c
slutstC.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/lutst.c
slutstF.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/lutst.c
sllttst.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/llttst.c
sllttstC.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/llttst.c
sllttstF.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/llttst.c
suumtst.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSREAL \
               $(mySRCdir)/uumtst.c
suumtstC.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSREAL \
               $(mySRCdir)/uumtst.c
suumtstF.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSREAL \
               $(mySRCdir)/uumtst.c

sscaltst.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS)  -DSREAL $(mySRCdir)/scaltst.c

sgemmtst.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS)  -DSREAL $(mySRCdir)/gemmtst.c

sgemmtst_aff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DTRUST_BIG -DATL_AFFINITY \
               $(mySRCdir)/gemmtst.c
sgemmtst_rkX.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h skbmmlib
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DTRUST_BIG -DATL_RKXOVER \
               $(mySRCdir)/gemmtst.c
sgemmtst_rk.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DTRUST_BIG -DTEST_RANKK \
               $(mySRCdir)/gemmtst.c
sgemmtst_big.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DTRUST_BIG   \
               $(mySRCdir)/gemmtst.c
xdgelstest : $(mySRCdir)/dgelstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/dgelstest.f90 \
               $(FLAPACKlib) $(LAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xdscltest : $(mySRCdir)/dscltest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/dscltest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xdger2test : $(mySRCdir)/dger2test.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/dger2test.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xdtranstest : $(mySRCdir)/dtranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/dtranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xdqrtranstest : $(mySRCdir)/dqrtranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/dqrtranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xdl1blastst_pt_dyn : dl1blastst_pt.o dl1lib_pt dtstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl1blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdl2blastst_pt_dyn : dl2blastst_pt.o dl2lib_pt dtstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl2blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdl3blastst_pt_dyn : $(INCAdir)/atlas_type.h dl3blastst_pt.o \
                   dtstlib_pt dl3lib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdorm_all_pt_dyn : xdormtst_pt xdormtstF_pt \
                   xdormtst_sys_pt xdormtstF_sys_pt PTshared

xdormtst_pt_dyn : dormtst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dormtst_pt.o
	$(MAKE) dormtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdormtstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dormtst_pt.o
	$(MAKE) dormtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdqr_all_pt_dyn : xdqrtst_pt xdqrtstF_pt \
                   xdqrtst_sys_pt xdqrtstF_sys_pt PTshared

xdqrtst_pt_dyn : dqrtst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dqrtst_pt.o
	$(MAKE) dqrtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdqrtstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dqrtst_pt.o
	$(MAKE) dqrtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdinv_all_pt_dyn : xdinvtst_pt xdinvtstF_pt \
                   xdinvtst_sys_pt xdinvtstF_sys_pt PTshared

xdinvtst_pt_dyn : dinvtst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dinvtst_pt.o
	$(MAKE) dinvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdinvtstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dinvtst_pt.o
	$(MAKE) dinvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdtrtri_all_pt_dyn : xdtrtritst_pt xdtrtritstF_pt \
                   xdtrtritst_sys_pt xdtrtritstF_sys_pt PTshared

xdtrtritst_pt_dyn : dtrtritst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dtrtritst_pt.o
	$(MAKE) dtrtritst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdtrtritstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dtrtritst_pt.o
	$(MAKE) dtrtritst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xduum_all_pt_dyn : xduumtst_pt xduumtstF_pt \
                   xduumtst_sys_pt xduumtstF_sys_pt PTshared

xduumtst_pt_dyn : duumtst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f duumtst_pt.o
	$(MAKE) duumtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xduumtstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f duumtst_pt.o
	$(MAKE) duumtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdslv_all_pt_dyn : xdslvtst_pt xdslvtstF_pt \
                   xdslvtst_sys_pt xdslvtstF_sys_pt PTshared

xdslvtst_pt_dyn : dslvtst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dslvtst_pt.o
	$(MAKE) dslvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdslvtstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dslvtst_pt.o
	$(MAKE) dslvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdllt_all_pt_dyn : xdllttst_pt xdllttstF_pt \
                   xdllttst_sys_pt xdllttstF_sys_pt PTshared

xdllttst_pt_dyn : dllttst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dllttst_pt.o
	$(MAKE) dllttst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdllttstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dllttst_pt.o
	$(MAKE) dllttst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdlu_all_pt_dyn : xdlutst_pt xdlutstF_pt \
                   xdlutst_sys_pt xdlutstF_sys_pt PTshared

xdlutst_pt_dyn : dlutst_pt.o dlapack dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt PTshared
	rm -f dlutst_pt.o
	$(MAKE) dlutst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdlutstF_pt_dyn : dPTlapack dtstlib_pt dl3lib_pt dl2lib_pt \
                   dl1lib_pt dcblas_pt dfblas_pt PTshared
	rm -f dlutst_pt.o
	$(MAKE) dlutst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdmmtst_pt_dyn : $(INCAdir)/atlas_type.h dgemmtst_pt.o dmmlib_pt dtstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_pt.o \
              $(TESTlib) $(dmmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)

xdl1blastst_dyn : dl1blastst.o dl1lib dtstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl1blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdl2blastst_dyn : dl2blastst.o dl2lib dtstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl2blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdl3blastst_dyn : $(INCAdir)/atlas_type.h dl3blastst.o \
                   dtstlib dl3lib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdorm_all_dyn : xdormtst xdormtstF \
                   xdormtst_sys xdormtstF_sys shared

xdormtst_dyn : dormtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dormtst.o
	$(MAKE) dormtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdormtstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dormtst.o
	$(MAKE) dormtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdqr_all_dyn : xdqrtst xdqrtstF \
                   xdqrtst_sys xdqrtstF_sys shared

xdqrtst_dyn : dqrtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dqrtst.o
	$(MAKE) dqrtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdqrtstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dqrtst.o
	$(MAKE) dqrtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdinv_all_dyn : xdinvtst xdinvtstF \
                   xdinvtst_sys xdinvtstF_sys shared

xdinvtst_dyn : dinvtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dinvtst.o
	$(MAKE) dinvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdinvtstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dinvtst.o
	$(MAKE) dinvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdtrtri_all_dyn : xdtrtritst xdtrtritstF \
                   xdtrtritst_sys xdtrtritstF_sys shared

xdtrtritst_dyn : dtrtritst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dtrtritst.o
	$(MAKE) dtrtritst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdtrtritstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dtrtritst.o
	$(MAKE) dtrtritst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xduum_all_dyn : xduumtst xduumtstF \
                   xduumtst_sys xduumtstF_sys shared

xduumtst_dyn : duumtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f duumtst.o
	$(MAKE) duumtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xduumtstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f duumtst.o
	$(MAKE) duumtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdslv_all_dyn : xdslvtst xdslvtstF \
                   xdslvtst_sys xdslvtstF_sys shared

xdslvtst_dyn : dslvtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dslvtst.o
	$(MAKE) dslvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdslvtstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dslvtst.o
	$(MAKE) dslvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdllt_all_dyn : xdllttst xdllttstF \
                   xdllttst_sys xdllttstF_sys shared

xdllttst_dyn : dllttst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dllttst.o
	$(MAKE) dllttst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdllttstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dllttst.o
	$(MAKE) dllttst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdlu_all_dyn : xdlutst xdlutstF \
                   xdlutst_sys xdlutstF_sys shared

xdlutst_dyn : dlutst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas shared
	rm -f dlutst.o
	$(MAKE) dlutst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdlutstF_dyn : dlapack dtstlib dl3lib dl2lib \
                   dl1lib dcblas dfblas shared
	rm -f dlutst.o
	$(MAKE) dlutst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xdmmtst_dyn : $(INCAdir)/atlas_type.h dgemmtst.o dmmlib dtstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst.o \
              $(TESTlib) $(dmmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)


xdl1blastst_mike : dl1blastst_mike.o dl1lib_pt dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl1blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xdl2blastst_mike : dl2blastst_pt.o dl2lib_pt dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl2blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xdl3blastst_mike : $(INCAdir)/atlas_type.h dl3blastst_pt.o \
                   dtstlib_pt dl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)

dtstlib_tb : dtstlib_pt
dtstlib_sb : dtstlib
dl1lib_sb : dl1lib
dl1lib_pb : dl1lib_pt
dl2lib_sb : dl2lib
dl2lib_pb : dl2lib_pt
dl3lib_sb : dl3lib
dl3lib_pb : dl3lib_pt
#
# Ugly renaming of old system to new; new is <pre>[t,s]name, where
# 't' is threaded and 's' is serial
#
dslapack : dlapack
dtlapack : dlapack_pt
dsl3lib : dl3lib
dtl3lib : dl3lib_pt
dsl2lib : dl2lib
dtl2lib : dl2lib_pt
dsl1lib : dl1lib
dtl1lib : dl1lib_pt
dststlib : dtstlib
dttstlib : dtstlib_pt
dsfblas : dfblas
dtfblas : dfblas_pt
dscblas : dcblas
dtcblas : dcblas_pt
#
# Default lanbtime, calls ATLAS's internal iface
#
xdslatime : dlatime.o dslapack dststlib \
      dsl3lib dsl2lib dsl1lib dsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlatime.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xdtlatime : dtlatime.o dtlapack dttstlib \
      dtl3lib dtl2lib dtl1lib dtfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtlatime.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
#
# Targets for building latime with the F77 iface, using ATLAS, system,
# or F77 BLAS and LAPACK in serial and parallel
#
# ATLAS-ATLAS
xdslatime_al_ab : dlatimeF.o dslapack dststlib \
      dsl3lib dsl2lib dsl1lib dsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlatimeF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xdtlatime_al_ab : dtlatimeF.o dtlapack dttstlib \
      dtl3lib dtl2lib dtl1lib dtfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtlatimeF.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# ATLAS2-ATLAS2 (to compare against competing ATLAS install)
xdslatime_2l_2b : dlatimeF.o dststlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlatimeF.o \
                   $(TESTlib) $(AT2dir)/liblapack.a $(AT2dir)/libcblas.a \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xdtlatime_2l_2b : dtlatimeF.o dttstlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtlatimeF.o \
                   $(TESTlib) $(AT2dir)/libptlapack.a $(AT2dir)/libptcblas.a \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
# F77-ATLAS
xdslatime_fl_ab : dlatimeF.o dslapack dststlib \
      dsl3lib dsl2lib dsl1lib dsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xdtlatime_fl_ab : dtlatimeF.o dtlapack dttstlib \
      dtl3lib dtl2lib dtl1lib dtfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# System-system; CBLASlib in there since tester can call that iface
xdslatime_sl_sb : dlatimeF.o dslapack dststlib \
      dsl3lib dsl2lib dsl1lib dsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlatimeF.o \
                   $(TESTlib) $(SSLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xdtlatime_sl_sb : dtlatimeF.o dtlapack dttstlib \
      dtl3lib dtl2lib dtl1lib dtfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtlatimeF.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
# F77-system
xdslatime_fl_sb : dlatimeF.o dslapack dststlib \
      dsl3lib dsl2lib dsl1lib dsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xdtlatime_fl_sb : dtlatimeF.o dtlapack dttstlib \
      dtl3lib dtl2lib dtl1lib dtfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)

dlatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeF77  -DDREAL $(mySRCdir)/latime.c
dlatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DDREAL $(mySRCdir)/latime.c
dlatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL $(mySRCdir)/latime.c
dcblas_sb : dcblas
dtlatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DTimeF77 \
                 -DDREAL $(mySRCdir)/latime.c
dtlatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DATL_USEPTHREADS=1 -DDREAL \
                 $(mySRCdir)/latime.c
dtlatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DDREAL \
                 $(mySRCdir)/latime.c
dcblas_sb : dcblas
dcblas_tb : dcblas_pt
dlapack_sl : dlapack
dlapack_al : dlapack_pt
# All ATLAS libs, C interface to lapack
xdormtst_tl_tb : dormtstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdqrtst_tl_tb : dqrtstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdinvtst_tl_tb : dinvtstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdtrtritst_tl_tb : dtrtritstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xduumtst_tl_tb : duumtstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdslvtst_tl_tb : dslvtstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdllttst_tl_tb : dllttstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlutst_tl_tb : dlutstC_pt.o dlapack_tl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdormtst_sl_tb : dormtstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdqrtst_sl_tb : dqrtstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdinvtst_sl_tb : dinvtstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdtrtritst_sl_tb : dtrtritstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xduumtst_sl_tb : duumtstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdslvtst_sl_tb : dslvtstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdllttst_sl_tb : dllttstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlutst_sl_tb : dlutstC.o dlapack_sl dtstlib_tb dl3lib_tb \
                  dl2lib_tb dl1lib_tb dcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdormtst_tl_sb : dormtstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdqrtst_tl_sb : dqrtstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdinvtst_tl_sb : dinvtstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdtrtritst_tl_sb : dtrtritstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xduumtst_tl_sb : duumtstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdslvtst_tl_sb : dslvtstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdllttst_tl_sb : dllttstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlutst_tl_sb : dlutstC_pt.o dlapack_tl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdormtst_sl_sb : dormtstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdqrtst_sl_sb : dqrtstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdinvtst_sl_sb : dinvtstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdtrtritst_sl_sb : dtrtritstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xduumtst_sl_sb : duumtstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdslvtst_sl_sb : dslvtstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdllttst_sl_sb : dllttstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlutst_sl_sb : dlutstC.o dlapack_sl dtstlib_sb dl3lib_sb \
                  dl2lib_sb dl1lib_sb dcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
xdl1blastst_pt : dl1blastst_pt.o dl1lib_pt dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl1blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xdl2blastst_pt : dl2blastst_pt.o dl2lib_pt dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl2blastst_pt.o \
                   $(TESTlib) $(BLASlib)  $(ATLASlib) $(LIBS)
xdl3blastst_pt : $(INCAdir)/atlas_type.h dl3blastst_pt.o \
                   dtstlib_pt dl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
dsanity_test_pt: xdinvtst_pt xdqrtst_pt xdslvtst_pt xdtrtritst_pt xduumtst_pt
	$(ATLRUN) $(BINdir) xdslvtst_pt -n 477 -r 517 -O 2 c r \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xdslvtst_pt -n 477 -r 517 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xdqrtst_pt -n 1 477 -m 1 517 -U 2 u l \
                  -S 2 r l >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xdtrtritst_pt -n 477  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xduumtst_pt -n 477 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdormtst_pt : dormtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdormtstC_pt : dormtstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdormtstF_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdormtst_sb_pt : dormtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdormtstF_sb_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdormtstF_sl_sb_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdormtstF_sl_ab_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdormtstF_fl_ab_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdormtstF_fl_sb_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdormtstF_fl_fb_pt : dormtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdorm_all_pt : xdormtst_pt xdormtstF_pt \
                   xdormtst_sb_pt xdormtstF_sys_pt \
                   xdormtst_sb_pt xdormtstF_sl_sb_pt \
                   xdormtstF_fl_ab_pt xdormtstF_sl_ab_pt \
                   xdormtstF_fl_fb_pt xdormtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdqrtst_pt : dqrtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdqrtstC_pt : dqrtstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdqrtstF_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdqrtst_sb_pt : dqrtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdqrtstF_sb_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdqrtstF_sl_sb_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdqrtstF_sl_ab_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdqrtstF_fl_ab_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdqrtstF_fl_sb_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdqrtstF_fl_fb_pt : dqrtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdqr_all_pt : xdqrtst_pt xdqrtstF_pt \
                   xdqrtst_sb_pt xdqrtstF_sys_pt \
                   xdqrtst_sb_pt xdqrtstF_sl_sb_pt \
                   xdqrtstF_fl_ab_pt xdqrtstF_sl_ab_pt \
                   xdqrtstF_fl_fb_pt xdqrtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdlanbtst_pt : dlanbtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlanbtstC_pt : dlanbtstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdlanbtstF_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdlanbtst_sb_pt : dlanbtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdlanbtstF_sb_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdlanbtstF_sl_sb_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdlanbtstF_sl_ab_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdlanbtstF_fl_ab_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdlanbtstF_fl_sb_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdlanbtstF_fl_fb_pt : dlanbtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdlanb_all_pt : xdlanbtst_pt xdlanbtstF_pt \
                   xdlanbtst_sb_pt xdlanbtstF_sys_pt \
                   xdlanbtst_sb_pt xdlanbtstF_sl_sb_pt \
                   xdlanbtstF_fl_ab_pt xdlanbtstF_sl_ab_pt \
                   xdlanbtstF_fl_fb_pt xdlanbtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdinvtst_pt : dinvtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdinvtstC_pt : dinvtstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdinvtstF_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdinvtst_sb_pt : dinvtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdinvtstF_sb_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdinvtstF_sl_sb_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdinvtstF_sl_ab_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdinvtstF_fl_ab_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdinvtstF_fl_sb_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdinvtstF_fl_fb_pt : dinvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdinv_all_pt : xdinvtst_pt xdinvtstF_pt \
                   xdinvtst_sb_pt xdinvtstF_sys_pt \
                   xdinvtst_sb_pt xdinvtstF_sl_sb_pt \
                   xdinvtstF_fl_ab_pt xdinvtstF_sl_ab_pt \
                   xdinvtstF_fl_fb_pt xdinvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdtrtritst_pt : dtrtritst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdtrtritstC_pt : dtrtritstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdtrtritstF_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdtrtritst_sb_pt : dtrtritst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdtrtritstF_sb_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdtrtritstF_sl_sb_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdtrtritstF_sl_ab_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdtrtritstF_fl_ab_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdtrtritstF_fl_sb_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdtrtritstF_fl_fb_pt : dtrtritstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdtrtri_all_pt : xdtrtritst_pt xdtrtritstF_pt \
                   xdtrtritst_sb_pt xdtrtritstF_sys_pt \
                   xdtrtritst_sb_pt xdtrtritstF_sl_sb_pt \
                   xdtrtritstF_fl_ab_pt xdtrtritstF_sl_ab_pt \
                   xdtrtritstF_fl_fb_pt xdtrtritstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xduumtst_pt : duumtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xduumtstC_pt : duumtstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xduumtstF_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xduumtst_sb_pt : duumtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xduumtstF_sb_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xduumtstF_sl_sb_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xduumtstF_sl_ab_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xduumtstF_fl_ab_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xduumtstF_fl_sb_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xduumtstF_fl_fb_pt : duumtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xduum_all_pt : xduumtst_pt xduumtstF_pt \
                   xduumtst_sb_pt xduumtstF_sys_pt \
                   xduumtst_sb_pt xduumtstF_sl_sb_pt \
                   xduumtstF_fl_ab_pt xduumtstF_sl_ab_pt \
                   xduumtstF_fl_fb_pt xduumtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdslvtst_pt : dslvtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdslvtstC_pt : dslvtstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdslvtstF_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdslvtst_sb_pt : dslvtst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdslvtstF_sb_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdslvtstF_sl_sb_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdslvtstF_sl_ab_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdslvtstF_fl_ab_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdslvtstF_fl_sb_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdslvtstF_fl_fb_pt : dslvtstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdslv_all_pt : xdslvtst_pt xdslvtstF_pt \
                   xdslvtst_sb_pt xdslvtstF_sys_pt \
                   xdslvtst_sb_pt xdslvtstF_sl_sb_pt \
                   xdslvtstF_fl_ab_pt xdslvtstF_sl_ab_pt \
                   xdslvtstF_fl_fb_pt xdslvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdllttst_pt : dllttst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdllttstC_pt : dllttstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdllttstF_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdllttst_sb_pt : dllttst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdllttstF_sb_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdllttstF_sl_sb_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdllttstF_sl_ab_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdllttstF_fl_ab_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdllttstF_fl_sb_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdllttstF_fl_fb_pt : dllttstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdllt_all_pt : xdllttst_pt xdllttstF_pt \
                   xdllttst_sb_pt xdllttstF_sys_pt \
                   xdllttst_sb_pt xdllttstF_sl_sb_pt \
                   xdllttstF_fl_ab_pt xdllttstF_sl_ab_pt \
                   xdllttstF_fl_fb_pt xdllttstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdlutst_pt : dlutst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlutstC_pt : dlutstC_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdlutstF_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdlutst_sb_pt : dlutst_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdlutstF_sb_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdlutstF_sl_sb_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdlutstF_sl_ab_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdlutstF_fl_ab_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdlutstF_fl_sb_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdlutstF_fl_fb_pt : dlutstF_pt.o dlapack_pt dtstlib_pt dl3lib_pt \
                  dl2lib_pt dl1lib_pt dcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xdlu_all_pt : xdlutst_pt xdlutstF_pt \
                   xdlutst_sb_pt xdlutstF_sys_pt \
                   xdlutst_sb_pt xdlutstF_sl_sb_pt \
                   xdlutstF_fl_ab_pt xdlutstF_sl_ab_pt \
                   xdlutstF_fl_fb_pt xdlutstF_fl_sb_pt
xdmmtst_pt : $(INCAdir)/atlas_type.h dgemmtst_pt.o dmmlib_pt dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_pt.o \
              $(TESTlib) $(BLASlib) $(dmmobj) $(ATLASlib) $(LIBS)

xdl1blastst : dl1blastst.o dl1lib dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl1blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
xdl2blastst : dl2blastst.o dl2lib dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl2blastst.o \
                   $(TESTlib) $(SBLASlib)  $(ATLASlib) $(LIBS)
xdl3blastst : $(INCAdir)/atlas_type.h dl3blastst.o \
                   dtstlib dl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
dsanity_test: xdinvtst xdqrtst xdslvtst xdtrtritst xduumtst
	$(ATLRUN) $(BINdir) xdslvtst -n 167 -r 83 -O 2 c r \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xdslvtst -n 167 -r 83 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xdqrtst -n 1 167 -m 1 83 -U 2 u l \
                  -S 2 r l >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xdtrtritst -n 167  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xduumtst -n 167 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdormtst : dormtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdormtstC : dormtstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdormtstF : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdormtst_sb : dormtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdormtstF_sb : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdormtstF_sl_sb : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdormtstF_sl_ab : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdormtstF_fl_ab : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdormtstF_fl_sb : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdormtstF_fl_fb : dormtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdorm_all : xdormtst xdormtstF \
                   xdormtst_sb xdormtstF_sys \
                   xdormtst_sb xdormtstF_sl_sb \
                   xdormtstF_fl_ab xdormtstF_sl_ab \
                   xdormtstF_fl_fb xdormtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdqrtst : dqrtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdqrtstC : dqrtstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdqrtstF : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdqrtst_sb : dqrtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdqrtstF_sb : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdqrtstF_sl_sb : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdqrtstF_sl_ab : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdqrtstF_fl_ab : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdqrtstF_fl_sb : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdqrtstF_fl_fb : dqrtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdqr_all : xdqrtst xdqrtstF \
                   xdqrtst_sb xdqrtstF_sys \
                   xdqrtst_sb xdqrtstF_sl_sb \
                   xdqrtstF_fl_ab xdqrtstF_sl_ab \
                   xdqrtstF_fl_fb xdqrtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdlanbtst : dlanbtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlanbtstC : dlanbtstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdlanbtstF : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdlanbtst_sb : dlanbtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdlanbtstF_sb : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdlanbtstF_sl_sb : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdlanbtstF_sl_ab : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdlanbtstF_fl_ab : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdlanbtstF_fl_sb : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdlanbtstF_fl_fb : dlanbtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdlanb_all : xdlanbtst xdlanbtstF \
                   xdlanbtst_sb xdlanbtstF_sys \
                   xdlanbtst_sb xdlanbtstF_sl_sb \
                   xdlanbtstF_fl_ab xdlanbtstF_sl_ab \
                   xdlanbtstF_fl_fb xdlanbtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdinvtst : dinvtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdinvtstC : dinvtstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdinvtstF : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdinvtst_sb : dinvtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdinvtstF_sb : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdinvtstF_sl_sb : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdinvtstF_sl_ab : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdinvtstF_fl_ab : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdinvtstF_fl_sb : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdinvtstF_fl_fb : dinvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdinv_all : xdinvtst xdinvtstF \
                   xdinvtst_sb xdinvtstF_sys \
                   xdinvtst_sb xdinvtstF_sl_sb \
                   xdinvtstF_fl_ab xdinvtstF_sl_ab \
                   xdinvtstF_fl_fb xdinvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdtrtritst : dtrtritst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdtrtritstC : dtrtritstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdtrtritstF : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdtrtritst_sb : dtrtritst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdtrtritstF_sb : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdtrtritstF_sl_sb : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdtrtritstF_sl_ab : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdtrtritstF_fl_ab : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdtrtritstF_fl_sb : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdtrtritstF_fl_fb : dtrtritstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dtrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdtrtri_all : xdtrtritst xdtrtritstF \
                   xdtrtritst_sb xdtrtritstF_sys \
                   xdtrtritst_sb xdtrtritstF_sl_sb \
                   xdtrtritstF_fl_ab xdtrtritstF_sl_ab \
                   xdtrtritstF_fl_fb xdtrtritstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xduumtst : duumtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xduumtstC : duumtstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xduumtstF : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xduumtst_sb : duumtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xduumtstF_sb : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xduumtstF_sl_sb : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xduumtstF_sl_ab : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xduumtstF_fl_ab : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xduumtstF_fl_sb : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xduumtstF_fl_fb : duumtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ duumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xduum_all : xduumtst xduumtstF \
                   xduumtst_sb xduumtstF_sys \
                   xduumtst_sb xduumtstF_sl_sb \
                   xduumtstF_fl_ab xduumtstF_sl_ab \
                   xduumtstF_fl_fb xduumtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdslvtst : dslvtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdslvtstC : dslvtstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdslvtstF : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdslvtst_sb : dslvtst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdslvtstF_sb : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdslvtstF_sl_sb : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdslvtstF_sl_ab : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdslvtstF_fl_ab : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdslvtstF_fl_sb : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdslvtstF_fl_fb : dslvtstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdslv_all : xdslvtst xdslvtstF \
                   xdslvtst_sb xdslvtstF_sys \
                   xdslvtst_sb xdslvtstF_sl_sb \
                   xdslvtstF_fl_ab xdslvtstF_sl_ab \
                   xdslvtstF_fl_fb xdslvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdllttst : dllttst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdllttstC : dllttstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdllttstF : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdllttst_sb : dllttst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdllttstF_sb : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdllttstF_sl_sb : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdllttstF_sl_ab : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdllttstF_fl_ab : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdllttstF_fl_sb : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdllttstF_fl_fb : dllttstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdllt_all : xdllttst xdllttstF \
                   xdllttst_sb xdllttstF_sys \
                   xdllttst_sb xdllttstF_sl_sb \
                   xdllttstF_fl_ab xdllttstF_sl_ab \
                   xdllttstF_fl_fb xdllttstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xdlutst : dlutst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xdlutstC : dlutstC.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xdlutstF : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xdlutst_sb : dlutst.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xdlutstF_sb : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xdlutstF_sl_sb : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xdlutstF_sl_ab : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xdlutstF_fl_ab : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xdlutstF_fl_sb : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xdlutstF_fl_fb : dlutstF.o dlapack dtstlib dl3lib \
                  dl2lib dl1lib dcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ dlutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xdlu_all : xdlutst xdlutstF \
                   xdlutst_sb xdlutstF_sys \
                   xdlutst_sb xdlutstF_sl_sb \
                   xdlutstF_fl_ab xdlutstF_sl_ab \
                   xdlutstF_fl_fb xdlutstF_fl_sb
xdmmtst : $(INCAdir)/atlas_type.h dgemmtst.o dmmlib dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst.o \
              $(TESTlib) $(SBLASlib) $(dmmobj) $(ATLASlib) $(LIBS)

#
#  These symbols are for timing, and time the same routine twice: atlas/sys
#
xdl3blastst_sys : $(INCAdir)/atlas_type.h dl3blastst_f77.o dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xdl3blastst_sys_pt : $(INCAdir)/atlas_type.h dl3blastst_f77_pt.o \
   dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_f77.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)
xdl3blastst_atl : $(INCAdir)/atlas_type.h dl3blastst_f77.o \
   dtstlib dl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xdl3blastst_atl_pt : $(INCAdir)/atlas_type.h dl3blastst_f77_pt.o \
   dtstlib_pt dl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_f77.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xdl3blastst_at2 : $(INCAdir)/atlas_type.h dl3blastst_f77.o \
   dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xdl3blastst_at2_pt : $(INCAdir)/atlas_type.h dl3blastst_f77_pt.o \
   dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xdmmtst_sys : $(INCAdir)/atlas_type.h dgemmtst_f77.o dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xdmmtst_atl : $(INCAdir)/atlas_type.h dgemmtst_f77.o dmmlib dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xdmmtst_atl_pt : $(INCAdir)/atlas_type.h dgemmtst_f77_pt.o dmmlib_pt dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_f77_pt.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xdmmtst_at2 : dgemmtst_f77_pt.o dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xdmmtst_at2_pt : dgemmtst_f77_pt.o dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xdmmtst_sys_pt : $(INCAdir)/atlas_type.h dgemmtst_f77_pt.o dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_f77_pt.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)

dl3blastst_f77.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DDREAL $(mySRCdir)/l3blastst.c
dl3blastst_f77_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DDREAL $(mySRCdir)/l3blastst.c
dgemmtst_f77.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DDREAL $(mySRCdir)/gemmtst.c
dgemmtst_f77_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DDREAL $(mySRCdir)/gemmtst.c

noaffobj = ATL_goparallel_noaf.o ATL_dyntlaunch_noaf.o \
           ATL_log2tlaunch_noaf.o ATL_thread_start_noaf.o
noaffobjd = $(BLDdir)/tune/threads/ATL_goparallel_noaf.o \
            $(BLDdir)/tune/threads/ATL_dyntlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_log2tlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_thread_start_noaf.o 

xdmmtst_noaff : $(INCAdir)/atlas_type.h dgemmtst_noaff.o \
   dmmlib_pt dtstlib_pt noaff_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_noaff.o $(noaffobjd) \
              $(TESTlib) $(BLASlib) $(dmmobj) $(ATLASlib) $(LIBS)
dgemmtst_noaff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_NOAFFINITY=1 \
               -DDREAL $(mySRCdir)/gemmtst.c

xdmmtst_aff : $(INCAdir)/atlas_type.h dgemmtst_aff.o dmmlib_pt \
              dtstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_aff.o \
                   $(TESTlib) $(BLASlib) $(dmmobj) $(ATLASlib) $(LIBS)
xdmmtst_big : $(INCAdir)/atlas_type.h dgemmtst_big.o dmmlib \
              dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgemmtst_big.o \
                   $(TESTlib) $(BLASlib) $(dmmobj) $(ATLASlib) $(LIBS)
$(MMTdir)/res/dvMMRES :
	cd $(MMTdir) ; make res/dvMMRES
	cd $(MMTdir) ; make vinstall pre=d
xdgpmmtst : $(INCAdir)/atlas_type.h dgpmmtst.o dmmlib dtstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ dgpmmtst.o \
              $(TESTlib) $(BLASlib) $(dmmobj) $(ATLASlib) $(LIBS)
dgpmmtst.o : $(mySRCdir)/gpmmtst.c $(INCSdir)/atlas_misc.h $(INCAdir)/dmm.h \
                  $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_pkblas.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL $(mySRCdir)/gpmmtst.c
	
dtstlib_pt : dtstlib
dtstlib :
	cd $(TSTdir) ; $(MAKE) dlib

dfblas_pt : dfblaslib_pt
dfblaslib_pt :
	cd $(FBLdir) ; $(MAKE) dptlib
dfblas : dfblaslib
dfblaslib :
	cd $(FBLdir) ; $(MAKE) dlib
dcblas_pt : dcblaslib_pt
dcblaslib_pt :
	cd $(CBLdir) ; $(MAKE) dptlib
dcblas : dcblaslib
dcblaslib :
	cd $(CBLdir) ; $(MAKE) dlib

dmmlib_pt : auxlib_pt
	cd $(GMMdir) ; $(MAKE) dlib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) dlib
dmmlib : auxlib
	cd $(GMMdir) ; $(MAKE) dlib
dpklib : auxlib
	cd $(BLDdir)/src/blas/pklevel3 ; $(MAKE) dlib

dl2lib_pt : dl2lib
	cd $(BLDdir)/src/threads/blas/level2 ; $(MAKE) dlib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
dl2lib : dl1lib auxlib
	cd $(GMVdir) ; $(MAKE) dlib
	cd $(GR1dir) ; $(MAKE) dlib
	cd $(L2Bdir) ; $(MAKE) dlib

dl3lib_pt : dl3lib dmmlib_pt
	cd $(BLDdir)/src/threads ; $(MAKE) dlib3
dl3lib : dmmlib
	cd $(L3Bdir) ; $(MAKE) dlib

dl1lib_pt : auxlib_pt dl1lib
dl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) dlvl1

dlapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) dlib
dlapack_pt : dtlapack
dtlapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) dtlib
	cd $(BLDdir)/src/threads/lapack ; $(MAKE) dtlib

xdprintblk : dprintblk.o
	$(CLINKER) -o $@ dprintblk.o $(ATLASlib)
dprintblk.o : $(INCSdir)/atlas_mv.h $(INCSdir)/atlas_r1.h \
                   $(INCAdir)/dmm.h $(mySRCdir)/printblk.c
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL $(mySRCdir)/printblk.c

dl1blastst_mike.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l1blastst.c
dl2blastst_mike.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l2blastst.c

dl3blastst_mike.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/l3blastst.c
dlanbtime_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
dlanbtimeC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtimeF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lanbtst.c
dormtst_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/ormtst.c
dormtstC_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/ormtst.c
dormtstF_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/ormtst.c
dqrtst_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/qrtst.c
dqrtstC_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/qrtst.c
dqrtstF_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/qrtst.c
dlanbtst_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtstC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtstF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lanbtst.c
dinvtst_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/invtst.c
dinvtstC_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/invtst.c
dinvtstF_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/invtst.c
dtrtritst_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/trtritst.c
dtrtritstC_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/trtritst.c
dtrtritstF_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/trtritst.c
dslvtst_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/slvtst.c
dslvtstC_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/slvtst.c
dslvtstF_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/slvtst.c
dlutst_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lutst.c
dlutstC_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lutst.c
dlutstF_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/lutst.c
dllttst_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/llttst.c
dllttstC_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/llttst.c
dllttstF_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/llttst.c
duumtst_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/uumtst.c
duumtstC_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/uumtst.c
duumtstF_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDREAL \
               $(mySRCdir)/uumtst.c

dscaltst_mike.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DDREAL $(mySRCdir)/scaltst.c

dgemmtst_mike.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DDREAL $(mySRCdir)/gemmtst.c

dl1blastst_pt.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DATL_USEPTHREADS $(mySRCdir)/l1blastst.c
dl2blastst_pt.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DATL_USEPTHREADS $(mySRCdir)/l2blastst.c

dl3blastst_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/l3blastst.c
dlanbtime_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
dlanbtimeC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtimeF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lanbtst.c
dormtst_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/ormtst.c
dormtstC_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/ormtst.c
dormtstF_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/ormtst.c
dqrtst_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/qrtst.c
dqrtstC_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/qrtst.c
dqrtstF_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/qrtst.c
dlanbtst_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtstC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtstF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lanbtst.c
dinvtst_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/invtst.c
dinvtstC_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/invtst.c
dinvtstF_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/invtst.c
dtrtritst_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/trtritst.c
dtrtritstC_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/trtritst.c
dtrtritstF_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/trtritst.c
dslvtst_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/slvtst.c
dslvtstC_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/slvtst.c
dslvtstF_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/slvtst.c
dlutst_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lutst.c
dlutstC_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lutst.c
dlutstF_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/lutst.c
dllttst_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/llttst.c
dllttstC_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/llttst.c
dllttstF_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/llttst.c
duumtst_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/uumtst.c
duumtstC_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/uumtst.c
duumtstF_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDREAL \
               $(mySRCdir)/uumtst.c

dscaltst_pt.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/scaltst.c

dgemmtst_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/gemmtst.c

dl1blastst.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL  $(mySRCdir)/l1blastst.c
dl2blastst.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL  $(mySRCdir)/l2blastst.c

dl3blastst.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DDREAL \
               $(mySRCdir)/l3blastst.c
dlanbtime.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
dlanbtimeC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtimeF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/lanbtst.c
dormtst.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/ormtst.c
dormtstC.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/ormtst.c
dormtstF.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/ormtst.c
dqrtst.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/qrtst.c
dqrtstC.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/qrtst.c
dqrtstF.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/qrtst.c
dlanbtst.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtstC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/lanbtst.c
dlanbtstF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/lanbtst.c
dinvtst.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/invtst.c
dinvtstC.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/invtst.c
dinvtstF.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/invtst.c
dtrtritst.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/trtritst.c
dtrtritstC.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/trtritst.c
dtrtritstF.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/trtritst.c
dslvtst.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/slvtst.c
dslvtstC.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/slvtst.c
dslvtstF.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/slvtst.c
dlutst.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/lutst.c
dlutstC.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/lutst.c
dlutstF.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/lutst.c
dllttst.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/llttst.c
dllttstC.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/llttst.c
dllttstF.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/llttst.c
duumtst.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDREAL \
               $(mySRCdir)/uumtst.c
duumtstC.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDREAL \
               $(mySRCdir)/uumtst.c
duumtstF.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDREAL \
               $(mySRCdir)/uumtst.c

dscaltst.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS)  -DDREAL $(mySRCdir)/scaltst.c

dgemmtst.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS)  -DDREAL $(mySRCdir)/gemmtst.c

dgemmtst_aff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DTRUST_BIG -DATL_AFFINITY \
               $(mySRCdir)/gemmtst.c
dgemmtst_rkX.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h dkbmmlib
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DTRUST_BIG -DATL_RKXOVER \
               $(mySRCdir)/gemmtst.c
dgemmtst_rk.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DTRUST_BIG -DTEST_RANKK \
               $(mySRCdir)/gemmtst.c
dgemmtst_big.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DTRUST_BIG   \
               $(mySRCdir)/gemmtst.c
xcgelstest : $(mySRCdir)/cgelstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/cgelstest.f90 \
               $(FLAPACKlib) $(LAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xcscltest : $(mySRCdir)/cscltest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/cscltest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xcger2test : $(mySRCdir)/cger2test.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/cger2test.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xctranstest : $(mySRCdir)/ctranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/ctranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xcqrtranstest : $(mySRCdir)/cqrtranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/cqrtranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xcl1blastst_pt_dyn : cl1blastst_pt.o cl1lib_pt ctstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl1blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcl2blastst_pt_dyn : cl2blastst_pt.o cl2lib_pt ctstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl2blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcl3blastst_pt_dyn : $(INCAdir)/atlas_type.h cl3blastst_pt.o \
                   ctstlib_pt cl3lib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcorm_all_pt_dyn : xcormtst_pt xcormtstF_pt \
                   xcormtst_sys_pt xcormtstF_sys_pt PTshared

xcormtst_pt_dyn : cormtst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f cormtst_pt.o
	$(MAKE) cormtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcormtstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f cormtst_pt.o
	$(MAKE) cormtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcqr_all_pt_dyn : xcqrtst_pt xcqrtstF_pt \
                   xcqrtst_sys_pt xcqrtstF_sys_pt PTshared

xcqrtst_pt_dyn : cqrtst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f cqrtst_pt.o
	$(MAKE) cqrtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcqrtstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f cqrtst_pt.o
	$(MAKE) cqrtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcinv_all_pt_dyn : xcinvtst_pt xcinvtstF_pt \
                   xcinvtst_sys_pt xcinvtstF_sys_pt PTshared

xcinvtst_pt_dyn : cinvtst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f cinvtst_pt.o
	$(MAKE) cinvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcinvtstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f cinvtst_pt.o
	$(MAKE) cinvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xctrtri_all_pt_dyn : xctrtritst_pt xctrtritstF_pt \
                   xctrtritst_sys_pt xctrtritstF_sys_pt PTshared

xctrtritst_pt_dyn : ctrtritst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f ctrtritst_pt.o
	$(MAKE) ctrtritst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xctrtritstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f ctrtritst_pt.o
	$(MAKE) ctrtritst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcuum_all_pt_dyn : xcuumtst_pt xcuumtstF_pt \
                   xcuumtst_sys_pt xcuumtstF_sys_pt PTshared

xcuumtst_pt_dyn : cuumtst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f cuumtst_pt.o
	$(MAKE) cuumtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcuumtstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f cuumtst_pt.o
	$(MAKE) cuumtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcslv_all_pt_dyn : xcslvtst_pt xcslvtstF_pt \
                   xcslvtst_sys_pt xcslvtstF_sys_pt PTshared

xcslvtst_pt_dyn : cslvtst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f cslvtst_pt.o
	$(MAKE) cslvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcslvtstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f cslvtst_pt.o
	$(MAKE) cslvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcllt_all_pt_dyn : xcllttst_pt xcllttstF_pt \
                   xcllttst_sys_pt xcllttstF_sys_pt PTshared

xcllttst_pt_dyn : cllttst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f cllttst_pt.o
	$(MAKE) cllttst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcllttstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f cllttst_pt.o
	$(MAKE) cllttst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xclu_all_pt_dyn : xclutst_pt xclutstF_pt \
                   xclutst_sys_pt xclutstF_sys_pt PTshared

xclutst_pt_dyn : clutst_pt.o clapack ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt PTshared
	rm -f clutst_pt.o
	$(MAKE) clutst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xclutstF_pt_dyn : cPTlapack ctstlib_pt cl3lib_pt cl2lib_pt \
                   cl1lib_pt ccblas_pt cfblas_pt PTshared
	rm -f clutst_pt.o
	$(MAKE) clutst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcmmtst_pt_dyn : $(INCAdir)/atlas_type.h cgemmtst_pt.o cmmlib_pt ctstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_pt.o \
              $(TESTlib) $(cmmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)

xcl1blastst_dyn : cl1blastst.o cl1lib ctstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl1blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcl2blastst_dyn : cl2blastst.o cl2lib ctstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl2blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcl3blastst_dyn : $(INCAdir)/atlas_type.h cl3blastst.o \
                   ctstlib cl3lib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcorm_all_dyn : xcormtst xcormtstF \
                   xcormtst_sys xcormtstF_sys shared

xcormtst_dyn : cormtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f cormtst.o
	$(MAKE) cormtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcormtstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f cormtst.o
	$(MAKE) cormtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcqr_all_dyn : xcqrtst xcqrtstF \
                   xcqrtst_sys xcqrtstF_sys shared

xcqrtst_dyn : cqrtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f cqrtst.o
	$(MAKE) cqrtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcqrtstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f cqrtst.o
	$(MAKE) cqrtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcinv_all_dyn : xcinvtst xcinvtstF \
                   xcinvtst_sys xcinvtstF_sys shared

xcinvtst_dyn : cinvtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f cinvtst.o
	$(MAKE) cinvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcinvtstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f cinvtst.o
	$(MAKE) cinvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xctrtri_all_dyn : xctrtritst xctrtritstF \
                   xctrtritst_sys xctrtritstF_sys shared

xctrtritst_dyn : ctrtritst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f ctrtritst.o
	$(MAKE) ctrtritst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xctrtritstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f ctrtritst.o
	$(MAKE) ctrtritst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcuum_all_dyn : xcuumtst xcuumtstF \
                   xcuumtst_sys xcuumtstF_sys shared

xcuumtst_dyn : cuumtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f cuumtst.o
	$(MAKE) cuumtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcuumtstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f cuumtst.o
	$(MAKE) cuumtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcslv_all_dyn : xcslvtst xcslvtstF \
                   xcslvtst_sys xcslvtstF_sys shared

xcslvtst_dyn : cslvtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f cslvtst.o
	$(MAKE) cslvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcslvtstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f cslvtst.o
	$(MAKE) cslvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcllt_all_dyn : xcllttst xcllttstF \
                   xcllttst_sys xcllttstF_sys shared

xcllttst_dyn : cllttst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f cllttst.o
	$(MAKE) cllttst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcllttstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f cllttst.o
	$(MAKE) cllttst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xclu_all_dyn : xclutst xclutstF \
                   xclutst_sys xclutstF_sys shared

xclutst_dyn : clutst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas shared
	rm -f clutst.o
	$(MAKE) clutst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xclutstF_dyn : clapack ctstlib cl3lib cl2lib \
                   cl1lib ccblas cfblas shared
	rm -f clutst.o
	$(MAKE) clutst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xcmmtst_dyn : $(INCAdir)/atlas_type.h cgemmtst.o cmmlib ctstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst.o \
              $(TESTlib) $(cmmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)


xcl1blastst_mike : cl1blastst_mike.o cl1lib_pt ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl1blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xcl2blastst_mike : cl2blastst_pt.o cl2lib_pt ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl2blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xcl3blastst_mike : $(INCAdir)/atlas_type.h cl3blastst_pt.o \
                   ctstlib_pt cl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)

ctstlib_tb : ctstlib_pt
ctstlib_sb : ctstlib
cl1lib_sb : cl1lib
cl1lib_pb : cl1lib_pt
cl2lib_sb : cl2lib
cl2lib_pb : cl2lib_pt
cl3lib_sb : cl3lib
cl3lib_pb : cl3lib_pt
#
# Ugly renaming of old system to new; new is <pre>[t,s]name, where
# 't' is threaded and 's' is serial
#
cslapack : clapack
ctlapack : clapack_pt
csl3lib : cl3lib
ctl3lib : cl3lib_pt
csl2lib : cl2lib
ctl2lib : cl2lib_pt
csl1lib : cl1lib
ctl1lib : cl1lib_pt
cststlib : ctstlib
cttstlib : ctstlib_pt
csfblas : cfblas
ctfblas : cfblas_pt
cscblas : ccblas
ctcblas : ccblas_pt
#
# Default lanbtime, calls ATLAS's internal iface
#
xcslatime : clatime.o cslapack cststlib \
      csl3lib csl2lib csl1lib csfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clatime.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xctlatime : ctlatime.o ctlapack cttstlib \
      ctl3lib ctl2lib ctl1lib ctfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctlatime.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
#
# Targets for building latime with the F77 iface, using ATLAS, system,
# or F77 BLAS and LAPACK in serial and parallel
#
# ATLAS-ATLAS
xcslatime_al_ab : clatimeF.o cslapack cststlib \
      csl3lib csl2lib csl1lib csfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clatimeF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xctlatime_al_ab : ctlatimeF.o ctlapack cttstlib \
      ctl3lib ctl2lib ctl1lib ctfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctlatimeF.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# ATLAS2-ATLAS2 (to compare against competing ATLAS install)
xcslatime_2l_2b : clatimeF.o cststlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ clatimeF.o \
                   $(TESTlib) $(AT2dir)/liblapack.a $(AT2dir)/libcblas.a \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xctlatime_2l_2b : ctlatimeF.o cttstlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctlatimeF.o \
                   $(TESTlib) $(AT2dir)/libptlapack.a $(AT2dir)/libptcblas.a \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
# F77-ATLAS
xcslatime_fl_ab : clatimeF.o cslapack cststlib \
      csl3lib csl2lib csl1lib csfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xctlatime_fl_ab : ctlatimeF.o ctlapack cttstlib \
      ctl3lib ctl2lib ctl1lib ctfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# System-system; CBLASlib in there since tester can call that iface
xcslatime_sl_sb : clatimeF.o cslapack cststlib \
      csl3lib csl2lib csl1lib csfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clatimeF.o \
                   $(TESTlib) $(SSLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xctlatime_sl_sb : ctlatimeF.o ctlapack cttstlib \
      ctl3lib ctl2lib ctl1lib ctfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctlatimeF.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
# F77-system
xcslatime_fl_sb : clatimeF.o cslapack cststlib \
      csl3lib csl2lib csl1lib csfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xctlatime_fl_sb : ctlatimeF.o ctlapack cttstlib \
      ctl3lib ctl2lib ctl1lib ctfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)

clatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeF77  -DSCPLX $(mySRCdir)/latime.c
clatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DSCPLX $(mySRCdir)/latime.c
clatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX $(mySRCdir)/latime.c
ccblas_sb : ccblas
ctlatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DTimeF77 \
                 -DSCPLX $(mySRCdir)/latime.c
ctlatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DATL_USEPTHREADS=1 -DSCPLX \
                 $(mySRCdir)/latime.c
ctlatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DSCPLX \
                 $(mySRCdir)/latime.c
ccblas_sb : ccblas
ccblas_tb : ccblas_pt
clapack_sl : clapack
clapack_al : clapack_pt
# All ATLAS libs, C interface to lapack
xcormtst_tl_tb : cormtstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcqrtst_tl_tb : cqrtstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcinvtst_tl_tb : cinvtstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xctrtritst_tl_tb : ctrtritstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcuumtst_tl_tb : cuumtstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcslvtst_tl_tb : cslvtstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcllttst_tl_tb : cllttstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclutst_tl_tb : clutstC_pt.o clapack_tl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcormtst_sl_tb : cormtstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcqrtst_sl_tb : cqrtstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcinvtst_sl_tb : cinvtstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xctrtritst_sl_tb : ctrtritstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcuumtst_sl_tb : cuumtstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcslvtst_sl_tb : cslvtstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcllttst_sl_tb : cllttstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclutst_sl_tb : clutstC.o clapack_sl ctstlib_tb cl3lib_tb \
                  cl2lib_tb cl1lib_tb ccblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcormtst_tl_sb : cormtstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcqrtst_tl_sb : cqrtstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcinvtst_tl_sb : cinvtstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xctrtritst_tl_sb : ctrtritstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcuumtst_tl_sb : cuumtstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcslvtst_tl_sb : cslvtstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcllttst_tl_sb : cllttstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclutst_tl_sb : clutstC_pt.o clapack_tl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcormtst_sl_sb : cormtstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcqrtst_sl_sb : cqrtstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcinvtst_sl_sb : cinvtstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xctrtritst_sl_sb : ctrtritstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcuumtst_sl_sb : cuumtstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcslvtst_sl_sb : cslvtstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcllttst_sl_sb : cllttstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclutst_sl_sb : clutstC.o clapack_sl ctstlib_sb cl3lib_sb \
                  cl2lib_sb cl1lib_sb ccblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
xcl1blastst_pt : cl1blastst_pt.o cl1lib_pt ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl1blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xcl2blastst_pt : cl2blastst_pt.o cl2lib_pt ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl2blastst_pt.o \
                   $(TESTlib) $(BLASlib)  $(ATLASlib) $(LIBS)
xcl3blastst_pt : $(INCAdir)/atlas_type.h cl3blastst_pt.o \
                   ctstlib_pt cl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
csanity_test_pt: xcinvtst_pt xcqrtst_pt xcslvtst_pt xctrtritst_pt xcuumtst_pt
	$(ATLRUN) $(BINdir) xcslvtst_pt -n 477 -r 517 -O 2 c r \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xcslvtst_pt -n 477 -r 517 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xcqrtst_pt -n 1 477 -m 1 517 -U 2 u l \
                  -S 2 r l >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xctrtritst_pt -n 477  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xcuumtst_pt -n 477 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcormtst_pt : cormtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcormtstC_pt : cormtstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcormtstF_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcormtst_sb_pt : cormtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcormtstF_sb_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcormtstF_sl_sb_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcormtstF_sl_ab_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcormtstF_fl_ab_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcormtstF_fl_sb_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcormtstF_fl_fb_pt : cormtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcorm_all_pt : xcormtst_pt xcormtstF_pt \
                   xcormtst_sb_pt xcormtstF_sys_pt \
                   xcormtst_sb_pt xcormtstF_sl_sb_pt \
                   xcormtstF_fl_ab_pt xcormtstF_sl_ab_pt \
                   xcormtstF_fl_fb_pt xcormtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcqrtst_pt : cqrtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcqrtstC_pt : cqrtstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcqrtstF_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcqrtst_sb_pt : cqrtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcqrtstF_sb_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcqrtstF_sl_sb_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcqrtstF_sl_ab_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcqrtstF_fl_ab_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcqrtstF_fl_sb_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcqrtstF_fl_fb_pt : cqrtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcqr_all_pt : xcqrtst_pt xcqrtstF_pt \
                   xcqrtst_sb_pt xcqrtstF_sys_pt \
                   xcqrtst_sb_pt xcqrtstF_sl_sb_pt \
                   xcqrtstF_fl_ab_pt xcqrtstF_sl_ab_pt \
                   xcqrtstF_fl_fb_pt xcqrtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xclanbtst_pt : clanbtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclanbtstC_pt : clanbtstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xclanbtstF_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xclanbtst_sb_pt : clanbtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xclanbtstF_sb_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xclanbtstF_sl_sb_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xclanbtstF_sl_ab_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xclanbtstF_fl_ab_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xclanbtstF_fl_sb_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xclanbtstF_fl_fb_pt : clanbtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xclanb_all_pt : xclanbtst_pt xclanbtstF_pt \
                   xclanbtst_sb_pt xclanbtstF_sys_pt \
                   xclanbtst_sb_pt xclanbtstF_sl_sb_pt \
                   xclanbtstF_fl_ab_pt xclanbtstF_sl_ab_pt \
                   xclanbtstF_fl_fb_pt xclanbtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcinvtst_pt : cinvtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcinvtstC_pt : cinvtstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcinvtstF_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcinvtst_sb_pt : cinvtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcinvtstF_sb_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcinvtstF_sl_sb_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcinvtstF_sl_ab_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcinvtstF_fl_ab_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcinvtstF_fl_sb_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcinvtstF_fl_fb_pt : cinvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcinv_all_pt : xcinvtst_pt xcinvtstF_pt \
                   xcinvtst_sb_pt xcinvtstF_sys_pt \
                   xcinvtst_sb_pt xcinvtstF_sl_sb_pt \
                   xcinvtstF_fl_ab_pt xcinvtstF_sl_ab_pt \
                   xcinvtstF_fl_fb_pt xcinvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xctrtritst_pt : ctrtritst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xctrtritstC_pt : ctrtritstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xctrtritstF_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xctrtritst_sb_pt : ctrtritst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xctrtritstF_sb_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xctrtritstF_sl_sb_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xctrtritstF_sl_ab_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xctrtritstF_fl_ab_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xctrtritstF_fl_sb_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xctrtritstF_fl_fb_pt : ctrtritstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xctrtri_all_pt : xctrtritst_pt xctrtritstF_pt \
                   xctrtritst_sb_pt xctrtritstF_sys_pt \
                   xctrtritst_sb_pt xctrtritstF_sl_sb_pt \
                   xctrtritstF_fl_ab_pt xctrtritstF_sl_ab_pt \
                   xctrtritstF_fl_fb_pt xctrtritstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcuumtst_pt : cuumtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcuumtstC_pt : cuumtstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcuumtstF_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcuumtst_sb_pt : cuumtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcuumtstF_sb_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcuumtstF_sl_sb_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcuumtstF_sl_ab_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcuumtstF_fl_ab_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcuumtstF_fl_sb_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcuumtstF_fl_fb_pt : cuumtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcuum_all_pt : xcuumtst_pt xcuumtstF_pt \
                   xcuumtst_sb_pt xcuumtstF_sys_pt \
                   xcuumtst_sb_pt xcuumtstF_sl_sb_pt \
                   xcuumtstF_fl_ab_pt xcuumtstF_sl_ab_pt \
                   xcuumtstF_fl_fb_pt xcuumtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcslvtst_pt : cslvtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcslvtstC_pt : cslvtstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcslvtstF_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcslvtst_sb_pt : cslvtst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcslvtstF_sb_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcslvtstF_sl_sb_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcslvtstF_sl_ab_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcslvtstF_fl_ab_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcslvtstF_fl_sb_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcslvtstF_fl_fb_pt : cslvtstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcslv_all_pt : xcslvtst_pt xcslvtstF_pt \
                   xcslvtst_sb_pt xcslvtstF_sys_pt \
                   xcslvtst_sb_pt xcslvtstF_sl_sb_pt \
                   xcslvtstF_fl_ab_pt xcslvtstF_sl_ab_pt \
                   xcslvtstF_fl_fb_pt xcslvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcllttst_pt : cllttst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcllttstC_pt : cllttstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcllttstF_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcllttst_sb_pt : cllttst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcllttstF_sb_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcllttstF_sl_sb_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcllttstF_sl_ab_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcllttstF_fl_ab_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcllttstF_fl_sb_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcllttstF_fl_fb_pt : cllttstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xcllt_all_pt : xcllttst_pt xcllttstF_pt \
                   xcllttst_sb_pt xcllttstF_sys_pt \
                   xcllttst_sb_pt xcllttstF_sl_sb_pt \
                   xcllttstF_fl_ab_pt xcllttstF_sl_ab_pt \
                   xcllttstF_fl_fb_pt xcllttstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xclutst_pt : clutst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclutstC_pt : clutstC_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xclutstF_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xclutst_sb_pt : clutst_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xclutstF_sb_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xclutstF_sl_sb_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xclutstF_sl_ab_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xclutstF_fl_ab_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xclutstF_fl_sb_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xclutstF_fl_fb_pt : clutstF_pt.o clapack_pt ctstlib_pt cl3lib_pt \
                  cl2lib_pt cl1lib_pt ccblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xclu_all_pt : xclutst_pt xclutstF_pt \
                   xclutst_sb_pt xclutstF_sys_pt \
                   xclutst_sb_pt xclutstF_sl_sb_pt \
                   xclutstF_fl_ab_pt xclutstF_sl_ab_pt \
                   xclutstF_fl_fb_pt xclutstF_fl_sb_pt
xcmmtst_pt : $(INCAdir)/atlas_type.h cgemmtst_pt.o cmmlib_pt ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_pt.o \
              $(TESTlib) $(BLASlib) $(cmmobj) $(ATLASlib) $(LIBS)

xcl1blastst : cl1blastst.o cl1lib ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl1blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
xcl2blastst : cl2blastst.o cl2lib ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl2blastst.o \
                   $(TESTlib) $(SBLASlib)  $(ATLASlib) $(LIBS)
xcl3blastst : $(INCAdir)/atlas_type.h cl3blastst.o \
                   ctstlib cl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
csanity_test: xcinvtst xcqrtst xcslvtst xctrtritst xcuumtst
	$(ATLRUN) $(BINdir) xcslvtst -n 167 -r 83 -O 2 c r \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xcslvtst -n 167 -r 83 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xcqrtst -n 1 167 -m 1 83 -U 2 u l \
                  -S 2 r l >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xctrtritst -n 167  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xcuumtst -n 167 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcormtst : cormtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcormtstC : cormtstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcormtstF : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcormtst_sb : cormtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcormtstF_sb : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcormtstF_sl_sb : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcormtstF_sl_ab : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcormtstF_fl_ab : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcormtstF_fl_sb : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcormtstF_fl_fb : cormtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcorm_all : xcormtst xcormtstF \
                   xcormtst_sb xcormtstF_sys \
                   xcormtst_sb xcormtstF_sl_sb \
                   xcormtstF_fl_ab xcormtstF_sl_ab \
                   xcormtstF_fl_fb xcormtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcqrtst : cqrtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcqrtstC : cqrtstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcqrtstF : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcqrtst_sb : cqrtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcqrtstF_sb : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcqrtstF_sl_sb : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcqrtstF_sl_ab : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcqrtstF_fl_ab : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcqrtstF_fl_sb : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcqrtstF_fl_fb : cqrtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcqr_all : xcqrtst xcqrtstF \
                   xcqrtst_sb xcqrtstF_sys \
                   xcqrtst_sb xcqrtstF_sl_sb \
                   xcqrtstF_fl_ab xcqrtstF_sl_ab \
                   xcqrtstF_fl_fb xcqrtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xclanbtst : clanbtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclanbtstC : clanbtstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xclanbtstF : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xclanbtst_sb : clanbtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xclanbtstF_sb : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xclanbtstF_sl_sb : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xclanbtstF_sl_ab : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xclanbtstF_fl_ab : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xclanbtstF_fl_sb : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xclanbtstF_fl_fb : clanbtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xclanb_all : xclanbtst xclanbtstF \
                   xclanbtst_sb xclanbtstF_sys \
                   xclanbtst_sb xclanbtstF_sl_sb \
                   xclanbtstF_fl_ab xclanbtstF_sl_ab \
                   xclanbtstF_fl_fb xclanbtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcinvtst : cinvtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcinvtstC : cinvtstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcinvtstF : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcinvtst_sb : cinvtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcinvtstF_sb : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcinvtstF_sl_sb : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcinvtstF_sl_ab : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcinvtstF_fl_ab : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcinvtstF_fl_sb : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcinvtstF_fl_fb : cinvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcinv_all : xcinvtst xcinvtstF \
                   xcinvtst_sb xcinvtstF_sys \
                   xcinvtst_sb xcinvtstF_sl_sb \
                   xcinvtstF_fl_ab xcinvtstF_sl_ab \
                   xcinvtstF_fl_fb xcinvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xctrtritst : ctrtritst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xctrtritstC : ctrtritstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xctrtritstF : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xctrtritst_sb : ctrtritst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xctrtritstF_sb : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xctrtritstF_sl_sb : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xctrtritstF_sl_ab : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xctrtritstF_fl_ab : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xctrtritstF_fl_sb : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xctrtritstF_fl_fb : ctrtritstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ctrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xctrtri_all : xctrtritst xctrtritstF \
                   xctrtritst_sb xctrtritstF_sys \
                   xctrtritst_sb xctrtritstF_sl_sb \
                   xctrtritstF_fl_ab xctrtritstF_sl_ab \
                   xctrtritstF_fl_fb xctrtritstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcuumtst : cuumtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcuumtstC : cuumtstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcuumtstF : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcuumtst_sb : cuumtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcuumtstF_sb : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcuumtstF_sl_sb : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcuumtstF_sl_ab : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcuumtstF_fl_ab : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcuumtstF_fl_sb : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcuumtstF_fl_fb : cuumtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cuumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcuum_all : xcuumtst xcuumtstF \
                   xcuumtst_sb xcuumtstF_sys \
                   xcuumtst_sb xcuumtstF_sl_sb \
                   xcuumtstF_fl_ab xcuumtstF_sl_ab \
                   xcuumtstF_fl_fb xcuumtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcslvtst : cslvtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcslvtstC : cslvtstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcslvtstF : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcslvtst_sb : cslvtst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcslvtstF_sb : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcslvtstF_sl_sb : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcslvtstF_sl_ab : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcslvtstF_fl_ab : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcslvtstF_fl_sb : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcslvtstF_fl_fb : cslvtstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcslv_all : xcslvtst xcslvtstF \
                   xcslvtst_sb xcslvtstF_sys \
                   xcslvtst_sb xcslvtstF_sl_sb \
                   xcslvtstF_fl_ab xcslvtstF_sl_ab \
                   xcslvtstF_fl_fb xcslvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xcllttst : cllttst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xcllttstC : cllttstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xcllttstF : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xcllttst_sb : cllttst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xcllttstF_sb : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xcllttstF_sl_sb : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xcllttstF_sl_ab : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xcllttstF_fl_ab : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xcllttstF_fl_sb : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xcllttstF_fl_fb : cllttstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ cllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xcllt_all : xcllttst xcllttstF \
                   xcllttst_sb xcllttstF_sys \
                   xcllttst_sb xcllttstF_sl_sb \
                   xcllttstF_fl_ab xcllttstF_sl_ab \
                   xcllttstF_fl_fb xcllttstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xclutst : clutst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xclutstC : clutstC.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xclutstF : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xclutst_sb : clutst.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xclutstF_sb : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xclutstF_sl_sb : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xclutstF_sl_ab : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xclutstF_fl_ab : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xclutstF_fl_sb : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xclutstF_fl_fb : clutstF.o clapack ctstlib cl3lib \
                  cl2lib cl1lib ccblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ clutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xclu_all : xclutst xclutstF \
                   xclutst_sb xclutstF_sys \
                   xclutst_sb xclutstF_sl_sb \
                   xclutstF_fl_ab xclutstF_sl_ab \
                   xclutstF_fl_fb xclutstF_fl_sb
xcmmtst : $(INCAdir)/atlas_type.h cgemmtst.o cmmlib ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst.o \
              $(TESTlib) $(SBLASlib) $(cmmobj) $(ATLASlib) $(LIBS)

#
#  These symbols are for timing, and time the same routine twice: atlas/sys
#
xcl3blastst_sys : $(INCAdir)/atlas_type.h cl3blastst_f77.o ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xcl3blastst_sys_pt : $(INCAdir)/atlas_type.h cl3blastst_f77_pt.o \
   ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_f77.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)
xcl3blastst_atl : $(INCAdir)/atlas_type.h cl3blastst_f77.o \
   ctstlib cl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xcl3blastst_atl_pt : $(INCAdir)/atlas_type.h cl3blastst_f77_pt.o \
   ctstlib_pt cl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_f77.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xcl3blastst_at2 : $(INCAdir)/atlas_type.h cl3blastst_f77.o \
   ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xcl3blastst_at2_pt : $(INCAdir)/atlas_type.h cl3blastst_f77_pt.o \
   ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xcmmtst_sys : $(INCAdir)/atlas_type.h cgemmtst_f77.o ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xcmmtst_atl : $(INCAdir)/atlas_type.h cgemmtst_f77.o cmmlib ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xcmmtst_atl_pt : $(INCAdir)/atlas_type.h cgemmtst_f77_pt.o cmmlib_pt ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_f77_pt.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xcmmtst_at2 : cgemmtst_f77_pt.o ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xcmmtst_at2_pt : cgemmtst_f77_pt.o ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xcmmtst_sys_pt : $(INCAdir)/atlas_type.h cgemmtst_f77_pt.o ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_f77_pt.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)

cl3blastst_f77.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DSCPLX $(mySRCdir)/l3blastst.c
cl3blastst_f77_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DSCPLX $(mySRCdir)/l3blastst.c
cgemmtst_f77.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DSCPLX $(mySRCdir)/gemmtst.c
cgemmtst_f77_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DSCPLX $(mySRCdir)/gemmtst.c

noaffobj = ATL_goparallel_noaf.o ATL_dyntlaunch_noaf.o \
           ATL_log2tlaunch_noaf.o ATL_thread_start_noaf.o
noaffobjd = $(BLDdir)/tune/threads/ATL_goparallel_noaf.o \
            $(BLDdir)/tune/threads/ATL_dyntlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_log2tlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_thread_start_noaf.o 

xcmmtst_noaff : $(INCAdir)/atlas_type.h cgemmtst_noaff.o \
   cmmlib_pt ctstlib_pt noaff_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_noaff.o $(noaffobjd) \
              $(TESTlib) $(BLASlib) $(cmmobj) $(ATLASlib) $(LIBS)
cgemmtst_noaff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_NOAFFINITY=1 \
               -DSCPLX $(mySRCdir)/gemmtst.c

xcmmtst_aff : $(INCAdir)/atlas_type.h cgemmtst_aff.o cmmlib_pt \
              ctstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_aff.o \
                   $(TESTlib) $(BLASlib) $(cmmobj) $(ATLASlib) $(LIBS)
xcmmtst_big : $(INCAdir)/atlas_type.h cgemmtst_big.o cmmlib \
              ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgemmtst_big.o \
                   $(TESTlib) $(BLASlib) $(cmmobj) $(ATLASlib) $(LIBS)
$(MMTdir)/res/cvMMRES :
	cd $(MMTdir) ; make res/cvMMRES
	cd $(MMTdir) ; make vinstall pre=c
xcgpmmtst : $(INCAdir)/atlas_type.h cgpmmtst.o cmmlib ctstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ cgpmmtst.o \
              $(TESTlib) $(BLASlib) $(cmmobj) $(ATLASlib) $(LIBS)
cgpmmtst.o : $(mySRCdir)/gpmmtst.c $(INCSdir)/atlas_misc.h $(INCAdir)/cmm.h \
                  $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_pkblas.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX $(mySRCdir)/gpmmtst.c
	
ctstlib_pt : ctstlib
ctstlib :
	cd $(TSTdir) ; $(MAKE) clib

cfblas_pt : cfblaslib_pt
cfblaslib_pt :
	cd $(FBLdir) ; $(MAKE) cptlib
cfblas : cfblaslib
cfblaslib :
	cd $(FBLdir) ; $(MAKE) clib
ccblas_pt : ccblaslib_pt
ccblaslib_pt :
	cd $(CBLdir) ; $(MAKE) cptlib
ccblas : ccblaslib
ccblaslib :
	cd $(CBLdir) ; $(MAKE) clib

cmmlib_pt : auxlib_pt
	cd $(GMMdir) ; $(MAKE) clib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) clib
cmmlib : auxlib
	cd $(GMMdir) ; $(MAKE) clib
cpklib : auxlib
	cd $(BLDdir)/src/blas/pklevel3 ; $(MAKE) clib

cl2lib_pt : cl2lib
	cd $(BLDdir)/src/threads/blas/level2 ; $(MAKE) clib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
cl2lib : cl1lib auxlib
	cd $(GMVdir) ; $(MAKE) clib
	cd $(GR1dir) ; $(MAKE) clib
	cd $(L2Bdir) ; $(MAKE) clib

cl3lib_pt : cl3lib cmmlib_pt
	cd $(BLDdir)/src/threads ; $(MAKE) clib3
cl3lib : cmmlib
	cd $(L3Bdir) ; $(MAKE) clib

cl1lib_pt : auxlib_pt cl1lib
cl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) clvl1

clapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) clib
clapack_pt : ctlapack
ctlapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) ctlib
	cd $(BLDdir)/src/threads/lapack ; $(MAKE) ctlib

xcprintblk : cprintblk.o
	$(CLINKER) -o $@ cprintblk.o $(ATLASlib)
cprintblk.o : $(INCSdir)/atlas_mv.h $(INCSdir)/atlas_r1.h \
                   $(INCAdir)/cmm.h $(mySRCdir)/printblk.c
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX $(mySRCdir)/printblk.c

cl1blastst_mike.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l1blastst.c
cl2blastst_mike.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l2blastst.c

cl3blastst_mike.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/l3blastst.c
clanbtime_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
clanbtimeC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtimeF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lanbtst.c
cormtst_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/ormtst.c
cormtstC_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/ormtst.c
cormtstF_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/ormtst.c
cqrtst_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/qrtst.c
cqrtstC_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/qrtst.c
cqrtstF_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/qrtst.c
clanbtst_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtstC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtstF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lanbtst.c
cinvtst_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/invtst.c
cinvtstC_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/invtst.c
cinvtstF_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/invtst.c
ctrtritst_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/trtritst.c
ctrtritstC_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/trtritst.c
ctrtritstF_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/trtritst.c
cslvtst_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/slvtst.c
cslvtstC_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/slvtst.c
cslvtstF_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/slvtst.c
clutst_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lutst.c
clutstC_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lutst.c
clutstF_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/lutst.c
cllttst_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/llttst.c
cllttstC_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/llttst.c
cllttstF_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/llttst.c
cuumtst_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/uumtst.c
cuumtstC_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/uumtst.c
cuumtstF_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DSCPLX \
               $(mySRCdir)/uumtst.c

cscaltst_mike.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX $(mySRCdir)/scaltst.c

cgemmtst_mike.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DSCPLX $(mySRCdir)/gemmtst.c

cl1blastst_pt.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DATL_USEPTHREADS $(mySRCdir)/l1blastst.c
cl2blastst_pt.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DATL_USEPTHREADS $(mySRCdir)/l2blastst.c

cl3blastst_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/l3blastst.c
clanbtime_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
clanbtimeC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtimeF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lanbtst.c
cormtst_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/ormtst.c
cormtstC_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/ormtst.c
cormtstF_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/ormtst.c
cqrtst_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/qrtst.c
cqrtstC_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/qrtst.c
cqrtstF_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/qrtst.c
clanbtst_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtstC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtstF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lanbtst.c
cinvtst_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/invtst.c
cinvtstC_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/invtst.c
cinvtstF_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/invtst.c
ctrtritst_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/trtritst.c
ctrtritstC_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/trtritst.c
ctrtritstF_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/trtritst.c
cslvtst_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/slvtst.c
cslvtstC_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/slvtst.c
cslvtstF_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/slvtst.c
clutst_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lutst.c
clutstC_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lutst.c
clutstF_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/lutst.c
cllttst_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/llttst.c
cllttstC_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/llttst.c
cllttstF_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/llttst.c
cuumtst_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/uumtst.c
cuumtstC_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/uumtst.c
cuumtstF_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DSCPLX \
               $(mySRCdir)/uumtst.c

cscaltst_pt.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/scaltst.c

cgemmtst_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/gemmtst.c

cl1blastst.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX  $(mySRCdir)/l1blastst.c
cl2blastst.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX  $(mySRCdir)/l2blastst.c

cl3blastst.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DSCPLX \
               $(mySRCdir)/l3blastst.c
clanbtime.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
clanbtimeC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtimeF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/lanbtst.c
cormtst.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/ormtst.c
cormtstC.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/ormtst.c
cormtstF.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/ormtst.c
cqrtst.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/qrtst.c
cqrtstC.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/qrtst.c
cqrtstF.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/qrtst.c
clanbtst.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtstC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/lanbtst.c
clanbtstF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/lanbtst.c
cinvtst.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/invtst.c
cinvtstC.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/invtst.c
cinvtstF.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/invtst.c
ctrtritst.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/trtritst.c
ctrtritstC.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/trtritst.c
ctrtritstF.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/trtritst.c
cslvtst.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/slvtst.c
cslvtstC.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/slvtst.c
cslvtstF.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/slvtst.c
clutst.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/lutst.c
clutstC.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/lutst.c
clutstF.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/lutst.c
cllttst.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/llttst.c
cllttstC.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/llttst.c
cllttstF.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/llttst.c
cuumtst.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DSCPLX \
               $(mySRCdir)/uumtst.c
cuumtstC.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DSCPLX \
               $(mySRCdir)/uumtst.c
cuumtstF.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DSCPLX \
               $(mySRCdir)/uumtst.c

cscaltst.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS)  -DSCPLX $(mySRCdir)/scaltst.c

cgemmtst.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS)  -DSCPLX $(mySRCdir)/gemmtst.c

cgemmtst_aff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DTRUST_BIG -DATL_AFFINITY \
               $(mySRCdir)/gemmtst.c
cgemmtst_rkX.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h ckbmmlib
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DTRUST_BIG -DATL_RKXOVER \
               $(mySRCdir)/gemmtst.c
cgemmtst_rk.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DTRUST_BIG -DTEST_RANKK \
               $(mySRCdir)/gemmtst.c
cgemmtst_big.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DSCPLX -DTRUST_BIG   \
               $(mySRCdir)/gemmtst.c
xzgelstest : $(mySRCdir)/zgelstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/zgelstest.f90 \
               $(FLAPACKlib) $(LAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xzscltest : $(mySRCdir)/zscltest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/zscltest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xzger2test : $(mySRCdir)/zger2test.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/zger2test.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xztranstest : $(mySRCdir)/ztranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/ztranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xzqrtranstest : $(mySRCdir)/zqrtranstest.f90 $(FLAPACKlib) $(ATLASlib)
	$(F77) $(F77FLAGS) -o $@ $(mySRCdir)/zqrtranstest.f90 \
               $(FLAPACKlib) $(F77BLASlib) $(CBLASlib) $(ATLASlib)
xzl1blastst_pt_dyn : zl1blastst_pt.o zl1lib_pt ztstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl1blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzl2blastst_pt_dyn : zl2blastst_pt.o zl2lib_pt ztstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl2blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzl3blastst_pt_dyn : $(INCAdir)/atlas_type.h zl3blastst_pt.o \
                   ztstlib_pt zl3lib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_pt.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzorm_all_pt_dyn : xzormtst_pt xzormtstF_pt \
                   xzormtst_sys_pt xzormtstF_sys_pt PTshared

xzormtst_pt_dyn : zormtst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zormtst_pt.o
	$(MAKE) zormtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzormtstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zormtst_pt.o
	$(MAKE) zormtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzqr_all_pt_dyn : xzqrtst_pt xzqrtstF_pt \
                   xzqrtst_sys_pt xzqrtstF_sys_pt PTshared

xzqrtst_pt_dyn : zqrtst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zqrtst_pt.o
	$(MAKE) zqrtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzqrtstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zqrtst_pt.o
	$(MAKE) zqrtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzinv_all_pt_dyn : xzinvtst_pt xzinvtstF_pt \
                   xzinvtst_sys_pt xzinvtstF_sys_pt PTshared

xzinvtst_pt_dyn : zinvtst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zinvtst_pt.o
	$(MAKE) zinvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzinvtstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zinvtst_pt.o
	$(MAKE) zinvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xztrtri_all_pt_dyn : xztrtritst_pt xztrtritstF_pt \
                   xztrtritst_sys_pt xztrtritstF_sys_pt PTshared

xztrtritst_pt_dyn : ztrtritst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f ztrtritst_pt.o
	$(MAKE) ztrtritst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xztrtritstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f ztrtritst_pt.o
	$(MAKE) ztrtritst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzuum_all_pt_dyn : xzuumtst_pt xzuumtstF_pt \
                   xzuumtst_sys_pt xzuumtstF_sys_pt PTshared

xzuumtst_pt_dyn : zuumtst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zuumtst_pt.o
	$(MAKE) zuumtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzuumtstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zuumtst_pt.o
	$(MAKE) zuumtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzslv_all_pt_dyn : xzslvtst_pt xzslvtstF_pt \
                   xzslvtst_sys_pt xzslvtstF_sys_pt PTshared

xzslvtst_pt_dyn : zslvtst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zslvtst_pt.o
	$(MAKE) zslvtst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzslvtstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zslvtst_pt.o
	$(MAKE) zslvtst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzllt_all_pt_dyn : xzllttst_pt xzllttstF_pt \
                   xzllttst_sys_pt xzllttstF_sys_pt PTshared

xzllttst_pt_dyn : zllttst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zllttst_pt.o
	$(MAKE) zllttst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzllttstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zllttst_pt.o
	$(MAKE) zllttst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzlu_all_pt_dyn : xzlutst_pt xzlutstF_pt \
                   xzlutst_sys_pt xzlutstF_sys_pt PTshared

xzlutst_pt_dyn : zlutst_pt.o zlapack ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt PTshared
	rm -f zlutst_pt.o
	$(MAKE) zlutst_pt.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst_pt.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzlutstF_pt_dyn : zPTlapack ztstlib_pt zl3lib_pt zl2lib_pt \
                   zl1lib_pt zcblas_pt zfblas_pt PTshared
	rm -f zlutst_pt.o
	$(MAKE) zlutst_pt.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzmmtst_pt_dyn : $(INCAdir)/atlas_type.h zgemmtst_pt.o zmmlib_pt ztstlib_pt PTshared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_pt.o \
              $(TESTlib) $(zmmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)

xzl1blastst_dyn : zl1blastst.o zl1lib ztstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl1blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzl2blastst_dyn : zl2blastst.o zl2lib ztstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl2blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzl3blastst_dyn : $(INCAdir)/atlas_type.h zl3blastst.o \
                   ztstlib zl3lib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst.o \
                   $(TESTlib) $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzorm_all_dyn : xzormtst xzormtstF \
                   xzormtst_sys xzormtstF_sys shared

xzormtst_dyn : zormtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zormtst.o
	$(MAKE) zormtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzormtstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zormtst.o
	$(MAKE) zormtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzqr_all_dyn : xzqrtst xzqrtstF \
                   xzqrtst_sys xzqrtstF_sys shared

xzqrtst_dyn : zqrtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zqrtst.o
	$(MAKE) zqrtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzqrtstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zqrtst.o
	$(MAKE) zqrtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzinv_all_dyn : xzinvtst xzinvtstF \
                   xzinvtst_sys xzinvtstF_sys shared

xzinvtst_dyn : zinvtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zinvtst.o
	$(MAKE) zinvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzinvtstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zinvtst.o
	$(MAKE) zinvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xztrtri_all_dyn : xztrtritst xztrtritstF \
                   xztrtritst_sys xztrtritstF_sys shared

xztrtritst_dyn : ztrtritst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f ztrtritst.o
	$(MAKE) ztrtritst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xztrtritstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f ztrtritst.o
	$(MAKE) ztrtritst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzuum_all_dyn : xzuumtst xzuumtstF \
                   xzuumtst_sys xzuumtstF_sys shared

xzuumtst_dyn : zuumtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zuumtst.o
	$(MAKE) zuumtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzuumtstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zuumtst.o
	$(MAKE) zuumtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzslv_all_dyn : xzslvtst xzslvtstF \
                   xzslvtst_sys xzslvtstF_sys shared

xzslvtst_dyn : zslvtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zslvtst.o
	$(MAKE) zslvtst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzslvtstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zslvtst.o
	$(MAKE) zslvtst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzllt_all_dyn : xzllttst xzllttstF \
                   xzllttst_sys xzllttstF_sys shared

xzllttst_dyn : zllttst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zllttst.o
	$(MAKE) zllttst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzllttstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zllttst.o
	$(MAKE) zllttst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzlu_all_dyn : xzlutst xzlutstF \
                   xzlutst_sys xzlutstF_sys shared

xzlutst_dyn : zlutst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas shared
	rm -f zlutst.o
	$(MAKE) zlutst.o
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst.o \
                   $(TESTlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzlutstF_dyn : zlapack ztstlib zl3lib zl2lib \
                   zl1lib zcblas zfblas shared
	rm -f zlutst.o
	$(MAKE) zlutst.o intf=-DTimeF77
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst.o \
                   $(TESTlib) $(FLAPACKlib) $(LIBdir)/liblapack.so \
                   $(LIBdir)/libf77blas.so $(LIBdir)/libcblas.so \
                   $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)
xzmmtst_dyn : $(INCAdir)/atlas_type.h zgemmtst.o zmmlib ztstlib shared
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst.o \
              $(TESTlib) $(zmmobj) $(LIBdir)/libf77blas.so \
              $(LIBdir)/libcblas.so $(LIBdir)/libatlas.so -Wl,--rpath $(LIBdir)


xzl1blastst_mike : zl1blastst_mike.o zl1lib_pt ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl1blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xzl2blastst_mike : zl2blastst_pt.o zl2lib_pt ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl2blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xzl3blastst_mike : $(INCAdir)/atlas_type.h zl3blastst_pt.o \
                   ztstlib_pt zl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_mike.o $(MIKElib) \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)

ztstlib_tb : ztstlib_pt
ztstlib_sb : ztstlib
zl1lib_sb : zl1lib
zl1lib_pb : zl1lib_pt
zl2lib_sb : zl2lib
zl2lib_pb : zl2lib_pt
zl3lib_sb : zl3lib
zl3lib_pb : zl3lib_pt
#
# Ugly renaming of old system to new; new is <pre>[t,s]name, where
# 't' is threaded and 's' is serial
#
zslapack : zlapack
ztlapack : zlapack_pt
zsl3lib : zl3lib
ztl3lib : zl3lib_pt
zsl2lib : zl2lib
ztl2lib : zl2lib_pt
zsl1lib : zl1lib
ztl1lib : zl1lib_pt
zststlib : ztstlib
zttstlib : ztstlib_pt
zsfblas : zfblas
ztfblas : zfblas_pt
zscblas : zcblas
ztcblas : zcblas_pt
#
# Default lanbtime, calls ATLAS's internal iface
#
xzslatime : zlatime.o zslapack zststlib \
      zsl3lib zsl2lib zsl1lib zsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlatime.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xztlatime : ztlatime.o ztlapack zttstlib \
      ztl3lib ztl2lib ztl1lib ztfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztlatime.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
#
# Targets for building latime with the F77 iface, using ATLAS, system,
# or F77 BLAS and LAPACK in serial and parallel
#
# ATLAS-ATLAS
xzslatime_al_ab : zlatimeF.o zslapack zststlib \
      zsl3lib zsl2lib zsl1lib zsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlatimeF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xztlatime_al_ab : ztlatimeF.o ztlapack zttstlib \
      ztl3lib ztl2lib ztl1lib ztfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztlatimeF.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# ATLAS2-ATLAS2 (to compare against competing ATLAS install)
xzslatime_2l_2b : zlatimeF.o zststlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlatimeF.o \
                   $(TESTlib) $(AT2dir)/liblapack.a $(AT2dir)/libcblas.a \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xztlatime_2l_2b : ztlatimeF.o zttstlib 
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztlatimeF.o \
                   $(TESTlib) $(AT2dir)/libptlapack.a $(AT2dir)/libptcblas.a \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
# F77-ATLAS
xzslatime_fl_ab : zlatimeF.o zslapack zststlib \
      zsl3lib zsl2lib zsl1lib zsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(CBLASlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xztlatime_fl_ab : ztlatimeF.o ztlapack zttstlib \
      ztl3lib ztl2lib ztl1lib ztfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(PTCBLASlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
# System-system; CBLASlib in there since tester can call that iface
xzslatime_sl_sb : zlatimeF.o zslapack zststlib \
      zsl3lib zsl2lib zsl1lib zsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlatimeF.o \
                   $(TESTlib) $(SSLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xztlatime_sl_sb : ztlatimeF.o ztlapack zttstlib \
      ztl3lib ztl2lib ztl1lib ztfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztlatimeF.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
# F77-system
xzslatime_fl_sb : zlatimeF.o zslapack zststlib \
      zsl3lib zsl2lib zsl1lib zsfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)
xztlatime_fl_sb : ztlatimeF.o ztlapack zttstlib \
      ztl3lib ztl2lib ztl1lib ztfblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztlatimeF.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(CBLASlib) \
                   $(ATLASlib) $(LIBS)

zlatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeF77  -DDCPLX $(mySRCdir)/latime.c
zlatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DDCPLX $(mySRCdir)/latime.c
zlatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX $(mySRCdir)/latime.c
zcblas_sb : zcblas
ztlatimeF.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DTimeF77 \
                 -DDCPLX $(mySRCdir)/latime.c
ztlatimeC.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DTimeC  -DATL_USEPTHREADS=1 -DDCPLX \
                 $(mySRCdir)/latime.c
ztlatime.o : $(mySRCdir)/latime.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS=1 -DDCPLX \
                 $(mySRCdir)/latime.c
zcblas_sb : zcblas
zcblas_tb : zcblas_pt
zlapack_sl : zlapack
zlapack_al : zlapack_pt
# All ATLAS libs, C interface to lapack
xzormtst_tl_tb : zormtstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzqrtst_tl_tb : zqrtstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzinvtst_tl_tb : zinvtstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xztrtritst_tl_tb : ztrtritstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzuumtst_tl_tb : zuumtstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzslvtst_tl_tb : zslvtstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzllttst_tl_tb : zllttstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlutst_tl_tb : zlutstC_pt.o zlapack_tl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzormtst_sl_tb : zormtstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzqrtst_sl_tb : zqrtstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzinvtst_sl_tb : zinvtstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xztrtritst_sl_tb : ztrtritstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzuumtst_sl_tb : zuumtstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzslvtst_sl_tb : zslvtstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzllttst_sl_tb : zllttstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlutst_sl_tb : zlutstC.o zlapack_sl ztstlib_tb zl3lib_tb \
                  zl2lib_tb zl1lib_tb zcblas_tb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstC.o \
                   $(TESTlib) $(LAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzormtst_tl_sb : zormtstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzqrtst_tl_sb : zqrtstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzinvtst_tl_sb : zinvtstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xztrtritst_tl_sb : ztrtritstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzuumtst_tl_sb : zuumtstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzslvtst_tl_sb : zslvtstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzllttst_tl_sb : zllttstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlutst_tl_sb : zlutstC_pt.o zlapack_tl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzormtst_sl_sb : zormtstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzqrtst_sl_sb : zqrtstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzinvtst_sl_sb : zinvtstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xztrtritst_sl_sb : ztrtritstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzuumtst_sl_sb : zuumtstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzslvtst_sl_sb : zslvtstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzllttst_sl_sb : zllttstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlutst_sl_sb : zlutstC.o zlapack_sl ztstlib_sb zl3lib_sb \
                  zl2lib_sb zl1lib_sb zcblas_sb
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
xzl1blastst_pt : zl1blastst_pt.o zl1lib_pt ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl1blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
xzl2blastst_pt : zl2blastst_pt.o zl2lib_pt ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl2blastst_pt.o \
                   $(TESTlib) $(BLASlib)  $(ATLASlib) $(LIBS)
xzl3blastst_pt : $(INCAdir)/atlas_type.h zl3blastst_pt.o \
                   ztstlib_pt zl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_pt.o \
                   $(TESTlib) $(BLASlib) $(ATLASlib) $(LIBS)
zsanity_test_pt: xzinvtst_pt xzqrtst_pt xzslvtst_pt xztrtritst_pt xzuumtst_pt
	$(ATLRUN) $(BINdir) xzslvtst_pt -n 477 -r 517 -O 2 c r \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xzslvtst_pt -n 477 -r 517 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xzqrtst_pt -n 1 477 -m 1 517 -U 2 u l \
                  -S 2 r l >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xztrtritst_pt -n 477  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/ptsanity.out
	$(ATLRUN) $(BINdir) xzuumtst_pt -n 477 -O 2 c r -U 2 u l \
                  >> $(BINdir)/ptsanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzormtst_pt : zormtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzormtstC_pt : zormtstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzormtstF_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzormtst_sb_pt : zormtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzormtstF_sb_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzormtstF_sl_sb_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzormtstF_sl_ab_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzormtstF_fl_ab_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzormtstF_fl_sb_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzormtstF_fl_fb_pt : zormtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzorm_all_pt : xzormtst_pt xzormtstF_pt \
                   xzormtst_sb_pt xzormtstF_sys_pt \
                   xzormtst_sb_pt xzormtstF_sl_sb_pt \
                   xzormtstF_fl_ab_pt xzormtstF_sl_ab_pt \
                   xzormtstF_fl_fb_pt xzormtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzqrtst_pt : zqrtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzqrtstC_pt : zqrtstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzqrtstF_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzqrtst_sb_pt : zqrtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzqrtstF_sb_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzqrtstF_sl_sb_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzqrtstF_sl_ab_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzqrtstF_fl_ab_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzqrtstF_fl_sb_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzqrtstF_fl_fb_pt : zqrtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzqr_all_pt : xzqrtst_pt xzqrtstF_pt \
                   xzqrtst_sb_pt xzqrtstF_sys_pt \
                   xzqrtst_sb_pt xzqrtstF_sl_sb_pt \
                   xzqrtstF_fl_ab_pt xzqrtstF_sl_ab_pt \
                   xzqrtstF_fl_fb_pt xzqrtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzlanbtst_pt : zlanbtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlanbtstC_pt : zlanbtstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzlanbtstF_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzlanbtst_sb_pt : zlanbtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzlanbtstF_sb_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzlanbtstF_sl_sb_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzlanbtstF_sl_ab_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzlanbtstF_fl_ab_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzlanbtstF_fl_sb_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzlanbtstF_fl_fb_pt : zlanbtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzlanb_all_pt : xzlanbtst_pt xzlanbtstF_pt \
                   xzlanbtst_sb_pt xzlanbtstF_sys_pt \
                   xzlanbtst_sb_pt xzlanbtstF_sl_sb_pt \
                   xzlanbtstF_fl_ab_pt xzlanbtstF_sl_ab_pt \
                   xzlanbtstF_fl_fb_pt xzlanbtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzinvtst_pt : zinvtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzinvtstC_pt : zinvtstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzinvtstF_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzinvtst_sb_pt : zinvtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzinvtstF_sb_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzinvtstF_sl_sb_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzinvtstF_sl_ab_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzinvtstF_fl_ab_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzinvtstF_fl_sb_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzinvtstF_fl_fb_pt : zinvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzinv_all_pt : xzinvtst_pt xzinvtstF_pt \
                   xzinvtst_sb_pt xzinvtstF_sys_pt \
                   xzinvtst_sb_pt xzinvtstF_sl_sb_pt \
                   xzinvtstF_fl_ab_pt xzinvtstF_sl_ab_pt \
                   xzinvtstF_fl_fb_pt xzinvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xztrtritst_pt : ztrtritst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xztrtritstC_pt : ztrtritstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xztrtritstF_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xztrtritst_sb_pt : ztrtritst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xztrtritstF_sb_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xztrtritstF_sl_sb_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xztrtritstF_sl_ab_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xztrtritstF_fl_ab_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xztrtritstF_fl_sb_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xztrtritstF_fl_fb_pt : ztrtritstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xztrtri_all_pt : xztrtritst_pt xztrtritstF_pt \
                   xztrtritst_sb_pt xztrtritstF_sys_pt \
                   xztrtritst_sb_pt xztrtritstF_sl_sb_pt \
                   xztrtritstF_fl_ab_pt xztrtritstF_sl_ab_pt \
                   xztrtritstF_fl_fb_pt xztrtritstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzuumtst_pt : zuumtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzuumtstC_pt : zuumtstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzuumtstF_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzuumtst_sb_pt : zuumtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzuumtstF_sb_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzuumtstF_sl_sb_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzuumtstF_sl_ab_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzuumtstF_fl_ab_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzuumtstF_fl_sb_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzuumtstF_fl_fb_pt : zuumtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzuum_all_pt : xzuumtst_pt xzuumtstF_pt \
                   xzuumtst_sb_pt xzuumtstF_sys_pt \
                   xzuumtst_sb_pt xzuumtstF_sl_sb_pt \
                   xzuumtstF_fl_ab_pt xzuumtstF_sl_ab_pt \
                   xzuumtstF_fl_fb_pt xzuumtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzslvtst_pt : zslvtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzslvtstC_pt : zslvtstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzslvtstF_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzslvtst_sb_pt : zslvtst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzslvtstF_sb_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzslvtstF_sl_sb_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzslvtstF_sl_ab_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzslvtstF_fl_ab_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzslvtstF_fl_sb_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzslvtstF_fl_fb_pt : zslvtstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzslv_all_pt : xzslvtst_pt xzslvtstF_pt \
                   xzslvtst_sb_pt xzslvtstF_sys_pt \
                   xzslvtst_sb_pt xzslvtstF_sl_sb_pt \
                   xzslvtstF_fl_ab_pt xzslvtstF_sl_ab_pt \
                   xzslvtstF_fl_fb_pt xzslvtstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzllttst_pt : zllttst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzllttstC_pt : zllttstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzllttstF_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzllttst_sb_pt : zllttst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzllttstF_sb_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzllttstF_sl_sb_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzllttstF_sl_ab_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzllttstF_fl_ab_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzllttstF_fl_sb_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzllttstF_fl_fb_pt : zllttstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzllt_all_pt : xzllttst_pt xzllttstF_pt \
                   xzllttst_sb_pt xzllttstF_sys_pt \
                   xzllttst_sb_pt xzllttstF_sl_sb_pt \
                   xzllttstF_fl_ab_pt xzllttstF_sl_ab_pt \
                   xzllttstF_fl_fb_pt xzllttstF_fl_sb_pt
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzlutst_pt : zlutst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlutstC_pt : zlutstC_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstC_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzlutstF_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(PTCBLASlib) $(PTF77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzlutst_sb_pt : zlutst_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzlutstF_sb_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(PTLAPACKlib) $(BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzlutstF_sl_sb_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzlutstF_sl_ab_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(SLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzlutstF_fl_ab_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(PTF77BLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzlutstF_fl_sb_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(BLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzlutstF_fl_fb_pt : zlutstF_pt.o zlapack_pt ztstlib_pt zl3lib_pt \
                  zl2lib_pt zl1lib_pt zcblas_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF_pt.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(PTCBLASlib) $(ATLASlib) $(LIBS)

xzlu_all_pt : xzlutst_pt xzlutstF_pt \
                   xzlutst_sb_pt xzlutstF_sys_pt \
                   xzlutst_sb_pt xzlutstF_sl_sb_pt \
                   xzlutstF_fl_ab_pt xzlutstF_sl_ab_pt \
                   xzlutstF_fl_fb_pt xzlutstF_fl_sb_pt
xzmmtst_pt : $(INCAdir)/atlas_type.h zgemmtst_pt.o zmmlib_pt ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_pt.o \
              $(TESTlib) $(BLASlib) $(zmmobj) $(ATLASlib) $(LIBS)

xzl1blastst : zl1blastst.o zl1lib ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl1blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
xzl2blastst : zl2blastst.o zl2lib ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl2blastst.o \
                   $(TESTlib) $(SBLASlib)  $(ATLASlib) $(LIBS)
xzl3blastst : $(INCAdir)/atlas_type.h zl3blastst.o \
                   ztstlib zl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst.o \
                   $(TESTlib) $(SBLASlib) $(ATLASlib) $(LIBS)
zsanity_test: xzinvtst xzqrtst xzslvtst xztrtritst xzuumtst
	$(ATLRUN) $(BINdir) xzslvtst -n 167 -r 83 -O 2 c r \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xzslvtst -n 167 -r 83 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xzqrtst -n 1 167 -m 1 83 -U 2 u l \
                  -S 2 r l >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xztrtritst -n 167  -O 2 c r -U 2 u l \
                  -D 2 n u >> $(BINdir)/sanity.out
	$(ATLRUN) $(BINdir) xzuumtst -n 167 -O 2 c r -U 2 u l \
                  >> $(BINdir)/sanity.out

# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzormtst : zormtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzormtstC : zormtstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzormtstF : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzormtst_sb : zormtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzormtstF_sb : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzormtstF_sl_sb : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzormtstF_sl_ab : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzormtstF_fl_ab : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzormtstF_fl_sb : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzormtstF_fl_fb : zormtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zormtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzorm_all : xzormtst xzormtstF \
                   xzormtst_sb xzormtstF_sys \
                   xzormtst_sb xzormtstF_sl_sb \
                   xzormtstF_fl_ab xzormtstF_sl_ab \
                   xzormtstF_fl_fb xzormtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzqrtst : zqrtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzqrtstC : zqrtstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzqrtstF : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzqrtst_sb : zqrtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzqrtstF_sb : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzqrtstF_sl_sb : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzqrtstF_sl_ab : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzqrtstF_fl_ab : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzqrtstF_fl_sb : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzqrtstF_fl_fb : zqrtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zqrtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzqr_all : xzqrtst xzqrtstF \
                   xzqrtst_sb xzqrtstF_sys \
                   xzqrtst_sb xzqrtstF_sl_sb \
                   xzqrtstF_fl_ab xzqrtstF_sl_ab \
                   xzqrtstF_fl_fb xzqrtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzlanbtst : zlanbtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlanbtstC : zlanbtstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzlanbtstF : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzlanbtst_sb : zlanbtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzlanbtstF_sb : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzlanbtstF_sl_sb : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzlanbtstF_sl_ab : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzlanbtstF_fl_ab : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzlanbtstF_fl_sb : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzlanbtstF_fl_fb : zlanbtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlanbtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzlanb_all : xzlanbtst xzlanbtstF \
                   xzlanbtst_sb xzlanbtstF_sys \
                   xzlanbtst_sb xzlanbtstF_sl_sb \
                   xzlanbtstF_fl_ab xzlanbtstF_sl_ab \
                   xzlanbtstF_fl_fb xzlanbtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzinvtst : zinvtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzinvtstC : zinvtstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzinvtstF : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzinvtst_sb : zinvtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzinvtstF_sb : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzinvtstF_sl_sb : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzinvtstF_sl_ab : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzinvtstF_fl_ab : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzinvtstF_fl_sb : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzinvtstF_fl_fb : zinvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zinvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzinv_all : xzinvtst xzinvtstF \
                   xzinvtst_sb xzinvtstF_sys \
                   xzinvtst_sb xzinvtstF_sl_sb \
                   xzinvtstF_fl_ab xzinvtstF_sl_ab \
                   xzinvtstF_fl_fb xzinvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xztrtritst : ztrtritst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xztrtritstC : ztrtritstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xztrtritstF : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xztrtritst_sb : ztrtritst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xztrtritstF_sb : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xztrtritstF_sl_sb : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xztrtritstF_sl_ab : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xztrtritstF_fl_ab : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xztrtritstF_fl_sb : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xztrtritstF_fl_fb : ztrtritstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ ztrtritstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xztrtri_all : xztrtritst xztrtritstF \
                   xztrtritst_sb xztrtritstF_sys \
                   xztrtritst_sb xztrtritstF_sl_sb \
                   xztrtritstF_fl_ab xztrtritstF_sl_ab \
                   xztrtritstF_fl_fb xztrtritstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzuumtst : zuumtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzuumtstC : zuumtstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzuumtstF : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzuumtst_sb : zuumtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzuumtstF_sb : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzuumtstF_sl_sb : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzuumtstF_sl_ab : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzuumtstF_fl_ab : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzuumtstF_fl_sb : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzuumtstF_fl_fb : zuumtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zuumtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzuum_all : xzuumtst xzuumtstF \
                   xzuumtst_sb xzuumtstF_sys \
                   xzuumtst_sb xzuumtstF_sl_sb \
                   xzuumtstF_fl_ab xzuumtstF_sl_ab \
                   xzuumtstF_fl_fb xzuumtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzslvtst : zslvtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzslvtstC : zslvtstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzslvtstF : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzslvtst_sb : zslvtst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzslvtstF_sb : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzslvtstF_sl_sb : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzslvtstF_sl_ab : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzslvtstF_fl_ab : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzslvtstF_fl_sb : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzslvtstF_fl_fb : zslvtstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zslvtstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzslv_all : xzslvtst xzslvtstF \
                   xzslvtst_sb xzslvtstF_sys \
                   xzslvtst_sb xzslvtstF_sl_sb \
                   xzslvtstF_fl_ab xzslvtstF_sl_ab \
                   xzslvtstF_fl_fb xzslvtstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzllttst : zllttst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzllttstC : zllttstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzllttstF : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzllttst_sb : zllttst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzllttstF_sb : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzllttstF_sl_sb : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzllttstF_sl_ab : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzllttstF_fl_ab : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzllttstF_fl_sb : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzllttstF_fl_fb : zllttstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zllttstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzllt_all : xzllttst xzllttstF \
                   xzllttst_sb xzllttstF_sys \
                   xzllttst_sb xzllttstF_sl_sb \
                   xzllttstF_fl_ab xzllttstF_sl_ab \
                   xzllttstF_fl_fb xzllttstF_fl_sb
# All ATLAS libs, C interface only; link in f77 blas for F2C routs
xzlutst : zlutst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, C interface to lapack
xzlutstC : zlutstC.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstC.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# All ATLAS libs, F77 interface to lapack
xzlutstF : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(LAPACKlib) $(CBLASlib) $(F77BLASlib) \
                   $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, C interface
xzlutst_sb : zlutst.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutst.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# ATLAS lapack, system BLAS, F77 interface to lapack
xzlutstF_sb : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(LAPACKlib) $(SBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# System lapack, system BLAS, F77 interface to lapack
xzlutstF_sl_sb : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# System lapack, ATLAS BLAS, F77 interface to lapack
xzlutstF_sl_ab : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(SLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, ATLAS BLAS, F77 interface to lapack
xzlutstF_fl_ab : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(F77BLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, system BLAS, F77 interface to lapack
xzlutstF_fl_sb : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(SBLASlib) $(ATLASlib) $(LIBS)
# F77lapack, F77BLAS, F77 interface to lapack
xzlutstF_fl_fb : zlutstF.o zlapack ztstlib zl3lib \
                  zl2lib zl1lib zcblas
	$(FLINKER) $(FCLINKFLAGS) -o $@ zlutstF.o \
                   $(TESTlib) $(FLAPACKlib) $(FBLASlib) \
                   $(CBLASlib) $(ATLASlib) $(LIBS)

xzlu_all : xzlutst xzlutstF \
                   xzlutst_sb xzlutstF_sys \
                   xzlutst_sb xzlutstF_sl_sb \
                   xzlutstF_fl_ab xzlutstF_sl_ab \
                   xzlutstF_fl_fb xzlutstF_fl_sb
xzmmtst : $(INCAdir)/atlas_type.h zgemmtst.o zmmlib ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst.o \
              $(TESTlib) $(SBLASlib) $(zmmobj) $(ATLASlib) $(LIBS)

#
#  These symbols are for timing, and time the same routine twice: atlas/sys
#
xzl3blastst_sys : $(INCAdir)/atlas_type.h zl3blastst_f77.o ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xzl3blastst_sys_pt : $(INCAdir)/atlas_type.h zl3blastst_f77_pt.o \
   ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_f77.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)
xzl3blastst_atl : $(INCAdir)/atlas_type.h zl3blastst_f77.o \
   ztstlib zl3lib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)
xzl3blastst_atl_pt : $(INCAdir)/atlas_type.h zl3blastst_f77_pt.o \
   ztstlib_pt zl3lib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_f77.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)
xzl3blastst_at2 : $(INCAdir)/atlas_type.h zl3blastst_f77.o \
   ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xzl3blastst_at2_pt : $(INCAdir)/atlas_type.h zl3blastst_f77_pt.o \
   ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zl3blastst_f77.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xzmmtst_sys : $(INCAdir)/atlas_type.h zgemmtst_f77.o ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_f77.o $(TESTlib) \
                   $(SBLASlib) $(ATLASlib) $(LIBS)
xzmmtst_atl : $(INCAdir)/atlas_type.h zgemmtst_f77.o zmmlib ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_f77.o $(TESTlib) \
                   $(F77BLASlib) $(ATLASlib) $(LIBS)

xzmmtst_atl_pt : $(INCAdir)/atlas_type.h zgemmtst_f77_pt.o zmmlib_pt ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_f77_pt.o $(TESTlib) \
                   $(PTF77BLASlib) $(ATLASlib) $(LIBS)

xzmmtst_at2 : zgemmtst_f77_pt.o ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xzmmtst_at2_pt : zgemmtst_f77_pt.o ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_f77_pt.o $(TESTlib) \
                   $(AT2dir)/libptf77blas.a $(AT2dir)/libatlas.a $(LIBS)
xzmmtst_sys_pt : $(INCAdir)/atlas_type.h zgemmtst_f77_pt.o ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_f77_pt.o $(TESTlib) \
                   $(BLASlib) $(ATLASlib) $(LIBS)

zl3blastst_f77.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DDCPLX $(mySRCdir)/l3blastst.c
zl3blastst_f77_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DDCPLX $(mySRCdir)/l3blastst.c
zgemmtst_f77.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DTEST_F77=1 -DDCPLX $(mySRCdir)/gemmtst.c
zgemmtst_f77_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DTEST_F77=1 \
               -DDCPLX $(mySRCdir)/gemmtst.c

noaffobj = ATL_goparallel_noaf.o ATL_dyntlaunch_noaf.o \
           ATL_log2tlaunch_noaf.o ATL_thread_start_noaf.o
noaffobjd = $(BLDdir)/tune/threads/ATL_goparallel_noaf.o \
            $(BLDdir)/tune/threads/ATL_dyntlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_log2tlaunch_noaf.o \
            $(BLDdir)/tune/threads/ATL_thread_start_noaf.o 

xzmmtst_noaff : $(INCAdir)/atlas_type.h zgemmtst_noaff.o \
   zmmlib_pt ztstlib_pt noaff_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_noaff.o $(noaffobjd) \
              $(TESTlib) $(BLASlib) $(zmmobj) $(ATLASlib) $(LIBS)
zgemmtst_noaff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_NOAFFINITY=1 \
               -DDCPLX $(mySRCdir)/gemmtst.c

xzmmtst_aff : $(INCAdir)/atlas_type.h zgemmtst_aff.o zmmlib_pt \
              ztstlib_pt
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_aff.o \
                   $(TESTlib) $(BLASlib) $(zmmobj) $(ATLASlib) $(LIBS)
xzmmtst_big : $(INCAdir)/atlas_type.h zgemmtst_big.o zmmlib \
              ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgemmtst_big.o \
                   $(TESTlib) $(BLASlib) $(zmmobj) $(ATLASlib) $(LIBS)
$(MMTdir)/res/zvMMRES :
	cd $(MMTdir) ; make res/zvMMRES
	cd $(MMTdir) ; make vinstall pre=z
xzgpmmtst : $(INCAdir)/atlas_type.h zgpmmtst.o zmmlib ztstlib
	$(FLINKER) $(FCLINKFLAGS) -o $@ zgpmmtst.o \
              $(TESTlib) $(BLASlib) $(zmmobj) $(ATLASlib) $(LIBS)
zgpmmtst.o : $(mySRCdir)/gpmmtst.c $(INCSdir)/atlas_misc.h $(INCAdir)/zmm.h \
                  $(INCSdir)/atlas_lvl3.h $(INCSdir)/atlas_pkblas.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX $(mySRCdir)/gpmmtst.c
	
ztstlib_pt : ztstlib
ztstlib :
	cd $(TSTdir) ; $(MAKE) zlib

zfblas_pt : zfblaslib_pt
zfblaslib_pt :
	cd $(FBLdir) ; $(MAKE) zptlib
zfblas : zfblaslib
zfblaslib :
	cd $(FBLdir) ; $(MAKE) zlib
zcblas_pt : zcblaslib_pt
zcblaslib_pt :
	cd $(CBLdir) ; $(MAKE) zptlib
zcblas : zcblaslib
zcblaslib :
	cd $(CBLdir) ; $(MAKE) zlib

zmmlib_pt : auxlib_pt
	cd $(GMMdir) ; $(MAKE) zlib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
	cd $(BLDdir)/src/threads/blas/level3 ; $(MAKE) zlib
zmmlib : auxlib
	cd $(GMMdir) ; $(MAKE) zlib
zpklib : auxlib
	cd $(BLDdir)/src/blas/pklevel3 ; $(MAKE) zlib

zl2lib_pt : zl2lib
	cd $(BLDdir)/src/threads/blas/level2 ; $(MAKE) zlib
	cd $(BLDdir)/src/threads ; $(MAKE) lib
zl2lib : zl1lib auxlib
	cd $(GMVdir) ; $(MAKE) zlib
	cd $(GR1dir) ; $(MAKE) zlib
	cd $(L2Bdir) ; $(MAKE) zlib

zl3lib_pt : zl3lib zmmlib_pt
	cd $(BLDdir)/src/threads ; $(MAKE) zlib3
zl3lib : zmmlib
	cd $(L3Bdir) ; $(MAKE) zlib

zl1lib_pt : auxlib_pt zl1lib
zl1lib : auxlib
	cd $(L1Bdir) ; $(MAKE) zlvl1

zlapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) zlib
zlapack_pt : ztlapack
ztlapack :
	cd $(BLDdir)/src/lapack ; $(MAKE) ztlib
	cd $(BLDdir)/src/threads/lapack ; $(MAKE) ztlib

xzprintblk : zprintblk.o
	$(CLINKER) -o $@ zprintblk.o $(ATLASlib)
zprintblk.o : $(INCSdir)/atlas_mv.h $(INCSdir)/atlas_r1.h \
                   $(INCAdir)/zmm.h $(mySRCdir)/printblk.c
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX $(mySRCdir)/printblk.c

zl1blastst_mike.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l1blastst.c
zl2blastst_mike.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DATL_USEPTHREADS -DATL_MIKE $(mySRCdir)/l2blastst.c

zl3blastst_mike.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/l3blastst.c
zlanbtime_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
zlanbtimeC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtimeF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lanbtst.c
zormtst_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/ormtst.c
zormtstC_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/ormtst.c
zormtstF_mike.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/ormtst.c
zqrtst_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/qrtst.c
zqrtstC_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/qrtst.c
zqrtstF_mike.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/qrtst.c
zlanbtst_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtstC_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtstF_mike.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lanbtst.c
zinvtst_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/invtst.c
zinvtstC_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/invtst.c
zinvtstF_mike.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/invtst.c
ztrtritst_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/trtritst.c
ztrtritstC_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/trtritst.c
ztrtritstF_mike.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/trtritst.c
zslvtst_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/slvtst.c
zslvtstC_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/slvtst.c
zslvtstF_mike.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/slvtst.c
zlutst_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lutst.c
zlutstC_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lutst.c
zlutstF_mike.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/lutst.c
zllttst_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/llttst.c
zllttstC_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/llttst.c
zllttstF_mike.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/llttst.c
zuumtst_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/uumtst.c
zuumtstC_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/uumtst.c
zuumtstF_mike.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DATL_MIKE -DDCPLX \
               $(mySRCdir)/uumtst.c

zscaltst_mike.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX $(mySRCdir)/scaltst.c

zgemmtst_mike.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DATL_MIKE -DDCPLX $(mySRCdir)/gemmtst.c

zl1blastst_pt.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DATL_USEPTHREADS $(mySRCdir)/l1blastst.c
zl2blastst_pt.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DATL_USEPTHREADS $(mySRCdir)/l2blastst.c

zl3blastst_pt.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/l3blastst.c
zlanbtime_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
zlanbtimeC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtimeF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lanbtst.c
zormtst_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/ormtst.c
zormtstC_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/ormtst.c
zormtstF_pt.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/ormtst.c
zqrtst_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/qrtst.c
zqrtstC_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/qrtst.c
zqrtstF_pt.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/qrtst.c
zlanbtst_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtstC_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtstF_pt.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lanbtst.c
zinvtst_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/invtst.c
zinvtstC_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/invtst.c
zinvtstF_pt.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/invtst.c
ztrtritst_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/trtritst.c
ztrtritstC_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/trtritst.c
ztrtritstF_pt.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/trtritst.c
zslvtst_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/slvtst.c
zslvtstC_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/slvtst.c
zslvtstF_pt.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/slvtst.c
zlutst_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lutst.c
zlutstC_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lutst.c
zlutstF_pt.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/lutst.c
zllttst_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/llttst.c
zllttstC_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/llttst.c
zllttstF_pt.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/llttst.c
zuumtst_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf) -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/uumtst.c
zuumtstC_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/uumtst.c
zuumtstF_pt.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77 -DATL_USEPTHREADS -DDCPLX \
               $(mySRCdir)/uumtst.c

zscaltst_pt.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/scaltst.c

zgemmtst_pt.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/gemmtst.c

zl1blastst.o : $(mySRCdir)/l1blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level1.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX  $(mySRCdir)/l1blastst.c
zl2blastst.o : $(mySRCdir)/l2blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCSdir)/atlas_tst.h $(INCSdir)/atlas_level2.h
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX  $(mySRCdir)/l2blastst.c

zl3blastst.o : $(mySRCdir)/l3blastst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT  -DDCPLX \
               $(mySRCdir)/l3blastst.c
zlanbtime.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
           -DATL_USE_ATL_ILAENV $(mySRCdir)/lanbtst.c
zlanbtimeC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtimeF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/lanbtst.c
zormtst.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/ormtst.c
zormtstC.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/ormtst.c
zormtstF.o : $(mySRCdir)/ormtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/ormtst.c
zqrtst.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/qrtst.c
zqrtstC.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/qrtst.c
zqrtstF.o : $(mySRCdir)/qrtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/qrtst.c
zlanbtst.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtstC.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/lanbtst.c
zlanbtstF.o : $(mySRCdir)/lanbtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/lanbtst.c
zinvtst.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/invtst.c
zinvtstC.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/invtst.c
zinvtstF.o : $(mySRCdir)/invtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/invtst.c
ztrtritst.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/trtritst.c
ztrtritstC.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/trtritst.c
ztrtritstF.o : $(mySRCdir)/trtritst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/trtritst.c
zslvtst.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/slvtst.c
zslvtstC.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/slvtst.c
zslvtstF.o : $(mySRCdir)/slvtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/slvtst.c
zlutst.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/lutst.c
zlutstC.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/lutst.c
zlutstF.o : $(mySRCdir)/lutst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/lutst.c
zllttst.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/llttst.c
zllttstC.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/llttst.c
zllttstF.o : $(mySRCdir)/llttst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/llttst.c
zuumtst.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT $(intf)  -DDCPLX \
               $(mySRCdir)/uumtst.c
zuumtstC.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeC  -DDCPLX \
               $(mySRCdir)/uumtst.c
zuumtstF.o : $(mySRCdir)/uumtst.c $(INCSdir)/atlas_misc.h \
                   $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h 
	$(KC) -o $@ -c $(KCFLAGS) -DATLCINT -DTimeF77  -DDCPLX \
               $(mySRCdir)/uumtst.c

zscaltst.o : $(mySRCdir)/scaltst.c $(INCSdir)/atlas_misc.h \
                  $(INCSdir)/atlas_f77.h 
	$(KC) -o $@ -c $(KCFLAGS)  -DDCPLX $(mySRCdir)/scaltst.c

zgemmtst.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                  $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h
	$(KC) -o $@ -c $(KCFLAGS)  -DDCPLX $(mySRCdir)/gemmtst.c

zgemmtst_aff.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DTRUST_BIG -DATL_AFFINITY \
               $(mySRCdir)/gemmtst.c
zgemmtst_rkX.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h zkbmmlib
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DTRUST_BIG -DATL_RKXOVER \
               $(mySRCdir)/gemmtst.c
zgemmtst_rk.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DTRUST_BIG -DTEST_RANKK \
               $(mySRCdir)/gemmtst.c
zgemmtst_big.o : $(mySRCdir)/gemmtst.c $(INCSdir)/atlas_misc.h \
                 $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h  
	$(KC) -o $@ -c $(KCFLAGS) -DDCPLX -DTRUST_BIG   \
               $(mySRCdir)/gemmtst.c
noaff_pt : 
	cd $(BLDdir)/tune/threads ; $(MAKE) $(noaffobj)
c_sblas3.o : $(mySRCdir)/c_rblas3.c $(INCSdir)/cblas.h $(INCSdir)/cblas_test.h
	$(KC) -o $@ -c $(KCFLAGS) -DSREAL -DF77INT_ \
               $(mySRCdir)/c_rblas3.c

c_dblas3.o : $(mySRCdir)/c_rblas3.c $(INCSdir)/cblas.h $(INCSdir)/cblas_test.h
	$(KC) -o $@ -c $(KCFLAGS) -DDREAL -DF77INT_ \
               $(mySRCdir)/c_rblas3.c

cblas_xerbla.o : $(mySRCdir)/cblas_xerbla.c
	$(KC) $(KCFLAGS) -o $@ -c $(mySRCdir)/cblas_xerbla.c

force_build:


$(INCAdir)/atlas_type.h : 
	cd $(SYSdir) ; $(MAKE) RunTyp
clean:
	rm -f *.o *.grd
