include Make.inc
mySRCdir = $(SRCdir)/src/blas/gemm

$(INCAdir)/atlas_type.h :
	cd $(SYSdir) ; $(MAKE) RunTyp


all : lib
lib : dlib slib zlib clib
tlib : dtlib stlib ztlib ctlib
double    : dlib
single    : slib
complex   : clib
complex16 : zlib


clean : dclean sclean cclean zclean
	rm -f KERNEL/*.o

sINCdep = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
          $(INCAdir)/smm.h $(INCSdir)/atlas_lvl3.h

sobj = ATL_sMBJBmm.o ATL_sIBNBmm.o ATL_sIBJBmm.o ATL_sgemm.o ATL_sGetNB.o \
       ATL_sGetNCNB.o ATL_sgemmNN.o ATL_sgemmNT.o ATL_sgemmTN.o ATL_sgemmTT.o \
       ATL_sNCmmIJK.o ATL_sNCmmJIK.o ATL_sNCmmIJK_c.o ATL_sNCmmJIK_c.o \
       ATL_saliased_gemm.o ATL_sAgemmNN.o ATL_sAgemmNT.o ATL_sAgemmTN.o \
       ATL_sAgemmTT.o ATL_smmJIK.o ATL_smmIJK.o ATL_smmJKI.o ATL_smmK.o \
       ATL_smmBPP.o ATL_smmJITcp.o ATL_scol2blk_aX.o ATL_srow2blkT_aX.o \
       ATL_scol2blk_a1.o ATL_srow2blkT_a1.o ATL_sputblk_bX.o ATL_sputblk_b1.o \
       ATL_sputblk_bn1.o ATL_sputblk_b0.o
stobj = ATL_stgemmNN.o ATL_stgemmNT.o ATL_stgemmTN.o ATL_stgemmTT.o

dINCdep = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
          $(INCAdir)/dmm.h $(INCSdir)/atlas_lvl3.h

dobj = ATL_dMBJBmm.o ATL_dIBNBmm.o ATL_dIBJBmm.o ATL_dgemm.o ATL_dGetNB.o \
       ATL_dGetNCNB.o ATL_dgemmNN.o ATL_dgemmNT.o ATL_dgemmTN.o ATL_dgemmTT.o \
       ATL_dNCmmIJK.o ATL_dNCmmJIK.o ATL_dNCmmIJK_c.o ATL_dNCmmJIK_c.o \
       ATL_daliased_gemm.o ATL_dAgemmNN.o ATL_dAgemmNT.o ATL_dAgemmTN.o \
       ATL_dAgemmTT.o ATL_dmmJIK.o ATL_dmmIJK.o ATL_dmmJKI.o ATL_dmmK.o \
       ATL_dmmBPP.o ATL_dmmJITcp.o ATL_dcol2blk_aX.o ATL_drow2blkT_aX.o \
       ATL_dcol2blk_a1.o ATL_drow2blkT_a1.o ATL_dputblk_bX.o ATL_dputblk_b1.o \
       ATL_dputblk_bn1.o ATL_dputblk_b0.o
dtobj = ATL_dtgemmNN.o ATL_dtgemmNT.o ATL_dtgemmTN.o ATL_dtgemmTT.o

cINCdep = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
          $(INCAdir)/cmm.h $(INCSdir)/atlas_lvl3.h

cobj = ATL_cMBJBmm.o ATL_cIBNBmm.o ATL_cIBJBmm.o ATL_cgemm.o ATL_cGetNB.o \
       ATL_cGetNCNB.o ATL_cgemmNN.o ATL_cgemmNT.o ATL_cgemmTN.o ATL_cgemmTT.o \
       ATL_cNCmmIJK.o ATL_cNCmmJIK.o ATL_cNCmmIJK_c.o ATL_cNCmmJIK_c.o \
       ATL_caliased_gemm.o ATL_cAgemmNN.o ATL_cAgemmNT.o ATL_cAgemmTN.o \
       ATL_cAgemmTT.o ATL_cmmJIK.o ATL_cmmIJK.o ATL_cmmJKI.o \
       ATL_cgereal2cplx.o ATL_cmmJITcp.o ATL_cAgemmNC.o ATL_cAgemmTC.o \
       ATL_cAgemmCN.o ATL_cAgemmCT.o ATL_cAgemmCC.o ATL_cCNBmm_b0.o \
       ATL_cCNBmm_b1.o ATL_cCNBmm_bX.o ATL_cgemmNC.o ATL_cgemmCN.o \
       ATL_cgemmTC.o ATL_cgemmCT.o ATL_cgemmCC.o ATL_ccol2blk_aXi0.o \
       ATL_crow2blkT_aXi0.o ATL_ccol2blkConj_aXi0.o ATL_crow2blkC_aXi0.o \
       ATL_ccol2blk_aX.o ATL_crow2blkT_aX.o ATL_ccol2blkConj_aX.o \
       ATL_crow2blkC_aX.o ATL_ccol2blk_a1.o ATL_crow2blkT_a1.o \
       ATL_ccol2blkConj_a1.o ATL_crow2blkC_a1.o ATL_cputblk_bXi0.o \
       ATL_cputblk_bX.o ATL_cputblk_b1.o ATL_cputblk_bn1.o ATL_cputblk_b0.o
ctobj = ATL_ctgemmNN.o ATL_ctgemmNT.o ATL_ctgemmTN.o ATL_ctgemmTT.o \
        ATL_ctgemmNC.o ATL_ctgemmCN.o ATL_ctgemmTC.o ATL_ctgemmCT.o \
        ATL_ctgemmCC.o

zINCdep = $(INCSdir)/atlas_misc.h $(INCAdir)/atlas_type.h \
          $(INCAdir)/zmm.h $(INCSdir)/atlas_lvl3.h

zobj = ATL_zMBJBmm.o ATL_zIBNBmm.o ATL_zIBJBmm.o ATL_zgemm.o ATL_zGetNB.o \
       ATL_zGetNCNB.o ATL_zgemmNN.o ATL_zgemmNT.o ATL_zgemmTN.o ATL_zgemmTT.o \
       ATL_zNCmmIJK.o ATL_zNCmmJIK.o ATL_zNCmmIJK_c.o ATL_zNCmmJIK_c.o \
       ATL_zaliased_gemm.o ATL_zAgemmNN.o ATL_zAgemmNT.o ATL_zAgemmTN.o \
       ATL_zAgemmTT.o ATL_zmmJIK.o ATL_zmmIJK.o ATL_zmmJKI.o \
       ATL_zgereal2cplx.o ATL_zmmJITcp.o ATL_zAgemmNC.o ATL_zAgemmTC.o \
       ATL_zAgemmCN.o ATL_zAgemmCT.o ATL_zAgemmCC.o ATL_zCNBmm_b0.o \
       ATL_zCNBmm_b1.o ATL_zCNBmm_bX.o ATL_zgemmNC.o ATL_zgemmCN.o \
       ATL_zgemmTC.o ATL_zgemmCT.o ATL_zgemmCC.o ATL_zcol2blk_aXi0.o \
       ATL_zrow2blkT_aXi0.o ATL_zcol2blkConj_aXi0.o ATL_zrow2blkC_aXi0.o \
       ATL_zcol2blk_aX.o ATL_zrow2blkT_aX.o ATL_zcol2blkConj_aX.o \
       ATL_zrow2blkC_aX.o ATL_zcol2blk_a1.o ATL_zrow2blkT_a1.o \
       ATL_zcol2blkConj_a1.o ATL_zrow2blkC_a1.o ATL_zputblk_bXi0.o \
       ATL_zputblk_bX.o ATL_zputblk_b1.o ATL_zputblk_bn1.o ATL_zputblk_b0.o
ztobj = ATL_ztgemmNN.o ATL_ztgemmNT.o ATL_ztgemmTN.o ATL_ztgemmTT.o \
        ATL_ztgemmNC.o ATL_ztgemmCN.o ATL_ztgemmTC.o ATL_ztgemmCT.o \
        ATL_ztgemmCC.o


auxillib :
	cd $(AUXdir) ; $(MAKE) lib
sclean :
	rm -f $(sobj) s*.grd
scleanuplib :
	cd KERNEL ; $(MAKE) -f sMakefile slib

slib : 
	$(MAKE) auxillib scleanuplib susergemm
	$(PMAKE) slib.grd
slib.grd : $(sobj) ATL_ssmallK_mm.o ATL_ssmall_mm.o \
           ATL_sbig_mm.o ATL_sbignork_mm.o
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
stlib : stlib.grd
stlib.grd : $(stobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(stobj)
	$(RANLIB) $(ATLASlib)
	touch stlib.grd

#
# This target no longer needed w/o support for GEMM replacement, but
# keep around for now as empty
#
susergemm :

susermm :
	touch susermm
ATL_stgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_sgemmRecNN.o : $(mySRCdir)/ATL_gemmRecXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_sgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_sAgemmNN.o : $(mySRCdir)/ATL_AgemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_AgemmXX.c
ATL_stgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_sgemmRecNT.o : $(mySRCdir)/ATL_gemmRecXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_sgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_sAgemmNT.o : $(mySRCdir)/ATL_AgemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_AgemmXX.c
ATL_stgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_sgemmRecTN.o : $(mySRCdir)/ATL_gemmRecXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_sgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_sAgemmTN.o : $(mySRCdir)/ATL_AgemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_AgemmXX.c
ATL_stgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_sgemmRecTT.o : $(mySRCdir)/ATL_gemmRecXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_sgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_sAgemmTT.o : $(mySRCdir)/ATL_AgemmXX.c $(sINCdep) $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_AgemmXX.c

ATL_sIBNBmm.o : $(mySRCdir)/ATL_IBNBmm.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_IBNBmm.c
ATL_sMBJBmm.o : $(mySRCdir)/ATL_MBJBmm.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_MBJBmm.c
ATL_sIBJBmm.o : $(mySRCdir)/ATL_IBJBmm.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_IBJBmm.c
ATL_smmK.o : $(mySRCdir)/ATL_mmK.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mmK.c
ATL_smmBPP.o : $(mySRCdir)/ATL_mmBPP.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mmBPP.c
ATL_smmJITcp.o : $(mySRCdir)/ATL_mmJITcp.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mmJITcp.c
ATL_smmJKI.o : $(mySRCdir)/ATL_mmJKI.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mmJKI.c
ATL_smmJIK.o : $(mySRCdir)/ATL_mmJIK.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mmJIK.c
ATL_smmIJK.o : $(mySRCdir)/ATL_mmIJK.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_mmIJK.c

ATL_srow2blkT_aX.o : $(mySRCdir)/ATL_row2blkT.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX $(mySRCdir)/ATL_row2blkT.c
ATL_scol2blk_aX.o : $(mySRCdir)/ATL_col2blk.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX $(mySRCdir)/ATL_col2blk.c
ATL_srow2blkT_a1.o : $(mySRCdir)/ATL_row2blkT.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 $(mySRCdir)/ATL_row2blkT.c
ATL_scol2blk_a1.o : $(mySRCdir)/ATL_col2blk.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 $(mySRCdir)/ATL_col2blk.c
ATL_sNCmmJIK_c.o : $(mySRCdir)/ATL_NCmmJIK_c.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL   $(mySRCdir)/ATL_NCmmJIK_c.c
ATL_sNCmmJIK.o : $(mySRCdir)/ATL_NCmmJIK.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL   $(mySRCdir)/ATL_NCmmJIK.c
ATL_sNCmmIJK_c.o : $(mySRCdir)/ATL_NCmmIJK_c.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL   $(mySRCdir)/ATL_NCmmIJK_c.c
ATL_sNCmmIJK.o : $(mySRCdir)/ATL_NCmmIJK.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL   $(mySRCdir)/ATL_NCmmIJK.c

ATL_sGetNCNB.o : $(mySRCdir)/ATL_GetNCNB.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_GetNCNB.c
ATL_sGetNB.o : $(mySRCdir)/ATL_GetNB.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_GetNB.c

ATL_sputblk_bX.o : $(mySRCdir)/ATL_putblk.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETAX $(mySRCdir)/ATL_putblk.c
ATL_sputblk_bn1.o : $(mySRCdir)/ATL_putblk.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETAN1 $(mySRCdir)/ATL_putblk.c
ATL_sputblk_b1.o : $(mySRCdir)/ATL_putblk.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA1 $(mySRCdir)/ATL_putblk.c
ATL_sputblk_b0.o : $(mySRCdir)/ATL_putblk.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA0 $(mySRCdir)/ATL_putblk.c

ATL_saliased_gemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_AgemmXX.c $(sINCdep) \
                           $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALIASED_GEMM -I./ \
              $(mySRCdir)/ATL_gemm.c

ATL_ssmallK_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DSMALLK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_ssmall_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DSMALL_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_sbignork_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBIGNORK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_sbig_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(sINCdep) \
                  $(INCAdir)/sXover.h $(INCAdir)/atlas_cacheedge.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBIG_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_sgemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(sINCdep) \
                  $(INCAdir)/atlas_cacheedge.h $(INCAdir)/sXover.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DATLGEMM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_sFindCE_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(sINCdep) \
                        $(INCAdir)/sXover.h
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DFindingCE -I./ $(mySRCdir)/ATL_gemm.c

ATL_sNCgemm.o : $(mySRCdir)/ATL_NCgemm.c $(sINCdep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL $(mySRCdir)/ATL_NCgemm.c

dclean :
	rm -f $(dobj) d*.grd
dcleanuplib :
	cd KERNEL ; $(MAKE) -f dMakefile dlib

dlib : 
	$(MAKE) auxillib dcleanuplib dusergemm
	$(PMAKE) dlib.grd
dlib.grd : $(dobj) ATL_dsmallK_mm.o ATL_dsmall_mm.o \
           ATL_dbig_mm.o ATL_dbignork_mm.o
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
dtlib : dtlib.grd
dtlib.grd : $(dtobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dtobj)
	$(RANLIB) $(ATLASlib)
	touch dtlib.grd

#
# This target no longer needed w/o support for GEMM replacement, but
# keep around for now as empty
#
dusergemm :

dusermm :
	touch dusermm
ATL_dtgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_dgemmRecNN.o : $(mySRCdir)/ATL_gemmRecXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_dgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_dAgemmNN.o : $(mySRCdir)/ATL_AgemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_AgemmXX.c
ATL_dtgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_dgemmRecNT.o : $(mySRCdir)/ATL_gemmRecXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_dgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_dAgemmNT.o : $(mySRCdir)/ATL_AgemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_AgemmXX.c
ATL_dtgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_dgemmRecTN.o : $(mySRCdir)/ATL_gemmRecXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_dgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_dAgemmTN.o : $(mySRCdir)/ATL_AgemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_AgemmXX.c
ATL_dtgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_tcacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_dgemmRecTT.o : $(mySRCdir)/ATL_gemmRecXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmRecXX.c
ATL_dgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_dAgemmTT.o : $(mySRCdir)/ATL_AgemmXX.c $(dINCdep) $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_AgemmXX.c

ATL_dIBNBmm.o : $(mySRCdir)/ATL_IBNBmm.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_IBNBmm.c
ATL_dMBJBmm.o : $(mySRCdir)/ATL_MBJBmm.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_MBJBmm.c
ATL_dIBJBmm.o : $(mySRCdir)/ATL_IBJBmm.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_IBJBmm.c
ATL_dmmK.o : $(mySRCdir)/ATL_mmK.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mmK.c
ATL_dmmBPP.o : $(mySRCdir)/ATL_mmBPP.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mmBPP.c
ATL_dmmJITcp.o : $(mySRCdir)/ATL_mmJITcp.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mmJITcp.c
ATL_dmmJKI.o : $(mySRCdir)/ATL_mmJKI.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mmJKI.c
ATL_dmmJIK.o : $(mySRCdir)/ATL_mmJIK.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mmJIK.c
ATL_dmmIJK.o : $(mySRCdir)/ATL_mmIJK.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_mmIJK.c

ATL_drow2blkT_aX.o : $(mySRCdir)/ATL_row2blkT.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX $(mySRCdir)/ATL_row2blkT.c
ATL_dcol2blk_aX.o : $(mySRCdir)/ATL_col2blk.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX $(mySRCdir)/ATL_col2blk.c
ATL_drow2blkT_a1.o : $(mySRCdir)/ATL_row2blkT.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 $(mySRCdir)/ATL_row2blkT.c
ATL_dcol2blk_a1.o : $(mySRCdir)/ATL_col2blk.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 $(mySRCdir)/ATL_col2blk.c
ATL_dNCmmJIK_c.o : $(mySRCdir)/ATL_NCmmJIK_c.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL   $(mySRCdir)/ATL_NCmmJIK_c.c
ATL_dNCmmJIK.o : $(mySRCdir)/ATL_NCmmJIK.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL   $(mySRCdir)/ATL_NCmmJIK.c
ATL_dNCmmIJK_c.o : $(mySRCdir)/ATL_NCmmIJK_c.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL   $(mySRCdir)/ATL_NCmmIJK_c.c
ATL_dNCmmIJK.o : $(mySRCdir)/ATL_NCmmIJK.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL   $(mySRCdir)/ATL_NCmmIJK.c

ATL_dGetNCNB.o : $(mySRCdir)/ATL_GetNCNB.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_GetNCNB.c
ATL_dGetNB.o : $(mySRCdir)/ATL_GetNB.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_GetNB.c

ATL_dputblk_bX.o : $(mySRCdir)/ATL_putblk.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETAX $(mySRCdir)/ATL_putblk.c
ATL_dputblk_bn1.o : $(mySRCdir)/ATL_putblk.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETAN1 $(mySRCdir)/ATL_putblk.c
ATL_dputblk_b1.o : $(mySRCdir)/ATL_putblk.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA1 $(mySRCdir)/ATL_putblk.c
ATL_dputblk_b0.o : $(mySRCdir)/ATL_putblk.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA0 $(mySRCdir)/ATL_putblk.c

ATL_daliased_gemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_AgemmXX.c $(dINCdep) \
                           $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALIASED_GEMM -I./ \
              $(mySRCdir)/ATL_gemm.c

ATL_dsmallK_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DSMALLK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_dsmall_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DSMALL_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_dbignork_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBIGNORK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_dbig_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(dINCdep) \
                  $(INCAdir)/dXover.h $(INCAdir)/atlas_cacheedge.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBIG_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_dgemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(dINCdep) \
                  $(INCAdir)/atlas_cacheedge.h $(INCAdir)/dXover.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DATLGEMM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_dFindCE_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(dINCdep) \
                        $(INCAdir)/dXover.h
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DFindingCE -I./ $(mySRCdir)/ATL_gemm.c

ATL_dNCgemm.o : $(mySRCdir)/ATL_NCgemm.c $(dINCdep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL $(mySRCdir)/ATL_NCgemm.c

cclean :
	rm -f $(cobj) c*.grd
ccleanuplib :
	cd KERNEL ; $(MAKE) -f cMakefile clib

clib : 
	$(MAKE) auxillib ccleanuplib cusergemm
	$(PMAKE) clib.grd
clib.grd : $(cobj) ATL_csmallK_mm.o ATL_csmall_mm.o \
           ATL_cbig_mm.o
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
ctlib : ctlib.grd
ctlib.grd : $(ctobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ctobj)
	$(RANLIB) $(ATLASlib)
	touch ctlib.grd

#
# This target no longer needed w/o support for GEMM replacement, but
# keep around for now as empty
#
cusergemm :

cusermm :
	touch cusermm
ATL_ctgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecNN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmNN.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNoTransA_ -DNoTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecNT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmNT.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNoTransA_ -DTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmNC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DConjTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecNC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmNC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DNoTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmNC.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNoTransA_ -DConjTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecTN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmTN.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DTransA_ -DNoTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecTT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmTT.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DTransA_ -DTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmTC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DConjTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecTC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmTC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmTC.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DTransA_ -DConjTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmCN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecCN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmCN.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmCN.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConjTransA_ -DNoTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmCT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecCT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmCT.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmCT.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConjTransA_ -DTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ctgemmCC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DConjTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmRecCC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cgemmCC.o : $(mySRCdir)/ATL_gemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX \
              -DConjTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_cAgemmCC.o : $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DConjTransA_ -DConjTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_cgereal2cplx.o : $(mySRCdir)/ATL_gereal2cplx.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_gereal2cplx.c

ATL_cIBNBmm.o : $(mySRCdir)/ATL_IBNBmm.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_IBNBmm.c
ATL_cMBJBmm.o : $(mySRCdir)/ATL_MBJBmm.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_MBJBmm.c
ATL_cIBJBmm.o : $(mySRCdir)/ATL_IBJBmm.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_IBJBmm.c

ATL_cmmJITcp.o : $(mySRCdir)/ATL_cmmJITcp.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_cmmJITcp.c
ATL_cmmK.o : $(mySRCdir)/ATL_cmmK.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_cmmK.c
ATL_cmmJKI.o : $(mySRCdir)/ATL_cmmJKI.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_cmmJKI.c
ATL_cmmIJK.o : $(mySRCdir)/ATL_cmmIJK.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_cmmIJK.c
ATL_cmmJIK.o : $(mySRCdir)/ATL_cmmJIK.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_cmmJIK.c

ATL_crow2blkT_aX.o : $(mySRCdir)/ATL_crow2blkT.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAX $(mySRCdir)/ATL_crow2blkT.c
ATL_ccol2blk_aX.o : $(mySRCdir)/ATL_ccol2blk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAX $(mySRCdir)/ATL_ccol2blk.c
ATL_ccol2blkConj_aX.o : $(mySRCdir)/ATL_ccol2blk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAX \
              -DConj_ $(mySRCdir)/ATL_ccol2blk.c
ATL_crow2blkC_aX.o : $(mySRCdir)/ATL_crow2blkT.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAX \
              -DConj_ $(mySRCdir)/ATL_crow2blkT.c

ATL_crow2blkT_aXi0.o : $(mySRCdir)/ATL_crow2blkT.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAXI0 $(mySRCdir)/ATL_crow2blkT.c
ATL_ccol2blk_aXi0.o : $(mySRCdir)/ATL_ccol2blk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAXI0 $(mySRCdir)/ATL_ccol2blk.c
ATL_ccol2blkConj_aXi0.o : $(mySRCdir)/ATL_ccol2blk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAXI0 \
              -DConj_ $(mySRCdir)/ATL_ccol2blk.c
ATL_crow2blkC_aXi0.o : $(mySRCdir)/ATL_crow2blkT.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHAXI0 \
              -DConj_ $(mySRCdir)/ATL_crow2blkT.c

ATL_crow2blkT_a1.o : $(mySRCdir)/ATL_crow2blkT.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHA1 $(mySRCdir)/ATL_crow2blkT.c
ATL_ccol2blk_a1.o : $(mySRCdir)/ATL_ccol2blk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHA1 $(mySRCdir)/ATL_ccol2blk.c
ATL_ccol2blkConj_a1.o : $(mySRCdir)/ATL_ccol2blk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHA1 \
              -DConj_ $(mySRCdir)/ATL_ccol2blk.c
ATL_crow2blkC_a1.o : $(mySRCdir)/ATL_crow2blkT.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALPHA1 \
              -DConj_ $(mySRCdir)/ATL_crow2blkT.c

ATL_cNCmmJIK_c.o : $(mySRCdir)/ATL_cNCmmJIK_c.c $(cINCdep) \
                           $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX   $(mySRCdir)/ATL_cNCmmJIK_c.c
ATL_cNCmmJIK.o : $(mySRCdir)/ATL_cNCmmJIK.c $(cINCdep) \
                           $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX   $(mySRCdir)/ATL_cNCmmJIK.c
ATL_cNCmmIJK_c.o : $(mySRCdir)/ATL_cNCmmIJK_c.c $(cINCdep) \
                           $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX   $(mySRCdir)/ATL_cNCmmIJK_c.c
ATL_cNCmmIJK.o : $(mySRCdir)/ATL_cNCmmIJK.c $(cINCdep) \
                           $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX   $(mySRCdir)/ATL_cNCmmIJK.c

ATL_cCNBmm_bX.o : $(mySRCdir)/ATL_CNBmm_bX.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_CNBmm_bX.c
ATL_cCNBmm_b1.o : $(mySRCdir)/ATL_CNBmm_b1.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_CNBmm_b1.c
ATL_cCNBmm_b0.o : $(mySRCdir)/ATL_CNBmm_b0.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_CNBmm_b0.c
ATL_cGetNCNB.o : $(mySRCdir)/ATL_GetNCNB.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_GetNCNB.c
ATL_cGetNB.o : $(mySRCdir)/ATL_GetNB.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_GetNB.c

ATL_cputblk_bXi0.o : $(mySRCdir)/ATL_putblk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 $(mySRCdir)/ATL_putblk.c
ATL_cputblk_bX.o : $(mySRCdir)/ATL_putblk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAX $(mySRCdir)/ATL_putblk.c
ATL_cputblk_bn1.o : $(mySRCdir)/ATL_putblk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAN1 $(mySRCdir)/ATL_putblk.c
ATL_cputblk_b1.o : $(mySRCdir)/ATL_putblk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 $(mySRCdir)/ATL_putblk.c
ATL_cputblk_b0.o : $(mySRCdir)/ATL_putblk.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 $(mySRCdir)/ATL_putblk.c

ATL_caliased_gemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_AgemmXX.c $(cINCdep) \
                           $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DALIASED_GEMM -I./ \
              $(mySRCdir)/ATL_gemm.c

ATL_csmallK_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                  $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DSMALLK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_csmall_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                  $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DSMALL_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_cbignork_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                  $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBIGNORK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_cbig_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                  $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBIG_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_csbig_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                  $(INCAdir)/cXover.h $(INCAdir)/atlas_cacheedge.h \
                  $(INCAdir)/atlas_csNKB.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DCRBIG_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_cgemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                  $(INCAdir)/atlas_cacheedge.h $(INCAdir)/cXover.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DATLGEMM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_cFindCE_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                        $(INCAdir)/cXover.h
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DFindingCE -I./ $(mySRCdir)/ATL_gemm.c

ATL_cNCgemm.o : $(mySRCdir)/ATL_NCgemm.c $(cINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_NCgemm.c

zclean :
	rm -f $(zobj) z*.grd
zcleanuplib :
	cd KERNEL ; $(MAKE) -f zMakefile zlib

zlib : 
	$(MAKE) auxillib zcleanuplib zusergemm
	$(PMAKE) zlib.grd
zlib.grd : $(zobj) ATL_zsmallK_mm.o ATL_zsmall_mm.o \
           ATL_zbig_mm.o
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
ztlib : ztlib.grd
ztlib.grd : $(ztobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ztobj)
	$(RANLIB) $(ATLASlib)
	touch ztlib.grd

#
# This target no longer needed w/o support for GEMM replacement, but
# keep around for now as empty
#
zusergemm :

zusermm :
	touch zusermm
ATL_ztgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecNN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmNN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmNN.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNoTransA_ -DNoTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecNT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmNT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmNT.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNoTransA_ -DTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmNC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DConjTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecNC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmNC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DNoTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmNC.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNoTransA_ -DConjTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecTN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmTN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmTN.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DTransA_ -DNoTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecTT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmTT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmTT.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DTransA_ -DTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmTC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DConjTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecTC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmTC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmTC.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DTransA_ -DConjTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmCN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DNoTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecCN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmCN.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DNoTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmCN.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConjTransA_ -DNoTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmCT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecCT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmCT.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmCT.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConjTransA_ -DTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_ztgemmCC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                   $(INCAdir)/atlas_tcacheedge.h $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DConjTransB_ -DATL_TGEMM $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmRecCC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zgemmCC.o : $(mySRCdir)/ATL_gemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX \
              -DConjTransA_ -DConjTransB_ $(mySRCdir)/ATL_gemmXX.c
ATL_zAgemmCC.o : $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) $(INCAdir)/atlas_cacheedge.h \
                   $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DConjTransA_ -DConjTransB_ \
              -DALIASED_GEMM $(mySRCdir)/ATL_AgemmXX.c
ATL_zgereal2cplx.o : $(mySRCdir)/ATL_gereal2cplx.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_gereal2cplx.c

ATL_zIBNBmm.o : $(mySRCdir)/ATL_IBNBmm.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_IBNBmm.c
ATL_zMBJBmm.o : $(mySRCdir)/ATL_MBJBmm.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_MBJBmm.c
ATL_zIBJBmm.o : $(mySRCdir)/ATL_IBJBmm.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_IBJBmm.c

ATL_zmmJITcp.o : $(mySRCdir)/ATL_cmmJITcp.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_cmmJITcp.c
ATL_zmmK.o : $(mySRCdir)/ATL_cmmK.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_cmmK.c
ATL_zmmJKI.o : $(mySRCdir)/ATL_cmmJKI.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_cmmJKI.c
ATL_zmmIJK.o : $(mySRCdir)/ATL_cmmIJK.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_cmmIJK.c
ATL_zmmJIK.o : $(mySRCdir)/ATL_cmmJIK.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_cmmJIK.c

ATL_zrow2blkT_aX.o : $(mySRCdir)/ATL_crow2blkT.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAX $(mySRCdir)/ATL_crow2blkT.c
ATL_zcol2blk_aX.o : $(mySRCdir)/ATL_ccol2blk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAX $(mySRCdir)/ATL_ccol2blk.c
ATL_zcol2blkConj_aX.o : $(mySRCdir)/ATL_ccol2blk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAX \
              -DConj_ $(mySRCdir)/ATL_ccol2blk.c
ATL_zrow2blkC_aX.o : $(mySRCdir)/ATL_crow2blkT.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAX \
              -DConj_ $(mySRCdir)/ATL_crow2blkT.c

ATL_zrow2blkT_aXi0.o : $(mySRCdir)/ATL_crow2blkT.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAXI0 $(mySRCdir)/ATL_crow2blkT.c
ATL_zcol2blk_aXi0.o : $(mySRCdir)/ATL_ccol2blk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAXI0 $(mySRCdir)/ATL_ccol2blk.c
ATL_zcol2blkConj_aXi0.o : $(mySRCdir)/ATL_ccol2blk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAXI0 \
              -DConj_ $(mySRCdir)/ATL_ccol2blk.c
ATL_zrow2blkC_aXi0.o : $(mySRCdir)/ATL_crow2blkT.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHAXI0 \
              -DConj_ $(mySRCdir)/ATL_crow2blkT.c

ATL_zrow2blkT_a1.o : $(mySRCdir)/ATL_crow2blkT.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHA1 $(mySRCdir)/ATL_crow2blkT.c
ATL_zcol2blk_a1.o : $(mySRCdir)/ATL_ccol2blk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHA1 $(mySRCdir)/ATL_ccol2blk.c
ATL_zcol2blkConj_a1.o : $(mySRCdir)/ATL_ccol2blk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHA1 \
              -DConj_ $(mySRCdir)/ATL_ccol2blk.c
ATL_zrow2blkC_a1.o : $(mySRCdir)/ATL_crow2blkT.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALPHA1 \
              -DConj_ $(mySRCdir)/ATL_crow2blkT.c

ATL_zNCmmJIK_c.o : $(mySRCdir)/ATL_cNCmmJIK_c.c $(zINCdep) \
                           $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX   $(mySRCdir)/ATL_cNCmmJIK_c.c
ATL_zNCmmJIK.o : $(mySRCdir)/ATL_cNCmmJIK.c $(zINCdep) \
                           $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX   $(mySRCdir)/ATL_cNCmmJIK.c
ATL_zNCmmIJK_c.o : $(mySRCdir)/ATL_cNCmmIJK_c.c $(zINCdep) \
                           $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX   $(mySRCdir)/ATL_cNCmmIJK_c.c
ATL_zNCmmIJK.o : $(mySRCdir)/ATL_cNCmmIJK.c $(zINCdep) \
                           $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX   $(mySRCdir)/ATL_cNCmmIJK.c

ATL_zCNBmm_bX.o : $(mySRCdir)/ATL_CNBmm_bX.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_CNBmm_bX.c
ATL_zCNBmm_b1.o : $(mySRCdir)/ATL_CNBmm_b1.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_CNBmm_b1.c
ATL_zCNBmm_b0.o : $(mySRCdir)/ATL_CNBmm_b0.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_CNBmm_b0.c
ATL_zGetNCNB.o : $(mySRCdir)/ATL_GetNCNB.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_GetNCNB.c
ATL_zGetNB.o : $(mySRCdir)/ATL_GetNB.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_GetNB.c

ATL_zputblk_bXi0.o : $(mySRCdir)/ATL_putblk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 $(mySRCdir)/ATL_putblk.c
ATL_zputblk_bX.o : $(mySRCdir)/ATL_putblk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAX $(mySRCdir)/ATL_putblk.c
ATL_zputblk_bn1.o : $(mySRCdir)/ATL_putblk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAN1 $(mySRCdir)/ATL_putblk.c
ATL_zputblk_b1.o : $(mySRCdir)/ATL_putblk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 $(mySRCdir)/ATL_putblk.c
ATL_zputblk_b0.o : $(mySRCdir)/ATL_putblk.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 $(mySRCdir)/ATL_putblk.c

ATL_zaliased_gemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_AgemmXX.c $(zINCdep) \
                           $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DALIASED_GEMM -I./ \
              $(mySRCdir)/ATL_gemm.c

ATL_zsmallK_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                  $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DSMALLK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_zsmall_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                  $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DSMALL_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_zbignork_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                  $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBIGNORK_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_zbig_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                  $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBIG_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_zdbig_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                  $(INCAdir)/zXover.h $(INCAdir)/atlas_cacheedge.h \
                  $(INCAdir)/atlas_zdNKB.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DCRBIG_MM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_zgemm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                  $(INCAdir)/atlas_cacheedge.h $(INCAdir)/zXover.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DATLGEMM -I./ \
              $(mySRCdir)/ATL_gemm.c
ATL_zFindCE_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                        $(INCAdir)/zXover.h
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DFindingCE -I./ $(mySRCdir)/ATL_gemm.c

ATL_zNCgemm.o : $(mySRCdir)/ATL_NCgemm.c $(zINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_NCgemm.c

ATL_csFindCE_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(cINCdep) \
                        $(INCAdir)/cXover.h $(sINCdep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DFindingJITCPCE -I./ $(mySRCdir)/ATL_gemm.c
ATL_zdFindCE_mm.o : $(mySRCdir)/ATL_gemm.c $(mySRCdir)/ATL_gemmXX.c $(zINCdep) \
                        $(INCAdir)/zXover.h $(dINCdep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DFindingJITCPCE -I./ $(mySRCdir)/ATL_gemm.c
ass.grd:
	- $(MAKE) ATL_dasrc2blk_NB_aX.o ATL_dasrc2blk_NB_a1.o
	- $(ARCHIVER) $(ARFLAGS) $(ATLASlib) \
                      ATL_dasrc2blk_NB_aX.o ATL_dasrc2blk_NB_a1.o
	- $(RANLIB) $(ATLASlib)

ATL_dasrc2blk_NB_aX.o: $(mySRCdir)/ATL_asrc2blk_NB.c
	$(GOODGCC) $(CDEFS) -o $@ -c -x assembler-with-cpp \
                    -DDREAL -DALPHAX $(mySRCdir)/ATL_asrc2blk_NB.c
ATL_dasrc2blk_NB_a1.o: $(mySRCdir)/ATL_asrc2blk_NB.c
	$(GOODGCC) $(CDEFS) -o $@ -c -x assembler-with-cpp \
                    -DDREAL -DALPHA1 $(mySRCdir)/ATL_asrc2blk_NB.c

