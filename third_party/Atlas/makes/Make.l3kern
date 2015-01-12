include Make.inc
mySRCdir = $(SRCdir)/src/blas/level3/kernel

sobj = ATL_ssycopyL_a1.o ATL_ssycopyL_aX.o ATL_ssycopyU_a1.o \
       ATL_ssycopyU_aX.o ATL_ssymmLL.o ATL_ssymmLU.o ATL_ssymmRL.o \
       ATL_ssymmRU.o ATL_ssyr2kLN.o ATL_ssyr2kLT.o ATL_ssyr2kUN.o \
       ATL_ssyr2kUT.o ATL_ssyr2k_putL_b0.o ATL_ssyr2k_putL_b1.o \
       ATL_ssyr2k_putL_bX.o ATL_ssyr2k_putU_b0.o ATL_ssyr2k_putU_b1.o \
       ATL_ssyr2k_putU_bX.o ATL_ssyrkLN.o ATL_ssyrkLT.o ATL_ssyrkUN.o \
       ATL_ssyrkUT.o ATL_strcopyL2L_N_a1.o ATL_strcopyL2L_N_aX.o \
       ATL_strcopyL2L_U_a1.o ATL_strcopyL2L_U_aX.o ATL_strcopyL2U_N_a1.o \
       ATL_strcopyL2U_N_aX.o ATL_strcopyL2U_U_a1.o ATL_strcopyL2U_U_aX.o \
       ATL_strcopyU2L_N_a1.o ATL_strcopyU2L_N_aX.o ATL_strcopyU2L_U_a1.o \
       ATL_strcopyU2L_U_aX.o ATL_strcopyU2U_N_a1.o ATL_strcopyU2U_N_aX.o \
       ATL_strcopyU2U_U_a1.o ATL_strcopyU2U_U_aX.o ATL_strinvertLN.o \
       ATL_strinvertLU.o ATL_strinvertUN.o ATL_strinvertUU.o ATL_strmmLLNN.o \
       ATL_strmmLLNU.o ATL_strmmLLTN.o ATL_strmmLLTU.o ATL_strmmLUNN.o \
       ATL_strmmLUNU.o ATL_strmmLUTN.o ATL_strmmLUTU.o ATL_strmmRLNN.o \
       ATL_strmmRLNU.o ATL_strmmRLTN.o ATL_strmmRLTU.o ATL_strmmRUNN.o \
       ATL_strmmRUNU.o ATL_strmmRUTN.o ATL_strmmRUTU.o ATL_strputL_b0.o \
       ATL_strputL_b1.o ATL_strputL_bX.o ATL_strputU_b0.o ATL_strputU_b1.o \
       ATL_strputU_bX.o ATL_strsmKLLNN.o ATL_strsmKLLNU.o ATL_strsmKLUNN.o \
       ATL_strsmKLUNU.o ATL_strsmKL_rk4.o ATL_strsmKRLNN.o ATL_strsmKRLNU.o \
       ATL_strsmKRUNN.o ATL_strsmKRUNU.o ATL_strsmKR_rk4.o ATL_strsmLLNN.o \
       ATL_strsmLLNU.o ATL_strsmLLTN.o ATL_strsmLLTU.o ATL_strsmLUNN.o \
       ATL_strsmLUNU.o ATL_strsmLUTN.o ATL_strsmLUTU.o ATL_strsmRLNN.o \
       ATL_strsmRLNU.o ATL_strsmRLTN.o ATL_strsmRLTU.o ATL_strsmRUNN.o \
       ATL_strsmRUNU.o ATL_strsmRUTN.o ATL_strsmRUTU.o

dobj = ATL_dsycopyL_a1.o ATL_dsycopyL_aX.o ATL_dsycopyU_a1.o \
       ATL_dsycopyU_aX.o ATL_dsymmLL.o ATL_dsymmLU.o ATL_dsymmRL.o \
       ATL_dsymmRU.o ATL_dsyr2kLN.o ATL_dsyr2kLT.o ATL_dsyr2kUN.o \
       ATL_dsyr2kUT.o ATL_dsyr2k_putL_b0.o ATL_dsyr2k_putL_b1.o \
       ATL_dsyr2k_putL_bX.o ATL_dsyr2k_putU_b0.o ATL_dsyr2k_putU_b1.o \
       ATL_dsyr2k_putU_bX.o ATL_dsyrkLN.o ATL_dsyrkLT.o ATL_dsyrkUN.o \
       ATL_dsyrkUT.o ATL_dtrcopyL2L_N_a1.o ATL_dtrcopyL2L_N_aX.o \
       ATL_dtrcopyL2L_U_a1.o ATL_dtrcopyL2L_U_aX.o ATL_dtrcopyL2U_N_a1.o \
       ATL_dtrcopyL2U_N_aX.o ATL_dtrcopyL2U_U_a1.o ATL_dtrcopyL2U_U_aX.o \
       ATL_dtrcopyU2L_N_a1.o ATL_dtrcopyU2L_N_aX.o ATL_dtrcopyU2L_U_a1.o \
       ATL_dtrcopyU2L_U_aX.o ATL_dtrcopyU2U_N_a1.o ATL_dtrcopyU2U_N_aX.o \
       ATL_dtrcopyU2U_U_a1.o ATL_dtrcopyU2U_U_aX.o ATL_dtrinvertLN.o \
       ATL_dtrinvertLU.o ATL_dtrinvertUN.o ATL_dtrinvertUU.o ATL_dtrmmLLNN.o \
       ATL_dtrmmLLNU.o ATL_dtrmmLLTN.o ATL_dtrmmLLTU.o ATL_dtrmmLUNN.o \
       ATL_dtrmmLUNU.o ATL_dtrmmLUTN.o ATL_dtrmmLUTU.o ATL_dtrmmRLNN.o \
       ATL_dtrmmRLNU.o ATL_dtrmmRLTN.o ATL_dtrmmRLTU.o ATL_dtrmmRUNN.o \
       ATL_dtrmmRUNU.o ATL_dtrmmRUTN.o ATL_dtrmmRUTU.o ATL_dtrputL_b0.o \
       ATL_dtrputL_b1.o ATL_dtrputL_bX.o ATL_dtrputU_b0.o ATL_dtrputU_b1.o \
       ATL_dtrputU_bX.o ATL_dtrsmKLLNN.o ATL_dtrsmKLLNU.o ATL_dtrsmKLUNN.o \
       ATL_dtrsmKLUNU.o ATL_dtrsmKL_rk4.o ATL_dtrsmKRLNN.o ATL_dtrsmKRLNU.o \
       ATL_dtrsmKRUNN.o ATL_dtrsmKRUNU.o ATL_dtrsmKR_rk4.o ATL_dtrsmLLNN.o \
       ATL_dtrsmLLNU.o ATL_dtrsmLLTN.o ATL_dtrsmLLTU.o ATL_dtrsmLUNN.o \
       ATL_dtrsmLUNU.o ATL_dtrsmLUTN.o ATL_dtrsmLUTU.o ATL_dtrsmRLNN.o \
       ATL_dtrsmRLNU.o ATL_dtrsmRLTN.o ATL_dtrsmRLTU.o ATL_dtrsmRUNN.o \
       ATL_dtrsmRUNU.o ATL_dtrsmRUTN.o ATL_dtrsmRUTU.o

cobj = ATL_cCtrsmKL.o ATL_cCtrsmKR.o ATL_cctrsmKL_rk2.o ATL_cctrsmKR_rk2.o \
       ATL_checopyL.o ATL_checopyU.o ATL_chemmLL.o ATL_chemmLU.o \
       ATL_chemmRL.o ATL_chemmRU.o ATL_cheputL_b0.o ATL_cheputL_b1.o \
       ATL_cheputL_bXi0.o ATL_cheputU_b0.o ATL_cheputU_b1.o \
       ATL_cheputU_bXi0.o ATL_cher2kLN.o ATL_cher2kLT.o ATL_cher2kUN.o \
       ATL_cher2kUT.o ATL_cher2k_putL_b0.o ATL_cher2k_putL_b1.o \
       ATL_cher2k_putL_bXi0.o ATL_cher2k_putU_b0.o ATL_cher2k_putU_b1.o \
       ATL_cher2k_putU_bXi0.o ATL_cherkLN.o ATL_cherkLT.o ATL_cherkUN.o \
       ATL_cherkUT.o ATL_csycopyL.o ATL_csycopyU.o ATL_csymmLL.o \
       ATL_csymmLU.o ATL_csymmRL.o ATL_csymmRU.o ATL_csyr2kLN.o \
       ATL_csyr2kLT.o ATL_csyr2kUN.o ATL_csyr2kUT.o ATL_csyr2k_putL_b0.o \
       ATL_csyr2k_putL_b1.o ATL_csyr2k_putL_bX.o ATL_csyr2k_putL_bXi0.o \
       ATL_csyr2k_putL_bn1.o ATL_csyr2k_putU_b0.o ATL_csyr2k_putU_b1.o \
       ATL_csyr2k_putU_bX.o ATL_csyr2k_putU_bXi0.o ATL_csyr2k_putU_bn1.o \
       ATL_csyrkLN.o ATL_csyrkLT.o ATL_csyrkUN.o ATL_csyrkUT.o \
       ATL_ctrcopyL2L_N.o ATL_ctrcopyL2L_U.o ATL_ctrcopyL2Lc_N.o \
       ATL_ctrcopyL2Lc_U.o ATL_ctrcopyL2U_N.o ATL_ctrcopyL2U_U.o \
       ATL_ctrcopyL2Uc_N.o ATL_ctrcopyL2Uc_U.o ATL_ctrcopyU2L_N.o \
       ATL_ctrcopyU2L_U.o ATL_ctrcopyU2Lc_N.o ATL_ctrcopyU2Lc_U.o \
       ATL_ctrcopyU2U_N.o ATL_ctrcopyU2U_U.o ATL_ctrcopyU2Uc_N.o \
       ATL_ctrcopyU2Uc_U.o ATL_ctrinvertLN.o ATL_ctrinvertLU.o \
       ATL_ctrinvertUN.o ATL_ctrinvertUU.o ATL_ctrmmLLCN.o ATL_ctrmmLLCU.o \
       ATL_ctrmmLLNN.o ATL_ctrmmLLNU.o ATL_ctrmmLLTN.o ATL_ctrmmLLTU.o \
       ATL_ctrmmLUCN.o ATL_ctrmmLUCU.o ATL_ctrmmLUNN.o ATL_ctrmmLUNU.o \
       ATL_ctrmmLUTN.o ATL_ctrmmLUTU.o ATL_ctrmmRLCN.o ATL_ctrmmRLCU.o \
       ATL_ctrmmRLNN.o ATL_ctrmmRLNU.o ATL_ctrmmRLTN.o ATL_ctrmmRLTU.o \
       ATL_ctrmmRUCN.o ATL_ctrmmRUCU.o ATL_ctrmmRUNN.o ATL_ctrmmRUNU.o \
       ATL_ctrmmRUTN.o ATL_ctrmmRUTU.o ATL_ctrmv_scalLNN_aX.o \
       ATL_ctrmv_scalLNU_an1.o ATL_ctrmv_scalUNN_aX.o ATL_ctrmv_scalUNU_an1.o \
       ATL_ctrputL_b0.o ATL_ctrputL_b1.o ATL_ctrputL_bX.o ATL_ctrputL_bXi0.o \
       ATL_ctrputL_bn1.o ATL_ctrputU_b0.o ATL_ctrputU_b1.o ATL_ctrputU_bX.o \
       ATL_ctrputU_bXi0.o ATL_ctrputU_bn1.o ATL_ctrsmLLCN.o ATL_ctrsmLLCU.o \
       ATL_ctrsmLLNN.o ATL_ctrsmLLNU.o ATL_ctrsmLLTN.o ATL_ctrsmLLTU.o \
       ATL_ctrsmLUCN.o ATL_ctrsmLUCU.o ATL_ctrsmLUNN.o ATL_ctrsmLUNU.o \
       ATL_ctrsmLUTN.o ATL_ctrsmLUTU.o ATL_ctrsmRLCN.o ATL_ctrsmRLCU.o \
       ATL_ctrsmRLNN.o ATL_ctrsmRLNU.o ATL_ctrsmRLTN.o ATL_ctrsmRLTU.o \
       ATL_ctrsmRUCN.o ATL_ctrsmRUCU.o ATL_ctrsmRUNN.o ATL_ctrsmRUNU.o \
       ATL_ctrsmRUTN.o ATL_ctrsmRUTU.o

zobj = ATL_zCtrsmKL.o ATL_zCtrsmKR.o ATL_zctrsmKL_rk2.o ATL_zctrsmKR_rk2.o \
       ATL_zhecopyL.o ATL_zhecopyU.o ATL_zhemmLL.o ATL_zhemmLU.o \
       ATL_zhemmRL.o ATL_zhemmRU.o ATL_zheputL_b0.o ATL_zheputL_b1.o \
       ATL_zheputL_bXi0.o ATL_zheputU_b0.o ATL_zheputU_b1.o \
       ATL_zheputU_bXi0.o ATL_zher2kLN.o ATL_zher2kLT.o ATL_zher2kUN.o \
       ATL_zher2kUT.o ATL_zher2k_putL_b0.o ATL_zher2k_putL_b1.o \
       ATL_zher2k_putL_bXi0.o ATL_zher2k_putU_b0.o ATL_zher2k_putU_b1.o \
       ATL_zher2k_putU_bXi0.o ATL_zherkLN.o ATL_zherkLT.o ATL_zherkUN.o \
       ATL_zherkUT.o ATL_zsycopyL.o ATL_zsycopyU.o ATL_zsymmLL.o \
       ATL_zsymmLU.o ATL_zsymmRL.o ATL_zsymmRU.o ATL_zsyr2kLN.o \
       ATL_zsyr2kLT.o ATL_zsyr2kUN.o ATL_zsyr2kUT.o ATL_zsyr2k_putL_b0.o \
       ATL_zsyr2k_putL_b1.o ATL_zsyr2k_putL_bX.o ATL_zsyr2k_putL_bXi0.o \
       ATL_zsyr2k_putL_bn1.o ATL_zsyr2k_putU_b0.o ATL_zsyr2k_putU_b1.o \
       ATL_zsyr2k_putU_bX.o ATL_zsyr2k_putU_bXi0.o ATL_zsyr2k_putU_bn1.o \
       ATL_zsyrkLN.o ATL_zsyrkLT.o ATL_zsyrkUN.o ATL_zsyrkUT.o \
       ATL_ztrcopyL2L_N.o ATL_ztrcopyL2L_U.o ATL_ztrcopyL2Lc_N.o \
       ATL_ztrcopyL2Lc_U.o ATL_ztrcopyL2U_N.o ATL_ztrcopyL2U_U.o \
       ATL_ztrcopyL2Uc_N.o ATL_ztrcopyL2Uc_U.o ATL_ztrcopyU2L_N.o \
       ATL_ztrcopyU2L_U.o ATL_ztrcopyU2Lc_N.o ATL_ztrcopyU2Lc_U.o \
       ATL_ztrcopyU2U_N.o ATL_ztrcopyU2U_U.o ATL_ztrcopyU2Uc_N.o \
       ATL_ztrcopyU2Uc_U.o ATL_ztrinvertLN.o ATL_ztrinvertLU.o \
       ATL_ztrinvertUN.o ATL_ztrinvertUU.o ATL_ztrmmLLCN.o ATL_ztrmmLLCU.o \
       ATL_ztrmmLLNN.o ATL_ztrmmLLNU.o ATL_ztrmmLLTN.o ATL_ztrmmLLTU.o \
       ATL_ztrmmLUCN.o ATL_ztrmmLUCU.o ATL_ztrmmLUNN.o ATL_ztrmmLUNU.o \
       ATL_ztrmmLUTN.o ATL_ztrmmLUTU.o ATL_ztrmmRLCN.o ATL_ztrmmRLCU.o \
       ATL_ztrmmRLNN.o ATL_ztrmmRLNU.o ATL_ztrmmRLTN.o ATL_ztrmmRLTU.o \
       ATL_ztrmmRUCN.o ATL_ztrmmRUCU.o ATL_ztrmmRUNN.o ATL_ztrmmRUNU.o \
       ATL_ztrmmRUTN.o ATL_ztrmmRUTU.o ATL_ztrmv_scalLNN_aX.o \
       ATL_ztrmv_scalLNU_an1.o ATL_ztrmv_scalUNN_aX.o ATL_ztrmv_scalUNU_an1.o \
       ATL_ztrputL_b0.o ATL_ztrputL_b1.o ATL_ztrputL_bX.o ATL_ztrputL_bXi0.o \
       ATL_ztrputL_bn1.o ATL_ztrputU_b0.o ATL_ztrputU_b1.o ATL_ztrputU_bX.o \
       ATL_ztrputU_bXi0.o ATL_ztrputU_bn1.o ATL_ztrsmLLCN.o ATL_ztrsmLLCU.o \
       ATL_ztrsmLLNN.o ATL_ztrsmLLNU.o ATL_ztrsmLLTN.o ATL_ztrsmLLTU.o \
       ATL_ztrsmLUCN.o ATL_ztrsmLUCU.o ATL_ztrsmLUNN.o ATL_ztrsmLUNU.o \
       ATL_ztrsmLUTN.o ATL_ztrsmLUTU.o ATL_ztrsmRLCN.o ATL_ztrsmRLCU.o \
       ATL_ztrsmRLNN.o ATL_ztrsmRLNU.o ATL_ztrsmRLTN.o ATL_ztrsmRLTU.o \
       ATL_ztrsmRUCN.o ATL_ztrsmRUCU.o ATL_ztrsmRUNN.o ATL_ztrsmRUNU.o \
       ATL_ztrsmRUTN.o ATL_ztrsmRUTU.o


all : lib
lvl3 : lib
lib : dlib slib clib zlib
double    : dlib
single    : slib
complex   : clib
complex16 : zlib
slvl3 : slib
dlvl3 : dlib
clvl3 : clib
zlvl3 : zlib

dep = $(INCSdir)/atlas_kern3.h

sl3ref :
	cd $(BLDdir)/src/blas/reference/level3 ; $(MAKE) slib
slib : 
	$(PMAKE) slib.grd
	$(MAKE) sl3ref
slib.grd: $(sobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(sobj)
	$(RANLIB) $(ATLASlib)
	touch slib.grd

dl3ref :
	cd $(BLDdir)/src/blas/reference/level3 ; $(MAKE) dlib
dlib : 
	$(PMAKE) dlib.grd
	$(MAKE) dl3ref
dlib.grd: $(dobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(dobj)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd

cl3ref :
	cd $(BLDdir)/src/blas/reference/level3 ; $(MAKE) clib
clib : 
	$(PMAKE) clib.grd
	$(MAKE) cl3ref
clib.grd: $(cobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(cobj)
	$(RANLIB) $(ATLASlib)
	touch clib.grd

zl3ref :
	cd $(BLDdir)/src/blas/reference/level3 ; $(MAKE) zlib
zlib : 
	$(PMAKE) zlib.grd
	$(MAKE) zl3ref
zlib.grd: $(zobj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(zobj)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd

ATL_ctrmv_scalLNN_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ -DALPHAN1 -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ctrmv_scalLNU_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ -DALPHAN1 -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ctrmv_scalLNN_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ -DALPHAX -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ctrmv_scalLNU_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ -DALPHAX -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_chemmRL.o : $(mySRCdir)/ATL_hemmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_hemmR.c
ATL_chemmLL.o : $(mySRCdir)/ATL_hemmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_hemmL.c
ATL_cher2kLN.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_her2k.c
ATL_cher2kLT.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ -DTranspose_ $(mySRCdir)/ATL_her2k.c
ATL_cherkLT.o : $(mySRCdir)/ATL_herk_T.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_herk_T.c
ATL_cherkLN.o : $(mySRCdir)/ATL_herk_N.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_herk_N.c
ATL_checopyL.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DHerm_ $(mySRCdir)/ATL_sycopyL.c
ATL_cheputL_bXi0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_trputL.c
ATL_cher2k_putL_bXi0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putL.c
ATL_cheputL_b0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_trputL.c
ATL_cher2k_putL_b0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putL.c
ATL_cheputL_b1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_trputL.c
ATL_cher2k_putL_b1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putL.c
ATL_ctrmv_scalUNN_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ -DALPHAN1 -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ctrmv_scalUNU_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ -DALPHAN1 -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ctrmv_scalUNN_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ -DALPHAX -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ctrmv_scalUNU_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ -DALPHAX -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_chemmRU.o : $(mySRCdir)/ATL_hemmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_hemmR.c
ATL_chemmLU.o : $(mySRCdir)/ATL_hemmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_hemmL.c
ATL_cher2kUN.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_her2k.c
ATL_cher2kUT.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ -DTranspose_ $(mySRCdir)/ATL_her2k.c
ATL_cherkUT.o : $(mySRCdir)/ATL_herk_T.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_herk_T.c
ATL_cherkUN.o : $(mySRCdir)/ATL_herk_N.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_herk_N.c
ATL_checopyU.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DHerm_ $(mySRCdir)/ATL_sycopyU.c
ATL_cheputU_bXi0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_trputU.c
ATL_cher2k_putU_bXi0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putU.c
ATL_cheputU_b0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_trputU.c
ATL_cher2k_putU_b0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putU.c
ATL_cheputU_b1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_trputU.c
ATL_cher2k_putU_b1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putU.c
ATL_cCtrsmKL.o : $(mySRCdir)/ATL_CtrsmK.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLeft_ $(mySRCdir)/ATL_CtrsmK.c
ATL_cCtrsmKR.o : $(mySRCdir)/ATL_CtrsmK.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DRight_ $(mySRCdir)/ATL_CtrsmK.c
ATL_ztrmv_scalLNN_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ -DALPHAN1 -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ztrmv_scalLNU_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ -DALPHAN1 -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ztrmv_scalLNN_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ -DALPHAX -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ztrmv_scalLNU_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ -DALPHAX -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_zhemmRL.o : $(mySRCdir)/ATL_hemmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_hemmR.c
ATL_zhemmLL.o : $(mySRCdir)/ATL_hemmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_hemmL.c
ATL_zher2kLN.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_her2k.c
ATL_zher2kLT.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ -DTranspose_ $(mySRCdir)/ATL_her2k.c
ATL_zherkLT.o : $(mySRCdir)/ATL_herk_T.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_herk_T.c
ATL_zherkLN.o : $(mySRCdir)/ATL_herk_N.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_herk_N.c
ATL_zhecopyL.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DHerm_ $(mySRCdir)/ATL_sycopyL.c
ATL_zheputL_bXi0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_trputL.c
ATL_zher2k_putL_bXi0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putL.c
ATL_zheputL_b0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_trputL.c
ATL_zher2k_putL_b0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putL.c
ATL_zheputL_b1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_trputL.c
ATL_zher2k_putL_b1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putL.c
ATL_ztrmv_scalUNN_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ -DALPHAN1 -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ztrmv_scalUNU_an1.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ -DALPHAN1 -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ztrmv_scalUNN_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ -DALPHAX -DNonUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_ztrmv_scalUNU_aX.o : $(mySRCdir)/ATL_trmv_scal.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ -DALPHAX -DUnitDiag_\
                 $(mySRCdir)/ATL_trmv_scal.c
ATL_zhemmRU.o : $(mySRCdir)/ATL_hemmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_hemmR.c
ATL_zhemmLU.o : $(mySRCdir)/ATL_hemmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_hemmL.c
ATL_zher2kUN.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_her2k.c
ATL_zher2kUT.o : $(mySRCdir)/ATL_her2k.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ -DTranspose_ $(mySRCdir)/ATL_her2k.c
ATL_zherkUT.o : $(mySRCdir)/ATL_herk_T.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_herk_T.c
ATL_zherkUN.o : $(mySRCdir)/ATL_herk_N.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_herk_N.c
ATL_zhecopyU.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DHerm_ $(mySRCdir)/ATL_sycopyU.c
ATL_zheputU_bXi0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_trputU.c
ATL_zher2k_putU_bXi0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putU.c
ATL_zheputU_b0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_trputU.c
ATL_zher2k_putU_b0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putU.c
ATL_zheputU_b1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_trputU.c
ATL_zher2k_putU_b1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 -DHerm_ \
                    $(mySRCdir)/ATL_syr2k_putU.c
ATL_zCtrsmKL.o : $(mySRCdir)/ATL_CtrsmK.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLeft_ $(mySRCdir)/ATL_CtrsmK.c
ATL_zCtrsmKR.o : $(mySRCdir)/ATL_CtrsmK.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DRight_ $(mySRCdir)/ATL_CtrsmK.c
ATL_strsmKR_rk4.o : $(mySRCdir)/ATL_trsmKR_rk4.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLeft_ $(mySRCdir)/ATL_trsmKR_rk4.c
ATL_strsmKL_rk4.o : $(mySRCdir)/ATL_trsmKL_rk4.c
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLeft_ $(mySRCdir)/ATL_trsmKL_rk4.c
ATL_ssycopyU_aX.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX $(mySRCdir)/ATL_sycopyU.c
ATL_ssycopyL_aX.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX $(mySRCdir)/ATL_sycopyL.c

ATL_strcopyL2U_N_aX.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_strcopyL2L_N_aX.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_strcopyU2U_N_aX.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_strcopyU2L_N_aX.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c
ATL_strcopyL2U_U_aX.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_strcopyL2L_U_aX.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_strcopyU2U_U_aX.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_strcopyU2L_U_aX.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c
ATL_ssycopyU_a1.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 $(mySRCdir)/ATL_sycopyU.c
ATL_ssycopyL_a1.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 $(mySRCdir)/ATL_sycopyL.c

ATL_strcopyL2U_N_a1.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_strcopyL2L_N_a1.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_strcopyU2U_N_a1.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_strcopyU2L_N_a1.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c
ATL_strcopyL2U_U_a1.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_strcopyL2L_U_a1.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_strcopyU2U_U_a1.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_strcopyU2L_U_a1.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c

ATL_ssymmRL.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ $(mySRCdir)/ATL_symmR.c
ATL_ssymmLL.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ $(mySRCdir)/ATL_symmL.c

ATL_ssyrkLN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ $(mySRCdir)/ATL_syrk_N.c
ATL_ssyrkLT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ $(mySRCdir)/ATL_syrk_T.c

ATL_ssyr2kLN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ $(mySRCdir)/ATL_syr2k_N.c
ATL_ssyr2kLT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ $(mySRCdir)/ATL_syr2k_T.c

ATL_strsmKRLNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_strsmKLLNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_strsmRLTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRLTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLLTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLLTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strsmRLNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRLNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLLNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLLNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strinvertLN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_strsmKRLNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_strsmKLLNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_strsmRLTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRLTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLLTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLLTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strsmRLNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRLNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLLNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLLNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strinvertLU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DLower_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_ssymmRU.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ $(mySRCdir)/ATL_symmR.c
ATL_ssymmLU.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ $(mySRCdir)/ATL_symmL.c

ATL_ssyrkUN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ $(mySRCdir)/ATL_syrk_N.c
ATL_ssyrkUT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ $(mySRCdir)/ATL_syrk_T.c

ATL_ssyr2kUN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ $(mySRCdir)/ATL_syr2k_N.c
ATL_ssyr2kUT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ $(mySRCdir)/ATL_syr2k_T.c

ATL_strsmKRUNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_strsmKLUNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_strsmRUTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRUTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLUTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLUTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strsmRUNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRUNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLUNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLUNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strinvertUN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_strsmKRUNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_strsmKLUNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_strsmRUTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRUTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLUTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLUTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strsmRUNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_strmmRUNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_strmmLUNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_strsmLUNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_strsmXover.h $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_strinvertUU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DUpper_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
   
ATL_ssyr2k_putU_bX.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETAX $(mySRCdir)/ATL_syr2k_putU.c
ATL_ssyr2k_putL_bX.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETAX $(mySRCdir)/ATL_syr2k_putL.c
ATL_strputU_bX.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETAX $(mySRCdir)/ATL_trputU.c
ATL_strputL_bX.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETAX $(mySRCdir)/ATL_trputL.c
ATL_ssyr2k_putU_b0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA0 $(mySRCdir)/ATL_syr2k_putU.c
ATL_ssyr2k_putL_b0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA0 $(mySRCdir)/ATL_syr2k_putL.c
ATL_strputU_b0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA0 $(mySRCdir)/ATL_trputU.c
ATL_strputL_b0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA0 $(mySRCdir)/ATL_trputL.c
ATL_ssyr2k_putU_b1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA1 $(mySRCdir)/ATL_syr2k_putU.c
ATL_ssyr2k_putL_b1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA1 $(mySRCdir)/ATL_syr2k_putL.c
ATL_strputU_b1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA1 $(mySRCdir)/ATL_trputU.c
ATL_strputL_b1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(SKC) -o $@ -c $(sKCFLAGS) -DSREAL -DBETA1 $(mySRCdir)/ATL_trputL.c

$(INCAdir)/atlas_strsmXover.h :
	touch $(INCAdir)/atlas_strsmXover.h

ATL_dtrsmKR_rk4.o : $(mySRCdir)/ATL_trsmKR_rk4.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLeft_ $(mySRCdir)/ATL_trsmKR_rk4.c
ATL_dtrsmKL_rk4.o : $(mySRCdir)/ATL_trsmKL_rk4.c
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLeft_ $(mySRCdir)/ATL_trsmKL_rk4.c
ATL_dsycopyU_aX.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX $(mySRCdir)/ATL_sycopyU.c
ATL_dsycopyL_aX.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX $(mySRCdir)/ATL_sycopyL.c

ATL_dtrcopyL2U_N_aX.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_dtrcopyL2L_N_aX.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_dtrcopyU2U_N_aX.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_dtrcopyU2L_N_aX.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c
ATL_dtrcopyL2U_U_aX.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_dtrcopyL2L_U_aX.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_dtrcopyU2U_U_aX.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_dtrcopyU2L_U_aX.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHAX -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c
ATL_dsycopyU_a1.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 $(mySRCdir)/ATL_sycopyU.c
ATL_dsycopyL_a1.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 $(mySRCdir)/ATL_sycopyL.c

ATL_dtrcopyL2U_N_a1.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_dtrcopyL2L_N_a1.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_dtrcopyU2U_N_a1.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_dtrcopyU2L_N_a1.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DNonUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c
ATL_dtrcopyL2U_U_a1.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2U.c
ATL_dtrcopyL2L_U_a1.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyL2L.c
ATL_dtrcopyU2U_U_a1.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2U.c
ATL_dtrcopyU2L_U_a1.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DALPHA1 -DUnitDiag_ \
	         $(mySRCdir)/ATL_trcopyU2L.c

ATL_dsymmRL.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ $(mySRCdir)/ATL_symmR.c
ATL_dsymmLL.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ $(mySRCdir)/ATL_symmL.c

ATL_dsyrkLN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ $(mySRCdir)/ATL_syrk_N.c
ATL_dsyrkLT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ $(mySRCdir)/ATL_syrk_T.c

ATL_dsyr2kLN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ $(mySRCdir)/ATL_syr2k_N.c
ATL_dsyr2kLT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ $(mySRCdir)/ATL_syr2k_T.c

ATL_dtrsmKRLNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_dtrsmKLLNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_dtrsmRLTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRLTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLLTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLLTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrsmRLNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRLNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLLNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLLNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrinvertLN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_dtrsmKRLNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_dtrsmKLLNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_dtrsmRLTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRLTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLLTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLLTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrsmRLNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRLNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLLNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLLNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrinvertLU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DLower_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_dsymmRU.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ $(mySRCdir)/ATL_symmR.c
ATL_dsymmLU.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ $(mySRCdir)/ATL_symmL.c

ATL_dsyrkUN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ $(mySRCdir)/ATL_syrk_N.c
ATL_dsyrkUT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ $(mySRCdir)/ATL_syrk_T.c

ATL_dsyr2kUN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ $(mySRCdir)/ATL_syr2k_N.c
ATL_dsyr2kUT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ $(mySRCdir)/ATL_syr2k_T.c

ATL_dtrsmKRUNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_dtrsmKLUNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_dtrsmRUTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRUTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLUTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLUTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrsmRUNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRUNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLUNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLUNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrinvertUN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_dtrsmKRUNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_dtrsmKLUNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_dtrsmRUTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRUTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLUTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLUTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrsmRUNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_dtrmmRUNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_dtrmmLUNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_dtrsmLUNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_dtrsmXover.h $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_dtrinvertUU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DUpper_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
   
ATL_dsyr2k_putU_bX.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETAX $(mySRCdir)/ATL_syr2k_putU.c
ATL_dsyr2k_putL_bX.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETAX $(mySRCdir)/ATL_syr2k_putL.c
ATL_dtrputU_bX.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETAX $(mySRCdir)/ATL_trputU.c
ATL_dtrputL_bX.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETAX $(mySRCdir)/ATL_trputL.c
ATL_dsyr2k_putU_b0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA0 $(mySRCdir)/ATL_syr2k_putU.c
ATL_dsyr2k_putL_b0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA0 $(mySRCdir)/ATL_syr2k_putL.c
ATL_dtrputU_b0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA0 $(mySRCdir)/ATL_trputU.c
ATL_dtrputL_b0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA0 $(mySRCdir)/ATL_trputL.c
ATL_dsyr2k_putU_b1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA1 $(mySRCdir)/ATL_syr2k_putU.c
ATL_dsyr2k_putL_b1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA1 $(mySRCdir)/ATL_syr2k_putL.c
ATL_dtrputU_b1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA1 $(mySRCdir)/ATL_trputU.c
ATL_dtrputL_b1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(DKC) -o $@ -c $(dKCFLAGS) -DDREAL -DBETA1 $(mySRCdir)/ATL_trputL.c

$(INCAdir)/atlas_dtrsmXover.h :
	touch $(INCAdir)/atlas_dtrsmXover.h

ATL_cctrsmKR_rk2.o : $(mySRCdir)/ATL_ctrsmKR_rk2.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLeft_ $(mySRCdir)/ATL_ctrsmKR_rk2.c
ATL_cctrsmKL_rk2.o : $(mySRCdir)/ATL_ctrsmKL_rk2.c
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLeft_ $(mySRCdir)/ATL_ctrsmKL_rk2.c
ATL_csycopyU.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_sycopyU.c
ATL_csycopyL.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX $(mySRCdir)/ATL_sycopyL.c

ATL_ctrcopyL2U_N.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyL2U.c
ATL_ctrcopyL2Uc_N.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2U.c
ATL_ctrcopyL2L_N.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyL2L.c
ATL_ctrcopyL2Lc_N.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2L.c
ATL_ctrcopyU2U_N.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyU2U.c
ATL_ctrcopyU2Uc_N.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2U.c
ATL_ctrcopyU2L_N.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyU2L.c
ATL_ctrcopyU2Lc_N.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2L.c
ATL_ctrcopyL2U_U.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyL2U.c
ATL_ctrcopyL2Uc_U.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2U.c
ATL_ctrcopyL2L_U.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyL2L.c
ATL_ctrcopyL2Lc_U.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2L.c
ATL_ctrcopyU2U_U.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyU2U.c
ATL_ctrcopyU2Uc_U.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2U.c
ATL_ctrcopyU2L_U.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyU2L.c
ATL_ctrcopyU2Lc_U.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2L.c

ATL_csymmRL.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_symmR.c
ATL_csymmLL.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_symmL.c

ATL_csyrkLN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_syrk_N.c
ATL_csyrkLT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_syrk_T.c

ATL_csyr2kLN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_syr2k_N.c
ATL_csyr2kLT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ $(mySRCdir)/ATL_syr2k_T.c

ATL_ctrsmKRLNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ctrsmKLLNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ctrsmRLTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRLTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLLTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLLTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRLNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRLNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLLNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLLNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRLCN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRLCN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLLCN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLLCN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrinvertLN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_ctrsmKRLNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ctrsmKLLNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ctrsmRLTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRLTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLLTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLLTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRLNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRLNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLLNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLLNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRLCU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRLCU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLLCU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLLCU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrinvertLU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DLower_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_csymmRU.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_symmR.c
ATL_csymmLU.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_symmL.c

ATL_csyrkUN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_syrk_N.c
ATL_csyrkUT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_syrk_T.c

ATL_csyr2kUN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_syr2k_N.c
ATL_csyr2kUT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ $(mySRCdir)/ATL_syr2k_T.c

ATL_ctrsmKRUNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ctrsmKLUNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ctrsmRUTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRUTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLUTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLUTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRUNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRUNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLUNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLUNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRUCN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRUCN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLUCN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLUCN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrinvertUN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_ctrsmKRUNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ctrsmKLUNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ctrsmRUTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRUTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLUTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLUTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRUNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRUNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLUNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLUNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrsmRUCU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ctrmmRUCU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ctrmmLUCU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ctrsmLUCU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ctrsmXover.h $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ctrinvertUU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DUpper_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
   
ATL_csyr2k_putU_bX.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAX $(mySRCdir)/ATL_syr2k_putU.c
ATL_csyr2k_putL_bX.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAX $(mySRCdir)/ATL_syr2k_putL.c
ATL_ctrputU_bX.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAX $(mySRCdir)/ATL_trputU.c
ATL_ctrputL_bX.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAX $(mySRCdir)/ATL_trputL.c
ATL_csyr2k_putU_b0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 $(mySRCdir)/ATL_syr2k_putU.c
ATL_csyr2k_putL_b0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 $(mySRCdir)/ATL_syr2k_putL.c
ATL_ctrputU_b0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 $(mySRCdir)/ATL_trputU.c
ATL_ctrputL_b0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA0 $(mySRCdir)/ATL_trputL.c
ATL_csyr2k_putU_b1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 $(mySRCdir)/ATL_syr2k_putU.c
ATL_csyr2k_putL_b1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 $(mySRCdir)/ATL_syr2k_putL.c
ATL_ctrputU_b1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 $(mySRCdir)/ATL_trputU.c
ATL_ctrputL_b1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETA1 $(mySRCdir)/ATL_trputL.c
ATL_csyr2k_putU_bn1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAN1 $(mySRCdir)/ATL_syr2k_putU.c
ATL_csyr2k_putU_bXi0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 $(mySRCdir)/ATL_syr2k_putU.c
ATL_csyr2k_putL_bn1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAN1 $(mySRCdir)/ATL_syr2k_putL.c
ATL_csyr2k_putL_bXi0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 $(mySRCdir)/ATL_syr2k_putL.c
ATL_ctrputU_bn1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAN1 $(mySRCdir)/ATL_trputU.c
ATL_ctrputU_bXi0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 $(mySRCdir)/ATL_trputU.c
ATL_ctrputL_bn1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAN1 $(mySRCdir)/ATL_trputL.c
ATL_ctrputL_bXi0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(CKC) -o $@ -c $(cKCFLAGS) -DSCPLX -DBETAXI0 $(mySRCdir)/ATL_trputL.c

$(INCAdir)/atlas_ctrsmXover.h :
	touch $(INCAdir)/atlas_ctrsmXover.h

ATL_zctrsmKR_rk2.o : $(mySRCdir)/ATL_ctrsmKR_rk2.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLeft_ $(mySRCdir)/ATL_ctrsmKR_rk2.c
ATL_zctrsmKL_rk2.o : $(mySRCdir)/ATL_ctrsmKL_rk2.c
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLeft_ $(mySRCdir)/ATL_ctrsmKL_rk2.c
ATL_zsycopyU.o : $(mySRCdir)/ATL_sycopyU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_sycopyU.c
ATL_zsycopyL.o : $(mySRCdir)/ATL_sycopyL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX $(mySRCdir)/ATL_sycopyL.c

ATL_ztrcopyL2U_N.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyL2U.c
ATL_ztrcopyL2Uc_N.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2U.c
ATL_ztrcopyL2L_N.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyL2L.c
ATL_ztrcopyL2Lc_N.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2L.c
ATL_ztrcopyU2U_N.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyU2U.c
ATL_ztrcopyU2Uc_N.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2U.c
ATL_ztrcopyU2L_N.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ $(mySRCdir)/ATL_trcopyU2L.c
ATL_ztrcopyU2Lc_N.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DNonUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2L.c
ATL_ztrcopyL2U_U.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyL2U.c
ATL_ztrcopyL2Uc_U.o : $(mySRCdir)/ATL_trcopyL2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2U.c
ATL_ztrcopyL2L_U.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyL2L.c
ATL_ztrcopyL2Lc_U.o : $(mySRCdir)/ATL_trcopyL2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyL2L.c
ATL_ztrcopyU2U_U.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyU2U.c
ATL_ztrcopyU2Uc_U.o : $(mySRCdir)/ATL_trcopyU2U.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2U.c
ATL_ztrcopyU2L_U.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ $(mySRCdir)/ATL_trcopyU2L.c
ATL_ztrcopyU2Lc_U.o : $(mySRCdir)/ATL_trcopyU2L.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUnitDiag_ -DConjTrans_ \
              $(mySRCdir)/ATL_trcopyU2L.c

ATL_zsymmRL.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_symmR.c
ATL_zsymmLL.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_symmL.c

ATL_zsyrkLN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_syrk_N.c
ATL_zsyrkLT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_syrk_T.c

ATL_zsyr2kLN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_syr2k_N.c
ATL_zsyr2kLT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ $(mySRCdir)/ATL_syr2k_T.c

ATL_ztrsmKRLNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ztrsmKLLNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ztrsmRLTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRLTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLLTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLLTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRLNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRLNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLLNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLLNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRLCN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRLCN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLLCN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLLCN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrinvertLN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_ztrsmKRLNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ztrsmKLLNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ztrsmRLTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRLTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLLTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLLTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRLNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRLNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLLNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLLNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRLCU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRLCU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLLCU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLLCU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrinvertLU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DLower_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_zsymmRU.o : $(mySRCdir)/ATL_symmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_symmR.c
ATL_zsymmLU.o : $(mySRCdir)/ATL_symmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_symmL.c

ATL_zsyrkUN.o : $(mySRCdir)/ATL_syrk_N.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_syrk_N.c
ATL_zsyrkUT.o : $(mySRCdir)/ATL_syrk_T.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_syrk_T.c

ATL_zsyr2kUN.o : $(mySRCdir)/ATL_syr2k_N.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_syr2k_N.c
ATL_zsyr2kUT.o : $(mySRCdir)/ATL_syr2k_T.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ $(mySRCdir)/ATL_syr2k_T.c

ATL_ztrsmKRUNN.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNonUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ztrsmKLUNN.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNonUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ztrsmRUTN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRUTN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLUTN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLUTN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRUNN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRUNN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLUNN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLUNN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRUCN.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRUCN.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLUCN.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLUCN.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DNonUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrinvertUN.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNonUnitDiag_ $(mySRCdir)/ATL_trinvert.c
ATL_ztrsmKRUNU.o : $(mySRCdir)/ATL_trsmKR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DUnitDiag_ -DRight_ $(mySRCdir)/ATL_trsmKR.c
ATL_ztrsmKLUNU.o : $(mySRCdir)/ATL_trsmKL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DUnitDiag_ -DLeft_ $(mySRCdir)/ATL_trsmKL.c
ATL_ztrsmRUTU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRUTU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLUTU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLUTU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DTranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRUNU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRUNU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLUNU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLUNU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DNotranspose_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrsmRUCU.o : $(mySRCdir)/ATL_trsmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmR.c
ATL_ztrmmRUCU.o : $(mySRCdir)/ATL_trmmR.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmR.c
ATL_ztrmmLUCU.o : $(mySRCdir)/ATL_trmmL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trmmL.c
ATL_ztrsmLUCU.o : $(mySRCdir)/ATL_trsmL.c $(INCAdir)/atlas_ztrsmXover.h $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DConjTrans_ -DUnitDiag_ $(mySRCdir)/ATL_trsmL.c
ATL_ztrinvertUU.o : $(mySRCdir)/ATL_trinvert.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DUpper_ \
	         -DUnitDiag_ $(mySRCdir)/ATL_trinvert.c
   
ATL_zsyr2k_putU_bX.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAX $(mySRCdir)/ATL_syr2k_putU.c
ATL_zsyr2k_putL_bX.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAX $(mySRCdir)/ATL_syr2k_putL.c
ATL_ztrputU_bX.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAX $(mySRCdir)/ATL_trputU.c
ATL_ztrputL_bX.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAX $(mySRCdir)/ATL_trputL.c
ATL_zsyr2k_putU_b0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 $(mySRCdir)/ATL_syr2k_putU.c
ATL_zsyr2k_putL_b0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 $(mySRCdir)/ATL_syr2k_putL.c
ATL_ztrputU_b0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 $(mySRCdir)/ATL_trputU.c
ATL_ztrputL_b0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA0 $(mySRCdir)/ATL_trputL.c
ATL_zsyr2k_putU_b1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 $(mySRCdir)/ATL_syr2k_putU.c
ATL_zsyr2k_putL_b1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 $(mySRCdir)/ATL_syr2k_putL.c
ATL_ztrputU_b1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 $(mySRCdir)/ATL_trputU.c
ATL_ztrputL_b1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETA1 $(mySRCdir)/ATL_trputL.c
ATL_zsyr2k_putU_bn1.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAN1 $(mySRCdir)/ATL_syr2k_putU.c
ATL_zsyr2k_putU_bXi0.o : $(mySRCdir)/ATL_syr2k_putU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 $(mySRCdir)/ATL_syr2k_putU.c
ATL_zsyr2k_putL_bn1.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAN1 $(mySRCdir)/ATL_syr2k_putL.c
ATL_zsyr2k_putL_bXi0.o : $(mySRCdir)/ATL_syr2k_putL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 $(mySRCdir)/ATL_syr2k_putL.c
ATL_ztrputU_bn1.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAN1 $(mySRCdir)/ATL_trputU.c
ATL_ztrputU_bXi0.o : $(mySRCdir)/ATL_trputU.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 $(mySRCdir)/ATL_trputU.c
ATL_ztrputL_bn1.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAN1 $(mySRCdir)/ATL_trputL.c
ATL_ztrputL_bXi0.o : $(mySRCdir)/ATL_trputL.c $(dep)
	$(ZKC) -o $@ -c $(zKCFLAGS) -DDCPLX -DBETAXI0 $(mySRCdir)/ATL_trputL.c

$(INCAdir)/atlas_ztrsmXover.h :
	touch $(INCAdir)/atlas_ztrsmXover.h


sclean :
	rm -f $(sobj) s*.grd
dclean :
	rm -f $(dobj) d*.grd
cclean :
	rm -f $(cobj) c*.grd
zclean :
	rm -f $(zobj) z*.grd
clean : sclean dclean cclean zclean

