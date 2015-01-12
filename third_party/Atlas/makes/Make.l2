########################################################################
#  
#  -- Automatically Tuned Linear Algebra Software (ATLAS)
#     (C) Copyright 2000 All Rights Reserved
#
########################################################################
#
#  -- Copyright notice and Licensing terms:
#
#  Redistribution  and  use in  source and binary forms, with or without
#  modification, are  permitted provided  that the following  conditions
#  are met:
#
#  1. Redistributions  of  source  code  must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce  the above copyright
#     notice,  this list of conditions, and the  following disclaimer in
#     the documentation and/or other materials provided with the distri-
#     bution.
#  3. The name of the University,  the ATLAS group,  or the names of its
#     contributors  may not be used to endorse or promote products deri-
#     ved from this software without specific written permission.
#
#  -- Disclaimer:
#
#  THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
#  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
#  CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#  TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
#  OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
#  RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
#  CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
#  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  
########################################################################
#
include Make.inc
mySRCdir = $(SRCdir)/src/blas/level2/
#
########################################################################
#
INC2dep      = \
   $(INCSdir)/atlas_misc.h    $(INCSdir)/atlas_aux.h        \
   $(INCSdir)/atlas_kernel2.h $(INCSdir)/atlas_lvl2.h       \
   $(INCSdir)/atlas_mv.h      $(INCSdir)/atlas_r1.h
#
## Object files ########################################################
#
ATL_slvl2obj = \
   ATL_L2AIsOverlapped.o ATL_sgbmv.o ATL_sgpmv.o ATL_sgpr.o ATL_ssbmv.o \
   ATL_sspmv.o ATL_sspr.o ATL_sspr2.o ATL_ssymv.o ATL_ssyr.o ATL_ssyr2.o \
   ATL_stbmv.o ATL_stbsv.o ATL_stpmv.o ATL_stpsv.o ATL_strmv.o ATL_strsv.o
#
ATL_dlvl2obj = \
   ATL_L2AIsOverlapped.o ATL_dgbmv.o ATL_dgpmv.o ATL_dgpr.o ATL_dsbmv.o \
   ATL_dspmv.o ATL_dspr.o ATL_dspr2.o ATL_dsymv.o ATL_dsyr.o ATL_dsyr2.o \
   ATL_dtbmv.o ATL_dtbsv.o ATL_dtpmv.o ATL_dtpsv.o ATL_dtrmv.o ATL_dtrsv.o
#
ATL_clvl2obj = \
   ATL_L2AIsOverlapped.o ATL_cgbmv.o ATL_cgpmv.o ATL_cgprc.o ATL_cgpru.o \
   ATL_chbmv.o ATL_chemv.o ATL_cher.o ATL_cher2.o ATL_chpmv.o ATL_chpr.o \
   ATL_chpr2.o ATL_ctbmv.o ATL_ctbsv.o ATL_ctpmv.o ATL_ctpsv.o ATL_ctrmv.o \
   ATL_ctrsv.o
#
ATL_zlvl2obj = \
   ATL_L2AIsOverlapped.o ATL_zgbmv.o ATL_zgpmv.o ATL_zgprc.o ATL_zgpru.o \
   ATL_zhbmv.o ATL_zhemv.o ATL_zher.o ATL_zher2.o ATL_zhpmv.o ATL_zhpr.o \
   ATL_zhpr2.o ATL_ztbmv.o ATL_ztbsv.o ATL_ztpmv.o ATL_ztpsv.o ATL_ztrmv.o \
   ATL_ztrsv.o
#
## Targets #############################################################
#
all   : lib
#
lvl2  : lib
#
lib   : dlib slib zlib clib
#
slvl2 : slib
dlvl2 : dlib
clvl2 : clib
zlvl2 : zlib
#
########################################################################
#
slib  : sblas2
dlib  : dblas2
clib  : cblas2
zlib  : zblas2
#
skern :
dkern :
ckern :
zkern :
#
sblas2 : sblas2.grd
dblas2 : dblas2.grd
cblas2 : cblas2.grd
zblas2 : zblas2.grd
#
sblas2.grd: $(ATL_slvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ATL_slvl2obj)
	$(RANLIB) $(ATLASlib)
	touch sblas2.grd
#
dblas2.grd: $(ATL_dlvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ATL_dlvl2obj)
	$(RANLIB) $(ATLASlib)
	touch dblas2.grd
#
cblas2.grd: $(ATL_clvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ATL_clvl2obj)
	$(RANLIB) $(ATLASlib)
	touch cblas2.grd
#
zblas2.grd: $(ATL_zlvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ATL_zlvl2obj)
	$(RANLIB) $(ATLASlib)
	touch zblas2.grd
#
########################################################################
#
ATL_sgpr.o               : $(mySRCdir)/ATL_gpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_gpr.c
ATL_sgpmv.o              : $(mySRCdir)/ATL_gpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_gpmv.c
ATL_sgbmv.o              : $(mySRCdir)/ATL_gbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_gbmv.c
ATL_ssbmv.o              : $(mySRCdir)/ATL_sbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_sbmv.c
ATL_sspmv.o              : $(mySRCdir)/ATL_spmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_spmv.c
ATL_ssymv.o              : $(mySRCdir)/ATL_symv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_symv.c
ATL_sspr.o               : $(mySRCdir)/ATL_spr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_spr.c
ATL_ssyr.o               : $(mySRCdir)/ATL_syr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_syr.c
ATL_sspr2.o              : $(mySRCdir)/ATL_spr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_spr2.c
ATL_ssyr2.o              : $(mySRCdir)/ATL_syr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_syr2.c
ATL_stbmv.o              : $(mySRCdir)/ATL_tbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_tbmv.c
ATL_stpmv.o              : $(mySRCdir)/ATL_tpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_tpmv.c
ATL_strmv.o              : $(mySRCdir)/ATL_trmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_trmv.c
ATL_stbsv.o              : $(mySRCdir)/ATL_tbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_tbsv.c
ATL_stpsv.o              : $(mySRCdir)/ATL_tpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_tpsv.c
ATL_strsv.o              : $(mySRCdir)/ATL_trsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_trsv.c
#
ATL_dgpr.o               : $(mySRCdir)/ATL_gpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_gpr.c
ATL_dgpmv.o              : $(mySRCdir)/ATL_gpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_gpmv.c
ATL_dgbmv.o              : $(mySRCdir)/ATL_gbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_gbmv.c
ATL_dsbmv.o              : $(mySRCdir)/ATL_sbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_sbmv.c
ATL_dspmv.o              : $(mySRCdir)/ATL_spmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_spmv.c
ATL_dsymv.o              : $(mySRCdir)/ATL_symv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_symv.c
ATL_dspr.o               : $(mySRCdir)/ATL_spr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_spr.c
ATL_dsyr.o               : $(mySRCdir)/ATL_syr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_syr.c
ATL_dspr2.o              : $(mySRCdir)/ATL_spr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_spr2.c
ATL_dsyr2.o              : $(mySRCdir)/ATL_syr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_syr2.c
ATL_dtbmv.o              : $(mySRCdir)/ATL_tbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_tbmv.c
ATL_dtpmv.o              : $(mySRCdir)/ATL_tpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_tpmv.c
ATL_dtrmv.o              : $(mySRCdir)/ATL_trmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_trmv.c
ATL_dtbsv.o              : $(mySRCdir)/ATL_tbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_tbsv.c
ATL_dtpsv.o              : $(mySRCdir)/ATL_tpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_tpsv.c
ATL_dtrsv.o              : $(mySRCdir)/ATL_trsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_trsv.c
#
ATL_cgpru.o              : $(mySRCdir)/ATL_gpru.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_gpru.c
ATL_cgprc.o              : $(mySRCdir)/ATL_gprc.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_gprc.c
ATL_cgpmv.o              : $(mySRCdir)/ATL_gpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_gpmv.c
ATL_cgbmv.o              : $(mySRCdir)/ATL_gbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_gbmv.c
ATL_chbmv.o              : $(mySRCdir)/ATL_hbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_hbmv.c
ATL_chpmv.o              : $(mySRCdir)/ATL_hpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_hpmv.c
ATL_chemv.o              : $(mySRCdir)/ATL_hemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_hemv.c
ATL_chpr.o               : $(mySRCdir)/ATL_hpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_hpr.c
ATL_cher.o               : $(mySRCdir)/ATL_her.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_her.c
ATL_chpr2.o              : $(mySRCdir)/ATL_hpr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_hpr2.c
ATL_cher2.o              : $(mySRCdir)/ATL_her2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_her2.c
ATL_ctbmv.o              : $(mySRCdir)/ATL_tbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_tbmv.c
ATL_ctpmv.o              : $(mySRCdir)/ATL_tpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_tpmv.c
ATL_ctrmv.o              : $(mySRCdir)/ATL_trmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_trmv.c
ATL_ctbsv.o              : $(mySRCdir)/ATL_tbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_tbsv.c
ATL_ctpsv.o              : $(mySRCdir)/ATL_tpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_tpsv.c
ATL_ctrsv.o              : $(mySRCdir)/ATL_trsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_trsv.c
#
ATL_zgpru.o              : $(mySRCdir)/ATL_gpru.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_gpru.c
ATL_zgprc.o              : $(mySRCdir)/ATL_gprc.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_gprc.c
ATL_zgpmv.o              : $(mySRCdir)/ATL_gpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_gpmv.c
ATL_zgbmv.o              : $(mySRCdir)/ATL_gbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_gbmv.c
ATL_zhbmv.o              : $(mySRCdir)/ATL_hbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_hbmv.c
ATL_zhpmv.o              : $(mySRCdir)/ATL_hpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_hpmv.c
ATL_zhemv.o              : $(mySRCdir)/ATL_hemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_hemv.c
ATL_zhpr.o               : $(mySRCdir)/ATL_hpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_hpr.c
ATL_zher.o               : $(mySRCdir)/ATL_her.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_her.c
ATL_zhpr2.o              : $(mySRCdir)/ATL_hpr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_hpr2.c
ATL_zher2.o              : $(mySRCdir)/ATL_her2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_her2.c
ATL_ztbmv.o              : $(mySRCdir)/ATL_tbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_tbmv.c
ATL_ztpmv.o              : $(mySRCdir)/ATL_tpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_tpmv.c
ATL_ztrmv.o              : $(mySRCdir)/ATL_trmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_trmv.c
ATL_ztbsv.o              : $(mySRCdir)/ATL_tbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_tbsv.c
ATL_ztpsv.o              : $(mySRCdir)/ATL_tpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_tpsv.c
ATL_ztrsv.o              : $(mySRCdir)/ATL_trsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_trsv.c
#
ATL_L2AIsOverlapped.o : $(mySRCdir)/ATL_L2AIsOverlapped.c
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_L2AIsOverlapped.c
########################################################################
#
sclean           :
	rm -f $(ATL_slvl2obj) s*.grd
#
dclean           :
	rm -f $(ATL_dlvl2obj) d*.grd
#
cclean           :
	rm -f $(ATL_clvl2obj) c*.grd
#
zclean           :
	rm -f $(ATL_zlvl2obj) z*.grd
#
clean            :
	rm -f *.o *.grd
#
########################################################################
