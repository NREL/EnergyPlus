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
mySRCdir = $(SRCdir)/src/blas/reference/level2
#
########################################################################
# 
REFBLASlib = $(ATLASlib)
INC2dep    = $(INCSdir)/atlas_enum.h    $(INCSdir)/atlas_refmisc.h \
             $(INCSdir)/atlas_reflvl2.h $(INCSdir)/atlas_reflevel2.h
#
## Object files ########################################################
#
ATL_slvl2obj = \
   ATL_srefgbmv.o ATL_srefgbmvN.o ATL_srefgbmvT.o ATL_srefgemv.o \
   ATL_srefgemvN.o ATL_srefgemvT.o ATL_srefger.o ATL_srefgpmv.o \
   ATL_srefgpmvLN.o ATL_srefgpmvLT.o ATL_srefgpmvUN.o ATL_srefgpmvUT.o \
   ATL_srefgpr.o ATL_srefsbmv.o ATL_srefsbmvL.o ATL_srefsbmvU.o \
   ATL_srefspmv.o ATL_srefspmvL.o ATL_srefspmvU.o ATL_srefspr.o \
   ATL_srefspr2.o ATL_srefspr2L.o ATL_srefspr2U.o ATL_srefsprL.o \
   ATL_srefsprU.o ATL_srefsymv.o ATL_srefsymvL.o ATL_srefsymvU.o \
   ATL_srefsyr.o ATL_srefsyr2.o ATL_srefsyr2L.o ATL_srefsyr2U.o \
   ATL_srefsyrL.o ATL_srefsyrU.o ATL_sreftbmv.o ATL_sreftbmvLNN.o \
   ATL_sreftbmvLNU.o ATL_sreftbmvLTN.o ATL_sreftbmvLTU.o ATL_sreftbmvUNN.o \
   ATL_sreftbmvUNU.o ATL_sreftbmvUTN.o ATL_sreftbmvUTU.o ATL_sreftbsv.o \
   ATL_sreftbsvLNN.o ATL_sreftbsvLNU.o ATL_sreftbsvLTN.o ATL_sreftbsvLTU.o \
   ATL_sreftbsvUNN.o ATL_sreftbsvUNU.o ATL_sreftbsvUTN.o ATL_sreftbsvUTU.o \
   ATL_sreftpmv.o ATL_sreftpmvLNN.o ATL_sreftpmvLNU.o ATL_sreftpmvLTN.o \
   ATL_sreftpmvLTU.o ATL_sreftpmvUNN.o ATL_sreftpmvUNU.o ATL_sreftpmvUTN.o \
   ATL_sreftpmvUTU.o ATL_sreftpsv.o ATL_sreftpsvLNN.o ATL_sreftpsvLNU.o \
   ATL_sreftpsvLTN.o ATL_sreftpsvLTU.o ATL_sreftpsvUNN.o ATL_sreftpsvUNU.o \
   ATL_sreftpsvUTN.o ATL_sreftpsvUTU.o ATL_sreftrmv.o ATL_sreftrmvLNN.o \
   ATL_sreftrmvLNU.o ATL_sreftrmvLTN.o ATL_sreftrmvLTU.o ATL_sreftrmvUNN.o \
   ATL_sreftrmvUNU.o ATL_sreftrmvUTN.o ATL_sreftrmvUTU.o ATL_sreftrsv.o \
   ATL_sreftrsvLNN.o ATL_sreftrsvLNU.o ATL_sreftrsvLTN.o ATL_sreftrsvLTU.o \
   ATL_sreftrsvUNN.o ATL_sreftrsvUNU.o ATL_sreftrsvUTN.o ATL_sreftrsvUTU.o
#
ATL_dlvl2obj = \
   ATL_drefgbmv.o ATL_drefgbmvN.o ATL_drefgbmvT.o ATL_drefgemv.o \
   ATL_drefgemvN.o ATL_drefgemvT.o ATL_drefger.o ATL_drefgpmv.o \
   ATL_drefgpmvLN.o ATL_drefgpmvLT.o ATL_drefgpmvUN.o ATL_drefgpmvUT.o \
   ATL_drefgpr.o ATL_drefsbmv.o ATL_drefsbmvL.o ATL_drefsbmvU.o \
   ATL_drefspmv.o ATL_drefspmvL.o ATL_drefspmvU.o ATL_drefspr.o \
   ATL_drefspr2.o ATL_drefspr2L.o ATL_drefspr2U.o ATL_drefsprL.o \
   ATL_drefsprU.o ATL_drefsymv.o ATL_drefsymvL.o ATL_drefsymvU.o \
   ATL_drefsyr.o ATL_drefsyr2.o ATL_drefsyr2L.o ATL_drefsyr2U.o \
   ATL_drefsyrL.o ATL_drefsyrU.o ATL_dreftbmv.o ATL_dreftbmvLNN.o \
   ATL_dreftbmvLNU.o ATL_dreftbmvLTN.o ATL_dreftbmvLTU.o ATL_dreftbmvUNN.o \
   ATL_dreftbmvUNU.o ATL_dreftbmvUTN.o ATL_dreftbmvUTU.o ATL_dreftbsv.o \
   ATL_dreftbsvLNN.o ATL_dreftbsvLNU.o ATL_dreftbsvLTN.o ATL_dreftbsvLTU.o \
   ATL_dreftbsvUNN.o ATL_dreftbsvUNU.o ATL_dreftbsvUTN.o ATL_dreftbsvUTU.o \
   ATL_dreftpmv.o ATL_dreftpmvLNN.o ATL_dreftpmvLNU.o ATL_dreftpmvLTN.o \
   ATL_dreftpmvLTU.o ATL_dreftpmvUNN.o ATL_dreftpmvUNU.o ATL_dreftpmvUTN.o \
   ATL_dreftpmvUTU.o ATL_dreftpsv.o ATL_dreftpsvLNN.o ATL_dreftpsvLNU.o \
   ATL_dreftpsvLTN.o ATL_dreftpsvLTU.o ATL_dreftpsvUNN.o ATL_dreftpsvUNU.o \
   ATL_dreftpsvUTN.o ATL_dreftpsvUTU.o ATL_dreftrmv.o ATL_dreftrmvLNN.o \
   ATL_dreftrmvLNU.o ATL_dreftrmvLTN.o ATL_dreftrmvLTU.o ATL_dreftrmvUNN.o \
   ATL_dreftrmvUNU.o ATL_dreftrmvUTN.o ATL_dreftrmvUTU.o ATL_dreftrsv.o \
   ATL_dreftrsvLNN.o ATL_dreftrsvLNU.o ATL_dreftrsvLTN.o ATL_dreftrsvLTU.o \
   ATL_dreftrsvUNN.o ATL_dreftrsvUNU.o ATL_dreftrsvUTN.o ATL_dreftrsvUTU.o
#
ATL_clvl2obj = \
   ATL_crefgbmv.o ATL_crefgbmvC.o ATL_crefgbmvH.o ATL_crefgbmvN.o \
   ATL_crefgbmvT.o ATL_crefgemv.o ATL_crefgemvC.o ATL_crefgemvH.o \
   ATL_crefgemvN.o ATL_crefgemvT.o ATL_crefgerc.o ATL_crefgeru.o \
   ATL_crefgpmv.o ATL_crefgpmvLC.o ATL_crefgpmvLH.o ATL_crefgpmvLN.o \
   ATL_crefgpmvLT.o ATL_crefgpmvUC.o ATL_crefgpmvUH.o ATL_crefgpmvUN.o \
   ATL_crefgpmvUT.o ATL_crefgprc.o ATL_crefgpru.o ATL_crefhbmv.o \
   ATL_crefhbmvL.o ATL_crefhbmvU.o ATL_crefhemv.o ATL_crefhemvL.o \
   ATL_crefhemvU.o ATL_crefher.o ATL_crefher2.o ATL_crefher2L.o \
   ATL_crefher2U.o ATL_crefherL.o ATL_crefherU.o ATL_crefhpmv.o \
   ATL_crefhpmvL.o ATL_crefhpmvU.o ATL_crefhpr.o ATL_crefhpr2.o \
   ATL_crefhpr2L.o ATL_crefhpr2U.o ATL_crefhprL.o ATL_crefhprU.o \
   ATL_creftbmv.o ATL_creftbmvLCN.o ATL_creftbmvLCU.o ATL_creftbmvLHN.o \
   ATL_creftbmvLHU.o ATL_creftbmvLNN.o ATL_creftbmvLNU.o ATL_creftbmvLTN.o \
   ATL_creftbmvLTU.o ATL_creftbmvUCN.o ATL_creftbmvUCU.o ATL_creftbmvUHN.o \
   ATL_creftbmvUHU.o ATL_creftbmvUNN.o ATL_creftbmvUNU.o ATL_creftbmvUTN.o \
   ATL_creftbmvUTU.o ATL_creftbsv.o ATL_creftbsvLCN.o ATL_creftbsvLCU.o \
   ATL_creftbsvLHN.o ATL_creftbsvLHU.o ATL_creftbsvLNN.o ATL_creftbsvLNU.o \
   ATL_creftbsvLTN.o ATL_creftbsvLTU.o ATL_creftbsvUCN.o ATL_creftbsvUCU.o \
   ATL_creftbsvUHN.o ATL_creftbsvUHU.o ATL_creftbsvUNN.o ATL_creftbsvUNU.o \
   ATL_creftbsvUTN.o ATL_creftbsvUTU.o ATL_creftpmv.o ATL_creftpmvLCN.o \
   ATL_creftpmvLCU.o ATL_creftpmvLHN.o ATL_creftpmvLHU.o ATL_creftpmvLNN.o \
   ATL_creftpmvLNU.o ATL_creftpmvLTN.o ATL_creftpmvLTU.o ATL_creftpmvUCN.o \
   ATL_creftpmvUCU.o ATL_creftpmvUHN.o ATL_creftpmvUHU.o ATL_creftpmvUNN.o \
   ATL_creftpmvUNU.o ATL_creftpmvUTN.o ATL_creftpmvUTU.o ATL_creftpsv.o \
   ATL_creftpsvLCN.o ATL_creftpsvLCU.o ATL_creftpsvLHN.o ATL_creftpsvLHU.o \
   ATL_creftpsvLNN.o ATL_creftpsvLNU.o ATL_creftpsvLTN.o ATL_creftpsvLTU.o \
   ATL_creftpsvUCN.o ATL_creftpsvUCU.o ATL_creftpsvUHN.o ATL_creftpsvUHU.o \
   ATL_creftpsvUNN.o ATL_creftpsvUNU.o ATL_creftpsvUTN.o ATL_creftpsvUTU.o \
   ATL_creftrmv.o ATL_creftrmvLCN.o ATL_creftrmvLCU.o ATL_creftrmvLHN.o \
   ATL_creftrmvLHU.o ATL_creftrmvLNN.o ATL_creftrmvLNU.o ATL_creftrmvLTN.o \
   ATL_creftrmvLTU.o ATL_creftrmvUCN.o ATL_creftrmvUCU.o ATL_creftrmvUHN.o \
   ATL_creftrmvUHU.o ATL_creftrmvUNN.o ATL_creftrmvUNU.o ATL_creftrmvUTN.o \
   ATL_creftrmvUTU.o ATL_creftrsv.o ATL_creftrsvLCN.o ATL_creftrsvLCU.o \
   ATL_creftrsvLHN.o ATL_creftrsvLHU.o ATL_creftrsvLNN.o ATL_creftrsvLNU.o \
   ATL_creftrsvLTN.o ATL_creftrsvLTU.o ATL_creftrsvUCN.o ATL_creftrsvUCU.o \
   ATL_creftrsvUHN.o ATL_creftrsvUHU.o ATL_creftrsvUNN.o ATL_creftrsvUNU.o \
   ATL_creftrsvUTN.o ATL_creftrsvUTU.o
#
ATL_zlvl2obj = \
   ATL_zrefgbmv.o ATL_zrefgbmvC.o ATL_zrefgbmvH.o ATL_zrefgbmvN.o \
   ATL_zrefgbmvT.o ATL_zrefgemv.o ATL_zrefgemvC.o ATL_zrefgemvH.o \
   ATL_zrefgemvN.o ATL_zrefgemvT.o ATL_zrefgerc.o ATL_zrefgeru.o \
   ATL_zrefgpmv.o ATL_zrefgpmvLC.o ATL_zrefgpmvLH.o ATL_zrefgpmvLN.o \
   ATL_zrefgpmvLT.o ATL_zrefgpmvUC.o ATL_zrefgpmvUH.o ATL_zrefgpmvUN.o \
   ATL_zrefgpmvUT.o ATL_zrefgprc.o ATL_zrefgpru.o ATL_zrefhbmv.o \
   ATL_zrefhbmvL.o ATL_zrefhbmvU.o ATL_zrefhemv.o ATL_zrefhemvL.o \
   ATL_zrefhemvU.o ATL_zrefher.o ATL_zrefher2.o ATL_zrefher2L.o \
   ATL_zrefher2U.o ATL_zrefherL.o ATL_zrefherU.o ATL_zrefhpmv.o \
   ATL_zrefhpmvL.o ATL_zrefhpmvU.o ATL_zrefhpr.o ATL_zrefhpr2.o \
   ATL_zrefhpr2L.o ATL_zrefhpr2U.o ATL_zrefhprL.o ATL_zrefhprU.o \
   ATL_zreftbmv.o ATL_zreftbmvLCN.o ATL_zreftbmvLCU.o ATL_zreftbmvLHN.o \
   ATL_zreftbmvLHU.o ATL_zreftbmvLNN.o ATL_zreftbmvLNU.o ATL_zreftbmvLTN.o \
   ATL_zreftbmvLTU.o ATL_zreftbmvUCN.o ATL_zreftbmvUCU.o ATL_zreftbmvUHN.o \
   ATL_zreftbmvUHU.o ATL_zreftbmvUNN.o ATL_zreftbmvUNU.o ATL_zreftbmvUTN.o \
   ATL_zreftbmvUTU.o ATL_zreftbsv.o ATL_zreftbsvLCN.o ATL_zreftbsvLCU.o \
   ATL_zreftbsvLHN.o ATL_zreftbsvLHU.o ATL_zreftbsvLNN.o ATL_zreftbsvLNU.o \
   ATL_zreftbsvLTN.o ATL_zreftbsvLTU.o ATL_zreftbsvUCN.o ATL_zreftbsvUCU.o \
   ATL_zreftbsvUHN.o ATL_zreftbsvUHU.o ATL_zreftbsvUNN.o ATL_zreftbsvUNU.o \
   ATL_zreftbsvUTN.o ATL_zreftbsvUTU.o ATL_zreftpmv.o ATL_zreftpmvLCN.o \
   ATL_zreftpmvLCU.o ATL_zreftpmvLHN.o ATL_zreftpmvLHU.o ATL_zreftpmvLNN.o \
   ATL_zreftpmvLNU.o ATL_zreftpmvLTN.o ATL_zreftpmvLTU.o ATL_zreftpmvUCN.o \
   ATL_zreftpmvUCU.o ATL_zreftpmvUHN.o ATL_zreftpmvUHU.o ATL_zreftpmvUNN.o \
   ATL_zreftpmvUNU.o ATL_zreftpmvUTN.o ATL_zreftpmvUTU.o ATL_zreftpsv.o \
   ATL_zreftpsvLCN.o ATL_zreftpsvLCU.o ATL_zreftpsvLHN.o ATL_zreftpsvLHU.o \
   ATL_zreftpsvLNN.o ATL_zreftpsvLNU.o ATL_zreftpsvLTN.o ATL_zreftpsvLTU.o \
   ATL_zreftpsvUCN.o ATL_zreftpsvUCU.o ATL_zreftpsvUHN.o ATL_zreftpsvUHU.o \
   ATL_zreftpsvUNN.o ATL_zreftpsvUNU.o ATL_zreftpsvUTN.o ATL_zreftpsvUTU.o \
   ATL_zreftrmv.o ATL_zreftrmvLCN.o ATL_zreftrmvLCU.o ATL_zreftrmvLHN.o \
   ATL_zreftrmvLHU.o ATL_zreftrmvLNN.o ATL_zreftrmvLNU.o ATL_zreftrmvLTN.o \
   ATL_zreftrmvLTU.o ATL_zreftrmvUCN.o ATL_zreftrmvUCU.o ATL_zreftrmvUHN.o \
   ATL_zreftrmvUHU.o ATL_zreftrmvUNN.o ATL_zreftrmvUNU.o ATL_zreftrmvUTN.o \
   ATL_zreftrmvUTU.o ATL_zreftrsv.o ATL_zreftrsvLCN.o ATL_zreftrsvLCU.o \
   ATL_zreftrsvLHN.o ATL_zreftrsvLHU.o ATL_zreftrsvLNN.o ATL_zreftrsvLNU.o \
   ATL_zreftrsvLTN.o ATL_zreftrsvLTU.o ATL_zreftrsvUCN.o ATL_zreftrsvUCU.o \
   ATL_zreftrsvUHN.o ATL_zreftrsvUHU.o ATL_zreftrsvUNN.o ATL_zreftrsvUNU.o \
   ATL_zreftrsvUTN.o ATL_zreftrsvUTU.o
#
## Targets #############################################################
#
all    : lib
#
lvl2   : lib
#
lib    : slib dlib clib zlib
#
slvl2 : slib
dlvl2 : dlib
clvl2 : clib
zlvl2 : zlib
#
########################################################################
#
slib  : slib.grd
dlib  : dlib.grd
clib  : clib.grd
zlib  : zlib.grd
#
slib.grd  : $(ATL_slvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_slvl2obj)
	$(RANLIB) $(REFBLASlib)
	touch slib.grd
#
dlib.grd  : $(ATL_dlvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_dlvl2obj)
	$(RANLIB) $(REFBLASlib)
	touch dlib.grd
#
clib.grd  : $(ATL_clvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_clvl2obj)
	$(RANLIB) $(REFBLASlib)
	touch clib.grd
#
zlib.grd  : $(ATL_zlvl2obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_zlvl2obj)
	$(RANLIB) $(REFBLASlib)
	touch zlib.grd
#
########################################################################
#
ATL_srefgbmv.o                 : $(mySRCdir)/ATL_srefgbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgbmv.c
ATL_srefgbmvN.o                : $(mySRCdir)/ATL_srefgbmvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgbmvN.c
ATL_srefgbmvT.o                : $(mySRCdir)/ATL_srefgbmvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgbmvT.c
ATL_srefgpmv.o                 : $(mySRCdir)/ATL_srefgpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgpmv.c
ATL_srefgpmvLN.o               : $(mySRCdir)/ATL_srefgpmvLN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgpmvLN.c
ATL_srefgpmvLT.o               : $(mySRCdir)/ATL_srefgpmvLT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgpmvLT.c
ATL_srefgpmvUN.o               : $(mySRCdir)/ATL_srefgpmvUN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgpmvUN.c
ATL_srefgpmvUT.o               : $(mySRCdir)/ATL_srefgpmvUT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgpmvUT.c
ATL_srefgemv.o                 : $(mySRCdir)/ATL_srefgemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemv.c
ATL_srefgemvN.o                : $(mySRCdir)/ATL_srefgemvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemvN.c
ATL_srefgemvT.o                : $(mySRCdir)/ATL_srefgemvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemvT.c
ATL_srefgpr.o                  : $(mySRCdir)/ATL_srefgpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgpr.c
ATL_srefger.o                  : $(mySRCdir)/ATL_srefger.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefger.c
ATL_srefsymv.o                 : $(mySRCdir)/ATL_srefsymv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymv.c
ATL_srefsymvL.o                : $(mySRCdir)/ATL_srefsymvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymvL.c
ATL_srefsymvU.o                : $(mySRCdir)/ATL_srefsymvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymvU.c
ATL_srefspmv.o                 : $(mySRCdir)/ATL_srefspmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspmv.c
ATL_srefspmvL.o                : $(mySRCdir)/ATL_srefspmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspmvL.c
ATL_srefspmvU.o                : $(mySRCdir)/ATL_srefspmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspmvU.c
ATL_srefsbmv.o                 : $(mySRCdir)/ATL_srefsbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsbmv.c
ATL_srefsbmvL.o                : $(mySRCdir)/ATL_srefsbmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsbmvL.c
ATL_srefsbmvU.o                : $(mySRCdir)/ATL_srefsbmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsbmvU.c
ATL_srefsprU.o                 : $(mySRCdir)/ATL_srefsprU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsprU.c
ATL_srefsprL.o                 : $(mySRCdir)/ATL_srefsprL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsprL.c
ATL_srefspr.o                  : $(mySRCdir)/ATL_srefspr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspr.c
ATL_srefsyrU.o                 : $(mySRCdir)/ATL_srefsyrU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrU.c
ATL_srefsyrL.o                 : $(mySRCdir)/ATL_srefsyrL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrL.c
ATL_srefsyr.o                  : $(mySRCdir)/ATL_srefsyr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr.c
ATL_srefspr2U.o                : $(mySRCdir)/ATL_srefspr2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspr2U.c
ATL_srefspr2L.o                : $(mySRCdir)/ATL_srefspr2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspr2L.c
ATL_srefspr2.o                 : $(mySRCdir)/ATL_srefspr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefspr2.c
ATL_srefsyr2U.o                : $(mySRCdir)/ATL_srefsyr2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2U.c
ATL_srefsyr2L.o                : $(mySRCdir)/ATL_srefsyr2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2L.c
ATL_srefsyr2.o                 : $(mySRCdir)/ATL_srefsyr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2.c
ATL_sreftrsvUTU.o              : $(mySRCdir)/ATL_sreftrsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvUTU.c
ATL_sreftrmvUTU.o              : $(mySRCdir)/ATL_sreftrmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvUTU.c
ATL_sreftpsvUTU.o              : $(mySRCdir)/ATL_sreftpsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvUTU.c
ATL_sreftpmvUTU.o              : $(mySRCdir)/ATL_sreftpmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvUTU.c
ATL_sreftbsvUTU.o              : $(mySRCdir)/ATL_sreftbsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvUTU.c
ATL_sreftbmvUTU.o              : $(mySRCdir)/ATL_sreftbmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvUTU.c
ATL_sreftrsvUTN.o              : $(mySRCdir)/ATL_sreftrsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvUTN.c
ATL_sreftrmvUTN.o              : $(mySRCdir)/ATL_sreftrmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvUTN.c
ATL_sreftpsvUTN.o              : $(mySRCdir)/ATL_sreftpsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvUTN.c
ATL_sreftpmvUTN.o              : $(mySRCdir)/ATL_sreftpmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvUTN.c
ATL_sreftbsvUTN.o              : $(mySRCdir)/ATL_sreftbsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvUTN.c
ATL_sreftbmvUTN.o              : $(mySRCdir)/ATL_sreftbmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvUTN.c
ATL_sreftrsvUNU.o              : $(mySRCdir)/ATL_sreftrsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvUNU.c
ATL_sreftrmvUNU.o              : $(mySRCdir)/ATL_sreftrmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvUNU.c
ATL_sreftpsvUNU.o              : $(mySRCdir)/ATL_sreftpsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvUNU.c
ATL_sreftpmvUNU.o              : $(mySRCdir)/ATL_sreftpmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvUNU.c
ATL_sreftbsvUNU.o              : $(mySRCdir)/ATL_sreftbsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvUNU.c
ATL_sreftbmvUNU.o              : $(mySRCdir)/ATL_sreftbmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvUNU.c
ATL_sreftrsvUNN.o              : $(mySRCdir)/ATL_sreftrsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvUNN.c
ATL_sreftrmvUNN.o              : $(mySRCdir)/ATL_sreftrmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvUNN.c
ATL_sreftpsvUNN.o              : $(mySRCdir)/ATL_sreftpsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvUNN.c
ATL_sreftpmvUNN.o              : $(mySRCdir)/ATL_sreftpmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvUNN.c
ATL_sreftbsvUNN.o              : $(mySRCdir)/ATL_sreftbsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvUNN.c
ATL_sreftbmvUNN.o              : $(mySRCdir)/ATL_sreftbmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvUNN.c
ATL_sreftrsvLTU.o              : $(mySRCdir)/ATL_sreftrsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvLTU.c
ATL_sreftrmvLTU.o              : $(mySRCdir)/ATL_sreftrmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvLTU.c
ATL_sreftpsvLTU.o              : $(mySRCdir)/ATL_sreftpsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvLTU.c
ATL_sreftpmvLTU.o              : $(mySRCdir)/ATL_sreftpmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvLTU.c
ATL_sreftbsvLTU.o              : $(mySRCdir)/ATL_sreftbsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvLTU.c
ATL_sreftbmvLTU.o              : $(mySRCdir)/ATL_sreftbmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvLTU.c
ATL_sreftrsvLTN.o              : $(mySRCdir)/ATL_sreftrsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvLTN.c
ATL_sreftrmvLTN.o              : $(mySRCdir)/ATL_sreftrmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvLTN.c
ATL_sreftpsvLTN.o              : $(mySRCdir)/ATL_sreftpsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvLTN.c
ATL_sreftpmvLTN.o              : $(mySRCdir)/ATL_sreftpmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvLTN.c
ATL_sreftbsvLTN.o              : $(mySRCdir)/ATL_sreftbsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvLTN.c
ATL_sreftbmvLTN.o              : $(mySRCdir)/ATL_sreftbmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvLTN.c
ATL_sreftrsvLNU.o              : $(mySRCdir)/ATL_sreftrsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvLNU.c
ATL_sreftrmvLNU.o              : $(mySRCdir)/ATL_sreftrmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvLNU.c
ATL_sreftpsvLNU.o              : $(mySRCdir)/ATL_sreftpsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvLNU.c
ATL_sreftpmvLNU.o              : $(mySRCdir)/ATL_sreftpmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvLNU.c
ATL_sreftbsvLNU.o              : $(mySRCdir)/ATL_sreftbsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvLNU.c
ATL_sreftbmvLNU.o              : $(mySRCdir)/ATL_sreftbmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvLNU.c
ATL_sreftrsvLNN.o              : $(mySRCdir)/ATL_sreftrsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsvLNN.c
ATL_sreftrmvLNN.o              : $(mySRCdir)/ATL_sreftrmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmvLNN.c
ATL_sreftpsvLNN.o              : $(mySRCdir)/ATL_sreftpsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsvLNN.c
ATL_sreftpmvLNN.o              : $(mySRCdir)/ATL_sreftpmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmvLNN.c
ATL_sreftbsvLNN.o              : $(mySRCdir)/ATL_sreftbsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsvLNN.c
ATL_sreftbmvLNN.o              : $(mySRCdir)/ATL_sreftbmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmvLNN.c
ATL_sreftrsv.o                 : $(mySRCdir)/ATL_sreftrsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsv.c
ATL_sreftrmv.o                 : $(mySRCdir)/ATL_sreftrmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmv.c
ATL_sreftpsv.o                 : $(mySRCdir)/ATL_sreftpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpsv.c
ATL_sreftpmv.o                 : $(mySRCdir)/ATL_sreftpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftpmv.c
ATL_sreftbsv.o                 : $(mySRCdir)/ATL_sreftbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbsv.c
ATL_sreftbmv.o                 : $(mySRCdir)/ATL_sreftbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftbmv.c
#
ATL_drefgbmv.o                 : $(mySRCdir)/ATL_drefgbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgbmv.c
ATL_drefgbmvN.o                : $(mySRCdir)/ATL_drefgbmvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgbmvN.c
ATL_drefgbmvT.o                : $(mySRCdir)/ATL_drefgbmvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgbmvT.c
ATL_drefgpmv.o                 : $(mySRCdir)/ATL_drefgpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgpmv.c
ATL_drefgpmvLN.o               : $(mySRCdir)/ATL_drefgpmvLN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgpmvLN.c
ATL_drefgpmvLT.o               : $(mySRCdir)/ATL_drefgpmvLT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgpmvLT.c
ATL_drefgpmvUN.o               : $(mySRCdir)/ATL_drefgpmvUN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgpmvUN.c
ATL_drefgpmvUT.o               : $(mySRCdir)/ATL_drefgpmvUT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgpmvUT.c
ATL_drefgemv.o                 : $(mySRCdir)/ATL_drefgemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemv.c
ATL_drefgemvN.o                : $(mySRCdir)/ATL_drefgemvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemvN.c
ATL_drefgemvT.o                : $(mySRCdir)/ATL_drefgemvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemvT.c
ATL_drefgpr.o                  : $(mySRCdir)/ATL_drefgpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgpr.c
ATL_drefger.o                  : $(mySRCdir)/ATL_drefger.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefger.c
ATL_drefsymv.o                 : $(mySRCdir)/ATL_drefsymv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymv.c
ATL_drefsymvL.o                : $(mySRCdir)/ATL_drefsymvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymvL.c
ATL_drefsymvU.o                : $(mySRCdir)/ATL_drefsymvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymvU.c
ATL_drefspmv.o                 : $(mySRCdir)/ATL_drefspmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspmv.c
ATL_drefspmvL.o                : $(mySRCdir)/ATL_drefspmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspmvL.c
ATL_drefspmvU.o                : $(mySRCdir)/ATL_drefspmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspmvU.c
ATL_drefsbmv.o                 : $(mySRCdir)/ATL_drefsbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsbmv.c
ATL_drefsbmvL.o                : $(mySRCdir)/ATL_drefsbmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsbmvL.c
ATL_drefsbmvU.o                : $(mySRCdir)/ATL_drefsbmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsbmvU.c
ATL_drefsprU.o                 : $(mySRCdir)/ATL_drefsprU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsprU.c
ATL_drefsprL.o                 : $(mySRCdir)/ATL_drefsprL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsprL.c
ATL_drefspr.o                  : $(mySRCdir)/ATL_drefspr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspr.c
ATL_drefsyrU.o                 : $(mySRCdir)/ATL_drefsyrU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrU.c
ATL_drefsyrL.o                 : $(mySRCdir)/ATL_drefsyrL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrL.c
ATL_drefsyr.o                  : $(mySRCdir)/ATL_drefsyr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr.c
ATL_drefspr2U.o                : $(mySRCdir)/ATL_drefspr2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspr2U.c
ATL_drefspr2L.o                : $(mySRCdir)/ATL_drefspr2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspr2L.c
ATL_drefspr2.o                 : $(mySRCdir)/ATL_drefspr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefspr2.c
ATL_drefsyr2U.o                : $(mySRCdir)/ATL_drefsyr2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2U.c
ATL_drefsyr2L.o                : $(mySRCdir)/ATL_drefsyr2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2L.c
ATL_drefsyr2.o                 : $(mySRCdir)/ATL_drefsyr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2.c
ATL_dreftrsvUTU.o              : $(mySRCdir)/ATL_dreftrsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvUTU.c
ATL_dreftrmvUTU.o              : $(mySRCdir)/ATL_dreftrmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvUTU.c
ATL_dreftpsvUTU.o              : $(mySRCdir)/ATL_dreftpsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvUTU.c
ATL_dreftpmvUTU.o              : $(mySRCdir)/ATL_dreftpmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvUTU.c
ATL_dreftbsvUTU.o              : $(mySRCdir)/ATL_dreftbsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvUTU.c
ATL_dreftbmvUTU.o              : $(mySRCdir)/ATL_dreftbmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvUTU.c
ATL_dreftrsvUTN.o              : $(mySRCdir)/ATL_dreftrsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvUTN.c
ATL_dreftrmvUTN.o              : $(mySRCdir)/ATL_dreftrmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvUTN.c
ATL_dreftpsvUTN.o              : $(mySRCdir)/ATL_dreftpsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvUTN.c
ATL_dreftpmvUTN.o              : $(mySRCdir)/ATL_dreftpmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvUTN.c
ATL_dreftbsvUTN.o              : $(mySRCdir)/ATL_dreftbsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvUTN.c
ATL_dreftbmvUTN.o              : $(mySRCdir)/ATL_dreftbmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvUTN.c
ATL_dreftrsvUNU.o              : $(mySRCdir)/ATL_dreftrsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvUNU.c
ATL_dreftrmvUNU.o              : $(mySRCdir)/ATL_dreftrmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvUNU.c
ATL_dreftpsvUNU.o              : $(mySRCdir)/ATL_dreftpsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvUNU.c
ATL_dreftpmvUNU.o              : $(mySRCdir)/ATL_dreftpmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvUNU.c
ATL_dreftbsvUNU.o              : $(mySRCdir)/ATL_dreftbsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvUNU.c
ATL_dreftbmvUNU.o              : $(mySRCdir)/ATL_dreftbmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvUNU.c
ATL_dreftrsvUNN.o              : $(mySRCdir)/ATL_dreftrsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvUNN.c
ATL_dreftrmvUNN.o              : $(mySRCdir)/ATL_dreftrmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvUNN.c
ATL_dreftpsvUNN.o              : $(mySRCdir)/ATL_dreftpsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvUNN.c
ATL_dreftpmvUNN.o              : $(mySRCdir)/ATL_dreftpmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvUNN.c
ATL_dreftbsvUNN.o              : $(mySRCdir)/ATL_dreftbsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvUNN.c
ATL_dreftbmvUNN.o              : $(mySRCdir)/ATL_dreftbmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvUNN.c
ATL_dreftrsvLTU.o              : $(mySRCdir)/ATL_dreftrsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvLTU.c
ATL_dreftrmvLTU.o              : $(mySRCdir)/ATL_dreftrmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvLTU.c
ATL_dreftpsvLTU.o              : $(mySRCdir)/ATL_dreftpsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvLTU.c
ATL_dreftpmvLTU.o              : $(mySRCdir)/ATL_dreftpmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvLTU.c
ATL_dreftbsvLTU.o              : $(mySRCdir)/ATL_dreftbsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvLTU.c
ATL_dreftbmvLTU.o              : $(mySRCdir)/ATL_dreftbmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvLTU.c
ATL_dreftrsvLTN.o              : $(mySRCdir)/ATL_dreftrsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvLTN.c
ATL_dreftrmvLTN.o              : $(mySRCdir)/ATL_dreftrmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvLTN.c
ATL_dreftpsvLTN.o              : $(mySRCdir)/ATL_dreftpsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvLTN.c
ATL_dreftpmvLTN.o              : $(mySRCdir)/ATL_dreftpmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvLTN.c
ATL_dreftbsvLTN.o              : $(mySRCdir)/ATL_dreftbsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvLTN.c
ATL_dreftbmvLTN.o              : $(mySRCdir)/ATL_dreftbmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvLTN.c
ATL_dreftrsvLNU.o              : $(mySRCdir)/ATL_dreftrsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvLNU.c
ATL_dreftrmvLNU.o              : $(mySRCdir)/ATL_dreftrmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvLNU.c
ATL_dreftpsvLNU.o              : $(mySRCdir)/ATL_dreftpsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvLNU.c
ATL_dreftpmvLNU.o              : $(mySRCdir)/ATL_dreftpmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvLNU.c
ATL_dreftbsvLNU.o              : $(mySRCdir)/ATL_dreftbsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvLNU.c
ATL_dreftbmvLNU.o              : $(mySRCdir)/ATL_dreftbmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvLNU.c
ATL_dreftrsvLNN.o              : $(mySRCdir)/ATL_dreftrsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsvLNN.c
ATL_dreftrmvLNN.o              : $(mySRCdir)/ATL_dreftrmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmvLNN.c
ATL_dreftpsvLNN.o              : $(mySRCdir)/ATL_dreftpsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsvLNN.c
ATL_dreftpmvLNN.o              : $(mySRCdir)/ATL_dreftpmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmvLNN.c
ATL_dreftbsvLNN.o              : $(mySRCdir)/ATL_dreftbsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsvLNN.c
ATL_dreftbmvLNN.o              : $(mySRCdir)/ATL_dreftbmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmvLNN.c
ATL_dreftrsv.o                 : $(mySRCdir)/ATL_dreftrsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsv.c
ATL_dreftrmv.o                 : $(mySRCdir)/ATL_dreftrmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmv.c
ATL_dreftpsv.o                 : $(mySRCdir)/ATL_dreftpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpsv.c
ATL_dreftpmv.o                 : $(mySRCdir)/ATL_dreftpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftpmv.c
ATL_dreftbsv.o                 : $(mySRCdir)/ATL_dreftbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbsv.c
ATL_dreftbmv.o                 : $(mySRCdir)/ATL_dreftbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftbmv.c
#
ATL_crefgbmv.o                 : $(mySRCdir)/ATL_crefgbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgbmv.c
ATL_crefgbmvN.o                : $(mySRCdir)/ATL_crefgbmvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgbmvN.c
ATL_crefgbmvC.o                : $(mySRCdir)/ATL_crefgbmvC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgbmvC.c
ATL_crefgbmvT.o                : $(mySRCdir)/ATL_crefgbmvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgbmvT.c
ATL_crefgbmvH.o                : $(mySRCdir)/ATL_crefgbmvH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgbmvH.c
ATL_crefgpmv.o                 : $(mySRCdir)/ATL_crefgpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmv.c
ATL_crefgpmvUN.o               : $(mySRCdir)/ATL_crefgpmvUN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvUN.c
ATL_crefgpmvUC.o               : $(mySRCdir)/ATL_crefgpmvUC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvUC.c
ATL_crefgpmvUT.o               : $(mySRCdir)/ATL_crefgpmvUT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvUT.c
ATL_crefgpmvUH.o               : $(mySRCdir)/ATL_crefgpmvUH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvUH.c
ATL_crefgpmvLN.o               : $(mySRCdir)/ATL_crefgpmvLN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvLN.c
ATL_crefgpmvLC.o               : $(mySRCdir)/ATL_crefgpmvLC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvLC.c
ATL_crefgpmvLT.o               : $(mySRCdir)/ATL_crefgpmvLT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvLT.c
ATL_crefgpmvLH.o               : $(mySRCdir)/ATL_crefgpmvLH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpmvLH.c
ATL_crefgemv.o                 : $(mySRCdir)/ATL_crefgemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemv.c
ATL_crefgemvN.o                : $(mySRCdir)/ATL_crefgemvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemvN.c
ATL_crefgemvC.o                : $(mySRCdir)/ATL_crefgemvC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemvC.c
ATL_crefgemvT.o                : $(mySRCdir)/ATL_crefgemvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemvT.c
ATL_crefgemvH.o                : $(mySRCdir)/ATL_crefgemvH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemvH.c
ATL_crefgprc.o                 : $(mySRCdir)/ATL_crefgprc.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgprc.c
ATL_crefgpru.o                 : $(mySRCdir)/ATL_crefgpru.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgpru.c
ATL_crefgerc.o                 : $(mySRCdir)/ATL_crefgerc.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgerc.c
ATL_crefgeru.o                 : $(mySRCdir)/ATL_crefgeru.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgeru.c
ATL_crefhemv.o                 : $(mySRCdir)/ATL_crefhemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemv.c
ATL_crefhemvL.o                : $(mySRCdir)/ATL_crefhemvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemvL.c
ATL_crefhemvU.o                : $(mySRCdir)/ATL_crefhemvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemvU.c
ATL_crefhpmv.o                 : $(mySRCdir)/ATL_crefhpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpmv.c
ATL_crefhpmvL.o                : $(mySRCdir)/ATL_crefhpmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpmvL.c
ATL_crefhpmvU.o                : $(mySRCdir)/ATL_crefhpmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpmvU.c
ATL_crefhbmv.o                 : $(mySRCdir)/ATL_crefhbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhbmv.c
ATL_crefhbmvL.o                : $(mySRCdir)/ATL_crefhbmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhbmvL.c
ATL_crefhbmvU.o                : $(mySRCdir)/ATL_crefhbmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhbmvU.c
ATL_crefhprU.o                 : $(mySRCdir)/ATL_crefhprU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhprU.c
ATL_crefhprL.o                 : $(mySRCdir)/ATL_crefhprL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhprL.c
ATL_crefhpr.o                  : $(mySRCdir)/ATL_crefhpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpr.c
ATL_crefherU.o                 : $(mySRCdir)/ATL_crefherU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherU.c
ATL_crefherL.o                 : $(mySRCdir)/ATL_crefherL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherL.c
ATL_crefher.o                  : $(mySRCdir)/ATL_crefher.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher.c
ATL_crefhpr2U.o                : $(mySRCdir)/ATL_crefhpr2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpr2U.c
ATL_crefhpr2L.o                : $(mySRCdir)/ATL_crefhpr2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpr2L.c
ATL_crefhpr2.o                 : $(mySRCdir)/ATL_crefhpr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhpr2.c
ATL_crefher2U.o                : $(mySRCdir)/ATL_crefher2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2U.c
ATL_crefher2L.o                : $(mySRCdir)/ATL_crefher2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2L.c
ATL_crefher2.o                 : $(mySRCdir)/ATL_crefher2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2.c
ATL_creftrsvUHU.o              : $(mySRCdir)/ATL_creftrsvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUHU.c
ATL_creftrmvUHU.o              : $(mySRCdir)/ATL_creftrmvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUHU.c
ATL_creftpsvUHU.o              : $(mySRCdir)/ATL_creftpsvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUHU.c
ATL_creftpmvUHU.o              : $(mySRCdir)/ATL_creftpmvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUHU.c
ATL_creftbsvUHU.o              : $(mySRCdir)/ATL_creftbsvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUHU.c
ATL_creftbmvUHU.o              : $(mySRCdir)/ATL_creftbmvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUHU.c
ATL_creftrsvUHN.o              : $(mySRCdir)/ATL_creftrsvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUHN.c
ATL_creftrmvUHN.o              : $(mySRCdir)/ATL_creftrmvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUHN.c
ATL_creftpsvUHN.o              : $(mySRCdir)/ATL_creftpsvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUHN.c
ATL_creftpmvUHN.o              : $(mySRCdir)/ATL_creftpmvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUHN.c
ATL_creftbsvUHN.o              : $(mySRCdir)/ATL_creftbsvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUHN.c
ATL_creftbmvUHN.o              : $(mySRCdir)/ATL_creftbmvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUHN.c
ATL_creftrsvUTU.o              : $(mySRCdir)/ATL_creftrsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUTU.c
ATL_creftrmvUTU.o              : $(mySRCdir)/ATL_creftrmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUTU.c
ATL_creftpsvUTU.o              : $(mySRCdir)/ATL_creftpsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUTU.c
ATL_creftpmvUTU.o              : $(mySRCdir)/ATL_creftpmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUTU.c
ATL_creftbsvUTU.o              : $(mySRCdir)/ATL_creftbsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUTU.c
ATL_creftbmvUTU.o              : $(mySRCdir)/ATL_creftbmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUTU.c
ATL_creftrsvUTN.o              : $(mySRCdir)/ATL_creftrsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUTN.c
ATL_creftrmvUTN.o              : $(mySRCdir)/ATL_creftrmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUTN.c
ATL_creftpsvUTN.o              : $(mySRCdir)/ATL_creftpsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUTN.c
ATL_creftpmvUTN.o              : $(mySRCdir)/ATL_creftpmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUTN.c
ATL_creftbsvUTN.o              : $(mySRCdir)/ATL_creftbsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUTN.c
ATL_creftbmvUTN.o              : $(mySRCdir)/ATL_creftbmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUTN.c
ATL_creftrsvUCU.o              : $(mySRCdir)/ATL_creftrsvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUCU.c
ATL_creftrmvUCU.o              : $(mySRCdir)/ATL_creftrmvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUCU.c
ATL_creftpsvUCU.o              : $(mySRCdir)/ATL_creftpsvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUCU.c
ATL_creftpmvUCU.o              : $(mySRCdir)/ATL_creftpmvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUCU.c
ATL_creftbsvUCU.o              : $(mySRCdir)/ATL_creftbsvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUCU.c
ATL_creftbmvUCU.o              : $(mySRCdir)/ATL_creftbmvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUCU.c
ATL_creftrsvUCN.o              : $(mySRCdir)/ATL_creftrsvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUCN.c
ATL_creftrmvUCN.o              : $(mySRCdir)/ATL_creftrmvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUCN.c
ATL_creftpsvUCN.o              : $(mySRCdir)/ATL_creftpsvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUCN.c
ATL_creftpmvUCN.o              : $(mySRCdir)/ATL_creftpmvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUCN.c
ATL_creftbsvUCN.o              : $(mySRCdir)/ATL_creftbsvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUCN.c
ATL_creftbmvUCN.o              : $(mySRCdir)/ATL_creftbmvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUCN.c
ATL_creftrsvUNU.o              : $(mySRCdir)/ATL_creftrsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUNU.c
ATL_creftrmvUNU.o              : $(mySRCdir)/ATL_creftrmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUNU.c
ATL_creftpsvUNU.o              : $(mySRCdir)/ATL_creftpsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUNU.c
ATL_creftpmvUNU.o              : $(mySRCdir)/ATL_creftpmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUNU.c
ATL_creftbsvUNU.o              : $(mySRCdir)/ATL_creftbsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUNU.c
ATL_creftbmvUNU.o              : $(mySRCdir)/ATL_creftbmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUNU.c
ATL_creftrsvUNN.o              : $(mySRCdir)/ATL_creftrsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvUNN.c
ATL_creftrmvUNN.o              : $(mySRCdir)/ATL_creftrmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvUNN.c
ATL_creftpsvUNN.o              : $(mySRCdir)/ATL_creftpsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvUNN.c
ATL_creftpmvUNN.o              : $(mySRCdir)/ATL_creftpmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvUNN.c
ATL_creftbsvUNN.o              : $(mySRCdir)/ATL_creftbsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvUNN.c
ATL_creftbmvUNN.o              : $(mySRCdir)/ATL_creftbmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvUNN.c
ATL_creftrsvLHU.o              : $(mySRCdir)/ATL_creftrsvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLHU.c
ATL_creftrmvLHU.o              : $(mySRCdir)/ATL_creftrmvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLHU.c
ATL_creftpsvLHU.o              : $(mySRCdir)/ATL_creftpsvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLHU.c
ATL_creftpmvLHU.o              : $(mySRCdir)/ATL_creftpmvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLHU.c
ATL_creftbsvLHU.o              : $(mySRCdir)/ATL_creftbsvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLHU.c
ATL_creftbmvLHU.o              : $(mySRCdir)/ATL_creftbmvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLHU.c
ATL_creftrsvLHN.o              : $(mySRCdir)/ATL_creftrsvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLHN.c
ATL_creftrmvLHN.o              : $(mySRCdir)/ATL_creftrmvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLHN.c
ATL_creftpsvLHN.o              : $(mySRCdir)/ATL_creftpsvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLHN.c
ATL_creftpmvLHN.o              : $(mySRCdir)/ATL_creftpmvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLHN.c
ATL_creftbsvLHN.o              : $(mySRCdir)/ATL_creftbsvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLHN.c
ATL_creftbmvLHN.o              : $(mySRCdir)/ATL_creftbmvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLHN.c
ATL_creftrsvLTU.o              : $(mySRCdir)/ATL_creftrsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLTU.c
ATL_creftrmvLTU.o              : $(mySRCdir)/ATL_creftrmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLTU.c
ATL_creftpsvLTU.o              : $(mySRCdir)/ATL_creftpsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLTU.c
ATL_creftpmvLTU.o              : $(mySRCdir)/ATL_creftpmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLTU.c
ATL_creftbsvLTU.o              : $(mySRCdir)/ATL_creftbsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLTU.c
ATL_creftbmvLTU.o              : $(mySRCdir)/ATL_creftbmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLTU.c
ATL_creftrsvLTN.o              : $(mySRCdir)/ATL_creftrsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLTN.c
ATL_creftrmvLTN.o              : $(mySRCdir)/ATL_creftrmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLTN.c
ATL_creftpsvLTN.o              : $(mySRCdir)/ATL_creftpsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLTN.c
ATL_creftpmvLTN.o              : $(mySRCdir)/ATL_creftpmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLTN.c
ATL_creftbsvLTN.o              : $(mySRCdir)/ATL_creftbsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLTN.c
ATL_creftbmvLTN.o              : $(mySRCdir)/ATL_creftbmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLTN.c
ATL_creftrsvLCU.o              : $(mySRCdir)/ATL_creftrsvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLCU.c
ATL_creftrmvLCU.o              : $(mySRCdir)/ATL_creftrmvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLCU.c
ATL_creftpsvLCU.o              : $(mySRCdir)/ATL_creftpsvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLCU.c
ATL_creftpmvLCU.o              : $(mySRCdir)/ATL_creftpmvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLCU.c
ATL_creftbsvLCU.o              : $(mySRCdir)/ATL_creftbsvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLCU.c
ATL_creftbmvLCU.o              : $(mySRCdir)/ATL_creftbmvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLCU.c
ATL_creftrsvLCN.o              : $(mySRCdir)/ATL_creftrsvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLCN.c
ATL_creftrmvLCN.o              : $(mySRCdir)/ATL_creftrmvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLCN.c
ATL_creftpsvLCN.o              : $(mySRCdir)/ATL_creftpsvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLCN.c
ATL_creftpmvLCN.o              : $(mySRCdir)/ATL_creftpmvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLCN.c
ATL_creftbsvLCN.o              : $(mySRCdir)/ATL_creftbsvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLCN.c
ATL_creftbmvLCN.o              : $(mySRCdir)/ATL_creftbmvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLCN.c
ATL_creftrsvLNU.o              : $(mySRCdir)/ATL_creftrsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLNU.c
ATL_creftrmvLNU.o              : $(mySRCdir)/ATL_creftrmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLNU.c
ATL_creftpsvLNU.o              : $(mySRCdir)/ATL_creftpsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLNU.c
ATL_creftpmvLNU.o              : $(mySRCdir)/ATL_creftpmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLNU.c
ATL_creftbsvLNU.o              : $(mySRCdir)/ATL_creftbsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLNU.c
ATL_creftbmvLNU.o              : $(mySRCdir)/ATL_creftbmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLNU.c
ATL_creftrsvLNN.o              : $(mySRCdir)/ATL_creftrsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsvLNN.c
ATL_creftrmvLNN.o              : $(mySRCdir)/ATL_creftrmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmvLNN.c
ATL_creftpsvLNN.o              : $(mySRCdir)/ATL_creftpsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsvLNN.c
ATL_creftpmvLNN.o              : $(mySRCdir)/ATL_creftpmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmvLNN.c
ATL_creftbsvLNN.o              : $(mySRCdir)/ATL_creftbsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsvLNN.c
ATL_creftbmvLNN.o              : $(mySRCdir)/ATL_creftbmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmvLNN.c
ATL_creftrsv.o                 : $(mySRCdir)/ATL_creftrsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsv.c
ATL_creftrmv.o                 : $(mySRCdir)/ATL_creftrmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmv.c
ATL_creftpsv.o                 : $(mySRCdir)/ATL_creftpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpsv.c
ATL_creftpmv.o                 : $(mySRCdir)/ATL_creftpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftpmv.c
ATL_creftbsv.o                 : $(mySRCdir)/ATL_creftbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbsv.c
ATL_creftbmv.o                 : $(mySRCdir)/ATL_creftbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftbmv.c
#
ATL_zrefgbmv.o                 : $(mySRCdir)/ATL_zrefgbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgbmv.c
ATL_zrefgbmvN.o                : $(mySRCdir)/ATL_zrefgbmvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgbmvN.c
ATL_zrefgbmvC.o                : $(mySRCdir)/ATL_zrefgbmvC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgbmvC.c
ATL_zrefgbmvT.o                : $(mySRCdir)/ATL_zrefgbmvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgbmvT.c
ATL_zrefgbmvH.o                : $(mySRCdir)/ATL_zrefgbmvH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgbmvH.c
ATL_zrefgpmv.o                 : $(mySRCdir)/ATL_zrefgpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmv.c
ATL_zrefgpmvUN.o               : $(mySRCdir)/ATL_zrefgpmvUN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvUN.c
ATL_zrefgpmvUC.o               : $(mySRCdir)/ATL_zrefgpmvUC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvUC.c
ATL_zrefgpmvUT.o               : $(mySRCdir)/ATL_zrefgpmvUT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvUT.c
ATL_zrefgpmvUH.o               : $(mySRCdir)/ATL_zrefgpmvUH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvUH.c
ATL_zrefgpmvLN.o               : $(mySRCdir)/ATL_zrefgpmvLN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvLN.c
ATL_zrefgpmvLC.o               : $(mySRCdir)/ATL_zrefgpmvLC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvLC.c
ATL_zrefgpmvLT.o               : $(mySRCdir)/ATL_zrefgpmvLT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvLT.c
ATL_zrefgpmvLH.o               : $(mySRCdir)/ATL_zrefgpmvLH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpmvLH.c
ATL_zrefgemv.o                 : $(mySRCdir)/ATL_zrefgemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemv.c
ATL_zrefgemvN.o                : $(mySRCdir)/ATL_zrefgemvN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemvN.c
ATL_zrefgemvC.o                : $(mySRCdir)/ATL_zrefgemvC.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemvC.c
ATL_zrefgemvT.o                : $(mySRCdir)/ATL_zrefgemvT.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemvT.c
ATL_zrefgemvH.o                : $(mySRCdir)/ATL_zrefgemvH.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemvH.c
ATL_zrefgprc.o                 : $(mySRCdir)/ATL_zrefgprc.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgprc.c
ATL_zrefgpru.o                 : $(mySRCdir)/ATL_zrefgpru.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgpru.c
ATL_zrefgerc.o                 : $(mySRCdir)/ATL_zrefgerc.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgerc.c
ATL_zrefgeru.o                 : $(mySRCdir)/ATL_zrefgeru.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgeru.c
ATL_zrefhemv.o                 : $(mySRCdir)/ATL_zrefhemv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemv.c
ATL_zrefhemvL.o                : $(mySRCdir)/ATL_zrefhemvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemvL.c
ATL_zrefhemvU.o                : $(mySRCdir)/ATL_zrefhemvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemvU.c
ATL_zrefhpmv.o                 : $(mySRCdir)/ATL_zrefhpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpmv.c
ATL_zrefhpmvL.o                : $(mySRCdir)/ATL_zrefhpmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpmvL.c
ATL_zrefhpmvU.o                : $(mySRCdir)/ATL_zrefhpmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpmvU.c
ATL_zrefhbmv.o                 : $(mySRCdir)/ATL_zrefhbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhbmv.c
ATL_zrefhbmvL.o                : $(mySRCdir)/ATL_zrefhbmvL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhbmvL.c
ATL_zrefhbmvU.o                : $(mySRCdir)/ATL_zrefhbmvU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhbmvU.c
ATL_zrefhprU.o                 : $(mySRCdir)/ATL_zrefhprU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhprU.c
ATL_zrefhprL.o                 : $(mySRCdir)/ATL_zrefhprL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhprL.c
ATL_zrefhpr.o                  : $(mySRCdir)/ATL_zrefhpr.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpr.c
ATL_zrefherU.o                 : $(mySRCdir)/ATL_zrefherU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherU.c
ATL_zrefherL.o                 : $(mySRCdir)/ATL_zrefherL.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherL.c
ATL_zrefher.o                  : $(mySRCdir)/ATL_zrefher.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher.c
ATL_zrefhpr2U.o                : $(mySRCdir)/ATL_zrefhpr2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpr2U.c
ATL_zrefhpr2L.o                : $(mySRCdir)/ATL_zrefhpr2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpr2L.c
ATL_zrefhpr2.o                 : $(mySRCdir)/ATL_zrefhpr2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhpr2.c
ATL_zrefher2U.o                : $(mySRCdir)/ATL_zrefher2U.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2U.c
ATL_zrefher2L.o                : $(mySRCdir)/ATL_zrefher2L.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2L.c
ATL_zrefher2.o                 : $(mySRCdir)/ATL_zrefher2.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2.c
ATL_zreftrsvUHU.o              : $(mySRCdir)/ATL_zreftrsvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUHU.c
ATL_zreftrmvUHU.o              : $(mySRCdir)/ATL_zreftrmvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUHU.c
ATL_zreftpsvUHU.o              : $(mySRCdir)/ATL_zreftpsvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUHU.c
ATL_zreftpmvUHU.o              : $(mySRCdir)/ATL_zreftpmvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUHU.c
ATL_zreftbsvUHU.o              : $(mySRCdir)/ATL_zreftbsvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUHU.c
ATL_zreftbmvUHU.o              : $(mySRCdir)/ATL_zreftbmvUHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUHU.c
ATL_zreftrsvUHN.o              : $(mySRCdir)/ATL_zreftrsvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUHN.c
ATL_zreftrmvUHN.o              : $(mySRCdir)/ATL_zreftrmvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUHN.c
ATL_zreftpsvUHN.o              : $(mySRCdir)/ATL_zreftpsvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUHN.c
ATL_zreftpmvUHN.o              : $(mySRCdir)/ATL_zreftpmvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUHN.c
ATL_zreftbsvUHN.o              : $(mySRCdir)/ATL_zreftbsvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUHN.c
ATL_zreftbmvUHN.o              : $(mySRCdir)/ATL_zreftbmvUHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUHN.c
ATL_zreftrsvUTU.o              : $(mySRCdir)/ATL_zreftrsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUTU.c
ATL_zreftrmvUTU.o              : $(mySRCdir)/ATL_zreftrmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUTU.c
ATL_zreftpsvUTU.o              : $(mySRCdir)/ATL_zreftpsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUTU.c
ATL_zreftpmvUTU.o              : $(mySRCdir)/ATL_zreftpmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUTU.c
ATL_zreftbsvUTU.o              : $(mySRCdir)/ATL_zreftbsvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUTU.c
ATL_zreftbmvUTU.o              : $(mySRCdir)/ATL_zreftbmvUTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUTU.c
ATL_zreftrsvUTN.o              : $(mySRCdir)/ATL_zreftrsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUTN.c
ATL_zreftrmvUTN.o              : $(mySRCdir)/ATL_zreftrmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUTN.c
ATL_zreftpsvUTN.o              : $(mySRCdir)/ATL_zreftpsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUTN.c
ATL_zreftpmvUTN.o              : $(mySRCdir)/ATL_zreftpmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUTN.c
ATL_zreftbsvUTN.o              : $(mySRCdir)/ATL_zreftbsvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUTN.c
ATL_zreftbmvUTN.o              : $(mySRCdir)/ATL_zreftbmvUTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUTN.c
ATL_zreftrsvUCU.o              : $(mySRCdir)/ATL_zreftrsvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUCU.c
ATL_zreftrmvUCU.o              : $(mySRCdir)/ATL_zreftrmvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUCU.c
ATL_zreftpsvUCU.o              : $(mySRCdir)/ATL_zreftpsvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUCU.c
ATL_zreftpmvUCU.o              : $(mySRCdir)/ATL_zreftpmvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUCU.c
ATL_zreftbsvUCU.o              : $(mySRCdir)/ATL_zreftbsvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUCU.c
ATL_zreftbmvUCU.o              : $(mySRCdir)/ATL_zreftbmvUCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUCU.c
ATL_zreftrsvUCN.o              : $(mySRCdir)/ATL_zreftrsvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUCN.c
ATL_zreftrmvUCN.o              : $(mySRCdir)/ATL_zreftrmvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUCN.c
ATL_zreftpsvUCN.o              : $(mySRCdir)/ATL_zreftpsvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUCN.c
ATL_zreftpmvUCN.o              : $(mySRCdir)/ATL_zreftpmvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUCN.c
ATL_zreftbsvUCN.o              : $(mySRCdir)/ATL_zreftbsvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUCN.c
ATL_zreftbmvUCN.o              : $(mySRCdir)/ATL_zreftbmvUCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUCN.c
ATL_zreftrsvUNU.o              : $(mySRCdir)/ATL_zreftrsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUNU.c
ATL_zreftrmvUNU.o              : $(mySRCdir)/ATL_zreftrmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUNU.c
ATL_zreftpsvUNU.o              : $(mySRCdir)/ATL_zreftpsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUNU.c
ATL_zreftpmvUNU.o              : $(mySRCdir)/ATL_zreftpmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUNU.c
ATL_zreftbsvUNU.o              : $(mySRCdir)/ATL_zreftbsvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUNU.c
ATL_zreftbmvUNU.o              : $(mySRCdir)/ATL_zreftbmvUNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUNU.c
ATL_zreftrsvUNN.o              : $(mySRCdir)/ATL_zreftrsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvUNN.c
ATL_zreftrmvUNN.o              : $(mySRCdir)/ATL_zreftrmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvUNN.c
ATL_zreftpsvUNN.o              : $(mySRCdir)/ATL_zreftpsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvUNN.c
ATL_zreftpmvUNN.o              : $(mySRCdir)/ATL_zreftpmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvUNN.c
ATL_zreftbsvUNN.o              : $(mySRCdir)/ATL_zreftbsvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvUNN.c
ATL_zreftbmvUNN.o              : $(mySRCdir)/ATL_zreftbmvUNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvUNN.c
ATL_zreftrsvLHU.o              : $(mySRCdir)/ATL_zreftrsvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLHU.c
ATL_zreftrmvLHU.o              : $(mySRCdir)/ATL_zreftrmvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLHU.c
ATL_zreftpsvLHU.o              : $(mySRCdir)/ATL_zreftpsvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLHU.c
ATL_zreftpmvLHU.o              : $(mySRCdir)/ATL_zreftpmvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLHU.c
ATL_zreftbsvLHU.o              : $(mySRCdir)/ATL_zreftbsvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLHU.c
ATL_zreftbmvLHU.o              : $(mySRCdir)/ATL_zreftbmvLHU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLHU.c
ATL_zreftrsvLHN.o              : $(mySRCdir)/ATL_zreftrsvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLHN.c
ATL_zreftrmvLHN.o              : $(mySRCdir)/ATL_zreftrmvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLHN.c
ATL_zreftpsvLHN.o              : $(mySRCdir)/ATL_zreftpsvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLHN.c
ATL_zreftpmvLHN.o              : $(mySRCdir)/ATL_zreftpmvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLHN.c
ATL_zreftbsvLHN.o              : $(mySRCdir)/ATL_zreftbsvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLHN.c
ATL_zreftbmvLHN.o              : $(mySRCdir)/ATL_zreftbmvLHN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLHN.c
ATL_zreftrsvLTU.o              : $(mySRCdir)/ATL_zreftrsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLTU.c
ATL_zreftrmvLTU.o              : $(mySRCdir)/ATL_zreftrmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLTU.c
ATL_zreftpsvLTU.o              : $(mySRCdir)/ATL_zreftpsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLTU.c
ATL_zreftpmvLTU.o              : $(mySRCdir)/ATL_zreftpmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLTU.c
ATL_zreftbsvLTU.o              : $(mySRCdir)/ATL_zreftbsvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLTU.c
ATL_zreftbmvLTU.o              : $(mySRCdir)/ATL_zreftbmvLTU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLTU.c
ATL_zreftrsvLTN.o              : $(mySRCdir)/ATL_zreftrsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLTN.c
ATL_zreftrmvLTN.o              : $(mySRCdir)/ATL_zreftrmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLTN.c
ATL_zreftpsvLTN.o              : $(mySRCdir)/ATL_zreftpsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLTN.c
ATL_zreftpmvLTN.o              : $(mySRCdir)/ATL_zreftpmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLTN.c
ATL_zreftbsvLTN.o              : $(mySRCdir)/ATL_zreftbsvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLTN.c
ATL_zreftbmvLTN.o              : $(mySRCdir)/ATL_zreftbmvLTN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLTN.c
ATL_zreftrsvLCU.o              : $(mySRCdir)/ATL_zreftrsvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLCU.c
ATL_zreftrmvLCU.o              : $(mySRCdir)/ATL_zreftrmvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLCU.c
ATL_zreftpsvLCU.o              : $(mySRCdir)/ATL_zreftpsvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLCU.c
ATL_zreftpmvLCU.o              : $(mySRCdir)/ATL_zreftpmvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLCU.c
ATL_zreftbsvLCU.o              : $(mySRCdir)/ATL_zreftbsvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLCU.c
ATL_zreftbmvLCU.o              : $(mySRCdir)/ATL_zreftbmvLCU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLCU.c
ATL_zreftrsvLCN.o              : $(mySRCdir)/ATL_zreftrsvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLCN.c
ATL_zreftrmvLCN.o              : $(mySRCdir)/ATL_zreftrmvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLCN.c
ATL_zreftpsvLCN.o              : $(mySRCdir)/ATL_zreftpsvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLCN.c
ATL_zreftpmvLCN.o              : $(mySRCdir)/ATL_zreftpmvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLCN.c
ATL_zreftbsvLCN.o              : $(mySRCdir)/ATL_zreftbsvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLCN.c
ATL_zreftbmvLCN.o              : $(mySRCdir)/ATL_zreftbmvLCN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLCN.c
ATL_zreftrsvLNU.o              : $(mySRCdir)/ATL_zreftrsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLNU.c
ATL_zreftrmvLNU.o              : $(mySRCdir)/ATL_zreftrmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLNU.c
ATL_zreftpsvLNU.o              : $(mySRCdir)/ATL_zreftpsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLNU.c
ATL_zreftpmvLNU.o              : $(mySRCdir)/ATL_zreftpmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLNU.c
ATL_zreftbsvLNU.o              : $(mySRCdir)/ATL_zreftbsvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLNU.c
ATL_zreftbmvLNU.o              : $(mySRCdir)/ATL_zreftbmvLNU.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLNU.c
ATL_zreftrsvLNN.o              : $(mySRCdir)/ATL_zreftrsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsvLNN.c
ATL_zreftrmvLNN.o              : $(mySRCdir)/ATL_zreftrmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmvLNN.c
ATL_zreftpsvLNN.o              : $(mySRCdir)/ATL_zreftpsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsvLNN.c
ATL_zreftpmvLNN.o              : $(mySRCdir)/ATL_zreftpmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmvLNN.c
ATL_zreftbsvLNN.o              : $(mySRCdir)/ATL_zreftbsvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsvLNN.c
ATL_zreftbmvLNN.o              : $(mySRCdir)/ATL_zreftbmvLNN.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmvLNN.c
ATL_zreftrsv.o                 : $(mySRCdir)/ATL_zreftrsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsv.c
ATL_zreftrmv.o                 : $(mySRCdir)/ATL_zreftrmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmv.c
ATL_zreftpsv.o                 : $(mySRCdir)/ATL_zreftpsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpsv.c
ATL_zreftpmv.o                 : $(mySRCdir)/ATL_zreftpmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftpmv.c
ATL_zreftbsv.o                 : $(mySRCdir)/ATL_zreftbsv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbsv.c
ATL_zreftbmv.o                 : $(mySRCdir)/ATL_zreftbmv.c $(INC2dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftbmv.c
#
########################################################################
#
sclean   :
	rm -f $(ATL_slvl2obj) s*.grd
dclean   :
	rm -f $(ATL_dlvl2obj) d*.grd
cclean   :
	rm -f $(ATL_clvl2obj) c*.grd
zclean   :
	rm -f $(ATL_zlvl2obj) z*.grd
#
clean    :
	rm -f *.o *.grd
#
########################################################################
