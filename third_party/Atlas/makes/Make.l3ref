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
mySRCdir = $(SRCdir)/src/blas/reference/level3
#
########################################################################
#
REFBLASlib = $(ATLASlib)
INC3dep    = $(INCSdir)/atlas_enum.h    $(INCSdir)/atlas_refmisc.h \
             $(INCSdir)/atlas_reflvl3.h $(INCSdir)/atlas_reflevel3.h
#
## Object files ########################################################
#
ATL_slvl3obj = \
   ATL_srefgemm.o ATL_srefgemmNN.o ATL_srefgemmNT.o ATL_srefgemmTN.o \
   ATL_srefgemmTT.o ATL_srefsymm.o ATL_srefsymmLL.o ATL_srefsymmLU.o \
   ATL_srefsymmRL.o ATL_srefsymmRU.o ATL_srefsyr2k.o ATL_srefsyr2kLN.o \
   ATL_srefsyr2kLT.o ATL_srefsyr2kUN.o ATL_srefsyr2kUT.o ATL_srefsyrk.o \
   ATL_srefsyrkLN.o ATL_srefsyrkLT.o ATL_srefsyrkUN.o ATL_srefsyrkUT.o \
   ATL_sreftrmm.o ATL_sreftrmmLLNN.o ATL_sreftrmmLLNU.o ATL_sreftrmmLLTN.o \
   ATL_sreftrmmLLTU.o ATL_sreftrmmLUNN.o ATL_sreftrmmLUNU.o \
   ATL_sreftrmmLUTN.o ATL_sreftrmmLUTU.o ATL_sreftrmmRLNN.o \
   ATL_sreftrmmRLNU.o ATL_sreftrmmRLTN.o ATL_sreftrmmRLTU.o \
   ATL_sreftrmmRUNN.o ATL_sreftrmmRUNU.o ATL_sreftrmmRUTN.o \
   ATL_sreftrmmRUTU.o ATL_sreftrsm.o ATL_sreftrsmLLNN.o ATL_sreftrsmLLNU.o \
   ATL_sreftrsmLLTN.o ATL_sreftrsmLLTU.o ATL_sreftrsmLUNN.o \
   ATL_sreftrsmLUNU.o ATL_sreftrsmLUTN.o ATL_sreftrsmLUTU.o \
   ATL_sreftrsmRLNN.o ATL_sreftrsmRLNU.o ATL_sreftrsmRLTN.o \
   ATL_sreftrsmRLTU.o ATL_sreftrsmRUNN.o ATL_sreftrsmRUNU.o \
   ATL_sreftrsmRUTN.o ATL_sreftrsmRUTU.o
#
ATL_dlvl3obj = \
   ATL_drefgemm.o ATL_drefgemmNN.o ATL_drefgemmNT.o ATL_drefgemmTN.o \
   ATL_drefgemmTT.o ATL_drefsymm.o ATL_drefsymmLL.o ATL_drefsymmLU.o \
   ATL_drefsymmRL.o ATL_drefsymmRU.o ATL_drefsyr2k.o ATL_drefsyr2kLN.o \
   ATL_drefsyr2kLT.o ATL_drefsyr2kUN.o ATL_drefsyr2kUT.o ATL_drefsyrk.o \
   ATL_drefsyrkLN.o ATL_drefsyrkLT.o ATL_drefsyrkUN.o ATL_drefsyrkUT.o \
   ATL_dreftrmm.o ATL_dreftrmmLLNN.o ATL_dreftrmmLLNU.o ATL_dreftrmmLLTN.o \
   ATL_dreftrmmLLTU.o ATL_dreftrmmLUNN.o ATL_dreftrmmLUNU.o \
   ATL_dreftrmmLUTN.o ATL_dreftrmmLUTU.o ATL_dreftrmmRLNN.o \
   ATL_dreftrmmRLNU.o ATL_dreftrmmRLTN.o ATL_dreftrmmRLTU.o \
   ATL_dreftrmmRUNN.o ATL_dreftrmmRUNU.o ATL_dreftrmmRUTN.o \
   ATL_dreftrmmRUTU.o ATL_dreftrsm.o ATL_dreftrsmLLNN.o ATL_dreftrsmLLNU.o \
   ATL_dreftrsmLLTN.o ATL_dreftrsmLLTU.o ATL_dreftrsmLUNN.o \
   ATL_dreftrsmLUNU.o ATL_dreftrsmLUTN.o ATL_dreftrsmLUTU.o \
   ATL_dreftrsmRLNN.o ATL_dreftrsmRLNU.o ATL_dreftrsmRLTN.o \
   ATL_dreftrsmRLTU.o ATL_dreftrsmRUNN.o ATL_dreftrsmRUNU.o \
   ATL_dreftrsmRUTN.o ATL_dreftrsmRUTU.o
#
ATL_clvl3obj = \
   ATL_crefgemm.o ATL_crefgemmCC.o ATL_crefgemmCN.o ATL_crefgemmCT.o \
   ATL_crefgemmNC.o ATL_crefgemmNN.o ATL_crefgemmNT.o ATL_crefgemmTC.o \
   ATL_crefgemmTN.o ATL_crefgemmTT.o ATL_crefhemm.o ATL_crefhemmLL.o \
   ATL_crefhemmLU.o ATL_crefhemmRL.o ATL_crefhemmRU.o ATL_crefher2k.o \
   ATL_crefher2kLC.o ATL_crefher2kLN.o ATL_crefher2kUC.o ATL_crefher2kUN.o \
   ATL_crefherk.o ATL_crefherkLC.o ATL_crefherkLN.o ATL_crefherkUC.o \
   ATL_crefherkUN.o ATL_crefsymm.o ATL_crefsymmLL.o ATL_crefsymmLU.o \
   ATL_crefsymmRL.o ATL_crefsymmRU.o ATL_crefsyr2k.o ATL_crefsyr2kLN.o \
   ATL_crefsyr2kLT.o ATL_crefsyr2kUN.o ATL_crefsyr2kUT.o ATL_crefsyrk.o \
   ATL_crefsyrkLN.o ATL_crefsyrkLT.o ATL_crefsyrkUN.o ATL_crefsyrkUT.o \
   ATL_creftrmm.o ATL_creftrmmLLCN.o ATL_creftrmmLLCU.o ATL_creftrmmLLNN.o \
   ATL_creftrmmLLNU.o ATL_creftrmmLLTN.o ATL_creftrmmLLTU.o \
   ATL_creftrmmLUCN.o ATL_creftrmmLUCU.o ATL_creftrmmLUNN.o \
   ATL_creftrmmLUNU.o ATL_creftrmmLUTN.o ATL_creftrmmLUTU.o \
   ATL_creftrmmRLCN.o ATL_creftrmmRLCU.o ATL_creftrmmRLNN.o \
   ATL_creftrmmRLNU.o ATL_creftrmmRLTN.o ATL_creftrmmRLTU.o \
   ATL_creftrmmRUCN.o ATL_creftrmmRUCU.o ATL_creftrmmRUNN.o \
   ATL_creftrmmRUNU.o ATL_creftrmmRUTN.o ATL_creftrmmRUTU.o ATL_creftrsm.o \
   ATL_creftrsmLLCN.o ATL_creftrsmLLCU.o ATL_creftrsmLLNN.o \
   ATL_creftrsmLLNU.o ATL_creftrsmLLTN.o ATL_creftrsmLLTU.o \
   ATL_creftrsmLUCN.o ATL_creftrsmLUCU.o ATL_creftrsmLUNN.o \
   ATL_creftrsmLUNU.o ATL_creftrsmLUTN.o ATL_creftrsmLUTU.o \
   ATL_creftrsmRLCN.o ATL_creftrsmRLCU.o ATL_creftrsmRLNN.o \
   ATL_creftrsmRLNU.o ATL_creftrsmRLTN.o ATL_creftrsmRLTU.o \
   ATL_creftrsmRUCN.o ATL_creftrsmRUCU.o ATL_creftrsmRUNN.o \
   ATL_creftrsmRUNU.o ATL_creftrsmRUTN.o ATL_creftrsmRUTU.o
#
ATL_zlvl3obj = \
   ATL_zrefgemm.o ATL_zrefgemmCC.o ATL_zrefgemmCN.o ATL_zrefgemmCT.o \
   ATL_zrefgemmNC.o ATL_zrefgemmNN.o ATL_zrefgemmNT.o ATL_zrefgemmTC.o \
   ATL_zrefgemmTN.o ATL_zrefgemmTT.o ATL_zrefhemm.o ATL_zrefhemmLL.o \
   ATL_zrefhemmLU.o ATL_zrefhemmRL.o ATL_zrefhemmRU.o ATL_zrefher2k.o \
   ATL_zrefher2kLC.o ATL_zrefher2kLN.o ATL_zrefher2kUC.o ATL_zrefher2kUN.o \
   ATL_zrefherk.o ATL_zrefherkLC.o ATL_zrefherkLN.o ATL_zrefherkUC.o \
   ATL_zrefherkUN.o ATL_zrefsymm.o ATL_zrefsymmLL.o ATL_zrefsymmLU.o \
   ATL_zrefsymmRL.o ATL_zrefsymmRU.o ATL_zrefsyr2k.o ATL_zrefsyr2kLN.o \
   ATL_zrefsyr2kLT.o ATL_zrefsyr2kUN.o ATL_zrefsyr2kUT.o ATL_zrefsyrk.o \
   ATL_zrefsyrkLN.o ATL_zrefsyrkLT.o ATL_zrefsyrkUN.o ATL_zrefsyrkUT.o \
   ATL_zreftrmm.o ATL_zreftrmmLLCN.o ATL_zreftrmmLLCU.o ATL_zreftrmmLLNN.o \
   ATL_zreftrmmLLNU.o ATL_zreftrmmLLTN.o ATL_zreftrmmLLTU.o \
   ATL_zreftrmmLUCN.o ATL_zreftrmmLUCU.o ATL_zreftrmmLUNN.o \
   ATL_zreftrmmLUNU.o ATL_zreftrmmLUTN.o ATL_zreftrmmLUTU.o \
   ATL_zreftrmmRLCN.o ATL_zreftrmmRLCU.o ATL_zreftrmmRLNN.o \
   ATL_zreftrmmRLNU.o ATL_zreftrmmRLTN.o ATL_zreftrmmRLTU.o \
   ATL_zreftrmmRUCN.o ATL_zreftrmmRUCU.o ATL_zreftrmmRUNN.o \
   ATL_zreftrmmRUNU.o ATL_zreftrmmRUTN.o ATL_zreftrmmRUTU.o ATL_zreftrsm.o \
   ATL_zreftrsmLLCN.o ATL_zreftrsmLLCU.o ATL_zreftrsmLLNN.o \
   ATL_zreftrsmLLNU.o ATL_zreftrsmLLTN.o ATL_zreftrsmLLTU.o \
   ATL_zreftrsmLUCN.o ATL_zreftrsmLUCU.o ATL_zreftrsmLUNN.o \
   ATL_zreftrsmLUNU.o ATL_zreftrsmLUTN.o ATL_zreftrsmLUTU.o \
   ATL_zreftrsmRLCN.o ATL_zreftrsmRLCU.o ATL_zreftrsmRLNN.o \
   ATL_zreftrsmRLNU.o ATL_zreftrsmRLTN.o ATL_zreftrsmRLTU.o \
   ATL_zreftrsmRUCN.o ATL_zreftrsmRUCU.o ATL_zreftrsmRUNN.o \
   ATL_zreftrsmRUNU.o ATL_zreftrsmRUTN.o ATL_zreftrsmRUTU.o
#
## Targets #############################################################
#
all   : lib
#
lvl3  : lib
#
lib   : slib dlib clib zlib
#
slvl3 : slib
dlvl3 : dlib
clvl3 : clib
zlvl3 : zlib
#
########################################################################
#
slib  : slib.grd
dlib  : dlib.grd
clib  : clib.grd
zlib  : zlib.grd
#
slib.grd  : $(ATL_slvl3obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_slvl3obj)
	$(RANLIB) $(REFBLASlib)
	touch slib.grd
#
dlib.grd  : $(ATL_dlvl3obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_dlvl3obj)
	$(RANLIB) $(REFBLASlib)
	touch dlib.grd
#
clib.grd  : $(ATL_clvl3obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_clvl3obj)
	$(RANLIB) $(REFBLASlib)
	touch clib.grd
#
zlib.grd  : $(ATL_zlvl3obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_zlvl3obj)
	$(RANLIB) $(REFBLASlib)
	touch zlib.grd
#
########################################################################
#
ATL_srefgemm.o                     : $(mySRCdir)/ATL_srefgemm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemm.c
ATL_srefgemmNN.o                   : $(mySRCdir)/ATL_srefgemmNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemmNN.c
ATL_srefgemmTN.o                   : $(mySRCdir)/ATL_srefgemmTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemmTN.c
ATL_srefgemmNT.o                   : $(mySRCdir)/ATL_srefgemmNT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemmNT.c
ATL_srefgemmTT.o                   : $(mySRCdir)/ATL_srefgemmTT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefgemmTT.c
ATL_srefsymm.o                     : $(mySRCdir)/ATL_srefsymm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymm.c
ATL_srefsymmRU.o                   : $(mySRCdir)/ATL_srefsymmRU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymmRU.c
ATL_srefsymmRL.o                   : $(mySRCdir)/ATL_srefsymmRL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymmRL.c
ATL_srefsymmLU.o                   : $(mySRCdir)/ATL_srefsymmLU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymmLU.c
ATL_srefsymmLL.o                   : $(mySRCdir)/ATL_srefsymmLL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsymmLL.c
ATL_srefsyrk.o                     : $(mySRCdir)/ATL_srefsyrk.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrk.c
ATL_srefsyrkUT.o                   : $(mySRCdir)/ATL_srefsyrkUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrkUT.c
ATL_srefsyrkUN.o                   : $(mySRCdir)/ATL_srefsyrkUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrkUN.c
ATL_srefsyrkLT.o                   : $(mySRCdir)/ATL_srefsyrkLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrkLT.c
ATL_srefsyrkLN.o                   : $(mySRCdir)/ATL_srefsyrkLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyrkLN.c
ATL_srefsyr2k.o                    : $(mySRCdir)/ATL_srefsyr2k.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2k.c
ATL_srefsyr2kUT.o                  : $(mySRCdir)/ATL_srefsyr2kUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2kUT.c
ATL_srefsyr2kUN.o                  : $(mySRCdir)/ATL_srefsyr2kUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2kUN.c
ATL_srefsyr2kLT.o                  : $(mySRCdir)/ATL_srefsyr2kLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2kLT.c
ATL_srefsyr2kLN.o                  : $(mySRCdir)/ATL_srefsyr2kLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefsyr2kLN.c
ATL_sreftrmm.o                     : $(mySRCdir)/ATL_sreftrmm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmm.c
ATL_sreftrsm.o                     : $(mySRCdir)/ATL_sreftrsm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsm.c
ATL_sreftrsmRUTU.o                 : $(mySRCdir)/ATL_sreftrsmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRUTU.c
ATL_sreftrsmRUTN.o                 : $(mySRCdir)/ATL_sreftrsmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRUTN.c
ATL_sreftrmmRUTU.o                 : $(mySRCdir)/ATL_sreftrmmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRUTU.c
ATL_sreftrmmRUTN.o                 : $(mySRCdir)/ATL_sreftrmmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRUTN.c
ATL_sreftrsmRUNU.o                 : $(mySRCdir)/ATL_sreftrsmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRUNU.c
ATL_sreftrsmRUNN.o                 : $(mySRCdir)/ATL_sreftrsmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRUNN.c
ATL_sreftrmmRUNU.o                 : $(mySRCdir)/ATL_sreftrmmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRUNU.c
ATL_sreftrmmRUNN.o                 : $(mySRCdir)/ATL_sreftrmmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRUNN.c
ATL_sreftrsmRLTU.o                 : $(mySRCdir)/ATL_sreftrsmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRLTU.c
ATL_sreftrsmRLTN.o                 : $(mySRCdir)/ATL_sreftrsmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRLTN.c
ATL_sreftrmmRLTU.o                 : $(mySRCdir)/ATL_sreftrmmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRLTU.c
ATL_sreftrmmRLTN.o                 : $(mySRCdir)/ATL_sreftrmmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRLTN.c
ATL_sreftrsmRLNU.o                 : $(mySRCdir)/ATL_sreftrsmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRLNU.c
ATL_sreftrsmRLNN.o                 : $(mySRCdir)/ATL_sreftrsmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmRLNN.c
ATL_sreftrmmRLNU.o                 : $(mySRCdir)/ATL_sreftrmmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRLNU.c
ATL_sreftrmmRLNN.o                 : $(mySRCdir)/ATL_sreftrmmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmRLNN.c
ATL_sreftrsmLUTU.o                 : $(mySRCdir)/ATL_sreftrsmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLUTU.c
ATL_sreftrsmLUTN.o                 : $(mySRCdir)/ATL_sreftrsmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLUTN.c
ATL_sreftrmmLUTU.o                 : $(mySRCdir)/ATL_sreftrmmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLUTU.c
ATL_sreftrmmLUTN.o                 : $(mySRCdir)/ATL_sreftrmmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLUTN.c
ATL_sreftrsmLUNU.o                 : $(mySRCdir)/ATL_sreftrsmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLUNU.c
ATL_sreftrsmLUNN.o                 : $(mySRCdir)/ATL_sreftrsmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLUNN.c
ATL_sreftrmmLUNU.o                 : $(mySRCdir)/ATL_sreftrmmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLUNU.c
ATL_sreftrmmLUNN.o                 : $(mySRCdir)/ATL_sreftrmmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLUNN.c
ATL_sreftrsmLLTU.o                 : $(mySRCdir)/ATL_sreftrsmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLLTU.c
ATL_sreftrsmLLTN.o                 : $(mySRCdir)/ATL_sreftrsmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLLTN.c
ATL_sreftrmmLLTU.o                 : $(mySRCdir)/ATL_sreftrmmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLLTU.c
ATL_sreftrmmLLTN.o                 : $(mySRCdir)/ATL_sreftrmmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLLTN.c
ATL_sreftrsmLLNU.o                 : $(mySRCdir)/ATL_sreftrsmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLLNU.c
ATL_sreftrsmLLNN.o                 : $(mySRCdir)/ATL_sreftrsmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrsmLLNN.c
ATL_sreftrmmLLNU.o                 : $(mySRCdir)/ATL_sreftrmmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLLNU.c
ATL_sreftrmmLLNN.o                 : $(mySRCdir)/ATL_sreftrmmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sreftrmmLLNN.c
#
ATL_drefgemm.o                     : $(mySRCdir)/ATL_drefgemm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemm.c
ATL_drefgemmNN.o                   : $(mySRCdir)/ATL_drefgemmNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemmNN.c
ATL_drefgemmTN.o                   : $(mySRCdir)/ATL_drefgemmTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemmTN.c
ATL_drefgemmNT.o                   : $(mySRCdir)/ATL_drefgemmNT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemmNT.c
ATL_drefgemmTT.o                   : $(mySRCdir)/ATL_drefgemmTT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefgemmTT.c
ATL_drefsymm.o                     : $(mySRCdir)/ATL_drefsymm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymm.c
ATL_drefsymmRU.o                   : $(mySRCdir)/ATL_drefsymmRU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymmRU.c
ATL_drefsymmRL.o                   : $(mySRCdir)/ATL_drefsymmRL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymmRL.c
ATL_drefsymmLU.o                   : $(mySRCdir)/ATL_drefsymmLU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymmLU.c
ATL_drefsymmLL.o                   : $(mySRCdir)/ATL_drefsymmLL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsymmLL.c
ATL_drefsyrk.o                     : $(mySRCdir)/ATL_drefsyrk.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrk.c
ATL_drefsyrkUT.o                   : $(mySRCdir)/ATL_drefsyrkUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrkUT.c
ATL_drefsyrkUN.o                   : $(mySRCdir)/ATL_drefsyrkUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrkUN.c
ATL_drefsyrkLT.o                   : $(mySRCdir)/ATL_drefsyrkLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrkLT.c
ATL_drefsyrkLN.o                   : $(mySRCdir)/ATL_drefsyrkLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyrkLN.c
ATL_drefsyr2k.o                    : $(mySRCdir)/ATL_drefsyr2k.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2k.c
ATL_drefsyr2kUT.o                  : $(mySRCdir)/ATL_drefsyr2kUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2kUT.c
ATL_drefsyr2kUN.o                  : $(mySRCdir)/ATL_drefsyr2kUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2kUN.c
ATL_drefsyr2kLT.o                  : $(mySRCdir)/ATL_drefsyr2kLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2kLT.c
ATL_drefsyr2kLN.o                  : $(mySRCdir)/ATL_drefsyr2kLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefsyr2kLN.c
ATL_dreftrmm.o                     : $(mySRCdir)/ATL_dreftrmm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmm.c
ATL_dreftrsm.o                     : $(mySRCdir)/ATL_dreftrsm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsm.c
ATL_dreftrsmRUTU.o                 : $(mySRCdir)/ATL_dreftrsmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRUTU.c
ATL_dreftrsmRUTN.o                 : $(mySRCdir)/ATL_dreftrsmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRUTN.c
ATL_dreftrmmRUTU.o                 : $(mySRCdir)/ATL_dreftrmmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRUTU.c
ATL_dreftrmmRUTN.o                 : $(mySRCdir)/ATL_dreftrmmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRUTN.c
ATL_dreftrsmRUNU.o                 : $(mySRCdir)/ATL_dreftrsmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRUNU.c
ATL_dreftrsmRUNN.o                 : $(mySRCdir)/ATL_dreftrsmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRUNN.c
ATL_dreftrmmRUNU.o                 : $(mySRCdir)/ATL_dreftrmmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRUNU.c
ATL_dreftrmmRUNN.o                 : $(mySRCdir)/ATL_dreftrmmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRUNN.c
ATL_dreftrsmRLTU.o                 : $(mySRCdir)/ATL_dreftrsmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRLTU.c
ATL_dreftrsmRLTN.o                 : $(mySRCdir)/ATL_dreftrsmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRLTN.c
ATL_dreftrmmRLTU.o                 : $(mySRCdir)/ATL_dreftrmmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRLTU.c
ATL_dreftrmmRLTN.o                 : $(mySRCdir)/ATL_dreftrmmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRLTN.c
ATL_dreftrsmRLNU.o                 : $(mySRCdir)/ATL_dreftrsmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRLNU.c
ATL_dreftrsmRLNN.o                 : $(mySRCdir)/ATL_dreftrsmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmRLNN.c
ATL_dreftrmmRLNU.o                 : $(mySRCdir)/ATL_dreftrmmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRLNU.c
ATL_dreftrmmRLNN.o                 : $(mySRCdir)/ATL_dreftrmmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmRLNN.c
ATL_dreftrsmLUTU.o                 : $(mySRCdir)/ATL_dreftrsmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLUTU.c
ATL_dreftrsmLUTN.o                 : $(mySRCdir)/ATL_dreftrsmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLUTN.c
ATL_dreftrmmLUTU.o                 : $(mySRCdir)/ATL_dreftrmmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLUTU.c
ATL_dreftrmmLUTN.o                 : $(mySRCdir)/ATL_dreftrmmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLUTN.c
ATL_dreftrsmLUNU.o                 : $(mySRCdir)/ATL_dreftrsmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLUNU.c
ATL_dreftrsmLUNN.o                 : $(mySRCdir)/ATL_dreftrsmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLUNN.c
ATL_dreftrmmLUNU.o                 : $(mySRCdir)/ATL_dreftrmmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLUNU.c
ATL_dreftrmmLUNN.o                 : $(mySRCdir)/ATL_dreftrmmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLUNN.c
ATL_dreftrsmLLTU.o                 : $(mySRCdir)/ATL_dreftrsmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLLTU.c
ATL_dreftrsmLLTN.o                 : $(mySRCdir)/ATL_dreftrsmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLLTN.c
ATL_dreftrmmLLTU.o                 : $(mySRCdir)/ATL_dreftrmmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLLTU.c
ATL_dreftrmmLLTN.o                 : $(mySRCdir)/ATL_dreftrmmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLLTN.c
ATL_dreftrsmLLNU.o                 : $(mySRCdir)/ATL_dreftrsmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLLNU.c
ATL_dreftrsmLLNN.o                 : $(mySRCdir)/ATL_dreftrsmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrsmLLNN.c
ATL_dreftrmmLLNU.o                 : $(mySRCdir)/ATL_dreftrmmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLLNU.c
ATL_dreftrmmLLNN.o                 : $(mySRCdir)/ATL_dreftrmmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dreftrmmLLNN.c
#
ATL_crefgemm.o                     : $(mySRCdir)/ATL_crefgemm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemm.c
ATL_crefgemmNN.o                   : $(mySRCdir)/ATL_crefgemmNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmNN.c
ATL_crefgemmTN.o                   : $(mySRCdir)/ATL_crefgemmTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmTN.c
ATL_crefgemmCN.o                   : $(mySRCdir)/ATL_crefgemmCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmCN.c
ATL_crefgemmNT.o                   : $(mySRCdir)/ATL_crefgemmNT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmNT.c
ATL_crefgemmNC.o                   : $(mySRCdir)/ATL_crefgemmNC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmNC.c
ATL_crefgemmTT.o                   : $(mySRCdir)/ATL_crefgemmTT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmTT.c
ATL_crefgemmCT.o                   : $(mySRCdir)/ATL_crefgemmCT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmCT.c
ATL_crefgemmTC.o                   : $(mySRCdir)/ATL_crefgemmTC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmTC.c
ATL_crefgemmCC.o                   : $(mySRCdir)/ATL_crefgemmCC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefgemmCC.c
ATL_crefhemm.o                     : $(mySRCdir)/ATL_crefhemm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemm.c
ATL_crefhemmRU.o                   : $(mySRCdir)/ATL_crefhemmRU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemmRU.c
ATL_crefhemmRL.o                   : $(mySRCdir)/ATL_crefhemmRL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemmRL.c
ATL_crefhemmLU.o                   : $(mySRCdir)/ATL_crefhemmLU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemmLU.c
ATL_crefhemmLL.o                   : $(mySRCdir)/ATL_crefhemmLL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefhemmLL.c
ATL_crefherk.o                     : $(mySRCdir)/ATL_crefherk.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherk.c
ATL_crefherkUC.o                   : $(mySRCdir)/ATL_crefherkUC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherkUC.c
ATL_crefherkUN.o                   : $(mySRCdir)/ATL_crefherkUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherkUN.c
ATL_crefherkLC.o                   : $(mySRCdir)/ATL_crefherkLC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherkLC.c
ATL_crefherkLN.o                   : $(mySRCdir)/ATL_crefherkLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefherkLN.c
ATL_crefher2k.o                    : $(mySRCdir)/ATL_crefher2k.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2k.c
ATL_crefher2kUC.o                  : $(mySRCdir)/ATL_crefher2kUC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2kUC.c
ATL_crefher2kUN.o                  : $(mySRCdir)/ATL_crefher2kUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2kUN.c
ATL_crefher2kLC.o                  : $(mySRCdir)/ATL_crefher2kLC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2kLC.c
ATL_crefher2kLN.o                  : $(mySRCdir)/ATL_crefher2kLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefher2kLN.c
ATL_crefsymm.o                     : $(mySRCdir)/ATL_crefsymm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsymm.c
ATL_crefsymmRU.o                   : $(mySRCdir)/ATL_crefsymmRU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsymmRU.c
ATL_crefsymmRL.o                   : $(mySRCdir)/ATL_crefsymmRL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsymmRL.c
ATL_crefsymmLU.o                   : $(mySRCdir)/ATL_crefsymmLU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsymmLU.c
ATL_crefsymmLL.o                   : $(mySRCdir)/ATL_crefsymmLL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsymmLL.c
ATL_crefsyrk.o                     : $(mySRCdir)/ATL_crefsyrk.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyrk.c
ATL_crefsyrkUT.o                   : $(mySRCdir)/ATL_crefsyrkUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyrkUT.c
ATL_crefsyrkUN.o                   : $(mySRCdir)/ATL_crefsyrkUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyrkUN.c
ATL_crefsyrkLT.o                   : $(mySRCdir)/ATL_crefsyrkLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyrkLT.c
ATL_crefsyrkLN.o                   : $(mySRCdir)/ATL_crefsyrkLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyrkLN.c
ATL_crefsyr2k.o                    : $(mySRCdir)/ATL_crefsyr2k.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyr2k.c
ATL_crefsyr2kUT.o                  : $(mySRCdir)/ATL_crefsyr2kUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyr2kUT.c
ATL_crefsyr2kUN.o                  : $(mySRCdir)/ATL_crefsyr2kUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyr2kUN.c
ATL_crefsyr2kLT.o                  : $(mySRCdir)/ATL_crefsyr2kLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyr2kLT.c
ATL_crefsyr2kLN.o                  : $(mySRCdir)/ATL_crefsyr2kLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefsyr2kLN.c
ATL_creftrmm.o                     : $(mySRCdir)/ATL_creftrmm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmm.c
ATL_creftrsm.o                     : $(mySRCdir)/ATL_creftrsm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsm.c
ATL_creftrsmRUCU.o                 : $(mySRCdir)/ATL_creftrsmRUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRUCU.c
ATL_creftrsmRUCN.o                 : $(mySRCdir)/ATL_creftrsmRUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRUCN.c
ATL_creftrmmRUCU.o                 : $(mySRCdir)/ATL_creftrmmRUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRUCU.c
ATL_creftrmmRUCN.o                 : $(mySRCdir)/ATL_creftrmmRUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRUCN.c
ATL_creftrsmRUTU.o                 : $(mySRCdir)/ATL_creftrsmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRUTU.c
ATL_creftrsmRUTN.o                 : $(mySRCdir)/ATL_creftrsmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRUTN.c
ATL_creftrmmRUTU.o                 : $(mySRCdir)/ATL_creftrmmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRUTU.c
ATL_creftrmmRUTN.o                 : $(mySRCdir)/ATL_creftrmmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRUTN.c
ATL_creftrsmRUNU.o                 : $(mySRCdir)/ATL_creftrsmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRUNU.c
ATL_creftrsmRUNN.o                 : $(mySRCdir)/ATL_creftrsmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRUNN.c
ATL_creftrmmRUNU.o                 : $(mySRCdir)/ATL_creftrmmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRUNU.c
ATL_creftrmmRUNN.o                 : $(mySRCdir)/ATL_creftrmmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRUNN.c
ATL_creftrsmRLCU.o                 : $(mySRCdir)/ATL_creftrsmRLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRLCU.c
ATL_creftrsmRLCN.o                 : $(mySRCdir)/ATL_creftrsmRLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRLCN.c
ATL_creftrmmRLCU.o                 : $(mySRCdir)/ATL_creftrmmRLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRLCU.c
ATL_creftrmmRLCN.o                 : $(mySRCdir)/ATL_creftrmmRLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRLCN.c
ATL_creftrsmRLTU.o                 : $(mySRCdir)/ATL_creftrsmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRLTU.c
ATL_creftrsmRLTN.o                 : $(mySRCdir)/ATL_creftrsmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRLTN.c
ATL_creftrmmRLTU.o                 : $(mySRCdir)/ATL_creftrmmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRLTU.c
ATL_creftrmmRLTN.o                 : $(mySRCdir)/ATL_creftrmmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRLTN.c
ATL_creftrsmRLNU.o                 : $(mySRCdir)/ATL_creftrsmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRLNU.c
ATL_creftrsmRLNN.o                 : $(mySRCdir)/ATL_creftrsmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmRLNN.c
ATL_creftrmmRLNU.o                 : $(mySRCdir)/ATL_creftrmmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRLNU.c
ATL_creftrmmRLNN.o                 : $(mySRCdir)/ATL_creftrmmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmRLNN.c
ATL_creftrsmLUCU.o                 : $(mySRCdir)/ATL_creftrsmLUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLUCU.c
ATL_creftrsmLUCN.o                 : $(mySRCdir)/ATL_creftrsmLUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLUCN.c
ATL_creftrmmLUCU.o                 : $(mySRCdir)/ATL_creftrmmLUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLUCU.c
ATL_creftrmmLUCN.o                 : $(mySRCdir)/ATL_creftrmmLUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLUCN.c
ATL_creftrsmLUTU.o                 : $(mySRCdir)/ATL_creftrsmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLUTU.c
ATL_creftrsmLUTN.o                 : $(mySRCdir)/ATL_creftrsmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLUTN.c
ATL_creftrmmLUTU.o                 : $(mySRCdir)/ATL_creftrmmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLUTU.c
ATL_creftrmmLUTN.o                 : $(mySRCdir)/ATL_creftrmmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLUTN.c
ATL_creftrsmLUNU.o                 : $(mySRCdir)/ATL_creftrsmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLUNU.c
ATL_creftrsmLUNN.o                 : $(mySRCdir)/ATL_creftrsmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLUNN.c
ATL_creftrmmLUNU.o                 : $(mySRCdir)/ATL_creftrmmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLUNU.c
ATL_creftrmmLUNN.o                 : $(mySRCdir)/ATL_creftrmmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLUNN.c
ATL_creftrsmLLCU.o                 : $(mySRCdir)/ATL_creftrsmLLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLLCU.c
ATL_creftrsmLLCN.o                 : $(mySRCdir)/ATL_creftrsmLLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLLCN.c
ATL_creftrmmLLCU.o                 : $(mySRCdir)/ATL_creftrmmLLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLLCU.c
ATL_creftrmmLLCN.o                 : $(mySRCdir)/ATL_creftrmmLLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLLCN.c
ATL_creftrsmLLTU.o                 : $(mySRCdir)/ATL_creftrsmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLLTU.c
ATL_creftrsmLLTN.o                 : $(mySRCdir)/ATL_creftrsmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLLTN.c
ATL_creftrmmLLTU.o                 : $(mySRCdir)/ATL_creftrmmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLLTU.c
ATL_creftrmmLLTN.o                 : $(mySRCdir)/ATL_creftrmmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLLTN.c
ATL_creftrsmLLNU.o                 : $(mySRCdir)/ATL_creftrsmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLLNU.c
ATL_creftrsmLLNN.o                 : $(mySRCdir)/ATL_creftrsmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrsmLLNN.c
ATL_creftrmmLLNU.o                 : $(mySRCdir)/ATL_creftrmmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLLNU.c
ATL_creftrmmLLNN.o                 : $(mySRCdir)/ATL_creftrmmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_creftrmmLLNN.c
#
ATL_zrefgemm.o                     : $(mySRCdir)/ATL_zrefgemm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemm.c
ATL_zrefgemmNN.o                   : $(mySRCdir)/ATL_zrefgemmNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmNN.c
ATL_zrefgemmTN.o                   : $(mySRCdir)/ATL_zrefgemmTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmTN.c
ATL_zrefgemmCN.o                   : $(mySRCdir)/ATL_zrefgemmCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmCN.c
ATL_zrefgemmNT.o                   : $(mySRCdir)/ATL_zrefgemmNT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmNT.c
ATL_zrefgemmNC.o                   : $(mySRCdir)/ATL_zrefgemmNC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmNC.c
ATL_zrefgemmTT.o                   : $(mySRCdir)/ATL_zrefgemmTT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmTT.c
ATL_zrefgemmCT.o                   : $(mySRCdir)/ATL_zrefgemmCT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmCT.c
ATL_zrefgemmTC.o                   : $(mySRCdir)/ATL_zrefgemmTC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmTC.c
ATL_zrefgemmCC.o                   : $(mySRCdir)/ATL_zrefgemmCC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefgemmCC.c
ATL_zrefhemm.o                     : $(mySRCdir)/ATL_zrefhemm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemm.c
ATL_zrefhemmRU.o                   : $(mySRCdir)/ATL_zrefhemmRU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemmRU.c
ATL_zrefhemmRL.o                   : $(mySRCdir)/ATL_zrefhemmRL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemmRL.c
ATL_zrefhemmLU.o                   : $(mySRCdir)/ATL_zrefhemmLU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemmLU.c
ATL_zrefhemmLL.o                   : $(mySRCdir)/ATL_zrefhemmLL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefhemmLL.c
ATL_zrefherk.o                     : $(mySRCdir)/ATL_zrefherk.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherk.c
ATL_zrefherkUC.o                   : $(mySRCdir)/ATL_zrefherkUC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherkUC.c
ATL_zrefherkUN.o                   : $(mySRCdir)/ATL_zrefherkUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherkUN.c
ATL_zrefherkLC.o                   : $(mySRCdir)/ATL_zrefherkLC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherkLC.c
ATL_zrefherkLN.o                   : $(mySRCdir)/ATL_zrefherkLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefherkLN.c
ATL_zrefher2k.o                    : $(mySRCdir)/ATL_zrefher2k.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2k.c
ATL_zrefher2kUC.o                  : $(mySRCdir)/ATL_zrefher2kUC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2kUC.c
ATL_zrefher2kUN.o                  : $(mySRCdir)/ATL_zrefher2kUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2kUN.c
ATL_zrefher2kLC.o                  : $(mySRCdir)/ATL_zrefher2kLC.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2kLC.c
ATL_zrefher2kLN.o                  : $(mySRCdir)/ATL_zrefher2kLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefher2kLN.c
ATL_zrefsymm.o                     : $(mySRCdir)/ATL_zrefsymm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsymm.c
ATL_zrefsymmRU.o                   : $(mySRCdir)/ATL_zrefsymmRU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsymmRU.c
ATL_zrefsymmRL.o                   : $(mySRCdir)/ATL_zrefsymmRL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsymmRL.c
ATL_zrefsymmLU.o                   : $(mySRCdir)/ATL_zrefsymmLU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsymmLU.c
ATL_zrefsymmLL.o                   : $(mySRCdir)/ATL_zrefsymmLL.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsymmLL.c
ATL_zrefsyrk.o                     : $(mySRCdir)/ATL_zrefsyrk.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyrk.c
ATL_zrefsyrkUT.o                   : $(mySRCdir)/ATL_zrefsyrkUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyrkUT.c
ATL_zrefsyrkUN.o                   : $(mySRCdir)/ATL_zrefsyrkUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyrkUN.c
ATL_zrefsyrkLT.o                   : $(mySRCdir)/ATL_zrefsyrkLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyrkLT.c
ATL_zrefsyrkLN.o                   : $(mySRCdir)/ATL_zrefsyrkLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyrkLN.c
ATL_zrefsyr2k.o                    : $(mySRCdir)/ATL_zrefsyr2k.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyr2k.c
ATL_zrefsyr2kUT.o                  : $(mySRCdir)/ATL_zrefsyr2kUT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyr2kUT.c
ATL_zrefsyr2kUN.o                  : $(mySRCdir)/ATL_zrefsyr2kUN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyr2kUN.c
ATL_zrefsyr2kLT.o                  : $(mySRCdir)/ATL_zrefsyr2kLT.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyr2kLT.c
ATL_zrefsyr2kLN.o                  : $(mySRCdir)/ATL_zrefsyr2kLN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefsyr2kLN.c
ATL_zreftrmm.o                     : $(mySRCdir)/ATL_zreftrmm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmm.c
ATL_zreftrsm.o                     : $(mySRCdir)/ATL_zreftrsm.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsm.c
ATL_zreftrsmRUCU.o                 : $(mySRCdir)/ATL_zreftrsmRUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRUCU.c
ATL_zreftrsmRUCN.o                 : $(mySRCdir)/ATL_zreftrsmRUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRUCN.c
ATL_zreftrmmRUCU.o                 : $(mySRCdir)/ATL_zreftrmmRUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRUCU.c
ATL_zreftrmmRUCN.o                 : $(mySRCdir)/ATL_zreftrmmRUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRUCN.c
ATL_zreftrsmRUTU.o                 : $(mySRCdir)/ATL_zreftrsmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRUTU.c
ATL_zreftrsmRUTN.o                 : $(mySRCdir)/ATL_zreftrsmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRUTN.c
ATL_zreftrmmRUTU.o                 : $(mySRCdir)/ATL_zreftrmmRUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRUTU.c
ATL_zreftrmmRUTN.o                 : $(mySRCdir)/ATL_zreftrmmRUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRUTN.c
ATL_zreftrsmRUNU.o                 : $(mySRCdir)/ATL_zreftrsmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRUNU.c
ATL_zreftrsmRUNN.o                 : $(mySRCdir)/ATL_zreftrsmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRUNN.c
ATL_zreftrmmRUNU.o                 : $(mySRCdir)/ATL_zreftrmmRUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRUNU.c
ATL_zreftrmmRUNN.o                 : $(mySRCdir)/ATL_zreftrmmRUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRUNN.c
ATL_zreftrsmRLCU.o                 : $(mySRCdir)/ATL_zreftrsmRLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRLCU.c
ATL_zreftrsmRLCN.o                 : $(mySRCdir)/ATL_zreftrsmRLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRLCN.c
ATL_zreftrmmRLCU.o                 : $(mySRCdir)/ATL_zreftrmmRLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRLCU.c
ATL_zreftrmmRLCN.o                 : $(mySRCdir)/ATL_zreftrmmRLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRLCN.c
ATL_zreftrsmRLTU.o                 : $(mySRCdir)/ATL_zreftrsmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRLTU.c
ATL_zreftrsmRLTN.o                 : $(mySRCdir)/ATL_zreftrsmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRLTN.c
ATL_zreftrmmRLTU.o                 : $(mySRCdir)/ATL_zreftrmmRLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRLTU.c
ATL_zreftrmmRLTN.o                 : $(mySRCdir)/ATL_zreftrmmRLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRLTN.c
ATL_zreftrsmRLNU.o                 : $(mySRCdir)/ATL_zreftrsmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRLNU.c
ATL_zreftrsmRLNN.o                 : $(mySRCdir)/ATL_zreftrsmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmRLNN.c
ATL_zreftrmmRLNU.o                 : $(mySRCdir)/ATL_zreftrmmRLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRLNU.c
ATL_zreftrmmRLNN.o                 : $(mySRCdir)/ATL_zreftrmmRLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmRLNN.c
ATL_zreftrsmLUCU.o                 : $(mySRCdir)/ATL_zreftrsmLUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLUCU.c
ATL_zreftrsmLUCN.o                 : $(mySRCdir)/ATL_zreftrsmLUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLUCN.c
ATL_zreftrmmLUCU.o                 : $(mySRCdir)/ATL_zreftrmmLUCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLUCU.c
ATL_zreftrmmLUCN.o                 : $(mySRCdir)/ATL_zreftrmmLUCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLUCN.c
ATL_zreftrsmLUTU.o                 : $(mySRCdir)/ATL_zreftrsmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLUTU.c
ATL_zreftrsmLUTN.o                 : $(mySRCdir)/ATL_zreftrsmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLUTN.c
ATL_zreftrmmLUTU.o                 : $(mySRCdir)/ATL_zreftrmmLUTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLUTU.c
ATL_zreftrmmLUTN.o                 : $(mySRCdir)/ATL_zreftrmmLUTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLUTN.c
ATL_zreftrsmLUNU.o                 : $(mySRCdir)/ATL_zreftrsmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLUNU.c
ATL_zreftrsmLUNN.o                 : $(mySRCdir)/ATL_zreftrsmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLUNN.c
ATL_zreftrmmLUNU.o                 : $(mySRCdir)/ATL_zreftrmmLUNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLUNU.c
ATL_zreftrmmLUNN.o                 : $(mySRCdir)/ATL_zreftrmmLUNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLUNN.c
ATL_zreftrsmLLCU.o                 : $(mySRCdir)/ATL_zreftrsmLLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLLCU.c
ATL_zreftrsmLLCN.o                 : $(mySRCdir)/ATL_zreftrsmLLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLLCN.c
ATL_zreftrmmLLCU.o                 : $(mySRCdir)/ATL_zreftrmmLLCU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLLCU.c
ATL_zreftrmmLLCN.o                 : $(mySRCdir)/ATL_zreftrmmLLCN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLLCN.c
ATL_zreftrsmLLTU.o                 : $(mySRCdir)/ATL_zreftrsmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLLTU.c
ATL_zreftrsmLLTN.o                 : $(mySRCdir)/ATL_zreftrsmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLLTN.c
ATL_zreftrmmLLTU.o                 : $(mySRCdir)/ATL_zreftrmmLLTU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLLTU.c
ATL_zreftrmmLLTN.o                 : $(mySRCdir)/ATL_zreftrmmLLTN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLLTN.c
ATL_zreftrsmLLNU.o                 : $(mySRCdir)/ATL_zreftrsmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLLNU.c
ATL_zreftrsmLLNN.o                 : $(mySRCdir)/ATL_zreftrsmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrsmLLNN.c
ATL_zreftrmmLLNU.o                 : $(mySRCdir)/ATL_zreftrmmLLNU.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLLNU.c
ATL_zreftrmmLLNN.o                 : $(mySRCdir)/ATL_zreftrmmLLNN.c $(INC3dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zreftrmmLLNN.c
#
########################################################################
#
sclean   :
	rm -f $(ATL_slvl3obj) s*.grd
dclean   :
	rm -f $(ATL_dlvl3obj) d*.grd
cclean   :
	rm -f $(ATL_clvl3obj) c*.grd
zclean   :
	rm -f $(ATL_zlvl3obj) z*.grd
#
clean    :
	rm -f *.o *.grd
#
########################################################################
