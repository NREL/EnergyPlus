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
mySRCdir = $(SRCdir)/src/blas/level3/rblas
#
########################################################################
#
AINCLUDES   = \
   $(INCSdir)/atlas_misc.h    $(INCSdir)/atlas_lvl3.h   \
   $(INCSdir)/atlas_kernel3.h $(INCSdir)/atlas_rblas3.h
SINCLUDES = \
   $(AINCLUDES) $(INCAdir)/smm.h
DINCLUDES = \
   $(AINCLUDES) $(INCAdir)/dmm.h
CINCLUDES = \
   $(AINCLUDES) $(INCAdir)/cmm.h
ZINCLUDES = \
   $(AINCLUDES) $(INCAdir)/zmm.h
#
## Object files ########################################################
# 
ARC3BLASAUX  = \
   ATL_rsymmLL.o ATL_rsymmLU.o ATL_rsymmRL.o ATL_rsymmRU.o ATL_rsyr2kLN.o \
   ATL_rsyr2kLT.o ATL_rsyr2kUN.o ATL_rsyr2kUT.o ATL_rsyrkLN.o ATL_rsyrkLT.o \
   ATL_rsyrkUN.o ATL_rsyrkUT.o ATL_rtrmmLLN.o ATL_rtrmmLLT.o ATL_rtrmmLUN.o \
   ATL_rtrmmLUT.o ATL_rtrmmRLN.o ATL_rtrmmRLT.o ATL_rtrmmRUN.o ATL_rtrmmRUT.o \
   ATL_rtrsmLLN.o ATL_rtrsmLLT.o ATL_rtrsmLUN.o ATL_rtrsmLUT.o ATL_rtrsmRLN.o \
   ATL_rtrsmRLT.o ATL_rtrsmRUN.o ATL_rtrsmRUT.o
#
CZRC3BLASAUX = \
   ATL_rhemmLL.o ATL_rhemmLU.o ATL_rhemmRL.o ATL_rhemmRU.o ATL_rher2kLC.o \
   ATL_rher2kLN.o ATL_rher2kUC.o ATL_rher2kUN.o ATL_rherkLC.o ATL_rherkLN.o \
   ATL_rherkUC.o ATL_rherkUN.o ATL_rtrmmLLC.o ATL_rtrmmLUC.o ATL_rtrmmRLC.o \
   ATL_rtrmmRUC.o ATL_rtrsmLLC.o ATL_rtrsmLUC.o ATL_rtrsmRLC.o ATL_rtrsmRUC.o
#
SRC3BLASAUX  = \
   ATL_sgemmRB.o
#
DRC3BLASAUX  = \
   ATL_dgemmRB.o
#
CRC3BLASAUX  = \
   ATL_cgemmRB.o
#
ZRC3BLASAUX  = \
   ATL_zgemmRB.o
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
slib  : slib.grd
dlib  : dlib.grd
clib  : clib.grd
zlib  : zlib.grd
#
slib.grd  : $(SRC3BLASAUX) $(ARC3BLASAUX)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(SRC3BLASAUX) $(ARC3BLASAUX)
	$(RANLIB) $(ATLASlib)
	touch slib.grd
#
dlib.grd  : $(DRC3BLASAUX) $(ARC3BLASAUX)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(DRC3BLASAUX) $(ARC3BLASAUX)
	$(RANLIB) $(ATLASlib)
	touch dlib.grd
#
clib.grd  : $(CRC3BLASAUX) $(ARC3BLASAUX) $(CZRC3BLASAUX)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(CRC3BLASAUX) $(ARC3BLASAUX) \
        $(CZRC3BLASAUX)
	$(RANLIB) $(ATLASlib)
	touch clib.grd
#
zlib.grd  : $(ZRC3BLASAUX) $(ARC3BLASAUX) $(CZRC3BLASAUX)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ZRC3BLASAUX) $(ARC3BLASAUX) \
        $(CZRC3BLASAUX)
	$(RANLIB) $(ATLASlib)
	touch zlib.grd
#
#
########################################################################
# 
ATL_sgemmRB.o        : $(mySRCdir)/ATL_gemmRB.c $(SINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_gemmRB.c
ATL_dgemmRB.o        : $(mySRCdir)/ATL_gemmRB.c $(DINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_gemmRB.c
ATL_cgemmRB.o        : $(mySRCdir)/ATL_gemmRB.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_gemmRB.c
ATL_zgemmRB.o        : $(mySRCdir)/ATL_gemmRB.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_gemmRB.c
#
ATL_rtrsmRUC.o       : $(mySRCdir)/ATL_rtrsmRUC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmRUC.c
ATL_rtrsmRLC.o       : $(mySRCdir)/ATL_rtrsmRLC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmRLC.c
ATL_rtrsmLUC.o       : $(mySRCdir)/ATL_rtrsmLUC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmLUC.c
ATL_rtrsmLLC.o       : $(mySRCdir)/ATL_rtrsmLLC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmLLC.c
ATL_rtrmmRUC.o       : $(mySRCdir)/ATL_rtrmmRUC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmRUC.c
ATL_rtrmmRLC.o       : $(mySRCdir)/ATL_rtrmmRLC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmRLC.c
ATL_rtrmmLUC.o       : $(mySRCdir)/ATL_rtrmmLUC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmLUC.c
ATL_rtrmmLLC.o       : $(mySRCdir)/ATL_rtrmmLLC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmLLC.c
ATL_rher2kUN.o       : $(mySRCdir)/ATL_rher2kUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rher2kUN.c
ATL_rher2kUC.o       : $(mySRCdir)/ATL_rher2kUC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rher2kUC.c
ATL_rher2kLN.o       : $(mySRCdir)/ATL_rher2kLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rher2kLN.c
ATL_rher2kLC.o       : $(mySRCdir)/ATL_rher2kLC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rher2kLC.c
ATL_rherkUN.o        : $(mySRCdir)/ATL_rherkUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rherkUN.c
ATL_rherkUC.o        : $(mySRCdir)/ATL_rherkUC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rherkUC.c
ATL_rherkLN.o        : $(mySRCdir)/ATL_rherkLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rherkLN.c
ATL_rherkLC.o        : $(mySRCdir)/ATL_rherkLC.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rherkLC.c
ATL_rhemmRU.o        : $(mySRCdir)/ATL_rhemmRU.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rhemmRU.c
ATL_rhemmRL.o        : $(mySRCdir)/ATL_rhemmRL.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rhemmRL.c
ATL_rhemmLU.o        : $(mySRCdir)/ATL_rhemmLU.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rhemmLU.c
ATL_rhemmLL.o        : $(mySRCdir)/ATL_rhemmLL.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rhemmLL.c
ATL_rtrsmRUT.o       : $(mySRCdir)/ATL_rtrsmRUT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmRUT.c
ATL_rtrsmRUN.o       : $(mySRCdir)/ATL_rtrsmRUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmRUN.c
ATL_rtrsmRLT.o       : $(mySRCdir)/ATL_rtrsmRLT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmRLT.c
ATL_rtrsmRLN.o       : $(mySRCdir)/ATL_rtrsmRLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmRLN.c
ATL_rtrsmLUT.o       : $(mySRCdir)/ATL_rtrsmLUT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmLUT.c
ATL_rtrsmLUN.o       : $(mySRCdir)/ATL_rtrsmLUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmLUN.c
ATL_rtrsmLLT.o       : $(mySRCdir)/ATL_rtrsmLLT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmLLT.c
ATL_rtrsmLLN.o       : $(mySRCdir)/ATL_rtrsmLLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrsmLLN.c
ATL_rtrmmRUT.o       : $(mySRCdir)/ATL_rtrmmRUT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmRUT.c
ATL_rtrmmRUN.o       : $(mySRCdir)/ATL_rtrmmRUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmRUN.c
ATL_rtrmmRLT.o       : $(mySRCdir)/ATL_rtrmmRLT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmRLT.c
ATL_rtrmmRLN.o       : $(mySRCdir)/ATL_rtrmmRLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmRLN.c
ATL_rtrmmLUT.o       : $(mySRCdir)/ATL_rtrmmLUT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmLUT.c
ATL_rtrmmLUN.o       : $(mySRCdir)/ATL_rtrmmLUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmLUN.c
ATL_rtrmmLLT.o       : $(mySRCdir)/ATL_rtrmmLLT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmLLT.c
ATL_rtrmmLLN.o       : $(mySRCdir)/ATL_rtrmmLLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rtrmmLLN.c
ATL_rsyr2kUT.o       : $(mySRCdir)/ATL_rsyr2kUT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyr2kUT.c
ATL_rsyr2kUN.o       : $(mySRCdir)/ATL_rsyr2kUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyr2kUN.c
ATL_rsyr2kLT.o       : $(mySRCdir)/ATL_rsyr2kLT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyr2kLT.c
ATL_rsyr2kLN.o       : $(mySRCdir)/ATL_rsyr2kLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyr2kLN.c
ATL_rsyrkUT.o        : $(mySRCdir)/ATL_rsyrkUT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyrkUT.c
ATL_rsyrkUN.o        : $(mySRCdir)/ATL_rsyrkUN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyrkUN.c
ATL_rsyrkLT.o        : $(mySRCdir)/ATL_rsyrkLT.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyrkLT.c
ATL_rsyrkLN.o        : $(mySRCdir)/ATL_rsyrkLN.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsyrkLN.c
ATL_rsymmRU.o        : $(mySRCdir)/ATL_rsymmRU.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsymmRU.c
ATL_rsymmRL.o        : $(mySRCdir)/ATL_rsymmRL.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsymmRL.c
ATL_rsymmLU.o        : $(mySRCdir)/ATL_rsymmLU.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsymmLU.c
ATL_rsymmLL.o        : $(mySRCdir)/ATL_rsymmLL.c $(AINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_rsymmLL.c
# 
########################################################################
#
sclean           :
	rm -f $(SRC3BLASAUX) $(ARC3BLASAUX) s*.grd 
#
dclean           :
	rm -f $(DRC3BLASAUX) $(ARC3BLASAUX) d*.grd 
#
cclean           :
	rm -f $(CRC3BLASAUX) $(ARC3BLASAUX) $(CZRC3BLASAUX) \
        c*.grd 
#
zclean           :
	rm -f $(ZRC3BLASAUX) $(ARC3BLASAUX) $(CZRC3BLASAUX) \
        z*.grd 
#
#
clean            :
	rm -f *.o *.grd
#
########################################################################
