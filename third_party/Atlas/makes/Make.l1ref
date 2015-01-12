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
mySRCdir = $(SRCdir)/src/blas/reference/level1
#
########################################################################
#
REFBLASlib = $(ATLASlib)
INC1dep    = $(INCSdir)/atlas_enum.h $(INCSdir)/atlas_refmisc.h \
             $(INCSdir)/atlas_reflevel1.h
#
## Object files ########################################################
#
ATL_slvl1obj = \
   ATL_dsrefdot.o ATL_isrefamax.o ATL_sdsrefdot.o ATL_srefasum.o \
   ATL_srefaxpy.o ATL_srefcopy.o ATL_srefdot.o ATL_srefnrm2.o ATL_srefrot.o \
   ATL_srefrotg.o ATL_srefrotm.o ATL_srefrotmg.o ATL_srefscal.o \
   ATL_srefswap.o
#
ATL_dlvl1obj = \
   ATL_drefasum.o ATL_drefaxpy.o ATL_drefcopy.o ATL_drefdot.o ATL_drefnrm2.o \
   ATL_drefrot.o ATL_drefrotg.o ATL_drefrotm.o ATL_drefrotmg.o ATL_drefscal.o \
   ATL_drefswap.o ATL_idrefamax.o
#
ATL_clvl1obj = \
   ATL_crefaxpy.o ATL_crefcopy.o ATL_crefdotc_sub.o ATL_crefdotu_sub.o \
   ATL_crefrotg.o ATL_crefscal.o ATL_crefswap.o ATL_csrefrot.o \
   ATL_csrefscal.o ATL_icrefamax.o ATL_screfasum.o ATL_screfnrm2.o
#
ATL_zlvl1obj = \
   ATL_dzrefasum.o ATL_dzrefnrm2.o ATL_izrefamax.o ATL_zdrefrot.o \
   ATL_zdrefscal.o ATL_zrefaxpy.o ATL_zrefcopy.o ATL_zrefdotc_sub.o \
   ATL_zrefdotu_sub.o ATL_zrefrotg.o ATL_zrefscal.o ATL_zrefswap.o
#
## Targets #############################################################
# 
all   : lib
#
lvl1  : lib
#
lib   : slib dlib clib zlib
#
slvl1 : slib
dlvl1 : dlib
clvl1 : clib
zlvl1 : zlib
#
########################################################################
#
slib  : slib.grd
dlib  : dlib.grd
clib  : clib.grd
zlib  : zlib.grd
#
slib.grd  : $(ATL_slvl1obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_slvl1obj)
	$(RANLIB) $(REFBLASlib)
	touch slib.grd
#
dlib.grd  : $(ATL_dlvl1obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_dlvl1obj)
	$(RANLIB) $(REFBLASlib)
	touch dlib.grd
#
clib.grd  : $(ATL_clvl1obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_clvl1obj)
	$(RANLIB) $(REFBLASlib)
	touch clib.grd
#
zlib.grd  : $(ATL_zlvl1obj)
	$(ARCHIVER) $(ARFLAGS) $(REFBLASlib) $(ATL_zlvl1obj)
	$(RANLIB) $(REFBLASlib)
	touch zlib.grd
#
########################################################################
# 
ATL_srefrotg.o                 : $(mySRCdir)/ATL_srefrotg.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefrotg.c
ATL_srefrotmg.o                : $(mySRCdir)/ATL_srefrotmg.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefrotmg.c
ATL_srefnrm2.o                 : $(mySRCdir)/ATL_srefnrm2.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefnrm2.c
ATL_srefasum.o                 : $(mySRCdir)/ATL_srefasum.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefasum.c
ATL_isrefamax.o                : $(mySRCdir)/ATL_isrefamax.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_isrefamax.c
ATL_srefscal.o                 : $(mySRCdir)/ATL_srefscal.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefscal.c
ATL_srefswap.o                 : $(mySRCdir)/ATL_srefswap.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefswap.c
ATL_srefcopy.o                 : $(mySRCdir)/ATL_srefcopy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefcopy.c
ATL_srefaxpy.o                 : $(mySRCdir)/ATL_srefaxpy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefaxpy.c
ATL_srefrot.o                  : $(mySRCdir)/ATL_srefrot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefrot.c
ATL_srefrotm.o                 : $(mySRCdir)/ATL_srefrotm.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefrotm.c
ATL_srefdot.o                  : $(mySRCdir)/ATL_srefdot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_srefdot.c
ATL_sdsrefdot.o                : $(mySRCdir)/ATL_sdsrefdot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_sdsrefdot.c
ATL_dsrefdot.o                 : $(mySRCdir)/ATL_dsrefdot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dsrefdot.c
#
ATL_drefrotg.o                 : $(mySRCdir)/ATL_drefrotg.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefrotg.c
ATL_drefrotmg.o                : $(mySRCdir)/ATL_drefrotmg.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefrotmg.c
ATL_drefnrm2.o                 : $(mySRCdir)/ATL_drefnrm2.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefnrm2.c
ATL_drefasum.o                 : $(mySRCdir)/ATL_drefasum.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefasum.c
ATL_idrefamax.o                : $(mySRCdir)/ATL_idrefamax.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_idrefamax.c
ATL_drefscal.o                 : $(mySRCdir)/ATL_drefscal.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefscal.c
ATL_drefswap.o                 : $(mySRCdir)/ATL_drefswap.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefswap.c
ATL_drefcopy.o                 : $(mySRCdir)/ATL_drefcopy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefcopy.c
ATL_drefaxpy.o                 : $(mySRCdir)/ATL_drefaxpy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefaxpy.c
ATL_drefrot.o                  : $(mySRCdir)/ATL_drefrot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefrot.c
ATL_drefrotm.o                 : $(mySRCdir)/ATL_drefrotm.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefrotm.c
ATL_drefdot.o                  : $(mySRCdir)/ATL_drefdot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_drefdot.c
#
ATL_crefrotg.o                 : $(mySRCdir)/ATL_crefrotg.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefrotg.c
ATL_screfnrm2.o                : $(mySRCdir)/ATL_screfnrm2.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_screfnrm2.c
ATL_screfasum.o                : $(mySRCdir)/ATL_screfasum.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_screfasum.c
ATL_icrefamax.o                : $(mySRCdir)/ATL_icrefamax.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_icrefamax.c
ATL_crefscal.o                 : $(mySRCdir)/ATL_crefscal.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefscal.c
ATL_csrefscal.o                : $(mySRCdir)/ATL_csrefscal.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_csrefscal.c
ATL_crefswap.o                 : $(mySRCdir)/ATL_crefswap.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefswap.c
ATL_crefcopy.o                 : $(mySRCdir)/ATL_crefcopy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefcopy.c
ATL_crefaxpy.o                 : $(mySRCdir)/ATL_crefaxpy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefaxpy.c
ATL_csrefrot.o                 : $(mySRCdir)/ATL_csrefrot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_csrefrot.c
ATL_crefdotc_sub.o             : $(mySRCdir)/ATL_crefdotc_sub.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefdotc_sub.c
ATL_crefdotu_sub.o             : $(mySRCdir)/ATL_crefdotu_sub.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_crefdotu_sub.c
#
ATL_zrefrotg.o                 : $(mySRCdir)/ATL_zrefrotg.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefrotg.c
ATL_dzrefnrm2.o                : $(mySRCdir)/ATL_dzrefnrm2.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dzrefnrm2.c
ATL_dzrefasum.o                : $(mySRCdir)/ATL_dzrefasum.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_dzrefasum.c
ATL_izrefamax.o                : $(mySRCdir)/ATL_izrefamax.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_izrefamax.c
ATL_zrefscal.o                 : $(mySRCdir)/ATL_zrefscal.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefscal.c
ATL_zdrefscal.o                : $(mySRCdir)/ATL_zdrefscal.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zdrefscal.c
ATL_zrefswap.o                 : $(mySRCdir)/ATL_zrefswap.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefswap.c
ATL_zrefcopy.o                 : $(mySRCdir)/ATL_zrefcopy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefcopy.c
ATL_zrefaxpy.o                 : $(mySRCdir)/ATL_zrefaxpy.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefaxpy.c
ATL_zdrefrot.o                 : $(mySRCdir)/ATL_zdrefrot.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zdrefrot.c
ATL_zrefdotc_sub.o             : $(mySRCdir)/ATL_zrefdotc_sub.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefdotc_sub.c
ATL_zrefdotu_sub.o             : $(mySRCdir)/ATL_zrefdotu_sub.c $(INC1dep)
	$(ICC) -o $@ -c $(ICCFLAGS) $(mySRCdir)/ATL_zrefdotu_sub.c
#
########################################################################
#
sclean   :
	rm -f $(ATL_slvl1obj) s*.grd
dclean   :
	rm -f $(ATL_dlvl1obj) d*.grd
cclean   :
	rm -f $(ATL_clvl1obj) c*.grd
zclean   :
	rm -f $(ATL_zlvl1obj) z*.grd
#
clean    :
	rm -f *.o *.grd
#
########################################################################
