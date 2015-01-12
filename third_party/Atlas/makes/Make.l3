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
mySRCdir = $(SRCdir)/src/blas/level3
#
########################################################################
#
AINCLUDES = \
   $(INCSdir)/atlas_rblas3.h   $(INCSdir)/atlas_kernel3.h \
   $(INCSdir)/atlas_lvl3.h     $(INCSdir)/atlas_level3.h
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
SRBLAS3 = \
   ATL_ssymm.o ATL_ssyr2k.o ATL_ssyrk.o ATL_strmm.o ATL_strsm.o
# 
DRBLAS3 = \
   ATL_dsymm.o ATL_dsyr2k.o ATL_dsyrk.o ATL_dtrmm.o ATL_dtrsm.o
# 
CRBLAS3 = \
   ATL_chemm.o ATL_cher2k.o ATL_cherk.o ATL_csymm.o ATL_csyr2k.o ATL_csyrk.o \
   ATL_ctrmm.o ATL_ctrsm.o
# 
ZRBLAS3 = \
   ATL_zhemm.o ATL_zher2k.o ATL_zherk.o ATL_zsymm.o ATL_zsyr2k.o ATL_zsyrk.o \
   ATL_ztrmm.o ATL_ztrsm.o
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
slib  : skern srblas sblas3
dlib  : dkern drblas dblas3
clib  : ckern crblas cblas3
zlib  : zkern zrblas zblas3
#
skern :
	( cd kernel; $(MAKE) slib )
dkern :
	( cd kernel; $(MAKE) dlib )
ckern :
	( cd kernel; $(MAKE) clib )
zkern :
	( cd kernel; $(MAKE) zlib )
#
srblas:
	( cd rblas;  $(MAKE) slib )
drblas:
	( cd rblas;  $(MAKE) dlib )
crblas:
	( cd rblas;  $(MAKE) clib )
zrblas:
	( cd rblas;  $(MAKE) zlib )
#
sblas3 :
	cd ../pklevel3 ; $(MAKE) slib
	$(PMAKE) sblas3.grd
dblas3 :
	cd ../pklevel3 ; $(MAKE) dlib
	$(PMAKE) dblas3.grd
cblas3 :
	cd ../pklevel3 ; $(MAKE) clib
	$(PMAKE) cblas3.grd
zblas3 :
	cd ../pklevel3 ; $(MAKE) zlib
	$(PMAKE) zblas3.grd
#
sblas3.grd: $(SRBLAS3)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(SRBLAS3)
	$(RANLIB) $(ATLASlib)
	touch sblas3.grd
#
dblas3.grd: $(DRBLAS3)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(DRBLAS3)
	$(RANLIB) $(ATLASlib)
	touch dblas3.grd
#
cblas3.grd: $(CRBLAS3)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(CRBLAS3)
	$(RANLIB) $(ATLASlib)
	touch cblas3.grd
#
zblas3.grd: $(ZRBLAS3)
	$(ARCHIVER) $(ARFLAGS) $(ATLASlib) $(ZRBLAS3)
	$(RANLIB) $(ATLASlib)
	touch zblas3.grd
#
########################################################################
# 
ATL_ssymm.o              : $(mySRCdir)/ATL_symm.c $(SINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_symm.c
ATL_ssyrk.o              : $(mySRCdir)/ATL_syrk.c $(SINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_syrk.c
ATL_ssyr2k.o             : $(mySRCdir)/ATL_syr2k.c $(SINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_syr2k.c
ATL_strmm.o              : $(mySRCdir)/ATL_trmm.c $(SINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_trmm.c
ATL_strsm.o : $(mySRCdir)/ATL_trsm.c $(SINCLUDES) $(INCAdir)/atlas_trsmNB.h
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/ATL_trsm.c
#
ATL_dsymm.o              : $(mySRCdir)/ATL_symm.c $(DINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_symm.c
ATL_dsyrk.o              : $(mySRCdir)/ATL_syrk.c $(DINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_syrk.c
ATL_dsyr2k.o             : $(mySRCdir)/ATL_syr2k.c $(DINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_syr2k.c
ATL_dtrmm.o              : $(mySRCdir)/ATL_trmm.c $(DINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_trmm.c
ATL_dtrsm.o : $(mySRCdir)/ATL_trsm.c $(DINCLUDES) $(INCAdir)/atlas_trsmNB.h
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/ATL_trsm.c
#
ATL_chemm.o              : $(mySRCdir)/ATL_hemm.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_hemm.c
ATL_cherk.o              : $(mySRCdir)/ATL_herk.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_herk.c
ATL_cher2k.o             : $(mySRCdir)/ATL_her2k.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_her2k.c
ATL_csymm.o              : $(mySRCdir)/ATL_symm.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_symm.c
ATL_csyrk.o              : $(mySRCdir)/ATL_syrk.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_syrk.c
ATL_csyr2k.o             : $(mySRCdir)/ATL_syr2k.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_syr2k.c
ATL_ctrmm.o              : $(mySRCdir)/ATL_trmm.c $(CINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_trmm.c
ATL_ctrsm.o : $(mySRCdir)/ATL_trsm.c $(CINCLUDES) $(INCAdir)/atlas_trsmNB.h
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/ATL_trsm.c
#
ATL_zhemm.o              : $(mySRCdir)/ATL_hemm.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_hemm.c
ATL_zherk.o              : $(mySRCdir)/ATL_herk.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_herk.c
ATL_zher2k.o             : $(mySRCdir)/ATL_her2k.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_her2k.c
ATL_zsymm.o              : $(mySRCdir)/ATL_symm.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_symm.c
ATL_zsyrk.o              : $(mySRCdir)/ATL_syrk.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_syrk.c
ATL_zsyr2k.o             : $(mySRCdir)/ATL_syr2k.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_syr2k.c
ATL_ztrmm.o              : $(mySRCdir)/ATL_trmm.c $(ZINCLUDES)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_trmm.c
ATL_ztrsm.o : $(mySRCdir)/ATL_trsm.c $(ZINCLUDES) $(INCAdir)/atlas_trsmNB.h
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/ATL_trsm.c
#
########################################################################
#
sclean           :
	( cd kernel; $(MAKE) sclean )
	( cd rblas;  $(MAKE) sclean )
	rm -f $(SRBLAS3) s*.grd
#
dclean           :
	( cd kernel; $(MAKE) dclean )
	( cd rblas;  $(MAKE) dclean )
	rm -f $(DRBLAS3) d*.grd
#
cclean           :
	( cd kernel; $(MAKE) cclean )
	( cd rblas;  $(MAKE) cclean )
	rm -f $(CRBLAS3) c*.grd
#
zclean           :
	( cd kernel; $(MAKE) zclean )
	( cd rblas;  $(MAKE) zclean )
	rm -f $(ZRBLAS3) z*.grd
#
#
clean            :
	( cd rblas;  $(MAKE)  clean )
	( cd kernel; $(MAKE)  clean )
	rm -f *.o *.grd
#
########################################################################
