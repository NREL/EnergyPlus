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
mySRCdir = $(SRCdir)/interfaces/blas/F77/src
#
########################################################################
#
INCdep = \
   $(INCSdir)/atlas_enum.h        $(INCSdir)/atlas_f77.h       \
   $(INCSdir)/atlas_refalias1.h   $(INCSdir)/atlas_refalias2.h \
   $(INCSdir)/atlas_refalias3.h   $(INCSdir)/atlas_ptalias1.h  \
   $(INCSdir)/atlas_ptalias2.h    $(INCSdir)/atlas_ptalias3.h  \
   $(INCSdir)/atlas_level1.h      $(INCSdir)/atlas_level2.h    \
   $(INCSdir)/atlas_level3.h
#
## Object files ########################################################
# 
aobj     = \
   dcabs1.o lsame.o xerbla.o
#
slvl1obj = \
   srotg.o srotmg.o sscal.o snrm2.o sasum.o isamax.o saxpy.o scopy.o sswap.o \
   srot.o srotm.o sdot.o dsdot.o sdsdot.o
#
dlvl1obj = \
   drotg.o drotmg.o dscal.o dnrm2.o dasum.o idamax.o daxpy.o dcopy.o dswap.o \
   drot.o drotm.o ddot.o
#
clvl1obj = \
   crotg.o cscal.o csscal.o scnrm2.o scasum.o icamax.o caxpy.o ccopy.o \
   cswap.o csrot.o cdotc.o cdotu.o
#
zlvl1obj = \
   zrotg.o zscal.o zdscal.o dznrm2.o dzasum.o izamax.o zaxpy.o zcopy.o \
   zswap.o zdrot.o zdotc.o zdotu.o
#
slvl2obj = \
   sgbmv.o sgemv.o sger.o sger2.o ssbmv.o sspmv.o sspr.o sspr2.o ssymv.o \
   ssyr.o ssyr2.o stbmv.o stbsv.o stpmv.o stpsv.o strmv.o strsv.o
#
dlvl2obj = \
   dgbmv.o dgemv.o dger.o dger2.o dsbmv.o dspmv.o dspr.o dspr2.o dsymv.o \
   dsyr.o dsyr2.o dtbmv.o dtbsv.o dtpmv.o dtpsv.o dtrmv.o dtrsv.o
#
clvl2obj = \
   cgbmv.o cgemv.o cger2c.o cger2u.o cgerc.o cgeru.o chbmv.o chemv.o cher.o \
   cher2.o chpmv.o chpr.o chpr2.o ctbmv.o ctbsv.o ctpmv.o ctpsv.o ctrmv.o \
   ctrsv.o
#
zlvl2obj = \
   zgbmv.o zgemv.o zger2c.o zger2u.o zgerc.o zgeru.o zhbmv.o zhemv.o zher.o \
   zher2.o zhpmv.o zhpr.o zhpr2.o ztbmv.o ztbsv.o ztpmv.o ztpsv.o ztrmv.o \
   ztrsv.o
#
slvl3obj = \
   sgemm.o ssymm.o ssyr2k.o ssyrk.o strmm.o strsm.o
#
dlvl3obj = \
   dgemm.o dsymm.o dsyr2k.o dsyrk.o dtrmm.o dtrsm.o
#
clvl3obj = \
   cgemm.o chemm.o cher2k.o cherk.o csymm.o csyr2k.o csyrk.o ctrmm.o ctrsm.o
#
zlvl3obj = \
   zgemm.o zhemm.o zher2k.o zherk.o zsymm.o zsyr2k.o zsyrk.o ztrmm.o ztrsm.o
#
ATL_slvl1obj = \
   ATL_F77wrap_srotg.o ATL_F77wrap_srotmg.o ATL_F77wrap_sscal.o \
   ATL_F77wrap_snrm2.o ATL_F77wrap_sasum.o ATL_F77wrap_isamax.o \
   ATL_F77wrap_saxpy.o ATL_F77wrap_scopy.o ATL_F77wrap_sswap.o \
   ATL_F77wrap_srot.o ATL_F77wrap_srotm.o ATL_F77wrap_sdot.o \
   ATL_F77wrap_dsdot.o ATL_F77wrap_sdsdot.o fatlas_saxpby.o fatlas_sset.o
#
ATL_dlvl1obj = \
   ATL_F77wrap_drotg.o ATL_F77wrap_drotmg.o ATL_F77wrap_dscal.o \
   ATL_F77wrap_dnrm2.o ATL_F77wrap_dasum.o ATL_F77wrap_idamax.o \
   ATL_F77wrap_daxpy.o ATL_F77wrap_dcopy.o ATL_F77wrap_dswap.o \
   ATL_F77wrap_drot.o ATL_F77wrap_drotm.o ATL_F77wrap_ddot.o fatlas_daxpby.o \
   fatlas_dset.o
#
ATL_clvl1obj = \
   ATL_F77wrap_crotg.o ATL_F77wrap_cscal.o ATL_F77wrap_csscal.o \
   ATL_F77wrap_scnrm2.o ATL_F77wrap_scasum.o ATL_F77wrap_icamax.o \
   ATL_F77wrap_caxpy.o ATL_F77wrap_ccopy.o ATL_F77wrap_cswap.o \
   ATL_F77wrap_csrot.o ATL_F77wrap_cdotc.o ATL_F77wrap_cdotu.o \
   fatlas_caxpby.o fatlas_cset.o
#
ATL_zlvl1obj = \
   ATL_F77wrap_zrotg.o ATL_F77wrap_zscal.o ATL_F77wrap_zdscal.o \
   ATL_F77wrap_dznrm2.o ATL_F77wrap_dzasum.o ATL_F77wrap_izamax.o \
   ATL_F77wrap_zaxpy.o ATL_F77wrap_zcopy.o ATL_F77wrap_zswap.o \
   ATL_F77wrap_zdrot.o ATL_F77wrap_zdotc.o ATL_F77wrap_zdotu.o \
   fatlas_zaxpby.o fatlas_zset.o
#
ATL_sptlvl1obj = \
   ATL_F77wrap_sptrotg.o ATL_F77wrap_sptrotmg.o ATL_F77wrap_sptscal.o \
   ATL_F77wrap_sptnrm2.o ATL_F77wrap_sptasum.o ATL_F77wrap_isptamax.o \
   ATL_F77wrap_sptaxpy.o ATL_F77wrap_sptcopy.o ATL_F77wrap_sptswap.o \
   ATL_F77wrap_sptrot.o ATL_F77wrap_sptrotm.o ATL_F77wrap_sptdot.o \
   ATL_F77wrap_dsptdot.o ATL_F77wrap_sdsptdot.o fatlas_sptaxpby.o \
   fatlas_sptset.o
#
ATL_dptlvl1obj = \
   ATL_F77wrap_dptrotg.o ATL_F77wrap_dptrotmg.o ATL_F77wrap_dptscal.o \
   ATL_F77wrap_dptnrm2.o ATL_F77wrap_dptasum.o ATL_F77wrap_idptamax.o \
   ATL_F77wrap_dptaxpy.o ATL_F77wrap_dptcopy.o ATL_F77wrap_dptswap.o \
   ATL_F77wrap_dptrot.o ATL_F77wrap_dptrotm.o ATL_F77wrap_dptdot.o \
   fatlas_dptaxpby.o fatlas_dptset.o
#
ATL_cptlvl1obj = \
   ATL_F77wrap_cptrotg.o ATL_F77wrap_cptscal.o ATL_F77wrap_csptscal.o \
   ATL_F77wrap_scptnrm2.o ATL_F77wrap_scptasum.o ATL_F77wrap_icptamax.o \
   ATL_F77wrap_cptaxpy.o ATL_F77wrap_cptcopy.o ATL_F77wrap_cptswap.o \
   ATL_F77wrap_csptrot.o ATL_F77wrap_cptdotc.o ATL_F77wrap_cptdotu.o \
   fatlas_cptaxpby.o fatlas_cptset.o
#
ATL_zptlvl1obj = \
   ATL_F77wrap_zptrotg.o ATL_F77wrap_zptscal.o ATL_F77wrap_zdptscal.o \
   ATL_F77wrap_dzptnrm2.o ATL_F77wrap_dzptasum.o ATL_F77wrap_izptamax.o \
   ATL_F77wrap_zptaxpy.o ATL_F77wrap_zptcopy.o ATL_F77wrap_zptswap.o \
   ATL_F77wrap_zdptrot.o ATL_F77wrap_zptdotc.o ATL_F77wrap_zptdotu.o \
   fatlas_zptaxpby.o fatlas_zptset.o
#
ATL_slvl2obj = \
   ATL_F77wrap_sgbmv.o ATL_F77wrap_sgemv.o ATL_F77wrap_sger.o \
   ATL_F77wrap_sger2.o ATL_F77wrap_ssbmv.o ATL_F77wrap_sspmv.o \
   ATL_F77wrap_sspr.o ATL_F77wrap_sspr2.o ATL_F77wrap_ssymv.o \
   ATL_F77wrap_ssyr.o ATL_F77wrap_ssyr2.o ATL_F77wrap_stbmv.o \
   ATL_F77wrap_stbsv.o ATL_F77wrap_stpmv.o ATL_F77wrap_stpsv.o \
   ATL_F77wrap_strmv.o ATL_F77wrap_strsv.o
#
ATL_dlvl2obj = \
   ATL_F77wrap_dgbmv.o ATL_F77wrap_dgemv.o ATL_F77wrap_dger.o \
   ATL_F77wrap_dger2.o ATL_F77wrap_dsbmv.o ATL_F77wrap_dspmv.o \
   ATL_F77wrap_dspr.o ATL_F77wrap_dspr2.o ATL_F77wrap_dsymv.o \
   ATL_F77wrap_dsyr.o ATL_F77wrap_dsyr2.o ATL_F77wrap_dtbmv.o \
   ATL_F77wrap_dtbsv.o ATL_F77wrap_dtpmv.o ATL_F77wrap_dtpsv.o \
   ATL_F77wrap_dtrmv.o ATL_F77wrap_dtrsv.o
#
ATL_clvl2obj = \
   ATL_F77wrap_cgbmv.o ATL_F77wrap_cgemv.o ATL_F77wrap_cger2c.o \
   ATL_F77wrap_cger2u.o ATL_F77wrap_cgerc.o ATL_F77wrap_cgeru.o \
   ATL_F77wrap_chbmv.o ATL_F77wrap_chemv.o ATL_F77wrap_cher.o \
   ATL_F77wrap_cher2.o ATL_F77wrap_chpmv.o ATL_F77wrap_chpr.o \
   ATL_F77wrap_chpr2.o ATL_F77wrap_ctbmv.o ATL_F77wrap_ctbsv.o \
   ATL_F77wrap_ctpmv.o ATL_F77wrap_ctpsv.o ATL_F77wrap_ctrmv.o \
   ATL_F77wrap_ctrsv.o
#
ATL_zlvl2obj = \
   ATL_F77wrap_zgbmv.o ATL_F77wrap_zgemv.o ATL_F77wrap_zger2c.o \
   ATL_F77wrap_zger2u.o ATL_F77wrap_zgerc.o ATL_F77wrap_zgeru.o \
   ATL_F77wrap_zhbmv.o ATL_F77wrap_zhemv.o ATL_F77wrap_zher.o \
   ATL_F77wrap_zher2.o ATL_F77wrap_zhpmv.o ATL_F77wrap_zhpr.o \
   ATL_F77wrap_zhpr2.o ATL_F77wrap_ztbmv.o ATL_F77wrap_ztbsv.o \
   ATL_F77wrap_ztpmv.o ATL_F77wrap_ztpsv.o ATL_F77wrap_ztrmv.o \
   ATL_F77wrap_ztrsv.o
#
ATL_sptlvl2obj = \
   ATL_F77wrap_sptgbmv.o ATL_F77wrap_sptgemv.o ATL_F77wrap_sptger.o \
   ATL_F77wrap_sptger2.o ATL_F77wrap_sptsbmv.o ATL_F77wrap_sptspmv.o \
   ATL_F77wrap_sptspr.o ATL_F77wrap_sptspr2.o ATL_F77wrap_sptsymv.o \
   ATL_F77wrap_sptsyr.o ATL_F77wrap_sptsyr2.o ATL_F77wrap_spttbmv.o \
   ATL_F77wrap_spttbsv.o ATL_F77wrap_spttpmv.o ATL_F77wrap_spttpsv.o \
   ATL_F77wrap_spttrmv.o ATL_F77wrap_spttrsv.o
#
ATL_dptlvl2obj = \
   ATL_F77wrap_dptgbmv.o ATL_F77wrap_dptgemv.o ATL_F77wrap_dptger.o \
   ATL_F77wrap_dptger2.o ATL_F77wrap_dptsbmv.o ATL_F77wrap_dptspmv.o \
   ATL_F77wrap_dptspr.o ATL_F77wrap_dptspr2.o ATL_F77wrap_dptsymv.o \
   ATL_F77wrap_dptsyr.o ATL_F77wrap_dptsyr2.o ATL_F77wrap_dpttbmv.o \
   ATL_F77wrap_dpttbsv.o ATL_F77wrap_dpttpmv.o ATL_F77wrap_dpttpsv.o \
   ATL_F77wrap_dpttrmv.o ATL_F77wrap_dpttrsv.o
#
ATL_cptlvl2obj = \
   ATL_F77wrap_cptgbmv.o ATL_F77wrap_cptgemv.o ATL_F77wrap_cptger2c.o \
   ATL_F77wrap_cptger2u.o ATL_F77wrap_cptgerc.o ATL_F77wrap_cptgeru.o \
   ATL_F77wrap_cpthbmv.o ATL_F77wrap_cpthemv.o ATL_F77wrap_cpther.o \
   ATL_F77wrap_cpther2.o ATL_F77wrap_cpthpmv.o ATL_F77wrap_cpthpr.o \
   ATL_F77wrap_cpthpr2.o ATL_F77wrap_cpttbmv.o ATL_F77wrap_cpttbsv.o \
   ATL_F77wrap_cpttpmv.o ATL_F77wrap_cpttpsv.o ATL_F77wrap_cpttrmv.o \
   ATL_F77wrap_cpttrsv.o
#
ATL_zptlvl2obj = \
   ATL_F77wrap_zptgbmv.o ATL_F77wrap_zptgemv.o ATL_F77wrap_zptger2c.o \
   ATL_F77wrap_zptger2u.o ATL_F77wrap_zptgerc.o ATL_F77wrap_zptgeru.o \
   ATL_F77wrap_zpthbmv.o ATL_F77wrap_zpthemv.o ATL_F77wrap_zpther.o \
   ATL_F77wrap_zpther2.o ATL_F77wrap_zpthpmv.o ATL_F77wrap_zpthpr.o \
   ATL_F77wrap_zpthpr2.o ATL_F77wrap_zpttbmv.o ATL_F77wrap_zpttbsv.o \
   ATL_F77wrap_zpttpmv.o ATL_F77wrap_zpttpsv.o ATL_F77wrap_zpttrmv.o \
   ATL_F77wrap_zpttrsv.o
#
ATL_slvl3obj = \
   ATL_F77wrap_sgemm.o ATL_F77wrap_ssymm.o ATL_F77wrap_ssyr2k.o \
   ATL_F77wrap_ssyrk.o ATL_F77wrap_strmm.o ATL_F77wrap_strsm.o
#
ATL_dlvl3obj = \
   ATL_F77wrap_dgemm.o ATL_F77wrap_dsymm.o ATL_F77wrap_dsyr2k.o \
   ATL_F77wrap_dsyrk.o ATL_F77wrap_dtrmm.o ATL_F77wrap_dtrsm.o
#
ATL_clvl3obj = \
   ATL_F77wrap_cgemm.o ATL_F77wrap_chemm.o ATL_F77wrap_cher2k.o \
   ATL_F77wrap_cherk.o ATL_F77wrap_csymm.o ATL_F77wrap_csyr2k.o \
   ATL_F77wrap_csyrk.o ATL_F77wrap_ctrmm.o ATL_F77wrap_ctrsm.o
#
ATL_zlvl3obj = \
   ATL_F77wrap_zgemm.o ATL_F77wrap_zhemm.o ATL_F77wrap_zher2k.o \
   ATL_F77wrap_zherk.o ATL_F77wrap_zsymm.o ATL_F77wrap_zsyr2k.o \
   ATL_F77wrap_zsyrk.o ATL_F77wrap_ztrmm.o ATL_F77wrap_ztrsm.o
#
ATL_sptlvl3obj = \
   ATL_F77wrap_sptgemm.o ATL_F77wrap_sptsymm.o ATL_F77wrap_sptsyr2k.o \
   ATL_F77wrap_sptsyrk.o ATL_F77wrap_spttrmm.o ATL_F77wrap_spttrsm.o
#
ATL_dptlvl3obj = \
   ATL_F77wrap_dptgemm.o ATL_F77wrap_dptsymm.o ATL_F77wrap_dptsyr2k.o \
   ATL_F77wrap_dptsyrk.o ATL_F77wrap_dpttrmm.o ATL_F77wrap_dpttrsm.o
#
ATL_cptlvl3obj = \
   ATL_F77wrap_cptgemm.o ATL_F77wrap_cpthemm.o ATL_F77wrap_cpther2k.o \
   ATL_F77wrap_cptherk.o ATL_F77wrap_cptsymm.o ATL_F77wrap_cptsyr2k.o \
   ATL_F77wrap_cptsyrk.o ATL_F77wrap_cpttrmm.o ATL_F77wrap_cpttrsm.o
#
ATL_zptlvl3obj = \
   ATL_F77wrap_zptgemm.o ATL_F77wrap_zpthemm.o ATL_F77wrap_zpther2k.o \
   ATL_F77wrap_zptherk.o ATL_F77wrap_zptsymm.o ATL_F77wrap_zptsyr2k.o \
   ATL_F77wrap_zptsyrk.o ATL_F77wrap_zpttrmm.o ATL_F77wrap_zpttrsm.o
#
## Targets #############################################################
#
all              : lib
#
lib              : slib dlib clib zlib
#
lvl1             : slvl1 dlvl1 clvl1 zlvl1
lvl2             : slvl2 dlvl2 clvl2 zlvl2
lvl3             : slvl3 dlvl3 clvl3 zlvl3
#
slib             : slvl1 slvl2 slvl3
dlib             : dlvl1 dlvl2 dlvl3
clib             : clvl1 clvl2 clvl3
zlib             : zlvl1 zlvl2 zlvl3
#
ptall            : ptlib
#
ptlib            : sptlib dptlib cptlib zptlib
#
ptlvl1           : sptlvl1 dptlvl1 cptlvl1 zptlvl1
ptlvl2           : sptlvl2 dptlvl2 cptlvl2 zptlvl2
ptlvl3           : sptlvl3 dptlvl3 cptlvl3 zptlvl3
#
sptlib         : sptlvl1 sptlvl2 sptlvl3
#  sptlib           : sptlvl3
dptlib         : dptlvl1 dptlvl2 dptlvl3
#  dptlib           : dptlvl3
cptlib         : cptlvl1 cptlvl2 cptlvl3
#  cptlib           : cptlvl3
zptlib         : zptlvl1 zptlvl2 zptlvl3
#  zptlib           : zptlvl3
#
########################################################################
#
slvl1            : 
	$(PMAKE) slvl1.grd
dlvl1            : 
	$(PMAKE) dlvl1.grd
clvl1            : 
	$(PMAKE) clvl1.grd
zlvl1            : 
	$(PMAKE) zlvl1.grd
#
slvl2            : 
	$(PMAKE) slvl2.grd
dlvl2            : 
	$(PMAKE) dlvl2.grd
clvl2            : 
	$(PMAKE) clvl2.grd
zlvl2            : 
	$(PMAKE) zlvl2.grd
#
slvl3            : 
	$(PMAKE) slvl3.grd
dlvl3            : 
	$(PMAKE) dlvl3.grd
clvl3            : 
	$(PMAKE) clvl3.grd
zlvl3            : 
	$(PMAKE) zlvl3.grd
#
sptlvl1          : 
	$(PMAKE) sptlvl1.grd
dptlvl1          : 
	$(PMAKE) dptlvl1.grd
cptlvl1          : 
	$(PMAKE) cptlvl1.grd
zptlvl1          : 
	$(PMAKE) zptlvl1.grd
#
sptlvl2          : 
	$(PMAKE) sptlvl2.grd
dptlvl2          : 
	$(PMAKE) dptlvl2.grd
cptlvl2          : 
	$(PMAKE) cptlvl2.grd
zptlvl2          : 
	$(PMAKE) zptlvl2.grd
#
sptlvl3          : 
	$(PMAKE) sptlvl3.grd
dptlvl3          : 
	$(PMAKE) dptlvl3.grd
cptlvl3          : 
	$(PMAKE) cptlvl3.grd
zptlvl3          : 
	$(PMAKE) zptlvl3.grd
#
slvl1.grd        : $(slvl1obj) $(ATL_slvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(slvl1obj) \
        $(ATL_slvl1obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch slvl1.grd
#
dlvl1.grd        : $(dlvl1obj) $(ATL_dlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(dlvl1obj) \
        $(ATL_dlvl1obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch dlvl1.grd
#
clvl1.grd        : $(clvl1obj) $(ATL_clvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(clvl1obj) \
        $(ATL_clvl1obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch clvl1.grd
#
zlvl1.grd        : $(zlvl1obj) $(ATL_zlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(zlvl1obj) \
        $(ATL_zlvl1obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch zlvl1.grd
#
slvl2.grd        : $(slvl2obj) $(ATL_slvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(slvl2obj) \
        $(ATL_slvl2obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch slvl2.grd
#
dlvl2.grd        : $(dlvl2obj) $(ATL_dlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(dlvl2obj) \
        $(ATL_dlvl2obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch dlvl2.grd
#
clvl2.grd        : $(clvl2obj) $(ATL_clvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(clvl2obj) \
        $(ATL_clvl2obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch clvl2.grd
#
zlvl2.grd        : $(zlvl2obj) $(ATL_zlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(zlvl2obj) \
        $(ATL_zlvl2obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch zlvl2.grd
#
slvl3.grd        : $(slvl3obj) $(ATL_slvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(slvl3obj) \
        $(ATL_slvl3obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch slvl3.grd
#
dlvl3.grd        : $(dlvl3obj) $(ATL_dlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(dlvl3obj) \
        $(ATL_dlvl3obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch dlvl3.grd
#
clvl3.grd        : $(clvl3obj) $(ATL_clvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(clvl3obj) \
        $(ATL_clvl3obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch clvl3.grd
#
zlvl3.grd        : $(zlvl3obj) $(ATL_zlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(F77BLASlib) $(zlvl3obj) \
        $(ATL_zlvl3obj) $(aobj)
	$(RANLIB) $(F77BLASlib)
	touch zlvl3.grd
#
#
sptlvl1.grd      : $(slvl1obj) $(ATL_sptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(slvl1obj) \
        $(ATL_sptlvl1obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch sptlvl1.grd
#  
dptlvl1.grd      : $(dlvl1obj) $(ATL_dptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(dlvl1obj) \
        $(ATL_dptlvl1obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch dptlvl1.grd
#  
cptlvl1.grd      : $(clvl1obj) $(ATL_cptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(clvl1obj) \
        $(ATL_cptlvl1obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch cptlvl1.grd
#  
zptlvl1.grd      : $(zlvl1obj) $(ATL_zptlvl1obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(zlvl1obj) \
        $(ATL_zptlvl1obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch zptlvl1.grd
#  
sptlvl2.grd      : $(slvl2obj) $(ATL_sptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(slvl2obj) \
        $(ATL_sptlvl2obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch sptlvl2.grd
#  
dptlvl2.grd      : $(dlvl2obj) $(ATL_dptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(dlvl2obj) \
        $(ATL_dptlvl2obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch dptlvl2.grd
#  
cptlvl2.grd      : $(clvl2obj) $(ATL_cptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(clvl2obj) \
        $(ATL_cptlvl2obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch cptlvl2.grd
#  
zptlvl2.grd      : $(zlvl2obj) $(ATL_zptlvl2obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(zlvl2obj) \
        $(ATL_zptlvl2obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch zptlvl2.grd
#  
sptlvl3.grd      : $(slvl3obj) $(ATL_sptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(slvl3obj) \
        $(ATL_sptlvl3obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch sptlvl3.grd
#  
dptlvl3.grd      : $(dlvl3obj) $(ATL_dptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(dlvl3obj) \
        $(ATL_dptlvl3obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch dptlvl3.grd
#  
cptlvl3.grd      : $(clvl3obj) $(ATL_cptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(clvl3obj) \
        $(ATL_cptlvl3obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch cptlvl3.grd
#  
zptlvl3.grd      : $(zlvl3obj) $(ATL_zptlvl3obj) $(aobj)
	$(ARCHIVER) $(ARFLAGS) $(PTF77BLASlib) $(zlvl3obj) \
        $(ATL_zptlvl3obj) $(aobj)
	$(RANLIB) $(PTF77BLASlib)
	touch zptlvl3.grd
#  
########################################################################
#
dcabs1.o             : $(mySRCdir)/dcabs1.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dcabs1.f   
xerbla.o             : $(mySRCdir)/xerbla.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/xerbla.f   
lsame.o              : $(mySRCdir)/lsame.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/lsame.f    
#
srotg.o              : $(mySRCdir)/srotg.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/srotg.f    
srotmg.o             : $(mySRCdir)/srotmg.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/srotmg.f   
sscal.o              : $(mySRCdir)/sscal.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sscal.f    
snrm2.o              : $(mySRCdir)/snrm2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/snrm2.f    
sasum.o              : $(mySRCdir)/sasum.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sasum.f    
isamax.o             : $(mySRCdir)/isamax.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/isamax.f   
saxpy.o              : $(mySRCdir)/saxpy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/saxpy.f    
scopy.o              : $(mySRCdir)/scopy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/scopy.f    
sswap.o              : $(mySRCdir)/sswap.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sswap.f    
srot.o               : $(mySRCdir)/srot.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/srot.f     
srotm.o              : $(mySRCdir)/srotm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/srotm.f    
sdot.o               : $(mySRCdir)/sdot.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sdot.f     
dsdot.o              : $(mySRCdir)/dsdot.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsdot.f    
sdsdot.o             : $(mySRCdir)/sdsdot.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sdsdot.f   
#
drotg.o              : $(mySRCdir)/drotg.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/drotg.f    
drotmg.o             : $(mySRCdir)/drotmg.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/drotmg.f   
dscal.o              : $(mySRCdir)/dscal.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dscal.f    
dnrm2.o              : $(mySRCdir)/dnrm2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dnrm2.f    
dasum.o              : $(mySRCdir)/dasum.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dasum.f    
idamax.o             : $(mySRCdir)/idamax.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/idamax.f   
daxpy.o              : $(mySRCdir)/daxpy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/daxpy.f    
dcopy.o              : $(mySRCdir)/dcopy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dcopy.f    
dswap.o              : $(mySRCdir)/dswap.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dswap.f    
drot.o               : $(mySRCdir)/drot.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/drot.f     
drotm.o              : $(mySRCdir)/drotm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/drotm.f    
ddot.o               : $(mySRCdir)/ddot.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ddot.f     
#
crotg.o              : $(mySRCdir)/crotg.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/crotg.f    
cscal.o              : $(mySRCdir)/cscal.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cscal.f    
csscal.o             : $(mySRCdir)/csscal.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/csscal.f   
scnrm2.o             : $(mySRCdir)/scnrm2.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/scnrm2.f   
scasum.o             : $(mySRCdir)/scasum.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/scasum.f   
icamax.o             : $(mySRCdir)/icamax.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/icamax.f   
caxpy.o              : $(mySRCdir)/caxpy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/caxpy.f    
ccopy.o              : $(mySRCdir)/ccopy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ccopy.f    
cswap.o              : $(mySRCdir)/cswap.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cswap.f    
csrot.o              : $(mySRCdir)/csrot.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/csrot.f    
cdotc.o              : $(mySRCdir)/cdotc.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cdotc.f    
cdotu.o              : $(mySRCdir)/cdotu.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cdotu.f    
#
zrotg.o              : $(mySRCdir)/zrotg.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zrotg.f    
zscal.o              : $(mySRCdir)/zscal.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zscal.f    
zdscal.o             : $(mySRCdir)/zdscal.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zdscal.f   
dznrm2.o             : $(mySRCdir)/dznrm2.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dznrm2.f   
dzasum.o             : $(mySRCdir)/dzasum.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dzasum.f   
izamax.o             : $(mySRCdir)/izamax.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/izamax.f   
zaxpy.o              : $(mySRCdir)/zaxpy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zaxpy.f    
zcopy.o              : $(mySRCdir)/zcopy.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zcopy.f    
zswap.o              : $(mySRCdir)/zswap.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zswap.f    
zdrot.o              : $(mySRCdir)/zdrot.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zdrot.f    
zdotc.o              : $(mySRCdir)/zdotc.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zdotc.f    
zdotu.o              : $(mySRCdir)/zdotu.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zdotu.f    
#
sgbmv.o              : $(mySRCdir)/sgbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sgbmv.f    
sgemv.o              : $(mySRCdir)/sgemv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sgemv.f    
sger2.o              : $(mySRCdir)/sger2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sger2.f    
sger.o               : $(mySRCdir)/sger.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sger.f     
ssbmv.o              : $(mySRCdir)/ssbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssbmv.f    
sspmv.o              : $(mySRCdir)/sspmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sspmv.f    
ssymv.o              : $(mySRCdir)/ssymv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssymv.f    
sspr.o               : $(mySRCdir)/sspr.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sspr.f     
ssyr.o               : $(mySRCdir)/ssyr.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssyr.f     
sspr2.o              : $(mySRCdir)/sspr2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sspr2.f    
ssyr2.o              : $(mySRCdir)/ssyr2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssyr2.f    
stbmv.o              : $(mySRCdir)/stbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/stbmv.f    
stpmv.o              : $(mySRCdir)/stpmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/stpmv.f    
strmv.o              : $(mySRCdir)/strmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/strmv.f    
stbsv.o              : $(mySRCdir)/stbsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/stbsv.f    
stpsv.o              : $(mySRCdir)/stpsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/stpsv.f    
strsv.o              : $(mySRCdir)/strsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/strsv.f    
#
dgbmv.o              : $(mySRCdir)/dgbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dgbmv.f    
dgemv.o              : $(mySRCdir)/dgemv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dgemv.f    
dger2.o              : $(mySRCdir)/dger2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dger2.f    
dger.o               : $(mySRCdir)/dger.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dger.f     
dsbmv.o              : $(mySRCdir)/dsbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsbmv.f    
dspmv.o              : $(mySRCdir)/dspmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dspmv.f    
dsymv.o              : $(mySRCdir)/dsymv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsymv.f    
dspr.o               : $(mySRCdir)/dspr.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dspr.f     
dsyr.o               : $(mySRCdir)/dsyr.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsyr.f     
dspr2.o              : $(mySRCdir)/dspr2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dspr2.f    
dsyr2.o              : $(mySRCdir)/dsyr2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsyr2.f    
dtbmv.o              : $(mySRCdir)/dtbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtbmv.f    
dtpmv.o              : $(mySRCdir)/dtpmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtpmv.f    
dtrmv.o              : $(mySRCdir)/dtrmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtrmv.f    
dtbsv.o              : $(mySRCdir)/dtbsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtbsv.f    
dtpsv.o              : $(mySRCdir)/dtpsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtpsv.f    
dtrsv.o              : $(mySRCdir)/dtrsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtrsv.f    
#
cgbmv.o              : $(mySRCdir)/cgbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cgbmv.f    
cgemv.o              : $(mySRCdir)/cgemv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cgemv.f    
cger2c.o             : $(mySRCdir)/cger2c.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cger2c.f   
cger2u.o             : $(mySRCdir)/cger2u.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cger2u.f   
cgerc.o              : $(mySRCdir)/cgerc.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cgerc.f    
cgeru.o              : $(mySRCdir)/cgeru.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cgeru.f    
chbmv.o              : $(mySRCdir)/chbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/chbmv.f    
chpmv.o              : $(mySRCdir)/chpmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/chpmv.f    
chemv.o              : $(mySRCdir)/chemv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/chemv.f    
chpr.o               : $(mySRCdir)/chpr.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/chpr.f     
cher.o               : $(mySRCdir)/cher.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cher.f     
chpr2.o              : $(mySRCdir)/chpr2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/chpr2.f    
cher2.o              : $(mySRCdir)/cher2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cher2.f    
ctbmv.o              : $(mySRCdir)/ctbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctbmv.f    
ctpmv.o              : $(mySRCdir)/ctpmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctpmv.f    
ctrmv.o              : $(mySRCdir)/ctrmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctrmv.f    
ctbsv.o              : $(mySRCdir)/ctbsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctbsv.f    
ctpsv.o              : $(mySRCdir)/ctpsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctpsv.f    
ctrsv.o              : $(mySRCdir)/ctrsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctrsv.f    
#
zgbmv.o              : $(mySRCdir)/zgbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zgbmv.f    
zgemv.o              : $(mySRCdir)/zgemv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zgemv.f    
zger2c.o             : $(mySRCdir)/zger2c.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zger2c.f   
zger2u.o             : $(mySRCdir)/zger2u.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zger2u.f   
zgerc.o              : $(mySRCdir)/zgerc.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zgerc.f    
zgeru.o              : $(mySRCdir)/zgeru.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zgeru.f    
zhbmv.o              : $(mySRCdir)/zhbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zhbmv.f    
zhpmv.o              : $(mySRCdir)/zhpmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zhpmv.f    
zhemv.o              : $(mySRCdir)/zhemv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zhemv.f    
zhpr.o               : $(mySRCdir)/zhpr.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zhpr.f     
zher.o               : $(mySRCdir)/zher.f     
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zher.f     
zhpr2.o              : $(mySRCdir)/zhpr2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zhpr2.f    
zher2.o              : $(mySRCdir)/zher2.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zher2.f    
ztbmv.o              : $(mySRCdir)/ztbmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztbmv.f    
ztpmv.o              : $(mySRCdir)/ztpmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztpmv.f    
ztrmv.o              : $(mySRCdir)/ztrmv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztrmv.f    
ztbsv.o              : $(mySRCdir)/ztbsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztbsv.f    
ztpsv.o              : $(mySRCdir)/ztpsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztpsv.f    
ztrsv.o              : $(mySRCdir)/ztrsv.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztrsv.f    
#
sgemm.o              : $(mySRCdir)/sgemm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/sgemm.f    
ssymm.o              : $(mySRCdir)/ssymm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssymm.f    
ssyrk.o              : $(mySRCdir)/ssyrk.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssyrk.f    
ssyr2k.o             : $(mySRCdir)/ssyr2k.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ssyr2k.f   
strmm.o              : $(mySRCdir)/strmm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/strmm.f    
strsm.o              : $(mySRCdir)/strsm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/strsm.f    
#
dgemm.o              : $(mySRCdir)/dgemm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dgemm.f    
dsymm.o              : $(mySRCdir)/dsymm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsymm.f    
dsyrk.o              : $(mySRCdir)/dsyrk.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsyrk.f    
dsyr2k.o             : $(mySRCdir)/dsyr2k.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dsyr2k.f   
dtrmm.o              : $(mySRCdir)/dtrmm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtrmm.f    
dtrsm.o              : $(mySRCdir)/dtrsm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/dtrsm.f    
#
cgemm.o              : $(mySRCdir)/cgemm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cgemm.f    
chemm.o              : $(mySRCdir)/chemm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/chemm.f    
cherk.o              : $(mySRCdir)/cherk.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cherk.f    
cher2k.o             : $(mySRCdir)/cher2k.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/cher2k.f   
csymm.o              : $(mySRCdir)/csymm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/csymm.f    
csyrk.o              : $(mySRCdir)/csyrk.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/csyrk.f    
csyr2k.o             : $(mySRCdir)/csyr2k.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/csyr2k.f   
ctrmm.o              : $(mySRCdir)/ctrmm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctrmm.f    
ctrsm.o              : $(mySRCdir)/ctrsm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ctrsm.f    
#
zgemm.o              : $(mySRCdir)/zgemm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zgemm.f    
zhemm.o              : $(mySRCdir)/zhemm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zhemm.f    
zherk.o              : $(mySRCdir)/zherk.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zherk.f    
zher2k.o             : $(mySRCdir)/zher2k.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zher2k.f   
zsymm.o              : $(mySRCdir)/zsymm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zsymm.f    
zsyrk.o              : $(mySRCdir)/zsyrk.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zsyrk.f    
zsyr2k.o             : $(mySRCdir)/zsyr2k.f   
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/zsyr2k.f   
ztrmm.o              : $(mySRCdir)/ztrmm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztrmm.f    
ztrsm.o              : $(mySRCdir)/ztrsm.f    
	$(F77) -o $@ -c $(F77FLAGS) $(mySRCdir)/ztrsm.f    
#
ATL_F77wrap_srotg.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_srotmg.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c
ATL_F77wrap_sscal.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_snrm2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_sasum.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_isamax.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_saxpy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_scopy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_sswap.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_srot.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_srotm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c
ATL_F77wrap_sdot.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c
ATL_F77wrap_dsdot.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_dsdot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_dsdot.c
ATL_F77wrap_sdsdot.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_sdsdot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_sdsdot.c
#
ATL_F77wrap_drotg.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_drotmg.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c
ATL_F77wrap_dscal.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_dnrm2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_dasum.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_idamax.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_daxpy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_dcopy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_dswap.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_drot.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_drotm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c
ATL_F77wrap_ddot.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c
#
ATL_F77wrap_crotg.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_cscal.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_csscal.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c
ATL_F77wrap_scnrm2.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_scasum.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_icamax.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_caxpy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_ccopy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_cswap.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_csrot.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_cdotc.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c
ATL_F77wrap_cdotu.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c
#
ATL_F77wrap_zrotg.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_zscal.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_zdscal.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c
ATL_F77wrap_dznrm2.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_dzasum.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_izamax.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_zaxpy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_zcopy.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_zswap.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_zdrot.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_zdotc.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c
ATL_F77wrap_zdotu.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c
#
ATL_F77wrap_sptrotg.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_sptrotmg.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c
ATL_F77wrap_sptscal.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_sptnrm2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_sptasum.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_isptamax.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_sptaxpy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_sptcopy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_sptswap.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_sptrot.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_sptrotm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c
ATL_F77wrap_sptdot.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c
ATL_F77wrap_dsptdot.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_dsdot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_dsdot.c
ATL_F77wrap_sdsptdot.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_sdsdot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_sdsdot.c
#
ATL_F77wrap_dptrotg.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_dptrotmg.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotmg.c
ATL_F77wrap_dptscal.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_dptnrm2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_dptasum.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_idptamax.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_dptaxpy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_dptcopy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_dptswap.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_dptrot.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_dptrotm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_rotm.c
ATL_F77wrap_dptdot.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_dot.c
#
ATL_F77wrap_cptrotg.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_cptscal.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_csptscal.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c
ATL_F77wrap_scptnrm2.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_scptasum.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_icptamax.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_cptaxpy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_cptcopy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_cptswap.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_csptrot.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_cptdotc.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c
ATL_F77wrap_cptdotu.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c
#
ATL_F77wrap_zptrotg.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rotg.c
ATL_F77wrap_zptscal.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_scal.c
ATL_F77wrap_zdptscal.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rscal.c
ATL_F77wrap_dzptnrm2.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_nrm2.c
ATL_F77wrap_dzptasum.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_asum.c
ATL_F77wrap_izptamax.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_amax.c
ATL_F77wrap_zptaxpy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_axpy.c
ATL_F77wrap_zptcopy.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_copy.c
ATL_F77wrap_zptswap.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_swap.c
ATL_F77wrap_zdptrot.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_rot.c
ATL_F77wrap_zptdotc.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotc.c
ATL_F77wrap_zptdotu.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_dotu.c
#
ATL_F77wrap_sgbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_sgemv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_sger2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c
ATL_F77wrap_sger.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c
ATL_F77wrap_ssbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c
ATL_F77wrap_sspmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c
ATL_F77wrap_ssymv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c
ATL_F77wrap_sspr.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c
ATL_F77wrap_ssyr.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c
ATL_F77wrap_sspr2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c
ATL_F77wrap_ssyr2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c
ATL_F77wrap_stbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_stpmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_strmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_stbsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_stpsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_strsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_dgbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_dgemv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_dger2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c
ATL_F77wrap_dger.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c
ATL_F77wrap_dsbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c
ATL_F77wrap_dspmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c
ATL_F77wrap_dsymv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c
ATL_F77wrap_dspr.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c
ATL_F77wrap_dsyr.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c
ATL_F77wrap_dspr2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c
ATL_F77wrap_dsyr2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c
ATL_F77wrap_dtbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_dtpmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_dtrmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_dtbsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_dtpsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_dtrsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_cgbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_cgemv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_cger2c.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c
ATL_F77wrap_cger2u.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c
ATL_F77wrap_cgerc.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c
ATL_F77wrap_cgeru.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c
ATL_F77wrap_chbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c
ATL_F77wrap_chpmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c
ATL_F77wrap_chemv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c
ATL_F77wrap_chpr.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c
ATL_F77wrap_cher.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_her.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her.c
ATL_F77wrap_chpr2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c
ATL_F77wrap_cher2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c
ATL_F77wrap_ctbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_ctpmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_ctrmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_ctbsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_ctpsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_ctrsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_zgbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_zgemv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_zger2c.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c
ATL_F77wrap_zger2u.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c
ATL_F77wrap_zgerc.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c
ATL_F77wrap_zgeru.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c
ATL_F77wrap_zhbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c
ATL_F77wrap_zhpmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c
ATL_F77wrap_zhemv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c
ATL_F77wrap_zhpr.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c
ATL_F77wrap_zher.o       : $(mySRCdir)/f77wrap/ATL_F77wrap_her.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her.c
ATL_F77wrap_zhpr2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c
ATL_F77wrap_zher2.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c
ATL_F77wrap_ztbmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_ztpmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_ztrmv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_ztbsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_ztpsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_ztrsv.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_sptgbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_sptgemv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_sptger2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c
ATL_F77wrap_sptger.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c
ATL_F77wrap_sptsbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c
ATL_F77wrap_sptspmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c
ATL_F77wrap_sptsymv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c
ATL_F77wrap_sptspr.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c
ATL_F77wrap_sptsyr.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c
ATL_F77wrap_sptspr2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c
ATL_F77wrap_sptsyr2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c
ATL_F77wrap_spttbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_spttpmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_spttrmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_spttbsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_spttpsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_spttrsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_dptgbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_dptgemv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_dptger2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger2.c
ATL_F77wrap_dptger.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_ger.c
ATL_F77wrap_dptsbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_sbmv.c
ATL_F77wrap_dptspmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spmv.c
ATL_F77wrap_dptsymv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symv.c
ATL_F77wrap_dptspr.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr.c
ATL_F77wrap_dptsyr.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr.c
ATL_F77wrap_dptspr2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_spr2.c
ATL_F77wrap_dptsyr2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2.c
ATL_F77wrap_dpttbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_dpttpmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_dpttrmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_dpttbsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_dpttpsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_dpttrsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_cptgbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_cptgemv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_cptger2c.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c
ATL_F77wrap_cptger2u.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c
ATL_F77wrap_cptgerc.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c
ATL_F77wrap_cptgeru.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c
ATL_F77wrap_cpthbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c
ATL_F77wrap_cpthpmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c
ATL_F77wrap_cpthemv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c
ATL_F77wrap_cpthpr.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c
ATL_F77wrap_cpther.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_her.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her.c
ATL_F77wrap_cpthpr2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c
ATL_F77wrap_cpther2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c
ATL_F77wrap_cpttbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_cpttpmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_cpttrmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_cpttbsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_cpttpsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_cpttrsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_zptgbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gbmv.c
ATL_F77wrap_zptgemv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemv.c
ATL_F77wrap_zptger2c.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2c.c
ATL_F77wrap_zptger2u.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_ger2u.c
ATL_F77wrap_zptgerc.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gerc.c
ATL_F77wrap_zptgeru.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_geru.c
ATL_F77wrap_zpthbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hbmv.c
ATL_F77wrap_zpthpmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpmv.c
ATL_F77wrap_zpthemv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemv.c
ATL_F77wrap_zpthpr.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr.c
ATL_F77wrap_zpther.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_her.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her.c
ATL_F77wrap_zpthpr2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hpr2.c
ATL_F77wrap_zpther2.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2.c
ATL_F77wrap_zpttbmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbmv.c
ATL_F77wrap_zpttpmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpmv.c
ATL_F77wrap_zpttrmv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmv.c
ATL_F77wrap_zpttbsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tbsv.c
ATL_F77wrap_zpttpsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_tpsv.c
ATL_F77wrap_zpttrsv.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsv.c
#
ATL_F77wrap_sgemm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_ssymm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_ssyrk.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_ssyr2k.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_strmm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_strsm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_dgemm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_dsymm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_dsyrk.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_dsyr2k.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_dtrmm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_dtrsm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_cgemm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_chemm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c
ATL_F77wrap_cherk.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c
ATL_F77wrap_cher2k.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c
ATL_F77wrap_csymm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_csyrk.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_csyr2k.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_ctrmm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_ctrsm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_zgemm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_zhemm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c
ATL_F77wrap_zherk.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c
ATL_F77wrap_zher2k.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c
ATL_F77wrap_zsymm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_zsyrk.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_zsyr2k.o     : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_ztrmm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_ztrsm.o      : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_sptgemm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_sptsymm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_sptsyrk.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_sptsyr2k.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_spttrmm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_spttrsm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_dptgemm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_dptsymm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_dptsyrk.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_dptsyr2k.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_dpttrmm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_dpttrsm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDREAL $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_cptgemm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_cpthemm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c
ATL_F77wrap_cptherk.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c
ATL_F77wrap_cpther2k.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c
ATL_F77wrap_cptsymm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_cptsyrk.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_cptsyr2k.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_cpttrmm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_cpttrsm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DSCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
ATL_F77wrap_zptgemm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_gemm.c
ATL_F77wrap_zpthemm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_hemm.c
ATL_F77wrap_zptherk.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_herk.c
ATL_F77wrap_zpther2k.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_her2k.c
ATL_F77wrap_zptsymm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_symm.c
ATL_F77wrap_zptsyrk.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syrk.c
ATL_F77wrap_zptsyr2k.o   : $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_syr2k.c
ATL_F77wrap_zpttrmm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trmm.c
ATL_F77wrap_zpttrsm.o    : $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c $(INCdep)
	$(ICC) -o $@ -c $(ICCFLAGS) -DATL_USEPTHREADS -DDCPLX $(mySRCdir)/f77wrap/ATL_F77wrap_trsm.c
#
fatlas_sptset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DSREAL -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_sptaxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DSREAL -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_dptset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DDREAL -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_dptaxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DDREAL -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_cptset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DSCPLX -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_cptaxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DSCPLX -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_zptset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DDCPLX -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_zptaxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DDCPLX -DATL_USEPTHREADS -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_sset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DSREAL -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_saxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DSREAL -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_dset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DDREAL -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_daxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DDREAL -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_cset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DSCPLX -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_caxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DSCPLX -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
fatlas_zset.o : $(mySRCdir)/f77wrap/fatlas_set.c
	$(ICC) $(ICCFLAGS) -DDCPLX -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_set.c
fatlas_zaxpby.o : $(mySRCdir)/f77wrap/fatlas_axpby.c
	$(ICC) $(ICCFLAGS) -DDCPLX -o $@ -c \
           $(mySRCdir)/f77wrap/fatlas_axpby.c
########################################################################
#
sclean           :
	rm -f $(slvl1obj) $(ATL_slvl1obj)
	rm -f $(slvl2obj) $(ATL_slvl2obj)
	rm -f $(slvl3obj) $(ATL_slvl3obj)
	rm -f $(aobj) s*.grd
dclean           :
	rm -f $(dlvl1obj) $(ATL_dlvl1obj)
	rm -f $(dlvl2obj) $(ATL_dlvl2obj)
	rm -f $(dlvl3obj) $(ATL_dlvl3obj)
	rm -f $(aobj) d*.grd
cclean           :
	rm -f $(clvl1obj) $(ATL_clvl1obj)
	rm -f $(clvl2obj) $(ATL_clvl2obj)
	rm -f $(clvl3obj) $(ATL_clvl3obj)
	rm -f $(aobj) c*.grd
zclean           :
	rm -f $(zlvl1obj) $(ATL_zlvl1obj)
	rm -f $(zlvl2obj) $(ATL_zlvl2obj)
	rm -f $(zlvl3obj) $(ATL_zlvl3obj)
	rm -f $(aobj) z*.grd
#
sptclean         :
	rm -f $(slvl1obj) $(ATL_sptlvl1obj)
	rm -f $(slvl2obj) $(ATL_sptlvl2obj)
	rm -f $(slvl3obj) $(ATL_sptlvl3obj)
	rm -f $(aobj) s*.grd
dptclean         :
	rm -f $(dlvl1obj) $(ATL_dptlvl1obj)
	rm -f $(dlvl2obj) $(ATL_dptlvl2obj)
	rm -f $(dlvl3obj) $(ATL_dptlvl3obj)
	rm -f $(aobj) d*.grd
cptclean         :
	rm -f $(clvl1obj) $(ATL_cptlvl1obj)
	rm -f $(clvl2obj) $(ATL_cptlvl2obj)
	rm -f $(clvl3obj) $(ATL_cptlvl3obj)
	rm -f $(aobj) c*.grd
zptclean         :
	rm -f $(zlvl1obj) $(ATL_zptlvl1obj)
	rm -f $(zlvl2obj) $(ATL_zptlvl2obj)
	rm -f $(zlvl3obj) $(ATL_zptlvl3obj)
	rm -f $(aobj) z*.grd
#
ptclean          :
	rm -f *.o *.grd
#
clean            :
	rm -f *.o *.grd
#
########################################################################
