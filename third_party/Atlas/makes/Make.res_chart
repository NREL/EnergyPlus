include ../Make.plinc
#
# List of macros that we need
#
pre=d#          # precision [s,d,c,z]
l3=trsm#        # l3 routine [symm,hemm,syrk,herk,syr2k,her2k,trmm,trsm]
la=getrf#       # lapack routine [getrf,geqrf,potrf]
rt=$(l3)#       # l3blas routine [symm,hemm,syrk,herk,syr2k,her2k,trmm,trsm]
                # or lapack routine [getrf,geqrf,potrf]
sd=L#           # side [L,R]
up=U#           # uplo [U,L]
ta=N#           # transA [N,T,C]
tb=N#           # transB [N,T,C]
sz=med#         # size [tin,med,lrg,mlr,cmb]
pt=#            # if _pt, then do threaded, if blank do serial
lib=atl#        # library [atl,at2,sys,f77]
AT2dir=/home/whaley/TEST/ATLAS3.9.45/obj64/lib#        # path to 2nd ATLAS install to compare against


$(pre)$(rt)_$(sd)$(up)_$(sz)_avf$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(sd)$(up)N_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) la=$(rt) sd=$(sd) \
           up=$(up) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(sd)$(up)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_avf$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) sd=$(sd) up=$(up) \
           ta=$(ta) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(ta)$(tb)_$(sz)_avf$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(ta)$(tb)_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) ta=$(ta) tb=$(tb) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(ta)$(tb)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(sd)$(up)_$(sz)_avs$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(sd)$(up)N_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) la=$(rt) sd=$(sd) \
           up=$(up) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(sd)$(up)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_avs$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) sd=$(sd) up=$(up) \
           ta=$(ta) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(ta)$(tb)_$(sz)_avs$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(ta)$(tb)_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) ta=$(ta) tb=$(tb) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(ta)$(tb)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(sd)$(up)_$(sz)_ava$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(sd)$(up)N_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) la=$(rt) sd=$(sd) \
           up=$(up) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(sd)$(up)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_ava$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) sd=$(sd) up=$(up) \
           ta=$(ta) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(sd)$(up)$(ta)_$(sz)_$(lib)$(pt).pl2
$(pre)$(rt)_$(ta)$(tb)_$(sz)_ava$(pt).eps : force_build $(ext)
	cd ../tvecs ; \
           $(MAKE) $(pre)$(rt)_$(ta)$(tb)_$(sz)_$(lib)$(pt).pl2 \
           pre=$(pre) lib=atl pt="$(pt)" rt=$(rt) ta=$(ta) tb=$(tb) sz=$(sz)
	$(MAKE) plot2bars nm1="atl" nm2=$(cmp) outf=$@ \
           title="$(pre)$(rt): ATLAS$(pt) vs. $(cmp)$(pt)" \
           dataf=../tvecs/$(pre)$(rt)_$(ta)$(tb)_$(sz)_$(lib)$(pt).pl2

$(pre)pcmm_l3sy_$(sd)$(up)$(ta)_$(lib)$(pt).eps : force_build
	cd ../tvecs ; $(MAKE) $(pre)mmvsy_$(sd)$(up)$(ta)_$(lib)$(pt).pl3p \
           pre=$(pre) lib=$(lib) pt="$(pt)" sd=$(sd) up=$(up) ta=$(ta)
	$(ext) -b $(mySRCdir)/report.base -o tmp.pls rout=pclinechart \
           -def datf "../tvecs/$(pre)mmvsy_$(sd)$(up)$(ta)_$(lib)$(pt).pl3p" \
           -def nlines "3" -def labs "SYMM SYRK SYR2K"
	$(PLOT) tmp.pls -eps -o $@

$(pre)pcmm_l3tr_$(sd)$(up)$(ta)_$(lib)$(pt).eps : force_build
	cd ../tvecs ; $(MAKE) $(pre)mmvtr_$(sd)$(up)$(ta)_$(lib)$(pt).pl2p \
           pre=$(pre) lib=$(lib) pt="$(pt)" sd=$(sd) up=$(up) ta=$(ta)
	$(ext) -b $(mySRCdir)/report.base -o tmp.pls rout=pclinechart \
           -def datf "../tvecs/$(pre)mmvtr_$(sd)$(up)$(ta)_$(lib)$(pt).pl2p" \
           -def nlines "2" -def labs "TRMM TRSM"
	$(PLOT) tmp.pls -eps -o $@

$(pre)qrvar_cmb_$(lib)$(pt).eps :  force_build
	cd ../tvecs ; $(MAKE) $(pre)qrvar_cmb_$(lib).pl4s pre=$(pre) \
           lib=$(lib) pt="$(pt)"
	$(MAKE) plot4lines dataf=../tvecs/$(pre)qrvar_cmb_$(lib).pl4s outf=$@ \
        title="$(lib) QR variant performance" \
        nm1="QR" nm2="QL" nm3="RQ" nm4="LQ" 
$(pre)factor_cmb_$(lib)$(pt).eps : $(ext) force_build
	cd ../tvecs ; $(MAKE) $(pre)factor_cmb_$(lib)$(pt).pl8 pre=$(pre) \
           lib=$(lib) pt="$(pt)"
	$(ext) -b $(mySRCdir)/report.base -o tmp.pls rout=linechart \
               -def datf "../tvecs/$(pre)factor_cmb_$(lib)$(pt).pl8" \
               -def nlines "8" -def labs "MM LU LL UU QR QL RQ LQ"
	$(PLOT) tmp.pls -eps -o $@
$(pre)pcmm_factor_cmb_$(lib)$(pt).eps : $(ext) force_build
	cd ../tvecs ; $(MAKE) $(pre)factor_cmb_$(lib)$(pt).pl7p pre=$(pre) \
           lib=$(lib) pt="$(pt)"
	$(ext) -b $(mySRCdir)/report.base -o tmp.pls rout=pclinechart \
               -def datf "../tvecs/$(pre)factor_cmb_$(lib)$(pt).pl7p" \
               -def nlines "7" -def labs "LU LL UU QR QL RQ LQ"
	$(PLOT) tmp.pls -eps -o $@
