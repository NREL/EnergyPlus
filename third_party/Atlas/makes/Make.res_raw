include ../Make.plinc
#
# List of macros that we need
#
pre=d#          # precision [s,d,c,z]
l3=trsm#        # l3 routine [symm,hemm,syrk,herk,syr2k,her2k,trmm,trsm]
la=getrf#       # lapack routine [getrf,geqrf,potrf]
sd=L#           # side [L,R]
up=U#           # uplo [U,L]
ta=N#           # transA [N,T,C]
tb=N#           # transB [N,T,C]
sz=med#         # size [tin,med,lrg,mlr,cmb]
pt=#            # if _pt, then do threaded, if blank do serial
lib=atl#        # library [atl,at2,sys,f77]
AT2dir=/home/whaley/TEST/ATLAS3.9.45/obj64/lib#        # path to 2nd ATLAS install to compare against

#
# Timer targets for L3BLAS and GEMM
#
$(BINdir)/x$(pre)l3blastst_$(lib)$(pt) :
	cd $(BINdir) ; $(MAKE) x$(pre)l3blastst_$(lib)$(pt) AT2dir=$(AT2dir)
$(BINdir)/x$(pre)mmtst_$(lib)$(pt) :
	cd $(BINdir) ; $(MAKE) x$(pre)mmtst_$(lib)$(pt) AT2dir=$(AT2dir)
#
# Timer targets for LAPACK
#
$(BINdir)/x$(pre)slatime_2l_2b :
	cd $(BINdir) ; make x$(pre)slatime_2l_2b AT2dir=$(AT2dir)
$(BINdir)/x$(pre)tlatime_2l_2b :
	cd $(BINdir) ; make x$(pre)tlatime_2l_2b AT2dir=$(AT2dir)
x$(pre)latime_at2 : $(BINdir)/x$(pre)slatime_2l_2b
	- rm -f $@
	ln -s $(BINdir)/x$(pre)slatime_2l_2b $@
x$(pre)latime_at2_pt : $(BINdir)/x$(pre)tlatime_2l_2b
	- rm -f $@
	ln -s $(BINdir)/x$(pre)tlatime_2l_2b $@
$(BINdir)/x$(pre)slatime_fl_fb :
	cd $(BINdir) ; make x$(pre)slatime_fl_fb AT2dir=$(AT2dir)
$(BINdir)/x$(pre)tlatime_fl_fb :
	cd $(BINdir) ; make x$(pre)tlatime_fl_fb AT2dir=$(AT2dir)
x$(pre)latime_f77 : $(BINdir)/x$(pre)slatime_fl_fb
	- rm -f $@
	ln -s $(BINdir)/x$(pre)slatime_fl_fb $@
x$(pre)latime_f77_pt : $(BINdir)/x$(pre)tlatime_fl_fb
	- rm -f $@
	ln -s $(BINdir)/x$(pre)tlatime_fl_fb $@
$(BINdir)/x$(pre)slatime_sl_sb :
	cd $(BINdir) ; make x$(pre)slatime_sl_sb AT2dir=$(AT2dir)
$(BINdir)/x$(pre)tlatime_sl_sb :
	cd $(BINdir) ; make x$(pre)tlatime_sl_sb AT2dir=$(AT2dir)
x$(pre)latime_sys : $(BINdir)/x$(pre)slatime_sl_sb
	- rm -f $@
	ln -s $(BINdir)/x$(pre)slatime_sl_sb $@
x$(pre)latime_sys_pt : $(BINdir)/x$(pre)tlatime_sl_sb
	- rm -f $@
	ln -s $(BINdir)/x$(pre)tlatime_sl_sb $@
$(BINdir)/x$(pre)slatime_al_ab :
	cd $(BINdir) ; make x$(pre)slatime_al_ab AT2dir=$(AT2dir)
$(BINdir)/x$(pre)tlatime_al_ab :
	cd $(BINdir) ; make x$(pre)tlatime_al_ab AT2dir=$(AT2dir)
x$(pre)latime_atl : $(BINdir)/x$(pre)slatime_al_ab
	- rm -f $@
	ln -s $(BINdir)/x$(pre)slatime_al_ab $@
x$(pre)latime_atl_pt : $(BINdir)/x$(pre)tlatime_al_ab
	- rm -f $@
	ln -s $(BINdir)/x$(pre)tlatime_al_ab $@
#
# targets to run various timings, leaving raw output in .out
#
$(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).out : $(BINdir)/x$(pre)mmtst_$(lib)$(pt)
	$(BINdir)/x$(pre)mmtst_$(lib)$(pt) -F 120 -T 0 -A 1 $(ta) -B 1 $(tb) \
           -N 2400 4000 400 -# 1 > $@
$(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).out : $(BINdir)/x$(pre)mmtst_$(lib)$(pt)
	$(BINdir)/x$(pre)mmtst_$(lib)$(pt) -F 120 -T 0 -k $(RK) \
           -A 1 $(ta) -B 1 $(tb) -N 2400 4000 400 -# 1 > $@
$(pre)$(l3)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).out : \
   $(BINdir)/x$(pre)l3blastst_$(lib)$(pt)
	$(BINdir)/x$(pre)l3blastst_$(lib)$(pt) -F 120 -T 0 -R $(l3) -S 1 $(sd) \
           -U 1 $(up) -A 1 $(ta) -N 2400 4000 400 -# 1 > $@
$(pre)$(la)_$(sd)$(up)_lrg_$(lib)$(pt).out : x$(pre)latime_$(lib)$(pt) 
	./x$(pre)latime_$(lib)$(pt) -F 40 -R 1 $(la) -S 1 $(sd) \
           -U 1 $(up) -N 2400 4000 400 -# 1 > $@
$(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).out : $(BINdir)/x$(pre)mmtst_$(lib)$(pt)
	$(BINdir)/x$(pre)mmtst_$(lib)$(pt) -F 120 -T 0 -A 1 $(ta) -B 1 $(tb) \
           -N 200 2000 200 -# 6 > $@
$(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).out : $(BINdir)/x$(pre)mmtst_$(lib)$(pt)
	$(BINdir)/x$(pre)mmtst_$(lib)$(pt) -F 120 -T 0 -k $(RK) \
           -A 1 $(ta) -B 1 $(tb) -N 200 2000 200 -# 6 > $@
$(pre)$(l3)_$(sd)$(up)$(ta)_med_$(lib)$(pt).out : \
   $(BINdir)/x$(pre)l3blastst_$(lib)$(pt)
	$(BINdir)/x$(pre)l3blastst_$(lib)$(pt) -F 120 -T 0 -R $(l3) -S 1 $(sd) \
           -U 1 $(up) -A 1 $(ta) -N 200 2000 200 -# 6 > $@
$(pre)$(la)_$(sd)$(up)_med_$(lib)$(pt).out : x$(pre)latime_$(lib)$(pt) 
	./x$(pre)latime_$(lib)$(pt) -F 40 -R 1 $(la) -S 1 $(sd) \
           -U 1 $(up) -N 200 2000 200 -# 6 > $@
$(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).out : $(BINdir)/x$(pre)mmtst_$(lib)$(pt)
	$(BINdir)/x$(pre)mmtst_$(lib)$(pt) -F 120 -T 0 -A 1 $(ta) -B 1 $(tb) \
           -N 10 100 10 -# 9 > $@
$(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).out : $(BINdir)/x$(pre)mmtst_$(lib)$(pt)
	$(BINdir)/x$(pre)mmtst_$(lib)$(pt) -F 120 -T 0 -k $(RK) \
           -A 1 $(ta) -B 1 $(tb) -N 10 100 10 -# 9 > $@
$(pre)$(l3)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).out : \
   $(BINdir)/x$(pre)l3blastst_$(lib)$(pt)
	$(BINdir)/x$(pre)l3blastst_$(lib)$(pt) -F 120 -T 0 -R $(l3) -S 1 $(sd) \
           -U 1 $(up) -A 1 $(ta) -N 10 100 10 -# 9 > $@
$(pre)$(la)_$(sd)$(up)_tin_$(lib)$(pt).out : x$(pre)latime_$(lib)$(pt) 
	./x$(pre)latime_$(lib)$(pt) -F 40 -R 1 $(la) -S 1 $(sd) \
           -U 1 $(up) -N 10 100 10 -# 9 > $@

