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

#
# Build initial tvecs from raw output files.  BLAS and LAPACK targets are
# separate, because BLAS targets have twice as many reps
# BLAS targets use rt macro, and lapack don't to keep them distinct
#
../raw/$(pre)$(l3)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).out \
           pre=$(pre) l3=$(rt) sd=$(sd) up=$(up) ta=$(ta) sz=$(sz) \
           lib=$(lib) pt="$(pt)"
$(pre)$(l3)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).tvec : xatl2tvec \
   xreducetvec ../raw/$(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).out
	./xatl2tvec -# 2 -H 2 N MFLOP \
           -i ../raw/$(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).out \
           pre=$(pre) ta=$(ta) tb=$(tb) lib=$(lib) pt="$(pt)"
$(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).tvec : xatl2tvec xreducetvec \
   ../raw/$(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).out
	./xatl2tvec -# 2 -H 2 N MFLOP  \
           -i ../raw/$(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).out \
           pre=$(pre) ta=$(ta) tb=$(tb) lib=$(lib) pt="$(pt)"
$(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).tvec : xatl2tvec xreducetvec \
   ../raw/$(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).out
	./xatl2tvec -# 2 -H 2 N MFLOP  \
           -i ../raw/$(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)$(la)_$(sd)$(up)_lrg_$(lib)$(pt).out : 
	cd ../raw ; $(MAKE) $(pre)$(la)_$(sd)$(up)_lrg_$(lib)$(pt).out \
           la=$(la) sd=$(sd) up=$(up) ta=N lib=$(lib) pt="$(pt)"
$(pre)$(la)_$(sd)$(up)N_lrg_$(lib)$(pt).tvec : xatl2tvec \
   xreducetvec ../raw/$(pre)$(la)_$(sd)$(up)_lrg_$(lib)$(pt).out
	./xatl2tvec -# 1 -H 2 N MFLOP \
           -i ../raw/$(pre)$(la)_$(sd)$(up)_lrg_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)$(l3)_$(sd)$(up)$(ta)_med_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).out \
           pre=$(pre) l3=$(rt) sd=$(sd) up=$(up) ta=$(ta) sz=$(sz) \
           lib=$(lib) pt="$(pt)"
$(pre)$(l3)_$(sd)$(up)$(ta)_med_$(lib)$(pt).tvec : xatl2tvec \
   xreducetvec ../raw/$(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).out
	./xatl2tvec -# 12 -H 2 N MFLOP \
           -i ../raw/$(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).out \
           pre=$(pre) ta=$(ta) tb=$(tb) lib=$(lib) pt="$(pt)"
$(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).tvec : xatl2tvec xreducetvec \
   ../raw/$(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).out
	./xatl2tvec -# 12 -H 2 N MFLOP  \
           -i ../raw/$(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).out \
           pre=$(pre) ta=$(ta) tb=$(tb) lib=$(lib) pt="$(pt)"
$(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).tvec : xatl2tvec xreducetvec \
   ../raw/$(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).out
	./xatl2tvec -# 12 -H 2 N MFLOP  \
           -i ../raw/$(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)$(la)_$(sd)$(up)_med_$(lib)$(pt).out : 
	cd ../raw ; $(MAKE) $(pre)$(la)_$(sd)$(up)_med_$(lib)$(pt).out \
           la=$(la) sd=$(sd) up=$(up) ta=N lib=$(lib) pt="$(pt)"
$(pre)$(la)_$(sd)$(up)N_med_$(lib)$(pt).tvec : xatl2tvec \
   xreducetvec ../raw/$(pre)$(la)_$(sd)$(up)_med_$(lib)$(pt).out
	./xatl2tvec -# 6 -H 2 N MFLOP \
           -i ../raw/$(pre)$(la)_$(sd)$(up)_med_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)$(l3)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).out \
           pre=$(pre) l3=$(rt) sd=$(sd) up=$(up) ta=$(ta) sz=$(sz) \
           lib=$(lib) pt="$(pt)"
$(pre)$(l3)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).tvec : xatl2tvec \
   xreducetvec ../raw/$(pre)$(rt)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).out
	./xatl2tvec -# 18 -H 2 N MFLOP \
           -i ../raw/$(pre)$(rt)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).out \
           pre=$(pre) ta=$(ta) tb=$(tb) lib=$(lib) pt="$(pt)"
$(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).tvec : xatl2tvec xreducetvec \
   ../raw/$(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).out
	./xatl2tvec -# 18 -H 2 N MFLOP  \
           -i ../raw/$(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).out :
	cd ../raw ; $(MAKE) $(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).out \
           pre=$(pre) ta=$(ta) tb=$(tb) lib=$(lib) pt="$(pt)"
$(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).tvec : xatl2tvec xreducetvec \
   ../raw/$(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).out
	./xatl2tvec -# 18 -H 2 N MFLOP  \
           -i ../raw/$(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
../raw/$(pre)$(la)_$(sd)$(up)_tin_$(lib)$(pt).out : 
	cd ../raw ; $(MAKE) $(pre)$(la)_$(sd)$(up)_tin_$(lib)$(pt).out \
           la=$(la) sd=$(sd) up=$(up) ta=N lib=$(lib) pt="$(pt)"
$(pre)$(la)_$(sd)$(up)N_tin_$(lib)$(pt).tvec : xatl2tvec \
   xreducetvec ../raw/$(pre)$(la)_$(sd)$(up)_tin_$(lib)$(pt).out
	./xatl2tvec -# 9 -H 2 N MFLOP \
           -i ../raw/$(pre)$(la)_$(sd)$(up)_tin_$(lib)$(pt).out | \
        ./xreducetvec -o $@ -R 1 N -C 1 MFLOP
#
# Merge medium and large runs into one combined range
#
$(pre)$(rt)_$(sd)$(up)$(ta)_mlr_$(lib)$(pt).tvec : xmergetvecs \
   $(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).tvec \
   $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).tvec 
	./xmergetvecs -r N -C 2 N MFLOP_avg -o $@ \
           -i1 $(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).tvec \
           -i2 $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).tvec 
$(pre)mmrk_$(ta)$(tb)_mlr_$(lib)$(pt).tvec : xmergetvecs \
   $(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).tvec \
   $(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).tvec 
	./xmergetvecs -r N -C 2 N MFLOP_avg -o $@ \
           -i1 $(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).tvec \
           -i2 $(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).tvec
$(pre)mmrk_$(ta)$(tb)_cmb_$(lib)$(pt).tvec : xmergetvecs \
   $(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).tvec \
   $(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).tvec \
   $(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).tvec 
	./xmergetvecs -r N -C 2 N MFLOP_avg \
           -i1 $(pre)mmrk_$(ta)$(tb)_tin_$(lib)$(pt).tvec \
           -i2 $(pre)mmrk_$(ta)$(tb)_med_$(lib)$(pt).tvec | \
        ./xmergetvecs -r N -C 2 N MFLOP_avg -o $@ \
           -i2 $(pre)mmrk_$(ta)$(tb)_lrg_$(lib)$(pt).tvec
$(pre)mmsq_$(ta)$(tb)_mlr_$(lib)$(pt).tvec : xmergetvecs \
   $(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).tvec \
   $(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).tvec 
	./xmergetvecs -r N -C 2 N MFLOP_avg -o $@ \
           -i1 $(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).tvec \
           -i2 $(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).tvec
$(pre)mmsq_$(ta)$(tb)_cmb_$(lib)$(pt).tvec : xmergetvecs \
   $(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).tvec \
   $(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).tvec \
   $(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).tvec 
	./xmergetvecs -r N -C 2 N MFLOP_avg \
           -i1 $(pre)mmsq_$(ta)$(tb)_tin_$(lib)$(pt).tvec \
           -i2 $(pre)mmsq_$(ta)$(tb)_med_$(lib)$(pt).tvec | \
        ./xmergetvecs -r N -C 2 N MFLOP_avg -o $@ \
           -i2 $(pre)mmsq_$(ta)$(tb)_lrg_$(lib)$(pt).tvec
#
# Merge tiny, medium, and large runs into one combined range
#
$(pre)$(rt)_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec : xmergetvecs \
   $(pre)$(rt)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).tvec \
   $(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).tvec \
   $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).tvec 
	./xmergetvecs -r N -C 2 N MFLOP_avg \
           -i1 $(pre)$(rt)_$(sd)$(up)$(ta)_tin_$(lib)$(pt).tvec \
           -i2 $(pre)$(rt)_$(sd)$(up)$(ta)_med_$(lib)$(pt).tvec | \
        ./xmergetvecs -r N -C 2 N MFLOP_avg -o $@ \
           -i2 $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_$(lib)$(pt).tvec
#
# Direct comparison tvecs
#
$(pre)$(rt)_$(sd)$(up)$(ta)_mlr_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=sys pt="$(pt)" \
                sz=mlr
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=mlr
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_mlr_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_mlr_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=sys pt="$(pt)" sz=mlr
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_mlr_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=mlr
	cat $(pre)$(rt)_$(ta)$(tb)_mlr_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_mlr_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_cmb_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=sys pt="$(pt)" \
                sz=cmb
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=cmb
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_cmb_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_cmb_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=sys pt="$(pt)" sz=cmb
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_cmb_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=cmb
	cat $(pre)$(rt)_$(ta)$(tb)_cmb_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_cmb_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_lrg_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=sys pt="$(pt)" \
                sz=lrg
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=lrg
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_lrg_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_lrg_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=sys pt="$(pt)" sz=lrg
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_lrg_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=lrg
	cat $(pre)$(rt)_$(ta)$(tb)_lrg_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_lrg_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_med_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_med_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=sys pt="$(pt)" \
                sz=med
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_med_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=med
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_med_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_med_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_med_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_med_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=sys pt="$(pt)" sz=med
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_med_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=med
	cat $(pre)$(rt)_$(ta)$(tb)_med_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_med_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_tin_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_tin_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=sys pt="$(pt)" \
                sz=tin
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_tin_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=tin
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_tin_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_tin_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_tin_avs$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_tin_sys$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=sys pt="$(pt)" sz=tin
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_tin_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=tin
	cat $(pre)$(rt)_$(ta)$(tb)_tin_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_tin_sys$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_mlr_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=at2 pt="$(pt)" \
                sz=mlr
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=mlr
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_mlr_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_mlr_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_mlr_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=at2 pt="$(pt)" sz=mlr
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_mlr_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=mlr
	cat $(pre)$(rt)_$(ta)$(tb)_mlr_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_mlr_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_cmb_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=at2 pt="$(pt)" \
                sz=cmb
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=cmb
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_cmb_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_cmb_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_cmb_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=at2 pt="$(pt)" sz=cmb
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_cmb_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=cmb
	cat $(pre)$(rt)_$(ta)$(tb)_cmb_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_cmb_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_lrg_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=at2 pt="$(pt)" \
                sz=lrg
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=lrg
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_lrg_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_lrg_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_lrg_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=at2 pt="$(pt)" sz=lrg
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_lrg_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=lrg
	cat $(pre)$(rt)_$(ta)$(tb)_lrg_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_lrg_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_med_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_med_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=at2 pt="$(pt)" \
                sz=med
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_med_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=med
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_med_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_med_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_med_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_med_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=at2 pt="$(pt)" sz=med
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_med_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=med
	cat $(pre)$(rt)_$(ta)$(tb)_med_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_med_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(sd)$(up)$(ta)_tin_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_tin_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=at2 pt="$(pt)" \
                sz=tin
	$(MAKE) $(pre)$(rt)_$(sd)$(up)$(ta)_tin_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) sd=$(sd) up=$(up) ta=$(ta) lib=atl pt="$(pt)" sz=tin
	cat $(pre)$(rt)_$(sd)$(up)$(ta)_tin_atl$(pt).tvec \
            $(pre)$(rt)_$(sd)$(up)$(ta)_tin_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
$(pre)$(rt)_$(ta)$(tb)_tin_ava$(pt).tvec : xcattvecs force_build
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_tin_at2$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=at2 pt="$(pt)" sz=tin
	$(MAKE) $(pre)$(rt)_$(ta)$(tb)_tin_atl$(pt).tvec pre=$(pre) \
                rt=$(rt) ta=$(ta) tb=$(tb) lib=atl pt="$(pt)" sz=tin
	cat $(pre)$(rt)_$(ta)$(tb)_tin_atl$(pt).tvec \
            $(pre)$(rt)_$(ta)$(tb)_tin_at2$(pt).tvec | \
        ./xcattvecs -# 2 -o $@ -C 2 N MFLOP_avg
#
# Cat combined Side&Uplo variants together with sqmm
#
$(pre)$(rt)_SdUp_cmb_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)$(rt)_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LLN_cmb_$(lib)$(pt).tvec sd=L up=L \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_RUN_cmb_$(lib)$(pt).tvec sd=R up=U \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_RLN_cmb_$(lib)$(pt).tvec sd=R up=L \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LLN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_RUN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_RLN_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 5 -o $@ -C 2 N MFLOP_avg
#
# Cat combined Uplo&Trans variants together with sqmm for
#
$(pre)$(rt)_UpTr_cmb_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)$(rt)_LUN_cmb_$(lib)$(pt).tvec up=U ta=N \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LUT_cmb_$(lib)$(pt).tvec up=U ta=T \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LLN_cmb_$(lib)$(pt).tvec up=L ta=N \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LLT_cmb_$(lib)$(pt).tvec up=L ta=T \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LUT_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LLN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LLT_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 5 -o $@ -C 2 N MFLOP_avg
#
# Cat combined Side,Uplo,Trans, variants together with sqmm
#
$(pre)$(rt)_SdUpTr_cmb_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)$(rt)_LUN_cmb_$(lib)$(pt).tvec sd=L up=U ta=N \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LUT_cmb_$(lib)$(pt).tvec sd=L up=U ta=T \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LLN_cmb_$(lib)$(pt).tvec sd=L up=L ta=N \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_LLT_cmb_$(lib)$(pt).tvec sd=L up=L ta=T \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_RUN_cmb_$(lib)$(pt).tvec sd=R up=U ta=N \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_RUT_cmb_$(lib)$(pt).tvec sd=R up=U ta=T \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_RLN_cmb_$(lib)$(pt).tvec sd=R up=L ta=N \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)$(rt)_RLT_cmb_$(lib)$(pt).tvec sd=R up=L ta=T \
                la=$(la) rt=$(rt) pre=$(pre) pt="$(pt)" lib=$(lib)
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LUT_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LLN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_LLT_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_RUN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_RUT_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_RLN_cmb_$(lib)$(pt).tvec \
            $(pre)$(rt)_RLT_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 9 -o $@ -C 2 N MFLOP_avg
#
# Change to percentage of GEMM speed
#
$(pre)pcmm_$(rt)_SdUp_cmb_$(lib)$(pt).tvec : \
   $(pre)$(rt)_SdUp_cmb_$(lib)$(pt).tvec
	$(MAKE) pcmm4 inf=$(pre)$(rt)_SdUp_cmb_$(lib)$(pt).tvec outf=$@
$(pre)pcmm_$(rt)_UpTr_cmb_$(lib)$(pt).tvec : \
   $(pre)$(rt)_UpTr_cmb_$(lib)$(pt).tvec
	$(MAKE) pcmm4 inf=$(pre)$(rt)_UpTr_cmb_$(lib)$(pt).tvec outf=$@
$(pre)pcmm_$(rt)_SdUpTr_cmb_$(lib)$(pt).tvec : \
   $(pre)$(rt)_SdUpTr_cmb_$(lib)$(pt).tvec
	$(MAKE) pcmm8 inf=$(pre)$(rt)_UpTr_cmb_$(lib)$(pt).tvec outf=$@
#
# symmetric routs (SYMM, SYRK & SYR2K) compared to square GEMM
#
$(pre)mmvsy_$(sd)$(up)$(ta)_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)symm_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec pre=$(pre) \
           l3=symm rt=symm sd=$(sd) up=$(up) ta=$(ta) lib=$(lib) pt="$(pt)"
	$(MAKE) $(pre)syrk_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec pre=$(pre) \
           l3=syrk rt=syrk up=$(up) ta=$(ta) lib=$(lib) pt="$(pt)"
	$(MAKE) $(pre)syr2k_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec pre=$(pre) \
           l3=syr2k rt=syr2k sd=$(sd) up=$(up) ta=$(ta) lib=$(lib) pt="$(pt)"
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)symm_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec \
            $(pre)syrk_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec \
            $(pre)syr2k_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 4 -o $@ -C 2 N MFLOP_avg
#
# Triangular routs (TRMM, TRSM) compared to square GEMM
#
$(pre)mmvtr_$(sd)$(up)$(ta)_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)trmm_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec pre=$(pre) \
           l3=trmm rt=trmm sd=$(sd) up=$(up) ta=$(ta) lib=$(lib) pt="$(pt)"
	$(MAKE) $(pre)trsm_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec pre=$(pre) \
           l3=trsm rt=trsm sd=$(sd) up=$(up) ta=$(ta) lib=$(lib) pt="$(pt)"
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)trmm_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec \
            $(pre)trsm_$(sd)$(up)$(ta)_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 3 -o $@ -C 2 N MFLOP_avg

#
# Create master file with all factorizations, in this order: 
#    MM,LU,LL,UU,QR,QL,RQ,LQ
#
$(pre)factor_cmb_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)getrf_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                rt=getrf la=getrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)potrf_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                rt=potrf la=potrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)potrf_LLN_cmb_$(lib)$(pt).tvec sd=L up=L \
                rt=potrf la=potrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_RUN_cmb_$(lib)$(pt).tvec sd=R up=U \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_RLN_cmb_$(lib)$(pt).tvec sd=R up=L \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_LLN_cmb_$(lib)$(pt).tvec sd=L up=L \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)getrf_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)potrf_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)potrf_LLN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_RUN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_RLN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_LLN_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 8 -o $@ -C 2 N MFLOP_avg
# 
# Create Triangular lapack variant file with mmsq
#
$(pre)trvar_cmb_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)getrf_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                rt=getrf la=getrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)potrf_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                rt=potrf la=potrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)potrf_LLN_cmb_$(lib)$(pt).tvec sd=L up=L \
                rt=potrf la=potrf pre=$(pre) pt="$(pt)" lib=$(lib)
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)getrf_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)potrf_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)potrf_LLN_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 4 -o $@ -C 2 N MFLOP_avg

#
# Create QR variant combined file
#
$(pre)qrvar_cmb_$(lib)$(pt).tvec : xcattvecs force_build \
   $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec
	$(MAKE) $(pre)geqrf_RUN_cmb_$(lib)$(pt).tvec sd=R up=U \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_RLN_cmb_$(lib)$(pt).tvec sd=R up=L \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_LUN_cmb_$(lib)$(pt).tvec sd=L up=U \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	$(MAKE) $(pre)geqrf_LLN_cmb_$(lib)$(pt).tvec sd=L up=L \
                rt=geqrf la=geqrf pre=$(pre) pt="$(pt)" lib=$(lib)
	cat $(pre)mmsq_NN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_RUN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_RLN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_LUN_cmb_$(lib)$(pt).tvec \
            $(pre)geqrf_LLN_cmb_$(lib)$(pt).tvec | \
        ./xcattvecs -# 5 -o $@ -C 2 N MFLOP_avg

.SUFFIXES:
.SUFFIXES: .pc1 .pc2 .pc3 .pc4 .pc5 .pc6 .pc7 .pc8 .pl1 .pl1p .pl1s .pl2 \
           .pl2p .pl2s .pl3 .pl3p .pl3s .pl4 .pl4p .pl4s .pl5 .pl5p .pl5s \
           .pl6 .pl6p .pl6s .pl7 .pl7p .pl7s .pl8 .pl8p .pl8s .tvec
.tvec.pc1 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 1  MFLOP_avg_1
.tvec.pc2 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 2  MFLOP_avg_1 MFLOP_avg_2
.tvec.pc3 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 3  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3
.tvec.pc4 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 4  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4
.tvec.pc5 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 5  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5
.tvec.pc6 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 6  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6
.tvec.pc7 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 7  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7
.tvec.pc8 :
	$(MAKE) xperctvecs
	./xperctvecs -i $< -o $@ -b MFLOP_avg -C 1 N -R 8  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7 MFLOP_avg_8
.tvec.pl1s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 2 N  MFLOP_avg_1
.tvec.pl1 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 3 N MFLOP_avg  MFLOP_avg_1
.tvec.pl1p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 1  MFLOP_avg_1 | \
        ./xtvec2plp -o $@ -C 2 N  MFLOP_avg_1
.tvec.pl2s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 3 N  MFLOP_avg_1 MFLOP_avg_2
.tvec.pl2 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 4 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2
.tvec.pl2p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 2  MFLOP_avg_1 MFLOP_avg_2 | \
        ./xtvec2plp -o $@ -C 3 N  MFLOP_avg_1 MFLOP_avg_2
.tvec.pl3s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 4 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3
.tvec.pl3 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 5 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3
.tvec.pl3p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 3  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 | \
        ./xtvec2plp -o $@ -C 4 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3
.tvec.pl4s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 5 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4
.tvec.pl4 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 6 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4
.tvec.pl4p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 4  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 | \
        ./xtvec2plp -o $@ -C 5 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4
.tvec.pl5s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 6 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5
.tvec.pl5 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 7 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5
.tvec.pl5p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 5  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 | \
        ./xtvec2plp -o $@ -C 6 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5
.tvec.pl6s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 7 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6
.tvec.pl6 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 8 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6
.tvec.pl6p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 6  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 | \
        ./xtvec2plp -o $@ -C 7 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6
.tvec.pl7s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 8 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7
.tvec.pl7 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 9 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7
.tvec.pl7p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 7  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7 | \
        ./xtvec2plp -o $@ -C 8 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7
.tvec.pl8s:
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 9 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7 MFLOP_avg_8
.tvec.pl8 :
	$(MAKE) xtvec2plp
	./xtvec2plp -i $< -o $@ -C 10 N MFLOP_avg  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7 MFLOP_avg_8
.tvec.pl8p:
	$(MAKE) xtvec2plp xperctvecs
	./xperctvecs -i $< -C 1 N -b MFLOP_avg -R 8  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7 MFLOP_avg_8 | \
        ./xtvec2plp -o $@ -C 9 N  MFLOP_avg_1 MFLOP_avg_2 MFLOP_avg_3 MFLOP_avg_4 MFLOP_avg_5 MFLOP_avg_6 MFLOP_avg_7 MFLOP_avg_8
