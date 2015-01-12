include Make.inc
mySRCdir = $(SRCdir)/tune/threads
CONFdir = $(SRCdir)/CONFIG
myTHRdir = $(SRCdir)/src/threads
THRdir = $(BLDdir)/src/threads
args=
CC=$(DKC)
CCFLAGS=$(CDEFS) $(DKCFLAGS)
acnt_arch = ATL_SetAtomicCount_arch.o ATL_ResetAtomicCount_arch.o \
            ATL_DecAtomicCount_arch.o ATL_FreeAtomicCount_arch.o
acnt_mut0 = ATL_SetAtomicCount_mut_mut.o ATL_ResetAtomicCount_mut_mut.o \
            ATL_DecAtomicCount_mut_mut.o ATL_FreeAtomicCount_mut_mut.o
acnt_mut = $(THRdir)/ATL_SetAtomicCount_mut_mut.o \
           $(THRdir)/ATL_ResetAtomicCount_mut_mut.o \
           $(THRdir)/ATL_DecAtomicCount_mut_mut.o \
           $(THRdir)/ATL_FreeAtomicCount_mut_mut.o

IThreadTune :
	- $(MAKE) ITune_aff
	$(MAKE) iTune_atomic
	$(MAKE) MakeThreadSum
force_build :

sumf = $(INCAdir)/atlas_tsumm.h
MakeThreadSum :
	- rm -f $(sumf)
	echo "#ifndef ATLAS_SUMM_H" > $(sumf)
	echo "   #define ATLAS_SUMM_H" >> $(sumf)
	if [ -f res/aff.h ]; then \
           cat res/aff.h >> $(sumf) ; \
        else \
           echo "#define ATL_TAFFINITY 0" >> $(sumf) ; \
        fi
	if grep -q ATL_DecAtomicCount_mut $(THRdir)/atomic.inc ; then \
           echo "#define ATL_ATOMIC_COUNT_MUT 1" >> $(sumf) ; \
        else \
           echo "#define ATL_ATOMIC_COUNT_MUT 0" >> $(sumf) ; \
        fi
	echo "#endif" >> $(sumf)
IRun_aff : xprobe_aff force_build
	- rm res/probe.out
	./xprobe_aff -v 1 -o $(INCAdir)/atlas_taffinity.h $(TIDLIST)

IRun_nthr : IRun_aff xprobe_nthr
	rm -f $(INCAdir)/atlas_pthreads.h
	./xprobe_nthr -o $(INCAdir)/atlas_pthreads.h
ITune_aff : IRun_nthr xtune_aff
	./xtune_aff -o $(INCAdir)/atlas_taffinity.h
tlib : 
	cd $(BLDdir)/src/threads ; $(MAKE)
xprobe_aff : probe_aff.o atlconf_misc.o
	$(XCC) $(XCCFLAGS) -o xprobe_aff probe_aff.o atlconf_misc.o $(LIBS)

xprobe_atomic_win64 : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_arch.o \
           ATL_ResetAtomicCount_win64.o ATL_DecAtomicCount_win64.o \
           ATL_FreeAtomicCount_arch.o
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_arch.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_win64.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_win64.o \
              $(BLDdir)/src/threads/ATL_FreeAtomicCount_arch.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch
xprobe_atomic_amd64 : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_arch.o \
           ATL_ResetAtomicCount_amd64.o ATL_DecAtomicCount_amd64.o \
           ATL_FreeAtomicCount_arch.o
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_arch.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_amd64.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_amd64.o \
              $(BLDdir)/src/threads/ATL_FreeAtomicCount_arch.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch
xprobe_atomic_ia32 : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_arch.o \
           ATL_ResetAtomicCount_ia32.o ATL_DecAtomicCount_ia32.o \
           ATL_FreeAtomicCount_arch.o
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_arch.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_ia32.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_ia32.o \
              $(BLDdir)/src/threads/ATL_FreeAtomicCount_arch.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch
xprobe_atomic_ppc : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_arch.o \
           ATL_ResetAtomicCount_ppc.o ATL_DecAtomicCount_ppc.o \
           ATL_FreeAtomicCount_arch.o
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_arch.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_ppc.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_ppc.o \
              $(BLDdir)/src/threads/ATL_FreeAtomicCount_arch.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch
xprobe_atomic_sparc : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_arch.o \
           ATL_ResetAtomicCount_sparc.o ATL_DecAtomicCount_sparc.o \
           ATL_FreeAtomicCount_arch.o
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_arch.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_sparc.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_sparc.o \
              $(BLDdir)/src/threads/ATL_FreeAtomicCount_arch.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch
xprobe_atomic_mips : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_arch.o \
           ATL_ResetAtomicCount_mips.o ATL_DecAtomicCount_mips.o \
           ATL_FreeAtomicCount_arch.o
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_arch.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_mips.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_mips.o \
              $(BLDdir)/src/threads/ATL_FreeAtomicCount_arch.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch
xprobe_atomic_mut : test_count.o
	rm -f $(BLDdir)/src/threads/atomic.inc
	echo "aobj =" > $(BLDdir)/src/threads/atomic.inc
	$(MAKE) tlib
	cd $(BLDdir)/src/threads ; $(MAKE) ATL_SetAtomicCount_mut.o \
           ATL_ResetAtomicCount_mut.o ATL_DecAtomicCount_mut.o 
	$(CC) $(CCFLAGS) -o xtst_arch test_count.o \
              $(BLDdir)/src/threads/ATL_SetAtomicCount_mut.o \
              $(BLDdir)/src/threads/ATL_ResetAtomicCount_mut.o \
              $(BLDdir)/src/threads/ATL_DecAtomicCount_mut.o \
              $(ATLASlib) $(LIBS)
	./xtst_arch

iTune_atomic :
	if $(MAKE) mutcnt ; then \
           if $(MAKE) iFind_atomic_arch ; then \
              $(MAKE) xtune_count ; ./xtune_count -r 1000000 -o yes; \
           fi ; \
        else \
           $(MAKE) iFind_atomic_arch ; \
        fi
iFind_atomic_arch :
	if $(MAKE) xprobe_atomic_amd64 ; then \
           cp $(myTHRdir)/ATL_*AtomicCount_arch.c . ; \
           cp $(myTHRdir)/ATL_ResetAtomicCount_amd64.S \
              ATL_ResetAtomicCount_arch.S ; \
           cp $(myTHRdir)/ATL_DecAtomicCount_amd64.S \
              ATL_DecAtomicCount_arch.S ; \
           rm $(BLDdir)/src/threads/atomic.inc ; \
           echo "aobj = ATL_SetAtomicCount_arch.o ATL_ResetAtomicCount_amd64.o ATL_DecAtomicCount_amd64.o ATL_FreeAtomicCount_arch.o" > $(BLDdir)/src/threads/atomic.inc ; \
	elif $(MAKE) xprobe_atomic_win64 ; then \
           cp $(myTHRdir)/ATL_*AtomicCount_arch.c . ; \
           cp $(myTHRdir)/ATL_ResetAtomicCount_win64.S \
              ATL_ResetAtomicCount_arch.S ; \
           cp $(myTHRdir)/ATL_DecAtomicCount_win64.S \
              ATL_DecAtomicCount_arch.S ; \
           rm $(BLDdir)/src/threads/atomic.inc ; \
           echo "aobj = ATL_SetAtomicCount_arch.o ATL_ResetAtomicCount_win64.o ATL_DecAtomicCount_win64.o ATL_FreeAtomicCount_arch.o" > $(BLDdir)/src/threads/atomic.inc ; \
	elif $(MAKE) xprobe_atomic_ia32 ; then \
           cp $(myTHRdir)/ATL_*AtomicCount_arch.c . ; \
           cp $(myTHRdir)/ATL_ResetAtomicCount_ia32.S \
              ATL_ResetAtomicCount_arch.S ; \
           cp $(myTHRdir)/ATL_DecAtomicCount_ia32.S \
              ATL_DecAtomicCount_arch.S ; \
           rm $(BLDdir)/src/threads/atomic.inc ; \
           echo "aobj = ATL_SetAtomicCount_arch.o ATL_ResetAtomicCount_ia32.o ATL_DecAtomicCount_ia32.o ATL_FreeAtomicCount_arch.o" > $(BLDdir)/src/threads/atomic.inc ; \
	elif $(MAKE) xprobe_atomic_sparc ; then \
           cp $(myTHRdir)/ATL_*AtomicCount_arch.c . ; \
           cp $(myTHRdir)/ATL_ResetAtomicCount_sparc.S \
              ATL_ResetAtomicCount_arch.S ; \
           cp $(myTHRdir)/ATL_DecAtomicCount_sparc.S \
              ATL_DecAtomicCount_arch.S ; \
           rm $(BLDdir)/src/threads/atomic.inc ; \
           echo "aobj = ATL_SetAtomicCount_arch.o ATL_ResetAtomicCount_sparc.o ATL_DecAtomicCount_sparc.o ATL_FreeAtomicCount_arch.o" > $(BLDdir)/src/threads/atomic.inc ; \
	elif $(MAKE) xprobe_atomic_mips ; then \
           cp $(myTHRdir)/ATL_*AtomicCount_arch.c . ; \
           cp $(myTHRdir)/ATL_ResetAtomicCount_mips.S \
              ATL_ResetAtomicCount_arch.S ; \
           cp $(myTHRdir)/ATL_DecAtomicCount_mips.S \
              ATL_DecAtomicCount_arch.S ; \
           rm $(BLDdir)/src/threads/atomic.inc ; \
           echo "aobj = ATL_SetAtomicCount_arch.o ATL_ResetAtomicCount_mips.o ATL_DecAtomicCount_mips.o ATL_FreeAtomicCount_arch.o" > $(BLDdir)/src/threads/atomic.inc ; \
        else \
           $(MAKE) iForceUseMutex ; \
        fi
	- rm -f $(BLDdir)/src/threads/lib.grd
	$(MAKE) tlib
iForceUseMutex:
	rm $(BLDdir)/src/threads/atomic.inc 
	echo "aobj = ATL_SetAtomicCount_mut.o ATL_ResetAtomicCount_mut.o ATL_DecAtomicCount_mut.o ATL_FreeAtomicCount_mut.o" > $(BLDdir)/src/threads/atomic.inc 
xprobe_nthr : probe_nthr.o
	$(CC) $(CCFLAGS) -o xprobe_nthr probe_nthr.o $(ATLASlib) $(LIBS)
mutcnt :
	cd $(THRdir) ; $(MAKE) $(acnt_mut0)
xtune_count : tune_count.o tlib mutcnt iFind_atomic_arch
	$(MAKE) $(acnt_arch)
	$(CC) $(CCFLAGS) -o $@ tune_count.o $(acnt_arch) $(acnt_mut) \
              $(ATLASlib) $(LIBS)
xtest_count_glb : test_count_glb.o tlib  # mutcnt arccnt
	$(CC) $(CCFLAGS) -o xtest_count_glb test_count_glb.o $(ATLASlib) $(LIBS)
xtest_count : test_count.o tlib  # mutcnt arccnt
	$(CC) $(CCFLAGS) -o xtest_count test_count.o $(ATLASlib) $(LIBS)

xtune_spawn_fp : tune_spawn_fp.o tlib ATL_goparallel_lin.o ATL_goparallel_log2.o \
               ATL_goparallel_dyn.o
	$(CC) $(CCFLAGS) -o xtune_spawn_fp tune_spawn_fp.o ATL_goparallel_lin.o \
              ATL_goparallel_log2.o ATL_goparallel_dyn.o $(ATLASlib) $(LIBS)
xtune_spawn : tune_spawn.o tlib ATL_goparallel_lin.o ATL_goparallel_log2.o \
               ATL_goparallel_dyn.o
	$(CC) $(CCFLAGS) -o xtune_spawn tune_spawn.o ATL_goparallel_lin.o \
              ATL_goparallel_log2.o ATL_goparallel_dyn.o $(ATLASlib) $(LIBS)
xtune_aff : tune_aff.o tlib  ATL_goparallel_noaff.o ATL_log2tlaunch_noaff.o \
             ATL_goparallel_aff.o ATL_log2tlaunch.o \
             ATL_thread_start_noaff.o ATL_thread_start.o
	$(CC) $(CCFLAGS) -o xtune_aff tune_aff.o ATL_goparallel_noaff.o \
              ATL_goparallel_aff.o ATL_log2tlaunch_noaff.o ATL_log2tlaunch.o \
              ATL_thread_start_noaff.o ATL_thread_start.o $(ATLASlib) $(LIBS)

ATL_goparallel_aff.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -DATL_TUNING -c $(myTHRdir)/ATL_goparallel.c
ATL_goparallel_log2.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -DATL_TUNING -DATL_LAUNCH_LOG2 \
              -c $(myTHRdir)/ATL_goparallel.c
ATL_goparallel_dyn.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -DATL_TUNING -DATL_LAUNCH_DYNAMIC \
              -c $(myTHRdir)/ATL_goparallel.c
ATL_goparallel_lin.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -DATL_TUNING -DATL_LAUNCH_LINEAR \
              -c $(myTHRdir)/ATL_goparallel.c
ATL_goparallel.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -c $(myTHRdir)/ATL_goparallel.c
ATL_log2tlaunch.o : $(myTHRdir)/ATL_log2tlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c $(myTHRdir)/ATL_log2tlaunch.c
ATL_dyntlaunch.o : $(myTHRdir)/ATL_dyntlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c $(myTHRdir)/ATL_dyntlaunch.c
ATL_lin0tlaunch.o : $(myTHRdir)/ATL_lin0tlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c $(myTHRdir)/ATL_lin0tlaunch.c
ATL_thread_start.o : $(myTHRdir)/ATL_thread_start.c
	$(CC) $(CCFLAGS) -o $@ -c $(myTHRdir)/ATL_thread_start.c
ATL_goparallel_noaff.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_TUNING -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_goparallel.c
ATL_log2tlaunch_noaff.o : $(myTHRdir)/ATL_log2tlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_TUNING -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_log2tlaunch.c
ATL_dyntlaunch_noaff.o : $(myTHRdir)/ATL_dyntlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_TUNING -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_dyntlaunch.c
ATL_lin0tlaunch_noaff.o : $(myTHRdir)/ATL_lin0tlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_TUNING -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_lin0tlaunch.c
ATL_thread_start_noaff.o : $(myTHRdir)/ATL_thread_start.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_TUNING -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_thread_start.c
ATL_goparallel_noaf.o : $(myTHRdir)/ATL_goparallel.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_goparallel.c
ATL_log2tlaunch_noaf.o : $(myTHRdir)/ATL_log2tlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_log2tlaunch.c
ATL_dyntlaunch_noaf.o : $(myTHRdir)/ATL_dyntlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_dyntlaunch.c
ATL_lin0tlaunch_noaf.o : $(myTHRdir)/ATL_lin0tlaunch.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_lin0tlaunch.c
ATL_thread_start_noaf.o : $(myTHRdir)/ATL_thread_start.c
	$(CC) $(CCFLAGS) -o $@ -c -DATL_NOAFFINITY=1 \
               $(myTHRdir)/ATL_thread_start.c

ATL_ResetAtomicCount_arch.o : ATL_ResetAtomicCount_arch.S
	$(CC) $(CCFLAGS) -o $@ -c ATL_ResetAtomicCount_arch.S
ATL_DecAtomicCount_arch.o : ATL_DecAtomicCount_arch.S
	$(CC) $(CCFLAGS) -o $@ -c ATL_DecAtomicCount_arch.S
ATL_FreeAtomicCount_arch.o : ATL_FreeAtomicCount_arch.c
	$(CC) $(CCFLAGS) -o $@ -c ATL_FreeAtomicCount_arch.c
ATL_GetAtomicCount_arch.o : ATL_GetAtomicCount_arch.c
	$(CC) $(CCFLAGS) -o $@ -c ATL_GetAtomicCount_arch.c
ATL_SetAtomicCount_arch.o : ATL_SetAtomicCount_arch.c
	$(CC) $(CCFLAGS) -o $@ -c ATL_SetAtomicCount_arch.c

tune_aff.o : $(mySRCdir)/tune_aff.c
	$(CC) $(CCFLAGS) -o tune_aff.o -c $(mySRCdir)/tune_aff.c
probe_nthr.o : $(mySRCdir)/probe_nthr.c
	$(CC) $(CCFLAGS) -o probe_nthr.o -c $(mySRCdir)/probe_nthr.c
test_count.o : $(mySRCdir)/test_count.c
	$(CC) $(CCFLAGS) -o test_count.o -c $(mySRCdir)/test_count.c
tune_count.o : $(mySRCdir)/tune_count.c
	$(CC) $(CCFLAGS) -o tune_count.o -c $(mySRCdir)/tune_count.c
tune_spawn.o : $(mySRCdir)/tune_spawn.c
	$(CC) $(CCFLAGS) -o tune_spawn.o -c $(mySRCdir)/tune_spawn.c
tune_spawn_fp.o : $(mySRCdir)/tune_spawn_fp.c
	$(CC) $(CCFLAGS) -o tune_spawn_fp.o -c $(mySRCdir)/tune_spawn_fp.c
test_count_glb.o : $(mySRCdir)/test_count.c
	$(CC) $(CCFLAGS) -DATL_GLOBAL -o test_count_glb.o \
              -c $(mySRCdir)/test_count.c
atlconf_misc.o : $(CONFdir)/src/atlconf_misc.c
	$(XCC) $(XCCFLAGS) -I$(CONFdir)/include -c -o $@ \
               $(CONFdir)/src/atlconf_misc.c
probe_aff.o : $(CONFdir)/src/probe_aff.c
	$(XCC) $(XCCFLAGS) -I$(CONFdir)/include -c -o $@ \
               $(CONFdir)/src/probe_aff.c
IRun_WIN64 :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_WIN64 \
               $(CONFdir)/src/backend/probe_aff_WIN64.c $(LIBS)
	./xprobe_WIN64 $(args) > res/probeB.out
	- cat res/probeB.out
IRun_WIN :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_WIN \
               $(CONFdir)/src/backend/probe_aff_WIN.c $(LIBS)
	./xprobe_WIN $(args) > res/probeB.out
	- cat res/probeB.out
IRun_CPUSET :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_CPUSET \
               $(CONFdir)/src/backend/probe_aff_CPUSET.c $(LIBS)
	./xprobe_CPUSET $(args) > res/probeB.out
	- cat res/probeB.out
IRun_SETPROCNP :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_SETPROCNP \
               $(CONFdir)/src/backend/probe_aff_SETPROCNP.c $(LIBS)
	./xprobe_SETPROCNP $(args) > res/probeB.out
	- cat res/probeB.out
IRun_SETAFFNP :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_SETAFFNP \
               $(CONFdir)/src/backend/probe_aff_SETAFFNP.c $(LIBS)
	./xprobe_SETAFFNP $(args) > res/probeB.out
	- cat res/probeB.out
IRun_SCHED :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_SCHED \
               $(CONFdir)/src/backend/probe_aff_SCHED.c $(LIBS)
	./xprobe_SCHED $(args) > res/probeB.out
	- cat res/probeB.out
IRun_RUNON :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_RUNON \
               $(CONFdir)/src/backend/probe_aff_RUNON.c $(LIBS)
	./xprobe_RUNON $(args) > res/probeB.out
	- cat res/probeB.out
IRun_PLPA :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_PLPA \
               $(CONFdir)/src/backend/probe_aff_PLPA.c $(LIBS)
	./xprobe_PLPA $(args) > res/probeB.out
	- cat res/probeB.out
IRun_PBIND :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_PBIND \
               $(CONFdir)/src/backend/probe_aff_PBIND.c $(LIBS)
	./xprobe_PBIND $(args) > res/probeB.out
	- cat res/probeB.out
IRun_BINDP :
	- rm res/probeB.out
	$(CC) $(CCFLAGS) -o xprobe_BINDP \
               $(CONFdir)/src/backend/probe_aff_BINDP.c $(LIBS)
	./xprobe_BINDP $(args) > res/probeB.out
	- cat res/probeB.out
