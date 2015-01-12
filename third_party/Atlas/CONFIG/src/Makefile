SHELL=/bin/sh
SRCdir = $(TOPdir)/CONFIG/src
INCdir = $(TOPdir)/CONFIG/include
mydir = $(BLDdir)
atlrun = atlas_run
INCFLAGS = -I$(INCdir)
MYFLAGS =
CCFLAGS = $(INCFLAGS) $(MYFLAGS) $(CFLAGS)
XCC = $(CC)
XCFLAGS = $(CCFLAGS)
LIBS =
args =
rdir = /tmp
arch=
BUNZIP = bunzip2
TAR = tar
wind=$(BLDdir)

all : build
xconfig : $(SRCdir)/config.c atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xconfig $(SRCdir)/config.c atlconf_misc.o $(LIBS)
xspew : $(SRCdir)/SpewMakeInc.c atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xspew $(SRCdir)/SpewMakeInc.c atlconf_misc.o \
               $(LIBS)
xisgcc : $(SRCdir)/IsGcc.c atlconf_misc.o
	$(XCC) $(XCFLAGS) -o $@ $(SRCdir)/IsGcc.c atlconf_misc.o $(LIBS)
mgwgfortran : $(SRCdir)/mgwcmp.c
	$(XCC) $(XCCFLAGS) "-DDEFDF=$(cmppath)" -o $@ $(SRCdir)/mgwcmp.c
mgwgcc : $(SRCdir)/mgwcmp.c
	$(XCC) $(XCCFLAGS) "-DDEFDF=$(cmppath)" -o $@ $(SRCdir)/mgwcmp.c
mgwar : $(SRCdir)/mgwcmp.c
	$(XCC) $(XCCFLAGS) "-DDEFDF=$(cmppath)" -o $@ $(SRCdir)/mgwcmp.c
mgwranlib : $(SRCdir)/mgwcmp.c
	$(XCC) $(XCCFLAGS) "-DDEFDF=$(cmppath)" -o $@ $(SRCdir)/mgwcmp.c

$(wind)/ATLwin_ifort.exe : $(SRCdir)/winf77.c
	cp $(SRCdir)/wincc.c $(wind)/.
	cd $(wind) ; icl /FeATLwin_ifort.exe /DDEFDF=ifort wincc.c
	rm -f $(wind)/wincc.c
$(wind)/ATLwin_icc.exe : $(SRCdir)/wincc.c
	cp $(SRCdir)/wincc.c $(wind)/.
	cd $(wind) ; icl /FeATLwin_icc.exe /DDEFDF=icl wincc.c
	rm -f $(wind)/wincc.c
$(wind)/ATLwin_cl.exe : $(SRCdir)/wincc.c
	cp $(SRCdir)/wincc.c $(wind)/.
	cd $(wind) ; cl /FeATLwin_cl.exe /DDEFDF=cl wincc.c
	rm -f $(wind)/wincc.c
xprobe_comp : xisgcc probe_comp.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_comp probe_comp.o atlconf_misc.o $(LIBS)
xprobe_aff : probe_aff.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_aff probe_aff.o atlconf_misc.o $(LIBS)
xprobe_pmake : probe_pmake.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_pmake probe_pmake.o atlconf_misc.o $(LIBS)
xprobe_f2c : probe_f2c.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_f2c probe_f2c.o atlconf_misc.o $(LIBS)
xprobe_arch : probe_arch.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_arch probe_arch.o atlconf_misc.o $(LIBS)
xprobe_vec : probe_vec.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_vec probe_vec.o atlconf_misc.o $(LIBS)
xprobe_asm : probe_asm.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_asm probe_asm.o atlconf_misc.o $(LIBS)
xprobe_OS : probe_OS.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xprobe_OS probe_OS.o atlconf_misc.o $(LIBS)

xprint_enums : $(SRCdir)/print_enums.c
	$(XCC) $(XCFLAGS) -o xprint_enums $(SRCdir)/print_enums.c
xatlbench : atlbench.o atlconf_misc.o
	$(XCC) $(XCFLAGS) -o xatlbench atlbench.o atlconf_misc.o
probe_aff.o : $(SRCdir)/probe_aff.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_aff.c
atlbench.o : $(SRCdir)/atlbench.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/atlbench.c
probe_pmake.o : $(SRCdir)/probe_pmake.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_pmake.c
probe_f2c.o : $(SRCdir)/probe_f2c.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_f2c.c
probe_comp.o : $(SRCdir)/probe_comp.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_comp.c
probe_arch.o : $(SRCdir)/probe_arch.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_arch.c
probe_vec.o : $(SRCdir)/probe_vec.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_vec.c
probe_asm.o : $(SRCdir)/probe_asm.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_asm.c
probe_OS.o : $(SRCdir)/probe_OS.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/probe_OS.c
atlconf_misc.o : $(SRCdir)/atlconf_misc.c
	$(XCC) $(XCFLAGS) -c $(SRCdir)/atlconf_misc.c
atlconf_misc_BE.o : $(SRCdir)/atlconf_misc.c
	$(CC) $(CCFLAGS) -o atlconf_misc_BE.o -c $(SRCdir)/atlconf_misc.c

atlas_run :
	cd $(atldir) ; ./$(exe) $(args) > $(redir)
atlas_runX :
	scp $(atldir)/$(exe) $(targ):$(rdir)/$(exe)
	ssh $(targ) 'cd $(rdir) ; ./$(exe) $(args)' > $(redir)
force_build:

#
# General probes always ran locally (possibly cross-compiling machine)
# General probes call system (they're essentially shell scripts)
#
IRun_comp: xprobe_comp force_build
	- rm -f config1.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_comp redir=config1.out \
                args="$(args) -d b $(BLDdir)"
	- cat config1.out
IRun_OS: xprobe_OS force_build
	- rm -f config1.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_OS args="$(args)" \
                redir=config1.out
	- cat config1.out
IRun_aff: xprobe_aff force_build
	- rm -f config2.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_aff args="$(args)" \
                redir=config2.out
	- cat config2.out
IRun_pmake: xprobe_pmake force_build
	- rm -f config2.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_pmake args="$(args)" \
                redir=config2.out
	- cat config2.out
IRun_arch: xprobe_arch force_build
	- rm -f config2.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_arch args="$(args)" \
                redir=config2.out
	- cat config2.out
IRun_vec: xprobe_vec force_build
	- rm -f config2.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_vec args="$(args)" \
                redir=config2.out
	- cat config2.out
IRun_asm: xprobe_asm force_build
	- rm -f config2.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_asm args="$(args)" \
                redir=config2.out
	- cat config2.out
IRun_f2c: xprobe_f2c force_build
	- rm -f config2.out
	$(MAKE) atlas_run atldir=$(mydir) exe=xprobe_f2c args="$(args)" \
                redir=config2.out
	- cat config2.out

#
# Machine-level probes run on machine lib being compiled for
# These probes do not call system, but actually do something
#
IRun_NEON :
	$(CC) $(CCFLAGS) -mfpu=neon -o xprobe_neon \
              $(SRCdir)/backend/probe_svec.c $(SRCdir)/backend/probe_NEON.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_neon args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_3DNow :
	$(CC) $(CCFLAGS) -o xprobe_3dnow $(SRCdir)/backend/probe_svec.c $(SRCdir)/backend/probe_3DNow.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_3dnow args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_VSX :
	$(CC) $(CCFLAGS) -o xprobe_vsx $(SRCdir)/backend/probe_dvec.c $(SRCdir)/backend/probe_VSX.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_vsx args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_AltiVec :
	$(CC) $(CCFLAGS) -o xprobe_altivec $(SRCdir)/backend/probe_svec.c $(SRCdir)/backend/probe_AltiVec.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_altivec args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_SSE2 :
	$(CC) $(CCFLAGS) -o xprobe_sse2 $(SRCdir)/backend/probe_dvec.c $(SRCdir)/backend/probe_SSE2.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_sse2 args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_SSE1 :
	$(CC) $(CCFLAGS) -o xprobe_sse1 $(SRCdir)/backend/probe_svec.c $(SRCdir)/backend/probe_SSE1.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_sse1 args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_SSE3 :
	$(CC) $(CCFLAGS) -o xprobe_sse3 $(SRCdir)/backend/probe_dSSE3.c $(SRCdir)/backend/probe_SSE3.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_sse3 args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_AVXFMA4 :
	$(CC) $(CCFLAGS) -o xprobe_avxmac $(SRCdir)/backend/probe_dAVX.c \
              $(SRCdir)/backend/probe_AVXFMA4.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_avxmac args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_AVXMAC :
	$(CC) $(CCFLAGS) -o xprobe_avxmac $(SRCdir)/backend/probe_dAVX.c \
              $(SRCdir)/backend/probe_AVXMAC.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_avxmac args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_AVX :
	$(CC) $(CCFLAGS) -o xprobe_avx $(SRCdir)/backend/probe_dAVX.c \
              $(SRCdir)/backend/probe_AVX.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_avx args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_WIN64 :
	$(CC) $(CCFLAGS) -o xprobe_WIN64 $(SRCdir)/backend/probe_aff_WIN64.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_WIN64 args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_WIN :
	$(CC) $(CCFLAGS) -o xprobe_WIN $(SRCdir)/backend/probe_aff_WIN.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_WIN args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_CPUSET :
	$(CC) $(CCFLAGS) -o xprobe_CPUSET $(SRCdir)/backend/probe_aff_CPUSET.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_CPUSET args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_SETPROCNP :
	$(CC) $(CCFLAGS) -o xprobe_SETPROCNP $(SRCdir)/backend/probe_aff_SETPROCNP.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_SETPROCNP args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_SETAFFNP :
	$(CC) $(CCFLAGS) -o xprobe_SETAFFNP $(SRCdir)/backend/probe_aff_SETAFFNP.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_SETAFFNP args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_SCHED :
	$(CC) $(CCFLAGS) -o xprobe_SCHED $(SRCdir)/backend/probe_aff_SCHED.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_SCHED args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_RUNON :
	$(CC) $(CCFLAGS) -o xprobe_RUNON $(SRCdir)/backend/probe_aff_RUNON.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_RUNON args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_PLPA :
	$(CC) $(CCFLAGS) -o xprobe_PLPA $(SRCdir)/backend/probe_aff_PLPA.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_PLPA args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_PBIND :
	$(CC) $(CCFLAGS) -o xprobe_PBIND $(SRCdir)/backend/probe_aff_PBIND.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_PBIND args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_BINDP :
	$(CC) $(CCFLAGS) -o xprobe_BINDP $(SRCdir)/backend/probe_aff_BINDP.c \
              $(LIBS)
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_BINDP args="$(args)" \
                redir=config0.out
	- cat config0.out

IRun_GAS_S390 :
	$(CC) $(CCFLAGS) -o xprobe_gas_s390 $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_s390.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_s390 args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_GAS_ARM :
	$(CC) $(CCFLAGS) -o xprobe_gas_arm $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_arm.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_arm args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_GAS_SPARC :
	$(CC) $(CCFLAGS) -o xprobe_gas_sparc $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_sparc.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_sparc args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_GAS_PARISC :
	$(CC) $(CCFLAGS) -o xprobe_gas_parisc $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_parisc.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_parisc args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_GAS_PPC :
	$(CC) $(CCFLAGS) -o xprobe_gas_ppc $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_ppc.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_ppc args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_GAS_x8664 :
	$(CC) $(CCFLAGS) -o xprobe_gas_x8664 $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_x8664.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_x8664 args="$(args)" \
                redir=config0.out
	- cat config0.out
IRun_GAS_x8632 :
	$(CC) $(CCFLAGS) -o xprobe_gas_x8632 $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_x8632.S
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xprobe_gas_x8632 args="$(args)" \
                redir=config0.out
	- cat config0.out

IRunC2C :
	- rm -f config0.out xc2c c2cslave.o
	$(CC1) $(CC1FLAGS) -c $(SRCdir)/backend/c2cslave.c
	$(CC) $(CCFLAGS) -o xc2c $(SRCdir)/backend/c2cmaster.c c2cslave.o
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xc2c args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunFlib :
	@ - rm config0.out
	@ $(F77) $(F77FLAGS) -v -o xtst $(SRCdir)/backend/comptestF.f 2>&1 | \
           fgrep -e '-L' > config0.out
	@ rm -f xtst
	@ - cat config0.out
IRunTestCFLink :
	@ - rm -f flibchk[F,C].o xflibchk
	$(F77) $(F77FLAGS) -c $(SRCdir)/backend/flibchkF.f
	$(CC) $(CCFLAGS) -o xflibchk $(SRCdir)/backend/flibchkC.c \
              flibchkF.o -l $(F77LIB) $(LIBS)
IRunF2C_str:
	- rm -f f2cstrF.o f2cstrC.o xfcstr config0.out
	$(CC) $(CCFLAGS) -c $(SRCdir)/backend/f2cstrC.c
	$(F77) $(F77FLAGS) -o xf2cstr $(SRCdir)/backend/f2cstrF.f \
               f2cstrC.o
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xf2cstr args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunF2C_int:
	- rm -f f2cintF.o f2cintC.o xfcint config0.out
	$(CC) $(CCFLAGS) -c $(SRCdir)/backend/f2cintC.c
	$(F77) $(F77FLAGS) -o xf2cint $(SRCdir)/backend/f2cintF.f \
               f2cintC.o
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xf2cint args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunF2C_name:
	- rm -f f2cnameF.o f2cnameC.o xfcname config0.out
	$(CC) $(CCFLAGS) -c $(SRCdir)/backend/f2cnameC.c
	$(F77) $(F77FLAGS) -o xf2cname $(SRCdir)/backend/f2cnameF.f \
               f2cnameC.o
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xf2cname args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunF77Comp :
	- rm -f xf77test comptestF.o config0.out
	$(F77) $(F77FLAGS) -o xf77test $(SRCdir)/backend/comptestF.f
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xf77test args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunCComp :
	- rm -f xctest comptestC.o config0.out
	$(CC) $(CCFLAGS) -o xctest $(SRCdir)/backend/comptestC.c
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xctest args="$(args)" \
                redir=config0.out
	- cat config0.out

IRunArchInfo_win: xarchinfo_win
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_win args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_sfu: xarchinfo_sfu
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_sfu args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_aix: xarchinfo_aix
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_aix args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_irix: xarchinfo_irix
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_irix args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_sunos: xarchinfo_sunos
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_sunos args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_freebsd: xarchinfo_freebsd
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_freebsd args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_linux: xarchinfo_linux
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_linux args="$(args)" \
                redir=config0.out
	- cat config0.out
IRunArchInfo_x86: xarchinfo_x86
	- rm -f config0.out
	$(MAKE) $(atlrun) atldir=$(mydir) exe=xarchinfo_x86 args="$(args)" \
                redir=config0.out
	- cat config0.out
xarchinfo_x86: $(SRCdir)/backend/archinfo_x86.c $(SRCdir)/backend/cpuid.S
	$(CC) $(CCFLAGS) -o xarchinfo_x86 $(SRCdir)/backend/archinfo_x86.c \
              $(SRCdir)/backend/cpuid.S
xarchinfo_win : $(SRCdir)/backend/archinfo_win.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_win $(SRCdir)/backend/archinfo_win.c \
              atlconf_misc_BE.o
xarchinfo_sfu : $(SRCdir)/backend/archinfo_sfu.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_sfu $(SRCdir)/backend/archinfo_sfu.c \
              atlconf_misc_BE.o
xarchinfo_aix : $(SRCdir)/backend/archinfo_aix.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_aix $(SRCdir)/backend/archinfo_aix.c \
              atlconf_misc_BE.o
xarchinfo_irix : $(SRCdir)/backend/archinfo_irix.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_irix $(SRCdir)/backend/archinfo_irix.c \
              atlconf_misc_BE.o
xarchinfo_sunos : $(SRCdir)/backend/archinfo_sunos.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_sunos $(SRCdir)/backend/archinfo_sunos.c \
              atlconf_misc_BE.o
xarchinfo_freebsd : $(SRCdir)/backend/archinfo_freebsd.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_freebsd $(SRCdir)/backend/archinfo_freebsd.c \
              atlconf_misc_BE.o
xarchinfo_linux : $(SRCdir)/backend/archinfo_linux.c atlconf_misc_BE.o
	$(CC) $(CCFLAGS) -o xarchinfo_linux $(SRCdir)/backend/archinfo_linux.c \
              atlconf_misc_BE.o
xprobe_parisc : $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_parisc.S
	$(CC) $(CCFLAGS) -o xprobe_parisc $(SRCdir)/backend/probe_this_asm.c \
              $(SRCdir)/backend/probe_gas_parisc.S
xprobe_sparc : $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_sparc.S
	$(CC) $(CCFLAGS) -o xprobe_sparc $(SRCdir)/backend/probe_this_asm.c \
              $(SRCdir)/backend/probe_gas_sparc.S
xprobe_ppc : $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_ppc.S
	$(CC) $(CCFLAGS) -o xprobe_ppc $(SRCdir)/backend/probe_this_asm.c \
              $(SRCdir)/backend/probe_gas_ppc.S
xprobe_x8632 : $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_x8632.S
	$(CC) $(CCFLAGS) -o xprobe_x8632 $(SRCdir)/backend/probe_this_asm.c \
              $(SRCdir)/backend/probe_gas_x8632.S
xprobe_x8684 : $(SRCdir)/backend/probe_this_asm.c $(SRCdir)/backend/probe_gas_x8684.S
	$(CC) $(CCFLAGS) -o xprobe_x8684 $(SRCdir)/backend/probe_this_asm.c \
              $(SRCdir)/backend/probe_gas_x8684.S

DoNothing:

make_subdirs:
	mkdir src bin tune interfaces
	cd src ; mkdir testing auxil blas lapack pthreads threads
	cd src/blas ; \
           mkdir f77reference reference gemv ger gemm kbmm \
                 level1 level2 level3 pklevel3
	cd src/blas/reference ; mkdir level1 level2 level3
	cd src/blas/level2 ; mkdir kernel
	cd src/blas/pklevel3 ; mkdir gpmm sprk
	cd src/blas/level3 ; mkdir rblas kernel
	cd src/pthreads ; mkdir blas misc
	cd src/pthreads/blas ; mkdir level1 level2 level3
	cd src/threads ; mkdir blas lapack
	cd src/threads/blas ; mkdir level3 level2
	cd tune ; mkdir blas sysinfo lapack threads
	cd tune/blas ; mkdir gemm gemv ger level1 level3
	cd interfaces ; mkdir blas lapack
	cd interfaces/lapack ; mkdir C F77
	cd interfaces/lapack/C ; mkdir src testing
	cd interfaces/lapack/F77 ; mkdir src testing
	cd interfaces/blas ; mkdir C F77
	cd interfaces/blas/C ; mkdir src testing
	cd interfaces/blas/F77 ; mkdir src testing
	cd interfaces/lapack ; mkdir C2F
	cd interfaces/lapack/C2F ; mkdir src
	mkdir ARCHS

#
# Simple test returns error if directory doesn't exist
#
DirIsThere :
	cd $(tdir)
$(arch)_defs : $(SRCdir)/CONFIG/$(arch).tar.gz
	$(BUNZIP) -c $(SRCdir)/CONFIG/$(ARCH).tar.bz2 | $(TAR) xvf -
archdef :
	$(MAKE) -f Make.top archdef
time : xatlbench
	$(MAKE) -f Make.top time
build :
	$(MAKE) -f Make.top build
	$(MAKE) clean
error_report:
	$(MAKE) -f Make.top error_report
startup : 
	$(MAKE) -f Make.top startup
refresh :
	$(MAKE) -f Make.top refresh
kill  :
	$(MAKE) -f Make.top kill
killall:
	$(MAKE) -f Make.top killall
C_test :
	$(MAKE) -f Make.top C_sanity_test
C_pttest :
	$(MAKE) -f Make.top C_ptsanity_test
check : $(TESTS)
ptcheck : $(PTTESTS)
full_test :
	$(MAKE) -f Make.top full_test
full_test_nh :
	$(MAKE) -f Make.top full_test_nh
scope_full_test :
	$(MAKE) -f Make.top scope_full_results
scope_lapack_test_pt_pt :
	$(MAKE) -f Make.top scope_lapack_test_pt_pt
lapack_test_pt_pt :
	$(MAKE) -f Make.top lapack_test_pt_pt
scope_lapack_test_pt_ab :
	$(MAKE) -f Make.top scope_lapack_test_pt_ab
lapack_test_pt_ab :
	$(MAKE) -f Make.top lapack_test_pt_ab
scope_lapack_test_pt_sb :
	$(MAKE) -f Make.top scope_lapack_test_pt_sb
lapack_test_pt_sb :
	$(MAKE) -f Make.top lapack_test_pt_sb
scope_lapack_test_pt_fb :
	$(MAKE) -f Make.top scope_lapack_test_pt_fb
lapack_test_pt_fb :
	$(MAKE) -f Make.top lapack_test_pt_fb
scope_lapack_test_al_pt :
	$(MAKE) -f Make.top scope_lapack_test_al_pt
lapack_test_al_pt :
	$(MAKE) -f Make.top lapack_test_al_pt
scope_lapack_test_al_ab :
	$(MAKE) -f Make.top scope_lapack_test_al_ab
lapack_test_al_ab :
	$(MAKE) -f Make.top lapack_test_al_ab
scope_lapack_test_al_sb :
	$(MAKE) -f Make.top scope_lapack_test_al_sb
lapack_test_al_sb :
	$(MAKE) -f Make.top lapack_test_al_sb
scope_lapack_test_al_fb :
	$(MAKE) -f Make.top scope_lapack_test_al_fb
lapack_test_al_fb :
	$(MAKE) -f Make.top lapack_test_al_fb
scope_lapack_test_sl_pt :
	$(MAKE) -f Make.top scope_lapack_test_sl_pt
lapack_test_sl_pt :
	$(MAKE) -f Make.top lapack_test_sl_pt
scope_lapack_test_sl_ab :
	$(MAKE) -f Make.top scope_lapack_test_sl_ab
lapack_test_sl_ab :
	$(MAKE) -f Make.top lapack_test_sl_ab
scope_lapack_test_sl_sb :
	$(MAKE) -f Make.top scope_lapack_test_sl_sb
lapack_test_sl_sb :
	$(MAKE) -f Make.top lapack_test_sl_sb
scope_lapack_test_sl_fb :
	$(MAKE) -f Make.top scope_lapack_test_sl_fb
lapack_test_sl_fb :
	$(MAKE) -f Make.top lapack_test_sl_fb
scope_lapack_test_fl_pt :
	$(MAKE) -f Make.top scope_lapack_test_fl_pt
lapack_test_fl_pt :
	$(MAKE) -f Make.top lapack_test_fl_pt
scope_lapack_test_fl_ab :
	$(MAKE) -f Make.top scope_lapack_test_fl_ab
lapack_test_fl_ab :
	$(MAKE) -f Make.top lapack_test_fl_ab
scope_lapack_test_fl_sb :
	$(MAKE) -f Make.top scope_lapack_test_fl_sb
lapack_test_fl_sb :
	$(MAKE) -f Make.top lapack_test_fl_sb
scope_lapack_test_fl_fb :
	$(MAKE) -f Make.top scope_lapack_test_fl_fb
lapack_test_fl_fb :
	$(MAKE) -f Make.top lapack_test_fl_fb
test :
	$(MAKE) -f Make.top sanity_test
pttest :
	$(MAKE) -f Make.top ptsanity_test
$(INCINSTdir) :
	mkdir -p $(INCINSTdir)
	chmod 0755 $(INCINSTdir)
$(INCINSTdir)/atlas :
	mkdir -p $(INCINSTdir)/atlas
	chmod 0755 $(INCINSTdir)/atlas
$(LIBINSTdir) :
	mkdir -p $(LIBINSTdir)
	chmod 0755 $(LIBINSTdir)
install : $(INCINSTdir) $(LIBINSTdir) $(INCINSTdir)/atlas
	$(MAKE) -f Make.top install_inc INSTdir=$(INCINSTdir) 
	$(MAKE) -f Make.top install_lib INSTdir=$(LIBINSTdir)

confclean: $(CLEANdep)
	rm -f *core* *.o config?.out
clean : $(CLEANdep)
	rm -f *.o x* config?.out *core*
cleanall: clean
	$(MAKE) -f Make.top clean
