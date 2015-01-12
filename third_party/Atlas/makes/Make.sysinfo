include Make.inc
mySRCdir = $(SRCdir)/tune/sysinfo

maxlat=6
mflop=200
flags=

sTestFlags : force_build
	$(MAKE) srbob `cat res/sBEST` pre='s' type=float

dTestFlags : force_build
	$(MAKE) drbob `cat res/dBEST` pre='d' type=double

res/L1CacheSize:
	$(MAKE) RunL1 MaxL1=$(MaxL1)
RunL1 : xL1
	$(ATLRUN) $(SYSdir) xL1 $(MaxL1)

$(INCAdir)/atlas_type.h:
	$(MAKE) RunTyp
	$(ATLFWAIT) -f $(INCAdir)/atlas_type.h
sRunSysSum : $(INCAdir)/atlas_ssysinfo.h

$(INCAdir)/atlas_ssysinfo.h : xsyssum $(ATLFWAIT)
	$(SYSdir)/xsyssum s $(INCAdir)/atlas_ssysinfo.h
	$(ATLFWAIT) -f $(INCAdir)/atlas_ssysinfo.h

res/sMULADD : 
	$(MAKE) RunMulAdd pre=s

dRunSysSum : $(INCAdir)/atlas_dsysinfo.h

$(INCAdir)/atlas_dsysinfo.h : xsyssum $(ATLFWAIT)
	$(SYSdir)/xsyssum d $(INCAdir)/atlas_dsysinfo.h
	$(ATLFWAIT) -f $(INCAdir)/atlas_dsysinfo.h

res/dMULADD : 
	$(MAKE) RunMulAdd pre=d

cRunSysSum : $(INCAdir)/atlas_csysinfo.h

$(INCAdir)/atlas_csysinfo.h : xsyssum $(ATLFWAIT)
	$(SYSdir)/xsyssum c $(INCAdir)/atlas_csysinfo.h
	$(ATLFWAIT) -f $(INCAdir)/atlas_csysinfo.h

res/cMULADD : 
	$(MAKE) RunMulAdd pre=c

zRunSysSum : $(INCAdir)/atlas_zsysinfo.h

$(INCAdir)/atlas_zsysinfo.h : xsyssum $(ATLFWAIT)
	$(SYSdir)/xsyssum z $(INCAdir)/atlas_zsysinfo.h
	$(ATLFWAIT) -f $(INCAdir)/atlas_zsysinfo.h

res/zMULADD : 
	$(MAKE) RunMulAdd pre=z

	
$(MMTdir)/res/sMULADD : res/sMULADD
	rm -f $(MMTdir)/res/sMULADD
	ln -s $(SYSdir)/res/sMULADD $(MMTdir)/res/sMULADD
res/snreg : $(MMTdir)/res/sMULADD
	cd $(MMTdir) ; $(MAKE) res/snreg
	cd res ; ln -s $(MMTdir)/res/snreg snreg
        
$(MMTdir)/res/dMULADD : res/dMULADD
	rm -f $(MMTdir)/res/dMULADD
	ln -s $(SYSdir)/res/dMULADD $(MMTdir)/res/dMULADD
res/dnreg : $(MMTdir)/res/dMULADD
	cd $(MMTdir) ; $(MAKE) res/dnreg
	cd res ; ln -s $(MMTdir)/res/dnreg dnreg
        

RunTLB : xtlb
	$(ATLRUN) $(SYSdir) xtlb

RunMADef : xmasrch
	./xmasrch -p $(pre) -o res/$(pre)MULADD

RunMulAdd: xmasrch
	./xmasrch -p $(pre) -o res/$(pre)MULADD

RunLamch : xemit_lamch
	$(ATLRUN) $(SYSdir) xemit_lamch $(INCAdir)
RunTyp: xemit_typ
	$(ATLRUN) $(SYSdir) xemit_typ > $(INCAdir)/atlas_type.h

xemit_buildinfo : emit_buildinfo.o
	$(XCC) $(XCCFLAGS) -o $@ emit_buildinfo.o
xsyssum : GetSysSum.o
	$(XCC) $(XCCFLAGS) -o $@ GetSysSum.o

xL1 : time.o L1CacheSize.o
	$(KC) $(KCFLAGS) -o $@ L1CacheSize.o time.o

xtlb : time.o tlb.o
	$(CLINKER) $(CLINKFLAGS) -o $@ tlb.o time.o

xL2 : time.o L2CacheSize.o
	$(CLINKER) $(CLINKFLAGS) -o $@ L2CacheSize.o time.o

xemit_lamch : emit_lamch.o
	$(KC) $(KCFLAGS) -o $@ emit_lamch.o $(LIBS)
xemit_typ : emit_typ.o
	$(KC) $(KCFLAGS) -o $@ emit_typ.o

xqma : force_build time.o qmuladd.c qmatime.o
	$(QKC) $(QKCFLAGS) $(SMAFLAGS) -c qmuladd.c
	$(QKC) $(QKCFLAGS) -o xqma matime.o qmuladd.o time.o
	$(ATLRUN) $(SYSdir) xqma -f $(outf) $(flags)
qmatime.o : $(mySRCdir)/matime.c
	$(KC) -c $(KCFLAGS) -DQREAL $(mySRCdir)/matime.c
xdma : force_build time.o dmuladd.c dmatime.o
	$(DKC) $(DKCFLAGS) $(SMAFLAGS) -c dmuladd.c
	$(DKC) $(DKCFLAGS) -o xdma matime.o dmuladd.o time.o
	$(ATLRUN) $(SYSdir) xdma -f $(outf) $(flags)
dmatime.o : $(mySRCdir)/matime.c
	$(KC) -c $(KCFLAGS) -DDREAL $(mySRCdir)/matime.c
xsma : force_build time.o smuladd.c smatime.o
	$(SKC) $(SKCFLAGS) $(SMAFLAGS) -c smuladd.c
	$(SKC) $(SKCFLAGS) -o xsma matime.o smuladd.o time.o
	$(ATLRUN) $(SYSdir) xsma -f $(outf) $(flags)
smatime.o : $(mySRCdir)/matime.c
	$(KC) -c $(KCFLAGS) -DSREAL $(mySRCdir)/matime.c
xmasrch : $(mySRCdir)/masrch.c
	$(XCC) $(XCCFLAGS) -o $@ $(mySRCdir)/masrch.c

ATL_cputime.c : 
	cp $(mySRCdir)/ATL_cputime.c .
ATL_cputime.o : ATL_cputime.c
	$(KC) -c $(KCFLAGS) ATL_cputime.c

ATL_walltime.c : 
	cp $(mySRCdir)/ATL_walltime.c .
ATL_walltime.o : ATL_walltime.c
	$(KC) -c $(KCFLAGS) ATL_walltime.c

ATL_Xwalltime.o : ATL_walltime.o
	$(XCC) $(XCCFLAGS) -c -o ATL_Xwalltime.o ATL_walltime.c

emit_buildinfo.o : $(mySRCdir)/emit_buildinfo.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/emit_buildinfo.c
GetSysSum.o : $(INCAdir)/atlas_type.h $(mySRCdir)/GetSysSum.c
	$(XCC) -c $(XCCFLAGS) $(mySRCdir)/GetSysSum.c
time.o : $(mySRCdir)/time.c
	$(KC) -c $(KCFLAGS) -I./ $(mySRCdir)/time.c
emit_lamch.o : $(mySRCdir)/emit_lamch.c
	$(KC) -c $(KCFLAGS) $(mySRCdir)/emit_lamch.c
emit_typ.o : $(mySRCdir)/emit_typ.c
	$(KC) -c $(KCFLAGS) $(mySRCdir)/emit_typ.c
L1CacheSize.o : $(mySRCdir)/L1CacheSize.c
	$(KC) -c $(KCFLAGS) $(mySRCdir)/L1CacheSize.c
findNT.o : $(mySRCdir)/findNT.c
	$(KC) -c $(KCFLAGS) $(mySRCdir)/findNT.c
tlb.o : $(mySRCdir)/tlb.c
	$(KC) -c $(KCFLAGS) $(mySRCdir)/tlb.c



force_build :

clean :
	rm -f xL1 xtlb xmasrch xemit_typ xsys_sum xfindNT *.o x?ma ?muladd.o

print_ARCH :
	@echo $(ARCH)
print_INSTFLAGS :
	@echo $(INSTFLAGS)
print_F2CDEFS :
	@echo $(F2CDEFS)
print_ARCHDEFS :
	@echo $(ARCHDEFS)
print_DKCFLAGS :
	@echo $(DKCFLAGS)
print_DKC :
	@echo $(DKC)
print_SKCFLAGS :
	@echo $(SKCFLAGS)
print_SKC :
	@echo $(SKC)
print_DMCFLAGS :
	@echo $(DMCFLAGS)
print_DMC :
	@echo $(DMC)
print_SMCFLAGS :
	@echo $(SMCFLAGS)
print_SMC :
	@echo $(SMC)
print_ICCFLAGS :
	@echo $(ICCFLAGS)
print_ICC :
	@echo $(ICC)
print_F77FLAGS :
	@echo $(F77FLAGS)
print_F77 :
	@echo $(F77)
