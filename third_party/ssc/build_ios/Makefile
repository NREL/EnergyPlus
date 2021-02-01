# mixing architectures so remove object files before archiving
all: makes libraries zip

makes:
	make -f Makefile-shared -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-shared -j8 ARCH=armv7s
	rm *.o
	make -f Makefile-shared -j8 ARCH=arm64
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=armv7s
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=arm64
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=armv7s
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=arm64
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=armv7s
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=arm64
	rm *.o
	make -f Makefile-tcs -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-tcs -j8 ARCH=armv7s
	rm *.o
	make -f Makefile-tcs -j8 ARCH=arm64
	rm *.o
	make -f Makefile-ssc -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-ssc -j8 ARCH=armv7s
	rm *.o
	make -f Makefile-ssc -j8 ARCH=arm64
	rm *.o

libraries:
	lipo -create shared_armv7s.a shared_x86_64.a shared_arm64.a -output shared.a	
	lipo -create nlopt_armv7s.a nlopt_x86_64.a nlopt_arm64.a -output nlopt.a
	lipo -create lpsolve_armv7s.a lpsolve_x86_64.a lpsolve_arm64.a -output lpsolve.a
	lipo -create solarpilot_armv7s.a solarpilot_x86_64.a solarpilot_arm64.a -output solarpilot.a
	lipo -create tcs_armv7s.a tcs_x86_64.a tcs_arm64.a -output tcs.a
	lipo -create ssc_armv7s.a  ssc_x86_64.a ssc_arm64.a -output ssc.a
	libtool -o libssc.so shared.a nlopt.a lpsolve.a solarpilot.a tcs.a ssc.a
	rm *.a
	mv libssc.so libssc.a
	mkdir lib
	mv libssc.a lib
	
zip:
	zip -r libssc.zip lib
		
clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	rm -rf lib
	rm libssc.zip
