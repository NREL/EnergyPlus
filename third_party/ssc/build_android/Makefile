# explicit for all architectures
# mixing architectures so remove object files before archiving
all: makes libraries zip

makes:
	make -f Makefile-splinter -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-shared -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-tcs -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-ssc -j8 ARCH=arm64-v8a
	rm *.o
	make -f Makefile-splinter -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-shared -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-tcs -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-ssc -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-splinter -j8 ARCH=x86
	rm *.o
	make -f Makefile-shared -j8 ARCH=x86
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=x86
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=x86
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=x86
	rm *.o
	make -f Makefile-tcs -j8 ARCH=x86
	rm *.o
	make -f Makefile-ssc -j8 ARCH=x86
	rm *.o
	make -f Makefile-splinter -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-shared -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-tcs -j8 ARCH=x86_64
	rm *.o
	make -f Makefile-ssc -j8 ARCH=x86_64
	rm *.o


libraries:
	mkdir lib
	mkdir lib/arm64-v8a
	mkdir lib/armeabi-v7a
	mkdir lib/x86
	mkdir lib/x86_64
	
	cp splinter_arm64-v8a.a lib/arm64-v8a/splinter.a
	cp shared_arm64-v8a.a lib/arm64-v8a/shared.a
	cp nlopt_arm64-v8a.a lib/arm64-v8a/nlopt.a
	cp lpsolve_arm64-v8a.a lib/arm64-v8a/lpsolve.a
	cp solarpilot_arm64-v8a.a lib/arm64-v8a/solarpilot.a
	cp tcs_arm64-v8a.a lib/arm64-v8a/tcs.a
	cp ssc_arm64-v8a.a lib/arm64-v8a/ssc.a
	
	cp splinter_armeabi-v7a.a lib/armeabi-v7a/splinter.a
	cp shared_armeabi-v7a.a lib/armeabi-v7a/shared.a
	cp nlopt_armeabi-v7a.a lib/armeabi-v7a/nlopt.a
	cp lpsolve_armeabi-v7a.a lib/armeabi-v7a/lpsolve.a
	cp solarpilot_armeabi-v7a.a lib/armeabi-v7a/solarpilot.a
	cp tcs_armeabi-v7a.a lib/armeabi-v7a/tcs.a
	cp ssc_armeabi-v7a.a lib/armeabi-v7a/ssc.a

	cp splinter_x86.a lib/x86/splinter.a
	cp shared_x86.a lib/x86/shared.a
	cp nlopt_x86.a lib/x86/nlopt.a
	cp lpsolve_x86.a lib/x86/lpsolve.a
	cp solarpilot_x86.a lib/x86/solarpilot.a
	cp tcs_x86.a lib/x86/tcs.a
	cp ssc_x86.a lib/x86/ssc.a

	cp splinter_x86_64.a lib/x86_64/splinter.a
	cp shared_x86_64.a lib/x86_64/shared.a
	cp nlopt_x86_64.a lib/x86_64/nlopt.a
	cp lpsolve_x86_64.a lib/x86_64/lpsolve.a
	cp solarpilot_x86_64.a lib/x86_64/solarpilot.a
	cp tcs_x86_64.a lib/x86_64/tcs.a
	cp ssc_x86_64.a lib/x86_64/ssc.a

	rm *.a

zip:
	zip -r libssc.zip lib

clean:
	make -f Makefile-splinter clean
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean
	rm -rf lib
	rm libssc.zip

