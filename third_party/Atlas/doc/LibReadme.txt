***************************** FILE DESCRIPTIONS *******************************
The files in this archive are:

Make.inc      : The Make include file used to build these libs
SUMMARY.LOG   : The SUMMARY.LOG created by atlas_install.  Provides further
                information about the installation machine.
cblas.h       : The C header file for the C interface to the BLAS.
clapack.h     : The C header file for the C interface to LAPACK.
liblapack.a   : The serial LAPACK routines provided by ATLAS.
libcblas.a    : The ANSI C interface to the BLAS.
libf77blas.a  : The Fortran77 interface to the BLAS.
libatlas.a    : The main ATLAS library, providing low-level routines for all
                interface libs.

Your archive may also contain additional libraries, if it has parallel thread
support.  These optional libs are:

libptcblas.a    : The ANSI C interface to the threaded (SMP) BLAS.
libptf77blas.a  : The Fortran77 interface to the threaded (SMP) BLAS.
libptlapack.a   : The parallel LAPACK routines provided by ATLAS.

********************************* LINKING *************************************
When linking, remember that order is important.  So, if you want uniprocessor
libs, your link line would contain IN THIS ORDER:
   -LLIBDIR -llapack -lcblas -lf77blas -latlas
And if you want to utilize an SMP version, it would be:
   -LLIBDIR -lptlapack -lptcblas -lptf77blas -latlas

NOTE: On Apple's OS X, the above won't work, since system directories are
searched before the -L directories.  OS X includes ATLAS internally, so
the above link line will always get you OS X's libs instead of the ones
that you have built.  The easiest solution is to explicitly link to each
library using the full path, rather than using the -L/-l combo.

************************** GETTING A FULL LAPACK LIB **************************
To get a full FORTRAN implementation of lapack, download netlib lapack
and use the --with-netlib-lapack-tarfile option discussed in the installation
guide.
