# Objexx Fortran-C++ Library (ObjexxFCL)

The ObjexxFCL is an open source C++ library providing Fortran-compatible array, string, and intrinsic function support.

The ObjexxFCL accompanies Objexx Fortran-to-C++ conversions and is used to support integration between Fortran and C++.

## Variant

This is the ObjexxFCL **C** variant, which uses a row-major array memory layout.
This variant omits the optional automatic dimensioning system of the A variant for simplicity and performance.
This variant omits the Fstring Fortran-style string type and is meant for use with code that has been migrated to std::string.

## Version

This is version **4.2**.

## Documentation

See the [ObjexxFCL documentation](doc/ObjexxFCL.html) for library building and usage information.

## Licensing

The ObjexxFCL is available with a commercial open source license from [Objexx Engineering, Inc.](http://objexx.com) that allows licensee modifications to the code for internal use.
Contact <info@objexx.com> for information about licensing the ObjexxFCL.