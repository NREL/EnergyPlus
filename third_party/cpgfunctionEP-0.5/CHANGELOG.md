# History of changes - cpgfunctionEP

## Version 0.6 (2021-09-02)

### Enhancements

* [Issue 48](https://github.com/j-c-cook/cpgfunctionEP/issues/48), [Issue 54](https://github.com/j-c-cook/cpgfunctionEP/issues/54) - The finite line source integration is replaced with an approximation presented by Cimmino (2021). This implementation introduced an exponential integral. The exponential integral is available in both the Windows and Linux standard library, but is not available in MacOS. A hand-rolled exponential integral is introduced to make the build cross-platform. 
* [Issue 49](https://github.com/j-c-cook/cpgfunctionEP/issues/49) - An additional segment response packed matrix had previously been introduced for purposes of interfacing with `BLAS`. At the time, that was a quick resolution to get the code working. The final solution has been to transpose `h_ij` from `[nSum x nt]` to `[nt x nSum]` so that `BLAS` functions can be readily made use of. The 1D solution will not work due to a maximum size that can be allocated to `std::vector`.

## Version 0.5 (2021-08-17)

### Changes

* [Issue 46](https://github.com/j-c-cook/cpgfunctionEP/issues/46) - A library named LU-Decomposition is added. The library only includes minimal functions to perform LU-decomposition and solve a system of linear equations.

### Removes

* [Issue 44](https://github.com/j-c-cook/cpgfunctionEP/issues/44) - The dependency on the Eigen library is removed due to an unexplainable segment fault that occurred only during the Linux Ubuntu tests on the EnergyPlus CI machines.

## Version 0.4 (2021-07-30)

### Build Changes

* [Issue 42](https://github.com/j-c-cook/cpgfunctionEP/issues/42) - The linked library targets are now imported with a
  double colon. This follows cmake-policy 28 [CMP0028](https://cmake.org/cmake/help/latest/policy/CMP0028.html).

* [Issue 41](https://github.com/j-c-cook/cpgfunctionEP/issues/41) - The type (PRIVATE, INTERFACE, PUBLIC) of linkage in
  targeted libraries is now defined (see cmake
  [docs](https://cmake.org/cmake/help/latest/command/target_link_libraries.html#libraries-for-a-target-and-or-its-dependents)
  or Mr. Scott's [explanation](https://cmake.org/pipermail/cmake/2016-May/063400.html) of libraries for a target and/or
  its dependents for more details on why this was done).

* [Issue 40](https://github.com/j-c-cook/cpgfunctionEP/issues/40) - When the `CMAKE_PROJECT_NAME` is not equal to the
  `PROJECT_NAME` the tests have not been built, and now the copy of Eigen located in EnergyPlus's third party folder
  will be linked rather than including the copy of Eigen with this library.

## Version 0.3.2 (2021-07-27)

### Fixes

* [Issue 38](https://github.com/j-c-cook/cpgfunctionEP/issues/38) - The investigation of this issue reopened
  [Issue 35](https://github.com/j-c-cook/cpgfunctionEP/issues/35). There was a bug that occurred when the time value in
  the interpolation function fell between 0 and t1. This bug had not propagated because time steps that small in the
  beginning had never been tested. It was false to claim Cimmino's load history reconstruction methodology required
  a special time-step. While it may be slightly more accurate with a geometric expansion, the solution is still valid
  for all ln(t/ts) values (as it should be).

* [Issue 34 (Reopened)](https://github.com/j-c-cook/cpgfunctionEP/issues/34) - Both arguments in
  `gt::segments::SegmentResponse` are now passed as a copy.

## Version 0.3.1 (2021-07-26)

### Fixes

* [Issue 35](https://github.com/j-c-cook/cpgfunctionEP/issues/35) - The adaptive g-function calculation is fixed by
  replacing the constant log time vector expansion function with a geometric expansion. The constant log time vector
  expansion is removed from the library because it is incompatible with Cimmino's load history reconstruction scheme.

* [Issue 34](https://github.com/j-c-cook/cpgfunctionEP/issues/34) - The discretization function in `gt::segments::adaptive`
  now takes height as a copy rather than reference. The reason is that EnergyPlus seemed to require it this way, given
  the variable was being passed from another objects instance.

* [Issue 33](https://github.com/j-c-cook/cpgfunctionEP/issues/33) - EnergyPlus has a clang-tidy check that runs at each
  push. The instantiation of `gt::segments::SegmentResponse` was made clang-tidy by reordering the arguments that are
  passed in. The instances must be in the same order as the arguments that are passed.

* [Issue 32](https://github.com/j-c-cook/cpgfunctionEP/issues/32) - The Eigen library is now referenced locally in
  CMakeLists. It is not acceptable to require EnergyPlus users to install libraries. Therefore, the find_package
  function of CMake is not able to be used for Eigen.

## Version 0.3 (2021-07-21)

### Changes

* [Issue 30](https://github.com/j-c-cook/cpgfunctionEP/issues/30) - An interp1d
  function is modified to perform nearest value extrapolation. The function now
  also returns a double rather than having to pass the double in by reference to
  be modified.

* [Issue 31](https://github.com/j-c-cook/cpgfunctionEP/issues/31) - The
  g-function API is changed. The boolean argument for adaptive discretization is
  removed. This is done because the number of segments is already an input.
  Having both arguments is redundant. The adaptive discretization object can
  obtain the number of segments necessary for the ideal UBHWT and then be
  passed into the g-function calculation.

### Enhancements

* [Issue 16](https://github.com/j-c-cook/cpgfunctionEP/issues/16) - An adaptive
  discretization is added by double interpolation. The ideal segment length
  data are available for 5 heights and a range of total drilling depths.
  Therefore, via interpolation, any height and drilling depth is allowed.
  Extrapolation makes use of the closest point.

* [Issue 22](https://github.com/j-c-cook/cpgfunctionEP/issues/22) - Adds time
  function that accepts time in seconds, hours, months or years. The time step
  is constant logarithmic with a default of 0.35, beginning at ln(t/ts) = -8.5.

## Version 0.2 (2021-07-20)

### Removes

* [Issue 19](https://github.com/j-c-cook/cpgfunctionEP/issues/19) - The
  references to nlohmann json are removed from inside of the library. The
  EnergyPlus team does not want the dependency, and there will never be a
  reason for EnergyPlus to use the coordinate input/output functions. The
  tests are still dependent on nlohmann when built.

### Fixes

* [Issue 12](https://github.com/j-c-cook/cpgfunctionEP/issues/12) - The use
  similarities false option now computes and fills the packed segment response
  matrix.

### Enhancements

* [Issue 14](https://github.com/j-c-cook/cpgfunctionEP/issues/14) - The size of
  the borehole fields being run in the regression tests are reduced so that less
  time is spent running tests (that occur at each push).

* [Issue 13](https://github.com/j-c-cook/cpgfunctionEP/issues/13) - The tests
  are only configured and ran if the main project being worked on is
  cpgfunctionEP. This is done because EnergyPlus will use this as a third party
  subdirectory, and will not want to run the tests located in this library.

### Maintenance

* [Issue 26](https://github.com/j-c-cook/cpgfunctionEP/issues/26) - The size
  (number of files) of the Eigen dependency is minimized by only including the
  necesary files to execute the LU decomposition.

* [Issue 28](https://github.com/j-c-cook/cpgfunctionEP/issues/28) - The third
  party library `nlohmann` json now only makes use of the single json.hpp
  header include. This has been done to reduce the number of files in this
  project.

### Changes

* [Issue 8](https://github.com/j-c-cook/cpgfunctionEP/issues/8) - The uniform
  borehole wall temperature g-function calculation has an API change. The
  boolean argument for multi-threading is taken away. The number of threads is
  still an argument, and the default is all available threads on the processor.

## Version 0.1 (2021-07-08)

### Maintenance

* [Issue 9](https://github.com/j-c-cook/cpgfunctionEP/issues/9) - The project builds on Linux, MacOS and Windows upon
  commit [558c63c](https://github.com/j-c-cook/cpgfunctionEP/pull/1/commits/558c63c9b5c4041282fdb473327463b8fe183fbd).

* [Issue 6](https://github.com/j-c-cook/cpgfunctionEP/issues/6) - Dependencies are placed into the `third_party/`
  folder. The dependencies source code is pasted in, rather than using `git subtree` or `git submodule`.

* [Issue 5](https://github.com/j-c-cook/cpgfunctionEP/issues/5) - Blas and lapack have been removed, thus deprecating
  libraries dependency on Fortran. The following lists what the Fortran function calls were replaced with by commit:
  - Commit [a20d3ea](https://github.com/j-c-cook/cpgfunctionEP/pull/1/commits/a20d3eacec67d5994b65d6716774c50404e26428) -
    Replaces the blas copy function with the native C++ algorithm standard copy function (`std::copy`)
  - Commits [ec35baf](https://github.com/j-c-cook/cpgfunctionEP/pull/1/commits/ec35baf21025bb61a105ac9edd06e1e348676702)
    and [70ab158](https://github.com/j-c-cook/cpgfunctionEP/pull/1/commits/70ab1585d0efe5dd53dca90f3cf5685585030ef9) -
    Replaces the `blas::axpy` Fortran code with a C++ version.
  - Commit [383bf08](https://github.com/j-c-cook/cpgfunctionEP/pull/1/commits/383bf08f31d020af2cabe63d0d8b86eb4bbb8191) -
    Replaces the symmetric packed matrix Fortran call with C++ code.
  - Commit [231e35e](https://github.com/j-c-cook/cpgfunctionEP/pull/1/commits/231e35ea416fe0943514cc82dc79238f3f2b79dd) - The
    LU decomposition of lapack is replaced by an Eigen function call.

* [Issue 3](https://github.com/j-c-cook/cpgfunctionEP/issues/3) - Boost is removed by replacing the thread pools with
  `OpenMP` calls, and the Gauss-Kronrod integration is replaced with a header only file by the name of
  [qdt](https://github.com/j-c-cook/cpgfunctionEP/tree/MilestoneV0.1/third_party/qdt-master). Qdt is a library
  written by Adolfo Munoz who released the code alongside a paper by the name of "Higher Order Ray Marching".
  The Kronrod integral of Munoz appears much faster than boost's.


### Fixes

* [Issue 10](https://github.com/j-c-cook/cpgfunctionEP/issues/10) - Vector over flow error associated with number of
  time steps being less than 6 is fixed.
  Commit [558c63c](https://github.com/j-c-cook/cpgfunctionEP/commit/558c63c9b5c4041282fdb473327463b8fe183fbd)
  fixes a vector overflow error that occured when the number of time steps described were less than 6 (`nt<6`). Commit
  [94e222b](https://github.com/j-c-cook/cpgfunctionEP/commit/94e222bb0cf670f1a4f3058bd1b7b6c7f8bf8233) now allows the
  function time_geometric to have less than 6 time steps.


# History of changes - cpgfunction

## Version 2.0.0 (2021-05-23)

### Enhancements

* [Issue 25](https://github.com/j-c-cook/cpgfunction/issues/25) - Removes all references to the 3D `h_ij`
  segment response matrix. See [PR 30](https://github.com/j-c-cook/cpgfunction/pull/30).

* [Issue 32](https://github.com/j-c-cook/cpgfunction/issues/32) - The multi-dimensional matrices,
  `q_reconstructed` and `h_ij`, are made one dimensional prior to passage into the temporal superposition
  function so that `BLAS` routines can be heavily depended on and the loops completely unraveled.  
  See [PR 30](https://github.com/j-c-cook/cpgfunction/pull/30).

* [Issue 33](https://github.com/j-c-cook/cpgfunction/issues/33) - It is found that the
  packed segment resopnse matrix can be directly made us of in `BLAS spmv`, and that addition
  greatly optimizes the temporal superposition function. For now the assumption is made that all
  segments in the field are of equivalent length, which is true and fine, but at some point in the
  future unequal segment lengths should be made possible again.
  See [PR 30](https://github.com/j-c-cook/cpgfunction/pull/30).

### New features

* [Issue 28](https://github.com/j-c-cook/cpgfunction/issues/28) -
  The third party library LinearAlgebra (`jcc:la`) is included and made use of for `LU`
  factorization in `gfunction.cpp`

* [Issue 12](https://github.com/j-c-cook/cpgfunction/issues/12) -
  A boolean toggle option is added for multi-threading for computing the
  uniform borehole wall temperature (UBHWT) g-function

### API Changes

* [Issue 16](https://github.com/j-c-cook/cpgfunction/issues/16) - The `uniform borehole wall temperature`
  g-function definition is defined for planned use in EnergyPlus with all arguments. Not all the arguments
  currently have a purpose, the adaptive discretization and number of thread arguments are place holders.

## Version 1.0.0 (2021-05-12)

### New features

* [Issue 20](https://github.com/j-c-cook/cpgfunction/issues/20) -
  Added OpenBlas as the basic linear algebra subprogram (BLAS) vendor to CMakeLists.txt

* [Issue 18](https://github.com/j-c-cook/cpgfunction/issues/18) -
  Added new borefield interface with API access to typical borehole configurations

* [Issue 13](https://github.com/j-c-cook/cpgfunction/issues/13) -
  Implemented g-function accuracy tests via CMakeLists.txt for a Rectangle, Open Rectangle, U shape,
  L shape and a custom (Poisson disk) configuration

* [Commit 45141fa](https://github.com/j-c-cook/cpgfunction/pull/14/commits/45141fa745d92ac8a08eea2a06801d7a01fac367) -
  Create new uniform borehole wall temperature API to consider the new borefield and time API's

* [Commit f8863ad](https://github.com/j-c-cook/cpgfunction/pull/14/commits/f8863ad6879bdcb43d8bbed48ab1be1701eb56f5) -
  Added time vector API and associated test

* [Commit 654160f](https://github.com/j-c-cook/cpgfunction/pull/14/commits/654160f9b508f57b917fc0630437cff726dc8440) -
  Modify API for creating a vector of boreholes (borefield)



