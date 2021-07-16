# History of changes (cpgfunctionEP)

## Version 0.1.0 (2021-07-08)

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


# History of changes (cpgfunction)

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




