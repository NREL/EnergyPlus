# History of changes for LU-Decomposition

## Version 0.2 (2021-08-30)

### Enhancements 

* [Issue 4](https://github.com/j-c-cook/LU-Decomposition/issues/4) - Performance of LU decomposition is increased by introducing multi-threading with OpenMP.

## Version 0.1 (2021-08-26)

### New features

* [Issue 1](https://github.com/j-c-cook/LU-Decomposition/issues/1) - Add LU decomposition to solve a system of equations. The methods are  inspired by the methods from Numerical Recipes in C++.
* [Issue 2](https://github.com/j-c-cook/LU-Decomposition/issues/2) - Add cross-platform tests. Includes Linux Ubuntu 20.04, MacOS-10.15, and Windows 10 2019.
* [Issue 3](https://github.com/j-c-cook/LU-Decomposition/issues/3) - Make CMake build tests only when the main CMake project is LU-Decomposition. The tests can be used for development purposes, but when included in other libraries, there is no reason to run the tests again.  