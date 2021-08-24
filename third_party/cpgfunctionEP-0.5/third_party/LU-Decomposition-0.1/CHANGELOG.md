# History of changes for LU-Decomposition

## Version 0.1 (2021-08-17)

### New features

* [Issue 1](https://github.com/j-c-cook/LU-Decomposition/issues/1) - Add LU-decomposition to solve system of linear equations. The methods are from Numerical Recipes in C++. There is currently no effort put into optimization or multi-threading.
* [Issue 2](https://github.com/j-c-cook/LU-Decomposition/issues/2) - Add cross-platform tests. Includes Linux Ubuntu 20.04, MacOS-10.15, and Windows 10 2019.
* [Issue 3](https://github.com/j-c-cook/LU-Decomposition/issues/3) - Make CMake build tests only when the main CMake project is LU-Decomposition. The tests can be used for development purposes, but when included in other libraries, there is no reason to run the tests again.  