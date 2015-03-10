# Requirements and Responsibilities

Any new library must meet the following requirements:

The library source code must be provided and stored in the main source code repository.

The library source code must be compatible with EnergyPlus's authorship requirements and license options **including its open-source version**.

The library souce code must be written in a high-level programming language that is fully compiled and portable on all three major platforms, Windows, Mac, and Linux.

There must be two version of the fortran source code that calls the library, one a stub and one that actually calls the library.  The stub will be used by developers who do not want to build with a dependency on your library.

When binding with C-based languages, the fortran code must use ISO standard for variable type declarations (use intrinsic ISO_C_Binding).

When binding with FORTRAN languages, the variable declarations should use EnergyPlus practice (see DataPrecisionGlobals.f90).

The developer adding a new library must assume the following responsibilities:

The developer must demonstrate that the library compiles on all platforms including: 1. Windows 32 bit, 2. Windows 64 bit, 3. Mac 32 bit, 4. Mac 64 bit, 5. Linux 32 bit and Linux 64 bit. Often when the source code is originally developed on only one platform, it needs to be modified to work correctly on all the other platforms.

The developer must submit modified CMAKE build system files that demonstrate the library compiles and links on all platforms.  See below for more information on the cross-platform build system.

The developer must show that example files that exercise the library run on all platforms and produce the same results.

For each subsequent release of EnergyPlus, that includes the feature, the developer must assist the team with testing the installation packages.