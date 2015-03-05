# External Libraries

Although the most of EnergyPlus is implemented in Fortran90 and compiled as one large exectutable program, shared libraries are also used.  As of version 7.2, the program includes statically linked libraries for SQLite and Delight and dynamically linked libraries for BCVTB, and FMU/FMI.  Then, whatever is required by the compiler version to be included. This section discusses what is required when adding new external libraries.

Although external libraries are allowed when necessary, developers should be aware that there are disadvantages to using them.  EnergyPlus is a cross-platform application and it is the developers responsibility to provide a library that is also cross-platform.  The core EnergyPlus team is small and currently can only maintain the core Fortran.