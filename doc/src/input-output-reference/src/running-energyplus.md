# Running EnergyPlus

EnergyPlus is written in language conforming to Fortran Standard 90/95/F2003. It runs as a 32 bit console (non-Windows) application on Intel compatible computers. Linux and Macintosh version are also available. Details on running EnergyPlus are available in a separate document (Running EnergyPlus in Auxiliary Programs). The following files are needed to run EnergyPlus:

~~~~~~~~~~~~~~~~~~~~

    EnergyPlus.exe (the executable file)
    Energy+.ini (described below)
    Energy+.idd (the input data dictionary file)
    In.idf (the input file)
    In.epw – optional (weather data file)
~~~~~~~~~~~~~~~~~~~~

The input data dictionary and input data file have been discussed in the previous sections of this document.

For weather simulations, EnergyPlus accepts EnergyPlus weather files. The actual file name is **in.epw**.

The usual release for Windows platform also includes bindings that require these files.

~~~~~~~~~~~~~~~~~~~~

    DElight2.dll
    libexpat.dll
    bcvtb.dll
~~~~~~~~~~~~~~~~~~~~