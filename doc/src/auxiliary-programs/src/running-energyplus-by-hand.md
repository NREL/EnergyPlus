# Running EnergyPlus by Hand

EnergyPlus is compiled as a 32 bit console application on Windows™ (Windows 98, Windows NT, Windows 2000, Windows ME) operating systems, commonly run on the Intel™ or compatible processing chips (aka WinTel machines). To run the program bring up the command prompt and "cd" to the directory containing the executable. Assume that the executable is called *EnergyPlus.exe*. In the same directory EnergyPlus expects *in.idf*, the input data file; *Energy+.idd*, the data dictionary file; *in.epw*, the weather file (needed only if there is a RunPeriod in the input); and optionally *Energy+.ini*, the initialization file. Typing "EnergyPlus" (and hitting the *Enter* key) will execute the program. EnergyPlus will write messages to the command window as it runs. A simulation with two design days and one run period looks like:

~~~~~~~~~~~~~~~~~~~~
     EnergyPlus Starting
     EnergyPlus, Version 1.3
     Warming up
     Initializing Response Factors
     Calculating CTFs for "EXTWALL80", Construction #1
     Calculating CTFs for "PARTITION06", Construction #2
     Calculating CTFs for "FLOOR SLAB 8 IN", Construction #3
     Calculating CTFs for "ROOF34", Construction #4
     Initializing Window Optical Properties
     Initializing Solar Calculations
     Initializing HVAC
     Warming up
     Warming up
     Warming up
     Performing Zone Sizing Simulation
     Warming up
     Warming up
     Warming up
     Performing Zone Sizing Simulation
     Initializing New Environment Parameters
     Warming up {1}
     Warming up {2}
     Warming up {3}
     Warming up {4}
     Starting Simulation at 01/14 for CHICAGO IL UNITED STATES TMY2 94846 WMO#=725340
     Initializing New Environment Parameters
     Warming up {1}
     Warming up {2}
     Warming up {3}
     Warming up {4}
     Starting Simulation at 07/07 for CHICAGO IL UNITED STATES TMY2 94846 WMO#=725340
     EnergyPlus Run Time=00hr 00min  7.31sec
~~~~~~~~~~~~~~~~~~~~

When execution is finished, *eplusout.err* and *eplusout.audit* will always appear. If the program terminated with an input error, these may be the only output files. If the program runs normally, *eplusout.eio* will appear. Depending on what was requested in the input, the other output files described above will also be written.
