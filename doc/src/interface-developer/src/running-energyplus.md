# Running EnergyPlus

EnergyPlus is written in language conforming to Fortran Standard 90/95.  It runs as a 32 bit console (non-Windows) application on Intel compatible computers (Windows NT, Windows 95/98).  More explicit details on running EnergyPlus are available in a separate document (Running EnergyPlus in Auxiliary Programs document).  The following files are used to run EnergyPlus:

~~~~~~~~~~~~~~~~~~~~

    EnergyPlus.exe (the executable file)
    Energy+.ini (described below)
    Energy+.idd (the input data dictionary file)
    In.idf (the input file)
    In.epw – optional (weather data file)
~~~~~~~~~~~~~~~~~~~~

The input data dictionary and input data file have been discussed in the previous sections of this document.

For weather simulations, EnergyPlus accepts EnergyPlus weather files. Previous versions accepted BLAST formatted weather files and now a BLASTWeatherConverter program is provided.  The actual file name is **in.epw**.

The Energy+.ini file is a "standard" Windows™ ini file and can be manipulated using the Windows API calls though EnergyPlus uses standard Fortran to manipulate it.  It is a very simple ini file and is fully described in the Auxiliary Programs document. Energy+.ini and in.idf file should be in the directory from which you are running EnergyPlus.exe.

For the advanced user, there is also the "EPMacro" program, described in the Auxiliary Programs Document.  You run it as a separate program before EnergyPlus (the batch file included in the install and shown in the GettingStarted document contains the commands).

EnergyPlus creates the following files (plus more):

Table: EnergyPlus Output Files

FileName|Description
--------|-----------
Audit.out|Echo of input
Eplusout.err|Error file
Eplusout.eso|Standard Output File
Eplusout.eio|One time output file
Eplusout.rdd|Report Variable Data Dictionary
Eplusout.dxf|DXF (from Report,Surfaces,DXF;)
Eplusout.end|A one line summary of success or failure

The eplusout.err file may contain three levels of errors (Warning, Severe, Fatal) as well as the possibility of just message lines.  These errors may be duplicated in other files (such as the standard output file).

Table: EnergyPlus Errors

Error Level|Action
-----------|------
Warning|Take note
Severe|Should Fix
Fatal|Program will abort

EnergyPlus produces several messages as it is executing, as a guide to its progress.  For example, the run of the 1ZoneUncontrolled input file from Appendix A produces:

~~~~~~~~~~~~~~~~~~~~

    EnergyPlus Starting
     EnergyPlus 1.3.0.011, 4/5/2006 2:59 PM
     Initializing New Environment Parameters
     Warming up {1}
     Initializing Response Factors
     Initializing Window Optical Properties
     Initializing Solar Calculations
     Initializing HVAC
     Warming up {2}
     Warming up {3}
     Warming up {4}
     Starting Simulation at 12/21 for DENVER_STAPLETON ANN HTG 99% CONDNS DB
     Initializing New Environment Parameters
     Warming up {1}
     Warming up {2}
     Warming up {3}
     Starting Simulation at 07/21 for DENVER_STAPLETON ANN CLG 1% CONDNS DB=>MWB
     EnergyPlus Run Time=00hr 00min  1.00sec
~~~~~~~~~~~~~~~~~~~~

Extensive timing studies and fine-tuning of EnergyPlus is NOT complete.  To give you an idea of comparable run times, we present the following (does not include HVAC) with an early version of EnergyPlus running on a 450MHZ machine.  Remember, BLAST would be 1 calculation per hour, EnergyPlus (in this case) was 4 calculations per hour.  Obviously, these are quite out of date.  However, a recent change in a developer's test machine illustrates the importance of maximum memory.  A 5 zone full year run on a 1.8GHZ, 1GB machine was running about 8 minutes – with a new 2.1GHZ, 2GB machine the same file takes about 2 minutes.

Table: Timings Comparison (EnergyPlus vs. BLAST)

File|BLAST Per Zone|EnergyPlus Per Zone
----|--------------|-------------------
GeometryTest (5 Zones, 2 Design Day, Full Weather Year)|13 sec|33 sec
SolarShadingTest (9 Zones, Full Weather Year)|7 sec|25 sec