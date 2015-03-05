# EnergyPlus Files

## Input Files

The following files are input to the EnergyPlus program.

### Energy+.idd

The *input data dictionary* (IDD) is an ascii (text) file containing a list of all possible EnergyPlus objects and a specification of the data each object requires. This file is analogous to the DOE-2 keyword file. The *Guide for Interface Developers* contains a full description of the input data dictionary.

### in.idf

The *input data file* (IDF) is an ascii file containing the data describing the building and HVAC system to be simulated. The *Guide for Interface Developers* shows examples of IDF input. Many example files are installed as part of the EnergyPlus installation.

### in.imf

The *input macro file* (IMF) is an ascii file that is formatted for the EP-Macro program.  Output from the EP-Macro program will be the standard in.idf format.  IMF files are not directly read by EnergyPlus.

### Energy+.ini

This is the EnergyPlus initialization file. It is an optional ascii input file that allows the user to specify the path for the directory containing Energy+.idd. This file, using the actual directories of the install, will be created during the install. An example is:

~~~~~~~~~~~~~~~~~~~~
    [program]
    dir=C:\EnergyPlus

    [weather]
    dir=

    [BasementGHT]
    dir=PreProcess\GrndTempCalc

    [SlabGHT]
    dir= PreProcess\GrndTempCalc
~~~~~~~~~~~~~~~~~~~~

Under [program], dir should indicate the folder where EnergyPlus is installed (e.g. C:\\Program Files\\EnergyPlusV2-0-0 or C:\\EnergyPlusV2-0-0).  This is automatically generated during the install and may be the "shortened form" of these folder names. The "weather" portion of the initialization file is unused for normal EnergyPlus. [BasementGHT] and [SlabGHT] are used by the EP-Launch program when the Utilities tab is used to execute the Basement and Slab programs, respectively.

### in.epw

The *EnergyPlus weather* file is an ascii file containing the hourly or sub-hourly weather data needed by the simulation program. The data format is described in this document in the section: EnergyPlus Weather File (EPW) Data Dictionary.

## Output Files

More information (and more up-to-date) about output files is shown in the Output Details and Examples Document.

### eplusout.err

A text file containing the error messages issued by EnergyPlus. This is the first output that should be examined after a simulation. Error messages are issued by EnergyPlus during its input phase or during the simulation. There are three levels of error severity: *fatal*, *severe*, and *warning* as well as simple *"message"* lines. A fatal error causes the program to terminate immediately. The following table illustrates the necessary actions.

Table: Error Message Levels – Required Actions

Error Level|Action
-----------|------
"Information"|Informative, usually a follow-on to one of the others. No action required.
Warning|Take note. Fix as applicable.
Severe|Should Fix
Fatal|Program will abort

An example of an error message due to an input syntax error is:

~~~~~~~~~~~~~~~~~~~~
    ** Severe  ** Did not find " DessignDay" in list of Objects
       **  Fatal  ** Errors occurred on processing IDF file –
           probable incorrect IDD file. View "audit.out" for details.
       ************* EnergyPlus Terminated--Error(s) Detected.
~~~~~~~~~~~~~~~~~~~~

### eplusout.audit

This is an text file which echoes the IDD and IDF files, flagging syntax errors in either file. Note that both *eplusout.err* and *eplusout.audit* will show the error messages caused by input syntax errors; however only *eplusout.err* will show errors issued during the actual simulation. *eplusout.audit* can be used when you need to see the context of the error message to fully ascertain the cause.

### eplusout.eso

The *EnergyPlus Standard Output* (ESO) is a text file containing the time varying simulation output. The format of the file is discussed in the *Guide for Interface Developers* and the *InputOutputReference*. The contents of the file are controlled by *Report Variable* commands in the IDF file. Although the ESO is a text file, it is not easily interpretable by a human. Usually postprocessing will be done on this file in order to put it in a format that can be read by a spreadsheet; however a quick visual inspection of the file does show whether the expected variables are output at the desired time step.

### eplusout.mtr

The *EnergyPlus Meter Output* (MTR) is a text file containing the time varying simulation output. The format of the file is similar to the ESO file. Meters are a powerful reporting tool in EnergyPlus. Values are grouped onto logical meters and can be viewed the same way that the ESO variables are used. The contents of the file are controlled by *Report Meter* commands in the IDF file. Although the MTR is a text file, it is not easily interpretable by a human. Usually postprocessing will be done on this file in order to put it in a format that can be read by a spreadsheet; however a quick visual inspection of the file does show whether the expected variables are output at the desired time step.

### eplusout.eio

The *EnergyPlus Invariant Output* (EIO) is a text file containing output that does not vary with time. For instance, location information (latitude, longitude, time zone, altitude) appears on this file.

### eplusout.rdd

The *Report (variable) Data Dictionary* (RDD) **is a text file listing those variables available for reporting (on the ESO or MTR) for this particular simulation. Which variables are available for output on the ESO or MTR depends on the actual simulation problem described in the IDF. A simulation with no chiller would not permit the output of any chiller report variables. The user may need to examine the RDD to find out which report variables are available in a particular simulation. The RDD is written only if

~~~~~~~~~~~~~~~~~~~~
    Report, Variable Dictionary;
~~~~~~~~~~~~~~~~~~~~

appears in the input (IDF) file.

### eplusout.dbg

This is a text file containing *debug* output for use by EnergyPlus developers. Generally developers will add debug print statements wherever in the code that that they wish. There is a "standard" debug output that prints out conditions at all the HVAC nodes. This output is triggered by placing

~~~~~~~~~~~~~~~~~~~~
    DEBUG OUTPUT,1;
~~~~~~~~~~~~~~~~~~~~

in the IDF file. If DEBUG OUTPUT, 0 is entered, you will get an empty eplusout.dbg file.

### eplusout.dxf

This is a file in AutoCad DXF format showing all the surfaces defined in the IDF file. It provides a means of viewing the building geometry. The DXF file from EnergyPlus highlights different building elements (shading, walls, subsurfaces) in differing colors. A number of programs can read and display DXF files. One that works well is Volo View Express, available free from the Autodesk web site. Output of this file is triggered by

~~~~~~~~~~~~~~~~~~~~
    Report, Surfaces, DXF;
~~~~~~~~~~~~~~~~~~~~

in the IDF.

### eplusout.sln

A text file containing the coordinates of the vertices of the surfaces in the IDF.

Output of this file is triggered by

~~~~~~~~~~~~~~~~~~~~
    Report, Surfaces, Lines;
~~~~~~~~~~~~~~~~~~~~

in the IDF.

## Postprocessing Program/Files

A postprocessing program *ReadVarsESO.exe* is available that will read an ESO or MTR file and produce a file that can be read by Excel™. It can use an input file or not. In batch mode it is run by the little batch file *RunReadESO.bat*:  Further information on this program is provided in the Input Output Reference as well as the Output Details and Examples documents.

You can also used the CSVProc and convertESOMTR described earlier in this document as part of your post processing strategy.
