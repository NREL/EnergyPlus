# EnergyPlus File Extensions

This section will present a list (perhaps not complete) of EnergyPlus file extensions and what they mean. This will help you after the EP-Launch program finishes.

## Input Files

The following files are input to the EnergyPlus program.

### IDD

The *input data dictionary* (IDD) is an ascii (text) file containing a list of all possible EnergyPlus objects and a specification of the data each object requires. This file is analogous to the DOE-2 keyword file. The *Guide for Interface Developers* contains a full description of the input data dictionary.

### idf

The *input data file* (IDF) is an ascii file containing the data describing the building and HVAC system to be simulated. Many example files are installed as part of the EnergyPlus installation. Additionally, a spreadsheet file "ExampleFiles.xls"  contains columnar descriptions of each file's features.

### imf

The *input macro file* (IMF) is an ascii file containing the data describing the building and HVAC system to be simulated and will have some contents of "macro" commands. The Auxiliary programs document describes use of the macro commands and the program that processes them – EP-Macro.   Many example files are installed as part of the EnergyPlus installation.

### ini

This is the EnergyPlus initialization file. It is an optional ascii input file that allows the user to specify the path for the directory containing Energy+.idd. This file, using the actual directories of the install, will be created during the install. Unless you change where the EnergyPlus.exe file resides, you will not need to change this file.

### epw

The *EnergyPlus weather* file is an ascii file containing the hourly or sub-hourly weather data needed by the simulation program. The data format is described in detail in the Auxiliary Programs Document. It is also described succinctly in the Input Output Reference document.

## Output Files

More information (and more up-to-date) about output files is shown in the Output Details and Examples Document.

### err

A text file containing the error messages issued by EnergyPlus. **This is the first output that should be examined after a simulation.** Error messages may be issued by EnergyPlus during its input phase or during the simulation. There are three levels of error severity: *fatal*, *severe*, and *warning* as well as simple *"message"* lines. A fatal error causes the program to terminate immediately. The following table illustrates the necessary actions.

Table: Error Message Levels – Required Actions

Error Level|Action
-----------|------
Information|Informative, usually a follow-on to one of the others. No action required.
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

### audit

This is an text file which echoes the IDD and IDF files, flagging syntax errors in either file. Note that both *err* and *audit* will show most of the error messages caused by input syntax errors; however only *err* will show errors issued during the actual simulation. The *audit* can be used when you need to see the context of the error message to fully ascertain the cause. The *audit* file also contains potentially extra information that may be useful from the input scan.

### eso

The *EnergyPlus Standard Output* (ESO) is a text file containing the time varying simulation output. The format of the file is discussed in the *Guide for Interface Developers* and the *InputOutputReference*. The contents of the file are controlled by *Output:Variable* commands in the IDF file. Although the ESO is a text file, it is not easily interpretable by a human. Usually postprocessing will be done on this file in order to put it in a format that can be read by a spreadsheet; however a quick visual inspection of the file does show whether the expected variables are output at the desired time step.

### mtr

The *EnergyPlus Meter Output* (MTR) is a text file containing the time varying simulation output. The format of the file is similar to the ESO file. As described in the Getting Started document, meters are a powerful reporting tool in EnergyPlus. Values are grouped onto logical meters and can be viewed the same way that the ESO variables are used. The contents of the file are controlled by *Output:Meter* commands in the IDF file. Although the MTR is a text file, it is not easily interpretable by a human. Usually postprocessing will be done on this file in order to put it in a format that can be read by a spreadsheet; however a quick visual inspection of the file does show whether the expected variables are output at the desired time step.

### mtd

This file contains all the details (i.e., which report variables are on a meter and, conversely, what meters contain) about meters.

### eio

The *EnergyPlus Invariant Output* (EIO) is a text file containing output that does not vary with time. For instance, location information (latitude, longitude, time zone, altitude) appears on this file.

### rdd

### mdd

The *Report (variable) Data Dictionary* (RDD) **is a text file listing those variables available for reporting (on the ESO) for this particular simulation. Which variables are available for output depends on the actual simulation problem described in the IDF. The *Report (meter) Data Dictionary* (MDD) **is a text file listing those variables available for reporting (on the MTR) for this particular simulation. Which meters are available for output depends on the actual simulation problem described in the IDF. A simulation with no chiller would not permit the output of any chiller report variables. The user may need to examine the RDD or MDD to find out which report variables are available in a particular simulation. The RDD and MDD are written only if the following is included in the IDF file.

~~~~~~~~~~~~~~~~~~~~
    Output:Reports, VariableDictionary;
~~~~~~~~~~~~~~~~~~~~

A variant produces the same files in a IDF "ready" format.

~~~~~~~~~~~~~~~~~~~~
    Output:Reports, VariableDictionary, IDF;
~~~~~~~~~~~~~~~~~~~~

### dbg

This is a text file containing *debug* output for use by EnergyPlus developers. Generally developers will add debug print statements wherever in the code that that they wish. There is a "standard" debug output that prints out conditions at all the HVAC nodes. This output is triggered by placing

~~~~~~~~~~~~~~~~~~~~
    Output:DebuggingData,1;
~~~~~~~~~~~~~~~~~~~~

in the IDF file. If Output:DebuggingData, 0 is entered, you will get an empty eplusout.dbg file.

### dxf

This is a file in AutoCad DXF format showing all the surfaces defined in the IDF file. It provides a means of viewing the building geometry. The DXF file from EnergyPlus highlights different building elements (shading, walls, subsurfaces) in differing colors. A number of programs can read and display DXF files. Output of this file is triggered by

~~~~~~~~~~~~~~~~~~~~
    Output:Reports, Surfaces, DXF;
~~~~~~~~~~~~~~~~~~~~

in the IDF.

### sln

A text file containing the coordinates of the vertices of the surfaces in the IDF.

Output of this file is triggered by

~~~~~~~~~~~~~~~~~~~~
    Output:Reports, Surfaces, Lines;
~~~~~~~~~~~~~~~~~~~~

in the IDF.

## Postprocessing Program/Files

A postprocessing program *ReadVarsESO.exe* is available that will read an ESO or MTR file and produce a file that can be read by Excel™. It can use an input file or not. In batch mode it is run by the little batch file *RunReadESO.bat*:  Further information on this program is provided in the Input Output Reference under a section heading called "Using ReadVarsESO".
