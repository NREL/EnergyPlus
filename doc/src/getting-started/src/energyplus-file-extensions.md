# EnergyPlus File Extensions

This section will present a list of common EnergyPlus file extensions and what they mean. This will help you after the EP-Launch program finishes.

## Input Files

The following files are input to the EnergyPlus program.

### IDD

The *input data dictionary* (IDD) is an ASCII (text) file containing a list of all possible EnergyPlus objects and a specification of the data each object requires. This file is analogous to the DOE-2 keyword file. The *Guide for Interface Developers* contains a full description of the input data dictionary.

### idf

The *input data file* (IDF) is an ASCII file containing the data describing the building and HVAC system to be simulated. Many example files are installed as part of the EnergyPlus installation. Additionally, a spreadsheet file "ExampleFiles.xls"  contains columnar descriptions of each file's features.

### imf

The *input macro file* (IMF) is an ascii file containing the data describing the building and HVAC system to be simulated and will have some contents of "macro" commands. The Auxiliary programs document describes use of the macro commands and the program that processes them – EP-Macro.   Many example files are installed as part of the EnergyPlus installation.

### ini

This is the EnergyPlus initialization file. It is an optional ascii input file that allows the user to specify the path for the directory containing Energy+.idd. This file, using the actual directories of the install, will be created during the install. Unless you change where the EnergyPlus.exe file resides, you will not need to change this file.

### epw

The *EnergyPlus weather* file is an ascii file containing the hourly or sub-hourly weather data needed by the simulation program. The data format is described in detail in the Auxiliary Programs Document. It is also described succinctly in the Input Output Reference document.

## Primary Output Files

The following output files are the most important for beginning users.  Then, there is the section on "other output" files.  And, more information about output files (complete list) is shown in the Output Details and Examples Document.

### err

A text file containing the error messages issued by EnergyPlus. **This is the first output that should be examined after a simulation.** Error messages may be issued by EnergyPlus during its input phase or during the simulation. There are three levels of error severity: *fatal*, *severe*, and *warning* as well as simple *"information"* lines. A fatal error causes the program to terminate immediately. The following table illustrates the necessary actions.

Table: Error Message Levels – Required Actions

Error Level|Action
-----------|------
Information, shown as \*\*\*\*\*\*\*\*\*|Informative, usually a follow-on to one of the others. No action required.
\*\*   ~~~   \*\*|This is a continuation of a previous message.  String all the words/sentences together to form the complete message.
Warning|Take note. Fix as applicable.
Severe|Should Fix
Fatal|Program will abort, Must Fix

### csv

When run normally, EP-Launch and EnergyPlus automatically creates post-processed standard output (eso) and meter output (mtr) files into columnar csv (comma separated variable) files.  These files are ready to be read by spreadsheet programs (such as Excel™).

### htm/html

Several of the report options produce html files that can be read in standard Web browsers.  These are very powerful, information packed files.  Though wary of information overload, the easiest way to see "everything" is to enter the following in your input file:

~~~~~~~~~~~~~~~~~~~~

    OutputControl:Table,
        HTML;                    !- ColumnSeparator

    Output:Table:SummaryReports,
        All Summary;
~~~~~~~~~~~~~~~~~~~~

Note that you can also get this file in other formats (the OutputControl:Table option).

## Other Output Files

### audit

This is an text file which echoes the IDD and IDF files, flagging syntax errors in either file. Note that both *err* and *audit* will show most of the error messages caused by input syntax errors; however only *err* will show errors issued during the actual simulation. The *audit* can be used when you need to see the context of the error message to fully ascertain the cause. The *audit* file also contains potentially extra information that may be useful from the input scan.

### eso

The *EnergyPlus Standard Output* (ESO) is a text file containing the time varying simulation output. The format of the file is discussed in the *Guide for Interface Developers* and the *InputOutputReference*. The contents of the file are controlled by *Output:Variable* commands in the IDF file. Although the ESO is a text file, it is not easily interpretable by a human. Usually postprocessing will be done on this file in order to put it in a format that can be read by a spreadsheet; however a quick visual inspection of the file does show whether the expected variables are output at the desired time step.

### mtr

The *EnergyPlus Meter Output* (MTR) is a text file containing the time varying simulation output. The format of the file is similar to the ESO file. As described in a later section of this document (see Energy Meters), meters are a powerful reporting tool in EnergyPlus. Values are grouped onto logical meters and can be viewed the same way that the ESO variables are used. The contents of the file are controlled by *Output:Meter* commands in the IDF file. Although the MTR is a text file, it is not easily interpretable by a human. Usually postprocessing will be done on this file in order to put it in a format that can be read by a spreadsheet; however a quick visual inspection of the file does show whether the expected variables are output at the desired time step.

### mtd

This file contains all the details (i.e., which report variables are on a meter and, conversely, what meters contain) about meters.

### eio

The *EnergyPlus Invariant Output* (EIO) is a text file containing output that does not vary with time. For instance, location information (latitude, longitude, time zone, altitude) appears on this file.

### rdd

### mdd

The *Report (variable) Data Dictionary* (RDD) **is a text file listing those variables available for reporting (on the ESO) for this particular simulation. Which variables are available for output depends on the actual simulation problem described in the IDF. The *Report (meter) Data Dictionary* (MDD) **is a text file listing those variables available for reporting (on the MTR) for this particular simulation. Which meters are available for output depends on the actual simulation problem described in the IDF. A simulation with no chiller does not permit the output of any chiller report variables. The user may need to examine the RDD or MDD to find out which report variables are available in a particular simulation. The RDD and MDD are written only if the following is included in the IDF file.

~~~~~~~~~~~~~~~~~~~~

    Output:Reports, VariableDictionary;
~~~~~~~~~~~~~~~~~~~~

A variant produces the same files in a IDF "ready" format.

~~~~~~~~~~~~~~~~~~~~

    Output:Reports, VariableDictionary, IDF;
~~~~~~~~~~~~~~~~~~~~

### dxf

This is a file in AutoCad™ DXF format showing all the surfaces defined in the IDF file. It provides a means of viewing the building geometry. The DXF file from EnergyPlus highlights different building elements (shading, walls, subsurfaces) in differing colors. A number of programs can read and display DXF files. Output of this file is triggered by

~~~~~~~~~~~~~~~~~~~~

    Output:Reports, Surfaces, DXF;
~~~~~~~~~~~~~~~~~~~~

in the IDF.