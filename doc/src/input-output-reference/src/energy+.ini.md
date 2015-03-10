# Energy+.ini

The Energy+.ini file is read by the EnergyPlus.exe program and is *very important* to the simulation run. In specific, this file contains a pointer to where the Energy+.idd file will be found. More description of the Energy+.ini contents is contained in the Auxiliary Programs document.

For the advanced user, there is also the "EPMacro" program, described in the Auxiliary programs document. You run it as a separate program before EnergyPlus (the batch file included in the install and shown in the GettingStarted document contains the commands). It is automatically run if you use the EP-Launch program.

EnergyPlus may create several output files. Whether some files are produced depends on the actual simulation (for example, the .rdd file is only produced when you request a Variable Dictionary using the Output:Report object). Similarly, the .dxf file is only produced when a DXF surface report is requested. Common output files are shown in the following table. A complete, up to date list is shown in the "Output Details and Examples Document".

Table: EnergyPlus Output Files

FileName|Description
--------|-----------
Eplusout.audit|Echo of input
Eplusout.err|Error file
Eplusout.eso|Standard Output File (contains results from both [Output:Variable](#outputvariable) and [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).
Eplusout.eio|One time output file
Eplusout.rdd|Output variable Data Dictionary
Eplusout.dxf|DXF (from Output:Report,Surfaces,DXF;)
Eplusout.mtr|Similar to .eso but only has Meter outputs.
Eplusout.mtd|Meter details report – what variables are on what meters and vice versa.
Eplusout.end|A one line summary of success or failure (useful for Interface programs)
Eplusout.dbg|From Debug Output object – may be useful to support to help track down problems
Eplusout.<ext>|Not directly from EnergyPlus – produced by utility programs that read the .eso file and produce output files for spreadsheet programs
Eplusout.sln|Similar to DXF output but less "formatted". Results of Report Surface, Lines object.
Epluszsz.<ext>|Results from the [Zone](#zone) Sizing object with extension noted by the Sizing Style object. This file is "spreadsheet" ready.
Eplusssz.<ext>|Results from the System Sizing object with extension noted by the Sizing Style object. This file is "spreadsheet" ready.
Eplusout.bnd|This file contains details about the nodes and branches. Useful in determining if all your nodes are connected correctly.
Eplustbl.<ext>|Results from various "Report Table" and a few other reporting commands. The extension is from the "style" command and this file is "spreadsheet" ready.