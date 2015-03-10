# Running the EP-Macro program

The EP-Macro program is run automatically from the batch files (RunEPlus or EPL-Run from EP-Launch). Skip this small section if you are using either the RunEPlus batch file or EP-Launch. If you wish to run it by hand, it is found in the main folder of the EnergyPlus install (bin folder in the Linux install). Note that in EP-Launch and other script files for use with EP-Macro the convention is to name the file <filename>.imf (input macro file). If you name it <filename>.idf (input data file), the scripts will most likely think it is a "normal" EnergyPlus input file and ignore using EP-Macro on it – giving you a less than desireable result.

Table: Files used in EP-Macro program

**File Name**|**Description**
--------------------------|----------------------------
in.imf|Input file containing the macro commands
audit.out|audit of the EPMacro run
out.idf|Output file that can be run with EnergyPlus

The EP-Macro program is a Console Application, so to run by hand you would need to open a command prompt in the Main EnergyPlus install folder. Then, you would need to copy your input file containing the macro commands from its folder to this folder with the name "in.imf". The installed name of the EP-Macro program is "epmacro.exe". After execution, you can save the out.idf in an appropriate folder or rename it to in.idf in order to execute EnergyPlus. You can view the audit.out file for any errors that might have occurred during EP-Macro processing.