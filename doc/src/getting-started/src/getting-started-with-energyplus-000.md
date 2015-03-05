# Getting Started with EnergyPlus

The remainder of this document is intended to give you a start on using the program with a few simple tools (EP-Launch to help run the simulation; IDFEditor to help create or look at input files) as well as some of the features (such as energy meters, simulation results) of using the program.

A developer has also created a tutorial set of pages for EnergyPlus, you may wish to visit:

http://www.vibyor.com Link to the tutorials section. It may be better to have two screens available so that you can go through the steps as you are reading the tutorial.

In addition, the install includes two spreadsheets:

- Example Files Summary Spreadsheet (highlights of each example file)
- Example Files Links to Objects (for any object, up to 3 files using that object are shown)

And for those who want to get results quickly, use the EnergyPlus Example File Generator:

http://apps1.eere.energy.gov/buildings/energyplus/cfm/inputs/

The Example File Generator  will take your descriptive details about a building (or you could use an EnergyPlus input file), run the simulation at the location you pick and send the results along with an input file to your email address.

The standard Windows install procedure has put the following information on your computer, in the directories/folders shown.

(You were allowed to select components, so not all of these may be there).

The main EnergyPlus folder contains **Energy+.idd, EnergyPlus.exe, RunEPlus.bat, shortcut to IDFEditor, readme file(s)**, **EP-Macro.exe, bugreprt.txt file**. This will also contain the translation.exe for converting last "release" input files to the current release (see: Auxiliary Programs document for details) and the WinEPDraw program (again in the Auxiliary Programs document). If chosen, the EP-Launch program will be in this directory as well.

The general layout of folders from the install looks like:

~~~~~~~~~~~~~~~~~~~~

    \EnergyPlus main folder
    \Documentation
    the PDF files of the documentation
    \DataSets
    Reference Data Sets (libraries)
    \MacroDataSets
    Macroized Reference Data Sets (libraries)
    \PreProcess
    \IDFEditor Program files for the IDFEditor
    \GrndTempCalcSpecial program to calculate ground temperatures.
    \BLASTTranslator
    \DOE2Translator
    \WeatherConverter
    \PostProcess
    ReadVarsEsoThe simple post processor exe.
    \ExampleFilesSample input, output, results files shipped with the program.
    \WeatherDataSample weather files shipped with the program.
~~~~~~~~~~~~~~~~~~~~